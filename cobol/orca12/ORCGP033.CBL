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
       PROGRAM-ID.             ORCGP033.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : オンライン資格患者確認画面（Ｐ０３３）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  20/09/XX    NMED-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.00.00    ORCAMO       21/07/XX  資格有効性表示対応
      *  05.00.00    ORCAMO       21/08/XX  保険の確認日付更新と処理済み対応
      *  05.00.00    ORCAMO       21/11/XX  オン資格
      *                                     限度額適用認定証・特定疾病療養受療証対応
      *                                     公費照会対応
      *  05.01.00    ORCAMO       23/01/XX  性別変更対応
      *  05.02.00    ORCAMO       23/04/XX  保険の未来開始日対応
      *  05.02.00    ORCAMO       23/05/XX  患者登録情報上限99件対応
      *  05.02.00    ORCAMO       23/11/XX  被保険者資格証明書対応
      *                                     「保険更新なし」対応
      *  05.02.00    ORCAMO       24/08/XX  資格取得年月日対応
      *  05.02.00    ORCAMO       24/12/XX  訪問診療・オン診療対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    患者登録スパ領域
           COPY    "P02COMMON-SPA".
      *    患者画面スパ領域
           COPY    "P02SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-ONSKAKU         PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-NO              PIC 9(01).
           03  FLG-KIKAN-OK        PIC 9(01).
           03  FLG-HKNHEN          PIC 9(01).
           03  FLG-KOHCHK          PIC 9(01).
           03  FLG-CHKKBN          PIC 9(01).
           03  FLG-HKNADD          PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
           03  IDX-SK              PIC 9(04).
           03  IDX-SK2             PIC 9(04).
           03  IDX-OSK             PIC 9(04).
           03  IDX-F               PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-HKNJANUM            PIC X(08).
           03  WRK-HKNJANAME           PIC X(50).
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-HKNNUM-CK           PIC X(03).
           03  WRK-HKNNUMMEI           PIC X(20).
           03  WRK-KIKAN.
               05  WRK-STYMD           PIC X(09).
               05  WRK-K1              PIC X(02).
               05  WRK-EDYMD           PIC X(09).
      *
           03  WRK-PIDMSG              PIC X(100).
           03  WRK-ERRCD               PIC X(04).
      *
           03  WRK-MIDASHI             PIC X(100).
      *
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-CHK-IDX         PIC 9(03).
           03  WRK-ZZZ             PIC ZZZ.
      *
           03  WRK-FTNRATE-9           PIC 9(03).
           03  WRK-Z9                  PIC Z9.
           03  WRK-FTNRATE-G.
               05  WRK-FTNRATE-X           PIC X(02).
               05  WRK-FTNRATE-M           PIC X(02).
           03  WRK-RES-POST.
               05  WRK-RES-POST1       PIC X(03).
               05  WRK-RES-POSTX       PIC X(01).
               05  WRK-RES-POST2       PIC X(04).
      *
      *    限度額適用認定証提供
           03  WRK-GENDO-SHO           PIC X(50).
           03  WRK-GENDO-TEK           PIC X(50).
           03  WRK-GENDO-STYMD         PIC X(09).
           03  WRK-GENDO-EDYMD         PIC X(09).
           03  WRK-GENDO-SKJGNSTYMD    PIC X(09).
      *
           03  WRK-SIKKAN              PIC X(50).
           03  WRK-SIKKAN-STYMD        PIC X(09).
           03  WRK-SIKKAN-EDYMD        PIC X(09).
           03  WRK-SIKKAN-FTNMONEY     PIC X(06).
           03  WRK-SIKKAN-FTNMONEY-9   PIC 9(06).
           03  WRK-ZZ9                 PIC ZZZZZ9.
      *
           03  WRK-PTHKN-HKNNUM        PIC X(03).
      *R03.8
           03  WRK-MAE-KAKUNINYMD      PIC X(08).
           03  WRK-OYA-UUID            PIC X(36).
      *
           03  WRK-CHKMSG              PIC X(100).
      *
      *   年齢表示
       01  WRK-NENREI-AREA.
           03  WRK-YMDS1.
               05  WRK-YYS1        PIC 9(04).
               05  WRK-MMS1        PIC 9(02).
               05  WRK-DDS1        PIC 9(02).
           03  WRK-YMDS2.
               05  WRK-YYS2        PIC 9(04).
               05  WRK-MMS2        PIC 9(02).
               05  WRK-DDS2        PIC 9(02).
           03  WRK-NENREI.
               05  WRK-NENREI-YY       PIC 9(04).
               05  WRK-NENREI-MM       PIC 9(02).
               05  WRK-NENREI-DD       PIC 9(02).
       01  WRK-NENREI-G.
           03  WRK-NENREI-Z        PIC ZZ9.
           03  WRK-NENREI-MEI      PIC X(03).
       01  WRK-NENREI-GR           REDEFINES   WRK-NENREI-G.
           03  WRK-NENREI-ZN       PIC Z9.
           03  WRK-NENREI-MEIN     PIC X(04).
      *
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
      *
      *
      *    オンライン資格確認照会結果テーブル
       01  HZN-ONSKAKU-REC.
           COPY    "CPONSHI-KAKU.INC"  REPLACING
                                     //ONSKAKU-// BY //HZN-ONSKAKU-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    オンライン資格確認照会結果テーブル
       01  ONSKAKU-REC.
           COPY    "CPONSHI-KAKU.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY "CPPTINF.INC".
      *    保険者号マスタ
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *    検索文字追加
           COPY    "CPORCSADDSIGN.INC".
      *
      *    補助区分変換パラメタ（API用）
           COPY    "CPORAPI012SUB1.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      * 
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    保険確認年月日履歴登録サブ
           COPY    "CPORCSPTHKNRRK.INC".
      *R03.9
      *    限度額適用認定証など編集パラメタ
           COPY    "CPORCSONSHI002.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA12SCRAREA.INC".
      *
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
           MOVE    SPA-ERRCD       TO  WRK-ERRCD
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面編集後、表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-P02KYOUTU     TO  SPA-WORK
           MOVE    SPA-AREA          TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA    TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *     INITIALIZE                  FLG-AREA
      *     INITIALIZE                  CNT-AREA
      *     INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "PERR"
               MOVE    SPACE               TO  SPA-MOTOPG
      *        公費ありの警告時は終了
               IF      WRK-ERRCD           =   "K100"
                    PERFORM 210-BACK
                    GO  TO  100-INIT-EXT
               END-IF
      *
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
      *
               IF      FLG-END             =   ZERO
      *            画面編集
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    "NEW"               TO  MCP-PUTTYPE
               MOVE    "P033"              TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  FLG-END
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           EVALUATE    SPA-MOTOPG
      *        確認メッセージ
               WHEN    "PID1"
                   PERFORM 3001-PID1-SYORI-SEC
      *
                   IF      SPA-ERRCD       NOT =   SPACE
                       PERFORM 510-ERRSET-SEC
                   END-IF
                   IF     (SPA-ERRCD           =   SPACE )
                   AND    (SPA-PIDCD       NOT =   SPACE )
                       PERFORM 520-JIDSET-SEC
                   END-IF
      *
               WHEN    "P034"
      *            公費確認
                   CONTINUE
      *
               WHEN    "P037"
      *            公費画像確認から
                   PERFORM 4037-KOUHI-HEN-SEC
      *
               WHEN    "PCHK"
      *        保険変更へ
                   PERFORM 405-P034-SYORI-SEC
      *
               WHEN    OTHER
                   PERFORM 3000-SCREEN-INIT-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期編集処理
      *****************************************************************
       3000-SCREEN-INIT-SEC            SECTION.
      *
           INITIALIZE                      SPA-P033-AREA
           INITIALIZE                      SPA-ONSKAKU-AREA
      *
           MOVE    SPA-MOTOPG          TO  SPA-P033-MOTOPG
      *    オンライン資格確認編集
           PERFORM 3001-ONSHIKAKU-SYORI-SEC
      *    患者一覧編集
      **   PERFORM 3002-PTINF-LIST-SEC
      *
           IF     (SPA-P033-SELNUM     =   ZERO )
             AND  (WRK-CHK-IDX         >   ZERO )
      *        氏名が一致する患者あり
               MOVE    WRK-CHK-IDX     TO  SPA-P033-SELNUM
           END-IF
           IF      SPA-P033-SELNUM    >   ZERO
      *        患者情報編集
               PERFORM 3003-PTINF-HENSYU-SEC
      *2024/01
               IF      SPA-P033-ON-CARD-CLASS  =   "A1"
      *            保険変更なしへ
                   MOVE    07              TO  SPA-P033-CUR
                   GO  TO  3000-SCREEN-INIT-EXT
               END-IF
      *
               IF      FLG-HKNHEN          =   1
      *2023/10     OR ( SPA-P033-SYORIKBN   =   "2" )
      *            保険変更
                   MOVE    10              TO  SPA-P033-CUR
               ELSE
                   MOVE    12              TO  SPA-P033-CUR
      *2023/10
      *            被保険者証照会で追加不可
                   IF    ( SPA-P033-SYORIKBN   =   "2" )
                    AND  ( SPA-P033-HEN-HKNADD =   SPACE)
                       MOVE    10              TO  SPA-P033-CUR
                   END-IF
               END-IF
           ELSE
               MOVE    1                   TO  SPA-P033-CUR
      *        新規患者へ
               IF      SPA-P033-MAX        =   ZERO
                   MOVE    08              TO  SPA-P033-CUR
               END-IF
           END-IF
           .
       3000-SCREEN-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    オンライン資格確認編集処理
      *****************************************************************
       3001-ONSHIKAKU-SYORI-SEC              SECTION.
      *
           INITIALIZE                      SPA-P033-ONSKAKU-G
      *
      *    オンライン資格確認UID検索処理
           INITIALIZE                      ONSKAKU-REC
           MOVE    SPA-HOSPNUM         TO  ONSKAKU-HOSPNUM
           MOVE    SPA-P031-SEL-UUID   TO  ONSKAKU-TBL-UUID
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
           IF      FLG-ONSKAKU         =   ZERO
      *        処理中フラグ設定
      ***      PERFORM 30012-ONSHIKAKU-MODESET-SEC
      *        オンライン資格確認情報編集処理
               PERFORM 30011-ONSHIKAKU-HEN-SEC
      *        患者一覧編集
               PERFORM 3002-PTINF-LIST-SEC
           END-IF
      *
      *    見出し
           MOVE    SPACE               TO  SPA-P033-MIDASHI
           EVALUATE    SPA-P033-SYORIKBN
               WHEN    "1"
                   MOVE    "【顔認証照会】"
                                           TO  WRK-MIDASHI
               WHEN    "2"
                   MOVE    "【被保険者証照会】"
                                           TO  WRK-MIDASHI
               WHEN    "3"
                   MOVE    "【保険証ＯＣＲ照会】"
                                           TO  WRK-MIDASHI
      *2024/12
               WHEN    "4"
                   MOVE    "【訪問診療照会】"
                                           TO  WRK-MIDASHI
               WHEN    "5"
                   MOVE    "【オンライン診療照会】"
                                           TO  WRK-MIDASHI
           END-EVALUATE
           IF      SPA-P033-ON-PTNUM   =   SPACE
               MOVE    WRK-MIDASHI         TO  SPA-P033-MIDASHI
           ELSE
               STRING  WRK-MIDASHI
                      "（患者番号："
                       SPA-P033-ON-PTNUM
                       "）"
                                       DELIMITED  BY  SPACE
                                       INTO  SPA-P033-MIDASHI
               END-STRING
           END-IF
      *
           .
       3001-ONSHIKAKU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    オンライン資格確認情報編集処理
      *****************************************************************
       30011-ONSHIKAKU-HEN-SEC              SECTION.
      *
      *    資格区分
           MOVE    ONSKAKU-SHOKAI-KBN      TO  SPA-P033-SYORIKBN
      *2022/09
      *    保険証ＯＣＲは処理区分＝３とする
           IF     (ONSKAKU-SHOKAI-KBN      =   "2"  )  AND
                  (ONSKAKU-ORCA-SHOKAI-KBN =   "2"  )
               MOVE    "3"                     TO  SPA-P033-SYORIKBN
           END-IF
      *2024/12
      *    訪問診療は処理区分＝４とする
           IF     (ONSKAKU-SHOKAI-KBN      =   SPACE )  AND
                  (ONSKAKU-ONSHINRYO-KBN   =   "2"   )
               MOVE    "4"                     TO  SPA-P033-SYORIKBN
           END-IF
      *    オン診療は処理区分＝５とする
           IF     (ONSKAKU-SHOKAI-KBN      =   SPACE )  AND
                  (ONSKAKU-ONSHINRYO-KBN   =   "3"   )
               MOVE    "5"                     TO  SPA-P033-SYORIKBN
           END-IF
      *
      *2023/10
      *    被保険者証区分(確認結果内容)
           MOVE    ONSKAKU-CARD-CLASS  TO  SPA-P033-ON-CARD-CLASS
      *2024/02
      *    医療扶助は保険なし
           IF      ONSKAKU-CARD-CLASS  =   "A1"
               CONTINUE
           ELSE
      *****
      *
           EVALUATE    ONSKAKU-CARD-CLASS
               WHEN    "05"
                   MOVE    "【被保険者資格証明書】"
                                           TO  SPA-P033-ON-SIKAKUYUKO
           END-EVALUATE
      *
      *    保険者番号
           MOVE    ONSKAKU-RES-HKNJANUM    TO  SPA-P033-ON-HKNJANUM
           MOVE    ONSKAKU-RES-HKNJANUM    TO  WRK-HKNJANUM
           MOVE    SPACE                   TO  WRK-PTHKN-HKNNUM
           MOVE    SPACE                   TO  WRK-HKNNUM
           PERFORM 300111-HKNJAINF-HEN-SEC
           MOVE    WRK-HKNJANAME       TO  SPA-P033-ON-HKNJANAME
      *2023/10
      *    保険者マスタ登録なし
           IF     (FLG-HKNJAINF        =   1     )
            AND   (WRK-HKNNUM          =   SPACE )
               IF     (ONSKAKU-RES-HKNJANUM(7:2)   =   SPACE )
                 AND  (ONSKAKU-RES-HKNJANUM(6:1)   NOT =   SPACE)
      *            6桁は国保
                   MOVE    "060"           TO  WRK-HKNNUM
               ELSE
      *            頭2桁を法別番号とする
                   PERFORM 300112-HKNNUM-HBTNUM-SEC
               END-IF
               IF      WRK-HKNNUM          =   "060"
                                           OR  "067"
                                           OR  "039"
      *            国保、退職国保、後期高齢者のみ対象
                   CONTINUE
               ELSE
                   MOVE    SPACE           TO  WRK-HKNNUM
                   MOVE    SPACE           TO  WRK-HKNNUMMEI
               END-IF
           END-IF
      *
      *    被保険者資格証明書
           IF      ONSKAKU-CARD-CLASS  =   "05"
               MOVE    SPACE               TO  WRK-HKNNUM-CK
      *        国保を「068 特別療養」
               IF      WRK-HKNNUM          =   "060"
                   MOVE    "068"           TO  WRK-HKNNUM-CK
               END-IF
      *        退職国保を「069 退職特別療養費」
               IF      WRK-HKNNUM          =   "067"
                   MOVE    "069"           TO  WRK-HKNNUM-CK
               END-IF
      *        後期高齢者を「040 後期特療費」
               IF      WRK-HKNNUM          =   "039"
                   MOVE    "040"           TO  WRK-HKNNUM-CK
               END-IF
               IF      WRK-HKNNUM-CK   NOT =   SPACE
                   MOVE    WRK-HKNNUM      TO  SPA-P033-ON-HKNNUM-BK
                   MOVE    WRK-HKNNUM-CK   TO  WRK-HKNNUM
      *            保険情報
                   MOVE    SPACE           TO  WRK-HKNNUMMEI
                   PERFORM 300111-HKNNUM-HEN-SEC
               END-IF
           END-IF
      *
           MOVE    WRK-HKNNUMMEI       TO  SPA-P033-ON-HKNNUMMEI
           MOVE    WRK-HKNNUM          TO  SPA-P033-ON-HKNNUM
      *    記号番号
           MOVE    ONSKAKU-RES-KIGO    TO  SPA-P033-ON-KIGO
           MOVE    ONSKAKU-RES-NUM     TO  SPA-P033-ON-NUM 
           MOVE    ONSKAKU-RES-EDABAN  TO  SPA-P033-ON-EDABAN
      *    本人家族区分
           MOVE    ONSKAKU-RES-HONKZKKBN   TO  SPA-P033-NAI-HONKZKKBN
      *
           EVALUATE    ONSKAKU-RES-HONKZKKBN
               WHEN    "1"
                   MOVE    "本人"          TO  SPA-P033-ON-HONKZKKBN
               WHEN    "2"
                   MOVE    "家族"          TO  SPA-P033-ON-HONKZKKBN
      *R03.5
               WHEN    OTHER
      *            後期高齢者で空白の時、本人
                   IF     (ONSKAKU-FTNRATE     =   ZERO 
                                              OR  SPACE  )
                       CONTINUE
                   ELSE
                       MOVE    "本人"      TO  SPA-P033-ON-HONKZKKBN
                       MOVE    "1"         TO  SPA-P033-NAI-HONKZKKBN
                   END-IF
      ******             
           END-EVALUATE
      *    被保険者氏名(世帯主氏名)
           MOVE    ONSKAKU-RES-HIHKNJANAME TO  SPA-P033-ON-HIHKNJANAME
      *****被保険者証交付年月日
      **** MOVE    ONSKAKU-SKKGETYMD   TO  WRK-SYMD
      *R03.7 
      *    被保険者証有効開始年月日（資格取得日）
           MOVE    ONSKAKU-TEKSTYMD    TO  WRK-SYMD
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-P033-ON-SKKGETYMD
      *
      *    被保険者証有効終了年月日
           MOVE    ONSKAKU-TEKEDYMD    TO  WRK-SYMD
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-P033-ON-TEKEDYMD
      *2023/11
           MOVE    ONSKAKU-TEKSTYMD    TO  SPA-P033-ON-SKKGETYMD-X
      *2024/08
      *    被保険者証有効開始年月日
           MOVE    ONSKAKU-TEKSTYMD    TO  WRK-SYMD
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-P033-ON-TEKSTYMD
           MOVE    ONSKAKU-TEKSTYMD    TO  SPA-P033-ON-TEKSTYMD-X
      *R05.4
           MOVE    ONSKAKU-TEKEDYMD    TO  SPA-P033-ON-TEKEDYMD-X
      *    補助区分（高齢者の負担割合）
           IF      ONSKAKU-FTNRATE         =   SPACE
                                           OR   ZERO
               MOVE    ZERO                TO  SPA-P033-ON-RATE
           ELSE
               MOVE    ONSKAKU-FTNRATE     TO  WRK-FTNRATE-9
               MOVE    WRK-FTNRATE-9       TO  SPA-P033-ON-RATE
           END-IF
      *    高齢受給者証があれば優先（前期高齢者）
           IF      ONSKAKU-ELDER-SKKGETYMD     NOT =   SPACE
               MOVE    "1"                     TO  SPA-P033-ON-ZENKI
      *        高齢受給者証交付年月日
      *******  MOVE    ONSKAKU-ELDER-SKKGETYMD TO  WRK-SYMD
      *R03.7
      *        高齢受給者証有効開始年月日（資格取得日）
      *R05.11
      *        被保険者資格証明書は、被保険者証有効開始年月日とする
               IF      ONSKAKU-CARD-CLASS  =   "05"
                   CONTINUE
               ELSE
                   MOVE    ONSKAKU-ELDER-TEKSTYMD
                                           TO  SPA-P033-ON-SKKGETYMD-X
                   MOVE    ONSKAKU-ELDER-TEKSTYMD  TO  WRK-SYMD
                   PERFORM 8001-SEIWA-YMDEDIT-SEC
                   MOVE    WRK-HENYMD      TO  SPA-P033-ON-SKKGETYMD
      *2024/08     被保険者証有効開始年月日
                   MOVE    ONSKAKU-ELDER-TEKSTYMD
                                           TO  SPA-P033-ON-TEKSTYMD-X
                   MOVE    ONSKAKU-ELDER-TEKSTYMD  TO  WRK-SYMD
                   PERFORM 8001-SEIWA-YMDEDIT-SEC
                   MOVE    WRK-HENYMD      TO  SPA-P033-ON-TEKSTYMD
               END-IF
      *
      *        高齢受給者証有効終了年月日
               MOVE    ONSKAKU-ELDER-TEKEDYMD  TO  WRK-SYMD
               PERFORM 8001-SEIWA-YMDEDIT-SEC
               MOVE    WRK-HENYMD          TO  SPA-P033-ON-TEKEDYMD
      *R05.4
               MOVE    ONSKAKU-ELDER-TEKEDYMD
                                           TO  SPA-P033-ON-TEKEDYMD-X
      *        高齢受給者一部負担金割合
               IF      ONSKAKU-ELDER-FTNRATE   =   SPACE
                                               OR   ZERO
                   MOVE    ZERO                TO  SPA-P033-ON-RATE
               ELSE
                   MOVE    ONSKAKU-ELDER-FTNRATE   TO  WRK-FTNRATE-9
                   MOVE    WRK-FTNRATE-9       TO  SPA-P033-ON-RATE
               END-IF
           END-IF
      *2024/08
      *    資格取得日　資格取得年月日があれば優先
           IF      ONSKAKU-JOIN-SKKGETYMD  =   SPACE
                                           OR  ZERO
               CONTINUE
           ELSE
               MOVE    ONSKAKU-JOIN-SKKGETYMD  TO  WRK-SYMD
               PERFORM 8001-SEIWA-YMDEDIT-SEC
               MOVE    WRK-HENYMD          TO  SPA-P033-ON-SKKGETYMD
               MOVE    ONSKAKU-JOIN-SKKGETYMD
                                       TO  SPA-P033-ON-SKKGETYMD-X
           END-IF
      *R05.4
           IF     (SPA-P033-ON-TEKEDYMD-X  =   SPACE )
               MOVE    "99999999"          TO  SPA-P033-ON-TEKEDYMD-X
               MOVE    "99999999"          TO  SPA-P033-ON-TEKEDYMD
           END-IF
      *2024/08
      *    有効期間
           MOVE    SPACE               TO  SPA-P033-ON-KIKAN
           IF      SPA-P033-ON-TEKSTYMD    NOT =   SPACE
               MOVE    SPACE                   TO  WRK-KIKAN
               MOVE    SPA-P033-ON-TEKSTYMD    TO  WRK-STYMD
               MOVE    SPA-P033-ON-TEKEDYMD    TO  WRK-EDYMD
               MOVE    "〜"                    TO  WRK-K1
               MOVE    WRK-KIKAN               TO  SPA-P033-ON-KIKAN
           END-IF
      *
      *    高齢者補助区分の設定
           MOVE    SPACE               TO  SPA-P033-ON-HOJOKBNNAME
           IF      SPA-P033-ON-RATE    NOT =   ZERO
      *        010 を　１割と表示
               MOVE    SPA-P033-ON-RATE    TO  WRK-FTNRATE-9
               COMPUTE WRK-Z9              =   WRK-FTNRATE-9
                                           /   10
               MOVE    WRK-Z9              TO  WRK-FTNRATE-X
               MOVE    "割"                TO  WRK-FTNRATE-M
               MOVE    WRK-FTNRATE-G       TO
                                           SPA-P033-ON-HOJOKBNNAME
           END-IF
      ****
      *2024/02
      *    医療扶助保険なし
           END-IF
      *
      *    氏名
           MOVE    ONSKAKU-RES-NAME        TO  SPA-P033-ON-NAME
           MOVE    ONSKAKU-RES-KANANAME    TO  SPA-P033-ON-KANANAME
      *    性別
           EVALUATE    ONSKAKU-RES-SEX1
               WHEN    "1"
                   MOVE    "男"            TO  SPA-P033-ON-SEX
               WHEN    "2"
                   MOVE    "女"            TO  SPA-P033-ON-SEX
           END-EVALUATE
           MOVE    ONSKAKU-RES-SEX1    TO  SPA-P033-ON-SEX-K
      *    生年月日
           MOVE    ONSKAKU-RES-BIRTHDAY    TO  WRK-SYMD
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-P033-ON-BIRTHDAY
      *    年齢計算
           PERFORM 300111-NENREI-SET-SEC
      *    郵便番号
           MOVE    ONSKAKU-RES-POST    TO  WRK-RES-POST
           MOVE    WRK-RES-POST1       TO  SPA-P033-ON-POST1
           MOVE    WRK-RES-POST2       TO  SPA-P033-ON-POST2
      *
           MOVE    ONSKAKU-RES-ADRS    TO  SPA-P033-ON-ADRS
      *    患者番号
           IF     (ONSKAKU-PTID    NOT =   ZERO )
               INITIALIZE                      ORCSPTIDAREA
               MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
               MOVE    ONSKAKU-PTID        TO  SPTID-PTID
               CALL    "ORCSPTID"          USING
                                           ORCSPTIDAREA
                                           SPA-AREA
               IF      SPTID-RC            =   ZERO
                   MOVE    SPTID-PTNUM     TO  SPA-P033-ON-PTNUM
               END-IF
      *
           END-IF
      *
      *R03.9
      *    限度額適用認定証などサブプロで編集
           MOVE    SPACE               TO  SPA-P033-SIKKAN-MSG-G
           MOVE    SPACE               TO  SPA-P033-GENDO-MSG
           MOVE    SPACE               TO  SPA-P033-KOHARI
      *    公費・低所得者情報
           INITIALIZE                  SPA-ONSKAKU-KOHTEI-AREA
      *2024/02
      *    医療扶助対応の為、判定廃止
      *     IF     (ONSKAKU-GENDO-DOUIFLG   =   "1" )
      *       OR   (ONSKAKU-SIKKAN-DOUIFLG  =   "1" )
      *      顔認証（公費照会）
      *       OR   (SPA-P033-SYORIKBN   NOT =   "2" )
               INITIALIZE                      ORCSONSHI002AREA
               MOVE    "03"                TO  LNK-ONSHI002-SYOKBN
      *        資格確認限度額承認
               MOVE    SPA-P031-GENDO-DOUIFLG
                                           TO  LNK-ONSHI002-DOUIFLG
      *        交付番号の転記
               MOVE    SPA-ONSHI-KOFUTNK   TO  LNK-ONSHI002-KFTNKKBN
               CALL    "ORCSONSHI002"      USING
                                           SPA-AREA
                                           ORCSONSHI002AREA
                                           ONSKAKU-REC
      *        限度額適用認定証
               MOVE    LNK-ONSHI002-GENDO-MSG
                                           TO  SPA-P033-GENDO-MSG
      *        特定疾病療養受療証提供
               PERFORM VARYING    IDX-SK      FROM    1   BY  1
                       UNTIL      IDX-SK      >   3
                   IF      LNK-ONSHI002-SIKKAN-MSG(IDX-SK)
                                         NOT =   SPACE
                       MOVE    LNK-ONSHI002-SIKKAN-MSG(IDX-SK)
                                             TO  SPA-P033-SIKKAN-MSG
                                                           (IDX-SK)
                   END-IF
               END-PERFORM 
      *2024/01
      *        医療扶助（限度額などはなし）
      *2024/01
               IF      ONSKAKU-CARD-CLASS  =   "A1"
                   MOVE    SPACE           TO  SPA-P033-SIKKAN-MSG-G
                   MOVE    LNK-ONSHI002-FUJYO-MSG(1)
                                           TO  SPA-P033-GENDO-MSG
                   IF      LNK-ONSHI002-FUJYO-MSG(2)   NOT =   SPACE
                       MOVE    LNK-ONSHI002-FUJYO-MSG(2)
                                           TO  SPA-P033-SIKKAN-MSG(1)
                   END-IF
                   IF      LNK-ONSHI002-FUJYO-MSG(3)   NOT =   SPACE
                       MOVE    LNK-ONSHI002-FUJYO-MSG(3)
                                           TO  SPA-P033-SIKKAN-MSG(2)
                   END-IF
               END-IF
      *        公費・低所得者あり
               MOVE    LNK-ONSHI002-KOH-ARI  TO  SPA-P033-KOHARI
      *2024/02
               MOVE    LNK-ONSHI002-FUJ-ARI  TO  SPA-P033-FUJARI
               PERFORM 300113-ONSHI-KOHHEN-SEC
      **** END-IF
      *
      *2024/02
           IF      ONSKAKU-CARD-CLASS  =   "A1"
               MOVE    "医療扶助"          TO  SPA-P033-LABEL01
               MOVE    SPACE               TO  SPA-P033-LABEL02
           ELSE
               MOVE    "限度額認定証"      TO  SPA-P033-LABEL01
               MOVE    "特定疾受給証"      TO  SPA-P033-LABEL02
           END-IF
      ***** 
      *    限度額適用認定証
      *     IF     (ONSKAKU-GENDO-DOUIFLG   =   "1" )
      *         IF     (ONSKAKU-GENDO-SHOKBN    =   SPACE
      **                                         OR  ZERO  )
      **             CONTINUE
      *         ELSE
      *             PERFORM 300113-GENDO-HEN-SEC
      *         END-IF
      *     END-IF
      *
      *    特定疾病療養受療証提供
      *     IF     (ONSKAKU-SIKKAN-DOUIFLG  =   "1" )
      *         PERFORM 300114-SIKKAN-HEN-SEC
      ***** END-IF
      *R03.7
      *    資格有効性
           MOVE    ONSKAKU-SIKAKU-YUKO     TO  SPA-P033-ON-SIKAKU-YUKO
           MOVE    SPACE               TO  SPA-P033-MIDASHI2
           EVALUATE    ONSKAKU-SIKAKU-YUKO
                WHEN    "1"
      ********      MOVE    "「有効」"      TO  SPA-P033-MIDASHI2
                    MOVE    SPACE           TO  SPA-P033-MIDASHI2
               WHEN    "2"
                   STRING  "「無効」"
                           "　窓口で確認して下さい。"
                                           DELIMITED BY SIZE
                                           INTO  SPA-P033-MIDASHI2
                   END-STRING
               WHEN    "3"
                   STRING  "「無効（新しい資格あり）」"
                           "　窓口で確認して下さい。"
                                           DELIMITED BY SIZE
                                           INTO  SPA-P033-MIDASHI2
                   END-STRING
               WHEN    "4"
                   STRING  "「該当資格なし」"
                           "　窓口で確認して下さい。"
                                           DELIMITED BY SIZE
                                           INTO  SPA-P033-MIDASHI2
                   END-STRING
               WHEN    "5"
                   MOVE    "「複数該当」"  TO  SPA-P033-MIDASHI2
            END-EVALUATE
            .
       30011-ONSHIKAKU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    限度額適用認定証編集処理
      *****************************************************************
       300113-ONSHI-KOHHEN-SEC     SECTION.
      *
           IF      SPA-P033-KOHARI     =   "3"
      *2024/02
                                       OR  "4"
              MOVE    "【公費照会の情報があります。】"
                                          TO  SPA-P033-KOHSYOMSG
      *2024/02
               IF      SPA-P033-KOHARI     =   "4"
                   MOVE    "【医療扶助の情報があります。】"
                                          TO  SPA-P033-KOHSYOMSG
                   IF      ONSKAKU-CARD-CLASS  =   "A1"
                       MOVE    SPACE           TO  SPA-P033-KOHSYOMSG
                   END-IF
               ELSE
                   IF      SPA-P033-FUJARI    =   1
                   MOVE    "【医療扶助・公費照会の情報があります。】"
                                          TO  SPA-P033-KOHSYOMSG
                   END-IF
               END-IF
           ELSE
               MOVE    SPACE               TO  SPA-P033-KOHSYOMSG
               MOVE    ONSKAKU-REC         TO  HZN-ONSKAKU-REC
      *        対象外の有無
               INITIALIZE                      ONSKAKU-REC
               MOVE    SPA-HOSPNUM         TO  ONSKAKU-HOSPNUM
               MOVE    SPA-P031-SEL-UUID 
                                          TO  ONSKAKU-KOUHI-UUID
      *
      **       kouhi-uuidでのみ検索へ
               MOVE    ONSKAKU-REC         TO  MCPDATA-REC
               MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
               MOVE    "key13"             TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "key13"             TO  MCP-PATHNAME
                   PERFORM 900-ONSKAKU-READ-SEC
               ELSE
                   MOVE    1               TO  FLG-ONSKAKU
               END-IF
               MOVE    ZERO               TO  FLG-CHKKBN
               PERFORM UNTIL       (FLG-ONSKAKU   =   1  )
                             OR    (FLG-CHKKBN    =   1  )
      *            処理結果区分
                   IF     (ONSKAKU-RESULT-KBN  =   "1"  )
      *              公費照会のみ対象
                    AND   (ONSKAKU-SHOKAI-KBN  =   "8"
                                               OR  "9"  )
                       MOVE    1               TO  FLG-CHKKBN
                   END-IF
                   MOVE    "key13"             TO  MCP-PATHNAME
                   PERFORM 900-ONSKAKU-READ-SEC
               END-PERFORM
               MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
               MOVE    "key13"             TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
               MOVE    HZN-ONSKAKU-REC     TO  ONSKAKU-REC
               IF      FLG-CHKKBN          =   1
                  STRING  "【公費照会の情報があります。】"
                          "（対象公費なし）"
                                     DELIMITED BY  SIZE
                                           INTO  SPA-P033-KOHSYOMSG
                  END-STRING
               END-IF
           END-IF
      *
      *    公費
           PERFORM VARYING  IDX-OSK        FROM     1   BY  1
                   UNTIL     ( IDX-OSK      >  LNK-ONSHI002-KOHMAX)
                         OR  ( IDX-OSK      >  7    )
               ADD     1               TO  SPA-ONSKAKU-KOHMAX
               MOVE    LNK-ONSHI002-KOHNUM(IDX-OSK)
                                       TO  SPA-ONSKAKU-KOHNUM (IDX-OSK)
               MOVE    LNK-ONSHI002-JKYSNUM (IDX-OSK)
                                       TO  SPA-ONSKAKU-JKYSNUM
                                                            (IDX-OSK)
               MOVE    LNK-ONSHI002-TEKSTYMD  (IDX-OSK)
                                       TO  SPA-ONSKAKU-TEKSTYMD
                                                            (IDX-OSK)
               MOVE    LNK-ONSHI002-TEKEDYMD  (IDX-OSK)
                                       TO  SPA-ONSKAKU-TEKEDYMD
                                                            (IDX-OSK)
      *        負担者番号（地方公費）
               MOVE    LNK-ONSHI002-FTNJANUM (IDX-OSK)
                                        TO  SPA-ONSKAKU-FTNJANUM
                                                            (IDX-OSK)
      *        地方公費
               MOVE    LNK-ONSHI002-CHIKOU (IDX-OSK)
                                        TO  SPA-ONSKAKU-CHIKOU
                                                            (IDX-OSK)
      *        特定疾病から
               MOVE    LNK-ONSHI002-SIPKBN (IDX-OSK)
                                        TO  SPA-ONSKAKU-SIPKBN
                                                           (IDX-OSK)
      *        公費照会のTBL-UUID
               MOVE    LNK-ONSHI002-KOHUUID (IDX-OSK)
                                        TO  SPA-ONSKAKU-KOHUUID
                                                           (IDX-OSK)
      *2024/02
      *       医療扶助　交付番号編集
              PERFORM VARYING  IDX-F       FROM    1   BY  1
                      UNTIL   (IDX-F       >   10   )
                           OR (LNK-ONSHI002-KOFUNUM (IDX-OSK IDX-F)
                                          =   SPACE)
                   MOVE    LNK-ONSHI002-KOFUNUM  (IDX-OSK IDX-F)
                                       TO  SPA-ONSKAKU-KOFUNUM
                                                      (IDX-OSK IDX-F)
                   MOVE    LNK-ONSHI002-NYUGAIKBN  (IDX-OSK IDX-F)
                                       TO  SPA-ONSKAKU-NYUGAIKBN
                                                      (IDX-OSK IDX-F)
                   MOVE    LNK-ONSHI002-SRYYM  (IDX-OSK IDX-F)
                                       TO  SPA-ONSKAKU-SRYYM
                                                      (IDX-OSK IDX-F)
               END-PERFORM
      ****
           END-PERFORM
      *    低所得１
           IF      LNK-ONSHI002-STK1-TEKSTYMD NOT =   SPACE
               MOVE    LNK-ONSHI002-STK1-TEKSTYMD
                                           TO  SPA-ONSKAKU-TNKSTYMD
               MOVE    LNK-ONSHI002-STK1-TEKEDYMD
                                           TO  SPA-ONSKAKU-TNKEDYMD
               MOVE    LNK-ONSHI002-STK1-RRIFUK
                                           TO  SPA-ONSKAKU-FUKUSHI
               MOVE    LNK-ONSHI002-STK1-HKNTEKKBN
                                           TO  SPA-ONSKAKU-TNKTEKKBN
               MOVE    LNK-ONSHI002-STK1-KYOUKAI
                                           TO  SPA-ONSKAKU-TNKKYOUKAI
           END-IF
      *
      *    低所得２
           IF      LNK-ONSHI002-STK2-TEKSTYMD NOT =   SPACE
               MOVE    LNK-ONSHI002-STK2-TEKSTYMD
                                           TO  SPA-ONSKAKU-TSYSTYMD
               MOVE    LNK-ONSHI002-STK2-TEKEDYMD
                                           TO  SPA-ONSKAKU-TSYEDYMD
               MOVE    LNK-ONSHI002-STK2-SKJGNSTYMD
                                           TO  SPA-ONSKAKU-TSYSKJGNSTYMD
               MOVE    LNK-ONSHI002-STK2-HKNTEKKBN
                                           TO  SPA-ONSKAKU-TSYTEKKBN
               MOVE    LNK-ONSHI002-STK2-KYOUKAI
                                           TO  SPA-ONSKAKU-TSYKYOUKAI
           END-IF
           .
       300113-ONSHI-KOHHEN-EXT.
           EXIT.
      *
      **以下未使用
      *****************************************************************
      *    限度額適用認定証編集処理
      *****************************************************************
       300113-GENDO-HEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-P033-GENDO-MSG
           MOVE    SPACE               TO  WRK-GENDO-SHO
           MOVE    SPACE               TO  WRK-GENDO-TEK
           EVALUATE    ONSKAKU-GENDO-SHOKBN
               WHEN    "01"
                   MOVE    "限度額適用区分認定証"
                                           TO  WRK-GENDO-SHO
               WHEN    "02"
                   MOVE    "限度額適用・減額認定証"
                                           TO  WRK-GENDO-SHO
               WHEN    "03"
                   MOVE    "標準負担額減額認定証"
                                           TO  WRK-GENDO-SHO
           END-EVALUATE
      *    限度額適用認定証適用区分
           EVALUATE    ONSKAKU-GENDO-TEKKBN
               WHEN    "A01"
                   MOVE    "ア"            TO  WRK-GENDO-TEK
               WHEN    "A02"
                   MOVE    "イ"            TO  WRK-GENDO-TEK
               WHEN    "A03"
                   MOVE    "ウ"            TO  WRK-GENDO-TEK
               WHEN    "A04"
                   MOVE    "エ"            TO  WRK-GENDO-TEK
               WHEN    "A05"
                   MOVE    "オ"            TO  WRK-GENDO-TEK
               WHEN    "A06"
                   MOVE    "オ（境）"      TO  WRK-GENDO-TEK
               WHEN    "B01"
                   MOVE    "現役並み３"    TO  WRK-GENDO-TEK
               WHEN    "B02"
                   MOVE    "現役並み２"    TO  WRK-GENDO-TEK
               WHEN    "B03"
                   MOVE    "現役並み１"    TO  WRK-GENDO-TEK
               WHEN    "B04"
                   MOVE    "一般"          TO  WRK-GENDO-TEK
               WHEN    "B05"
                   MOVE    "低所得２"      TO  WRK-GENDO-TEK
               WHEN    "B06"
                   MOVE    "低所得１"      TO  WRK-GENDO-TEK
               WHEN    "B07"
                   MOVE    "低所得１（老福）"  TO  WRK-GENDO-TEK
               WHEN    "B08"
                   MOVE    "低所得１（境）"    TO  WRK-GENDO-TEK
           END-EVALUATE
           MOVE    ONSKAKU-GENDO-TEKSTYMD  TO  WRK-SYMD
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  WRK-GENDO-STYMD
           MOVE    ONSKAKU-GENDO-TEKEDYMD  TO  WRK-SYMD
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  WRK-GENDO-EDYMD
      *    限度額適用認定証長期入院該当年月日
           MOVE    ONSKAKU-GENDO-SKJGNSTYMD    TO  WRK-SYMD
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  WRK-GENDO-SKJGNSTYMD
      *
           MOVE    SPACE               TO  SPA-P033-GENDO-MSG
           STRING  ONSKAKU-GENDO-SHOKBN    DELIMITED  BY  SPACE
                   "："                    DELIMITED  BY  SPACE
                   WRK-GENDO-SHO           DELIMITED  BY  SPACE
                   "　"                    DELIMITED  BY  SPACE
                   ONSKAKU-GENDO-TEKKBN    DELIMITED  BY  SPACE
                   "："                    DELIMITED  BY  SPACE
                   WRK-GENDO-TEK           DELIMITED  BY  SPACE
                   "　"
                   WRK-GENDO-STYMD         DELIMITED  BY  SIZE
                   "〜"
                   WRK-GENDO-EDYMD         DELIMITED  BY  SIZE
                   "　長期入院該当年月日："
                   WRK-GENDO-SKJGNSTYMD    DELIMITED  BY  SIZE
                                           INTO  SPA-P033-GENDO-MSG
           END-STRING
      *
           .
       300113-GENDO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    特定疾病療養受療証提供編集処理
      *****************************************************************
       300114-SIKKAN-HEN-SEC              SECTION.
      *
           MOVE    SPACE                  TO  SPA-P033-SIKKAN-MSG-G
           MOVE    ZERO                   TO  IDX-SK2
           PERFORM VARYING    IDX-SK      FROM    1   BY  1
                   UNTIL      IDX-SK      >   3
              IF      ONSKAKU-SIKKAN-SIPKBN(IDX-SK)   NOT =   SPACE
                   PERFORM 3001141-SIKKAN-HENSYU-SEC
              END-IF
           END-PERFORM
           .
       300114-SIKKAN-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    特定疾病療養受療証提供編集処理
      *****************************************************************
       3001141-SIKKAN-HENSYU-SEC          SECTION.
      *
           ADD     1               TO  IDX-SK2
           MOVE    SPACE           TO  WRK-SIKKAN
           MOVE    SPACE           TO  WRK-SIKKAN-FTNMONEY
           EVALUATE    ONSKAKU-SIKKAN-SIPKBN(IDX-SK)
               WHEN    "1"
                   MOVE    "人工透析　　　　"     TO  WRK-SIKKAN
               WHEN    "2"
                   MOVE    "血液凝固因子障害"     TO  WRK-SIKKAN
               WHEN    "3"
                   MOVE    "ＨＩＶ感染症　　"     TO  WRK-SIKKAN
           END-EVALUATE
      *
           MOVE    ONSKAKU-SIKKAN-TEKSTYMD (IDX-SK)
                                       TO  WRK-SYMD
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  WRK-SIKKAN-STYMD
           MOVE    ONSKAKU-SIKKAN-TEKEDYMD (IDX-SK)
                                       TO  WRK-SYMD
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  WRK-SIKKAN-EDYMD
      *
           IF      ONSKAKU-SIKKAN-FTNMONEY(IDX-SK) NOT =   SPACE
               INITIALIZE                      ORCSNUMAREA
               MOVE    ONSKAKU-SIKKAN-FTNMONEY(IDX-SK) TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   MOVE    ONSKAKU-SIKKAN-FTNMONEY (IDX-SK)
                                           TO   WRK-SIKKAN-FTNMONEY
               ELSE
                   MOVE    SNUM-OUTNUM     TO  WRK-ZZ9
                   MOVE    WRK-ZZ9         TO  WRK-SIKKAN-FTNMONEY
               END-IF
           END-IF
      *
           STRING  ONSKAKU-SIKKAN-SIPKBN(IDX-SK)
                                           DELIMITED  BY  SPACE
                   "："                    DELIMITED  BY  SPACE
                   WRK-SIKKAN              DELIMITED  BY  SPACE
                   "　"                    DELIMITED  BY  SPACE
                   "　"
                   WRK-SIKKAN-STYMD         DELIMITED  BY  SIZE
                   "〜"
                   WRK-SIKKAN-EDYMD         DELIMITED  BY  SIZE
                   "　自己負担限度額："
                   WRK-SIKKAN-FTNMONEY      DELIMITED  BY  SIZE
                                           INTO  SPA-P033-SIKKAN-MSG
                                                        (IDX-SK2)
           END-STRING
           .
       3001141-SIKKAN-HENSYU-EXT.
           EXIT.
      ***** ここまで未使用
      *
      *****************************************************************
      *    患者番号編集処理
      *****************************************************************
       300111-HKNJAINF-HEN-SEC              SECTION.
      *
      *    保険者番号　検索チェック
           INITIALIZE                    HKNJAINF-REC
           MOVE    SPA-HOSPNUM           TO  HKNJA-HOSPNUM
           MOVE    WRK-HKNJANUM          TO  HKNJA-HKNJANUM
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
           MOVE    HKNJA-HKNJANAME     TO  WRK-HKNJANAME
           MOVE    SPACE               TO  WRK-HKNNUMMEI
      *
           IF     (WRK-PTHKN-HKNNUM    NOT =   SPACE )
      *        患者保険情報の保険の種類を優先する
               MOVE    WRK-PTHKN-HKNNUM    TO  HKNJA-HKNNUM
           END-IF
      *
      *****IF      FLG-HKNJAINF        =   ZERO
           IF      HKNJA-HKNNUM    NOT =   SPACE
               MOVE    HKNJA-HKNNUM        TO  WRK-HKNNUM
      *        保険情報
               PERFORM 300111-HKNNUM-HEN-SEC
           END-IF
           .
       300111-HKNJAINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険情報編集処理
      *****************************************************************
       300111-HKNNUM-HEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    WRK-HKNNUM          TO  HKN-HKNNUM
           MOVE    SPA-SYSYMD          TO  HKN-TEKSTYMD
           MOVE    SPA-SYSYMD          TO  HKN-TEKEDYMD
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 921-HKNNUM-READ-SEC
           ELSE
               INITIALIZE              HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           PERFORM
                  UNTIL   FLG-HKNNUM      =   1
      *        支払区分のないものは対象外とする
               IF      HKN-PAYKBN        =   SPACE
                   CONTINUE
               ELSE
                   MOVE    1                 TO  FLG-HKNNUM
                   MOVE    HKN-TANSEIDONAME  TO  WRK-HKNNUMMEI
               END-IF
      *
               IF      FLG-HKNNUM          =   ZERO
                   MOVE    "tbl_hknnum"        TO  MCP-TABLE
                   MOVE    "key5"              TO  MCP-PATHNAME
                   PERFORM 921-HKNNUM-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       300111-HKNNUM-HEN-EXT.
           EXIT.
      ****************************************************************
      *    法別番号から保険種類編集処理
      ****************************************************************
       300112-HKNNUM-HBTNUM-SEC     SECTION.
      *
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    ONSKAKU-RES-HKNJANUM (1:2)
                                       TO  HKN-HBTNUM
           MOVE    SPA-SYSYMD          TO  HKN-TEKSTYMD
           MOVE    SPA-SYSYMD          TO  HKN-TEKEDYMD
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
               IF     (HKN-HKNKOHSBTKBN    =   "1"   )
                AND   (HKN-PAYKBN      NOT =   SPACE )
                   MOVE    HKN-HKNNUM          TO  WRK-HKNNUM
                   MOVE    HKN-TANSEIDONAME    TO  WRK-HKNNUMMEI
                   MOVE    1                   TO  FLG-HKNNUM
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
       300112-HKNNUM-HBTNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    年齢計算処理
      *****************************************************************
       300111-NENREI-SET-SEC             SECTION.
      *
           INITIALIZE                  WRK-NENREI-AREA
      *
           MOVE    ONSKAKU-RES-BIRTHDAY    TO  WRK-YMDS1
           MOVE    SPA-SYSYMD          TO  WRK-YMDS2
           COMPUTE WRK-NENREI-YY       =   WRK-YYS2
                                       -   WRK-YYS1
           IF      WRK-YMDS1(5:4)      >   WRK-YMDS2(5:4)
               COMPUTE WRK-NENREI-YY   =   WRK-NENREI-YY
                                           -   1
               COMPUTE WRK-NENREI-MM   =  (WRK-MMS2  +  12 )
                                           -   WRK-MMS1
           ELSE
               COMPUTE WRK-NENREI-MM   =   WRK-MMS2
                                           -   WRK-MMS1
           END-IF
           IF      WRK-DDS1            >   WRK-DDS2
               COMPUTE WRK-NENREI-MM   =   WRK-NENREI-MM
                                           -   1
           END-IF
      *
      *    画面表示用
           MOVE    SPACE               TO  WRK-NENREI-G
           IF      WRK-NENREI-YY       =   ZERO
               MOVE    WRK-NENREI-MM       TO  WRK-NENREI-ZN
               MOVE    "ヶ月"              TO  WRK-NENREI-MEIN
           ELSE
               MOVE    WRK-NENREI-YY       TO  WRK-NENREI-Z
               MOVE    "才"                TO  WRK-NENREI-MEI
           END-IF
           MOVE    WRK-NENREI-G        TO  SPA-P033-ON-NENREI
      *
           .
       300111-NENREI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理中フラグ設定処理
      *****************************************************************
       30012-ONSHIKAKU-MODESET-SEC        SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    SPACE               TO  ONSKAKU-LOCK-KBN
           MOVE    SPA-OPID            TO  ONSKAKU-OPID
           MOVE    SMCNDATE-YMD        TO  ONSKAKU-UPYMD
           MOVE    SMCNDATE-HMS        TO  ONSKAKU-UPHMS
      *
           MOVE    ONSKAKU-REC         TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY "P033  ONSKAKU KEY UPD ERR:"  MCP-RC
                       ",KEY:" ONSKAKU-KEY
           END-IF
           .
       30012-ONSHIKAKU-MODESET-EXT.
           EXIT.
      *
      *****************************************************************
      *    一覧表編集処理
      *****************************************************************
       3002-PTINF-LIST-SEC              SECTION.
      *
           MOVE    ZERO                TO  SPA-P033-MAX
           INITIALIZE                      SPA-P033-LIST-G
           MOVE    ZERO                TO  SPA-P033-CHK-PTID
           MOVE    ZERO                TO  SPA-P033-ON-PTID
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    ONSKAKU-RES-BIRTHDAY    TO  PTINF-BIRTHDAY
           MOVE    ONSKAKU-RES-SEX1        TO  PTINF-SEX
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
      ***  MOVE    "key7"              TO  MCP-PATHNAME
           MOVE    "key25"             TO  MCP-PATHNAME
      *
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key25"             TO  MCP-PATHNAME
               PERFORM 900-PTINF-READ-SEC
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  FLG-CHK
           PERFORM   UNTIL  ( FLG-PTINF    =   1  )
                      OR    ( IDX          >=  99 )
      *2023/11
      *        テスト患者対象外
               IF     (PTINF-TSTPTNUMKBN   =   "1" )
                  AND (ONSKAKU-PTID   NOT =   PTINF-PTID )
                   CONTINUE
               ELSE
                  PERFORM 30021-PTINF-HEN-SEC
               END-IF
      *
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key25"             TO  MCP-PATHNAME
               PERFORM 900-PTINF-READ-SEC
           END-PERFORM
           MOVE    IDX                 TO  SPA-P033-MAX
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key25"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    オンライン資格確認データで患者が決定されている
           IF     (ONSKAKU-PTID    NOT =   ZERO    )
      ****   AND  (SPA-P033-MAX        =   50      )
             AND  (SPA-P033-MAX        =   99      )
             AND  (SPA-P033-SELNUM     =   ZERO    )
      *
               INITIALIZE                      PTINF-REC
               MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
               MOVE    ONSKAKU-PTID        TO  PTINF-PTID
               PERFORM 9001-PTINF-KEYREAD-SEC
      *        生年月日が一致すること 
               IF     (PTINF-BIRTHDAY      =   ONSKAKU-RES-BIRTHDAY )
      *            先頭に編集する
                   MOVE    ZERO            TO  IDX
                   PERFORM 30021-PTINF-HEN-SEC
               END-IF
           END-IF
           .
       3002-PTINF-LIST-EXT.
           EXIT.
      *****************************************************************
      *    患者情報編集処理
      *****************************************************************
       30021-PTINF-HEN-SEC              SECTION.
      *
           ADD     1                   TO  IDX
      **** MOVE    IDX                 TO  WRK-ZZZ
           MOVE    IDX                 TO  SPA-P033-TNUM (IDX)
           MOVE    PTINF-PTID          TO  SPA-P033-TPTID (IDX)
      *    患者番号
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    PTINF-PTID          TO  SPTID-PTID
           CALL    "ORCSPTID"          USING
                                           ORCSPTIDAREA
                                           SPA-AREA
           IF      SPTID-RC            =   ZERO
               MOVE    SPTID-PTNUM     TO  SPA-P033-TPTNUM (IDX)
           END-IF
      *
           MOVE    PTINF-NAME          TO  SPA-P033-TNAME (IDX)
      *    性別
           EVALUATE    PTINF-SEX
               WHEN    "1"
                   MOVE    "男"            TO  SPA-P033-TSEX (IDX)
               WHEN    "2"
                   MOVE    "女"            TO  SPA-P033-TSEX (IDX)
           END-EVALUATE
      *    生年月日
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-P033-TBIRTHDAY (IDX)
      *
           MOVE    PTINF-HOME-POST     TO  SPA-P033-TPOST    (IDX)
           STRING  PTINF-HOME-ADRS     DELIMITED  BY  SPACE
                   PTINF-HOME-BANTI    DELIMITED  BY  SPACE
                                       INTO    SPA-P033-TADRS (IDX)
           END-STRING
      **** MOVE    PTINF-HOME-TEL1     TO  SPA-P033-TTEL (IDX)
      *
      *    同一氏名あり
           IF     (ONSKAKU-RES-NAME    =   PTINF-NAME )
              AND (ONSKAKU-RES-KANANAME    =   PTINF-KANANAME)
               MOVE    PTINF-PTID      TO  SPA-P033-CHK-PTID
               MOVE    IDX             TO  WRK-CHK-IDX
           END-IF
      *    オンライン資格確認データで患者が決定されている
           IF     (ONSKAKU-PTID    NOT =   ZERO )  AND
                  (ONSKAKU-PTID        =   PTINF-PTID )
               MOVE    IDX                 TO  SPA-P033-SELNUM
               MOVE    ONSKAKU-PTID        TO  SPA-P033-ON-PTID
           END-IF
           MOVE    ONSKAKU-MOD-FLG     TO   SPA-P033-ON-MOD
           .
       30021-PTINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報編集処理
      *****************************************************************
       3003-PTINF-HENSYU-SEC              SECTION.
      *
           MOVE    SPA-P033-SELNUM     TO  IDX
      *
           INITIALIZE                      SPA-P033-PTINF-G
      *     MOVE    SPA-P033-TPTID(IDX) TO  SPA-P033-PTID
           INITIALIZE                      SPA-P033-HEN-AREA
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-P033-TPTID(IDX)
                                       TO  PTINF-PTID
      *
           PERFORM 9001-PTINF-KEYREAD-SEC
      *
      *
           IF     (SPA-P033-ON-PTID    NOT =  ZERO )
             AND  (SPA-P033-ON-PTID    NOT =  PTINF-PTID)
             AND  (SPA-P033-ON-MOD     NOT =  SPACE )
               MOVE    "0007"              TO  SPA-ERRCD
               MOVE    01                  TO  SPA-P033-CUR
               GO   TO  3003-PTINF-HENSYU-EXT
           END-IF
      *
      *    P032 からは新規患者登録不可
           IF     (SPA-P033-MOTOPG     =   "P032")
             AND  (SPA-P033-ON-PTID    NOT =   SPA-P033-TPTID(IDX))
               MOVE    "0009"              TO  SPA-ERRCD
               MOVE    01                  TO  SPA-P033-CUR
               GO   TO  3003-PTINF-HENSYU-EXT
           END-IF
      *
      *    患者番号
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    PTINF-PTID          TO  SPTID-PTID
           CALL    "ORCSPTID"          USING
                                           ORCSPTIDAREA
                                           SPA-AREA
           IF      SPTID-RC            =   ZERO
               MOVE    SPTID-PTNUM     TO  SPA-P033-PTNUM
           END-IF
      *
           MOVE    PTINF-PTID          TO  SPA-P033-PTID
      *
           MOVE    PTINF-NAME          TO  SPA-P033-NAME
           MOVE    PTINF-KANANAME      TO  SPA-P033-KANANAME
      *    性別
           EVALUATE    PTINF-SEX
               WHEN    "1"
                   MOVE    "男"            TO  SPA-P033-SEX
               WHEN    "2"
                   MOVE    "女"            TO  SPA-P033-SEX
           END-EVALUATE
           MOVE    PTINF-SEX           TO  SPA-P033-SEX-K
      *    生年月日
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-P033-BIRTHDAY
      *
           STRING  PTINF-HOME-ADRS     DELIMITED  BY  SPACE
                   PTINF-HOME-BANTI    DELIMITED  BY  SPACE
                                       INTO    SPA-P033-ADRS
           END-STRING
           MOVE    PTINF-HOME-POST     TO  SPA-P033-POST
           IF      SPA-P033-POST       =   SPA-P033-ON-POST
               MOVE    SPACE           TO  SPA-P033-POST-CHK
           ELSE
               MOVE    "1"             TO  SPA-P033-POST-CHK
           END-IF
      *****MOVE    PTINF-HOME-TEL1     TO  SPA-P033-TEL
      *
      *    保険情報検索
           INITIALIZE                  PTHKNINF-REC
           MOVE    PTINF-HOSPNUM       TO  PTHKN-HOSPNUM
           MOVE    PTINF-PTID          TO  PTHKN-PTID
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
           MOVE    ZERO                TO  FLG-OK
           PERFORM UNTIL   FLG-PTHKNINF    =   1
               MOVE    ZERO            TO  FLG-NO
               IF     (PTHKN-SAKJOKBN      NOT =   SPACE )  OR
                      (PTHKN-HKNNUM(1:1)       =   "9"   )
                   MOVE    1               TO  FLG-NO
               END-IF
      *??
      *        船員等の職務上は対象外
               IF      (PTHKN-HKNNUM       =   "002"
                                           OR  "031"
                                           OR  "032"
                                           OR  "033"
                                           OR  "034")
                  AND  (PTHKN-HONKZKKBN    =   "1"   )
                  AND ((PTHKN-HOJOKBN      =   "1"
                                           OR  "2"
                                           OR  "3"   ) OR
                       (PTHKN-HOJOKBN      >=  "A"  AND
                                           <=  "I"   ))
                   MOVE    1               TO  FLG-NO
      *            システム日で有効であれば表示対象
                   IF     (PTHKN-TEKSTYMD      <=  SPA-SYSYMD )  AND
                          (PTHKN-TEKEDYMD      >=  SPA-SYSYMD )
                       MOVE    1               TO  FLG-KIKAN-OK
                       MOVE    PTHKNINF-REC    TO  HZN-PTHKNINF-REC
                   END-IF
               END-IF
      *
               IF      FLG-NO              =   ZERO
                   MOVE    1                   TO  FLG-OK
                   MOVE    PTHKN-HKNJANUM      TO  SPA-P033-HKNJANUM
                   MOVE    PTHKN-HKNJANUM      TO  WRK-HKNJANUM
                   MOVE    PTHKN-HKNNUM        TO  WRK-PTHKN-HKNNUM
                   PERFORM 300111-HKNJAINF-HEN-SEC
                   MOVE    WRK-HKNJANAME       TO  SPA-P033-HKNJANAME
                   MOVE    WRK-HKNNUMMEI       TO  SPA-P033-HKNNUMMEI
                   MOVE    WRK-HKNNUM          TO  SPA-P033-HKNNUM
      *            記号番号
                   MOVE    PTHKN-KIGO          TO  SPA-P033-KIGO
                   MOVE    PTHKN-NUM           TO  SPA-P033-NUM
                   MOVE    PTHKN-EDABAN        TO  SPA-P033-EDABAN
      *            本人家族区分
                   EVALUATE    PTHKN-HONKZKKBN
                   WHEN    "1"
                       MOVE    "本人"          TO  SPA-P033-HONKZKKBN
                   WHEN    "2"
                       MOVE    "家族"          TO  SPA-P033-HONKZKKBN
                   END-EVALUATE
      *            補助区分
                   PERFORM 30031-HOJOKBN-NAME-HEN-SEC
      *            資格取得日
                   MOVE    SPACE               TO  SPA-P033-SKKGETYMD
                   MOVE    PTHKN-SKKGETYMD     TO  WRK-SYMD
                   PERFORM 8001-SEIWA-YMDEDIT-SEC
                   MOVE    WRK-HENYMD          TO  SPA-P033-SKKGETYMD
      *            被保険者名
                   MOVE    PTHKN-HIHKNJANAME   TO  SPA-P033-HIHKNJANAME
      *            有効期間
                   MOVE    SPACE               TO  WRK-KIKAN
                   MOVE    PTHKN-TEKSTYMD      TO  WRK-SYMD
                   PERFORM 8001-SEIWA-YMDEDIT-SEC
                   MOVE    WRK-HENYMD          TO  WRK-STYMD
                   MOVE    PTHKN-TEKEDYMD      TO  WRK-SYMD
                   PERFORM 8001-SEIWA-YMDEDIT-SEC
                   MOVE    WRK-HENYMD          TO  WRK-EDYMD
                   MOVE    "〜"                TO  WRK-K1
                   MOVE    WRK-KIKAN           TO  SPA-P033-YUKOKIKAN
      *            システム日で有効
                   IF     (PTHKN-TEKSTYMD      <=  SPA-SYSYMD )  AND
                          (PTHKN-TEKEDYMD      >=  SPA-SYSYMD )
                       MOVE    1               TO  SPA-P033-KIKAN-OK
                       MOVE    1                   TO  FLG-OK
                   ELSE
                       MOVE    ZERO            TO  SPA-P033-KIKAN-OK
                   END-IF
      *R03.8       確認年月日
                   MOVE    PTHKN-KAKUNINYMD    TO  SPA-P033-KAKUNINYMD
      *R04.9
                   MOVE    PTHKN-TEKSTYMD      TO  SPA-P033-TEKSTYMD
      *
                   MOVE    ZERO                TO  SPA-P033-HKNID
                   IF     (PTHKN-HKNJANUM      =   SPA-P033-ON-HKNJANUM)
                      AND (PTHKN-HONKZKKBN     =
                                               SPA-P033-NAI-HONKZKKBN)
                       MOVE    PTHKN-HKNID         TO  SPA-P033-HKNID
                   END-IF
      *R05.4
      *            システム日期間外で未来日開始日）
                   IF     (SPA-P033-KIKAN-OK   =   ZERO )
                      IF      (PTHKN-TEKSTYMD      >   SPA-SYSYMD)
                          OR ((SPA-P033-ON-TEKEDYMD-X
                                               NOT =   SPACE )  AND
                              (PTHKN-TEKSTYMD      >
                                             SPA-P033-ON-TEKEDYMD-X ))
      *                次の履歴を検索する
                           MOVE    ZERO            TO  FLG-OK
                           MOVE    PTHKN-TEKSTYMD
                                           TO  SPA-P033-MAE-TEKSTYMD
                       END-IF
                   END-IF
               END-IF
      *
               IF      FLG-OK              =   1
                   MOVE    1                   TO  FLG-PTHKNINF
               ELSE
                   MOVE    "tbl_pthkninf"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 910-PTHKNINF-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    変更箇所
           IF      SPA-P033-NAME       NOT =   SPA-P033-ON-NAME
               MOVE    "1"                 TO  SPA-P033-HEN-NAME
           END-IF
      *R03.6
      *    必須項目以外は空白は変更なし 
           IF     (SPA-P033-ON-KANANAME    NOT =   SPACE    )
             AND  (SPA-P033-KANANAME   NOT =   SPA-P033-ON-KANANAME )
               MOVE    "1"                 TO  SPA-P033-HEN-KANANAME
           END-IF
           IF     (SPA-P033-ON-POST    NOT =   SPACE )
             AND  (SPA-P033-POST       NOT =   SPA-P033-ON-POST )
               MOVE    "1"                 TO  SPA-P033-HEN-POST
           END-IF
           IF     (SPA-P033-ON-ADRS    NOT =   SPACE  )
             AND  (SPA-P033-ADRS       NOT =   SPA-P033-ON-ADRS )
               MOVE    "1"                 TO  SPA-P033-HEN-ADRS
           END-IF
      *2023/01
      *    性別
           IF     (SPA-P033-ON-SEX-K       =   "1"
                                           OR  "2"    )
             AND  (SPA-P033-SEX-K      NOT =   SPA-P033-ON-SEX-K )
               MOVE    "1"                 TO  SPA-P033-HEN-SEX
           END-IF
      *2023/10
           IF     (SPA-P033-ON-HKNNUM  NOT =   SPACE )
             AND  (SPA-P033-HKNNUM     NOT =   SPA-P033-ON-HKNNUM )
               MOVE    "1"                 TO  SPA-P033-HEN-HKNNUM
           END-IF
      *
           IF      SPA-P033-HKNJANUM   NOT =   SPA-P033-ON-HKNJANUM
               MOVE    "1"                 TO  SPA-P033-HEN-HKNJANUM
           END-IF
           IF      SPA-P033-KIGO       NOT =   SPA-P033-ON-KIGO
               MOVE    "1"                 TO  SPA-P033-HEN-KIGO
           END-IF
           IF      SPA-P033-NUM        NOT =   SPA-P033-ON-NUM
               MOVE    "1"                 TO  SPA-P033-HEN-NUM
           END-IF
           IF      SPA-P033-EDABAN     NOT =   SPA-P033-ON-EDABAN
               MOVE    "1"                 TO  SPA-P033-HEN-EDABAN
           END-IF
           IF      SPA-P033-HONKZKKBN  NOT =   SPA-P033-ON-HONKZKKBN
               MOVE    "1"                 TO  SPA-P033-HEN-HONKZKKBN
           END-IF
           IF      SPA-P033-SKKGETYMD  NOT =   SPA-P033-ON-SKKGETYMD
               MOVE    "1"                 TO  SPA-P033-HEN-SKKGETYMD
           END-IF
           IF     (SPA-P033-ON-HIHKNJANAME NOT =   SPACE  )
             AND  (SPA-P033-HIHKNJANAME
                                       NOT =   SPA-P033-ON-HIHKNJANAME)
               MOVE    "1"             TO  SPA-P033-HEN-HIHKNJANAME
           END-IF
      *    補助区分（高齢者のみ）
           IF     (SPA-P033-ON-HOJOKBNNAME NOT =   SPACE  )
            AND   (SPA-P033-FTN-RATE   NOT =   SPA-P033-ON-RATE )
               MOVE    "1"             TO  SPA-P033-HEN-HOJOKBN
           END-IF
      *    保険変更可
           MOVE    ZERO                TO  FLG-HKNHEN
           MOVE    ZERO                TO  FLG-HKNADD
      **   IF     (SPA-P033-ON-HKNNUM      =   SPA-P033-HKNNUM  )
           IF     (SPA-P033-ON-HKNJANUM    =   SPA-P033-HKNJANUM)
             AND  (SPA-P033-ON-HONKZKKBN   =   SPA-P033-HONKZKKBN)
               IF    ( ( SPA-P033-ON-RATE       =   ZERO  ) OR
                      ((SPA-P033-ON-RATE   NOT =   ZERO  ) AND
                       (SPA-P033-ON-RATE       =   SPA-P033-FTN-RATE)))
                 AND   (SPA-P033-HEN-HKNNUM    =   SPACE )
      *            保険種類、保険者番号、本人家族、負担割合一致
                   MOVE    1                   TO  FLG-HKNHEN
               END-IF
      *2023/10
      *        被保険者証照会で負担割・保険の種類不一致
               IF     (SPA-P033-SYORIKBN   =   "2" )
                   IF     ((SPA-P033-ON-RATE   NOT =   ZERO ) AND
                           (SPA-P033-ON-RATE   NOT =
                                               SPA-P033-FTN-RATE ))
                     OR   ((SPA-P033-ON-HKNNUM NOT =   SPACE) AND
                           (SPA-P033-ON-HKNNUM NOT =   SPA-P033-HKNNUM))
      *                負担割・保険の種類が不一致は追加可能
                       MOVE    "1"             TO  SPA-P033-HEN-HKNADD
                   END-IF
               END-IF
           END-IF
           .
       3003-PTINF-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報編集処理
      *****************************************************************
       30032-PTHKNINF-HEN-SEC             SECTION.
      *
           .
       30032-PTHKNINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    補助区分名称編集
      *****************************************************************
       30031-HOJOKBN-NAME-HEN-SEC             SECTION.
      *
           INITIALIZE                  ORAPI012SUB1AREA
           MOVE    WRK-HKNNUM          TO  API012SUB1-HKNNUM
           MOVE    PTHKN-HOJOKBN       TO  API012SUB1-HOJOKBN
           MOVE    SPA-SYSYMD          TO  API012SUB1-SYSYMD
           CALL    "ORAPI012SUB1"          USING
                                           ORAPI012SUB1AREA
                                           SPA-AREA
      *    負担割合
           MOVE    API012SUB1-RATE     TO  SPA-P033-FTN-RATE
           MOVE    API012SUB1-HOJOKBN-NAME
                                       TO  SPA-P033-HOJOKBN
      *
           .
       30031-HOJOKBN-NAME-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    確認画面より処理
      *****************************************************************
       3001-PID1-SYORI-SEC              SECTION.
      *
           EVALUATE    SPA-PIDCD
               WHEN    "0101"
               WHEN    "0102"
                   MOVE    SPACE               TO  SPA-PIDCD
                   IF      SPA-PID1-FLG        =   "OK"
      *            新規患者登録
                       INITIALIZE              SPA-ONSKAKU-AREA
                       MOVE    "1"             TO  SPA-ONSKAKU-SYORI
                       PERFORM 490-KAKUTEI-SYORI-SEC
                   ELSE
                       MOVE    10              TO  SPA-P033-CUR
                       IF      SPA-P033-ON-CARD-CLASS  =   "A1"
      *                    医療扶助は保険変更なしへ
                           MOVE    07              TO  SPA-P033-CUR
                       END-IF
                   END-IF
               WHEN    "0103"
      *            保険変更
                   MOVE    SPACE               TO  SPA-PIDCD
                   IF      SPA-PID1-FLG        =   "OK"
                       INITIALIZE                  SPA-ONSKAKU-AREA
                       MOVE    "4"             TO  SPA-ONSKAKU-SYORI
                       MOVE    SPA-P033-PTID   TO  SPA-ONSKAKU-PTID
                       MOVE    SPA-P033-PTNUM  TO  SPA-ONSKAKU-PTNUM
                       MOVE    SPA-P033-HKNID  TO  SPA-ONSKAKU-HKNID
                       PERFORM 490-KAKUTEI-SYORI-SEC
                   ELSE
                       MOVE    12                      TO  SPA-P033-CUR
                   END-IF
               WHEN    "0104"
      *            保険追加
                   MOVE    SPACE               TO  SPA-PIDCD
                   IF      SPA-PID1-FLG        =   "OK"
                       INITIALIZE                  SPA-ONSKAKU-AREA
                       MOVE    "3"             TO  SPA-ONSKAKU-SYORI
                       MOVE    SPA-P033-PTID   TO  SPA-ONSKAKU-PTID
                       MOVE    SPA-P033-PTNUM  TO  SPA-ONSKAKU-PTNUM
                       PERFORM 490-KAKUTEI-SYORI-SEC
                   ELSE
                       MOVE    10                      TO  SPA-P033-CUR
                   END-IF
      *R03/8
      *
               WHEN    "1101"
               WHEN    "1102"
      *            確認日付更新処理
                   MOVE    SPACE               TO  SPA-PIDCD 
                   IF      SPA-PID1-FLG        =   "OK"
                       PERFORM 4111-HKNKAKUNIN-SYORI-SEC
                   ELSE
      *                P031 からは排他制御解除
                       IF     (SPA-P033-MOTOPG     =   "P031")
                           PERFORM 990-LOCK-OUT-SEC
                       END-IF
                       MOVE    10              TO  SPA-P033-CUR
                   END-IF
      *R03.11
               WHEN    "1103"
               WHEN    "1105"
               WHEN    "1109"
      *            公費照会あり、保険更新へ
                   MOVE    SPACE               TO  SPA-PIDCD 
                   IF      SPA-PID1-FLG        =   "OK"
      *                保険変更へ
                       INITIALIZE                  SPA-ONSKAKU-AREA
                       MOVE    "2"             TO  SPA-ONSKAKU-SYORI
                       MOVE    SPA-P033-PTID   TO  SPA-ONSKAKU-PTID
                       MOVE    SPA-P033-PTNUM  TO  SPA-ONSKAKU-PTNUM
                       MOVE    SPA-P033-HKNID  TO  SPA-ONSKAKU-HKNID
      *                MOVE    1               TO  FLG-KOHCHK
                       PERFORM 490-KAKUTEI-SYORI-SEC
      *
                       IF     (SPA-ERRCD           =   SPACE )
                       AND    (SPA-PIDCD       NOT =   SPACE )
                           PERFORM 520-JIDSET-SEC
                       END-IF
                   ELSE
                       MOVE    10              TO  SPA-P033-CUR
                   END-IF
      *R03.12   (PCHK へ変更)
      *         WHEN    "1104"
      *            公費照会あり、照会画面へ
      *             MOVE    SPACE               TO  SPA-PIDCD 
      *             IF      SPA-PID1-FLG        =   "OK"
      *                保険変更へ
      *                 PERFORM 405-P034-SYORI-SEC
      *             ELSE
      *                 PERFORM 210-BACK
      ****          END-IF
      *2023/11
               WHEN    "1106"
               WHEN    "1107"
               WHEN    "1108"
                   MOVE    SPACE               TO  SPA-PIDCD 
                   IF      SPA-PID1-FLG        =   "OK"
      *                公費のみ変更
                       INITIALIZE              SPA-ONSKAKU-AREA
                       MOVE    "6"             TO  SPA-ONSKAKU-SYORI
                       MOVE    SPA-P033-PTID   TO  SPA-ONSKAKU-PTID
                       MOVE    SPA-P033-PTNUM  TO  SPA-ONSKAKU-PTNUM
                       PERFORM 490-KAKUTEI-SYORI-SEC
                       IF     (SPA-ERRCD           =   SPACE )
                       AND    (SPA-PIDCD       NOT =   SPACE )
                           PERFORM 520-JIDSET-SEC
                       END-IF
                   ELSE
                       MOVE    10              TO  SPA-P033-CUR
                   END-IF
      *
           END-EVALUATE
      *
      *
           .
       3001-PID1-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  P033
           INITIALIZE                      P033
           MOVE    SPA-P033-ON-HKNJANUM    TO  P033-ON-HKNJANUM
           MOVE    SPA-P033-ON-HKNJANAME   TO  P033-ON-HKNJANAME
           MOVE    SPA-P033-ON-HKNNUMMEI   TO  P033-ON-HKNNUMMEI
           MOVE    SPA-P033-ON-KIGO        TO  P033-ON-KIGO
           MOVE    SPA-P033-ON-NUM         TO  P033-ON-NUM
           MOVE    SPA-P033-ON-EDABAN      TO  P033-ON-EDABAN
           MOVE    SPA-P033-ON-HONKZKKBN   TO  P033-ON-HONKZKKBN
           MOVE    SPA-P033-ON-HOJOKBNNAME TO  P033-ON-HOJOKBN
           MOVE    SPA-P033-ON-SKKGETYMD   TO  P033-ON-SKKGETYMD
           MOVE    SPA-P033-ON-HIHKNJANAME TO  P033-ON-HIHKNJANAME
      *2024/08
      *****MOVE    SPA-P033-ON-TEKEDYMD    TO  P033-ON-TEKEDYMD
           MOVE    SPA-P033-ON-KIKAN       TO  P033-ON-KIKAN
           MOVE    SPA-P033-ON-SIKAKUYUKO  TO  P033-ON-SIKAKUYUKO
      *2023/10
           IF       SPA-P033-ON-CARD-CLASS  =   "05"
               MOVE    "red"           TO   P033-ON-SIKAKUYUKO-STYLE
           ELSE
               MOVE    "black"         TO   P033-ON-SIKAKUYUKO-STYLE
           END-IF
      *
           MOVE    SPA-P033-ON-BIRTHDAY    TO  P033-ON-BIRTHDAY
           MOVE    SPA-P033-ON-SEX         TO  P033-ON-SEX
           MOVE    SPA-P033-ON-NENREI      TO  P033-ON-NENREI
           MOVE    SPA-P033-ON-KANANAME    TO  P033-ON-KANANAME
           MOVE    SPA-P033-ON-NAME        TO  P033-ON-NAME
           MOVE    SPA-P033-ON-POST        TO  P033-ON-POST
           MOVE    SPA-P033-ON-ADRS        TO  P033-ON-ADRS
      *
           MOVE    SPA-P033-MIDASHI        TO  P033-ON-MIDASHI
      *R03.7
           MOVE    SPA-P033-MIDASHI2       TO  P033-ON-MIDASHI2
           EVALUATE    SPA-P033-ON-SIKAKU-YUKO
               WHEN    "1"
                   MOVE    "black"         TO   P033-ON-MIDASHI2-STYLE
               WHEN    "2"
               WHEN    "3"
               WHEN    "4"            
                   MOVE    "red"           TO   P033-ON-MIDASHI2-STYLE
               WHEN    "5"
                   MOVE    "blue"          TO   P033-ON-MIDASHI2-STYLE
               WHEN    OTHER
                   MOVE    "black"         TO   P033-ON-MIDASHI2-STYLE
           END-EVALUATE
      *
           MOVE    SPA-P033-GENDO-MSG      TO  P033-GENDO-MSG
           MOVE    SPA-P033-SIKKAN-MSG(1)  TO  P033-SIKKAN-MSG1
           MOVE    SPA-P033-SIKKAN-MSG(2)  TO  P033-SIKKAN-MSG2
           MOVE    SPA-P033-SIKKAN-MSG(3)  TO  P033-SIKKAN-MSG3
      *
           MOVE    SPA-P033-KOHSYOMSG      TO  P033-ON-KOHSYOMSG
           IF      SPA-P033-KOHSYOMSG  NOT =   SPACE
               MOVE    "blue"              TO  P033-ON-KOHSYOMSG-STYLE
           END-IF
      *R05.01
           MOVE    SPA-P033-LABEL01    TO  P033-LABEL01
           MOVE    SPA-P033-LABEL02    TO  P033-LABEL02
      *
      *
           PERFORM   VARYING    IDX   FROM    1   BY  1
                     UNTIL      IDX      >    SPA-P033-MAX
               MOVE    SPA-P033-TNUM   (IDX)  TO  WRK-ZZZ
               MOVE    WRK-ZZZ                TO  P033-TNUM  (IDX)
               MOVE    SPA-P033-TPTNUM (IDX)  TO  P033-TPTNUM(IDX)
               MOVE    SPA-P033-TNAME  (IDX)  TO  P033-TNAME (IDX)
               MOVE    SPA-P033-TSEX   (IDX)  TO  P033-TSEX   (IDX)
               MOVE    SPA-P033-TBIRTHDAY(IDX)
                                              TO  P033-TBIRTHDAY (IDX)
               MOVE    SPA-P033-TADRS  (IDX)  TO  P033-TADRS (IDX)
               MOVE    SPA-P033-TPOST  (IDX)  TO  P033-TPOST (IDX)
               IF      IDX                 =   SPA-P033-SELNUM
                   MOVE    "T"                 TO  P033-SELECT
                                                               (IDX)
                   MOVE    IDX                 TO  P033-ROW
               ELSE
                   MOVE    "F"                 TO  P033-SELECT
                                                               (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-P033-MAX        TO  P033-COUNT
      *
           MOVE    SPA-P033-SELNUM     TO  P033-SELNUM
      *
           MOVE    SPA-P033-PTNUM      TO  P033-PTNUM
           MOVE    SPA-P033-POST       TO  P033-POST
           MOVE    SPA-P033-ADRS       TO  P033-ADRS
     ***   MOVE    SPA-P033-TEL        TO  P033-TEL
           MOVE    SPA-P033-BIRTHDAY   TO  P033-BIRTHDAY
           MOVE    SPA-P033-SEX        TO  P033-SEX
           MOVE    SPA-P033-NAME       TO  P033-NAME
           MOVE    SPA-P033-KANANAME   TO  P033-KANANAME
      *
           MOVE    "F"                 TO  P033-CHK-NAME-ISACTIVE
           MOVE    "F"                 TO  P033-CHK-KANANAME-ISACTIVE
           MOVE    "F"                 TO  P033-CHK-ADRS-ISACTIVE
           IF      SPA-P033-HEN-NAME   =   SPACE
               MOVE    "black"         TO  P033-NAME-STYLE
               MOVE    "black"         TO  P033-ON-NAME-STYLE
           ELSE
               MOVE    "red"           TO  P033-NAME-STYLE
               MOVE    "blue"          TO  P033-ON-NAME-STYLE
      *        変更あり
               IF      SPA-P033-CHK-NAME   =   "1"
                   MOVE    "T"              TO  P033-CHK-NAME-ISACTIVE
               END-IF
           END-IF
           IF      SPA-P033-HEN-KANANAME   =   SPACE
               MOVE    "black"         TO  P033-KANANAME-STYLE
               MOVE    "black"         TO  P033-ON-KANANAME-STYLE
           ELSE
               MOVE    "red"           TO  P033-KANANAME-STYLE
               MOVE    "blue"          TO  P033-ON-KANANAME-STYLE
      *        変更あり
               IF      SPA-P033-CHK-KANANAME =   "1"
                   MOVE    "T"          TO  P033-CHK-KANANAME-ISACTIVE
               END-IF
           END-IF
      *2023/01  性別
           IF      SPA-P033-HEN-SEX    =   SPACE
               MOVE    "black"         TO  P033-SEX-STYLE
               MOVE    "black"         TO  P033-ON-SEX-STYLE
           ELSE
               MOVE    "red"           TO  P033-SEX-STYLE
               MOVE    "blue"          TO  P033-ON-SEX-STYLE
      *        変更あり
               IF      SPA-P033-CHK-SEX    =   "1"
                   MOVE    "T"          TO  P033-CHK-SEX-ISACTIVE
               END-IF
           END-IF
      *
           IF      SPA-P033-HEN-ADRS   =   SPACE
               MOVE    "black"         TO  P033-ADRS-STYLE
               MOVE    "black"         TO  P033-ON-ADRS-STYLE
           ELSE
               MOVE    "red"           TO  P033-ADRS-STYLE
               MOVE    "blue"          TO  P033-ON-ADRS-STYLE
      *        変更あり
               IF      SPA-P033-CHK-ADRS   =   "1"
                   MOVE    "T"             TO  P033-CHK-ADRS-ISACTIVE
               END-IF
           END-IF
           IF      SPA-P033-HEN-POST   =   SPACE
               MOVE    "black"         TO  P033-POST-STYLE
               MOVE    "black"         TO  P033-ON-POST-STYLE
           ELSE
               MOVE    "red"           TO  P033-POST-STYLE
               MOVE    "blue"          TO  P033-ON-POST-STYLE
           END-IF
      *
      *
           MOVE    SPA-P033-HKNJANUM   TO  P033-HKNJANUM
           MOVE    SPA-P033-HKNJANAME  TO  P033-HKNJANAME
           MOVE    SPA-P033-HKNNUMMEI  TO  P033-HKNNUMMEI
           MOVE    SPA-P033-KIGO       TO  P033-KIGO
           MOVE    SPA-P033-NUM        TO  P033-NUM
           MOVE    SPA-P033-EDABAN     TO  P033-EDABAN
           MOVE    SPA-P033-HONKZKKBN  TO  P033-HONKZKKBN
           MOVE    SPA-P033-YUKOKIKAN  TO  P033-KIKAN
           MOVE    SPA-P033-HOJOKBN    TO  P033-HOJOKBN
           MOVE    SPA-P033-SKKGETYMD  TO  P033-SKKGETYMD
           MOVE    SPA-P033-HIHKNJANAME    TO  P033-HIHKNJANAME
      *
           IF      SPA-P033-HEN-HKNJANUM   =   SPACE
               MOVE    "black"         TO  P033-HKNJANUM-STYLE
               MOVE    "black"         TO  P033-ON-HKNJANUM-STYLE
           ELSE
               MOVE    "red"           TO  P033-HKNJANUM-STYLE
               MOVE    "blue"          TO  P033-ON-HKNJANUM-STYLE
           END-IF
           IF      SPA-P033-HEN-KIGO       =   SPACE
               MOVE    "black"         TO  P033-KIGO-STYLE
               MOVE    "black"         TO  P033-ON-KIGO-STYLE
           ELSE
               MOVE    "red"           TO  P033-KIGO-STYLE
               MOVE    "blue"          TO  P033-ON-KIGO-STYLE
           END-IF
           IF      SPA-P033-HEN-NUM        =   SPACE
               MOVE    "black"         TO  P033-NUM-STYLE
               MOVE    "black"         TO  P033-ON-NUM-STYLE
           ELSE
               MOVE    "red"           TO  P033-NUM-STYLE
               MOVE    "blue"          TO  P033-ON-NUM-STYLE
           END-IF
           IF      SPA-P033-HEN-EDABAN     =   SPACE
               MOVE    "black"         TO  P033-EDABAN-STYLE
               MOVE    "black"         TO  P033-ON-EDABAN-STYLE
           ELSE
               MOVE    "red"           TO  P033-EDABAN-STYLE
               MOVE    "blue"          TO  P033-ON-EDABAN-STYLE
           END-IF
           IF      SPA-P033-HEN-HONKZKKBN  =   SPACE
               MOVE    "black"         TO  P033-HONKZKKBN-STYLE
               MOVE    "black"         TO  P033-ON-HONKZKKBN-STYLE
           ELSE
               MOVE    "red"           TO  P033-HONKZKKBN-STYLE
               MOVE    "blue"          TO  P033-ON-HONKZKKBN-STYLE
           END-IF
           IF      SPA-P033-HEN-SKKGETYMD  =   SPACE
               MOVE    "black"         TO  P033-SKKGETYMD-STYLE
               MOVE    "black"         TO  P033-ON-SKKGETYMD-STYLE
           ELSE
               MOVE    "red"           TO  P033-SKKGETYMD-STYLE
               MOVE    "blue"          TO  P033-ON-SKKGETYMD-STYLE
           END-IF
           IF      SPA-P033-HEN-HIHKNJANAME  =   SPACE
               MOVE    "black"         TO  P033-HIHKNJANAME-STYLE
               MOVE    "black"         TO  P033-ON-HIHKNJANAME-STYLE
           ELSE
               MOVE    "red"           TO  P033-HIHKNJANAME-STYLE
               MOVE    "blue"          TO  P033-ON-HIHKNJANAME-STYLE
           END-IF
           IF      SPA-P033-HEN-HOJOKBN  =   SPACE
               MOVE    "black"         TO  P033-HOJOKBN-STYLE
               MOVE    "black"         TO  P033-ON-HOJOKBN-STYLE
           ELSE
               MOVE    "red"           TO  P033-HOJOKBN-STYLE
               MOVE    "blue"          TO  P033-ON-HOJOKBN-STYLE
           END-IF
      *
           IF      SPA-P033-HEN-HKNNUM  =   SPACE
               MOVE    "black"         TO  P033-HKNNUMMEI-STYLE
               MOVE    "black"         TO  P033-ON-HKNNUMMEI-STYLE
           ELSE
               MOVE    "red"           TO  P033-HKNNUMMEI-STYLE
               MOVE    "blue"          TO  P033-ON-HKNNUMMEI-STYLE
           END-IF
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           EVALUATE    SPA-P033-CUR
               WHEN    01
                   MOVE    "SELNUM"    TO  MCP-WIDGET
               WHEN    07
                   MOVE    "B07"       TO  MCP-WIDGET
               WHEN    08
                   MOVE    "B08"       TO  MCP-WIDGET
               WHEN    10
                   MOVE    "B10"       TO  MCP-WIDGET
               WHEN    12
                   MOVE    "B12"       TO  MCP-WIDGET
               WHEN    99
                   MOVE    "B01"       TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE    SPACE               TO  SPA-ONSKAKU-SYORI
                   PERFORM 210-BACK
      *    クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 402-CLEAR-SEC
      *    新規患者
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 408-SINKI-SYORI-SEC
      *    患者情報のみ変更
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 409-PTINFUP-SYORI-SEC
      *    保険変更
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 410-HKNUPD-SYORI-SEC
      *    保険追加
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 412-HKNADD-SYORI-SEC
      *R03.8 
      *    保険確認
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 411-HKNKAKUNIN-CHECK-SEC
      *
      *2022/12
      *    公費画像確認
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 406-P037-SYORI-SEC
      *
      *2023/08
      *    公費のみ反映
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 407-KOHNOMI-SYORI-SEC
      *
      *    氏名変更区分
               WHEN    "CLICKED"       ALSO    "CHK_NAME"
                   PERFORM 400-CHK-NAME-SEC
      *    カナ氏名変更区分
               WHEN    "CLICKED"       ALSO    "CHK_KANANAME"
                   PERFORM 400-CHK-KANANAME-SEC
      *2023/01 性別変更区分
              WHEN    "CLICKED"       ALSO    "CHK_SEX"
                  PERFORM 400-CHK-SEX-SEC
      *    住所変更区分
               WHEN    "CLICKED"       ALSO    "CHK_ADRS"
                   PERFORM 400-CHK-ADRS-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    MCP-WIDGET          TO  SPA-MCP-WIDGET
           MOVE    ZERO                TO  SPA-P033-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        リスト選択
               WHEN    ANY             ALSO    "DUALLIST"
                   PERFORM 4001-LIST-SEL-SEC
               WHEN    OTHER
      *            入力チェック処理
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    一覧表選択処理
      *****************************************************************
       4001-LIST-SEL-SEC          SECTION.
      *
      *    行選択
           MOVE    ZERO                TO  SPA-P033-SELNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-P033-MAX
               IF      P033-SELECT (IDX)  =   "T"
                   MOVE    IDX             TO  SPA-P033-SELNUM
               END-IF
           END-PERFORM
      *
           IF      SPA-P033-SELNUM    NOT =   ZERO
               PERFORM 4102-SELNUM-CHK-SEC
           END-IF
      *
           .
       4001-LIST-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           PERFORM 4102-SELNUM-CHK-SEC
           .
      *
       410-INPUT-CHK-EXT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           MOVE    P033-SELNUM        TO  SPA-P033-SELNUM
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号処理
      *****************************************************************
       4102-SELNUM-CHK-SEC                 SECTION.
      *
           IF     (SPA-P033-SELNUM     =   ZERO )  OR
                  (SPA-P033-SELNUM     >   SPA-P033-MAX)
               INITIALIZE                      SPA-P033-PTINF-G
               INITIALIZE                      SPA-P033-HEN-AREA
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-P033-CUR
           ELSE
      *        患者情報編集
               PERFORM 3003-PTINF-HENSYU-SEC
               IF      SPA-ERRCD           =   SPACE
                   IF      FLG-HKNHEN          =   1
      *                保険変更可
                       MOVE    10              TO  SPA-P033-CUR
                   ELSE
                       MOVE    12              TO  SPA-P033-CUR
      *                被保険者証照会は追加不可
                       IF    ( SPA-P033-SYORIKBN   =   "2" )
      *2023/10  追加不可
                        AND  ( SPA-P033-HEN-HKNADD =   SPACE)
                           MOVE    10              TO  SPA-P033-CUR
                       END-IF
                   END-IF
      *2024/01
                   IF      SPA-P033-ON-CARD-CLASS  =   "A1"
      *                医療扶助は保険変更なしへ
                       MOVE    07              TO  SPA-P033-CUR
                   END-IF
               END-IF
           END-IF
           .
       4102-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア処理
      *****************************************************************
       402-CLEAR-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-P033-SELNUM
           MOVE    1                   TO  SPA-P033-CUR
           INITIALIZE                      SPA-P033-PTINF-G
           .
       402-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *     新規患者処理
      *****************************************************************
       408-SINKI-SYORI-SEC             SECTION.
      *
           IF      SPA-P033-SYORIKBN   =   "2"
      *        被保険者証照会は選択不可
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    10                  TO  SPA-P033-CUR
               GO  TO          408-SINKI-SYORI-EXT
           END-IF
      *
           IF     (SPA-P033-ON-PTID    NOT =   ZERO )
             AND  (SPA-P033-ON-MOD     NOT =  SPACE )
               MOVE    "0007"              TO  SPA-ERRCD
               MOVE    10                  TO  SPA-P033-CUR
               GO  TO          408-SINKI-SYORI-EXT
           END-IF
      *    P032 からは新規患者登録不可
           IF     (SPA-P033-MOTOPG     =   "P032")
               MOVE    "0009"              TO  SPA-ERRCD
               MOVE    10                  TO  SPA-P033-CUR
               GO  TO          408-SINKI-SYORI-EXT
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )
               IF     (SPA-P033-ON-PTID    NOT =   ZERO)
                   MOVE    "0101"          TO  SPA-PIDCD
               END-IF
               IF     (SPA-PIDCD            =   SPACE )
                 AND  (SPA-P033-CHK-PTID   NOT =   ZERO)
                   MOVE    "0102"          TO  SPA-PIDCD
               END-IF
           END-IF
           IF     (SPA-ERRCD           =   SPACE ) AND
                  (SPA-PIDCD           =   SPACE )
               INITIALIZE                  SPA-ONSKAKU-AREA
               MOVE    "1"                 TO  SPA-ONSKAKU-SYORI
               PERFORM 490-KAKUTEI-SYORI-SEC
           END-IF
           .
        408-SINKI-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    患者情報のみ変更処理
      *****************************************************************
       409-PTINFUP-SYORI-SEC             SECTION.
      *
      *    IF      SPA-P033-SYORIKBN   =   "2"
      *        被保険者証照会は選択不可
      *        MOVE    "0002"              TO  SPA-ERRCD
      *        MOVE    10                  TO  SPA-P033-CUR
      *        GO  TO          409-PTINFUP-SYORI-EXT
      *    END-IF
      *
           IF     (SPA-P033-SELNUM     =   ZERO )  OR
                  (SPA-P033-SELNUM     >   SPA-P033-MAX)
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-P033-CUR
           ELSE
               MOVE    SPA-P033-SELNUM    TO  IDX
               IF      SPA-P033-PTID      =   ZERO
                   MOVE    "0003"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT = SPACE
               GO  TO          409-PTINFUP-SYORI-EXT
           END-IF
      *    チェックの確認
           IF     (SPA-P033-CHK-NAME       =   "1"  )
              OR  (SPA-P033-CHK-KANANAME   =   "1"  )
              OR  (SPA-P033-CHK-ADRS       =   "1"  )
      *2023/01
              OR  (SPA-P033-CHK-SEX        =   "1"  )
               CONTINUE
           ELSE
               MOVE    "0006"              TO  SPA-ERRCD
           END-IF
           IF     (SPA-ERRCD           =   SPACE ) AND
                  (SPA-PIDCD           =   SPACE )
      *        患者基本情報変更
               INITIALIZE                  SPA-ONSKAKU-AREA
               MOVE    "5"                    TO  SPA-ONSKAKU-SYORI
               MOVE    SPA-P033-PTID          TO  SPA-ONSKAKU-PTID
               MOVE    SPA-P033-PTNUM         TO  SPA-ONSKAKU-PTNUM
               PERFORM 490-KAKUTEI-SYORI-SEC
           END-IF
           .
       409-PTINFUP-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    保険変更処理
      *****************************************************************
       410-HKNUPD-SYORI-SEC             SECTION.
      *
      *2024/01
      *    医療扶助は保険チェックなし
           IF      SPA-P033-ON-CARD-CLASS  =   "A1"
               MOVE    "0021"              TO  SPA-ERRCD
               MOVE    07                  TO  SPA-P033-CUR
               GO  TO  410-HKNUPD-SYORI-EXT
           END-IF
      *
           IF     (SPA-P033-SELNUM     =   ZERO )  OR
                  (SPA-P033-SELNUM     >   SPA-P033-MAX)
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-P033-CUR
           ELSE
               MOVE    SPA-P033-SELNUM    TO  IDX
               IF      SPA-P033-PTID      =   ZERO
                   MOVE    "0003"          TO  SPA-ERRCD
               END-IF
      *        処理済みは同じ患者
               IF     (SPA-P033-ON-PTID    NOT =  ZERO )
                 AND  (SPA-P033-ON-PTID    NOT =  SPA-P033-PTID)
                 AND  (SPA-P033-ON-MOD     NOT =  SPACE )
                 AND  (SPA-ERRCD               =  SPACE )
                   MOVE    "0007"              TO  SPA-ERRCD
                   MOVE    01                  TO  SPA-P033-CUR
               END-IF
      *
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        保険が違う場合、変更なし
               IF     (SPA-P033-PTID      NOT =   ZERO )
      *保険の種類は判定なし(2023/10 判定追加)
                  AND ((SPA-P033-ON-HKNNUM     =   SPACE ) OR
                      ((SPA-P033-ON-HKNNUM NOT =   SPACE ) AND
                       (SPA-P033-ON-HKNNUM     =   SPA-P033-HKNNUM)))
                  AND (SPA-P033-ON-HKNJANUM    =   SPA-P033-HKNJANUM)
                  AND (SPA-P033-ON-HONKZKKBN   =   SPA-P033-HONKZKKBN)
                  AND ( (SPA-P033-ON-RATE      =   ZERO  ) OR
                       ((SPA-P033-ON-RATE  NOT =   ZERO  ) AND
                        (SPA-P033-ON-RATE      =   SPA-P033-FTN-RATE)))
                   IF     (SPA-P033-KIKAN-OK      =   ZERO )
                       MOVE    "0103"          TO  SPA-PIDCD
                   END-IF
      *R05.4
      *            次（未来）の保険がある時、オン資格終了日とのチェック
                   IF     (SPA-P033-MAE-TEKSTYMD   NOT =   SPACE)
                     AND  (SPA-P033-MAE-TEKSTYMD   >   SPA-SYSYMD )
                     AND  (SPA-P033-MAE-TEKSTYMD   <=
                                               SPA-P033-ON-TEKEDYMD-X )
                     AND  (SPA-ERRCD               =   SPACE )
      *                終了日の更新で、次の保険と重複する
                       MOVE    "0017"              TO  SPA-ERRCD
      *                戻るへ
                       MOVE    99                  TO  SPA-P033-CUR
                   END-IF
               ELSE
                   IF      SPA-ERRCD           =   SPACE
                       MOVE    "0004"              TO  SPA-ERRCD
                       IF      SPA-P033-SYORIKBN   =   "2"
                       AND    (SPA-P033-HEN-HKNADD =   SPACE )
                           MOVE    "0008"          TO  SPA-ERRCD
      *                    戻るへ
                           MOVE    99              TO  SPA-P033-CUR
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF     (SPA-ERRCD           =   SPACE ) AND
                  (SPA-PIDCD           =   SPACE )
      *        保険変更
               INITIALIZE                  SPA-ONSKAKU-AREA
               MOVE    "2"                 TO  SPA-ONSKAKU-SYORI
               MOVE    SPA-P033-PTID          TO  SPA-ONSKAKU-PTID
               MOVE    SPA-P033-PTNUM         TO  SPA-ONSKAKU-PTNUM
               MOVE    SPA-P033-HKNID         TO  SPA-ONSKAKU-HKNID
               PERFORM 490-KAKUTEI-SYORI-SEC
           END-IF
           .
       410-HKNUPD-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    保険追加処理
      *****************************************************************
       412-HKNADD-SYORI-SEC             SECTION.
      *
      *2024/01
      *    医療扶助は保険なし
           IF      SPA-P033-ON-CARD-CLASS  =   "A1"
               MOVE    "0021"              TO  SPA-ERRCD
               MOVE    07                  TO  SPA-P033-CUR
               GO  TO  412-HKNADD-SYORI-EXT
           END-IF
      **
      *     IF      SPA-P033-SYORIKBN   =   "2"
      *        被保険者証照会は選択不可
      *         MOVE    "0002"              TO  SPA-ERRCD
      *         MOVE    10                  TO  SPA-P033-CUR
      *         GO  TO          412-HKNADD-SYORI-EXT
      *     END-IF
      *
           IF     (SPA-P033-SELNUM     =   ZERO )  OR
                  (SPA-P033-SELNUM     >   SPA-P033-MAX)
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-P033-CUR
           ELSE
               MOVE    SPA-P033-SELNUM    TO  IDX
               IF      SPA-P033-PTID      =   ZERO
                   MOVE    "0003"          TO  SPA-ERRCD
               END-IF
           END-IF
      *    処理済みは同じ患者
           IF     (SPA-P033-ON-PTID    NOT =  ZERO )
             AND  (SPA-P033-ON-PTID    NOT =  SPA-P033-PTID)
             AND  (SPA-P033-ON-MOD     NOT =  SPACE )
             AND  (SPA-ERRCD               =  SPACE )
               MOVE    "0007"              TO  SPA-ERRCD
               MOVE    01                  TO  SPA-P033-CUR
           END-IF
      *??????
      *2022/09
      *    被保険者証照会
           IF     (SPA-P033-SYORIKBN   =   "2" )
             AND  (SPA-ERRCD           =   SPACE)
      *        負担割合の変更は可とする
               IF     (SPA-P033-PTID       NOT =   ZERO )
                  AND (SPA-P033-ON-HKNJANUM    =   SPA-P033-HKNJANUM)
                  AND (SPA-P033-ON-KIGO        =   SPA-P033-KIGO    )
                  AND (SPA-P033-ON-NUM         =   SPA-P033-NUM     )
                  AND (SPA-P033-ON-HONKZKKBN   =   SPA-P033-HONKZKKBN)
      *2023/10
      *           負担割合のみ異なる
      *           保険の種類が異なる
                  IF  ((SPA-P033-ON-RATE   NOT =   ZERO  ) AND
                       (SPA-P033-ON-RATE   NOT =   SPA-P033-FTN-RATE))
                  OR  ((SPA-P033-ON-HKNNUM NOT =   SPACE ) AND
                       (SPA-P033-ON-HKNNUM NOT =   SPA-P033-HKNNUM))
      *            保険の開始日が適応開始より前
                       IF      SPA-P033-TEKSTYMD
      *2024/08                         <   SPA-P033-ON-SKKGETYMD-X
                                       <   SPA-P033-ON-TEKSTYMD-X
                           CONTINUE
                       ELSE
      *            被保険者証照会は選択不可
      ****                 MOVE    "0002"              TO  SPA-ERRCD
                           MOVE    "0019"              TO  SPA-ERRCD
      *                    戻るへ
                           MOVE    99              TO  SPA-P033-CUR
                       END-IF
      *            被保険者証照会は選択不可
                   ELSE
                       MOVE    "0002"              TO  SPA-ERRCD
                       MOVE    10                  TO  SPA-P033-CUR
                   END-IF
               ELSE
                   MOVE    "0002"              TO  SPA-ERRCD
                   MOVE    10                  TO  SPA-P033-CUR
               END-IF
               IF      SPA-ERRCD       NOT =   SPACE
                   GO  TO          412-HKNADD-SYORI-EXT
               END-IF
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE    )
               IF     (SPA-P033-PTID  NOT =   ZERO )
      *????       AND (SPA-P033-ON-HKNNUM      =   SPA-P033-HKNNUM)
                  AND (SPA-P033-ON-HKNJANUM    =   SPA-P033-HKNJANUM)
                  AND (SPA-P033-ON-KIGO        =   SPA-P033-KIGO    )
                  AND (SPA-P033-ON-NUM         =   SPA-P033-NUM     )
                  AND (SPA-P033-ON-HONKZKKBN   =   SPA-P033-HONKZKKBN)
                  AND ( (SPA-P033-ON-RATE      =   ZERO  ) OR
                       ((SPA-P033-ON-RATE  NOT =   ZERO  ) AND
                        (SPA-P033-ON-RATE      =   SPA-P033-FTN-RATE)))
      *2023/10    保険の種類が一致
                  AND  ((SPA-P033-ON-HKNNUM    =   SPACE ) OR
                       ((SPA-P033-ON-HKNNUM NOT =   SPACE ) AND
                        (SPA-P033-ON-HKNNUM     =   SPA-P033-HKNNUM)))
                   IF     (SPA-P033-KIKAN-OK      =   1 )
                       MOVE    "0005"          TO  SPA-ERRCD
                       MOVE    10              TO  SPA-P033-CUR
                   ELSE
                       MOVE    "0104"          TO  SPA-PIDCD
                   END-IF
               END-IF
           END-IF
           IF     (SPA-ERRCD           =   SPACE ) AND
                  (SPA-PIDCD           =   SPACE )
      *        患者追加
               INITIALIZE                      SPA-ONSKAKU-AREA
               MOVE    "3"                    TO  SPA-ONSKAKU-SYORI
               MOVE    SPA-P033-PTID          TO  SPA-ONSKAKU-PTID
               MOVE    SPA-P033-PTNUM         TO  SPA-ONSKAKU-PTNUM
               PERFORM 490-KAKUTEI-SYORI-SEC
           END-IF
           .
      *
       412-HKNADD-SYORI-EXT.
           EXIT.
      *
      *R03.8
      *****************************************************************
      *    保険確認処理
      *****************************************************************
       411-HKNKAKUNIN-CHECK-SEC             SECTION.
      *
      *2024/01
      *    医療扶助は保険なし
           IF      SPA-P033-ON-CARD-CLASS  =   "A1"
               MOVE    "0021"              TO  SPA-ERRCD
               MOVE    07                  TO  SPA-P033-CUR
               GO  TO  411-HKNKAKUNIN-CHECK-EXT
           END-IF
      *
      *    患者の選択があり、保険情報が一致すること
           IF     (SPA-P033-SELNUM     =   ZERO )  OR
                  (SPA-P033-SELNUM     >   SPA-P033-MAX)
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-P033-CUR
           ELSE
               MOVE    SPA-P033-SELNUM     TO  IDX
               IF      SPA-P033-PTID       =   ZERO
                   MOVE    "0003"          TO  SPA-ERRCD
               END-IF
      *        オン資格の患者番号あること
               IF      (SPA-P033-ON-PTID   =   ZERO )
                 AND   (SPA-ERRCD          =   SPACE )
                   MOVE    "0016"              TO  SPA-ERRCD
                   MOVE    10                  TO  SPA-P033-CUR
               END-IF
      *        オン資格の患者と同じであること
               IF      (SPA-P033-ON-PTID   NOT =   SPA-P033-PTID)
                 AND   (SPA-ERRCD              =   SPACE )
                   MOVE    "0015"              TO  SPA-ERRCD
                   MOVE    10                  TO  SPA-P033-CUR
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        保険が違う場合、変更なし
               IF     (SPA-P033-PTID      NOT =   ZERO )
                  AND (SPA-P033-HKNID     NOT =   ZERO )
                  AND (SPA-P033-ON-HKNJANUM    =   SPA-P033-HKNJANUM)
                  AND (SPA-P033-ON-KIGO        =   SPA-P033-KIGO    )
                  AND (SPA-P033-ON-NUM         =   SPA-P033-NUM     )
                  AND (SPA-P033-ON-HONKZKKBN   =   SPA-P033-HONKZKKBN)
                  AND (SPA-P033-ON-EDABAN      =   SPA-P033-EDABAN )
                  AND ( (SPA-P033-ON-RATE      =   ZERO  ) OR
                       ((SPA-P033-ON-RATE  NOT =   ZERO  ) AND
                        (SPA-P033-ON-RATE      =   SPA-P033-FTN-RATE)))
      *            当日が有効期間内であること
                   IF      ( SPA-P033-KIKAN-OK     =   ZERO )
                       MOVE    "0013"              TO  SPA-ERRCD
                       MOVE    10                  TO  SPA-P033-CUR
                   END-IF
      *2023/10
                   IF      (SPA-ERRCD              =   SPACE )
                    AND   ((SPA-P033-ON-HKNNUM NOT =   SPACE ) AND
                           (SPA-P033-ON-HKNNUM NOT =   SPA-P033-HKNNUM))
      *                保険の種類が一致しない時
                       MOVE    "0011"              TO  SPA-ERRCD
                       MOVE    10                  TO  SPA-P033-CUR
                   END-IF
               ELSE
                   IF      SPA-ERRCD           =   SPACE
                       MOVE    "0011"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
      *    限度額認定証あり
           IF     (SPA-P033-KOHARI     =   "1"
                                       OR  "2"   )
             AND  (SPA-ERRCD           =   SPACE )
               MOVE    "1105"          TO  SPA-PIDCD
               GO      TO      411-HKNKAKUNIN-CHECK-EXT
           END-IF
      *    地方公費あり
           IF     (SPA-P033-KOHARI     =   "3"   )
             AND  (SPA-ERRCD           =   SPACE )
               MOVE    "1103"          TO  SPA-PIDCD
               GO      TO      411-HKNKAKUNIN-CHECK-EXT
           END-IF
      *2024/01
      *    医療扶助あり
           IF     (SPA-P033-KOHARI     =   "4"   )
             AND  (SPA-ERRCD           =   SPACE )
               MOVE    "1109"          TO  SPA-PIDCD
               GO      TO      411-HKNKAKUNIN-CHECK-EXT
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )
             AND  (SPA-P033-MOTOPG     =   "P031")
      *        P031からの時、排他制御を行う
               PERFORM 990-LOCK-IN-SEC
               IF      SPA-ERRCD        NOT =   SPACE
      *            排他中
                   MOVE    "0012"           TO  SPA-ERRCD
                   MOVE    1                TO  SPA-P033-CUR
      *            排他解除
                   PERFORM 990-LOCK-OUT-SEC
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-P033-KAKUNINYMD     =   SPA-SYSYMD
                   MOVE    "1102"          TO  SPA-PIDCD
               ELSE
                   MOVE    "1101"          TO  SPA-PIDCD
               END-IF
           END-IF
            .
       411-HKNKAKUNIN-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    保険確認処理
      *****************************************************************
       4111-HKNKAKUNIN-SYORI-SEC             SECTION.
      *
      *
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    保険情報検索
           INITIALIZE                  PTHKNINF-REC
           MOVE    SPA-HOSPNUM         TO  PTHKN-HOSPNUM
           MOVE    SPA-P033-PTID       TO  PTHKN-PTID
           MOVE    SPA-P033-HKNID      TO  PTHKN-HKNID
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
      *
           IF     (FLG-PTHKNINF        =   ZERO  )
            AND   (PTHKN-SAKJOKBN      =   SPACE )
            AND   (SPA-SYSYMD      NOT =   PTHKN-KAKUNINYMD)
      *        前回確認日付
               MOVE    PTHKN-KAKUNINYMD    TO  WRK-MAE-KAKUNINYMD
      *        確認年月日の変更時のみ行う
               MOVE    SPA-SYSYMD          TO  PTHKN-KAKUNINYMD
      *
               MOVE    SMCNDATE-YMD        TO  PTHKN-UPYMD
               MOVE    SMCNDATE-HMS        TO  PTHKN-UPHMS
               MOVE    SPA-OPID            TO  PTHKN-OPID
      *
               MOVE    PTHKNINF-REC        TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO 
                   MOVE    "1011"              TO  SPA-ERRCD
                   DISPLAY "P033 PTHKNINF UPDATE ERR:" MCP-RC
                                               ",KEY:" PTHKN-KEY
               END-IF
               IF     (SPA-ERRCD           =   SPACE )
                  AND (WRK-MAE-KAKUNINYMD  NOT =   SPACE)
      *            確認年月日履歴処理
                   INITIALIZE                      ORCSPTHKNRRKAREA
                   MOVE    SPA-P033-PTID       TO  ORCSHKNRRK-PTID
                   MOVE    SPA-P033-HKNID      TO  ORCSHKNRRK-HKNID
                   MOVE    WRK-MAE-KAKUNINYMD
                                           TO  ORCSHKNRRK-KAKUNINYMD
                   CALL    "ORCSPTHKNRRK"      USING
                                               SPA-AREA
                                               ORCSPTHKNRRKAREA
                   IF      ORCSHKNRRK-RC       =   9
                       DISPLAY "P033 PTHKNRRK UPD ERR:" ORCSHKNRRK-RC
                                             ",HKNID:" ORCSHKNRRK-HKNID
                       MOVE    "1011"              TO  SPA-ERRCD
                   END-IF
               END-IF
      *
           ELSE
      *        確認日付更新なし
               IF     (FLG-PTHKNINF        =   1  )
                  OR  (PTHKN-SAKJOKBN  NOT =   SPACE)
                   MOVE    "0014"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        オン資格　処理済み設定
               PERFORM 4111-ONSKAKU-MODEUPD-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPACE               TO  SPA-P031-SEL-UUID
               IF      SPA-P033-MOTOPG     =   "P032"
      *            患者登録画面へ
                   MOVE    "9"                 TO  SPA-ONSKAKU-SYORI
                   PERFORM 210-BACK
               ELSE
      *            P031へ戻る
      *            排他制御解除
                   PERFORM 990-LOCK-OUT-SEC
      *
                   MOVE    SPACE               TO  SPA-ONSKAKU-SYORI
                   PERFORM 210-BACK
               END-IF
            END-IF
            .
       411-HKNKAKUNIN-SYORI-EXT.
           EXIT.
      *****
      ******
      *2023/11
      *****************************************************************
      *    公費情報のみ反映処理
      *****************************************************************
       407-KOHNOMI-SYORI-SEC             SECTION.
      *
           IF     (SPA-P033-SELNUM     =   ZERO )  OR
                  (SPA-P033-SELNUM     >   SPA-P033-MAX)
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-P033-CUR
           ELSE
               MOVE    SPA-P033-SELNUM    TO  IDX
               IF      SPA-P033-PTID      =   ZERO
                   MOVE    "0003"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
      *    処理済みは同じ患者
           IF     (SPA-P033-ON-PTID    NOT =  ZERO )
             AND  (SPA-P033-ON-PTID    NOT =  SPA-P033-PTID)
             AND  (SPA-P033-ON-MOD     NOT =  SPACE )
             AND  (SPA-ERRCD               =  SPACE )
               MOVE    "0007"              TO  SPA-ERRCD
               MOVE    01                  TO  SPA-P033-CUR
           END-IF
      *
      *    被保険者証照会・P032から
           IF     (SPA-P033-SYORIKBN   =   "2"   )
              OR  (SPA-P033-MOTOPG     =   "P032")
      *        患者が一致すること
               IF    ( SPA-P033-ON-PTID    NOT =   SPA-P033-PTID )
                 AND ( SPA-ERRCD           =   SPACE )
                   MOVE    "0009"              TO  SPA-ERRCD
                   MOVE    10                  TO  SPA-P033-CUR
               END-IF
           END-IF
      *
           IF     (SPA-P033-ON-PTID    NOT =   ZERO  )
              AND (SPA-P033-ON-PTID    NOT =   SPA-P033-PTID )
              AND ( SPA-ERRCD           =   SPACE )
               MOVE    "0018"              TO  SPA-ERRCD
               MOVE    10                  TO  SPA-P033-CUR
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  407-KOHNOMI-SYORI-EXT
           END-IF
      *2024/01
      *    医療扶助は保険チェックなし
           IF      SPA-P033-ON-CARD-CLASS  =   "A1"
               CONTINUE
           ELSE
      *
           IF     (SPA-P033-ON-SIKAKU-YUKO    =   "1"
                                              OR  "5")
      *        本人家族区分、負担割合以外が一致すること
               IF     (SPA-P033-PTID      NOT =   ZERO )
                  AND (SPA-P033-HKNID     NOT =   ZERO )
                  AND (SPA-P033-ON-HKNJANUM    =   SPA-P033-HKNJANUM)
                  AND (SPA-P033-ON-KIGO        =   SPA-P033-KIGO    )
                  AND (SPA-P033-ON-NUM         =   SPA-P033-NUM     )
                  AND (SPA-P033-ON-EDABAN      =   SPA-P033-EDABAN )
                  AND ( (SPA-P033-ON-RATE      =   ZERO  ) OR
                       ((SPA-P033-ON-RATE  NOT =   ZERO  ) AND
                        (SPA-P033-ON-RATE      =   SPA-P033-FTN-RATE)))
                 AND  ( (SPA-P033-ON-HKNNUM    =   SPACE )  OR
                       ((SPA-P033-ON-HKNNUM NOT =   SPACE ) AND
                        (SPA-P033-ON-HKNNUM    =   SPA-P033-HKNNUM)))
                   MOVE    "1108"              TO  SPA-PIDCD
               ELSE
                   MOVE    "1107"              TO  SPA-PIDCD
               END-IF
           ELSE
      *        無効、該当なしの時
               IF     (SPA-P033-ON-PTID    =   ZERO  )
               OR     (SPA-P033-ON-PTID    NOT =   SPA-P033-PTID )
                   MOVE    "1106"          TO  SPA-PIDCD
               END-IF
           END-IF
      *2024/10
           END-IF
      *
      *
           IF     (SPA-ERRCD           =   SPACE ) AND
                  (SPA-PIDCD           =   SPACE )
      *        公費のみ変更
               INITIALIZE                  SPA-ONSKAKU-AREA
               MOVE    "6"                    TO  SPA-ONSKAKU-SYORI
               MOVE    SPA-P033-PTID          TO  SPA-ONSKAKU-PTID
               MOVE    SPA-P033-PTNUM         TO  SPA-ONSKAKU-PTNUM
               PERFORM 490-KAKUTEI-SYORI-SEC
           END-IF
           .
       407-KOHNOMI-SYORI-EXT.
           EXIT.
      ******
      *****************************************************************
      *    オンライン資格確認更新処理
      *****************************************************************
       4111-ONSKAKU-MODEUPD-SEC         SECTION.
      *
      *    オンライン資格確認UID検索処理
           INITIALIZE                      ONSKAKU-REC
           MOVE    SPA-HOSPNUM         TO  ONSKAKU-HOSPNUM
           MOVE    SPA-P031-SEL-UUID   TO  ONSKAKU-TBL-UUID
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
           MOVE    SPACE               TO  WRK-OYA-UUID
           IF      FLG-ONSKAKU         =   ZERO
      *
      *        親ＩＤがあればすべて処理済みとする
               MOVE    ONSKAKU-OYA-UUID        TO  WRK-OYA-UUID
      *        更新終了
               MOVE    "1"                 TO  ONSKAKU-MOD-FLG
      *        処理中解除と更新処理
               PERFORM 49901-ONSKAKU-MODEUPDATE-SEC
           END-IF
      *
           IF     (WRK-OYA-UUID    NOT =   SPACE )
              AND (SPA-ERRCD           =   SPACE )
      *        親UUIDで検索
               INITIALIZE                      ONSKAKU-REC
               MOVE    SPA-HOSPNUM         TO  ONSKAKU-HOSPNUM
               MOVE    WRK-OYA-UUID        TO  ONSKAKU-OYA-UUID
      *
               MOVE    ONSKAKU-REC         TO  MCPDATA-REC
               MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "key7"              TO  MCP-PATHNAME
                   PERFORM 900-ONSKAKU-READ-SEC
               ELSE
                   MOVE    1               TO  FLG-ONSKAKU
               END-IF
               PERFORM UNTIL    FLG-ONSKAKU    =   1
      *
                   IF      ONSKAKU-MOD-FLG     NOT =   "1"
      *                更新終了
                       MOVE    "1"             TO  ONSKAKU-MOD-FLG
      *                処理中解除と更新処理
                       PERFORM 49901-ONSKAKU-MODEUPDATE-SEC
                   END-IF
      *
                   MOVE    "key7"              TO  MCP-PATHNAME
                   PERFORM 900-ONSKAKU-READ-SEC
               END-PERFORM
               MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-IF
           .
       4111-ONSKAKU-MODEUPD-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    オンライン資格確認解除処理
      *****************************************************************
       49901-ONSKAKU-MODEUPDATE-SEC         SECTION.
      *
      *    処理中解除
           MOVE    SPACE               TO  ONSKAKU-LOCK-KBN
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
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY "P033 ONSKAKU KEY UPD ERR:"  MCP-RC
                   ",KEY:" ONSKAKU-KEY
           END-IF
           .
       49901-ONSKAKU-MODEUPDATE-EXT.
           EXIT.
      **
      *****************************************************************
      *    患者確定処理
      *****************************************************************
       490-KAKUTEI-SYORI-SEC           SECTION.
      *
      *    処理中確認
           PERFORM 4901-LOCK-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  490-KAKUTEI-SYORI-EXT
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )
             AND  (SPA-P033-MOTOPG     =   "P031")
             AND  (SPA-P033-PTID   NOT =   ZERO )
      *        P031からの時、排他制御を行う
               PERFORM 990-LOCK-IN-SEC
               IF      SPA-ERRCD        NOT =   SPACE
      *            排他中
                   MOVE    "0012"           TO  SPA-ERRCD
                   MOVE    1                TO  SPA-P033-CUR
      *            排他解除
                   PERFORM 990-LOCK-OUT-SEC
                   GO  TO  490-KAKUTEI-SYORI-EXT
               END-IF
           END-IF
      *
           MOVE    SPA-P031-SEL-UUID   TO  SPA-ONSKAKU-UID
      *R04.1
           MOVE    SPA-P031-GENDO-DOUIFLG  TO  SPA-GENDO-DOUIFLG
           MOVE    SPA-P033-ON-CARD-CLASS  TO  SPA-ONSKAKU-CARD-CLASS
      *
           IF    ( SPA-ONSKAKU-SYORI   NOT =   "1" )
             AND ( SPA-P031-SYORI      NOT =   2   )
      *        更新の有無
               IF      SPA-P033-CHK-NAME   =   "1"
                   MOVE    "1"             TO  SPA-ONSKCNG-NAME
               END-IF
               IF      SPA-P033-CHK-KANANAME   =   "1"
                   MOVE    "1"             TO  SPA-ONSKCNG-KANANAME
               END-IF
      *R05.1   性別
               IF      SPA-P033-CHK-SEX    =   "1"
                  MOVE    "1"             TO  SPA-ONSKCNG-SEX
               END-IF
      **       IF      SPA-P033-CHK-POST   =   "1"
      *            MOVE    "1"             TO  SPA-ONSKCNG-POST
      **       END-IF
               IF      SPA-P033-CHK-ADRS   =   "1"
                   MOVE    "1"             TO  SPA-ONSKCNG-ADRS
               END-IF
           END-IF
      *
      *    地方公費あり
           IF     (SPA-P033-KOHARI     =   "3"
      *2025/06    限度額認定証等も対象
                                       OR  "1"
                                       OR  "2"
                                       OR  "4"   )
              AND (SPA-ONSKAKU-SYORI   =   "1"
                                       OR  "2"
                                       OR  "3"
                                       OR  "4"
      *2023/08  
                                       OR  "6" )
               PERFORM 409-PCHK-SYORI-SEC
               GO       TO      490-KAKUTEI-SYORI-EXT
           END-IF
      *
           PERFORM 210-BACK
           .
       490-KAKUTEI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理中確認
      *****************************************************************
       4901-LOCK-CHK-SEC           SECTION.
      *
           INITIALIZE                      ONSKAKU-REC
           MOVE    SPA-HOSPNUM         TO  ONSKAKU-HOSPNUM
           MOVE    SPA-P031-SEL-UUID   TO  ONSKAKU-TBL-UUID
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
           IF      FLG-ONSKAKU         =   1
               MOVE    "1001"              TO  SPA-ERRCD
               GO  TO   4901-LOCK-CHK-EXT
           END-IF
           IF      ONSKAKU-LOCK-KBN    =   SPACE
               MOVE    "0010"              TO  SPA-ERRCD
      *        処理中設定       
               INITIALIZE                  ORCSMCNDATEAREA
               CALL    "ORCSMCNDATE"       USING
                                           ORCSMCNDATEAREA
      *
               MOVE    "1"                 TO  ONSKAKU-LOCK-KBN
               MOVE    SPA-OPID            TO  ONSKAKU-OPID
               MOVE    SMCNDATE-YMD        TO  ONSKAKU-UPYMD
               MOVE    SMCNDATE-HMS        TO  ONSKAKU-UPHMS
      *
               MOVE    ONSKAKU-REC         TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO 
                   MOVE    "1001"              TO  SPA-ERRCD
                   DISPLAY "P033 ONSKAKU KEY UPD ERR:"  MCP-RC
                       ",KEY:" ONSKAKU-KEY
               END-IF
           END-IF
           .
       4901-LOCK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    氏名変更区分処理
      *****************************************************************
       400-CHK-NAME-SEC           SECTION.
      *
           EVALUATE    SPA-P033-CHK-NAME
               WHEN    "1"
                   MOVE    "0"         TO  SPA-P033-CHK-NAME
               WHEN    "0"
               WHEN    SPACE
                   MOVE    "1"         TO  SPA-P033-CHK-NAME
      *            氏名が同じ時、チェックなし
                   IF      SPA-P033-ON-NAME    =   SPA-P033-NAME
                       MOVE    "0"         TO  SPA-P033-CHK-NAME
                   END-IF
           END-EVALUATE
           .
      *
       400-CHK-NAME-EXT.
           EXIT.
      *
      *****************************************************************
      *    カナ氏名変更区分処理
      *****************************************************************
       400-CHK-KANANAME-SEC           SECTION.
      *
           EVALUATE    SPA-P033-CHK-KANANAME
               WHEN    "1"
                   MOVE    "0"         TO  SPA-P033-CHK-KANANAME
               WHEN    "0"
               WHEN    SPACE
                   MOVE    "1"         TO  SPA-P033-CHK-KANANAME
      *            氏名が同じ時、チェックなし
                   IF     (SPA-P033-ON-KANANAME
                                           =   SPA-P033-KANANAME )
                     OR   (SPA-P033-HEN-KANANAME
                                           =   SPACE      )
                       MOVE    "0"         TO  SPA-P033-CHK-KANANAME
                   END-IF
           END-EVALUATE
           .
      *
       400-CHK-KANANAME-EXT.
           EXIT.
      *****************************************************************
      *    性別変更区分処理
      *****************************************************************
       400-CHK-SEX-SEC           SECTION.
      *
           EVALUATE    SPA-P033-CHK-SEX
               WHEN    "1"
                   MOVE    "0"         TO  SPA-P033-CHK-SEX
               WHEN    "0"
               WHEN    SPACE
                   MOVE    "1"         TO  SPA-P033-CHK-SEX
      *
                   IF      (SPA-P033-SEX-K     =   SPA-P033-ON-SEX-K)
                       OR  (SPA-P033-HEN-SEX   =   SPACE     )
                       MOVE    "0"         TO  SPA-P033-CHK-SEX
                   END-IF
      *            オン資格の性別が１，２以外は変更なし
                   IF      (SPA-P033-ON-SEX-K  =   "1"
                                               OR  "2" )
                       CONTINUE
                   ELSE
                       MOVE    "0"         TO  SPA-P033-CHK-SEX
                   END-IF
           END-EVALUATE
           .
      *
       400-CHK-SEX-EXT.
           EXIT.
      *****************************************************************
      *    住所変更変更区分処理
      *****************************************************************
       400-CHK-ADRS-SEC           SECTION.
      *
           EVALUATE    SPA-P033-CHK-ADRS
               WHEN    "1"
                   MOVE    "0"         TO  SPA-P033-CHK-ADRS
               WHEN    "0"
               WHEN    SPACE
                   MOVE    "1"         TO  SPA-P033-CHK-ADRS
                   IF    ((SPA-P033-ADRS   =   SPA-P033-ON-ADRS)
                      AND (SPA-P033-POST   =   SPA-P033-ON-POST))
                    OR   ( SPA-P033-HEN-ADRS   =   SPACE )
                       MOVE    "0"         TO  SPA-P033-CHK-ADRS
                   END-IF
           END-EVALUATE
           .
      *
       400-CHK-ADRS-EXT.
           EXIT.
      *****************************************************************
      *    公費医療券画像確認処理
      *****************************************************************
       406-P037-SYORI-SEC                    SECTION.
      *
      *    保険証OCRまたは、顔認証のみ
           IF      SPA-P033-SYORIKBN   =   "1"
                                       OR  "3"
               CONTINUE
           ELSE
               GO  TO  406-P037-SYORI-EXT
           END-IF
      *
           INITIALIZE                  SPA-P037-AREA
      *
           MOVE    "P033"              TO  SPA-MOTOPG
           MOVE    "P033"              TO  SPA-P037-MOTOPG
           MOVE    "P037"              TO  SPA-SAKIPG
           MOVE    SPA-P031-SEL-UUID   TO  SPA-P037-HKN-TBL-UUID
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP037"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-P02SCRAREA
           MOVE    SPA-WORK            TO  SPA-P02KYOUTU
      *
           IF      SPA-P037-LSTMAX     =   ZERO
      *        対象なし
               MOVE    "0020"              TO  SPA-ERRCD
               GO  TO  406-P037-SYORI-EXT
           END-IF
      *
           MOVE    "BIRTHDAY"          TO  MCP-WIDGET
      * 
           MOVE    "P033"              TO  SPA-MOTOPG
           MOVE    "P037"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "P037"              TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                   TO  FLG-END
           .
       406-P037-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費医療券画像確認処理
      *****************************************************************
       4037-KOUHI-HEN-SEC         SECTION.
      *
      *    オンライン資格確認UID検索処理
           INITIALIZE                      ONSKAKU-REC
           MOVE    SPA-HOSPNUM         TO  ONSKAKU-HOSPNUM
           MOVE    SPA-P031-SEL-UUID   TO  ONSKAKU-TBL-UUID
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
           IF      FLG-ONSKAKU         =   ZERO
               CONTINUE
           ELSE
               GO  TO  4037-KOUHI-HEN-EXT
           END-IF
      *
      *    限度額適用認定証などサブプロで編集
           MOVE    SPACE               TO  SPA-P033-SIKKAN-MSG-G
           MOVE    SPACE               TO  SPA-P033-GENDO-MSG
           MOVE    SPACE               TO  SPA-P033-KOHARI
      *    公費・低所得者情報
           INITIALIZE                  SPA-ONSKAKU-KOHTEI-AREA
      *2024/02
      *    医療扶助対応の為、判定廃止
      *     IF     (ONSKAKU-GENDO-DOUIFLG   =   "1" )
      *       OR   (ONSKAKU-SIKKAN-DOUIFLG  =   "1" )
      *      顔認証（公費照会）
      *       OR   (SPA-P033-SYORIKBN   NOT =   "2" )
               INITIALIZE                      ORCSONSHI002AREA
               MOVE    "03"                TO  LNK-ONSHI002-SYOKBN
      *        資格確認限度額承認
               MOVE    SPA-P031-GENDO-DOUIFLG
                                           TO  LNK-ONSHI002-DOUIFLG
      *        交付番号の転記
               MOVE    SPA-ONSHI-KOFUTNK   TO  LNK-ONSHI002-KFTNKKBN
               CALL    "ORCSONSHI002"      USING
                                           SPA-AREA
                                           ORCSONSHI002AREA
                                           ONSKAKU-REC
      *        限度額適用認定証
               MOVE    LNK-ONSHI002-GENDO-MSG
                                           TO  SPA-P033-GENDO-MSG
      *        特定疾病療養受療証提供
               PERFORM VARYING    IDX-SK      FROM    1   BY  1
                       UNTIL      IDX-SK      >   3
                   IF      LNK-ONSHI002-SIKKAN-MSG(IDX-SK)
                                         NOT =   SPACE
                       MOVE    LNK-ONSHI002-SIKKAN-MSG(IDX-SK)
                                             TO  SPA-P033-SIKKAN-MSG
                                                           (IDX-SK)
                   END-IF
               END-PERFORM 
      *        公費・低所得者あり
               MOVE    LNK-ONSHI002-KOH-ARI  TO  SPA-P033-KOHARI
      *2024/01
               MOVE    LNK-ONSHI002-FUJ-ARI  TO  SPA-P033-FUJARI
               PERFORM 300113-ONSHI-KOHHEN-SEC
      **** END-IF
           .
       4037-KOUHI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "P033"              TO  SPA-MOTOPG
           MOVE    SPA-P033-MOTOPG     TO  SPA-SAKIPG
           IF      SPA-ONSKAKU-SYORI   NOT =   SPACE
               MOVE    "P02"           TO  SPA-SAKIPG
           END-IF
      *
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *R03.9 
      *****************************************************************
      *    公費確認（P034）へ処理
      *****************************************************************
       405-P034-SYORI-SEC           SECTION.
      *
      *
           INITIALIZE                  SPA-P034-AREA
      *
           MOVE    "P033"              TO  SPA-MOTOPG
           MOVE    "P033"              TO  SPA-P034-MOTOPG
           MOVE    "P034"              TO  SPA-SAKIPG
           MOVE    SPA-P031-SEL-UUID   TO  SPA-P034-TBL-UUID
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP034"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-P02SCRAREA
           MOVE    SPA-WORK            TO  SPA-P02KYOUTU
      *
      *
           MOVE    "B01"               TO  MCP-WIDGET
      * 
           MOVE    "P033"              TO  SPA-MOTOPG
           MOVE    "P034"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "P034"              TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                   TO  FLG-END
           .
       405-P034-SYORI-EXT.
           EXIT.
      *TEST
      *****************************************************************
      *    公費確認メッセージ（PCHK)へ処理
      *****************************************************************
       409-PCHK-SYORI-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-CHKMSG
           STRING  "公費照会の情報があります。"
                   "公費の確認画面を表示します。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-CHKMSG
           END-STRING
      *
      *
           MOVE    SPACE               TO  PCHK
           INITIALIZE                      PCHK
           MOVE    WRK-CHKMSG          TO  PCHK-CHKMSG
      *
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "P033"              TO  SPA-MOTOPG
           MOVE    "PCHK"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "PCHK"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       409-PCHK-SYORI-EXT.
           EXIT.
      *****
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO              500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-PIDCD       NOT =   SPACE
               PERFORM 520-JIDSET-SEC
               GO      TO              500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "P033"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "選択番号入力エラー"    TO  SPA-ERRMSG
               WHEN    "0002"
                   STRING  "被保険者証照会です。"
                           "保険変更のみ行います。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0003"
                   STRING  "患者が確定されていません。"
                           "患者確認を行って下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0004"
                   STRING  "オンライン資格確認の保険と"
                           "選択されている患者の保険が違います。"
                           "保険変更できません"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0005"
                   STRING  "オンライン資格確認の保険と同一の保険です。"
                           "保険変更をして下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0006"
                   STRING  "患者情報に変更箇所がありません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0007"
                   STRING  "患者番号で処理済みです。"
                           "別の患者番号で処理はできません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *
               WHEN    "0008"
                   STRING  "オンライン資格確認の保険と"
                           "患者の保険が違います。"
                           "患者登録で変更して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *
               WHEN    "0009"
                   STRING  "患者番号が確定されています。"
                           "患者番号の変更はできません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *
      *R03.5
               WHEN    "0010"
                   STRING  "資格確認処理中が解除となっていました。"
                           "処理中設定をしました。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "1001"
                   STRING  "オンライン資格確認データ更新が"
                           "できませんでした。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *R03.8
               WHEN    "0011"
                   STRING  "オンライン資格確認の保険と"
                           "患者の保険が違います。"
                           "確認処理はできません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0012"
                   STRING  "選択した患者は他端末で処理中です。"
                           "処理できません。"
                                               DELIMITED  BY  SIZE
                           SLOCK-MSG           DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
                WHEN    "0013"
                   STRING  "対象の保険は有効期間外です。"
                           "保険変更を行って下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                    END-STRING
                WHEN    "0014"
                   STRING  "対象の保険が存在しませんでした。"
                           "再度確認して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
                 WHEN    "0015"
                   STRING  "オンライン資格確認の患者番号と"
                           "選択した患者番号が違います。"
                           "保険変更を行って下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
                 WHEN    "0016"
                   STRING  "オンライン資格確認の患者番号が"
                           "決定されていません。"
                           "保険変更を行って下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K100"
                   STRING  "警告！"
                           "公費照会の情報があります。"
                           "「オン資格」再押下で確認して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *2022/12
                 WHEN    "0020"
                   STRING  "公費医療券画像情報は存在しません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *
               WHEN    "0017"
                   STRING  "保険終了日の変更で次の保険と"
                           "期間が重複します。"
                           "次の保険の期間変更をして下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *2023/08
               WHEN    "0018"
                   STRING  "オンライン資格確認の患者番号と"
                           "選択した患者番号が違います。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *
      *2023/10
               WHEN    "0019"
                   STRING  "保険の開始日が資格取得日より後です。"
                           "患者登録で変更して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *2024/01
               WHEN    "0021"
                   STRING  "医療扶助です。保険情報はありません。"
                           "保険変更なしで変更して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *"00XX"
      *
               WHEN    "1011"
                   STRING  "保険確認更新エラー"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *   
           END-EVALUATE
      *
           MOVE    SPACE               TO  PERR
           MOVE    SPA-ERRCD           TO  PERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  PERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "P033"             TO  SPA-MOTOPG
           MOVE    "PERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "PERR"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-JIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-PIDMSG
           EVALUATE    SPA-PIDCD
               WHEN    "0101"
                   STRING  "資格確認で患者番号が決定されています。"
                           "新規の患者番号登録でよろしいですか？"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-PIDMSG
                   END-STRING
               WHEN    "0102"
      ****         STRING  "保険情報・性別・生年月日で同一の患者が"
                   STRING  "氏名・性別・生年月日で同一の患者が"
                           "あります。新規患者でよろしいですか？"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-PIDMSG
                   END-STRING
               WHEN    "0103"
                   STRING  "オンライン資格確認と同一の保険の期間変更を"
                           "行います。よろしいですか？"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-PIDMSG
                   END-STRING
               WHEN    "0104"
                   STRING  "オンライン資格確認と同一の保険があります。"
                           "保険追加でよろしいですか？"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-PIDMSG
                   END-STRING
      *R03.8
               WHEN    "1101"
                   STRING  "表示している保険の確認年月日を"
                           "更新します。元に戻すことはできません。"
                           "よろしいですか？"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-PIDMSG
                   END-STRING
               WHEN    "1102"
                   STRING  "表示している保険の確認年月日は"
                           "既に更新済みです。"
                           "処理済み対応を行いますか？"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-PIDMSG
                   END-STRING
               WHEN    "1103"
                   STRING  "公費照会の情報があります。"
                           "「OK」で保険変更を行います。"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-PIDMSG
                   END-STRING
      ***         WHEN    "1104"
      *             STRING  "公費照会の情報があります。"
      *                     "「OK」で公費確認画面を表示します。"
      *                                         DELIMITED  BY  SIZE
      *                                         INTO  WRK-PIDMSG
      **             END-STRING
      *
               WHEN    "1105"
                   STRING  "限度額認定証などの情報があります。"
                           "「OK」で保険変更を行います。"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-PIDMSG
                   END-STRING
      *2024/01
               WHEN    "1109"
                   STRING  "医療扶助の情報があります。"
                           "「OK」で保険変更を行います。"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-PIDMSG
                   END-STRING
      *2023/11
               WHEN    "1106"
                   STRING  "オンライン資格確認の患者番号と"
                           "選択した患者番号が一致しません。"
                           "この患者でよろしいですか？"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-PIDMSG
                   END-STRING
               WHEN    "1107"
                   STRING  "オンライン資格確認の保険と"
                           "患者の保険が違います。"
                           "「保険変更なし」でよろしいですか？"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-PIDMSG
                   END-STRING
               WHEN    "1108"
                   STRING  "患者保険情報の変更はしません。"
                           "よろしいですか？"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-PIDMSG
                   END-STRING
             WHEN    OTHER
                   MOVE    SPA-PIDCD
                                       TO  WRK-PIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-PID1-FLG
      *
           MOVE    SPACE               TO  PID1
           INITIALIZE                      PID1
           MOVE    SPA-PIDCD           TO  PID1-ID1CODE
           MOVE    WRK-PIDMSG          TO  PID1-ID1MSG
           MOVE    "戻る"              TO  PID1-B01
           MOVE    "ＯＫ"              TO  PID1-B12
      *
           MOVE    "B01"               TO  MCP-WIDGET
           EVALUATE    SPA-PIDCD
               WHEN    "0103"
               WHEN    "1103"
                   MOVE    "B12"               TO  MCP-WIDGET
           END-EVALUATE
      *
           MOVE    "P033"             TO  SPA-MOTOPG
           MOVE    "PID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "PID1"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-JIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦→和暦FORMAT日付編集
      *****************************************************************
       8001-SEIWA-YMDEDIT-SEC          SECTION.
      *
           IF      WRK-SYMD            =   SPACE
                                       OR  ZERO
                                       OR  ALL "9"
               MOVE    WRK-SYMD            TO  WRK-HENYMD
           ELSE
               MOVE    SPACE               TO  STS-AREA-DAY
               INITIALIZE                      STS-AREA-DAY
               MOVE    SPACE               TO  LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
               IF      STS-DAY-RC1             =   ZERO
                   MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMD
               ELSE
                   MOVE    SPACE               TO  WRK-HENYMD
               END-IF
           END-IF
           .
       8001-SEIWA-YMDEDIT-EXT.
           EXIT.
      *****************************************************************
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC         SECTION.
      *
      *    排他制御処理
           MOVE    1                   TO  SLOCK-MODE
           MOVE    SPA-P033-PTID       TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "9999"              TO  SPA-ERRCD
           END-IF
           .
       990-LOCK-IN-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御解除処理
      *****************************************************************
       990-LOCK-OUT-SEC         SECTION.
      *
      *    排他制御解除処理
           MOVE    2                   TO  SLOCK-MODE
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           .
       990-LOCK-OUT-EXT.
           EXIT.
      *****************************************************************
      *    オンライン資格確認読込
      *****************************************************************
       900-ONSKAKU-READ-SEC         SECTION.
      *
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
      *R03.12
      *****************************************************************
      **    患者マスターキー読み込み
      *****************************************************************
       9001-PTINF-KEYREAD-SEC          SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-PTINF-READ-SEC
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       9001-PTINF-KEYREAD-EXT.
           EXIT.
      *****************************************************************
      **    患者マスター読み込み
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
               MOVE    ZERO            TO  FLG-PTINF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
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
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
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
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
