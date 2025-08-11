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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORAPION009R1V2.
      ******************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 予約患者一括資格確認照会処理
      *                     （資格確認結果登録処理）
      *  管理者            :
      *  作成日付    作業者        記述
      *  21/11/30    ORCAMO        新規作成
      ******************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.02.01    ORCAMO       24/06/26  医療扶助併用対応
      *  05.02.07    ORCAMO       24/11/15  訪問診療対応
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
           03  FLG-YYK                 PIC 9(01).
           03  FLG-JOBKANRI            PIC 9(01).
           03  FLG-PTHKNINF            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTKOHINF            PIC 9(01).
           03  FLG-MONTHLYNUM          PIC 9(01).
           03  FLG-ONSHI-AIDLST        PIC 9(01).
           03  FLG-ONSHI-HOUMON        PIC 9(01).
      *
           03  PTHKNINF-OK             PIC 9(01).
           03  FLG-ARI                 PIC 9(01).
           03  FLG-HENERR              PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDW                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDX1                    PIC 9(04).
           03  IDXX                    PIC 9(04).
           03  IDYY                    PIC 9(04).
           03  IDZZ                    PIC 9(04).
      *     
           03  IDH1                    PIC 9(04).
           03  IDH2                    PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
            03  WRK-CNT-X.
                05  WRK-CNT            PIC 9(04).
            03  WRK-PTID               PIC 9(10).
            03  WRK-UUID               PIC X(36).
            03  WRK-FUJYO-UUID         PIC X(36).
            03  WRK-KOH-UUID           PIC X(36).
            03  WRK-TAIHI-PTID         PIC 9(10).
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
           03  WRK-HENKAN-OT           PIC X(30).
           03  WRK-HENKAN-IN           PIC X(30).
      *    総処理件数
           03  WRK-ALL-CNT             PIC 9(04).
      *    AIDLST RENNUM
           03  WRK-RENNUM              PIC 9(03).
      *
       01  WRK-QUAAPPREQ2-AREA.     
           03  WRK-QUAAPPREQ2-RESULT-HKN
                                       PIC 9(02).
           03  WRK-QUAAPPREQ2-RESULT-SEH
                                       PIC 9(02).
      *    配列をWRK-CONS-RES-MAXにあわせる  
           03  WRK-QUAAPPREQ2-OCC OCCURS   200.     
               05  WRK-QUAAPPREQ2-SHO-KIGO PIC X(80).
               05  WRK-QUAAPPREQ2-SHO-NUM  PIC X(80).
               05  WRK-QUAAPPREQ2-RESULT-AREA.
      *            配列をWRK-CONS-RESULT-MAXにあわせる  
                   07  WRK-QUAAPPREQ2-RESULT-OCC   OCCURS  2.
                       09  WRK-QUAAPPREQ2-RES-KIGO
                                       PIC X(80).
                       09  WRK-QUAAPPREQ2-RES-NUM
                                       PIC X(80).
                       09  WRK-QUAAPPREQ2-RES-HIHKNJANAME
                                       PIC X(200).
                       09  WRK-QUAAPPREQ2-RES-NAME
                                       PIC X(200).
                       09  WRK-QUAAPPREQ2-RES-NAME-ETC
                                       PIC X(200).
                       09  WRK-QUAAPPREQ2-RES-KANANAME
                                       PIC X(200).
                       09  WRK-QUAAPPREQ2-RES-KANANAME1
                                       PIC X(200).
                       09  WRK-QUAAPPREQ2-RES-KANANAME2
                                       PIC X(200).
                       09  WRK-QUAAPPREQ2-RES-KANANAME-ETC
                                       PIC X(200).
                       09  WRK-QUAAPPREQ2-RES-ADRS
                                       PIC X(500).
                       09  WRK-QUAAPPREQ2-HKNJANUM-NAME
                                       PIC X(200).
      *                PTID不一致で初期化したときのエラーコード
                       05  WRK-QUAAPPREQ2-ERR-CODE
                                       PIC X(09).
                       05  WRK-QUAAPPREQ2-ERR-PTID
                                       PIC 9(10).
      *
       01  WRK-QUAAPPREQ2-PTID-AREA.
      *    配列をWRK-CONS-RESULT-MAXにあわせる  
           03  WRK-QUAAPPREQ2-PTID-OCC     OCCURS  2.   
               05  WRK-QUAAPPREQ2-PTID PIC 9(10).
               05  WRK-QUAAPPREQ2-UUID PIC X(36).
      *    設定するPTID
       01  WRK-QUAAPPREQ2-PTID-NEW-AREA.
      *    配列をWRK-CONS-RESULT-MAXにあわせる  
           03  WRK-QUAAPPREQ2-PTID-N-OCC   OCCURS  2.   
               05  WRK-QUAAPPREQ2-N-PTID
                                       PIC 9(10).
               05  WRK-QUAAPPREQ2-N-UUID
                                       PIC X(36).
      *
      *    患者毎の資格確認結果
       01  WRK-TAIHI-QUAAPPREQ2-AREA.
           03  WRK-TAIHI-QUAAPPREQ2-INDEX
                                       PIC 9(03)   OCCURS  2.
           03  WRK-TAIHI-QUAAPPREQ2-MAX
                                       PIC 9(02).
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
      *
      *    全角半角変換テーブル
       01  TBL-HENKAN-AREA.
           03  FILLER              PIC X(01)   VALUE   "0".
           03  FILLER              PIC X(02)   VALUE   "０".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(02)   VALUE   "１".
           03  FILLER              PIC X(01)   VALUE   "2".
           03  FILLER              PIC X(02)   VALUE   "２".
           03  FILLER              PIC X(01)   VALUE   "3".
           03  FILLER              PIC X(02)   VALUE   "３".
           03  FILLER              PIC X(01)   VALUE   "4".
           03  FILLER              PIC X(02)   VALUE   "４".
           03  FILLER              PIC X(01)   VALUE   "5".
           03  FILLER              PIC X(02)   VALUE   "５".
           03  FILLER              PIC X(01)   VALUE   "6".
           03  FILLER              PIC X(02)   VALUE   "６".
           03  FILLER              PIC X(01)   VALUE   "7".
           03  FILLER              PIC X(02)   VALUE   "７".
           03  FILLER              PIC X(01)   VALUE   "8".
           03  FILLER              PIC X(02)   VALUE   "８".
           03  FILLER              PIC X(01)   VALUE   "9".
           03  FILLER              PIC X(02)   VALUE   "９".
      *
      *英字
           03  FILLER              PIC X(01)   VALUE   "A".
           03  FILLER              PIC X(02)   VALUE   "Ａ".
           03  FILLER              PIC X(01)   VALUE   "B".
           03  FILLER              PIC X(02)   VALUE   "Ｂ".
           03  FILLER              PIC X(01)   VALUE   "C".
           03  FILLER              PIC X(02)   VALUE   "Ｃ".
           03  FILLER              PIC X(01)   VALUE   "D".
           03  FILLER              PIC X(02)   VALUE   "Ｄ".
           03  FILLER              PIC X(01)   VALUE   "E".
           03  FILLER              PIC X(02)   VALUE   "Ｅ".
           03  FILLER              PIC X(01)   VALUE   "F".
           03  FILLER              PIC X(02)   VALUE   "Ｆ".
           03  FILLER              PIC X(01)   VALUE   "G".
           03  FILLER              PIC X(02)   VALUE   "Ｇ".
           03  FILLER              PIC X(01)   VALUE   "H".
           03  FILLER              PIC X(02)   VALUE   "Ｈ".
           03  FILLER              PIC X(01)   VALUE   "I".
           03  FILLER              PIC X(02)   VALUE   "Ｉ".
           03  FILLER              PIC X(01)   VALUE   "J".
           03  FILLER              PIC X(02)   VALUE   "Ｊ".
           03  FILLER              PIC X(01)   VALUE   "K".
           03  FILLER              PIC X(02)   VALUE   "Ｋ".
           03  FILLER              PIC X(01)   VALUE   "L".
           03  FILLER              PIC X(02)   VALUE   "Ｌ".
           03  FILLER              PIC X(01)   VALUE   "N".
           03  FILLER              PIC X(02)   VALUE   "Ｎ".
           03  FILLER              PIC X(01)   VALUE   "M".
           03  FILLER              PIC X(02)   VALUE   "Ｍ".
           03  FILLER              PIC X(01)   VALUE   "O".
           03  FILLER              PIC X(02)   VALUE   "Ｏ".
           03  FILLER              PIC X(01)   VALUE   "P".
           03  FILLER              PIC X(02)   VALUE   "Ｐ".
           03  FILLER              PIC X(01)   VALUE   "Q".
           03  FILLER              PIC X(02)   VALUE   "Ｑ".
           03  FILLER              PIC X(01)   VALUE   "R".
           03  FILLER              PIC X(02)   VALUE   "Ｒ".
           03  FILLER              PIC X(01)   VALUE   "S".
           03  FILLER              PIC X(02)   VALUE   "Ｓ".
           03  FILLER              PIC X(01)   VALUE   "T".
           03  FILLER              PIC X(02)   VALUE   "Ｔ".
           03  FILLER              PIC X(01)   VALUE   "U".
           03  FILLER              PIC X(02)   VALUE   "Ｕ".
           03  FILLER              PIC X(01)   VALUE   "V".
           03  FILLER              PIC X(02)   VALUE   "Ｖ".
           03  FILLER              PIC X(01)   VALUE   "W".
           03  FILLER              PIC X(02)   VALUE   "Ｗ".
           03  FILLER              PIC X(01)   VALUE   "X".
           03  FILLER              PIC X(02)   VALUE   "Ｘ".
           03  FILLER              PIC X(01)   VALUE   "Y".
           03  FILLER              PIC X(02)   VALUE   "Ｙ".
           03  FILLER              PIC X(01)   VALUE   "Z".
           03  FILLER              PIC X(02)   VALUE   "Ｚ".
      *英字
           03  FILLER              PIC X(01)   VALUE   "a".
           03  FILLER              PIC X(02)   VALUE   "ａ".
           03  FILLER              PIC X(01)   VALUE   "b".
           03  FILLER              PIC X(02)   VALUE   "ｂ".
           03  FILLER              PIC X(01)   VALUE   "c".
           03  FILLER              PIC X(02)   VALUE   "ｃ".
           03  FILLER              PIC X(01)   VALUE   "d".
           03  FILLER              PIC X(02)   VALUE   "ｄ".
           03  FILLER              PIC X(01)   VALUE   "e".
           03  FILLER              PIC X(02)   VALUE   "ｅ".
           03  FILLER              PIC X(01)   VALUE   "f".
           03  FILLER              PIC X(02)   VALUE   "ｆ".
           03  FILLER              PIC X(01)   VALUE   "g".
           03  FILLER              PIC X(02)   VALUE   "ｇ".
           03  FILLER              PIC X(01)   VALUE   "h".
           03  FILLER              PIC X(02)   VALUE   "ｈ".
           03  FILLER              PIC X(01)   VALUE   "i".
           03  FILLER              PIC X(02)   VALUE   "ｉ".
           03  FILLER              PIC X(01)   VALUE   "j".
           03  FILLER              PIC X(02)   VALUE   "ｊ".
           03  FILLER              PIC X(01)   VALUE   "k".
           03  FILLER              PIC X(02)   VALUE   "ｋ".
           03  FILLER              PIC X(01)   VALUE   "l".
           03  FILLER              PIC X(02)   VALUE   "ｌ".
           03  FILLER              PIC X(01)   VALUE   "n".
           03  FILLER              PIC X(02)   VALUE   "ｎ".
           03  FILLER              PIC X(01)   VALUE   "m".
           03  FILLER              PIC X(02)   VALUE   "ｍ".
           03  FILLER              PIC X(01)   VALUE   "o".
           03  FILLER              PIC X(02)   VALUE   "ｏ".
           03  FILLER              PIC X(01)   VALUE   "p".
           03  FILLER              PIC X(02)   VALUE   "ｐ".
           03  FILLER              PIC X(01)   VALUE   "q".
           03  FILLER              PIC X(02)   VALUE   "ｑ".
           03  FILLER              PIC X(01)   VALUE   "r".
           03  FILLER              PIC X(02)   VALUE   "ｒ".
           03  FILLER              PIC X(01)   VALUE   "s".
           03  FILLER              PIC X(02)   VALUE   "ｓ".
           03  FILLER              PIC X(01)   VALUE   "t".
           03  FILLER              PIC X(02)   VALUE   "ｔ".
           03  FILLER              PIC X(01)   VALUE   "u".
           03  FILLER              PIC X(02)   VALUE   "ｕ".
           03  FILLER              PIC X(01)   VALUE   "v".
           03  FILLER              PIC X(02)   VALUE   "ｖ".
           03  FILLER              PIC X(01)   VALUE   "w".
           03  FILLER              PIC X(02)   VALUE   "ｗ".
           03  FILLER              PIC X(01)   VALUE   "x".
           03  FILLER              PIC X(02)   VALUE   "ｘ".
           03  FILLER              PIC X(01)   VALUE   "y".
           03  FILLER              PIC X(02)   VALUE   "ｙ".
           03  FILLER              PIC X(01)   VALUE   "z".
           03  FILLER              PIC X(02)   VALUE   "ｚ".
      *
           03  FILLER              PIC X(01)   VALUE   "-".
           03  FILLER              PIC X(02)   VALUE   "?".
           03  FILLER              PIC X(01)   VALUE   "(".
           03  FILLER              PIC X(02)   VALUE   "（".
           03  FILLER              PIC X(01)   VALUE   ")".
           03  FILLER              PIC X(02)   VALUE   "）".
           03  FILLER              PIC X(01)   VALUE   "\".
           03  FILLER              PIC X(02)   VALUE   "￥".
           03  FILLER              PIC X(01)   VALUE   ".".
           03  FILLER              PIC X(02)   VALUE   "．".
           03  FILLER              PIC X(01)   VALUE   "/".
           03  FILLER              PIC X(02)   VALUE   "／".
           03  FILLER              PIC X(01)   VALUE   "<".
           03  FILLER              PIC X(02)   VALUE   "＜".
           03  FILLER              PIC X(01)   VALUE   ">".
           03  FILLER              PIC X(02)   VALUE   "＞".
           03  FILLER              PIC X(01)   VALUE   "*".
           03  FILLER              PIC X(02)   VALUE   "＊".
           03  FILLER              PIC X(01)   VALUE   "?".
           03  FILLER              PIC X(02)   VALUE   "？".
           03  FILLER              PIC X(01)   VALUE   "%".
           03  FILLER              PIC X(02)   VALUE   "％".
           03  FILLER              PIC X(01)   VALUE   "+".
           03  FILLER              PIC X(02)   VALUE   "＋".
           03  FILLER              PIC X(01)   VALUE   "!".
           03  FILLER              PIC X(02)   VALUE   "！".
           03  FILLER              PIC X(01)   VALUE   "#".
           03  FILLER              PIC X(02)   VALUE   "＃".
           03  FILLER              PIC X(01)   VALUE   "&".
           03  FILLER              PIC X(02)   VALUE   "＆".
           03  FILLER              PIC X(01)   VALUE   "=".
           03  FILLER              PIC X(02)   VALUE   "＝".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(02)   VALUE   "，".
           03  FILLER              PIC X(01)   VALUE   "_".
           03  FILLER              PIC X(02)   VALUE   "＿".
           03  FILLER              PIC X(01)   VALUE   "@".
           03  FILLER              PIC X(02)   VALUE   "＠".
           03  FILLER              PIC X(01)   VALUE   "[".
           03  FILLER              PIC X(02)   VALUE   "［".
           03  FILLER              PIC X(01)   VALUE   "]".
           03  FILLER              PIC X(02)   VALUE   "］".
      *
       01  TBL-HENKAN-AREA-R           REDEFINES   TBL-HENKAN-AREA.
           03  TBL-HENKAN-OCC          OCCURS   83
                                       INDEXED  TBL-IDX.
               05  TBL-HALF            PIC X(01).
               05  TBL-ZEN             PIC X(02).
       01  TBL-MAX                     PIC 9(04)   VALUE   83.
      *
       01  WRK-CONS-AREA.
      *    ジョブ管理ＤＢのキー値（予約）
           03  WRK-CONS-JOB-SHELLID
                                       PIC X(08)   VALUE   "yoyaku01".
      *    ジョブ管理ＤＢのキー値（訪問診療）
           03  WRK-CONS-JOB-SHELLID1
                                       PIC X(08)   VALUE   "P02V001".
           03  WRK-CONS-JOB-JOBID      PIC 9(07)   VALUE   ZERO.
           03  WRK-CONS-RES-MAX        PIC 9(04)   VALUE   200.
           03  WRK-CONS-RESULT-MAX     PIC 9(04)   VALUE   2.
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *   日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
      *     COPY    "CPORCSPTID.INC".
      *    患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *    拡張漢字チェックサブ
           COPY    "CPORCSKANACHK2.INC".
           COPY    "CPORCSCOMMON.INC".
      *    全角半角変換
           COPY    "CPORCSHANKCHG.INC".
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *    API XML読み込み用定義体
           COPY    "CPONLINEQUAAPPREQ2.INC".
      *    API XML書き込み用定義体
           COPY    "CPONLINEQUAAPPRES1.INC".
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
      *    予約
       01  YYK-REC.
           COPY    "CPYYK.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
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
           DISPLAY   "****************************"
           DISPLAY   "*   onlinequaapp2  start   *"
           DISPLAY   "****************************"
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
               MOVE   404                  TO  APILST09-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-RGTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "****************************"
           DISPLAY   "*   onlinequaapp2    end   *"
           DISPLAY   "****************************"
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  XML-ONLINEQUAAPPRES1
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *    日付・時間出力編集
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  QUAAPPRES1-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  QUAAPPRES1-INFORMATION-TIME
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
      *    資格確認結果登録処理
           PERFORM 2003-JOBKANRI-UPDATE-SEC
           IF      WRK-ERRCD   NOT =   SPACE
               GO  TO  200-MAIN-EXT
           END-IF
      *     
           IF      QUAAPPREQ2-RESULT-KBN
                                   =   "1"
      *        資格確認レコード作成処理
               PERFORM 2002-SIKAKU-KAKUNIN-INSERT-SEC
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       1000-RGTXMLREAD-SEC   SECTION.
      *
           IF      APILST09-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           INITIALIZE                      XML-ONLINEQUAAPPREQ2
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_onlinequaappreq2"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST09-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "onlinequaappreq2"  TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-ONLINEQUAAPPREQ2
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
               MOVE    "000"               TO  QUAAPPRES1-API-RESULT
               MOVE    "処理終了"          TO  QUAAPPRES1-API-RESULT-MSG
           ELSE
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD       TO  QUAAPPRES1-API-RESULT
               MOVE    WRK-ERRMSG      TO  QUAAPPRES1-API-RESULT-MSG
           END-IF
      *
           IF      APILST09-BODY       NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "xml_onlinequaappres1"
                                       TO  MCP-TABLE
           MOVE    1                   TO  QUAAPPRES1-MODE
           MOVE    "onlinequaappres1"  TO  QUAAPPRES1-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-ONLINEQUAAPPRES1
                                       SPA-AREA
           IF    ( MCP-RC              =   ZERO    OR  1 )
               MOVE  QUAAPPRES1-OBJECT    TO  APILST09-BODY
               MOVE  "application/xml" TO  APILST09-CONTENT-TYPE
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
           IF      QUAAPPREQ2-DATAREQ
                               =   SPACE 
               MOVE    "E15"           TO  WRK-ERRCD
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
           IF      QUAAPPREQ2-RESULT-KBN
                               =   "1"    OR  "9" 
                CONTINUE
           ELSE
               MOVE    "E12"       TO  WRK-ERRCD
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     資格確認レコード作成処理
      *****************************************************************
       2002-SIKAKU-KAKUNIN-INSERT-SEC    SECTION.
      *
      *    PTID UUID 取得
           INITIALIZE                  WRK-QUAAPPREQ2-PTID-AREA
                                       WRK-QUAAPPREQ2-AREA
      *
      *    訪問診療(同意一括)のとき
           IF      QUAAPPREQ2-PROCESS-CLASS
                                   =   "2"
               PERFORM 20021-SIKAKU-KAKUNIN-HOUMON-SEC
           ELSE
               PERFORM 20021-SIKAKU-KAKUNIN-YYK-SEC
           END-IF
      *
           .
       2002-SIKAKU-KAKUNIN-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *     資格確認レコード作成処理（予約）
      *****************************************************************
       20021-SIKAKU-KAKUNIN-YYK-SEC    SECTION.
      *
           PERFORM VARYING IDX     FROM    1   BY  1 
                   UNTIL   IDX     >       WRK-CONS-RES-MAX
                           OR      QUAAPPREQ2-SHO-SIKIBETU (IDX)
                                           =   SPACE
               INITIALIZE              WRK-PTID
                                       WRK-UUID
               UNSTRING    QUAAPPREQ2-SHO-SIKIBETU (IDX)
                                       DELIMITED   BY  ","
                                       INTO    WRK-PTID
                                               WRK-UUID
               END-UNSTRING
               MOVE    WRK-PTID        TO  WRK-QUAAPPREQ2-PTID (IDX)
               MOVE    WRK-UUID        TO  WRK-QUAAPPREQ2-UUID (IDX)
           END-PERFORM
      *     
           MOVE    ZERO            TO  WRK-TAIHI-PTID
           MOVE    1               TO  IDX
      *
      *    併用であれば、扶助ＵＩＤを設定     
           PERFORM         UNTIL   IDX     >   WRK-CONS-RES-MAX
                           OR      QUAAPPREQ2-SHO-SIKIBETU (IDX)
                                           =   SPACE
               INITIALIZE                  WRK-TAIHI-QUAAPPREQ2-AREA
               MOVE    WRK-QUAAPPREQ2-PTID (IDX)
                                       TO  WRK-TAIHI-PTID
               PERFORM         UNTIL   IDX     >   WRK-CONS-RES-MAX
                               OR      WRK-TAIHI-PTID
                                           NOT =
                                               WRK-QUAAPPREQ2-PTID (IDX)
                               OR      QUAAPPREQ2-SHO-SIKIBETU (IDX)
                                               =   SPACE
                   ADD     1               TO  WRK-TAIHI-QUAAPPREQ2-MAX
                   MOVE    IDX             TO
                                            WRK-TAIHI-QUAAPPREQ2-INDEX
                                              (WRK-TAIHI-QUAAPPREQ2-MAX)
      *                                        
                   ADD    1                TO  IDX
               END-PERFORM    
      *
               MOVE    SPACE               TO  WRK-FUJYO-UUID
               IF      WRK-TAIHI-QUAAPPREQ2-MAX    >   1
      *            UIDコード取得処理
                   PERFORM 20022-UID-GET-SEC
                   MOVE    C-UID           TO  WRK-FUJYO-UUID
               END-IF
      *
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL   IDX1    >       WRK-TAIHI-QUAAPPREQ2-MAX
                 MOVE    WRK-TAIHI-QUAAPPREQ2-INDEX (IDX1)
                                         TO  IDXX
                 PERFORM VARYING IDYY    FROM    1   BY  1
                         UNTIL   IDYY    >       WRK-CONS-RESULT-MAX
      *            全角変換処理
                   PERFORM 20021-KANACHK-HENSYU-SEC
      *
                   INITIALIZE                  ONSKAKU-REC
                   MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
                   MOVE    WRK-QUAAPPREQ2-UUID (IDXX)
                                           TO  ONSKAKU-TBL-UUID
                   IF      WRK-TAIHI-QUAAPPREQ2-MAX    >   1
                       MOVE    WRK-FUJYO-UUID  TO  ONSKAKU-FUJYO-UUID
                                                   ONSKAKU-FUJYO-UUID1
                       MOVE    "1"             TO  ONSKAKU-FUJYO-HEIYO 
                   END-IF
                   MOVE    "03"            TO  ONSKAKU-KAKU-ENDFLG
                   MOVE    WRK-QUAAPPREQ2-N-PTID (IDXX)
                                           TO  ONSKAKU-PTID
                   MOVE    "1"             TO  ONSKAKU-SET-PTID
                   MOVE    JOB-SRYYMD      TO  ONSKAKU-KAKUNINYMD
                                               ONSKAKU-KAKU-KAKUNINYMD
                   MOVE    "000000"        TO  ONSKAKU-KAKU-KAKUNINHMS 
      *************MOVE    "1"             TO  ONSKAKU-ORCA-SHOKAI-KBN
                   MOVE    QUAAPPREQ2-SHORI-TIME
                                           TO  ONSKAKU-SHORI-TIME 
                   MOVE    QUAAPPREQ2-HOSPCD  TO  ONSKAKU-HOSPCD
                   MOVE    QUAAPPREQ2-FILE-SIKIBETU
                                           TO  ONSKAKU-FILE-SIKIBETU
      *************MOVE    QUAAPPREQ2-SHOKAI-KBN
                   MOVE    "2"             TO  ONSKAKU-SHOKAI-KBN
                   MOVE    QUAAPPREQ2-RESULT-KBN
                                           TO  ONSKAKU-RESULT-KBN
                   MOVE    QUAAPPREQ2-ERR-CODE
                                           TO  ONSKAKU-ERR-CODE
                   MOVE    QUAAPPREQ2-ERR-MSG TO  ONSKAKU-ERR-MSG
                   MOVE    QUAAPPREQ2-CODE-TYPE
                                           TO  ONSKAKU-CODE-TYPE
      *                             
                   MOVE    QUAAPPREQ2-SHO-HKNJANUM (IDXX)
                                           TO  ONSKAKU-SHO-HKNJANUM
                   MOVE    QUAAPPREQ2-SHO-KIGO     (IDXX)
                                           TO  ONSKAKU-SHO-KIGO
                   MOVE    QUAAPPREQ2-SHO-NUM      (IDXX)
                                           TO  ONSKAKU-SHO-NUM
                   MOVE    QUAAPPREQ2-SHO-EDABAN   (IDXX)
                                           TO  ONSKAKU-SHO-EDABAN
                   MOVE    QUAAPPREQ2-SHO-BIRTHDAY (IDXX)
                                           TO  ONSKAKU-SHO-BIRTHDAY
                   MOVE    QUAAPPREQ2-RESULT       (IDXX)
                                           TO  ONSKAKU-RESULT
                   MOVE    QUAAPPREQ2-RESULT-CODE  (IDXX)
                                           TO  ONSKAKU-RESULT-CODE
                   MOVE    QUAAPPREQ2-RESULT-MSG   (IDXX)
                                           TO  ONSKAKU-RESULT-MSG
                   MOVE    QUAAPPREQ2-SIKAKU-YUKO  (IDXX)
                                           TO  ONSKAKU-SIKAKU-YUKO
      *
                   MOVE    QUAAPPREQ2-CARD-CLASS   (IDXX IDYY) 
                                           TO  ONSKAKU-CARD-CLASS 
                   IF    ( QUAAPPREQ2-SHO-HKNJANUM (IDXX) (1:2)
                                           =   "12"  )
                   AND   ( QUAAPPREQ2-SHO-HKNJANUM (IDXX) (7:2)
                                       NOT =   SPACE )
      *               負担者番号が医療扶助のとき
                       MOVE    "A1"            TO  ONSKAKU-CARD-CLASS 
                   END-IF
                   MOVE    QUAAPPREQ2-RES-HKNJANUM (IDXX IDYY)
                                           TO  ONSKAKU-RES-HKNJANUM
                   MOVE    WRK-QUAAPPREQ2-RES-KIGO (IDXX IDYY)
                                           TO  ONSKAKU-RES-KIGO
                   MOVE    WRK-QUAAPPREQ2-RES-NUM (IDXX IDYY)
                                           TO  ONSKAKU-RES-NUM 
                   MOVE    QUAAPPREQ2-RES-EDABAN (IDXX IDYY) 
                                           TO  ONSKAKU-RES-EDABAN 
                   MOVE    QUAAPPREQ2-RES-HONKZKKBN (IDXX IDYY) 
                                           TO  ONSKAKU-RES-HONKZKKBN 
                   MOVE    WRK-QUAAPPREQ2-RES-HIHKNJANAME (IDXX IDYY)
                                           TO  ONSKAKU-RES-HIHKNJANAME 
                   MOVE    WRK-QUAAPPREQ2-RES-NAME (IDXX IDYY)
                                           TO  ONSKAKU-RES-NAME 
                   MOVE    WRK-QUAAPPREQ2-RES-NAME-ETC (IDXX IDYY)
                                           TO  ONSKAKU-RES-NAME-ETC 
                   MOVE    WRK-QUAAPPREQ2-RES-KANANAME (IDXX IDYY)
                                           TO  ONSKAKU-RES-KANANAME 
                   MOVE    WRK-QUAAPPREQ2-RES-KANANAME-ETC (IDXX IDYY)
                                           TO  ONSKAKU-RES-KANANAME-ETC
                   MOVE    QUAAPPREQ2-RES-SEX1 (IDXX IDYY)
                                           TO  ONSKAKU-RES-SEX1 
                   MOVE    QUAAPPREQ2-RES-SEX2 (IDXX IDYY)
                                           TO  ONSKAKU-RES-SEX2 
                   MOVE    QUAAPPREQ2-RES-BIRTHDAY (IDXX IDYY)
                                           TO  ONSKAKU-RES-BIRTHDAY 
                   MOVE    WRK-QUAAPPREQ2-RES-ADRS (IDXX IDYY)
                                           TO  ONSKAKU-RES-ADRS 
                   MOVE    QUAAPPREQ2-RES-POST (IDXX IDYY)
                                           TO  ONSKAKU-RES-POST 
                   MOVE    QUAAPPREQ2-SKKGETYMD (IDXX IDYY)
                                           TO  ONSKAKU-SKKGETYMD 
                   MOVE    QUAAPPREQ2-TEKSTYMD (IDXX IDYY)
                                           TO  ONSKAKU-TEKSTYMD 
                   MOVE    QUAAPPREQ2-TEKEDYMD (IDXX IDYY)
                                           TO  ONSKAKU-TEKEDYMD 
                   MOVE    QUAAPPREQ2-FTNRATE (IDXX IDYY)
                                           TO  ONSKAKU-FTNRATE 
                   MOVE    QUAAPPREQ2-MISYUGAKU (IDXX IDYY)
                                           TO  ONSKAKU-MISYUGAKU 
                   MOVE    QUAAPPREQ2-LOSS-JIYU  (IDXX IDYY)
                                           TO  ONSKAKU-LOSS-JIYU 
                   MOVE    WRK-QUAAPPREQ2-HKNJANUM-NAME (IDXX IDYY)
                                           TO  ONSKAKU-HKNJANUM-NAME 
                   MOVE    QUAAPPREQ2-ELDER-SKKGETYMD (IDXX IDYY)
                                           TO  ONSKAKU-ELDER-SKKGETYMD 
                   MOVE    QUAAPPREQ2-ELDER-TEKSTYMD (IDXX IDYY)
                                           TO  ONSKAKU-ELDER-TEKSTYMD 
                   MOVE    QUAAPPREQ2-ELDER-TEKEDYMD (IDXX IDYY)
                                           TO  ONSKAKU-ELDER-TEKEDYMD 
                   MOVE    QUAAPPREQ2-ELDER-FTNRATE (IDXX IDYY)
                                           TO  ONSKAKU-ELDER-FTNRATE 
                   MOVE    QUAAPPREQ2-GENDO-SHOKBN (IDXX)
                                           TO  ONSKAKU-GENDO-SHOKBN 
                   MOVE    QUAAPPREQ2-GENDO-TEKKBN (IDXX)
                                           TO  ONSKAKU-GENDO-TEKKBN 
                   MOVE    QUAAPPREQ2-FILE-SIKIBETU
                                           TO  ONSKAKU-SIKIBETU 
                   MOVE    QUAAPPREQ2-SHOKAI-NUM (IDXX)
                                           TO  ONSKAKU-SHOKAI-NUM
      *                             
                   MOVE    SPA-OPID        TO  ONSKAKU-OPID
                   MOVE    SPA-TERMID      TO  ONSKAKU-TERMID
                   MOVE    SMCNDATE-YMD    TO  ONSKAKU-CREYMD
                   MOVE    SMCNDATE-HMS    TO  ONSKAKU-CREHMS
                   MOVE    SPA-OPID        TO  ONSKAKU-OPID
      *
                   MOVE    ONSKAKU-REC     TO  MCPDATA-REC
                   MOVE    "DBINSERT"      TO  MCP-FUNC
                   MOVE    "tbl_onshi_kaku"
                                           TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC      NOT =   ZERO 
                        MOVE   "E20"             TO  WRK-ERRCD
                        DISPLAY "TBL_ONSHI_KAKU INSERT ERR " MCP-RC 
                   END-IF
                 END-PERFORM
               END-PERFORM
           END-PERFORM
      *
           .        
       20021-SIKAKU-KAKUNIN-YYK-EXT.
           EXIT.
      *
      *****************************************************************
      *     資格確認レコード作成処理（訪問診療）
      *****************************************************************
       20021-SIKAKU-KAKUNIN-HOUMON-SEC SECTION.
      *
      *    総処理件数
           MOVE    ZERO            TO  WRK-ALL-CNT 
           MOVE    QUAAPPREQ2-OK-CNT
                                   TO  SNUM-INX
           CALL    "ORCSNUM"       USING
                                   ORCSNUMAREA
           IF      SNUM-RC         =   ZERO
               MOVE    SNUM-OUTNUM     TO  WRK-ALL-CNT
           END-IF
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       WRK-ALL-CNT
      *        全角変換処理
               PERFORM 20021-KANACHK-HENSYU-SEC
           END-PERFORM    
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       WRK-ALL-CNT
               INITIALIZE                  WRK-QUAAPPREQ2-PTID-AREA
                                           WRK-QUAAPPREQ2-RESULT-HKN
                                           WRK-QUAAPPREQ2-RESULT-SEH
                                           WRK-FUJYO-UUID
                                           WRK-KOH-UUID
      *        患者ＩＤ取得
               MOVE    IDXX            TO  IDX 
               IF      QUAAPPREQ2-SHOKAI-NUM (IDX)
                                   NOT =   SPACE
      *            照会番号から患者ＩＤ取得
                   PERFORM 20022-PTID-02-SEC
                   PERFORM VARYING IDYY    FROM    1   BY  1
                           UNTIL   IDYY    >       WRK-CONS-RESULT-MAX
                           OR      QUAAPPREQ2-CARD-CLASS (IDX IDYY)
                                           =       SPACE 
                       IF      QUAAPPREQ2-CARD-CLASS (IDX IDYY)
                                           =   "A1"
                           MOVE    IDYY        TO
                                               WRK-QUAAPPREQ2-RESULT-SEH
                       ELSE
                           MOVE    IDYY        TO
                                               WRK-QUAAPPREQ2-RESULT-HKN
                       END-IF
                   END-PERFORM 
               END-IF
               IF      WRK-QUAAPPREQ2-PTID (1)
                                       =   ZERO
      *            保険情報から患者ＩＤ取得 
                   PERFORM VARYING IDYY    FROM    1   BY  1
                           UNTIL   IDYY    >       WRK-CONS-RESULT-MAX
                           OR      QUAAPPREQ2-CARD-CLASS (IDX IDYY)
                                           =       SPACE 
                       IF      QUAAPPREQ2-CARD-CLASS (IDX IDYY)
                                           =   "A1"
                           MOVE    IDYY        TO
                                               WRK-QUAAPPREQ2-RESULT-SEH
                          PERFORM 20023-PTID-03-SEC
                       ELSE
                           MOVE    IDYY        TO
                                               WRK-QUAAPPREQ2-RESULT-HKN
                           PERFORM 20021-PTID-01-SEC
                       END-IF
                   END-PERFORM 
               END-IF
      *??
            DISPLAY "in PTID=" WRK-QUAAPPREQ2-PTID (1) 
              " " WRK-QUAAPPREQ2-PTID (2) 
              " HKN=" WRK-QUAAPPREQ2-RESULT-HKN
              " SEH=" WRK-QUAAPPREQ2-RESULT-SEH "#"
      *        複数返却時のＰＴＩＤの設定
               MOVE    WRK-QUAAPPREQ2-PTID-AREA
                                   TO  WRK-QUAAPPREQ2-PTID-NEW-AREA 
      *        併用のときPTIDが違うとき
               IF    ( QUAAPPREQ2-CARD-CLASS (IDXX 2)
                                   NOT =   SPACE )
               AND   ( WRK-QUAAPPREQ2-PTID (1)
                                   NOT =   WRK-QUAAPPREQ2-PTID (2) )
                   IF      WRK-QUAAPPREQ2-RESULT-HKN
                                       >   ZERO
      *                保険と医療扶助の併用のとき
                       IF      WRK-QUAAPPREQ2-N-PTID
                                           (WRK-QUAAPPREQ2-RESULT-HKN)
                                           =   ZERO
      *                    保険のＰＴＩＤが設定できなかったとき全てのＰＴＩＤを設定しない
                           INITIALIZE      WRK-QUAAPPREQ2-PTID-NEW-AREA
                       ELSE
      *                    医療扶助のＰＴＩＤが設定できなかったとき保険のＰＴＩＤを設定
                           IF      WRK-QUAAPPREQ2-N-PTID 
                                           (WRK-QUAAPPREQ2-RESULT-SEH)
                                               =   ZERO
                               MOVE    WRK-QUAAPPREQ2-N-PTID
                                           (WRK-QUAAPPREQ2-RESULT-HKN)
                                                   TO
                                           WRK-QUAAPPREQ2-N-PTID
                                           (WRK-QUAAPPREQ2-RESULT-SEH)
                           ELSE                            
      *                        医療扶助のＰＴＩＤが保険のＰＴＩＤと異なったとき全てのＰＴＩＤを設定しない
                               IF      WRK-QUAAPPREQ2-N-PTID
                                           (WRK-QUAAPPREQ2-RESULT-SEH)
                                                       NOT =
                                              WRK-QUAAPPREQ2-N-PTID
                                             (WRK-QUAAPPREQ2-RESULT-HKN)
                                   INITIALIZE
                                           WRK-QUAAPPREQ2-PTID-NEW-AREA
                              END-IF
                           END-IF
                       END-IF
                   END-IF
      *            資格確認から設定したＰＴＩＤと違うＰＴＩＤを設定したとき
      *            エラーコード設定
                   PERFORM VARYING IDYY    FROM    1   BY  1
                           UNTIL   IDYY    >       WRK-CONS-RESULT-MAX
                       IF      WRK-QUAAPPREQ2-PTID (IDYY)
                                       NOT =
                                           WRK-QUAAPPREQ2-N-PTID (IDYY)
                           MOVE    "ORCAE18"               TO
                                           WRK-QUAAPPREQ2-ERR-CODE(IDYY)
                           MOVE    WRK-QUAAPPREQ2-PTID (IDYY)  TO
                                           WRK-QUAAPPREQ2-ERR-PTID(IDYY)
                       END-IF
                   END-PERFORM
               END-IF
      *??
            DISPLAY "PTID=" WRK-QUAAPPREQ2-N-PTID (1) 
              " " WRK-QUAAPPREQ2-N-PTID (2) 
      *
      *        扶助UIDコード設定
               IF      WRK-QUAAPPREQ2-RESULT-SEH
                                       >   ZERO
      *            UIDコード取得処理
                   PERFORM 20022-UID-GET-SEC
                   MOVE    C-UID           TO  WRK-FUJYO-UUID
               END-IF
      *
               IF      WRK-QUAAPPREQ2-RESULT-HKN
                                       >   0
                   MOVE    WRK-QUAAPPREQ2-RESULT-HKN
                                       TO  IDYY
                   PERFORM 200211-SIKAKU-KAKUNIN-INS-SEC
               END-IF
               IF      WRK-QUAAPPREQ2-RESULT-SEH
                                       >   0
                   MOVE    WRK-QUAAPPREQ2-RESULT-SEH
                                       TO  IDYY
                   PERFORM 200211-SIKAKU-KAKUNIN-INS-SEC
               END-IF
           END-PERFORM    
      *
           .        
       20021-SIKAKU-KAKUNIN-HOUMON-EXT.
           EXIT.
      * 
      ******************************************************************
      *    資格確認レコード作成処理（訪問診療）
      ******************************************************************
       200211-SIKAKU-KAKUNIN-INS-SEC  SECTION.
      *
                   INITIALIZE                  ONSKAKU-REC
                   MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
      *            UIDコード取得処理
                   PERFORM 20022-UID-GET-SEC
                   MOVE    C-UID           TO  ONSKAKU-TBL-UUID
                   IF    ( QUAAPPREQ2-CARD-CLASS (IDXX 2)
                                       NOT =   SPACE )
      *************AND   ( WRK-QUAAPPREQ2-PTID (IDXX)
      *************                    NOT =   ZERO  ) 
                       MOVE    WRK-FUJYO-UUID  TO  ONSKAKU-FUJYO-UUID
      *###                                         ONSKAKU-FUJYO-UUID1
      *###             MOVE    "1"             TO  ONSKAKU-FUJYO-HEIYO
                       IF      QUAAPPREQ2-CARD-CLASS (IDXX IDYY)
                                               =   "A1"
                           MOVE    WRK-KOH-UUID
                                               TO  ONSKAKU-KOUHI-UUID
                       ELSE
                           MOVE    ONSKAKU-TBL-UUID
                                               TO  WRK-KOH-UUID
                       END-IF
                   ELSE
                       IF      QUAAPPREQ2-CARD-CLASS (IDXX IDYY)
                                               =   "A1"
                           MOVE    WRK-FUJYO-UUID
                                               TO  ONSKAKU-FUJYO-UUID
                       END-IF        
                   END-IF
                   MOVE    "2"             TO  ONSKAKU-PROCESS-CLASS
                   MOVE    "2"             TO  ONSKAKU-ONSHINRYO-KBN
                   MOVE    "03"            TO  ONSKAKU-KAKU-ENDFLG
                   MOVE    WRK-QUAAPPREQ2-N-PTID (IDYY)
                                           TO  ONSKAKU-PTID
                   MOVE    "1"             TO  ONSKAKU-SET-PTID
                   MOVE    QUAAPPREQ2-SHORI-TIME (1:8)
                                           TO  ONSKAKU-KAKUNINYMD
                                               ONSKAKU-KAKU-KAKUNINYMD
                   MOVE    QUAAPPREQ2-SHORI-TIME (9:6)
                                           TO  ONSKAKU-KAKU-KAKUNINHMS 
                   MOVE    QUAAPPREQ2-SHORI-TIME
                                           TO  ONSKAKU-SHORI-TIME 
                   MOVE    QUAAPPREQ2-HOSPCD  TO  ONSKAKU-HOSPCD
                   MOVE    QUAAPPREQ2-FILE-SIKIBETU
                                           TO  ONSKAKU-FILE-SIKIBETU
                   MOVE    QUAAPPREQ2-RESULT-KBN
                                           TO  ONSKAKU-RESULT-KBN
                   MOVE    QUAAPPREQ2-ERR-CODE
                                           TO  ONSKAKU-ERR-CODE
                   MOVE    QUAAPPREQ2-ERR-MSG TO  ONSKAKU-ERR-MSG
                   IF    (  ONSKAKU-ERR-CODE 
                                           =   SPACE )
                   AND   ( WRK-QUAAPPREQ2-ERR-CODE (IDXX)
                                       NOT =   SPACE )
                       MOVE    WRK-QUAAPPREQ2-ERR-CODE (IDXX)
                                               TO  WRK-ERR-CODE
                       MOVE    WRK-QUAAPPREQ2-ERR-PTID (IDX)
                                               TO  WRK-ERR-PTID
                   END-IF 
                   PERFORM 2002211-ONSKAKU-ERR-HENSYU-SEC
                   MOVE    QUAAPPREQ2-CODE-TYPE
                                           TO  ONSKAKU-CODE-TYPE
      *                             
                   MOVE    QUAAPPREQ2-SHO-HKNJANUM (IDXX)
                                           TO  ONSKAKU-SHO-HKNJANUM
                   MOVE    WRK-QUAAPPREQ2-SHO-KIGO     (IDXX)
                                           TO  ONSKAKU-SHO-KIGO
                   MOVE    WRK-QUAAPPREQ2-SHO-NUM      (IDXX)
                                           TO  ONSKAKU-SHO-NUM
                   MOVE    QUAAPPREQ2-SHO-EDABAN   (IDXX)
                                           TO  ONSKAKU-SHO-EDABAN
                   MOVE    QUAAPPREQ2-SHO-BIRTHDAY (IDXX)
                                           TO  ONSKAKU-SHO-BIRTHDAY
                   MOVE    QUAAPPREQ2-RESULT       (IDXX)
                                           TO  ONSKAKU-RESULT
                   MOVE    QUAAPPREQ2-RESULT-CODE  (IDXX)
                                           TO  ONSKAKU-RESULT-CODE
                   MOVE    QUAAPPREQ2-RESULT-MSG   (IDXX)
                                           TO  ONSKAKU-RESULT-MSG
                   MOVE    QUAAPPREQ2-SIKAKU-YUKO  (IDXX)
                                           TO  ONSKAKU-SIKAKU-YUKO
      *
                   MOVE    QUAAPPREQ2-CARD-CLASS   (IDXX IDYY) 
                                           TO  ONSKAKU-CARD-CLASS 
                   MOVE    QUAAPPREQ2-RES-HKNJANUM (IDXX IDYY)
                                           TO  ONSKAKU-RES-HKNJANUM
                   MOVE    WRK-QUAAPPREQ2-RES-KIGO (IDXX IDYY)
                                           TO  ONSKAKU-RES-KIGO
                   MOVE    WRK-QUAAPPREQ2-RES-NUM (IDXX IDYY)
                                           TO  ONSKAKU-RES-NUM 
                   MOVE    QUAAPPREQ2-RES-EDABAN (IDXX IDYY) 
                                           TO  ONSKAKU-RES-EDABAN 
                   MOVE    QUAAPPREQ2-RES-HONKZKKBN (IDXX IDYY) 
                                           TO  ONSKAKU-RES-HONKZKKBN 
                   MOVE    WRK-QUAAPPREQ2-RES-HIHKNJANAME (IDXX IDYY)
                                           TO  ONSKAKU-RES-HIHKNJANAME 
                   MOVE    WRK-QUAAPPREQ2-RES-NAME (IDXX IDYY)
                                           TO  ONSKAKU-RES-NAME 
                   MOVE    WRK-QUAAPPREQ2-RES-NAME-ETC (IDXX IDYY)
                                           TO  ONSKAKU-RES-NAME-ETC 
                   MOVE    WRK-QUAAPPREQ2-RES-KANANAME (IDXX IDYY)
                                           TO  ONSKAKU-RES-KANANAME 
                   MOVE    WRK-QUAAPPREQ2-RES-KANANAME-ETC (IDXX IDYY)
                                           TO  ONSKAKU-RES-KANANAME-ETC
                   MOVE    QUAAPPREQ2-RES-SEX1 (IDXX IDYY)
                                           TO  ONSKAKU-RES-SEX1 
                   MOVE    QUAAPPREQ2-RES-SEX2 (IDXX IDYY)
                                           TO  ONSKAKU-RES-SEX2 
                   MOVE    QUAAPPREQ2-RES-BIRTHDAY (IDXX IDYY)
                                           TO  ONSKAKU-RES-BIRTHDAY 
                   MOVE    WRK-QUAAPPREQ2-RES-ADRS (IDXX IDYY)
                                           TO  ONSKAKU-RES-ADRS 
                   MOVE    QUAAPPREQ2-RES-POST (IDXX IDYY)
                                           TO  ONSKAKU-RES-POST 
                   MOVE    QUAAPPREQ2-SKKGETYMD (IDXX IDYY)
                                           TO  ONSKAKU-SKKGETYMD 
                   IF      QUAAPPREQ2-CARD-CLASS (IDXX IDYY)
                                           =   "A1"
                       MOVE    QUAAPPREQ2-TEKSTYMD (IDXX IDYY)
                                           TO  ONSKAKU-KOH-TEKSTYMD (1)
                       MOVE    QUAAPPREQ2-TEKEDYMD (IDXX IDYY)
                                           TO  ONSKAKU-KOH-TEKEDYMD (1)
                   ELSE
                       MOVE    QUAAPPREQ2-TEKSTYMD (IDXX IDYY)
                                               TO  ONSKAKU-TEKSTYMD 
                       MOVE    QUAAPPREQ2-TEKEDYMD (IDXX IDYY)
                                               TO  ONSKAKU-TEKEDYMD
                   END-IF
                   MOVE    QUAAPPREQ2-FTNRATE (IDXX IDYY)
                                           TO  ONSKAKU-FTNRATE 
                   MOVE    QUAAPPREQ2-MISYUGAKU (IDXX IDYY)
                                           TO  ONSKAKU-MISYUGAKU 
                   MOVE    QUAAPPREQ2-LOSS-JIYU  (IDXX IDYY)
                                           TO  ONSKAKU-LOSS-JIYU 
                   MOVE    WRK-QUAAPPREQ2-HKNJANUM-NAME (IDXX IDYY)
                                           TO  ONSKAKU-HKNJANUM-NAME 
                   MOVE    QUAAPPREQ2-ELDER-SKKGETYMD (IDXX IDYY)
                                           TO  ONSKAKU-ELDER-SKKGETYMD 
                   MOVE    QUAAPPREQ2-ELDER-TEKSTYMD (IDXX IDYY)
                                           TO  ONSKAKU-ELDER-TEKSTYMD 
                   MOVE    QUAAPPREQ2-ELDER-TEKEDYMD (IDXX IDYY)
                                           TO  ONSKAKU-ELDER-TEKEDYMD 
                   MOVE    QUAAPPREQ2-ELDER-FTNRATE (IDXX IDYY)
                                           TO  ONSKAKU-ELDER-FTNRATE
                   MOVE    QUAAPPREQ2-HO-GENDO-DOUIFLG (IDXX IDYY)
                                           TO  ONSKAKU-SHO-DOUIFLG
                   MOVE    QUAAPPREQ2-HO-GENDO-TIME    (IDXX IDYY)
                                           TO  ONSKAKU-GENDO-TIME
      *************MOVE    QUAAPPREQ2-HO-GENDO-KIGEN   (IDXX IDYY)
      *************                        TO  ONSKAKU-GENDO-KIGEN
                   MOVE    QUAAPPREQ2-HO-GENDO-SHOKBN   (IDXX IDYY)
                                           TO  ONSKAKU-GENDO-SHOKBN
                   MOVE    QUAAPPREQ2-HO-GENDO-TEKKBN   (IDXX IDYY)
                                           TO  ONSKAKU-GENDO-TEKKBN
                   MOVE    QUAAPPREQ2-HO-GENDO-SKKGETYMD (IDXX IDYY)
                                           TO  ONSKAKU-GENDO-SKKGETYMD
                   MOVE    QUAAPPREQ2-HO-GENDO-TEKSTYMD (IDXX IDYY)
                                           TO  ONSKAKU-GENDO-TEKSTYMD
                   MOVE    QUAAPPREQ2-HO-GENDO-TEKEDYMD (IDXX IDYY)
                                           TO  ONSKAKU-GENDO-TEKEDYMD
                   MOVE    QUAAPPREQ2-HO-GENDO-SKJGNSTYMD (IDXX IDYY)
                                           TO  ONSKAKU-GENDO-SKJGNSTYMD
                   MOVE    QUAAPPREQ2-HO-SIKKAN-DOUIFLG (IDXX IDYY)
                                           TO  ONSKAKU-SIKKAN-DOUIFLG
                   MOVE    QUAAPPREQ2-HO-SIKKAN-TIME    (IDXX IDYY)
                                           TO  ONSKAKU-SIKKAN-TIME
      *************MOVE    QUAAPPREQ2-HO-SIKKAN-KIGEN   (IDXX IDYY)
      *************                        TO  ONSKAKU-SIKKAN-KIGEN
      *************MOVE    QUAAPPREQ2-HO-SIKKAN-RIYU    (IDXX IDYY)
      *************                        TO  ONSKAKU-SIKKAN-RIYU
                   PERFORM VARYING IDZZ    FROM    1   BY  1
                           UNTIL   IDZZ    >       3
                       MOVE    QUAAPPREQ2-HO-SIKKAN-SIPKBN
                                                   (IDXX IDYY IDZZ)
                                           TO  ONSKAKU-SIKKAN-SIPKBN
                                                               (IDZZ)
                       MOVE    QUAAPPREQ2-HO-SIKKAN-SKJGNSTYMD
                                                   (IDXX IDYY IDZZ)
                                           TO  ONSKAKU-SIKKAN-SKJGNSTYMD
                                                               (IDZZ)
                       MOVE    QUAAPPREQ2-HO-SIKKAN-TEKSTYMD
                                                   (IDXX IDYY IDZZ)
                                           TO  ONSKAKU-SIKKAN-TEKSTYMD
                                                               (IDZZ)
                       MOVE    QUAAPPREQ2-HO-SIKKAN-TEKEDYMD
                                                   (IDXX IDYY IDZZ)
                                           TO  ONSKAKU-SIKKAN-TEKEDYMD
                                                               (IDZZ)
                       MOVE    QUAAPPREQ2-HO-SIKKAN-FTNMONEY
                                                   (IDXX IDYY IDZZ)
                                           TO  ONSKAKU-SIKKAN-FTNMONEY
                                                               (IDZZ)
                   END-PERFORM
                   MOVE    QUAAPPREQ2-HO-KENSIN-DOUIFLG (IDXX IDYY)
                                           TO  ONSKAKU-KENSIN-DOUIFLG
                   MOVE    QUAAPPREQ2-HO-KENSIN-TIME    (IDXX IDYY)
                                           TO  ONSKAKU-KENSIN-TIME
                   MOVE    QUAAPPREQ2-HO-KENSIN-KIGENYMD  (IDXX IDYY)
                                           TO  ONSKAKU-KENSIN-KIGENYMD
      ************ MOVE    QUAAPPREQ2-HO-KENSIN-RIYU    (IDXX IDYY)
      ************                         TO  ONSKAKU-KENSIN-RIYU
                   MOVE    QUAAPPREQ2-HO-YAKUZAI-DOUIFLG  (IDXX IDYY)
                                           TO  ONSKAKU-YAKUZAI-DOUIFLG
                   MOVE    QUAAPPREQ2-HO-YAKUZAI-TIME   (IDXX IDYY)
                                           TO  ONSKAKU-YAKUZAI-TIME
                   MOVE    QUAAPPREQ2-HO-YAKUZAI-KIGEN  (IDXX IDYY)
                                           TO  ONSKAKU-YAKUZAI-KIGEN
      ************ MOVE    QUAAPPREQ2-HO-YAKUZAI-RIYU   (IDXX IDYY)
      ************                         TO  ONSKAKU-YAKUZAI-RIYU
                   MOVE    QUAAPPREQ2-HO-SHINRYO-DOUIFLG  (IDXX IDYY)
                                           TO  ONSKAKU-SHINRYO-DOUIFLG
                   MOVE    QUAAPPREQ2-HO-SHINRYO-TIME   (IDXX IDYY)
                                           TO  ONSKAKU-SHINRYO-TIME
                   MOVE    QUAAPPREQ2-HO-SHINRYO-KIGEN  (IDXX IDYY)
                                           TO  ONSKAKU-SHINRYO-KIGEN
      ************ MOVE    QUAAPPREQ2-HO-SHINRYO-RIYU   (IDXX IDYY)
      ************                         TO  ONSKAKU-SHINRYO-RIYU
                   MOVE    QUAAPPREQ2-HO-OPE-DOUIFLG    (IDXX IDYY)
                                           TO  ONSKAKU-OPE-DOUIFLG
                   MOVE    QUAAPPREQ2-HO-OPE-TIME       (IDXX IDYY)
                                           TO  ONSKAKU-OPE-TIME
                   MOVE    QUAAPPREQ2-HO-OPE-KIGEN      (IDXX IDYY)
                                           TO  ONSKAKU-OPE-KIGEN
      ************ MOVE    QUAAPPREQ2-HO-OPE-RIYU       (IDXX IDYY)
      ************                         TO  ONSKAKU-OPE-RIYU
                   MOVE    QUAAPPREQ2-FILE-SIKIBETU
                                           TO  ONSKAKU-SIKIBETU 
                   MOVE    QUAAPPREQ2-SHOKAI-NUM (IDXX)
                                           TO  ONSKAKU-SHOKAI-NUM
                   MOVE    QUAAPPREQ2-PROCESS-CLASS
                                           TO  ONSKAKU-PROCESS-CLASS
                   MOVE    QUAAPPREQ2-HO-ONSHI-CREYMD (IDXX IDYY)
                                           TO  ONSKAKU-ONSHI-UPYMD
      *                             
                   MOVE    SPA-OPID        TO  ONSKAKU-OPID
                   MOVE    SPA-TERMID      TO  ONSKAKU-TERMID
                   MOVE    SMCNDATE-YMD    TO  ONSKAKU-CREYMD
                   MOVE    SMCNDATE-HMS    TO  ONSKAKU-CREHMS
                   MOVE    SPA-OPID        TO  ONSKAKU-OPID
      *
                   MOVE    ONSKAKU-REC     TO  MCPDATA-REC
                   MOVE    "DBINSERT"      TO  MCP-FUNC
                   MOVE    "tbl_onshi_kaku"
                                           TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC      NOT =   ZERO 
                       MOVE   "E20"            TO  WRK-ERRCD
                       DISPLAY "TBL_ONSHI_KAKU INSERT ERR " MCP-RC
                       GO  TO  200211-SIKAKU-KAKUNIN-INS-EXT 
                   END-IF
      *             
                   IF      ONSKAKU-CARD-CLASS      =   "A1"
      *                医療扶助交付番号格納レコード作成処理
                       PERFORM 20021-ONSHI-AIDLST-SYORI-SEC
                       IF      MCP-RC      NOT =   ZERO 
                            MOVE   "E21"           TO  WRK-ERRCD
                            DISPLAY "TBL_ONSHI_AIDLST INSERT ERR "
                                                         MCP-RC 
                           GO  TO  200211-SIKAKU-KAKUNIN-INS-EXT 
                       END-IF
                   END-IF
      *
      *            資格確認訪問診療レコード作成処理
      *            （併用の生保は作成しない）
                   IF    ( QUAAPPREQ2-CARD-CLASS (IDXX 2)
                                       NOT =   SPACE )
                   AND   ( QUAAPPREQ2-CARD-CLASS (IDXX IDYY)
                                           =   "A1"  )
                       CONTINUE
                   ELSE                            
                       PERFORM 20021-ONSHI-HOUMON-SYORI-SEC
                       IF      MCP-RC      NOT =   ZERO 
                           MOVE   "E22"           TO  WRK-ERRCD
                           DISPLAY "TBL_ONSHI_HOUMON INSERT ERR " MCP-RC
                           GO  TO  200211-SIKAKU-KAKUNIN-INS-EXT 
                       END-IF
                   END-IF    
      *
           .        
       200211-SIKAKU-KAKUNIN-INS-EXT.
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
      *****************************************************************
      *     顔認証資格確認患者ＩＤ取得（照会番号の設定がない場合）
      *     医療扶助以外
      *****************************************************************
       20021-PTID-01-SEC  SECTION.
      *
      *    保険情報から患者保険情報を検索
           INITIALIZE                  WRK-PTHKNINF-AREA
      *         
           INITIALIZE                  PTHKNINF-REC
           MOVE    SPA-HOSPNUM     TO  PTHKN-HOSPNUM
           MOVE    QUAAPPREQ2-RES-HKNJANUM (IDX IDYY)
                                   TO  PTHKN-HKNJANUM
           MOVE    WRK-QUAAPPREQ2-RES-KIGO (IDX IDYY) 
                                   TO  PTHKN-KIGO
           MOVE    WRK-QUAAPPREQ2-RES-NUM  (IDX IDYY)
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
                       IF      QUAAPPREQ2-RES-EDABAN   (IDX IDYY)
                                           =   WRK-PTHKN-EDABAN (IDY)
                           MOVE    IDY             TO  WRK-PTHKNINF-OK
                       END-IF
                   END-IF
               END-PERFORM
      *
               IF      WRK-PTHKNINF-OK >   ZERO
      *            枝番まで一致した場合
                   MOVE    WRK-PTHKN-PTID (WRK-PTHKNINF-OK)
                                           TO  WRK-QUAAPPREQ2-PTID
                                                               (IDYY)
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
      *                        IF    (  QUAAPPREQ2-RES-SEX1     (IDX)
      *                                            =   PTINF-SEX      )
      *                        AND   (  QUAAPPREQ2-RES-BIRTHDAY (IDX)
      *                                            =   PTINF-BIRTHDAY )
      *                            IF    ( WRK-QUAAPPREQ2-RES-KANANAME
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
      *     顔認証資格確認患者ＩＤ取得（照会番号の設定がある場合）
      *****************************************************************
       20022-PTID-02-SEC  SECTION.
      *
      *    照会番号から患者ＩＤ取得
           INITIALIZE                  ORCSPTNUMAREA
           MOVE    QUAAPPREQ2-SHOKAI-NUM (IDX)
                                  TO  SPTNUM-PTNUM
           CALL    "ORCSPTNUM"    USING   ORCSPTNUMAREA
                                          SPA-AREA
           IF      SPTNUM-RC       =   00
               PERFORM VARYING IDYY    FROM    1   BY  1
                       UNTIL   IDYY    >       WRK-CONS-RESULT-MAX
                   MOVE    SPTNUM-PTID TO  WRK-QUAAPPREQ2-PTID (IDYY)
               END-PERFORM
           ELSE
               DISPLAY "SHOKAI-NUM ERR " QUAAPPREQ2-SHOKAI-NUM (IDX)
           END-IF
      *
           .
       20022-PTID-02-EXT.
           EXIT.
      *
      *****************************************************************
      *     顔認証資格確認登録、更新処理
      *     医療扶助
      *****************************************************************
       20023-PTID-03-SEC  SECTION.
      * 
      *    公費情報から患者公費険情報を検索
           INITIALIZE                  WRK-PTKOHINF-AREA
      *         
           INITIALIZE                  PTKOHINF-REC
           MOVE    SPA-HOSPNUM     TO  PTKOH-HOSPNUM
           MOVE    QUAAPPREQ2-RES-HKNJANUM (IDX IDYY)
                                   TO  PTKOH-FTNJANUM
           MOVE    WRK-QUAAPPREQ2-RES-NUM  (IDX IDYY)
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
               ADD     1               TO  WRK-PTKOHINF-MAX
               MOVE    PTKOH-PTID      TO  WRK-PTKOH-PTID
                                               (WRK-PTKOHINF-MAX)
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
      *    患者公費情報から検索できなかったとき、月代り受給者番号を検索
           IF      WRK-PTKOHINF-MAX    =   ZERO
               INITIALIZE                  MONTHLYNUM-REC
               MOVE    SPA-HOSPNUM     TO  MONTHLYNUM-HOSPNUM
               MOVE    QUAAPPREQ2-RES-HKNJANUM (IDX IDYY)
                                       TO  MONTHLYNUM-XXFTNJANUM
               MOVE    WRK-QUAAPPREQ2-RES-NUM  (IDX IDYY)
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
                   MOVE    "tbl_monthlynum"
                                          TO  MCP-TABLE
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
                   MOVE    "tbl_monthlynum"
                                          TO  MCP-TABLE
                   MOVE    "key6"         TO  MCP-PATHNAME
                   PERFORM 900-MONTHLYNUM-READ-N-SEC
               END-PERFORM
      *
               MOVE    "tbl_monthlynum"
                                      TO  MCP-TABLE
               MOVE    "key6"         TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
      *
           IF      WRK-PTKOHINF-MAX    =   1
               MOVE    WRK-PTKOH-PTID (1)  TO  WRK-QUAAPPREQ2-PTID
                                                           (IDYY)
           END-IF
      *
           .
       20023-PTID-03-EXT.
           EXIT.
      *
      ******************************************************************
      *    医療扶助交付番号格納レコード作成処理
      ******************************************************************
       20021-ONSHI-AIDLST-SYORI-SEC   SECTION.
      *
           MOVE    ZERO            TO  WRK-RENNUM
      *     
           PERFORM VARYING IDZZ    FROM    1   BY  1
                   UNTIL   IDZZ    >       4
                   OR      QUAAPPREQ2-HO-FUJYO-KOFUNUM (IDXX IDYY IDZZ)  
                                   =   SPACE       
      *        医療券で、入院・外来
               IF    ( QUAAPPREQ2-HO-TICKET-TYPE  (IDXX IDYY IDZZ)
                                       =   "01"            )
               AND   ( QUAAPPREQ2-HO-MEDICAL-TYPE  (IDXX IDYY IDZZ)
                                       =   "01"    OR  "02")
                   INITIALIZE                  ONSHI-AIDLST-REC
                   MOVE    SPA-HOSPNUM     TO  ONS-AIDLST-HOSPNUM
                   MOVE    ONSKAKU-PTID    TO  ONS-AIDLST-PTID
      *            UIDコード取得処理
                   PERFORM 20022-UID-GET-SEC
                   MOVE    C-UID           TO  ONS-AIDLST-TBL-UUID
      *            FUJYO_UUID
                   MOVE    ONSKAKU-FUJYO-UUID
                                           TO  ONS-AIDLST-FUJYO-UUID
      *            負担者番号
                   MOVE    QUAAPPREQ2-RES-HKNJANUM (IDXX IDYY)
                                           TO  ONS-AIDLST-FTNJANUM
      *            受給者番号
                   MOVE    WRK-QUAAPPREQ2-RES-NUM (IDXX IDYY)
                                           TO  ONS-AIDLST-JKYSNUM
      *            連番
                   ADD     1               TO  WRK-RENNUM
                   MOVE    WRK-RENNUM      TO  ONS-AIDLST-RENNUM
      *            交付番号 (2024-12 訪問のみ)
                   MOVE    QUAAPPREQ2-HO-FUJYO-KOFUNUM (IDXX IDYY IDZZ)
                                           TO  WRK-HENKAN-IN
                   PERFORM 803-HANKAKU-HEN-SEC
                   MOVE    WRK-HENKAN-OT   TO  ONS-AIDLST-KOFUNUM
      *            診療年月 (2024-12 訪問のみ)
                   MOVE    QUAAPPREQ2-HO-FUJYO-SRYYM (IDXX IDYY IDZZ)
                                           TO  ONS-AIDLST-SRYYM
      *            診療別 (2024-12 訪問のみ)
                   MOVE    QUAAPPREQ2-HO-MEDICAL-TYPE (IDXX IDYY IDZZ)
                                           TO  ONS-AIDLST-MEDICAL-TYPE
      *
                   MOVE    SPA-OPID        TO  ONS-AIDLST-OPID
                   MOVE    SMCNDATE-YMD    TO  ONS-AIDLST-CREYMD
                   MOVE    SMCNDATE-HMS    TO  ONS-AIDLST-CREHMS
                   MOVE    SMCNDATE-YMD    TO  ONS-AIDLST-UPYMD
                   MOVE    SMCNDATE-HMS    TO  ONS-AIDLST-UPHMS
      *
                   MOVE    ONSHI-AIDLST-REC
                                           TO  MCPDATA-REC
                   MOVE    "DBINSERT"      TO  MCP-FUNC
                   MOVE    "tbl_onshi_aidlst"
                                           TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"     USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               END-IF
           END-PERFORM
            .
       20021-ONSHI-AIDLST-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    資格確認訪問診療レコード作成処理
      *****************************************************************
       20021-ONSHI-HOUMON-SYORI-SEC    SECTION.
      *
           IF      ONSKAKU-PTID    >   0
               INITIALIZE                  ONSHI-HOUMON-REC
               MOVE    SPA-HOSPNUM     TO  ONS-HOUMON-HOSPNUM
               MOVE    ONSKAKU-PTID    TO  ONS-HOUMON-PTID
               MOVE    "2"             TO  ONS-HOUMON-ONSHINRYO-KBN
               MOVE    ONSHI-HOUMON-REC
                                       TO  MCPDATA-REC
               MOVE    "key4"          TO  WRK-MCP-PATHNAME
               PERFORM 900-ONSHI-HOUMON-READ-SEC
           ELSE
      *        PTIDが設定できなかったとき     
               IF      QUAAPPREQ2-CARD-CLASS (IDXX IDYY)
                                       =   "A1"
                   IF      WRK-KOH-UUID    =   SPACE
                       MOVE    1               TO  FLG-ONSHI-HOUMON
                   ELSE
                       INITIALIZE                  ONSHI-HOUMON-REC    
                       MOVE    SPA-HOSPNUM     TO  ONS-HOUMON-HOSPNUM
                       MOVE    WRK-KOH-UUID    TO  ONS-HOUMON-TBL-UUID
                       MOVE    ONSHI-HOUMON-REC
                                               TO  MCPDATA-REC
                       MOVE    "key"           TO  WRK-MCP-PATHNAME
                       PERFORM 900-ONSHI-HOUMON-READ-SEC
                   END-IF    
               ELSE    
                   MOVE    1               TO  FLG-ONSHI-HOUMON
               END-IF
           END-IF         
      *
           IF      FLG-ONSHI-HOUMON    =   1
               INITIALIZE                  ONSHI-HOUMON-REC    
               MOVE    SPA-HOSPNUM     TO  ONS-HOUMON-HOSPNUM
               MOVE    ONSKAKU-TBL-UUID
                                       TO  ONS-HOUMON-TBL-UUID
               MOVE    ONSKAKU-PTID    TO  ONS-HOUMON-PTID
               MOVE    "2"             TO  ONS-HOUMON-ONSHINRYO-KBN
           END-IF
           MOVE    SPACE           TO  ONS-HOUMON-SHOKAI-KBN
           IF    ( QUAAPPREQ2-CARD-CLASS (IDXX IDYY) 
                                   =   "A1"  )
      *        医療扶助のとき
               MOVE    QUAAPPREQ2-RES-HKNJANUM (IDXX IDYY)
                                           TO  ONS-HOUMON-FTNJANUM
               MOVE    WRK-QUAAPPREQ2-RES-NUM (IDXX IDYY)
                                           TO  ONS-HOUMON-JKYSNUM
            ELSE
               MOVE    QUAAPPREQ2-RES-HKNJANUM (IDXX IDYY)
                                           TO  ONS-HOUMON-HKNJANUM
               MOVE    WRK-QUAAPPREQ2-RES-KIGO (IDXX IDYY)
                                           TO  ONS-HOUMON-KIGO
               MOVE    WRK-QUAAPPREQ2-RES-NUM  (IDXX IDYY)
                                           TO  ONS-HOUMON-NUM
               MOVE    QUAAPPREQ2-RES-EDABAN   (IDXX IDYY)
                                           TO  ONS-HOUMON-EDABAN
           END-IF
           MOVE    QUAAPPREQ2-HO-DOUI-STARYMD
                                   TO  ONS-HOUMON-DOUI-STARTYMD
           MOVE    QUAAPPREQ2-HO-DOUI-ENDYMD
                                   TO  ONS-HOUMON-DOUI-ENDYMD
      *
           MOVE    ONSKAKU-KAKU-KAKUNINYMD
                                   TO  ONS-HOUMON-KAKU-KAKUNINYMD
           MOVE    ONSKAKU-KAKU-KAKUNINHMS
                                   TO  ONS-HOUMON-KAKU-KAKUNINHMS
           MOVE    ONSKAKU-SHO-DOUIFLG
                                   TO  ONS-HOUMON-GENDO-SHUTOKUFLG
           MOVE    ONSKAKU-SIKKAN-DOUIFLG
                                   TO  ONS-HOUMON-SIKKAN-SHUTOKUFLG
           MOVE    ONSKAKU-KENSIN-DOUIFLG
                                   TO  ONS-HOUMON-KENSHIN-DOUIFLG
           MOVE    ONSKAKU-YAKUZAI-DOUIFLG
                                   TO  ONS-HOUMON-YAKUZAI-DOUIFLG
           MOVE    ONSKAKU-SHINRYO-DOUIFLG
                                   TO  ONS-HOUMON-SHINRYO-DOUIFLG
           MOVE    ONSKAKU-OPE-DOUIFLG
                                   TO  ONS-HOUMON-OPE-DOUIFLG
      *
      *    MOVE                    TO  ONS-HOUMON-TERMID 
           MOVE    SPA-OPID        TO  ONS-HOUMON-OPID
      * 
           IF      FLG-ONSHI-HOUMON    =   1
               MOVE    SMCNDATE-YMD    TO  ONS-HOUMON-CREYMD
               MOVE    SMCNDATE-HMS    TO  ONS-HOUMON-CREHMS
               MOVE    SMCNDATE-YMD    TO  ONS-HOUMON-UPYMD
               MOVE    SMCNDATE-HMS    TO  ONS-HOUMON-UPHMS
      *
               MOVE    ONSHI-HOUMON-REC
                                       TO  MCPDATA-REC
               MOVE    "DBINSERT"      TO  MCP-FUNC
               MOVE    "tbl_onshi_houmon"
                                       TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           ELSE    
               MOVE    SMCNDATE-YMD    TO  ONS-HOUMON-UPYMD
               MOVE    SMCNDATE-HMS    TO  ONS-HOUMON-UPHMS
      *
               MOVE    ONSHI-HOUMON-REC
                                       TO  MCPDATA-REC
               MOVE    "DBUPDATE"      TO  MCP-FUNC
               MOVE    "tbl_onshi_houmon"
                                       TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           END-IF
           .
       20021-ONSHI-HOUMON-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       20021-KANACHK-HENSYU-SEC    SECTION.
      * 
           MOVE    QUAAPPREQ2-SHO-KIGO (IDXX)
                                   TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-QUAAPPREQ2-SHO-KIGO (IDXX)
           IF    ( QUAAPPREQ2-SHO-HKNJANUM (IDXX) (1:2)
                                   =   "12"  )
           AND   ( QUAAPPREQ2-SHO-HKNJANUM (IDXX) (7:2)
                               NOT =   SPACE )
      *        負担者番号が医療扶助のとき番号は半角のまま
               MOVE    QUAAPPREQ2-SHO-NUM (IDXX)
                                       TO  WRK-QUAAPPREQ2-SHO-NUM (IDXX)
           ELSE
               MOVE    QUAAPPREQ2-SHO-NUM (IDXX)
                                       TO  WRK-KANACHK-MAE-INPUT
               PERFORM 800-KANACHK-SEC
               MOVE    WRK-KANACHK-OUT-INPUT
                                       TO  WRK-QUAAPPREQ2-SHO-NUM (IDXX)
           END-IF
      *
           PERFORM VARYING IDYY    FROM    1   BY  1
                   UNTIL   IDYY    >       WRK-CONS-RESULT-MAX
                   OR      QUAAPPREQ2-CARD-CLASS (IDXX IDYY)
                                   =       SPACE 
               MOVE    QUAAPPREQ2-RES-KIGO (IDXX IDYY)
                                   TO  WRK-KANACHK-MAE-INPUT
               PERFORM 800-KANACHK-SEC
               MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-QUAAPPREQ2-RES-KIGO
                                                             (IDXX IDYY)
               IF      QUAAPPREQ2-CARD-CLASS (IDXX IDYY)
                                   =   "A1"
                   MOVE    QUAAPPREQ2-RES-NUM (IDXX IDYY)
                                       TO  WRK-QUAAPPREQ2-RES-NUM
                                                             (IDXX IDYY)
               ELSE                            
                   MOVE    QUAAPPREQ2-RES-NUM (IDXX IDYY)
                                       TO    WRK-KANACHK-MAE-INPUT
                   PERFORM 800-KANACHK-SEC
                   MOVE    WRK-KANACHK-OUT-INPUT
                                       TO  WRK-QUAAPPREQ2-RES-NUM
                                                             (IDXX IDYY)
               END-IF
               MOVE    QUAAPPREQ2-RES-HIHKNJANAME (IDXX IDYY)
                                   TO  WRK-KANACHK-MAE-INPUT
               MOVE    200             TO  WRK-KANACHK-LEN
               PERFORM 801-KANACHK2-SEC                       
               MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-QUAAPPREQ2-RES-HIHKNJANAME
                                                             (IDXX IDYY)
               MOVE    QUAAPPREQ2-RES-NAME (IDXX IDYY)
                                   TO  WRK-KANACHK-MAE-INPUT
               MOVE    200         TO  WRK-KANACHK-LEN
               PERFORM 801-KANACHK2-SEC                       
               MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-QUAAPPREQ2-RES-NAME
                                                             (IDXX IDYY)
                MOVE    QUAAPPREQ2-RES-NAME-ETC (IDXX IDYY)
                                   TO  WRK-KANACHK-MAE-INPUT
               MOVE    200         TO  WRK-KANACHK-LEN
               PERFORM 801-KANACHK2-SEC                       
               MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-QUAAPPREQ2-RES-NAME-ETC
                                                             (IDXX IDYY)
               MOVE    QUAAPPREQ2-RES-KANANAME (IDXX IDYY)
                                   TO    WRK-KANACHK-MAE-INPUT
               PERFORM 800-KANACHK-SEC
               MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-QUAAPPREQ2-RES-KANANAME
                                                             (IDXX IDYY)
               MOVE    QUAAPPREQ2-RES-KANANAME-ETC (IDXX IDYY)
                                   TO    WRK-KANACHK-MAE-INPUT
               PERFORM 800-KANACHK-SEC
               MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-QUAAPPREQ2-RES-KANANAME-ETC
                                                             (IDXX IDYY)
               MOVE    QUAAPPREQ2-RES-ADRS (IDXX IDYY)
                                   TO    WRK-KANACHK-MAE-INPUT
               PERFORM 800-KANACHK-SEC
               MOVE    1               TO  IDZ
               PERFORM VARYING IDZZ    FROM    1  BY  2
                       UNTIL   IDZZ    >       250
                       OR      WRK-KANACHK-OUT-INPUT (IDZZ:)
                                       =       SPACE
                   IF      WRK-KANACHK-OUT-INPUT (IDZZ:2)
                                       NOT =   "　"
                       MOVE    WRK-KANACHK-OUT-INPUT (IDZZ:2)
                                       TO  WRK-QUAAPPREQ2-RES-ADRS
                                                   (IDXX IDYY)(IDZ:2)
                       ADD     2       TO  IDZ
                   END-IF
               END-PERFORM
               MOVE    QUAAPPREQ2-HKNJANUM-NAME (IDXX IDYY)
                                   TO    WRK-KANACHK-MAE-INPUT
               PERFORM 800-KANACHK-SEC
               MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-QUAAPPREQ2-HKNJANUM-NAME
                                                             (IDXX IDYY)
           END-PERFORM
      *
           .        
       20021-KANACHK-HENSYU-EXT.
           EXIT.
      * 
      ******************************************************************
      *    UIDコード取得処理
      ******************************************************************
       20022-UID-GET-SEC     SECTION.
      *
      *    UIDコード取得処理
           INITIALIZE                  UIDIO-AREA
           CALL    "orcuidset"         USING
                                       ST
                                       UIDIO-AREA
           .
       20022-UID-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理更新処理
      *****************************************************************
       2003-JOBKANRI-UPDATE-SEC  SECTION.
      *
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           IF      QUAAPPREQ2-PROCESS-CLASS
                                   =   "2"
               MOVE    WRK-CONS-JOB-SHELLID1
                                       TO  JOB-SHELLID
           ELSE
               MOVE    WRK-CONS-JOB-SHELLID
                                       TO  JOB-SHELLID
           END-IF                            
           MOVE    QUAAPPREQ2-FILE-SIKIBETU (1:36)
                                   TO  JOB-KARTE-UID
           PERFORM 900-JOBKANRI-READ-SEC
           IF      FLG-JOBKANRI    =   1
               MOVE    "E14"           TO  WRK-ERRCD
           ELSE
      *        訪問診療(同意一括)のとき
               IF      QUAAPPREQ2-PROCESS-CLASS
                                       =   "2"
                   MOVE    "0003"          TO  JOB-ERRCD
               END-IF
               MOVE    QUAAPPREQ2-RESULT-KBN
                                       TO  JOB-SYOKBN
               INITIALIZE                  ORCSNUMAREA
               MOVE    QUAAPPREQ2-ALL-CNT
                                       TO  SNUM-INX
               CALL    "ORCSNUM"       USING
                                       ORCSNUMAREA
               IF      SNUM-RC         =   ZERO
                   MOVE    SNUM-OUTNUM     TO  JOB-UPDCNT
               END-IF    
               INITIALIZE                  ORCSNUMAREA
               MOVE    QUAAPPREQ2-OK-CNT
                                       TO  SNUM-INX
               CALL    "ORCSNUM"       USING
                                       ORCSNUMAREA
               IF      SNUM-RC         =   ZERO
                   MOVE    SNUM-OUTNUM     TO  JOB-UPDCNT1
               END-IF
               INITIALIZE                  ORCSNUMAREA
               MOVE    QUAAPPREQ2-ERR-CNT
                                       TO  SNUM-INX
               CALL    "ORCSNUM"       USING
                                       ORCSNUMAREA
               IF      SNUM-RC         =   ZERO
                   MOVE    SNUM-OUTNUM     TO  JOB-UPDCNT2
               END-IF
               MOVE    JOBKANRI-REC    TO  MCPDATA-REC
               MOVE    "DBUPDATE"      TO  MCP-FUNC
               MOVE    "tbl_jobkanri"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC          >   ZERO
                   DISPLAY "JOBKANRI UPDATE ERR=" MCP-RC
                   MOVE    "E23"           TO  WRK-ERRCD
               END-IF
           END-IF
           .
       2003-JOBKANRI-UPDATE-EXT.
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
      *     半角変換処理
      *****************************************************************
       803-HANKAKU-HEN-SEC             SECTION.
      *
           INITIALIZE                  ORCSHANKCHGAREA
           MOVE    "1"                 TO  HANKCHG-SYORI
           MOVE    "1"                 TO  HANKCHG-SYORIKBN
           MOVE     WRK-HENKAN-IN      TO  HANKCHG-MAE-INPUT
           CALL    "ORCSHANKCHG"       USING   ORCSHANKCHGAREA
           IF    ( KANACHK-RC          =   ZERO   )
               MOVE    HANKCHG-OUT-INPUT   TO  WRK-HENKAN-OT
           ELSE
      *        英数値以外がある
               MOVE    WRK-HENKAN-IN       TO  WRK-HENKAN-OT
           END-IF
           GO  TO  803-HANKAKU-HEN-EXT
      *
           MOVE    ZERO                TO  IDH2
           MOVE    SPACE               TO  WRK-HENKAN-OT
           MOVE    ZERO                TO  FLG-HENERR
           MOVE    ZERO                TO  FLG-ARI
      *
           PERFORM VARYING IDH1    FROM    1   BY  2
                   UNTIL  (IDH1        >   30    )
                   OR     (WRK-HENKAN-IN (IDX:) 
                                       =   SPACE)
                   OR     (IDH2        >=  15    )
               SET     TBL-IDX             TO  1
               SEARCH  TBL-HENKAN-OCC      VARYING   TBL-IDX
                   AT  END
                       MOVE    ZERO             TO  FLG-ARI
                   WHEN    WRK-HENKAN-IN (IDH1:2)   =   TBL-ZEN
                                                          (TBL-IDX)
                       ADD     1               TO  IDH2
                       MOVE    TBL-HALF  (TBL-IDX)
                                               TO  WRK-HENKAN-OT
                                                            (IDH2:1)
                       MOVE    1               TO  FLG-ARI
               END-SEARCH
                IF      FLG-ARI             =   ZERO
                   EVALUATE   WRK-HENKAN-IN (IDH1:2)
      *                A8 A1
                       WHEN    "─"
      *                A1 DD
                       WHEN    "?"
      *                A1 BE
                       WHEN    "‐"
      *                A1 BD
                       WHEN    "―"
      *                A1 BC
                       WHEN    "ー"
                           ADD     1           TO  IDH2
                           MOVE    "-"         TO  WRK-HENKAN-OT
                                                            (IDH2:1)
                           MOVE    1           TO  FLG-ARI 
                       WHEN    "　"
                           ADD     1           TO  IDH2
                           MOVE    SPACE       TO  WRK-HENKAN-OT
                                                            (IDH2:1)
                           MOVE    1           TO  FLG-ARI 
                   END-EVALUATE
               END-IF
      *
               IF      FLG-ARI             =   ZERO
      *            対象なし
                   ADD     1               TO  IDH2
                   MOVE    "?"             TO  WRK-HENKAN-OT
                                                            (IDH2:1)
               END-IF
           END-PERFORM
           .
       803-HANKAKU-HEN-EXT.
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
               WHEN    "E11"
                   MOVE    "最終予約番号の設定に誤りがあります"
                                                   TO  WRK-ERRMSG
               WHEN    "E12"
                   MOVE    "処理区分の設定に誤りがあります"
                                                   TO  WRK-ERRMSG
               WHEN    "E13"
                   MOVE    "ＵＩＤが未設定です"    TO  WRK-ERRMSG
               WHEN    "E14"
                   MOVE    "ＵＩＤに一致する処理がありません"
                                                   TO  WRK-ERRMSG
               WHEN    "E15"
                   MOVE    "リクエストが空です。"  TO  WRK-ERRMSG
               WHEN    "E20"
                   MOVE    "資格確認レコードの追加ができませんでした。"
                                                   TO  WRK-ERRMSG
               WHEN    "E21"
                    MOVE
                "医療扶助交付番号格納レコードの追加ができませんでした。"
                                                   TO  WRK-ERRMSG
               WHEN    "E22"
                   MOVE
                   "資格確認訪問診療レコードの追加ができませんでした。"
                                                   TO  WRK-ERRMSG
               WHEN    "E23"
                   MOVE    "ジョブ管理の更新ができませんでした。"
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
      *    システム管理マスター読込処理
      *****************************************************************
       900-SYSKANRI-READ-SEC       SECTION.
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
      *    ジョブ管理マスター読込処理
      *****************************************************************
       900-JOBKANRI-READ-SEC       SECTION.
      * 
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jobkanri"      TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  JOBKANRI-REC
                   MOVE    ZERO                TO  FLG-JOBKANRI
               ELSE
                   INITIALIZE                      JOBKANRI-REC
                   MOVE    1                   TO  FLG-JOBKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-JOBKANRI
           END-IF
      *
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-JOBKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    予約マスター読込処理
      *****************************************************************
       900-YYK-READ-N-SEC          SECTION.
      * 
           MOVE    "tbl_yyk"       TO  MCP-TABLE
           MOVE    "key13"         TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  YYK-REC
               MOVE    ZERO                TO  FLG-YYK
           ELSE
               INITIALIZE                      YYK-REC
               MOVE    1                   TO  FLG-YYK
           END-IF
           .
       900-YYK-READ-N-EXT.
           EXIT.
      *
      ******************************************************************
      *    患者保険マスター読込処理
      ******************************************************************
       900-PTHKNINF-READ-SEC     SECTION.
      *
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
           MOVE    "tbl_pthkninf"  TO  MCP-TABLE
           MOVE    "key8"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
          .
       900-PTHKNINF-READ-EXT.
           EXIT.
      *
      ******************************************************************
      *    患者保険マスター読込処理
      ******************************************************************
       900-PTHKNINF-READ-N-SEC     SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTHKNINF-REC
      *         
               IF      PTHKN-HKNNUM (1:1)
                                       NOT =   "0"
                   GO  TO  900-PTHKNINF-READ-N-SEC
               END-IF
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
      *    患者マスター読込処理
      ******************************************************************
       900-PTINF-READ-SEC          SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
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
      ******************************************************************
      *    オンライン資格確認訪問診療
      ******************************************************************
       900-ONSHI-HOUMON-READ-SEC         SECTION.
      *
           MOVE    "tbl_onshi_houmon"  TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-ONSHI-HOUMON-READ-N-SEC
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
      *****************************************************************
      *    オンライン資格確認訪問診療読込
      *****************************************************************
       900-ONSHI-HOUMON-READ-N-SEC         SECTION.
      *
           MOVE    "tbl_onshi_houmon"  TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  ONSHI-HOUMON-REC
               MOVE    ZERO                TO  FLG-ONSHI-HOUMON
               IF      ONS-HOUMON-ONSHINRYO-KBN
                                       NOT =    "2"
                   GO  TO  900-ONSHI-HOUMON-READ-N-SEC
               END-IF
           ELSE
               INITIALIZE                      ONSHI-HOUMON-REC
               MOVE    1                   TO  FLG-ONSHI-HOUMON
           END-IF
           .
      *
       900-ONSHI-HOUMON-READ-N-EXT.
           EXIT.
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

