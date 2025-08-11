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
       PROGRAM-ID.             ORAPI071BR1V2.
      ******************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 訪問診療患者一括資格確認照会処理
      *                     （資格確認結果登録処理）
      *  管理者            :
      *  作成日付    作業者        記述
      *  25/02/07    ORCAMO        新規作成
      ******************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.02.01    ORCAMO       25/06/04  限度額適用認定証提供同意フラグ
      *                                     の設定修正
      ******************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    xml格納用データ
           SELECT  XML-FILE        ASSIGN  ASGNPARA
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL
                                   FILE    STATUS  IS  STS-XML.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *    xml格納用データ
       FD  XML-FILE.
       01  XML-REC                     PIC X(20000).
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                 PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEERR.INC".
      * 
      *    ステータス領域
       01  STS-AREA.
           03  STS-RECEERR             PIC X(02).
           03  STS-XML                 PIC X(02).
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       01  FLG-AREA.
           03  FLG-YYK                 PIC 9(01).
           03  FLG-JOBKANRI            PIC 9(01).
           03  FLG-PTHKNINF            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTKOHINF            PIC 9(01).
           03  FLG-MONTHLYNUM          PIC 9(01).
           03  FLG-ONSHI-AIDLST        PIC 9(01).
           03  FLG-ONSHI-HOUMON        PIC 9(01).
      *
           03  PTHKNINF-OK             PIC 9(01).
           03  FLG-ARI                 PIC 9(01).
           03  FLG-HENERR              PIC 9(01).
           03  FLG-END                 PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-OUT                 PIC 9(10).
           03  CNT-COMMIT              PIC 9(05). 
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
      *    パラメタ領域
       01  WRK-PARA.
      *****COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC X(02).
           03  WRK-PARA-ERRFILE    PIC X(64).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-CNT                 PIC 9(10).
           03  WRK-PTID                PIC 9(10).
           03  WRK-UUID                PIC X(36).
           03  WRK-FUJYO-UUID          PIC X(36).
           03  WRK-KOH-UUID            PIC X(36).
           03  WRK-TAIHI-PTID          PIC 9(10).
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
      *    AIDLST RENNUM
           03  WRK-RENNUM              PIC 9(03).
           03  WRK-RECEERR             PIC X(200).
      *
       01  WRK-QUAAPPREQ3-AREA.     
           03  WRK-QUAAPPREQ3-RESULT-HKN
                                       PIC 9(02).
           03  WRK-QUAAPPREQ3-RESULT-SEH
                                       PIC 9(02).
      *****配列をWRK-CONS-RES-MAXにあわせる  
      *****03  WRK-QUAAPPREQ3-OCC OCCURS   1.     
           03  WRK-QUAAPPREQ3-OCC.     
               05  WRK-QUAAPPREQ3-SHO-KIGO PIC X(80).
               05  WRK-QUAAPPREQ3-SHO-NUM  PIC X(80).
               05  WRK-QUAAPPREQ3-RESULT-AREA.
      *            配列をWRK-CONS-RESULT-MAXにあわせる  
                   07  WRK-QUAAPPREQ3-RESULT-OCC   OCCURS  2.
                       09  WRK-QUAAPPREQ3-RES-KIGO
                                       PIC X(80).
                       09  WRK-QUAAPPREQ3-RES-NUM
                                       PIC X(80).
                       09  WRK-QUAAPPREQ3-RES-HIHKNJANAME
                                       PIC X(200).
                       09  WRK-QUAAPPREQ3-RES-NAME
                                       PIC X(200).
                       09  WRK-QUAAPPREQ3-RES-NAME-ETC
                                       PIC X(200).
                       09  WRK-QUAAPPREQ3-RES-KANANAME
                                       PIC X(200).
                       09  WRK-QUAAPPREQ3-RES-KANANAME1
                                       PIC X(200).
                       09  WRK-QUAAPPREQ3-RES-KANANAME2
                                       PIC X(200).
                       09  WRK-QUAAPPREQ3-RES-KANANAME-ETC
                                       PIC X(200).
                       09  WRK-QUAAPPREQ3-RES-ADRS
                                       PIC X(500).
                       09  WRK-QUAAPPREQ3-HKNJANUM-NAME
                                       PIC X(200).
      *                PTID不一致で初期化したときのエラーコード
                       09  WRK-QUAAPPREQ3-ERR-CODE
                                       PIC X(09).
                       09  WRK-QUAAPPREQ3-ERR-PTID
                                       PIC 9(10).
      *
       01  WRK-QUAAPPREQ3-PTID-AREA.
      *    配列をWRK-CONS-RESULT-MAXにあわせる  
           03  WRK-QUAAPPREQ3-PTID-OCC     OCCURS  2.   
               05  WRK-QUAAPPREQ3-PTID PIC 9(10).
               05  WRK-QUAAPPREQ3-UUID PIC X(36).
      *    設定するPTID
       01  WRK-QUAAPPREQ3-PTID-NEW-AREA.
      *    配列をWRK-CONS-RESULT-MAXにあわせる  
           03  WRK-QUAAPPREQ3-PTID-N-OCC   OCCURS  2.   
               05  WRK-QUAAPPREQ3-N-PTID
                                       PIC 9(10).
               05  WRK-QUAAPPREQ3-N-UUID
                                       PIC X(36).
      *
      *    患者毎の資格確認結果
       01  WRK-TAIHI-QUAAPPREQ3-AREA.
           03  WRK-TAIHI-QUAAPPREQ3-INDEX
                                       PIC 9(03)   OCCURS  2.
           03  WRK-TAIHI-QUAAPPREQ3-MAX
                                       PIC 9(02).
      * 
      *    退避患者保険情報
       01  WRK-PTHKNINF-AREA.
           02  WRK-PTHKNINF-OK         PIC 9(02). 
           02  WRK-PTHKNINF-MAX        PIC 9(02).
           02  FLG-PTHKNINF-EDABAN     PIC 9(01). 
           02  WRK-PTHKNINF-REC        OCCURS  10.   
           COPY    "CPPTHKNINF.INC"    REPLACING   //PTHKN//
                                       BY          //WRK-PTHKN//.
      * 
      *    退避患者公費情報
       01  WRK-PTKOHINF-AREA.
           02  WRK-PTKOHINF-OK         PIC 9(02). 
           02  WRK-PTKOHINF-MAX        PIC 9(02).
           02  WRK-PTKOHINF-REC        OCCURS  10.   
               03  WRK-PTKOH-PTID      PIC 9(10).
      *         
      *     メッセージヘッダー部退避
           COPY    "CPONLINEQUAAPPREQ3.INC"
                                       REPLACING  //QUAAPPREQ3//
                                       BY         //HEAD-QUAREQ3//.
      *
      *    全角半角変換テーブル
       01  TBL-HENKAN-AREA.
           03  FILLER                  PIC X(01)   VALUE   "0".
           03  FILLER                  PIC X(02)   VALUE   "０".
           03  FILLER                  PIC X(01)   VALUE   "1".
           03  FILLER                  PIC X(02)   VALUE   "１".
           03  FILLER                  PIC X(01)   VALUE   "2".
           03  FILLER                  PIC X(02)   VALUE   "２".
           03  FILLER                  PIC X(01)   VALUE   "3".
           03  FILLER                  PIC X(02)   VALUE   "３".
           03  FILLER                  PIC X(01)   VALUE   "4".
           03  FILLER                  PIC X(02)   VALUE   "４".
           03  FILLER                  PIC X(01)   VALUE   "5".
           03  FILLER                  PIC X(02)   VALUE   "５".
           03  FILLER                  PIC X(01)   VALUE   "6".
           03  FILLER                  PIC X(02)   VALUE   "６".
           03  FILLER                  PIC X(01)   VALUE   "7".
           03  FILLER                  PIC X(02)   VALUE   "７".
           03  FILLER                  PIC X(01)   VALUE   "8".
           03  FILLER                  PIC X(02)   VALUE   "８".
           03  FILLER                  PIC X(01)   VALUE   "9".
           03  FILLER                  PIC X(02)   VALUE   "９".
      *
      *英字
           03  FILLER                  PIC X(01)   VALUE   "A".
           03  FILLER                  PIC X(02)   VALUE   "Ａ".
           03  FILLER                  PIC X(01)   VALUE   "B".
           03  FILLER                  PIC X(02)   VALUE   "Ｂ".
           03  FILLER                  PIC X(01)   VALUE   "C".
           03  FILLER                  PIC X(02)   VALUE   "Ｃ".
           03  FILLER                  PIC X(01)   VALUE   "D".
           03  FILLER                  PIC X(02)   VALUE   "Ｄ".
           03  FILLER                  PIC X(01)   VALUE   "E".
           03  FILLER                  PIC X(02)   VALUE   "Ｅ".
           03  FILLER                  PIC X(01)   VALUE   "F".
           03  FILLER                  PIC X(02)   VALUE   "Ｆ".
           03  FILLER                  PIC X(01)   VALUE   "G".
           03  FILLER                  PIC X(02)   VALUE   "Ｇ".
           03  FILLER                  PIC X(01)   VALUE   "H".
           03  FILLER                  PIC X(02)   VALUE   "Ｈ".
           03  FILLER                  PIC X(01)   VALUE   "I".
           03  FILLER                  PIC X(02)   VALUE   "Ｉ".
           03  FILLER                  PIC X(01)   VALUE   "J".
           03  FILLER                  PIC X(02)   VALUE   "Ｊ".
           03  FILLER                  PIC X(01)   VALUE   "K".
           03  FILLER                  PIC X(02)   VALUE   "Ｋ".
           03  FILLER                  PIC X(01)   VALUE   "L".
           03  FILLER                  PIC X(02)   VALUE   "Ｌ".
           03  FILLER                  PIC X(01)   VALUE   "N".
           03  FILLER                  PIC X(02)   VALUE   "Ｎ".
           03  FILLER                  PIC X(01)   VALUE   "M".
           03  FILLER                  PIC X(02)   VALUE   "Ｍ".
           03  FILLER                  PIC X(01)   VALUE   "O".
           03  FILLER                  PIC X(02)   VALUE   "Ｏ".
           03  FILLER                  PIC X(01)   VALUE   "P".
           03  FILLER                  PIC X(02)   VALUE   "Ｐ".
           03  FILLER                  PIC X(01)   VALUE   "Q".
           03  FILLER                  PIC X(02)   VALUE   "Ｑ".
           03  FILLER                  PIC X(01)   VALUE   "R".
           03  FILLER                  PIC X(02)   VALUE   "Ｒ".
           03  FILLER                  PIC X(01)   VALUE   "S".
           03  FILLER                  PIC X(02)   VALUE   "Ｓ".
           03  FILLER                  PIC X(01)   VALUE   "T".
           03  FILLER                  PIC X(02)   VALUE   "Ｔ".
           03  FILLER                  PIC X(01)   VALUE   "U".
           03  FILLER                  PIC X(02)   VALUE   "Ｕ".
           03  FILLER                  PIC X(01)   VALUE   "V".
           03  FILLER                  PIC X(02)   VALUE   "Ｖ".
           03  FILLER                  PIC X(01)   VALUE   "W".
           03  FILLER                  PIC X(02)   VALUE   "Ｗ".
           03  FILLER                  PIC X(01)   VALUE   "X".
           03  FILLER                  PIC X(02)   VALUE   "Ｘ".
           03  FILLER                  PIC X(01)   VALUE   "Y".
           03  FILLER                  PIC X(02)   VALUE   "Ｙ".
           03  FILLER                  PIC X(01)   VALUE   "Z".
           03  FILLER                  PIC X(02)   VALUE   "Ｚ".
      *英字
           03  FILLER                  PIC X(01)   VALUE   "a".
           03  FILLER                  PIC X(02)   VALUE   "ａ".
           03  FILLER                  PIC X(01)   VALUE   "b".
           03  FILLER                  PIC X(02)   VALUE   "ｂ".
           03  FILLER                  PIC X(01)   VALUE   "c".
           03  FILLER                  PIC X(02)   VALUE   "ｃ".
           03  FILLER                  PIC X(01)   VALUE   "d".
           03  FILLER                  PIC X(02)   VALUE   "ｄ".
           03  FILLER                  PIC X(01)   VALUE   "e".
           03  FILLER                  PIC X(02)   VALUE   "ｅ".
           03  FILLER                  PIC X(01)   VALUE   "f".
           03  FILLER                  PIC X(02)   VALUE   "ｆ".
           03  FILLER                  PIC X(01)   VALUE   "g".
           03  FILLER                  PIC X(02)   VALUE   "ｇ".
           03  FILLER                  PIC X(01)   VALUE   "h".
           03  FILLER                  PIC X(02)   VALUE   "ｈ".
           03  FILLER                  PIC X(01)   VALUE   "i".
           03  FILLER                  PIC X(02)   VALUE   "ｉ".
           03  FILLER                  PIC X(01)   VALUE   "j".
           03  FILLER                  PIC X(02)   VALUE   "ｊ".
           03  FILLER                  PIC X(01)   VALUE   "k".
           03  FILLER                  PIC X(02)   VALUE   "ｋ".
           03  FILLER                  PIC X(01)   VALUE   "l".
           03  FILLER                  PIC X(02)   VALUE   "ｌ".
           03  FILLER                  PIC X(01)   VALUE   "n".
           03  FILLER                  PIC X(02)   VALUE   "ｎ".
           03  FILLER                  PIC X(01)   VALUE   "m".
           03  FILLER                  PIC X(02)   VALUE   "ｍ".
           03  FILLER                  PIC X(01)   VALUE   "o".
           03  FILLER                  PIC X(02)   VALUE   "ｏ".
           03  FILLER                  PIC X(01)   VALUE   "p".
           03  FILLER                  PIC X(02)   VALUE   "ｐ".
           03  FILLER                  PIC X(01)   VALUE   "q".
           03  FILLER                  PIC X(02)   VALUE   "ｑ".
           03  FILLER                  PIC X(01)   VALUE   "r".
           03  FILLER                  PIC X(02)   VALUE   "ｒ".
           03  FILLER                  PIC X(01)   VALUE   "s".
           03  FILLER                  PIC X(02)   VALUE   "ｓ".
           03  FILLER                  PIC X(01)   VALUE   "t".
           03  FILLER                  PIC X(02)   VALUE   "ｔ".
           03  FILLER                  PIC X(01)   VALUE   "u".
           03  FILLER                  PIC X(02)   VALUE   "ｕ".
           03  FILLER                  PIC X(01)   VALUE   "v".
           03  FILLER                  PIC X(02)   VALUE   "ｖ".
           03  FILLER                  PIC X(01)   VALUE   "w".
           03  FILLER                  PIC X(02)   VALUE   "ｗ".
           03  FILLER                  PIC X(01)   VALUE   "x".
           03  FILLER                  PIC X(02)   VALUE   "ｘ".
           03  FILLER                  PIC X(01)   VALUE   "y".
           03  FILLER                  PIC X(02)   VALUE   "ｙ".
           03  FILLER                  PIC X(01)   VALUE   "z".
           03  FILLER                  PIC X(02)   VALUE   "ｚ".
      *
           03  FILLER                  PIC X(01)   VALUE   "-".
           03  FILLER                  PIC X(02)   VALUE   "?".
           03  FILLER                  PIC X(01)   VALUE   "(".
           03  FILLER                  PIC X(02)   VALUE   "（".
           03  FILLER                  PIC X(01)   VALUE   ")".
           03  FILLER                  PIC X(02)   VALUE   "）".
           03  FILLER                  PIC X(01)   VALUE   "\".
           03  FILLER                  PIC X(02)   VALUE   "￥".
           03  FILLER                  PIC X(01)   VALUE   ".".
           03  FILLER                  PIC X(02)   VALUE   "．".
           03  FILLER                  PIC X(01)   VALUE   "/".
           03  FILLER                  PIC X(02)   VALUE   "／".
           03  FILLER                  PIC X(01)   VALUE   "<".
           03  FILLER                  PIC X(02)   VALUE   "＜".
           03  FILLER                  PIC X(01)   VALUE   ">".
           03  FILLER                  PIC X(02)   VALUE   "＞".
           03  FILLER                  PIC X(01)   VALUE   "*".
           03  FILLER                  PIC X(02)   VALUE   "＊".
           03  FILLER                  PIC X(01)   VALUE   "?".
           03  FILLER                  PIC X(02)   VALUE   "？".
           03  FILLER                  PIC X(01)   VALUE   "%".
           03  FILLER                  PIC X(02)   VALUE   "％".
           03  FILLER                  PIC X(01)   VALUE   "+".
           03  FILLER                  PIC X(02)   VALUE   "＋".
           03  FILLER                  PIC X(01)   VALUE   "!".
           03  FILLER                  PIC X(02)   VALUE   "！".
           03  FILLER                  PIC X(01)   VALUE   "#".
           03  FILLER                  PIC X(02)   VALUE   "＃".
           03  FILLER                  PIC X(01)   VALUE   "&".
           03  FILLER                  PIC X(02)   VALUE   "＆".
           03  FILLER                  PIC X(01)   VALUE   "=".
           03  FILLER                  PIC X(02)   VALUE   "＝".
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  FILLER                  PIC X(02)   VALUE   "，".
           03  FILLER                  PIC X(01)   VALUE   "_".
           03  FILLER                  PIC X(02)   VALUE   "＿".
           03  FILLER                  PIC X(01)   VALUE   "@".
           03  FILLER                  PIC X(02)   VALUE   "＠".
           03  FILLER                  PIC X(01)   VALUE   "[".
           03  FILLER                  PIC X(02)   VALUE   "［".
           03  FILLER                  PIC X(01)   VALUE   "]".
           03  FILLER                  PIC X(02)   VALUE   "］".
      *
       01  TBL-HENKAN-AREA-R           REDEFINES   TBL-HENKAN-AREA.
           03  TBL-HENKAN-OCC          OCCURS   83
                                       INDEXED  TBL-IDX.
               05  TBL-HALF            PIC X(01).
               05  TBL-ZEN             PIC X(02).
       01  TBL-MAX                     PIC 9(04)   VALUE   83.
      *
       01  WRK-CONS-AREA.
           03  WRK-CONS-RES-MAX        PIC 9(04)   VALUE   1.
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
           COPY    "CPONLINEQUAAPPREQ3.INC".
      *    API XML書き込み用定義体
           COPY    "CPONLINEQUAAPPRES1.INC".
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
           COPY    "MCPDATA2.INC".
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *     
           COPY    "MCPAREA".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *     
      *    一時デイレクトリ領域
           COPY    "CPASGNPARA.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＵＩＤ取得用エリア
       01  UIDIO-AREA.
           03  C-RET               PIC X(2).
           03  C-UID               PIC X(36).
       01  ST                      PIC 9(2).
      *
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
       01  COMMAND-PARAM.
           02  FILLER              PIC X(256).
      *
      ******************************************************************
      *
       PROCEDURE               DIVISION
                                   USING
                                   COMMAND-PARAM.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL       FLG-END     =   1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  SPA-AREA
                                       CNT-AREA
                                       XML-ONLINEHEAD-QUAREQ3
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HOSPNUM
                                               ASGNPARA
                                               RECEERR
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    ASGNPARA            TO  SGETTEMP-BASENAMES  (2)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SPACE               TO  ASGNPARA
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  ASGNPARA
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           OPEN    INPUT   XML-FILE
      *
           PERFORM 900-XML-READ-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    "ORAPI071BR1V2"     TO  JOB-PGID
           MOVE    "訪問診療一括返却"  TO  JOB-SHELLMSG
           PERFORM 900-CALL-ORCSJOB-SEC
      *
      *    処理件数を設定         
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           PERFORM 900-JOBKANRI-READ-SEC
           IF      FLG-JOBKANRI        =   1
               MOVE    "E17"               TO  WRK-ERRCD
               MOVE    1                   TO  FLG-END
           ELSE
               MOVE    JOB-UPDCNT          TO  WRK-CNT
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
           MOVE    XML-REC         TO  XML-ONLINEQUAAPPREQ3
      *
      *    一件目のMessageHeaderを退避
           IF      CNT-OUT         =   ZERO
               MOVE    XML-ONLINEQUAAPPREQ3
                                       TO  XML-ONLINEHEAD-QUAREQ3
           END-IF                            
      *
      *    PTID UUID 取得
           INITIALIZE                  WRK-QUAAPPREQ3-PTID-AREA
                                       WRK-QUAAPPREQ3-AREA
                                       WRK-FUJYO-UUID
                                       WRK-KOH-UUID
      *    全角変換処理
           PERFORM 2001-KANACHK-HENSYU-SEC
      *    患者ＩＤ取得
           IF      QUAAPPREQ3-SHOKAI-NUM 
                               NOT =   SPACE
      *        照会番号から患者ＩＤ取得
               PERFORM 2003-PTID-02-SEC
               PERFORM VARYING IDYY    FROM    1   BY  1
                       UNTIL   IDYY    >       WRK-CONS-RESULT-MAX
                       OR      QUAAPPREQ3-CARD-CLASS (IDYY)
                                       =       SPACE 
                   IF      QUAAPPREQ3-CARD-CLASS (IDYY)
                                       =   "A1"
                       MOVE    IDYY        TO  WRK-QUAAPPREQ3-RESULT-SEH
                   ELSE
                       MOVE    IDYY        TO  WRK-QUAAPPREQ3-RESULT-HKN
                   END-IF
               END-PERFORM 
           END-IF
           IF      WRK-QUAAPPREQ3-PTID (1)
                                   =   ZERO
      *        保険情報から患者ＩＤ取得 
               PERFORM VARYING IDYY    FROM    1   BY  1
                       UNTIL   IDYY    >       WRK-CONS-RESULT-MAX
                       OR      QUAAPPREQ3-CARD-CLASS (IDYY)
                                       =       SPACE 
                   IF      QUAAPPREQ3-CARD-CLASS (IDYY)
                                           =   "A1"
                       MOVE    IDYY        TO  WRK-QUAAPPREQ3-RESULT-SEH
                       PERFORM 2004-PTID-03-SEC
                   ELSE
                       MOVE    IDYY        TO  WRK-QUAAPPREQ3-RESULT-HKN
                       PERFORM 2002-PTID-01-SEC
                   END-IF
               END-PERFORM 
            END-IF
      *
      *    複数返却時のＰＴＩＤの設定
           MOVE    WRK-QUAAPPREQ3-PTID-AREA
                                   TO  WRK-QUAAPPREQ3-PTID-NEW-AREA 
      *    併用のときPTIDが違うとき
           IF    ( QUAAPPREQ3-CARD-CLASS (2)
                               NOT =   SPACE )
           AND   ( WRK-QUAAPPREQ3-PTID (1)
                               NOT =   WRK-QUAAPPREQ3-PTID (2) )
               IF      WRK-QUAAPPREQ3-RESULT-HKN
                                       >   ZERO
      *            保険と医療扶助の併用のとき
                   IF      WRK-QUAAPPREQ3-N-PTID
                                       (WRK-QUAAPPREQ3-RESULT-HKN)
                                           =   ZERO
      *                保険のＰＴＩＤが設定できなかったとき全てのＰＴＩＤを設定しない
                       INITIALIZE      WRK-QUAAPPREQ3-PTID-NEW-AREA
                   ELSE
      *                医療扶助のＰＴＩＤが設定できなかったとき保険のＰＴＩＤを設定
                       IF      WRK-QUAAPPREQ3-N-PTID 
                                           (WRK-QUAAPPREQ3-RESULT-SEH)
                                               =   ZERO
                           MOVE    WRK-QUAAPPREQ3-N-PTID
                                           (WRK-QUAAPPREQ3-RESULT-HKN)
                                               TO
                                           WRK-QUAAPPREQ3-N-PTID
                                           (WRK-QUAAPPREQ3-RESULT-SEH)
                       ELSE                            
      *                    医療扶助のＰＴＩＤが保険のＰＴＩＤと異なったとき全てのＰＴＩＤを設定しない
                           IF      WRK-QUAAPPREQ3-N-PTID
                                           (WRK-QUAAPPREQ3-RESULT-SEH)
                                               NOT =
                                      WRK-QUAAPPREQ3-N-PTID
                                             (WRK-QUAAPPREQ3-RESULT-HKN)
                               INITIALIZE  WRK-QUAAPPREQ3-PTID-NEW-AREA
                           END-IF
                       END-IF
                   END-IF
               END-IF
      *        資格確認から設定したＰＴＩＤと違うＰＴＩＤを設定したとき
      *        エラーコード設定
               PERFORM VARYING IDYY    FROM    1   BY  1
                       UNTIL   IDYY    >       WRK-CONS-RESULT-MAX
                   IF      WRK-QUAAPPREQ3-PTID (IDYY)
                                   NOT =
                                           WRK-QUAAPPREQ3-N-PTID (IDYY)
                       MOVE    "ORCAE01"                   TO
                                           WRK-QUAAPPREQ3-ERR-CODE(IDYY)
                       MOVE    WRK-QUAAPPREQ3-PTID (IDYY)  TO
                                           WRK-QUAAPPREQ3-ERR-PTID(IDYY)
                   END-IF
               END-PERFORM
           END-IF
      *
      *     IF  WRK-QUAAPPREQ3-RESULT-HKN > ZERO
      *      DISPLAY "PTID  HKN=" 
      *          WRK-QUAAPPREQ3-PTID (WRK-QUAAPPREQ3-RESULT-HKN) "=>"
      *          WRK-QUAAPPREQ3-N-PTID (WRK-QUAAPPREQ3-RESULT-HKN) 
      *     END-IF
      *     IF  WRK-QUAAPPREQ3-RESULT-SEH > ZERO     
      *      DISPLAY "      SEH="
      *          WRK-QUAAPPREQ3-PTID (WRK-QUAAPPREQ3-RESULT-SEH) "=>"
      *          WRK-QUAAPPREQ3-N-PTID (WRK-QUAAPPREQ3-RESULT-SEH)
      *     END-IF     
      *
      *    扶助UIDコード設定
           IF      WRK-QUAAPPREQ3-RESULT-SEH
                                   >   ZERO
      *        UIDコード取得処理
               PERFORM 800-UID-GET-SEC
               MOVE    C-UID           TO  WRK-FUJYO-UUID
           END-IF
      *
           IF      WRK-QUAAPPREQ3-RESULT-HKN
                                   >   0
               MOVE    WRK-QUAAPPREQ3-RESULT-HKN
                                       TO  IDYY
               PERFORM 2005-SIKAKU-KAKUNIN-INS-SEC
           END-IF
           IF      WRK-QUAAPPREQ3-RESULT-SEH
                                   >   0
               MOVE    WRK-QUAAPPREQ3-RESULT-SEH
                                       TO  IDYY
               PERFORM 2005-SIKAKU-KAKUNIN-INS-SEC
           END-IF
      *
           IF      WRK-ERRCD       =   SPACE
               ADD     1               TO  CNT-OUT
               ADD     1               TO  WRK-CNT
      *
               ADD     1               TO  CNT-COMMIT
               IF      CNT-COMMIT      =   100
                   MOVE    ZERO            TO  CNT-COMMIT
                   PERFORM 900-DBCOMMIT-SEC
                   PERFORM 900-DBSTART-SEC
               END-IF
      *
               PERFORM 900-XML-READ-SEC
           ELSE
               MOVE    1               TO  FLG-END
           END-IF    
      *
           .        
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       2001-KANACHK-HENSYU-SEC    SECTION.
      *
           MOVE    QUAAPPREQ3-SHO-KIGO 
                                   TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-QUAAPPREQ3-SHO-KIGO 
           IF    ( QUAAPPREQ3-SHO-HKNJANUM  (1:2)
                                   =   "12"  )
           AND   ( QUAAPPREQ3-SHO-HKNJANUM  (7:2)
                               NOT =   SPACE )
      *        負担者番号が医療扶助のとき番号は半角のまま
               MOVE    QUAAPPREQ3-SHO-NUM 
                                       TO  WRK-QUAAPPREQ3-SHO-NUM 
           ELSE
               MOVE    QUAAPPREQ3-SHO-NUM 
                                       TO  WRK-KANACHK-MAE-INPUT
               PERFORM 800-KANACHK-SEC
               MOVE    WRK-KANACHK-OUT-INPUT
                                       TO  WRK-QUAAPPREQ3-SHO-NUM 
           END-IF
      *
           PERFORM VARYING IDYY    FROM    1   BY  1
                   UNTIL   IDYY    >       WRK-CONS-RESULT-MAX
                   OR      QUAAPPREQ3-CARD-CLASS  (IDYY)
                                   =       SPACE 
               MOVE    QUAAPPREQ3-RES-KIGO  (IDYY)
                                   TO  WRK-KANACHK-MAE-INPUT
               PERFORM 800-KANACHK-SEC
               MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-QUAAPPREQ3-RES-KIGO
                                                              (IDYY)
               IF      QUAAPPREQ3-CARD-CLASS  (IDYY)
                                   =   "A1"
                   MOVE    QUAAPPREQ3-RES-NUM  (IDYY)
                                       TO  WRK-QUAAPPREQ3-RES-NUM
                                                              (IDYY)
               ELSE                            
                   MOVE    QUAAPPREQ3-RES-NUM  (IDYY)
                                       TO    WRK-KANACHK-MAE-INPUT
                   PERFORM 800-KANACHK-SEC
                   MOVE    WRK-KANACHK-OUT-INPUT
                                       TO  WRK-QUAAPPREQ3-RES-NUM
                                                              (IDYY)
               END-IF
               MOVE    QUAAPPREQ3-RES-HIHKNJANAME  (IDYY)
                                   TO  WRK-KANACHK-MAE-INPUT
               MOVE    200             TO  WRK-KANACHK-LEN
               PERFORM 801-KANACHK2-SEC                       
               MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-QUAAPPREQ3-RES-HIHKNJANAME
                                                              (IDYY)
               MOVE    QUAAPPREQ3-RES-NAME  (IDYY)
                                   TO  WRK-KANACHK-MAE-INPUT
               MOVE    200         TO  WRK-KANACHK-LEN
               PERFORM 801-KANACHK2-SEC                       
               MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-QUAAPPREQ3-RES-NAME
                                                              (IDYY)
                MOVE    QUAAPPREQ3-RES-NAME-ETC  (IDYY)
                                   TO  WRK-KANACHK-MAE-INPUT
               MOVE    200         TO  WRK-KANACHK-LEN
               PERFORM 801-KANACHK2-SEC                       
               MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-QUAAPPREQ3-RES-NAME-ETC
                                                              (IDYY)
               MOVE    QUAAPPREQ3-RES-KANANAME  (IDYY)
                                   TO    WRK-KANACHK-MAE-INPUT
               PERFORM 800-KANACHK-SEC
               MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-QUAAPPREQ3-RES-KANANAME
                                                              (IDYY)
               MOVE    QUAAPPREQ3-RES-KANANAME-ETC  (IDYY)
                                   TO    WRK-KANACHK-MAE-INPUT
               PERFORM 800-KANACHK-SEC
               MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-QUAAPPREQ3-RES-KANANAME-ETC
                                                              (IDYY)
               MOVE    QUAAPPREQ3-RES-ADRS  (IDYY)
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
                                       TO  WRK-QUAAPPREQ3-RES-ADRS
                                                    (IDYY)(IDZ:2)
                       ADD     2       TO  IDZ
                   END-IF
               END-PERFORM
               MOVE    QUAAPPREQ3-HKNJANUM-NAME  (IDYY)
                                   TO    WRK-KANACHK-MAE-INPUT
               PERFORM 800-KANACHK-SEC
               MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-QUAAPPREQ3-HKNJANUM-NAME
                                                              (IDYY)
           END-PERFORM
      *
           .        
       2001-KANACHK-HENSYU-EXT.
           EXIT.
     * 
      *****************************************************************
      *     顔認証資格確認患者ＩＤ取得（照会番号の設定がない場合）
      *     医療扶助以外
      *****************************************************************
       2002-PTID-01-SEC  SECTION.
      *
      *    保険情報から患者保険情報を検索
           INITIALIZE                  WRK-PTHKNINF-AREA
      *         
           INITIALIZE                  PTHKNINF-REC
           MOVE    SPA-HOSPNUM     TO  PTHKN-HOSPNUM
           MOVE    QUAAPPREQ3-RES-HKNJANUM (IDYY)
                                   TO  PTHKN-HKNJANUM
           MOVE    WRK-QUAAPPREQ3-RES-KIGO (IDYY) 
                                   TO  PTHKN-KIGO
           MOVE    WRK-QUAAPPREQ3-RES-NUM  (IDYY)
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
                       IF      QUAAPPREQ3-RES-EDABAN   (IDYY)
                                           =   WRK-PTHKN-EDABAN (IDY)
                           MOVE    IDY             TO  WRK-PTHKNINF-OK
                       END-IF
                   END-IF
               END-PERFORM

      *
               IF      WRK-PTHKNINF-OK >   ZERO
      *            枝番まで一致した場合
                   MOVE    WRK-PTHKN-PTID (WRK-PTHKNINF-OK)
                                           TO  WRK-QUAAPPREQ3-PTID
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
      *                        IF    (  QUAAPPREQ3-RES-SEX1     
      *                                            =   PTINF-SEX      )
      *                        AND   (  QUAAPPREQ3-RES-BIRTHDAY 
      *                                            =   PTINF-BIRTHDAY )
      *                            IF    ( WRK-QUAAPPREQ3-RES-KANANAME
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
       2002-PTID-01-EXT.
           EXIT.
      *
      *****************************************************************
      *     顔認証資格確認患者ＩＤ取得（照会番号の設定がある場合）
      *****************************************************************
       2003-PTID-02-SEC  SECTION.
      *
      *    照会番号から患者ＩＤ取得
           INITIALIZE                  ORCSPTNUMAREA
           MOVE    QUAAPPREQ3-SHOKAI-NUM 
                                  TO  SPTNUM-PTNUM
           CALL    "ORCSPTNUM"    USING   ORCSPTNUMAREA
                                          SPA-AREA
           IF      SPTNUM-RC       =   00
               PERFORM VARYING IDYY    FROM    1   BY  1
                       UNTIL   IDYY    >       WRK-CONS-RESULT-MAX
                   MOVE    SPTNUM-PTID TO  WRK-QUAAPPREQ3-PTID (IDYY)
               END-PERFORM
           ELSE
               DISPLAY "SHOKAI-NUM ERR " QUAAPPREQ3-SHOKAI-NUM 
           END-IF
      *
           .
       2003-PTID-02-EXT.
           EXIT.
      *
      *****************************************************************
      *     顔認証資格確認登録、更新処理
      *     医療扶助
      *****************************************************************
       2004-PTID-03-SEC  SECTION.
      * 
      *    公費情報から患者公費険情報を検索
           INITIALIZE                  WRK-PTKOHINF-AREA
      *
      *    資格確認結果の保険情報が空白のとき、公費情報の検索は行わない
           IF    ( QUAAPPREQ3-RES-HKNJANUM (IDYY)
                                   =   SPACE )
           AND   ( WRK-QUAAPPREQ3-RES-NUM  (IDYY)
                                   =   SPACE )
               GO  TO  2004-PTID-03-EXT
           END-IF     
      *         
           INITIALIZE                  PTKOHINF-REC
           MOVE    SPA-HOSPNUM     TO  PTKOH-HOSPNUM
           MOVE    QUAAPPREQ3-RES-HKNJANUM (IDYY)
                                   TO  PTKOH-FTNJANUM
           MOVE    WRK-QUAAPPREQ3-RES-NUM  (IDYY)
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
               MOVE    QUAAPPREQ3-RES-HKNJANUM (IDYY)
                                       TO  MONTHLYNUM-XXFTNJANUM
               MOVE    WRK-QUAAPPREQ3-RES-NUM  (IDYY)
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
               MOVE    WRK-PTKOH-PTID (1)  TO  WRK-QUAAPPREQ3-PTID
                                                           (IDYY)
           END-IF
      *
           .
       2004-PTID-03-EXT.
           EXIT.
      *     
      ******************************************************************
      *    資格確認レコード作成処理（訪問診療）
      ******************************************************************
       2005-SIKAKU-KAKUNIN-INS-SEC  SECTION.
      *
           INITIALIZE                  ONSKAKU-REC
           MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
      *    UIDコード取得処理
           PERFORM 800-UID-GET-SEC
           MOVE    C-UID           TO  ONSKAKU-TBL-UUID
           IF    ( QUAAPPREQ3-CARD-CLASS (2)
                               NOT =   SPACE )
      *****AND   ( WRK-QUAAPPREQ3-PTID 
      *****                    NOT =   ZERO  ) 
               MOVE    WRK-FUJYO-UUID  TO  ONSKAKU-FUJYO-UUID
      *###                                 ONSKAKU-FUJYO-UUID1
      *MOVE    "1"             TO  ONSKAKU-FUJYO-HEIYO
               IF      QUAAPPREQ3-CARD-CLASS  (IDYY)
                                       =   "A1"
                   MOVE    WRK-KOH-UUID
                                       TO  ONSKAKU-KOUHI-UUID
               ELSE
                   MOVE    ONSKAKU-TBL-UUID
                                       TO  WRK-KOH-UUID
               END-IF
           ELSE
               IF      QUAAPPREQ3-CARD-CLASS  (IDYY)
                                       =   "A1"
                   MOVE    WRK-FUJYO-UUID
                                       TO  ONSKAKU-FUJYO-UUID
               END-IF        
           END-IF
           MOVE    HEAD-QUAREQ3-PROCESS-CLASS
                                   TO  ONSKAKU-PROCESS-CLASS
           MOVE    HEAD-QUAREQ3-HO-SRYKBN
                                   TO  ONSKAKU-ONSHINRYO-KBN
           MOVE    "03"            TO  ONSKAKU-KAKU-ENDFLG
           MOVE    WRK-QUAAPPREQ3-N-PTID (IDYY)
                                   TO  ONSKAKU-PTID
           MOVE    "1"             TO  ONSKAKU-SET-PTID
           MOVE    HEAD-QUAREQ3-SHORI-TIME (1:8)
                                   TO  ONSKAKU-KAKUNINYMD
                                       ONSKAKU-KAKU-KAKUNINYMD
           MOVE    HEAD-QUAREQ3-SHORI-TIME (9:6)
                                   TO  ONSKAKU-KAKU-KAKUNINHMS 
           MOVE    HEAD-QUAREQ3-SHORI-TIME
                                   TO  ONSKAKU-SHORI-TIME 
           MOVE    HEAD-QUAREQ3-HOSPCD
                                   TO  ONSKAKU-HOSPCD
           MOVE    HEAD-QUAREQ3-FILE-SIKIBETU
                                   TO  ONSKAKU-FILE-SIKIBETU
           MOVE    HEAD-QUAREQ3-RESULT-KBN
                                   TO  ONSKAKU-RESULT-KBN
           MOVE    HEAD-QUAREQ3-ERR-CODE
                                   TO  ONSKAKU-ERR-CODE
           MOVE    HEAD-QUAREQ3-ERR-MSG
                                  TO  ONSKAKU-ERR-MSG
           IF    (  ONSKAKU-ERR-CODE 
                                   =   SPACE )
           AND   ( WRK-QUAAPPREQ3-ERR-CODE (IDYY) 
                               NOT =   SPACE )
               MOVE    WRK-QUAAPPREQ3-ERR-CODE (IDYY) 
                                       TO  WRK-ERR-CODE
               MOVE    WRK-QUAAPPREQ3-ERR-PTID (IDYY) 
                                       TO  WRK-ERR-PTID
           END-IF 
           PERFORM 20051-ONSKAKU-ERR-HENSYU-SEC
           MOVE    HEAD-QUAREQ3-CODE-TYPE
                                   TO  ONSKAKU-CODE-TYPE
      *                     
           MOVE    QUAAPPREQ3-SHO-HKNJANUM 
                                   TO  ONSKAKU-SHO-HKNJANUM
           MOVE    WRK-QUAAPPREQ3-SHO-KIGO     
                                   TO  ONSKAKU-SHO-KIGO
           MOVE    WRK-QUAAPPREQ3-SHO-NUM      
                                   TO  ONSKAKU-SHO-NUM
           MOVE    QUAAPPREQ3-SHO-EDABAN   
                                   TO  ONSKAKU-SHO-EDABAN
           MOVE    QUAAPPREQ3-SHO-BIRTHDAY 
                                   TO  ONSKAKU-SHO-BIRTHDAY
           MOVE    QUAAPPREQ3-RESULT       
                                   TO  ONSKAKU-RESULT
           MOVE    QUAAPPREQ3-RESULT-CODE  
                                   TO  ONSKAKU-RESULT-CODE
           MOVE    QUAAPPREQ3-RESULT-MSG   
                                   TO  ONSKAKU-RESULT-MSG
           MOVE    QUAAPPREQ3-SIKAKU-YUKO  
                                   TO  ONSKAKU-SIKAKU-YUKO
      *
           MOVE    QUAAPPREQ3-CARD-CLASS    (IDYY) 
                                   TO  ONSKAKU-CARD-CLASS 
           MOVE    QUAAPPREQ3-RES-HKNJANUM  (IDYY)
                                   TO  ONSKAKU-RES-HKNJANUM
           MOVE    WRK-QUAAPPREQ3-RES-KIGO  (IDYY)
                                   TO  ONSKAKU-RES-KIGO
           MOVE    WRK-QUAAPPREQ3-RES-NUM  (IDYY)
                                   TO  ONSKAKU-RES-NUM 
           MOVE    QUAAPPREQ3-RES-EDABAN  (IDYY) 
                                   TO  ONSKAKU-RES-EDABAN 
           MOVE    QUAAPPREQ3-RES-HONKZKKBN  (IDYY) 
                                   TO  ONSKAKU-RES-HONKZKKBN 
           MOVE    WRK-QUAAPPREQ3-RES-HIHKNJANAME  (IDYY)
                                   TO  ONSKAKU-RES-HIHKNJANAME 
           MOVE    WRK-QUAAPPREQ3-RES-NAME  (IDYY)
                                   TO  ONSKAKU-RES-NAME 
           MOVE    WRK-QUAAPPREQ3-RES-NAME-ETC  (IDYY)
                                   TO  ONSKAKU-RES-NAME-ETC 
           MOVE    WRK-QUAAPPREQ3-RES-KANANAME  (IDYY)
                                   TO  ONSKAKU-RES-KANANAME 
           MOVE    WRK-QUAAPPREQ3-RES-KANANAME-ETC  (IDYY)
                                   TO  ONSKAKU-RES-KANANAME-ETC
           MOVE    QUAAPPREQ3-RES-SEX1  (IDYY)
                                   TO  ONSKAKU-RES-SEX1 
           MOVE    QUAAPPREQ3-RES-SEX2  (IDYY)
                                   TO  ONSKAKU-RES-SEX2 
           MOVE    QUAAPPREQ3-RES-BIRTHDAY  (IDYY)
                                   TO  ONSKAKU-RES-BIRTHDAY 
           MOVE    WRK-QUAAPPREQ3-RES-ADRS  (IDYY)
                                   TO  ONSKAKU-RES-ADRS 
           MOVE    QUAAPPREQ3-RES-POST  (IDYY)
                                   TO  ONSKAKU-RES-POST 
           MOVE    QUAAPPREQ3-SKKGETYMD  (IDYY)
                                   TO  ONSKAKU-SKKGETYMD 
           IF      QUAAPPREQ3-CARD-CLASS  (IDYY)
                                   =   "A1"
               MOVE    QUAAPPREQ3-JOIN-SKKGETYMD  (IDYY)
                                   TO  ONSKAKU-KOH-TEKSTYMD (1)
               MOVE    QUAAPPREQ3-JOIN-SKKLOSSYMD (IDYY)
                                   TO  ONSKAKU-KOH-TEKEDYMD (1)
           ELSE
               MOVE    QUAAPPREQ3-TEKSTYMD  (IDYY)
                                       TO  ONSKAKU-TEKSTYMD 
               MOVE    QUAAPPREQ3-TEKEDYMD  (IDYY)
                                       TO  ONSKAKU-TEKEDYMD
           END-IF
           MOVE    QUAAPPREQ3-FTNRATE  (IDYY)
                                   TO  ONSKAKU-FTNRATE 
           MOVE    QUAAPPREQ3-MISYUGAKU  (IDYY)
                                   TO  ONSKAKU-MISYUGAKU 
           MOVE    QUAAPPREQ3-LOSS-JIYU   (IDYY)
                                   TO  ONSKAKU-LOSS-JIYU 
           MOVE    WRK-QUAAPPREQ3-HKNJANUM-NAME  (IDYY)
                                   TO  ONSKAKU-HKNJANUM-NAME 
           MOVE    QUAAPPREQ3-ELDER-SKKGETYMD  (IDYY)
                                   TO  ONSKAKU-ELDER-SKKGETYMD 
           MOVE    QUAAPPREQ3-ELDER-TEKSTYMD  (IDYY)
                                   TO  ONSKAKU-ELDER-TEKSTYMD 
           MOVE    QUAAPPREQ3-ELDER-TEKEDYMD  (IDYY)
                                   TO  ONSKAKU-ELDER-TEKEDYMD 
           MOVE    QUAAPPREQ3-ELDER-FTNRATE  (IDYY)
                                   TO  ONSKAKU-ELDER-FTNRATE
           MOVE    QUAAPPREQ3-HO-GENDO-DOUIFLG  (IDYY)
                                   TO  ONSKAKU-GENDO-DOUIFLG
           MOVE    QUAAPPREQ3-HO-GENDO-TIME     (IDYY)
                                   TO  ONSKAKU-GENDO-TIME
      *****MOVE    QUAAPPREQ3-HO-GENDO-KIGEN    (IDYY)
      *****                        TO  ONSKAKU-GENDO-KIGEN
           MOVE    QUAAPPREQ3-HO-GENDO-SHOKBN    (IDYY)
                                   TO  ONSKAKU-GENDO-SHOKBN
           MOVE    QUAAPPREQ3-HO-GENDO-TEKKBN    (IDYY)
                                   TO  ONSKAKU-GENDO-TEKKBN
           MOVE    QUAAPPREQ3-HO-GENDO-SKKGETYMD  (IDYY)
                                   TO  ONSKAKU-GENDO-SKKGETYMD
           MOVE    QUAAPPREQ3-HO-GENDO-TEKSTYMD  (IDYY)
                                   TO  ONSKAKU-GENDO-TEKSTYMD
           MOVE    QUAAPPREQ3-HO-GENDO-TEKEDYMD  (IDYY)
                                   TO  ONSKAKU-GENDO-TEKEDYMD
           MOVE    QUAAPPREQ3-HO-GENDO-SKJGNSTYMD  (IDYY)
                                   TO  ONSKAKU-GENDO-SKJGNSTYMD
           MOVE    QUAAPPREQ3-HO-SIKKAN-DOUIFLG  (IDYY)
                                   TO  ONSKAKU-SIKKAN-DOUIFLG
           MOVE    QUAAPPREQ3-HO-SIKKAN-TIME     (IDYY)
                                   TO  ONSKAKU-SIKKAN-TIME
      *****MOVE    QUAAPPREQ3-HO-SIKKAN-KIGEN    (IDYY)
      *****                        TO  ONSKAKU-SIKKAN-KIGEN
      *****MOVE    QUAAPPREQ3-HO-SIKKAN-RIYU     (IDYY)
      *****                        TO  ONSKAKU-SIKKAN-RIYU
           PERFORM VARYING IDZZ    FROM    1   BY  1
                   UNTIL   IDZZ    >       3
               MOVE    QUAAPPREQ3-HO-SIKKAN-SIPKBN
                                           (IDYY IDZZ)
                                   TO  ONSKAKU-SIKKAN-SIPKBN
                                                       (IDZZ)
               MOVE    QUAAPPREQ3-HO-SIKKAN-SKJGNSTYMD
                                           (IDYY IDZZ)
                                   TO  ONSKAKU-SIKKAN-SKJGNSTYMD
                                                       (IDZZ)
               MOVE    QUAAPPREQ3-HO-SIKKAN-TEKSTYMD
                                           (IDYY IDZZ)
                                   TO  ONSKAKU-SIKKAN-TEKSTYMD
                                                       (IDZZ)
               MOVE    QUAAPPREQ3-HO-SIKKAN-TEKEDYMD
                                           (IDYY IDZZ)
                                   TO  ONSKAKU-SIKKAN-TEKEDYMD
                                                       (IDZZ)
               MOVE    QUAAPPREQ3-HO-SIKKAN-FTNMONEY
                                           (IDYY IDZZ)
                                   TO  ONSKAKU-SIKKAN-FTNMONEY
                                                       (IDZZ)
           END-PERFORM
           MOVE    QUAAPPREQ3-HO-KENSIN-DOUIFLG  (IDYY)
                                   TO  ONSKAKU-KENSIN-DOUIFLG
           MOVE    QUAAPPREQ3-HO-KENSIN-TIME     (IDYY)
                                   TO  ONSKAKU-KENSIN-TIME
           MOVE    QUAAPPREQ3-HO-KENSIN-KIGENYMD   (IDYY)
                                   TO  ONSKAKU-KENSIN-KIGENYMD
      **** MOVE    QUAAPPREQ3-HO-KENSIN-RIYU     (IDYY)
      ****                         TO  ONSKAKU-KENSIN-RIYU
           MOVE    QUAAPPREQ3-HO-YAKUZAI-DOUIFLG   (IDYY)
                                   TO  ONSKAKU-YAKUZAI-DOUIFLG
           MOVE    QUAAPPREQ3-HO-YAKUZAI-TIME    (IDYY)
                                   TO  ONSKAKU-YAKUZAI-TIME
           MOVE    QUAAPPREQ3-HO-YAKUZAI-KIGEN   (IDYY)
                                   TO  ONSKAKU-YAKUZAI-KIGEN
      **** MOVE    QUAAPPREQ3-HO-YAKUZAI-RIYU    (IDYY)
      ****                         TO  ONSKAKU-YAKUZAI-RIYU
           MOVE    QUAAPPREQ3-HO-SHINRYO-DOUIFLG   (IDYY)
                                   TO  ONSKAKU-SHINRYO-DOUIFLG
           MOVE    QUAAPPREQ3-HO-SHINRYO-TIME    (IDYY)
                                   TO  ONSKAKU-SHINRYO-TIME
           MOVE    QUAAPPREQ3-HO-SHINRYO-KIGEN   (IDYY)
                                   TO  ONSKAKU-SHINRYO-KIGEN
      **** MOVE    QUAAPPREQ3-HO-SHINRYO-RIYU    (IDYY)
      ****                         TO  ONSKAKU-SHINRYO-RIYU
           MOVE    QUAAPPREQ3-HO-OPE-DOUIFLG     (IDYY)
                                   TO  ONSKAKU-OPE-DOUIFLG
           MOVE    QUAAPPREQ3-HO-OPE-TIME        (IDYY)
                                   TO  ONSKAKU-OPE-TIME
           MOVE    QUAAPPREQ3-HO-OPE-KIGEN       (IDYY)
                                   TO  ONSKAKU-OPE-KIGEN
      **** MOVE    QUAAPPREQ3-HO-OPE-RIYU        (IDYY)
      ****                         TO  ONSKAKU-OPE-RIYU
           MOVE    HEAD-QUAREQ3-FILE-SIKIBETU
                                   TO  ONSKAKU-SIKIBETU 
           MOVE    QUAAPPREQ3-SHOKAI-NUM 
                                   TO  ONSKAKU-SHOKAI-NUM
           MOVE    QUAAPPREQ3-HO-ONSHI-CREYMD  (IDYY)
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
           IF      MCP-RC          =   ZERO
               CONTINUE
           ELSE     
               MOVE   "E20"            TO  WRK-ERRCD
               DISPLAY "TBL_ONSHI_KAKU INSERT ERR " MCP-RC
               GO  TO  2005-SIKAKU-KAKUNIN-INS-EXT 
           END-IF
      *             
           IF      ONSKAKU-CARD-CLASS      =   "A1"
      *        医療扶助交付番号格納レコード作成処理
               PERFORM 20052-ONSHI-AIDLST-SYORI-SEC
               IF      MCP-RC      NOT =   ZERO 
                    MOVE   "E21"           TO  WRK-ERRCD
                    DISPLAY "TBL_ONSHI_AIDLST INSERT ERR "
                                                 MCP-RC 
                   GO  TO  2005-SIKAKU-KAKUNIN-INS-EXT 
               END-IF
           END-IF
      *
      *    資格確認訪問診療レコード作成処理
      *    （併用の生保は作成しない）
           IF    ( QUAAPPREQ3-CARD-CLASS (2)
                               NOT =   SPACE )
           AND   ( QUAAPPREQ3-CARD-CLASS  (IDYY)
                                   =   "A1"  )
               CONTINUE
           ELSE                            
               PERFORM 20053-ONSHI-HOUMON-SYORI-SEC
               IF      MCP-RC      NOT =   ZERO 
                   MOVE   "E22"           TO  WRK-ERRCD
                   DISPLAY "TBL_ONSHI_HOUMON INSERT ERR " MCP-RC
                   GO  TO  2005-SIKAKU-KAKUNIN-INS-EXT 
               END-IF
           END-IF    
      *
           .        
       2005-SIKAKU-KAKUNIN-INS-EXT.
           EXIT.
      *
      ******************************************************************
      *    処理結果コードのチェックによる設定処理
      ******************************************************************
       20051-ONSKAKU-ERR-HENSYU-SEC  SECTION.
      *
      *    オン資側で設定済なら終了
           IF      ONSKAKU-ERR-CODE
                                   NOT =   SPACE
               GO  TO  20051-ONSKAKU-ERR-HENSYU-EXT
           END-IF
           IF      WRK-ERR-CODE        =   SPACE
               GO  TO  20051-ONSKAKU-ERR-HENSYU-EXT
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
       20051-ONSKAKU-ERR-HENSYU-EXT.
           EXIT.
      *
      ******************************************************************
      *    医療扶助交付番号格納レコード作成処理
      ******************************************************************
       20052-ONSHI-AIDLST-SYORI-SEC   SECTION.
      *
           MOVE    ZERO            TO  WRK-RENNUM
      *     
           PERFORM VARYING IDZZ    FROM    1   BY  1
                   UNTIL   IDZZ    >       4
                   OR      QUAAPPREQ3-HO-FUJYO-KOFUNUM (IDYY IDZZ)  
                                   =   SPACE       
      *        医療券で入院・外来のとき対象
               IF    ( QUAAPPREQ3-HO-TICKET-TYPE  (IDYY IDZZ)
                                       =   "01"            )
               AND   ( QUAAPPREQ3-HO-MEDICAL-TYPE  (IDYY IDZZ)
                                       =   "01"    OR  "02")
                   INITIALIZE                  ONSHI-AIDLST-REC
                   MOVE    SPA-HOSPNUM     TO  ONS-AIDLST-HOSPNUM
                   MOVE    ONSKAKU-PTID    TO  ONS-AIDLST-PTID
      *            UIDコード取得処理
                   PERFORM 800-UID-GET-SEC
                   MOVE    C-UID           TO  ONS-AIDLST-TBL-UUID
      *            FUJYO_UUID
                   MOVE    ONSKAKU-FUJYO-UUID
                                           TO  ONS-AIDLST-FUJYO-UUID
      *            負担者番号
                   MOVE    QUAAPPREQ3-RES-HKNJANUM  (IDYY)
                                           TO  ONS-AIDLST-FTNJANUM
      *            受給者番号
                   MOVE    WRK-QUAAPPREQ3-RES-NUM  (IDYY)
                                           TO  ONS-AIDLST-JKYSNUM
      *            連番
                   ADD     1               TO  WRK-RENNUM
                   MOVE    WRK-RENNUM      TO  ONS-AIDLST-RENNUM
      *            交付番号 (2024-12 訪問のみ)
                   MOVE    QUAAPPREQ3-HO-FUJYO-KOFUNUM (IDYY IDZZ)
                                           TO  WRK-HENKAN-IN
                   PERFORM 803-HANKAKU-HEN-SEC
                   MOVE    WRK-HENKAN-OT   TO  ONS-AIDLST-KOFUNUM
      *            診療年月 (2024-12 訪問のみ)
                   MOVE    QUAAPPREQ3-HO-FUJYO-SRYYM (IDYY IDZZ)
                                           TO  ONS-AIDLST-SRYYM
      *            診療別 (2024-12 訪問のみ)
                   MOVE    QUAAPPREQ3-HO-MEDICAL-TYPE (IDYY IDZZ)
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
       20052-ONSHI-AIDLST-SYORI-EXT.
           EXIT.
      *     
      *****************************************************************
      *    資格確認訪問診療レコード作成処理
      *****************************************************************
       20053-ONSHI-HOUMON-SYORI-SEC    SECTION.
      *
           IF      ONSKAKU-PTID    >   0
               INITIALIZE                  ONSHI-HOUMON-REC
               MOVE    SPA-HOSPNUM     TO  ONS-HOUMON-HOSPNUM
               MOVE    ONSKAKU-PTID    TO  ONS-HOUMON-PTID
               MOVE    ONSKAKU-ONSHINRYO-KBN
                                       TO  ONS-HOUMON-ONSHINRYO-KBN
               MOVE    ONSHI-HOUMON-REC
                                       TO  MCPDATA-REC
               MOVE    "key4"          TO  WRK-MCP-PATHNAME
               PERFORM 900-ONSHI-HOUMON-READ-SEC
           ELSE
      *        PTIDが設定できなかったとき     
               IF      QUAAPPREQ3-CARD-CLASS  (IDYY)
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
               MOVE    ONSKAKU-ONSHINRYO-KBN
                                       TO  ONS-HOUMON-ONSHINRYO-KBN
           END-IF
           MOVE    SPACE           TO  ONS-HOUMON-SHOKAI-KBN
           IF    ( QUAAPPREQ3-CARD-CLASS  (IDYY) 
                                   =   "A1"  )
      *        医療扶助のとき
               MOVE    QUAAPPREQ3-RES-HKNJANUM  (IDYY)
                                           TO  ONS-HOUMON-FTNJANUM
               MOVE    WRK-QUAAPPREQ3-RES-NUM  (IDYY)
                                           TO  ONS-HOUMON-JKYSNUM
            ELSE
               MOVE    QUAAPPREQ3-RES-HKNJANUM  (IDYY)
                                           TO  ONS-HOUMON-HKNJANUM
               MOVE    WRK-QUAAPPREQ3-RES-KIGO  (IDYY)
                                           TO  ONS-HOUMON-KIGO
               MOVE    WRK-QUAAPPREQ3-RES-NUM   (IDYY)
                                           TO  ONS-HOUMON-NUM
               MOVE    QUAAPPREQ3-RES-EDABAN    (IDYY)
                                           TO  ONS-HOUMON-EDABAN
           END-IF
           MOVE    QUAAPPREQ3-HO-DOUI-STARYMD
                                   TO  ONS-HOUMON-DOUI-STARTYMD
           MOVE    QUAAPPREQ3-HO-DOUI-ENDYMD
                                   TO  ONS-HOUMON-DOUI-ENDYMD
      *
           MOVE    ONSKAKU-KAKU-KAKUNINYMD
                                   TO  ONS-HOUMON-KAKU-KAKUNINYMD
           MOVE    ONSKAKU-KAKU-KAKUNINHMS
                                   TO  ONS-HOUMON-KAKU-KAKUNINHMS
           MOVE    ONSKAKU-GENDO-DOUIFLG
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
       20053-ONSHI-HOUMON-SYORI-EXT.
           EXIT.
      *     
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *     
      *    エラーコードが設定されたとき
           IF      WRK-ERRCD   NOT =   SPACE    
               MOVE    "STM"               TO  SJOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
               MOVE    WRK-ERRCD           TO  JOB-YOBI
              PERFORM 900-CALL-ORCSJOB-SEC
           ELSE   
      *        ジョブ管理更新処理（処理件数）
               PERFORM 20031-JOBKANRI-SYORI-SEC
           END-IF
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           CLOSE   XML-FILE
      *     
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "*** ORAPI071BR1V2 OUT:" CNT-OUT
           DISPLAY "*** ORAPI071BR1V2 END ***"
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理更新処理（処理件数）
      *****************************************************************
       20031-JOBKANRI-SYORI-SEC        SECTION.
      *
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           PERFORM 900-JOBKANRI-READ-SEC
           IF      FLG-JOBKANRI    =   1
               MOVE    "E17"           TO  WRK-ERRCD
           ELSE
               MOVE    WRK-CNT         TO  JOB-UPDCNT
      *
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
       20031-JOBKANRI-SYORI-EXT.
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
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *     
      *    ステップ途中処理
           MOVE    "STM"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-ERRCD           TO  JOB-YOBI
              PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           EVALUATE    WRK-ERRCD
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
           END-EVALUATE
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＸＭＬファイル読込処理
      *****************************************************************
       900-XML-READ-SEC            SECTION.
      *
           READ    XML-FILE
               AT  END
                   MOVE    1           TO  FLG-END
           END-READ         
           .
       900-XML-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理マスター読込処理
      *****************************************************************
       900-JOBKANRI-READ-SEC       SECTION.
      * 
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jobkanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
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
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-JOBKANRI-READ-EXT.
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
                                       NOT =   HEAD-QUAREQ3-HO-SRYKBN
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
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　ＣＯＭＭＩＴ処理
      *****************************************************************
       900-DBCOMMIT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBCOMMIT-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢ　ＳＴＡＲＴ処理
      *****************************************************************
       900-DBSTART-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSTART-EXT.
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

