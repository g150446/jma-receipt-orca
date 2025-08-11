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
      *----------------------------------------------------------*
      *    電子処方箋データ出力パラメタ    COPY (CPORCSEPRECSV.INC)
      *                                        (ORCHC02QEPRE02 で使用）
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 05.02.00     ORCAMO       25/04/XX  使用期限年月日等追加
      ************************************************************
      *
       01  ORCSEPRECSVAREA.
      *    API:CSVテーブル返却
           03  ORCSEPRECSV-SYORIKBN          PIC X(03).
      *2025/04 CHK:チェック用
           03  ORCSEPRECSV-CHKKBN            PIC X(03).
      *
      *    共通情報
           03  ORCSEPRECSV-IN01-G.
      *        県番号
               05  ORCSEPRECSV-PREFNUM       PIC  9(02).
      *        医療機関コード
               05  ORCSEPRECSV-HOSPCD        PIC  9(07).
      *        医療機関種別
               05  ORCSEPRECSV-HOSPSBT       PIC  9(01).
      *        医療機関名称
               05  ORCSEPRECSV-HOSPNAME      PIC  X(180).
      *        医療機関郵便
               05  ORCSEPRECSV-HOSPPOST      PIC  X(08).
      *        医療機関住所
               05  ORCSEPRECSV-HOSPADRS      PIC  X(150).
      *        医療機関電話番号
               05  ORCSEPRECSV-HOSPTEL       PIC  X(13).
      *        医療機関FAX
               05  ORCSEPRECSV-HOSPFAX       PIC  X(13).
      *        診療科
               05  ORCSEPRECSV-RECESRYKA     PIC  X(02).
               05  ORCSEPRECSV-SRYKANAME     PIC  X(120).
      *        医師情報
      *        フリガナ
               05  ORCSEPRECSV-DR-KANANAME   PIC  X(60).
      *        氏名
               05  ORCSEPRECSV-DR-NAME       PIC  X(60).
      *        医師コード
               05  ORCSEPRECSV-DR-CD         PIC  X(15).
      *        麻薬施用コード
               05  ORCSEPRECSV-MAYAKULICENSE PIC  X(60).
      *        患者情報
               05  ORCSEPRECSV-PTNUM         PIC  X(15).
               05  ORCSEPRECSV-NAME          PIC  X(60).
               05  ORCSEPRECSV-KANANAME      PIC  X(60).
               05  ORCSEPRECSV-SEX           PIC  X(01).
               05  ORCSEPRECSV-BIRTHDAY      PIC  X(08).
               05  ORCSEPRECSV-ADRS          PIC  X(150).
               05  ORCSEPRECSV-TEL           PIC  X(13).
      *        患者保険情報
               05  ORCSEPRECSV-HKNCOMBINUM   PIC  X(04).
      *        患者公費ＩＤ
               05  ORCSEPRECSV-KOHID-G       OCCURS   4.
                   07  ORCSEPRECSV-KOHID         PIC 9(10).
                   07  ORCSEPRECSV-KOHHKNNUM     PIC X(03).
      *R05.3
      *            月代わり受給者番号
                   07  ORCSEPRECSV-MONTH-JKYSNUM PIC X(20).
      *        麻毒フラグ
               05  ORCSEPRECSV-MAYAKU        PIC  9(01).
      *        一部負担区分
               05  ORCSEPRECSV-BIKO-FTN      PIC  X(01).
      *        残薬確認指示
               05  ORCSEPRECSV-ZANSIJI       PIC  X(01).
      ****
      *        分割指示　回数
      *         05  ORCSEPRECSV-BUNKATU-MAX   PIC  9(01).
      *        分割指示　回
      **       05  ORCSEPRECSV-BUNKATU-CNT   PIC  9(01).
      *
      *        フィリル回数
               05  ORCSEPRECSV-REFILL-CNT      PIC  9(01).
      *2025/04
      *        処方箋使用期限年月日
               05  ORCSEPRECSV-KIKAN-YMD       PIC  X(08).
      *
      *    備考内容
           03  ORCSEPRECSV-BIKOU-AREA.
               05  ORCSEPRECSV-BIKOU-TBL       OCCURS   20.
                   07  ORCSEPRECSV-BIKOU       PIC X(100).
      *            備考 種別
                   07  ORCSEPRECSV-BIKOU-KBN   PIC X(02).
      *2024/03
      *            備考 公費ID
                   07  ORCSEPRECSV-BIKOU-KOHID PIC 9(10).
      *
      *    剤毎の内容
           03  ORCSEPRECSV-NAIYOU-AREA.
               05  ORCSEPRECSV-NAIYOU-TBL       OCCURS   50.
                   07  ORCSEPRECSV-ZAIKATA          PIC X(01).
                   07  ORCSEPRECSV-KAISU            PIC 9(03).
      *            分割総投与日数
                   07  ORCSEPRECSV-GOKKAISU         PIC 9(03).
      *2023/04     07  ORCSEPRECSV-NAIYOU-TBL       OCCURS   50.
                   07  ORCSEPRECSV-NAIYOU-TBL       OCCURS   100.
                       09  ORCSEPRECSV-SRYCD            PIC X(09).
                       09  ORCSEPRECSV-TNS-NAME         PIC X(180).
                       09  ORCSEPRECSV-SURYOU           PIC 9(05)V9(05).
                       09  ORCSEPRECSV-YKZKBN           PIC X(01).
                       09  ORCSEPRECSV-TANINAME         PIC X(12).
                       09  ORCSEPRECSV-KISAIKBN         PIC X(01).
                       09  ORCSEPRECSV-KANZANCHI        PIC 9(05)V9(5).
      *                用法コメント区分
                       09  ORCSEPRECSV-YCOMMNTKBN       PIC X(01).
      *                一般名コード（薬価基準コード）
                       09  ORCSEPRECSV-YAKKAKJNCD       PIC X(13).
      *2025/04         一般名コード
                       09  ORCSEPRECSV-GECODE           PIC X(12).
      *                薬剤名称（省略不可）
                       09  ORCSEPRECSV-CSV-NAME         PIC X(180).
      *                商品名
                       09  ORCSEPRECSV-SHO-NAME         PIC X(180).
      *                内服１回量
                       09  ORCSEPRECSV-SURYOU-SG        PIC 9(05)V9(05).
      *                内服服用回数
                       09  ORCSEPRECSV-KAISU-SG         PIC 9(03).
      *2025/01         経過措置年月日
                       09  ORCSEPRECSV-KEIKASOCHIYMD    PIC X(08).
      *
      *    処方箋情報
           03  ORCSEPRECSV-PRESCRIPTION-INFO.
      *        処方箋発行形態
               05  ORCSEPRECSV-PRE-ISSUESELECT   PIC X(1).
      *        処方箋ID
               05  ORCSEPRECSV-PRE-ID            PIC X(36).
      *        引換番号
               05  ORCSEPRECSV-ACCESS-CODE       PIC X(16).
      *        処方箋コメント情報
               05  ORCSEPRECSV-COMMENT-G.
                   07   ORCSEPRECSV-COMMENT-OCC   OCCURS  10.
                        09   ORCSEPRECSV-PRE-NAME     PIC X(180).
                        09   ORCSEPRECSV-PRE-COMMENT  PIC X(300).
      *
      *    ファイル名組み立て項目
           03  ORCSEPRECSV-SYSTIME       PIC 9(08).
      *    エラーコード
           03  ORCSEPRECSV-ERRCD         PIC 9(01).
      *    qrencode return-code
      *     03  ORCSEPRECSV-RET                PIC X(01).
      *    ＣＳＶファイル名称
           03  ORCSEPRECSV-CSVFILE            PIC X(1024).
      *
      *    APIの時ＣＳＶデータを返却
           03  ORCSEPRECSV-CSVDATA-AREA.
               05  ORCSEPRECSV-CSVDATA-OCC       OCCURS   500.
                   07  ORCSEPRECSV-CSVDATA       PIC X(500).
               05  ORCSEPRECSV-CSVMAX            PIC 9(04).
      *2025/01
      *    警告コード区分１（ダミーコード有り）
           03  ORCSEPRECSV-WARNING1         PIC 9(01).
      *    警告コード区分２（経過措置年月日有り）
           03  ORCSEPRECSV-WARNING2         PIC 9(01).
      *
