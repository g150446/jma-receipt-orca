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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      ******************************************************************
      *    ＡＰＩレスポンス点数マスタ情報取得
      *                                      COPY (CPMEDICATIONGETV2RES.INC)
      ******************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
        01  XML-MEDICATIONGETRES.
            03  MEDIRES-CONTEXT           PIC S9(9)   BINARY.
            03  MEDIRES-OBJECT            PIC X(36).
            03  MEDIRES-MODE              PIC S9(9)   BINARY.
            03  MEDIRES-RECORDNAME        PIC X(128).
            03  MEDIRES-MEDICATIONRES.
      *       実施日
              05  MEDIRES-INFORMATION-DATE    PIC X(10).
      *       実施時間
              05  MEDIRES-INFORMATION-TIME    PIC X(8).
      *       返却コード
              05  MEDIRES-API-RESULT          PIC X(3).
      *       返却メッセージ
              05  MEDIRES-API-RESULT-MSG      PIC X(100).
      *
              05  MEDIRES-RESKEY              PIC X(100).
      *       送信情報
              05  MEDIRES-INFOMATION.
      *           リクエストコード
                  07  MEDIRES-REQUEST-CODE         PIC X(20).
      *           基準日付
                  07  MEDIRES-BASE-DATE            PIC X(10).
      *       点数マスタ情報
              05  MEDIRES-MEDICATION-INF.
      *           診療コード
                  07  MEDIRES-MEDICATION-CODE      PIC X(09).
      *           漢字名称
                  07  MEDIRES-MEDICATION-NAME      PIC X(200).
      *           カナ名称
                  07  MEDIRES-MEDICATION-KNAME     PIC X(200).
      *           単位コード
                  07  MEDIRES-UNIT-CODE            PIC X(3).
      *           単位名称
                  07  MEDIRES-UNIT-NAME            PIC X(24).
      *           開始日
                  07  MEDIRES-START-DATE           PIC X(10).
      *           終了日
                  07  MEDIRES-END-DATE             PIC X(10).
      *
      *       選択式コメント一覧情報
              05  MEDIRES-RECEKISAI-COM-INF.
                  07  MEDIRES-RECEKISAI-TBL       OCCURS   200.
      *               コメントコード
                      09  MEDIRES-RKISAI-CODE      PIC X(09).
                      09  MEDIRES-RKISAI-NAME      PIC X(200).
      *               項番
                      09  MEDIRES-RKISAI-ITEMNO        PIC X(04).
      *               枝番
                      09  MEDIRES-RKISAI-EDANO         PIC X(02).
      *               区分
                      09  MEDIRES-RKISAI-KUBUN         PIC X(64).
      *               条件区分
                      09  MEDIRES-RKISAI-JYOKEN         PIC X(02).
      *               非算定理由コメント
                      09  MEDIRES-RKISAI-HISANTEI      PIC X(01).
      *               処理区分
                      09  MEDIRES-RKISAI-SYORIKBN      PIC X(01).
      *               診療行為名称等（点数マスタ登録なし）
                      09  MEDIRES-RKISAI-MEISYOETC     PIC X(300).
      *
