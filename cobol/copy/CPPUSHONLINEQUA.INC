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
       01  PUSHQUA-REC.
           03  PUSHQUA-EVENT                   PIC X(50).
           03  PUSHQUA-BODY.
      *        医療機関番号
               05  PUSHQUA-HOSPNUM             PIC X(2).
      *        患者番号
               05  PUSHQUA-PATIEND-ID          PIC X(20).
      *        処理日
               05  PUSHQUA-INFORMATION-DATE    PIC X(10).
      *        処理時間
               05  PUSHQUA-INFORMATION-TIME    PIC X(8).
      *
               05  PUSHQUA-CLASS               PIC X(4).
      *        UUID
               05  PUSHQUA-UUID                PIC X(36).
      *        資格確認日（予約日）
               05  PUSHQUA-KAKUNINYMD          PIC X(10).
      ****     期間区分（1:同意年月日、2:診療年月日）
               05  PUSHQUA-KIKANKBN            PIC X(01).
      *****         
      *2024/04
      *        開始日付
               05  PUSHQUA-STRYMD              PIC X(10).
      *        終了日付
               05  PUSHQUA-ENDYMD              PIC X(10).
      *        照会区分（2:訪問診療、3:オンライン診療）
               05  PUSHQUA-SHOKAIKBN           PIC X(01).
      *2024/11
      *        プロセスクラス（訪問同意等の識別)
               05  PUSHQUA-PROCESS-CLASS       PIC X(5).
      
