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
      *----------------------------------------------------------------*
      *   リハビリコードテーブル（労災外） COPY (CMRIHATBL2024.INC)
      *----------------------------------------------------------------*
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *    リハビリのコード
       01  TBL-RIHABIRI2024-AREA.
      *    心大血管疾患リハビリテーション料（１）（理学療法士による場合）
           03  FILLER          PIC X(16)   VALUE   "111   180744210".
           03  FILLER          PIC X(16)   VALUE   "112   180744310".
           03  FILLER          PIC X(16)   VALUE   "113   180744410".
           03  FILLER          PIC X(16)   VALUE   "114   180744510".
           03  FILLER          PIC X(16)   VALUE   "115   180744610".
      *    心大血管疾患リハビリテーション料（２）
           03  FILLER          PIC X(16)   VALUE   "121   180744710".
           03  FILLER          PIC X(16)   VALUE   "122   180744810".
           03  FILLER          PIC X(16)   VALUE   "123   180744910".
           03  FILLER          PIC X(16)   VALUE   "124   180745010".
           03  FILLER          PIC X(16)   VALUE   "125   180745110".
      *
      *    脳血管疾患等リハビリテーション料（１）（理学療法士による場合）
           03  FILLER          PIC X(16)   VALUE   "211   180745310".
           03  FILLER          PIC X(16)   VALUE   "212   180745410".
           03  FILLER          PIC X(16)   VALUE   "213   180745510".
           03  FILLER          PIC X(16)   VALUE   "214   180745610".
      *    脳血管疾患等リハビリテーション料（２）（理学療法士による場合）
           03  FILLER          PIC X(16)   VALUE   "221   180745710".
           03  FILLER          PIC X(16)   VALUE   "222   180745810".
           03  FILLER          PIC X(16)   VALUE   "223   180745910".
           03  FILLER          PIC X(16)   VALUE   "224   180746010".
      *    脳血管疾患等リハビリテーション料（３）（理学療法士による場合）
           03  FILLER          PIC X(16)   VALUE   "231   180746110".
           03  FILLER          PIC X(16)   VALUE   "232   180746210".
           03  FILLER          PIC X(16)   VALUE   "233   180746310".
           03  FILLER          PIC X(16)   VALUE   "234   180746410".
           03  FILLER          PIC X(16)   VALUE   "235   180746510".
      *    脳血管疾患等リハビリテーション料（１）（要介護）（理学療法士）
           03  FILLER          PIC X(16)   VALUE   "2111  180746610".
           03  FILLER          PIC X(16)   VALUE   "2121  180746710".
           03  FILLER          PIC X(16)   VALUE   "2131  180746810".
           03  FILLER          PIC X(16)   VALUE   "2141  180746910".
      *    脳血管疾患等リハビリテーション料（２）（要介護）（理学療法士）
           03  FILLER          PIC X(16)   VALUE   "2211  180747010".
           03  FILLER          PIC X(16)   VALUE   "2221  180747110".
           03  FILLER          PIC X(16)   VALUE   "2231  180747210".
           03  FILLER          PIC X(16)   VALUE   "2241  180747310".
      *    脳血管疾患等リハビリテーション料（３）（要介護）（理学療法士）
           03  FILLER          PIC X(16)   VALUE   "2311  180747410".
           03  FILLER          PIC X(16)   VALUE   "2321  180747510".
           03  FILLER          PIC X(16)   VALUE   "2331  180747610".
           03  FILLER          PIC X(16)   VALUE   "2341  180747710".
           03  FILLER          PIC X(16)   VALUE   "2351  180747810".
      *    脳血管疾患等リハビリテーション料（１）（理学療法士）（リ減）
           03  FILLER          PIC X(16)   VALUE   "211 G 180747930".
           03  FILLER          PIC X(16)   VALUE   "212 G 180748030".
           03  FILLER          PIC X(16)   VALUE   "213 G 180748130".
           03  FILLER          PIC X(16)   VALUE   "214 G 180748230".
      *    脳血管疾患等リハビリテーション料（２）（理学療法士）（リ減）
           03  FILLER          PIC X(16)   VALUE   "221 G 180748330".
           03  FILLER          PIC X(16)   VALUE   "222 G 180748430".
           03  FILLER          PIC X(16)   VALUE   "223 G 180748530".
           03  FILLER          PIC X(16)   VALUE   "224 G 180748630".
      *    脳血管疾患等リハビリテーション料（３）（理学療法士）（リ減）
           03  FILLER          PIC X(16)   VALUE   "231 G 180748730".
           03  FILLER          PIC X(16)   VALUE   "232 G 180748830".
           03  FILLER          PIC X(16)   VALUE   "233 G 180748930".
           03  FILLER          PIC X(16)   VALUE   "234 G 180749030".
           03  FILLER          PIC X(16)   VALUE   "235 G 180749130".
      *    脳血管疾患等リハビリテーション料（１）（要介護）（理学）（リ減）
           03  FILLER          PIC X(16)   VALUE   "2111G 180749230".
           03  FILLER          PIC X(16)   VALUE   "2121G 180749330".
           03  FILLER          PIC X(16)   VALUE   "2131G 180749430".
           03  FILLER          PIC X(16)   VALUE   "2141G 180749530".
      *    脳血管疾患等リハビリテーション料（２）（要介護）（理学）（リ減）
           03  FILLER          PIC X(16)   VALUE   "2211G 180749630".
           03  FILLER          PIC X(16)   VALUE   "2221G 180749730".
           03  FILLER          PIC X(16)   VALUE   "2231G 180749830".
           03  FILLER          PIC X(16)   VALUE   "2241G 180749930".
      *    脳血管疾患等リハビリテーション料（３）（要介護）（理学）（リ減）
           03  FILLER          PIC X(16)   VALUE   "2311G 180750030".
           03  FILLER          PIC X(16)   VALUE   "2321G 180750130".
           03  FILLER          PIC X(16)   VALUE   "2331G 180750230".
           03  FILLER          PIC X(16)   VALUE   "2341G 180750330".
           03  FILLER          PIC X(16)   VALUE   "2351G 180750430".
      *
      *    廃用症候群リハビリテーション料（１）（理学療法士による場合）
           03  FILLER          PIC X(16)   VALUE   "911   180750510".
           03  FILLER          PIC X(16)   VALUE   "912   180750610".
           03  FILLER          PIC X(16)   VALUE   "913   180750710".
           03  FILLER          PIC X(16)   VALUE   "914   180750810".
      *    廃用症候群リハビリテーション料（２）（理学療法士による場合）
           03  FILLER          PIC X(16)   VALUE   "921   180750910".
           03  FILLER          PIC X(16)   VALUE   "922   180751010".
           03  FILLER          PIC X(16)   VALUE   "923   180751110".
           03  FILLER          PIC X(16)   VALUE   "924   180751210".
      *    廃用症候群リハビリテーション料（３）（理学療法士による場合）
           03  FILLER          PIC X(16)   VALUE   "931   180751310".
           03  FILLER          PIC X(16)   VALUE   "932   180751410".
           03  FILLER          PIC X(16)   VALUE   "933   180751510".
           03  FILLER          PIC X(16)   VALUE   "934   180751610".
           03  FILLER          PIC X(16)   VALUE   "935   180751710".
      *    廃用症候群リハビリテーション料（１）（要介護）（理学）
           03  FILLER          PIC X(16)   VALUE   "9111  180751810".
           03  FILLER          PIC X(16)   VALUE   "9121  180751910".
           03  FILLER          PIC X(16)   VALUE   "9131  180752010".
           03  FILLER          PIC X(16)   VALUE   "9141  180752110".
      *    廃用症候群リハビリテーション料（２）（要介護）（理学）
           03  FILLER          PIC X(16)   VALUE   "9211  180752210".
           03  FILLER          PIC X(16)   VALUE   "9221  180752310".
           03  FILLER          PIC X(16)   VALUE   "9231  180752410".
           03  FILLER          PIC X(16)   VALUE   "9241  180752510".
      *    廃用症候群リハビリテーション料（３）（要介護）（理学）
           03  FILLER          PIC X(16)   VALUE   "9311  180752610".
           03  FILLER          PIC X(16)   VALUE   "9321  180752710".
           03  FILLER          PIC X(16)   VALUE   "9331  180752810".
           03  FILLER          PIC X(16)   VALUE   "9341  180752910".
           03  FILLER          PIC X(16)   VALUE   "9351  180753010".
      *    廃用症候群リハビリテーション料（１）（理学）（リ減）
           03  FILLER          PIC X(16)   VALUE   "911 G 180753130".
           03  FILLER          PIC X(16)   VALUE   "912 G 180753230".
           03  FILLER          PIC X(16)   VALUE   "913 G 180753330".
           03  FILLER          PIC X(16)   VALUE   "914 G 180753430".
      *    廃用症候群リハビリテーション料（２）（理学）（リ減）
           03  FILLER          PIC X(16)   VALUE   "921 G 180753530".
           03  FILLER          PIC X(16)   VALUE   "922 G 180753630".
           03  FILLER          PIC X(16)   VALUE   "923 G 180753730".
           03  FILLER          PIC X(16)   VALUE   "924 G 180753830".
      *    廃用症候群リハビリテーション料（３）（理学）（リ減）
           03  FILLER          PIC X(16)   VALUE   "931 G 180753930".
           03  FILLER          PIC X(16)   VALUE   "932 G 180754030".
           03  FILLER          PIC X(16)   VALUE   "933 G 180754130".
           03  FILLER          PIC X(16)   VALUE   "934 G 180754230".
           03  FILLER          PIC X(16)   VALUE   "935 G 180754330".
      *    廃用症候群リハビリテーション料（１）（要介護）（理学）（リ減）
           03  FILLER          PIC X(16)   VALUE   "9111G 180754430".
           03  FILLER          PIC X(16)   VALUE   "9121G 180754530".
           03  FILLER          PIC X(16)   VALUE   "9131G 180754630".
           03  FILLER          PIC X(16)   VALUE   "9141G 180754730".
      *    廃用症候群リハビリテーション料（２）（要介護）（理学）（リ減）
           03  FILLER          PIC X(16)   VALUE   "9211G 180754830".
           03  FILLER          PIC X(16)   VALUE   "9221G 180754930".
           03  FILLER          PIC X(16)   VALUE   "9231G 180755030".
           03  FILLER          PIC X(16)   VALUE   "9241G 180755130".
      *    廃用症候群リハビリテーション料（３）（要介護）（理学）（リ減）
           03  FILLER          PIC X(16)   VALUE   "9311G 180755230".
           03  FILLER          PIC X(16)   VALUE   "9321G 180755330".
           03  FILLER          PIC X(16)   VALUE   "9331G 180755430".
           03  FILLER          PIC X(16)   VALUE   "9341G 180755530".
           03  FILLER          PIC X(16)   VALUE   "9351G 180755630".
      *
      *    運動器リハビリテーション料（１）（理学療法士による場合）
           03  FILLER          PIC X(16)   VALUE   "311   180755710".
           03  FILLER          PIC X(16)   VALUE   "312   180755810".
           03  FILLER          PIC X(16)   VALUE   "313   180755910".
      *    運動器リハビリテーション料（２）（理学療法士による場合）
           03  FILLER          PIC X(16)   VALUE   "321   180756010".
           03  FILLER          PIC X(16)   VALUE   "322   180756110".
           03  FILLER          PIC X(16)   VALUE   "323   180756210".
      *    運動器リハビリテーション料（３）（理学療法士による場合）
           03  FILLER          PIC X(16)   VALUE   "331   180756310".
           03  FILLER          PIC X(16)   VALUE   "332   180756410".
           03  FILLER          PIC X(16)   VALUE   "333   180756510".
           03  FILLER          PIC X(16)   VALUE   "334   180756610".
      *    運動器リハビリテーション料（１）（理学療法士による場合）（リ減）
           03  FILLER          PIC X(16)   VALUE   "311 G 180756730".
           03  FILLER          PIC X(16)   VALUE   "312 G 180756830".
           03  FILLER          PIC X(16)   VALUE   "313 G 180756930".
      *    運動器リハビリテーション料（２）（理学療法士による場合）（リ減）
           03  FILLER          PIC X(16)   VALUE   "321 G 180757030".
           03  FILLER          PIC X(16)   VALUE   "322 G 180757130".
           03  FILLER          PIC X(16)   VALUE   "323 G 180757230".
      *    運動器リハビリテーション料（３）（理学療法士による場合）（リ減）
           03  FILLER          PIC X(16)   VALUE   "331 G 180757330".
           03  FILLER          PIC X(16)   VALUE   "332 G 180757430".
           03  FILLER          PIC X(16)   VALUE   "333 G 180757530".
           03  FILLER          PIC X(16)   VALUE   "334 G 180757630".
      *   運動器リハビリテーション料（１）（要介護）（理学）
           03  FILLER          PIC X(16)   VALUE   "3111  180757710".
           03  FILLER          PIC X(16)   VALUE   "3121  180757810".
           03  FILLER          PIC X(16)   VALUE   "3131  180757910".
      *   運動器リハビリテーション料（２）（要介護）（理学）
           03  FILLER          PIC X(16)   VALUE   "3211  180758010".
           03  FILLER          PIC X(16)   VALUE   "3221  180758110".
           03  FILLER          PIC X(16)   VALUE   "3231  180758210".
      *   運動器リハビリテーション料（３）（要介護）（理学）
           03  FILLER          PIC X(16)   VALUE   "3311  180758310".
           03  FILLER          PIC X(16)   VALUE   "3321  180758410".
           03  FILLER          PIC X(16)   VALUE   "3331  180758510".
           03  FILLER          PIC X(16)   VALUE   "3341  180758610".
      *   運動器リハビリテーション料（１）（要介護）（理学）（リ減）
           03  FILLER          PIC X(16)   VALUE   "311G 180758730".
           03  FILLER          PIC X(16)   VALUE   "3121G 180758830".
           03  FILLER          PIC X(16)   VALUE   "3131G 180758930".
      *   運動器リハビリテーション料（２）（要介護）（理学）（リ減）
           03  FILLER          PIC X(16)   VALUE   "3111G 180759030".
           03  FILLER          PIC X(16)   VALUE   "3121G 180759130".
           03  FILLER          PIC X(16)   VALUE   "3131G 180759230".
      *   運動器リハビリテーション料（３）（要介護）（理学）（リ減）
           03  FILLER          PIC X(16)   VALUE   "3111G 180759330".
           03  FILLER          PIC X(16)   VALUE   "3121G 180759430".
           03  FILLER          PIC X(16)   VALUE   "3131G 180759530".
           03  FILLER          PIC X(16)   VALUE   "3141G 180759630".
      *
      *    呼吸器リハビリテーション料（１）（理学療法士による場合）
           03  FILLER          PIC X(16)   VALUE   "411   180759710".
           03  FILLER          PIC X(16)   VALUE   "412   180759810".
           03  FILLER          PIC X(16)   VALUE   "413   180759910".
           03  FILLER          PIC X(16)   VALUE   "414   180760010".
      *    呼吸器リハビリテーション料（２）（理学療法士による場合）
           03  FILLER          PIC X(16)   VALUE   "421   180760110".
           03  FILLER          PIC X(16)   VALUE   "422   180760210".
           03  FILLER          PIC X(16)   VALUE   "423   180760310".
           03  FILLER          PIC X(16)   VALUE   "424   180760410".
      *
      *
       01  TBL-RIHABIRI2024-AREA-R REDEFINES   TBL-RIHABIRI2024-AREA.
           03  TBL-RIHABIRI2024-OCC   OCCURS  162  INDEXED   TBL-RIH24.
      *        リハビリ種類
               05  TBL-RIH24-KBN        PIC X(01).
      *        1:(1),2:(2),3:(3)
               05  TBL-RIH24-KBN2       PIC X(01).
      *         1:（理学）から5:
               05  TBL-RIH24-KBN3       PIC X(01).
      *        1:要介護
               05  TBL-RIH24-KIG        PIC X(01).
      *        G:減点
               05  TBL-RIH24-GENKBN     PIC X(01).
      *
               05  TBL-RIH24-YOBI       PIC X(01).
      *
               05  TBL-RIH24-SRYCD      PIC X(09).
      *
               05  TBL-RIH24-YOBI2       PIC X(01).
      *
       01  TBL-RIHA2024-MAX            PIC 9(04)  VALUE    162.
      *
