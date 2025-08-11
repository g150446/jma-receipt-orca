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
      *    時間外加算コードテーブル　令和４年４月改正から            *
      *                         COPY (CMTIMEKSN2022.INC)               *
      *----------------------------------------------------------------*
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
      *    時間外加算テーブル
       01  TBL-TIMEKSN22-AREA.
      *初診
      *    時間外
           03  FILLER              PIC X(14)   VALUE   "11   111000570".
      *    休日
           03  FILLER              PIC X(14)   VALUE   "12   111000670".
      *    深夜
           03  FILLER              PIC X(14)   VALUE   "13   111000770".
      *    時間外特例
           03  FILLER              PIC X(14)   VALUE   "14   111000870".
      *    早朝加算
           03  FILLER              PIC X(14)   VALUE   "18   111012470".
      *（乳幼児） 時間外
           03  FILLER              PIC X(14)   VALUE   "1101 111011970".
      *    休日
           03  FILLER              PIC X(14)   VALUE   "1201 111012070".
      *    深夜
           03  FILLER              PIC X(14)   VALUE   "1301 111012170".
      *    時間外特例
           03  FILLER              PIC X(14)   VALUE   "1401 111012270".
      *    夜間（時間外）加算
           03  FILLER              PIC X(14)   VALUE   "1501 111011570".
      *    休日加算
           03  FILLER              PIC X(14)   VALUE   "1601 111011670".
      *    深夜加算
           03  FILLER              PIC X(14)   VALUE   "1701 111011770".
      *再診料加算
      *    時間外
           03  FILLER              PIC X(14)   VALUE   "21   112001110".
      *    休日
           03  FILLER              PIC X(14)   VALUE   "22   112001210".
      *    深夜
           03  FILLER              PIC X(14)   VALUE   "23   112001310".
      *    時間外特例
           03  FILLER              PIC X(14)   VALUE   "24   112001410".
      *    早朝加算
           03  FILLER              PIC X(14)   VALUE   "28   112015570".
      *（乳幼児）    時間外
           03  FILLER              PIC X(14)   VALUE   "2102 112014770".
      *    休日
           03  FILLER              PIC X(14)   VALUE   "2202 112014870".
      *    深夜
           03  FILLER              PIC X(14)   VALUE   "2302 112014970".
      *    時間外特例
           03  FILLER              PIC X(14)   VALUE   "2402 112015070".
      *    夜間（時間外）加算
           03  FILLER              PIC X(14)   VALUE   "2502 112014170".
      *    休日加算
           03  FILLER              PIC X(14)   VALUE   "2602 112014270".
      *    深夜加算
           03  FILLER              PIC X(14)   VALUE   "2702 112014370".
      *外来診察料加算
      *    時間外
           03  FILLER              PIC X(14)   VALUE   "21  G112006470".
      *    休日
           03  FILLER              PIC X(14)   VALUE   "22  G112006570".
      *    深夜
           03  FILLER              PIC X(14)   VALUE   "23  G112006670".
      *    時間外特例
           03  FILLER              PIC X(14)   VALUE   "24  G112006770".
      *（乳幼児）    時間外
           03  FILLER              PIC X(14)   VALUE   "2103 112015170".
      *    休日
           03  FILLER              PIC X(14)   VALUE   "2203 112015270".
      *    深夜
           03  FILLER              PIC X(14)   VALUE   "2303 112015370".
      *    時間外特例
           03  FILLER              PIC X(14)   VALUE   "2403 112015470".
      *    夜間（時間外）加算
           03  FILLER              PIC X(14)   VALUE   "2503 112014470".
      *    休日加算
           03  FILLER              PIC X(14)   VALUE   "2603 112014570".
      *    深夜加算
           03  FILLER              PIC X(14)   VALUE   "2703 112014670".
      *小児科外来診療科（初診）
      *    時間外
           03  FILLER              PIC X(14)   VALUE   "1104 113009670".
      *    休日
           03  FILLER              PIC X(14)   VALUE   "1204 113007170".
      *    深夜
           03  FILLER              PIC X(14)   VALUE   "1304 113007270".
      *    時間外特例
           03  FILLER              PIC X(14)   VALUE   "1404 111010770".
      *    夜間（時間外）加算
           03  FILLER              PIC X(14)   VALUE   "1504 113007070".
      *    休日加算
           03  FILLER              PIC X(14)   VALUE   "1604 113007170".
      *    深夜加算
           03  FILLER              PIC X(14)   VALUE   "1704 113007270".
      *小児科外来診療科（再診）
      *    時間外
           03  FILLER              PIC X(14)   VALUE   "2105 113009770".
      *    休日
           03  FILLER              PIC X(14)   VALUE   "2205 113007470".
      *    深夜
           03  FILLER              PIC X(14)   VALUE   "2305 113007570".
      *    時間外特例
           03  FILLER              PIC X(14)   VALUE   "2405 112006070".
      *    夜間（時間外）加算
           03  FILLER              PIC X(14)   VALUE   "2505 113007370".
      *    休日加算
           03  FILLER              PIC X(14)   VALUE   "2605 113007470".
      *    深夜加算
           03  FILLER              PIC X(14)   VALUE   "2705 113007570".
      *小児科外来診療科（外来診療科）
      *    時間外
           03  FILLER              PIC X(14)   VALUE   "2106 113009870".
      *    休日
           03  FILLER              PIC X(14)   VALUE   "2206 113007770".
      *    深夜
           03  FILLER              PIC X(14)   VALUE   "2306 113007870".
      *    時間外特例
           03  FILLER              PIC X(14)   VALUE   "2406 113005570".
      *    夜間（時間外）加算
           03  FILLER              PIC X(14)   VALUE   "2506 113007670".
      *    休日加算
           03  FILLER              PIC X(14)   VALUE   "2606 113007770".
      *    深夜加算
           03  FILLER              PIC X(14)   VALUE   "2706 113007870".
      *地域包括診療料
      *    時間外
           03  FILLER              PIC X(14)   VALUE   "31   113016270".
      *    休日
           03  FILLER              PIC X(14)   VALUE   "32   113016370".
      *    深夜
           03  FILLER              PIC X(14)   VALUE   "33   113016470".
      *    時間外特例
           03  FILLER              PIC X(14)   VALUE   "34   113016870".
      *    乳幼児時間外加算
           03  FILLER              PIC X(14)   VALUE   "3107 113016570".
      *    乳幼児休日加算
           03  FILLER              PIC X(14)   VALUE   "3207 113016670".
      *    乳幼児深夜加算
           03  FILLER              PIC X(14)   VALUE   "3307 113016770".
      *    乳幼児時間外特例加算
      **** 03  FILLER              PIC X(14)   VALUE   "3407 113016870".
           03  FILLER              PIC X(14)   VALUE   "3407 113016970".
      *    小児科 乳幼児夜間
           03  FILLER              PIC X(14)   VALUE   "3507 113017070".
      *    小児科 乳幼児休日
           03  FILLER              PIC X(14)   VALUE   "3607 113017170".
      *    小児科 乳幼児深夜
           03  FILLER              PIC X(14)   VALUE   "3707 113017270".
      *    早朝加算
           03  FILLER              PIC X(14)   VALUE   "38   113017370".
      *
      *認知症地域包括診療料
      *    時間外
           03  FILLER              PIC X(14)   VALUE   "41   113018570".
      *    休日
           03  FILLER              PIC X(14)   VALUE   "42   113018670".
      *    深夜
           03  FILLER              PIC X(14)   VALUE   "43   113018770".
      *    時間外特例
           03  FILLER              PIC X(14)   VALUE   "44   113019170".
      *    乳幼児時間外加算
           03  FILLER              PIC X(14)   VALUE   "4108 113018870".
      *    乳幼児休日加算
           03  FILLER              PIC X(14)   VALUE   "4208 113018970".
      *    乳幼児深夜加算
           03  FILLER              PIC X(14)   VALUE   "4308 113019070".
      *    乳幼児時間外特例加算
           03  FILLER              PIC X(14)   VALUE   "4408 113019270".
      *    小児科 乳幼児夜間
           03  FILLER              PIC X(14)   VALUE   "4508 113019370".
      *    小児科 乳幼児休日
           03  FILLER              PIC X(14)   VALUE   "4608 113019470".
      *    小児科 乳幼児深夜
           03  FILLER              PIC X(14)   VALUE   "4708 113019570".
      *    早朝加算
           03  FILLER              PIC X(14)   VALUE   "48   113019670".
      *
      *小児かかりつけ診療科（初診）（乳幼児）
      *    乳幼児　時間外
           03  FILLER              PIC X(14)   VALUE   "1109 113020170".
      *    乳幼児　休日（小児科）
           03  FILLER              PIC X(14)   VALUE   "1209 113020670".
      *    乳幼児　深夜（小児科）
           03  FILLER              PIC X(14)   VALUE   "1309 113020770".
      *    乳幼児　時間外特例（小児科）
           03  FILLER              PIC X(14)   VALUE   "1409 113020470".
      *    乳幼児　夜間（小児科）
           03  FILLER              PIC X(14)   VALUE   "1509 113020570".
      *    乳幼児　休日（小児科）
           03  FILLER              PIC X(14)   VALUE   "1609 113020670".
      *    乳幼児　深夜（小児科）
           03  FILLER              PIC X(14)   VALUE   "1709 113020770".
      *小児かかりつけ診療科（再診）（乳幼児）
      *    乳幼児　時間外
           03  FILLER              PIC X(14)   VALUE   "2110 113020870".
      *    乳幼児　休日（小児科）
           03  FILLER              PIC X(14)   VALUE   "2210 113021370".
      *    乳幼児　深夜（小児科）
           03  FILLER              PIC X(14)   VALUE   "2310 113021470".
      *    乳幼児　時間外特例
           03  FILLER              PIC X(14)   VALUE   "2410 113021170".
      *    乳幼児　夜間（小児科）
           03  FILLER              PIC X(14)   VALUE   "2510 113021270".
      *    乳幼児　休日（小児科）
           03  FILLER              PIC X(14)   VALUE   "2610 113021370".
      *    乳幼児　深夜（小児科）
           03  FILLER              PIC X(14)   VALUE   "2710 113021470".
      *小児かかりつけ診療科(外来診療科）（乳幼児）
      *    乳幼児　時間外
           03  FILLER              PIC X(14)   VALUE   "2111 113021570".
      *    乳幼児　休日（小児科）
           03  FILLER              PIC X(14)   VALUE   "2211 113022070".
      *    乳幼児　深夜（小児科）
           03  FILLER              PIC X(14)   VALUE   "2311 113022170".
      *    乳幼児　時間外特例
           03  FILLER              PIC X(14)   VALUE   "2411 113021870".
      *    乳幼児　夜間（小児科）
           03  FILLER              PIC X(14)   VALUE   "2511 113021970".
      *    乳幼児　休日（小児科）
           03  FILLER              PIC X(14)   VALUE   "2611 113022070".
      *    乳幼児　深夜（小児科）
           03  FILLER              PIC X(14)   VALUE   "2711 113022170".
      *
      *小児かかりつけ診療科（初診）（７歳まで）
      *   時間外
           03  FILLER              PIC X(14)   VALUE   "1112 113026670".
      *    休日
           03  FILLER              PIC X(14)   VALUE   "1212 113026770".
      *    深夜
           03  FILLER              PIC X(14)   VALUE   "1312 113026870".
      *    時間外特例
           03  FILLER              PIC X(14)   VALUE   "1412 113026970".
      *小児かかりつけ診療科（再診）（７歳まで）
      *    時間外
           03  FILLER              PIC X(14)   VALUE   "2113 113027070".
      *    休日
           03  FILLER              PIC X(14)   VALUE   "2213 113027170".
      *    深夜
           03  FILLER              PIC X(14)   VALUE   "2313 113027270".
      *    時間外特例
           03  FILLER              PIC X(14)   VALUE   "2413 113027370".
      *小児かかりつけ診療科(外来診療科）（７歳まで）
      *    時間外
           03  FILLER              PIC X(14)   VALUE   "2114 113027470".
      *    休日
           03  FILLER              PIC X(14)   VALUE   "2214 113027570".
      *    深夜
           03  FILLER              PIC X(14)   VALUE   "2314 113027670".
      *    時間外特例
           03  FILLER              PIC X(14)   VALUE   "2414 113027770".
      *
      *外来腫瘍化学療法診療料
      *    時間外加算（初診）（外来腫瘍化学療法診療料）
           03  FILLER              PIC X(14)   VALUE   "5115 113038570".
      *    休日
           03  FILLER              PIC X(14)   VALUE   "5215 113038670".
      *    深夜
           03  FILLER              PIC X(14)   VALUE   "5315 113038770".
      *    時間外特例
           03  FILLER              PIC X(14)   VALUE   "5415 113038970".
      *    時間外加算（再診）（外来腫瘍化学療法診療料）
           03  FILLER              PIC X(14)   VALUE   "5116 113039570".
      *    休日
           03  FILLER              PIC X(14)   VALUE   "5216 113039670".
      *    深夜
           03  FILLER              PIC X(14)   VALUE   "5316 113039770".
      *    時間外特例
           03  FILLER              PIC X(14)   VALUE   "5416 113039970".
      *    時間外加算（外来診療料）（外来腫瘍化学療法診療料）
           03  FILLER              PIC X(14)   VALUE   "5117 113040570".
      *    休日
           03  FILLER              PIC X(14)   VALUE   "5217 113040670".
      *    深夜
           03  FILLER              PIC X(14)   VALUE   "5317 113040770".
      *    時間外特例
           03  FILLER              PIC X(14)   VALUE   "5417 113040970".
      *
      *    乳幼児時間外加算（初診）（外来腫瘍化学療法診療料）
           03  FILLER              PIC X(14)   VALUE   "5118 113038870".
      *    乳幼児休日加算（初診）（外来腫瘍化学療法診療料）
           03  FILLER              PIC X(14)   VALUE   "5218 113043670".
      *    乳幼児深夜加算（初診）（外来腫瘍化学療法診療料）
           03  FILLER              PIC X(14)   VALUE   "5318 113043770".
      *    乳幼児時間外特例医療機関加算（初診）（外来腫瘍化学療法診療料）
           03  FILLER              PIC X(14)   VALUE   "5418 113039070".
      *    乳幼児夜間加算（小児科初診）（外来腫瘍化学療法診療料）
           03  FILLER              PIC X(14)   VALUE   "5518 113039170".
      *    乳幼児休日加算（小児科初診）
           03  FILLER              PIC X(14)   VALUE   "5618 113039270".
      *    乳幼児深夜加算（小児科初診）
           03  FILLER              PIC X(14)   VALUE   "5718 113039370".
      *
      *    乳幼児時間外加算（再診）（外来腫瘍化学療法診療料）
           03  FILLER              PIC X(14)   VALUE   "5119 113039870".
      *    乳幼児休日加算（再診）（外来腫瘍化学療法診療料）
           03  FILLER              PIC X(14)   VALUE   "5219 113043870".
      *    乳幼児深夜加算（再診）（外来腫瘍化学療法診療料）
           03  FILLER              PIC X(14)   VALUE   "5319 113043970".
      *    乳幼児時間外特例医療機関加算（再診）（外来腫瘍化学療法診療料）
           03  FILLER              PIC X(14)   VALUE   "5419 113040070".
      *    乳幼児夜間加算（小児科再診）（外来腫瘍化学療法診療料）
           03  FILLER              PIC X(14)   VALUE   "5519 113040170".
      *    乳幼児休日加算（小児科再診）
           03  FILLER              PIC X(14)   VALUE   "5619 113040270".
      *    乳幼児深夜加算（小児科再診）
           03  FILLER              PIC X(14)   VALUE   "5719 113040370".
      *
      *    乳幼児時間外加算（外来診療料）（外来腫瘍化学療法診療料）
           03  FILLER              PIC X(14)   VALUE   "5120 113040870".
      *    乳幼児休日加算（外来診療料）（外来腫瘍化学療法診療料）
           03  FILLER              PIC X(14)   VALUE   "5220 113044070".
      *    乳幼児深夜加算（外来診療料）（外来腫瘍化学療法診療料）
           03  FILLER              PIC X(14)   VALUE   "5320 113044170".
      *    乳幼児時間外特例医療機関加算（外来診療料・外来腫瘍化学療法診療料）
           03  FILLER              PIC X(14)   VALUE   "5420 113041070".
      *    乳幼児夜間加算（小児科・外来診療料）（外来腫瘍化学療法診療料）
           03  FILLER              PIC X(14)   VALUE   "5520 113041170".
      *    乳幼児休日加算（小児科・外来診療料）
           03  FILLER              PIC X(14)   VALUE   "5620 113041270".
      *    乳幼児深夜加算（小児科・外来診療料）
           03  FILLER              PIC X(14)   VALUE   "5720 113041370".
      *
       01  TBL-TIMEKSN22-AREA-R      REDEFINES   TBL-TIMEKSN22-AREA.
           03  TBL-TIMEKSN22-OCC     OCCURS   146
                                     INDEXED  TBL-TIDX22.
      *        1:初診、2：再診、3:地域包括診療料、4：認知症地域
      *        5:外来腫瘍化学療法診療料
               05  TBL-TM22-KBN1         PIC X(01).
      *        1:時間外、2：休日、3:深夜、4:特例時間外、5:夜間、
      *        6:小児科特例、7:小児科特例、8:夜間・早朝
               05  TBL-TM22-TIMKBN       PIC X(01).
      *        01:乳幼児（初診）、02:乳幼児（再診）、03:乳幼児（外来診療料）
      *        04:小児科（初診）、05:小児科（再診）、06:小児科（外来診療料）
      *        07:乳幼児（地域包括）、08:乳幼児（認知症）
      *        09:小児かかり（初診）、10:小児かかり（再診）
      *        11:小児かかり（外来診療料）
      *        12:小児かかり（７歳）（初診）
      *        13:小児かかり（７歳）（再診）
      *        14:小児かかり（７歳）（外来診療料）
      **       15:外来腫瘍化学療法診療料（初診）
      *        16:外来腫瘍化学療法診療料（再診）
      *        17:外来腫瘍化学療法診療料（外来診療料）
      *        18:外来腫瘍化学療法診療料（乳幼児時間外加算（初診）
      *        19:外来腫瘍化学療法診療料（乳幼児時間外加算（再診）
      *        20:外来腫瘍化学療法診療料（乳幼児時間外加算（外来診療料）
               05  TBL-TM22-KBN2         PIC X(02).
      *        G:外来診療料
               05  TBL-TM22-KBN3         PIC X(01).
      *
               05  TBL-TM22-SRYCD        PIC X(09).
      *
