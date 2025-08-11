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
       PROGRAM-ID.             ORCHCN68R06.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院会計照会
      *  コンポーネント名  : 医療区分・ＡＤＬ評価票（令和６年以降）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  24/05/26    ORCAMO        新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    印刷用データ
           SELECT  PRT-FILE        ASSIGN  ASGNPARA
                                   FILE    STATUS  IS  STS-PARA.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    印刷用データ
       FD  PRT-FILE.
       01  PRT-REC.
           03  PRT-PRTDATA         PIC X(20000).
      *
       WORKING-STORAGE             SECTION.
      *
      *    印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
           COPY    "CPASGNPARA.INC".
      *
      *    帳票領域
           COPY    "HCN68R06V01.INC"
                                   REPLACING //HCN68R06// BY //HCN68//.
           COPY    "HCN69R06.INC"  REPLACING //HCN69R06// BY //HCN69//.
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-RYOCOM          PIC 9(01).
      *
           03  FLG-MEDKBN3                 PIC 9(01).
           03  FLG-MEDKBN2                 PIC 9(01).
           03  FLG-MEDKBN3LMT              PIC 9(01).
           03  FLG-MEDKBN2LMT              PIC 9(01).
           03  FLG-ADLKBN                  PIC 9(01).
           03  FLG-MEDCOM                  PIC 9(01).
           03  FLG-MEDMORE                 PIC 9(01).
           03  FLG-GAITO                   PIC 9(01).
      *
      *    インデックス領域
       01  IDX-AREA.
           03  IDXL                PIC 9(05).
           03  IDXM                PIC 9(05).
           03  IDXX                PIC 9(05).
           03  IDYY                PIC 9(05).
           03  IDX-MED             PIC 9(05).
           03  IDX-ADL             PIC 9(05).
           03  IDX-RYO             PIC 9(05).
           03  IDX-DAY             PIC 9(05).
           03  IDX0                        PIC 9(05).
           03  IDX1                        PIC 9(05).
           03  IDX2                        PIC 9(05).
           03  IDX-LST                     PIC 9(05).
           03  IDX-MAX                     PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD            PIC X(08).
           03  WRK-EDTYMD1         PIC X(10).
           03  WRK-EDTYMD3         PIC X(22).
           03  WRK-PRT-SRYYM       PIC X(16).
           03  WRK-PRT-SAKYMD      PIC X(22).
           03  WRK-TEN-G.
               05  WRK-TEN      PIC 9(02).
           03  WRK-PRT-TEN      PIC X(02).
           03  WRK-KANACHK-STR     PIC X(500).
      *     
       01  WRK-RYOLIST-AREA.
      *     03  WRK-RYOLIST-ADL-STIDX   PIC 9(03).
      *     03  WRK-RYOLIST-DESIGNR-STIDX
      *                                 PIC 9(03).
           03  WRK-RYOLIST-OCC         OCCURS  100.
      *        05  WRK-TZAISKBKBN      PIC X(01).
               05  WRK-TMEDKBN         PIC 9(01).
               05  WRK-TMEDKBN1        PIC X(01).
               05  WRK-TDSPKBN         PIC X(01).
               05  WRK-TLIMIT          PIC 9(03).
               05  WRK-TZAINUM         PIC 9(04).
               05  WRK-TSRYCD          pic X(09).
               05  WRK-TNAME           PIC X(106).
      *
               05  WRK-MEDKBN-LINE     PIC 9(03).
           03  WRK-RYOLST-MAX          PIC 9(03).    
           03  WRK-DESIGNR-EVAL-DAY-G.
               05  WRK-DESIGNR-EVAL-DAY
                                       PIC 9(1)    OCCURS  31.
      * 01  WRK-MEDKBN-LINE-AREA.
      *    CPMEDKBN.INC MEDR06-MAXの値を設定     
      *     03  WRK-MEDKBN-LINE         PIC 9(03)   OCCURS  49.
      *
       01  CONST-AREA.
           03  CONST-HCN68R06-MAX     PIC 9(03)    VALUE   68.
      *
           03  CONST-LINE.
               05  FILLER             PIC X(56)   VALUE
           "ーーーーーーーーーーーーーーーーーーーーーーーーーーーー".
               05  FILLER             PIC X(24)   VALUE
           "ーーーーーーーーーーーー".
      *  
           03  CONST-GAITO-AREA.
               05  FILLER             PIC X(24)   VALUE
           "　　　　　　　　　　　　".
               05  FILLER             PIC X(56)    VALUE
           "　　疾患・状態に係る医療区分３（スモンを除く）の該当有無".
               05  FILLER             PIC X(24)   VALUE
           "　　　　　　　　　　　　".
               05  FILLER             PIC X(56)    VALUE
           "　　　　　疾患・状態に係る医療区分３（スモン）の該当有無".
               05  FILLER             PIC X(24)   VALUE
           "　　　　　　　　　　　　".
               05  FILLER             PIC X(56)    VALUE
           "　　　　　　　　　　　　処置等に係る医療区分３の該当有無".
               05  FILLER             PIC X(24)   VALUE
           "　　　　　　　　　　　　".
               05  FILLER             PIC X(56)    VALUE
           "　　　　　　　　　　疾患・状態に係る医療区分２の該当有無".
               05  FILLER             PIC X(24)   VALUE
           "　　　　　　　　　　　　".
               05  FILLER             PIC X(56)    VALUE
           "　　　　　　　　　　　　　処置に係る医療区分２の該当有無".
               05  FILLER             PIC X(24)   VALUE
           "　　　　　　　　　　　　".
               05  FILLER             PIC X(56)    VALUE
           "　　　　　　　上記いずれにも該当しない場合（医療区分１）".
      *         
               05  FILLER             PIC X(24)   VALUE
           "　　　　　　　　　　　　".
               05  FILLER             PIC X(56)    VALUE
           "　　　　　　　　　　　　　　　　　　医療区分３の該当有無".
               05  FILLER             PIC X(24)   VALUE
           "　　　　　　　　　　　　".
               05  FILLER             PIC X(56)    VALUE
           "　　　　　　　　　　　　　　　　　　医療区分２の該当有無".
               05  FILLER             PIC X(24)   VALUE
           "　　　　　　　　　　　　".
               05  FILLER             PIC X(56)    VALUE
           "　　　　　　　　　医療区分３・２いずれも０（医療区分１）".
               05  FILLER             PIC X(240)   VALUE   SPACE.
           03  CONST-GAIRO-AREA-R      REDEFINES   CONST-GAITO-AREA.
      *        1:療養病棟 2:有床診療所       
               05  CONST-GAITO-OCC     OCCURS  2.
                   07  CONST-GAITO     PIC X(80)   OCCURS  6.
      *
           COPY    "CPMEDKBN.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1001.INC".
           COPY    "CPSK1034.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    全角変換サブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
           COPY    "CPORCHCN68.INC".
           COPY    "CPORCHCN68S01.INC".
      *
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       COPY    "COMMON-SPA".
       COPY    "CPONPRTDATA.INC".
      *
      ******************************************************************
       PROCEDURE               DIVISION
                                   USING
                                   SPA-AREA
                                   ONPRTDATA
                                   .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    ONPRTDATA-DATA  TO  ORCHCN68AREA
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
                                       WRK-RYOLIST-AREA
      *
           PERFORM 800-MCNDATE-SEC
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                  SYSKANRI-REC
           INITIALIZE                  ORCSPRTNMAREA
           MOVE    "2"             TO  ORCSPRTNM-KBN
           MOVE    SPA-SYSYMD      TO  ORCSPRTNM-SRYYMD
           MOVE    "1"             TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                       ORCSPRTNMAREA
                                       SPA-AREA
                                       SYSKANRI-REC
           IF    ( ORCSPRTNM-RC    =   ZERO )
               MOVE    SYSKANRI-REC    TO  SYS-1034-REC
           ELSE
               INITIALIZE                  SYS-1034-REC
           END-IF
      *
           MOVE    ORCHCN68-SRYYM  TO  WRK-SYMD
           MOVE    "01"            TO  WRK-SYMD(7:)
           PERFORM 800-HIZUKE-EDIT-SEC
           MOVE    WRK-EDTYMD3     TO  WRK-PRT-SRYYM
      *
           MOVE    SPA-SYSYMD      TO  WRK-SYMD
           PERFORM 800-HIZUKE-EDIT-SEC
           MOVE    WRK-EDTYMD3     TO  WRK-PRT-SAKYMD
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "1001"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    ORCHCN68-SRYYM  TO  SYS-STYUKYMD
           MOVE    "01"            TO  SYS-STYUKYMD (7:2)
           MOVE    SYS-STYUKYMD    TO  SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1001-REC
      *
           INITIALIZE                  HCN68S01-AREA
           MOVE    "1"             TO  HCN68S01-RANGE
           MOVE    ORCHCN68-PTID   TO  HCN68S01-PTID
           MOVE    ORCHCN68-SRYYM  TO  HCN68S01-SRYYM
                                       HCN68S01-SRYYMD
           MOVE    "01"            TO  HCN68S01-SRYYMD (7:)
           CALL    "ORCHCN68S01"   USING
                                   SPA-AREA
                                   HCN68S01-AREA
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    ZERO            TO  IDXM
           IF      SPA-HOSPSBT     =   1
               MOVE    1               TO  IDXX
           ELSE
               MOVE    2               TO  IDXX
           END-IF              
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   MEDR06-MAX )
                   OR    ( MEDR06-STYMD (IDXX IDX0)
                                   =   SPACE )
                IF    ( ORCHCN68-SRYYM  >=
                                      MEDR06-STYMD (IDXX IDX0) (1:6) )
                AND   ( ORCHCN68-SRYYM  <=
                                      MEDR06-EDYMD (IDXX IDX0) (1:6) )
                   COMPUTE IDXM  = IDXM    +   1
                   MOVE  IDXM          TO  WRK-RYOLST-MAX
                   MOVE  MEDR06-MEDKBN (IDXX IDX0)
                                       TO  WRK-TMEDKBN (IDXM)
                   MOVE  MEDR06-MEDKBN1(IDXX IDX0)
                                       TO  WRK-TMEDKBN1(IDXM)
                   MOVE  MEDR06-DSPKBN (IDXX IDX0)
                                       TO  WRK-TDSPKBN (IDXM)
                   MOVE  MEDR06-LIMIT (IDXX IDX0)
                                       TO  WRK-TLIMIT  (IDXM)
                   MOVE  MEDR06-ZAINUM (IDXX IDX0)
                                       TO  WRK-TZAINUM (IDXM)
                   MOVE  MEDR06-CDL    (IDXX IDX0)
                                       TO  WRK-TSRYCD  (IDXM)
      **             STRING  "　"            DELIMITED  BY  SIZE
      **                     MEDR06-MEIL (IDXX IDX0)
      **                                     DELIMITED  BY  SIZE
      **                                 INTO  WRK-TNAME   (IDXM)
      **             END-STRING                    
                   MOVE         MEDR06-MEIL (IDXX IDX0)
                                       TO  WRK-TNAME  (IDXM)
                END-IF
            END-PERFORM
      *
            IF    ( ORCHCN68-PRTKBN =   "0" )
            OR   ( HCN68S01-RYOYO-FLG  =   ZERO )
               PERFORM 2000-EDIT-SEC
           ELSE
               PERFORM 2001-EDIT-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票出力処理
      *****************************************************************
       2000-EDIT-SEC               SECTION.
      *
           INITIALIZE              HCN68
           PERFORM 20001-HCN68-HEADER-SEC
           PERFORM 230-PRINT-SEC
      *
           INITIALIZE              HCN69
           PERFORM 20001-HCN69-HEADER-SEC
           PERFORM 231-PRINT-SEC
      *
           .
       2000-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    医療区分評価票見出し部編集処理
      *****************************************************************
       20001-HCN68-HEADER-SEC          SECTION.
      *
           MOVE    WRK-PRT-SRYYM   TO  HCN68-SRYYM
           MOVE    ORCHCN68-PTNUM  TO  HCN68-PTNUM
           MOVE    PTINF-NAME      TO  HCN68-NAME
      *
           EVALUATE    PTINF-SEX
           WHEN    "1"
               MOVE    "男"        TO  HCN68-SEX
           WHEN    "2"
               MOVE    "女"        TO  HCN68-SEX
           END-EVALUATE
      *
           MOVE    PTINF-BIRTHDAY  TO  WRK-SYMD
           PERFORM 800-HIZUKE-EDIT-SEC
           MOVE    WRK-EDTYMD3     TO  HCN68-BIRTHDAY
      *
           MOVE    "発行日"        TO  HCN68-LBLSAKYMD
           MOVE    WRK-PRT-SAKYMD  TO  HCN68-SAKYMD
      *
      *
           MOVE    ZERO                TO  FLG-MEDKBN3
                                           FLG-MEDKBN3LMT
                                           FLG-MEDKBN2
                                           FLG-MEDKBN2LMT
                                           FLG-ADLKBN
                                           FLG-MEDCOM
                                           FLG-MEDMORE
                                           FLG-GAITO
      *     INITIALIZE                  WRK-MEDKBN-LINE-AREA
      *
           IF    ( SYS-1001-HOSPSBT    =   "1" )
               MOVE    ZERO            TO  IDX-LST
               MOVE    CONST-HCN68R06-MAX
                                       TO  IDX-MAX
           ELSE
               MOVE    68              TO  IDX-LST
               COMPUTE IDX-MAX =   CONST-HCN68R06-MAX  +
                                   CONST-HCN68R06-MAX
           END-IF
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL   IDX0    >       49
                   OR      IDX-LST >=      IDX-MAX
      *
               COMPUTE IDX-LST =   IDX-LST +   1
      *
               EVALUATE    TRUE
                   WHEN  ( WRK-TMEDKBN (IDX0)  =   3 )
                    AND  ( WRK-TLIMIT  (IDX0)  >   0 )
                    AND  ( FLG-MEDKBN3LMT          =   0 )
                       IF    ( FLG-MEDKBN3LMT      =   0 )
                        AND  ( FLG-MEDKBN2LMT      =   0 )
                           MOVE    "Ｉ　算定期間に限りがある区分"
                                   TO  HCN68-MEDKBN-NAME(IDX-LST)
                           COMPUTE IDX-LST =   IDX-LST +   1
                       END-IF
                       MOVE    1       TO  FLG-MEDKBN3LMT
                       IF      WRK-TDSPKBN (IDX0)  =   "1" OR  "9"
                           EVALUATE    WRK-TMEDKBN1 (IDX0)
                               WHEN    "0"
                                   MOVE    "　　【医療区分３】"
                                       TO  HCN68-MEDKBN-NAME(IDX-LST)
                               WHEN    "1"
                                   MOVE
                                   "　　【疾患・状態に係る医療区分３】"
                                       TO  HCN68-MEDKBN-NAME(IDX-LST)
                               WHEN    "2"
                                   MOVE
                                   "　　【処置等に係る医療区分３】"
                                       TO  HCN68-MEDKBN-NAME(IDX-LST)
                           END-EVALUATE                    
                           COMPUTE IDX-LST =   IDX-LST +   1
                       END-IF    
                   WHEN  ( WRK-TMEDKBN (IDX0)  =   2 )
                    AND  ( WRK-TLIMIT  (IDX0)  >   0 )
                    AND  ( FLG-MEDKBN2LMT          =   0 )
                       IF    ( FLG-MEDKBN3LMT      =   0 )
                        AND  ( FLG-MEDKBN2LMT      =   0 )
                           MOVE    "Ｉ　算定期間に限りがある区分"
                                   TO  HCN68-MEDKBN-NAME(IDX-LST)
                           COMPUTE IDX-LST =   IDX-LST +   1
                       END-IF
                       MOVE    1       TO  FLG-MEDKBN2LMT
                       IF      WRK-TDSPKBN (IDX0)  =   "1" OR  "9"
                           EVALUATE    WRK-TMEDKBN1 (IDX0)
                               WHEN    "0"
                                   MOVE    "　　【医療区分２】"
                                       TO  HCN68-MEDKBN-NAME(IDX-LST)
                               WHEN    "1"
                                   MOVE
                                   "　　【疾患・状態に係る医療区分２】"
                                       TO  HCN68-MEDKBN-NAME(IDX-LST)
                               WHEN    "2"
                                   MOVE
                                   "　　【処置等に係る医療区分２】"
                                       TO  HCN68-MEDKBN-NAME(IDX-LST)
                           END-EVALUATE                    
                           COMPUTE IDX-LST =   IDX-LST +   1
                       END-IF    
                   WHEN  ( WRK-TMEDKBN (IDX0)  =   3 )
                    AND  ( WRK-TLIMIT  (IDX0)  =   0 )
                    AND  ( FLG-MEDKBN3             =   0 )
                       IF    ( FLG-MEDKBN3         =   0 )
                        AND  ( FLG-MEDKBN2         =   0 )
                           MOVE    "ＩＩ算定期間に限りがない区分"
                                   TO  HCN68-MEDKBN-NAME(IDX-LST)
                           COMPUTE IDX-LST =   IDX-LST +   1
                       END-IF
                       MOVE    1           TO  FLG-MEDKBN3
                       IF      WRK-TDSPKBN (IDX0)  =   "1" OR  "9"
                           EVALUATE    WRK-TMEDKBN1 (IDX0)
                               WHEN    "0"
                                   MOVE    "　　【医療区分３】"
                                       TO  HCN68-MEDKBN-NAME(IDX-LST)
                               WHEN    "1"
                                   MOVE
                                   "　　【疾患・状態に係る医療区分３】"
                                       TO  HCN68-MEDKBN-NAME(IDX-LST)
                               WHEN    "2"
                                   MOVE
                                   "　　【処置等に係る医療区分３】"
                                       TO  HCN68-MEDKBN-NAME(IDX-LST)
                           END-EVALUATE                    
                           COMPUTE IDX-LST =   IDX-LST +   1
                       END-IF    
                   WHEN  ( WRK-TMEDKBN (IDX0)  =   2 )
                    AND  ( WRK-TLIMIT  (IDX0)  =   0 )
                    AND  ( FLG-MEDKBN2             =   0 )
                       IF    ( FLG-MEDKBN3         =   0 )
                        AND  ( FLG-MEDKBN2         =   0 )
                           MOVE    "ＩＩ算定期間に限りがない区分"
                                   TO  HCN68-MEDKBN-NAME(IDX-LST)
                           COMPUTE IDX-LST =   IDX-LST +   1
                       END-IF
                       MOVE    1       TO  FLG-MEDKBN2
                       IF      WRK-TDSPKBN (IDX0)  =   "1" OR  "9"
                           EVALUATE    WRK-TMEDKBN1 (IDX0)
                               WHEN    "0"
                                   MOVE    "　　【医療区分２】"
                                       TO  HCN68-MEDKBN-NAME(IDX-LST)
                               WHEN    "1"
                                   MOVE
                                   "　　【疾患・状態に係る医療区分２】"
                                       TO  HCN68-MEDKBN-NAME(IDX-LST)
                               WHEN    "2"
                                   MOVE
                                   "　　【処置等に係る医療区分２】"
                                       TO  HCN68-MEDKBN-NAME(IDX-LST)
                           END-EVALUATE                    
                           COMPUTE IDX-LST =   IDX-LST +   1
                       END-IF    
                   WHEN  ( WRK-TMEDKBN (IDX0)  =   3   )
                    AND  ( WRK-TDSPKBN (IDX0)  =   "1" OR  "9" )
                       EVALUATE    WRK-TMEDKBN1 (IDX0)
                           WHEN    "1"
                               MOVE
                               "　　【疾患・状態に係る医療区分３】"
                                       TO  HCN68-MEDKBN-NAME(IDX-LST)
                           WHEN    "2"
                               MOVE
                               "　　【処置等に係る医療区分３】"
                                       TO  HCN68-MEDKBN-NAME(IDX-LST)
                        END-EVALUATE                    
                       COMPUTE IDX-LST =   IDX-LST +   1
                   WHEN  ( WRK-TMEDKBN (IDX0)  =   2 )
                    AND  ( WRK-TDSPKBN (IDX0)  =   "1" OR  "9" )
                        EVALUATE    WRK-TMEDKBN1 (IDX0)
                           WHEN    "1"
                               MOVE
                               "　　【疾患・状態に係る医療区分２】"
                                       TO  HCN68-MEDKBN-NAME(IDX-LST)
                           WHEN    "2"
                               MOVE
                               "　　【処置等に係る医療区分２】"
                                       TO  HCN68-MEDKBN-NAME(IDX-LST)
                       END-EVALUATE                    
                       COMPUTE IDX-LST =   IDX-LST +   1
                   WHEN  ( WRK-TMEDKBN (IDX0)  =   0 )
                    AND  ( FLG-GAITO           =   0 )
                       IF    ( SYS-1001-HOSPSBT    =   "1" )
                           MOVE    1               TO  IDYY
                       ELSE    
                           MOVE    2               TO  IDYY
                       END-IF    
                       PERFORM     VARYING  IDXX    FROM    1   BY  1
                                   UNTIL    IDXX    >       6
                                   OR      CONST-GAITO (IDYY IDXX)
                                                    =   SPACE
                           MOVE    CONST-GAITO (IDYY IDXX)
                                       TO  HCN68-MEDKBN-NAME(IDX-LST)
                           COMPUTE IDX-LST =   IDX-LST +   1
                       END-PERFORM                      
                       MOVE    1       TO  FLG-GAITO
                       MOVE    CONST-LINE
                                       TO  HCN68-MEDKBN-NAME(IDX-LST)
                       COMPUTE IDX-LST =   IDX-LST +   1
                   WHEN    OTHER
                       CONTINUE
                   END-EVALUATE
                   MOVE    WRK-TNAME (IDX0)
                                   TO  HCN68-MEDKBN-NAME(IDX-LST)
                   MOVE    IDX-LST TO  WRK-MEDKBN-LINE (IDX0)
      *
                   IF    ( SYS-1001-HOSPSBT    =   "1" )
                       EVALUATE    IDX0
                           WHEN    10
                           WHEN    41             
                           WHEN    48             
                               COMPUTE IDX-LST =   IDX-LST +   1
                               MOVE    CONST-LINE
                                       TO  HCN68-MEDKBN-NAME(IDX-LST)
                       END-EVALUATE
                   ELSE                        
                       EVALUATE    IDX0
                           WHEN    9
                           WHEN    39           
                           WHEN    46             
                               COMPUTE IDX-LST =   IDX-LST +   1
                               MOVE    CONST-LINE
                                       TO  HCN68-MEDKBN-NAME(IDX-LST)
                       END-EVALUATE
                   END-IF
           END-PERFORM
      *
           IF    ( SYS-1001-HOSPSBT    =   "1" )
               STRING  "MonpeLayerIn"      DELIMITED  BY  SIZE
                       " -L CLINIC "       DELIMITED  BY  SIZE
                       "MonpeLayerOut"     DELIMITED  BY  SIZE
               INTO    HCN68-LOPTION
               END-STRING
           ELSE
               STRING  "MonpeLayerIn"      DELIMITED  BY  SIZE
                       " -L HOSP "         DELIMITED  BY  SIZE
                       "MonpeLayerOut"     DELIMITED  BY  SIZE
               INTO    HCN68-LOPTION
               END-STRING
           END-IF
            .
       20001-HCN68-HEADER-EXT.
           EXIT.
      *****************************************************************
      *    ＡＤＬ区分評価票見出し部編集処理
      *****************************************************************
       20001-HCN69-HEADER-SEC          SECTION.
      *
           MOVE    WRK-PRT-SRYYM   TO  HCN69-SRYYM
           MOVE    ORCHCN68-PTNUM  TO  HCN69-PTNUM
           MOVE    PTINF-NAME      TO  HCN69-NAME
      *
           EVALUATE    PTINF-SEX
           WHEN    "1"
               MOVE    "男"        TO  HCN69-SEX
           WHEN    "2"
               MOVE    "女"        TO  HCN69-SEX
           END-EVALUATE
      *
           MOVE    PTINF-BIRTHDAY  TO  WRK-SYMD
           PERFORM 800-HIZUKE-EDIT-SEC
           MOVE    WRK-EDTYMD3     TO  HCN69-BIRTHDAY
      *
           MOVE    "発行日"        TO  HCN69-LBLSAKYMD
           MOVE    WRK-PRT-SAKYMD  TO  HCN69-SAKYMD
      *
           IF    ( SYS-1001-HOSPSBT    =   "1" )
               STRING  "MonpeLayerIn"      DELIMITED  BY  SIZE
                       " -L CLINIC "       DELIMITED  BY  SIZE
                       "MonpeLayerOut"     DELIMITED  BY  SIZE
               INTO    HCN69-LOPTION
               END-STRING
           ELSE
               STRING  "MonpeLayerIn"      DELIMITED  BY  SIZE
                       " -L HOSP "         DELIMITED  BY  SIZE
                       "MonpeLayerOut"     DELIMITED  BY  SIZE
               INTO    HCN69-LOPTION
               END-STRING
           END-IF
      *
      *
           .
       20001-HCN69-HEADER-EXT.
           EXIT.
      *****************************************************************
      *    帳票出力処理
      *****************************************************************
       2001-EDIT-SEC               SECTION.
      *
           INITIALIZE              HCN68
           PERFORM 20001-HCN68-HEADER-SEC
           PERFORM 20011-EDIT-HCN68-SEC
           PERFORM 230-PRINT-SEC
      *
           INITIALIZE              HCN69
           PERFORM 20001-HCN69-HEADER-SEC
           PERFORM 20011-EDIT-HCN69-SEC
           PERFORM 231-PRINT-SEC
      *
           .
       2001-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    医療区分評価票編集処理
      *****************************************************************
       20011-EDIT-HCN68-SEC            SECTION.
      *
           PERFORM VARYING IDXL    FROM    1   BY  1
                   UNTIL ( IDXL    >   HCN68S01-RYOLIST-MAX )
      *
               MOVE    ZERO        TO  IDX-MED
      *
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL ( IDXX    >   MEDR06-MAX )
                   IF      HCN68S01-TZAINUM (IDXL)
                                           =   WRK-TZAINUM (IDXX)
                      MOVE    WRK-MEDKBN-LINE (IDXX)
                                               TO  IDX-MED
                       MOVE    MEDR06-MAX      TO  IDXX
                   END-IF
               END-PERFORM
      *
               IF    ( IDX-MED         >   ZERO )
                   PERFORM 200111-EDIT-HCN68-SEC
               END-IF
      *
           END-PERFORM
      *
           IF    ( SYS-1001-HOSPSBT    =   "1" )
               IF    ( HCN68S01-MEDKBN-DAYALL (5)   =    1 )
                   MOVE    "○"        TO  HCN68-MEDKBN-ALL (53)
               END-IF
               IF    ( HCN68S01-MEDKBN-SUMONFLG     =    1 )
                   MOVE    "○"        TO  HCN68-MEDKBN-ALL (54)
               END-IF
               IF    ( HCN68S01-MEDKBN-DAYALL (3)   =    1 )
                   MOVE    "○"        TO  HCN68-MEDKBN-ALL (55)
               END-IF
               IF    ( HCN68S01-MEDKBN-DAYALL (2)   =    1 )
                   MOVE    "○"        TO  HCN68-MEDKBN-ALL (56)
               END-IF
               IF    ( HCN68S01-MEDKBN-DAYALL (4)   =    1 )
                   MOVE    "○"        TO  HCN68-MEDKBN-ALL (57)
               END-IF
      *
               PERFORM VARYING IDX-DAY FROM    1   BY  1
                       UNTIL ( IDX-DAY >   31 )
      *
                   IF    ( HCN68S01-MEDKBN-DAY (5 IDX-DAY) =  1 )
                       MOVE    "○"    TO  HCN68-MEDKBN-DAY (53 IDX-DAY)
                   END-IF
      *
                   IF    ( HCN68S01-MEDKBN-DAY (3 IDX-DAY) =  1 )
                       MOVE    "○"    TO  HCN68-MEDKBN-DAY (55 IDX-DAY)
                   END-IF
      *
                   IF    ( HCN68S01-MEDKBN-DAY (2 IDX-DAY) =  1 )
                       MOVE    "○"    TO  HCN68-MEDKBN-DAY (56 IDX-DAY)
                   END-IF
      *
                   IF    ( HCN68S01-MEDKBN-DAY (4 IDX-DAY) =  1 )
                       MOVE    "○"    TO  HCN68-MEDKBN-DAY (57 IDX-DAY)
                   END-IF
      *
      *            医療区分１
                   IF    ( HCN68S01-RYOYO-CD (IDX-DAY)
                                       NOT =   SPACE )
      *
                       IF    ( HCN68S01-MEDKBN-DAYALL (1)      = ZERO )
                        AND  ( HCN68S01-MEDKBN-DAY (1 IDX-DAY) = ZERO )
                        AND  ( HCN68S01-MEDKBN-DAYALL (2)      = ZERO )
                        AND  ( HCN68S01-MEDKBN-DAY (2 IDX-DAY) = ZERO )
                        AND  ( HCN68S01-MEDKBN-DAYALL (3)      = ZERO )
                        AND  ( HCN68S01-MEDKBN-DAY (3 IDX-DAY) = ZERO )
                        AND  ( HCN68S01-MEDKBN-DAYALL (4)      = ZERO )
                        AND  ( HCN68S01-MEDKBN-DAY (4 IDX-DAY) = ZERO )
                           MOVE    "○"
                                       TO  HCN68-MEDKBN-DAY (58 IDX-DAY)
                       END-IF
                   END-IF
               END-PERFORM
           ELSE
               IF    ( HCN68S01-MEDKBN-DAYALL (1)   =    1 )
                   MOVE    "○"        TO  HCN68-MEDKBN-ALL (116)
               END-IF
               IF    ( HCN68S01-MEDKBN-DAYALL (2)   =    1    )
                   MOVE    "○"        TO  HCN68-MEDKBN-ALL (117)
               END-IF
      *
               PERFORM VARYING IDX-DAY FROM    1   BY  1
                       UNTIL ( IDX-DAY >   31 )
      *
                   IF    ( HCN68S01-MEDKBN-DAY (1 IDX-DAY) =  1 )
                       MOVE    "○"
                                   TO  HCN68-MEDKBN-DAY (116 IDX-DAY)
                   END-IF
      *
                   IF    ( HCN68S01-MEDKBN-DAY (2 IDX-DAY)   =  1 )
                       MOVE    "○"
                                   TO  HCN68-MEDKBN-DAY (117 IDX-DAY)
                   END-IF
      *
                   IF    ( HCN68S01-RYOYO-CD (IDX-DAY)
                                       NOT =   SPACE )
      *
                       IF    ( HCN68S01-MEDKBN-DAYALL (1)      = ZERO )
                        AND  ( HCN68S01-MEDKBN-DAY (1 IDX-DAY) = ZERO )
                        AND  ( HCN68S01-MEDKBN-DAYALL (2)      = ZERO )
                        AND  ( HCN68S01-MEDKBN-DAY (2 IDX-DAY) = ZERO )
                           MOVE    "○"
                                   TO  HCN68-MEDKBN-DAY (118 IDX-DAY)
                       END-IF
                   END-IF
               END-PERFORM    
           END-IF    
      *
           .
       20011-EDIT-HCN68-EXT.
           EXIT.
      *****************************************************************
      *    医療区分評価票編集処理
      *****************************************************************
       200111-EDIT-HCN68-SEC           SECTION.
      *
         IF ( HCN68S01-TALLKBN (IDXL)  =   1 )
      *
             IF  ( HCN68S01-TDAYALL (IDXL)
                                       >   ZERO )
                   MOVE    "○"    TO  HCN68-MEDKBN-ALL(IDX-MED)
             END-IF
         ELSE
      *D      DISPLAY IDX-MED " - " IDX-DAY
             PERFORM VARYING IDX-DAY FROM    1   BY  1
                     UNTIL ( IDX-DAY >   31 )
                 IF  ( HCN68S01-TDAY (IDXL IDX-DAY)
                                           >   ZERO )
                       MOVE    "○"
                               TO  HCN68-MEDKBN-DAY (IDX-MED IDX-DAY)
                 END-IF
             END-PERFORM
         END-IF
      *
           .
       200111-EDIT-HCN68-EXT.
           EXIT.
      *****************************************************************
      *    医療区分評価票印刷処理
      *****************************************************************
       230-PRINT-SEC               SECTION.
      *
           COMPUTE ORCHCN68-CNT    =   ORCHCN68-CNT    +   1
      *
           IF    ( ORCHCN68-BATCH  =   "1" )
               PERFORM 2301-BATCH-PRINT-SEC
           ELSE
               PERFORM 2301-PRINT-SEC
           END-IF
      *
           .
       230-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    医療区分評価票印刷処理
      *****************************************************************
       2301-PRINT-SEC              SECTION.
      *
      *    印刷用ファイル作成
           MOVE    SPA-HOSPNUM     TO  HC01PARA-HOSPNUM
           MOVE    "HC68"          TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID      TO  HC01PARA-TERMID
           MOVE    SMCNDATE-HMS    TO  HC01PARA-SYOHMS
           MOVE    ORCHCN68-CNT    TO  HC01PARA-CNT
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    HC01PARA        TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME
                                   TO  ASGNPARA
      *
           OPEN    OUTPUT              PRT-FILE
           MOVE    SPACE           TO  PRT-REC
      *
      *    印刷用データ出力
           MOVE    HCN68           TO  PRT-PRTDATA
           WRITE                       PRT-REC
      *
           CLOSE                       PRT-FILE
      *
      *    帳票印刷処理
           INITIALIZE                  ORCSMKPRTAREA
           MOVE    "HC01.sh"       TO  MKPRT-ID
           MOVE    "HCN68R06V01.red"
                                   TO  MKPRT-DIA
           MOVE    SPACE           TO  MKPRT-DEF
           MOVE    ASGNPARA        TO  MKPRT-DAT
           MOVE    SYS-1034-PRTNM(14)
                                   TO  MKPRT-PRTNM
           MOVE   "医療区分・ＡＤＬ評価票"
                                   TO  MKPRT-INFO
           CALL   "ORCSMKPRT1"         USING
                                       ORCSMKPRTAREA
                                       SPA-AREA
      *
           .
       2301-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    医療区分評価票印刷処理（バッチ処理）
      *****************************************************************
       2301-BATCH-PRINT-SEC        SECTION.
      *
           MOVE    ORCHCN68-PRTAREA    TO  ORCSPRTAREA
      *
           MOVE    ORCHCN68-PTID       TO  SPRT-PTID
           MOVE    "HCN68R06V01.red"   TO  SPRT-PRTID
           MOVE    HCN68               TO  SPRT-PRTDATA
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
      *
           .
       2301-BATCH-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    医療区分評価票編集処理
      *****************************************************************
       20011-EDIT-HCN69-SEC            SECTION.
      *
           PERFORM VARYING IDXL    FROM    1   BY  1
                   UNTIL ( IDXL    >   HCN68S01-RYOLIST-MAX )
      *
               MOVE    ZERO        TO  IDX-ADL
      *
               EVALUATE    HCN68S01-TZAINUM (IDXL)
               WHEN    1001
                   MOVE    01      TO  IDX-ADL
               WHEN    1002
                   MOVE    02      TO  IDX-ADL
               WHEN    1003
                   MOVE    03      TO  IDX-ADL
               WHEN    1004
                   MOVE    04      TO  IDX-ADL
               END-EVALUATE
      *
               IF    ( IDX-ADL         >   ZERO )
                   PERFORM 200111-EDIT-HCN69-SEC
               END-IF
      *
               IF    ( HCN68S01-TZAINUM (IDXL)
                                   =   DESIGNR-ZAINUM )
                   PERFORM VARYING IDX-DAY FROM    1   BY  1
                           UNTIL ( IDX-DAY >   31 )
                       IF    ( HCN68S01-RYOYO-CD (IDX-DAY)
                                       NOT =   SPACE )
                        AND  ( HCN68S01-DESIGNR-EVAL-DAY (IDX-DAY)
                                           >   ZERO )
                           MOVE    HCN68S01-TDAY (IDXL IDX-DAY)
                                           TO  WRK-TEN
                           PERFORM 2001111-EDIT-TEN-SEC
                           MOVE    WRK-PRT-TEN
                                   TO  HCN69-DESIGNR-DAY (IDX-DAY)
                       END-IF
                   END-PERFORM
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
               IF    ( HCN68S01-RYOYO-CD (IDX-DAY)
                                   NOT =   SPACE )
                   MOVE    HCN68S01-ADLTEN-DAY (IDX-DAY)
                                   TO  WRK-TEN
                   PERFORM 2001111-EDIT-TEN-SEC
                   MOVE    WRK-PRT-TEN
                                   TO  HCN69-TTL-ADLTEN-DAY (IDX-DAY)
               END-IF
           END-PERFORM
      *
           IF    ( SYS-1001-HOSPSBT    =   1 )
               PERFORM VARYING IDX-DAY FROM    1   BY  1
                       UNTIL ( IDX-DAY >   31 )
      *            IF    ( HCN68S01-RYOYO-KHN (IDX-DAY)
      *                                = 5 OR 6 OR 9 OR 10 )
      *                CONTINUE
      *            ELSE
                       MOVE    HCN68S01-PTSTS-DAY3(IDX-DAY)
                                           TO  IDX-RYO
                       IF    ( IDX-RYO  >=  1  )
                        AND  ( IDX-RYO  <=  30 )
                           MOVE    "○"
                               TO  HCN69-HPTSTS-DAY (IDX-RYO IDX-DAY)
                       END-IF
      *            END-IF
               END-PERFORM
           ELSE
               PERFORM VARYING IDX-DAY FROM    1   BY  1
                       UNTIL ( IDX-DAY >   31 )
      *            IF    ( HCN68S01-RYOYO-KHN (IDX-DAY)
      *                                = 5 OR 6 OR 9 OR 10 )
      *                CONTINUE
      *            ELSE
                       MOVE    HCN68S01-PTSTS-DAY3(IDX-DAY)
                                           TO  IDX-RYO
                       IF    ( IDX-RYO  >=  1 )
                        AND  ( IDX-RYO  <=  5 )
                           MOVE    "○"
                               TO  HCN69-CPTSTS-DAY (IDX-RYO IDX-DAY)
                       END-IF
      *            END-IF
               END-PERFORM
           END-IF
      *
           .
       20011-EDIT-HCN69-EXT.
           EXIT.
      *****************************************************************
      *    医療区分評価票編集処理
      *****************************************************************
       200111-EDIT-HCN69-SEC           SECTION.
      *
         PERFORM VARYING IDX-DAY FROM    1   BY  1
                 UNTIL ( IDX-DAY >   31 )
               IF    ( HCN68S01-RYOYO-CD (IDX-DAY)
                                   NOT =   SPACE )
                   MOVE    HCN68S01-TDAY (IDXL IDX-DAY)
                           TO  WRK-TEN
                   PERFORM 2001111-EDIT-TEN-SEC
                   MOVE    WRK-PRT-TEN
                           TO  HCN69-ADLTEN-DAY (IDX-ADL IDX-DAY)
               END-IF
         END-PERFORM
      *
           .
       200111-EDIT-HCN69-EXT.
           EXIT.
      *****************************************************************
      *    点数編集処理
      *****************************************************************
       2001111-EDIT-TEN-SEC         SECTION.
      *
           IF    ( WRK-TEN-G (1:1)  =   "0" )
               MOVE  WRK-TEN-G (2:1)
                                   TO  WRK-PRT-TEN
           ELSE
               MOVE  WRK-TEN-G     TO  WRK-PRT-TEN
           END-IF
      *
           .
       2001111-EDIT-TEN-EXT.
           EXIT.
      *****************************************************************
      *    ＡＤＬ区分評価票印刷処理
      *****************************************************************
       231-PRINT-SEC               SECTION.
      *
           COMPUTE ORCHCN68-CNT    =   ORCHCN68-CNT    +   1
      *
           IF    ( ORCHCN68-BATCH  =   "1" )
               PERFORM 2311-BATCH-PRINT-SEC
           ELSE
               PERFORM 2311-PRINT-SEC
           END-IF
      *
      *
           .
       231-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    ＡＤＬ区分評価票印刷処理
      *****************************************************************
       2311-PRINT-SEC               SECTION.
      *
      *    印刷用ファイル作成
           MOVE    SPA-HOSPNUM     TO  HC01PARA-HOSPNUM
           MOVE    "HC68"          TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID      TO  HC01PARA-TERMID
           MOVE    SMCNDATE-HMS    TO  HC01PARA-SYOHMS
           MOVE    ORCHCN68-CNT    TO  HC01PARA-CNT
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    HC01PARA        TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME
                                   TO  ASGNPARA
      *
           OPEN    OUTPUT              PRT-FILE
           MOVE    SPACE           TO  PRT-REC
      *
      *    印刷用データ出力
           MOVE    HCN69           TO  PRT-PRTDATA
           WRITE                       PRT-REC
      *
           CLOSE                       PRT-FILE
      *
      *    帳票印刷処理
           INITIALIZE                  ORCSMKPRTAREA
           MOVE    "HC01.sh"       TO  MKPRT-ID
           MOVE    "HCN69R06.red"  TO  MKPRT-DIA
           MOVE    SPACE           TO  MKPRT-DEF
           MOVE    ASGNPARA        TO  MKPRT-DAT
           MOVE    SYS-1034-PRTNM(14)
                                   TO  MKPRT-PRTNM
           MOVE   "医療区分・ＡＤＬ評価票"
                                   TO  MKPRT-INFO
           CALL   "ORCSMKPRT1"         USING
                                       ORCSMKPRTAREA
                                       SPA-AREA
      *
           .
       2311-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    ＡＤＬ区分評価票印刷処理（バッチ処理）
      *****************************************************************
       2311-BATCH-PRINT-SEC        SECTION.
      *
           MOVE    ORCHCN68-PRTAREA    TO  ORCSPRTAREA
      *
           MOVE    ORCHCN68-PTID       TO  SPRT-PTID
           MOVE    "HCN69R06.red"      TO  SPRT-PRTID
           MOVE    HCN69               TO  SPRT-PRTDATA
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
      *
           .
       2311-BATCH-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           MOVE    ORCHCN68AREA    TO  ONPRTDATA-DATA
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       800-ZENKAKU-SEC                 SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE   "2"              TO  KANACHK-SYORI
           MOVE  500               TO  KANACHK-MAX-CNT
           MOVE  WRK-KANACHK-STR   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-KANACHK-STR
      *
           .
       800-ZENKAKU-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-HIZUKE-EDIT-SEC             SECTION.
      *
           MOVE    SPACE           TO  WRK-EDTYMD1
                                       WRK-EDTYMD3
      *
           IF    ( WRK-SYMD            =   SPACE )
               CONTINUE
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"   USING
                                   STS-AREA-DAY
                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-EDTYMD1
               MOVE    LNK-DAY2-EDTYMD3    TO  WRK-EDTYMD3
               INSPECT WRK-EDTYMD3 REPLACING  ALL "  "  BY  "　"
           END-IF
      *
      *
           .
       800-HIZUKE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者情報取得処理
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    ORCHCN68-PTID       TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHを行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
