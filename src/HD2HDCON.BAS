DECLARE SUB TITLE ()
DECLARE SUB GETKEY ()
DECLARE SUB MMenuVert (BYVAL Address%, Choice%, Start%, SCANCODE%, NormClr%, HiLite%, NumRows%, Row%, Column%)
DECLARE SUB ReadFile (BYVAL Address%)
DECLARE FUNCTION GetDrive% ()
DECLARE FUNCTION FCount% (FSpec$)
DECLARE FUNCTION QPRTRIM$ (A$)
DECLARE SUB SortStr (BYVAL Address%, size%, Direction%)
DECLARE FUNCTION EXIST% (FILENAME$)
DECLARE SUB MAMenu (BYVAL Address%, SELECTION%, Start%, Count%, ExitCode%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%)
  CLS
   DIM O%(100), O$(100), TR%(100), H$(100), Z1$(40), HW(50), AW(50), HL(50), AL(50), R1(50), R2(50), R3(50), R4(50)
   DIM Q$(377), B%(14, 19), B$(14), A$(14, 1), VV$(500), VV%(500), Z2$(40)
   DIM TM1$(40), OPP$(100), ZA$(40), Z1%(40), Z3$(40)
   DIM TAW(40), DV$(4), THW(40), item$(2), THL(40), TAL(40), TR3(40), TR4(40), D$(1)
   DIM TR1(40), TR2(40), QQ$(1), T%(9), L%(4)
   D% = GetDrive%
   OPEN "DEV.BAS" FOR INPUT AS #2
   FOR I = 0 TO 1: INPUT #2, D$(I): NEXT
   CLOSE #2

600 CALL TITLE
   item$(1) = "VIEW TEAM VS CONFERENCE"
   item$(2) = "RETURN TO MAIN  MENU   "
     SELECTION% = 1
     Count% = 2
     Start% = 1
     SCANCODE% = 2
     FileColr% = 31
     HiLiteColr% = 112
     Spacing% = 1
     Row% = 8
     Column% = 25
     CALL box0(7, 24, 10, 48, 2, 31)
     CALL SHOWCURSOR
     MMenuVert VARPTR(item$(1)), SELECTION%, Start%, SCANCODE%, FileColr%, HiLiteColr%, Count%, Row%, Column%

     CALL HIDECURSOR
     COLOR , 0
     IF SCANCODE% = 27 THEN SELECTION% = 2
     IF SELECTION% = 1 THEN 630

     IF D% < 67 THEN
      CLS : COLOR 15: PRINT "MAKE SURE GAME DISK IS IN DRIVE"
      LOCATE 23: PRINT "HIT ANY KEY TO CONTINUE"
      CALL GETKEY
   END IF
   RUN "HELLO.EXE 3"

630 RDD = 0

39 ER = 0
40 GOSUB 3056
   B$ = QQ$(1): GOSUB 6585
   Z$ = MID$(B$, 4, QQ) + "." + LEFT$(B$, 2)
   IF NOT EXIST%(D$(1) + Z$ + "S") THEN 4500
    OPEN D$(1) + Z$ + "S" FOR INPUT AS #4
    INPUT #4, Y
    FOR J = 1 TO 100: INPUT #4, O%(J): NEXT
    FOR J = 1 TO 100: INPUT #4, O$(J): NEXT
    FOR J = 1 TO 100: INPUT #4, TR%(J): NEXT
    FOR J = 1 TO 100: INPUT #4, H$(J): NEXT
    CLOSE #4
    GOSUB 1440
    LG$ = F$
49   CLS : J = 5: GOSUB 2075: COLOR 7
     LOCATE , 30: PRINT "STATS TO "; : COLOR 15: PRINT "S"; : COLOR 7: PRINT "CREEN"
     LOCATE , 30: PRINT "STATS TO "; : COLOR 15: PRINT "P"; : COLOR 7: PRINT "RINTER"
     VPP = 0
8651 I$ = INKEY$: IF I$ = "" THEN 8651
     COLOR 14, 0
     IF UCASE$(I$) = "P" THEN X1 = 1
     IF UCASE$(I$) = "S" THEN VPP = 1
78 W$ = LG$ + ".LIB"
   IF UCASE$(LEFT$(D$(1), 1)) = "A" OR UCASE$(LEFT$(D$(1), 1)) = "B" THEN PRINT "MAKE SURE LEAGUE DISK IS IN DRIVE "; D$(1): CALL GETKEY
   OPEN D$(1) + W$ FOR INPUT AS #1
   INPUT #1, X
   INPUT #1, LG$
   INPUT #1, D1$: INPUT #1, D2$
   FOR X = 1 TO 30
   INPUT #1, Z1$(X): INPUT #1, Z1%(X): INPUT #1, Z2$(X)
   NEXT
   NT = 30
   CLOSE 1
   CLS
    COLOR 15: LOCATE 18, 18: PRINT "COMPUTING W/L/T RECORDS . . . PLEASE WAIT"
    FOR Q = 1 TO 30
    AW(Q) = 0: HW(Q) = 0
    AL(Q) = 0: HL(Q) = 0
    R1(Q) = 0: R2(Q) = 0
    R3(Q) = 0: R4(Q) = 0
    NEXT
    FOR K = 1 TO 2
    THW(K) = 0: THL(K) = 0
    TAW(K) = 0: TAL(K) = 0
    TR1(K) = 0: TR2(K) = 0
    TR3(K) = 0: TR4(K) = 0
    NEXT
    FOR K = 1 TO 30
    IF Z1$(K) = "XXX" THEN 181
    FOR I = 1 TO Y
    IF O%(I) > TR%(I) AND LEFT$(H$(I), 1) = "V" AND O$(I) = Z1$(K) THEN AW(K) = AW(K) + 1
    IF O%(I) > TR%(I) AND (LEFT$(H$(I), 1) = "H" OR LEFT$(H$(I), 1) = "N") AND O$(I) = Z1$(K) THEN HW(K) = HW(K) + 1
    IF O%(I) < TR%(I) AND LEFT$(H$(I), 1) = "V" AND O$(I) = Z1$(K) THEN AL(K) = AL(K) + 1
    IF O%(I) < TR%(I) AND (LEFT$(H$(I), 1) = "H" OR LEFT$(H$(I), 1) = "N") AND O$(I) = Z1$(K) THEN HL(K) = HL(K) + 1
    IF O$(I) = Z1$(K) AND (LEFT$(H$(I), 1) = "H" OR LEFT$(H$(I), 1) = "N") THEN R2(K) = R2(K) + TR%(I)
    IF O$(I) = Z1$(K) AND (LEFT$(H$(I), 1) = "H" OR LEFT$(H$(I), 1) = "N") THEN R1(K) = R1(K) + O%(I)
    IF O$(I) = Z1$(K) AND LEFT$(H$(I), 1) = "V" THEN R4(K) = R4(K) + TR%(I)
    IF O$(I) = Z1$(K) AND LEFT$(H$(I), 1) = "V" THEN R3(K) = R3(K) + O%(I)
181 NEXT I: NEXT K
    IF VPP <> 1 THEN 1200
190 DV$(1) = D1$
    DV$(2) = D2$

200 CLS : COLOR 12
    LOCATE 2, 35: COLOR T%(0), T%(1)
    PRINT Z$: COLOR 0, 0
    COLOR 3: LOCATE 4
    PRINT TAB(15); STRING$(7, 196); " HOME "; STRING$(8, 196); TAB(36); CHR$(194); STRING$(7, 196); " AWAY "; STRING$(8, 196); TAB(58); CHR$(194); STRING$(6, 196); " TOTALS "; STRING$(7, 196)
    LOCATE 5: PRINT TAB(15); "W"; TAB(17); "L"; TAB(21); "PCT."; TAB(26); "PF/G"; TAB(32); "PA/G"; TAB(36); CHR$(179); TAB(37); "W"; TAB(39); "L"; TAB(43); "PCT."; TAB(48); "PF/G"; TAB(54); "PA/G"; TAB(58); CHR$(179); TAB(59); "W"; TAB(61); "L"; TAB _
(65); "PCT."; TAB(70); "PF/G"; TAB(76); "PA/G"
    RDD = RDD + 1
    IF RDD = 1 THEN 210
    IF RDD = 2 THEN 211
210 DD = 1: EE = 15: IF DV$(RDD) = "XXX" OR DV$(RDD) = "" THEN 400 ELSE 215
211 DD = 16: EE = 30: IF DV$(RDD) = "XXX" OR DV$(RDD) = "" THEN 400 ELSE 215
215 COLOR 11: PRINT DV$(RDD); : COLOR 3: PRINT TAB(36); CHR$(179); TAB(58); CHR$(179)
    FOR K = DD TO EE
    IF Z1$(K) = "XXX" OR Z1$(K) = TM$ THEN 250
    COLOR 15: LOCATE , 1: PRINT LEFT$(Z1$(K), 13);
    COLOR 7: LOCATE , 15: PRINT USING "#"; HW(K);
    LOCATE , 17: PRINT USING "#"; HL(K);
    IF HW(K) = HW(K) + HL(K) AND HW(K) <> 0 THEN LOCATE , 19: PRINT "1.000";
    IF HW(K) = 0 THEN LOCATE , 20: PRINT ".000";
    IF HW(K) > 0 AND HW(K) < HW(K) + HL(K) THEN LOCATE , 20: PRINT USING ".###"; (HW(K) / (HW(K) + HL(K)));
    IF R1(K) = 0 THEN LOCATE , 25: PRINT "  0.0";
    IF R1(K) > 0 THEN LOCATE , 25: PRINT USING "###.#"; R1(K) / (HW(K) + HL(K));
    IF R2(K) = 0 THEN LOCATE , 31: PRINT "  0.0";
    IF R2(K) > 0 THEN LOCATE , 31: PRINT USING "###.#"; R2(K) / (HW(K) + HL(K));
    COLOR 3: PRINT TAB(36); CHR$(179);
    COLOR 7: LOCATE , 37: PRINT USING "#"; AW(K);
    LOCATE , 39: PRINT USING "#"; AL(K);
    IF AW(K) = AW(K) + AL(K) AND AW(K) <> 0 THEN LOCATE , 41: PRINT "1.000";
    IF AW(K) = 0 THEN LOCATE , 42: PRINT ".000";
    IF AW(K) > 0 AND AW(K) < AW(K) + AL(K) THEN LOCATE , 42: PRINT USING ".###"; (AW(K) / (AW(K) + AL(K)));
    IF R3(K) = 0 THEN LOCATE , 47: PRINT "  0.0";
    IF R3(K) > 0 THEN LOCATE , 47: PRINT USING "###.#"; R3(K) / (AW(K) + AL(K));
    IF R4(K) = 0 THEN LOCATE , 53: PRINT "  0.0";
    IF R4(K) > 0 THEN LOCATE , 53: PRINT USING "###.#"; R4(K) / (AW(K) + AL(K));
    COLOR 3: PRINT TAB(58); CHR$(179);
    COLOR 7: LOCATE , 59: PRINT USING "##"; HW(K) + AW(K);
    LOCATE , 61: PRINT USING "##"; HL(K) + AL(K);
    IF (HW(K) + AW(K)) = (HW(K) + AW(K) + HL(K) + AL(K)) AND (HW(K) + AW(K)) <> 0 THEN LOCATE , 63: PRINT "1.000";
    IF (HW(K) + AW(K)) = 0 THEN LOCATE , 64: PRINT ".000";
    IF (HW(K) + AW(K)) > 0 AND (HW(K) + AW(K)) < (HW(K) + AW(K) + HL(K) + AL(K)) THEN LOCATE , 64: PRINT USING ".###"; ((HW(K) + AW(K)) / (HW(K) + AW(K) + HL(K) + AL(K)));
    IF R1(K) + R3(K) = 0 THEN LOCATE , 69: PRINT "  0.0";
    IF R1(K) + R3(K) > 0 THEN LOCATE , 69: PRINT USING "###.#"; (R1(K) + R3(K)) / (HW(K) + HL(K) + AW(K) + AL(K));
    IF R2(K) + R4(K) = 0 THEN LOCATE , 75: PRINT "  0.0";
    IF R2(K) + R4(K) > 0 THEN LOCATE , 75: PRINT USING "###.#"; (R2(K) + R4(K)) / (HW(K) + HL(K) + AW(K) + AL(K))
    COLOR 3: PRINT TAB(36); CHR$(179); TAB(58); CHR$(179);
    THW(RDD) = THW(RDD) + HW(K)
    THL(RDD) = THL(RDD) + HL(K)
    TAW(RDD) = TAW(RDD) + AW(K)
    TAL(RDD) = TAL(RDD) + AL(K)
    TR1(RDD) = TR1(RDD) + R1(K)
    TR2(RDD) = TR2(RDD) + R2(K)
    TR3(RDD) = TR3(RDD) + R3(K)
    TR4(RDD) = TR4(RDD) + R4(K)
250 NEXT K
    PRINT
    COLOR 11: LOCATE , 14: PRINT USING "##"; THW(RDD);
    IF THW(RDD) = THW(RDD) + THL(RDD) AND THW(RDD) <> 0 THEN LOCATE , 19: PRINT "1.000";
    IF THW(RDD) = 0 THEN LOCATE , 20: PRINT ".000";
    IF THW(RDD) > 0 AND THW(RDD) < THW(RDD) + THL(RDD) THEN LOCATE , 20: PRINT USING ".###"; (THW(RDD) / (THW(RDD) + THL(RDD)));
    IF TR2(RDD) < 1 THEN LOCATE , 32: PRINT " 0.0";
    IF TR2(RDD) > 0 THEN LOCATE , 31: PRINT USING "###.#"; TR2(RDD) / (THW(RDD) + THL(RDD));
    LOCATE , 38: PRINT USING "##"; TAL(RDD);
    IF TR3(RDD) < 1 THEN LOCATE , 48: PRINT " 0.0";
    IF TR3(RDD) > 0 THEN LOCATE , 47: PRINT USING "###.#"; TR3(RDD) / (TAW(RDD) + TAL(RDD));
    LOCATE , 57: PRINT USING "###"; THW(RDD) + TAW(RDD);
    COLOR 11: IF THW(RDD) + TAW(RDD) = THW(RDD) + THL(RDD) + TAW(RDD) + TAL(RDD) AND THW(RDD) + TAW(RDD) <> 0 THEN LOCATE , 63: PRINT "1.000";
    IF THW(RDD) + TAW(RDD) = 0 THEN LOCATE , 64: PRINT ".000";
    IF THW(RDD) + TAW(RDD) > 0 AND THW(RDD) + TAW(RDD) < THW(RDD) + TAW(RDD) + THL(RDD) + TAL(RDD) THEN LOCATE , 64: PRINT USING ".###"; (THW(RDD) + TAW(RDD)) / (THW(RDD) + TAW(RDD) + THL(RDD) + TAL(RDD));
    IF TR2(RDD) + TR4(RDD) < 1 THEN LOCATE , 76: PRINT " 0.0"
    IF TR2(RDD) + TR4(RDD) > 0 THEN LOCATE , 75: PRINT USING "###.#"; (TR2(RDD) + TR4(RDD)) / (THW(RDD) + THL(RDD) + TAW(RDD) + TAL(RDD))
    LOCATE , 5: PRINT "TOTALS";
    IF TR1(RDD) < 1 THEN LOCATE , 26: PRINT " 0.0";
    IF TR1(RDD) > 0 THEN LOCATE , 25: PRINT USING "###.#"; TR1(RDD) / (THW(RDD) + THL(RDD));
    COLOR 11: IF TAW(RDD) = TAW(RDD) + TAL(RDD) AND TAW(RDD) <> 0 THEN LOCATE , 41: PRINT "1.000";
    IF TAW(RDD) = 0 THEN LOCATE , 42: PRINT ".000";
    IF TAW(RDD) > 0 AND TAW(RDD) < TAW(RDD) + TAL(RDD) THEN LOCATE , 42: PRINT USING ".###"; (TAW(RDD) / (TAW(RDD) + TAL(RDD)));
    IF TR4(RDD) < 1 THEN LOCATE , 54: PRINT " 0.0";
    IF TR4(RDD) > 0 THEN LOCATE , 53: PRINT USING "###.#"; TR4(RDD) / (TAW(RDD) + TAL(RDD));
    IF TR1(RDD) + TR3(RDD) = 0 THEN LOCATE , 70: PRINT " 0.0";
    IF TR1(RDD) + TR3(RDD) > 0 THEN LOCATE , 69: PRINT USING "###.#"; (TR1(RDD) + TR3(RDD)) / (THW(RDD) + THL(RDD) + TAW(RDD) + TAL(RDD));
    LOCATE , 16: PRINT USING "##"; THL(RDD);
    LOCATE , 36: PRINT USING "##"; TAW(RDD);
    LOCATE , 59: PRINT USING "###"; THL(RDD) + TAL(RDD)
    LOCATE 20, 5: COLOR 14: INPUT "HIT ENTER TO CONTINUE"; DUMMY$
    IF RDD = 4 THEN 400 ELSE 200
400 CLS : COLOR 12: LOCATE 2, 35: COLOR T%(0), T%(1): PRINT Z$: COLOR 0, 0
    COLOR 3: LOCATE 4: PRINT TAB(9); STRING$(8, 196); " HOME "; STRING$(9, 196); TAB(32); CHR$(194); STRING$(8, 196); " AWAY "; STRING$(9, 196); TAB(56); CHR$(194); STRING$(7, 196); " TOTALS "; STRING$(8, 196)
    LOCATE 5: PRINT TAB(10); "W"; TAB(13); "L"; TAB(17); "PCT."; TAB(22); "PF/G"; TAB(28); "PA/G"; TAB(32); CHR$(179); TAB(34); "W"; TAB(37); "L"; TAB(41); "PCT."; TAB(46); "PF/G"; TAB(52); "PA/G"; TAB(56); CHR$(179); TAB(58); "W"; TAB(61); "L"; TAB _
(65); "PCT."; TAB(70); "PF/G"; TAB(76); "PA/G"
    COLOR 3: PRINT TAB(32); CHR$(179); TAB(56); CHR$(179)
    FOR W = 1 TO 2
    IF DV$(W) = "XXX" OR DV$(W) = "" THEN 490 ELSE 461
461 COLOR 15: LOCATE , 1: PRINT LEFT$(DV$(W), 10);
    COLOR 7: LOCATE , 9: PRINT USING "##"; THW(W);
    LOCATE , 12: PRINT USING "##"; THL(W);
    IF THW(W) = THW(W) + THL(W) AND THW(W) <> 0 THEN LOCATE , 15: PRINT "1.000";
    IF THW(W) = 0 THEN LOCATE , 16: PRINT ".000";
    IF THW(W) > 0 AND THW(W) < THW(W) + THL(W) THEN LOCATE , 16: PRINT USING ".###"; (THW(W) / (THW(W) + THL(W)));
    IF TR1(W) = 0 THEN LOCATE , 21: PRINT "  0.0";
    IF TR1(W) > 0 THEN LOCATE , 21: PRINT USING "###.#"; TR1(W) / (THW(W) + THL(W));
    IF TR2(W) = 0 THEN LOCATE , 27: PRINT "  0.0";
    IF TR2(W) > 0 THEN LOCATE , 27: PRINT USING "###.#"; TR2(W) / (THW(W) + THL(W));
    COLOR 3: PRINT TAB(32); CHR$(179);
    COLOR 7: LOCATE , 33: PRINT USING "##"; TAW(W);
    LOCATE , 36: PRINT USING "##"; TAL(W);
    IF TAW(W) = TAW(W) + TAL(W) AND TAW(W) <> 0 THEN LOCATE , 39: PRINT "1.000";
    IF TAW(W) = 0 THEN LOCATE , 40: PRINT ".000";
    IF TAW(W) > 0 AND TAW(W) < TAW(W) + TAL(W) THEN LOCATE , 40: PRINT USING ".###"; (TAW(W) / (TAW(W) + TAL(W)));
    IF TR3(W) = 0 THEN LOCATE , 45: PRINT "  0.0";
    IF TR3(W) > 0 THEN LOCATE , 45: PRINT USING "###.#"; TR3(W) / (TAW(W) + TAL(W));
    IF TR4(W) = 0 THEN LOCATE , 51: PRINT "  0.0";
    IF TR4(W) > 0 THEN LOCATE , 51: PRINT USING "###.#"; TR4(W) / (TAW(W) + TAL(W));
    COLOR 3: PRINT TAB(56); CHR$(179);
    COLOR 7: LOCATE , 57: PRINT USING "##"; THW(W) + TAW(W);
    LOCATE , 60: PRINT USING "##"; THL(W) + TAL(W);
    IF THW(W) + TAW(W) = THW(W) + TAW(W) + THL(W) + TAL(W) AND THW(W) + TAW(W) <> 0 THEN LOCATE , 63: PRINT "1.000";
    IF (THW(W) + TAW(W)) = 0 THEN LOCATE , 64: PRINT ".000";
    IF (THW(W) + TAW(W)) > 0 AND THW(W) + TAW(W) < THW(W) + TAW(W) + THL(W) + TAL(W) THEN LOCATE , 64: PRINT USING ".###"; ((THW(W) + TAW(W)) / (THW(W) + TAW(W) + THL(W) + TAL(W)));
    IF TR1(W) + TR3(W) = 0 THEN LOCATE , 69: PRINT "  0.0";
    IF TR1(W) + TR3(W) > 0 THEN LOCATE , 69: PRINT USING "###.#"; (TR1(W) + TR3(W)) / (THW(W) + THL(W) + TAW(W) + TAL(W));
    IF TR2(W) + TR4(W) = 0 THEN LOCATE , 75: PRINT "  0.0";
    IF TR2(W) + TR4(W) > 0 THEN LOCATE , 75: PRINT USING "###.#"; (TR2(W) + TR4(W)) / (THW(W) + THL(W) + TAW(W) + TAL(W))
    COLOR 3: PRINT TAB(32); CHR$(179); TAB(56); CHR$(179);
490 NEXT W
    THWW = 0: THLL = 0: TAWW = 0: TALL = 0
    FOR I = 1 TO 4
    THWW = THWW + THW(I): THLL = THLL + THL(I): TAWW = TAWW + TAW(I): TALL = TALL + TAL(I)
    NEXT
    TR11 = 0: TR22 = 0: TR33 = 0: TR44 = 0
    FOR I = 1 TO 4
    TR11 = TR11 + TR1(I): TR22 = TR22 + TR2(I): TR33 = TR33 + TR3(I): TR44 = TR44 + TR4(I)
    NEXT
    PRINT
    COLOR 11: PRINT "TOTALS";
    LOCATE , 9: PRINT USING "##"; THWW;
    LOCATE , 12: PRINT USING "##"; THLL;
    IF THWW = THWW + THLL AND THWW <> 0 THEN LOCATE , 15: PRINT "1.000";
    IF THWW = 0 THEN LOCATE , 16: PRINT ".000";
    IF THWW > 0 AND THWW < THWW + THLL THEN LOCATE , 16: PRINT USING ".###"; THWW / (THWW + THLL);
    IF TR11 = 0 THEN LOCATE , 21: PRINT "  0.0";
    IF TR11 > 0 THEN LOCATE , 21: PRINT USING "###.#"; TR11 / (THWW + THLL);
    IF TR22 = 0 THEN LOCATE , 27: PRINT "  0.0";
    IF TR22 > 0 THEN LOCATE , 27: PRINT USING "###.#"; TR22 / (THWW + THLL);
    COLOR 3: PRINT TAB(32); CHR$(179);
    COLOR 11: LOCATE , 33: PRINT USING "##"; TAWW;
    LOCATE , 36: PRINT USING "##"; TALL;
    IF TAWW = TAWW + TALL AND TAWW <> 0 THEN LOCATE , 39: PRINT "1.000";
    IF TAWW = 0 THEN LOCATE , 40: PRINT ".000";
    IF TAWW > 0 AND TAWW < TAWW + TALL THEN LOCATE , 40: PRINT USING ".###"; TAWW / (TAWW + TALL);
    IF TR33 = 0 THEN LOCATE , 45: PRINT "  0.0";
    IF TR33 > 0 THEN LOCATE , 45: PRINT USING "###.#"; TR33 / (TAWW + TALL);
    IF TR44 = 0 THEN LOCATE , 51: PRINT "  0.0";
    IF TR44 > 0 THEN LOCATE , 51: PRINT USING "###.#"; TR44 / (TAWW + TALL);
    COLOR 3: PRINT TAB(56); CHR$(179);
    COLOR 11: LOCATE , 57: PRINT USING "##"; THWW + TAWW;
    LOCATE , 60: PRINT USING "##"; THLL + TALL;
    IF THWW + TAWW = THWW + TAWW + THLL + TALL AND THWW + TAWW <> 0 THEN LOCATE , 63: PRINT "1.000";
    IF THWW + TAWW = 0 THEN LOCATE , 64: PRINT ".000";
    IF THWW + TAWW > 0 AND THWW + TAWW < THWW + TAWW + THLL + TALL THEN LOCATE , 64: PRINT USING ".###"; (THWW + TAWW) / (THWW + TAWW + THLL + TALL);
    IF TR11 + TR33 = 0 THEN LOCATE , 69: PRINT "  0.0";
    IF TR11 + TR33 > 0 THEN LOCATE , 69: PRINT USING "###.#"; (TR11 + TR33) / (THWW + THLL + TAWW + TALL);
    IF TR22 + TR44 = 0 THEN LOCATE , 75: PRINT "  0.0";
    IF TR22 + TR44 > 0 THEN LOCATE , 75: PRINT USING "###.#"; (TR22 + TR44) / (THWW + THLL + TAWW + TALL)
    CALL GETKEY
    GOTO 600




1200 LPRINT CHR$(15)
     WIDTH "LPT1:1", 130
     DV$(1) = D1$
     DV$(2) = D2$
     LPRINT TAB(55); Z$: LPRINT
     LPRINT TAB(22); "--------- HOME ----------"; TAB(54); "--------- AWAY ----------"; TAB(86); "---------- TOTALS ----------"
     LPRINT TAB(22); "W"; TAB(26); "L"; TAB(30); "PCT."; TAB(36); "PF/G"; TAB(43); "PA/G"; TAB(54); "W"; TAB(58); "L"; TAB(62); "PCT."; TAB(68); "PF/G"; TAB(75); "PA/G"; TAB(86); "W"; TAB(91); "L"; TAB(97); "PCT."; TAB(103); "PF/G"; TAB(110); "PA/G"
1204 RDD = RDD + 1
     IF RDD = 1 THEN 1210
     IF RDD = 2 THEN 1211
1210 DD = 1: EE = 15: IF DV$(RDD) = "XXX" OR DV$(RDD) = "" THEN 1401 ELSE 1215
1211 DD = 16: EE = 30: IF DV$(RDD) = "XXX" OR DV$(RDD) = "" THEN 1401 ELSE 1215
1215 LPRINT TAB(2); DV$(RDD)
     FOR K = DD TO EE
     IF Z1$(K) = "XXX" OR Z1$(K) = TM$ THEN 1250
     LPRINT Z1$(K);
     LPRINT TAB(22); USING "#"; HW(K);
     LPRINT TAB(26); USING "#"; HL(K);
     IF HW(K) = HW(K) + HL(K) AND HW(K) <> 0 THEN LPRINT TAB(28); "1.000";
     IF HW(K) = 0 THEN LPRINT TAB(29); ".000";
     IF HW(K) > 0 AND HW(K) < HW(K) + HL(K) THEN LPRINT TAB(29); USING ".###"; (HW(K) / (HW(K) + HL(K)));
     IF R1(K) = 0 THEN LPRINT TAB(35); "  0.0";
     IF R1(K) > 0 THEN LPRINT TAB(35); USING "###.#"; R1(K) / (HW(K) + HL(K));
     IF R2(K) = 0 THEN LPRINT TAB(42); "  0.0";
     IF R2(K) > 0 THEN LPRINT TAB(42); USING "###.#"; R2(K) / (HW(K) + HL(K));
     LPRINT TAB(54); USING "#"; AW(K);
     LPRINT TAB(58); USING "#"; AL(K);
     IF AW(K) = AW(K) + AL(K) AND AW(K) <> 0 THEN LPRINT TAB(60); "1.000";
     IF AW(K) = 0 THEN LPRINT TAB(61); ".000";
     IF AW(K) > 0 AND AW(K) < AW(K) + AL(K) THEN LPRINT TAB(61); USING ".###"; (AW(K) / (AW(K) + AL(K)));
     IF R3(K) = 0 THEN LPRINT TAB(67); "  0.0";
     IF R3(K) > 0 THEN LPRINT TAB(67); USING "###.#"; R3(K) / (AW(K) + AL(K));
     IF R4(K) = 0 THEN LPRINT TAB(74); "  0.0";
     IF R4(K) > 0 THEN LPRINT TAB(74); USING "###.#"; R4(K) / (AW(K) + AL(K));
     LPRINT TAB(86); USING "##"; HW(K) + AW(K);
     LPRINT TAB(91); USING "##"; HL(K) + AL(K);
     IF HW(K) + AW(K) = HW(K) + AW(K) + HL(K) + AL(K) AND HW(K) + AW(K) <> 0 THEN LPRINT TAB(95); "1.000";
     IF (HW(K) + AW(K)) = 0 THEN LPRINT TAB(96); ".000";
     IF (HW(K) + AW(K)) > 0 AND (HW(K) + AW(K)) < (HW(K) + AW(K) + HL(K) + AL(K)) THEN LPRINT TAB(96); USING ".###"; ((HW(K) + AW(K)) / (HW(K) + AW(K) + HL(K) + AL(K)));
     IF R1(K) + R3(K) = 0 THEN LPRINT TAB(102); "  0.0";
     IF R1(K) + R3(K) > 0 THEN LPRINT TAB(102); USING "###.#"; (R1(K) + R3(K)) / (HW(K) + HL(K) + AW(K) + AL(K));
     IF R2(K) + R4(K) = 0 THEN LPRINT TAB(109); "  0.0"
     IF R2(K) + R4(K) > 0 THEN LPRINT TAB(109); USING "###.#"; (R2(K) + R4(K)) / (HW(K) + HL(K) + AW(K) + AL(K))
     THW(RDD) = THW(RDD) + HW(K)
     THL(RDD) = THL(RDD) + HL(K)
     TAW(RDD) = TAW(RDD) + AW(K)
     TAL(RDD) = TAL(RDD) + AL(K)
     TR1(RDD) = TR1(RDD) + R1(K)
     TR2(RDD) = TR2(RDD) + R2(K)
     TR3(RDD) = TR3(RDD) + R3(K)
     TR4(RDD) = TR4(RDD) + R4(K)
1250 NEXT K
     LPRINT TAB(5); "TOTALS";
     LPRINT TAB(21); USING "##"; THW(RDD);
     LPRINT TAB(25); USING "##"; THL(RDD);
     IF THW(RDD) = THW(RDD) + THL(RDD) AND THW(RDD) <> 0 THEN LPRINT TAB(28); "1.000";
     IF THW(RDD) = 0 THEN LPRINT TAB(29); ".000";
     IF THW(RDD) > 0 AND THW(RDD) < THW(RDD) + THL(RDD) THEN LPRINT TAB(29); USING ".###"; (THW(RDD) / (THW(RDD) + THL(RDD)));
     IF TR1(RDD) = 0 THEN LPRINT TAB(35); "  0.0";
     IF TR1(RDD) > 0 THEN LPRINT TAB(35); USING "###.#"; TR1(RDD) / (THW(RDD) + THL(RDD));
     IF TR2(RDD) = 0 THEN LPRINT TAB(42); "  0.0";
     IF TR2(RDD) > 0 THEN LPRINT TAB(42); USING "###.#"; TR2(RDD) / (THW(RDD) + THL(RDD));
     LPRINT TAB(53); USING "##"; TAW(RDD);
     LPRINT TAB(57); USING "##"; TAL(RDD);
     IF TAW(RDD) = TAW(RDD) + TAL(RDD) AND TAW(RDD) <> 0 THEN LPRINT TAB(60); "1.000";
     IF TAW(RDD) = 0 THEN LPRINT TAB(61); ".000";
     IF TAW(RDD) > 0 AND TAW(RDD) < TAW(RDD) + TAL(RDD) THEN LPRINT TAB(61); USING ".###"; (TAW(RDD) / (TAW(RDD) + TAL(RDD)));
     IF TR3(RDD) = 0 THEN LPRINT TAB(67); "  0.0";
     IF TR3(RDD) > 0 THEN LPRINT TAB(67); USING "###.#"; TR3(RDD) / (TAW(RDD) + TAL(RDD));
     IF TR4(RDD) = 0 THEN LPRINT TAB(74); "  0.0";
     IF TR4(RDD) > 0 THEN LPRINT TAB(74); USING "###.#"; TR4(RDD) / (TAW(RDD) + TAL(RDD));
     LPRINT TAB(84); USING "###"; THW(RDD) + TAW(RDD);
     LPRINT TAB(89); USING "###"; THL(RDD) + TAL(RDD);
     IF THW(RDD) + TAW(RDD) = THW(RDD) + TAW(RDD) + THL(RDD) + TAL(RDD) AND THW(RDD) + TAW(RDD) <> 0 THEN LPRINT TAB(95); "1.000";
     IF THW(RDD) + TAW(RDD) = 0 THEN LPRINT TAB(96); ".000";
     IF THW(RDD) + TAW(RDD) > 0 AND THW(RDD) + TAW(RDD) < THW(RDD) + TAW(RDD) + THL(RDD) + TAL(RDD) THEN LPRINT TAB(96); USING ".###"; ((THW(RDD) + TAW(RDD)) / (THW(RDD) + TAW(RDD) + THL(RDD) + TAL(RDD)));
     IF TR1(RDD) + TR3(RDD) = 0 THEN LPRINT TAB(102); "  0.0";
     IF TR1(RDD) + TR3(RDD) > 0 THEN LPRINT TAB(102); USING "###.#"; (TR1(RDD) + TR3(RDD)) / (THW(RDD) + THL(RDD) + TAW(RDD) + TAL(RDD));
     IF TR2(RDD) + TR4(RDD) = 0 THEN LPRINT TAB(109); "  0.0"
     IF TR2(RDD) + TR4(RDD) > 0 THEN LPRINT TAB(109); USING "###.#"; (TR2(RDD) + TR4(RDD)) / (THW(RDD) + THL(RDD) + TAW(RDD) + TAL(RDD))
     LPRINT
     IF RDD = 2 THEN 1401 ELSE 1204
1401 LPRINT : LPRINT : LPRINT TAB(22); "--------- HOME ----------"; TAB(54); "--------- AWAY ----------"; TAB(86); "---------- TOTALS ----------"
     LPRINT TAB(22); "W"; TAB(26); "L"; TAB(30); "PCT."; TAB(36); "PF/G"; TAB(43); "PA/G"; TAB(54); "W"; TAB(58); "L"; TAB(62); "PCT."; TAB(68); "PF/G"; TAB(75); "PA/G"; TAB(86); "W"; TAB(91); "L"; TAB(97); "PCT."; TAB(103); "PF/G"; TAB(110); "PA/G"
     LPRINT
     FOR W = 1 TO 2
     IF DV$(W) = "XXX" OR DV$(W) = "" THEN 1490 ELSE 1461
1461 LPRINT TAB(2); DV$(W);
     LPRINT TAB(21); USING "##"; THW(W);
     LPRINT TAB(25); USING "##"; THL(W);
     IF THW(W) = THW(W) + THL(W) AND THW(W) <> 0 THEN LPRINT TAB(28); "1.000";
     IF THW(W) = 0 THEN LPRINT TAB(29); ".000";
     IF THW(W) > 0 AND THW(W) < THW(W) + THL(W) THEN LPRINT TAB(29); USING ".###"; (THW(W) / (THW(W) + THL(W)));
     IF TR1(W) = 0 THEN LPRINT TAB(35); "  0.0";
     IF TR1(W) > 0 THEN LPRINT TAB(35); USING "###.#"; TR1(W) / (THW(W) + THL(W));
     IF TR2(W) = 0 THEN LPRINT TAB(42); "  0.0";
     IF TR2(W) > 0 THEN LPRINT TAB(42); USING "###.#"; TR2(W) / (THW(W) + THL(W));
     LPRINT TAB(53); USING "##"; TAW(W);
     LPRINT TAB(57); USING "##"; TAL(W);
     IF TAW(W) = TAW(W) + TAL(W) AND TAW(W) <> 0 THEN LPRINT TAB(60); "1.000";
     IF TAW(W) = 0 THEN LPRINT TAB(61); ".000";
     IF TAW(W) > 0 AND TAW(W) < TAW(W) + TAL(W) THEN LPRINT TAB(61); USING ".###"; (TAW(W) / (TAW(W) + TAL(W)));
     IF TR3(W) = 0 THEN LPRINT TAB(67); "  0.0";
     IF TR3(W) > 0 THEN LPRINT TAB(67); USING "###.#"; TR3(W) / (TAW(W) + TAL(W));
     IF TR4(W) = 0 THEN LPRINT TAB(74); "  0.0";
     IF TR4(W) > 0 THEN LPRINT TAB(74); USING "###.#"; TR4(W) / (TAW(W) + TAL(W));
     LPRINT TAB(84); USING "###"; THW(W) + TAW(W);
     LPRINT TAB(89); USING "###"; THL(W) + TAL(W);
     IF THW(W) + TAW(W) = THW(W) + TAW(W) + THL(W) + TAL(W) AND THW(W) + TAW(W) <> 0 THEN LPRINT TAB(95); "1.000";
     IF (THW(W) + TAW(W)) = 0 THEN LPRINT TAB(96); ".000";
     IF (THW(W) + TAW(W)) > 0 AND THW(W) + TAW(W) < THW(W) + TAW(W) + THL(W) + TAL(W) THEN LPRINT TAB(96); USING ".###"; ((THW(W) + TAW(W)) / (THW(W) + TAW(W) + THL(W) + TAL(W)));
     IF TR1(W) + TR3(W) = 0 THEN LPRINT TAB(102); "  0.0";
     IF TR1(W) + TR3(W) > 0 THEN LPRINT TAB(102); USING "###.#"; (TR1(W) + TR3(W)) / (THW(W) + THL(W) + TAW(W) + TAL(W));
     IF TR2(W) + TR4(W) = 0 THEN LPRINT TAB(109); "  0.0";
     IF TR2(W) + TR4(W) > 0 THEN LPRINT TAB(109); USING "###.#"; (TR2(W) + TR4(W)) / (THW(W) + THL(W) + TAW(W) + TAL(W))
1490 NEXT W
     THWW = 0: THLL = 0: TAWW = 0: TALL = 0
     FOR I = 1 TO 4
     THWW = THWW + THW(I): THLL = THLL + THL(I): TAWW = TAWW + TAW(I): TALL = TALL + TAL(I)
     NEXT
     TR11 = 0: TR22 = 0: TR33 = 0: TR44 = 0
     FOR I = 1 TO 4
     TR11 = TR11 + TR1(I): TR22 = TR22 + TR2(I): TR33 = TR33 + TR3(I): TR44 = TR44 + TR4(I)
     NEXT
     LPRINT TAB(5); "TOTALS";
     LPRINT TAB(20); USING "###"; THWW;
     LPRINT TAB(24); USING "###"; THLL;
     IF THWW = THWW + THLL AND THWW <> 0 THEN LPRINT TAB(28); "1.000";
     IF THWW = 0 THEN LPRINT TAB(29); ".000";
     IF THWW > 0 AND THWW < THWW + THLL THEN LPRINT TAB(29); USING ".###"; THWW / (THWW + THLL);
     IF TR11 = 0 THEN LPRINT TAB(35); "  0.0";
     IF TR11 > 0 THEN LPRINT TAB(35); USING "###.#"; TR11 / (THWW + THLL);
     IF TR22 = 0 THEN LPRINT TAB(42); "  0.0";
     IF TR22 > 0 THEN LPRINT TAB(42); USING "###.#"; TR22 / (THWW + THLL);
     LPRINT TAB(52); USING "###"; TAWW;
     LPRINT TAB(56); USING "###"; TALL;
     IF TAWW = TAWW + TALL AND TAWW <> 0 THEN LPRINT TAB(60); "1.000";
     IF TAWW = 0 THEN LPRINT TAB(61); ".000";
     IF TAWW > 0 AND TAWW < TAWW + TALL THEN LPRINT TAB(61); USING ".###"; TAWW / (TAWW + TALL);
     IF TR33 = 0 THEN LPRINT TAB(67); "  0.0";
     IF TR33 > 0 THEN LPRINT TAB(67); USING "###.#"; TR33 / (TAWW + TALL);
     IF TR44 = 0 THEN LPRINT TAB(74); "  0.0";
     IF TR44 > 0 THEN LPRINT TAB(74); USING "###.#"; TR44 / (TAWW + TALL);
     LPRINT TAB(84); USING "###"; THWW + TAWW;
     LPRINT TAB(89); USING "###"; THLL + TALL;
     IF THWW + TAWW = THWW + TAWW + THLL + TALL AND THWW + TAWW <> 0 THEN LPRINT TAB(95); "1.000";
     IF THWW + TAWW = 0 THEN LPRINT TAB(96); ".000";
     IF THWW + TAWW > 0 AND THWW + TAWW < THWW + TAWW + THLL + TALL THEN LPRINT TAB(96); USING ".###"; (THWW + TAWW) / (THWW + TAWW + THLL + TALL);
     IF TR11 + TR33 = 0 THEN LPRINT TAB(102); "  0.0";
     IF TR11 + TR33 > 0 THEN LPRINT TAB(102); USING "###.#"; (TR11 + TR33) / (THWW + THLL + TAWW + TALL);
     IF TR22 + TR44 = 0 THEN LPRINT TAB(109); "  0.0";
     IF TR22 + TR44 > 0 THEN LPRINT TAB(109); USING "###.#"; (TR22 + TR44) / (THWW + THLL + TAWW + TALL)
     LPRINT CHR$(18)
     WIDTH "LPT1:1", 80
     LPRINT CHR$(12)
     GOTO 600

3056 COLOR 15, 0: CLS
   JJ = 6
   SN = 1: LOCATE 6, 8: COLOR 15, 4
   PRINT " Ö"; STRING$(61, 196); "· "; : FOR II = 1 TO JJ: LOCATE 6 + II, 8
   PRINT " º"; STRING$(61, 32); "º "; : NEXT
   LOCATE 7 + JJ, 8
   PRINT " Ó"; STRING$(61, 196); "½ "; : LOCATE 7, 32
   LOCATE , 12: PRINT "ENTER DISK ID: ": PRINT
   LOCATE , 12: PRINT "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
   LOCATE , 12: PRINT "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
   PRINT
   LOCATE , 12: PRINT "ESC TO RETURN TO MAIN MENU"
3057 I$ = INKEY$: IF I$ = "" THEN 3057
   IF I$ = CHR$(27) THEN 600
   IF I$ = CHR$(13) THEN GOSUB 1102: GOTO 3060
   LOCATE 7, 27: PRINT I$
   LOCATE 7, 28: LINE INPUT NY$
   NY$ = I$ + NY$
   COLOR 15, 0
   GOSUB 4105
   IF NY$ = "" THEN GOSUB 1102: GOTO 3060
   YN$ = NY$
   FOR I = 1 TO 500: VV$(I) = "": NEXT
   GOSUB 1101
3060 IF BO% = 1 THEN BO% = 0: GOTO 3056
3058 LOCATE 2, 33: COLOR T%(0), T%(1): PRINT QQ$(1)
     COLOR 15, 4: LOCATE , 33: PRINT "ANY CHANGE (YN)";
     I$ = INKEY$: IF I$ = "" THEN 3058
    IF UCASE$(I$) = "Y" THEN 3056
    COLOR 15, 0
     RETURN
1101 OPEN D$(0) + "BASK." + YN$ FOR RANDOM AS #1 LEN = 655
     Y = LOF(1)
     FIELD #1, 15 AS Q$(0), 640 AS X$
     FOR I1 = 1 TO Y / 655
     GET #1, I1
     VV$(I1) = Q$(0): VV%(I1) = I1
    NEXT
    CLOSE 1
    NTMS = Y / 655
1102 SELECTION% = 1
     Count% = NTMS
     Start% = 1
     FileColr% = 79
     HiLiteColr% = 112
     Rows% = 20
     Columns% = 4
     Spacing% = 6
     Row% = 4
     Column% = 1
     COLOR , 4: CLS : LOCATE , 33: COLOR 14
     PRINT "TEAM SELECTION": COLOR 15
     LOCATE 25, 1: COLOR 14: PRINT "ESC"; : COLOR 15
     PRINT "-SELECT NEW ID  PGDN  PGUP  "; : COLOR 14
     PRINT "ENTER"; : COLOR 15: PRINT "-SELECTS TEAM";
     CALL SHOWCURSOR
20   MAMenu VARPTR(VV$(1)), SELECTION%, Start%, Count%, ExitCode%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%
     CALL HIDECURSOR
     IF ExitCode% = 27 THEN BO% = 1: RETURN
     IF ExitCode% = -71 THEN Start% = 1: ExitCode% = 3: SELECTION% = 1: GOTO 20
     IF ExitCode% = -79 THEN Start% = NTMS: ExitCode% = 3: SELECTION% = NTMS: GOTO 20
     IF ExitCode% = -73 THEN Start% = Start% - 80: IF Start% < 1 THEN Start% = 1
     IF ExitCode% = -81 THEN Start% = Start% + 80: IF Start% > NTMS THEN Start% = NTMS
     IF ExitCode% = -81 OR ExitCode% = -73 THEN ExitCode% = 3: SELECTION% = Start%: GOTO 20
     T1 = VV%(SELECTION%)
9000 OPEN D$(0) + "BASK." + YN$ FOR RANDOM AS #1 LEN = 655
FIELD #1, 15 AS Q$(0), 2 AS Q$(1), 2 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 2 AS Q$(14), 2 AS Q$(15), 2 AS Q$(16), 2 AS Q$(17), 2 AS Q$(18), 2 _
 AS Q$(19), 602 AS X$
FOR I = 0 TO 13
FIELD #1, 53 + I * 43 AS X$, 15 AS Q$(I * 15 + 20), 2 AS Q$(I * 15 + 21), 2 AS Q$(I * 15 + 22), 2 AS Q$(I * 15 + 23), 2 AS Q$(I * 15 + 24), 2 AS Q$(I * 15 + 25), 2 AS Q$(I * 15 + 26), 2 AS Q$(I * 15 + 27), 2 AS Q$(I * 15 + 28), 2 AS Q$(I * 15 + 29) _
, 2 AS Q$(I * 15 + 30), 2 AS Q$(I * 15 + 31), 2 AS Q$(I * 15 + 32), 2 AS Q$(I * 15 + 33), 2 AS Q$(I * 15 + 34), 655 - 53 - 43 - I * 43 AS X$
NEXT
GET #1, T1
     TM$ = Q$(0)
9005 IF RIGHT$(TM$, 1) = " " THEN TM$ = LEFT$(TM$, LEN(TM$) - 1): GOTO 9005
     QQ$(1) = TM$
     SP = CVI(Q$(1)): DA = CVI(Q$(2)): V1% = CVI(Q$(3)): V2% = CVI(Q$(4))
     FOR I = 0 TO 4: L%(I) = CVI(Q$(I + 5)): NEXT
     FOR I = 0 TO 9: T%(I) = CVI(Q$(I + 10)): NEXT
     CLOSE 1
     RETURN

6585 FOR XX = 4 TO 11: IF MID$(B$, XX, 1) = " " OR MID$(B$, XX, 1) = "." THEN MID$(B$, XX, 1) = "_"
     NEXT XX
     FOR XX = 12 TO 14: IF MID$(B$, XX, 2) = "ST" THEN MID$(B$, 9, 1) = "_": MID$(B$, 10, 2) = "ST"
     NEXT XX
     QQ = 8: FOR XX = 11 TO 4 STEP -1: IF MID$(B$, XX, 1) = "_" THEN QQ = XX - 4: GOTO 6615
     GOTO 6620
6615 NEXT XX
6620 RETURN

4500 CLS : J = 4: GOSUB 2075: LOCATE 10, 18
     PRINT "COMPILED STATS WERE NOT FOUND"
     LOCATE , 18: PRINT "PRESS ANY KEY TO RETURN TO MAIN MENU"
     CALL GETKEY
     COLOR , 0
     GOTO 600

2075 REM:
     LOCATE 8, 8: COLOR 15, 1: PRINT " Ö"; STRING$(61, 196); "· ";
     FOR II = 1 TO J: LOCATE 8 + II, 8: PRINT " º"; STRING$(61, 32); "º "; : NEXT
     LOCATE 9 + J, 8: PRINT " Ó"; STRING$(61, 196); "½ "; : LOCATE 10, 15: RETURN
4105 IF UCASE$(LEFT$(D$(0), 1)) <> "A" AND UCASE$(LEFT$(D$(0), 1)) <> "B" THEN RETURN
     CLS : PRINT "MAKE SURE CORRECT TEAMS DISK IS IN DRIVE "; D$(0)
     LOCATE 23, 1: PRINT "PRESS ANY KEY TO CONTINUE"
     CALL GETKEY
     RETURN

1440 Spec$ = D$(1) + "*.LIB"
    Count% = FCount%(Spec$)
   IF Count% = 0 THEN 4600
   REDIM F$(0 TO Count%)
   FOR X = 1 TO Count%
   F$(X) = SPACE$(12)
   NEXT
   F$(0) = Spec$
   CALL ReadFile(VARPTR(F$(0)))
   CALL SortStr(VARPTR(F$(1)), Count%, 0)

1445 CLS
    COLOR , 1
    CLS
    LOCATE , 28
    COLOR 14
    PRINT "SELECT LEAGUE": COLOR 15
     SELECTION% = 1
     Start% = 1
     FileColr% = 31
     HiLiteColr% = 112
     Rows% = 20
     Columns% = 4
     Spacing% = 6
     Row% = 4
     Column% = 1
    LOCATE 25, 1
    COLOR 14
    PRINT "ENTER"; : COLOR 15
    PRINT "-SELECTS LEAGUE";
CALL SHOWCURSOR
MAMenu VARPTR(F$(1)), SELECTION%, Start%, Count%, ExitCode%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%
CALL HIDECURSOR
     IF ExitCode% = 27 THEN 600
     F$ = F$(SELECTION%)
   LOCATE 2, 33: PRINT F$: LOCATE , 33: PRINT "ANY CHANGE (YN)";
     COLOR 15, 0
3912 I$ = INKEY$: IF I$ = "" THEN 3912
    IF UCASE$(I$) = "Y" THEN 1445
3913 F$ = QPRTRIM$(F$)
    F$ = LEFT$(F$, LEN(F$) - 4)
    RETURN

4600 LOCATE 10, 8
     PRINT "NO LEAGUE FILES WERE FOUND"
     LOCATE , 8: PRINT "PRESS ANY KEY TO RETURN TO MAIN MENU"
3806 I$ = INKEY$: IF I$ = "" THEN 3806
     GOTO 600

   SUB GETKEY ()
45 I$ = INKEY$: IF I$ = "" THEN 45
END SUB

SUB TITLE ()
   COLOR , 0: KEY OFF: CLS
   COLOR , 1: LOCATE 1, 1: PRINT TAB(80); : LOCATE , 23: COLOR 12
   PRINT "COURTSIDE COLLEGE BASKETBALL V4.0"
   LOCATE 24, 1: PRINT TAB(80); : LOCATE , 1
   PRINT "(C) LANCE HAFFNER/SHANNON LYNN 1987-1994"; TAB(70); LEFT$(TIME$, 5);
   LOCATE 3, 1: COLOR 2, 0
   COLOR 15: PRINT "        PROGRAMMED AND DEVELOPED BY LANCE HAFFNER AND SHANNON LYNN"
             PRINT "     FOR TECHNICAL SUPPORT, CALL (615) 366-8088 OR SportGames@AOL.COM"
   COLOR 10
END SUB

