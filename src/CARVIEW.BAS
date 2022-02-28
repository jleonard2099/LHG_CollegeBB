   DECLARE SUB ReadFile (BYVAL Address%)
   DECLARE SUB SortStr (BYVAL Address%, Size%, Direction%)
   DECLARE SUB MAMenu (BYVAL Address%, SELECTION%, START%, COUNT%, EXITCODE%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%)
   DECLARE FUNCTION QPRTrim$ (Work$)
   DECLARE SUB MQPrint (X$, Colr)
   DECLARE FUNCTION FCount% (FSpec$)
   DECLARE FUNCTION EXIST% (FILENAME$)
   DECLARE SUB MAMenu (BYVAL Address%, SELECTION%, START%, COUNT%, EXITCODE%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%)
   DECLARE FUNCTION GetDrive% ()
   DECLARE SUB MMenuVert (BYVAL Address%, Choice%, START%, SCANCODE%, NormClr%, HiLite%, NumRows%, Row%, Column%)
  CLS : TMR = 930
  DIM A$(62), A(72, 11), W0(62), W1(62)
  DIM AR$(62), AR(62, 11), W0R(62), W1R(62), D$(1)
  REDIM ARS(15, 62, 15), W0S(15, 62), W1S(15, 62), TT(62)
  DRIVE = GetDrive%
   OPEN "DEV.BAS" FOR INPUT AS #1
   FOR I = 0 TO 1: INPUT #1, D$(I)
   NEXT
   CLOSE #1
30 Spec$ = D$(1) + "*.CAR"
   COUNT% = FCount%(Spec$)
   IF COUNT% = 0 THEN 4500
   REDIM F$(0 TO COUNT%)
   FOR X = 1 TO COUNT%
       F$(X) = SPACE$(12)
   NEXT
   F$(0) = Spec$
   CALL ReadFile(VARPTR(F$(0)))
   CALL SortStr(VARPTR(F$(1)), COUNT%, 0)

1310 CLS
    COLOR , 1
    CLS
    LOCATE , 28
    COLOR 14
    PRINT "SELECT CAREER FILE TO VIEW": COLOR 15
     SELECTION% = 1
     START% = 1
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
    PRINT "-SELECTS FILE";
CALL SHOWCURSOR
20 MAMenu VARPTR(F$(1)), SELECTION%, START%, COUNT%, EXITCODE%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%
CALL HIDECURSOR
     IF EXITCODE% = 27 THEN RUN "CAREER.EXE"
     IF EXITCODE% = -71 THEN START% = 1: EXITCODE% = 3: SELECTION% = 1: GOTO 20
     IF EXITCODE% = -79 THEN START% = COUNT%: EXITCODE% = 3: SELECTION% = COUNT%: GOTO 20
     IF EXITCODE% = -73 THEN START% = START% - 80: IF START% < 1 THEN START% = 1
     IF EXITCODE% = -81 THEN START% = START% + 80: IF START% > COUNT% THEN START% = COUNT%
     IF EXITCODE% = -81 OR EXITCODE% = -73 THEN EXITCODE% = 3: SELECTION% = START%: GOTO 20
     F$ = F$(SELECTION%)
   LOCATE 2, 33: PRINT F$: LOCATE , 33: PRINT "ANY CHANGE (YN)";
     COLOR 15, 0
3910 I$ = INKEY$: IF I$ = "" THEN 3910
    IF UCASE$(I$) = "Y" THEN 1310
     F$ = QPRTrim$(F$)
    Z$ = LEFT$(F$, LEN(F$) - 4)
   GOSUB 9500
   COLOR 15: LOCATE 12, 10: PRINT "LOADING CAREER FILE . . . . PLEASE WAIT"
   COLOR 15: LOCATE 12, 10: PRINT "LOADING CAREER FILE . . . . PLEASE WAIT"
   OPEN D$(1) + Z$ + ".CAR" FOR INPUT AS #2
   INPUT #2, NB, B$
   FOR N = 1 TO 15
   FOR I = 0 TO 61: INPUT #2, A$(I)
   FOR J = 0 TO 15: INPUT #2, ARS(N, I, J): NEXT J: INPUT #2, W0S(N, I): INPUT #2, W1S(N, I): LOCATE 12, 52: COLOR 5: TMR = TMR - 1: PRINT USING "####"; TMR: NEXT I: NEXT N
   CLOSE #2
   LOCATE 18, 10: COLOR 12: PRINT "P"; : COLOR 7: PRINT "RINT OR "; : COLOR 12: PRINT "V"; : COLOR 7: PRINT "IEW"
81 I$ = INKEY$: IF I$ = "" THEN 81
   IF UCASE$(I$) <> "P" AND UCASE$(I$) <> "V" THEN 81
   IF UCASE$(I$) = "P" THEN 1100
   GOTO 2000

100 CLS : FOR I = 0 TO 61
    IF A$(I) = "" OR A$(I) = "XXX" THEN 242
    TT1 = 0: TT2 = 0: TT3 = 0: TT4 = 0: TT5 = 0: TT6 = 0: TT7 = 0: TT8 = 0: TT9 = 0: TT10 = 0: TT11 = 0: TT12 = 0: TT13 = 0: TT14 = 0: TT15 = 0: TT16 = 0: TT17 = 0
    COLOR 11: LOCATE 1: PRINT Z$
    COLOR 12: LOCATE 2
    PRINT "PLAYER         YEAR  G FGM  FGA   PCT FTM FTA   PCT 3FGM 3FGA PCT  PTS  AVG"
    LOCATE 3: COLOR 15: PRINT LEFT$(A$(I), 13);
    VV = ARS(1, I, 14) + ARS(2, I, 14) + ARS(3, I, 14) + ARS(4, I, 14) + ARS(5, I, 14) + ARS(6, I, 14) + ARS(7, I, 14) + ARS(8, I, 14) + ARS(9, I, 14) + ARS(10, I, 14) + ARS(11, I, 14) + ARS(12, I, 14) + ARS(13, I, 14) + ARS(14, I, 14) + ARS(15, I,  _
14)
    FOR V = 1 TO VV
    COLOR 7: PRINT TAB(15); ARS(V, I, 15);
    PRINT TAB(21); USING "##"; ARS(V, I, 0);
    PRINT TAB(24); USING "###"; ARS(V, I, 2);
    PRINT TAB(28); USING "####"; ARS(V, I, 1);
    IF ARS(V, I, 2) = 0 THEN PRINT TAB(33); " --.-";
    IF ARS(V, I, 2) > 0 THEN PRINT TAB(33); USING "###.#"; (ARS(V, I, 2) / ARS(V, I, 1)) * 100;
    PRINT TAB(39); USING "###"; ARS(V, I, 4);
    PRINT TAB(43); USING "###"; ARS(V, I, 3);
    IF ARS(V, I, 4) = 0 THEN PRINT TAB(47); " --.-";
    IF ARS(V, I, 4) > 0 THEN PRINT TAB(47); USING "###.#"; (ARS(V, I, 4) / ARS(V, I, 3)) * 100;
    PRINT TAB(53); USING "###"; W0S(V, I);
    PRINT TAB(57); USING "###"; W1S(V, I);
    IF W0S(V, I) = 0 THEN PRINT TAB(61); " --.-";
    IF W0S(V, I) > 0 THEN PRINT TAB(61); USING "###.#"; (W0S(V, I) / W1S(V, I)) * 100;
    PRINT TAB(67); USING "####"; ARS(V, I, 2) * 2 + ARS(V, I, 4) + W0S(V, I);
    IF ARS(V, I, 2) * 2 + ARS(V, I, 4) + W0S(V, I) = 0 THEN PRINT TAB(72); " 0.0";
    IF ARS(V, I, 2) * 2 + ARS(V, I, 4) + W0S(V, I) > 0 THEN PRINT TAB(72); USING "##.#"; (ARS(V, I, 2) * 2 + ARS(V, I, 4) + W0S(V, I)) / ARS(V, I, 0);
    NEXT V
    GOSUB 600
    CLS : COLOR 11: LOCATE 1: PRINT Z$
    COLOR 12: PRINT "PLAYER         YEAR OREB DREB  TRB  AVG  PF  AST BLK  ST  TO HI   MIN  AVG."
    LOCATE 3: COLOR 15: PRINT LEFT$(A$(I), 13);
    FOR V = 1 TO VV
    COLOR 7: PRINT TAB(15); ARS(V, I, 15);
    PRINT TAB(21); USING "####"; ARS(V, I, 12);
    PRINT TAB(26); USING "####"; ARS(V, I, 5) - ARS(V, I, 12);
    PRINT TAB(31); USING "####"; ARS(V, I, 5);
    IF ARS(V, I, 5) = 0 THEN PRINT TAB(36); " 0.0";
    IF ARS(V, I, 5) > 0 THEN PRINT TAB(36); USING "##.#"; ARS(V, I, 5) / ARS(V, I, 0);
    PRINT TAB(41); USING "###"; ARS(V, I, 6);
    PRINT TAB(46); USING "###"; ARS(V, I, 8);
    PRINT TAB(50); USING "###"; ARS(V, I, 9);
    PRINT TAB(54); USING "###"; ARS(V, I, 7);
    PRINT TAB(58); USING "###"; ARS(V, I, 11);
    PRINT TAB(61); USING "###"; ARS(V, I, 13);
    IF ARS(V, I, 10) = 0 THEN PRINT TAB(65); "   0";
    IF ARS(V, I, 10) > 0 THEN PRINT TAB(66); USING "####"; INT((ARS(V, I, 10) + 30) / 60);
    IF ARS(V, I, 10) = 0 THEN PRINT TAB(73); " 0";
    IF ARS(V, I, 10) > 0 THEN PRINT TAB(73); USING "##"; INT(((ARS(V, I, 10) / 60) / ARS(V, I, 0)) + .5);
    NEXT V
    GOSUB 650
    TT1 = 0: TT2 = 0: TT3 = 0: TT4 = 0: TT5 = 0: TT6 = 0: TT7 = 0: TT8 = 0: TT9 = 0: TT10 = 0: TT11 = 0: TT12 = 0: TT13 = 0: TT14 = 0: TT15 = 0: TT16 = 0: TT17 = 0
    PRINT : PRINT : PRINT : COLOR 14: PRINT "HIT ENTER TO CONTINUE OR ESC TO GO TO MAIN MENU"
233 I$ = INKEY$: IF I$ = "" THEN 233
    IF I$ = CHR$(27) THEN 2000
241 CLS
242 NEXT I
    CLS : LOCATE 5, 20: COLOR 14: PRINT "VIEW AGAIN (Y/N)"
371 I$ = INKEY$: IF I$ = "" THEN 371
    IF UCASE$(I$) = "Y" THEN 100 ELSE 373
373 GOTO 2000
375 IF DRIVE < 67 THEN GOSUB 400
    RUN "CAREER.EXE"
400 CLS : PRINT "MAKE SURE PROGRAM DISK IS IN DRIVE"
    LOCATE 23, 1: PRINT "PRESS ANY KEY TO CONTINUE"
405 I$ = INKEY$: IF I$ = "" THEN 405
    RETURN
600 VV = ARS(1, I, 14) + ARS(2, I, 14) + ARS(3, I, 14) + ARS(4, I, 14) + ARS(5, I, 14) + ARS(6, I, 14) + ARS(7, I, 14) + ARS(8, I, 14) + ARS(9, I, 14) + ARS(10, I, 14) + ARS(11, I, 14) + ARS(12, I, 14) + ARS(13, I, 14) + ARS(14, I, 14) + ARS(15, I,  _
14)
    FOR V = 1 TO VV
    TT1 = TT1 + ARS(V, I, 0)
    TT2 = TT2 + ARS(V, I, 2)
    TT3 = TT3 + ARS(V, I, 1)
    TT4 = TT4 + ARS(V, I, 4)
    TT5 = TT5 + ARS(V, I, 3)
    TT6 = TT6 + (ARS(V, I, 2) * 2 + ARS(V, I, 4) + W0S(V, I))
    TT7 = TT7 + W0S(V, I)
    TT8 = TT8 + W1S(V, I)
    NEXT V
    PRINT : COLOR 3
    PRINT
    PRINT TAB(18); USING "##"; VV;
    PRINT TAB(22); USING "#####"; TT2;
    IF TT2 = 0 THEN PRINT TAB(33); " --.-";
    IF TT2 > 0 THEN PRINT TAB(33); USING "###.#"; (TT2 / TT3) * 100;
    PRINT TAB(41); USING "#####"; TT5;
    PRINT TAB(52); USING "####"; TT7;
    IF TT7 = 0 THEN PRINT TAB(61); " --.-";
    IF TT7 > 0 THEN PRINT TAB(61); USING "###.#"; (TT7 / TT8) * 100;
    IF TT6 = 0 THEN PRINT TAB(72); " 0.0"
    IF TT6 > 0 THEN PRINT TAB(72); USING "##.#"; TT6 / TT1
    PRINT "TOTALS";
    PRINT TAB(19); USING "####"; TT1;
    PRINT TAB(27); USING "#####"; TT3;
    PRINT TAB(37); USING "#####"; TT4;
    IF TT4 = 0 THEN PRINT TAB(47); " --.-";
    IF TT4 > 0 THEN PRINT TAB(47); USING "###.#"; (TT4 / TT5) * 100;
    PRINT TAB(56); USING "####"; TT8;
    PRINT TAB(67); USING "####"; TT6
    PRINT : PRINT : PRINT : COLOR 14: LINE INPUT "HIT ENTER TO CONTINUE"; DUMMY$
    RETURN
650 VV = ARS(1, I, 14) + ARS(2, I, 14) + ARS(3, I, 14) + ARS(4, I, 14) + ARS(5, I, 14) + ARS(6, I, 14) + ARS(7, I, 14) + ARS(8, I, 14) + ARS(9, I, 14) + ARS(10, I, 14) + ARS(11, I, 14) + ARS(12, I, 14) + ARS(13, I, 14) + ARS(14, I, 14) + ARS(15, I,  _
14)
    FOR V = 1 TO VV
    TT9 = TT9 + ARS(V, I, 5)
    TT10 = TT10 + ARS(V, I, 6)
    TT11 = TT11 + ARS(V, I, 8)
    TT12 = TT12 + ARS(V, I, 9)
    TT13 = TT13 + ARS(V, I, 7)
    TT14 = TT14 + ARS(V, I, 10)
    TT16 = TT16 + ARS(V, I, 11)
    TT17 = TT17 + ARS(V, I, 12)
    IF ARS(V, I, 13) > TT15 THEN TT15 = ARS(V, I, 13)
    NEXT V
    PRINT : COLOR 3
    PRINT
    PRINT TAB(18); USING "##"; VV;
    PRINT TAB(26); USING "####"; TT9 - TT17;
    IF TT9 = 0 THEN PRINT TAB(36); " 0.0";
    IF TT9 > 0 THEN PRINT TAB(36); USING "##.#"; TT9 / TT1;
    PRINT TAB(45); USING "####"; TT11;
    PRINT TAB(54); USING "###"; TT13;
    PRINT TAB(62); USING "##"; TT15;
    IF TT14 = 0 THEN PRINT TAB(73); " 0"
    IF TT14 > 0 THEN PRINT TAB(73); USING "##"; INT((TT14 / 60) / TT1) + .5
    PRINT "TOTALS";
    PRINT TAB(21); USING "####"; TT17;
    PRINT TAB(30); USING "#####"; TT9;
    PRINT TAB(40); USING "####"; TT10;
    PRINT TAB(49); USING "####"; TT12;
    PRINT TAB(58); USING "###"; TT16;
    IF TT14 = 0 THEN PRINT TAB(65); "    0"
    IF TT14 > 0 THEN PRINT TAB(65); USING "#####"; INT((TT14 + 30) / 60)
    RETURN
1100 PTN = 0
     LPRINT CHR$(15)
     WIDTH "LPT1:1", 130
     LPRINT TAB(59); Z$: LPRINT
     LPRINT "PLAYER      YEAR   G  FGM   FGA  PCT  FTM   FTA  PCT   PTS  AVG.  3FGM 3FGA  PCT   RB   AVG   PF  AST  BLK   ST  HI   MIN  AVG."
     FOR I = 0 TO 61
     IF A$(I) = "" OR A$(I) = "XXX" THEN 1242
     TT1 = 0: TT2 = 0: TT3 = 0: TT4 = 0: TT5 = 0: TT6 = 0: TT7 = 0: TT8 = 0: TT9 = 0: TT10 = 0: TT11 = 0: TT12 = 0: TT13 = 0: TT14 = 0: TT15 = 0: TT16 = 0: TT17 = 0
     VV = ARS(1, I, 14) + ARS(2, I, 14) + ARS(3, I, 14) + ARS(4, I, 14) + ARS(5, I, 14) + ARS(6, I, 14) + ARS(7, I, 14) + ARS(8, I, 14) + ARS(9, I, 14) + ARS(10, I, 14) + ARS(11, I, 14) + ARS(12, I, 14) + ARS(13, I, 14) + ARS(14, I, 14) + ARS(15, I _
, 14)
     IF 55 - PTN < VV THEN GOSUB 3000 ELSE 1112
1112 LPRINT LEFT$(A$(I), 11);
     FOR V = 1 TO VV
     PTN = PTN + 1
     LPRINT TAB(12); ARS(V, I, 15);
     LPRINT TAB(19); USING "##"; ARS(V, I, 0);
     LPRINT TAB(23); USING "###"; ARS(V, I, 2);
     LPRINT TAB(28); USING "####"; ARS(V, I, 1);
     IF ARS(V, I, 2) = 0 THEN LPRINT TAB(34); "--.-";
     IF ARS(V, I, 2) > 0 THEN LPRINT TAB(33); USING "###.#"; (ARS(V, I, 2) / ARS(V, I, 1)) * 100;
     LPRINT TAB(39); USING "###"; ARS(V, I, 4);
     LPRINT TAB(45); USING "###"; ARS(V, I, 3);
     IF ARS(V, I, 4) = 0 THEN LPRINT TAB(50); "--.-";
     IF ARS(V, I, 4) > 0 THEN LPRINT TAB(49); USING "###.#"; (ARS(V, I, 4) / ARS(V, I, 3)) * 100;
     LPRINT TAB(55); USING "####"; ARS(V, I, 2) * 2 + ARS(V, I, 4) + W0S(V, I);
     IF ARS(V, I, 2) * 2 + ARS(V, I, 4) + W0S(V, I) = 0 THEN LPRINT TAB(61); " 0.0";
     IF ARS(V, I, 2) * 2 + ARS(V, I, 4) + W0S(V, I) > 0 THEN LPRINT TAB(61); USING "##.#"; (ARS(V, I, 2) * 2 + ARS(V, I, 4) + W0S(V, I)) / ARS(V, I, 0);
     LPRINT TAB(68); USING "###"; W0S(V, I);
     LPRINT TAB(73); USING "###"; W1S(V, I);
     IF W0S(V, I) = 0 THEN LPRINT TAB(78); "--.-";
     IF W0S(V, I) > 0 THEN LPRINT TAB(77); USING "###.#"; (W0S(V, I) / W1S(V, I)) * 100;
     LPRINT TAB(83); USING "###"; ARS(V, I, 5);
     IF ARS(V, I, 5) = 0 THEN LPRINT TAB(88); " 0.0";
     IF ARS(V, I, 5) > 0 THEN LPRINT TAB(88); USING "##.#"; ARS(V, I, 5) / ARS(V, I, 0);
     LPRINT TAB(94); USING "###"; ARS(V, I, 6);
     LPRINT TAB(99); USING "###"; ARS(V, I, 8);
     LPRINT TAB(104); USING "###"; ARS(V, I, 9);
     LPRINT TAB(109); USING "###"; ARS(V, I, 7);
     LPRINT TAB(113); USING "###"; ARS(V, I, 13);
     IF ARS(V, I, 10) = 0 THEN LPRINT TAB(118); "   0";
     IF ARS(V, I, 10) > 0 THEN LPRINT TAB(118); USING "####"; INT((ARS(V, I, 10) + 30) / 60);
     IF ARS(V, I, 10) = 0 THEN LPRINT TAB(125); " 0"
     IF ARS(V, I, 10) > 0 THEN LPRINT TAB(125); USING "##"; INT(((ARS(V, I, 10) / 60) / ARS(V, I, 0)) + .5)
     NEXT V
     GOSUB 1600
     PTN = PTN + 2
1242 NEXT I
     GOTO 375
1600 VV = ARS(1, I, 14) + ARS(2, I, 14) + ARS(3, I, 14) + ARS(4, I, 14) + ARS(5, I, 14) + ARS(6, I, 14) + ARS(7, I, 14) + ARS(8, I, 14) + ARS(9, I, 14) + ARS(10, I, 14) + ARS(11, I, 14) + ARS(12, I, 14) + ARS(13, I, 14) + ARS(14, I, 14) + ARS(15, I _
, 14)
     FOR V = 1 TO VV
     TT1 = TT1 + ARS(V, I, 0)
     TT2 = TT2 + ARS(V, I, 2)
     TT3 = TT3 + ARS(V, I, 1)
     TT4 = TT4 + ARS(V, I, 4)
     TT5 = TT5 + ARS(V, I, 3)
     TT6 = TT6 + (ARS(V, I, 2) * 2 + ARS(V, I, 4) + W0S(V, I))
     TT7 = TT7 + W0S(V, I)
     TT8 = TT8 + W1S(V, I)
     TT9 = TT9 + ARS(V, I, 5)
     TT10 = TT10 + ARS(V, I, 6)
     TT11 = TT11 + ARS(V, I, 8)
     TT12 = TT12 + ARS(V, I, 9)
     TT13 = TT13 + ARS(V, I, 7)
     TT14 = TT14 + ARS(V, I, 10)
     TT16 = TT16 + ARS(V, I, 11)
     TT17 = TT17 + ARS(V, I, 12)
     IF ARS(V, I, 13) > TT15 THEN TT15 = ARS(V, I, 13)
     NEXT V
     LPRINT "  TOTALS";
     LPRINT TAB(15); USING "##"; VV;
     LPRINT TAB(17); USING "####"; TT1;
     LPRINT TAB(21); USING "#####"; TT2;
     LPRINT TAB(27); USING "#####"; TT3;
     IF TT2 = 0 THEN LPRINT TAB(34); "--.-";
     IF TT2 > 0 THEN LPRINT TAB(34); USING "###.#"; (TT2 / TT3) * 100;
     LPRINT TAB(37); USING "#####"; TT4;
     LPRINT TAB(43); USING "#####"; TT5;
     IF TT4 = 0 THEN LPRINT TAB(50); "--.-";
     IF TT4 > 0 THEN LPRINT TAB(50); USING "###.#"; (TT4 / TT5) * 100;
     LPRINT TAB(55); USING "####"; TT6;
     IF TT6 = 0 THEN LPRINT TAB(61); " 0.0";
     IF TT6 > 0 THEN LPRINT TAB(61); USING "##.#"; TT6 / TT1;
     LPRINT TAB(67); USING "####"; TT7;
     LPRINT TAB(72); USING "####"; TT8;
     IF TT7 = 0 THEN LPRINT TAB(78); "--.-";
     IF TT7 > 0 THEN LPRINT TAB(78); USING "###.#"; (TT7 / TT8) * 100;
     LPRINT TAB(81); USING "#####"; TT9;
     IF TT9 = 0 THEN LPRINT TAB(88); " 0.0";
     IF TT9 > 0 THEN LPRINT TAB(88); USING "##.#"; TT9 / TT1;
     LPRINT TAB(93); USING "####"; TT10;
     LPRINT TAB(98); USING "####"; TT11;
     LPRINT TAB(103); USING "####"; TT12;
     LPRINT TAB(109); USING "###"; TT13;
     LPRINT TAB(114); USING "##"; TT15;
     IF TT14 = 0 THEN LPRINT TAB(117); "    0";
     IF TT14 > 0 THEN LPRINT TAB(117); USING "#####"; INT((TT14 + 30) / 60);
     IF TT14 = 0 THEN LPRINT TAB(125); " 0"
     IF TT14 > 0 THEN LPRINT TAB(125); USING "##"; INT((TT14 / 60) / TT1) + .5
     LPRINT
     RETURN

2000 CLS : COLOR 7: PRINT "VIEW "; : COLOR 12: PRINT "P"; : COLOR 7: PRINT "LAYERS, "; : COLOR 12: PRINT "R"; : COLOR 7: PRINT "OSTER IN SEQUENCE, OR "; : COLOR 12: PRINT "E"; : COLOR 7: PRINT "XIT TO MAIN MENU"
2001 I$ = INKEY$: IF I$ = "" THEN 2001
     IF UCASE$(I$) = "B" THEN 2010
     IF UCASE$(I$) = "R" THEN 100
     IF UCASE$(I$) = "E" THEN 375
2010 CLS : COLOR 12: PRINT Z$
     FOR I = 0 TO 15: LOCATE I + 2: COLOR 3: PRINT USING "##"; I; : PRINT " "; : COLOR 7: PRINT A$(I): NEXT
     FOR I = 16 TO 31: LOCATE I - 14, 20: COLOR 3: PRINT USING "##"; I; : PRINT " "; : COLOR 7: PRINT A$(I): NEXT
     FOR I = 32 TO 46: LOCATE I - 30, 40: COLOR 3: PRINT USING "##"; I; : PRINT " "; : COLOR 7: PRINT A$(I): NEXT
     FOR I = 47 TO 61: LOCATE I - 45, 60: COLOR 3: PRINT USING "##"; I; : PRINT " "; : COLOR 7: PRINT A$(I): NEXT
     PRINT
     LOCATE 22, 48: COLOR 15: INPUT "ENTER # OF PLAYER TO VIEW"; B9
     CLS
     TT1 = 0: TT2 = 0: TT3 = 0: TT4 = 0: TT5 = 0: TT6 = 0: TT7 = 0: TT8 = 0: TT9 = 0: TT10 = 0: TT11 = 0: TT12 = 0: TT13 = 0: TT14 = 0: TT15 = 0: TT16 = 0: TT17 = 0
     COLOR 11: LOCATE 1: PRINT Z$
     COLOR 12: LOCATE 2: PRINT "PLAYER         YEAR  G  FGM  FGA  PCT FTM  FTA  PCT 3FGM 3FGA  PCT  PTS  AVG."
     LOCATE 3: COLOR 15: PRINT LEFT$(A$(B9), 13);
     VV = ARS(1, B9, 14) + ARS(2, B9, 14) + ARS(3, B9, 14) + ARS(4, B9, 14) + ARS(5, B9, 14) + ARS(6, B9, 14) + ARS(7, B9, 14) + ARS(8, B9, 14) + ARS(9, B9, 14) + ARS(10, B9, 14) + ARS(11, B9, 14) + ARS(12, B9, 14) + ARS(13, B9, 14) + ARS(14, B9, 14 _
) + ARS(15, B9, 14)
     FOR V = 1 TO VV
     COLOR 7: PRINT TAB(15); ARS(V, B9, 15);
     PRINT TAB(21); USING "##"; ARS(V, B9, 0);
     PRINT TAB(25); USING "###"; ARS(V, B9, 2);
     PRINT TAB(29); USING "####"; ARS(V, B9, 1);
     IF ARS(V, B9, 2) = 0 THEN PRINT TAB(35); "--.-";
     IF ARS(V, B9, 2) > 0 THEN PRINT TAB(34); USING "###.#"; (ARS(V, B9, 2) / ARS(V, B9, 1)) * 100;
     PRINT TAB(39); USING "###"; ARS(V, B9, 4);
     PRINT TAB(44); USING "###"; ARS(V, B9, 3);
     IF ARS(V, B9, 4) = 0 THEN PRINT TAB(49); "--.-";
     IF ARS(V, B9, 4) > 0 THEN PRINT TAB(48); USING "###.#"; (ARS(V, B9, 4) / ARS(V, B9, 3)) * 100;
     PRINT TAB(54); USING "###"; W0S(V, B9);
     PRINT TAB(59); USING "###"; W1S(V, B9);
     IF W0S(V, B9) = 0 THEN PRINT TAB(64); "--.-";
     IF W0S(V, B9) > 0 THEN PRINT TAB(63); USING "###.#"; (W0S(V, B9) / W1S(V, B9)) * 100;
     PRINT TAB(68); USING "####"; ARS(V, B9, 2) * 2 + ARS(V, B9, 4) + W0S(V, B9);
     IF ARS(V, B9, 2) * 2 + ARS(V, B9, 4) + W0S(V, B9) = 0 THEN PRINT TAB(74); " 0.0";
     IF ARS(V, B9, 2) * 2 + ARS(V, B9, 4) + W0S(V, B9) > 0 THEN PRINT TAB(74); USING "##.#"; (ARS(V, B9, 2) * 2 + ARS(V, B9, 4) + W0S(V, B9)) / ARS(V, B9, 0);
     NEXT V
     GOSUB 2600
     CLS : COLOR 11: LOCATE 1: PRINT Z$
     COLOR 12: PRINT "PLAYER         YEAR OREB DREB  TRB  AVG  PF  AST BLK  ST  TO HI   MIN  AVG."
     LOCATE 3: COLOR 15: PRINT LEFT$(A$(B9), 13);
     FOR V = 1 TO VV
     COLOR 7: PRINT TAB(15); ARS(V, B9, 15);
     PRINT TAB(21); USING "####"; ARS(V, B9, 12);
     PRINT TAB(26); USING "####"; ARS(V, B9, 5) - ARS(V, B9, 12);
     PRINT TAB(31); USING "####"; ARS(V, B9, 5);
     IF ARS(V, B9, 5) = 0 THEN PRINT TAB(36); " 0.0";
     IF ARS(V, B9, 5) > 0 THEN PRINT TAB(36); USING "##.#"; ARS(V, B9, 5) / ARS(V, B9, 0);
     PRINT TAB(41); USING "###"; ARS(V, B9, 6);
     PRINT TAB(46); USING "###"; ARS(V, B9, 8);
     PRINT TAB(50); USING "###"; ARS(V, B9, 9);
     PRINT TAB(54); USING "###"; ARS(V, B9, 7);
     PRINT TAB(58); USING "###"; ARS(V, B9, 11);
     PRINT TAB(61); USING "###"; ARS(V, B9, 13);
     IF ARS(V, B9, 10) = 0 THEN PRINT TAB(65); "   0";
     IF ARS(V, B9, 10) > 0 THEN PRINT TAB(66); USING "####"; INT((ARS(V, B9, 10) + 30) / 60);
     IF ARS(V, B9, 10) = 0 THEN PRINT TAB(73); " 0";
     IF ARS(V, B9, 10) > 0 THEN PRINT TAB(73); USING "##"; INT(((ARS(V, B9, 10) / 60) / ARS(V, B9, 0)) + .5);
     NEXT V
     GOSUB 2650
     GOTO 2790
2600 FOR V = 1 TO VV
     TT1 = TT1 + ARS(V, B9, 0)
     TT2 = TT2 + ARS(V, B9, 2)
     TT3 = TT3 + ARS(V, B9, 1)
     TT4 = TT4 + ARS(V, B9, 4)
     TT5 = TT5 + ARS(V, B9, 3)
     TT6 = TT6 + (ARS(V, B9, 2) * 2 + ARS(V, B9, 4) + W0S(V, B9))
    TT7 = TT7 + W0S(V, B9)
    TT8 = TT8 + W1S(V, B9)
     NEXT V
     PRINT : COLOR 3
     PRINT
     PRINT TAB(18); USING "##"; VV;
    PRINT TAB(23); USING "#####"; TT2;
    IF TT2 = 0 THEN PRINT TAB(35); "--.-";
    IF TT2 > 0 THEN PRINT TAB(35); USING "##.#"; (TT2 / TT3) * 100;
    PRINT TAB(42); USING "#####"; TT5;
    PRINT TAB(53); USING "####"; TT7;
    IF TT7 = 0 THEN PRINT TAB(64); "--.-";
    IF TT7 > 0 THEN PRINT TAB(64); USING "##.#"; (TT7 / TT8) * 100;
    IF TT6 = 0 THEN PRINT TAB(74); " 0.0"
    IF TT6 > 0 THEN PRINT TAB(74); USING "##.#"; TT6 / TT1
    PRINT "TOTALS";
     PRINT TAB(19); USING "####"; TT1;
     PRINT TAB(28); USING "#####"; TT3;
     PRINT TAB(37); USING "#####"; TT4;
     IF TT4 = 0 THEN PRINT TAB(49); "--.-";
     IF TT4 > 0 THEN PRINT TAB(49); USING "##.#"; (TT4 / TT5) * 100;
    PRINT TAB(58); USING "####"; TT8;
     PRINT TAB(68); USING "####"; TT6
     PRINT : PRINT : PRINT : COLOR 14: LINE INPUT "HIT ENTER TO CONTINUE"; DUMMY$
     RETURN
2650 FOR V = 1 TO VV
     TT9 = TT9 + ARS(V, B9, 5)
     TT10 = TT10 + ARS(V, B9, 6)
     TT11 = TT11 + ARS(V, B9, 8)
     TT12 = TT12 + ARS(V, B9, 9)
     TT13 = TT13 + ARS(V, B9, 7)
     TT14 = TT14 + ARS(V, B9, 10)
    TT16 = TT16 + ARS(V, B9, 11)
    TT17 = TT17 + ARS(V, B9, 12)
     IF ARS(V, B9, 13) > TT15 THEN TT15 = ARS(V, B9, 13)
     NEXT V
     PRINT : COLOR 3
     PRINT
     PRINT TAB(18); USING "##"; VV;
    PRINT TAB(26); USING "####"; TT9 - TT17;
    IF TT9 = 0 THEN PRINT TAB(36); " 0.0";
    IF TT9 > 0 THEN PRINT TAB(36); USING "##.#"; TT9 / TT1;
    PRINT TAB(45); USING "####"; TT11;
    PRINT TAB(54); USING "###"; TT13;
    PRINT TAB(62); USING "##"; TT15;
    IF TT14 = 0 THEN PRINT TAB(73); "--.-"
    IF TT14 > 0 THEN PRINT TAB(73); USING "##"; INT((TT14 / 60) / TT1) + .5
     PRINT "TOTALS";
    PRINT TAB(21); USING "####"; TT17;
     PRINT TAB(30); USING "#####"; TT9;
     PRINT TAB(40); USING "####"; TT10;
     PRINT TAB(49); USING "####"; TT12;
    PRINT TAB(58); USING "###"; TT16;
    IF TT14 = 0 THEN PRINT TAB(65); "  --.-"
    IF TT14 > 0 THEN PRINT TAB(65); USING "#####"; INT((TT14 + 30) / 60)
     RETURN
2790 PRINT : PRINT : COLOR 15: INPUT "HIT ENTER TO CONTINUE"; DUMMY$
     GOTO 2000
3000 LPRINT CHR$(12)
     LPRINT TAB(59); Z$: LPRINT
     LPRINT "PLAYER      YEAR   G  FGM   FGA  PCT  FTM   FTA  PCT   PTS  AVG.  3FGM 3FGA  PCT   RB   AVG   PF  AST  BLK   ST  HI   MIN  AVG."
     PTN = 0
     RETURN
4500 CLS
     PRINT "NO CAREER FILES WERE FOUND"
     PRINT "PRESS ANY KEY TO RETURN TO MAIN MENU"
3805 I$ = INKEY$: IF I$ = "" THEN 3805
     RUN "CAREER.EXE"
9500 IF LEFT$(D$(1), 1) <> "A" AND LEFT$(D$(1), 1) <> "B" THEN RETURN
     CLS : COLOR 15: PRINT "MAKE SURE DISK WITH CAREER FILE IS IN DRIVE "; : COLOR 12: PRINT D$(1)
     COLOR 15: LOCATE 23: PRINT "HIT ANY KEY TO CONTINUE"
9502 I$ = INKEY$: IF I$ = "" THEN 9502
     RETURN

