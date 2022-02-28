   DECLARE SUB ReadFile (BYVAL Address%)
   DECLARE SUB SortStr (BYVAL Address%, Size%, Direction%)
   DECLARE SUB MAMenu (BYVAL Address%, SELECTION%, Start%, Count%, EXITCODE%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%)
   DECLARE FUNCTION QPRTrim$ (Work$)
   DECLARE SUB MQPrint (X$, Colr)
   DECLARE FUNCTION FCount% (FSpec$)
   DECLARE FUNCTION EXIST% (FILENAME$)
   DECLARE SUB MAMenu (BYVAL Address%, SELECTION%, Start%, Count%, EXITCODE%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%)
   DECLARE FUNCTION GetDrive% ()
   DECLARE SUB MMenuVert (BYVAL Address%, Choice%, Start%, SCANCODE%, NormClr%, HiLite%, NumRows%, Row%, Column%)
   DIM A$(62), A(62, 14), W0(62), W1(62), Z(15), Z1(15), ZR(15), Z1R(15)
   DIM AR$(62), AR(62, 15), W0R(72), W1R(62), BLYR(21), BL(21), BL$(21)
   DIM W0S(15, 62), W1S(15, 62), T%(34), Q$(500), L%(4)
   DIM item$(9), VV%(500), p$(2)
   REDIM VV$(500), ars(15, 62, 15)
1 CLS : CH = 0: TMR = 945: TMR1 = 1875: NB = -1: NP = -1
   OPEN "DEV.BAS" FOR INPUT AS #1: FOR I = 0 TO 1: INPUT #1, p$(I): NEXT: CLOSE #1
   COLOR , 1: LOCATE 1, 1: PRINT TAB(80); : LOCATE , 22: COLOR 14
   PRINT "COURTSIDE COLLEGE BASKETBALL: CAREER STATS"
   LOCATE 24, 1: PRINT TAB(80); : LOCATE , 1
   PRINT "(C) LANCE HAFFNER/SHANNON LYNN 1991"; TAB(70); LEFT$(TIME$, 5);
   LOCATE 4, 1: COLOR 1, 0

   item$(1) = "CREATE AND MERGE INITIAL STAT FILE"
   item$(2) = "MERGE / UPDATE CAREER FILES       "
   item$(3) = "VIEW CAREER FILE                  "
   item$(4) = "PRINT CAREER FILE                 "
   item$(5) = "VIEW / PRINT CAREER RECORDS       "
   item$(6) = "EXIT TO MAIN MENU                 "

     SELECTION% = 1
     Count% = 6
     Start% = 1
     SCANCODE% = 6
     FileColr% = 31
     HiLiteColr% = 112
     Spacing% = 1
     Row% = 8
     Column% = 25
     CALL box0(7, 24, 14, 59, 2, 31)
     CALL ShowCursor
     MMenuVert VARPTR(item$(1)), SELECTION%, Start%, SCANCODE%, FileColr%, HiLiteColr%, Count%, Row%, Column%
     CALL HideCursor
     COLOR , 0
     I = SELECTION%
     IF SCANCODE% = 27 THEN I = 6
     IF I = 1 THEN 1900
     IF I = 2 THEN 2000
     IF I = 3 THEN RUN "CARVIEW.EXE"
     IF I = 4 THEN RUN "CARVIEW.EXE"
     IF I = 5 THEN 3000
     IF GetDrive% >= 67 THEN RUN "HELLO.EXE"
     CLS
     PRINT "MAKE SURE PROGRAM DISK IS IN DRIVE"
5    I$ = INKEY$: IF I$ = "" THEN 5
     RUN "HELLO.EXE"

1900 CLS : COLOR 14: LOCATE 5, 27: PRINT " - - - C A U T I O N - - -"
     COLOR 15: LOCATE 8, 18: PRINT "THIS OPTION SHOULD ONLY BE USED WHEN CREATING"
     LOCATE , 18: PRINT "AND MERGING THE INITIAL STAT FILE.  IF YOU ARE"
     LOCATE , 18: PRINT "ADDING AN ADDITIONAL YEAR TO AN EXISTING FILE,"
     LOCATE , 18: PRINT "            USE OPTION #2."
     PRINT : PRINT : PRINT
     COLOR 12: LOCATE , 25: PRINT "DO YOU WISH TO CONTINUE? (Y/N)"
1907 I$ = INKEY$: IF I$ = "" THEN 1907
     IF UCASE$(I$) = "N" THEN 1
   CLS
45 CLS : FOR J = 15 TO 18: LOCATE J: PRINT "                                                                    "
   NEXT
   COLOR 11: LOCATE 15, 5: INPUT "ENTER NAME OF CAREER STAT FILE (MAX. - 8 CHARACTERS)"; Z$
   GOSUB 3056
   B$ = A$: GOSUB 6585
   Z1$ = MID$(B$, 4, QQ) + "." + LEFT$(B$, 2)
   CLS : LOCATE 8, 25: INPUT "ENTER YEAR TO ADD (EX. - 1961)"; Y
   LOCATE 9, 25: PRINT "ANY CHANGE (YN)"
60 I$ = INKEY$: IF I$ = "" THEN 60
   IF UCASE$(I$) = "Y" THEN 45
   VV = 1: LOCATE , 5: PRINT "IS THIS THE FINAL UPDATE FOR THE CURRENT SEASON? (Y/N)"
76 I$ = INKEY$: IF I$ = "" THEN 76
   IF UCASE$(I$) = "Y" THEN JX = 1
   IF UCASE$(I$) = "N" THEN JX = 0
    GOSUB 1000
    IF NOT EXIST%(p$(1) + Z1$) THEN 900
    OPEN p$(1) + Z1$ FOR INPUT AS #1
    INPUT #1, B$
    FOR I = 0 TO 13: INPUT #1, A$(I)
    FOR J = 0 TO 13: INPUT #1, A(I, J): NEXT: INPUT #1, W0(I)
    INPUT #1, W1(I): LOCATE 21, 38: COLOR 5: TMR = TMR - 1
    PRINT USING "####"; TMR
    NEXT
    FOR L = 0 TO 15: INPUT #1, Z(L): NEXT
    FOR L = 0 TO 15: INPUT #1, Z1(L): NEXT
    CLOSE #1
    FOR I = 0 TO 61
    N = 1
    IF A$(I) = "END" OR A$(I) = "XXX" OR A$(I) = "" THEN 404
    NB = NB + 1: GOTO 408
404 A$(I) = ""
408 FOR J = 0 TO 13: ars(N, I, J) = A(I, J)
    NEXT J
    ars(N, I, 14) = 1: ars(N, I, 15) = Y
    W0S(N, I) = W0(I): W1S(N, I) = W1(I)
    NEXT I
    FOR I = 0 TO 61: AR$(I) = A$(I): NEXT
    IF JX = 1 THEN 704
    IF JX = 0 THEN 705
704 GOSUB 2080
705 GOSUB 1000
    LOCATE 20, 20: COLOR 15: PRINT "MERGING CAREER STATS . . . . PLEASE WAIT"
    OPEN p$(1) + Z$ + ".CAR" FOR OUTPUT AS #2
    WRITE #2, NB, B$
    FOR N = 1 TO 15
    FOR I = 0 TO 61: WRITE #2, AR$(I)
    FOR J = 0 TO 15: WRITE #2, ars(N, I, J): NEXT J
    WRITE #2, W0S(N, I): WRITE #2, W1S(N, I)
    LOCATE 21, 38: COLOR 5: TMR = TMR - 1: PRINT USING "####"; TMR
    NEXT I
    NEXT N
    CLOSE #2
    GOTO 1

900 CLS : COLOR 7: PRINT "UNABLE TO LOCATE STAT FILE . . . PLEASE RE-ENTER"
    PRINT : LINE INPUT "HIT ENTER TO CONTINUE"; DUMMY$
    GOTO 1

1000 IF LEFT$(p$(1), 1) <> "A" AND LEFT$(p$(1), 1) <> "B" THEN 1002
     LOCATE 20: COLOR 3: PRINT "MAKE SURE SEASON STATS ARE IN DRIVE ";
     COLOR 14: PRINT p$(1)
1001 I$ = INKEY$: IF I$ = "" THEN 1001
1002 FOR J = 20 TO 22
     LOCATE J
     PRINT "                                                                     "
     NEXT
     RETURN

1150 Spec$ = p$(1) + "*.CAR"
    Count% = FCount%(Spec$)
   IF Count% = 0 THEN 4500
   REDIM F$(0 TO Count%)
   FOR X = 1 TO Count%
       F$(X) = SPACE$(12)
   NEXT
   F$(0) = Spec$
   CALL ReadFile(VARPTR(F$(0)))
   CALL SortStr(VARPTR(F$(1)), Count%, 0)

1311 CLS
    COLOR , 1
    CLS
    LOCATE , 28
    COLOR 14
    PRINT "SELECT FILE TO UPDATE": COLOR 15
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
    PRINT "-SELECTS TEAM LIBRARY";
CALL ShowCursor
20 MAMenu VARPTR(F$(1)), SELECTION%, Start%, Count%, EXITCODE%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%
CALL HideCursor
     IF EXITCODE% = 27 THEN 1
     IF EXITCODE% = -71 THEN Start% = 1: EXITCODE% = 3: SELECTION% = 1: GOTO 20
     IF EXITCODE% = -79 THEN Start% = Count%: EXITCODE% = 3: SELECTION% = Count%: GOTO 20
     IF EXITCODE% = -73 THEN Start% = Start% - 80: IF Start% < 1 THEN Start% = 1
     IF EXITCODE% = -81 THEN Start% = Start% + 80: IF Start% > Count% THEN Start% = Count%
     IF EXITCODE% = -81 OR EXITCODE% = -73 THEN EXITCODE% = 3: SELECTION% = Start%: GOTO 20
     F$ = F$(SELECTION%)
   LOCATE 2, 33: PRINT F$: LOCATE , 33: PRINT "ANY CHANGE (YN)";
     COLOR 15, 0
3910 I$ = INKEY$: IF I$ = "" THEN 3910
    IF UCASE$(I$) = "Y" THEN 1311
     F$ = QPRTrim$(F$)
    Z$ = LEFT$(F$, LEN(F$) - 4)
     GOSUB 3056
     B$ = A$: GOSUB 6585
     Z1$ = MID$(B$, 4, QQ) + "." + LEFT$(B$, 2)
     CLS : LOCATE 8, 25: INPUT "ENTER YEAR TO ADD (EX. - 1961)"; Y
   LOCATE 9, 25: PRINT "ANY CHANGE (YN)"
160 I$ = INKEY$: IF I$ = "" THEN 160
   IF UCASE$(I$) = "Y" THEN 1
   VV = 0: LOCATE , 5: PRINT "IS THIS THE FINAL UPDATE FOR THE CURRENT SEASON? (Y/N)"
176 I$ = INKEY$: IF I$ = "" THEN 176
   IF UCASE$(I$) = "Y" THEN JX = 1
   IF UCASE$(I$) = "N" THEN JX = 0
    GOSUB 1000

     LOCATE 20, 5: COLOR 15: PRINT "MERGING CAREER STATS AND UPDATING CAREER LEADER FILE . . . . PLEASE WAIT"
     OPEN p$(1) + Z$ + ".CAR" FOR INPUT AS #2
     INPUT #2, NB, B$
     FOR N = 1 TO 15
     FOR I = 0 TO 61: INPUT #2, AR$(I)
     FOR J = 0 TO 15: INPUT #2, ars(N, I, J): NEXT J: INPUT #2, W0S(N, I)
     INPUT #2, W1S(N, I): LOCATE 21, 38: COLOR 5: TMR1 = TMR1 - 1
     PRINT USING "####"; TMR1: NEXT I: NEXT N
     CLOSE #2
1266 GOSUB 1000
     LOCATE 20, 20: COLOR 15: PRINT "MERGING CAREER STATS . . . . PLEASE WAIT"
     IF NOT EXIST%(p$(1) + Z1$) THEN 900
     OPEN p$(1) + Z1$ FOR INPUT AS #1
     INPUT #1, B$
     FOR I = 0 TO 13: INPUT #1, A$(I)
     FOR J = 0 TO 13: INPUT #1, A(I, J): NEXT: INPUT #1, W0(I): INPUT #1, W1(I): LOCATE 21, 38: COLOR 5: TMR1 = TMR1 - 1: PRINT USING "####"; TMR1: NEXT
     FOR L = 0 TO 15: INPUT #1, Z(L): NEXT
     FOR L = 0 TO 15: INPUT #1, Z1(L): NEXT
     CLOSE #1
     FOR I = 0 TO 13
     RR = 0: CHK = 0
     FOR J = 0 TO 61
     IF AR$(J) <> A$(I) THEN 1424
1411 FOR W = 1 TO 15
     IF ars(W, J, 15) <> Y THEN 1413
1412 CHK = CHK + 1
1413 NEXT W
     IF CHK <= 0 THEN 1419
1418 N = ars(1, J, 14) + ars(2, J, 14) + ars(3, J, 14) + ars(4, J, 14)
     N = N + ars(5, J, 14) + ars(6, J, 14) + ars(7, J, 14) + ars(8, J, 14)
     N = N + ars(9, J, 14) + ars(10, J, 14) + ars(11, J, 14)
     N = N + ars(12, J, 14) + ars(13, J, 14) + ars(14, J, 14) + ars(15, J, 14)
     GOTO 1420
1419 N = ars(1, J, 14) + ars(2, J, 14) + ars(3, J, 14) + ars(4, J, 14)
     N = N + ars(5, J, 14) + ars(6, J, 14) + ars(7, J, 14) + ars(8, J, 14)
     N = N + ars(9, J, 14) + ars(10, J, 14) + ars(11, J, 14)
     N = N + ars(12, J, 14) + ars(13, J, 14) + ars(14, J, 14) + ars(15, J, 14) + 1
1420 FOR K = 0 TO 13
     ars(N, J, K) = A(I, K)
     NEXT K
     ars(N, J, 14) = 1: ars(N, J, 15) = Y
     W0S(N, J) = W0(J): W1S(N, J) = W1(J)
     GOTO 1430
1424 RR = RR + 1
     IF RR = 62 THEN GOSUB 1450
     NEXT J
1430 NEXT I
     GOTO 1700

1450 IF A$(I) = "END" OR A$(I) = "XXX" OR A$(I) = "" THEN 1457 ELSE 1451
1451 NB = NB + 1
     AR$(NB) = A$(I)
     N = 1
     FOR K = 0 TO 13: ars(N, NB, K) = A(I, K): NEXT
     ars(N, NB, 14) = 1: ars(N, NB, 15) = Y
     W0S(N, NB) = W0(NB): W1S(N, NB) = W1(NB)
1457 RETURN
1700 IF JX = 1 THEN 1704
     IF JX = 0 THEN 1705
1704 GOSUB 2080
1705 GOSUB 1000
     LOCATE 20, 20: COLOR 15: PRINT "MERGING CAREER STATS . . . . PLEASE WAIT"
     OPEN p$(1) + Z$ + ".CAR" FOR OUTPUT AS #2
     WRITE #2, NB, B$
     FOR N = 1 TO 15
     FOR I = 0 TO 61: WRITE #2, AR$(I)
     FOR J = 0 TO 15: WRITE #2, ars(N, I, J): NEXT J: WRITE #2, W0S(N, I): WRITE #2, W1S(N, I): LOCATE 21, 38: COLOR 5: TMR1 = TMR1 - 1: PRINT USING "####"; TMR1: NEXT I: NEXT N
     CLOSE #2
     GOTO 1

2000 COLOR 14, 0: CLS : LOCATE 5, 27: PRINT "- - - C A U T I O N - - -"
     COLOR 15: LOCATE 8, 1: PRINT "THIS OPTION SHOULD ONLY BE USED WHEN ADDING YEARS TO AN EXISTING STAT"
     PRINT "FILE. IF YOU ARE CREATING A NEW CAREER FILE, USE THE CREATE OPTION.": PRINT
PRINT "IF YOU ARE ADDING AN ADDITIONAL CAREER YEAR, MAKE SURE THAT ANY UNUSED PLAYER"
PRINT "SLOTS (XXX, END, ETC.) ARE AT THE END OF THE SEASON STAT LIST BEFORE YOU MERGE."
     PRINT : PRINT "ALSO MAKE SURE THAT THE PLAYERS NAMES IN THE FILE YOU ARE ADDING, MATCH"
     PRINT "THE EXACT SPELLING OF THE NAME OF THE SAME PLAYER IN THE CAREER FILE FOR THE"
     PRINT "PLAYERS THAT HAD PLAYED IN PREVIOUS YEARS.  EXAMPLE - IF BILLY WILLIAMS IS"
     PRINT "LISTED AS 'WILLIAMS' IN THE CAREER FILE AND THE SEASON STATS HAD HIM AS"
     PRINT "'B.WILLIAMS', HE SHOULD BE CHANGED TO 'WILLIAMS' IN THE SEASON FILE BEFORE"
     PRINT "MERGING."
     PRINT : PRINT
     COLOR 12: LOCATE , 25: PRINT "DO YOU WISH TO CONTINUE? (Y/N)"
2018 I$ = INKEY$: IF I$ = "" THEN 2018
     IF UCASE$(I$) = "N" THEN 1
     GOTO 1150

2080 IF NOT EXIST%(p$(1) + Z$ + ".cld") THEN 2100
     OPEN p$(1) + Z$ + ".CLD" FOR INPUT AS #1
     FOR I = 0 TO 20
     INPUT #1, BLYR(I), BL$(I), BL(I): NEXT
     CLOSE #1
2100 FOR I = 0 TO 61
     IF AR$(I) = "" OR AR$(I) = "XXX" OR AR$(I) = "END" THEN 2360
     FOR J = 1 TO 15
     IF ars(J, I, 15) = 0 THEN 2350
     IF ars(J, I, 1) <= BL(0) THEN 2113
     BL(0) = ars(J, I, 1): BL$(0) = AR$(I): BLYR(0) = ars(J, I, 15)
2113 IF ars(J, I, 2) <= BL(1) THEN 2123
     BL(1) = ars(J, I, 2): BL$(1) = AR$(I): BLYR(1) = ars(J, I, 15)
2123 IF ars(J, I, 1) = 0 OR ars(J, I, 1) / ars(J, I, 0) < 3.65 THEN 2133
     IF ars(J, I, 2) / ars(J, I, 1) <= BL(2) THEN 2133
     BL(2) = ars(J, I, 2) / ars(J, I, 1): BL$(2) = AR$(I): BLYR(2) = ars(J, I, 15)
2133 IF ars(J, I, 4) <= BL(3) THEN 2143
     BL(3) = ars(J, I, 4): BL$(3) = AR$(I): BLYR(3) = ars(J, I, 15)
2143 IF ars(J, I, 3) <= BL(4) THEN 2153
     BL(4) = ars(J, I, 3): BL$(4) = AR$(I): BLYR(4) = ars(J, I, 15)
2153 IF ars(J, I, 3) = 0 OR ars(J, I, 3) / ars(J, I, 0) < 1.5 THEN 2163
     IF ars(J, I, 4) / ars(J, I, 3) <= BL(5) THEN 2163
     BL(5) = ars(J, I, 4) / ars(J, I, 3): BL$(5) = AR$(I): BLYR(5) = ars(J, I, 15)
2163 IF W0S(J, I) <= BL(6) THEN 2173
2164 BL(6) = W0S(J, I): BL$(6) = AR$(I): BLYR(6) = ars(J, I, 15)
2173 IF W1S(J, I) <= BL(7) THEN 2182
     BL(7) = W1S(J, I): BL$(7) = AR$(I): BLYR(7) = ars(J, I, 15)
2182 IF W1S(J, I) = 0 OR W1S(J, I) / ars(J, I, 0) < .3 THEN 2193
     IF W0S(J, I) / W1S(J, I) <= BL(8) THEN 2193
     BL(8) = W0S(J, I) / W1S(J, I): BL$(8) = AR$(I): BLYR(8) = ars(J, I, 15)
2193 IF ars(J, I, 2) * 2 + ars(J, I, 4) + W0S(J, I) <= BL(9) THEN 2203
     BL(9) = ars(J, I, 2) * 2 + ars(J, I, 4) + W0S(J, I): BL$(9) = AR$(I): BLYR(9) = ars(J, I, 15)
2203 IF ars(J, I, 0) = 0 THEN 2213
     IF (ars(J, I, 2) * 2 + ars(J, I, 4) + W0S(J, I)) / ars(J, I, 0) <= BL(10) THEN 2213
     BL(10) = (ars(J, I, 2) * 2 + ars(J, I, 4) + W0S(J, I)) / ars(J, I, 0): BL$(10) = AR$(I): BLYR(10) = ars(J, I, 15)
2213 IF ars(J, I, 12) <= BL(11) THEN 2223
     BL(11) = ars(J, I, 12): BL$(11) = AR$(I): BLYR(11) = ars(J, I, 15)
2223 IF ars(J, I, 5) - ars(J, I, 12) <= BL(12) THEN 2233
     BL(12) = ars(J, I, 5) - ars(J, I, 12): BL$(12) = AR$(I): BLYR(12) = ars(J, I, 15)
2233 IF ars(J, I, 5) <= BL(13) THEN 2243
     BL(13) = ars(J, I, 5): BL$(13) = AR$(I): BLYR(13) = ars(J, I, 15)
2243 IF ars(J, I, 0) = 0 THEN 2253
     IF ars(J, I, 5) / ars(J, I, 0) <= BL(14) THEN 2253
     BL(14) = ars(J, I, 5) / ars(J, I, 0): BL$(14) = AR$(I): BLYR(14) = ars(J, I, 15)
2253 IF ars(J, I, 6) <= BL(15) THEN 2263
     BL(15) = ars(J, I, 6): BL$(15) = AR$(I): BLYR(15) = ars(J, I, 15)
2263 IF ars(J, I, 8) <= BL(16) THEN 2273
     BL(16) = ars(J, I, 8): BL$(16) = AR$(I): BLYR(16) = ars(J, I, 15)
2273 IF ars(J, I, 9) <= BL(17) THEN 2283
     BL(17) = ars(J, I, 9): BL$(17) = AR$(I): BLYR(17) = ars(J, I, 15)
2283 IF ars(J, I, 7) <= BL(18) THEN 2293
     BL(18) = ars(J, I, 7): BL$(18) = AR$(I): BLYR(18) = ars(J, I, 15)
2293 IF ars(J, I, 11) <= BL(19) THEN 2303
     BL(19) = ars(J, I, 11): BL$(19) = AR$(I): BLYR(19) = ars(J, I, 15)
2303 IF ars(J, I, 13) <= BL(20) THEN 2350
     BL(20) = ars(J, I, 13): BL$(20) = AR$(I): BLYR(20) = ars(J, I, 15)
2350 NEXT J
2360 NEXT I
     IF VV = 1 THEN 2805
     IF VV = 0 THEN 2810
2805 OPEN p$(1) + Z$ + ".CLD" FOR OUTPUT AS #1: GOTO 2811
2810 OPEN p$(1) + Z$ + ".CLD" FOR OUTPUT AS #1
2811 FOR I = 0 TO 20: WRITE #1, BLYR(I), BL$(I), BL(I): NEXT
     CLOSE #1
     RETURN
3000 Spec$ = p$(1) + "*.CLD"
    Count% = FCount%(Spec$)
   IF Count% = 0 THEN 4500
   REDIM F$(0 TO Count%)
   FOR X = 1 TO Count%
       F$(X) = SPACE$(12)
   NEXT
   F$(0) = Spec$
   CALL ReadFile(VARPTR(F$(0)))
   CALL SortStr(VARPTR(F$(1)), Count%, 0)

1310 CLS
    COLOR , 1
    CLS
    LOCATE , 28
    COLOR 14
    PRINT "SELECT FILE TO VIEW": COLOR 15
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
    PRINT "-SELECTS FILE";
CALL ShowCursor
202 MAMenu VARPTR(F$(1)), SELECTION%, Start%, Count%, EXITCODE%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%
CALL HideCursor
     IF EXITCODE% = 27 THEN 1
     IF EXITCODE% = -71 THEN Start% = 1: EXITCODE% = 3: SELECTION% = 1: GOTO 202
     IF EXITCODE% = -79 THEN Start% = Count%: EXITCODE% = 3: SELECTION% = Count%: GOTO 202
     IF EXITCODE% = -73 THEN Start% = Start% - 80: IF Start% < 1 THEN Start% = 1
     IF EXITCODE% = -81 THEN Start% = Start% + 80: IF Start% > Count% THEN Start% = Count%
     IF EXITCODE% = -81 OR EXITCODE% = -73 THEN EXITCODE% = 3: SELECTION% = Start%: GOTO 202
     F$ = F$(SELECTION%)
   LOCATE 2, 33: PRINT F$: LOCATE , 33: PRINT "ANY CHANGE (YN)";
     COLOR 15, 0
3912 I$ = INKEY$: IF I$ = "" THEN 3912
    IF UCASE$(I$) = "Y" THEN 1310
     F$ = QPRTrim$(F$)
    ZZ$ = LEFT$(F$, LEN(F$) - 4)
     OPEN p$(1) + ZZ$ + ".CLD" FOR INPUT AS #1
     FOR I = 0 TO 20: INPUT #1, BLYR(I), BL$(I), BL(I): NEXT
     CLOSE #1
     LOCATE 15, 10: COLOR 12: PRINT "V"; : COLOR 7: PRINT "iew or "; : COLOR 12: PRINT "P"; : COLOR 7: PRINT "rint"
3151 I$ = INKEY$: IF I$ = "" THEN 3151
     IF UCASE$(I$) = "V" THEN 3200
     IF UCASE$(I$) = "P" THEN 3500
     IF UCASE$(I$) <> "V" AND UCASE$(I$) <> "P" THEN 3151
3200 CLS
     COLOR 12: PRINT "CATEGORY                     RECORD           PLAYER              YEAR"
     PRINT
     COLOR 3: PRINT "FIELD GOALS MADE"; : COLOR 7: PRINT TAB(33); USING "####"; BL(0); : PRINT TAB(47); BL$(0); : PRINT TAB(66); BLYR(0)
     COLOR 3: PRINT "FIELD GOAL ATTEMPTS"; : COLOR 7: PRINT TAB(33); USING "####"; BL(1); : PRINT TAB(47); BL$(1); : PRINT TAB(66); BLYR(1)
     COLOR 3: PRINT "FIELD GOAL PCT."; : COLOR 7: PRINT TAB(32); USING "#.###"; BL(2); : PRINT TAB(47); BL$(2); : PRINT TAB(66); BLYR(2)
     COLOR 3: PRINT "FREE THROWS MADE"; : COLOR 7: PRINT TAB(33); USING "####"; BL(3); : PRINT TAB(47); BL$(3); : PRINT TAB(66); BLYR(3)
     COLOR 3: PRINT "FREE THROW ATTEMPTS"; : COLOR 7: PRINT TAB(33); USING "####"; BL(4); : PRINT TAB(47); BL$(4); : PRINT TAB(66); BLYR(4)
     COLOR 3: PRINT "FREE THROW PCT."; : COLOR 7: PRINT TAB(32); USING "#.###"; BL(5); : PRINT TAB(47); BL$(5); : PRINT TAB(66); BLYR(5)
     COLOR 3: PRINT "3 PT. FIELD GOALS MADE"; : COLOR 7: PRINT TAB(33); USING "####"; BL(6); : PRINT TAB(47); BL$(6); : PRINT TAB(66); BLYR(6)
     COLOR 3: PRINT "3 PT. FILED GOAL ATTEMPTS"; : COLOR 7: PRINT TAB(33); USING "####"; BL(7); : PRINT TAB(47); BL$(7); : PRINT TAB(66); BLYR(7)
     COLOR 3: PRINT "3 PT. FIELD GOAL PCT."; : COLOR 7: PRINT TAB(32); USING "#.###"; BL(8); : PRINT TAB(47); BL$(8); : PRINT TAB(66); BLYR(8)
     COLOR 3: PRINT "POINTS"; : COLOR 7: PRINT TAB(32); USING "#####"; BL(9); : PRINT TAB(47); BL$(9); : PRINT TAB(66); BLYR(9)
     COLOR 3: PRINT "POINTS PER GAME"; : COLOR 7: PRINT TAB(32); USING "###.#"; BL(10); : PRINT TAB(47); BL$(10); : PRINT TAB(66); BLYR(10)
     COLOR 3: PRINT "OFFENSIVE REBOUNDS"; : COLOR 7: PRINT TAB(32); USING "#####"; BL(11); : PRINT TAB(47); BL$(11); : PRINT TAB(66); BLYR(11)
     COLOR 3: PRINT "DEFENSIVE REBOUNDS"; : COLOR 7: PRINT TAB(32); USING "#####"; BL(12); : PRINT TAB(47); BL$(12); : PRINT TAB(66); BLYR(12)
     COLOR 3: PRINT "TOTAL REBOUNDS"; : COLOR 7: PRINT TAB(32); USING "#####"; BL(13); : PRINT TAB(47); BL$(13); : PRINT TAB(66); BLYR(13)
     COLOR 3: PRINT "REBOUNDS PER GAME"; : COLOR 7: PRINT TAB(32); USING "###.#"; BL(14); : PRINT TAB(47); BL$(14); : PRINT TAB(66); BLYR(14)
     COLOR 15: LOCATE 22: PRINT "HIT ANY KEY TO CONTINUE"
3271 I$ = INKEY$: IF I$ = "" THEN 3271
     CLS
     COLOR 12: PRINT "CATEGORY                     RECORD           PLAYER              YEAR"
     PRINT
     COLOR 3: PRINT "PERSONAL FOULS"; : COLOR 7: PRINT TAB(33); USING "####"; BL(15); : PRINT TAB(47); BL$(15); : PRINT TAB(66); BLYR(15)
     COLOR 3: PRINT "ASSISTS"; : COLOR 7: PRINT TAB(32); USING "#####"; BL(16); : PRINT TAB(47); BL$(16); : PRINT TAB(66); BLYR(16)
     COLOR 3: PRINT "SHOTS BLOCKED"; : COLOR 7: PRINT TAB(33); USING "####"; BL(17); : PRINT TAB(47); BL$(17); : PRINT TAB(66); BLYR(17)
     COLOR 3: PRINT "STEALS"; : COLOR 7: PRINT TAB(32); USING "#####"; BL(18); : PRINT TAB(47); BL$(18); : PRINT TAB(66); BLYR(18)
     COLOR 3: PRINT "TURNOVERS"; : COLOR 7: PRINT TAB(32); USING "#####"; BL(19); : PRINT TAB(47); BL$(19); : PRINT TAB(66); BLYR(19)
     COLOR 3: PRINT "HI-GAME (POINTS)"; : COLOR 7: PRINT TAB(33); USING "####"; BL(20); : PRINT TAB(47); BL$(20); : PRINT TAB(66); BLYR(20)
     COLOR 15: LOCATE 22: PRINT "VIEW AGAIN (Y/N)"
3391 I$ = INKEY$: IF I$ = "" THEN 3391
     IF UCASE$(I$) = "Y" THEN 3200
     GOTO 1
3500 LPRINT "CATEGORY                     RECORD           PLAYER              YEAR"
     LPRINT
     LPRINT "FIELD GOALS MADE"; : LPRINT TAB(33); USING "####"; BL(0); : LPRINT TAB(47); BL$(0); : LPRINT TAB(66); BLYR(0)
     LPRINT "FIELD GOAL ATTEMPTS"; : LPRINT TAB(33); USING "####"; BL(1); : LPRINT TAB(47); BL$(1); : LPRINT TAB(66); BLYR(1)
     LPRINT "FIELD GOAL PCT."; : LPRINT TAB(32); USING "#.###"; BL(2); : LPRINT TAB(47); BL$(2); : LPRINT TAB(66); BLYR(2)
     LPRINT "FREE THROWS MADE"; : LPRINT TAB(33); USING "####"; BL(3); : LPRINT TAB(47); BL$(3); : LPRINT TAB(66); BLYR(3)
     LPRINT "FREE THROW ATTEMPTS"; : LPRINT TAB(33); USING "####"; BL(4); : LPRINT TAB(47); BL$(4); : LPRINT TAB(66); BLYR(4)
     LPRINT "FREE THROW PCT."; : LPRINT TAB(32); USING "#.###"; BL(5); : LPRINT TAB(47); BL$(5); : LPRINT TAB(66); BLYR(5)
     LPRINT "3 PT. FIELD GOALS MADE"; : LPRINT TAB(33); USING "####"; BL(6); : LPRINT TAB(47); BL$(6); : LPRINT TAB(66); BLYR(6)
     LPRINT "3 PT. FILED GOAL ATTEMPTS"; : LPRINT TAB(33); USING "####"; BL(7); : LPRINT TAB(47); BL$(7); : LPRINT TAB(66); BLYR(7)
     LPRINT "3 PT. FIELD GOAL PCT."; : LPRINT TAB(32); USING "#.###"; BL(8); : LPRINT TAB(47); BL$(8); : LPRINT TAB(66); BLYR(8)
     LPRINT "POINTS"; : LPRINT TAB(32); USING "#####"; BL(9); : LPRINT TAB(47); BL$(9); : LPRINT TAB(66); BLYR(9)
     LPRINT "POINTS PER GAME"; : LPRINT TAB(32); USING "###.#"; BL(10); : LPRINT TAB(47); BL$(10); : LPRINT TAB(66); BLYR(10)
     LPRINT "OFFENSIVE REBOUNDS"; : LPRINT TAB(32); USING "#####"; BL(11); : LPRINT TAB(47); BL$(11); : LPRINT TAB(66); BLYR(11)
     LPRINT "DEFENSIVE REBOUNDS"; : LPRINT TAB(32); USING "#####"; BL(12); : LPRINT TAB(47); BL$(12); : LPRINT TAB(66); BLYR(12)
     LPRINT "TOTAL REBOUNDS"; : LPRINT TAB(32); USING "#####"; BL(13); : LPRINT TAB(47); BL$(13); : LPRINT TAB(66); BLYR(13)
     LPRINT "REBOUNDS PER GAME"; : LPRINT TAB(32); USING "###.#"; BL(14); : LPRINT TAB(47); BL$(14); : LPRINT TAB(66); BLYR(14)
     LPRINT "PERSONAL FOULS"; : LPRINT TAB(33); USING "####"; BL(15); : LPRINT TAB(47); BL$(15); : LPRINT TAB(66); BLYR(15)
     LPRINT "ASSISTS"; : LPRINT TAB(32); USING "#####"; BL(16); : LPRINT TAB(47); BL$(16); : LPRINT TAB(66); BLYR(16)
     LPRINT "SHOTS BLOCKED"; : LPRINT TAB(33); USING "####"; BL(17); : LPRINT TAB(47); BL$(17); : LPRINT TAB(66); BLYR(17)
     LPRINT "STEALS"; : LPRINT TAB(32); USING "#####"; BL(18); : LPRINT TAB(47); BL$(18); : LPRINT TAB(66); BLYR(18)
     LPRINT "TURNOVERS"; : LPRINT TAB(32); USING "#####"; BL(19); : LPRINT TAB(47); BL$(19); : LPRINT TAB(66); BLYR(19)
     LPRINT "HI-GAME (POINTS)"; : LPRINT TAB(33); USING "####"; BL(20); : LPRINT TAB(47); BL$(20); : LPRINT TAB(66); BLYR(20)
     LPRINT CHR$(12)
     GOTO 1


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
   IF I$ = CHR$(27) THEN 1
   IF I$ = CHR$(13) THEN GOSUB 1102: GOTO 3060
   LOCATE 7, 27: PRINT I$
   LOCATE 7, 28: LINE INPUT NY$
   NY$ = I$ + NY$
   COLOR 15, 0
   GOSUB 4105
   IF NY$ = "" THEN GOSUB 1102: GOTO 3060
   YN$ = NY$
   FOR I = 1 TO 500: VV$(I) = "": NEXT
   GOSUB 1100
3060 IF BO% = 1 THEN BO% = 0: GOTO 3056
3058 LOCATE 2, 33: COLOR T%(0), T%(1): PRINT A$
     COLOR 15, 4: LOCATE , 33: PRINT "ANY CHANGE (YN)";
     I$ = INKEY$: IF I$ = "" THEN 3058
   IF UCASE$(I$) = "Y" THEN 3056
   COLOR 15, 0
     RETURN
1100 OPEN p$(0) + "BASK." + YN$ FOR RANDOM AS #1 LEN = 655
     Y1 = LOF(1)
     FIELD #1, 15 AS Q$(0), 640 AS X$
     FOR I1 = 1 TO Y1 / 655
     GET #1, I1
     VV$(I1) = Q$(0): VV%(I1) = I1
    NEXT
    CLOSE 1
    NTMS = Y1 / 655
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
     CALL ShowCursor
120   MAMenu VARPTR(VV$(1)), SELECTION%, Start%, Count%, EXITCODE%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%
     CALL HideCursor
     IF EXITCODE% = 27 THEN BO% = 1: RETURN
     IF EXITCODE% = -71 THEN Start% = 1: EXITCODE% = 3: SELECTION% = 1: GOTO 120
     IF EXITCODE% = -79 THEN Start% = NTMS: EXITCODE% = 3: SELECTION% = NTMS: GOTO 120
     IF EXITCODE% = -73 THEN Start% = Start% - 80: IF Start% < 1 THEN Start% = 1
     IF EXITCODE% = -81 THEN Start% = Start% + 80: IF Start% > NTMS THEN Start% = NTMS
     IF EXITCODE% = -81 OR EXITCODE% = -73 THEN EXITCODE% = 3: SELECTION% = Start%: GOTO 120
     T1 = VV%(SELECTION%)
9000 OPEN p$(0) + "BASK." + YN$ FOR RANDOM AS #1 LEN = 655
FIELD #1, 15 AS Q$(0), 2 AS Q$(1), 2 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 2 AS Q$(14), 2 AS Q$(15), 2 AS Q$(16), 2 AS Q$(17), 2 AS Q$(18), 2 _
 AS Q$(19), 602 AS X$
FOR I = 0 TO 13
FIELD #1, 53 + I * 43 AS X$, 15 AS Q$(I * 15 + 20), 2 AS Q$(I * 15 + 21), 2 AS Q$(I * 15 + 22), 2 AS Q$(I * 15 + 23), 2 AS Q$(I * 15 + 24), 2 AS Q$(I * 15 + 25), 2 AS Q$(I * 15 + 26), 2 AS Q$(I * 15 + 27), 2 AS Q$(I * 15 + 28), 2 AS Q$(I * 15 + 29) _
, 2 AS Q$(I * 15 + 30), 2 AS Q$(I * 15 + 31), 2 AS Q$(I * 15 + 32), 2 AS Q$(I * 15 + 33), 2 AS Q$(I * 15 + 34), 655 - 53 - 43 - I * 43 AS X$
NEXT
GET #1, T1
   A$ = Q$(0)
9005 IF RIGHT$(A$, 1) = " " THEN A$ = LEFT$(A$, LEN(A$) - 1): GOTO 9005
     B$ = A$
     SP = CVI(Q$(1)): DA = CVI(Q$(2)): V1% = CVI(Q$(3)): V2% = CVI(Q$(4))
     FOR I = 0 TO 4: L%(I) = CVI(Q$(I + 5)): NEXT
     FOR I = 0 TO 9: T%(I) = CVI(Q$(I + 10)): NEXT
     CLOSE 1
     B$ = A$
     RETURN
6585 FOR XX = 4 TO 11: IF MID$(B$, XX, 1) = " " OR MID$(B$, XX, 1) = "." THEN MID$(B$, XX, 1) = "_"
     NEXT XX
     FOR XX = 12 TO 14: IF MID$(B$, XX, 2) = "ST" THEN MID$(B$, 9, 1) = "_": MID$(B$, 10, 2) = "ST"
     NEXT XX
     QQ = 8: FOR XX = 11 TO 4 STEP -1: IF MID$(B$, XX, 1) = "_" THEN QQ = XX - 4: GOTO 6615
     GOTO 6620
6615 NEXT XX
6620 RETURN

4500 GOSUB 4600
     LOCATE , 10: PRINT "NO CAREER FILES WERE FOUND"
     LOCATE , 10: PRINT "PRESS ANY KEY TO RETURN TO MAIN MENU"
3805 I$ = INKEY$: IF I$ = "" THEN 3805
     CLEAR : GOTO 1
4600 JJ = 4
     LOCATE 6, 8: COLOR 15, 1
     PRINT " "; CHR$(201); STRING$(61, 205); CHR$(187); " ";
     FOR II = 1 TO JJ: LOCATE 6 + II, 8
     PRINT " º"; STRING$(61, 32); "º "; : NEXT
     LOCATE 7 + JJ, 8
     PRINT " "; CHR$(200); STRING$(61, 205); CHR$(188); " "; : LOCATE 7
     RETURN
4105 IF UCASE$(LEFT$(p$(0), 1)) <> "A" AND UCASE$(LEFT$(p$(0), 1)) <> "B" THEN RETURN
     CLS : PRINT "MAKE SURE CORRECT TEAMS DISK IS IN DRIVE "; p$(0)
     LOCATE 23, 1: PRINT "PRESS ANY KEY TO CONTINUE"
4106 I$ = INKEY$: IF I$ = "" THEN 4106
     RETURN

