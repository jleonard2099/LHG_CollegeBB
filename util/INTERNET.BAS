1 DIM Q$(400), E0(16), XX(13), X(14, 6), T%(9), L%(9), A(14, 19), DF(13), RS(13, 13)
  DIM D$(1), QQ(3)
  OPEN "DEV.BAS" FOR INPUT AS #2: INPUT #2, D$(0): CLOSE 2
4 CLS
  IF UCASE$(LEFT$(D$(0), 1)) <> "A" AND UCASE$(LEFT$(D$(0), 1)) <> "B" THEN 8
  GOSUB 4500
  U$ = "INSERT TEAMS DISK IN DRIVE " + D$(0)
  LOCATE , (80 - LEN(U$)) / 2
  PRINT U$
  U$ = "HIT ANY KEY TO CONTINUE"
  LOCATE , (80 - LEN(U$)) / 2
  PRINT U$
  COLOR 15, 0
5 I$ = INKEY$: IF I$ = "" THEN 5
8 DATA "COLLEGE BASKETBALL","H S BASKETBALL"
9 DIM X$(3), N$(14), P$(14)
10 READ X$(0), X$(1)
11 CLS
12 COLOR 12: PRINT "SELECT TYPE OF TEAMS YOU ARE ENTERING": PRINT
   COLOR 14: PRINT "(0) "; : COLOR 15: PRINT "COLLEGE BASKETBALL": PRINT
   COLOR 14: PRINT "(1) "; : COLOR 15: PRINT "HIGH SCHOOL BASKETBALL"
13 I$ = INKEY$: IF I$ = "" THEN 13
   IF I$ = CHR$(27) THEN RUN "HELLO.EXE 2"
14 H = VAL(I$): IF H < 0 OR H > 1 THEN 11
200 CLS
203 PRINT X$(H): L7 = 6: L3 = 20: L6 = 14
    IF H = 1 THEN L7 = 5: L6 = 11: L3 = 16
300 LOCATE 3: INPUT "1. SEASON FGA/G "; L1: IF C1 = 1 THEN 307
    IF L1 = 999 THEN 11
301 LOCATE 4: INPUT "2. SEASON REBOUNDS/GAME "; L2: IF C1 = 1 THEN 307
302 LOCATE 5: INPUT "3. SEASON FG % "; L4: IF C1 = 1 THEN 307
303 LOCATE 6: INPUT "4. SEASON STEALS/GAME "; BA: IF C1 = 1 THEN 307
304 LOCATE 7: INPUT "5. SEASON TURNOVERS/GAME "; BB: IF C1 = 1 THEN 307
305 LOCATE 8: INPUT "6. SEASON 3 FGA/FGA % "; Q7: IF C1 = 1 THEN 307
306 LOCATE 9: INPUT "7. SEASON 3 FG % "; Q8: IF C1 = 1 THEN 307
    T%(2) = 999
307 C1 = 1: LOCATE 22: INPUT "# TO CHANGE (0 TO CONTINUE) "; B9
    ON B9 GOTO 300, 301, 302, 303, 304, 305, 306
250 C1 = 0: GOSUB 1600: CLS
308 LOCATE 1: PRINT X$(H)
    LOCATE 2: INPUT " 0. TEAM # (0-EXIT)"; T: IF C1 = 1 THEN 326
    IF T = 999 THEN 200
    IF T = 0 THEN RUN "HELLO.EXE 2"
309 LOCATE 3: INPUT " 1. NAME "; A$: IF C1 = 1 THEN 326
310 LOCATE 4: INPUT " 2. STRENGTH OF SCHEDULE ADJ "; BC: IF C1 = 1 THEN 326
311 LOCATE 5: INPUT " 3. GAMES "; L5: IF C1 = 1 THEN 326
157 LOCATE 6: INPUT "17. TEXT COLOR (0-15) "; T%(0): IF C1 = 1 THEN 326
158 LOCATE 7: INPUT "18. BACKGROUND (0-7) "; T%(1): LOCATE 8, 1: COLOR T%(0), T%(1): PRINT A$: COLOR 15, 0: IF C1 = 1 THEN 326
159 LOCATE 2, 37: INPUT "19. NICKNAME "; A1$: IF C1 = 1 THEN 326
160 LOCATE 3, 37: INPUT "20. WINS "; T%(7): IF C1 = 1 THEN 326
161 LOCATE 4, 37: INPUT "21. LOSSES "; T%(8): IF C1 = 1 THEN 326
162 LOCATE 5, 37: INPUT "22. POWER RATING "; T%(9): IF C1 = 1 THEN 326
163 LOCATE 6, 37: INPUT "23. COACH "; C$: IF C1 = 1 THEN 326
165 LOCATE 7, 37: INPUT "24. ARENA "; S$: IF C1 = 1 THEN 326
166 LOCATE 8, 37: INPUT "25. AVG ATT "; ATT: IF C1 = 1 THEN 326
312 LOCATE 9, 10: PRINT A$; : LOCATE 9, 30: PRINT "OPPONENTS"
313 LOCATE 10, 1: PRINT "4. FGA "; : LOCATE 10, 15: INPUT T1
    LOCATE 10, 30: INPUT T2: IF C1 = 1 THEN 326
314 LOCATE 11, 1: PRINT "5. FG % "; : LOCATE 11, 15: INPUT T6
    LOCATE 11, 30: INPUT BE: IF C1 = 1 THEN 326
315 LOCATE 12, 1: PRINT "6. FTA"; : LOCATE 12, 15: INPUT T7
    LOCATE 12, 30: INPUT BF: IF C1 = 1 THEN 326
316 LOCATE 13, 1: PRINT "7. FT %"; : LOCATE 13, 15: INPUT T8
    LOCATE 13, 30: INPUT BG: IF C1 = 1 THEN 326
317 LOCATE 14, 1: PRINT "8. RB"; : LOCATE 14, 15: INPUT T3
    LOCATE 14, 30: INPUT T4: IF C1 = 1 THEN 326
318 LOCATE 15, 1: PRINT "9. AST"; : LOCATE 15, 15: INPUT T0
    LOCATE 15, 30: INPUT XX: IF C1 = 1 THEN 326
319 LOCATE 16, 1: PRINT "10. FOULS"; : LOCATE 16, 15: INPUT L3
    LOCATE 16, 30: INPUT BI: IF C1 = 1 THEN 326
320 LOCATE 17, 1: PRINT "11. TO"; : LOCATE 17, 15: INPUT BJ
    LOCATE 17, 30: INPUT BK: IF C1 = 1 THEN 326
321 LOCATE 18, 1: PRINT "12. STLS"; : LOCATE 18, 15: INPUT T9
    LOCATE 18, 30: INPUT BH: IF C1 = 1 THEN 326
322 LOCATE 19, 1: PRINT "13. 3 FGA "; : LOCATE 19, 15: PRINT "---";
    LOCATE 19, 30: INPUT W0: IF C1 = 1 THEN 326
323 LOCATE 20, 1: PRINT "14. 3 FGA % "; : LOCATE 20, 15: PRINT "---";
    LOCATE 20, 30: INPUT W1: IF C1 = 1 THEN 326
324 LOCATE 21, 1: PRINT "15. DISK ID "; : INPUT YN$: IF C1 = 1 THEN 326
164 LOCATE 22: INPUT "16. HOMECOURT FACTOR (0-6)"; T%(6): IF C1 = 1 THEN 326
325 LOCATE 22, 30: INPUT "OPP 2FG% "; OE
    IF BI = 0 THEN BI = 20 * L5
    IF L3 = 0 THEN L3 = 20 * L5
326 C1 = 1
    LOCATE 8, 1: COLOR T%(0), T%(1): PRINT A$: COLOR 15, 0
    LOCATE 23: INPUT "# TO CHANGE (99 TO CONTINUE) "; B9
    ON B9 + 1 GOTO 308, 309, 310, 311, 313, 314, 315, 316, 317, 318, 319, 320, 321, 322, 323, 324, 164, 157, 158, 159, 160, 161, 162, 163, 165, 166, 325
REM:*** TM FGA/G
    T%(3) = INT(((T1 + T2) / (L5 * 2)) + .5)
    T%(4) = L1
    T%(5) = L5
327 C1 = 0: F3 = 40: IF H = 1 THEN F3 = 32


330 input "enter file name for stat file" t$
  for i = 0 to 13:n$(i) = "XXX":next
open t$ for input as #1
line input #1, x$
line input #1, x$
line input #1, x$
p = 0

10 line input #1, p$
   nm$ = mid$(p$,4,20)
15 if right$(nm$,1) = "." then nm$=left$(nm$,len(nm$)-1): goto 15
   if p = 14 then 17
   if nm$<>"Total" then 16
17 line input #1,x$
   if left$(x$, 5) <> "-----" then 17
   p = p - 1
   goto 100
16 for i = 1 to len (nm$)
   if mid$(nm$,i,1) = " " then 20
   next
20 nm$ = left$(nm$,1) + "." + right$(nm$,len(nm$)-i)
   nm$=ucase$(nm$)
   print nm$, p
   n$(p) = nm$
   print "enter position for ";nm$;" ";:input p$(p)
   a(p, 7) = val(mid$(p$, 25, 2)):x(p, 0) = a(p, 7)
   a(p, 13) = val(mid$(p$, 65, 3))
   a(p, 14) = val(mid$(p$, 69, 3))
   p = p + 1
   goto 10

100 for i = 0 to p
    line input #1, p$
    rs(p, 3) = val(mid$(p$, 26, 3))
    a(p, 9) = val(mid$(p$, 30, 3)):x(p, 1) = a(p, 9)
    rs(p, 13) = val(mid$(p$, 41, 3))
    a(p, 17) = val(mid$(p$, 45, 3))
    rs(p, 5) = val(mid$(p$, 56, 3))
    a(p, 16) = val(mid$(p$, 60, 3))
    next
101 line input #1, p$
    if left$(p$, 5) <> "Total" then 101
    for i = 1 to 5
    line input #1, p$
    next
    for i = 0 to p
    line input #1, p$
    a(p, 8) = val(mid$(p$, 26, 4))
    a(p, 19) = val(mid$(p$, 32, 3))
    a(p, 10) = val(mid$(p$, 42, 3))
    a(p, 12) = val(mid$(p$, 47, 3))
    a(p, 11) = val(mid$(p$, 56, 3))
    next
    close 1

    for i = 0 to p
    RS(I, 0) = A(I, 7)
    RS(I, 1) = A(I, 8)
    RS(I, 2) = A(I, 9)
    IF RS(I, 2) = 0 THEN
       A(I, 0) = 0
    ELSE
       A(I, 0) = INT(RS(I, 3) / RS(I, 2) * 100 + .5)
    END IF
    RS(I, 4) = A(I, 16)
    IF RS(I, 4) = 0 THEN
       A(I, 1) = 0
    ELSE
       A(I, 1) = INT(RS(I, 5) / RS(I, 4) * 100 + .5)
    END IF
    RS(I, 6) = A(I, 19)
    RS(I, 7) = A(I, 10)
    RS(I, 8) = A(I, 12)
    RS(I, 9) = A(I, 11)
    RS(I, 10) = A(I, 14)
    RS(I, 11) = A(I, 13)
    RS(I, 12) = A(I, 17)
    IF RS(I, 12) = 0 THEN
       A(I, 18) = 0: X(I, 2) = 0
    ELSE
       A(I, 18) = INT(RS(I, 13) / RS(I, 12) * 100 + .5): X(I, 2) = A(I, 18)
    END IF
    NEXT I


329 GOSUB 1500
346 X9 = 0: GOSUB 1400
347 FOR I = 0 TO 13
    IF N$(I) = "XXX" THEN 100
    X9 = X9 + A(I, 10) + A(I, 9)
100 NEXT I
348 FOR I = 0 TO 13
    IF N$(I) = "XXX" THEN 356
    IF A(I, 8) <> 0 AND H = 0 THEN M = 40 / (A(I, 8) / A(I, 7))
    IF A(I, 8) <> 0 AND H = 1 THEN M = 32 / (A(I, 8) / A(I, 7))
    IF A(I, 8) = 0 THEN M = (X9 / 5) / (A(I, 9) + A(I, 10)): MI = (A(I, 9) + A(I, 10)) / (X9 / 5)
    IF A(I, 8) = 0 AND M < 1.1 THEN M = 1.1
    A(I, 2) = INT((((A(I, 10) / A(I, 7)) * M) * M7) + .5)
    GOSUB 3500
    IF QQ(0) = 0 THEN A(I, 4) = INT(((A(I, 11) / A(I, 7)) * M) * (L6 / (T0 / L5)) + .5)
    IF QQ(2) = 0 THEN A(I, 5) = INT(((A(I, 13) / A(I, 7)) * M) + .5)
    IF QQ(3) = 0 THEN A(I, 6) = ((A(I, 14) / A(I, 7)) * M): A(I, 6) = INT(A(I, 6) + .5)
356 NEXT I
    X = L1 + L2 + 20: Y = (T1 + T2 + T3 + T4 + L3 + BI) / (2 * L5)
357 S = INT(((2 * Y) - X) + .5)
358 X1 = (T1 + T2) / (L5 * 2)
    X3 = (BC / 2) / X1: X3 = INT((X3 * 100) + .5)
    BC = INT(X3 / 2): BY = INT(X3 / 2)
    X4 = INT(RND(1) * 2)
    IF X3 - BC - BY > 0 AND X4 = 0 THEN BC = BC + 1
    IF X3 - BC - BY > 0 AND X4 = 1 THEN BY = BY + 1
    U7 = INT((46 - L4) / 2)
    D = OE - L4 + BY - U7
366 FOR I = 0 TO 13
    IF N$(I) = "XXX" THEN 367
    A(I, 3) = (A(I, 9) + A(I, 10)) / X9: A(I, 3) = INT((100 * A(I, 3)) + .5)
367 NEXT I
    V% = INT(((W0 / T2) + .005) * 100)
    V2% = V% - Q7
    IF V2% < 0 THEN V2% = 100 + V2%
    V1% = W1 - Q8
    IF V1% < 0 THEN V1% = 100 + V1%
    FOR I = 0 TO 13
    IF N$(I) = "XXX" THEN 378
    IF A(I, 9) <> 0 THEN X(I, 3) = INT(A(I, 17) / A(I, 9) * 100 + .5)
    IF A(I, 8) <> 0 AND H = 0 THEN M = 40 / (A(I, 8) / A(I, 7))
    IF A(I, 8) <> 0 AND H = 1 THEN M = 32 / (A(I, 8) / A(I, 7))
    IF A(I, 8) = 0 THEN M = (X9 / 5) / (A(I, 9) + A(I, 10))
    IF A(I, 8) = 0 AND M < 1 THEN M = 1
    B3 = (A(I, 12) / A(I, 7)) * M: B3 = INT((B3 * 10) + .5)
    X(I, 5) = B3
    B3 = A(I, 9) * (A(I, 0) / 100): B4 = A(I, 17) * (A(I, 18) / 100)
    IF A(I, 9) - A(I, 17) <> 0 THEN A(I, 0) = ((B3 - B4) / (A(I, 9) - A(I, 17))) + .005
    A(I, 0) = INT((A(I, 0) * 100) + .5)
378 NEXT I
    FOR I = 0 TO 13
    IF N$(I) = "XXX" THEN 385
    A(I, 0) = A(I, 0) - BC + U7
385 NEXT I
    L%(4) = 99
368 FOR I = 0 TO 13
    IF N$(I) = "XXX" THEN 374
    X = ((A(I, 9) * A(I, 0)) / 100): MI = (A(I, 8) / A(I, 7)) / 40
    IF A(I, 8) = 0 THEN MI = (A(I, 9) + A(I, 10)) / (X9 / 5)
369 X = X * .1: IF MI > 1 AND A(I, 8) = 0 THEN MI = 1
370 X = X + ((A(I, 7) * 1.64) * MI)
371 X = ((A(I, 16) - X) / 2)
372 X = INT(((X / (A(I, 9) + X)) * 100) + .5): IF X < 0 THEN X = 0
373 X(I, 6) = X
374 NEXT I
    GOSUB 1300
    GOSUB 1650
375 GOSUB 9800
379 T9 = 0
380 FOR I = 0 TO 13
    FOR I1 = 0 TO 19
    A(I, I1) = 0
    NEXT I1
    FOR I1 = 0 TO 6
    X(I, 0) = 0
    NEXT I1
    FOR I1 = 0 TO 13
    RS(I, I1) = 0
    NEXT I1
    DF(I) = 0: P$(I) = ""
    NEXT I
    A1$ = "": S$ = "": ATT = 0: C$ = ""
383 FOR I = 0 TO 13: N$(I) = "": NEXT I
384 GOTO 250


1300 R = 1200 - (((T1 + T2) / (L5 * 2)) * 4)
1301 R = R / 6
1302 Y = BH / L5
1303 X = (Y - 7) / .4
1306 X = INT(X + .5)
1308 IF X < 0 THEN L%(0) = 100 + X
1309 IF X > 0 THEN L%(0) = X
1310 Y = (BJ - BH) / L5
1311 X = (Y / R) - .01
1312 X = INT((X * 100) + .5)
1313 L%(1) = X
1320 Y = (BK - T9) / L5
1321 X = (Y / R) - .01
1322 X = INT((X * 100) + .5)
1323 X = X - 4
1324 IF X < 0 THEN L%(2) = 100 + X
1325 IF X > 0 THEN L%(2) = X

1330 R1 = INT(((T2 * BE) / 100) + .5)
1331 X = (R1 * .1) + (L5 * 8.2)
1332 X = (BF - X) / 2
1333 X = (X / (T2 + X)) - .09
1334 X = INT((X * 100) + .5)
1335 IF X > 0 THEN L%(3) = X
1336 IF X < 0 THEN L%(3) = X + 100
1337 RETURN

1400 X1 = T2 - ((BE * T2) / 100) + BF - ((BF * BG) / 100)
1401 X2 = T1 - ((T1 * T6) / 100) + T7 - ((T7 * T8) / 100)
1402 RD = (T3 - X1 + X2 - T4) / (L5 * 2): HS = T3 - X1 + X2 - T4
1403 M7 = ((35 + RD) / L2)
1404 IF H = 2 THEN M7 = ((44 + RD) / L2)
1405 RETURN

1500 X1 = VAL(MID$(A$, 1, 2))
1501 IF X1 < 1 OR X1 > 72 THEN 1505
1502 X2 = .83: IF X1 > 52 AND X1 < 58 THEN X2 = .7
1503 T7 = INT((T7 * X2) + .5): BF = INT((BF * X2) + .5)
1504 FOR I = 0 TO 13: A(I, 16) = INT((A(I, 16) * X2) + .5): NEXT I
1505 RETURN
1600 CLS : PRINT "YOU MUST HAVE THE FOLLOWING TEAM &": PRINT "INDIVIDUAL STATS: FGA,FG %,": PRINT "FTA,FT %,REBOUNDS": PRINT
1601 PRINT "IF YOU ARE INPUTTING A TEAM WITH ANY ": PRINT "OF THE FOLLOWING STATS MISSING,": PRINT "SIMPLY CHECK 'N' AND THE PROGRAM WILL"
1602 PRINT "ALLOW YOU TO INPUT RATINGS DIRECTLY"
1603 PRINT : PRINT "DO YOU HAVE THE FOLLOWING:"
1604 PRINT : INPUT "ASSISTS (YN) "; I$: QQ(0) = 0: IF I$ = "N" THEN QQ(0) = 1
1605 INPUT "PERSONAL FOULS (YN) "; I$: QQ(1) = 0: IF I$ = "N" THEN QQ(1) = 1
1606 INPUT "STEALS & TURNOVERS (YN) "; I$: QQ(2) = 0: IF I$ = "N" THEN QQ(2) = 1
1607 INPUT "BLOCKS (YN) "; I$: QQ(3) = 0: IF I$ = "N" THEN QQ(3) = 1
1608 PRINT : INPUT "ANY CHANGE (YN) "; I$
1609 IF I$ = "Y" THEN 1600
     RETURN
1650 IF QQ(0) = 1 THEN GOSUB 1700
1651 IF QQ(1) = 1 THEN GOSUB 1800
1652 IF QQ(2) = 1 THEN GOSUB 1900
1654 IF QQ(3) = 1 THEN GOSUB 2100
1655 RETURN
1700 CLS : PRINT "INPUT ASSIST RATINGS"
     PRINT "AA SHOULD = APPROX. 3 WHEN FINISHED"
     PRINT "FORWARDS,CENTERS ARE USUALLY 0,1,2,3 ; GUARDS ARE USUALLY 3,4,5,6"
1701 C1 = 0
     PRINT
     FOR I = 0 TO 13
1702 LOCATE I + 6, 1: PRINT I; " "; N$(I); : LOCATE I + 6, 20: INPUT E0(I)
1703 IF C1 = 1 THEN 1705
1704 NEXT I
1705 C1 = 1: GOSUB 1715: LOCATE 21, 1: PRINT "ANY CHANGE (0-13) (N=NO CHANGE) "
1706 INPUT I$
1707 IF UCASE$(I$) = "N" THEN 1709
1708 I = VAL(I$): GOTO 1702
1709 FOR I = 0 TO 13
     A(I, 4) = A(I, 4) + E0(I)
     NEXT I
     C1 = 0
     RETURN
1715 AA = 0
     FOR I1 = 0 TO 13
     AA = AA + E0(I1) * INT(A(I1, 3))
     NEXT I1
1716 LOCATE 20, 1
     PRINT "AA = "; AA / 100
     RETURN
1800 C1 = 0: CLS
     PRINT "INPUT FOUL COMMITING RATINGS"
     PRINT "PF SHOULD = APPROX. 40 WHEN FINISHED"
1801 PRINT "AVG. RATING = 40 ; RANGE = 15 TO 99"
     PRINT
     FOR I = 0 TO 13
1802 LOCATE I + 6, 1
     PRINT I; " "; N$(I); : LOCATE I + 6, 20: INPUT E0(I)
1803 IF C1 = 1 THEN 1805
1804 NEXT I
1805 C1 = 1: GOSUB 1815: LOCATE 21, 1: PRINT "ANY CHANGE (0-13) (N=NO CHANGE) "
     INPUT I$
1807 IF UCASE$(I$) = "N" THEN 1809
1808 I = VAL(I$): GOTO 1802
1809 FOR I = 0 TO 13
     X(I, 5) = E0(I)
     NEXT I
     C1 = 0
     RETURN
1815 PF = 0: TP = 0
     FOR I1 = 0 TO 13
     IF N$(I1) = "XXX" THEN 1810
     TP = TP + 1
     PF = PF + E0(I1)
1810 NEXT I1
1816 LOCATE 20, 1
     PRINT "PF = "; PF / TP
     RETURN
1900 HQ = T1 - T2
     HR = T7 - BF
     GOSUB 2200
     HS = HS / 2: HX = INT((HR + HS - HQ) + .5): HX = INT(((HX / L5) * 10) + .5)
1901 CLS
     L%(4) = 99
     PRINT "INPUT STEALS & TURNOVERS RATINGS. THIS TEAMS ST SHOULD = APPROX. "; HX / 10
1902 PRINT "IND. STL RATINGS: AVG=1 RANGE = 0 TO 4"
1903 PRINT "TM OFF STL ADJ: AVG=0 RANGE=-10 TO 10": C1 = 0
1904 PRINT "TM TURNOVR RATING: AVG=4 RANGE = 1 TO 7"
1905 PRINT "TM DEF TRNOVR ADJ: AVG=0 RANGE -4 TO 4"
     FOR I = 0 TO 13
1906 LOCATE I + 6, 1: PRINT I; " "; N$(I); : LOCATE I + 6, 20
     INPUT E0(I)
     IF C1 = 1 THEN 1911
1907 NEXT I
1908 LOCATE 20, 1: INPUT "X-TM OFF STL ADJ   "; E0(14): IF C1 = 1 THEN 1911
1909 LOCATE 21, 1: INPUT "Y-TM TURNOVER ADJ  "; E0(15): IF C1 = 1 THEN 1911
1910 LOCATE 22, 1: INPUT "Z-TM DEF TURNOVR ADJ"; E0(16): IF C1 = 1 THEN 1911
1911 C1 = 1: GOSUB 1925
     LOCATE 23, 1: PRINT "ANY CHANGE (0-Z) (N = NO CHANGE) ";
1913 INPUT I$
     IF UCASE$(I$) = "N" THEN 1930
1914 I = VAL(I$)
     IF UCASE$(I$) = "X" THEN 1908
1915 IF UCASE$(I$) = "Y" THEN 1909
1916 IF UCASE$(I$) = "Z" THEN 1910
1917 GOTO 1906
1925 ST = 0: FOR I = 0 TO 13: ST = ST + E0(I) * INT(A(I, 3)): NEXT I
1926 ST = ST / 20: SY = 7 + E0(14) * .4
1927 SV = (R * E0(15)) / 100: SW = ((4 + E0(16)) * R) / 100
1928 ST = ST - SY - SV + SW: LOCATE 10, 26: PRINT "ST = "; ST; : RETURN
1930 FOR I = 0 TO 13
     A(I, 5) = A(I, 5) + E0(I)
     NEXT I
     IF E0(14) < 0 THEN E0(14) = 100 + E0(14)
1933 IF E0(16) < 0 THEN E0(16) = 100 + E0(16)
     FOR I = 0 TO 2
     L%(I) = E0(I + 14)
     NEXT
     FOR I = 0 TO 4
     PRINT "L%("; I; ")="; L%(I)
     NEXT
1936 I$ = INKEY$: IF I$ = "" THEN 1936
1935 C1 = 0: RETURN
2100 CLS
     PRINT "INPUT BLOCK RATINGS"
     PRINT "GUARDS ARE USUALLY 0,1 ; FORWARDS,CENTERS ARE USUALLY 0,1,2,3"
     PRINT "THE GREAT SHOT BLOCKERS ARE USUALLY 5,6,7"
     PRINT
2101 C1 = 0
     FOR I = 0 TO 13
2102 LOCATE I + 6, 1: PRINT I; " "; N$(I); : LOCATE I + 6, 20: INPUT E0(I)
2103 IF C1 = 1 THEN 2105
2104 NEXT I
2105 C1 = 1: GOSUB 2115: LOCATE 20, 1: PRINT "ANY CHANGE (0-13) (N=NO CHANGE) ";
2106 INPUT I$
2107 IF UCASE$(I$) = "N" THEN 2109
2108 I = VAL(I$): GOTO 2102
2109 FOR I = 0 TO 13: A(I, 6) = A(I, 6) + E0(I): NEXT I: C1 = 0: RETURN
2115 RETURN
2200 HW = HR / 350: IF HW > 1 THEN HW = 1
2201 HW = ((HW * 35) + 32) / 100: IF HW < 0 THEN HW = HW * -1
2202 HR = HR * HW: RETURN
3500 X = INT(((((A(I, 10) / A(I, 7)) * M) * M7) * 10) + .5)
     IF A(I, 19) = 0 THEN 3504
3502 X(I, 4) = INT((X * (A(I, 19) / A(I, 10))) + .5): GOTO 3505
3504 X(I, 4) = INT((X * .35) + .5)
3505 A(I, 2) = X - X(I, 4)
     RETURN
4500 JJ = 4
     LOCATE 6, 8: COLOR 15, 1
     PRINT " "; CHR$(201); STRING$(61, 205); CHR$(187); " ";
     FOR II = 1 TO JJ: LOCATE 6 + II, 8
     PRINT " º"; STRING$(61, 32); "º "; : NEXT
     LOCATE 7 + JJ, 8
     PRINT " "; CHR$(200); STRING$(61, 205); CHR$(188); " "; : LOCATE 7
     RETURN
9800 OPEN D$(0) + "BASK." + YN$ FOR RANDOM AS #1 LEN = 655
FIELD #1, 15 AS Q$(0), 2 AS Q$(1), 2 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 2 AS Q$(14), 2 AS Q$(15), 2 AS Q$(16), 2 AS Q$(17), 2 AS Q$(18), 2 _
 AS Q$(19), 602 AS X$
FOR I = 0 TO 13
FIELD #1, 53 + I * 43 AS X$, 15 AS Q$(I * 15 + 20), 2 AS Q$(I * 15 + 21), 2 AS Q$(I * 15 + 22), 2 AS Q$(I * 15 + 23), 2 AS Q$(I * 15 + 24), 2 AS Q$(I * 15 + 25), 2 AS Q$(I * 15 + 26), 2 AS Q$(I * 15 + 27), 2 AS Q$(I * 15 + 28), 2 AS Q$(I * 15 + 29) _
, 2 AS Q$(I * 15 + 30), 2 AS Q$(I * 15 + 31), 2 AS Q$(I * 15 + 32), 2 AS Q$(I * 15 + 33), 2 AS Q$(I * 15 + 34), 655 - 53 - 43 - I * 43 AS X$
NEXT
     LSET Q$(0) = A$
     LSET Q$(1) = MKI$(S)
     LSET Q$(2) = MKI$(D)
     LSET Q$(3) = MKI$(V1%)
     LSET Q$(4) = MKI$(V2%)
     FOR I = 0 TO 4: LSET Q$(I + 5) = MKI$(L%(I)): NEXT
     T%(2) = 999
     FOR I = 0 TO 9: LSET Q$(I + 10) = MKI$(T%(I)): NEXT
     FOR I = 0 TO 13
     LSET Q$(I * 15 + 20) = N$(I)
     FOR I1 = 0 TO 6
     LSET Q$(I * 15 + I1 + 21) = MKI$(A(I, I1))
     LSET Q$(I * 15 + I1 + 28) = MKI$(X(I, I1))
     NEXT: NEXT
     PUT #1, T: CLOSE 1

OPEN D$(0) + "COLBBTMS." + YN$ FOR RANDOM AS #1 LEN = 566
FIELD #1, 15 AS Q$(0), 40 AS Q$(1), 15 AS Q$(2), 4 AS Q$(3)
FOR I = 0 TO 13
FIELD #1, 74 + I * 32 AS X$, 2 AS Q$(I * 16 + 4), 2 AS Q$(I * 16 + 5), 2 AS Q$(I * 16 + 6), 2 AS Q$(I * 16 + 7), 2 AS Q$(I * 16 + 8), 2 AS Q$(I * 16 + 9), 2 AS Q$(I * 16 + 10), 2 AS Q$(I * 16 + 11), 2 AS Q$(I * 16 + 12), 2 AS Q$(I * 16 + 13), 2 AS  _
Q$(I * 16 + 14), 2 AS Q$(I * 16 + 15), 2 AS Q$(I * 16 + 16), 2 AS Q$(I * 16 + 17), 2 AS Q$(I * 16 + 18), 2 AS Q$(I * 16 + 19)
FIELD #1, 522 AS X$, 2 AS Q$(228), 2 AS Q$(229), 2 AS Q$(230), 2 AS Q$(231), 2 AS Q$(232), 2 AS Q$(233), 2 AS Q$(234), 2 AS Q$(235), 2 AS Q$(236), 2 AS Q$(237), 2 AS Q$(238), 2 AS Q$(239), 2 AS Q$(240), 2 AS Q$(241), 2 AS Q$(242), 2 AS Q$(243), 2 AS _
 Q$(244), 2 AS Q$(245), 2 AS Q$(246), 2 AS Q$(247), 2 AS Q$(248), 2 AS Q$(249)
NEXT
     LSET Q$(0) = C$
     LSET Q$(1) = S$
     LSET Q$(2) = A1$
     LSET Q$(3) = MKL$(ATT)

     FOR I = 0 TO 13
     LSET Q$(I * 16 + 4) = MKI$(DF(I))
     LSET Q$(I * 16 + 5) = P$(I)
     FOR I1 = 0 TO 13
     LSET Q$(I * 16 + I1 + 6) = MKI$(RS(I, I1))
     NEXT: NEXT
     LSET Q$(228) = MKI$(L5)
     LSET Q$(229) = MKI$(T1)
     LSET Q$(230) = MKI$(T2)
     LSET Q$(231) = MKI$(T6)
     LSET Q$(232) = MKI$(BE)
     LSET Q$(233) = MKI$(T7)
     LSET Q$(234) = MKI$(BF)
     LSET Q$(235) = MKI$(T8)
     LSET Q$(236) = MKI$(BG)
     LSET Q$(237) = MKI$(T3)
     LSET Q$(238) = MKI$(T4)
     LSET Q$(239) = MKI$(T0)
     LSET Q$(240) = MKI$(XX)
     LSET Q$(241) = MKI$(L3)
     LSET Q$(242) = MKI$(BI)
     LSET Q$(243) = MKI$(BJ)
     LSET Q$(244) = MKI$(BK)
     LSET Q$(245) = MKI$(T9)
     LSET Q$(246) = MKI$(BH)
     LSET Q$(247) = MKI$(W0)
     LSET Q$(248) = MKI$(W1)
     LSET Q$(249) = MKI$(999)
     PUT #1, T: CLOSE 1
10010 RETURN

