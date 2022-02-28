DECLARE SUB TITLE ()
DECLARE SUB GETKEY ()
DECLARE SUB ReadFile (BYVAL Address%)
DECLARE SUB SortStr (BYVAL Address%, Size%, Direction%)
DECLARE SUB MAMenu (BYVAL Address%, SELECTION%, START%, COUNT%, EXITCODE%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%)
DECLARE FUNCTION QPRTrim$ (Work$)
DECLARE SUB MQPrint (X$, Colr)
DECLARE FUNCTION FCount% (FSpec$)
DECLARE SUB MPaintBox (ULRow, ULCol, LRRow, LRCol, Colr)
DECLARE SUB MScrnSave (ULRow, ULCol, LRRow, LRCol, SEG Address)
DECLARE SUB MScrnRest (ULRow, ULCol, LRRow, LRCol, SEG Address)
DECLARE FUNCTION GetDrive% ()
DECLARE FUNCTION EXIST (FILENAME$)
  DIM T$(30), O1(30), O2(30), P$(260), S$(60), P(60), BA$(120), ER$(20)
  DIM TT(120, 4), O3(30), O4(30), O5(30), O6(30)
  DIM Z2$(260), Z3$(260), Z(260), Z1(260), OL$(60), OL(60, 20), DL$(60), DL(60, 20)
  DIM D$(1)
  '$INCLUDE: 'DEFCNF.BI'
  '$INCLUDE: 'SetCnf.BI'          'queries the system info for MsgBox
  D% = GetDrive%
  H$ = " 1/2"
  OPEN "DEV.BAS" FOR INPUT AS #1: FOR I = 0 TO 1: INPUT #1, D$(I): NEXT: CLOSE 1

   IF UCASE$(LEFT$(D$(1), 1)) <> "A" AND UCASE$(LEFT$(D$(1), 1)) <> "B" THEN 4000
   PRINT "MAKE SURE DISK WITH STATS AND LEADER FILES IS IN DRIVE "; D$(1)
   CALL GETKEY

4000 Spec$ = D$(1) + "*.LDR"
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
    PRINT "SELECT LEAGUE TO VIEW": COLOR 15
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
    PRINT "-SELECTS LEAGUE  ESC - EXIT PROGRAM";
CALL SHOWCURSOR
20 MAMenu VARPTR(F$(1)), SELECTION%, START%, COUNT%, EXITCODE%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%
CALL HIDECURSOR
     IF EXITCODE% = 27 THEN 2900
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
    WE$ = LEFT$(F$, LEN(F$) - 4)
    GOTO 7

4500 GOSUB 4600
     LOCATE , 12: PRINT "NO LEAGUE FILES WERE FOUND"
     LOCATE , 12: PRINT "PRESS ANY KEY TO RETURN TO MAIN MENU"
3805 I$ = INKEY$: IF I$ = "" THEN 3805
     GOTO 2900

7 WE$ = LEFT$(WE$, 8)
   OPEN D$(1) + WE$ + ".TMS" FOR INPUT AS #2
   INPUT #2, FF$, D1$, D2$
   CLOSE 2
   GOSUB 200

   CLS
   LOCATE 9, 1: PRINT "      PLEASE WAIT WHILE THE LEADER FILES ARE LOADING!!"
   OPEN D$(1) + WE$ + ".LDR" FOR INPUT AS #2
   FOR X = 1 TO 30: INPUT #2, T$(X), O1(X), O2(X), O3(X), O4(X), O5(X), O6(X): NEXT
   FOR X = 1 TO 250: INPUT #2, Z2$(X), Z3$(X), Z(X), Z1(X): NEXT
   FOR X = 1 TO 40: INPUT #2, OL$(X): FOR M = 1 TO 20: INPUT #2, OL(X, M): NEXT: NEXT
   FOR X = 1 TO 40: INPUT #2, DL$(X): FOR M = 1 TO 20: INPUT #2, DL(X, M): NEXT: NEXT
   CLOSE #2
1202 CLS : CALL TITLE
     J = 5: GOSUB 2075: COLOR 7
     LOCATE , 30: PRINT "STATS TO "; : COLOR 15: PRINT "S"; : COLOR 7: PRINT "CREEN"
     LOCATE , 30: PRINT "STATS TO "; : COLOR 15: PRINT "P"; : COLOR 7: PRINT "RINTER"
     LOCATE , 30: PRINT "STATS TO "; : COLOR 15: PRINT "F"; : COLOR 7: PRINT "ILE"
8651 I$ = INKEY$: IF I$ = "" THEN 8651
     COLOR 14, 0
     IF UCASE$(I$) = "P" THEN 3200
     IF UCASE$(I$) = "F" THEN X1 = 1: GOTO 3100
     GOTO 1200

200 L = 8: IF LEN(D1$) < L THEN D1$ = D1$ + " ": GOTO 200
201 L = 8: IF LEN(D2$) < L THEN D2$ = D2$ + " ": GOTO 201
204 RETURN

1200 CLS
     COLOR 12: PRINT FF$ + " CONFERENCE"
     IF D1$ <> "" THEN PRINT D1$; " DIVISION"
     PRINT "                 CONFERENCE                         FULL SEASON"
     COLOR 14
     PRINT "                W    L     PCT                          W    L     PCT"
     COLOR 15
     FOR P = 1 TO 15
     IF T$(P) = "" OR T$(P) = "*" THEN 1256
1255 PRINT T$(P); TAB(16);
     PRINT USING "##   ##   "; O4(P); O5(P);
     IF O6(P) = 0 AND O4(P) = 0 AND O5(P) = 0 THEN PRINT " .---";
     IF O6(P) = 0 AND (O4(P) <> 0 OR O5(P) <> 0) THEN PRINT " .000";
     IF O6(P) = 1 THEN PRINT "1.000";
     IF O6(P) <> 0 AND O6(P) <> 1 THEN PRINT USING " .###"; O6(P);

     LOCATE , 41
     PRINT T$(P); TAB(56);
     PRINT USING "##   ##   "; O1(P); O2(P);

     IF O3(P) = 0 AND O1(P) = 0 AND O2(P) = 0 THEN PRINT " .---"
     IF O3(P) = 0 AND (O1(P) <> 0 OR O2(P) <> 0) THEN PRINT " .000"
     IF O3(P) = 1 THEN PRINT "1.000"
     IF O3(P) <> 0 AND O3(P) <> 1 THEN PRINT USING " .###"; O3(P)
1256 NEXT P
1277 GOSUB 7900
     CLS
     COLOR 12: PRINT FF$ + " CONFERENCE"
     IF D2$ <> "" THEN PRINT D2$; " DIVISION"
     PRINT "                 CONFERENCE                         FULL SEASON"
     COLOR 14
     PRINT "                W    L     PCT                          W    L     PCT"
     COLOR 15
     FOR P = 16 TO 30
     IF T$(P) = "" OR T$(P) = "*" THEN 1258
     PRINT T$(P); TAB(16);
     PRINT USING "##   ##   "; O4(P); O5(P);
     IF O6(P) = 0 AND O4(P) = 0 AND O5(P) = 0 THEN PRINT " .---";
     IF O6(P) = 0 AND (O4(P) <> 0 OR O5(P) <> 0) THEN PRINT " .000";
     IF O6(P) = 1 THEN PRINT "1.000";
     IF O6(P) <> 0 AND O6(P) <> 1 THEN PRINT USING " .###"; O6(P);

     LOCATE , 41
     PRINT T$(P); TAB(56);
     PRINT USING "##   ##   "; O1(P); O2(P);

     IF O3(P) = 0 AND O1(P) = 0 AND O2(P) = 0 THEN PRINT " .---"
     IF O3(P) = 0 AND (O1(P) <> 0 OR O2(P) <> 0) THEN PRINT " .000"
     IF O3(P) = 1 THEN PRINT "1.000"
     IF O3(P) <> 0 AND O3(P) <> 1 THEN PRINT USING " .###"; O3(P)
1258 NEXT P
     GOSUB 7900

     CLS : COLOR 14
     PRINT "FIELD GOAL % (MIN 5 FGM/G)"; TAB(41); "FREE THROW % (MIN 2.5 FTM/G)": PRINT : COLOR 15
     PRINT "                            PCT   FGA"; TAB(41); "                            PCT   FTA"
     FOR P = 41 TO 60
     IF Z2$(P) = "" THEN 1305
     PRINT LEFT$(Z2$(P), 10) + ", " + Z3$(P); : LOCATE , 28
     PRINT USING "##.#  ####"; Z(P) * 100; Z1(P)
1305 NEXT
     LOCATE 4
     FOR P = 61 TO 80
     IF Z2$(P) = "" THEN 1313
     LOCATE , 41: PRINT LEFT$(Z2$(P), 10) + ", " + Z3$(P); : LOCATE , 67
     PRINT USING "###.#  ####"; Z(P) * 100; Z1(P)
1313 NEXT
     GOSUB 7900

     CLS
     COLOR 14: PRINT "SCORING"
     COLOR 14: PRINT "                                PTS/G": COLOR 15
     FOR P = 1 TO 20
     IF Z2$(P) = "" THEN 1330
     PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 34
     PRINT USING "##.#"; Z(P)
1330 NEXT
     LOCATE 1, 41: COLOR 14: PRINT "TOTAL POINTS"
     LOCATE , 41: PRINT "                                  PTS": COLOR 15
     FOR P = 21 TO 40
     IF Z2$(P) = "" THEN 1333
     LOCATE , 41: PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 74
     PRINT USING "####"; Z(P)
1333 NEXT
     GOSUB 7900
     CLS
     COLOR 14: PRINT "STEALS PER GAME": COLOR 15
     FOR P = 81 TO 90
     IF Z2$(P) = "" THEN 1420
     PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 34
     PRINT USING "##.#"; Z(P)
1420 NEXT
     LOCATE 1, 41: COLOR 14: PRINT "STEALS": COLOR 15
     FOR P = 91 TO 100
     IF Z2$(P) = "" THEN 1426
     LOCATE , 41: PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 74
     PRINT USING "####"; Z(P)
1426 NEXT
     LOCATE 13: COLOR 14: PRINT "ASSISTS PER GAME": COLOR 15
     FOR P = 101 TO 110
     IF Z2$(P) = "" THEN 1432
     PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 34
     PRINT USING "##.#"; Z(P)
1432 NEXT
     LOCATE 13, 41: COLOR 14: PRINT "ASSISTS": COLOR 15
     FOR P = 111 TO 120
     IF Z2$(P) = "" THEN 1433
     LOCATE , 41: PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 74
     PRINT USING "####"; Z(P)
1433 NEXT
     GOSUB 7900
     CLS
     COLOR 14: PRINT "THREE FIELD GOAL% (MIN 1.5 3FGM/G)": COLOR 15
     FOR P = 121 TO 130
     IF Z2$(P) = "" THEN 1443
     PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 34
     PRINT USING "##.#"; Z(P) * 100
1443 NEXT P
     LOCATE 1, 41
     COLOR 14: PRINT "MINUTES PER GAME": COLOR 15
     FOR P = 141 TO 150
     IF Z2$(P) = "" THEN 1449
     LOCATE , 41: PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 74
     PRINT USING "##.#"; Z(P)
1449 NEXT P
     LOCATE 13
     COLOR 14: PRINT "THREE POINT FIELD GOALS MADE": COLOR 15
     FOR P = 131 TO 140
     IF Z2$(P) = "" THEN 1450
     PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 34
     PRINT USING "####"; Z(P)
1450 NEXT
     LOCATE 13, 41: COLOR 14: PRINT "TOTAL MINUTES": COLOR 15
     FOR P = 151 TO 160
     IF Z2$(P) = "" THEN 1451
     LOCATE , 41: PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 74
     PRINT USING "####"; Z(P)
1451 NEXT
     GOSUB 7900
     CLS
     COLOR 14: PRINT "REBOUNDS PER GAME   ": COLOR 15
     FOR P = 161 TO 170
     IF Z2$(P) = "" THEN 1452
     PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 34
     PRINT USING "##.#"; Z(P)
1452 NEXT
     LOCATE 1, 41: COLOR 14: PRINT "REBOUNDS": COLOR 15
     FOR P = 171 TO 180
     IF Z2$(P) = "" THEN 1453
     LOCATE , 41: PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 74
     PRINT USING "####"; Z(P)
1453 NEXT
     LOCATE 13: COLOR 14: PRINT "BLOCKS PER GAME": COLOR 15
     FOR P = 181 TO 190
     IF Z2$(P) = "" THEN 1454
     PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 34
     PRINT USING "##.#"; Z(P)
1454 NEXT
     LOCATE 13, 41: COLOR 14: PRINT "BLOCKS": COLOR 15
     FOR P = 191 TO 200
     IF Z2$(P) = "" THEN 1455
     LOCATE , 41: PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 74
     PRINT USING "####"; Z(P)
1455 NEXT
     GOSUB 7900
     CLS
     COLOR 14: PRINT "FIELD GOALS MADE ": COLOR 15
     FOR P = 201 TO 210
     IF Z2$(P) = "" THEN 1456
     PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 34
     PRINT USING "####"; Z(P)
1456 NEXT
     LOCATE 1, 41: COLOR 14: PRINT "FREE THROWS MADE": COLOR 15
     FOR P = 211 TO 220
     IF Z2$(P) = "" THEN 1457
     LOCATE , 41: PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 74
     PRINT USING "####"; Z(P)
1457 NEXT
     LOCATE 13: COLOR 14: PRINT "FOULS PER GAME": COLOR 15
     FOR P = 221 TO 230
     IF Z2$(P) = "" THEN 1458
     PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 34
     PRINT USING "##.#"; Z(P)
1458 NEXT
     LOCATE 13, 41: COLOR 14: PRINT "FOULS": COLOR 15
     FOR P = 231 TO 240
     IF Z2$(P) = "" THEN 1459
     LOCATE , 41: PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 74
     PRINT USING "####"; Z(P)
1459 NEXT
     GOSUB 7900
     CLS
     COLOR 14: PRINT "MOST VALUABLE PLAYER POINTS": COLOR 15
     FOR P = 241 TO 250
     IF Z2$(P) = "" THEN 1460
     PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 34
     PRINT USING "####"; Z(P)
1460 NEXT
     GOSUB 7900

1900 CLS : COLOR 12: PRINT "                         OFFENSIVE TOTALS"
1901 COLOR 14: PRINT "             G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG": COLOR 15
1905 FOR X = 1 TO 20
1910 IF OL$(X) = "" OR OL$(X) = "*" OR OL$(X) = " " THEN 1980
1915 PRINT LEFT$(OL$(X), 11); TAB(13); : PRINT USING "##"; OL(X, 16); : PRINT TAB(16); : PRINT USING "####"; OL(X, 2); : PRINT TAB(21); : PRINT USING "####"; OL(X, 3); : PRINT TAB(26);
1920 PRINT USING ".###"; OL(X, 17); : PRINT TAB(31); : PRINT USING "#### #### .###"; OL(X, 4); OL(X, 5); OL(X, 18);
1930 PRINT TAB(46); : PRINT USING "####"; OL(X, 6); : PRINT TAB(51); : PRINT USING "####"; OL(X, 9); : PRINT TAB(56); : PRINT USING "####"; OL(X, 7); : PRINT TAB(61);
1935 PRINT USING "###"; OL(X, 8); : PRINT TAB(65); : PRINT USING "###"; OL(X, 10); : PRINT TAB(69); : PRINT USING "#####"; OL(X, 20);
1940 PRINT TAB(75); : PRINT USING "###.#"; OL(X, 19)
1980 NEXT: GOSUB 7900
      CLS : COLOR 12: PRINT "                         DEFENSIVE TOTALS"
2901 COLOR 14: PRINT "             G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG": COLOR 15
     FOR X = 1 TO 20
2910 IF DL$(X) = "" OR DL$(X) = "*" OR DL$(X) = " " THEN 2980
2915 PRINT LEFT$(DL$(X), 11); TAB(13); : PRINT USING "##"; DL(X, 16); : PRINT TAB(16); : PRINT USING "####"; DL(X, 2); : PRINT TAB(21); : PRINT USING "####"; DL(X, 3); : PRINT TAB(26);
2920 PRINT USING ".###"; DL(X, 17); : PRINT TAB(31); : PRINT USING "#### #### .###"; DL(X, 4); DL(X, 5); DL(X, 18);
2930 PRINT TAB(46); : PRINT USING "#### ####"; DL(X, 6); DL(X, 9); : PRINT TAB(56); : PRINT USING "####"; DL(X, 7); : PRINT TAB(61);
2935 PRINT USING "###"; DL(X, 8); : PRINT TAB(65); : PRINT USING "###"; DL(X, 10); : PRINT TAB(69); : PRINT USING "#####"; DL(X, 20);
2940 PRINT TAB(75); : PRINT USING "###.#"; DL(X, 19)
2980 NEXT: GOSUB 7900

3900 GOSUB 4600
     U$ = "VIEW LEAGUE STATS AGAIN (YN)"
     LOCATE , (80 - LEN(U$)) / 2
     PRINT U$
     COLOR 15, 0
1820 A$ = INKEY$: IF A$ = "" THEN 1820
     IF UCASE$(A$) = "Y" THEN 1202
     GOTO 1310
2900 IF D% >= 67 THEN 2905
     CLS
     PRINT "MAKE SURE PROGRAM DISK IS IN DRIVE"
     CALL GETKEY
2905 RUN "HELLO.EXE 3"


3100 COLOR 15, 0: CLS
    JJ = 7
    LOCATE 6, 8: COLOR 15, 1
    PRINT " Ö"; STRING$(61, 196); "· "; : FOR II = 1 TO JJ: LOCATE 6 + II, 8
    PRINT " º"; STRING$(61, 32); "º "; : NEXT
    LOCATE 7 + JJ, 8
    PRINT " Ó"; STRING$(61, 196); "½ "; : LOCATE 7, 32
    LOCATE , 12: PRINT "ENTER NAME FOR FILE: ": PRINT
    LOCATE , 12: PRINT "THE NAME IS 8 CHARACTERS LONG WITH NO PERIODS OR"
    LOCATE , 12: PRINT "SPACES. DO NOT ADD A FILE EXTENSION, THE PROGRAM"
    LOCATE , 12: PRINT "WILL AUTOMATICALLY ADD .RPT"
    LOCATE 7, 37: INPUT SF$
    IF SF$ = "" THEN 3100
    LOCATE 13, 12: PRINT "ANY CHANGE (YN) "
705 I$ = INKEY$: IF I$ = "" THEN 705
   IF UCASE$(I$) = "Y" THEN 3100
   COLOR 15, 0
3200 CLS
     IF X1 <> 1 THEN OPEN "REPORT" FOR OUTPUT AS #2 ELSE OPEN SF$ + ".RPT" FOR OUTPUT AS #2
     PRINT #2, FF$ + " CONFERENCE"
     IF D1$ <> "" THEN PRINT #2, D1$; " DIVISION"
     PRINT #2, "                 CONFERENCE                         FULL SEASON"
     PRINT #2, "                W    L     PCT                          W    L     PCT"
     FOR P = 1 TO 15
     IF T$(P) = "" OR T$(P) = "*" THEN 3256
     PRINT #2,  T$(P); TAB(16);
     PRINT #2,  USING "##   ##   "; O4(P); O5(P);
     IF O6(P) = 0 AND O4(P) = 0 AND O5(P) = 0 THEN PRINT #2,  " .---";
     IF O6(P) = 0 AND (O4(P) <> 0 OR O5(P) <> 0) THEN PRINT #2,  " .000";
     IF O6(P) = 1 THEN PRINT #2,  "1.000";
     IF O6(P) <> 0 AND O6(P) <> 1 THEN PRINT #2,  USING " .###"; O6(P);

     PRINT #2, TAB(41);
     PRINT #2,  T$(P); TAB(56);
     PRINT #2,  USING "##   ##   "; O1(P); O2(P);

     IF O3(P) = 0 AND O1(P) = 0 AND O2(P) = 0 THEN PRINT #2,  " .---"
     IF O3(P) = 0 AND (O1(P) <> 0 OR O2(P) <> 0) THEN PRINT #2,  " .000"
     IF O3(P) = 1 THEN PRINT #2,  "1.000"
     IF O3(P) <> 0 AND O3(P) <> 1 THEN PRINT #2,  USING " .###"; O3(P)
3256 NEXT P
     PRINT #2,
     PRINT #2,  FF$ + " CONFERENCE"
     IF D2$ <> "" THEN PRINT #2,  D2$; " DIVISION"
     PRINT #2,  "                 CONFERENCE                         FULL SEASON"
     PRINT #2,  "                W    L     PCT                          W    L     PCT"
     FOR P = 16 TO 30
     IF T$(P) = "" OR T$(P) = "*" THEN 3258
     PRINT #2,  T$(P); TAB(16);
     PRINT #2,  USING "##   ##   "; O4(P); O5(P);
     IF O6(P) = 0 AND O4(P) = 0 AND O5(P) = 0 THEN PRINT #2,  " .---";
     IF O6(P) = 0 AND (O4(P) <> 0 OR O5(P) <> 0) THEN PRINT #2,  " .000";
     IF O6(P) = 1 THEN PRINT #2,  "1.000";
     IF O6(P) <> 0 AND O6(P) <> 1 THEN PRINT #2,  USING " .###"; O6(P);

     PRINT #2, TAB(41);
     PRINT #2,  T$(P); TAB(56);
     PRINT #2,  USING "##   ##   "; O1(P); O2(P);

     IF O3(P) = 0 AND O1(P) = 0 AND O2(P) = 0 THEN PRINT #2,  " .---"
     IF O3(P) = 0 AND (O1(P) <> 0 OR O2(P) <> 0) THEN PRINT #2,  " .000"
     IF O3(P) = 1 THEN PRINT #2,  "1.000"
     IF O3(P) <> 0 AND O3(P) <> 1 THEN PRINT #2,  USING " .###"; O3(P)
3258 NEXT P

     PRINT #2, CHR$(12)

     PRINT #2, "FIELD GOAL % (MIN 5 FGM/G)"; TAB(41); "FREE THROW % (MIN 2.5 FTM/G)"
     PRINT #2,
     PRINT #2, "                            PCT   FGA"; TAB(41); "                            PCT   FTA"
     FOR P = 41 TO 60
     IF Z2$(P) = "" THEN
         PRINT #2, TAB(41);
     ELSE
         PRINT #2, LEFT$(Z2$(P), 10) + ", " + Z3$(P);
         PRINT #2, TAB(28);
         PRINT #2, USING "##.#  ####"; Z(P) * 100; Z1(P);
         PRINT #2, TAB(41);
     END IF
     IF Z2$(P + 20) = "" THEN
        PRINT #2,
        GOTO 3320
     ELSE
        PRINT #2, LEFT$(Z2$(P + 20), 10) + ", " + Z3$(P + 20);
        PRINT #2, TAB(67);
        PRINT #2, USING "###.#  ####"; Z(P + 20) * 100; Z1(P)
     END IF
3320 NEXT
     PRINT #2,
     PRINT #2, "SCORING"; TAB(41); "TOTAL POINTS"
     FOR P = 1 TO 20
     IF Z2$(P) = "" THEN
         PRINT #2, TAB(41);
     ELSE
         PRINT #2, Z2$(P) + ", " + Z3$(P);
         PRINT #2, TAB(34);
         PRINT #2, USING "##.#"; Z(P);
         PRINT #2, TAB(41);
     END IF
     IF Z2$(P + 20) = "" THEN
        PRINT #2,
        GOTO 3340
     ELSE
        PRINT #2, Z2$(P + 20) + ", " + Z3$(P + 20);
        PRINT #2, TAB(74);
        PRINT #2, USING "####"; Z(P + 20)
     END IF
3340 NEXT
     PRINT #2, CHR$(12)
     PRINT #2, "STEALS PER GAME"; TAB(41); "STEALS"
     FOR P = 81 TO 90
     IF Z2$(P) = "" THEN
         PRINT #2, TAB(41);
     ELSE
         PRINT #2, Z2$(P) + ", " + Z3$(P);
         PRINT #2, TAB(34);
         PRINT #2, USING "##.#"; Z(P);
         PRINT #2, TAB(41);
     END IF
     IF Z2$(P + 10) = "" THEN
        PRINT #2,
        GOTO 3360
     ELSE
        PRINT #2, Z2$(P + 10) + ", " + Z3$(P + 10);
        PRINT #2, TAB(74);
        PRINT #2, USING "####"; Z(P + 10)
     END IF
3360 NEXT
     PRINT #2,
     PRINT #2, "ASSISTS PER GAME"; TAB(41); "ASSISTS"
     FOR P = 101 TO 110
     IF Z2$(P) = "" THEN
         PRINT #2, TAB(41);
     ELSE
         PRINT #2, Z2$(P) + ", " + Z3$(P);
         PRINT #2, TAB(34);
         PRINT #2, USING "##.#"; Z(P);
         PRINT #2, TAB(41);
     END IF
     IF Z2$(P + 10) = "" THEN
        PRINT #2,
        GOTO 3370
     ELSE
        PRINT #2, Z2$(P + 10) + ", " + Z3$(P + 10);
        PRINT #2, TAB(74);
        PRINT #2, USING "####"; Z(P + 10)
     END IF
3370 NEXT
     PRINT #2,
     PRINT #2, "THREE FIELD GOAL % (MIN 1.5 3FGM/G)"; TAB(41); "MINUTES PER GAME"
     FOR P = 121 TO 130
     IF Z2$(P) = "" THEN
         PRINT #2, TAB(41);
     ELSE
         PRINT #2, Z2$(P) + ", " + Z3$(P);
         PRINT #2, TAB(34);
         PRINT #2, USING "##.#"; Z(P) * 100;
         PRINT #2, TAB(41);
     END IF
     IF Z2$(P + 20) = "" THEN
        PRINT #2,
        GOTO 3361
     ELSE
        PRINT #2, Z2$(P + 20) + ", " + Z3$(P + 20);
        PRINT #2, TAB(74);
        PRINT #2, USING "##.#"; Z(P + 20)
     END IF
3361 NEXT
     PRINT #2,
     PRINT #2, "THREE POINT FIELD GOALS MADE"; TAB(41); "MINUTES"
     FOR P = 131 TO 140
     IF Z2$(P) = "" THEN
         PRINT #2, TAB(41);
     ELSE
         PRINT #2, Z2$(P) + ", " + Z3$(P);
         PRINT #2, TAB(34);
         PRINT #2, USING "####"; Z(P);
         PRINT #2, TAB(41);
     END IF
     IF Z2$(P + 20) = "" THEN
        PRINT #2,
        GOTO 3371
     ELSE
        PRINT #2, Z2$(P + 20) + ", " + Z3$(P + 20);
        PRINT #2, TAB(74);
        PRINT #2, USING "####"; Z(P + 20)
     END IF
3371 NEXT
     PRINT #2,
     PRINT #2, CHR$(12)
     PRINT #2, "REBOUNDS PER GAME"; TAB(41); "REBOUNDS"
     FOR P = 161 TO 170
     IF Z2$(P) = "" THEN
         PRINT #2, TAB(41);
     ELSE
         PRINT #2, Z2$(P) + ", " + Z3$(P);
         PRINT #2, TAB(34);
         PRINT #2, USING "##.#"; Z(P);
         PRINT #2, TAB(41);
     END IF
     IF Z2$(P + 10) = "" THEN
        PRINT #2,
        GOTO 3362
     ELSE
        PRINT #2, Z2$(P + 10) + ", " + Z3$(P + 10);
        PRINT #2, TAB(74);
        PRINT #2, USING "####"; Z(P + 10)
     END IF
3362 NEXT
     PRINT #2,
     PRINT #2, "BLOCKS PER GAME"; TAB(41); "BLOCKS"
     FOR P = 181 TO 190
     IF Z2$(P) = "" THEN
         PRINT #2, TAB(41);
     ELSE
         PRINT #2, Z2$(P) + ", " + Z3$(P);
         PRINT #2, TAB(34);
         PRINT #2, USING "##.#"; Z(P);
         PRINT #2, TAB(41);
     END IF
     IF Z2$(P + 10) = "" THEN
        PRINT #2,
        GOTO 3372
     ELSE
        PRINT #2, Z2$(P + 10) + ", " + Z3$(P + 10);
        PRINT #2, TAB(74);
        PRINT #2, USING "####"; Z(P + 20)
     END IF
3372 NEXT
     PRINT #2,
     PRINT #2, "FIELD GOALS MADE"; TAB(41); "FREE THROWS MADE"
     FOR P = 201 TO 210
     IF Z2$(P) = "" THEN
         PRINT #2, TAB(41);
     ELSE
         PRINT #2, Z2$(P) + ", " + Z3$(P);
         PRINT #2, TAB(34);
         PRINT #2, USING "####"; Z(P);
         PRINT #2, TAB(41);
     END IF
     IF Z2$(P + 10) = "" THEN
        PRINT #2,
        GOTO 3363
     ELSE
        PRINT #2, Z2$(P + 10) + ", " + Z3$(P + 10);
        PRINT #2, TAB(74);
        PRINT #2, USING "####"; Z(P + 10)
     END IF
3363 NEXT
     PRINT #2,
     PRINT #2, "FOULS PER GAME"; TAB(41); "FOULS"
     FOR P = 221 TO 230
     IF Z2$(P) = "" THEN
         PRINT #2, TAB(41);
     ELSE
         PRINT #2, Z2$(P) + ", " + Z3$(P);
         PRINT #2, TAB(34);
         PRINT #2, USING "##.#"; Z(P);
         PRINT #2, TAB(41);
     END IF
     IF Z2$(P + 10) = "" THEN
        PRINT #2,
        GOTO 3373
     ELSE
        PRINT #2, Z2$(P + 10) + ", " + Z3$(P + 10);
        PRINT #2, TAB(74);
        PRINT #2, USING "####"; Z(P + 10)
     END IF
3373 NEXT
     PRINT #2,
     PRINT #2, "MOST VALUABLE PLAYER"
     FOR P = 241 TO 250
     IF Z2$(P) = "" THEN 3364
     PRINT #2, Z2$(P) + ", " + Z3$(P);
     PRINT #2, TAB(34);
     PRINT #2, USING "####"; Z(P)
3364 NEXT
     PRINT #2,
     PRINT #2, CHR$(12)

    PRINT #2, "                         OFFENSIVE TOTALS"
    PRINT #2, "             G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG"
     FOR X = 1 TO 20
     IF OL$(X) = "" OR OL$(X) = "*" OR OL$(X) = " " THEN 1981
     PRINT #2, LEFT$(OL$(X), 11); TAB(13); : PRINT #2, USING "##"; OL(X, 16); : PRINT #2, TAB(16); : PRINT #2, USING "####"; OL(X, 2); : PRINT #2, TAB(21); : PRINT #2, USING "####"; OL(X, 3); : PRINT #2, TAB(26);
     PRINT #2, USING ".###"; OL(X, 17); : PRINT #2, TAB(31); : PRINT #2, USING "#### #### .###"; OL(X, 4); OL(X, 5); OL(X, 18);
     PRINT #2, TAB(46); : PRINT #2, USING "####"; OL(X, 6); : PRINT #2, TAB(51); : PRINT #2, USING "####"; OL(X, 9); : PRINT #2, TAB(56); : PRINT #2, USING "####"; OL(X, 7); : PRINT #2, TAB(61);
     PRINT #2, USING "###"; OL(X, 8); : PRINT #2, TAB(65); : PRINT #2, USING "###"; OL(X, 10); : PRINT #2, TAB(69); : PRINT #2, USING "#####"; OL(X, 20);
     PRINT #2, TAB(75); : PRINT #2, USING "###.#"; OL(X, 19)
1981 NEXT
     PRINT #2,
      PRINT #2, "                         DEFENSIVE TOTALS"
     PRINT #2, "             G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG": COLOR 15
     FOR X = 1 TO 20
     IF DL$(X) = "" OR DL$(X) = "*" OR DL$(X) = " " THEN 2981
     PRINT #2, LEFT$(DL$(X), 11); TAB(13); : PRINT #2, USING "##"; DL(X, 16); : PRINT #2, TAB(16); : PRINT #2, USING "####"; DL(X, 2); : PRINT #2, TAB(21); : PRINT #2, USING "####"; DL(X, 3); : PRINT #2, TAB(26);
     PRINT #2, USING ".###"; DL(X, 17); : PRINT #2, TAB(31); : PRINT #2, USING "#### #### .###"; DL(X, 4); DL(X, 5); DL(X, 18);
     PRINT #2, TAB(46); : PRINT #2, USING "#### ####"; DL(X, 6); DL(X, 9); : PRINT #2, TAB(56); : PRINT #2, USING "####"; DL(X, 7); : PRINT #2, TAB(61);
     PRINT #2, USING "###"; DL(X, 8); : PRINT #2, TAB(65); : PRINT #2, USING "###"; DL(X, 10); : PRINT #2, TAB(69); : PRINT #2, USING "#####"; DL(X, 20);
     PRINT #2, TAB(75); : PRINT #2, USING "###.#"; DL(X, 19)
2981 NEXT
     CLOSE 2
     IF X1 = 1 THEN 3900
     OPEN "REPORT" FOR INPUT AS #2
     DO WHILE NOT EOF(2)
         LINE INPUT #2, X$
         LPRINT X$
     LOOP
     CLOSE 2
     LPRINT CHR$(12)
     GOTO 3900

7900 HZ = 1: VT = 24
     LOCATE VT, HZ: PRINT "                          ";
9005 QK$ = INKEY$: IF QK$ = "" THEN 9005
     RETURN



4600 JJ = 4
     LOCATE 6, 8: COLOR 15, 1
     PRINT " "; CHR$(201); STRING$(61, 205); CHR$(187); " ";
     FOR II = 1 TO JJ: LOCATE 6 + II, 8
     PRINT " º"; STRING$(61, 32); "º "; : NEXT
     LOCATE 7 + JJ, 8
     PRINT " "; CHR$(200); STRING$(61, 205); CHR$(188); " "; : LOCATE 7
     RETURN

2075 REM:
     LOCATE 8, 8: COLOR 15, 1: PRINT " Ö"; STRING$(61, 196); "· ";
     FOR II = 1 TO J: LOCATE 8 + II, 8: PRINT " º"; STRING$(61, 32); "º "; : NEXT
     LOCATE 9 + J, 8: PRINT " Ó"; STRING$(61, 196); "½ "; : LOCATE 10, 15: RETURN

SUB GETKEY ()
4605 I$ = INKEY$: IF I$ = "" THEN 4605
END SUB

SUB TITLE ()
     COLOR , 2
     LOCATE 1, 1: PRINT TAB(80); : LOCATE , 26: COLOR 0
     PRINT "HAT TRICK HOCKEY STATISTICS"
     LOCATE 24, 1: PRINT TAB(80); : LOCATE , 1
     PRINT "(C) LANCE HAFFNER/SHANNON LYNN 1991"; TAB(70); LEFT$(TIME$, 5);
     LOCATE 4, 1: COLOR 2, 0
END SUB

