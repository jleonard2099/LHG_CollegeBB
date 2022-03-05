DECLARE SUB TITLE ()
DEFINT A-Z
DECLARE SUB GETKEY ()
DECLARE FUNCTION EXIST (FILENAME$)
DECLARE SUB TextIn (T$, Max, NumOnly, CapsOn, EXITCODE, Colr)
DECLARE FUNCTION QPRTrim$ (Work$)
DECLARE FUNCTION GetDrive% ()
DECLARE SUB YesNoB (YN$, EXITCODE, Colr)
DECLARE SUB MMenuVert (BYVAL Address%, Choice%, Start%, SCANCODE%, NormClr%, HiLite%, NumRows%, Row%, Column%)
DECLARE SUB ClearEOL (Colr)
DECLARE FUNCTION QPRTrim$ (Work$)
DECLARE SUB ReadFile (BYVAL Address%)
DECLARE FUNCTION FUsing$ (Number$, Mask$)
DECLARE FUNCTION OneColor% (FG, BG)
DECLARE FUNCTION Peek1% (Segment, Address)
DECLARE SUB QPrint (X$, Colr, Page)
DECLARE FUNCTION FCount% (FSpec$)
DECLARE SUB SortStr (BYVAL Address%, Size%, Direction%)
DECLARE SUB MAMenu (BYVAL Address%, SELECTION%, Start%, Count%, EXITCODE%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%)
DECLARE SUB CapNum ()
DECLARE SUB CSMsgBox (Message$, Wdth, Cnf AS ANY)
DECLARE SUB MQPrint (X$, Colr)
DECLARE SUB MPaintBox (ULRow, ULCol, LRRow, LRCol, Colr)
DECLARE SUB MScrnSave (ULRow, ULCol, LRRow, LRCol, SEG Address)
DECLARE SUB MScrnRest (ULRow, ULCol, LRRow, LRCol, SEG Address)
1 CLS
2 DIM A(1, 6), A1(1, 6), A2(1, 6), A3(1, 6), A4(1, 5), A5(1, 5), A6(1, 2), A7(1, 2), A8(1, 2), A9(1, 2), B(1, 2), B1(1, 2), B3(1, 2), B6(11, 7), Q6%(1)
3 DIM O1(30), Z1$(30), Z1%(30), Z2$(30), N$(25, 2), NM$(10, 6, 3), O2(30), O3(30), O4(30)
4 DIM B2(1, 2), T1(1), K3(1, 6), R9%(1)
  DIM J6(1): DIM S(1, 10), O$(16), D3%(5, 6), P$(2), P(2), F(4), F1(4): DIM S1%(7, 6, 4): DIM W6(1, 1): DIM A$(1), A1$(1, 6), A2$(1, 5), A3$(1, 2), A4$(1, 2), A5$(1, 2), A6$(1, 2), Q$(300)
  DIM D$(1), T%(9)
  DIM VV$(500), V%(500), L%(4), item$(3)
  DIM Y AS LONG
  '$INCLUDE: 'DEFCNF.BI'
  '$INCLUDE: 'SETCNF.BI'          'queries the system info for MsgBox
  DEF FnExt$ (X$, Length) = X$ + SPACE$(Length - LEN(X$))  'for display only
'determine if the monitor is mono or color
   '
      FG = 0
      BG = 7

   Colr = OneColor%(FG, BG)             'pack FG and BG into a single byte
   D% = GetDrive%
   OPEN "DEV.BAS" FOR INPUT AS #2
   FOR I = 0 TO 1
   INPUT #2, D$(I)
   NEXT
   CLOSE 2
   CLS
   IF UCASE$(LEFT$(D$(1), 1)) <> "A" AND UCASE$(LEFT$(D$(1), 1)) <> "B" THEN 50
   PRINT "MAKE SURE DISK WITH STATS AND LEADER FILES IS IN DRIVE "; D$(1)
   CALL GETKEY
   GOSUB 4600
   U$ = "INSERT DISK WITH CONFERENCE FILES IN DRIVE " + D$(1)
   LOCATE , (80 - LEN(U$)) / 2
   PRINT U$
   U$ = "HIT ANY KEY TO CONTINUE"
   LOCATE , (80 - LEN(U$)) / 2
   PRINT U$
   COLOR 15, 0
45 I$ = INKEY$: IF I$ = "" THEN 45
50 FOR X = 1 TO 30: Z1$(X) = "XXX": NEXT
   CALL TITLE
   item$(2) = "ADD / REVIEW CONFERENCE MEMBERS "
   item$(1) = "CREATE / DEFINE NEW CONFERENCES "
   item$(3) = "RETURN TO STANDINGS/LEADERS MENU"
     SELECTION% = 1
     Count% = 3
     Start% = 1
     SCANCODE% = 3
     FileColr% = 31
     HiLiteColr% = 112
     Spacing% = 1
     Row% = 8
     Column% = 25
     CALL box0(7, 24, 11, 57, 2, 31)
     CALL ShowCursor
     MMenuVert VARPTR(item$(1)), SELECTION%, Start%, SCANCODE%, FileColr%, HiLiteColr%, Count%, Row%, Column%
     CALL HideCursor
     COLOR , 0
     IF SCANCODE% = 27 THEN 1195
    I1 = SELECTION%
    IF I1 = 2 THEN 4000
    IF I1 = 3 THEN 1195

130 COLOR 15, 0: CLS
    JJ = 7
    LOCATE 6, 8: COLOR 15, 1
    PRINT " Ö"; STRING$(61, 196); "· "; : FOR II = 1 TO JJ: LOCATE 6 + II, 8
    PRINT " º"; STRING$(61, 32); "º "; : NEXT
    LOCATE 7 + JJ, 8
    PRINT " Ó"; STRING$(61, 196); "½ "; : LOCATE 7, 32
    LOCATE , 12: PRINT "ENTER NAME FOR CONFERENCE: ": PRINT
    LOCATE , 12: PRINT "THE NAME IS 8 CHARACTERS LONG WITH NO PERIODS OR"
    LOCATE , 12: PRINT "SPACES. TO EXIT, JUST HIT 'ENTER'"
    LOCATE , 12: PRINT "SAMPLE NAMES ARE SEC, SWC, 94-ACC"
    LOCATE 7, 37: INPUT LG$
    IF LG$ = "" THEN 50
    LG$ = LEFT$(LG$, 8)
    LOCATE 13, 12: PRINT "ANY CHANGE (YN) "
22  I$ = INKEY$: IF I$ = "" THEN 22
    IF UCASE$(I$) = "Y" THEN 130
    COLOR 15, 0
    PRINT : PRINT
    LOCATE , 12: INPUT "ENTER NAME OF DIVISION 1 (ENTER FOR NONE) "; D1$
    LOCATE , 12: INPUT "ENTER NAME OF DIVISION 2 (ENTER FOR NONE) "; D2$

220 CLS : PRINT : COLOR 14: PRINT "  "; LG$; " CONFERENCE": PRINT : COLOR 15
    LOCATE , 6: PRINT D1$; : LOCATE , 53: PRINT D2$
230 FOR I = 1 TO 15
    PRINT USING "  ##"; I;
    PRINT " "; Z1$(I);
    LOCATE , 50
    PRINT USING "##"; I + 15;
    PRINT " "; Z1$(I + 15)
    NEXT I
240 GOSUB 1220: COLOR 2
    PRINT "1-ADD TEAM"
    PRINT "  2-REMOVE TEAM"
    PRINT "  3-END AND SAVE"
250 I$ = INKEY$: IF I$ = "" THEN 250
260 IF I$ = "3" THEN 410
270 LOCATE 20, 30: PRINT "TEAM # (1-30) ";
280 N = 1: INPUT B
290 IF B < 1 OR B > 30 THEN 270
300 IF I$ = "2" THEN Z1$(B) = "XXX": GOTO 220
181 COLOR 15, 0: CLS
    JJ = 7
    LOCATE 6, 8: COLOR 15, 4
    PRINT " Ö"; STRING$(61, 196); "· "; : FOR II = 1 TO JJ: LOCATE 6 + II, 8
    PRINT " º"; STRING$(61, 32); "º "; : NEXT
    LOCATE 7 + JJ, 8
    PRINT " Ó"; STRING$(61, 196); "½ "; : LOCATE 7, 32
    LOCATE , 12: PRINT "ENTER DISK ID: ": PRINT
    LOCATE , 12: PRINT "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
    LOCATE , 12: PRINT "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
    LOCATE , 12: PRINT
    LOCATE , 12: PRINT "HIT <ENTER> TO USE PREVIOUS ID"
    LOCATE 7, 27: INPUT NY$
    IF NY$ = "" THEN GOSUB 3102: GOTO 182
    YN$ = NY$
    FOR I = 1 TO 500: VV$(I) = "": NEXT
    IF D% >= 67 THEN 310
350 CLS : PRINT "INSERT APPROPRIATE TEAM DISK": PRINT "PRESS ANY KEY TO CONTINUE"
360 I$ = INKEY$: IF I$ = "" THEN 360
310 GOSUB 3100
182 IF BO% = 1 THEN BO% = 0: GOTO 181
183 LOCATE 2, 33: COLOR T%(0), T%(1): PRINT A$: COLOR 15, 4
    LOCATE , 33: PRINT "ANY CHANGE (YN)"; : I$ = INKEY$: IF I$ = "" THEN 183
    IF UCASE$(I$) = "Y" THEN 181
    COLOR 15, 0
390 Z1$(B) = A$: Z2$(B) = YN$: Z1%(B) = T1
400 GOTO 220
410 GOSUB 500: GOSUB 1170: GOTO 2237
500 IF D% >= 67 THEN 510
    CLS : PRINT "INSERT DISK TO SAVE LEAGUE LEADER FILES IN DRIVE "; D$(1)
501 LOCATE 23, 1: PRINT "PRESS ANY KEY TO CONTINUE";
505 I$ = INKEY$: IF I$ = "" THEN 505
510 RETURN
1195 CLS
     IF D% >= 67 THEN RUN "HELLO.EXE 3"
     PRINT "MAKE SURE GAME DISK IS IN DRIVE"
     CALL GETKEY
     RUN "HELLO.EXE 3"
     END

4000 Spec$ = D$(1) + "*.LIB"
    Count = FCount%(Spec$)
   IF Count = 0 THEN 4500
   REDIM F$(0 TO Count)
   FOR X = 1 TO Count
       F$(X) = SPACE$(12)
   NEXT
   F$(0) = Spec$
   CALL ReadFile(VARPTR(F$(0)))
   CALL SortStr(VARPTR(F$(1)), Count, 0)

1310 CLS
    COLOR , 1
    CLS
    LOCATE , 28
    COLOR 14
    PRINT "SELECT CONFERENCE TO SCHEDULE": COLOR 15
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
    PRINT "-SELECTS TEAM";
CALL ShowCursor
MAMenu VARPTR(F$(1)), SELECTION%, Start%, Count%, EXITCODE%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%
CALL HideCursor
     IF EXITCODE% = 27 THEN 50
     F$ = F$(SELECTION%)
   LOCATE 2, 33: PRINT F$: LOCATE , 33: PRINT "ANY CHANGE (YN)";
     COLOR 15, 0
3910 I$ = INKEY$: IF I$ = "" THEN 3910
    IF UCASE$(I$) = "Y" THEN 1310
3911 F$ = QPRTrim$(F$)
    LG$ = LEFT$(F$, LEN(F$) - 4)
1130 OPEN D$(1) + LG$ + ".LIB" FOR INPUT AS #2
     INPUT #2, LG%
1140 INPUT #2, LG$
     INPUT #2, D1$: INPUT #2, D2$
1150 FOR X = 1 TO 30: INPUT #2, Z1$(X): INPUT #2, Z1%(X): INPUT #2, Z2$(X): NEXT
1160 CLOSE 2
1165 GOTO 220

1170 OPEN D$(1) + LG$ + ".LIB" FOR OUTPUT AS #2
     PRINT #2, 0
1180 PRINT #2, LG$
     PRINT #2, D1$: PRINT #2, D2$
1190 FOR X = 1 TO 30: PRINT #2, Z1$(X): PRINT #2, Z1%(X): PRINT #2, Z2$(X): NEXT
1200 CLOSE 2
1210 RETURN
1220 LOCATE 21, 3: RETURN
1230 LOCATE 24, 1: PRINT "PRESS ANY KEY TO CONTINUE";
1240 I$ = INKEY$: IF I$ = "" THEN 1240
1250 RETURN
2237 FOR X = 1 TO 30
2238 IF Z1$(X) = "XXX" THEN Z1$(X) = CHR$(42): GOTO 2249
2239 FOR XX = 4 TO 11: IF MID$(Z1$(X), XX, 1) = " " OR MID$(Z1$(X), XX, 1) = "/" OR MID$(Z1$(X), XX, 1) = "." THEN MID$(Z1$(X), XX, 1) = "_"
2240 NEXT XX
2241 FOR XX = 12 TO 14: IF MID$(Z1$(X), XX, 2) = "ST" THEN MID$(Z1$(X), 9, 1) = "_": MID$(Z1$(X), 10, 2) = "ST"
2242 NEXT XX
2243 QQ = 8: FOR XX = 11 TO 4 STEP -1: IF MID$(Z1$(X), XX, 1) = "_" THEN QQ = XX - 4: GOTO 2245
2244 GOTO 2248
2245 NEXT XX
2248 YR$ = LEFT$(Z1$(X), 2): Z1$(X) = MID$(Z1$(X), 4, QQ) + "." + YR$
2249 NEXT X
2250 J$ = LEFT$(LG$, 8): Q$ = J$ + ".TMS"
2255 OPEN D$(1) + Q$ FOR OUTPUT AS #2
2260 WRITE #2, LG$
     WRITE #2, D1$: WRITE #2, D2$
2270 FOR X = 1 TO 30: WRITE #2, Z1$(X): NEXT
2280 CLOSE 2
     GOTO 50

3100 NT = 0
     OPEN D$(0) + "BASK." + YN$ FOR RANDOM AS #1 LEN = 655
     Y = LOF(1)
     FIELD #1, 15 AS Q$(0), 640 AS X$
     FOR I1 = 1 TO Y / 655
     GET #1, I1
     NT = NT + 1
     VV$(I1) = Q$(0): V%(I1) = I1
     NEXT
     CLOSE 1
     NT = Y / 655
     TS% = INT((NT - 1) / 80)

3102 SELECTION% = 1
     Count% = NT
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
20   MAMenu VARPTR(VV$(1)), SELECTION%, Start%, Count%, EXITCODE%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%
     CALL HideCursor
     IF EXITCODE% = 27 THEN BO% = 1: RETURN
     IF EXITCODE% = -71 THEN Start% = 1: EXITCODE% = 3: SELECTION% = 1: GOTO 20
     IF EXITCODE% = -79 THEN Start% = NT: EXITCODE% = 3: SELECTION% = NT: GOTO 20
     IF EXITCODE% = -73 THEN Start% = Start% - 80: IF Start% < 1 THEN Start% = 1
     IF EXITCODE% = -81 THEN Start% = Start% + 80: IF Start% > NT THEN Start% = NT
     IF EXITCODE% = -81 OR EXITCODE% = -73 THEN EXITCODE% = 3: SELECTION% = Start%: GOTO 20
     T1 = V%(SELECTION%)

9000 OPEN D$(0) + "BASK." + YN$ FOR RANDOM AS #1 LEN = 655
FIELD #1, 15 AS Q$(0), 2 AS Q$(1), 2 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 2 AS Q$(14), 2 AS Q$(15), 2 AS Q$(16), 2 AS Q$(17), 2 AS Q$(18), 2 _
 AS Q$(19), 602 AS X$
FOR I = 0 TO 13
FIELD #1, 53 + I * 43 AS X$, 15 AS Q$(I * 15 + 20), 2 AS Q$(I * 15 + 21), 2 AS Q$(I * 15 + 22), 2 AS Q$(I * 15 + 23), 2 AS Q$(I * 15 + 24), 2 AS Q$(I * 15 + 25), 2 AS Q$(I * 15 + 26), 2 AS Q$(I * 15 + 27), 2 AS Q$(I * 15 + 28), 2 AS Q$(I * 15 + 29) _
, 2 AS Q$(I * 15 + 30), 2 AS Q$(I * 15 + 31), 2 AS Q$(I * 15 + 32), 2 AS Q$(I * 15 + 33), 2 AS Q$(I * 15 + 34), 655 - 53 - 43 - I * 43 AS X$
NEXT
GET #1, T1
   A$ = Q$(0)
9005 IF RIGHT$(A$, 1) = " " THEN A$ = LEFT$(A$, LEN(A$) - 1): GOTO 9005
     FOR I = 0 TO 9: T%(I) = CVI(Q$(I + 10)): NEXT
     CLOSE 1

9524 RETURN

4600 JJ = 4
     LOCATE 6, 8: COLOR 15, 1
     PRINT " "; CHR$(201); STRING$(61, 205); CHR$(187); " ";
     FOR II = 1 TO JJ: LOCATE 6 + II, 8
     PRINT " º"; STRING$(61, 32); "º "; : NEXT
     LOCATE 7 + JJ, 8
     PRINT " "; CHR$(200); STRING$(61, 205); CHR$(188); " "; : LOCATE 7
     RETURN

4500 LOCATE 10, 8
     GOSUB 4600
     PRINT
     LOCATE , 15: PRINT "NO LEAGUE FILES WERE FOUND"
     LOCATE , 15: PRINT "PRESS ANY KEY TO RETURN TO MAIN MENU"
3805 I$ = INKEY$: IF I$ = "" THEN 3805
     GOTO 1195

SUB GETKEY ()
46 I$ = INKEY$: IF I$ = "" THEN 46
END SUB

SUB TITLE ()
   COLOR , 0: KEY OFF: CLS
   COLOR , 1: LOCATE 1, 1: PRINT TAB(80); : LOCATE , 24: COLOR 12
   PRINT "COURTSIDE COLLEGE BASKETBALL V4.0"
   LOCATE 24, 1: PRINT TAB(80); : LOCATE , 1
   PRINT "(C) LANCE HAFFNER/SHANNON LYNN 1987-1994"; TAB(70); LEFT$(TIME$, 5);
   LOCATE 3, 1: COLOR 2, 0
   COLOR 15: PRINT "        PROGRAMMED AND DEVELOPED BY LANCE HAFFNER AND SHANNON LYNN"
             PRINT "              FOR TECHNICAL SUPPORT, CALL (615) 366-8088"
   COLOR 10
END SUB

