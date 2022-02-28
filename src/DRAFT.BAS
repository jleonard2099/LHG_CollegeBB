DECLARE SUB TITLE ()
   DECLARE SUB MAMenu (BYVAL Address%, SELECTION%, Start%, Count%, EXITCODE%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%)
   DECLARE SUB MMenuVert (BYVAL Address%, Choice%, Start%, SCANCODE%, NormClr%, HiLite%, NumRows%, Row%, Column%)
   DECLARE FUNCTION GetDrive% ()
   DECLARE FUNCTION EXIST% (FILENAME$)

   DIM Q$(500), A$(1, 13), A(1, 13, 6), X(1, 13, 6), V$(500)
   DIM S$(1), A1$(1), ATT(1), DF(1, 13), P$(1, 13), RS(1, 13, 13)
   DIM T1%(1, 20), CK(1), C$(1), V%(500), YN$(1), T1N(1), item$(6)
   DIM T%(1, 9), L%(1, 4), T$(1), SP(1), DA(1), V1%(1), V2%(1)
   DIM D$(1), TR%(1, 13, 12), Y AS LONG

   OPEN "DEV.BAS" FOR INPUT AS #1: INPUT #1, D$(0): CLOSE 1

10   FOR I = 0 TO 1
     T$(I) = "XXX"
     SP(I) = 0
     DA(I) = 0
     V1%(I) = 0
     V2%(I) = 0
     FOR I1 = 0 TO 4: L%(I, I1) = 0: NEXT
     FOR I1 = 0 TO 9: T%(I, I1) = 0: NEXT
     FOR I1 = 0 TO 13
     A$(I, I1) = "XXX"
     FOR I2 = 0 TO 6
     A(I, I1, I2) = 0
     X(I, I1, I2) = 0
     NEXT: NEXT: NEXT
   CALL TITLE
   item$(1) = "DRAFT A NEW TEAM         "
   item$(2) = "DRAFT TO AN EXISTING TEAM"
   item$(3) = "EXIT TO MAIN MENU        "
     SELECTION% = 1
     Count% = 3
     Start% = 1
     SCANCODE% = 3
     FileColr% = 31
     HiLiteColr% = 112
     Spacing% = 1
     Row% = 8
     Column% = 25
     CALL box0(7, 24, 11, 50, 2, 31)
     CALL ShowCursor
     MMenuVert VARPTR(item$(1)), SELECTION%, Start%, SCANCODE%, FileColr%, HiLiteColr%, Count%, Row%, Column%
     CALL HideCursor
     COLOR , 0
     IF SCANCODE% = 27 OR SELECTION% = 3 THEN 1
     IF SELECTION% = 1 THEN DNT% = 0: GOTO 100
     IF SELECTION% = 2 THEN DNT% = 1: GOTO 2000

1   IF GetDrive% >= 67 THEN RUN "HELLO.EXE 2"
    CLS : PRINT "MAKE SURE PROGRAM DISK IS IN DRIVE"
15 I$ = INKEY$: IF I$ = "" THEN 15
   RUN "HELLO.EXE 2"

100 CLS : COLOR 14: PRINT "TEAM INFORMATION": X% = 2: COLOR 15
147 LOCATE 2: INPUT " 1. TEAM NUMBER "; T1N(0): IF C1 = 1 THEN 190
162 LOCATE 3: INPUT " 2. DISK ID "; YN$(0): IF C1 = 1 THEN 190
163 LOCATE 4: INPUT " 3. TEAM NAME "; T$(0): IF C1 = 1 THEN 190
157 LOCATE 5: INPUT " 4. TEXT COLOR (0-15) "; T%(0, 0): IF C1 = 1 THEN 190
158 LOCATE 6: INPUT " 5. BACKGROUND (0-7) "; T%(0, 1): IF C1 = 1 THEN 190
164 LOCATE 7: INPUT " 6. HOMECOURT FACTOR (0-6) "; T%(0, 6): IF C1 = 1 THEN 190
190 LOCATE 9, 1: COLOR T%(0, 0), T%(0, 1)
    PRINT "TEAM APPEARANCE": COLOR 15, 0
    C1 = 1
    LOCATE 11, 1: PRINT "SELECT # TO CHANGE 1-6 (0 TO CONTINUE) ";
    INPUT B9: IF B9 < 1 OR B9 > 6 THEN 200
    ON B9 GOTO 147, 162, 163, 157, 158, 164
200 IF T%(0, 6) < 0 THEN T%(0, 6) = 0
    IF T%(0, 6) > 6 THEN T%(0, 6) = 6
    C1 = 0
450 COLOR 15, 0
    CLS : COLOR T%(0, 0), T%(0, 1): PRINT T$(0); : COLOR 14, 0: PRINT "     (DRAFTING)"
451 PRINT : PRINT "PLAYERS"
452 FOR I = 0 TO 13
    COLOR 2: PRINT USING "## "; I; : COLOR 15: PRINT A$(0, I)
    NEXT
454 LOCATE 21, 1
    COLOR 14: PRINT "(1) "; : COLOR 15: PRINT "DRAFT PLAYER"
    COLOR 14: PRINT "(Z) "; : COLOR 15: PRINT "SAVE TEAM"
455 I$ = INKEY$: IF I$ = "" THEN 455
456 IF UCASE$(I$) = "Z" THEN 800
457 IF I$ = "1" THEN 470
458 GOTO 454
470 LOCATE 21, 20: PRINT "SLOT # (0-13) ";
471 N = 1: INPUT B
472 IF B < 0 OR B > 14 THEN 470
1440 COLOR 15, 0: CLS
    JJ = 7
    LOCATE 6, 8: COLOR 15, 4
    PRINT " Ö"; STRING$(61, 196); "· "; : FOR II = 1 TO JJ: LOCATE 6 + II, 8
    PRINT " º"; STRING$(61, 32); "º "; : NEXT
    LOCATE 7 + JJ, 8
    PRINT " Ó"; STRING$(61, 196); "½ "; : LOCATE 7, 32
    LOCATE , 12: PRINT "ENTER DISK ID: ": PRINT
    LOCATE , 12: PRINT "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
    LOCATE , 12: PRINT "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
    LOCATE 7, 27: INPUT NY$
    IF NY$ = "" THEN GOSUB 1102: GOTO 182
    YN$(1) = NY$
    FOR I = 1 TO 500: V$(I) = "": NEXT
    GOSUB 1100
182 IF BO% = 1 THEN BO% = 0: GOTO 1440
183 LOCATE 2, 33: COLOR T%(1, 0), T%(1, 1): PRINT T$(1): COLOR 15, 4
    LOCATE , 33: PRINT "ANY CHANGE (YN)"; : I$ = INKEY$: IF I$ = "" THEN 183
    IF UCASE$(I$) = "Y" THEN 1440
550 COLOR , 0: CLS : COLOR T%(1, 0), T%(1, 1): PRINT T$(1): COLOR 14, 0
551 PRINT : PRINT "PLAYERS"
552 FOR I = 0 TO 13
    COLOR 2: PRINT USING "## "; I; : COLOR 15: PRINT A$(1, I)
    NEXT
560 COLOR 14: LOCATE 21, 1: PRINT "SELECT PLAYER # TO DRAFT ";
561 INPUT B1
562 IF B1 < 0 OR B1 > 13 THEN 560
571 A$(0, B) = A$(1, B1)
    DF(0, B) = DF(1, B1)
    P$(0, B) = P$(1, B1)
    FOR I = 0 TO 13
    RS(0, B, I) = RS(1, B1, I)
    NEXT
    FOR I = 1 TO 12
    TR%(0, B, I) = TR%(1, B1, I)
    NEXT
572 FOR I = 0 TO 6
    A(0, B, I) = A(1, B1, I)
    X(0, B, I) = X(1, B1, I)
    NEXT


574 GOTO 450
800 CLS : COLOR T%(0, 0), T%(0, 1): PRINT A$: COLOR 14, 0
    PRINT "PLAYER          CONTRIBUTION"
    PRINT "                PERCENT"
    COLOR 15, 0
    T3 = 0
    FOR I = 0 TO 13
    PRINT USING "## "; I; : PRINT A$(0, I);
    LOCATE , 17: PRINT A(0, I, 3): LOCATE , 1
    T3 = T3 + A(0, I, 3)
    NEXT I
    PRINT
    COLOR 14: PRINT "TOTAL CP "; : LOCATE , 17: COLOR 15, 0: PRINT T3
805 LOCATE 21: PRINT "SELECT PLAYER TO CHANGE 0-13 (22-NO MORE CHANGE) ";
    INPUT X9
    IF X9 = 22 THEN 825
    IF X9 < 0 OR X9 > 13 THEN 805
    LOCATE 22, 18: PRINT A$(0, X9): LOCATE , 1
    LOCATE 23, 1
    INPUT "ENTER CP: "; A(0, X9, 3)
    GOTO 800
825 IF T3 > 98 AND T3 < 102 THEN 1000
    CLS
    GOSUB 4500
    U$ = "TOTAL CONTRIBUTION MUST BE 100 %"
    LOCATE , (80 - LEN(U$)) / 2
    PRINT U$
    U$ = "HIT ANY KEY TO CONTINUE"
    LOCATE , (80 - LEN(U$)) / 2
    PRINT U$
    COLOR 15, 0
830 I$ = INKEY$: IF I$ = "" THEN 830
    GOTO 800
1000 CLS
     IF UCASE$(LEFT$(D$(0), 1)) = "C" THEN 1002
     GOSUB 4500
     U$ = "INSERT TEAMS DISK IN DRIVE " + D$(0)
     LOCATE , (80 - LEN(U$)) / 2
     PRINT U$
     U$ = "HIT ANY KEY TO CONTINUE"
     LOCATE , (80 - LEN(U$)) / 2
     PRINT U$
     COLOR 15, 0
1001 I$ = INKEY$: IF I$ = "" THEN 1001
1002 GOSUB 9800
1008 CLS
     GOSUB 4500
     U$ = "DRAFT ANOTHER TEAM (YN)"
     LOCATE , (80 - LEN(U$)) / 2
     PRINT U$
     COLOR 15, 0
1009 I$ = INKEY$: IF I$ = "" THEN 1009
1010 IF UCASE$(I$) = "Y" THEN 10
1011 CLS
     GOSUB 4500
     U$ = "INSERT DRAFT/TRADE PROGRAM DISK IN DRIVE "
     LOCATE , (80 - LEN(U$)) / 2
     PRINT U$
     U$ = "HIT ANY KEY TO CONTINUE"
     LOCATE , (80 - LEN(U$)) / 2
     PRINT U$
     COLOR 15, 0
1012 I$ = INKEY$: IF I$ = "" THEN 1012
     RUN "HELLO.EXE 2"
     END
1100 GOSUB 3000
     OPEN D$(0) + "BASK." + YN$(1) FOR RANDOM AS #1 LEN = 655
     Y = LOF(1)
     FIELD #1, 15 AS Q$(0), 640 AS X$
     FOR I1 = 1 TO Y / 655
     GET #1, I1
     V$(I1) = Q$(0): V%(I1) = I1
    NEXT
    CLOSE 1
    NT = Y / 655
1102 SELECTION% = 1
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
20   MAMenu VARPTR(V$(1)), SELECTION%, Start%, Count%, EXITCODE%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%
     CALL HideCursor
     IF EXITCODE% = 27 THEN BO% = 1: RETURN
     IF EXITCODE% = -71 THEN Start% = 1: EXITCODE% = 3: SELECTION% = 1: GOTO 20
     IF EXITCODE% = -79 THEN Start% = NT: EXITCODE% = 3: SELECTION% = NT: GOTO 20
     IF EXITCODE% = -73 THEN Start% = Start% - 80: IF Start% < 1 THEN Start% = 1
     IF EXITCODE% = -81 THEN Start% = Start% + 80: IF Start% > NT THEN Start% = NT
     IF EXITCODE% = -81 OR EXITCODE% = -73 THEN EXITCODE% = 3: SELECTION% = Start%: GOTO 20
     D = V%(SELECTION%)
     GOTO 9000

1200 GOSUB 3000
     OPEN D$(0) + "BASK." + YN$(0) FOR RANDOM AS #1 LEN = 655
     Y = LOF(1)
     FIELD #1, 15 AS Q$(0), 640 AS X$
     FOR I1 = 1 TO Y / 655
     GET #1, I1
     V$(I1) = Q$(0): V%(I1) = I1
    NEXT
    CLOSE 1
    NT = Y / 655
1202 SELECTION% = 1
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
21   MAMenu VARPTR(V$(1)), SELECTION%, Start%, Count%, EXITCODE%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%
     CALL HideCursor
     IF EXITCODE% = 27 THEN BO% = 1: RETURN
     IF EXITCODE% = -71 THEN Start% = 1: EXITCODE% = 3: SELECTION% = 1: GOTO 21
     IF EXITCODE% = -79 THEN Start% = NT: EXITCODE% = 3: SELECTION% = NT: GOTO 21
     IF EXITCODE% = -73 THEN Start% = Start% - 80: IF Start% < 1 THEN Start% = 1
     IF EXITCODE% = -81 THEN Start% = Start% + 80: IF Start% > NT THEN Start% = NT
     IF EXITCODE% = -81 OR EXITCODE% = -73 THEN EXITCODE% = 3: SELECTION% = Start%: GOTO 21
     T = V%(SELECTION%)
     GOTO 9100
2000 COLOR 15, 0: CLS
    JJ = 7
    LOCATE 6, 8: COLOR 15, 4
    PRINT " Ö"; STRING$(61, 196); "· "; : FOR II = 1 TO JJ: LOCATE 6 + II, 8
    PRINT " º"; STRING$(61, 32); "º "; : NEXT
    LOCATE 7 + JJ, 8
    PRINT " Ó"; STRING$(61, 196); "½ "; : LOCATE 7, 32
    LOCATE , 12: PRINT "ENTER DISK ID: ": PRINT
    LOCATE , 12: PRINT "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
    LOCATE , 12: PRINT "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
    LOCATE 7, 27: INPUT NY$
    IF NY$ = "" THEN GOSUB 1102: GOTO 2001
    YN$(0) = NY$
    FOR I = 1 TO 500: V$(I) = "": NEXT
    GOSUB 1200
2001 IF BO% = 1 THEN BO% = 0: GOTO 2000
2002 LOCATE 2, 33: COLOR T%(0, 0), T%(0, 1): PRINT T$(0): COLOR 15, 4
    LOCATE , 33: PRINT "ANY CHANGE (YN)"; : I$ = INKEY$: IF I$ = "" THEN 2002
    IF UCASE$(I$) = "Y" THEN 2000
    GOTO 450

3000 CLS
     IF UCASE$(LEFT$(D$(0), 1)) <> "A" AND UCASE$(LEFT$(D$(0), 1)) <> "B" THEN 3006
     GOSUB 4500
     U$ = "INSERT TEAMS DISK IN DRIVE " + D$(0)
     LOCATE , (80 - LEN(U$)) / 2
     PRINT U$
     U$ = "HIT ANY KEY TO CONTINUE"
     LOCATE , (80 - LEN(U$)) / 2
     PRINT U$
     COLOR 15, 0
3005 I$ = INKEY$: IF I$ = "" THEN 3005
3006 RETURN

4000 I$ = INKEY$: IF I$ = "" THEN 4000
4001 IF UCASE$(I$) = "Y" THEN I = 1: GOTO 4005
4002 IF UCASE$(I$) = "N" THEN I = 0: GOTO 4005
4003 GOTO 4000
4005 RETURN
4500 JJ = 4
     LOCATE 6, 8: COLOR 15, 1
     PRINT " "; CHR$(201); STRING$(61, 205); CHR$(187); " ";
     FOR II = 1 TO JJ: LOCATE 6 + II, 8
     PRINT " º"; STRING$(61, 32); "º "; : NEXT
     LOCATE 7 + JJ, 8
     PRINT " "; CHR$(200); STRING$(61, 205); CHR$(188); " "; : LOCATE 7
     RETURN


9000 OPEN D$(0) + "BASK." + YN$(1) FOR RANDOM AS #1 LEN = 655
FIELD #1, 15 AS Q$(0), 2 AS Q$(1), 2 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 2 AS Q$(14), 2 AS Q$(15), 2 AS Q$(16), 2 AS Q$(17), 2 AS Q$(18), 2 _
 AS Q$(19), 602 AS X$
FOR I = 0 TO 13
FIELD #1, 53 + I * 43 AS X$, 15 AS Q$(I * 15 + 20), 2 AS Q$(I * 15 + 21), 2 AS Q$(I * 15 + 22), 2 AS Q$(I * 15 + 23), 2 AS Q$(I * 15 + 24), 2 AS Q$(I * 15 + 25), 2 AS Q$(I * 15 + 26), 2 AS Q$(I * 15 + 27), 2 AS Q$(I * 15 + 28), 2 AS Q$(I * 15 + 29) _
, 2 AS Q$(I * 15 + 30), 2 AS Q$(I * 15 + 31), 2 AS Q$(I * 15 + 32), 2 AS Q$(I * 15 + 33), 2 AS Q$(I * 15 + 34), 655 - 53 - 43 - I * 43 AS X$
NEXT
     GET #1, D
     T$(1) = Q$(0)
9005 IF RIGHT$(T$(1), 1) = " " THEN T$(1) = LEFT$(T$(1), LEN(T$(1)) - 1): GOTO 9005
     SP(1) = CVI(Q$(1)): DA(1) = CVI(Q$(2))
     V1%(1) = CVI(Q$(3)): V2%(1) = CVI(Q$(4))
     FOR I = 0 TO 4: L%(1, I) = CVI(Q$(I + 5)): NEXT
     FOR I = 0 TO 9: T%(1, I) = CVI(Q$(I + 10)): NEXT
     FOR I = 0 TO 13
     A$(1, I) = Q$(I * 15 + 20)
9007 IF RIGHT$(A$(1, I), 1) = " " THEN A$(1, I) = LEFT$(A$(1, I), LEN(A$(1, I)) - 1): GOTO 9007
     FOR I1 = 0 TO 6
     A(1, I, I1) = CVI(Q$(I * 15 + I1 + 21))
     X(1, I, I1) = CVI(Q$(I * 15 + I1 + 28))
     NEXT: NEXT
     CLOSE 1
     IF V2%(1) > 20 THEN V2%(1) = 100 - V2%(1)
     IF V1%(1) > 20 THEN V1%(1) = 100 - V1%(1)
     IF L%(1, 0) > 20 THEN L%(1, 0) = 100 - L%(1, 0)
     IF L%(1, 2) > 20 THEN L%(1, 2) = 100 - L%(1, 2)
     IF L%(1, 3) > 20 THEN L%(1, 3) = 100 - L%(1, 3)
OPEN D$(0) + "COLBBTMS." + YN$(1) FOR RANDOM AS #1 LEN = 566
FIELD #1, 15 AS Q$(0), 40 AS Q$(1), 15 AS Q$(2), 4 AS Q$(3)
FOR I = 0 TO 13
FIELD #1, 74 + I * 32 AS X$, 2 AS Q$(I * 16 + 4), 2 AS Q$(I * 16 + 5), 2 AS Q$(I * 16 + 6), 2 AS Q$(I * 16 + 7), 2 AS Q$(I * 16 + 8), 2 AS Q$(I * 16 + 9), 2 AS Q$(I * 16 + 10), 2 AS Q$(I * 16 + 11), 2 AS Q$(I * 16 + 12), 2 AS Q$(I * 16 + 13), 2 AS  _
Q$(I * 16 + 14), 2 AS Q$(I * 16 + 15), 2 AS Q$(I * 16 + 16), 2 AS Q$(I * 16 + 17), 2 AS Q$(I * 16 + 18), 2 AS Q$(I * 16 + 19)
FIELD #1, 522 AS X$, 2 AS Q$(228), 2 AS Q$(229), 2 AS Q$(230), 2 AS Q$(231), 2 AS Q$(232), 2 AS Q$(233), 2 AS Q$(234), 2 AS Q$(235), 2 AS Q$(236), 2 AS Q$(237), 2 AS Q$(238), 2 AS Q$(239), 2 AS Q$(240), 2 AS Q$(241), 2 AS Q$(242), 2 AS Q$(243), 2 AS _
 Q$(244), 2 AS Q$(245), 2 AS Q$(246), 2 AS Q$(247), 2 AS Q$(248), 2 AS Q$(249)
NEXT
GET #1, D
     C$(1) = Q$(0)
     S$(1) = Q$(1)
     A1$(1) = Q$(2)
     ATT(1) = CVL(Q$(3))

     FOR I = 0 TO 13
     DF(1, I) = CVI(Q$(I * 16 + 4))
     P$(1, I) = Q$(I * 16 + 5)
     FOR I1 = 0 TO 13
     RS(1, I, I1) = CVI(Q$(I * 16 + I1 + 6))
     NEXT: NEXT
     FOR I = 0 TO 20
     T1%(1, I) = CVI(Q$(228 + I))
     NEXT
     CK(1) = CVI(Q$(249))
     CLOSE 1

      FOR I = 0 TO 13
      TR%(1, I, 1) = SP(1)
      TR%(1, I, 2) = DA(1)
      TR%(1, I, 3) = V2%(1)
      TR%(1, I, 4) = V1%(1)
      TR%(1, I, 5) = L%(1, 0)
      TR%(1, I, 6) = L%(1, 2)
      TR%(1, I, 7) = L%(1, 3)
      TR%(1, I, 8) = 99
      TR%(1, I, 9) = T%(1, 3)
      TR%(1, I, 10) = T%(1, 4)
      TR%(1, I, 11) = T%(1, 5)
      TR%(1, I, 12) = T%(1, 6)
      NEXT
IF CK(1) <> 999 THEN
   C$(1) = "": S$(1) = "": A1$(1) = "": ATT(1) = 0
   FOR I = 0 TO 13: DF(1, I) = 5: P$(1, I) = ""
   FOR I1 = 0 TO 13: RS(1, I, I1) = 0
   NEXT: NEXT
   FOR I = 0 TO 20: T1%(1, I) = 0: NEXT
END IF

9010 RETURN


9100 OPEN D$(0) + "BASK." + YN$(0) FOR RANDOM AS #1 LEN = 655
FIELD #1, 15 AS Q$(0), 2 AS Q$(1), 2 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 2 AS Q$(14), 2 AS Q$(15), 2 AS Q$(16), 2 AS Q$(17), 2 AS Q$(18), 2 _
 AS Q$(19), 602 AS X$
FOR I = 0 TO 13
FIELD #1, 53 + I * 43 AS X$, 15 AS Q$(I * 15 + 20), 2 AS Q$(I * 15 + 21), 2 AS Q$(I * 15 + 22), 2 AS Q$(I * 15 + 23), 2 AS Q$(I * 15 + 24), 2 AS Q$(I * 15 + 25), 2 AS Q$(I * 15 + 26), 2 AS Q$(I * 15 + 27), 2 AS Q$(I * 15 + 28), 2 AS Q$(I * 15 + 29) _
, 2 AS Q$(I * 15 + 30), 2 AS Q$(I * 15 + 31), 2 AS Q$(I * 15 + 32), 2 AS Q$(I * 15 + 33), 2 AS Q$(I * 15 + 34), 655 - 53 - 43 - I * 43 AS X$
NEXT
     GET #1, T
     T$(0) = Q$(0)
9105 IF RIGHT$(T$(0), 1) = " " THEN T$(0) = LEFT$(T$(0), LEN(T$(0)) - 1): GOTO 9105
     SP(0) = CVI(Q$(1)): DA(0) = CVI(Q$(2))
     V1%(0) = CVI(Q$(3)): V2%(0) = CVI(Q$(4))
     FOR I = 0 TO 4: L%(0, I) = CVI(Q$(I + 5)): NEXT
     FOR I = 0 TO 9: T%(0, I) = CVI(Q$(I + 10)): NEXT
     FOR I = 0 TO 13
     A$(0, I) = Q$(I * 15 + 20)
9107 IF RIGHT$(A$(0, I), 1) = " " THEN A$(0, I) = LEFT$(A$(0, I), LEN(A$(0, I)) - 1): GOTO 9107
     FOR I1 = 0 TO 6
     A(0, I, I1) = CVI(Q$(I * 15 + I1 + 21))
     X(0, I, I1) = CVI(Q$(I * 15 + I1 + 28))
     NEXT: NEXT
     CLOSE 1
     IF V1%(0) > 20 THEN V1%(0) = 100 - V1%(0)
     IF V2%(0) > 20 THEN V2%(0) = 100 - V2%(0)
     IF L%(0, 0) > 20 THEN L%(0, 0) = 100 - L%(0, 0)
     IF L%(0, 2) > 20 THEN L%(0, 2) = 100 - L%(0, 2)
     IF L%(0, 3) > 20 THEN L%(0, 3) = 100 - L%(0, 3)
OPEN D$(0) + "COLBBTMS." + YN$(0) FOR RANDOM AS #1 LEN = 566
FIELD #1, 15 AS Q$(0), 40 AS Q$(1), 15 AS Q$(2), 4 AS Q$(3)
FOR I = 0 TO 13
FIELD #1, 74 + I * 32 AS X$, 2 AS Q$(I * 16 + 4), 2 AS Q$(I * 16 + 5), 2 AS Q$(I * 16 + 6), 2 AS Q$(I * 16 + 7), 2 AS Q$(I * 16 + 8), 2 AS Q$(I * 16 + 9), 2 AS Q$(I * 16 + 10), 2 AS Q$(I * 16 + 11), 2 AS Q$(I * 16 + 12), 2 AS Q$(I * 16 + 13), 2 AS  _
Q$(I * 16 + 14), 2 AS Q$(I * 16 + 15), 2 AS Q$(I * 16 + 16), 2 AS Q$(I * 16 + 17), 2 AS Q$(I * 16 + 18), 2 AS Q$(I * 16 + 19)
FIELD #1, 522 AS X$, 2 AS Q$(228), 2 AS Q$(229), 2 AS Q$(230), 2 AS Q$(231), 2 AS Q$(232), 2 AS Q$(233), 2 AS Q$(234), 2 AS Q$(235), 2 AS Q$(236), 2 AS Q$(237), 2 AS Q$(238), 2 AS Q$(239), 2 AS Q$(240), 2 AS Q$(241), 2 AS Q$(242), 2 AS Q$(243), 2 AS _
 Q$(244), 2 AS Q$(245), 2 AS Q$(246), 2 AS Q$(247), 2 AS Q$(248), 2 AS Q$(249)
NEXT
GET #1, T
     C$(0) = Q$(0)
     S$(0) = Q$(1)
     A1$(0) = Q$(2)
     ATT(0) = CVL(Q$(3))

     FOR I = 0 TO 13
     DF(0, I) = CVI(Q$(I * 16 + 4))
     P$(0, I) = Q$(I * 16 + 5)
     FOR I1 = 0 TO 13
     RS(0, I, I1) = CVI(Q$(I * 16 + I1 + 6))
     NEXT: NEXT
     FOR I = 0 TO 20
     T1%(0, I) = CVI(Q$(228 + I))
     NEXT
     CK(0) = CVI(Q$(249))
     CLOSE 1

      FOR I = 0 TO 13
      TR%(0, I, 1) = SP(0)
      TR%(0, I, 2) = DA(0)
      TR%(0, I, 3) = V2%(0)
      TR%(0, I, 4) = V1%(0)
      TR%(0, I, 5) = L%(0, 0)
      TR%(0, I, 6) = L%(0, 2)
      TR%(0, I, 7) = L%(0, 3)
      TR%(0, I, 8) = 99
      TR%(0, I, 9) = T%(0, 3)
      TR%(0, I, 10) = T%(0, 4)
      TR%(0, I, 11) = T%(0, 5)
      TR%(0, I, 12) = T%(0, 6)
      NEXT
IF CK(0) <> 999 THEN
   C$(0) = "": S$(0) = "": A1$(0) = "": ATT(0) = 0
   FOR I = 0 TO 13: DF(0, I) = 5: P$(0, I) = ""
   FOR I1 = 0 TO 13: RS(0, I, I1) = 0
   NEXT: NEXT
   FOR I = 0 TO 20: T1%(0, I) = 0: NEXT
END IF

9110 RETURN


9800 NP = 0
     FOR I = 0 TO 13
     IF A$(0, I) = "XXX" THEN 9801
     NP = NP + 1
     SP(0) = SP(0) + TR%(0, I, 1)
     DA(0) = DA(0) + TR%(0, I, 2)
     V2%(0) = V2%(0) + TR%(0, I, 3)
     V1%(0) = V1%(0) + TR%(0, I, 4)
     L%(0, 0) = L%(0, 0) + TR%(0, I, 5)
     L%(0, 2) = L%(0, 2) + TR%(0, I, 6)
     L%(0, 3) = L%(0, 3) + TR%(0, I, 7)
     T%(0, 3) = T%(0, 3) + TR%(0, I, 9)
     T%(0, 4) = T%(0, 4) + TR%(0, I, 10)
     T%(0, 5) = T%(0, 5) + TR%(0, I, 11)
     T%(0, 6) = T%(0, 6) + TR%(0, I, 12)
9801 NEXT
     SP(0) = SP(0) / (NP - 1)
     DA(0) = DA(0) / (NP - 1)
     V2%(0) = V2%(0) / (NP - 1)
     IF V2%(0) > 20 THEN V2%(0) = 100 - V2%(0)
     V1%(0) = V1%(0) / (NP - 1)
     IF V1%(0) > 20 THEN V1%(0) = 100 - V1%(0)
     L%(0, 0) = L%(0, 0) / (NP - 1)
     IF L%(0, 0) > 20 THEN L%(0, 0) = 100 - L%(0, 0)
     L%(0, 2) = L%(0, 2) / (NP - 1)
     IF L%(0, 2) > 20 THEN L%(0, 2) = 100 - L%(0, 2)
     L%(0, 3) = L%(0, 3) / (NP - 1)
     IF L%(0, 3) > 20 THEN L%(0, 3) = 100 - L%(0, 3)
     L%(0, 4) = 99
     T%(0, 3) = T%(0, 3) / (NP - 1)
     T%(0, 4) = T%(0, 4) / (NP - 1)
     T%(0, 5) = T%(0, 5) / (NP - 1)
     T%(0, 6) = T%(0, 6) / (NP - 1)

     OPEN D$(0) + "BASK." + YN$(0) FOR RANDOM AS #1 LEN = 655
     Y = LOF(1)
     IF DNT% = 0 THEN T = Y / 655 + 1
FIELD #1, 15 AS Q$(0), 2 AS Q$(1), 2 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 2 AS Q$(14), 2 AS Q$(15), 2 AS Q$(16), 2 AS Q$(17), 2 AS Q$(18), 2 _
 AS Q$(19), 602 AS X$
FOR I = 0 TO 13
FIELD #1, 53 + I * 43 AS X$, 15 AS Q$(I * 15 + 20), 2 AS Q$(I * 15 + 21), 2 AS Q$(I * 15 + 22), 2 AS Q$(I * 15 + 23), 2 AS Q$(I * 15 + 24), 2 AS Q$(I * 15 + 25), 2 AS Q$(I * 15 + 26), 2 AS Q$(I * 15 + 27), 2 AS Q$(I * 15 + 28), 2 AS Q$(I * 15 + 29) _
, 2 AS Q$(I * 15 + 30), 2 AS Q$(I * 15 + 31), 2 AS Q$(I * 15 + 32), 2 AS Q$(I * 15 + 33), 2 AS Q$(I * 15 + 34), 655 - 53 - 43 - I * 43 AS X$
NEXT
     LSET Q$(0) = T$(0): LSET Q$(1) = MKI$(SP(0)): LSET Q$(2) = MKI$(DA(0)): LSET Q$(3) = MKI$(V1%(0))
     LSET Q$(4) = MKI$(V2%(0)): FOR I = 0 TO 4: LSET Q$(I + 5) = MKI$(L%(0, I)): NEXT
     T%(0, 2) = 999
     FOR I = 0 TO 9: LSET Q$(I + 10) = MKI$(T%(0, I)): NEXT
     FOR I = 0 TO 13
     LSET Q$(I * 15 + 20) = A$(0, I)
     FOR I1 = 0 TO 6
     LSET Q$(I * 15 + I1 + 21) = MKI$(A(0, I, I1))
     LSET Q$(I * 15 + I1 + 28) = MKI$(X(0, I, I1))
     NEXT: NEXT
     PUT #1, T: CLOSE 1

OPEN D$(0) + "COLBBTMS." + YN$(0) FOR RANDOM AS #1 LEN = 566
FIELD #1, 15 AS Q$(0), 40 AS Q$(1), 15 AS Q$(2), 4 AS Q$(3)
FOR I = 0 TO 13
FIELD #1, 74 + I * 32 AS X$, 2 AS Q$(I * 16 + 4), 2 AS Q$(I * 16 + 5), 2 AS Q$(I * 16 + 6), 2 AS Q$(I * 16 + 7), 2 AS Q$(I * 16 + 8), 2 AS Q$(I * 16 + 9), 2 AS Q$(I * 16 + 10), 2 AS Q$(I * 16 + 11), 2 AS Q$(I * 16 + 12), 2 AS Q$(I * 16 + 13), 2 AS  _
Q$(I * 16 + 14), 2 AS Q$(I * 16 + 15), 2 AS Q$(I * 16 + 16), 2 AS Q$(I_ * 16 + 17), 2 AS Q$(I * 16 + 18), 2 AS Q$(I * 16 + 19)
FIELD #1, 522 AS X$, 2 AS Q$(228), 2 AS Q$(229), 2 AS Q$(230), 2 AS Q$(231), 2 AS Q$(232), 2 AS Q$(233), 2 AS Q$(234), 2 AS Q$(235), 2 AS Q$(236), 2 AS Q$(237), 2 AS Q$(238), 2 AS Q$(239), 2 AS Q$(240), 2 AS Q$(241), 2 AS Q$(242), 2 AS Q$(243), 2 AS _
 Q$(244), 2 AS Q$(245), 2 AS Q$(246), 2 AS Q$(247), 2 AS Q$(248), 2 AS Q$(249)
NEXT
     LSET Q$(0) = C$(0)
     LSET Q$(1) = S$(0)
     LSET Q$(2) = A1$(0)
     LSET Q$(3) = MKL$(ATT(0))

     FOR I = 0 TO 13
     LSET Q$(I * 16 + 4) = MKI$(DF(0, I))
     LSET Q$(I * 16 + 5) = P$(0, I)
     FOR I1 = 0 TO 13
     LSET Q$(I * 16 + I1 + 6) = MKI$(RS(0, I, I1))
     NEXT: NEXT
     FOR I = 0 TO 20
     LSET Q$(228 + I) = MKI$(T1%(0, I))
     NEXT
     LSET Q$(249) = MKI$(999)
     PUT #1, T: CLOSE 1

9821 RETURN

SUB TITLE ()
   COLOR , 0: KEY OFF: CLS
   COLOR , 1: LOCATE 1, 1: PRINT TAB(80); : LOCATE , 22: COLOR 12
   PRINT "COURTSIDE COLLEGE BASKETBALL V4.0: DRAFT"
   LOCATE 24, 1: PRINT TAB(80); : LOCATE , 1
   PRINT "(C) LANCE HAFFNER/SHANNON LYNN 1987-1997"; TAB(70); LEFT$(TIME$, 5);
   LOCATE 3, 1: COLOR 2, 0
   COLOR 15: PRINT "        PROGRAMMED AND DEVELOPED BY LANCE HAFFNER AND SHANNON LYNN"
             PRINT "     FOR TECHNICAL SUPPORT, CALL (800) 477-7032 OR SportGames@AOL.COM"
   COLOR 10
END SUB

