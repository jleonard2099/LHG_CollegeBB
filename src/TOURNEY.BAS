DECLARE FUNCTION EXIST% (FILENAME$)
DECLARE SUB MAMenu (BYVAL Address%, SELECTION%, Start%, Count%, EXITCODE%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%)
DECLARE FUNCTION QPRTrim$ (Work$)
DECLARE FUNCTION FCount% (FSpec$)
DECLARE SUB ReadFile (BYVAL Address%)
DECLARE SUB SortStr (BYVAL Address%, Size%, Direction%)
  '$INCLUDE: 'DEFCNF.BI'
  '$INCLUDE: 'SetCnf.BI'          'queries the system info for MsgBox
  DRIVE = GetDrive%
   DIM VV$(500)
   DIM A$(11), A(11, 6), Q$(375), YN$(500, 3), AP%(1), NG%(500, 17), T(34), NGG(500)
   DIM SC$(2), D$(1), TS$(1), U5$(3), LC$(2), CM$(1), TE$(1), TF$(1)
   REDIM MON%(500), DY%(500), yr%(500), H$(500), V$(500), HS%(500), VS%(500)
   OPEN "DEV.BAS" FOR INPUT AS #1: INPUT #1, D$(0): CLOSE 1
   U5$(0) = "HUMAN OPPONENT            "
   U5$(1) = "COMPUTER IS VISITING TEAM "
   U5$(2) = "COMPUTER IS HOME TEAM     "
   U5$(3) = "COMPUTER VS COMPUTER      "
   LC$(1) = "HOME COURT ADVANTAGE"
   LC$(2) = "NEUTRAL SITE        "
   CM$(0) = "COACH AND SELECT SHOTS           "
   CM$(1) = "COACH ONLY AND LET COMPUTER SHOOT"
   TE$(0) = "SHOOT 1+1"
   TE$(1) = "SHOOT 2  "
   TF$(0) = "N": TF$(1) = "Y"
10 Spec$ = D$(0) + "*.TOU"
     Count% = FCount%(Spec$)
     IF Count% = 0 THEN 6500
     REDIM F$(0 TO Count%)
     FOR X = 1 TO Count%
     F$(X) = SPACE$(12)
     NEXT
     F$(0) = Spec$
     CALL ReadFile(VARPTR(F$(0)))
     CALL SortStr(VARPTR(F$(1)), Count%, 0)
     nf% = Count%
6500 REM:



   COLOR 7, 0: CLS
   NG%(0, 8) = 0
   NG%(0, 3) = 1
   NG%(0, 7) = 0
   NG%(0, 2) = 1
   NG%(0, 5) = 5
   YN$(0, 0) = ""
   YN$(0, 1) = ""
   FOR I = 1 TO 500
   V$(I) = "": NG%(I, 13) = 0
   H$(I) = ""
   NEXT
   YN$(0, 3) = "10-01-91"
   YN$(0, 2) = "07:30P"
   
   TS$(0) = "N": TS$(1) = "Y"
   SC$(0) = "NO SHOT CLOCK"
   SC$(1) = "45 SEC CLOCK "
   SC$(2) = "35 SEC CLOCK "

600 REM *** LOAD IN A SAVED SCHEDULE ***
    GOTO 2100

605 OPEN D$(0) + F$ + ".TOU" FOR RANDOM AS #1 LEN = 90
    Y = LOF(1)
    CLOSE 1
    AP%(0) = Y / 90
    BS% = INT((AP%(0) - 1) / 20)
    FOR I = 1 TO AP%(0)
    OPEN D$(0) + F$ + ".TOU" FOR RANDOM AS #1 LEN = 90
    FOR X = 0 TO 14
    FIELD #1, X * 2 AS X$, 2 AS Q$(X + 1), 90 - 2 - 2 * X AS X$
    NEXT
    FIELD #1, 30 AS X$, 2 AS Q$(16), 2 AS Q$(17), 15 AS Q$(18), 15 AS Q$(19), 26 AS X$
    FIELD #1, 64 AS X$, 3 AS Q$(20), 3 AS Q$(21), 6 AS Q$(22), 8 AS Q$(23), 6 AS X$
    FIELD #1, 84 AS X$, 2 AS Q$(24), 2 AS Q$(25), 2 AS Q$(26)
    GET #1, I
    FOR X = 0 TO 14: NG%(I, X) = CVI(Q$(X + 1)): NEXT
    FOR X = 0 TO 3: YN$(I, X) = Q$(20 + X): NEXT
    VS%(I) = CVI(Q$(16)): HS%(I) = CVI(Q$(17))
    V$(I) = Q$(18): H$(I) = Q$(19)
    FOR X = 1 TO 3: NG%(I, X + 14) = CVI(Q$(X + 23)): NEXT
    FOR X = 2 TO 3
610 IF RIGHT$(YN$(I, X), 1) = " " THEN YN$(I, X) = LEFT$(YN$(I, X), LEN(YN$(I, X)) - 1): GOTO 610
    NEXT
    CLOSE 1: NEXT


25 CLS
   LOCATE 23, 1: COLOR , 1: PRINT TAB(80); : LOCATE 23, 1
   COLOR 13: PRINT "SC"; : COLOR 14: PRINT "-SHOT CLOCK  ";
   COLOR 13: PRINT "HC"; : COLOR 14: PRINT "-HOME COURT ADVANTAGE  ";
   COLOR 13: PRINT "3"; : COLOR 14: PRINT "-USE 3-POINT SHOT  ";
   LOCATE 24, 1: COLOR , 1: PRINT TAB(80); : LOCATE 24, 1
   COLOR 11: PRINT "PGUP  PGDN  ";
   COLOR 13: PRINT "REG"; : COLOR 14: PRINT "-REGION  ";
   COLOR 13: PRINT "RND"; : COLOR 14: PRINT "-ROUND  ";
   COLOR 13: PRINT "GM"; : COLOR 14: PRINT "-GAME  ";
   LOCATE 25, 1: COLOR , 1: PRINT TAB(80); : LOCATE 25, 1
   COLOR 11: PRINT "P"; : COLOR 14: PRINT "-PLAY TOURNAMENT   ";
   COLOR 11: PRINT "Q"; : COLOR 14: PRINT "-QUIT  ";
   COLOR 11: PRINT "E"; : COLOR 14: PRINT "-EDIT A GAME  ";
   LOCATE 1, 1: PRINT TAB(80); : LOCATE 1, 1: PRINT F$: LOCATE 1, 33: COLOR 14: PRINT "AUTO SCHEDULER"
   COLOR , 0
50 LOCATE 2, 1
   COLOR 14
   PRINT "GM ID#  REG  RND  GM    VISITING TEAM        HOME TEAM            HC  3  SC": COLOR 15
   FOR X = 1 TO 20
   LOCATE X + 2, 1
   IF X + NS% * 20 > AP%(0) THEN 100
   PRINT USING "###      ##   ##   ##"; X + NS% * 20; NG%(X + NS% * 20, 15); NG%(X + NS% * 20, 16); NG%(X + NS% * 20, 17);
   'LOCATE , 17: PRINT YN$(X + NS% * 20, 2);
   LOCATE , 25: PRINT V$(X + NS% * 20); " ";
   IF NG%(X + NS% * 20, 13) = 1 THEN PRINT USING "###"; VS%(X + NS% * 20);
   LOCATE , 46: PRINT H$(X + NS% * 20); " ";
   IF NG%(X + NS% * 20, 13) = 1 THEN PRINT USING "###"; HS%(X + NS% * 20);
   LOCATE , 67: IF NG%(X + NS% * 20, 2) = 1 THEN PRINT "Y"; :  ELSE PRINT "N";
   LOCATE , 71: PRINT TS$(NG%(X + NS% * 20, 3));
   LOCATE , 74: PRINT LEFT$(SC$(NG%(X + NS% * 20, 7)), 2)
   NEXT
100 I$ = INKEY$: IF I$ = "" THEN 100
   IF I$ = CHR$(0) + "Q" THEN GOSUB 130: GOTO 25
   IF I$ = CHR$(0) + "I" THEN GOSUB 135: GOTO 25
   IF UCASE$(I$) = "E" THEN GOSUB 140: GOTO 25
   IF UCASE$(I$) = "Q" THEN RUN "ADDTOURN.EXE"
   IF UCASE$(I$) = "P" THEN GOSUB 350: GOTO 300
   GOTO 100
130 REM *** PAGE DOWN ***
    IF NS% >= BS% THEN NS% = BS%: GOTO 133
    NS% = NS% + 1
133 RETURN
135 REM *** PAGE UP ***
    IF NS% = 0 THEN 137
    NS% = NS% - 1
137 RETURN

140 REM *** EDIT A GAME ***
    LOCATE 23, 1: PRINT "ENTER GAME # TO EDIT        "
    LOCATE 23, 1: INPUT "ENTER GAME # TO EDIT "; NG%
    IF NG% < 1 OR NG% > AP%(0) THEN 140
    V$ = V$(NG%)
    H$ = H$(NG%)
    GOTO 149

149 CLS : COLOR 2: PRINT TAB(29); "SELECT GAME OPTIONS": PRINT
151 LOCATE 2, 2: COLOR 14: PRINT "    "; : COLOR 15: PRINT "VISITING TEAM: "; V$
152 LOCATE 3, 2: COLOR 14: PRINT "    "; : COLOR 15: PRINT "HOME TEAM: "; H$
153 LOCATE 4, 2: COLOR 14: PRINT "(1) "; : COLOR 15: PRINT U5$(NG%(NG%, 8))
154 LOCATE 5, 2: COLOR 14: PRINT "(2) "; : COLOR 15: PRINT "3-POINT SHOT: "; TS$(NG%(NG%, 3))
155 LOCATE 6, 2: COLOR 14: PRINT "(3) "; : COLOR 15: PRINT "SHOT CLOCK "; SC$(NG%(NG%, 7))
156 LOCATE 7, 2: COLOR 14: PRINT "(4) "; : COLOR 15: PRINT "HOME COURT ADVANTAGE: "; LC$(NG%(NG%, 2))
157 LOCATE 8, 2: COLOR 14: PRINT "(5) "; : COLOR 15: PRINT USING "DISPLAY PAUSE IN SECONDS: ##.#  "; NG%(NG%, 4) / 10
158 LOCATE 9, 2: COLOR 14: PRINT "(6) "; : COLOR 15: PRINT "FOULS TO DISQUALIFY: "; NG%(NG%, 5)
159 LOCATE 10, 2: COLOR 14: PRINT "(7) "; : COLOR 15: PRINT "HUMAN COACHES WILL: "; CM$(NG%(NG%, 9))
162 LOCATE 11, 2: COLOR 14: PRINT "(8) "; : COLOR 15: PRINT "FREE THROWS ON 10TH FOUL "; TE$(NG%(NG%, 6))
164 LOCATE 12, 2: COLOR 14: PRINT "(9) "; : COLOR 15: PRINT "3-FT WHEN FOULED ON 3-PT ATTEMPT "; TF$(NG%(NG%, 10))
165 LOCATE 13, 2: COLOR 14: PRINT "(A) "; : COLOR 15: PRINT "1 FT ON 1ST 6 FLOOR FOULS (PRIOR TO 1973): "; TF$(NG%(NG%, 14))
163 LOCATE 14, 2: COLOR 14: PRINT "(0) "; : COLOR 15: PRINT "ACCEPT SETTINGS"
175 I$ = INKEY$: IF I$ = "" THEN 175
    IF UCASE$(I$) = "A" THEN I$ = "10"
    I8% = VAL(I$): IF I8% < 0 OR I8% > 10 THEN 175
    ON I8% GOTO 180, 184, 185, 186, 187, 188, 189, 192, 193, 194
    RETURN

193 NG%(NG%, 10) = 1 - NG%(NG%, 10): GOTO 149
194 NG%(NG%, 14) = 1 - NG%(NG%, 14): GOTO 149

180 NG%(NG%, 8) = NG%(NG%, 8) + 1: IF NG%(NG%, 8) > 3 THEN NG%(NG%, 8) = 0
    GOTO 151

184 NG%(NG%, 3) = 1 - NG%(NG%, 3): GOTO 154
185 NG%(NG%, 7) = 1 + NG%(NG%, 7)
    IF NG%(NG%, 7) > 2 THEN NG%(NG%, 7) = 0
    GOTO 155

186 N = 1 - (NG%(NG%, 2) - 1): NG%(NG%, 2) = N + 1: GOTO 156

187 LOCATE 8, 32: INPUT U6: NG%(NG%, 4) = U6 * 10: GOTO 157
188 NG%(NG%, 5) = 1 + NG%(NG%, 5): IF NG%(NG%, 5) > 6 THEN NG%(NG%, 5) = 5
    GOTO 158
189 NG%(NG%, 9) = 1 - NG%(NG%, 9): GOTO 159
192 NG%(NG%, 6) = 1 - NG%(NG%, 6): GOTO 162

300 REM *** PLAY GAMES ***
    COLOR , 0: CLS
    JJ = 7
    LOCATE 6, 8: COLOR 15, 4
    PRINT " Ö"; STRING$(61, 196); "· "; : FOR II = 1 TO JJ: LOCATE 6 + II, 8
    PRINT " º"; STRING$(61, 32); "º "; : NEXT
    LOCATE 7 + JJ, 8
    PRINT " Ó"; STRING$(61, 196); "½ "; : LOCATE 7, 32
    LOCATE , 12: PRINT "TOTAL GAMES ="; AP%(0)
    LOCATE , 12: PRINT "SELECT GAME # TO START AND END WITH": PRINT
    LOCATE , 12: INPUT "ENTER START GAME #: "; SG%: PRINT
    LOCATE , 12: INPUT "ENTER END GAME #:"; EG%
    LOCATE , 12: PRINT "ANY CHANGE (YN)"
301 I$ = INKEY$: IF I$ = "" THEN 301
    IF UCASE$(I$) = "Y" THEN 300
    IF SG% <= 0 THEN SG% = 1
    IF EG% > AP%(0) THEN EG% = AP%(0)
    OPEN "TOURNEY" FOR OUTPUT AS #2
    PRINT #2, F$: REM: + ".TNY"
    PRINT #2, SG%
    PRINT #2, EG%
    PRINT #2, 0
    CLOSE 2
    GOSUB 1200
    OPEN "TICKER" FOR OUTPUT AS #2
    FOR I = 1 TO 32: FOR I1 = 0 TO 1: FOR I2 = 1 TO 14
    PRINT #2, -1: NEXT: NEXT: NEXT
    FOR I = 1 TO 32: FOR I1 = 0 TO 1: PRINT #2, "XXX"
    NEXT: NEXT
    FOR I = SG% TO SG% + 31
    PRINT #2, NG%(I, 12)
    NEXT
    CLOSE 2
335 RUN "COLHOOP.EXE"


350 REM *** SAVE SCHEDULE FILE ***
'    IF EXIST%(D$(0) + F$ + ".TNY") THEN
'       KILL D$(0) + F$ + ".TNY"
'    END IF
    
    OPEN D$(0) + F$ + ".TOU" FOR RANDOM AS #1 LEN = 90
    FOR X = 0 TO 14
    FIELD #1, X * 2 AS X$, 2 AS Q$(X + 1), 90 - 2 - 2 * X AS X$
    NEXT
    FIELD #1, 30 AS X$, 2 AS Q$(16), 2 AS Q$(17), 15 AS Q$(18), 15 AS Q$(19), 26 AS X$
    FIELD #1, 64 AS X$, 3 AS Q$(20), 3 AS Q$(21), 6 AS Q$(22), 8 AS Q$(23), 6 AS X$
    FIELD #1, 84 AS X$, 2 AS Q$(24), 2 AS Q$(25), 2 AS Q$(26)
    FOR I = 1 TO AP%(0)
    FOR X = 0 TO 14: LSET Q$(X + 1) = MKI$(NG%(I, X)): NEXT
    FOR X = 0 TO 3: LSET Q$(20 + X) = YN$(I, X): NEXT
    LSET Q$(16) = MKI$(VS%(I)): LSET Q$(17) = MKI$(HS%(I))
    LSET Q$(18) = V$(I): LSET Q$(19) = H$(I)
    FOR X = 1 TO 3: LSET Q$(X + 23) = MKI$(NG%(I, X + 14)): NEXT
    PUT #1, I
    NEXT
    CLOSE 1
    RETURN


1200 FOR I = 1 TO AP%(0)
     L = LEN(YN$(I, 2))
     IF L = 5 THEN HR = VAL(LEFT$(YN$(I, 2), 1)): MIN = VAL(MID$(YN$(I, 2), 3, 2))
     IF L = 6 THEN HR = VAL(LEFT$(YN$(I, 2), 2)): MIN = VAL(MID$(YN$(I, 2), 4, 2))
     MON%(I) = VAL(LEFT$(YN$(I, 3), 2))
     DY%(I) = VAL(MID$(YN$(I, 3), 4, 2))
     yr%(I) = VAL(MID$(YN$(I, 3), 7, 2))
     T$ = RIGHT$(YN$(I, 2), 1)
     IF UCASE$(T$) = "P" AND LEFT$(YN$(I, 2), 2) <> "12" THEN HR = HR + 12
     TM = HR * 60 + MIN
     NGG(I) = INT(TM / 30 - 18): NG%(I, 12) = NGG(I)
     NEXT
     RETURN

2100 CLS
    COLOR , 1
    CLS
    LOCATE , 28
    COLOR 14
    PRINT "SELECT FIELD TO PLAY": COLOR 15
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
    PRINT "-SELECTS TOURNEY";
CALL ShowCursor
320 MAMenu VARPTR(F$(1)), SELECTION%, Start%, Count%, EXITCODE%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%
CALL HideCursor
     IF EXITCODE% = 27 THEN RUN "ADDTOURN.EXE"
     IF EXITCODE% = -71 THEN Start% = 1: EXITCODE% = 3: SELECTION% = 1: GOTO 320
     IF EXITCODE% = -79 THEN Start% = Count%: EXITCODE% = 3: SELECTION% = Count%: GOTO 320
     IF EXITCODE% = -73 THEN Start% = Start% - 80: IF Start% < 1 THEN Start% = 1
     IF EXITCODE% = -81 THEN Start% = Start% + 80: IF Start% > Count% THEN Start% = Count%
     IF EXITCODE% = -81 OR EXITCODE% = -73 THEN EXITCODE% = 3: SELECTION% = Start%: GOTO 320
     F$ = F$(SELECTION%)
     T1 = SELECTION%
   LOCATE 2, 33: PRINT F$: LOCATE , 33: PRINT "ANY CHANGE (YN)";
     COLOR 15, 0


3910 I$ = INKEY$: IF I$ = "" THEN 3910
    IF UCASE$(I$) = "Y" THEN 2100
    F$ = QPRTrim$(F$)
    WE$ = LEFT$(F$, LEN(F$) - 4)
    COLOR 15, 0
    F$ = WE$
    GOTO 605



