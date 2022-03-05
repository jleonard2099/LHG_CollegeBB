DEFINT A-Z
DECLARE SUB TITLE ()
DECLARE FUNCTION FCount% (FSpec$)
DECLARE SUB SortStr (BYVAL Address%, Size%, Direction%)
DECLARE SUB TextIn (T$, Max, NumOnly, CapsOn, EXITCODE, Colr)
DECLARE FUNCTION EXIST (FILENAME$)
DECLARE SUB CSMsgBox (Message$, Wdth, Cnf AS ANY)
DECLARE SUB MQPrint (X$, Colr)
DECLARE SUB ReadFile (BYVAL Address%)
DECLARE SUB MPaintBox (ULRow, ULCol, LRRow, LRCol, Colr)
DECLARE SUB MScrnSave (ULRow, ULCol, LRRow, LRCol, SEG Address)
DECLARE SUB FCopy (Source$, Dest$, Buffer$, ErrCode%)
DECLARE SUB MScrnRest (ULRow, ULCol, LRRow, LRCol, SEG Address)
DECLARE SUB MAMenu (BYVAL Address%, SELECTION%, Start%, Count%, EXITCODE%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%)
DECLARE SUB YesNoB (YN$, EXITCODE%, Colr%)
DECLARE FUNCTION OneColor% (FG, BG)
DECLARE SUB QPrint (X$, Colr, Page)
DECLARE SUB CHAMPIONSHIP ()
DECLARE SUB SETTINGS ()
DECLARE SUB MMenuVert (BYVAL Address%, Choice%, Start%, SCANCODE%, NormClr%, HiLite%, NumRows%, Row%, Column%)
DECLARE SUB Box0 (ULRow%, ULCol%, LRRow%, LRCol%, Char%, Colr%)
DECLARE SUB SORTGAMES ()
DECLARE SUB WRITEGAME ()
DECLARE FUNCTION Peek1% (Segment, Address)
DECLARE FUNCTION QPTRIM$ (X$)
DECLARE SUB ClearEOL (Colr)
DIM NN%(17): REM NUMBER OF TEAMS IN A REGION, UP TO 16 REGIONS
DIM N$(17, 17, 3): REM TEAM NAMES FOR EACH SEED SLOT
DIM R$(17): REM REGIONAL NAMES
DIM D$(3), U6 AS SINGLE, Y AS LONG
DIM OP%(14): REM TOURNAMENT DEFAULTS
DIM Q$(300), VV$(500), V%(500), NG%(300, 17), H$(300), V$(300), HS%(300), VS%(300)
DIM YN$(300, 3), L%(4), AP%(1), A$(9)
DIM N%(17, 17, 3): REM: REGION, SEED NUMBER, TEAM#/MODE OF PLAY (0,1)
DIM C%(1): REM 0-CONSOLATION IN REGIONS, 1-CONSOLATION IN CHAMPIONSHIP
DIM CM$(1), TE$(1), HC$(2), TS$(1), SC$(2), TF$(1), ITEM$(18), T%(9)
'$INCLUDE: 'DEFCNF.BI'
'$INCLUDE: 'SETCNF.BI'          'queries the system info for MsgBox
   OPEN "DEV.BAS" FOR INPUT AS #1
   INPUT #1, D$(0)
   CLOSE 1
10 CALL TITLE
   ITEM$(1) = "VIEW BRACKETS            "
   ITEM$(2) = "VIEW SEEDINGS            "
   ITEM$(3) = "RETURN TO TOURNAMENT MENU"

     SELECTION% = 1
     Count% = 3
     Start% = 1
     SCANCODE% = 3
     FileColr% = 31
     HiLiteColr% = 112
     Spacing% = 1
     Row% = 10
     Column% = 25
     CALL Box0(9, 24, 13, 50, 2, 31)
     CALL SHOWCURSOR
     MMenuVert VARPTR(ITEM$(1)), SELECTION%, Start%, SCANCODE%, FileColr%, HiLiteColr%, Count%, Row%, Column%
     CALL HIDECURSOR
     COLOR , 0
     IF SCANCODE% = 27 THEN SELECTION% = 3
     Q = SELECTION%
     IF Q = 3 THEN RUN "ADDTOURN.EXE"

610 REM *** LOAD IN A SAVED TOURNAMENT ***
   Spec$ = D$(0) + "*.TOU"
   Count = FCount%(Spec$)
   IF Count = 0 THEN 4050
   REDIM F$(0 TO Count)
   FOR X = 1 TO Count
       F$(X) = SPACE$(12)
   NEXT
   F$(0) = Spec$
   CALL ReadFile(VARPTR(F$(0)))
   CALL SortStr(VARPTR(F$(1)), Count, 0)
    CLS
    COLOR , 1: CLS : LOCATE , 28: COLOR 14
    PRINT "SELECT TOURNAMENT FIELD TO VIEW": COLOR 15
     SELECTION% = 1
     Start% = 1
     FileColr% = 31
     HiLiteColr% = 112
     Rows% = 20
     Columns% = 4
     Spacing% = 6
     Row% = 4
     Column% = 1
    LOCATE 25, 1: COLOR 14: PRINT "ESC"; : COLOR 15
    PRINT "-SELECT NEW ID  PGDN  PGUP  "; : COLOR 14
    PRINT "ENTER"; : COLOR 15
    PRINT "-SELECTS TOURNAMENT FIELD";
CALL SHOWCURSOR
MAMenu VARPTR(F$(1)), SELECTION%, Start%, Count%, EXITCODE%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%
CALL HIDECURSOR
     IF EXITCODE% = 27 THEN 10
     F$ = F$(SELECTION%)
   LOCATE 2, 33: PRINT F$: LOCATE , 33: PRINT "ANY CHANGE (YN)";
     COLOR 15, 0
604 I$ = INKEY$: IF I$ = "" THEN 604
    IF UCASE$(I$) = "Y" THEN 610
    F$ = QPTRIM$(F$)
    F$ = LEFT$(F$, LEN(F$) - 4)
     OPEN D$(0) + F$ + ".INF" FOR INPUT AS #2
     INPUT #2, FF$
     INPUT #2, R: REM NUMBER OF REGIONS
     FOR I = 1 TO 16
     INPUT #2, NN%(I): REM NUMBER OF TEAMS IN REGION
     INPUT #2, R$(I): REM NAME OF REGION
     NEXT
     FOR I = 1 TO 16
     FOR I1 = 1 TO 16
     FOR I2 = 0 TO 3
     INPUT #2, N$(I, I1, I2): REM SEED SLOT NAME
     INPUT #2, N%(I, I1, I2): REM REGION, SEED NUMBER, MODE PLAY/TM#
     NEXT: NEXT: NEXT
     INPUT #2, C%(0): INPUT #2, C%(1): REM CONSOLATION GAME
     CLOSE 2
     OPEN D$(0) + F$ + ".TNY" FOR INPUT AS #1
     FOR I = 0 TO 14
     INPUT #1, OP%(I)
     NEXT
     CLOSE 1
     IF C%(0) = 1 THEN C$ = "Y" ELSE C$ = "N"
     NR$ = STR$(R)
     OPEN D$(0) + F$ + ".TOU" FOR RANDOM AS #1 LEN = 90
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
611 IF RIGHT$(YN$(I, X), 1) = " " THEN YN$(I, X) = LEFT$(YN$(I, X), LEN(YN$(I, X)) - 1): GOTO 611
    NEXT
    CLOSE 1: NEXT

35 REM:EDIT A GIVEN BRACKET
    COLOR , 0: CLS
    COLOR , 6: LOCATE 1, 1: PRINT TAB(80); : LOCATE , 23: COLOR 15
    PRINT "TOURNAMENT  - REGION/BRACKET EDITOR"
    LOCATE 25, 1: PRINT TAB(80); : LOCATE , 1: COLOR , 0
   FOR I = 1 TO R
   IF R$(I) = "" THEN R$(I) = "TO BE NAMED"
   ITEM$(I) = R$(I)
   NEXT

     SELECTION% = 1
     Count% = R + 2
     Start% = 1
     SCANCODE% = R + 2
     FileColr% = 15
     HiLiteColr% = 112
     Spacing% = 1
     Row% = 4
     Column% = 32
     ITEM$(R + 1) = "CHAMPIONSHIP BRACKET"
     ITEM$(R + 2) = "EXIT                "
     CALL Box0(3, 31, 3 + R + 3, 52, 2, 15)
     CALL SHOWCURSOR
     MMenuVert VARPTR(ITEM$(1)), SELECTION%, Start%, SCANCODE%, FileColr%, HiLiteColr%, Count%, Row%, Column%
     CALL HIDECURSOR
     COLOR , 0
     IF SCANCODE% = 27 THEN 10
     NR = SELECTION%
     IF NR = R + 1 THEN NR = 17
     IF NR = R + 2 THEN 10
     IF Q = 2 THEN 21
N = NR

COLOR , 0

CLS
R$ = R$(N)
Y = INT((80 - LEN(R$)) / 2): LOCATE 1, Y: PRINT R$
LOCATE 2
   IF N = 17 THEN 50
   ON NN%(N) - 3 GOTO 50, 47, 46, 45, 40, 36, 34, 32, 30, 27, 25, 22, 20
20 REM:16 TEAM BRACKET
   REG = N: GAME = 1: GOSUB 300
   GOSUB 350
   GOSUB 360
   PRINT
   REG = N: GAME = 8: GOSUB 300
   GOSUB 350
   GOSUB 360
   PRINT
   REG = N: GAME = 4: GOSUB 300
   GOSUB 350
   GOSUB 360
   PRINT
   REG = N: GAME = 5: GOSUB 300
   GOSUB 350
   GOSUB 360
   PRINT
   REG = N: GAME = 3: GOSUB 300
   GOSUB 350
   GOSUB 360
   PRINT
   REG = N: GAME = 6: GOSUB 300
   GOSUB 350
   GOSUB 360
   PRINT
   REG = N: GAME = 2: GOSUB 300
   GOSUB 350
   GOSUB 360
   PRINT
   REG = N: GAME = 7: GOSUB 300
   GOSUB 350
   GOSUB 360
   COLOR 15
   FOR I = 0 TO 7
   IF I / 2 = INT(I / 2) THEN LOCATE 3 * I + 2, 22: PRINT A$(I + 1)
   IF I / 2 <> INT(I / 2) THEN LOCATE 3 * I + 1, 22: PRINT A$(I + 1)
   IF I / 2 = INT(I / 2) THEN LOCATE 3 * I + 3, 21: PRINT STRING$(16, CHR$(196)); CHR$(191)
   IF I / 2 = INT(I / 2) THEN LOCATE 3 * I + 4, 37: PRINT CHR$(195)
   IF I / 2 <> INT(I / 2) THEN LOCATE 3 * I + 2, 21: PRINT STRING$(16, CHR$(196)); CHR$(217)
   NEXT
   COLOR 7
   GOSUB 200
   REG = N: GAME = 9: GOSUB 300
   LOCATE 2, 21: GOSUB 350
   LOCATE 4, 21: GOSUB 360
   REG = N: GAME = 10: GOSUB 300
   LOCATE 8, 21: GOSUB 350
   LOCATE 10, 21: GOSUB 360
   REG = N: GAME = 11: GOSUB 300
   LOCATE 14, 21: GOSUB 350
   LOCATE 16, 21: GOSUB 360
   REG = N: GAME = 12: GOSUB 300
   LOCATE 20, 21: GOSUB 350
   LOCATE 22, 21: GOSUB 360
   REG = N: GAME = 13: GOSUB 300
   LOCATE 3, 39: GOSUB 350
   LOCATE 9, 39: GOSUB 360
   REG = N: GAME = 14: GOSUB 300
   LOCATE 15, 39: GOSUB 350
   LOCATE 21, 39: GOSUB 360
   REG = N: GAME = 15 + C%(0): GOSUB 300
   LOCATE 6, 56: GOSUB 350
   LOCATE 18, 56: GOSUB 360
   GOTO 370

21  CLS
    COLOR , 0: CLS
    COLOR , 6: LOCATE 1, 1: PRINT TAB(80); : LOCATE , 23: COLOR 15
    PRINT "TOURNAMENT  - REGION/BRACKET EDITOR"
    LOCATE 25, 1: PRINT TAB(80); : LOCATE , 1: COLOR , 0
    MAXSEED = NN%(NR) + 1 + 4
    CALL Box0(3, 24, MAXSEED, 54, 2, 15)
    LOCATE 4, 25: PRINT "SEED  TEAM"
    FOR I1 = 1 TO NN%(NR)
    COLOR 2
    LOCATE , 25
    PRINT USING "##    "; I1; : COLOR 15: PRINT N$(NR, I1, 0)
    NEXT I1
    GOSUB 5000: GOTO 35

22 REM:15 TEAM BRACKET
   REG = N: GAME = 1: GOSUB 300
   GOSUB 350
   GOSUB 360
   PRINT
   REG = N: GAME = 6: GOSUB 300
   GOSUB 350
   GOSUB 360
   PRINT
   REG = N: GAME = 2: GOSUB 300
   GOSUB 350
   GOSUB 360
   PRINT
   REG = N: GAME = 5: GOSUB 300
   GOSUB 350
   GOSUB 360
   PRINT
   REG = N: GAME = 3: GOSUB 300
   GOSUB 350
   GOSUB 360
   PRINT
   REG = N: GAME = 4: GOSUB 300
   GOSUB 350
   GOSUB 360
   PRINT
   REG = N: GAME = 7: GOSUB 300
   GOSUB 350
   GOSUB 360
   FOR I = 0 TO 7
   IF I / 2 = INT(I / 2) THEN LOCATE 3 * I + 2, 22: PRINT A$(I + 1)
   IF I / 2 <> INT(I / 2) THEN LOCATE 3 * I + 1, 22: PRINT A$(I + 1)
   IF I / 2 = INT(I / 2) THEN LOCATE 3 * I + 3, 21: PRINT STRING$(16, CHR$(196)); CHR$(191)
   IF I / 2 = INT(I / 2) THEN LOCATE 3 * I + 4, 37: PRINT CHR$(195)
   IF I / 2 <> INT(I / 2) THEN LOCATE 3 * I + 2, 21: PRINT STRING$(16, CHR$(196)); CHR$(217)
   NEXT
   GOSUB 200
   REG = N: GAME = 8: GOSUB 300
   LOCATE 2, 21: GOSUB 350
   LOCATE 4, 21: GOSUB 360
   REG = N: GAME = 9: GOSUB 300
   LOCATE 8, 21: GOSUB 350
   LOCATE 10, 21: GOSUB 360
   REG = N: GAME = 10: GOSUB 300
   LOCATE 14, 21: GOSUB 350
   LOCATE 16, 21: GOSUB 360
   REG = N: GAME = 11: GOSUB 300
   LOCATE 20, 21: GOSUB 350
   LOCATE 22, 21: GOSUB 360
   REG = N: GAME = 12: GOSUB 300
   LOCATE 3, 39: GOSUB 350
   LOCATE 9, 39: GOSUB 360
   REG = N: GAME = 13: GOSUB 300
   LOCATE 15, 39: GOSUB 350
   LOCATE 21, 39: GOSUB 360
   REG = N: GAME = 14 + C%(0): GOSUB 300
   LOCATE 6, 56: GOSUB 350
   LOCATE 18, 56: GOSUB 360
   GOTO 370

25 REM:14 TEAM BRACKET
   REG = N: GAME = 1: GOSUB 300
   GOSUB 350
   GOSUB 360
   PRINT
   REG = N: GAME = 4: GOSUB 300
   GOSUB 350
   GOSUB 360
   PRINT
   REG = N: GAME = 5: GOSUB 300
   GOSUB 350
   GOSUB 360
   PRINT
   PRINT
   PRINT
   PRINT
   REG = N: GAME = 2: GOSUB 300
   GOSUB 350
   GOSUB 360
   PRINT
   REG = N: GAME = 3: GOSUB 300
   GOSUB 350
   GOSUB 360
   PRINT
   REG = N: GAME = 6: GOSUB 300
   GOSUB 350
   GOSUB 360
   FOR I = 0 TO 7
   IF I / 2 = INT(I / 2) THEN LOCATE 3 * I + 2, 22: PRINT A$(I + 1)
   IF I / 2 <> INT(I / 2) THEN LOCATE 3 * I + 1, 22: PRINT A$(I + 1)
   IF I / 2 = INT(I / 2) THEN LOCATE 3 * I + 3, 21: PRINT STRING$(16, CHR$(196)); CHR$(191)
   IF I / 2 = INT(I / 2) THEN LOCATE 3 * I + 4, 37: PRINT CHR$(195)
   IF I / 2 <> INT(I / 2) THEN LOCATE 3 * I + 2, 21: PRINT STRING$(16, CHR$(196)); CHR$(217)
   NEXT
   GOSUB 200
   REG = N: GAME = 7: GOSUB 300
   LOCATE 2, 21: GOSUB 350
   LOCATE 4, 21: GOSUB 360
   REG = N: GAME = 8: GOSUB 300
   LOCATE 8, 21: GOSUB 350
   LOCATE 10, 21: GOSUB 360
   REG = N: GAME = 9: GOSUB 300
   LOCATE 14, 21: GOSUB 350
   LOCATE 16, 21: GOSUB 360
   REG = N: GAME = 10: GOSUB 300
   LOCATE 20, 21: GOSUB 350
   LOCATE 22, 21: GOSUB 360
   REG = N: GAME = 11: GOSUB 300
   LOCATE 3, 39: GOSUB 350
   LOCATE 9, 39: GOSUB 360
   REG = N: GAME = 12: GOSUB 300
   LOCATE 15, 39: GOSUB 350
   LOCATE 21, 39: GOSUB 360
   REG = N: GAME = 13 + C%(0): GOSUB 300
   LOCATE 6, 56: GOSUB 350
   LOCATE 18, 56: GOSUB 360
   GOTO 370

27 REM:13 TEAM BRACKET
   REG = N: GAME = 1: GOSUB 300
   GOSUB 350
   GOSUB 360
   PRINT
   REG = N: GAME = 2: GOSUB 300
   GOSUB 350
   GOSUB 360
   PRINT
   REG = N: GAME = 3: GOSUB 300
   GOSUB 350
   GOSUB 360
   PRINT
   PRINT
   PRINT
   PRINT
   REG = N: GAME = 4: GOSUB 300
   GOSUB 350
   GOSUB 360
   PRINT
   PRINT
   PRINT
   PRINT
   REG = N: GAME = 5: GOSUB 300
   GOSUB 350
   GOSUB 360
   PRINT
   PRINT
   PRINT
   FOR I = 0 TO 7
   IF I / 2 = INT(I / 2) THEN LOCATE 3 * I + 2, 22: PRINT A$(I + 1)
   IF I / 2 <> INT(I / 2) THEN LOCATE 3 * I + 1, 22: PRINT A$(I + 1)
   IF I / 2 = INT(I / 2) THEN LOCATE 3 * I + 3, 21: PRINT STRING$(16, CHR$(196)); CHR$(191)
   IF I / 2 = INT(I / 2) THEN LOCATE 3 * I + 4, 37: PRINT CHR$(195)
   IF I / 2 <> INT(I / 2) THEN LOCATE 3 * I + 2, 21: PRINT STRING$(16, CHR$(196)); CHR$(217)
   NEXT
   GOSUB 200
   REG = N: GAME = 6: GOSUB 300
   LOCATE 2, 21: GOSUB 350
   LOCATE 4, 21: GOSUB 360
   REG = N: GAME = 7: GOSUB 300
   LOCATE 8, 21: GOSUB 350
   LOCATE 10, 21: GOSUB 360
   REG = N: GAME = 8: GOSUB 300
   LOCATE 14, 21: GOSUB 350
   LOCATE 16, 21: GOSUB 360
   REG = N: GAME = 9: GOSUB 300
   LOCATE 20, 21: GOSUB 350
   LOCATE 22, 21: GOSUB 360
   REG = N: GAME = 10: GOSUB 300
   LOCATE 3, 39: GOSUB 350
   LOCATE 9, 39: GOSUB 360
   REG = N: GAME = 11: GOSUB 300
   LOCATE 15, 39: GOSUB 350
   LOCATE 21, 39: GOSUB 360
   REG = N: GAME = 12 + C%(0): GOSUB 300
   LOCATE 6, 56: GOSUB 350
   LOCATE 18, 56: GOSUB 360
   GOTO 370

300 FOR I = 1 TO AP%(0)
    IF NG%(I, 15) = N AND NG%(I, 17) = GAME THEN 305
    NEXT
305 RETURN
310 FOR X = 1 TO 16
    FOR X1 = 1 TO 16
    IF X$ = N$(X, X1, 0) THEN 312
    NEXT: NEXT
311 COLOR 7, 0: GOTO 315
312 COLOR N%(X, X1, 2), N%(X, X1, 3)
315 RETURN
350 IF LEFT$(V$(I), 3) = "REG" THEN PRINT : GOTO 355
    X$ = QPTRIM(V$(I)): GOSUB 310
    IF NG%(I, 13) = 0 THEN PRINT QPTRIM(V$(I)): GOTO 355
    PRINT QPTRIM(V$(I)); : COLOR 14, 0: PRINT USING " ###"; VS%(I)
355 COLOR 7, 0
    RETURN
360 IF LEFT$(H$(I), 3) = "REG" THEN 355
    X$ = QPTRIM(H$(I)): GOSUB 310
    IF NG%(I, 13) = 0 THEN PRINT QPTRIM$(H$(I)): GOTO 355
    PRINT QPTRIM(H$(I)); : COLOR 14, 0: PRINT USING " ###"; HS%(I)
365 COLOR 7, 0
    RETURN
370 LOCATE 12, 64
    IF NG%(I, 13) = 0 THEN 375
    IF HS%(I) > VS%(I) THEN X$ = QPTRIM(H$(I)) ELSE X$ = QPTRIM(V$(I))
    GOSUB 310: PRINT X$
375 GOSUB 5000: GOTO 35
30 REM:12 TEAM BRACKET
   REG = N: GAME = 1: GOSUB 300
   GOSUB 350
   GOSUB 360
   PRINT
   PRINT
   PRINT
   PRINT
   REG = N: GAME = 2: GOSUB 300
   GOSUB 350
   GOSUB 360
   PRINT
   PRINT
   PRINT
   PRINT
   REG = N: GAME = 3: GOSUB 300
   GOSUB 350
   GOSUB 360
   PRINT
   PRINT
   PRINT
   PRINT
   REG = N: GAME = 4: GOSUB 300
   GOSUB 350
   GOSUB 360
   PRINT
   PRINT
   PRINT
   COLOR 15
   FOR I = 0 TO 7
   IF I / 2 = INT(I / 2) THEN LOCATE 3 * I + 2, 22: PRINT A$(I + 1)
   IF I / 2 <> INT(I / 2) THEN LOCATE 3 * I + 1, 22: PRINT A$(I + 1)
   IF I / 2 = INT(I / 2) THEN LOCATE 3 * I + 3, 21: PRINT STRING$(16, CHR$(196)); CHR$(191)
   IF I / 2 = INT(I / 2) THEN LOCATE 3 * I + 4, 37: PRINT CHR$(195)
   IF I / 2 <> INT(I / 2) THEN LOCATE 3 * I + 2, 21: PRINT STRING$(16, CHR$(196)); CHR$(217)
   NEXT
   COLOR 7
   GOSUB 200
   REG = N: GAME = 5: GOSUB 300
   LOCATE 2, 21: GOSUB 350
   LOCATE 4, 21: GOSUB 360
   REG = N: GAME = 6: GOSUB 300
   LOCATE 8, 21: GOSUB 350
   LOCATE 10, 21: GOSUB 360
   REG = N: GAME = 7: GOSUB 300
   LOCATE 14, 21: GOSUB 350
   LOCATE 16, 21: GOSUB 360
   REG = N: GAME = 8: GOSUB 300
   LOCATE 20, 21: GOSUB 350
   LOCATE 22, 21: GOSUB 360
   REG = N: GAME = 9: GOSUB 300
   LOCATE 3, 39: GOSUB 350
   LOCATE 9, 39: GOSUB 360
   REG = N: GAME = 10: GOSUB 300
   LOCATE 15, 39: GOSUB 350
   LOCATE 21, 39: GOSUB 360
   REG = N: GAME = 11 + C%(0): GOSUB 300
   LOCATE 6, 56: GOSUB 350
   LOCATE 18, 56: GOSUB 360
   LOCATE 12, 64
   GOTO 370

32 REM:11 TEAM BRACKET
   REG = N: GAME = 1: GOSUB 300
   GOSUB 350
   GOSUB 360
   PRINT
   PRINT
   PRINT
   PRINT
   REG = N: GAME = 2: GOSUB 300
   GOSUB 350
   GOSUB 360
   PRINT
   PRINT
   PRINT
   PRINT
   PRINT
   PRINT
   PRINT
   PRINT
   PRINT
   PRINT
   REG = N: GAME = 3: GOSUB 300
   GOSUB 350
   GOSUB 360
   FOR I = 0 TO 7
   IF I / 2 = INT(I / 2) THEN LOCATE 3 * I + 2, 22: PRINT A$(I + 1)
   IF I / 2 <> INT(I / 2) THEN LOCATE 3 * I + 1, 22: PRINT A$(I + 1)
   IF I / 2 = INT(I / 2) THEN LOCATE 3 * I + 3, 21: PRINT STRING$(16, CHR$(196)); CHR$(191)
   IF I / 2 = INT(I / 2) THEN LOCATE 3 * I + 4, 37: PRINT CHR$(195)
   IF I / 2 <> INT(I / 2) THEN LOCATE 3 * I + 2, 21: PRINT STRING$(16, CHR$(196)); CHR$(217)
   NEXT
   GOSUB 200
   REG = N: GAME = 4: GOSUB 300
   LOCATE 2, 21: GOSUB 350
   LOCATE 4, 21: GOSUB 360
   REG = N: GAME = 5: GOSUB 300
   LOCATE 8, 21: GOSUB 350
   LOCATE 10, 21: GOSUB 360
   REG = N: GAME = 6: GOSUB 300
   LOCATE 14, 21: GOSUB 350
   LOCATE 16, 21: GOSUB 360
   REG = N: GAME = 7: GOSUB 300
   LOCATE 20, 21: GOSUB 350
   LOCATE 22, 21: GOSUB 360
   REG = N: GAME = 8: GOSUB 300
   LOCATE 3, 39: GOSUB 350
   LOCATE 9, 39: GOSUB 360
   REG = N: GAME = 9: GOSUB 300
   LOCATE 15, 39: GOSUB 350
   LOCATE 21, 39: GOSUB 360
   REG = N: GAME = 10 + C%(0): GOSUB 300
   LOCATE 6, 56: GOSUB 350
   LOCATE 18, 56: GOSUB 360
   GOTO 370

34 REM:10 TEAM BRACKET
   REG = N: GAME = 1: GOSUB 300
   GOSUB 350
   GOSUB 360
   PRINT
   PRINT
   PRINT
   PRINT
   PRINT
   PRINT
   PRINT
   PRINT
   PRINT
   PRINT
   PRINT
   PRINT
   PRINT
   PRINT
   PRINT
   PRINT
   REG = N: GAME = 2: GOSUB 300
   GOSUB 350
   GOSUB 360
   FOR I = 0 TO 7
   IF I / 2 = INT(I / 2) THEN LOCATE 3 * I + 2, 22: PRINT A$(I + 1)
   IF I / 2 <> INT(I / 2) THEN LOCATE 3 * I + 1, 22: PRINT A$(I + 1)
   IF I / 2 = INT(I / 2) THEN LOCATE 3 * I + 3, 21: PRINT STRING$(16, CHR$(196)); CHR$(191)
   IF I / 2 = INT(I / 2) THEN LOCATE 3 * I + 4, 37: PRINT CHR$(195)
   IF I / 2 <> INT(I / 2) THEN LOCATE 3 * I + 2, 21: PRINT STRING$(16, CHR$(196)); CHR$(217)
   NEXT
   GOSUB 200
   REG = N: GAME = 3: GOSUB 300
   LOCATE 2, 21: GOSUB 350
   LOCATE 4, 21: GOSUB 360
   REG = N: GAME = 4: GOSUB 300
   LOCATE 8, 21: GOSUB 350
   LOCATE 10, 21: GOSUB 360
   REG = N: GAME = 5: GOSUB 300
   LOCATE 14, 21: GOSUB 350
   LOCATE 16, 21: GOSUB 360
   REG = N: GAME = 6: GOSUB 300
   LOCATE 20, 21: GOSUB 350
   LOCATE 22, 21: GOSUB 360
   REG = N: GAME = 7: GOSUB 300
   LOCATE 3, 39: GOSUB 350
   LOCATE 9, 39: GOSUB 360
   REG = N: GAME = 8: GOSUB 300
   LOCATE 15, 39: GOSUB 350
   LOCATE 21, 39: GOSUB 360
   REG = N: GAME = 9 + C%(0): GOSUB 300
   LOCATE 6, 56: GOSUB 350
   LOCATE 18, 56: GOSUB 360
   GOTO 370

36 REM:9 TEAM BRACKET
   LOCATE 22, 1
   REG = N: GAME = 1: GOSUB 300
   GOSUB 350
   GOSUB 360
   FOR I = 0 TO 7
   IF I / 2 = INT(I / 2) THEN LOCATE 3 * I + 2, 22: PRINT A$(I + 1)
   IF I / 2 <> INT(I / 2) THEN LOCATE 3 * I + 1, 22: PRINT A$(I + 1)
   IF I / 2 = INT(I / 2) THEN LOCATE 3 * I + 3, 21: PRINT STRING$(16, CHR$(196)); CHR$(191)
   IF I / 2 = INT(I / 2) THEN LOCATE 3 * I + 4, 37: PRINT CHR$(195)
   IF I / 2 <> INT(I / 2) THEN LOCATE 3 * I + 2, 21: PRINT STRING$(16, CHR$(196)); CHR$(217)
   NEXT
   GOSUB 200
   REG = N: GAME = 2: GOSUB 300
   LOCATE 2, 21: GOSUB 350
   LOCATE 4, 21: GOSUB 360
   REG = N: GAME = 3: GOSUB 300
   LOCATE 8, 21: GOSUB 350
   LOCATE 10, 21: GOSUB 360
   REG = N: GAME = 4: GOSUB 300
   LOCATE 14, 21: GOSUB 350
   LOCATE 16, 21: GOSUB 360
   REG = N: GAME = 5: GOSUB 300
   LOCATE 20, 21: GOSUB 350
   LOCATE 22, 21: GOSUB 360
   REG = N: GAME = 6: GOSUB 300
   LOCATE 3, 39: GOSUB 350
   LOCATE 9, 39: GOSUB 360
   REG = N: GAME = 7: GOSUB 300
   LOCATE 15, 39: GOSUB 350
   LOCATE 21, 39: GOSUB 360
   REG = N: GAME = 8 + C%(0): GOSUB 300
   LOCATE 6, 56: GOSUB 350
   LOCATE 18, 56: GOSUB 360
   GOTO 370

40 REM: 8 TEAM BRACKET
   FOR I = 0 TO 7
   IF I / 2 = INT(I / 2) THEN LOCATE 3 * I + 2, 22: PRINT A$(I + 1)
   IF I / 2 <> INT(I / 2) THEN LOCATE 3 * I + 1, 22: PRINT A$(I + 1)
   IF I / 2 = INT(I / 2) THEN LOCATE 3 * I + 3, 21: PRINT STRING$(16, CHR$(196)); CHR$(191)
   IF I / 2 = INT(I / 2) THEN LOCATE 3 * I + 4, 37: PRINT CHR$(195)
   IF I / 2 <> INT(I / 2) THEN LOCATE 3 * I + 2, 21: PRINT STRING$(16, CHR$(196)); CHR$(217)
   NEXT
   GOSUB 200
   REG = N: GAME = 1: GOSUB 300
   LOCATE 2, 21: GOSUB 350
   LOCATE 4, 21: GOSUB 360
   REG = N: GAME = 2: GOSUB 300
   LOCATE 8, 21: GOSUB 350
   LOCATE 10, 21: GOSUB 360
   REG = N: GAME = 3: GOSUB 300
   LOCATE 14, 21: GOSUB 350
   LOCATE 16, 21: GOSUB 360
   REG = N: GAME = 4: GOSUB 300
   LOCATE 20, 21: GOSUB 350
   LOCATE 22, 21: GOSUB 360
   REG = N: GAME = 5: GOSUB 300
   LOCATE 3, 39: GOSUB 350
   LOCATE 9, 39: GOSUB 360
   REG = N: GAME = 6: GOSUB 300
   LOCATE 15, 39: GOSUB 350
   LOCATE 21, 39: GOSUB 360
   REG = N: GAME = 7 + C%(0): GOSUB 300
   LOCATE 6, 56: GOSUB 350
   LOCATE 18, 56: GOSUB 360
   GOTO 370

45 REM: 7 TEAM BRACKET
   FOR I = 0 TO 7
   IF I / 2 = INT(I / 2) THEN LOCATE 3 * I + 2, 22: PRINT A$(I + 1)
   IF I / 2 <> INT(I / 2) THEN LOCATE 3 * I + 1, 22: PRINT A$(I + 1)
   IF I / 2 = INT(I / 2) AND I <> 4 THEN LOCATE 3 * I + 3, 21: PRINT STRING$(16, CHR$(196)); CHR$(191)
   IF I / 2 = INT(I / 2) AND I <> 4 THEN LOCATE 3 * I + 4, 37: PRINT CHR$(195)
   IF I / 2 <> INT(I / 2) AND I <> 5 THEN LOCATE 3 * I + 2, 21: PRINT STRING$(16, CHR$(196)); CHR$(217)
   NEXT
   GOSUB 200
   REG = N: GAME = 1: GOSUB 300
   LOCATE 2, 21: GOSUB 350
   LOCATE 4, 21: GOSUB 360
   REG = N: GAME = 2: GOSUB 300
   LOCATE 8, 21: GOSUB 350
   LOCATE 10, 21: GOSUB 360
   REG = N: GAME = 3: GOSUB 300
   LOCATE 20, 21: GOSUB 350
   LOCATE 22, 21: GOSUB 360
   REG = N: GAME = 4: GOSUB 300
   LOCATE 3, 39: GOSUB 350
   LOCATE 9, 39: GOSUB 360
   REG = N: GAME = 5: GOSUB 300
   LOCATE 15, 39: GOSUB 350
   LOCATE 21, 39: GOSUB 360
   REG = N: GAME = 6 + C%(0): GOSUB 300
   LOCATE 6, 56: GOSUB 350
   LOCATE 18, 56: GOSUB 360
   GOTO 370

46 REM: 6 TEAM BRACKET
   FOR I = 0 TO 7
   IF I / 2 = INT(I / 2) THEN LOCATE 3 * I + 2, 22: PRINT A$(I + 1)
   IF I / 2 <> INT(I / 2) THEN LOCATE 3 * I + 1, 22: PRINT A$(I + 1)
   IF I / 2 = INT(I / 2) AND (I <> 4 AND I <> 2) THEN LOCATE 3 * I + 3, 21: PRINT STRING$(16, CHR$(196)); CHR$(191)
   IF I / 2 = INT(I / 2) AND (I <> 4 AND I <> 2) THEN LOCATE 3 * I + 4, 37: PRINT CHR$(195)
   IF I / 2 <> INT(I / 2) AND (I <> 5 AND I <> 3) THEN LOCATE 3 * I + 2, 21: PRINT STRING$(16, CHR$(196)); CHR$(217)
   NEXT
   GOSUB 200
   REG = N: GAME = 1: GOSUB 300
   LOCATE 2, 21: GOSUB 350
   LOCATE 4, 21: GOSUB 360
   REG = N: GAME = 2: GOSUB 300
   LOCATE 20, 21: GOSUB 350
   LOCATE 22, 21: GOSUB 360
   REG = N: GAME = 3: GOSUB 300
   LOCATE 3, 39: GOSUB 350
   LOCATE 9, 39: GOSUB 360
   REG = N: GAME = 4: GOSUB 300
   LOCATE 15, 39: GOSUB 350
   LOCATE 21, 39: GOSUB 360
   REG = N: GAME = 5 + C%(0): GOSUB 300
   LOCATE 6, 56: GOSUB 350
   LOCATE 18, 56: GOSUB 360
   GOTO 370

47 REM: 5 TEAM BRACKET
   FOR I = 0 TO 1
   IF I / 2 = INT(I / 2) THEN LOCATE 3 * I + 2, 22: PRINT A$(I + 1)
   IF I / 2 <> INT(I / 2) THEN LOCATE 3 * I + 1, 22: PRINT A$(I + 1)
   IF I / 2 = INT(I / 2) THEN LOCATE 3 * I + 3, 21: PRINT STRING$(16, CHR$(196)); CHR$(191)
   IF I / 2 = INT(I / 2) THEN LOCATE 3 * I + 4, 37: PRINT CHR$(195)
   IF I / 2 <> INT(I / 2) THEN LOCATE 3 * I + 2, 21: PRINT STRING$(16, CHR$(196)); CHR$(217)
   NEXT
   GOSUB 200
   REG = N: GAME = 1: GOSUB 300
   LOCATE 2, 21: GOSUB 350
   LOCATE 4, 21: GOSUB 360
   REG = N: GAME = 2: GOSUB 300
   LOCATE 3, 39: GOSUB 350
   LOCATE 9, 39: GOSUB 360
   REG = N: GAME = 3: GOSUB 300
   LOCATE 15, 39: GOSUB 350
   LOCATE 21, 39: GOSUB 360
   REG = N: GAME = 4 + C%(0): GOSUB 300
   LOCATE 6, 56: GOSUB 350
   LOCATE 18, 56: GOSUB 360
   GOTO 370

50 REM:FOUR TEAM BRACKET
GOSUB 200
   REG = N: GAME = 1: GOSUB 300
   LOCATE 3, 39: GOSUB 350
   LOCATE 9, 39: GOSUB 360
   REG = N: GAME = 2: GOSUB 300
   LOCATE 15, 39: GOSUB 350
   LOCATE 21, 39: GOSUB 360
   REG = N: GAME = 3 + C%(0): GOSUB 300
   LOCATE 6, 56: GOSUB 350
   LOCATE 18, 56: GOSUB 360
   GOTO 370

55 REM:3 TEAM BRACKET
     FOR I = 0 TO 1

      IF I / 2 = INT(I / 2) THEN LOCATE 6 * I + 5, 47: PRINT CHR$(179)
      IF I / 2 = INT(I / 2) THEN LOCATE 6 * I + 6, 47: PRINT CHR$(179)
      IF I / 2 = INT(I / 2) THEN LOCATE 6 * I + 7, 47: PRINT CHR$(195)
      IF I / 2 = INT(I / 2) THEN LOCATE 6 * I + 8, 47: PRINT CHR$(179)
      IF I / 2 = INT(I / 2) THEN LOCATE 6 * I + 9, 47: PRINT CHR$(179)

IF I / 2 = INT(I / 2) THEN LOCATE 6 * I + 4, 32: PRINT STRING$(15, CHR$(196)); CHR$(191)
IF I / 2 <> INT(I / 2) THEN LOCATE 6 * I + 4, 32: PRINT STRING$(15, CHR$(196)); CHR$(217)
NEXT

    FOR I = 0 TO 1
    IF I = 0 THEN LOCATE 7, 48: PRINT STRING$(15, CHR$(196)); CHR$(191)
    FOR I1 = 7 TO 17: LOCATE I1 + 1, 63: PRINT CHR$(179): NEXT
    LOCATE 13, 63: PRINT CHR$(195); STRING$(15, CHR$(196))
    IF I = 1 THEN LOCATE 19, 48: PRINT STRING$(15, CHR$(196)); CHR$(217)
    NEXT
   LOCATE 3, 32: PRINT V$(1); STR$(VS%(1))
   LOCATE 9, 32: PRINT H$(1); STR$(HS%(1))
   LOCATE 6, 48: PRINT LEFT$(V$(2), 13); STR$(VS%(2))
   LOCATE 18, 48: PRINT LEFT$(H$(2), 13); STR$(HS%(2))
   GOTO 370

60 REM:TWO TEAM BRACKET
    FOR I = 0 TO 1
    IF I = 0 THEN LOCATE 7, 48: PRINT STRING$(15, CHR$(196)); CHR$(191)
    FOR I1 = 7 TO 17: LOCATE I1 + 1, 63: PRINT CHR$(179): NEXT
    LOCATE 13, 63: PRINT CHR$(195); STRING$(15, CHR$(196))
    IF I = 1 THEN LOCATE 19, 48: PRINT STRING$(15, CHR$(196)); CHR$(217)
    LOCATE 6, 48: PRINT V$(4); STR$(VS%(4))
    LOCATE 18, 48: PRINT H$(4); STR$(HS%(4))
    NEXT
    GOTO 370

200  REM:4 TEAM SEMI BRACKET
     COLOR 15
     FOR I = 0 TO 3

      IF I / 2 = INT(I / 2) THEN LOCATE 6 * I + 5, 55: PRINT CHR$(179)
      IF I / 2 = INT(I / 2) THEN LOCATE 6 * I + 6, 55: PRINT CHR$(179)
      IF I / 2 = INT(I / 2) THEN LOCATE 6 * I + 7, 55: PRINT CHR$(195)
      IF I / 2 = INT(I / 2) THEN LOCATE 6 * I + 8, 55: PRINT CHR$(179)
      IF I / 2 = INT(I / 2) THEN LOCATE 6 * I + 9, 55: PRINT CHR$(179)

IF I / 2 = INT(I / 2) THEN LOCATE 6 * I + 4, 38: PRINT STRING$(17, CHR$(196)); CHR$(191)
IF I / 2 <> INT(I / 2) THEN LOCATE 6 * I + 4, 38: PRINT STRING$(17, CHR$(196)); CHR$(217)
NEXT
    FOR I = 0 TO 1
    IF I = 0 THEN LOCATE 7, 56: PRINT STRING$(13, CHR$(196)); CHR$(191)
    FOR I1 = 7 TO 17: LOCATE I1 + 1, 69: PRINT CHR$(179): NEXT
    LOCATE 13, 69: PRINT CHR$(195); STRING$(9, CHR$(196))
    IF I = 1 THEN LOCATE 19, 56: PRINT STRING$(13, CHR$(196)); CHR$(217)
    NEXT
    COLOR 7
RETURN
5000 I$ = INKEY$: IF I$ = "" THEN 5000
      RETURN
4050 LOCATE 10, 8
     CALL CSMsgBox("NO TOURNAMENT FILES WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Cnf)
     EDITCODE% = 0
3810 I$ = INKEY$: IF I$ = "" THEN 3810
     CALL CSMsgBox("", 0, Cnf)
     GOTO 10

STATIC SUB CSMsgBox (Message$, Wdth, Cnf AS Config)
    IF LEN(Message$) THEN
       Row = CSRLIN                             'Get the Row Position
       REDIM Temp$(23)                          'Make a temporary array to
                                                '  hold the parsed up lines
       MesLen = LEN(Message$)                   'Get the total length of the
                                                '  help message
       LN = 0                                   'Line number
       Strt = 1                                 'Starting parse position
       '*** Parse the message into lines
       DO
           WHILE MID$(Message$, Strt, 1) = " "  'Skip over blanks
              Strt = Strt + 1
           WEND
           ND = Strt + Wdth                     'Guess at the end of line
                                                'Loop backwards to find
           WHILE MID$(Message$, ND, 1) <> " " AND ND <= MesLen AND ND > Strt
               ND = ND - 1                      '  blank space
           WEND
           LN = LN + 1                          'Increment the Line number
                                                'Pull out the new line
           Temp$(LN) = MID$(Message$, Strt, ND - Strt)
           Strt = ND + 1                        'Reset starting posit. for
                                                '  next line
       LOOP UNTIL ND >= MesLen                  'Get more unless @ end of mes
       BotRow = Row + LN + 1                    'Find the bottom row #
       IF BotRow > 24 THEN BotRow = 24          'No farther than line 24
       ArraySize = 80 * (BotRow - Row + 2)      'Find the array size to use
       REDIM ScrBuf(ArraySize)                  '  for the background screen
       MScrnSave Row, 1, BotRow + 1, 80, ScrBuf(0)
       BoxWdth = Wdth + 4                       'Find the outer box width
       ColPos = 40 - (BoxWdth \ 2)              'Starting posit for centered
                                                '  box
       LOCATE Row, ColPos                       'Print the top of box
       MQPrint "É" + STRING$(Wdth + 2, "Í") + "»", Cnf.MenBox
       Box$ = "º" + SPACE$(Wdth + 2) + "º"      'Make the box sides
       FOR N = 1 TO LN                          'Print message lines
           LOCATE N + Row, ColPos
           MQPrint Box$, Cnf.MenBox
           LOCATE , ColPos + 2
           MQPrint Temp$(N), Cnf.ActivCh
       NEXT
       LOCATE N + Row, ColPos                   'Print the bottom of box
       MQPrint "È" + STRING$(Wdth + 2, "Í") + "¼", Cnf.MenBox
       MPaintBox Row + 1, ColPos + BoxWdth, N + Row, ColPos + BoxWdth + 1, 8
       MPaintBox N + Row + 1, ColPos + 2, N + Row + 1, ColPos + BoxWdth + 1, 8
       Pass = 1                                 'Set flag say we've been here
       ERASE Temp$                              'Don't need this anymore
    ELSEIF Pass = 1 THEN                        'If we've been here before,
                                                '  restore the screen
       MScrnRest Row, 1, BotRow + 1, 80, SEG ScrBuf(0)
       ERASE ScrBuf                             'Clean up memory
       Pass = 0                                 'Reset flag for next call
    END IF
END SUB

SUB TITLE ()
   COLOR , 0: KEY OFF: CLS
   COLOR , 1: LOCATE 1, 1: PRINT TAB(80); : LOCATE , 16: COLOR 12
   PRINT "COURTSIDE COLLEGE BASKETBALL TOURNAMENT SCHEDULER"
   LOCATE 24, 1: PRINT TAB(80); : LOCATE , 1
   PRINT "(C) LANCE HAFFNER/SHANNON LYNN 1987-1994";
   LOCATE 3, 1: COLOR 2, 0
   COLOR 15: PRINT "        PROGRAMMED AND DEVELOPED BY LANCE HAFFNER AND SHANNON LYNN"
             PRINT "              FOR TECHNICAL SUPPORT, CALL (615) 366-8088"
   COLOR 10
END SUB

