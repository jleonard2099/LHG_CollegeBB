'$INCLUDE: 'Config.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

DEFINT A-Z
DECLARE FUNCTION FnExt$ (X$, Length)
DECLARE SUB TITLE ()
DECLARE SUB CHAMPIONSHIP ()
DECLARE SUB SETTINGS ()
DECLARE SUB SORTGAMES ()
DECLARE SUB WRITEGAME ()

DIM NN%(16): ' NUMBER OF TEAMS IN A REGION, UP TO 16 REGIONS
DIM N$(16, 16, 3): ' TEAM NAMES FOR EACH SEED SLOT
DIM R$(16): ' REGIONAL NAMES
DIM D$(3), U6 AS SINGLE, Y AS LONG
DIM OP%(14): ' TOURNAMENT DEFAULTS
DIM Q$(300), VV$(500), V%(500), NG%(300, 17), H$(300), V$(300), HS%(300), VS%(300)
DIM YN$(300, 3), L%(4)
DIM N%(16, 16, 3): ' REGION, SEED NUMBER, TEAM#/MODE OF PLAY (0,1)
DIM C%(1): ' 0-CONSOLATION IN REGIONS, 1-CONSOLATION IN CHAMPIONSHIP
DIM CM$(1), TE$(1), HC$(2), TS$(1), SC$(2), TF$(1), ITEM$(17), T%(9)
COMMON SHARED R, TC, NG%(), OP%(), V$(), H$(), YN$(), F$, HS%(), VS%(), Q$(), C%(), D$()

	TS$(0) = "N": TS$(1) = "Y": SC$(0) = "NO SHOT CLOCK": SC$(1) = "45 SEC CLOCK ": SC$(2) = "35 SEC CLOCK "
	HC$(1) = "Y": HC$(2) = "N"
	TF$(0) = "N": TF$(1) = "Y"
	TE$(0) = "SHOOT 1+1": TE$(1) = "SHOOT 2  "
	CM$(0) = "COACH AND SELECT SHOTS           "
	CM$(1) = "COACH ONLY AND LET COMPUTER SHOOT"

OPEN "DEV.BAS" FOR INPUT AS #2
	INPUT #2, D$(0): INPUT #2, D$(1)
CLOSE 2

OP%(3) = 1
OP%(7) = 2
OP%(5) = 5
OP%(6) = 1
OP%(10) = 1
OP%(14) = 0

' build menu screen

10 CALL TITLE
	ITEM$(1) = "CREATE TOURNAMENT SEEDS      "
	ITEM$(2) = "EDIT EXISTING SEEDS          "
	ITEM$(3) = "VIEW BRACKETS / SEEDS        "
	ITEM$(4) = "RESET A TOURNAMENT           "
	ITEM$(5) = "PLAY TOURNAMENT GAMES        "
	ITEM$(6) = "RETURN TO COURTSIDE MAIN MENU"
	  SELECTION% = 1
	Count% = 6
	Start% = 1
	SCANCODE% = 6
	FileFGColr% = 15:FileBGColr% = 1
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Spacing% = 1
	Row% = 10
	Column% = 25

CALL box0(9, 24, 16, 54, 2, 31)
_MouseShow "DEFAULT"
MMenuVert VARPTR(ITEM$(1)), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

	COLOR , 0
	IF SCANCODE% = 27 THEN RUN "HELLO.EXE"
	Q = SELECTION% - 1
	IF Q = 2 THEN RUN "BRACKET.EXE"
	IF Q = 5 THEN RUN "HELLO.EXE"
	IF Q = 4 THEN RUN "TOURNEY.EXE"
	GOSUB 611
	IF Q = 1 OR Q = 3 THEN 610
	
'GLOBAL SETUP TOURNEY DEFAULTS

20  CLS
	COLOR , 4: LOCATE 1, 1: PRINT TAB(80); : LOCATE , 19: COLOR 0
	PRINT "SELECT GENERAL TOURNAMENT OPTIONS"
	LOCATE 25, 1: PRINT TAB(80); : LOCATE , 1: COLOR , 0

154 LOCATE 5, 2: COLOR 14: PRINT "(1) "; : COLOR 15: PRINT "3-POINT SHOT: "; TS$(OP%(3))

155 LOCATE 6, 2: COLOR 14: PRINT "(2) "; : COLOR 15: PRINT SC$(OP%(7))

157 LOCATE 7, 2: COLOR 14: PRINT "(3) "; : COLOR 15: PRINT USING "DISPLAY PAUSE IN SECONDS: ##.#  "; OP%(4) / 10

158 LOCATE 8, 2: COLOR 14: PRINT "(4) "; : COLOR 15: PRINT "FOULS TO DISQUALIFY: "; OP%(5)

162 LOCATE 9, 2: COLOR 14: PRINT "(5) "; : COLOR 15: PRINT "FREE THROWS ON 10TH FOUL "; TE$(OP%(6))

164 LOCATE 10, 2: COLOR 14: PRINT "(6) "; : COLOR 15: PRINT "3-FT WHEN FOULED ON 3-PT ATTEMPT "; TF$(OP%(10))
	LOCATE 11, 2: COLOR 14: PRINT "(7) "; : COLOR 15: PRINT "1 FT ON 1ST 6 FLOOR FOULS (PRIOR TO 1973): "; TF$(OP%(14))

163 LOCATE 12, 2: COLOR 14: PRINT "(8) "; : COLOR 15: PRINT "ACCEPT SETTINGS"
165 I$ = GetKeyPress$
	IF I$ = CHR$(27) THEN 10
	I = VAL(I$): IF I < 1 OR I > 8 THEN 165
	IF I = 1 THEN OP%(3) = 1 - OP%(3): GOTO 154
	IF I = 2 THEN OP%(7) = OP%(7) + 1
	IF OP%(7) > 2 THEN OP%(7) = 0
	IF I = 2 THEN 154
	IF I = 3 THEN LOCATE 7, 32: INPUT U6: OP%(4) = U6 * 10: GOTO 154
	IF I = 4 THEN OP%(5) = OP%(5) + 1
	IF OP%(5) > 6 THEN OP%(5) = 5
	IF I = 4 THEN 154
	IF I = 5 THEN OP%(6) = 1 - OP%(6): GOTO 154
	IF I = 6 THEN OP%(10) = 1 - OP%(10): GOTO 154
	IF I = 7 THEN OP%(14) = 1 - OP%(14): GOTO 154
	
	COLOR 15, 1
	FG = 0
	BG = 7
	Colr = OneColor%(FG, BG)             'pack FG and BG into a single byte
	CLS
	
	COLOR , 6: LOCATE 1, 1: PRINT TAB(80); : LOCATE , 22: COLOR 15
	PRINT "TOURNAMENT  - GENERAL INFORMATION"
	LOCATE 25, 1: PRINT TAB(80); : LOCATE , 1: COLOR , 0
	LOCATE 7, 1: QPrint "FILE NAME (8 CHARACTERS) ", 30, -1
	LOCATE , 26: QPrint FnExt$(F$, 8), FG, BG, -1
	LOCATE 9, 1: QPrint "NAME OF TOURNAMENT ", 30, -1
	LOCATE , 20: QPrint FnExt$(FF$, 40), FG, BG, -1
	LOCATE 11, 1: QPrint "CONSOLATION GAMES IN REGIONALS/FINALS ", 30, -1
	LOCATE , 40: QPrint FnExt$(C$, 1), FG, BG, -1
	LOCATE 13, 1: QPrint "NUMBER OF BRACKETS/REGIONS (1-16) ", 30, -1
	LOCATE , 35: QPrint FnExt$(NR$, 2), FG, BG, -1

GETDIV1:
	LOCATE 7, 26                         'location of beginning of field
	CapsOn = 0: NumOnly = 0: Max = 8     'specify field parameters
	CALL TextIn(F$, Max, NumOnly, CapsOn, XCode, Colr)        'get input
	IF XCode = 1 GOTO GETDIV1:           'can't back up any more, stay here
	IF XCode = 2 GOTO Done               'all done entering data
	F$ = QPTRIM$(F$)
GetDiv2:
	LOCATE 9, 20                         'location of beginning of field
	CapsOn = 0: NumOnly = 0: Max = 40    'specify field parameters
	CALL TextIn(FF$, Max, NumOnly, CapsOn, XCode, Colr)        'get input
	IF XCode = 1 GOTO GETDIV1:           'can't back up any more, stay here
	IF XCode = 2 GOTO Done               'all done entering data
	FF$ = QPTRIM$(FF$)
GetDiv3:
	LOCATE 11, 40                         'location of beginning of field
	CapsOn = 0: NumOnly = 0: Max = 1    'specify field parameters
	CALL TextIn(C$, Max, NumOnly, CapsOn, XCode, Colr)        'get input
	IF XCode = 1 GOTO GetDiv2:           'can't back up any more, stay here
	IF XCode = 2 GOTO Done               'all done entering data
	C$ = QPTRIM$(C$)

GETDIV4:
	LOCATE 13, 35                         'location of beginning of field
	CapsOn = 0: NumOnly = 0: Max = 2    'specify field parameters
	CALL TextIn(NR$, Max, NumOnly, CapsOn, XCode, Colr)        'get input
	IF XCode = 1 GOTO GetDiv3:           'can't back up any more, stay here
	NR$ = QPTRIM$(NR$)

Done:
	LOCATE 23, 1
	COLOR 15, 1
	PRINT "Is this okay? (Yes/No) ";
	X$ = ""
	CALL YesNoB(X$, XCode, Colr)
	LOCATE , 1
	ClearEOL 30
	IF XCode = 1 GOTO GETDIV4
	IF X$ = CHR$(27) THEN 10
	IF X$ = "N" GOTO GETDIV1
	IF X$ <> "Y" GOTO Done               'insist on either Y or N or up-arrow
	IF UCASE$(C$) = "Y" THEN C%(0) = 1 ELSE C%(0) = 0
	C%(1) = C%(0)
	IF F$ = "" THEN

	LOCATE 10
	CALL CSMsgBox("TOUNAMENT FILE NAME IS BLANK PRESS ANY KEY TO CONTINUE SELECTING", 28, Cnf)
	I$=GetKeyPress$
	CALL CSMsgBox("", 0, Cnf)

	GOTO GETDIV1
END IF

R = VAL(NR$)
IF R < 1 OR R > 16 THEN GOTO GETDIV4

OPEN D$(0) + F$ + ".TNY" FOR OUTPUT AS #1
FOR I = 0 TO 14
PRINT #1, OP%(I)
NEXT
CLOSE 1

IF EDITCODE% = 1 THEN 30

FOR NR = 1 TO R

SEEDBRACKET:

CLS
		COLOR 15, 1
	 FG = 0
	 BG = 7
	Colr = OneColor%(FG, BG)             'pack FG and BG into a single byte
	CLS
	COLOR , 6: LOCATE 1, 1: PRINT TAB(80); : LOCATE , 26: COLOR 15
	PRINT "REGIONAL/BRACKET INFORMATION"
	LOCATE 25, 1: PRINT TAB(80); : LOCATE , 1: COLOR , 0
	X$ = "NAME OF REGION" + STR$(NR) + " "
	LOCATE 7, 1: QPrint X$, 30, -1
	LOCATE , 26: QPrint FnExt$(R$(NR), 16), FG, BG, -1
	LOCATE 9, 1: QPrint "NUMBER OF TEAMS IN REGION (4-16) ", 30, -1
	LOCATE , 34: QPrint FnExt$(NT$, 2), FG, BG, -1

NameRegion:
	LOCATE 7, 26                        'location of beginning of field
	CapsOn = 0: NumOnly = 0: Max = 16    'specify field parameters
	CALL TextIn(R$(NR), Max, NumOnly, CapsOn, XCode, Colr)        'get input
	IF XCode = 1 GOTO REGIONSIZE:           'can't back up any more, stay here
	IF XCode = 2 GOTO Done1               'all done entering data
	R$(NR) = QPTRIM$(R$(NR))

REGIONSIZE:
	LOCATE 9, 34                         'location of beginning of field
	CapsOn = 0: NumOnly = 0: Max = 2    'specify field parameters
	CALL TextIn(NT$, Max, NumOnly, CapsOn, XCode, Colr)        'get input
	IF XCode = 1 GOTO NameRegion:           'can't back up any more, stay here
	NT$ = QPTRIM$(NT$)

Done1:
	LOCATE 23, 1: COLOR 15, 1
	PRINT "Is this okay? (Yes/No) ";
	X$ = ""
	CALL YesNoB(X$, XCode, Colr)
	LOCATE , 1
	ClearEOL 30
	IF XCode = 1 GOTO REGIONSIZE
	IF X$ = CHR$(27) THEN 10
	IF X$ = "N" GOTO NameRegion
	IF X$ <> "Y" GOTO Done1              'insist on either Y or N or up-arrow

NN%(NR) = VAL(NT$)
IF NN%(NR) < 4 OR NN%(NR) > 16 THEN GOTO REGIONSIZE

220 COLOR , 0: CLS
	COLOR , 6: LOCATE 1, 1: PRINT TAB(80); : LOCATE , 26: COLOR 15
	X = LEN(R$(NR) + " REGIONAL/CONFERENCE SEEDING")
	X = (80 - X) / 2
	LOCATE , X
	PRINT R$(NR) + " REGIONAL/CONFERENCE SEEDING"
	LOCATE 25, 1: PRINT TAB(80); : LOCATE , 1: COLOR , 0
	 COLOR 12
	MAXSEED = NN%(NR) + 1 + 4
CALL box0(3, 24, MAXSEED, 54, 2, 15)
	LOCATE 4, 25: PRINT "SEED  TEAM"
	FOR I1 = 1 TO NN%(NR)
	COLOR 2
	LOCATE , 25
	PRINT USING "##    "; I1; : COLOR 15: PRINT N$(NR, I1, 0)
	NEXT I1
	LOCATE 25, 1
	COLOR 0, 6: PRINT "A-"; : COLOR 15: PRINT "ADD A TEAM    ";
	COLOR 0, 6: PRINT "R-"; : COLOR 15: PRINT "REMOVE A TEAM    ";
	COLOR 0, 6: PRINT "S-"; : COLOR 15: PRINT "SAVE SEEDING ";
	COLOR , 0: LOCATE 1, 1
250 I$ = GetKeyPress$
	IF UCASE$(I$) = "A" THEN I$ = "1"
	IF UCASE$(I$) = "R" THEN I$ = "2"
	IF UCASE$(I$) = "S" THEN I$ = "3"
	IF I$ = CHR$(27) THEN GOTO SEEDBRACKET
	IF VAL(I$) < 1 OR VAL(I$) > 3 THEN 250

260 IF I$ = "3" THEN GOSUB 80
	IF I$ = "3" AND NF = 0 THEN 75
	IF NF = 1 THEN NF = 0: GOTO 250

270 LOCATE 22, 30: PRINT "SEED # (1-"; STR$(NN%(NR)); ") ";

280 INPUT B

290 IF B < 1 OR B > NN%(NR) THEN 270
	IF I$ = "2" THEN N$(NR, B, 0) = "": GOTO 220

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
	LOCATE , 12: PRINT
	LOCATE , 12: PRINT "HIT <ENTER> TO USE PREVIOUS ID"
	LOCATE 7, 27: INPUT NY$
	IF NY$ = "" THEN GOSUB 1102: GOTO 182
	YN$ = NY$
	FOR I = 1 TO 500: VV$(I) = "": NEXT
	CLS
	IF UCASE$(LEFT$(D$(0), 1)) = "C" THEN 210
	GOSUB 4505

210 GOSUB 1104

182 IF BO% = 1 THEN BO% = 0: GOTO 1440

183 LOCATE 2, 33: COLOR 15, 0: COLOR T%(0), T%(1): PRINT A$: COLOR 15, 4
	LOCATE , 33: PRINT "ANY CHANGE (YN)";
	I$=GetKeyPress$
	IF UCASE$(I$) = "Y" THEN 1440

390 N$(NR, B, 0) = A$
	N$(NR, B, 1) = YN$
	N%(NR, B, 0) = D
	N%(NR, B, 1) = 3
	N%(NR, B, 2) = T%(0)
	N%(NR, B, 3) = T%(1)
'    LOCATE , 23: PRINT "(C)OMPUTER OR (H)UMAN CONTROL";
'
391 I$ = GetKeyPress$
'    IF UCASE$(I$) <> "C" AND UCASE$(I$) <> "H" THEN 391
'    IF UCASE$(I$) = "C" THEN N%(NR, B, 1) = 1 ELSE N%(NR, B, 1) = 0
	N%(NR, B, 1) = 1
	GOTO 220

80  FOR I = 1 TO NN%(NR)
	IF N$(NR, I, 0) <> "" THEN 81
	LOCATE 10
	
	CALL CSMsgBox("ALL SEEDS HAVE NOT BEEN FILLED PRESS ANY KEY TO CONTINUE SELECTING", 30, Cnf)
82  I$=GetKeyPress$
	CALL CSMsgBox("", 0, Cnf)
	NF = 1: RETURN

81  NEXT
	RETURN

75  IF EDITCODE% = 1 THEN 30
	NEXT

76 IF _FILEEXISTS(D$(0) + F$ + ".TMP") THEN KILL D$(0) + F$ + ".TMP"
	IF _FILEEXISTS(D$(0) + F$ + ".TCX") THEN KILL D$(0) + F$ + ".TCX"
	IF _FILEEXISTS(D$(0) + F$ + ".TOU") THEN KILL D$(0) + F$ + ".TOU"
	FOR NR = 1 TO R
	IF NN%(NR) < 4 THEN 50
	ON NN%(NR) - 3 GOTO 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300

50 NEXT
	  CALL CHAMPIONSHIP

5050 'SAVE TOURNAMENT INFO AND QUIT
OPEN D$(0) + F$ + ".INF" FOR OUTPUT AS #2
	PRINT #2, FF$
	PRINT #2, R: ' NUMBER OF REGIONS
	FOR I = 1 TO 16
	PRINT #2, NN%(I): ' NUMBER OF TEAMS IN REGION
	PRINT #2, R$(I): ' NAME OF REGION
	NEXT
	FOR I = 1 TO 16
	FOR I1 = 1 TO 16
	FOR I2 = 0 TO 3
	PRINT #2, N$(I, I1, I2): ' SEED SLOT NAME
	PRINT #2, N%(I, I1, I2): ' REGION, SEED NUMBER, MODE PLAY/TM#
	NEXT: NEXT: NEXT
	PRINT #2, C%(0): PRINT #2, C%(1): ' CONSOLATION GAME
CLOSE 2
	CALL SORTGAMES
	LOCATE 22, 25: PRINT "                                  "
	LOCATE 10
	
	CALL CSMsgBox("ALL FILES SAVED...PRESS ANY KEY TO RETURN TO TOURNAMENT OPTIONS", 18, Cnf)
12   I$=GetKeyPress$
	CALL CSMsgBox("", 0, Cnf)
	GOTO 10

30 'EDIT A GIVEN BRACKET
	COLOR , 0: CLS
	COLOR , 6: LOCATE 1, 1: PRINT TAB(80); : LOCATE , 23: COLOR 15
	PRINT "TOURNAMENT  - REGION/BRACKET EDITOR"
	LOCATE 25, 1: PRINT TAB(80); : LOCATE , 1: COLOR , 0
	FOR I = 1 TO R
	IF R$(I) = "" THEN R$(I) = "TO BE NAMED"
	ITEM$(I) = R$(I)
	NEXT
	  SELECTION% = 1
	Count% = R + 1
	Start% = 1
	SCANCODE% = R + 1
	FileColr% = 15
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Spacing% = 1
	Row% = 4
	Column% = 32
	ITEM$(R + 1) = "SAVE AND EXIT"
CALL box0(3, 31, 3 + R + 2, 48, 2, 15)
	
_MouseShow "DEFAULT"
MMenuVert VARPTR(ITEM$(1)), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

	COLOR , 0
	IF SCANCODE% = 27 THEN 10
	NR = SELECTION%
	IF NR = R + 1 THEN 76
	NT$ = STR$(NN%(SELECTION%))
	NT$ = QPTRIM$(NT$)
	GOTO SEEDBRACKET
	END

4505 JJ = 4
	LOCATE 6, 8: COLOR 15, 1
	PRINT " "; CHR$(201); STRING$(61, 205); CHR$(187); " ";
	FOR II = 1 TO JJ: LOCATE 6 + II, 8
	PRINT " º"; STRING$(61, 32); "º "; : NEXT
	LOCATE 7 + JJ, 8
	PRINT " "; CHR$(200); STRING$(61, 205); CHR$(188); " "; : LOCATE 7
	RETURN

4500 LOCATE 10, 8

	CALL CSMsgBox("NO TEAMS MATCHING ID WERE FOUND... PRESS ANY KEY TO SELECT ANOTHER ID", 26, Cnf)
	I$ = GetKeyPress$
	CALL CSMsgBox("", 0, Cnf)
	RETURN

100 '4 TEAM REGION
	TC = TC + 1
	REGION = NR
	GAME = 1
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 3, 0)
	NG%(TC, 1) = N%(NR, 2, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 3, 0)
	H$(TC) = N$(NR, 2, 0)
	YN$(TC, 0) = N$(NR, 3, 1)
	YN$(TC, 1) = N$(NR, 2, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 2
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 4, 0)
	NG%(TC, 1) = N%(NR, 1, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 4, 0)
	H$(TC) = N$(NR, 1, 0)
	YN$(TC, 0) = N$(NR, 4, 1)
	YN$(TC, 1) = N$(NR, 1, 1)
	CALL WRITEGAME
	 REGION = NR
	IF C%(0) = 0 THEN 105: '1-YES, 0-NO CONSOLATION
	 TC = TC + 1
	 GAME = 3
	 ROUND = 88
	CALL SETTINGS
	NG%(TC, 0) = 801
	NG%(TC, 1) = 802
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 1 LOSER"
	H$(TC) = "REG" + STR$(REGION) + " GM 2 LOSER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
		TC = TC + 1
	 REGION = NR
	 GAME = 4
	 ROUND = 89
	CALL SETTINGS
	NG%(TC, 0) = 901
	NG%(TC, 1) = 902
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 1 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 2 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
		GOTO 50

105   TC = TC + 1
	 REGION = NR
	 GAME = 3
	 ROUND = 89
	CALL SETTINGS
	NG%(TC, 0) = 901
	NG%(TC, 1) = 902
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 1 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 2 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 GOTO 50

200 '5 TEAM REGION
	TC = TC + 1
	REGION = NR
	GAME = 1
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 5, 0)
	NG%(TC, 1) = N%(NR, 4, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 5, 0)
	H$(TC) = N$(NR, 4, 0)
	YN$(TC, 0) = N$(NR, 5, 1)
	YN$(TC, 1) = N$(NR, 4, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 2
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = 901
	NG%(TC, 1) = N%(NR, 1, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 1 WINNER"
	H$(TC) = N$(NR, 1, 0)
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = N$(NR, 1, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 3
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 3, 0)
	NG%(TC, 1) = N%(NR, 2, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 3, 0)
	H$(TC) = N$(NR, 2, 0)
	YN$(TC, 0) = N$(NR, 3, 1)
	YN$(TC, 1) = N$(NR, 2, 1)
	CALL WRITEGAME
	 REGION = NR
	IF C%(0) = 0 THEN 205: '1-YES, 0-NO CONSOLATION
	TC = TC + 1
	 GAME = 4
	 ROUND = 88
	CALL SETTINGS
	NG%(TC, 0) = 802
	NG%(TC, 1) = 803
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 2 LOSER"
	H$(TC) = "REG" + STR$(REGION) + " GM 3 LOSER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
		TC = TC + 1
	 REGION = NR
	 GAME = 5
	 ROUND = 89
	 CALL SETTINGS
	NG%(TC, 0) = 902
	NG%(TC, 1) = 903
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 2 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 3 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 GOTO 50

205   TC = TC + 1
	 REGION = NR
	 GAME = 4
	 ROUND = 89
	 CALL SETTINGS
	NG%(TC, 0) = 902
	NG%(TC, 1) = 903
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 2 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 3 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 GOTO 50

300 '6 TEAM REGION
	TC = TC + 1
	REGION = NR
	GAME = 1
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 5, 0)
	NG%(TC, 1) = N%(NR, 4, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 5, 0)
	H$(TC) = N$(NR, 4, 0)
	YN$(TC, 0) = N$(NR, 5, 1)
	YN$(TC, 1) = N$(NR, 4, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 2
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 6, 0)
	NG%(TC, 1) = N%(NR, 3, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 6, 0)
	H$(TC) = N$(NR, 3, 0)
	YN$(TC, 0) = N$(NR, 6, 1)
	YN$(TC, 1) = N$(NR, 3, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 3
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = 901
	NG%(TC, 1) = N%(NR, 1, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 1 WINNER"
	H$(TC) = N$(NR, 1, 0)
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = N$(NR, 1, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 4
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = 902
	NG%(TC, 1) = N%(NR, 2, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 2 WINNER"
	H$(TC) = N$(NR, 2, 0)
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = N$(NR, 2, 1)
	CALL WRITEGAME
	 REGION = NR
	IF C%(0) = 0 THEN 305: '1-YES, 0-NO CONSOLATION
	TC = TC + 1
	 GAME = 5
	 ROUND = 88
	CALL SETTINGS
	NG%(TC, 0) = 803
	NG%(TC, 1) = 804
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 3 LOSER"
	H$(TC) = "REG" + STR$(REGION) + " GM 4 LOSER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
		TC = TC + 1
	 REGION = NR
	 GAME = 6
	 ROUND = 89
	CALL SETTINGS
	NG%(TC, 0) = 903
	NG%(TC, 1) = 904
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 3 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 4 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
		GOTO 50

305   TC = TC + 1
	 REGION = NR
	 GAME = 5
	 ROUND = 89
	CALL SETTINGS
	NG%(TC, 0) = 903
	NG%(TC, 1) = 904
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 3 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 4 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 GOTO 50

400 '7 TEAM REGION
	TC = TC + 1
	REGION = NR
	GAME = 1
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 6, 0)
	NG%(TC, 1) = N%(NR, 3, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 6, 0)
	H$(TC) = N$(NR, 3, 0)
	YN$(TC, 0) = N$(NR, 6, 1)
	YN$(TC, 1) = N$(NR, 3, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 2
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 7, 0)
	NG%(TC, 1) = N%(NR, 2, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 7, 0)
	H$(TC) = N$(NR, 2, 0)
	YN$(TC, 0) = N$(NR, 7, 1)
	YN$(TC, 1) = N$(NR, 2, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 3
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 5, 0)
	NG%(TC, 1) = N%(NR, 4, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 5, 0)
	H$(TC) = N$(NR, 4, 0)
	YN$(TC, 0) = N$(NR, 5, 1)
	YN$(TC, 1) = N$(NR, 4, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 4
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = 901
	NG%(TC, 1) = 902
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 1 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 2 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 5
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = 903
	NG%(TC, 1) = N%(NR, 1, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 3 WINNER"
	H$(TC) = N$(NR, 1, 0)
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = N$(NR, 1, 1)
	CALL WRITEGAME
	 REGION = NR
	IF C%(0) = 0 THEN 405: '1-YES, 0-NO CONSOLATION
	TC = TC + 1
	 GAME = 6
	 ROUND = 88
	CALL SETTINGS
	NG%(TC, 0) = 804
	NG%(TC, 1) = 805
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 4 LOSER"
	H$(TC) = "REG" + STR$(REGION) + " GM 5 LOSER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
		TC = TC + 1
	 REGION = NR
	 GAME = 7
	 ROUND = 89
	CALL SETTINGS
	NG%(TC, 0) = 904
	NG%(TC, 1) = 905
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 4 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 5 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 GOTO 50

405   TC = TC + 1
	 REGION = NR
	 GAME = 6
	 ROUND = 89
	CALL SETTINGS
	NG%(TC, 0) = 904
	NG%(TC, 1) = 905
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 4 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 5 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 GOTO 50

500 '8 TEAM REGION
	TC = TC + 1
	REGION = NR
	GAME = 1
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 7, 0)
	NG%(TC, 1) = N%(NR, 2, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 7, 0)
	H$(TC) = N$(NR, 2, 0)
	YN$(TC, 0) = N$(NR, 7, 1)
	YN$(TC, 1) = N$(NR, 2, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 2
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 6, 0)
	NG%(TC, 1) = N%(NR, 3, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 6, 0)
	H$(TC) = N$(NR, 3, 0)
	YN$(TC, 0) = N$(NR, 6, 1)
	YN$(TC, 1) = N$(NR, 3, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 3
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 5, 0)
	NG%(TC, 1) = N%(NR, 4, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 5, 0)
	H$(TC) = N$(NR, 4, 0)
	YN$(TC, 0) = N$(NR, 5, 1)
	YN$(TC, 1) = N$(NR, 4, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 4
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 8, 0)
	NG%(TC, 1) = N%(NR, 1, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 8, 0)
	H$(TC) = N$(NR, 1, 0)
	YN$(TC, 0) = N$(NR, 8, 1)
	YN$(TC, 1) = N$(NR, 1, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 5
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = 901
	NG%(TC, 1) = 902
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 1 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 2 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 6
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = 903
	NG%(TC, 1) = 904
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 3 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 4 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 REGION = NR
	IF C%(0) = 0 THEN 505: '1-YES, 0-NO CONSOLATION
	TC = TC + 1
	 GAME = 7
	 ROUND = 88
	CALL SETTINGS
	NG%(TC, 0) = 805
	NG%(TC, 1) = 806
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 5 LOSER"
	H$(TC) = "REG" + STR$(REGION) + " GM 6 LOSER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
		TC = TC + 1
	 REGION = NR
	 GAME = 8
	 ROUND = 89
	CALL SETTINGS
	NG%(TC, 0) = 905
	NG%(TC, 1) = 906
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 5 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 6 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 GOTO 50

505   TC = TC + 1
	 REGION = NR
	 GAME = 7
	 ROUND = 89
	CALL SETTINGS
	NG%(TC, 0) = 905
	NG%(TC, 1) = 906
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 5 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 6 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 GOTO 50

600 '9 TEAM REGION
	TC = TC + 1
	REGION = NR
	GAME = 1
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 9, 0)
	NG%(TC, 1) = N%(NR, 8, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 9, 0)
	H$(TC) = N$(NR, 8, 0)
	YN$(TC, 0) = N$(NR, 9, 1)
	YN$(TC, 1) = N$(NR, 8, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 2
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 7, 0)
	NG%(TC, 1) = N%(NR, 2, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 7, 0)
	H$(TC) = N$(NR, 2, 0)
	YN$(TC, 0) = N$(NR, 7, 1)
	YN$(TC, 1) = N$(NR, 2, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 3
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 6, 0)
	NG%(TC, 1) = N%(NR, 3, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 6, 0)
	H$(TC) = N$(NR, 3, 0)
	YN$(TC, 0) = N$(NR, 6, 1)
	YN$(TC, 1) = N$(NR, 3, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 4
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 5, 0)
	NG%(TC, 1) = N%(NR, 4, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 5, 0)
	H$(TC) = N$(NR, 4, 0)
	YN$(TC, 0) = N$(NR, 5, 1)
	YN$(TC, 1) = N$(NR, 4, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 5
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = 901
	NG%(TC, 1) = N%(NR, 1, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 1 WINNER"
	H$(TC) = N$(NR, 1, 0)
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = N$(NR, 1, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 6
	ROUND = 3
	CALL SETTINGS
	NG%(TC, 0) = 902
	NG%(TC, 1) = 903
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 2 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 3 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 7
	ROUND = 3
	CALL SETTINGS
	NG%(TC, 0) = 904
	NG%(TC, 1) = 905
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 4 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 5 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 REGION = NR
	IF C%(0) = 0 THEN 605: '1-YES, 0-NO CONSOLATION
	TC = TC + 1
	 GAME = 8
	 ROUND = 88
	CALL SETTINGS
	NG%(TC, 0) = 806
	NG%(TC, 1) = 807
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 6 LOSER"
	H$(TC) = "REG" + STR$(REGION) + " GM 7 LOSER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
		TC = TC + 1
	 REGION = NR
	 GAME = 9
	 ROUND = 89
	CALL SETTINGS
	NG%(TC, 0) = 906
	NG%(TC, 1) = 907
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 6 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 7 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
		GOTO 50

605   TC = TC + 1
	 REGION = NR
	 GAME = 8
	 ROUND = 89
	CALL SETTINGS
	NG%(TC, 0) = 906
	NG%(TC, 1) = 907
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 6 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 7 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 GOTO 50

700 '10 TEAM REGION
	TC = TC + 1
	REGION = NR
	GAME = 1
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 10, 0)
	NG%(TC, 1) = N%(NR, 7, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 10, 0)
	H$(TC) = N$(NR, 7, 0)
	YN$(TC, 0) = N$(NR, 10, 1)
	YN$(TC, 1) = N$(NR, 7, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 2
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 9, 0)
	NG%(TC, 1) = N%(NR, 8, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 9, 0)
	H$(TC) = N$(NR, 8, 0)
	YN$(TC, 0) = N$(NR, 9, 1)
	YN$(TC, 1) = N$(NR, 8, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 3
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = 901
	NG%(TC, 1) = N%(NR, 2, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 1 WINNER"
	H$(TC) = N$(NR, 2, 0)
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = N$(NR, 2, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 4
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 6, 0)
	NG%(TC, 1) = N%(NR, 3, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 6, 0)
	H$(TC) = N$(NR, 3, 0)
	YN$(TC, 0) = N$(NR, 6, 1)
	YN$(TC, 1) = N$(NR, 3, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 5
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 5, 0)
	NG%(TC, 1) = N%(NR, 4, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 5, 0)
	H$(TC) = N$(NR, 4, 0)
	YN$(TC, 0) = N$(NR, 5, 1)
	YN$(TC, 1) = N$(NR, 4, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 6
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = 902
	NG%(TC, 1) = N%(NR, 1, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 2 WINNER"
	H$(TC) = N$(NR, 1, 0)
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = N$(NR, 1, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 7
	ROUND = 3
	CALL SETTINGS
	NG%(TC, 0) = 903
	NG%(TC, 1) = 904
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 3 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 4 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 8
	ROUND = 3
	CALL SETTINGS
	NG%(TC, 0) = 905
	NG%(TC, 1) = 906
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 5 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 6 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 REGION = NR
	IF C%(0) = 0 THEN 705: '1-YES, 0-NO CONSOLATION
	TC = TC + 1
	 GAME = 9
	 ROUND = 88
	CALL SETTINGS
	NG%(TC, 0) = 807
	NG%(TC, 1) = 808
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 7 LOSER"
	H$(TC) = "REG" + STR$(REGION) + " GM 8 LOSER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
		TC = TC + 1
	 REGION = NR
	 GAME = 10
	 ROUND = 89
	CALL SETTINGS
	NG%(TC, 0) = 907
	NG%(TC, 1) = 908
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 7 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 8 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 GOTO 50

705   TC = TC + 1
	 REGION = NR
	 GAME = 9
	 ROUND = 89
	CALL SETTINGS
	NG%(TC, 0) = 907
	NG%(TC, 1) = 908
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 7 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 8 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 GOTO 50

800 '11 TEAM REGION
	TC = TC + 1
	REGION = NR
	GAME = 1
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 10, 0)
	NG%(TC, 1) = N%(NR, 7, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 10, 0)
	H$(TC) = N$(NR, 7, 0)
	YN$(TC, 0) = N$(NR, 10, 1)
	YN$(TC, 1) = N$(NR, 7, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 2
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 11, 0)
	NG%(TC, 1) = N%(NR, 6, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 11, 0)
	H$(TC) = N$(NR, 6, 0)
	YN$(TC, 0) = N$(NR, 11, 1)
	YN$(TC, 1) = N$(NR, 6, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 3
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 9, 0)
	NG%(TC, 1) = N%(NR, 8, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 9, 0)
	H$(TC) = N$(NR, 8, 0)
	YN$(TC, 0) = N$(NR, 9, 1)
	YN$(TC, 1) = N$(NR, 8, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 4
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = 901
	NG%(TC, 1) = N%(NR, 2, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 1 WINNER"
	H$(TC) = N$(NR, 2, 0)
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = N$(NR, 2, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 5
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = 902
	NG%(TC, 1) = N%(NR, 3, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 2 WINNER"
	H$(TC) = N$(NR, 3, 0)
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = N$(NR, 3, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 6
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 5, 0)
	NG%(TC, 1) = N%(NR, 4, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 5, 0)
	H$(TC) = N$(NR, 4, 0)
	YN$(TC, 0) = N$(NR, 5, 1)
	YN$(TC, 1) = N$(NR, 4, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 7
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = 903
	NG%(TC, 1) = N%(NR, 1, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 3 WINNER"
	H$(TC) = N$(NR, 1, 0)
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = N$(NR, 1, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 8
	ROUND = 3
	CALL SETTINGS
	NG%(TC, 0) = 904
	NG%(TC, 1) = 905
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 4 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 5 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 9
	ROUND = 3
	CALL SETTINGS
	NG%(TC, 0) = 906
	NG%(TC, 1) = 907
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 6 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 7 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 REGION = NR
	IF C%(0) = 0 THEN 805: '1-YES, 0-NO CONSOLATION
	TC = TC + 1
	 GAME = 10
	 ROUND = 88
	CALL SETTINGS
	NG%(TC, 0) = 808
	NG%(TC, 1) = 809
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 8 LOSER"
	H$(TC) = "REG" + STR$(REGION) + " GM 9 LOSER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
		TC = TC + 1
	 REGION = NR
	 GAME = 11
	 ROUND = 89
	CALL SETTINGS
	NG%(TC, 0) = 908
	NG%(TC, 1) = 909
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 8 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 9 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 GOTO 50

805   TC = TC + 1
	 REGION = NR
	 GAME = 10
	 ROUND = 89
	CALL SETTINGS
	NG%(TC, 0) = 908
	NG%(TC, 1) = 909
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 8 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 9 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 GOTO 50

900 '12 TEAM REGION
	TC = TC + 1
	REGION = NR
	GAME = 1
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 10, 0)
	NG%(TC, 1) = N%(NR, 7, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 10, 0)
	H$(TC) = N$(NR, 7, 0)
	YN$(TC, 0) = N$(NR, 10, 1)
	YN$(TC, 1) = N$(NR, 7, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 2
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 11, 0)
	NG%(TC, 1) = N%(NR, 6, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 11, 0)
	H$(TC) = N$(NR, 6, 0)
	YN$(TC, 0) = N$(NR, 11, 1)
	YN$(TC, 1) = N$(NR, 6, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 3
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 12, 0)
	NG%(TC, 1) = N%(NR, 5, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 12, 0)
	H$(TC) = N$(NR, 5, 0)
	YN$(TC, 0) = N$(NR, 12, 1)
	YN$(TC, 1) = N$(NR, 5, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 4
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 9, 0)
	NG%(TC, 1) = N%(NR, 8, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 9, 0)
	H$(TC) = N$(NR, 8, 0)
	YN$(TC, 0) = N$(NR, 9, 1)
	YN$(TC, 1) = N$(NR, 8, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 5
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = 901
	NG%(TC, 1) = N%(NR, 2, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 1 WINNER"
	H$(TC) = N$(NR, 2, 0)
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = N$(NR, 2, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 6
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = 902
	NG%(TC, 1) = N%(NR, 3, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 2 WINNER"
	H$(TC) = N$(NR, 3, 0)
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = N$(NR, 3, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 7
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = 903
	NG%(TC, 1) = N%(NR, 4, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 3 WINNER"
	H$(TC) = N$(NR, 4, 0)
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = N$(NR, 4, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 8
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = 904
	NG%(TC, 1) = N%(NR, 1, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 4 WINNER"
	H$(TC) = N$(NR, 1, 0)
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = N$(NR, 1, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 9
	ROUND = 3
	CALL SETTINGS
	NG%(TC, 0) = 905
	NG%(TC, 1) = 906
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 5 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 6 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 10
	ROUND = 3
	CALL SETTINGS
	NG%(TC, 0) = 907
	NG%(TC, 1) = 908
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 7 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 8 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 REGION = NR
	IF C%(0) = 0 THEN 905: '1-YES, 0-NO CONSOLATION
	TC = TC + 1
	 GAME = 11
	 ROUND = 88
	CALL SETTINGS
	NG%(TC, 0) = 809
	NG%(TC, 1) = 810
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 9 LOSER"
	H$(TC) = "REG" + STR$(REGION) + " GM 10 LOSER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
		TC = TC + 1
	 REGION = NR
	 GAME = 12
	 ROUND = 89
	CALL SETTINGS
	NG%(TC, 0) = 909
	NG%(TC, 1) = 910
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 9 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 10 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 GOTO 50

905   TC = TC + 1
	 REGION = NR
	 GAME = 11
	 ROUND = 89
	CALL SETTINGS
	NG%(TC, 0) = 909
	NG%(TC, 1) = 910
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 9 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 10 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 GOTO 50

1000 '13 TEAM REGION
	TC = TC + 1
	REGION = NR
	GAME = 1
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 13, 0)
	NG%(TC, 1) = N%(NR, 4, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 13, 0)
	H$(TC) = N$(NR, 4, 0)
	YN$(TC, 0) = N$(NR, 13, 1)
	YN$(TC, 1) = N$(NR, 4, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 2
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 12, 0)
	NG%(TC, 1) = N%(NR, 5, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 12, 0)
	H$(TC) = N$(NR, 5, 0)
	YN$(TC, 0) = N$(NR, 12, 1)
	YN$(TC, 1) = N$(NR, 5, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 3
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 9, 0)
	NG%(TC, 1) = N%(NR, 8, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 9, 0)
	H$(TC) = N$(NR, 8, 0)
	YN$(TC, 0) = N$(NR, 9, 1)
	YN$(TC, 1) = N$(NR, 8, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 4
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 10, 0)
	NG%(TC, 1) = N%(NR, 7, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 10, 0)
	H$(TC) = N$(NR, 7, 0)
	YN$(TC, 0) = N$(NR, 10, 1)
	YN$(TC, 1) = N$(NR, 7, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 5
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 11, 0)
	NG%(TC, 1) = N%(NR, 6, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 11, 0)
	H$(TC) = N$(NR, 6, 0)
	YN$(TC, 0) = N$(NR, 11, 1)
	YN$(TC, 1) = N$(NR, 6, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 6
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = 901
	NG%(TC, 1) = 902
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 1 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 2 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 7
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = 903
	NG%(TC, 1) = N%(NR, 1, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 3 WINNER"
	H$(TC) = N$(NR, 1, 0)
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = N$(NR, 1, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 8
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = 904
	NG%(TC, 1) = N%(NR, 2, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 4 WINNER"
	H$(TC) = N$(NR, 2, 0)
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = N$(NR, 2, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 9
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = 905
	NG%(TC, 1) = N%(NR, 3, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 5 WINNER"
	H$(TC) = N$(NR, 3, 0)
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = N$(NR, 3, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 10
	ROUND = 3
	CALL SETTINGS
	NG%(TC, 0) = 906
	NG%(TC, 1) = 907
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 6 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 7 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 11
	ROUND = 3
	CALL SETTINGS
	NG%(TC, 0) = 908
	NG%(TC, 1) = 909
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 8 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 9 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 REGION = NR
	IF C%(0) = 0 THEN 1005: '1-YES, 0-NO CONSOLATION
	TC = TC + 1
	 GAME = 12
	 ROUND = 88
	CALL SETTINGS
	NG%(TC, 0) = 810
	NG%(TC, 1) = 811
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 10 LOSER"
	H$(TC) = "REG" + STR$(REGION) + " GM 11 LOSER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
		TC = TC + 1
	 REGION = NR
	 GAME = 13
	 ROUND = 89
	CALL SETTINGS
	NG%(TC, 0) = 910
	NG%(TC, 1) = 911
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 10 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 11 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 GOTO 50

1005  TC = TC + 1
	 REGION = NR
	 GAME = 12
	 ROUND = 89
	CALL SETTINGS
	NG%(TC, 0) = 910
	NG%(TC, 1) = 911
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 10 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 11 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 GOTO 50

1100 '14 TEAM REGION
	TC = TC + 1
	REGION = NR
	GAME = 1
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 14, 0)
	NG%(TC, 1) = N%(NR, 3, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 14, 0)
	H$(TC) = N$(NR, 3, 0)
	YN$(TC, 0) = N$(NR, 14, 1)
	YN$(TC, 1) = N$(NR, 3, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 2
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 13, 0)
	NG%(TC, 1) = N%(NR, 4, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 13, 0)
	H$(TC) = N$(NR, 4, 0)
	YN$(TC, 0) = N$(NR, 13, 1)
	YN$(TC, 1) = N$(NR, 4, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 3
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 12, 0)
	NG%(TC, 1) = N%(NR, 5, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 12, 0)
	H$(TC) = N$(NR, 5, 0)
	YN$(TC, 0) = N$(NR, 12, 1)
	YN$(TC, 1) = N$(NR, 5, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 4
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 11, 0)
	NG%(TC, 1) = N%(NR, 6, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 11, 0)
	H$(TC) = N$(NR, 6, 0)
	YN$(TC, 0) = N$(NR, 11, 1)
	YN$(TC, 1) = N$(NR, 6, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 5
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 10, 0)
	NG%(TC, 1) = N%(NR, 7, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 10, 0)
	H$(TC) = N$(NR, 7, 0)
	YN$(TC, 0) = N$(NR, 10, 1)
	YN$(TC, 1) = N$(NR, 7, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 6
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 9, 0)
	NG%(TC, 1) = N%(NR, 8, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 9, 0)
	H$(TC) = N$(NR, 8, 0)
	YN$(TC, 0) = N$(NR, 9, 1)
	YN$(TC, 1) = N$(NR, 8, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 7
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = 901
	NG%(TC, 1) = 904
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 1 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 4 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 8
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = 905
	NG%(TC, 1) = N%(NR, 2, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 5 WINNER"
	H$(TC) = N$(NR, 2, 0)
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = N$(NR, 2, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 9
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = 902
	NG%(TC, 1) = 903
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 2 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 3 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 10
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = 906
	NG%(TC, 1) = N%(NR, 1, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 6 WINNER"
	H$(TC) = N$(NR, 1, 0)
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = N$(NR, 1, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 11
	ROUND = 3
	CALL SETTINGS
	NG%(TC, 0) = 907
	NG%(TC, 1) = 908
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 7 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 8 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 12
	ROUND = 3
	CALL SETTINGS
	NG%(TC, 0) = 909
	NG%(TC, 1) = 910
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 9 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 10 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 REGION = NR
	IF C%(0) = 0 THEN 1105: '1-YES, 0-NO CONSOLATION
	TC = TC + 1
	 GAME = 13
	 ROUND = 88
	CALL SETTINGS
	NG%(TC, 0) = 811
	NG%(TC, 1) = 812
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 11 LOSER"
	H$(TC) = "REG" + STR$(REGION) + " GM 12 LOSER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
		TC = TC + 1
	 REGION = NR
	 GAME = 14
	 ROUND = 89
	CALL SETTINGS
	NG%(TC, 0) = 911
	NG%(TC, 1) = 912
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 11 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 12 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 GOTO 50

1105  TC = TC + 1
	 REGION = NR
	 GAME = 13
	 ROUND = 89
	CALL SETTINGS
	NG%(TC, 0) = 911
	NG%(TC, 1) = 912
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 11 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 12 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 GOTO 50

1200 '15 TEAM REGION
	TC = TC + 1
	REGION = NR
	GAME = 1
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 15, 0)
	NG%(TC, 1) = N%(NR, 2, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 15, 0)
	H$(TC) = N$(NR, 2, 0)
	YN$(TC, 0) = N$(NR, 15, 1)
	YN$(TC, 1) = N$(NR, 2, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 2
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 14, 0)
	NG%(TC, 1) = N%(NR, 3, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 14, 0)
	H$(TC) = N$(NR, 3, 0)
	YN$(TC, 0) = N$(NR, 14, 1)
	YN$(TC, 1) = N$(NR, 3, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 3
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 13, 0)
	NG%(TC, 1) = N%(NR, 4, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 13, 0)
	H$(TC) = N$(NR, 4, 0)
	YN$(TC, 0) = N$(NR, 13, 1)
	YN$(TC, 1) = N$(NR, 4, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 4
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 12, 0)
	NG%(TC, 1) = N%(NR, 5, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 12, 0)
	H$(TC) = N$(NR, 5, 0)
	YN$(TC, 0) = N$(NR, 12, 1)
	YN$(TC, 1) = N$(NR, 5, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 5
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 11, 0)
	NG%(TC, 1) = N%(NR, 6, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 11, 0)
	H$(TC) = N$(NR, 6, 0)
	YN$(TC, 0) = N$(NR, 11, 1)
	YN$(TC, 1) = N$(NR, 6, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 6
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 10, 0)
	NG%(TC, 1) = N%(NR, 7, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 10, 0)
	H$(TC) = N$(NR, 7, 0)
	YN$(TC, 0) = N$(NR, 10, 1)
	YN$(TC, 1) = N$(NR, 7, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 7
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 9, 0)
	NG%(TC, 1) = N%(NR, 8, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 9, 0)
	H$(TC) = N$(NR, 8, 0)
	YN$(TC, 0) = N$(NR, 9, 1)
	YN$(TC, 1) = N$(NR, 8, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 8
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = 901
	NG%(TC, 1) = 906
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 1 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 6 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 9
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = 902
	NG%(TC, 1) = 905
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 2 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 5 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 10
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = 903
	NG%(TC, 1) = 904
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 3 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 4 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 11
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = 907
	NG%(TC, 1) = N%(NR, 1, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 7 WINNER"
	H$(TC) = N$(NR, 1, 0)
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = N$(NR, 1, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 12
	ROUND = 3
	CALL SETTINGS
	NG%(TC, 0) = 908
	NG%(TC, 1) = 909
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 8 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 9 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 13
	ROUND = 3
	CALL SETTINGS
	NG%(TC, 0) = 910
	NG%(TC, 1) = 911
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 10 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 11 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 REGION = NR
	IF C%(0) = 0 THEN 1205: '1-YES, 0-NO CONSOLATION
	TC = TC + 1
	 GAME = 14
	 ROUND = 88
	CALL SETTINGS
	NG%(TC, 0) = 812
	NG%(TC, 1) = 813
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 12 LOSER"
	H$(TC) = "REG" + STR$(REGION) + " GM 13 LOSER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
		TC = TC + 1
	 REGION = NR
	 GAME = 15
	 ROUND = 89
	CALL SETTINGS
	NG%(TC, 0) = 912
	NG%(TC, 1) = 913
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 12 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 13 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 GOTO 50

1205  TC = TC + 1
	 REGION = NR
	 GAME = 14
	 ROUND = 89
	CALL SETTINGS
	NG%(TC, 0) = 912
	NG%(TC, 1) = 913
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 12 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 13 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 GOTO 50

1300 '16 TEAM REGION
	TC = TC + 1
	REGION = NR
	GAME = 1
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 16, 0)
	NG%(TC, 1) = N%(NR, 1, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 16, 0)
	H$(TC) = N$(NR, 1, 0)
	YN$(TC, 0) = N$(NR, 16, 1)
	YN$(TC, 1) = N$(NR, 1, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 2
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 15, 0)
	NG%(TC, 1) = N%(NR, 2, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 15, 0)
	H$(TC) = N$(NR, 2, 0)
	YN$(TC, 0) = N$(NR, 15, 1)
	YN$(TC, 1) = N$(NR, 2, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 3
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 14, 0)
	NG%(TC, 1) = N%(NR, 3, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 14, 0)
	H$(TC) = N$(NR, 3, 0)
	YN$(TC, 0) = N$(NR, 14, 1)
	YN$(TC, 1) = N$(NR, 3, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 4
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 13, 0)
	NG%(TC, 1) = N%(NR, 4, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 13, 0)
	H$(TC) = N$(NR, 4, 0)
	YN$(TC, 0) = N$(NR, 13, 1)
	YN$(TC, 1) = N$(NR, 4, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 5
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 12, 0)
	NG%(TC, 1) = N%(NR, 5, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 12, 0)
	H$(TC) = N$(NR, 5, 0)
	YN$(TC, 0) = N$(NR, 12, 1)
	YN$(TC, 1) = N$(NR, 5, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 6
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 11, 0)
	NG%(TC, 1) = N%(NR, 6, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 11, 0)
	H$(TC) = N$(NR, 6, 0)
	YN$(TC, 0) = N$(NR, 11, 1)
	YN$(TC, 1) = N$(NR, 6, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 7
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 10, 0)
	NG%(TC, 1) = N%(NR, 7, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 10, 0)
	H$(TC) = N$(NR, 7, 0)
	YN$(TC, 0) = N$(NR, 10, 1)
	YN$(TC, 1) = N$(NR, 7, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 8
	ROUND = 1
	CALL SETTINGS
	NG%(TC, 0) = N%(NR, 9, 0)
	NG%(TC, 1) = N%(NR, 8, 0)
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = N$(NR, 9, 0)
	H$(TC) = N$(NR, 8, 0)
	YN$(TC, 0) = N$(NR, 9, 1)
	YN$(TC, 1) = N$(NR, 8, 1)
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 9
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = 901
	NG%(TC, 1) = 908
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 1 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 8 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 10
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = 904
	NG%(TC, 1) = 905
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 4 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 5 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 11
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = 903
	NG%(TC, 1) = 906
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 3 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 6 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 12
	ROUND = 2
	CALL SETTINGS
	NG%(TC, 0) = 902
	NG%(TC, 1) = 907
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 2 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 7 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 13
	ROUND = 3
	CALL SETTINGS
	NG%(TC, 0) = 909
	NG%(TC, 1) = 910
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 9 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 10 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 TC = TC + 1
	REGION = NR
	GAME = 14
	ROUND = 3
	CALL SETTINGS
	NG%(TC, 0) = 911
	NG%(TC, 1) = 912
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 11 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 12 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 REGION = NR
	IF C%(0) = 0 THEN 1305: '1-YES, 0-NO CONSOLATION
	TC = TC + 1
	 GAME = 15
	 ROUND = 88
	CALL SETTINGS
	NG%(TC, 0) = 813
	NG%(TC, 1) = 814
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 13 LOSER"
	H$(TC) = "REG" + STR$(REGION) + " GM 14 LOSER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
		TC = TC + 1
	 REGION = NR
	 GAME = 16
	 ROUND = 89
	CALL SETTINGS
	NG%(TC, 0) = 913
	NG%(TC, 1) = 914
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 13 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 14 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 GOTO 50

1305  TC = TC + 1
	 REGION = NR
	 GAME = 15
	 ROUND = 89
	CALL SETTINGS
	NG%(TC, 0) = 913
	NG%(TC, 1) = 914
	NG%(TC, 15) = REGION
	NG%(TC, 16) = ROUND
	NG%(TC, 17) = GAME
	V$(TC) = "REG" + STR$(REGION) + " GM 13 WINNER"
	H$(TC) = "REG" + STR$(REGION) + " GM 14 WINNER"
	YN$(TC, 0) = "XXX"
	YN$(TC, 1) = "XXX"
	CALL WRITEGAME
	 GOTO 50

1104 NT = 0
	IF NOT _FILEEXISTS(D$(0) + "BASK." + YN$) THEN GOSUB 4500: BO% = 1: RETURN
OPEN D$(0) + "BASK." + YN$ FOR RANDOM AS #1 LEN = 655
	Y = LOF(1)
	FIELD #1, 15 AS Q$(0)
	FOR I1 = 1 TO Y / 655
	GET #1, I1
	FOR I2 = 1 TO 15
	IF ASC(MID$(Q$(0), I2, 1)) < 32 OR ASC(MID$(Q$(0), I2, 1)) > 123 OR UCASE$(LEFT$(Q$(0), 3)) = "NOT" OR UCASE$(LEFT$(Q$(0), 3)) = "UN" THEN 1103
	NEXT I2
	NT = NT + 1
	VV$(NT) = Q$(0): V%(NT) = I1

1103 NEXT I1
CLOSE 1

1102 SELECTION% = 1
	Count% = NT
	Start% = 1
	FileFGColr% = 15:FileBGColr% = 4
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Rows% = 20
	Columns% = 4
	Spacing% = 6
	Row% = 4
	Column% = 1

1106 COLOR , 4: CLS : LOCATE , 33: COLOR 14: PRINT "TEAM SELECTION": COLOR 15
	LOCATE 25, 1: COLOR 14: PRINT "ESC"; : COLOR 15: PRINT "-SELECT NEW ID  PGDN  PGUP  "; : COLOR 14: PRINT "ENTER"; : COLOR 15: PRINT "-SELECTS TEAM";

_MouseShow "DEFAULT"
MAMenu VARPTR(VV$(1)), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

	IF EXITCODE% = 27 THEN BO% = 1: RETURN
	IF EXITCODE% = -81 THEN Start% = Start% + 80: SELECTION% = Start%
	IF EXITCODE% = -73 THEN Start% = Start% - 80: SELECTION% = Start%
	IF EXITCODE% = -71 THEN Start% = 1: SELECTION% = 1: GOTO 1106
	IF EXITCODE% = -79 THEN Start% = NT: SELECTION% = NT: GOTO 1106
	IF Start% < 1 THEN Start% = 1: SELECTION% = Start%
	IF Start% > NT THEN Start% = NT: SELECTION% = Start%
	IF EXITCODE% = -81 OR EXITCODE% = -73 THEN 1106
	D = V%(SELECTION%)
	LOCATE 25, 1: COLOR 14: PRINT "ESC"; : COLOR 15: PRINT "-SELECT NEW ID  LEFT/RIGHT ARROW  UP/DOWN ARROW  "; : COLOR 14: PRINT "ENTER"; : COLOR 15: PRINT "-SELECTS TEAM";
OPEN D$(0) + "BASK." + YN$ FOR RANDOM AS #1 LEN = 655
FIELD #1, 15 AS Q$(0), 2 AS Q$(1), 2 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 2 AS Q$(14), 2 AS Q$(15), 2 AS Q$(16), 2 AS Q$(17), 2 AS Q$(18), 2 _
AS Q$(19), 602 AS X$
GET #1, D
	A$ = Q$(0)

9005 IF RIGHT$(A$, 1) = " " THEN A$ = LEFT$(A$, LEN(A$) - 1): GOTO 9005
	SP = CVI(Q$(1)): DA = CVI(Q$(2)): V1% = CVI(Q$(3)): V2% = CVI(Q$(4))
	FOR I = 0 TO 4: L%(I) = CVI(Q$(I + 5)): NEXT
	FOR I = 0 TO 9: T%(I) = CVI(Q$(I + 10)): NEXT
CLOSE 1
	RETURN

610 ' *** LOAD IN A SAVED TOURNAMENT ***
	EDITCODE% = Q
	Spec$ = D$(0) + "*.TOU"
	null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
	Count% = DirCount%
	IF Count% = 0 THEN 4050
	REDIM F$(0 TO Count%)
	FOR X = 1 TO Count%
	  F$(X) = SPACE$(12)
	NEXT
	F$(0) = Spec$
	CALL ReadFile(F$())
	'CALL SortStr(VARPTR(F$(1)), Count, 0)
	 CLS
	COLOR , 1: CLS : LOCATE , 28: COLOR 14
	PRINT "SELECT TOURNAMENT FIELD TO EDIT": COLOR 15
	SELECTION% = 1
	Start% = 1
	FileFGColr% = 15:FileBGColr% = 1
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Rows% = 20
	Columns% = 4
	Spacing% = 6
	Row% = 4
	Column% = 1
	LOCATE 25, 1: COLOR 14: PRINT "ESC"; : COLOR 15
	PRINT "-SELECT NEW ID  PGDN  PGUP  "; : COLOR 14
	PRINT "ENTER"; : COLOR 15
	PRINT "-SELECTS TOURNAMENT FIELD";
	
_MouseShow "DEFAULT"
206  MAMenu F$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

	IF EXITCODE% = 27 THEN 10
	IF EXITCODE% = -71 THEN Start% = 1: EXITCODE% = 3: SELECTION% = 1: GOTO 206
	IF EXITCODE% = -79 THEN Start% = COUNT: EXITCODE% = 3: SELECTION% = COUNT: GOTO 206
	IF EXITCODE% = -73 THEN Start% = Start% - 80: IF Start% < 1 THEN Start% = 1
	IF EXITCODE% = -81 THEN Start% = Start% + 80: IF Start% > COUNT THEN Start% = COUNT
	IF EXITCODE% = -81 OR EXITCODE% = -73 THEN EXITCODE% = 3: SELECTION% = Start%: GOTO 206
	F$ = F$(SELECTION%)
	LOCATE 2, 33: PRINT F$
LOCATE , 33: PRINT "ANY CHANGE (YN)";
	COLOR 15, 0
604 I$ = GetKeyPress$
	IF UCASE$(I$) = "Y" THEN 610
	F$ = QPTRIM$(F$)
	F$ = LEFT$(F$, LEN(F$) - 4)
	IF EDITCODE% = 3 THEN 612
OPEN D$(0) + F$ + ".INF" FOR INPUT AS #2
	INPUT #2, FF$
	INPUT #2, R: ' NUMBER OF REGIONS
	FOR I = 1 TO 16
	INPUT #2, NN%(I): ' NUMBER OF TEAMS IN REGION
	INPUT #2, R$(I): ' NAME OF REGION
	NEXT
	FOR I = 1 TO 16
	FOR I1 = 1 TO 16
	FOR I2 = 0 TO 3
	INPUT #2, N$(I, I1, I2): ' SEED SLOT NAME
	INPUT #2, N%(I, I1, I2): ' REGION, SEED NUMBER, MODE PLAY/TM#
	NEXT: NEXT: NEXT
	INPUT #2, C%(0): INPUT #2, C%(1): ' CONSOLATION GAME
CLOSE 2
OPEN D$(0) + F$ + ".TNY" FOR INPUT AS #1
	FOR I = 0 TO 14
	INPUT #1, OP%(I)
	NEXT
CLOSE 1
	IF C%(0) = 1 THEN C$ = "Y" ELSE C$ = "N"
	NR$ = STR$(R)
	GOTO 20

'Basically creating copies of TCX files with TOU extension
612	CALL FCOPY (D$(0)+F$+".TOU", D$(0)+F$+".TCX", BUFFER$, ERRCODE%)
	'SHELL "COPY " + D$(0) + F$ + ".TCX " + D$(0) + F$ + ".TOU >NUL:"
	LOCATE 10
	
	CALL CSMsgBox("ALL FILES SAVED...PRESS ANY KEY TO RETURN TO TOURNAMENT OPTIONS", 18, Cnf)
	I$=GetKeyPress$
	CALL CSMsgBox("", 0, Cnf)
	
	EDITCODE% = 0
	GOTO 10
	  PRINT "THERE ARE "; R; " REGIONS"
	FOR I = 1 TO R
	PRINT "REGION"; I; " HAS "; NN%(I); " TEAMS"
	PRINT "REGION NAME IS "; R$(I)
	NEXT
	END

611  FOR I = 1 TO 16
	NN%(I) = 0
	R$(I) = ""
	NEXT
	FOR I = 1 TO 16
	FOR I1 = 1 TO 16
	FOR I2 = 0 TO 3
	N$(I, I1, I2) = ""
	N%(I, I1, I2) = 0
	NEXT: NEXT: NEXT
	C%(0) = 0: C%(1) = 0
RETURN

4050 LOCATE 10, 8

	CALL CSMsgBox("NO TOURNAMENT FILES WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Cnf)
	I$ = GetKeyPress$
	CALL CSMsgBox("", 0, Cnf)
	GOTO 10


FUNCTION FnExt$ (X$, Length) 
'Essentially this pads space to the right of the string
	X$ = X$ + SPACE$(Length - LEN(X$))  'for display only
END FUNCTION


SUB TITLE ()
	COLOR , 0: KEY OFF: CLS
	COLOR , 1: LOCATE 1, 1: PRINT TAB(80); : LOCATE , 16: COLOR 12
	PRINT "COURTSIDE COLLEGE BASKETBALL TOURNAMENT SCHEDULER"
	LOCATE 24, 1: PRINT TAB(80); : LOCATE , 1
	PRINT "(C) LANCE HAFFNER/SHANNON LYNN 1987-1994";
	LOCATE 3, 1: COLOR 2, 0
	COLOR 15: PRINT "        PROGRAMMED AND DEVELOPED BY LANCE HAFFNER AND SHANNON LYNN"
	  PRINT "    TECHNICAL SUPPORT, CALL (800) 477-7032 EMAIL AT SportGames@AOL.COM"
	COLOR 10
END SUB

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
