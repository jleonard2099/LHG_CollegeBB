'$INCLUDE: 'Config.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

DEFINT A-Z
DECLARE FUNCTION FnExt$ (X$, Length)

DIM Z1$(40), Z2$(40), Z1%(40), D$(2)
DIM Q$(15), VV$(200), V%(200), T%(34), L%(13), ITEM$(3)
DIM Y AS LONG

FG = 0
BG = 7
Colr = OneColor%(FG, BG)             'pack FG and BG into a single byte

OPEN "DEV.BAS" FOR INPUT AS #2
	FOR I = 0 TO 1
	INPUT #2, D$(I)
	NEXT
CLOSE 2

CLS

50 FOR X = 1 TO 40
	Z1$(X) = "XXX": Z2$(X) = "XXX": Z1%(X) = -1
	NEXT

60 CALL TITLE
	item$(1) = "ADD / DELETE /REVIEW CONFERENCES"
	item$(2) = "CREATE / DEFINE LEAGUE & MEMBERS"
	item$(3) = "RETURN TO STANDINGS/LEADERS MENU"
	SELECTION% = 1
	Count% = 3
	Start% = 1
	SCANCODE% = 3
	FileFGColr% = 15:FileBGColr% = 1
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Spacing% = 1
	Row% = 8
	Column% = 25

CALL box0(7, 24, 11, 57, 2, 31)	
_MouseShow "DEFAULT"
MMenuVert VARPTR(item$(1)), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

	COLOR , 0
	IF SCANCODE% = 27 THEN RUN "HELLO.EXE 3"
	I1 = SELECTION%
	IF I1 = 1 THEN 4000
	IF I1 = 3 THEN RUN "HELLO.EXE 3"

130 COLOR 15, 0: CLS
	JJ = 7
	LOCATE 6, 8: COLOR 15, 1
	PRINT " Ö"; STRING$(61, 196); "· "; : FOR II = 1 TO JJ: LOCATE 6 + II, 8
	PRINT " º"; STRING$(61, 32); "º "; : NEXT
	LOCATE 7 + JJ, 8
	PRINT " Ó"; STRING$(61, 196); "½ "; : LOCATE 7, 32
	LOCATE , 12: PRINT "ENTER NAME FOR LEAGUE: ": PRINT
	LOCATE , 12: PRINT "THE NAME IS 8 CHARACTERS LONG WITH NO PERIODS OR"
	LOCATE , 12: PRINT "SPACES. TO EXIT, JUST HIT 'ENTER'"
	LOCATE , 12: PRINT "SAMPLE NAMES ARE NCAA-90, SOUTH, MIDWEST"
	LOCATE 7, 37: INPUT LG$
	IF LG$ = "" THEN 60
	LG$ = LEFT$(LG$, 8)
	LOCATE 13, 12: PRINT "ANY CHANGE (YN) "

22  I$=GetKeyPress$
	IF UCASE$(I$) = "Y" THEN 130

220 COLOR 14, 0
	CLS
	PRINT
	PRINT "  "; LG$: PRINT : COLOR 12

230 FOR I = 1 TO 10
	COLOR 2: PRINT USING "## "; I; : COLOR 15: PRINT Z1$(I);
	LOCATE , 21: COLOR 2: PRINT USING "## "; I + 10; : COLOR 15: PRINT Z1$(I + 10);
	LOCATE , 41: COLOR 2: PRINT USING "## "; I + 20; : COLOR 15: PRINT Z1$(I + 20);
	LOCATE , 61: COLOR 2: PRINT USING "## "; I + 30; : COLOR 15: PRINT Z1$(I + 30)
	NEXT I

240 LOCATE 19, 1
	COLOR 14: PRINT "(1) "; : COLOR 15: PRINT "ADD A CONFERENCE"
	COLOR 14: PRINT "(2) "; : COLOR 15: PRINT "REMOVE A CONFERENCE"
	COLOR 14: PRINT "(3) "; : COLOR 15: PRINT "END AND SAVE LEAGUE"
250 I$ = GetKeyPress$

260 IF I$ = "3" THEN 950

270 LOCATE 20, 30: PRINT "SLOT # (1-40) ";

280 N = 1: INPUT B

290 IF B < 1 OR B > 40 THEN 270

300 IF I$ = "2" THEN Z1$(B) = "XXX": GOTO 220

310 GOSUB 1440

390 Z1$(B) = F$

400 GOTO 220

950 CLS

970  OPEN D$(1) + LG$ + ".CON" FOR OUTPUT AS #2
	PRINT #2, LG$
	FOR X = 1 TO 40
	PRINT #2, Z1$(X)
	NEXT
CLOSE 2
	GOTO 50

1195 CLS
	RUN "HELLO.EXE 3"
	END

1440 Spec$ = D$(1) + "*.LIB"
	null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
	Count% = DirCount%
	IF Count% = 0 THEN 4600
REDIM F$(0 TO Count%)
	FOR X = 1 TO Count%
	  F$(X) = SPACE$(12)
	NEXT
	F$(0) = Spec$
	CALL ReadFile(F$())
	'CALL SortStr(VARPTR(F$(1)), Count, 0)

1445 CLS
	COLOR , 1
	CLS
	LOCATE , 28
	COLOR 14
	PRINT "SELECT CONFERENCE": COLOR 15
	SELECTION% = 1
	Start% = 1
	FileFGColr% = 15:FileBGColr% = 1
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Rows% = 20
	Columns% = 4
	Spacing% = 6
	Row% = 4
	Column% = 1
	LOCATE 25, 1
	COLOR 14
	PRINT "ENTER"; : COLOR 15
	PRINT "-SELECTS CONFERENCE";

_MouseShow "DEFAULT"
MAMenu F$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

	IF ExitCode% = 27 THEN 220
	F$ = F$(SELECTION%)
	LOCATE 2, 33: PRINT F$
LOCATE , 33: PRINT "ANY CHANGE (YN)";
	COLOR 15, 0
3912 I$ = GetKeyPress$
	IF UCASE$(I$) = "Y" THEN 1445

3913 F$ = RTrim$(F$)
	F$ = LEFT$(F$, LEN(F$) - 4)
	RETURN

2060 CLS : PRINT "ERROR:: UNABLE TO READ TEAM FILE": PRINT

2720 BO% = 1: RETURN

4000 Spec$ = D$(1) + "*.CON"
	null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
	Count% = DirCount%
	IF Count% = 0 THEN 4500
	REDIM F$(0 TO Count%)
	FOR X = 1 TO Count%
	  F$(X) = SPACE$(12)
	NEXT
	F$(0) = Spec$
	CALL ReadFile(F$())
	'CALL SortStr(VARPTR(F$(1)), Count, 0)

1310 CLS
	COLOR , 1
	CLS
	LOCATE , 28
	COLOR 14
	PRINT "SELECT LEAGUE TO SCHEDULE": COLOR 15
	SELECTION% = 1
	Start% = 1
	FileFGColr% = 15:FileBGColr% = 1
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Rows% = 20
	Columns% = 4
	Spacing% = 6
	Row% = 4
	Column% = 1
	LOCATE 25, 1
	COLOR 14
	PRINT "ENTER"; : COLOR 15
	PRINT "-SELECTS TEAM";

_MouseShow "DEFAULT"
MAMenu F$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

	IF ExitCode% = 27 THEN 60
	F$ = F$(SELECTION%)
	LOCATE 2, 33: PRINT F$
LOCATE , 33: PRINT "ANY CHANGE (YN)";
	COLOR 15, 0
3910 I$ = GetKeyPress$
	IF UCASE$(I$) = "Y" THEN 1310

3911 F$ = RTrim$(F$)
	LG$ = LEFT$(F$, LEN(F$) - 4)
	 OPEN D$(1) + LG$ + ".CON" FOR INPUT AS #2
	INPUT #2, LG$
	FOR X = 1 TO 40
	INPUT #2, Z1$(X)
	NEXT
CLOSE 2
	COLOR 15, 1
	GOTO 220

4500 LOCATE 10, 8
	CALL CSMsgBox("NO LEAGUE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Cnf)
	I$ = GetKeyPress$
	CALL CSMsgBox("", 0, Cnf)
	GOTO 50

4600 LOCATE 10, 8
	CALL CSMsgBox("NO CONFERENCE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Cnf)
	I$ = GetKeyPress$
	CALL CSMsgBox("", 0, Cnf)
	GOTO 50


SUB GETKEY ()
LOCATE 24, 1: PRINT "PRESS ANY KEY TO CONTINUE";
1240 I$ = GetKeyPress$
END SUB


FUNCTION FnExt$ (X$, Length) 
'Essentially this pads space to the right of the string
	X$ = X$ + SPACE$(Length - LEN(X$))  'for display only
END FUNCTION


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


'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
