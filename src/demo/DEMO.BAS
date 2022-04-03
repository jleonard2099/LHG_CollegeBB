'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Courtside College Basketball - DEMO"
''$ExeIcon:'./lhg_ccb.ico'
'_Icon
_AllowFullScreen

DEFINT A-Z
DECLARE FUNCTION PadRight$ (X$, Length)
DECLARE SUB TITLE ()

DIM D$(3), menuItems$(13)
DIM A$(1 TO 15)

OPEN "DEV.BAS" FOR INPUT AS #1
	FOR I = 0 TO 1
	INPUT #1, D$(I)
	NEXT
CLOSE 1

CALL Comline(N, A$(), 10)     ' Get what was typed on the command line
'FOR I = 1 TO N: PRINT A$(I): NEXT I
	IF A$(1) = "3" THEN A$(1) = "": GOTO 800
	IF A$(1) = "2" THEN A$(1) = "": GOTO 700
	IF A$(1) = "4" THEN A$(1) = "": GOTO 750

1  CALL TITLE
	ERASE menuItems$
	menuItems$(1) = "PLAY DEMO GAME                  "
	menuItems$(2) = "TEAM MANAGEMENT / ORGANIZATION  "
	menuItems$(3) = "REPLAY STATISTICS AND REPORTS   "
	menuItems$(4) = "LEADERS / STANDINGS / RECORDS   "
	menuItems$(5) = "SCHEDULER / AUTOPLAY / BOXSCORES"
	menuItems$(6) = "TOURNAMENT PLAY                 "
	menuItems$(7) = "HARD DRIVE INSTALLATION         "
	menuItems$(8) = "CONFIGURE DRIVES FOR TEAMS,STATS"
	menuItems$(9) = "EXIT TO DOS                     "
	SELECTION% = 1
	Count% = 9
	Start% = 1
	SCANCODE% = 9
	FileFGColr% = 15:FileBGColr% = 1
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Spacing% = 1
	Row% = 8
	Column% = 25

CALL box0(7, 24, 17, 57, 2, 31)	
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

	COLOR , 0
	IF SCANCODE% = 27 THEN END
	 IF SELECTION% = 1 THEN RUN "demoplay.EXE"
	IF SELECTION% = 2 THEN 700
	IF SELECTION% = 3 THEN 750
	IF SELECTION% = 4 THEN 800
	IF SELECTION% = 5 THEN RUN "SCHEDEMO.EXE"
	IF SELECTION% = 6 THEN RUN "ADDTOURN.EXE"
	IF SELECTION% = 7 THEN RUN "INSTALL.EXE"
	IF SELECTION% = 8 THEN 505
	IF SELECTION% = 9 THEN CLS : END

505 COLOR 15, 1
	 FG = 0
	 BG = 7
	 FOR I = 0 TO 1
	 D$(I) = LEFT$(D$(I), LEN(D$(I)) - 1)
	 NEXT
	Colr = OneColor%(FG, BG)             'pack FG and BG into a single byte
	CLS : LOCATE , 16: PRINT "SPECIFY PATHS FOR YOUR TEAMS AND COMPILED STATS": PRINT

510 LOCATE , 31: PRINT "PATHNAME EXAMPLES:": PRINT

515 LOCATE , 31: PRINT "A:"

520 LOCATE , 31: PRINT "C:\COLBB\TEAMS"
	LOCATE 7, 1: QPrint "Path for Teams ", 30, -1
	LOCATE , 16: QPrint PadRight$(D$(0), 30), FG, BG, -1
	LOCATE 9, 1: QPrint "Path for Stats ", 30, -1
	LOCATE , 16: QPrint PadRight$(D$(1), 30), FG, BG, -1

GetDiv1:
	LOCATE 7, 16                         'location of beginning of field
	CapsOn = 0: NumOnly = 0: Max = 30     'specify field parameters
	CALL TextIn(D$(0), Max, NumOnly, CapsOn, XCode, Colr)        'get input
	IF XCode = 1 GOTO GetDiv1:           'can't back up any more, stay here
	IF XCode = 2 GOTO Done               'all done entering data
	D$(0) = _TRIM$(D$(0))
GetDiv2:
	LOCATE 9, 16                         'location of beginning of field
	CapsOn = 0: NumOnly = 0: Max = 30    'specify field parameters
	CALL TextIn(D$(1), Max, NumOnly, CapsOn, XCode, Colr)        'get input
	IF XCode = 1 GOTO GetDiv1:           'can't back up any more, stay here
	IF XCode = 2 GOTO Done               'all done entering data
	D$(1) = _TRIM$(D$(1))

Done:
	LOCATE 23, 1
	PRINT "Is this okay? (Yes/No) ";
	X$ = ""
	CALL YesNoB(X$, XCode, FG, BG)
	LOCATE , 1
	ClearEOL 14, 1
	IF XCode = 1 GOTO GetDiv2
	IF X$ = "N" GOTO GetDiv1
	IF X$ <> "Y" GOTO Done               'insist on either Y or N or up-arrow

580 OPEN "DEV.BAS" FOR OUTPUT AS #1

585 FOR R = 0 TO 2: D$(R) = D$(R) + "\": NEXT R

590 PRINT #1, D$(0)

595 PRINT #1, D$(1)

600 CLOSE #1
	 GOTO 1

700 CALL TITLE
	ERASE menuItems$
	menuItems$(1) = "INPUT A NEW TEAM              "
	menuItems$(2) = "EDIT AN EXISTING TEAM         "
	menuItems$(3) = "PRINT TEAM ROSTER / REPORT    "
	menuItems$(4) = "VIEW TEAM ROSTER / REPORT     "
	menuItems$(5) = "DRAFT TO NEW / EXISTING TEAM  "
	menuItems$(6) = "RETURN TO MAIN MENU           "
	SELECTION% = 1
	Count% = 6
	Start% = 1
	SCANCODE% = 6
	FileFGColr% = 15:FileBGColr% = 1
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Spacing% = 1
	Row% = 8
	Column% = 27
CALL box0(7, 26, 14, 57, 2, 31)
	
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

	COLOR , 0
	IF SCANCODE% = 27 THEN 1
	IF SELECTION% = 1 THEN RUN "CBBINPUT.EXE"
	IF SELECTION% = 2 THEN RUN "CNGRAT.EXE 1"
	IF SELECTION% = 3 THEN RUN "CNGRAT.EXE 2"
	IF SELECTION% = 4 THEN RUN "CNGRAT.EXE 3"
	IF SELECTION% = 5 THEN RUN "DRAFT.EXE"
	IF SELECTION% = 6 THEN 1

750 CALL TITLE
	ERASE menuItems$
	menuItems$(1) = "VIEW / PRINT COMPILED STATISTICS "
	menuItems$(2) = "ENCYCLOPEDIA/ TEAM REPLAY OPTIONS"
	menuItems$(3) = "COMPARE REPLAY STATS TO ACTUAL   "
	menuItems$(4) = "MERGE SEPARATE STATISTICAL FILES "
	menuItems$(5) = "RETURN TO COURTSIDE MAIN MENU    "
	SELECTION% = 1
	Count% = 5
	Start% = 1
	SCANCODE% = 5
	FileFGColr% = 15:FileBGColr% = 1
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Spacing% = 1
	Row% = 8
	Column% = 25

CALL box0(7, 24, 13, 58, 2, 31)	
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

	COLOR , 0
	IF SCANCODE% = 27 THEN 1
	IF SELECTION% = 1 THEN RUN "LOOKY.EXE"
	IF SELECTION% = 2 THEN RUN "CAREER.EXE"
	IF SELECTION% = 3 THEN RUN "COMPARE.EXE"
	IF SELECTION% = 4 THEN RUN "MERGE.EXE"
	IF SELECTION% = 5 THEN 1

800 CALL TITLE
	ERASE menuItems$
	menuItems$(1) = "CREATE/ EDIT A CONFERENCE"
	menuItems$(2) = "UPDATE STATS/ STANDINGS  "
	menuItems$(3) = "VIEW/ PRINT REPORTS      "
	menuItems$(4) = "HEAD-TO-HEAD STATS       "
	menuItems$(5) = "RECORD BOOKS             "
	menuItems$(6) = "EXPANDED LEADERS         "
	menuItems$(7) = "CREATE/ EDIT ORGANIZATION"
	menuItems$(8) = "UPDATE POLL/ STATS       "
	menuItems$(9) = "VIEW/ PRINT REPORTS      "
	menuItems$(10) = "RECORD BOOKS             "
	menuItems$(11) = "RETURN TO COURTSIDE MENU "
	menuItems$(12) = "EXIT TO DOS              "
	SELECTION% = 1
	Count% = 12
	Start% = 1
	FileFGColr% = 15:FileBGColr% = 1
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Rows% = 6
	Columns% = 2
	Spacing% = 4
	Row% = 9
	Column% = 12

CALL box0(7, 11, 15, 37, 2, 31)
CALL box0(7, 40, 15, 66, 2, 31)

	COLOR 15, 0
	LOCATE 8, 14: PRINT "COLLEGE CONFERENCE"
	LOCATE 8, 41: PRINT "NATIONAL ORGANIZATION"
	LOCATE 17
	LOCATE , 10: PRINT "COLLEGE CONFERENCE IS USED TO DEFINE THE TEAMS IN A CONFERENCE"
	PRINT
	LOCATE , 10: PRINT "NATIONAL ORGANIZATION ALLOWS YOU TO SELECT MULTIPLE CONFERENCES"
	LOCATE , 10: PRINT "TO TRACK AND UPDATE TO GENERATE A NATIONAL POLL AND LEADERS."
	
_MouseShow "DEFAULT"
20   MAMenu menuItems$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

	IF ExitCode% = 27 THEN 1
	IF ExitCode% = -71 THEN Start% = 1: ExitCode% = 3: SELECTION% = 1: GOTO 20
	IF ExitCode% = -79 THEN Start% = 15: ExitCode% = 3: SELECTION% = 15: GOTO 20
	IF ExitCode% = -73 THEN Start% = Start% - 80: IF Start% < 1 THEN Start% = 1
	IF ExitCode% = -81 THEN Start% = Start% + 80: IF Start% > 15 THEN Start% = 15
	IF ExitCode% = -81 OR ExitCode% = -73 THEN ExitCode% = 3: SELECTION% = Start%: GOTO 20
	COLOR , 0
	IF SELECTION% = 1 THEN RUN "CREATCON.EXE"
	IF SELECTION% = 2 THEN RUN "COMPCON.EXE"
	IF SELECTION% = 3 THEN RUN "SEECON.EXE"
	IF SELECTION% = 4 THEN RUN "HD2HDCON.EXE"
	IF SELECTION% = 5 THEN RUN "RECCON.EXE"
	IF SELECTION% = 6 THEN RUN "EXPANCON.EXE"
	IF SELECTION% = 7 THEN RUN "CREATNAT.EXE"
	IF SELECTION% = 8 THEN RUN "COMPNAT.EXE"
	IF SELECTION% = 9 THEN RUN "SEENAT.EXE"
	IF SELECTION% = 10 THEN RUN "RECCON.EXE"
	IF SELECTION% = 11 THEN 1
	IF SELECTION% = 12 THEN CLS : END


'------------------------------
'      PadRight Function
'------------------------------
' Pads string to the right with spaces
Function PadRight$ (X$, Length)
    PadRight$ = X$ + Space$(Length - Len(X$)) 'for display only
End Function


'------------------------------
'      TITLE Subroutine
'------------------------------
' Displays window title / game copyright
SUB TITLE ()
	COLOR , 0: KEY OFF: CLS
	COLOR , 1: LOCATE 1, 1: PRINT TAB(80); : LOCATE , 24: COLOR 12
	PRINT "COURTSIDE COLLEGE BASKETBALL V4.1"
	LOCATE 24, 1: PRINT TAB(80); : LOCATE , 1
	PRINT "(C) LANCE HAFFNER/SHANNON LYNN 1987-1994"; TAB(70); LEFT$(TIME$, 5);
	LOCATE 2, 1: COLOR 2, 0
	COLOR 15
	PRINT
	PRINT "            PROGRAMMED AND DEVELOPED BY LANCE HAFFNER AND SHANNON LYNN"
	PRINT "        TECHNICAL SUPPORT, CALL (800) 477-7032 EMAIL AT shannon@LHgames.com"
	PRINT "                         DISK ID FOR THIS DEMO IS 'DEM'"
	COLOR 10
END SUB

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'

