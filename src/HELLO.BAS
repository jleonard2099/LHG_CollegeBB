'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

DefInt A-Z
DECLARE FUNCTION PadRight$ (X$, Length)
DECLARE SUB TITLE ()

Dim D$(3), item$(18)
Dim A$(1 To 15)

Open "DEV.BAS" For Input As #1
For I = 0 To 1
    Input #1, D$(I)
Next
Close 1

Call Comline(N, A$(), 10) ' Get what was typed on the command line
For I = 1 To N: Print A$(I): Next I
If A$(1) = "2" Then A$(1) = "": GoTo 700
If A$(1) = "3" Then A$(1) = "": GoTo 800
If A$(1) = "4" Then A$(1) = "": GoTo 750

1 Call TITLE
item$(1) = "PLAY SINGLE GAME                "
item$(2) = "TEAM MANAGEMENT / ORGANIZATION  "
item$(3) = "REPLAY STATISTICS AND REPORTS   "
item$(4) = "LEADERS / STANDINGS / RECORDS   "
item$(5) = "SCHEDULER / AUTOPLAY / BOXSCORES"
item$(6) = "TOURNAMENT PLAY                 "
item$(7) = "HARD DRIVE INSTALLATION         "
item$(8) = "CONFIGURE DRIVES FOR TEAMS,STATS"
item$(9) = "EXIT TO DOS                     "
SELECTION% = 1
Count% = 9
Start% = 1
SCANCODE% = 9
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 8
Column% = 25

Call box0(7, 24, 17, 57, 2, 31)
    
_MouseShow "DEFAULT"
MMenuVert item$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then End
If SELECTION% = 1 Then Run "COLHOOP.EXE"
If SELECTION% = 2 Then 700
If SELECTION% = 3 Then 750
If SELECTION% = 4 Then 800
If SELECTION% = 5 Then Run "SCHEDULE.EXE"
If SELECTION% = 6 Then Run "ADDTOURN.EXE"
'If SELECTION% = 7 Then Run "INSTALL.EXE"
If SELECTION% = 8 Then 505
If SELECTION% = 9 Then Cls: End

505 Color 15, 1
FG = 0
BG = 7
For I = 0 To 1
    D$(I) = Left$(D$(I), Len(D$(I)) - 1)
Next
Colr = OneColor%(FG, BG) 'pack FG and BG into a single byte
Cls: Locate , 16: Print "SPECIFY PATHS FOR YOUR TEAMS AND COMPILED STATS": Print

510 Locate , 31: Print "PATHNAME EXAMPLES:": Print

515 Locate , 31: Print "A:"

520 Locate , 31: Print "C:\COLBB\TEAMS"
Locate 7, 1: QPrint "Path for Teams ", 14, 1, -1
Locate , 16: QPrint PadRight$(D$(0), 30), FG, BG, -1
Locate 9, 1: QPrint "Path for Stats ", 14, 1, -1
Locate , 16: QPrint PadRight$(D$(1), 30), FG, BG, -1

GetDiv1:
Locate 7, 16 'location of beginning of field
CapsOn = 0: NumOnly = 0: Max = 30 'specify field parameters
Call TextIn(D$(0), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
If XCode = 2 GoTo Done 'all done entering data
D$(0) = _Trim$(D$(0))
GetDiv2:
Locate 9, 16 'location of beginning of field
CapsOn = 0: NumOnly = 0: Max = 30 'specify field parameters
Call TextIn(D$(1), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
If XCode = 2 GoTo Done 'all done entering data
D$(1) = _Trim$(D$(1))

Done:
Locate 23, 1
Print "Is this okay? (Yes/No) ";
X$ = ""
Call YesNoB(X$, XCode, FG, BG)
Locate , 1
ClearEOL 14, 1
If XCode = 1 GoTo GetDiv2
If X$ = "N" GoTo GetDiv1
If X$ <> "Y" GoTo Done 'insist on either Y or N or up-arrow

580 Open "DEV.BAS" For Output As #1

585 For R = 0 To 2: D$(R) = D$(R) + "\": Next R

590 Print #1, D$(0)

595 Print #1, D$(1)

600 Close #1
GoTo 1

700 Call TITLE
item$(1) = "INPUT A NEW TEAM              "
item$(2) = "EDIT AN EXISTING TEAM         "
item$(3) = "PRINT TEAM ROSTER / REPORT    "
item$(4) = "VIEW TEAM ROSTER / REPORT     "
item$(5) = "DRAFT TO NEW / EXISTING TEAM  "
item$(6) = "RETURN TO MAIN MENU           "
SELECTION% = 1
Count% = 6
Start% = 1
SCANCODE% = 6
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 8
Column% = 27
Call box0(7, 26, 14, 57, 2, 31)
    
_MouseShow "DEFAULT"
MMenuVert item$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then 1
If SELECTION% = 1 Then Run "CBBINPUT.EXE"
If SELECTION% = 2 Then Run "CNGRAT.EXE 1"
If SELECTION% = 3 Then Run "CNGRAT.EXE 2"
If SELECTION% = 4 Then Run "CNGRAT.EXE 3"
If SELECTION% = 5 Then Run "DRAFT.EXE"
If SELECTION% = 6 Then 1

750 Call TITLE
item$(1) = "VIEW / PRINT COMPILED STATISTICS "
item$(2) = "ENCYCLOPEDIA/ TEAM REPLAY OPTIONS"
item$(3) = "COMPARE REPLAY STATS TO ACTUAL   "
item$(4) = "MERGE SEPARATE STATISTICAL FILES "
item$(5) = "RETURN TO COURTSIDE MAIN MENU    "
SELECTION% = 1
Count% = 5
Start% = 1
SCANCODE% = 5
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 8
Column% = 25

Call box0(7, 24, 13, 58, 2, 31)
    
_MouseShow "DEFAULT"
MMenuVert item$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then 1
If SELECTION% = 1 Then Run "LOOKY.EXE"
If SELECTION% = 2 Then Run "CAREER.EXE"
If SELECTION% = 3 Then Run "COMPARE.EXE"
If SELECTION% = 4 Then Run "MERGE.EXE"
If SELECTION% = 5 Then 1

800 Call TITLE
item$(1) = "CREATE/ EDIT A CONFERENCE"
item$(2) = "UPDATE STATS/ STANDINGS  "
item$(3) = "VIEW/ PRINT REPORTS      "
item$(4) = "HEAD-TO-HEAD STATS       "
item$(5) = "RECORD BOOKS             "
item$(6) = "EXPANDED LEADERS         "
item$(7) = "DELETE CONFERENCE STATS  "
item$(8) = "CREATE/ EDIT ORGANIZATION"
item$(9) = "UPDATE POLL/ STATS       "
item$(10) = "VIEW/ PRINT REPORTS      "
item$(11) = "RECORD BOOKS             "
item$(12) = "DELETE ORGANIZATION STATS"
item$(13) = "RETURN TO COURTSIDE MENU "
item$(14) = "EXIT TO DOS              "
SELECTION% = 1
Count% = 12
Start% = 1
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Rows% = 6
Columns% = 2
Spacing% = 4
Row% = 9
Column% = 12
Call box0(7, 11, 15, 37, 2, 31)
Call box0(7, 40, 15, 66, 2, 31)
Color 15, 0
Locate 8, 14: Print "COLLEGE CONFERENCE"
Locate 8, 41: Print "NATIONAL ORGANIZATION"
Locate 17
Locate , 10: Print "COLLEGE CONFERENCE IS USED TO DEFINE THE TEAMS IN A CONFERENCE"
Print
Locate , 10: Print "NATIONAL ORGANIZATION ALLOWS YOU TO SELECT MULTIPLE CONFERENCES"
Locate , 10: Print "TO TRACK AND UPDATE TO GENERATE A NATIONAL POLL AND LEADERS."
    
_MouseShow "DEFAULT"
20 MAMenu item$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If ExitCode% = 27 Then 1
If ExitCode% = -71 Then Start% = 1: ExitCode% = 3: SELECTION% = 1: GoTo 20
If ExitCode% = -79 Then Start% = 15: ExitCode% = 3: SELECTION% = 15: GoTo 20
If ExitCode% = -73 Then Start% = Start% - 80: If Start% < 1 Then Start% = 1
If ExitCode% = -81 Then Start% = Start% + 80: If Start% > 15 Then Start% = 15
If ExitCode% = -81 Or ExitCode% = -73 Then ExitCode% = 3: SELECTION% = Start%: GoTo 20
Color , 0
If SELECTION% = 1 Then Run "CREATCON.EXE"
If SELECTION% = 2 Then Run "COMPCON.EXE"
If SELECTION% = 3 Then Run "SEECON.EXE"
If SELECTION% = 4 Then Run "HD2HDCON.EXE"
If SELECTION% = 5 Then Run "RECCON.EXE"
If SELECTION% = 6 Then Run "EXPANCON.EXE"
If SELECTION% = 7 Then Run "BLANK.EXE"
If SELECTION% = 8 Then Run "CREATNAT.EXE"
If SELECTION% = 9 Then Run "COMPNAT.EXE"
If SELECTION% = 10 Then Run "SEENAT.EXE"
If SELECTION% = 11 Then Run "RECCON.EXE"
If SELECTION% = 12 Then Run "BLANKORG.EXE"
If SELECTION% = 13 Then 1
If SELECTION% = 14 Then Cls: End

Function PadRight$ (X$, Length)
    'Essentially this pads space to the right of the string
    PadRight$ = X$ + Space$(Length - Len(X$)) 'for display only
End Function

Sub TITLE ()
    Color , 0: KEY Off: Cls
    Color , 1: Locate 1, 1: Print Tab(80);: Locate , 24: Color 12
    Print "COURTSIDE COLLEGE BASKETBALL V4.1"
    Locate 24, 1: Print Tab(80);: Locate , 1
    Print "(C) LANCE HAFFNER/SHANNON LYNN 1987-1994"; Tab(70); Left$(Time$, 5);
    Locate 2, 1: Color 2, 0
    Color 15
    Print
    Print "            PROGRAMMED AND DEVELOPED BY LANCE HAFFNER AND SHANNON LYNN"
    Print "          TECHNICAL SUPPORT, CALL (800) 477-7032 EMAIL AT ecciLR@AOL.COM"
    Color 10
End Sub

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
