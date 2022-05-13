'$INCLUDE: 'Config.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Courtside College Basketball - Create National Org"
''$ExeIcon:'./lhg_ccb.ico'
'_Icon
_AllowFullScreen

DefInt A-Z

DECLARE FUNCTION PadRight$ (X$, Length)

Dim Z1$(40), Z2$(40), Z1%(40), D$(2)
'Dim Q$(15), teamItems$(200), V%(200), T%(34), L%(13)
Dim menuItems$(4)
'Dim Y As Long

FG = 0
BG = 7
'Colr = OneColor%(FG, BG) 'pack FG and BG into a single byte

Open "DEV.BAS" For Input As #2
For I = 0 To 1
    Input #2, D$(I)
Next
Close 2

Cls

50 For X = 1 To 40
    Z1$(X) = "XXX": Z2$(X) = "XXX": Z1%(X) = -1
Next

60 Call TITLE
menuItems$(1) = "ADD / DELETE /REVIEW CONFERENCES"
menuItems$(2) = "CREATE / DEFINE LEAGUE & MEMBERS"
menuItems$(3) = "RETURN TO STANDINGS/LEADERS MENU"
SELECTION% = 1
Count% = 3
Start% = 1
SCANCODE% = 3
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 8
Column% = 25

Call box0(7, 24, 11, 57, 2, 15, 1)
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then Run "HELLO.EXE 3"
I1 = SELECTION%
If I1 = 1 Then 4000
If I1 = 3 Then Run "HELLO.EXE 3"

130 Color 15, 0: Cls
JJ = 7
Locate 6, 8: Color 15, 1
'Print box top
Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

'Print box sides
For II = 1 To JJ
    Locate 6 + II, 8
    Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
Next

'Print box bottom
Locate 7 + JJ, 8
Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

Locate 7, 32
Locate , 12: Print "ENTER NAME FOR LEAGUE: ": Print
Locate , 12: Print "THE NAME IS 8 CHARACTERS LONG WITH NO PERIODS OR"
Locate , 12: Print "SPACES. TO EXIT, JUST HIT 'ENTER'"
Locate , 12: Print "SAMPLE NAMES ARE NCAA-90, SOUTH, MIDWEST"
Locate 7, 37: Input LG$
If LG$ = "" Then 60
LG$ = Left$(LG$, 8)
Locate 13, 12: Print "ANY CHANGE (YN) "

22 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 130

220 Color 14, 0
Cls
Print
Print "  "; LG$: Print: Color 12

230 For I = 1 To 10
    Color 2: Print Using "## "; I;: Color 15: Print Z1$(I);
    Locate , 21: Color 2: Print Using "## "; I + 10;: Color 15: Print Z1$(I + 10);
    Locate , 41: Color 2: Print Using "## "; I + 20;: Color 15: Print Z1$(I + 20);
    Locate , 61: Color 2: Print Using "## "; I + 30;: Color 15: Print Z1$(I + 30)
Next I

240 Locate 19, 1
Color 14: Print "(1) ";: Color 15: Print "ADD A CONFERENCE"
Color 14: Print "(2) ";: Color 15: Print "REMOVE A CONFERENCE"
Color 14: Print "(3) ";: Color 15: Print "END AND SAVE LEAGUE"
250 I$ = GetKeyPress$

260 If I$ = "3" Then 950

270 Locate 20, 30: Print "SLOT # (1-40) ";

280 N = 1: Input B

290 If B < 1 Or B > 40 Then 270

300 If I$ = "2" Then Z1$(B) = "XXX": GoTo 220

310 GoSub 1440

390 Z1$(B) = targetFile$

400 GoTo 220

950 Cls

970 Open D$(1) + LG$ + ".CON" For Output As #2
Print #2, LG$
For X = 1 To 40
    Print #2, Z1$(X)
Next
Close 2
GoTo 50

1195 Cls
Run "HELLO.EXE 3"
End

1440 Spec$ = D$(1) + "*.LIB"
null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
Count% = DirCount%
If Count% = 0 Then 4600
ReDim leagueFiles$(0 To Count%)
For X = 1 To Count%
    leagueFiles$(X) = Space$(12)
Next
leagueFiles$(0) = Spec$
Call ReadFile(leagueFiles$())
'CALL SortStr(VARPTR(leagueFiles$(1)), Count, 0)

1445 Cls
Color , 1
Cls
Locate , 28
Color 14
Print "SELECT CONFERENCE": Color 15
SELECTION% = 1
Start% = 1
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Rows% = 20
Columns% = 4
Spacing% = 6
Row% = 4
Column% = 1
Locate 25, 1
Color 14
Print "ENTER";: Color 15
Print "-SELECTS CONFERENCE";

_MouseShow "DEFAULT"
MAMenu leagueFiles$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If ExitCode% = 27 Then 220
targetFile$ = leagueFiles$(SELECTION%)
Locate 2, 33: Print targetFile$
Locate , 33: Print "ANY CHANGE (YN)";
Color 15, 0
3912 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 1445

3913 targetFile$ = RTrim$(targetFile$)
targetFile$ = Left$(targetFile$, Len(targetFile$) - 4)
Return

2060 Cls: Print "ERROR:: UNABLE TO READ TEAM FILE": Print

2720 BO% = 1: Return

4000 Spec$ = D$(1) + "*.CON"
null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
Count% = DirCount%
If Count% = 0 Then 4500
ReDim conferenceFiles$(0 To Count%)
For X = 1 To Count%
    conferenceFiles$(X) = Space$(12)
Next
conferenceFiles$(0) = Spec$
Call ReadFile(conferenceFiles$())
'CALL SortStr(VARPTR(conferenceFiles$(1)), Count, 0)

1310 Cls
Color , 1
Cls
Locate , 28
Color 14
Print "SELECT LEAGUE TO SCHEDULE": Color 15
SELECTION% = 1
Start% = 1
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Rows% = 20
Columns% = 4
Spacing% = 6
Row% = 4
Column% = 1
Locate 25, 1
Color 14
Print "ENTER";: Color 15
Print "-SELECTS TEAM";

_MouseShow "DEFAULT"
MAMenu conferenceFiles$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If ExitCode% = 27 Then 60
targetFile$ = conferenceFiles$(SELECTION%)
Locate 2, 33: Print targetFile$
Locate , 33: Print "ANY CHANGE (YN)";
Color 15, 0
3910 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 1310

3911 targetFile$ = RTrim$(targetFile$)
LG$ = Left$(targetFile$, Len(targetFile$) - 4)
Open D$(1) + LG$ + ".CON" For Input As #2
Input #2, LG$
For X = 1 To 40
    Input #2, Z1$(X)
Next
Close 2
Color 15, 1
GoTo 220

4500 Locate 10, 8
Call CSMsgBox("NO LEAGUE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)
GoTo 50

4600 Locate 10, 8
Call CSMsgBox("NO CONFERENCE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)
GoTo 50


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
Sub TITLE ()
    Color , 0: KEY Off: Cls
    Color , 1: Locate 1, 1: Print Tab(80);: Locate , 24: Color 12
    Print "COURTSIDE COLLEGE BASKETBALL V4.0"
    Locate 24, 1: Print Tab(80);: Locate , 1
    Print "(C) LANCE HAFFNER/SHANNON LYNN 1987-1994"; Tab(70); Left$(Time$, 5);
    Locate 3, 1: Color 2, 0
    Color 15: Print "        PROGRAMMED AND DEVELOPED BY LANCE HAFFNER AND SHANNON LYNN"
    Print "              FOR TECHNICAL SUPPORT, CALL (615) 366-8088"
    Color 10
End Sub


'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
