'$INCLUDE: 'Config.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Courtside College Basketball - Delete Conference Stats"
''$ExeIcon:'./lhg_ccb.ico'
'_Icon
_AllowFullScreen

DefInt A-Z

'Dim O1(40), TM$(40), JS(46, 42) As Single, ZM%(46), NM$(46)
Dim H1$(40), D$(2)
'Dim PS$(40),BA$(40)
'Dim DFT(40, 20) As Single, ER$(40), OFT(40, 20) As Single
'Dim O2(40), Z2$(255), Z3$(255), Z1(255, 2) As Single, BAT(24), PIT(23)
'Dim J As Single

Open "DEV.BAS" For Input As #2
For I = 0 To 1
    Input #2, D$(I)
Next
Close #2

Cls

'CheckFiles:

50 Spec$ = D$(1) + "*.LIB"
null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
Count% = DirCount%
If Count% = 0 Then GoTo 100
ReDim F$(0 To Count%)
For X = 1 To Count%
    F$(X) = Space$(12)
Next
F$(0) = Spec$
Call ReadFile(F$())

'LeagueSelect:

75 Cls
Color , 1
Cls
Locate , 28
Color 14
Print "SELECT LEAGUE TO ERASE"

Color 15
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
Print "ENTER";
Color 15
Print "-SELECTS LEAGUE   ";
Color 14
Print "HIT ESCAPE KEY TO EXIT";
Color 15

_MouseShow "DEFAULT"
MAMenu F$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If ExitCode% = 27 Then Run "HELLO.EXE"

F$ = F$(SELECTION%)
Locate 2, 33: Print F$
Locate , 33: Print "ANY CHANGE (YN)";
Color 15, 0
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then GoTo 75
    
F$ = RTrim$(F$)
WE$ = Left$(F$, Len(F$) - 4)
LH$ = Left$(WE$, 8)

Cls

Locate 10, 8
Call CSMsgBox("WARNING!!!::: THIS WILL ERASE ALL COMPILED STATS AND LEAGUE FILES FOR THIS LEAGUE. IF YOU STILL WISH TO ERASE ALL LEAGUE FILES AND STATS, HIT 'Y' FOR YES", 26, Pass)
I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)
If I$ <> "Y" Then GoTo 75

Open D$(1) + LH$ + ".TMS" For Input As #2
Input #2, LG$, D1$, D2$, D3$, D4$
For XW = 1 To 40
    Input #2, H1$(XW)
Next
Close 2

For XW = 1 To 40
    If H1$(XW) = Chr$(42) Then _Continue
    Print "DELETING "; H1$(XW); " STATS"
    Kill D$(1) + H1$(XW)
    Kill D$(1) + H1$(XW) + "R"
    Kill D$(1) + H1$(XW) + "S"
    Kill D$(1) + H1$(XW) + "D"
Next
    
Cls
    
GoTo 50

'MsgBox:

100 Locate 10, 8
Call CSMsgBox("NO LEAGUE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)

Run "HELLO.EXE"


'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
