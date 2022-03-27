'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Courtside College Basketball - Manage Conference"
''$ExeIcon:'./lhg_ccb.ico'
'_Icon
_AllowFullScreen

DefInt A-Z

DECLARE FUNCTION PadRight$ (X$, Length)
DECLARE SUB TITLE ()

1 Cls

2
'Dim A(1, 6), A1(1, 6), A2(1, 6), A3(1, 6), A4(1, 5), A5(1, 5), A6(1, 2), A7(1, 2), A8(1, 2), A9(1, 2)
Dim B(1, 2)
'Dim B1(1, 2), B3(1, 2), B6(11, 7), Q6%(1)

3 Dim Z1$(30), Z1%(30), Z2$(30)
'Dim O1(30), N$(25, 2), NM$(10, 6, 3), O2(30), O3(30), O4(30)
4 Dim T1(1)
'Dim B2(1, 2), K3(1, 6), R9%(1)
'Dim J6(1)
'Dim S(1, 10), O$(16), D3%(5, 6), P$(2), P(2), F(4), F1(4)
'Dim S1%(7, 6, 4)
'Dim W6(1, 1)
'Dim A$(1), A1$(1, 6), A2$(1, 5), A3$(1, 2), A4$(1, 2), A5$(1, 2), A6$(1, 2)

Dim Q$(300), D$(1), T%(9)
Dim teamItems$(500), V%(500), menuItems$(3)
'Dim L%(4)
Dim Y As Long

FG = 0
BG = 7
Colr = OneColor%(FG, BG) 'pack FG and BG into a single byte

Open "DEV.BAS" For Input As #2
For I = 0 To 1
    Input #2, D$(I)
Next
Close 2

Cls

50 For X = 1 To 30: Z1$(X) = "XXX": Next
Call TITLE
menuItems$(2) = "ADD / REVIEW CONFERENCE MEMBERS "
menuItems$(1) = "CREATE / DEFINE NEW CONFERENCES "
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

Call box0(7, 24, 11, 57, 2, 31)
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then 1195
I1 = SELECTION%
If I1 = 2 Then 4000
If I1 = 3 Then 1195

130 Color 15, 0: Cls
JJ = 7
Locate 6, 8: Color 15, 1
Print " Ö"; String$(61, 196); "· ";: For II = 1 To JJ: Locate 6 + II, 8
Print " º"; String$(61, 32); "º ";: Next
Locate 7 + JJ, 8
Print " Ó"; String$(61, 196); "½ ";: Locate 7, 32
Locate , 12: Print "ENTER NAME FOR CONFERENCE: ": Print
Locate , 12: Print "THE NAME IS 8 CHARACTERS LONG WITH NO PERIODS OR"
Locate , 12: Print "SPACES. TO EXIT, JUST HIT 'ENTER'"
Locate , 12: Print "SAMPLE NAMES ARE SEC, SWC, 94-ACC"
Locate 7, 37: Input LG$
If LG$ = "" Then 50
LG$ = Left$(LG$, 8)
Locate 13, 12: Print "ANY CHANGE (YN) "

22 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 130
Color 15, 0
Print: Print
Locate , 12: Input "ENTER NAME OF DIVISION 1 (ENTER FOR NONE) "; D1$
Locate , 12: Input "ENTER NAME OF DIVISION 2 (ENTER FOR NONE) "; D2$

220 Cls: Print: Color 14: Print "  "; LG$; " CONFERENCE": Print: Color 15
Locate , 6: Print D1$;: Locate , 53: Print D2$

230 For I = 1 To 15
    Print Using "  ##"; I;
    Print " "; Z1$(I);
    Locate , 50
    Print Using "##"; I + 15;
    Print " "; Z1$(I + 15)
Next I

240 GoSub 1220: Color 2
Print "1-ADD TEAM"
Print "  2-REMOVE TEAM"
Print "  3-END AND SAVE"
250 I$ = GetKeyPress$

260 If I$ = "3" Then 410

270 Locate 20, 30: Print "TEAM # (1-30) ";

280 N = 1: Input B

290 If B < 1 Or B > 30 Then 270

300 If I$ = "2" Then Z1$(B) = "XXX": GoTo 220

181 Color 15, 0: Cls
JJ = 7
Locate 6, 8: Color 15, 4
Print " Ö"; String$(61, 196); "· ";: For II = 1 To JJ: Locate 6 + II, 8
Print " º"; String$(61, 32); "º ";: Next
Locate 7 + JJ, 8
Print " Ó"; String$(61, 196); "½ ";: Locate 7, 32
Locate , 12: Print "ENTER DISK ID: ": Print
Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
Locate , 12: Print
Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
Locate 7, 27: Input NY$
If NY$ = "" Then GoSub 3102: GoTo 182
YN$ = NY$
For I = 1 To 500: teamItems$(I) = "": Next
GoSub 3100

182 If BO% = 1 Then BO% = 0: GoTo 181

183 Locate 2, 33: Color T%(0), T%(1): Print A$: Color 15, 4
Locate , 33: Print "ANY CHANGE (YN)";
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 181
Color 15, 0

390 Z1$(B) = A$: Z2$(B) = YN$: Z1%(B) = T1

400 GoTo 220

410 GoSub 1170: GoTo 2237

1195 Cls
Run "HELLO.EXE 3"
End

4000 Spec$ = D$(1) + "*.LIB"
null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
Count% = DirCount%
If Count% = 0 Then 4500
ReDim F$(0 To Count%)
For X = 1 To Count%
    F$(X) = Space$(12)
Next
F$(0) = Spec$
Call ReadFile(F$())
'CALL SortStr(VARPTR(F$(1)), Count, 0)

1310 Cls
Color , 1
Cls
Locate , 28
Color 14
Print "SELECT CONFERENCE TO SCHEDULE": Color 15
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
MAMenu F$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% = 27 Then 50
F$ = F$(SELECTION%)
Locate 2, 33: Print F$
Locate , 33: Print "ANY CHANGE (YN)";
Color 15, 0
3910 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 1310

3911 F$ = RTrim$(F$)
LG$ = Left$(F$, Len(F$) - 4)

1130 Open D$(1) + LG$ + ".LIB" For Input As #2
Input #2, LG%

1140 Input #2, LG$
Input #2, D1$: Input #2, D2$

1150 For X = 1 To 30: Input #2, Z1$(X): Input #2, Z1%(X): Input #2, Z2$(X): Next

1160 Close 2

1165 GoTo 220

1170 Open D$(1) + LG$ + ".LIB" For Output As #2
Print #2, 0

1180 Print #2, LG$
Print #2, D1$: Print #2, D2$

1190 For X = 1 To 30: Print #2, Z1$(X): Print #2, Z1%(X): Print #2, Z2$(X): Next

1200 Close 2

1210 Return

1220 Locate 21, 3: Return

1230 Locate 24, 1: Print "PRESS ANY KEY TO CONTINUE";
1240 I$ = GetKeyPress$

1250 Return

2237 For X = 1 To 30

    2238 If Z1$(X) = "XXX" Then Z1$(X) = Chr$(42): GoTo 2249

    2239 For XX = 4 To 11: If Mid$(Z1$(X), XX, 1) = " " Or Mid$(Z1$(X), XX, 1) = "/" Or Mid$(Z1$(X), XX, 1) = "." Then Mid$(Z1$(X), XX, 1) = "_"

    2240 Next XX

    2241 For XX = 12 To 14: If Mid$(Z1$(X), XX, 2) = "ST" Then Mid$(Z1$(X), 9, 1) = "_": Mid$(Z1$(X), 10, 2) = "ST"

    2242 Next XX

    2243 QQ = 8: For XX = 11 To 4 Step -1: If Mid$(Z1$(X), XX, 1) = "_" Then QQ = XX - 4: GoTo 2245

        2244 GoTo 2248

    2245 Next XX

    2248 YR$ = Left$(Z1$(X), 2): Z1$(X) = Mid$(Z1$(X), 4, QQ) + "." + YR$

2249 Next X

2250 J$ = Left$(LG$, 8): Q$ = J$ + ".TMS"

2255 Open D$(1) + Q$ For Output As #2

2260 Write #2, LG$
Write #2, D1$: Write #2, D2$

2270 For X = 1 To 30: Write #2, Z1$(X): Next

2280 Close 2
GoTo 50

3100 NT = 0
Open D$(0) + "BASK." + YN$ For Random As #1 Len = 655
Y = LOF(1)
Field #1, 15 As Q$(0), 640 As X$
For I1 = 1 To Y / 655
    Get #1, I1
    NT = NT + 1
    teamItems$(I1) = Q$(0): V%(I1) = I1
Next
Close 1
NT = Y / 655
TS% = Int((NT - 1) / 80)

3102 SELECTION% = 1
Count% = NT
Start% = 1
FileFGColr% = 15: FileBGColr% = 4
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Rows% = 20
Columns% = 4
Spacing% = 6
Row% = 4
Column% = 1
Color , 4: Cls: Locate , 33: Color 14
Print "TEAM SELECTION": Color 15
Locate 25, 1: Color 14: Print "ESC";: Color 15
Print "-SELECT NEW ID  PGDN  PGUP  ";: Color 14
Print "ENTER";: Color 15: Print "-SELECTS TEAM";
    
_MouseShow "DEFAULT"
20 MAMenu teamItems$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% = 27 Then BO% = 1: Return
If EXITCODE% = -71 Then Start% = 1: EXITCODE% = 3: SELECTION% = 1: GoTo 20
If EXITCODE% = -79 Then Start% = NT: EXITCODE% = 3: SELECTION% = NT: GoTo 20
If EXITCODE% = -73 Then Start% = Start% - 80: If Start% < 1 Then Start% = 1
If EXITCODE% = -81 Then Start% = Start% + 80: If Start% > NT Then Start% = NT
If EXITCODE% = -81 Or EXITCODE% = -73 Then EXITCODE% = 3: SELECTION% = Start%: GoTo 20
T1 = V%(SELECTION%)

9000 Open D$(0) + "BASK." + YN$ For Random As #1 Len = 655
FIELD #1, 15 AS Q$(0), 2 AS Q$(1), 2 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 2 AS Q$(14), 2 AS Q$(15), 2 AS Q$(16), 2 AS Q$(17), 2 AS Q$(18), 2 _
AS Q$(19), 602 AS X$
For I = 0 To 13
FIELD #1, 53 + I * 43 AS X$, 15 AS Q$(I * 15 + 20), 2 AS Q$(I * 15 + 21), 2 AS Q$(I * 15 + 22), 2 AS Q$(I * 15 + 23), 2 AS Q$(I * 15 + 24), 2 AS Q$(I * 15 + 25), 2 AS Q$(I * 15 + 26), 2 AS Q$(I * 15 + 27), 2 AS Q$(I * 15 + 28), 2 AS Q$(I * 15 + 29) _
, 2 AS Q$(I * 15 + 30), 2 AS Q$(I * 15 + 31), 2 AS Q$(I * 15 + 32), 2 AS Q$(I * 15 + 33), 2 AS Q$(I * 15 + 34), 655 - 53 - 43 - I * 43 AS X$
Next
Get #1, T1
A$ = Q$(0)

9005 If Right$(A$, 1) = " " Then A$ = Left$(A$, Len(A$) - 1): GoTo 9005
For I = 0 To 9: T%(I) = CVI(Q$(I + 10)): Next
Close 1

9524 Return

4600 JJ = 4
Locate 6, 8: Color 15, 1
Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";
For II = 1 To JJ: Locate 6 + II, 8
Print " º"; String$(61, 32); "º ";: Next
Locate 7 + JJ, 8
Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";: Locate 7
Return

4500 Locate 10, 8
GoSub 4600
Print
Locate , 15: Print "NO LEAGUE FILES WERE FOUND"
Locate , 15: Print "PRESS ANY KEY TO RETURN TO MAIN MENU"
3805 I$ = GetKeyPress$
GoTo 1195

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
