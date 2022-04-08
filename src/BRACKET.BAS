'$INCLUDE: 'Config.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Courtside College Basketball - View Brackets/Seeding"
''$ExeIcon:'./lhg_ccb.ico'
'_Icon
_AllowFullScreen

DefInt A-Z
DECLARE SUB TITLE ()

Dim NN%(17): ' NUMBER OF TEAMS IN A REGION, UP TO 16 REGIONS
Dim N$(17, 17, 3): ' TEAM NAMES FOR EACH SEED SLOT
Dim R$(17): ' REGIONAL NAMES
Dim D$(3), Y As Long
Dim OP%(14): ' TOURNAMENT DEFAULTS
Dim Q$(300), NG%(300, 17), H$(300), V$(300), HS%(300), VS%(300)
Dim YN$(300, 3), AP%(1), A$(9)
Dim N%(17, 17, 3): ' REGION, SEED NUMBER, TEAM#/MODE OF PLAY (0,1)
Dim C%(1): ' 0-CONSOLATION IN REGIONS, 1-CONSOLATION IN CHAMPIONSHIP

'Dim U6 As Single
'Dim teamItems$(500), V%(500), L%(4)
'Dim CM$(1), TE$(1), HC$(2), TS$(1), SC$(2), TF$(1), T%(9)
Dim menuItems$(4)

Open "DEV.BAS" For Input As #1
Input #1, D$(0)
Close 1

10 Call TITLE
menuItems$(1) = "VIEW BRACKETS            "
menuItems$(2) = "VIEW SEEDINGS            "
menuItems$(3) = "RETURN TO TOURNAMENT MENU"
SELECTION% = 1
Count% = 3
Start% = 1
SCANCODE% = 3
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 10
Column% = 25

Call box0(9, 24, 13, 50, 2, 31)
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then SELECTION% = 3
Q = SELECTION%
If Q = 3 Then Run "ADDTOURN.EXE"

610 ' *** LOAD IN A SAVED TOURNAMENT ***
Spec$ = D$(0) + "*.TOU"
null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
Count% = DirCount%
If Count% = 0 Then 4050
ReDim F$(0 To Count%)
For X = 1 To Count%
    F$(X) = Space$(12)
Next
F$(0) = Spec$
Call ReadFile(F$())
'CALL SortStr(VARPTR(F$(1)), Count, 0)
Cls
Color , 1: Cls: Locate , 28: Color 14
Print "SELECT TOURNAMENT FIELD TO VIEW": Color 15
SELECTION% = 1
Start% = 1
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Rows% = 20
Columns% = 4
Spacing% = 6
Row% = 4
Column% = 1
Locate 25, 1: Color 14: Print "ESC";: Color 15
Print "-SELECT NEW ID  PGDN  PGUP  ";: Color 14
Print "ENTER";: Color 15
Print "-SELECTS TOURNAMENT FIELD";

_MouseShow "DEFAULT"
MAMenu F$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% = 27 Then 10
F$ = F$(SELECTION%)
Locate 2, 33: Print F$
Locate , 33: Print "ANY CHANGE (YN)";
Color 15, 0
604 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 610
F$ = _Trim$(F$)
F$ = Left$(F$, Len(F$) - 4)
Open D$(0) + F$ + ".INF" For Input As #2
Input #2, FF$
Input #2, R: ' NUMBER OF REGIONS
For I = 1 To 16
    Input #2, NN%(I): ' NUMBER OF TEAMS IN REGION
    Input #2, R$(I): ' NAME OF REGION
Next
For I = 1 To 16
    For I1 = 1 To 16
        For I2 = 0 To 3
            Input #2, N$(I, I1, I2): ' SEED SLOT NAME
            Input #2, N%(I, I1, I2): ' REGION, SEED NUMBER, MODE PLAY/TM#
Next: Next: Next
Input #2, C%(0): Input #2, C%(1): ' CONSOLATION GAME
Close 2
Open D$(0) + F$ + ".TNY" For Input As #1
For I = 0 To 14
    Input #1, OP%(I)
Next
Close 1
If C%(0) = 1 Then C$ = "Y" Else C$ = "N"
NR$ = Str$(R)
Open D$(0) + F$ + ".TOU" For Random As #1 Len = 90
Y = LOF(1)
Close 1
AP%(0) = Y / 90
BS% = Int((AP%(0) - 1) / 20)
For I = 1 To AP%(0)
    Open D$(0) + F$ + ".TOU" For Random As #1 Len = 90
    For X = 0 To 14
        Field #1, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
    Next
    Field #1, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
    Field #1, 64 As X$, 3 As Q$(20), 3 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
    Field #1, 84 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)
    Get #1, I
    For X = 0 To 14: NG%(I, X) = CVI(Q$(X + 1)): Next
    For X = 0 To 3: YN$(I, X) = Q$(20 + X): Next
    VS%(I) = CVI(Q$(16)): HS%(I) = CVI(Q$(17))
    V$(I) = Q$(18): H$(I) = Q$(19)
    For X = 1 To 3: NG%(I, X + 14) = CVI(Q$(X + 23)): Next
    For X = 2 To 3

        611 If Right$(YN$(I, X), 1) = " " Then YN$(I, X) = Left$(YN$(I, X), Len(YN$(I, X)) - 1): GoTo 611
    Next
Close 1: Next

35 'EDIT A GIVEN BRACKET
Color , 0: Cls
Color , 6: Locate 1, 1: Print Tab(80);: Locate , 23: Color 15
Print "TOURNAMENT  - REGION/BRACKET EDITOR"
Locate 25, 1: Print Tab(80);: Locate , 1: Color , 0
For I = 1 To R
    If R$(I) = "" Then R$(I) = "TO BE NAMED"
    menuItems$(I) = R$(I)
Next
SELECTION% = 1
Count% = R + 2
Start% = 1
SCANCODE% = R + 2
FileColr% = 15
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 4
Column% = 32
menuItems$(R + 1) = "CHAMPIONSHIP BRACKET"
menuItems$(R + 2) = "EXIT                "
Call box0(3, 31, 3 + R + 3, 52, 2, 15)
    
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then 10
NR = SELECTION%
If NR = R + 1 Then NR = 17
If NR = R + 2 Then 10
If Q = 2 Then 21
N = NR

Color , 0

Cls
R$ = R$(N)
Y = Int((80 - Len(R$)) / 2): Locate 1, Y: Print R$
Locate 2
If N = 17 Then 50
On NN%(N) - 3 GOTO 50, 47, 46, 45, 40, 36, 34, 32, 30, 27, 25, 22, 20

20 '16 TEAM BRACKET
REG = N: GAME = 1: GoSub 300
GoSub 350
GoSub 360
Print
REG = N: GAME = 8: GoSub 300
GoSub 350
GoSub 360
Print
REG = N: GAME = 4: GoSub 300
GoSub 350
GoSub 360
Print
REG = N: GAME = 5: GoSub 300
GoSub 350
GoSub 360
Print
REG = N: GAME = 3: GoSub 300
GoSub 350
GoSub 360
Print
REG = N: GAME = 6: GoSub 300
GoSub 350
GoSub 360
Print
REG = N: GAME = 2: GoSub 300
GoSub 350
GoSub 360
Print
REG = N: GAME = 7: GoSub 300
GoSub 350
GoSub 360
Color 15
For I = 0 To 7
    If I / 2 = Int(I / 2) Then Locate 3 * I + 2, 22: Print A$(I + 1)
    If I / 2 <> Int(I / 2) Then Locate 3 * I + 1, 22: Print A$(I + 1)
    If I / 2 = Int(I / 2) Then Locate 3 * I + 3, 21: Print String$(16, Chr$(196)); Chr$(191)
    If I / 2 = Int(I / 2) Then Locate 3 * I + 4, 37: Print Chr$(195)
    If I / 2 <> Int(I / 2) Then Locate 3 * I + 2, 21: Print String$(16, Chr$(196)); Chr$(217)
Next
Color 7
GoSub 200
REG = N: GAME = 9: GoSub 300
Locate 2, 21: GoSub 350
Locate 4, 21: GoSub 360
REG = N: GAME = 10: GoSub 300
Locate 8, 21: GoSub 350
Locate 10, 21: GoSub 360
REG = N: GAME = 11: GoSub 300
Locate 14, 21: GoSub 350
Locate 16, 21: GoSub 360
REG = N: GAME = 12: GoSub 300
Locate 20, 21: GoSub 350
Locate 22, 21: GoSub 360
REG = N: GAME = 13: GoSub 300
Locate 3, 39: GoSub 350
Locate 9, 39: GoSub 360
REG = N: GAME = 14: GoSub 300
Locate 15, 39: GoSub 350
Locate 21, 39: GoSub 360
REG = N: GAME = 15 + C%(0): GoSub 300
Locate 6, 56: GoSub 350
Locate 18, 56: GoSub 360
GoTo 370

21 Cls
Color , 0: Cls
Color , 6: Locate 1, 1: Print Tab(80);: Locate , 23: Color 15
Print "TOURNAMENT  - REGION/BRACKET EDITOR"
Locate 25, 1: Print Tab(80);: Locate , 1: Color , 0
MAXSEED = NN%(NR) + 1 + 4
Call box0(3, 24, MAXSEED, 54, 2, 15)
Locate 4, 25: Print "SEED  TEAM"
For I1 = 1 To NN%(NR)
    Color 2
    Locate , 25
    Print Using "##    "; I1;: Color 15: Print N$(NR, I1, 0)
Next I1
GoSub 5000: GoTo 35

22 '15 TEAM BRACKET
REG = N: GAME = 1: GoSub 300
GoSub 350
GoSub 360
Print
REG = N: GAME = 6: GoSub 300
GoSub 350
GoSub 360
Print
REG = N: GAME = 2: GoSub 300
GoSub 350
GoSub 360
Print
REG = N: GAME = 5: GoSub 300
GoSub 350
GoSub 360
Print
REG = N: GAME = 3: GoSub 300
GoSub 350
GoSub 360
Print
REG = N: GAME = 4: GoSub 300
GoSub 350
GoSub 360
Print
REG = N: GAME = 7: GoSub 300
GoSub 350
GoSub 360
For I = 0 To 7
    If I / 2 = Int(I / 2) Then Locate 3 * I + 2, 22: Print A$(I + 1)
    If I / 2 <> Int(I / 2) Then Locate 3 * I + 1, 22: Print A$(I + 1)
    If I / 2 = Int(I / 2) Then Locate 3 * I + 3, 21: Print String$(16, Chr$(196)); Chr$(191)
    If I / 2 = Int(I / 2) Then Locate 3 * I + 4, 37: Print Chr$(195)
    If I / 2 <> Int(I / 2) Then Locate 3 * I + 2, 21: Print String$(16, Chr$(196)); Chr$(217)
Next
GoSub 200
REG = N: GAME = 8: GoSub 300
Locate 2, 21: GoSub 350
Locate 4, 21: GoSub 360
REG = N: GAME = 9: GoSub 300
Locate 8, 21: GoSub 350
Locate 10, 21: GoSub 360
REG = N: GAME = 10: GoSub 300
Locate 14, 21: GoSub 350
Locate 16, 21: GoSub 360
REG = N: GAME = 11: GoSub 300
Locate 20, 21: GoSub 350
Locate 22, 21: GoSub 360
REG = N: GAME = 12: GoSub 300
Locate 3, 39: GoSub 350
Locate 9, 39: GoSub 360
REG = N: GAME = 13: GoSub 300
Locate 15, 39: GoSub 350
Locate 21, 39: GoSub 360
REG = N: GAME = 14 + C%(0): GoSub 300
Locate 6, 56: GoSub 350
Locate 18, 56: GoSub 360
GoTo 370

25 '14 TEAM BRACKET
REG = N: GAME = 1: GoSub 300
GoSub 350
GoSub 360
Print
REG = N: GAME = 4: GoSub 300
GoSub 350
GoSub 360
Print
REG = N: GAME = 5: GoSub 300
GoSub 350
GoSub 360
Print
Print
Print
Print
REG = N: GAME = 2: GoSub 300
GoSub 350
GoSub 360
Print
REG = N: GAME = 3: GoSub 300
GoSub 350
GoSub 360
Print
REG = N: GAME = 6: GoSub 300
GoSub 350
GoSub 360
For I = 0 To 7
    If I / 2 = Int(I / 2) Then Locate 3 * I + 2, 22: Print A$(I + 1)
    If I / 2 <> Int(I / 2) Then Locate 3 * I + 1, 22: Print A$(I + 1)
    If I / 2 = Int(I / 2) Then Locate 3 * I + 3, 21: Print String$(16, Chr$(196)); Chr$(191)
    If I / 2 = Int(I / 2) Then Locate 3 * I + 4, 37: Print Chr$(195)
    If I / 2 <> Int(I / 2) Then Locate 3 * I + 2, 21: Print String$(16, Chr$(196)); Chr$(217)
Next
GoSub 200
REG = N: GAME = 7: GoSub 300
Locate 2, 21: GoSub 350
Locate 4, 21: GoSub 360
REG = N: GAME = 8: GoSub 300
Locate 8, 21: GoSub 350
Locate 10, 21: GoSub 360
REG = N: GAME = 9: GoSub 300
Locate 14, 21: GoSub 350
Locate 16, 21: GoSub 360
REG = N: GAME = 10: GoSub 300
Locate 20, 21: GoSub 350
Locate 22, 21: GoSub 360
REG = N: GAME = 11: GoSub 300
Locate 3, 39: GoSub 350
Locate 9, 39: GoSub 360
REG = N: GAME = 12: GoSub 300
Locate 15, 39: GoSub 350
Locate 21, 39: GoSub 360
REG = N: GAME = 13 + C%(0): GoSub 300
Locate 6, 56: GoSub 350
Locate 18, 56: GoSub 360
GoTo 370

27 '13 TEAM BRACKET
REG = N: GAME = 1: GoSub 300
GoSub 350
GoSub 360
Print
REG = N: GAME = 2: GoSub 300
GoSub 350
GoSub 360
Print
REG = N: GAME = 3: GoSub 300
GoSub 350
GoSub 360
Print
Print
Print
Print
REG = N: GAME = 4: GoSub 300
GoSub 350
GoSub 360
Print
Print
Print
Print
REG = N: GAME = 5: GoSub 300
GoSub 350
GoSub 360
Print
Print
Print
For I = 0 To 7
    If I / 2 = Int(I / 2) Then Locate 3 * I + 2, 22: Print A$(I + 1)
    If I / 2 <> Int(I / 2) Then Locate 3 * I + 1, 22: Print A$(I + 1)
    If I / 2 = Int(I / 2) Then Locate 3 * I + 3, 21: Print String$(16, Chr$(196)); Chr$(191)
    If I / 2 = Int(I / 2) Then Locate 3 * I + 4, 37: Print Chr$(195)
    If I / 2 <> Int(I / 2) Then Locate 3 * I + 2, 21: Print String$(16, Chr$(196)); Chr$(217)
Next
GoSub 200
REG = N: GAME = 6: GoSub 300
Locate 2, 21: GoSub 350
Locate 4, 21: GoSub 360
REG = N: GAME = 7: GoSub 300
Locate 8, 21: GoSub 350
Locate 10, 21: GoSub 360
REG = N: GAME = 8: GoSub 300
Locate 14, 21: GoSub 350
Locate 16, 21: GoSub 360
REG = N: GAME = 9: GoSub 300
Locate 20, 21: GoSub 350
Locate 22, 21: GoSub 360
REG = N: GAME = 10: GoSub 300
Locate 3, 39: GoSub 350
Locate 9, 39: GoSub 360
REG = N: GAME = 11: GoSub 300
Locate 15, 39: GoSub 350
Locate 21, 39: GoSub 360
REG = N: GAME = 12 + C%(0): GoSub 300
Locate 6, 56: GoSub 350
Locate 18, 56: GoSub 360
GoTo 370

300 For I = 1 To AP%(0)
    If NG%(I, 15) = N And NG%(I, 17) = GAME Then 305
Next

305 Return

310 For X = 1 To 16
    For X1 = 1 To 16
        If X$ = N$(X, X1, 0) Then 312
Next: Next

311 Color 7, 0: GoTo 315

312 Color N%(X, X1, 2), N%(X, X1, 3)

315 Return

350 If Left$(V$(I), 3) = "REG" Then Print: GoTo 355
X$ = _Trim$(V$(I)): GoSub 310
If NG%(I, 13) = 0 Then Print _Trim$(V$(I)): GoTo 355
Print _Trim$(V$(I));: Color 14, 0: Print Using " ###"; VS%(I)

355 Color 7, 0
Return

360 If Left$(H$(I), 3) = "REG" Then 355
X$ = _Trim$(H$(I)): GoSub 310
If NG%(I, 13) = 0 Then Print _Trim$(H$(I)): GoTo 355
Print _Trim$(H$(I));: Color 14, 0: Print Using " ###"; HS%(I)

365 Color 7, 0
Return

370 Locate 12, 64
If NG%(I, 13) = 0 Then 375
If HS%(I) > VS%(I) Then X$ = _Trim$(H$(I)) Else X$ = _Trim$(V$(I))
GoSub 310: Print X$

375 GoSub 5000: GoTo 35

30 '12 TEAM BRACKET
REG = N: GAME = 1: GoSub 300
GoSub 350
GoSub 360
Print
Print
Print
Print
REG = N: GAME = 2: GoSub 300
GoSub 350
GoSub 360
Print
Print
Print
Print
REG = N: GAME = 3: GoSub 300
GoSub 350
GoSub 360
Print
Print
Print
Print
REG = N: GAME = 4: GoSub 300
GoSub 350
GoSub 360
Print
Print
Print
Color 15
For I = 0 To 7
    If I / 2 = Int(I / 2) Then Locate 3 * I + 2, 22: Print A$(I + 1)
    If I / 2 <> Int(I / 2) Then Locate 3 * I + 1, 22: Print A$(I + 1)
    If I / 2 = Int(I / 2) Then Locate 3 * I + 3, 21: Print String$(16, Chr$(196)); Chr$(191)
    If I / 2 = Int(I / 2) Then Locate 3 * I + 4, 37: Print Chr$(195)
    If I / 2 <> Int(I / 2) Then Locate 3 * I + 2, 21: Print String$(16, Chr$(196)); Chr$(217)
Next
Color 7
GoSub 200
REG = N: GAME = 5: GoSub 300
Locate 2, 21: GoSub 350
Locate 4, 21: GoSub 360
REG = N: GAME = 6: GoSub 300
Locate 8, 21: GoSub 350
Locate 10, 21: GoSub 360
REG = N: GAME = 7: GoSub 300
Locate 14, 21: GoSub 350
Locate 16, 21: GoSub 360
REG = N: GAME = 8: GoSub 300
Locate 20, 21: GoSub 350
Locate 22, 21: GoSub 360
REG = N: GAME = 9: GoSub 300
Locate 3, 39: GoSub 350
Locate 9, 39: GoSub 360
REG = N: GAME = 10: GoSub 300
Locate 15, 39: GoSub 350
Locate 21, 39: GoSub 360
REG = N: GAME = 11 + C%(0): GoSub 300
Locate 6, 56: GoSub 350
Locate 18, 56: GoSub 360
Locate 12, 64
GoTo 370

32 '11 TEAM BRACKET
REG = N: GAME = 1: GoSub 300
GoSub 350
GoSub 360
Print
Print
Print
Print
REG = N: GAME = 2: GoSub 300
GoSub 350
GoSub 360
Print
Print
Print
Print
Print
Print
Print
Print
Print
Print
REG = N: GAME = 3: GoSub 300
GoSub 350
GoSub 360
For I = 0 To 7
    If I / 2 = Int(I / 2) Then Locate 3 * I + 2, 22: Print A$(I + 1)
    If I / 2 <> Int(I / 2) Then Locate 3 * I + 1, 22: Print A$(I + 1)
    If I / 2 = Int(I / 2) Then Locate 3 * I + 3, 21: Print String$(16, Chr$(196)); Chr$(191)
    If I / 2 = Int(I / 2) Then Locate 3 * I + 4, 37: Print Chr$(195)
    If I / 2 <> Int(I / 2) Then Locate 3 * I + 2, 21: Print String$(16, Chr$(196)); Chr$(217)
Next
GoSub 200
REG = N: GAME = 4: GoSub 300
Locate 2, 21: GoSub 350
Locate 4, 21: GoSub 360
REG = N: GAME = 5: GoSub 300
Locate 8, 21: GoSub 350
Locate 10, 21: GoSub 360
REG = N: GAME = 6: GoSub 300
Locate 14, 21: GoSub 350
Locate 16, 21: GoSub 360
REG = N: GAME = 7: GoSub 300
Locate 20, 21: GoSub 350
Locate 22, 21: GoSub 360
REG = N: GAME = 8: GoSub 300
Locate 3, 39: GoSub 350
Locate 9, 39: GoSub 360
REG = N: GAME = 9: GoSub 300
Locate 15, 39: GoSub 350
Locate 21, 39: GoSub 360
REG = N: GAME = 10 + C%(0): GoSub 300
Locate 6, 56: GoSub 350
Locate 18, 56: GoSub 360
GoTo 370

34 '10 TEAM BRACKET
REG = N: GAME = 1: GoSub 300
GoSub 350
GoSub 360
Print
Print
Print
Print
Print
Print
Print
Print
Print
Print
Print
Print
Print
Print
Print
Print
REG = N: GAME = 2: GoSub 300
GoSub 350
GoSub 360
For I = 0 To 7
    If I / 2 = Int(I / 2) Then Locate 3 * I + 2, 22: Print A$(I + 1)
    If I / 2 <> Int(I / 2) Then Locate 3 * I + 1, 22: Print A$(I + 1)
    If I / 2 = Int(I / 2) Then Locate 3 * I + 3, 21: Print String$(16, Chr$(196)); Chr$(191)
    If I / 2 = Int(I / 2) Then Locate 3 * I + 4, 37: Print Chr$(195)
    If I / 2 <> Int(I / 2) Then Locate 3 * I + 2, 21: Print String$(16, Chr$(196)); Chr$(217)
Next
GoSub 200
REG = N: GAME = 3: GoSub 300
Locate 2, 21: GoSub 350
Locate 4, 21: GoSub 360
REG = N: GAME = 4: GoSub 300
Locate 8, 21: GoSub 350
Locate 10, 21: GoSub 360
REG = N: GAME = 5: GoSub 300
Locate 14, 21: GoSub 350
Locate 16, 21: GoSub 360
REG = N: GAME = 6: GoSub 300
Locate 20, 21: GoSub 350
Locate 22, 21: GoSub 360
REG = N: GAME = 7: GoSub 300
Locate 3, 39: GoSub 350
Locate 9, 39: GoSub 360
REG = N: GAME = 8: GoSub 300
Locate 15, 39: GoSub 350
Locate 21, 39: GoSub 360
REG = N: GAME = 9 + C%(0): GoSub 300
Locate 6, 56: GoSub 350
Locate 18, 56: GoSub 360
GoTo 370

36 '9 TEAM BRACKET
Locate 22, 1
REG = N: GAME = 1: GoSub 300
GoSub 350
GoSub 360
For I = 0 To 7
    If I / 2 = Int(I / 2) Then Locate 3 * I + 2, 22: Print A$(I + 1)
    If I / 2 <> Int(I / 2) Then Locate 3 * I + 1, 22: Print A$(I + 1)
    If I / 2 = Int(I / 2) Then Locate 3 * I + 3, 21: Print String$(16, Chr$(196)); Chr$(191)
    If I / 2 = Int(I / 2) Then Locate 3 * I + 4, 37: Print Chr$(195)
    If I / 2 <> Int(I / 2) Then Locate 3 * I + 2, 21: Print String$(16, Chr$(196)); Chr$(217)
Next
GoSub 200
REG = N: GAME = 2: GoSub 300
Locate 2, 21: GoSub 350
Locate 4, 21: GoSub 360
REG = N: GAME = 3: GoSub 300
Locate 8, 21: GoSub 350
Locate 10, 21: GoSub 360
REG = N: GAME = 4: GoSub 300
Locate 14, 21: GoSub 350
Locate 16, 21: GoSub 360
REG = N: GAME = 5: GoSub 300
Locate 20, 21: GoSub 350
Locate 22, 21: GoSub 360
REG = N: GAME = 6: GoSub 300
Locate 3, 39: GoSub 350
Locate 9, 39: GoSub 360
REG = N: GAME = 7: GoSub 300
Locate 15, 39: GoSub 350
Locate 21, 39: GoSub 360
REG = N: GAME = 8 + C%(0): GoSub 300
Locate 6, 56: GoSub 350
Locate 18, 56: GoSub 360
GoTo 370

40 ' 8 TEAM BRACKET
For I = 0 To 7
    If I / 2 = Int(I / 2) Then Locate 3 * I + 2, 22: Print A$(I + 1)
    If I / 2 <> Int(I / 2) Then Locate 3 * I + 1, 22: Print A$(I + 1)
    If I / 2 = Int(I / 2) Then Locate 3 * I + 3, 21: Print String$(16, Chr$(196)); Chr$(191)
    If I / 2 = Int(I / 2) Then Locate 3 * I + 4, 37: Print Chr$(195)
    If I / 2 <> Int(I / 2) Then Locate 3 * I + 2, 21: Print String$(16, Chr$(196)); Chr$(217)
Next
GoSub 200
REG = N: GAME = 1: GoSub 300
Locate 2, 21: GoSub 350
Locate 4, 21: GoSub 360
REG = N: GAME = 2: GoSub 300
Locate 8, 21: GoSub 350
Locate 10, 21: GoSub 360
REG = N: GAME = 3: GoSub 300
Locate 14, 21: GoSub 350
Locate 16, 21: GoSub 360
REG = N: GAME = 4: GoSub 300
Locate 20, 21: GoSub 350
Locate 22, 21: GoSub 360
REG = N: GAME = 5: GoSub 300
Locate 3, 39: GoSub 350
Locate 9, 39: GoSub 360
REG = N: GAME = 6: GoSub 300
Locate 15, 39: GoSub 350
Locate 21, 39: GoSub 360
REG = N: GAME = 7 + C%(0): GoSub 300
Locate 6, 56: GoSub 350
Locate 18, 56: GoSub 360
GoTo 370

45 ' 7 TEAM BRACKET
For I = 0 To 7
    If I / 2 = Int(I / 2) Then Locate 3 * I + 2, 22: Print A$(I + 1)
    If I / 2 <> Int(I / 2) Then Locate 3 * I + 1, 22: Print A$(I + 1)
    If I / 2 = Int(I / 2) And I <> 4 Then Locate 3 * I + 3, 21: Print String$(16, Chr$(196)); Chr$(191)
    If I / 2 = Int(I / 2) And I <> 4 Then Locate 3 * I + 4, 37: Print Chr$(195)
    If I / 2 <> Int(I / 2) And I <> 5 Then Locate 3 * I + 2, 21: Print String$(16, Chr$(196)); Chr$(217)
Next
GoSub 200
REG = N: GAME = 1: GoSub 300
Locate 2, 21: GoSub 350
Locate 4, 21: GoSub 360
REG = N: GAME = 2: GoSub 300
Locate 8, 21: GoSub 350
Locate 10, 21: GoSub 360
REG = N: GAME = 3: GoSub 300
Locate 20, 21: GoSub 350
Locate 22, 21: GoSub 360
REG = N: GAME = 4: GoSub 300
Locate 3, 39: GoSub 350
Locate 9, 39: GoSub 360
REG = N: GAME = 5: GoSub 300
Locate 15, 39: GoSub 350
Locate 21, 39: GoSub 360
REG = N: GAME = 6 + C%(0): GoSub 300
Locate 6, 56: GoSub 350
Locate 18, 56: GoSub 360
GoTo 370

46 ' 6 TEAM BRACKET
For I = 0 To 7
    If I / 2 = Int(I / 2) Then Locate 3 * I + 2, 22: Print A$(I + 1)
    If I / 2 <> Int(I / 2) Then Locate 3 * I + 1, 22: Print A$(I + 1)
    If I / 2 = Int(I / 2) And (I <> 4 And I <> 2) Then Locate 3 * I + 3, 21: Print String$(16, Chr$(196)); Chr$(191)
    If I / 2 = Int(I / 2) And (I <> 4 And I <> 2) Then Locate 3 * I + 4, 37: Print Chr$(195)
    If I / 2 <> Int(I / 2) And (I <> 5 And I <> 3) Then Locate 3 * I + 2, 21: Print String$(16, Chr$(196)); Chr$(217)
Next
GoSub 200
REG = N: GAME = 1: GoSub 300
Locate 2, 21: GoSub 350
Locate 4, 21: GoSub 360
REG = N: GAME = 2: GoSub 300
Locate 20, 21: GoSub 350
Locate 22, 21: GoSub 360
REG = N: GAME = 3: GoSub 300
Locate 3, 39: GoSub 350
Locate 9, 39: GoSub 360
REG = N: GAME = 4: GoSub 300
Locate 15, 39: GoSub 350
Locate 21, 39: GoSub 360
REG = N: GAME = 5 + C%(0): GoSub 300
Locate 6, 56: GoSub 350
Locate 18, 56: GoSub 360
GoTo 370

47 ' 5 TEAM BRACKET
For I = 0 To 1
    If I / 2 = Int(I / 2) Then Locate 3 * I + 2, 22: Print A$(I + 1)
    If I / 2 <> Int(I / 2) Then Locate 3 * I + 1, 22: Print A$(I + 1)
    If I / 2 = Int(I / 2) Then Locate 3 * I + 3, 21: Print String$(16, Chr$(196)); Chr$(191)
    If I / 2 = Int(I / 2) Then Locate 3 * I + 4, 37: Print Chr$(195)
    If I / 2 <> Int(I / 2) Then Locate 3 * I + 2, 21: Print String$(16, Chr$(196)); Chr$(217)
Next
GoSub 200
REG = N: GAME = 1: GoSub 300
Locate 2, 21: GoSub 350
Locate 4, 21: GoSub 360
REG = N: GAME = 2: GoSub 300
Locate 3, 39: GoSub 350
Locate 9, 39: GoSub 360
REG = N: GAME = 3: GoSub 300
Locate 15, 39: GoSub 350
Locate 21, 39: GoSub 360
REG = N: GAME = 4 + C%(0): GoSub 300
Locate 6, 56: GoSub 350
Locate 18, 56: GoSub 360
GoTo 370

50 'FOUR TEAM BRACKET
GoSub 200
REG = N: GAME = 1: GoSub 300
Locate 3, 39: GoSub 350
Locate 9, 39: GoSub 360
REG = N: GAME = 2: GoSub 300
Locate 15, 39: GoSub 350
Locate 21, 39: GoSub 360
REG = N: GAME = 3 + C%(0): GoSub 300
Locate 6, 56: GoSub 350
Locate 18, 56: GoSub 360
GoTo 370

55 '3 TEAM BRACKET
For I = 0 To 1
    If I / 2 = Int(I / 2) Then Locate 6 * I + 5, 47: Print Chr$(179)
    If I / 2 = Int(I / 2) Then Locate 6 * I + 6, 47: Print Chr$(179)
    If I / 2 = Int(I / 2) Then Locate 6 * I + 7, 47: Print Chr$(195)
    If I / 2 = Int(I / 2) Then Locate 6 * I + 8, 47: Print Chr$(179)
    If I / 2 = Int(I / 2) Then Locate 6 * I + 9, 47: Print Chr$(179)

    If I / 2 = Int(I / 2) Then Locate 6 * I + 4, 32: Print String$(15, Chr$(196)); Chr$(191)
    If I / 2 <> Int(I / 2) Then Locate 6 * I + 4, 32: Print String$(15, Chr$(196)); Chr$(217)
Next
For I = 0 To 1
    If I = 0 Then Locate 7, 48: Print String$(15, Chr$(196)); Chr$(191)
    For I1 = 7 To 17: Locate I1 + 1, 63: Print Chr$(179): Next
    Locate 13, 63: Print Chr$(195); String$(15, Chr$(196))
    If I = 1 Then Locate 19, 48: Print String$(15, Chr$(196)); Chr$(217)
Next
Locate 3, 32: Print V$(1); Str$(VS%(1))
Locate 9, 32: Print H$(1); Str$(HS%(1))
Locate 6, 48: Print Left$(V$(2), 13); Str$(VS%(2))
Locate 18, 48: Print Left$(H$(2), 13); Str$(HS%(2))
GoTo 370

60 'TWO TEAM BRACKET
For I = 0 To 1
    If I = 0 Then Locate 7, 48: Print String$(15, Chr$(196)); Chr$(191)
    For I1 = 7 To 17: Locate I1 + 1, 63: Print Chr$(179): Next
    Locate 13, 63: Print Chr$(195); String$(15, Chr$(196))
    If I = 1 Then Locate 19, 48: Print String$(15, Chr$(196)); Chr$(217)
    Locate 6, 48: Print V$(4); Str$(VS%(4))
    Locate 18, 48: Print H$(4); Str$(HS%(4))
Next
GoTo 370

200 '4 TEAM SEMI BRACKET
Color 15
For I = 0 To 3
    If I / 2 = Int(I / 2) Then Locate 6 * I + 5, 55: Print Chr$(179)
    If I / 2 = Int(I / 2) Then Locate 6 * I + 6, 55: Print Chr$(179)
    If I / 2 = Int(I / 2) Then Locate 6 * I + 7, 55: Print Chr$(195)
    If I / 2 = Int(I / 2) Then Locate 6 * I + 8, 55: Print Chr$(179)
    If I / 2 = Int(I / 2) Then Locate 6 * I + 9, 55: Print Chr$(179)

    If I / 2 = Int(I / 2) Then Locate 6 * I + 4, 38: Print String$(17, Chr$(196)); Chr$(191)
    If I / 2 <> Int(I / 2) Then Locate 6 * I + 4, 38: Print String$(17, Chr$(196)); Chr$(217)
Next
For I = 0 To 1
    If I = 0 Then Locate 7, 56: Print String$(13, Chr$(196)); Chr$(191)
    For I1 = 7 To 17: Locate I1 + 1, 69: Print Chr$(179): Next
    Locate 13, 69: Print Chr$(195); String$(9, Chr$(196))
    If I = 1 Then Locate 19, 56: Print String$(13, Chr$(196)); Chr$(217)
Next
Color 7
Return
5000 I$ = GetKeyPress$
Return

4050 Locate 10, 8
Call CSMsgBox("NO TOURNAMENT FILES WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)
GoTo 10


'------------------------------
'      TITLE Subroutine
'------------------------------
' Displays window title / game copyright
Sub TITLE ()
    Color , 0: KEY Off: Cls
    Color , 1: Locate 1, 1: Print Tab(80);: Locate , 16: Color 12
    Print "COURTSIDE COLLEGE BASKETBALL TOURNAMENT SCHEDULER"
    Locate 24, 1: Print Tab(80);: Locate , 1
    Print "(C) LANCE HAFFNER/SHANNON LYNN 1987-1994";
    Locate 3, 1: Color 2, 0
    Color 15: Print "        PROGRAMMED AND DEVELOPED BY LANCE HAFFNER AND SHANNON LYNN"
    Print "              FOR TECHNICAL SUPPORT, CALL (615) 366-8088"
    Color 10
End Sub


'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
