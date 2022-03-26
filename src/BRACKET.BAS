'$INCLUDE: 'Config.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

DefInt A-Z
DECLARE SUB TITLE ()
DECLARE SUB CHAMPIONSHIP ()
DECLARE SUB SETTINGS ()
DECLARE SUB SORTGAMES ()
DECLARE SUB WRITEGAME ()

Dim NN%(17): ' NUMBER OF TEAMS IN A REGION, UP TO 16 REGIONS
Dim N$(17, 17, 3): ' TEAM NAMES FOR EACH SEED SLOT
Dim R$(17): ' REGIONAL NAMES
Dim D$(3), U6 As Single, Y As Long
Dim OP%(14): ' TOURNAMENT DEFAULTS
Dim Q$(300), VV$(500), V%(500), NG%(300, 17), H$(300), V$(300), HS%(300), VS%(300)
Dim YN$(300, 3), L%(4), AP%(1), A$(9)
Dim N%(17, 17, 3): ' REGION, SEED NUMBER, TEAM#/MODE OF PLAY (0,1)
Dim C%(1): ' 0-CONSOLATION IN REGIONS, 1-CONSOLATION IN CHAMPIONSHIP
Dim CM$(1), TE$(1), HC$(2), TS$(1), SC$(2), TF$(1), ITEM$(18), T%(9)

Open "DEV.BAS" For Input As #1
Input #1, D$(0)
Close 1

10 Call TITLE
ITEM$(1) = "VIEW BRACKETS            "
ITEM$(2) = "VIEW SEEDINGS            "
ITEM$(3) = "RETURN TO TOURNAMENT MENU"
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
MMenuVert ITEM$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
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
    ITEM$(I) = R$(I)
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
ITEM$(R + 1) = "CHAMPIONSHIP BRACKET"
ITEM$(R + 2) = "EXIT                "
Call box0(3, 31, 3 + R + 3, 52, 2, 15)
    
_MouseShow "DEFAULT"
MMenuVert ITEM$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
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
    IF HS%(I) > VS%(I) THEN X$ = _TRIM$(H$(I)) ELSE X$ = _TRIM$(V$(I))
    GOSUB 310: PRINT X$

375 GOSUB 5000: GOTO 35

30 '12 TEAM BRACKET
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

32 '11 TEAM BRACKET
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

34 '10 TEAM BRACKET
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

36 '9 TEAM BRACKET
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

40 ' 8 TEAM BRACKET
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

45 ' 7 TEAM BRACKET
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

46 ' 6 TEAM BRACKET
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

47 ' 5 TEAM BRACKET
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

50 'FOUR TEAM BRACKET
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

55 '3 TEAM BRACKET
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

60 'TWO TEAM BRACKET
    FOR I = 0 TO 1
    IF I = 0 THEN LOCATE 7, 48: PRINT STRING$(15, CHR$(196)); CHR$(191)
    FOR I1 = 7 TO 17: LOCATE I1 + 1, 63: PRINT CHR$(179): NEXT
    LOCATE 13, 63: PRINT CHR$(195); STRING$(15, CHR$(196))
    IF I = 1 THEN LOCATE 19, 48: PRINT STRING$(15, CHR$(196)); CHR$(217)
    LOCATE 6, 48: PRINT V$(4); STR$(VS%(4))
    LOCATE 18, 48: PRINT H$(4); STR$(HS%(4))
    NEXT
    GOTO 370

200  '4 TEAM SEMI BRACKET
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
5000 I$ = GetKeyPress$
     RETURN

4050 LOCATE 10, 8
    CALL CSMsgBox("NO TOURNAMENT FILES WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Cnf)
    I$ = GetKeyPress$
    CALL CSMsgBox("", 0, Pass)
    GOTO 10


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

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
