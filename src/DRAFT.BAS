'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Courtside College Basketball - Team Draft"
''$ExeIcon:'./lhg_ccb.ico'
'_Icon
_AllowFullScreen

DECLARE SUB TITLE ()

Dim Q$(500), A$(1, 13), A(1, 13, 6), X(1, 13, 6), teamItems$(500)
Dim S$(1), A1$(1), ATT(1), DF(1, 13), P$(1, 13), RS(1, 13, 13)
Dim T1%(1, 20), CK(1), C$(1), V%(500), YN$(1), T1N(1), menuItems$(4)
Dim T%(1, 9), L%(1, 4), T$(1), SP(1), DA(1), V1%(1), V2%(1)
Dim D$(1), TR%(1, 13, 12), Y As Long
Open "DEV.BAS" For Input As #1: Input #1, D$(0): Close 1

10 For I = 0 To 1
    T$(I) = "XXX"
    SP(I) = 0
    DA(I) = 0
    V1%(I) = 0
    V2%(I) = 0
    For I1 = 0 To 4: L%(I, I1) = 0: Next
    For I1 = 0 To 9: T%(I, I1) = 0: Next
    For I1 = 0 To 13
        A$(I, I1) = "XXX"
        For I2 = 0 To 6
            A(I, I1, I2) = 0
            X(I, I1, I2) = 0
Next: Next: Next
Call TITLE
menuItems$(1) = "DRAFT A NEW TEAM         "
menuItems$(2) = "DRAFT TO AN EXISTING TEAM"
menuItems$(3) = "EXIT TO MAIN MENU        "
SELECTION% = 1
Count% = 3
Start% = 1
SCANCODE% = 3
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 8
Column% = 25

CALL box0(7, 24, 11, 50, 2,  15, 1)
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Or SELECTION% = 3 Then 1
If SELECTION% = 1 Then DNT% = 0: GoTo 100
If SELECTION% = 2 Then DNT% = 1: GoTo 2000

1 Run "HELLO.EXE 2"

100 Cls: Color 14: Print "TEAM INFORMATION": X% = 2: Color 15

147 Locate 2: Input " 1. TEAM NUMBER "; T1N(0): If C1 = 1 Then 190

162 Locate 3: Input " 2. DISK ID "; YN$(0): If C1 = 1 Then 190

163 Locate 4: Input " 3. TEAM NAME "; T$(0): If C1 = 1 Then 190

157 Locate 5: Input " 4. TEXT COLOR (0-15) "; T%(0, 0): If C1 = 1 Then 190

158 Locate 6: Input " 5. BACKGROUND (0-7) "; T%(0, 1): If C1 = 1 Then 190

164 Locate 7: Input " 6. HOMECOURT FACTOR (0-6) "; T%(0, 6): If C1 = 1 Then 190

190 Locate 9, 1: Color T%(0, 0), T%(0, 1)
Print "TEAM APPEARANCE": Color 15, 0
C1 = 1
Locate 11, 1: Print "SELECT # TO CHANGE 1-6 (0 TO CONTINUE) ";
Input B9: If B9 < 1 Or B9 > 6 Then 200
On B9 GOTO 147, 162, 163, 157, 158, 164

200 If T%(0, 6) < 0 Then T%(0, 6) = 0
If T%(0, 6) > 6 Then T%(0, 6) = 6
C1 = 0

450 Color 15, 0
Cls: Color T%(0, 0), T%(0, 1): Print T$(0);: Color 14, 0: Print "     (DRAFTING)"

451 Print: Print "PLAYERS"

452 For I = 0 To 13
    Color 2: Print Using "## "; I;: Color 15: Print A$(0, I)
Next

454 Locate 21, 1
Color 14: Print "(1) ";: Color 15: Print "DRAFT PLAYER"
Color 14: Print "(Z) ";: Color 15: Print "SAVE TEAM"
455 I$ = GetKeyPress$

456 If UCase$(I$) = "Z" Then 800

457 If I$ = "1" Then 470

458 GoTo 454

470 Locate 21, 20: Print "SLOT # (0-13) ";

471 N = 1: Input B

472 If B < 0 Or B > 14 Then 470

1440 Color 15, 0: Cls
JJ = 7
Locate 6, 8: Color 15, 4
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
Locate , 12: Print "ENTER DISK ID: ": Print
Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
Locate 7, 27: Input NY$
If NY$ = "" Then GoSub 1102: GoTo 182
YN$(1) = NY$
ERASE teamItems$
GoSub 1100

182 If BO% = 1 Then BO% = 0: GoTo 1440

183 Locate 2, 33: Color T%(1, 0), T%(1, 1): Print T$(1): Color 15, 4
Locate , 33: Print "ANY CHANGE (YN)";
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 1440

550 Color , 0: Cls: Color T%(1, 0), T%(1, 1): Print T$(1): Color 14, 0

551 Print: Print "PLAYERS"

552 For I = 0 To 13
    Color 2: Print Using "## "; I;: Color 15: Print A$(1, I)
Next

560 Color 14: Locate 21, 1: Print "SELECT PLAYER # TO DRAFT ";

561 Input B1

562 If B1 < 0 Or B1 > 13 Then 560

571 A$(0, B) = A$(1, B1)
DF(0, B) = DF(1, B1)
P$(0, B) = P$(1, B1)
For I = 0 To 13
    RS(0, B, I) = RS(1, B1, I)
Next
For I = 1 To 12
    TR%(0, B, I) = TR%(1, B1, I)
Next

572 For I = 0 To 6
    A(0, B, I) = A(1, B1, I)
    X(0, B, I) = X(1, B1, I)
Next

574 GoTo 450

800 Cls: Color T%(0, 0), T%(0, 1): Print A$: Color 14, 0
Print "PLAYER          CONTRIBUTION"
Print "                PERCENT"
Color 15, 0
T3 = 0
For I = 0 To 13
    Print Using "## "; I;: Print A$(0, I);
    Locate , 17: Print A(0, I, 3): Locate , 1
    T3 = T3 + A(0, I, 3)
Next I
Print
Color 14: Print "TOTAL CP ";: Locate , 17: Color 15, 0: Print T3

805 Locate 21: Print "SELECT PLAYER TO CHANGE 0-13 (22-NO MORE CHANGE) ";
Input X9
If X9 = 22 Then 825
If X9 < 0 Or X9 > 13 Then 805
Locate 22, 18: Print A$(0, X9): Locate , 1
Locate 23, 1
Input "ENTER CP: "; A(0, X9, 3)
GoTo 800

825 If T3 > 98 And T3 < 102 Then 1000
Cls
GoSub 4500
U$ = "TOTAL CONTRIBUTION MUST BE 100 %"
Locate , (80 - Len(U$)) / 2
Print U$
U$ = "HIT ANY KEY TO CONTINUE"
Locate , (80 - Len(U$)) / 2
Print U$
Color 15, 0
830 I$ = GetKeyPress$
GoTo 800

1000 Cls

1002 GoSub 9800

1008 Cls
GoSub 4500
U$ = "DRAFT ANOTHER TEAM (YN)"
Locate , (80 - Len(U$)) / 2
Print U$
Color 15, 0
1009 I$ = GetKeyPress$

1010 If UCase$(I$) = "Y" Then 10

1011 Cls
GoSub 4500
Run "HELLO.EXE 2"
End

1100 GoSub 3000
Open D$(0) + "BASK." + YN$(1) For Random As #1 Len = 655
Y = LOF(1)
Field #1, 15 As Q$(0), 640 As X$
For I1 = 1 To Y / 655
    Get #1, I1
    teamItems$(I1) = Q$(0): V%(I1) = I1
Next
Close 1
NT = Y / 655

1102 SELECTION% = 1
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
D = V%(SELECTION%)
GoTo 9000

1200 GoSub 3000
Open D$(0) + "BASK." + YN$(0) For Random As #1 Len = 655
Y = LOF(1)
Field #1, 15 As Q$(0), 640 As X$
For I1 = 1 To Y / 655
    Get #1, I1
    teamItems$(I1) = Q$(0): V%(I1) = I1
Next
Close 1
NT = Y / 655

1202 SELECTION% = 1
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
21 MAMenu teamItems$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% = 27 Then BO% = 1: Return
If EXITCODE% = -71 Then Start% = 1: EXITCODE% = 3: SELECTION% = 1: GoTo 21
If EXITCODE% = -79 Then Start% = NT: EXITCODE% = 3: SELECTION% = NT: GoTo 21
If EXITCODE% = -73 Then Start% = Start% - 80: If Start% < 1 Then Start% = 1
If EXITCODE% = -81 Then Start% = Start% + 80: If Start% > NT Then Start% = NT
If EXITCODE% = -81 Or EXITCODE% = -73 Then EXITCODE% = 3: SELECTION% = Start%: GoTo 21
T = V%(SELECTION%)
GoTo 9100

2000 Color 15, 0: Cls
JJ = 7
Locate 6, 8: Color 15, 4
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
Locate , 12: Print "ENTER DISK ID: ": Print
Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
Locate 7, 27: Input NY$
If NY$ = "" Then GoSub 1102: GoTo 2001
YN$(0) = NY$
Erase teamItems$
GoSub 1200

2001 If BO% = 1 Then BO% = 0: GoTo 2000

2002 Locate 2, 33: Color T%(0, 0), T%(0, 1): Print T$(0): Color 15, 4
Locate , 33: Print "ANY CHANGE (YN)";
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 2000
GoTo 450

3000 Cls

3006 Return


9000 Open D$(0) + "BASK." + YN$(1) For Random As #1 Len = 655
FIELD #1, 15 AS Q$(0), 2 AS Q$(1), 2 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 2 AS Q$(14), 2 AS Q$(15), 2 AS Q$(16), 2 AS Q$(17), 2 AS Q$(18), 2 _
AS Q$(19), 602 AS X$
For I = 0 To 13
FIELD #1, 53 + I * 43 AS X$, 15 AS Q$(I * 15 + 20), 2 AS Q$(I * 15 + 21), 2 AS Q$(I * 15 + 22), 2 AS Q$(I * 15 + 23), 2 AS Q$(I * 15 + 24), 2 AS Q$(I * 15 + 25), 2 AS Q$(I * 15 + 26), 2 AS Q$(I * 15 + 27), 2 AS Q$(I * 15 + 28), 2 AS Q$(I * 15 + 29) _
, 2 AS Q$(I * 15 + 30), 2 AS Q$(I * 15 + 31), 2 AS Q$(I * 15 + 32), 2 AS Q$(I * 15 + 33), 2 AS Q$(I * 15 + 34), 655 - 53 - 43 - I * 43 AS X$
Next
Get #1, D
T$(1) = Q$(0)

9005 If Right$(T$(1), 1) = " " Then T$(1) = Left$(T$(1), Len(T$(1)) - 1): GoTo 9005
SP(1) = CVI(Q$(1)): DA(1) = CVI(Q$(2))
V1%(1) = CVI(Q$(3)): V2%(1) = CVI(Q$(4))
For I = 0 To 4: L%(1, I) = CVI(Q$(I + 5)): Next
For I = 0 To 9: T%(1, I) = CVI(Q$(I + 10)): Next
For I = 0 To 13
    A$(1, I) = Q$(I * 15 + 20)

    9007 If Right$(A$(1, I), 1) = " " Then A$(1, I) = Left$(A$(1, I), Len(A$(1, I)) - 1): GoTo 9007
    For I1 = 0 To 6
        A(1, I, I1) = CVI(Q$(I * 15 + I1 + 21))
        X(1, I, I1) = CVI(Q$(I * 15 + I1 + 28))
Next: Next
Close 1
If V2%(1) > 20 Then V2%(1) = 100 - V2%(1)
If V1%(1) > 20 Then V1%(1) = 100 - V1%(1)
If L%(1, 0) > 20 Then L%(1, 0) = 100 - L%(1, 0)
If L%(1, 2) > 20 Then L%(1, 2) = 100 - L%(1, 2)
If L%(1, 3) > 20 Then L%(1, 3) = 100 - L%(1, 3)
Open D$(0) + "COLBBTMS." + YN$(1) For Random As #1 Len = 566
Field #1, 15 As Q$(0), 40 As Q$(1), 15 As Q$(2), 4 As Q$(3)
For I = 0 To 13
FIELD #1, 74 + I * 32 AS X$, 2 AS Q$(I * 16 + 4), 2 AS Q$(I * 16 + 5), 2 AS Q$(I * 16 + 6), 2 AS Q$(I * 16 + 7), 2 AS Q$(I * 16 + 8), 2 AS Q$(I * 16 + 9), 2 AS Q$(I * 16 + 10), 2 AS Q$(I * 16 + 11), 2 AS Q$(I * 16 + 12), 2 AS Q$(I * 16 + 13), 2 AS  _
Q$(I * 16 + 14), 2 AS Q$(I * 16 + 15), 2 AS Q$(I * 16 + 16), 2 AS Q$(I * 16 + 17), 2 AS Q$(I * 16 + 18), 2 AS Q$(I * 16 + 19)
FIELD #1, 522 AS X$, 2 AS Q$(228), 2 AS Q$(229), 2 AS Q$(230), 2 AS Q$(231), 2 AS Q$(232), 2 AS Q$(233), 2 AS Q$(234), 2 AS Q$(235), 2 AS Q$(236), 2 AS Q$(237), 2 AS Q$(238), 2 AS Q$(239), 2 AS Q$(240), 2 AS Q$(241), 2 AS Q$(242), 2 AS Q$(243), 2 AS _
Q$(244), 2 AS Q$(245), 2 AS Q$(246), 2 AS Q$(247), 2 AS Q$(248), 2 AS Q$(249)
Next
Get #1, D
C$(1) = Q$(0)
S$(1) = Q$(1)
A1$(1) = Q$(2)
ATT(1) = CVL(Q$(3))
For I = 0 To 13
    DF(1, I) = CVI(Q$(I * 16 + 4))
    P$(1, I) = Q$(I * 16 + 5)
    For I1 = 0 To 13
        RS(1, I, I1) = CVI(Q$(I * 16 + I1 + 6))
Next: Next
For I = 0 To 20
    T1%(1, I) = CVI(Q$(228 + I))
Next
CK(1) = CVI(Q$(249))
Close 1
For I = 0 To 13
    TR%(1, I, 1) = SP(1)
    TR%(1, I, 2) = DA(1)
    TR%(1, I, 3) = V2%(1)
    TR%(1, I, 4) = V1%(1)
    TR%(1, I, 5) = L%(1, 0)
    TR%(1, I, 6) = L%(1, 2)
    TR%(1, I, 7) = L%(1, 3)
    TR%(1, I, 8) = 99
    TR%(1, I, 9) = T%(1, 3)
    TR%(1, I, 10) = T%(1, 4)
    TR%(1, I, 11) = T%(1, 5)
    TR%(1, I, 12) = T%(1, 6)
Next
If CK(1) <> 999 Then
    C$(1) = "": S$(1) = "": A1$(1) = "": ATT(1) = 0
    For I = 0 To 13: DF(1, I) = 5: P$(1, I) = ""
        For I1 = 0 To 13: RS(1, I, I1) = 0
    Next: Next
    For I = 0 To 20: T1%(1, I) = 0: Next
End If

9010 Return

9100 Open D$(0) + "BASK." + YN$(0) For Random As #1 Len = 655
FIELD #1, 15 AS Q$(0), 2 AS Q$(1), 2 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 2 AS Q$(14), 2 AS Q$(15), 2 AS Q$(16), 2 AS Q$(17), 2 AS Q$(18), 2 _
AS Q$(19), 602 AS X$
For I = 0 To 13
FIELD #1, 53 + I * 43 AS X$, 15 AS Q$(I * 15 + 20), 2 AS Q$(I * 15 + 21), 2 AS Q$(I * 15 + 22), 2 AS Q$(I * 15 + 23), 2 AS Q$(I * 15 + 24), 2 AS Q$(I * 15 + 25), 2 AS Q$(I * 15 + 26), 2 AS Q$(I * 15 + 27), 2 AS Q$(I * 15 + 28), 2 AS Q$(I * 15 + 29) _
, 2 AS Q$(I * 15 + 30), 2 AS Q$(I * 15 + 31), 2 AS Q$(I * 15 + 32), 2 AS Q$(I * 15 + 33), 2 AS Q$(I * 15 + 34), 655 - 53 - 43 - I * 43 AS X$
Next
Get #1, T
T$(0) = Q$(0)

9105 If Right$(T$(0), 1) = " " Then T$(0) = Left$(T$(0), Len(T$(0)) - 1): GoTo 9105
SP(0) = CVI(Q$(1)): DA(0) = CVI(Q$(2))
V1%(0) = CVI(Q$(3)): V2%(0) = CVI(Q$(4))
For I = 0 To 4: L%(0, I) = CVI(Q$(I + 5)): Next
For I = 0 To 9: T%(0, I) = CVI(Q$(I + 10)): Next
For I = 0 To 13
    A$(0, I) = Q$(I * 15 + 20)

    9107 If Right$(A$(0, I), 1) = " " Then A$(0, I) = Left$(A$(0, I), Len(A$(0, I)) - 1): GoTo 9107
    For I1 = 0 To 6
        A(0, I, I1) = CVI(Q$(I * 15 + I1 + 21))
        X(0, I, I1) = CVI(Q$(I * 15 + I1 + 28))
Next: Next
Close 1
If V1%(0) > 20 Then V1%(0) = 100 - V1%(0)
If V2%(0) > 20 Then V2%(0) = 100 - V2%(0)
If L%(0, 0) > 20 Then L%(0, 0) = 100 - L%(0, 0)
If L%(0, 2) > 20 Then L%(0, 2) = 100 - L%(0, 2)
If L%(0, 3) > 20 Then L%(0, 3) = 100 - L%(0, 3)
Open D$(0) + "COLBBTMS." + YN$(0) For Random As #1 Len = 566
Field #1, 15 As Q$(0), 40 As Q$(1), 15 As Q$(2), 4 As Q$(3)
For I = 0 To 13
FIELD #1, 74 + I * 32 AS X$, 2 AS Q$(I * 16 + 4), 2 AS Q$(I * 16 + 5), 2 AS Q$(I * 16 + 6), 2 AS Q$(I * 16 + 7), 2 AS Q$(I * 16 + 8), 2 AS Q$(I * 16 + 9), 2 AS Q$(I * 16 + 10), 2 AS Q$(I * 16 + 11), 2 AS Q$(I * 16 + 12), 2 AS Q$(I * 16 + 13), 2 AS  _
Q$(I * 16 + 14), 2 AS Q$(I * 16 + 15), 2 AS Q$(I * 16 + 16), 2 AS Q$(I * 16 + 17), 2 AS Q$(I * 16 + 18), 2 AS Q$(I * 16 + 19)
FIELD #1, 522 AS X$, 2 AS Q$(228), 2 AS Q$(229), 2 AS Q$(230), 2 AS Q$(231), 2 AS Q$(232), 2 AS Q$(233), 2 AS Q$(234), 2 AS Q$(235), 2 AS Q$(236), 2 AS Q$(237), 2 AS Q$(238), 2 AS Q$(239), 2 AS Q$(240), 2 AS Q$(241), 2 AS Q$(242), 2 AS Q$(243), 2 AS _
Q$(244), 2 AS Q$(245), 2 AS Q$(246), 2 AS Q$(247), 2 AS Q$(248), 2 AS Q$(249)
Next
Get #1, T
C$(0) = Q$(0)
S$(0) = Q$(1)
A1$(0) = Q$(2)
ATT(0) = CVL(Q$(3))
For I = 0 To 13
    DF(0, I) = CVI(Q$(I * 16 + 4))
    P$(0, I) = Q$(I * 16 + 5)
    For I1 = 0 To 13
        RS(0, I, I1) = CVI(Q$(I * 16 + I1 + 6))
Next: Next
For I = 0 To 20
    T1%(0, I) = CVI(Q$(228 + I))
Next
CK(0) = CVI(Q$(249))
Close 1
For I = 0 To 13
    TR%(0, I, 1) = SP(0)
    TR%(0, I, 2) = DA(0)
    TR%(0, I, 3) = V2%(0)
    TR%(0, I, 4) = V1%(0)
    TR%(0, I, 5) = L%(0, 0)
    TR%(0, I, 6) = L%(0, 2)
    TR%(0, I, 7) = L%(0, 3)
    TR%(0, I, 8) = 99
    TR%(0, I, 9) = T%(0, 3)
    TR%(0, I, 10) = T%(0, 4)
    TR%(0, I, 11) = T%(0, 5)
    TR%(0, I, 12) = T%(0, 6)
Next
If CK(0) <> 999 Then
    C$(0) = "": S$(0) = "": A1$(0) = "": ATT(0) = 0
    For I = 0 To 13: DF(0, I) = 5: P$(0, I) = ""
        For I1 = 0 To 13: RS(0, I, I1) = 0
    Next: Next
    For I = 0 To 20: T1%(0, I) = 0: Next
End If

9110 Return

9800 NP = 0
For I = 0 To 13
    If A$(0, I) = "XXX" Then 9801
    NP = NP + 1
    SP(0) = SP(0) + TR%(0, I, 1)
    DA(0) = DA(0) + TR%(0, I, 2)
    V2%(0) = V2%(0) + TR%(0, I, 3)
    V1%(0) = V1%(0) + TR%(0, I, 4)
    L%(0, 0) = L%(0, 0) + TR%(0, I, 5)
    L%(0, 2) = L%(0, 2) + TR%(0, I, 6)
    L%(0, 3) = L%(0, 3) + TR%(0, I, 7)
    T%(0, 3) = T%(0, 3) + TR%(0, I, 9)
    T%(0, 4) = T%(0, 4) + TR%(0, I, 10)
    T%(0, 5) = T%(0, 5) + TR%(0, I, 11)
    T%(0, 6) = T%(0, 6) + TR%(0, I, 12)

9801 Next
SP(0) = SP(0) / (NP - 1)
DA(0) = DA(0) / (NP - 1)
V2%(0) = V2%(0) / (NP - 1)
If V2%(0) > 20 Then V2%(0) = 100 - V2%(0)
V1%(0) = V1%(0) / (NP - 1)
If V1%(0) > 20 Then V1%(0) = 100 - V1%(0)
L%(0, 0) = L%(0, 0) / (NP - 1)
If L%(0, 0) > 20 Then L%(0, 0) = 100 - L%(0, 0)
L%(0, 2) = L%(0, 2) / (NP - 1)
If L%(0, 2) > 20 Then L%(0, 2) = 100 - L%(0, 2)
L%(0, 3) = L%(0, 3) / (NP - 1)
If L%(0, 3) > 20 Then L%(0, 3) = 100 - L%(0, 3)
L%(0, 4) = 99
T%(0, 3) = T%(0, 3) / (NP - 1)
T%(0, 4) = T%(0, 4) / (NP - 1)
T%(0, 5) = T%(0, 5) / (NP - 1)
T%(0, 6) = T%(0, 6) / (NP - 1)
Open D$(0) + "BASK." + YN$(0) For Random As #1 Len = 655
Y = LOF(1)
If DNT% = 0 Then T = Y / 655 + 1
FIELD #1, 15 AS Q$(0), 2 AS Q$(1), 2 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 2 AS Q$(14), 2 AS Q$(15), 2 AS Q$(16), 2 AS Q$(17), 2 AS Q$(18), 2 _
AS Q$(19), 602 AS X$
For I = 0 To 13
FIELD #1, 53 + I * 43 AS X$, 15 AS Q$(I * 15 + 20), 2 AS Q$(I * 15 + 21), 2 AS Q$(I * 15 + 22), 2 AS Q$(I * 15 + 23), 2 AS Q$(I * 15 + 24), 2 AS Q$(I * 15 + 25), 2 AS Q$(I * 15 + 26), 2 AS Q$(I * 15 + 27), 2 AS Q$(I * 15 + 28), 2 AS Q$(I * 15 + 29) _
, 2 AS Q$(I * 15 + 30), 2 AS Q$(I * 15 + 31), 2 AS Q$(I * 15 + 32), 2 AS Q$(I * 15 + 33), 2 AS Q$(I * 15 + 34), 655 - 53 - 43 - I * 43 AS X$
Next
LSet Q$(0) = T$(0): LSet Q$(1) = MKI$(SP(0)): LSet Q$(2) = MKI$(DA(0)): LSet Q$(3) = MKI$(V1%(0))
LSet Q$(4) = MKI$(V2%(0)): For I = 0 To 4: LSet Q$(I + 5) = MKI$(L%(0, I)): Next
T%(0, 2) = 999
For I = 0 To 9: LSet Q$(I + 10) = MKI$(T%(0, I)): Next
For I = 0 To 13
    LSet Q$(I * 15 + 20) = A$(0, I)
    For I1 = 0 To 6
        LSet Q$(I * 15 + I1 + 21) = MKI$(A(0, I, I1))
        LSet Q$(I * 15 + I1 + 28) = MKI$(X(0, I, I1))
Next: Next
Put #1, T: Close 1

Open D$(0) + "COLBBTMS." + YN$(0) For Random As #1 Len = 566
Field #1, 15 As Q$(0), 40 As Q$(1), 15 As Q$(2), 4 As Q$(3)
For I = 0 To 13
    Field #1, (74 + I * 32) As X$, 2 As Q$(I * 16 + 4), 2 As Q$(I * 16 + 5), 2 As Q$(I * 16 + 6), 2 As Q$(I * 16 + 7), 2 As Q$(I * 16 + 8), 2 As Q$(I * 16 + 9), 2 As Q$(I * 16 + 10), 2 As Q$(I * 16 + 11), 2 As Q$(I * 16 + 12), 2 As Q$(I * 16 + 13), 2 As Q$(I * 16 + 14), 2 As Q$(I * 16 + 15), 2 As Q$(I * 16 + 16), 2 As Q$(I * 16 + 17), 2 As Q$(I * 16 + 18), 2 As Q$(I * 16 + 19)
    Field #1, 522 As X$, 2 As Q$(228), 2 As Q$(229), 2 As Q$(230), 2 As Q$(231), 2 As Q$(232), 2 As Q$(233), 2 As Q$(234), 2 As Q$(235), 2 As Q$(236), 2 As Q$(237), 2 As Q$(238), 2 As Q$(239), 2 As Q$(240), 2 As Q$(241), 2 As Q$(242), 2 As Q$(243), 2 As Q$(244), 2 As Q$(245), 2 As Q$(246), 2 As Q$(247), 2 As Q$(248), 2 As Q$(249)
Next
LSet Q$(0) = C$(0)
LSet Q$(1) = S$(0)
LSet Q$(2) = A1$(0)
LSet Q$(3) = MKL$(ATT(0))
For I = 0 To 13
    LSet Q$(I * 16 + 4) = MKI$(DF(0, I))
    LSet Q$(I * 16 + 5) = P$(0, I)
    For I1 = 0 To 13
        LSet Q$(I * 16 + I1 + 6) = MKI$(RS(0, I, I1))
Next: Next
For I = 0 To 20
    LSet Q$(228 + I) = MKI$(T1%(0, I))
Next
LSet Q$(249) = MKI$(999)
Put #1, T: Close 1

9821 Return

'------------------------------
'      TITLE Subroutine
'------------------------------
' Displays window title / game copyright
Sub TITLE ()
    Color , 0: KEY Off: Cls
    Color , 1: Locate 1, 1: Print Tab(80);: Locate , 22: Color 12
    Print "COURTSIDE COLLEGE BASKETBALL V4.0: DRAFT"
    Locate 24, 1: Print Tab(80);: Locate , 1
    Print "(C) LANCE HAFFNER/SHANNON LYNN 1987-1997"; Tab(70); Left$(Time$, 5);
    Locate 3, 1: Color 2, 0
    Color 15: Print "        PROGRAMMED AND DEVELOPED BY LANCE HAFFNER AND SHANNON LYNN"
    Print "     FOR TECHNICAL SUPPORT, CALL (800) 477-7032 OR SportGames@AOL.COM"
    Color 10
End Sub

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
