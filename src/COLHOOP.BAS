'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'
'$INCLUDE: 'GameRoutines.BI'

_Title "Courtside College Basketball - HOOPS!"
$ExeIcon:'./lhg_ccb.ico'
_Icon
_AllowFullScreen

DefInt A-Z

Dim Shared AP, B, C1, C7, DY, CT, D, FO, F3, G9, H, I, IN, J, JJ
Dim Shared M5, MJ, N, NT, p, P9, Q, S2, S9, TMT, TOU, U5, XM, XS

Dim Shared B1(1, 4), C(1, 13, 6), D1(1), D2(1), D8(4, 3), DF(1, 13), DP(2, 4)
Dim Shared F1(13), G4(13), G5(13), P2(1), P4(14), P5(14), P7(1)
Dim Shared QQ(1, 8, 13, 14), QR(1, 7, 14), R3(1), RB(9), RS(1, 13, 13)
Dim Shared S(1, 9), S1(1), S3(13), T0(1), T1(1), V9(14)
Dim Shared W0(1, 13), W1(1, 13), W2(1, 13), W3(1, 13), Z5(1), Z6(1), ZZ(1, 13)

Dim Shared BO%, BU%, CM%, DK%, EG%, F3S%, FB%, FT%, HT%, J8%, LC%, N7%, NF%, NL%
Dim Shared PA%, PASS%, PB%, PT%, PZ%, ST%, TB%, TF%, TS%, X3%, X0%, X1%, XX%

Dim Shared AP%(2), APT%(100, 1), B%(1, 13, 18), CZ%(1), DT%(1), E%(13), F5%(1, 8), FA%(1, 13), FY%(2)
Dim Shared G9%(1), HF%(1, 6), HT%(100), L%(1, 4), N%(16, 16, 3), NG%(18), NG1%(18)
Dim Shared O%(100), OF%(1), PR%(1, 1), ST%(32), SX%(32, 1, 14)
Dim Shared T2%(1, 20), TF%(1), TM%(1, 13), TP%(1), TR%(1, 9), teamIndex%(500), V1%(1), V2%(1)
Dim Shared vg%(8), vh%(8), W%(1, 13, 1)

Dim Shared A1$, B1$, C1$, D1$, E1$, F1$, FF$, G1$, H1$, J$, PB$, U$, VT$, VT1$, W$, W1$, YN$

Dim Shared C7$(2) 'C7$(3) would allow for the 30-second shot clock option
Dim Shared A1$(1), AA$(1, 13), B$(1), C$(1), CM$(1), D$(15), DB$(14)
Dim Shared H$(100), HO$(100), LC$(2), N$(16, 16, 3), O$(9), OB$(9), p$(1)
Dim Shared PB$(1), PO$(1, 13), PS$(4), Q$(250), R$(14), S$(1), SITE$(100), SX$(32, 2)
Dim Shared TB$(1), TS$(1), U5$(3), V$(500), X$(3), Y$(1), YN$(5), YN1$(3)

Dim Shared A(1, 13, 24) As Single
Dim Shared Z(15) As Single, Z1(15) As Single, Z2(13, 13) As Single
Dim Shared F As Single, R0 As Single
Dim Shared T As Single, TE As Single
Dim Shared displayPause As Single
Dim Shared ATT(1) As Long, CRD(100) As Long
Dim Shared Y2 As Long, Y3 As Long


'----------------------------------------
'           GAME CODE
'----------------------------------------


' Setup Game
'--------------------
Clear

Randomize Timer

Open "DEV.BAS" For Input As #2: Input #2, p$(0): Input #2, p$(1): Close 2
Open "DEFAULT" For Input As #1
Input #1, U5: Input #1, X3%: Input #1, C7: Input #1, N: Input #1, NF%: Input #1, displayPause: Input #1, CM%: Input #1, TB%: Input #1, TF%: Input #1, N7%: Input #1, PB%: Close 1

If Not _FileExists("TICKER") Then TMT = 0: GoTo 5
Open "TICKER" For Input As #2
For I = 1 To 32
    For I1 = 0 To 1
        For I2 = 1 To 14
            Input #2, SX%(I, I1, I2)
        Next
    Next
Next
For I = 1 To 32
    For I1 = 0 To 1
        Input #2, SX$(I, I1)
    Next:
Next
For I = 1 To 32: Input #2, ST%(I): Next
Close 2
TMT = 1


5 If _FileExists("GAME82") Then

    Open "GAME82" For Input As #2
    Input #2, FF$
    For I = 0 To 2: Input #2, AP%(I): Next
    Close 2

    Open p$(0) + FF$ For Random As #1 Len = 48
    For X = 0 To 18
        Field #1, X * 2 As X$, 2 As Q$(X + 1), 48 - 2 - 2 * X As X$
    Next
    Field #1, 38 As X$, 2 As Q$(22), 2 As Q$(23), 3 As Q$(20), 3 As Q$(21)
    Get #1, AP%(0) + AP%(2)
    For X = 0 To 18: NG%(X) = CVI(Q$(X + 1)): Next
    For X = 0 To 1: YN$(X) = Q$(20 + X): Next
    VS% = CVI(Q$(22)): HS% = CVI(Q$(23))
    Close 1

    AP = 1
End If

6 If _FileExists("TOURNEY") Then
    Open "TOURNEY" For Input As #2
    Input #2, FF$
    For I = 0 To 2: Input #2, AP%(I): Next
    Close 2

    If AP%(2) = AP%(1) Then Kill "TOURNEY": End

    Open p$(0) + FF$ + ".TOU" As #1 Len = 90
    For X = 0 To 14
        Field #1, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
    Next

    Field #1, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
    Field #1, 64 As X$, 3 As Q$(20), 3 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
    Field #1, 84 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)

    Get #1, AP%(0) + AP%(2)

    VS% = CVI(Q$(16)): HS% = CVI(Q$(17))
    VT$ = Q$(18): W$ = Q$(19)

    For X = 0 To 14: NG%(X) = CVI(Q$(X + 1)): Next
    For X = 0 To 3: YN$(X) = Q$(20 + X): Next
    For X = 1 To 3: NG%(X + 14) = CVI(Q$(X + 23)): Next
    Close 1

    AP = 1
    TOU = 1
End If

T = 1200
H = 1
T1(1) = 5
T1(0) = 5
X$(0) = "A "
X$(1) = "B "
X$(2) = "C "
X$(3) = "D "
C7$(0) = "NO SHOT CLOCK    "
C7$(1) = "45 SEC SHOT CLOCK"
C7$(2) = "35 SEC SHOT CLOCK"
'C7$(3) = "30 SEC SHOT CLOCK"

For I = 0 To 1
    For I1 = 0 To 4
        B1(I, I1) = -1
    Next I1
Next I

If AP = 1 Then Call AUTOSETUP
If AP = 1 And TOU = 0 Then 51

'Initialize variables
'--------------------
NL% = 1
PA% = -1
TR%(0, 0) = 15: TR%(0, 1) = 1
TR%(1, 0) = 15: TR%(1, 1) = 1

Data L,1,1,1,1,1,1,H,H,2,2,2,2,2,F
For I = 0 To 14: Read R$(I): Next
Data 0,1,1,2,2,2,-1,-1,-1,-1,-2,-3
For I = 0 To 1
    For I1 = 1 To 6
        Read HF%(I, I1)
    Next
Next
Data 1,4,2,5,15,29,22,36,7,9,7,12
For I = 0 To 2
    For I1 = 1 To 4
        Read DP(I, I1)
    Next
Next
Data -7,-10,-4,-10,70,100,20,100,-1,1,-2,2,15,22,8,29,5,7,3,9
For I = 0 To 4
    For I1 = 0 To 3
        Read D8(I, I1)
    Next
Next

Data "NORMAL"
Data "AGGRESSIVE"
Data "SAFE"
Data "QUICK OFFENSE (LAST 3 MIN)"
Data "DELAY OFFENSE (LAST 3 MIN)"
Data "MILK CLOCK TO 15 SEC (IF USING SHOT CLOCK)"
Data "STALL-ONLY REAL GOOD SHOT (MUST BE WITHIN 10 POINTS)"
Data "TOTAL STALL (LAST 3 MIN)"
Data "HOLD BALL VS PASSIVE ZONE (MUST BE AHEAD, WITH NO SHOT CLOCK)"
Data "SHOOT ONLY THREE POINTERS (LAST 3 MIN)"
For I = 0 To 9: Read O$(I): Next

Data "SOLID MAN TO MAN"
Data "PRESSURE MAN TO MAN"
Data "PASSIVE 2-3 ZONE"
Data "1-3-1 TRAPPING ZONE"
Data "FULL COURT MAN TO MAN PRESS/SOLID MAN TO MAN"
Data "FULL COURT MAN TO MAN (RUN & JUMP) PRESS/SOLID MAN TO MAN"
Data "FULL COURT MAN TO MAN (RUN & JUMP) PRESS/PRESSURE MAN TO MAN"
Data "PASSIVE 2-2-1 ZONE PRESS/2-3 ZONE"
Data "PASSIVE 2-2-1 ZONE PRESS/SOLID MAN TO MAN"
Data "PASSIVE 2-2-1 ZONE PRESS/PRESSURE MAN TO MAN"
Data "TRAPPING 1-2-1-1 ZONE PRESS/SOLID MAN TO MAN"
Data "TRAPPING 1-2-1-1 ZONE PRESS/PRESSURE MAN TO MAN"
Data "TRAPPING 1-2-1-1 ZONE PRESS/2-3 ZONE  "
Data "TRAPPING 1-2-1-1 ZONE PRESS/1-3-1 TRAPPING ZONE"
Data "FULL COURT M-M DENIAL & FOUL (LAST 3 MIN WHEN DOWN)"
For I = 0 To 14
    Read D$(I)
Next I

Data "NORMAL                            "
Data "AGGRESSIVE                        "
Data "SAFE                              "
Data "QUICK OFFENSE (LAST 3 MIN)        "
Data "DELAY OFFENSE (LAST 3 MIN)        "
Data "MILK CLOCK TO 15 SEC-IF USING S.C."
Data "STALL-GOOD SHOT-MUST BE WITHIN 10 "
Data "TOTAL STALL (LAST 3 MIN)          "
Data "HOLD BALL VS PASS.ZN-AHEAD-NO S.C."
Data "SHOOT ONLY THREES (LAST 3 MIN)    "
For I = 0 To 9
    Read OB$(I)
Next

Data "SOLID MAN TO MAN                  "
Data "PRESSURE MAN TO MAN               "
Data "PASSIVE 2-3 ZONE                  "
Data "1-3-1 TRAPPING ZONE               "
Data "FULL COURT M-M PRESS/SOLID M-M    "
Data "M-M (RUN & JUMP) PRESS/SOLID M-M  "
Data "M-M (RUN & JMP) PRESS/PRESSURE M-M"
Data "PASSIVE 2-2-1 ZONE PRESS/2-3 ZONE "
Data "PASSIVE 2-2-1 ZONE PRESS/SOLID M-M"
Data "PASSIVE 2-2-1 ZN PRSS/PRESSURE M-M"
Data "TRAP 1-2-1-1 ZONE PRESS/SOLID M-M "
Data "TRAP 1-2-1-1 ZN PRESS/PRESSURE M-M"
Data "TRAP 1-2-1-1 ZONE PRESS/2-3 ZONE  "
Data "TRAP 1-2-1-1 ZN PRSS/1-3-1 TRAP ZN"
Data "DENIAL & FOUL-LAST 3 MIN WHEN DOWN"
For I = 0 To 14
    Read DB$(I)
Next I

TS$(0) = "N": TS$(1) = "Y"

CM$(0) = "COACH AND SELECT SHOTS           "
CM$(1) = "COACH ONLY AND LET COMPUTER SHOOT"

U5$(0) = "HUMAN OPPONENT           "
U5$(1) = "COMPUTER IS VISITING TEAM"
U5$(2) = "COMPUTER IS HOME TEAM    "
U5$(3) = "COMPUTER VS COMPUTER     "

LC$(0) = "@ TEAM 1    "
LC$(1) = "HOME COURT ADVANTAGE"
LC$(2) = "NEUTRAL SITE        "
TB$(0) = "SHOOT 1+1": TB$(1) = "SHOOT 2  "
PB$(0) = "VERBOSE        "
PB$(1) = "HAFFNER CLASSIC"


24 Cls
Color 2
Print ; Tab(29); "SELECT OPENING OPTIONS"
Print

25 Locate 2, 2: Color 14: Print "(1) ";: Color 15: Print U5$(U5)
Locate 3, 2: Color 14: Print "(2) ";: Color 15: Print "VISITING TEAM: ";: Color TR%(0, 0), TR%(0, 1): Print B$(0): Color , 0
Locate 4, 2: Color 14: Print "(3) ";: Color 15: Print "HOME TEAM: ";: Color TR%(1, 0), TR%(1, 1): Print B$(1): Color , 0

28 Locate 5, 2: Color 14: Print "(4) ";: Color 15: Print "3-POINT SHOT: "; TS$(X3%)

29 Locate 6, 2: Color 14: Print "(5) ";: Color 15: Print C7$(C7)

30 Locate 7, 2: Color 14: Print "(6) ";: Color 15: Print "LOCATION: "; LC$(N)
Locate 8, 2: Color 14: Print "(7) ";: Color 15: Print Using "DISPLAY PAUSE IN SECONDS: ##.#"; displayPause

32 Locate 9, 2: Color 14: Print "(8) ";: Color 15: Print "START GAME"
Locate 10, 2: Color 14: Print "(9) ";: Color 15: Print "FOULS TO DISQUALIFY: "; NF%
Locate 11, 2: Color 14: Print "(0) ";: Color 15: Print "SAVE SETTINGS"
Locate 12, 2: Color 14: Print "(A) ";: Color 15: Print "FREE THROWS ON 10TH FOUL: "; TB$(TB%)
Locate 13, 2: Color 14: Print "(B) ";: Color 15: Print "HUMAN COACHES WILL: "; CM$(CM%);: Locate 1, 1
Locate 14, 2: Color 14: Print "(C) ";: Color 15: Print "3 FT WHEN FOULED ON 3-PT ATT: "; TS$(TF%)
Locate 15, 2: Color 14: Print "(D) ";: Color 15: Print "1 FT ON 1ST 6 FLOOR FOULS : "; TS$(N7%)
Locate 16, 2: Color 14: Print "(E) ";: Color 15: Print "PLAY BY PLAY: "; PB$(PB%)
Locate 17, 2: Print "NOTE:: 1 FT ON 1ST 6 FLOOR FOULS THROUGH 1972-73 SEASON"
Locate , 2: Print "       45 SEC SHOT CLOCK WAS ADOPTED IN 1984-85, BUT NOT USED IN"
Locate , 2: Print "       84-85 POST SEASON TOURNAMENTS"
Locate , 2: Print "       3 PT SHOT WAS ADOPTED IN 1986-87"
Locate , 2: Print "       2 FT INSTEAD OF 1+1 AFTER 10TH FOUL ADOPTED IN 1990-91"
Locate , 2: Print "       3 FT WHEN FOULED ON A 3 PT ATT ADOPTED IN 1990-91"
Locate , 2: Print "       35 SEC SHOT CLOCK WAS ADOPTED IN 1993-94"

'Locate 17, 2: Print "NOTE::"
'Locate , 2: Print "       1 FT ON 1ST 6 FLOOR FOULS THROUGH 1972-73 SEASON"
'Locate , 2: Print "       45 SEC SHOT CLOCK WAS ADOPTED IN 1984-85, BUT NOT USED IN"
'Locate , 2: Print "       45 SEC SHOT CLOCK ADOPTED 1984-85, BUT NOT FOR POST-SEASON"
'Locate , 2: Print "       3 PT SHOT ADOPTED IN 1986-87"
'Locate , 2: Print "       2 FT INSTEAD OF 1+1 AFTER 10TH FOUL ADOPTED 1990-91"
'Locate , 2: Print "       3 FT WHEN FOULED ON A 3 PT ATT ADOPTED 1990-91"
'Locate , 2: Print "       35 SEC SHOT CLOCK ADOPTED 1993-94"
'Locate , 2: Print "       30 SEC SHOT CLOCK ADOPTED 2015-16"

35 Color 7
I$ = GetKeyPress$
If I$ = Chr$(27) Then Run "HELLO.EXE"
I = Val(I$)
If UCase$(I$) = "A" Then I = 11
If UCase$(I$) = "B" Then I = 12
If UCase$(I$) = "C" Then I = 13
If UCase$(I$) = "D" Then I = 14
If UCase$(I$) = "E" Then I = 15
If I < 0 Or I > 15 Then 35
If I = 0 Then I = 10

On I GOTO 38, 40, 41, 42, 44, 46, 48, 50, 45, 43, 47, 33, 34, 39, 22

22 PB% = 1 - PB%: GoTo 32

33 CM% = 1 - CM%: GoTo 32

34 TF% = 1 - TF%: GoTo 32

38 U5 = U5 + 1: If U5 > 3 Then U5 = 0
GoTo 25

39 N7% = 1 - N7%: GoTo 32

40 P9 = 0: Call SELECTID: GoTo 24

41 P9 = 1: Call SELECTID: GoTo 24

42 X3% = 1 - X3%: GoTo 28

43 Open "DEFAULT" For Output As #1
Print #1, U5: Print #1, X3%: Print #1, C7: Print #1, N: Print #1, NF%: Print #1, displayPause: Print #1, CM%: Print #1, TB%: Print #1, TF%: Print #1, N7%: Print #1, PB%
Close 1

GoTo 32

44 C7 = C7 + 1: If C7 > 2 Then C7 = 0
GoTo 29

45 NF% = NF% + 1: If NF% > 6 Then NF% = 5
GoTo 32

46 N = N + 1: If N > 2 Then N = 1
GoTo 30

47 TB% = 1 - TB%: GoTo 32

48 Locate 8, 32: Input displayPause: If displayPause < 0 Then 48
GoTo 24

50 If B$(0) = "" Or B$(1) = "" Then 24


'---------------------------------------
'      Start of Actual Game Play
'---------------------------------------
Cls
Color 3

SN = 1
Locate 6, 8
Color TR%(1, 0), TR%(1, 1)

'Print box top
Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

'Print box sides
JJ = 6
For II = 1 To JJ
    Locate 6 + II, 8
    Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
Next
Locate 7 + JJ, 8

'Print box bottom
Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

Locate 7, 11

Print "IF YOU ARE USING THE 10 MINUTE TICKER OPTION FOR THIS GAME,"
Locate , 11: Print "ENTER STARTING TIME FOR GAME IN THIS FORMAT"
Locate , 11: Print "(EXAMPLES: 1:00P, 11:30A). OTHERWISE, JUST HIT ENTER": Print
Locate , 11: Input "START TIME: "; ST$
L = Len(ST$)
If L = 5 Then HR = Val(Left$(ST$, 1)): MIN = Val(Mid$(ST$, 3, 2))
If L = 6 Then HR = Val(Left$(ST$, 2)): MIN = Val(Mid$(ST$, 4, 2))
TT$ = Right$(ST$, 1)
If UCase$(TT$) = "P" And Left$(ST$, 2) <> "12" Then HR = HR + 12
TM = HR * 60 + MIN
ST% = Int(TM / 30 - 18)
Call EDITCX

51 FY%(2) = Int(((FY%(1) + FY%(0)) / 2) + .5)
For I = 0 To 1
    For I1 = 0 To 13
        TM%(I, I1) = 1200
    Next
Next
If U5 = 1 Then CT = 0: GoTo 55
If U5 = 2 Then U5 = 1: CT = 1: GoTo 55
If U5 = 3 Then U5 = 2

55 Call LOADSTATS
GoTo 900

159 Call CLOCKOVER
Call SCOREBOARD
Return

170 For I = 0 To 1
    DT%(I) = 0
    D2(I) = 0
    P7(I) = 0
Next

T = 0
Call QUARTERTOTAL
H = H + 1
T = 1200
XS = 1
X = 0

If H = 2 Then
    U$ = "END OF HALF"
    Select Case PB%
        Case 0: Color 7, 0: Call WINDEX: Print U$
        Case 1: If displayPause > 0 Then Call WINDEX: Print U$
    End Select
End If

If H > 3 Then
    U$ = "END OF OVERTIME" + Str$(H - 3)
    Select Case PB%
        Case 0: Color 7, 0: Print U$
        Case 1: If displayPause > 0 Then Call WINDEX: Print U$
    End Select
End If
GoSub 178

If H < 3 And U5 <> 0 Then GoSub 175
If H < 3 Then P2(0) = 0: P2(1) = 0: GoSub 159: Print: B = -1: Call DEADBALLOPTIONS: T = 1200: Call MAINSCREEN: For I = 0 To 1: For I1 = 0 To 13: TM%(I, I1) = 1200: FA%(I, I1) = 0: Next: Next: GoTo 250

GoSub 159

If H = 3 Then
    U$ = "END OF REGULATION"
    Select Case PB%
        Case 0: Color 7, 0: Call WINDEX: Print U$
        Case 1: If displayPause > 0 Then Call WINDEX: Print U$
    End Select
End If
Locate 2, 31: Print "END OF REGULATION"

'If scores are tied, it's Overtime!
If S(0, 0) = S(1, 0) Then
    U$ = "OVERTIME !!!"
    Select Case PB%
        Case 0: Color 7, 0: Call WINDEX: Print U$
        Case 1: Call WINDEX: Print U$: Call DELAY
    End Select
    GoTo 190
End If

'Call HITKEY: GoTo 8400
If Not (AP = 1 And U5 = 2) Then
    I$ = GetKeyPress
End If
GoTo 8400

175 If U5 = 1 Then P9 = CT: Call COMCOACH
If U5 = 2 Then P9 = 0: Call COMCOACH: P9 = 1: Call COMCOACH
Return

178 If H = 2 Then
    U$ = "HALFTIME!"
    Select Case PB%
        Case 0: Color 7, 0: Call WINDEX: Print U$
        Case 1: Call WINDEX: Print U$: Call DELAY
    End Select
    GoSub 8515
End If
Return

190 T1(0) = T1(0) + 1: T1(1) = T1(1) + 1: T = 300
Call MAINSCREEN
For I = 0 To 1
    For I1 = 0 To 13: TM%(I, I1) = 300: FA%(I, I1) = 0
    Next
Next
B = -1
Call DEADBALLOPTIONS
Call SCOREBOARD
GoTo 250

240 S9 = (S1(0) + S1(1)) / 2
For P9 = 0 To 1
    If U5 = 1 And P9 = CT Or U5 = 2 Then Call SETSTAMINA: Call COMCOACH: GoTo 243
    Call SETSTAMINA: Call LINEUPS: Call FOULSAFE

243 GoSub 819: Next P9

GoSub 159
Call STEALADJ

250 p = Int(Rnd(1) * 2): If p = 0 Then D = 1
If p = 1 Then D = 0
GoSub 3000
Call LASTSHOT: If T <= 0 Then 170
GoSub 1033

If displayPause = 0 And (H = 1 Or H = 3) Then PA% = D: GoTo 300
If displayPause = 0 And (H <> 1 Or H > 3) Then p = PA%: D = 1 - p: PA% = 1 - PA%: GoTo 300

If (H = 1 Or H = 3) And PB% = 0 Then U$ = B$(p) + " GETS THE JUMP": PA% = D: Call PBP
If (H = 1 Or H = 3) And PB% = 1 Then Call WINDEX: Print B$(p) + " GETS THE JUMP": PA% = D: Call DELAY

If (H <> 1 And H <> 3 Or H > 3) And PB% = 0 Then p = PA%: D = 1 - p: PA% = 1 - PA%: U$ = B$(p) + " INBOUNDS THE BALL": Call PBP
If (H <> 1 And H <> 3 Or H > 3) And PB% = 1 Then p = PA%: D = 1 - p: PA% = 1 - PA%: Call WINDEX: Print B$(p) + " INBOUNDS THE BALL": Call DELAY
GoTo 300

COMPFIVE:

RN = Int(Rnd(1) * 100) + 1
R0 = S(p, 0) - S(D, 0)
If T >= 3 And T <= 5 And X3% = 0 And R0 < 3 Then I1% = 5
If T >= 3 And T <= 5 And X3% = 1 And R0 = 3 Then I1% = 4
If T >= 3 And T <= 5 And X3% = 1 And R0 = 2 And RN <= 50 Then I1% = 5
If T >= 3 And T <= 5 And X3% = 1 And R0 = 2 And RN > 50 Then I1% = 4
If T < 3 And X3% = 0 And R0 < 3 Then I1% = 0
If T < 3 And X3% = 1 And R0 = 3 Then I1% = 1
If T < 3 And X3% = 1 And R0 = 2 And RN <= 50 Then I1% = 0
If T < 3 And X3% = 1 And R0 = 2 And RN > 50 Then I1% = 1
GoTo 281

LASTFIVE:
If U5 = 2 Or U5 = 1 And p = CT Then GoTo COMPFIVE
Call LASTFIVEMENU

280 I$ = GetKeyPress$
I1% = Val(I$): If I1% < 0 Or I1% > 5 Then 280
If I1% = 3 And T1(P9) <= 0 Then 280
If (I1% = 1 Or I1% = 4) And X3% = 0 Then 280

Color 15, 0
Call SCOREBOARD

281 B = Int(Rnd(1) * 5)
RN = Int(Rnd(1) * 100) + 1
If I1% = 0 And RN <= 50 Or I1% = 1 And RN <= 33 Then U$ = "LONG PASS DOWN COURT": Call PBP: Call DELAY: U$ = "IT'S INTERCEPTED...GAME OVER!!": Call PBP: GoTo TIMEEXP
If I1% = 2 Then I3% = Int(Rnd(1) * 45) + 45: U$ = AA$(p, B1(p, B)) + " HAS THE BALL": Call PBP: U$ = "HE PUTS IT UP FROM" + Str$(I3%) + " FEET": Call PBP: Call DELAY: If RN = 61 Then U$ = "GOOD!!": Call PBP: GoSub BASKET: GoTo TIMEEXP
If I1% = 2 Then U$ = "SHOT MISSES...GAME OVER": Call PBP: GoSub MBASKET: GoTo TIMEEXP
If I1% >= 3 And RN <= 8 Then U$ = "BALL LOST O.B.  GAME OVER": Call PBP: GoTo TIMEEXP
If I1% >= 3 And RN > 8 And RN <= 18 Then U$ = "PASS INTERCEPTED": Call PBP: GoTo TIMEEXP
If I1% = 3 And T < 1.5 Or I1% >= 4 And T < 3 Then U$ = "INBOUND PASS TO " + AA$(p, B1(p, B)): Call PBP: U$ = "TIME EXPIRES": Call PBP: GoTo TIMEEXP
If I1% = 3 Then TE = (Rnd(1) + 1.5): T = T - TE: Call PLAYINGTIME: GoSub SHTCLK: U$ = B$(p) + " GETS THE BALL TO HALF COURT": Call PBP: U$ = "THEY QUICKLY CALL FOR A TIMEOUT": Call PBP: T1(p) = T1(p) - 1: Call DEADBALLOPTIONS: Call SCOREBOARD: GoSub HALFCT
If I1% = 0 Or I1% = 4 Or (I1% = 3 And I2% = 1) Then GoTo TAKE3

RN = Int(Rnd(1) * 100) + 1
If T <= .5 Then XX% = 3
If T > .5 And T <= 1.8 Then XX% = 16
If T > 1.8 Then XX% = 29
If RN > XX% Then U$ = AA$(p, B1(p, B)) + " MISSES": Call PBP: A(p, B1(p, B), 7) = A(p, B1(p, B), 7) + 1: GoTo TIMEEXP
U$ = "BASKET BY " + AA$(p, B1(p, B)) + "!!": Call PBP: A(p, B1(p, B), 7) = A(p, B1(p, B), 7) + 1: A(p, B1(p, B), 8) = A(p, B1(p, B), 8) + 1: S(p, H) = S(p, H) + 2: S(p, 0) = S(p, 0) + 2: GoSub 1054: Call MAINSCREEN: GoTo TIMEEXP

SHTCLK:
If C7 >= 1 Then X8 = X8 - TE
If X8 < 0 Then X8 = X8 - X8: T = T - X8: TE = X8: Call PLAYINGTIME
Return

TAKE3:

RN = Int(Rnd(1) * 100) + 1
If T <= .5 Then XX% = 2
If T > .5 And T <= 1.8 Then XX% = 12
If T > 1.8 Then XX% = 20
If RN > XX% Then U$ = AA$(p, B1(p, B)) + " MISSES": Call PBP: A(p, B1(p, B), 7) = A(p, B1(p, B), 7) + 1: W%(p, B1(p, B), 1) = W%(p, B1(p, B), 1) + 1: GoTo TIMEEXP
     U$ = "BASKET BY " + AA$(P, B1(P, B)) + "!!": CALL PBP: A(P, B1(P, B), 7) = A(P, B1(P, B), 7) + 1: A(P, B1(P, B), 8) = A(P, B1(P, B), 8) + 1: S(P, H) = S(P, H) + 3: S(P, 0) = S(P, 0) + 3: W%(P, B1(P, B), 1) = W%(P, B1(P, B), 1) + 1: W%(P, B1(P, B _
), 0) = W%(P, B1(P, B), 0) + 1: GOSUB 1054: CALL MAINSCREEN: GOTO TIMEEXP

HALFCT:
Locate 8, 8
Color TR%(p, 0), TR%(p, 1)

'Print box top
Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

'Print box sides
JJ = 8
For II = 1 To JJ
    Locate 8 + II, 8
    Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
Next

'Print box bottom
Locate 9 + JJ, 8
Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

Locate 10, 15
Print B$(p); " HALF COURT OPTION"
Locate 12, 14
Locate , 14: Print Using "TIME REMAINING #.#"; T
Locate , 14: Print "0 - SHOOT A 2"
Locate , 14: Print "1 - SHOOT A 3"

290 I$ = GetKeyPress$
I2% = Val(I$): If I2% < 0 Or I2% > 1 Then 290

If I2% = 1 And X3% = 0 Then I2% = 0
Color 15, 0
Call SCOREBOARD
Return

BASKET:
S(p, H) = S(p, H) + 2: S(p, 0) = S(p, 0) + 2: GoSub 1054
A(p, B1(p, B), 8) = A(p, B1(p, B), 8) + 1: A(p, B1(p, B), 7) = A(p, B1(p, B), 7) + 1
If X3% = 0 Then Call MAINSCREEN: Return
S(p, H) = S(p, H) + 1: S(p, 0) = S(p, 0) + 1
W%(p, B1(p, B), 1) = W%(p, B1(p, B), 1) + 1
W%(p, B1(p, B), 0) = W%(p, B1(p, B), 0) + 1
Call MAINSCREEN
Return

MBASKET:
A(p, B1(p, B), 7) = A(p, B1(p, B), 7) + 1
If X3% = 0 Then Return
W%(p, B1(p, B), 1) = W%(p, B1(p, B), 1) + 1
Return

TIMEEXP:
TE = T
Call PLAYINGTIME
T = 0
Call ZEROCHECK
GoTo 170

300 If T <= 5 And T1(D) <= 0 And S(p, 0) > S(D, 0) Then TE = T: Call PLAYINGTIME: T = 0: Call ZEROCHECK: Call WINDEX: Print B$(p); " RUNS THE CLOCK OUT!": Call DELAY: Call DELAY: GoTo 170
If T <= 5 And (X3% = 1 And S(D, 0) - S(p, 0) <= 3 Or X3% = 0 And S(D, 0) - S(p, 0) <= 2) And S(D, 0) - S(p, 0) >= 0 And H >= 2 Then GoTo LASTFIVE
ST = 0: FO = 0: LS = 0: XS = 0
X8 = 45
If C7 = 2 Then X8 = 35
LS = 0
If T < 6 And LT <> 4 Then LS = 4
P6 = 0: D6 = 0: PASS% = 1: A4 = 0: A6 = 0: F = 0: If X <> 90 Then X = 0
If (D2(D) = 1 Or D2(D) = 3) And P7(D) = 0 Then X0% = 50: X1% = -1: Call PRDFAT
If D2(D) = 2 And P7(D) = 0 Then X0% = 100: X1% = 1: Call PRDFAT: Call PRPFAT
If D2(D) = 1 And P7(D) = 0 Then X0% = 25: X1% = -1: Call PRPFAT
GoTo 400

350 PASS% = PASS% + 1: P6 = 0: A4 = 0: D6 = 0: A6 = 1: F = 0: X = 0
If (D2(D) = 1 Or D2(D) = 3) And PASS% = 2 And P7(D) > 0 Then X0% = 50: X1% = -1: Call PRDFAT
If D2(D) = 2 And P7(D) > 0 And PASS% = 2 Then X0% = 100: X1% = 1: Call PRDFAT: Call PRPFAT
If D2(D) = 1 And P7(D) > 0 And PASS% = 2 Then X0% = 25: X1% = -1: Call PRPFAT
GoTo 400

359 J8% = 0: If X3% = 0 Then P3 = 1: P6 = 1: GoTo 537
S = Int(Rnd(1) * 100) + 1: W%(p, B1(p, B), 1) = W%(p, B1(p, B), 1) + 1: A(p, B1(p, B), 7) = A(p, B1(p, B), 7) + 1
PT% = 3: Call MAKEPBP: Call PBP
GoTo 9030

361 If PB% = 1 And displayPause <> 0 Then Call WINDEX: Print "3 PT FG MISSED BY "; AA$(p, B1(p, B)): Call DELAY
If PB% = 0 Then PT% = 5: TP% = 1: Call MAKEPBP: Call PBP
GoSub 1033: T = T - 4: TE = 4: Call PLAYINGTIME: GoSub SHTCLK
If T <= 0 Then TE = T: Call PLAYINGTIME: TP% = 0: GoTo 170
Call MAINSCREEN: GoTo 670

363 If PB% = 1 And displayPause <> 0 Then Call WINDEX: Print "3 PT FG GOOD BY "; AA$(p, B1(p, B)); " !!!": Call DELAY
If PB% = 0 Then PT% = 4: Call MAKEPBP: Call PBP

369 If XX% = 1 Then XX% = 0: S(p, H) = S(p, H) + 2: S(p, 0) = S(p, 0) + 2: W%(p, B1(p, B), 1) = W%(p, B1(p, B), 1) - 1: GoTo 364
S(p, H) = S(p, H) + 3: S(p, 0) = S(p, 0) + 3: W%(p, B1(p, B), 0) = W%(p, B1(p, B), 0) + 1

364 A(p, B1(p, B), 8) = A(p, B1(p, B), 8) + 1
GoSub 1054: Call MAINSCREEN: GoTo 562

400 A4 = 0: D6 = 0: P3 = 0: B = Int(Rnd(1) * 5)
Call BALLDISTRIBUTE
If P7(D) > 0 And PASS% = 1 Then 428
R0 = Int(Rnd(1) * 100) + 1
GoTo 461

448 T = T - 4: TE = 4: Call PLAYINGTIME: GoSub SHTCLK: GoSub WHOLAY
If PASS% <> 1 Then U$ = AA$(p, B1(p, B)) + " HAS THE BALL"
If PASS% = 1 Then U$ = AA$(p, B1(p, B)) + " GETS THE INBOUND PASS"
Call PBP
U$ = "HE IS STREAKING DOWN THE FLOOR!"
Call PBP
GoTo LAYUP

DENIAL:
If Int(Rnd(1) * 100) + 1 <= 10 Then 448

RN = Int(Rnd(1) * 100) + 1
R0 = Int(Rnd(1) * 100) + 1
IN = 99
C1 = 2: P9 = D
U$ = "DESIGNATED FOULER (0-4)"
Call SELECTPBP
If U5 = 1 And D = CT Or U5 = 2 Then DF% = Int(Rnd(1) * 5): GoTo 431

430 I$ = GetKeyPress$
DF% = Val(I$): If DF% < 0 Or DF% > 4 Then 430
If PASS% > 1 Then 434

431 If RN <= 4 And T1(p) > 0 Then U$ = B$(p) + " CAN'T INBOUND THE BALL": Call SELECTPBP: U$ = B$(p) + " CALLS A TIME OUT": GoSub 729: PASS% = 0: GoTo 640
If RN <= 4 And T1(p) = 0 Or RN > 4 And RN <= 7 Then U$ = B$(p) + " CAN'T INBOUND THE BALL": Call SELECTPBP: U$ = "5 SECOND VIOLATION...TURNOVER": SV% = 1: GoTo 517
CY = 7 + DP(0, 4)
If RN <= CY Then GoSub ADJ: GoTo 515
CY = CY + F5%(p, 4) + DP(1, 4)
If RN <= CY Then GoSub ADJ: GoTo 435

432 U$ = "PASS TO " + AA$(p, B1(p, B)): Call SELECTPBP

434 If Int(Rnd(1) * 100) + 1 <= 5 Then IN% = 1

RN = Int(Rnd(1) * 100) + 1
If RN <= 80 Then TE = Int(Rnd(1) * 3) + 1: T = T - TE: Call PLAYINGTIME: GoSub SHTCLK
If RN > 80 And RN <= 90 Then TE = Int(Rnd(1) * 3) + 8: T = T - TE: Call PLAYINGTIME: GoSub SHTCLK
If RN > 90 Then TE = Int(Rnd(1) * 3) + 5: T = T - TE: Call PLAYINGTIME: GoSub SHTCLK

RN = Int(Rnd(1) * 100) + 1
If RN <= 80 Then W5 = DF%: GoTo 439
If RN > 80 Then GoSub 630: GoTo 566

439 GoSub 631: GoTo 566

428 '*** FULL COURT PRESS ***
If P7(D) = 1 Then X0% = 50: X1% = -1: Call PRDFAT
If P7(D) = 2 Or P7(D) = 4 Then X0% = 50: X1% = -1: Call PRDFAT
If P7(D) = 3 Then X0% = 25: X1% = -1: Call PRDFAT
If H >= 2 And T <= 180 And P7(D) = 5 Then GoTo DENIAL
If P7(D) = 1 Then X0% = 25: X1% = -1: FA%(p, B1(p, BU%)) = FA%(p, B1(p, BU%)) - 1: Call PRPFAT
If P7(D) > 1 Then X0% = 25: X1% = -1: Call PRPFAT
R0 = Int(Rnd(1) * 100) + 1
If P7(D) = 1 Or P7(D) = 3 Then T = T - 2: TE = 2
If P7(D) = 2 And R0 <= 75 Then T = T - 2: TE = 2
If P7(D) = 2 And R0 > 75 Then T = T + 2: TE = -2
If P7(D) = 4 And R0 <= 50 Then T = T - 2: TE = 2
If P7(D) = 4 And R0 > 50 Then T = T + 2: TE = -2
Call PLAYINGTIME: GoSub SHTCLK
R0 = Int(Rnd(1) * 100) + 1
If R0 <= DP(2, P7(D)) Then 512
If R0 <= DP(2, P7(D)) + 1 Then 511
CY = DP(2, P7(D)) + 1 + F5%(p, 5) + F5%(D, 6) + DP(0, P7(D)) - PR%(D, 0)
If R0 <= CY Then 515
CY = CY + F5%(p, 4) + DP(1, P7(D)) - PR%(D, 1)
PC%(D) = PC%(D) + 1
If PC%(D) > 3 Then PR%(D, 0) = 0: PR%(D, 1) = 0
If R0 <= CY Then 435
GoTo 465

ADJ:
If R0 <= 80 Then T = T + 4: TE = -4: Call PLAYINGTIME: GoSub SHTCLK
If R0 > 80 And R0 <= 90 Then T = T - 3: TE = 3: Call PLAYINGTIME: GoSub SHTCLK
Return

435 P9 = Int(Rnd(1) * 5)
If Int(Rnd(1) * 5) + 1 <= A(D, B1(D, P9), 5) Then A(D, B1(D, P9), 18) = A(D, B1(D, P9), 18) + 1: GoTo 513
R0 = Int(Rnd(1) * 100) + 1
If P7(D) = 1 And PASS% = 1 And R0 <= A(p, B1(p, BU%), 4) Then GoTo LAYUP
If P7(D) = 2 And PASS% = 1 And R0 <= A(p, B1(p, BU%), 4) * 2 Then GoTo LAYUP
If P7(D) = 3 And PASS% = 1 And R0 <= 7 Then GoTo LAYUP
If P7(D) = 4 And PASS% = 1 And R0 <= 15 Then GoTo LAYUP
If P7(D) = 1 And PASS% = 1 And R0 <= A(p, B1(p, BU%), 4) * 2 Then GoTo GDSHOT
If P7(D) = 2 And PASS% = 1 And R0 <= A(p, B1(p, BU%), 4) * 4 Then GoTo GDSHOT
If P7(D) = 3 And PASS% = 1 And R0 <= 14 Then GoTo GDSHOT
If P7(D) = 4 And PASS% = 1 And R0 <= 30 Then GoTo GDSHOT
If P7(D) = 5 Then 432

437 P6 = 0: R0 = Int(Rnd(1) * 3) + 1: J8% = 0: F3S% = Int(Rnd(1) * 7) + 3
If A(p, B1(p, B), 0) > 60 And R0 = 1 Then F = 24
If A(p, B1(p, B), 0) > 60 And R0 = 2 Then F = 30
If A(p, B1(p, B), 0) > 60 And R0 = 3 Then F = 36
If A(p, B1(p, B), 0) <= 60 And R0 = 1 Then F = Int(A(p, B1(p, B), 0) * .4 + .5)
If A(p, B1(p, B), 0) <= 60 And R0 = 2 Then F = Int(A(p, B1(p, B), 0) * .5 + .5)
If A(p, B1(p, B), 0) <= 60 And R0 = 3 Then F = Int(A(p, B1(p, B), 0) * .6 + .5)
PZ% = 1
If Int(Rnd(1) * 150) + 1 = 40 Then GoTo TIEUP

444 X = 0
If PZ% = 2 And Int(Rnd(1) * 100) + 1 <= 50 Then PZ% = 3: P6 = 1
GoTo 520

445 If OF%(p) = 4 Then 437
If H <= 2 Then 450

446 P6 = 1
Call SHOTPERCENT

447 PZ% = Int(Rnd(1) * 3) + 5
GoTo 444

450 If H = 2 And T <= 120 Or H > 2 Then 446
If A(p, B1(p, B), 3) - A(p, B1(p, B), 7) - A(p, B1(p, B), 11) - A(p, B1(p, B), 12) <= 0 Then 437
GoTo 446

453 A4 = 0: If H = 2 And T <= 120 Or H > 2 Then 456
If H > 2 Then 456
If A(p, B1(p, B), 3) - A(p, B1(p, B), 7) - A(p, B1(p, B), 11) - A(p, B1(p, B), 12) <= 0 Then 437

456 Call SHOTPERCENT: A4 = Int(Rnd(1) * 28) + 1
R0 = Int(Rnd(1) * 9) + 1
I2 = Int(Rnd(1) * 5)
If FA%(p, B1(p, B)) <= -5 Then 460
If R0 <= A(p, B1(p, I2), 4) Then F = A(p, B1(p, B), 0) + D1(D) + A4 + TF%(p) + TF%(D): TF%(p) = 0: TF%(D) = 0: PZ% = 2: P6 = 1: GoTo 444

460 GoTo 445

461 If (OF%(p) = 8 Or OF%(p) = 7) And D2(D) = 2 Then 465
If R0 <= D8(4, D2(D)) Then 512
If R0 <= D8(4, D2(D)) + 1 Then 511
OY% = 0
If OF%(p) = 4 Or OF%(p) = 7 Then OY% = Int((D8(2, D2(D)) / 2) + .5) * -1
CY = D8(4, D2(D)) + 1 + F5%(p, 5) + F5%(D, 6) + D8(2, D2(D)) + OY% + 1
If R0 <= CY Then 515
CY = CY + F5%(p, 4) + D8(3, D2(D))
OY% = 0
If OF%(p) = 4 Or OF%(p) = 7 Then OY% = Int((D8(3, D2(D)) / 2) + .5) * -1
If OF%(p) = 1 Then OY% = 7
If OF%(p) = 2 Then OY% = -7
If R0 <= CY + OY% Then 435

465 R0 = Int(Rnd(1) * 100) + 1
If C7 = 2 Then GS% = 39 + Int(((FY%(2) - 58) * .8) + .5)
If C7 = 1 Then GS% = 39 + Int(((FY%(2) - 52) * .8) + .5)
If C7 = 0 Then GS% = 39 + Int(((FY%(2) - 52) * .8) + .5)

'REM THE NO SHOT CLOCK CASE WAS PREVIOUSLY - 40

If D2(D) = 1 Or D2(D) = 2 Then GS% = GS% + 7
If D2(D) = 3 Then GS% = GS% - 7
If OF%(p) = 3 Then GS% = GS% + 21
If OF%(p) = 4 Then GS% = GS% - 21
If OF%(p) = 7 Or OF%(p) = 8 Then 437
If OF%(p) = 5 And X8 > 15 And C7 >= 1 Then 437
If R0 > GS% Then 437
R0 = Int(Rnd(1) * 100)
If R0 <= D8(1, D2(D)) Then 453
GoTo 445

511 X = 0: GoTo 580

512 Call LASTSHOT: GoSub 1033
Call MAINSCREEN: X = 88: GoTo 565

513 If P7(D) > 0 And PASS% = 1 Then 508
Call LASTSHOT

508 GoSub 1033: A(p, B1(p, B), 23) = A(p, B1(p, B), 23) + 1
If displayPause = 0 Then 505
U$ = "BALL STOLEN BY " + AA$(D, B1(D, P9)) + "!"
If PB% = 1 Then Call WINDEX: Print U$: Call DELAY: GoTo 505

RN = Int(Rnd(1) * 100) + 1
If RN <= 3 Then U$ = AA$(D, B1(D, P9)) + " QUICKLY REACHES IN!": Call PBP
If RN > 3 And RN <= 6 Then U$ = AA$(D, B1(D, P9)) + " SNEAKS IN FROM BEHIND!": Call PBP
If RN > 6 And RN <= 10 Then U$ = AA$(D, B1(D, P9)) + " GETS A HAND ON THE BALL!": Call PBP

RN = Int(Rnd(1) * 20) + 1

If RN = 1 Then U$ = "PASS INTERCEPTED BY " + AA$(D, B1(D, P9)) + "!"
If RN = 2 Then U$ = "PASS IS PICKED OFF BY " + AA$(D, B1(D, P9)) + "!"
If RN = 3 Then U$ = "BALL IS STOLEN BY " + AA$(D, B1(D, P9)) + "!"
If RN = 4 Then U$ = AA$(D, B1(D, P9)) + " WITH THE STEAL!"
If RN = 5 Then U$ = AA$(D, B1(D, P9)) + " TAKES IT AWAY!"
If RN = 6 Then U$ = AA$(D, B1(D, P9)) + " STEALS IT!"
If RN = 7 Then U$ = AA$(D, B1(D, P9)) + " RIPS IT LOOSE!"
If RN = 8 Then U$ = AA$(D, B1(D, P9)) + " TAKES THE BALL AWAY!"
If RN = 9 Then U$ = AA$(D, B1(D, P9)) + " STEALS THE BALL!"
If RN = 10 Then U$ = AA$(D, B1(D, P9)) + " PICKS IT OFF!"
If RN = 11 Then U$ = AA$(D, B1(D, P9)) + " SWIPES THE BALL!"
If RN = 12 Then U$ = AA$(D, B1(D, P9)) + " WITH THE STOLEN BALL!"
If RN = 13 Then U$ = AA$(D, B1(D, P9)) + " STRIPS IT AWAY!"
If RN = 14 Then U$ = AA$(D, B1(D, P9)) + " WITH THE STEAL!"
If RN = 15 Then U$ = AA$(D, B1(D, P9)) + " SWIPES AND STEALS IT!"
If RN = 16 Then U$ = AA$(D, B1(D, P9)) + " SNATCHES IT AWAY!"
If RN = 17 Then U$ = AA$(D, B1(D, P9)) + " QUICKLY REACHES IN!"
If RN = 18 Then U$ = AA$(D, B1(D, P9)) + " TAKES IT THE OTHER WAY!"
If RN = 19 Then U$ = AA$(D, B1(D, P9)) + " POKES IT LOOSE!"
If RN = 20 Then U$ = AA$(D, B1(D, P9)) + " STRIPS IT CLEAN!"

Call PBP

505 A(D, B1(D, P9), 13) = A(D, B1(D, P9), 13) + 1: T0(p) = T0(p) + 1
Call MAINSCREEN
C1 = 2: P9 = D: If T <= 0 Then 170
GoSub 3000: FB% = 1

RN = Int(Rnd(1) * 100) + 1
If RN <= 38 Then B = 0
If RN > 38 And RN <= 75 Then B = 1
If RN > 75 And RN <= 83 Then B = 2
If RN > 83 And RN <= 91 Then B = 3
If RN > 91 Then B = 4

RN = Int(Rnd(1) * 100) + 1
If PASS% > 1 And (D2(P9) = 0 Or D2(P9) = 1 Or D2(P9) = 3) And RN <= 15 Then GoTo LAYUP
If PASS% > 1 And D2(P9) = 2 And RN <= 5 Then GoTo LAYUP
If PASS% = 1 And P7(p) > 0 And RN <= 33 Then GoTo LAYUP
If PASS% = 1 And P7(p) > 0 And P7(p) <> 3 And RN <= 40 Then GoTo LAYUP
GoTo 300

515 Call LASTSHOT

517 GoSub 1033
R0 = Int(Rnd(1) * 25) + 1
If R0 = 13 Then GoTo TIEUP
A(p, B1(p, B), 23) = A(p, B1(p, B), 23) + 1: T0(p) = T0(p) + 1
If SV% <> 1 Then PT% = 1: Call MAKEPBP
SV% = 0
If PB% = 1 And displayPause <> 0 Then Call WINDEX: Print "TURNOVER BY "; B$(p): Call DELAY
If PB% = 0 Then Call PBP
U$ = "BALL OUT OF BOUNDS TO " + B$(D)
GoSub 729
Call DELAY
Call MAINSCREEN: Locate 10, 1: If T <= 0 Then 170
GoSub 3000
Call DEADBALLOPTIONS: GoSub BADPR: GoTo 300
LAYUP:
FB% = 1: F = 98: P6 = 1: GoTo 520
GDSHOT:
FX% = A(p, B1(p, B), 0)
If FA%(p, B1(p, B)) <= -5 Then FX% = FX% - 10
F = FX% + Int(Rnd(20)) + 11 + TF%(D) + TF%(p): TF%(p) = 0: TF%(D) = 0
PZ% = 10
GoTo 447

GDSHOTDES:
If displayPause = 0 Then 529

RN = Int(Rnd(1) * 100) + 1
If RN <= 25 Then U$ = AA$(p, B1(p, B)) + " BREAKS IN TO FRONT COURT"
If RN > 25 And RN <= 50 Then U$ = AA$(p, B1(p, B)) + " GETS THE PASS IN FRONT COURT"
If RN > 50 And RN <= 75 Then U$ = AA$(p, B1(p, B)) + " BREAKS THE PRESS"
If RN > 75 Then U$ = AA$(p, B1(p, B)) + " DEFEATS THE PRESS"
Call PBP

RN = Int(Rnd(1) * 100) + 1
If RN <= 33 Then U$ = "HE HAS AN OPEN JUMPER AT THE FT LINE"
If RN > 33 And RN <= 66 Then U$ = "HE'S OPEN ON THE BASELINE"
If RN > 66 Then U$ = "HE'S OPEN FROM THE WING"
Call SHOTSTRING
Call PBP
GoTo 529

520 If J8% = 1 Then PZ% = 4
Call LASTSHOT: If T <= 0 Then 170
If PZ% = 4 And F3S% <= 0 Then F3S% = 3
If displayPause = 0 Then 529
If PZ% = 10 Then GoTo GDSHOTDES
If F > 70 And FB% = 1 And PASS% = 1 Then PT% = 6: PZ% = 8: Call MAKEPBP: Call PBP: GoTo 529
If AA$(p, B1(p, B)) <> PB$ And PB$ = "" Then U$ = "PASS TO " + AA$(p, B1(p, B)): Call PBP
If AA$(p, B1(p, B)) <> PB$ And PB$ <> "" Then PT% = 10: Call MAKEPBP: Call PBP
If AA$(p, B1(p, B)) = PB$ Then PT% = 9: Call MAKEPBP: Call PBP
If X8 <= 0 Then U$ = "SHOT CLOCK HAS EXPIRED!": SV% = 1: GoTo 515
PB$ = AA$(p, B1(p, B))
YY$ = "HE"
On PZ% GOTO 521, 522, 523, 524, 525, 526, 527

521 If displayPause = 0 Then 601

RN = Int(Rnd(1) * 12) + 1
If RN = 1 Then U$ = YY$ + "'S HEAVILY GUARDED"
If RN = 2 Then U$ = YY$ + "'S GUARDED"
If RN = 3 Then U$ = YY$ + "'S GUARDED BY " + AA$(D, B1(D, B))
If RN = 4 Then U$ = YY$ + " HAS A MAN ON HIM"
If RN = 5 Then U$ = YY$ + " HAS A DEFENDER ON HIM"
If RN = 6 Then U$ = YY$ + "'S COVERED"
If RN = 7 Then U$ = YY$ + " CAN'T GET AROUND THE DEFENDER"
If RN = 8 Then U$ = YY$ + "'S PRESSURED"
If RN = 9 Then U$ = YY$ + " HAS NOWHERE TO GO"
If RN = 10 Then U$ = YY$ + " HAS A HAND IN HIS FACE"
If RN = 11 Then U$ = YY$ + "'S WATCHED BY " + AA$(D, B1(D, B))
If RN = 12 Then U$ = YY$ + "'S WORKING ON " + AA$(D, B1(D, B))

601 Call PBP: GoTo 529

522 U$ = YY$ + " WORKS FREE INSIDE": Call PBP: U$ = "HE HAS AN OPEN SHOT": Call SHOTSTRING: Call PBP: P6 = 1: GoTo 529

523 If displayPause = 0 Then 529
P6 = 1

RN = Int(Rnd(1) * 100) + 1
If B = 4 And RN <= 25 Then U$ = YY$ + " MAKES A MOVE DOWN LOW": Call PBP: GoTo 602
If B = 4 And RN <= 50 Then U$ = YY$ + " POSITIONS HIMSELF DOWN LOW": Call PBP: GoTo 602
If B = 4 And RN <= 75 Then U$ = YY$ + " POSTS UP UNDERNEATH": Call PBP: GoTo 602
If B = 4 Then U$ = YY$ + " BACKS IN AGAINST " + AA$(D, B1(D, 4)): Call PBP

602 If B = 4 Then U$ = "HE HAS A GOOD SHOT INSIDE": Call SHOTSTRING: Call PBP: GoTo 529
If (B = 3 Or B = 2) And RN <= 17 Then U$ = YY$ + " DRIVES THE BASELINE": Call PBP: GoTo 603
If (B = 3 Or B = 2) And RN <= 34 Then U$ = YY$ + " PENETRATES THE LANE": Call PBP: GoTo 603
If (B = 3 Or B = 2) And RN <= 50 Then U$ = YY$ + " MOVES INSIDE": Call PBP: GoTo 603
If (B = 3 Or B = 2) And RN <= 67 Then U$ = YY$ + " DRIVES THE LANE": Call PBP: GoTo 603
If (B = 3 Or B = 2) And RN <= 84 Then U$ = YY$ + " MOVES INTO THE PAINT": Call PBP: GoTo 603
If (B = 3 Or B = 2) Then U$ = YY$ + " DRIVES UNDERNEATH": Call PBP

603 If (B = 3 Or B = 2) Then U$ = "HE'S GOT A GOOD SHOT UNDERNEATH": Call SHOTSTRING: Call PBP: GoTo 529
If (B = 0 Or B = 1) And RN <= 25 Then U$ = YY$ + " DRIVES THE LANE": Call PBP: GoTo 604
If (B = 0 Or B = 1) And RN <= 50 Then U$ = YY$ + " DRIVES THROUGH THE KEY": Call PBP: U$ = "HE'S INTO THE LANE": Call PBP: GoTo 604
If (B = 0 Or B = 1) And RN <= 75 Then U$ = YY$ + " DRIVES TO THE BASKET": Call PBP: GoTo 604
U$ = YY$ + " DRIVES THROUGH AN OPENING": Call PBP

604 U$ = "HE HAS A GOOD SHOT CLOSE IN": Call SHOTSTRING: Call PBP
GoTo 529

524 U$ = "HE'S OPEN FROM 3-POINT RANGE!"
U$ = U$ + Str$(F3S%) + "*"
Call PBP: P6 = 1: GoTo 529

525 U$ = "HE'S OPEN FROM MEDIUM RANGE"
Call SHOTSTRING
Call PBP: P6 = 1: GoTo 529

526 U$ = "HE'S OPEN FROM MEDIUM RANGE"
Call SHOTSTRING
Call PBP: P6 = 1: GoTo 529

527 U$ = "HE'S OPEN FROM MEDIUM RANGE"
Call SHOTSTRING
Call PBP: P6 = 1: GoTo 529

529 If F <= 0 Then F = 1
If F >= 100 Then F = 99
If displayPause = 0 Then 607
If PB% = 1 Then Call WINDEX: Print "PASS TO "; AA$(p, B1(p, B)); " OF "; B$(p)
If PB% = 1 And PZ% <> 4 Then Locate 10: Print "SHOT % "; F;
If PB% = 1 And PZ% = 4 Then Locate 10: Print "3 PT SHOT % "; F3S%;: Print " *";: GoTo 528
If PB% = 1 And PZ% >= 2 And PZ% <= 8 Then Print "*";

607 If displayPause = 0 Or PB% = 1 Then 528
If PZ% <> 4 Then Locate 8, 41: Print Using "SHOT % ###   "; F;
If PZ% = 4 Then Locate 8, 41: Print Using "SHOT % ###"; F3S%;: Print " *";: GoTo 528
If PZ% >= 2 And PZ% <= 8 Then Locate , 52: Print "*";

528 Print: If PB% = 1 And displayPause <> 0 Then Call DELAY
If U5 <> 0 Or CM% = 1 Then 949

530 I$ = GetKeyPress$
If UCase$(I$) = "T" Then Call TICKER: Call SCOREBOARD: GoTo 529
If UCase$(I$) = "D" Then Call ZONECHANGE: Call SCOREBOARD: GoTo 529
P6 = Val(I$): If P6 < 1 Or P6 > 4 Then P6 = 0
If P6 = 4 Then FT% = 1

532 If Val(I$) = 3 And (U5 = 0 Or U5 = 1 And p = CT) And H >= 4 And T < 240 Then P6 = 3
If Val(I$) = 2 And (U5 = 0 Or U5 = 1 And D = CT) And T1(p) > 0 Then P6 = 2
P3 = P6: If P6 = 2 And T1(p) = 0 Then 530
If P6 = 3 And H = 1 Or P6 = 3 And U5 = 1 And D = CT Then 530
If P6 = 3 And H = 2 And T > 240 Then 530
'
If U5 = 1 And p = CT And J8% = 1 Or U5 = 2 And J8% = 1 Then P6 = 4
If U5 = 1 And p = CT And H >= 2 And T < 30 And S(D, 0) - S(p, 0) = 3 Or U5 = 2 And H >= 2 And T < 30 And S(D, 0) - S(p, 0) = 3 Then P6 = 4
If (P6 = 1 Or P6 = 4) And PZ% = 4 Then P6 = 4
On P6 + 1 GOTO 350, 537, 640, 584, 359

537 ' *** SHOOT !!! ***
PT% = 3
Call MAKEPBP
Call PBP
S = 0: SS = 0: R0 = Int(Rnd(1) * 100) + 1: A(p, B1(p, B), 7) = A(p, B1(p, B), 7) + 1
GG = S(D, 0) - S(p, 0): If H > 1 And T <= 30 And GG = 2 Or H > 1 And T <= 30 And GG = 0 Or H > 1 And T <= 30 And GG = -2 Then 540
If R0 <= A(p, B1(p, B), 16) + F5%(D, 7) Then A(p, B1(p, B), 7) = A(p, B1(p, B), 7) - 1: GoSub 1033: LT = 0: GoTo 589

540 S = Int(Rnd(1) * 100) + 1
R0 = F * ((DY / 100) + (.03 * X3%))
If H > 1 And T < 30 And GG = 2 Or H > 1 And T < 30 And GG = 0 Or H > 1 And T < 30 And GG = -2 Then 542
If S <= R0 Then GoSub 1033: LT = 0: GoTo 610

542 If S <= F Then LT = 0: GoTo 553
R0 = Int(Rnd(1) * 100) + 1
LT = 0: GoTo 548

545 If PB% = 1 And displayPause <> 0 Then Call WINDEX: Print "SHOT MISSED BY "; AA$(p, B1(p, B)): Call DELAY
If PB% = 0 Then PT% = 8: Call MAKEPBP: Call PBP

546 GoSub 1033: T = T - 4: TE = 4
Call PLAYINGTIME: GoSub SHTCLK
If T <= 0 Then TE = T: Call PLAYINGTIME: GoTo 170
Call MAINSCREEN: LT = 0: GoTo 670

548 I2 = Int(Rnd(1) * 5) + 1
I3 = Int(Rnd(1) * 5)
If I2 > A(D, B1(D, I3), 6) Then 545
If displayPause = 0 Then 554
If PB% = 1 Then Call WINDEX: Print "SHOT BLOCKED BY "; AA$(D, B1(D, I3)): Call DELAY: GoTo 554

RN = Int(Rnd(1) * 100) + 1
If RN <= 10 Then U$ = AA$(D, B1(D, I3)) + " STUFFS THE SHOT!!": GoTo 549
If RN <= 15 Then U$ = "DENIED BY " + AA$(D, B1(D, I3)) + "!!": GoTo 549
If RN <= 20 Then U$ = "ERASED BY " + AA$(D, B1(D, I3)) + "!!": GoTo 549
If RN <= 25 Then U$ = "EXPUNGED BY " + AA$(D, B1(D, I3)) + "!!": GoTo 549
If RN <= 30 Then U$ = "REBUKED BY " + AA$(D, B1(D, I3)) + "!!": GoTo 549
If RN <= 35 Then U$ = "SHOT BLOCKED CLEANLY BY " + AA$(D, B1(D, I3)) + "!!": GoTo 549
If RN <= 70 Then U$ = "SHOT BLOCKED BY " + AA$(D, B1(D, I3)) + "!!": GoTo 549
If RN <= 90 Then U$ = "SHOT REJECTED BY " + AA$(D, B1(D, I3)) + "!!": GoTo 549
U$ = "SHOT SWATTED OUT BY " + AA$(D, B1(D, I3)) + "!!": GoTo 549

549 Call PBP

554 A(D, B1(D, I3), 15) = A(D, B1(D, I3), 15) + 1: X = 98: GoTo 546

553 If DK% = 1 And PB% = 0 Then PT% = 2: Call MAKEPBP: U$ = "STUFF BY ": DK% = 0: GoTo 559
If DK% = 1 And PB% = 1 Then U$ = "STUFF BY ": DK% = 0: GoTo 559
If PB% = 0 And PZ% = 8 Then PT% = 7: Call MAKEPBP: Call PBP: PZ% = 0: GoTo 560
U$ = "BASKET BY "

559 U$ = U$ + AA$(p, B1(p, B)) + " !!!"
'--- Why would we not call SELECTPBP here???
'essentially if PB% = 0 then we're not printing it!
If PB% = 1 And displayPause <> 0 Then Call WINDEX: Print U$: Call DELAY: GoTo 560

RN = Int(Rnd(1) * 100) + 1
If RN <= 50 Then 561

RN = Int(Rnd(1) * 22) + 1
If RN = 1 Then U$ = "NOTHING BUT THE BOTTOM OF THE NET!"
If RN = 2 Then U$ = "COUNT IT!"
If RN = 3 Then U$ = AA$(p, B1(p, B)) + " TICKLES THE TWINE!"
If RN = 4 Then U$ = "NO RIM, NO BOARD, JUST CORD!"
If RN = 5 Then U$ = "GOOD!"
If RN = 6 Then U$ = "YES!"
If RN = 7 Then U$ = "COUNT IT!"
If RN = 8 Then U$ = "IT'S GOOD!"
If RN = 9 Then U$ = "BINGO!"
If RN = 10 Then U$ = "IT FALLS!"
If RN = 11 Then U$ = "HE DRAINS IT!"
If RN = 12 Then U$ = "SWISH!"
If RN = 13 Then U$ = "ALL NET!"
If RN = 14 Then U$ = "IT'S THERE!"
If RN = 15 Then U$ = "HE BURIES IT!"
If RN = 16 Then U$ = "IT GOES IN!"
If RN = 17 Then U$ = "RING IT UP!"
If RN = 18 Then U$ = "IT GOES DOWN!"
If RN = 19 Then U$ = "BULLSEYE!"
If RN = 20 Then U$ = "HE SCORES!"
If RN = 21 Then U$ = "HE HAS IT!"
If RN = 22 Then U$ = "HE CONNECTS!"

561 If PB% = 0 Then Call PBP

560 S(p, H) = S(p, H) + 2
S(p, 0) = S(p, 0) + 2
GoSub 1054
Call MAINSCREEN
A(p, B1(p, B), 8) = A(p, B1(p, B), 8) + 1

562 U$ = B$(D) + " BALL O.B. AFTER SCORE"
If PB% = 1 And displayPause <> 0 Then Locate 9, 1: Print U$
If PB% = 0 Then GoSub 871: Call PBP
Call DELAY: GoSub 1033: T = T - 4: TE = 4: Call PLAYINGTIME: GoSub SHTCLK
If T <= 0 Then TE = T: Call PLAYINGTIME: GoTo 170
Call MAINSCREEN: GoSub 3000: Call STOPPAGE: Call PRESSGUARD: GoTo 300

565 GoSub 630: LT = 4

566 GoTo 9065

567 If T <= 0 Then 170
Call DEADBALLOPTIONS: GoSub BADPR
If N7% = 0 And PB% = 0 Or N7% = 0 And displayPause = 0 Then Call WINDEX: U$ = B$(p) + " BALL O.B. AFTER FOUL": Call PBP: GoSub 871: GoTo 350
If N7% = 0 And PB% = 1 Then Call WINDEX: Print B$(p) + " BALL O.B. AFTER FOUL": Call DELAY: GoSub 871: GoTo 350
GoTo 8000

570 If displayPause = 0 Then 571
If PB% = 0 Then U$ = "1+1 FT: " + AA$(p, B1(p, B)) + Str$(A(p, B1(p, B), 1)) + " %": Call PBP: GoTo 571
                  
Call WINDEX: Print B$(p)
Print "1+1 FT: "; AA$(p, B1(p, B)); " "; A(p, B1(p, B), 1); " %"
          

571 X = 50: Call DEADBALLOPTIONS: GoSub BADPR: X = 0
If displayPause = 0 Then 572
If PB% = 0 Then U$ = "1+1 FT: " + AA$(p, B1(p, B)) + Str$(A(p, B1(p, B), 1)) + " %": Call PBP: GoTo 572

Call WINDEX: Print B$(p): Print "1+1 FT: "; AA$(p, B1(p, B)); "   "; A(p, B1(p, B), 1); " %"

572 For F3 = 1 To 2
    A(p, B1(p, B), 9) = A(p, B1(p, B), 9) + 1
    S = Int(Rnd(1) * 100) + 1
    FX% = A(p, B1(p, B), 1)
    If FA%(p, B1(p, B)) <= -5 Then FX% = Int(FX% * .9 + .5)
    If S <= FX% And PB% = 0 Then Call THREEFT
    If S <= FX% And PB% = 1 Then Call MADEFT
    If S > FX% And PB% = 0 Then Call BADFT
    If S > FX% And PB% = 1 Then Call MISSFT
    If F3 = 1 And S > A(p, B1(p, B), 1) Then 575
Next F3

575 If T <= 0 Then 170
If S > FX% Then 650
GoSub 3000: X = 0
U$ = B$(p) + " BALL O.B. AFTER SCORE"
Call SELECTPBP
GoSub 871
Call DELAY
Call STOPPAGE
Call PRESSGUARD: GoTo 300

580 Call LASTSHOT: GoSub 1033: GoSub 3000: GoSub 630: B = W5: If P2(D) > 6 Then 570
If T <= 0 Then 170
U$ = "OFFENSIVE FOUL"
Call SELECTPBP

U$ = B$(p) + " BALL O.B. AFTER FOUL"
GoSub 729
GoSub 871: B = 6
Call DELAY
T0(D) = T0(D) + 1
Call MAINSCREEN
Call DEADBALLOPTIONS: GoSub BADPR
Call PRESSGUARD: GoTo 300

584 IN = 99
Call LASTSHOT: If T <= 0 Then 170
C1 = 2: P9 = D
U$ = "DESIGNATED FOULER (0-4)"
Call SELECTPBP
If U5 = 1 And D = CT Or U5 = 2 Then W5 = Int(Rnd(1) * 5): GoTo 588

586 I$ = GetKeyPress$
W5 = Val(I$): If W5 < 0 Or W5 > 4 Then 586

588 GoSub 631: GoTo 566

589 T = T - 4: TE = 4
Call PLAYINGTIME: GoSub SHTCLK
Call MAINSCREEN: GoSub 630
If PB% = 0 Then
    U$ = "2 FT: " + AA$(p, B1(p, B)) + Str$(A(p, B1(p, B), 1)) + " %"
    Call PBP
    Call DELAY
    GoTo 591
End If
Call WINDEX
Print B$(p)
Print "2 FT: "; AA$(p, B1(p, B)); "   "; A(p, B1(p, B), 1); " %"

591 X = 50: Call DEADBALLOPTIONS: GoSub BADPR: X = 0: A(p, B1(p, B), 9) = A(p, B1(p, B), 9) + 2
If displayPause = 0 Then 592
If PB% = 0 Then U$ = "2 FT: " + AA$(p, B1(p, B)) + Str$(A(p, B1(p, B), 1)) + " %": Call PBP: GoTo 592
Call WINDEX: Print B$(p)
Print "2 FT: "; AA$(p, B1(p, B)); "   "; A(p, B1(p, B), 1); " %"

592 For F3 = 1 To 2: S = Int(Rnd(1) * 100) + 1
    FX% = A(p, B1(p, B), 1)
    If FA%(p, B1(p, B)) <= -5 Then FX% = Int(FX% * .9 + .5)
    If S <= FX% And PB% = 0 Then Call THREEFT
    If S <= FX% And PB% = 1 Then Call MADEFT
    If S > FX% And PB% = 0 Then Call BADFT
    If S > FX% And PB% = 1 Then Call MISSFT
Next F3: LT = 0
If T <= 0 Then TE = T: Call PLAYINGTIME: GoTo 170
If S > FX% Then 650

598 GoSub 3000

U$ = B$(p) + " BALL O.B. AFTER SCORE"
Call SELECTPBP
GoSub 871
Call DELAY
Call STOPPAGE: B = 6: X = 0
Call PRESSGUARD: GoTo 300

599 A(p, B1(p, B), 10) = A(p, B1(p, B), 10) + 1
If displayPause <> 0 Then U$ = "FREE THROW IS GOOD": Call PBP
S(p, H) = S(p, H) + 1: S(p, 0) = S(p, 0) + 1
Call MAINSCREEN
Return

600 If displayPause <> 0 Then U$ = "FREE THROW IS MISSED": Call PBP
Return

610 T = T - 4: TE = 4: Call PLAYINGTIME: GoSub SHTCLK: Call MAINSCREEN: GoSub 1054
If DK% = 1 And PB% = 0 Then PT% = 2: Call MAKEPBP: U$ = "STUFF BY " + AA$(p, B1(p, B)): Call PBP: DK% = 0: GoTo 617
If DK% = 1 And PB% = 1 Then Call WINDEX: Print "STUFF BY " + AA$(p, B1(p, B)): Call DELAY: DK% = 0: GoTo 617
If PZ% = 8 Then PT% = 7: Call MAKEPBP: PZ% = 0: Call PBP: GoTo 617
F3 = 1
If PB% = 0 Then U$ = "BASKET GOOD BY " + AA$(p, B1(p, B)): Call PBP
If PB% = 1 And displayPause <> 0 Then Call WINDEX: Print "BASKET GOOD BY " + AA$(p, B1(p, B)): Call DELAY

617 U$ = "... AND HE'S FOULED !!!": GoSub 729: Call DELAY: F3 = 1
S(p, H) = S(p, H) + 2: S(p, 0) = S(p, 0) + 2: A(p, B1(p, B), 8) = A(p, B1(p, B), 8) + 1: Call MAINSCREEN: GoSub 630

619 If PB% = 1 And displayPause <> 0 Then Call WINDEX: Print B$(p): Print "1 FT: "; AA$(p, B1(p, B)); "   "; A(p, B1(p, B), 1); " %"
If PB% = 0 Then U$ = AA$(p, B1(p, B)) + " WILL SHOOT 1": Call PBP
X = 50: Call DEADBALLOPTIONS: GoSub BADPR: X = 0
A(p, B1(p, B), 9) = A(p, B1(p, B), 9) + 1: S = Int(Rnd(1) * 100) + 1
If PB% = 1 And displayPause <> 0 Then Call WINDEX: Print B$(p): Print "1 FT: "; AA$(p, B1(p, B)); "   "; A(p, B1(p, B), 1); " %"
FX% = A(p, B1(p, B), 1)
If FA%(p, B1(p, B)) <= -5 Then FX% = Int(FX% * .9 + .5)
If S <= FX% And PB% = 0 Then GoSub 599
If S <= FX% And PB% = 1 Then Call MADEFT
If S > FX% And PB% = 0 Then GoSub 600
If S > FX% And PB% = 1 Then Call MISSFT
If T <= 0 Then TE = T: Call PLAYINGTIME: GoTo 170
X = 99: If S > FX% Then 670
GoSub 3000
U$ = B$(p) + " BALL O.B. AFTER SCORE"
Call SELECTPBP
GoSub 871: Call DELAY
Call STOPPAGE: B = 6
X = 0: Call PRESSGUARD: GoTo 300

630 GoSub 1065

631 A(D, B1(D, W5), 12) = A(D, B1(D, W5), 12) + 1: P2(D) = P2(D) + 1: Call MAINSCREEN
If A(D, B1(D, W5), 3) - A(D, B1(D, W5), 7) - A(D, B1(D, W5), 11) - A(D, B1(D, W5), 12) = 0 Then A(D, B1(D, W5), 3) = A(D, B1(D, W5), 3) + 1
If A(D, B1(D, W5), 12) < NF% Then GoSub 638
If A(D, B1(D, W5), 12) >= NF% Then GoSub 1000: GoSub 635
Return

635 If PB% = 1 And displayPause <> 0 Then Call WINDEX: Print "FOUL ON "; AA$(D, B1(D, W5)): Call DELAY
If PB% = 0 Then U$ = "FOUL CALLED ON " + AA$(D, B1(D, W5)): Call PBP
If IN% = 1 Then U$ = "FOUL IS RULED INTENTIONAL!": GoSub 729: Call DELAY
U$ = "AND HE HAS JUST FOULED OUT !!!"
GoSub 729
Call DELAY
If P2(D) > 6 Then X = 50
If U5 = 0 Or U5 = 1 And D <> CT Then P9 = D: C1 = 1: Call LINEUPS: Call FOULSAFE: Call DEADBALLOPTIONS: GoSub BADPR: Call SCOREBOARD
Call CLOCKOVER: X = 0: Return

638 If PB% = 1 And displayPause <> 0 Then Call WINDEX: Print "FOUL ON "; AA$(D, B1(D, W5)): Call DELAY: Print "HE HAS "; A(D, B1(D, W5), 12): Call DELAY
If PB% = 0 Then U$ = "FOUL NUMBER" + Str$(A(D, B1(D, W5), 12)) + " CALLED ON " + AA$(D, B1(D, W5)): Call PBP
Call DELAY
Return

640 PB$ = "": If T1(p) <= 0 Then 648
T1(p) = T1(p) - 1
Call SCOREBOARD

RN = Int(Rnd(1) * 100)
If RN <= 8 Then TF%(p) = 10
If RN > 8 And RN <= 16 Then TF%(p) = 5

RN = Int(Rnd(1) * 100)
If RN <= 8 Then TF%(D) = -10
If RN > 8 And RN <= 16 Then TF%(D) = -5
T = T + M5: TE = -M5: Call PLAYINGTIME
If C7 >= 1 Then X8 = X8 + M5
'
For I = 0 To 1
    For I1 = 0 To 1
        PR%(I, I1) = 0
    Next
Next

U$ = "TIME OUT " + B$(p)
Call SELECTPBP
Call DELAY: Call DEADBALLOPTIONS
For I = 0 To 1
    For I1 = 0 To 13
        FA%(I, I1) = FA%(I, I1) + 1
        If FA%(I, I1) > 0 Then FA%(I, I1) = 0
    Next
Next

648 If A6 = 0 Then Call PRESSGUARD: GoTo 300
GoTo 350
WHOLAY:

RN = Int(Rnd(1) * 100) + 1
If RN <= 38 Then B = 0
If RN > 38 And RN <= 75 Then B = 1
If RN > 75 And RN <= 83 Then B = 2
If RN > 83 And RN <= 91 Then B = 3
If RN > 91 Then B = 4
Return
BADPR:
For I = 0 To 1
    If P7(I) = 0 Then PR%(I, 0) = 1: PR%(I, 1) = 7: PC%(I) = 0
Next
Return

650 A6 = 1: P6 = 0: A4 = 0: D6 = 0: GoSub 1033: S = 0
R0 = Int(Rnd(1) * 100) + 1
If H > 1 And T < 30 And S(D, 0) - S(p, 0) = 2 And R0 <= 12 Or H > 1 And T < 30 And S(p, 0) - S(D, 0) = 0 And R0 <= 12 Then Call OFFREBOUND: GoTo 760
R0 = Int(Rnd(1) * 100) + 1
If R0 <= 11 Then FF% = 1: GoTo 707
If R0 <= 19 Then Call OFFREBOUND: GoTo 760
Call DEFREBOUND: GoTo 800

670 A6 = 1: P6 = 0: A4 = 0: D6 = 0: GoSub 1033: GoTo 700

700 S = 0: A4 = 0: D6 = 0: R0 = Int(Rnd(1) * 100) + 1
GG = S(D, 0) - S(p, 0)
If H > 1 And T < 30 And (GG = 2 Or GG = 0 Or GG = -2) And R0 <= 12 Then 700
If TP% = 1 And R0 <= 19 Then TP% = 0
If R0 <= 1 Then 725
If R0 <= DY Then FF% = 1: GoTo 707
If R0 <= 17 Then 750
If R0 <= 19 Then 730
If R0 <= 20 Then 740

707 TP% = 0
GoTo 770

710 If displayPause = 0 Then 713
If PB% = 1 Then Call WINDEX: Print AA$(D, B1(D, B)); " HAS THE DEF RB FOR": Print B$(D); " AND IS FOULED !!!": Call DELAY: GoTo 713
U$ = AA$(D, B1(D, B)) + " HAS THE DEF RB": Call PBP
U$ = "HE'S FOULED !!!": Call PBP

713 R3(D) = R3(D) + 1: A(D, B1(D, B), 11) = A(D, B1(D, B), 11) + 1: Call MAINSCREEN: GoSub 3000: GoTo 565

720 U$ = AA$(p, B1(p, B)) + " HAS THE OFF RB"
If PB% = 0 Then Call PBP
If PB% = 1 And displayPause <> 0 Then Call WINDEX: Print U$: Print "FOR "; B$(p): Call DELAY
U$ = "HE'S FOULED !!!"
GoSub 729
A(p, B1(p, B), 11) = A(p, B1(p, B), 11) + 1: A(p, B1(p, B), 24) = A(p, B1(p, B), 24) + 1
R3(p) = R3(p) + 1: Call MAINSCREEN: GoTo 565

725 If X = 98 Then 700
T0(p) = T0(p) + 1
U$ = "OFFENSIVE GOAL TENDING"
Call SELECTPBP

U$ = "BALL O.B. TO " + B$(D)
GoSub 729
Call DELAY
GoSub 3000: Call DEADBALLOPTIONS: GoSub BADPR: GoTo 300

729 If PB% = 0 Then Call PBP
If PB% = 1 And displayPause <> 0 Then Print U$: Call DELAY
Return

730 If X = 99 Then 700
Call OFFREBOUND
A(p, B1(p, B), 7) = A(p, B1(p, B), 7) + 1: A(p, B1(p, B), 8) = A(p, B1(p, B), 8) + 1
A(p, B1(p, B), 11) = A(p, B1(p, B), 11) + 1: R3(p) = R3(p) + 1: S(p, H) = S(p, H) + 2
S(p, 0) = S(p, 0) + 2: Call MAINSCREEN
If PB% = 1 And displayPause <> 0 Then Call WINDEX: Print "TIPPED IN BY "; AA$(p, B1(p, B)): Print "   BASKET GOOD !!!": Call DELAY
If PB% = 0 Then U$ = AA$(p, B1(p, B)) + " TIPS IT IN!!": Call PBP
GoSub 3000
U$ = B$(p) + " BALL O.B. AFTER SCORE": GoSub 871
GoSub 729: Call DELAY
Call STOPPAGE: Call PRESSGUARD
GoTo 300

740 If X = 99 Then 700
U$ = "DEFENSIVE GOAL TENDING"
Call SELECTPBP
If TP% = 1 Then TP% = 0: GoTo 369
GoTo 560

750 R0 = Int(Rnd(1) * 100) + 1: If R0 <= 35 Then 752
R3(D) = R3(D) + 1: Call MAINSCREEN
U$ = "RB KNOCKED O.B. TO " + B$(D)
Call SELECTPBP
Call DELAY: Call DEADBALLOPTIONS: GoSub BADPR: GoSub 3000: C1 = 2: P9 = p: GoTo 300

752 R3(p) = R3(p) + 1: Call MAINSCREEN
U$ = "RB KNOCKED O.B. TO " + B$(p)
Call SELECTPBP
Call DELAY: Call DEADBALLOPTIONS: GoSub BADPR: LT = 4: GoTo 520

760 If FF% = 1 Then FF% = 0: GoTo 720
A(p, B1(p, B), 24) = A(p, B1(p, B), 24) + 1: A(p, B1(p, B), 11) = A(p, B1(p, B), 11) + 1: R3(p) = R3(p) + 1: Call MAINSCREEN
U$ = AA$(p, B1(p, B)) + " HAS THE OFF RB"
If PB% = 0 And displayPause <> 0 Then Call PBP
If PB% = 1 And displayPause <> 0 Then Call WINDEX: Print U$: Print "FOR " + B$(p): Call DELAY
If T <= 0 Then 170
R0 = Int(Rnd(1) * 130) + 1
PB$ = AA$(p, B1(p, B))
If R0 = 32 Then GoTo RBTIEUP
R0 = Int(Rnd(1) * 10) + 1: Call SHOTPERCENT
If H > 2 Or H = 2 And T <= 120 Then 765
If A(p, B1(p, B), 3) - A(p, B1(p, B), 7) - A(p, B1(p, B), 11) - A(p, B1(p, B), 12) <= 0 Then F = Int(F * .6 + .5)

765 If R0 <= 8 Then PZ% = 9: P6 = 1: GoTo 537
GoTo 520

770 RB = 0
For I = 0 To 9: RB(I) = 0: Next
For I = 0 To 4
    XZ = A(p, B1(p, I), 21)
    If H > 2 Or H = 2 And T <= 120 Then 773
    If A(p, B1(p, I), 3) - A(p, B1(p, I), 7) - A(p, B1(p, I), 11) - A(p, B1(p, I), 12) <= 0 Then XZ = Int(XZ * .5 + .5)
    773 If FA%(p, B1(p, I)) <= -5 Then XZ = Int(XZ * .9 + .5)
    RB(I) = RB(I) + XZ: RB = RB + XZ
Next
ORB = RB
For I = 0 To 4
    XZ = A(1 - p, B1(1 - p, I), 2)
    If H > 2 Or H = 2 And T <= 120 Then 778
    If A(1 - p, B1(1 - p, I), 3) - A(1 - p, B1(1 - p, I), 7) - A(1 - p, B1(1 - p, I), 11) - A(1 - p, B1(1 - p, I), 12) <= 0 Then XZ = Int(XZ * .5 + .5)
    778 If FA%(1 - p, B1(1 - p, I)) <= -5 Then XZ = Int(XZ * .9 + .5)
    RB(I + 5) = RB(I + 5) + XZ: RB = RB + XZ
Next
DRB = RB - ORB
If D2(D) = 0 Then XQ = Int((RB * .06) + .5)
If D2(D) = 1 Then XQ = Int((RB * .12) + .5)
If D2(D) = 2 Then XQ = Int((RB * .1) + .5)
If D2(D) = 3 Then XQ = Int((RB * .14) + .5)
If CZ%(p) = 1 Then XQ = Int((RB * .17) + .5)

RN = Rnd(1) * RB
If RN <= ORB + XQ Then GoTo OREB

RN = Rnd(1) * DRB
For B = 5 To 9
    If RN <= RB(B) Then B = B - 5: GoTo 800
    RB(B + 1) = RB(B + 1) + RB(B)
Next
B = Int(Rnd(1) * 5): GoTo 800
OREB:

RN = Rnd(1) * ORB
For B = 0 To 4
    If RN <= RB(B) Then 760
    RB(B + 1) = RB(B + 1) + RB(B)
Next
B = Int(Rnd(1) * 5)
GoTo 760

800 If FF% = 1 Then FF% = 0: GoTo 710
A(D, B1(D, B), 11) = A(D, B1(D, B), 11) + 1: R3(D) = R3(D) + 1: Call MAINSCREEN
U$ = AA$(D, B1(D, B)) + " HAS THE DEF RB"
If PB% = 0 And displayPause <> 0 Then Call PBP
If PB% = 1 And displayPause <> 0 Then Call WINDEX: Print U$: Print "FOR " + B$(D): Call DELAY
PB$ = AA$(D, B1(D, B))
C1 = 2: P9 = D: GoSub 3000: FB% = 1
PB$ = AA$(p, B1(p, B))
R0 = Int(Rnd(1) * 130) + 1
If R0 = 15 Then FB% = 0: GoTo RBTIEUP
If CZ%(D) = 1 And Int(Rnd(1) * 100) + 1 <= 50 Then T = T - 4: TE = 4: Call PLAYINGTIME: GoSub SHTCLK: GoSub WHOLAY: GoTo LAYUP
GoTo 300

819 If X3% = 0 Then 822
For I = 0 To 13
    M8 = 100 - A(P9, I, 19)
    If M8 = 0 Then 821
    M9 = 100 / M8

821 A(P9, I, 16) = Int((A(P9, I, 16) * M9) + .5): Next I

822 Return

871 Locate 8, 41: Print "            ": Return

900 If AP = 1 Then N = NG%(2)
If C7 = 1 Then X8 = 45
If C7 = 2 Then X8 = 35
'If C7 = 3 Then X8 = 30
GoTo 240

949 If H >= 2 And T <= 240 And S(p, 0) - S(D, 0) > 0 And U5 = 1 And p = CT Then 951
If U5 = 2 Or U5 = 1 And D = CT Then 980

951 If T <= 1 Or C7 >= 1 And X8 <= 7 Then P6 = 1: GoTo 532
If T <= 8 And S(p, 0) - S(D, 0) <= -3 And H >= 2 Then P6 = 1: GoTo 532
If T <= 20 And S(p, 0) - S(D, 0) <= -5 And H >= 2 Then P6 = 1: GoTo 532
FX% = A(p, B1(p, B), 0)
If FA%(p, B1(p, B)) <= -5 Then FX% = FX% - 10
If P6 = 1 Then 532
If (U5 = 0 And CM% = 1 Or U5 = 1 And D = CT And CM% = 1 Or U5 = 1 And p = CT Or U5 = 2) And T <= 660 And T >= 540 And H = 1 And T1(p) >= 5 Then P6 = 2: CTO% = 1: GoTo 532
If (U5 = 0 And CM% = 1 Or U5 = 1 And D = CT And CM% = 1 Or U5 = 1 And p = CT Or U5 = 2) And T <= 240 And T >= 120 And H = 1 And T1(p) >= 4 Then P6 = 2: CTO% = 1: GoTo 532
If (U5 = 0 And CM% = 1 Or U5 = 1 And D = CT And CM% = 1 Or U5 = 1 And p = CT Or U5 = 2) And T <= 840 And T >= 720 And H = 2 And T1(p) >= 3 Then P6 = 2: CTO% = 1: GoTo 532
If (U5 = 0 And CM% = 1 Or U5 = 1 And D = CT And CM% = 1 Or U5 = 1 And p = CT Or U5 = 2) And T <= 300 And T >= 180 And H = 2 And T1(p) >= 2 Then P6 = 2: CTO% = 1: GoTo 532
GoTo 532

980 If (U5 = 1 Or U5 = 0) And CM% <> 1 Then 530
GoTo 951

1000 M9 = 0
For I3 = 0 To 13
    If I3 = B1(D, W5) Or AA$(D, I3) = "XXX" Or B%(D, I3, 18) <> 0 Then 1002
    M9 = M9 + 1
1002 Next

For I3 = 0 To 13
    If I3 = B1(D, W5) Or AA$(D, I3) = "XXX" Or B%(D, I3, 18) <> 0 Then 1003
    If A(D, I3, 0) <= A(D, B1(D, W5), 0) Then A(D, I3, 3) = A(D, I3, 3) + Int((A(D, B1(D, W5), 3) / M9) + .5)
1003 Next I3
Return


1033 If C7 > 0 Then

    Select Case C7
        Case 1: X8 = 45
        Case 2: X8 = 35
            'Case 3: X8 = 30
    End Select

    LS = 4
    If T < 46 And X8 >= T Or H > 2 And T = 300 Then Locate 5, 5: Print "     "
    If T < 6 And LS = 4 And LT <> 4 Then F = 5

End If

Return


1054 R0 = Int(Rnd(1) * 3) + 1: I2 = Int(Rnd(1) * 5)
If R0 <= A(p, B1(p, I2), 4) And AA$(p, B1(p, B)) <> AA$(p, B1(p, I2)) Then 1060
If A(p, B1(p, I2), 4) > 3 And R0 < A(p, B1(p, I2), 4) - 3 And AA$(p, B1(p, B)) <> AA$(p, B1(p, I2)) Then 1060

1058 Return

1060 W5 = 0
For M8 = 0 To 4
    W5 = W5 + A(p, B1(p, M8), 4)
Next
R0 = Int(Rnd(1) * W5) + 1
W5 = 0
For M8 = 0 To 4
    W5 = W5 + A(p, B1(p, M8), 4)
    If R0 <= W5 Then 1062
Next

1062 I2 = M8
If AA$(p, B1(p, B)) = AA$(p, B1(p, I2)) Then 1060
A(p, B1(p, I2), 14) = A(p, B1(p, I2), 14) + 1
GoTo 1058

1065 W5 = 0: M9 = Int(Rnd(1) * F5%(D, 0)) + 1
For M8 = 0 To 4: W5 = W5 + A(D, B1(D, M8), 17)
    If M9 <= W5 Then 1069
Next M8

1069 If M8 < 0 Or M8 > 4 Then M8 = Int(Rnd(1) * 5)
W5 = M8: Return

3000 PB$ = "": If p = 1 Then p = 0: D = 1: P9 = 0: C1 = 2: Return
If p = 0 Then p = 1: D = 0: P9 = 1: C1 = 2: Return

TIEUP:
If PB% = 1 Then GoTo RBTIEUP
R0 = Int(Rnd(1) * 100) + 1
If R0 <= 20 Then U$ = "BALL IS SLAPPED AWAY"
If R0 > 20 And R0 <= 40 Then U$ = "BALL IS LOOSE ON THE FLOOR"
If R0 > 40 And R0 <= 60 Then U$ = AA$(p, B1(p, B)) + " IS TRAPPED"
If R0 > 60 And R0 <= 80 Then U$ = "TRIES TO WORK LOOSE"
If R0 > 80 Then U$ = AA$(p, B1(p, B)) + " IS PINNED ON THE LINE"
Call PBP
RBTIEUP:
R0 = Int(Rnd(1) * 100) + 1
P9 = Int(Rnd(1) * 5)
If PB% = 1 And displayPause <> 0 Then Call WINDEX: Print "JUMP BALL!": Call DELAY: GoTo 5005
If R0 <= 25 Then U$ = AA$(p, B1(p, B)) + " IS TIED UP BY " + AA$(D, B1(D, P9))
If R0 > 25 And R0 <= 50 Then U$ = AA$(D, B1(D, P9)) + " TIES UP " + AA$(p, B1(p, B))
If R0 > 50 And R0 <= 75 Then U$ = "HELD BALL!"
If R0 > 75 Then U$ = "JUMP BALL!"
Call PBP

5005 U$ = "POSSESSION ARROW TO " + B$(PA%)
GoSub 729
Call DELAY
If PA% = D Then T0(p) = T0(p) + 1: Call DEADBALLOPTIONS: GoSub BADPR: GoSub 3000: C1 = 2: P9 = p: PA% = 1 - PA%: GoTo 300
PA% = 1 - PA%: Call DEADBALLOPTIONS: GoSub BADPR: LT = 4: GoTo 350

8000 Call MAINSCREEN
U$ = "1 FT: " + AA$(p, B1(p, B)) + Str$(A(p, B1(p, B), 1)) + " %": Call PBP
X = 50: Call DEADBALLOPTIONS: GoSub BADPR: X = 0: A(p, B1(p, B), 9) = A(p, B1(p, B), 9) + 1
U$ = "1 FT: " + AA$(p, B1(p, B)) + Str$(A(p, B1(p, B), 1)) + " %": Call PBP
F3 = 1: S = Int(Rnd(1) * 100) + 1
FX% = A(p, B1(p, B), 1)
If FA%(p, B1(p, B)) <= -5 Then FX% = Int(FX% * .9 + .5)
If S <= FX% Then Call THREEFT
If S > FX% Then Call BADFT
LT = 0
If T <= 0 Then TE = T: Call PLAYINGTIME: GoTo 170
If S > FX% Then 650
GoSub 3000: U$ = B$(p) + " BALL O.B. AFTER SCORE": GoSub 871: Call PBP: Call DELAY: Call STOPPAGE: B = 6: X = 0: Call PRESSGUARD
GoTo 300

8400 'CHECK SPECIAL EVENTS
If N = 2 Or ATT(1) = 0 Then
    Y3 = Int(Rnd(1) * 2000) + 9000
Else
    Y3 = Int(Rnd(1) * 2000) + ATT(1) - 1000
End If
Call RECORDSCHECK
Call EVENTS(COUNT, SPECIAL)

If SPECIAL = 1 Then
    NM$ = "BOX" + Mid$(Str$(COUNT), 2, Len(Str$(COUNT))) + ".SPC"
    Call BOX2SPC(NM$)
End If

8500 GoSub 8650
Cls
If X = 1 Then 8532
Call BOXTOP

8515 GoSub 8670
GoSub 8660
Call BOXSCREEN

If H > 2 Then EG% = 1
Call TICKER: EG% = 0
If H = 2 Then Call FOULHARD: Return

8532 GoSub 8655
Color 15, 0
Cls
I = 1
Call DRAWBOX
U$ = "DISPLAY STATS AGAIN (YN)"
Locate , (80 - Len(U$)) / 2
Print U$
Color 15, 0
If AP = 1 And U5 = 2 Then 8535

I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 8500

8535 Color 15, 0
Cls
I = 1
Call DRAWBOX
U$ = "SAVE STATS (YN)"
Locate , (80 - Len(U$)) / 2
Print U$
Color 15, 0
If AP = 1 Then 9506
I$ = GetKeyPress$
If UCase$(I$) <> "Y" Then 9507

9506 Call SAVESTATS

9507 Color 15, 0
Cls
I = 1
Call DRAWBOX
U$ = "PLAY ANOTHER GAME (YN)"
Locate , (80 - Len(U$)) / 2
Print U$

If AP <> 1 Then
    Color 15, 0
    I$ = GetKeyPress$
    If UCase$(I$) <> "Y" Then
        Run "HELLO.EXE"
    Else
        Cls
        Run "COLHOOP.EXE"
    End If
Else
    Cls
    Run "COLHOOP.EXE"
End If

8650 Cls
Color 14: Print "(0) ";: Color 15: Print "STATS TO SCREEN": Print
Color 14: Print "(1) ";: Color 15: Print "STATS TO PRINTER": Print
Color 14: Print "(2) ";: Color 15: Print "STATS TO TEXT FILE"
If AP = 1 And U5 = 2 Then X = 2: Call BOX2FILE: Return

I$ = GetKeyPress$
X = Val(I$)
If X = 1 Then Call BOX2PRINTER
If X = 2 Then Call BOX2FILE

8655 If X >= 1 Then X = 0
Return

8660 If X = 0 Then Cls
Return

8670 If X = 0 Then
    If Not (AP = 1 And U5 = 2) Then
        I$ = GetKeyPress
    End If
End If
Return

9030 I2 = Int(Rnd(1) * 50) + 1: If I2 = 43 And TF% = 1 And S > F3S% Then A(p, B1(p, B), 7) = A(p, B1(p, B), 7) - 1: W%(p, B1(p, B), 1) = W%(p, B1(p, B), 1) - 1: GoSub 1033: GoTo 9055
If I2 = 43 And TF% = 1 And S <= F3S% Then 9040
If S > F3S% Then 361
GoTo 363

9040 T = T - 4: TE = 4: Call PLAYINGTIME: GoSub SHTCLK: Call MAINSCREEN: GoSub 1054
U$ = "3 PT BASKET GOOD BY " + AA$(p, B1(p, B))
Call SELECTPBP
GoSub 1054
U$ = "AND HE WAS FOULED !!!"
GoSub 729
F3 = 1
S(p, H) = S(p, H) + 3: S(p, 0) = S(p, 0) + 3: A(p, B1(p, B), 8) = A(p, B1(p, B), 8) + 1: W%(p, B1(p, B), 0) = W%(p, B1(p, B), 0) + 1: Call MAINSCREEN: GoSub 630
GoTo 619

9055 T = T - 4: TE = 4: Call PLAYINGTIME: GoSub SHTCLK: Call MAINSCREEN: GoSub 630
If displayPause = 0 Then 9056
If PB% = 0 Then U$ = "3 FT: " + AA$(p, B1(p, B)) + Str$(A(p, B1(p, B), 1)) + " %": Call PBP: Call DELAY: GoTo 9056
Call WINDEX: Print B$(p): Print "3 FT: "; AA$(p, B1(p, B)); "   "; A(p, B1(p, B), 1); " %"

9056 X = 50: Call DEADBALLOPTIONS: GoSub BADPR: X = 0: A(p, B1(p, B), 9) = A(p, B1(p, B), 9) + 3
If displayPause = 0 Then 9057
If PB% = 0 Then U$ = "3 FT: " + AA$(p, B1(p, B)) + Str$(A(p, B1(p, B), 1)) + " %": Call PBP: GoTo 9057
Call WINDEX: Print B$(p)
Print "3 FT: "; AA$(p, B1(p, B)); "   "; A(p, B1(p, B), 1); " %"

9057 For F3 = 1 To 3: S = Int(Rnd(1) * 100) + 1
    FX% = A(p, B1(p, B), 1)
    If FA%(p, B1(p, B)) <= -5 Then FX% = Int(FX% * .9 + .5)
    If S <= FX% And PB% = 0 Then Call THREEFT
    If S <= FX% And PB% = 1 Then Call MADEFT
    If S > FX% And PB% = 0 Then Call BADFT
    If S > FX% And PB% = 1 Then Call MISSFT
Next F3: LT = 0
If T <= 0 Then TE = T: Call PLAYINGTIME: GoTo 170
If S > FX% Then 650
GoTo 598

9065 If IN% = 1 Then 9070
If TB% = 1 And P2(D) >= 10 Then 9070
If P2(D) > 6 Then 570
GoTo 567

9070 If displayPause = 0 Then 9071
If PB% = 0 Then U$ = "2 FT: " + AA$(p, B1(p, B)) + Str$(A(p, B1(p, B), 1)) + " %": Call PBP: Call DELAY: GoTo 9071
Call WINDEX: Print B$(p): Print "2 FT: "; AA$(p, B1(p, B)); "   "; A(p, B1(p, B), 1); " %"

9071 X = 50: Call DEADBALLOPTIONS: GoSub BADPR: X = 0
If displayPause = 0 Then 9072
If PB% = 0 Then U$ = "2 FT: " + AA$(p, B1(p, B)) + Str$(A(p, B1(p, B), 1)) + " %": Call PBP: GoTo 9072
Call WINDEX: Print B$(p)
Print "2 FT: "; AA$(p, B1(p, B)); "   "; A(p, B1(p, B), 1); " %"

9072 For F3 = 1 To 2: A(p, B1(p, B), 9) = A(p, B1(p, B), 9) + 1: S = Int(Rnd(1) * 100) + 1
    FX% = A(p, B1(p, B), 1)
    If FA%(p, B1(p, B)) <= -5 Then FX% = Int(FX% * .9 + .5)
    If S <= FX% And PB% = 0 Then Call THREEFT
    If S <= FX% And PB% = 1 Then Call MADEFT
    If S > FX% And PB% = 0 Then Call BADFT
    If S > FX% And PB% = 1 Then Call MISSFT
Next F3
If T <= 0 Then 170
Call CLEARPBPBOX: If S > FX% And IN% <> 1 Then 650
If IN% = 1 Then PASS% = 1: IN% = 0: X = 0: U$ = B$(p) + " BALL AFTER INT. FOUL": Call SELECTPBP: GoSub 871: Call DELAY: Call STOPPAGE: Call PRESSGUARD: GoTo 300
GoSub 3000
X = 0
U$ = B$(p) + " BALL O.B. AFTER SCORE"
GoSub 871
Call SELECTPBP
Call DELAY
Call STOPPAGE
Call PRESSGUARD
GoTo 300


'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
'$INCLUDE: 'GameRoutines.BM'
