'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Courtside College Basketball - HOOPS!"
$ExeIcon:'./lhg_ccb.ico'
_Icon
_AllowFullScreen

DefInt A-Z
DECLARE SUB AICOMCOACH
DECLARE SUB ALPHANUM (I$,J)
DECLARE SUB AUTOSETUP ()
DECLARE SUB BADFT ()
DECLARE SUB BALLDISTRIBUTE ()
DECLARE SUB BLANKLINE ()
DECLARE SUB BOX2FILE
DECLARE SUB BOX2PRINTER
DECLARE SUB BOX2SPC (NM$)
DECLARE SUB BOXSCREEN ()
DECLARE SUB BOXTOP ()
DECLARE SUB CLEARPBP ()
DECLARE SUB CLEARPBPBOX ()
DECLARE SUB CLOCKOVER ()
DECLARE SUB COLCNG ()
DECLARE SUB COMCOACH
DECLARE SUB COMPUTERSUB ()
DECLARE SUB DEADBALLOPTIONS ()
DECLARE SUB DEFENSEMENU ()
DECLARE SUB DEFREBOUND ()
DECLARE SUB DELAY ()
DECLARE SUB DISABLEPLAYER ()
DECLARE SUB DRAWBOX ()
DECLARE SUB EDITCX()
DECLARE SUB EVENTS (COUNT, SPECIAL)
DECLARE SUB FATIGUE ()
DECLARE SUB FOULHARD ()
DECLARE SUB FOULSAFE ()
DECLARE SUB FRAME ()
DECLARE SUB GETATEAM (I1, YN$)
DECLARE SUB HORN ()
DECLARE SUB LASTFIVEMENU ()
DECLARE SUB LASTSHOT ()
DECLARE SUB LINEUPS ()
DECLARE SUB LOADSTATS
DECLARE SUB MADEFT ()
DECLARE SUB MAINSCREEN ()
DECLARE SUB MAKEPBP ()
DECLARE SUB MISSFT ()
DECLARE SUB OFFENSE ()
DECLARE SUB OFFREBOUND ()
DECLARE SUB PBP ()
DECLARE SUB PLAYINGTIME ()
DECLARE SUB PRDFAT ()
DECLARE SUB PRELOAD ()
DECLARE SUB PRESSGUARD ()
DECLARE SUB PRPFAT ()
DECLARE SUB QUARTERTOTAL ()
DECLARE SUB REBOUNDSORT
DECLARE SUB RECORDSCHECK ()
DECLARE SUB RESETFATIGUE ()
DECLARE SUB SAVESTATS
DECLARE SUB SCOREBOARD ()
DECLARE SUB SELECTID ()
DECLARE SUB SELECTPBP ()
DECLARE SUB SETSTAMINA ()
DECLARE SUB SHOTPERCENT ()
DECLARE SUB SHOTSTRING ()
DECLARE SUB STAMINASORT
DECLARE SUB STATCONVERT (Q,QQ)
DECLARE SUB STEALADJ ()
DECLARE SUB STOPPAGE ()
DECLARE SUB TEAMSELECT ()
DECLARE SUB TEAMSETUP ()
DECLARE SUB THREEFT ()
DECLARE SUB TICKER ()
DECLARE SUB TICKERCHECK ()
DECLARE SUB TOURNEY
DECLARE SUB WHOGETSBALL ()
DECLARE SUB WINDEX ()
DECLARE SUB ZEROCHECK ()
DECLARE SUB ZONECHANGE ()

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
Dim Shared T2%(1, 20), TF%(1), TM%(1, 13), TP%(1), TR%(1, 9), V%(500), V1%(1), V2%(1)
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


'------------------------------
'   DISABLEPLAYER Subroutine
'------------------------------
' ...explanation...
Sub DISABLEPLAYER ()
    Color 15, 0
    Cls: I = P9
    Call DRAWBOX
    U$ = "DISABLE PLAYERS FOR " + B$(P9) + " (YN)"
    Locate , (80 - Len(U$)) / 2
    Print U$
    Color 15, 0
    Print: Print
    Print "NOTE:: THE COMPUTER WILL AUTOMATICALLY DISABLE PLAYERS BASED ON"
    Print "       THE RATIO OF GAMES PLAYED FOR EACH PLAYER TO THE TOTAL"
    Print "       NUMBER OF GAMES PLAYED BY THE TEAM. YOU MAY WANT TO CHECK"
    Print "       TO ACTIVATE ANY COMPUTER DEACTIVATED PLAYERS"
    I$ = GetKeyPress$
    If UCase$(I$) <> "Y" Then 9210
    DX% = 0: Cls: Print "TEAM ROSTER FOR ";: Color TR%(P9, 0), TR%(P9, 1): Print B$(P9): Color 15, 0
    Print "# NAME          POS   GP"
    For I = 0 To 13
        If I <= 9 Then Print Using "#"; I;
        If I = 10 Then Print "A";
        If I = 11 Then Print "B";
        If I = 12 Then Print "C";
        If I = 13 Then Print "D";
        If AA$(P9, I) = "XXX" Then B%(P9, I, 18) = -1: Color TR%(P9, 0), TR%(P9, 1)
        If B%(P9, I, 18) = 1 Or B%(P9, I, 18) = -1 Then Color TR%(P9, 0), TR%(P9, 1)
        Print " "; AA$(P9, I);: Color 15, 0: Print ; Tab(18); PO$(P9, I); Tab(22); C(P9, I, 0)
    Next
    9203 Locate 19, 1: Print "ENTER PLAYER # TO DISABLE/ENABLE (Z=FINISHED)"
    Print "YOU MUST HAVE AT LEAST 8 ACTIVE PLAYERS"
    I$ = GetKeyPress$
    If UCase$(I$) = "Z" Then 9210
    I1 = Val(I$)
    If UCase$(I$) = "A" Then I1 = 10
    If UCase$(I$) = "B" Then I1 = 11
    If UCase$(I$) = "C" Then I1 = 12
    If UCase$(I$) = "D" Then I1 = 13
    If B%(P9, I1, 18) = 0 And DX% < 4 Then B%(P9, I1, 18) = -1: DX% = DX% + 1: Locate 3 + I1, 3: Color TR%(P9, 0), TR%(P9, 1): Print AA$(P9, I1): Color 15, 0: GoTo 9203
    If B%(P9, I1, 18) = -1 Then B%(P9, I1, 18) = 0: DX% = DX% - 1: Locate 3 + I1, 2: Color 15, 0: Print " "; AA$(P9, I1): GoTo 9203
    GoTo 9203

9210 End Sub


'------------------------------
'      DRAWBOX Subroutine
'------------------------------
' ...explanation...
Sub DRAWBOX ()
    Locate 6, 8
    Color TR%(I, 0), TR%(I, 1)

    'Print box top
    Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";

    'Print box sides
    JJ = 1
    For II = 1 To JJ
        Locate 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
    Next

    'Print box bottom
    Locate 7 + JJ, 8
    Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";
    Locate 7
End Sub


'------------------------------
'      GETATEAM Subroutine
'------------------------------
' ...explanation...
Sub GETATEAM (I1, YN$)
    Open p$(0) + "BASK." + YN$ For Random As #1 Len = 655
FIELD #1, 15 AS Q$(0), 2 AS Q$(1), 2 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 2 AS Q$(14), 2 AS Q$(15), 2 AS Q$(16), 2 AS Q$(17), 2 AS Q$(18), 2 _
 AS Q$(19), 602 AS X$
    For I = 0 To 13
FIELD #1, 53 + I * 43 AS X$, 15 AS Q$(I * 15 + 20), 2 AS Q$(I * 15 + 21), 2 AS Q$(I * 15 + 22), 2 AS Q$(I * 15 + 23), 2 AS Q$(I * 15 + 24), 2 AS Q$(I * 15 + 25), 2 AS Q$(I * 15 + 26), 2 AS Q$(I * 15 + 27), 2 AS Q$(I * 15 + 28), 2 AS Q$(I * 15 + 29) _
, 2 AS Q$(I * 15 + 30), 2 AS Q$(I * 15 + 31), 2 AS Q$(I * 15 + 32), 2 AS Q$(I * 15 + 33), 2 AS Q$(I * 15 + 34), 655 - 53 - 43 - I * 43 AS X$
    Next
    Get #1, I1
    B$(P9) = Q$(0)
    9010 If Right$(B$(P9), 1) = " " Then B$(P9) = Left$(B$(P9), Len(B$(P9)) - 1): GoTo 9010
    S1(P9) = CVI(Q$(1)): D1(P9) = CVI(Q$(2))
    V1%(P9) = CVI(Q$(3)): V2%(P9) = CVI(Q$(4))
    For I3 = 0 To 4: L%(P9, I3) = CVI(Q$(I3 + 5)): Next
    For I3 = 0 To 9: TR%(P9, I3) = CVI(Q$(I3 + 10)): Next
    For I3 = 0 To 13
        AA$(P9, I3) = Q$(I3 * 15 + 20)
        9015 If Right$(AA$(P9, I3), 1) = " " Then AA$(P9, I3) = Left$(AA$(P9, I3), Len(AA$(P9, I3)) - 1): GoTo 9015
        For I4 = 0 To 6
            A(P9, I3, I4) = CVI(Q$(I3 * 15 + I4 + 21))
            C(P9, I3, I4) = CVI(Q$(I3 * 15 + I4 + 28))
        Next
    Next
    Close 1

    FY%(P9) = 2 * TR%(P9, 3) - TR%(P9, 4)
    9016 CNT% = 0: CNTX% = 0
    If TR%(P9, 5) = 0 Then 9018
    For I = 0 To 13
        If B%(P9, I, 18) = -1 Then B%(P9, I, 18) = 0
        RN = Int(Rnd(1) * 100) + 1
        If UCase$(AA$(P9, I)) = "XXX" Then CNTX% = CNTX% + 1
        If UCase$(AA$(P9, I)) = "XXX" Or (B%(P9, I, 18) = 0 And RN > Int(C(P9, I, 0) / TR%(P9, 5) * 100 + .5)) Then B%(P9, I, 18) = -1
    Next
    For I = 0 To 13: If B%(P9, I, 18) = 0 Then CNT% = CNT% + 1
    Next
    If CNT% < 10 And CNTX% < 4 Then 9016
    If CNTX% >= 5 And CNT% < 8 Then 9016
    9018 '

    Open p$(0) + "COLBBTMS." + YN$ As #1 Len = 566
    Field #1, 15 As Q$(0), 40 As Q$(1), 15 As Q$(2), 4 As Q$(3)
    For I = 0 To 13
FIELD #1,74+I*32 AS X$,2 AS Q$(I*16+4),2 AS Q$(I*16+5),2 AS Q$(I*16+6),2 AS Q$(I*16+7),2 AS Q$(I*16+8),2 AS Q$(I*16+9),2 AS Q$(I*16+10),2 AS Q$(I*16+11),2 AS Q$(I*16+12),2 AS Q$(I*16+13),2 AS Q$(I*16+14),2 AS Q$(I*16+15),2 AS Q$(I*16+16),2 AS Q$(I  _
*16+17),2 AS Q$(I*16+18),2 AS Q$(I*16+19)
FIELD #1, 522 AS X$,2 AS Q$(228),2 AS Q$(229),2 AS Q$(230),2 AS Q$(231),2 AS Q$(232),2 AS Q$(233),2 AS Q$(234),2 AS Q$(235),2 AS Q$(236),2 AS Q$(237),2 AS Q$(238),2 AS Q$(239),2 AS Q$(240),2 AS Q$(241),2 AS Q$(242),2 AS Q$(243),2 AS Q$(244),2 AS    _
Q$(245),2 AS Q$(246),2 AS Q$(247),2 AS Q$(248),2 AS Q$(249)
    Next
    Get #1, I1
    C$(P9) = Q$(0)
    S$(P9) = Q$(1)
    A1$(P9) = Q$(2)
    ATT(P9) = CVL(Q$(3))

    For I = 0 To 13
        DF(P9, I) = CVI(Q$(I * 16 + 4))
        PO$(P9, I) = Q$(I * 16 + 5)
        For I1 = 0 To 13
            RS(P9, I, I1) = CVI(Q$(I * 16 + I1 + 6))
        Next
    Next
    For I = 0 To 20
        T2%(P9, I) = CVI(Q$(228 + I))
    Next
    CK = CVI(Q$(249))
    Close 1

    If CK <> 999 Then
        C$(P9) = "": S$(P9) = "": A1$(P9) = "": ATT(P9) = 0
        For I = 0 To 13: DF(P9, I) = 5: PO$(P9, I) = ""
            For I1 = 0 To 13: RS(P9, I, I1) = 0
            Next
        Next
        For I = 0 To 20: T2%(P9, I) = 0: Next
    End If
End Sub


'------------------------------
'      HORN Subroutine
'------------------------------
' ...explanation...
Sub HORN ()
    For PP = 1 To 70
        Sound 50, .05
        Sound 100, .05
        Sound 317, .3
    Next PP
    For PZ = 1 To 2000
    Next
End Sub


'------------------------------
'      MAKEPBP Subroutine
'------------------------------
' ...explanation...
Sub MAKEPBP ()
    1400 'PLAY BY PLAY
    If displayPause = 0 Then 1495
    RN = Int(Rnd(1) * 100) + 1
    On PT% GOTO 1410, 1420, 1430, 1440, 1445, 1450, 1460, 1470, 1480, 1490
    U$ = AA$(p, B1(p, B))
    1410 'TURNOVER
    U$ = AA$(p, B1(p, B))
    If RN > 35 And RN <= 68 And P7(D) > 1 And PASS% = 1 Then U$ = B$(p) + " CALLED FOR 10 SEC VIOLATION": GoTo 1495
    RN = Int(Rnd(1) * 14) + 1
    If RN = 1 Then U$ = U$ + " TURNS THE BALL OVER"
    If RN = 2 Then U$ = U$ + " THROWS IT AWAY"
    If RN = 3 Then U$ = U$ + " IS WHISTLED FOR TRAVELING"
    If RN = 4 Then U$ = U$ + " TURNS IT OVER"
    If RN = 5 Then U$ = U$ + " LOSES CONTROL OF THE BALL"
    If RN = 6 Then U$ = U$ + " LOSES IT OUT OF BOUNDS"
    If RN = 7 Then U$ = U$ + " LOSES THE HANDLE"
    If RN = 8 Then U$ = U$ + " LOSES IT"
    If RN = 9 Then U$ = U$ + " IS CALLED FOR STEPS"
    If RN = 10 Then U$ = U$ + " TRAVELS"
    If RN = 11 Then U$ = U$ + " WALKS WITH IT"
    If RN = 12 Then U$ = U$ + " LETS IT GET AWAY"
    If RN = 13 Then U$ = U$ + " DOUBLE DRIBBLES"
    If RN = 14 Then U$ = U$ + " LOSES CONTROL"
    GoTo 1495
    1420 'DUNK
    RN = Int(Rnd(1) * 39) + 1
    If RN = 1 Then U$ = "DIPSY-DOO DUNK-A-ROO!!!"
    If RN = 2 Then U$ = AA$(p, B1(p, B)) + " MAKES IT HAMMER TIME!!!"
    If RN = 3 Then U$ = AA$(p, B1(p, B)) + " SLAMS IT IN!!!"
    If RN = 4 Then U$ = AA$(p, B1(p, B)) + " WITH THE 2-HANDED SLAM!!!"
    If RN = 5 Then U$ = AA$(p, B1(p, B)) + " SLAMS IT HOME!!!"
    If RN = 6 Then U$ = AA$(p, B1(p, B)) + " JAMS IT HOME!!!"
    If RN = 7 Then U$ = AA$(p, B1(p, B)) + " THROWS IT DOWN!!!"
    If RN = 8 Then U$ = AA$(p, B1(p, B)) + " THUNDERSLAM!!!"
    If RN = 9 Then U$ = "RIM ROCKING JAM!!!"
    If RN = 10 Then U$ = AA$(p, B1(p, B)) + " WITH THE MONSTER SLAM!!"
    If RN = 11 Then U$ = AA$(p, B1(p, B)) + "POWER SLAMS IT!!!"
    If RN = 12 Then U$ = AA$(p, B1(p, B)) + " WITH THE 2-HANDED STUFF!!!"
    If RN = 13 Then U$ = AA$(p, B1(p, B)) + " BACKWARDS JAM!!!"
    If RN = 14 Then U$ = AA$(p, B1(p, B)) + " WITH THE 1-HANDED SLAM!!!"
    If RN = 15 Then U$ = AA$(p, B1(p, B)) + " WITH THE REVERSE SLAM!!!"
    If RN = 16 Then U$ = AA$(p, B1(p, B)) + " WITH THE REVERSE JAM!!!"
    If RN = 17 Then U$ = AA$(p, B1(p, B)) + " WITH THE WINDMILL JAM!!!"
    If RN = 18 Then U$ = AA$(p, B1(p, B)) + " WITH THE WINDMILL SLAM!!!"
    If RN = 19 Then U$ = AA$(p, B1(p, B)) + " WITH THE TOMAHAWK SLAM!!!"
    If RN = 20 Then U$ = AA$(p, B1(p, B)) + " WITH THE 360 SLAM!!!"
    If RN = 21 Then U$ = AA$(p, B1(p, B)) + " WITH THE SPINNING SLAM!!!"
    If RN = 22 Then U$ = AA$(p, B1(p, B)) + " WITH THE LEFT HANDED DUNK!!!"
    If RN = 23 Then U$ = "DOUBLE CLUTCH JAM!!!"
    If RN = 24 Then U$ = AA$(p, B1(p, B)) + " GETS THE FLUSH!!!"
    If RN = 25 Then U$ = AA$(p, B1(p, B)) + " PUNCHES IT HOME!!!"
    If RN = 26 Then U$ = AA$(p, B1(p, B)) + "...DOUBLE PUMP SLAM!!!"
    If RN = 27 Then U$ = AA$(p, B1(p, B)) + "...IN YOUR FACE SLAM!!!"
    If RN = 28 Then U$ = AA$(p, B1(p, B)) + " IS UP": Call PBP: U$ = "HE SPINS": Call PBP: U$ = "BEHIND THE BACK SLAM!!!"


    If RN = 29 Then U$ = "HE SLAMS IT HOME!!"
    If RN = 30 Then U$ = "HE LAYS IT UP AND IN!!"
    If RN = 31 Then U$ = "HE BANGS IT IN!!"
    If RN = 32 Then U$ = "HE RAMS IT IN!!"
    If RN = 33 Then U$ = "HE SLAMS IT IN!!!"
    If RN = 34 Then U$ = "DIPSY-DOO DUNK-A-ROO!!!"
    If RN = 35 Then U$ = "HE MAKES IT HAMMER TIME!!!"
    If RN = 36 Then U$ = AA$(p, B1(p, B)) + " WITH THE 2-HANDED SLAM!!!"
    If RN = 37 Then U$ = AA$(p, B1(p, B)) + " IS UP": Call PBP: U$ = "HE SPINS": Call PBP: U$ = "BEHIND THE BACK SLAM!!!"


    If RN = 38 Then U$ = "HE JAMS IT HOME!!!"
    If RN = 39 Then U$ = "HE HAMMERS IT HOME!"
    GoTo 1495

    1427 If Int(Rnd(1) * 100) + 1 <= 2 Then DK% = 1
    Return
    1428 If (B = 0 Or B = 1) And Int(Rnd(1) * 100) + 1 <= 1 Then DK% = 1
    If (B = 2 Or B = 3) And Int(Rnd(1) * 100) + 1 <= 5 Then DK% = 1
    If B = 4 And Int(Rnd(1) * 100) + 1 <= 7 Then DK% = 1
    Return

    1430 'SHOOTS...PZ%567 SML
    U$ = AA$(p, B1(p, B))
    On PZ% GOTO 1431, 1432, 1433, 1434, 1435, 1436, 1437, 1438, 1439, 1446
    1431 'BAD
    If RN <= 40 Then U$ = U$ + " TAKES A TOUGH SHOT"
    If RN > 40 And RN <= 45 Then U$ = U$ + " SHOOTS OFF BALANCE"
    If RN > 45 And RN <= 50 Then U$ = U$ + " FORCES ONE UP"
    If RN > 50 And RN <= 60 Then U$ = U$ + " SHOOTS UNDER PRESSURE"
    If RN > 60 And RN <= 65 Then U$ = U$ + " WITH A BAD SHOT"
    If RN > 65 And RN <= 70 Then U$ = U$ + " SHOOTS IN HEAVY TRAFFIC"
    If RN > 70 And RN <= 75 Then U$ = U$ + " THROWS UP A PRAYER"
    If RN > 75 And RN <= 80 Then U$ = U$ + " TAKES A BAD SHOT"
    If RN > 80 And RN <= 85 Then U$ = U$ + " FORCES THE INSIDE SHOT"
    If RN > 85 Then U$ = U$ + " TURNS AND SHOOTS"
    GoTo 1495
    1432 'INSIDE W/ASSIST

    If RN <= 10 Then U$ = "HE CUTS TO THE BASKET": Call PBP: U$ = "TAKES IT UP": GoSub 1428: GoTo 1495
    If RN <= 20 Then U$ = "HE TAKES IT TO THE HOLE": GoSub 1428: GoTo 1495
    If RN <= 30 Then U$ = "HE TAKES IT TO THE HOOP": GoSub 1428: GoTo 1495
    If RN <= 40 Then U$ = "HE LEANS IN AND SHOOTS": GoTo 1495
    If RN <= 50 Then U$ = "HE PUTS IT UP": GoTo 1495
    If RN <= 60 Then U$ = "HE TAKES IT TO THE BOARD": GoSub 1428: GoTo 1495
    If RN <= 70 Then U$ = "HE PUTS IT OFF THE GLASS": GoTo 1495
    If RN <= 80 And B = 4 Then U$ = "HE SHOOTS THE HOOK": GoTo 1495
    If RN <= 80 And B <> 4 Then U$ = "HE GOES IN FOR THE LAYUP": GoTo 1495
    If RN <= 90 Then U$ = "HE TAKES IT UP STRONG": GoSub 1428: GoTo 1495
    If RN <= 95 Then U$ = "HE POWERS IT UP": GoSub 1428: GoTo 1495
    U$ = "HE LAYS IT UP": GoTo 1495
    1433 'PENETRATE
    If B = 4 And RN <= 15 Then U$ = "HE PUTS IT OFF THE GLASS": GoTo 1495
    If B = 4 And RN <= 25 Then U$ = "HE PIVOTS TO THE BASKET": GoTo 1495
    If B = 4 And RN <= 40 Then U$ = "HE POWERS IT UP": GoTo 1495
    If B = 4 And RN <= 58 Then U$ = "HE TAKES IT TO THE HOOP": GoSub 1425: GoTo 1495
    If B = 4 And RN <= 70 Then U$ = "HE TAKES THE SHOT": GoTo 1495
    If B = 4 And RN <= 72 Then U$ = "HE CURLS OFF A PICK IN THE PAINT": Call PBP: U$ = "TAKES IT UP": GoSub 1425: GoTo 1495


    If B = 4 And RN <= 74 Then U$ = "HE MAKES A NICE SPIN MOVE": Call PBP: U$ = "AND SOARS TO THE HOOP": GoSub 1425: GoTo 1495
    If B = 4 Then U$ = "HE TAKES IT UP STRONG": GoSub 1425: GoTo 1495
    If (B = 2 Or B = 3) And RN <= 16 Then U$ = "HE TAKES IT UP": GoSub 1426: GoTo 1495
    If (B = 2 Or B = 3) And RN <= 32 Then U$ = "HE SPINS FREE": Call PBP: U$ = "SHOOTS": GoSub 1426: GoTo 1495

    If (B = 2 Or B = 3) And RN <= 48 Then U$ = "HE GETS LOOSE IN THE PAINT": Call PBP: U$ = "TAKES IT UP": GoSub 1426: GoTo 1495

    If (B = 2 Or B = 3) And RN <= 64 Then U$ = "HE PUTS IT UP": GoTo 1495
    If (B = 2 Or B = 3) And RN <= 67 Then U$ = "HE CURLS OFF A PICK IN THE PAINT": Call PBP: U$ = "TAKES IT UP": GoSub 1426: GoTo 1495

    If (B = 2 Or B = 3) And RN <= 69 Then U$ = "HE MAKES A NICE SPIN MOVE": Call PBP: U$ = "AND SOARS TO THE HOOP": GoSub 1426: GoTo 1495

                                                                       
    If (B = 2 Or B = 3) And RN <= 71 Then U$ = "HE DRIVES THE BASELINE": Call PBP: U$ = "SPINS RIGHT AND GOES UP": GoSub 1426: GoTo 1495
    If (B = 2 Or B = 3) And RN = 72 Then U$ = "HE SLICES IN THE HOLE": Call PBP: U$ = "UNDERHAND SCOOP SHOT": GoTo 1495

    If (B = 2 Or B = 3) And RN = 73 Then U$ = "STRONG CUT ALONG THE BASELINE": Call PBP: U$ = "DOUBLE PUMP REVERSE LAYUP": GoTo 1495
                                                         
                                                             
    If (B = 2 Or B = 3) And RN = 74 Then U$ = "HE PENETRATES": Call PBP: U$ = "GORGEOUS FINGER ROLL!": GoTo 1495
    If (B = 2 Or B = 3) And RN > 80 Then U$ = "HE SHOOTS": GoTo 1495
    If (B = 2 Or B = 3) Then U$ = "HE TAKES THE SHOT": GoTo 1495
                                         
    If RN <= 25 Then U$ = "HE SPINS FREE": Call PBP: U$ = "SHOOTS": GoTo 1495
    If RN <= 50 Then U$ = "HE TAKES IT UP UNDER THE BASKET": GoSub 1427: GoTo 1495
                                                     
    If RN <= 52 Then U$ = "HE MAKES A NICE SPIN MOVE": Call PBP: U$ = "AND SOARS TO THE HOOP": GoSub 1427: GoTo 1495
    If RN <= 54 Then U$ = "HE BLOWS PAST TWO DEFENDERS": GoTo 1495
                                                  
    If RN <= 56 Then U$ = "HE DRIVES THE BASELINE": Call PBP: U$ = "SPINS RIGHT AND GOES UP": GoSub 1427: GoTo 1495
                                                
    If RN = 57 Then U$ = "HE SLICES IN THE HOLE": Call PBP: U$ = "UNDERHAND SCOOP SHOT": GoTo 1495
                                                        
    If RN = 58 Then U$ = "STRONG CUT ALONG THE BASELINE": Call PBP: U$ = "DOUBLE PUMP REVERSE LAYUP": GoTo 1495
                                        
    If RN = 59 Then U$ = "HE PENETRATES": Call PBP: U$ = "GORGEOUS FINGER ROLL!": GoTo 1495
    If RN <= 75 Then U$ = "HE TAKES THE JUMPER": GoTo 1495
                                    
    If RN <= 90 Then U$ = "HE STOPS": Call PBP: U$ = "SHOOTS": GoTo 1495
                                            
                                   
    If RN <= 91 Then U$ = "HE LOOKS TO PASS": Call PBP: U$ = "NO ONE IS OPEN": Call PBP: U$ = "HE PUTS IT UP": GoTo 1495
    U$ = "HE TAKES THE SHOT": GoTo 1495

    1425 If Int(Rnd(1) * 100) + 1 <= 15 Then DK% = 1
    Return
    1426 If Int(Rnd(1) * 100) + 1 <= 7 Then DK% = 1
    Return
    1434 'THREE!
    If FT% = 1 And RN <= 50 Then U$ = U$ + " THROWS UP A PRAYER"
    If FT% = 1 And RN > 50 Then U$ = U$ + " LAUNCHES A THREE"
    If FT% = 1 Then FT% = 0: GoTo 1495
    If RN <= 10 Then U$ = U$ + " TRIES FOR THREE!"
    If RN > 10 And RN <= 20 Then U$ = U$ + " SHOOTS FROM 3-POINT RANGE!"
    If RN > 20 And RN <= 30 Then U$ = U$ + " HOISTS A THREE!"
    If RN > 30 And RN <= 40 Then U$ = U$ + " BOMBS A THREE FROM THE CORNER!"
    If RN > 40 And RN <= 50 Then U$ = U$ + " FIRES FROM THE 3-POINT LINE!"
    If RN > 50 And RN <= 60 Then U$ = U$ + " SHOOTS FOR THREE!"
    If RN > 60 And RN <= 70 Then U$ = U$ + " SHOOTS THE 3-POINTER!"
    If RN > 70 And RN <= 80 Then U$ = U$ + " FOR 3...!"
    If RN > 80 And RN <= 90 Then U$ = U$ + " PUTS UP THE TREY!"
    If RN > 90 Then U$ = U$ + " TAKES THE 3-POINTER!"
    GoTo 1495

    1435 'SHORT JUMP
    DK% = 0
    RN = Int(Rnd(1) * 59) + 1
    If RN = 1 Then U$ = U$ + " WITH A TWISTING JUMPER"
    If RN = 2 Then U$ = U$ + " SHOOTS THE J"
    If RN = 3 And B = 4 Then U$ = U$ + " RELEASES THE HOOK"
    If RN = 3 And B <> 4 Then U$ = U$ + " TAKES THE SHOT"
    If RN = 4 Then U$ = U$ + " WITH A SHORT JUMPER"
    If RN = 5 Then U$ = U$ + " WITH THE 10 FOOTER"
    If RN = 6 And B <> 4 Then U$ = U$ + " WITH THE RUNNER"
    If RN = 6 And B = 4 Then U$ = U$ + " WITH THE RUNNING HOOK"
    If RN = 7 And B = 4 Then U$ = U$ + " WITH A SWEEPING HOOK"
    If RN = 7 And B <> 4 Then U$ = U$ + " WORKS FREE AND SHOOTS"
    If RN = 8 And B <> 4 Then U$ = U$ + " TAKES THE SHOT"
    If RN = 8 And B = 4 Then U$ = U$ + " WITH A BABY HOOK SHOT"
    If RN = 9 Then U$ = U$ + " LEANS IN AND SHOOTS"
    If RN = 10 Then U$ = U$ + " SHOOTS"
    If RN = 11 Then U$ = U$ + " SHOOTS FALLING AWAY"
    If RN = 12 Then U$ = U$ + " LEANS IN FOR THE SHOT"
    If RN = 13 Then U$ = U$ + " TAKES THE LEFT HANDER"
    If RN = 14 Then U$ = U$ + " HARD TO THE HOLE..."
    If RN = 15 Then U$ = U$ + " FLIPS IT UP"
    If RN = 16 Then U$ = U$ + " FORCES THE INSIDE SHOT"
    If RN = 17 Then U$ = U$ + " CUTS TO THE BASKET"
    If RN = 18 Then U$ = U$ + " TAKES IT IN THE PAINT"
    If RN = 19 Then U$ = U$ + " FLOATS IT OVER HIS MAN"
    If RN = 20 Then U$ = U$ + " TAKES HIS MAN INSIDE"
    If RN = 21 Then U$ = U$ + " WHEELS IN THE LANE"
    If RN = 22 Then U$ = U$ + " CUTS INSIDE FOR THE SHOT"
    If RN = 23 Then U$ = U$ + " PENETRATES AND SHOOTS"
    If RN = 24 Then U$ = U$ + " SLICES INSIDE"
    If RN = 25 Then U$ = U$ + " LOOKS FOR TWO"
    If RN = 26 Then U$ = U$ + " DRIVES INTO THE PAINT"
    If RN = 27 Then U$ = U$ + " HAS THE BASELINE JUMPER"
    If RN = 28 Then U$ = U$ + " GLIDES TO THE HOLE"
    If RN = 29 Then U$ = U$ + " GETS INSIDE FOR THE SHOT"
    If RN = 30 Then U$ = U$ + " SPINS TOWARD THE HOOP"
    If RN = 31 Then U$ = U$ + " FROM SHORT RANGE"
    If RN = 32 Then U$ = U$ + " GOES BASELINE"
    If RN = 33 Then U$ = U$ + " PENETRATES AND SHOOTS"
    If RN = 34 Then U$ = U$ + " FROM 8 FEET"
    If RN = 35 Then U$ = U$ + " FROM 6 FEET"
    If RN = 36 Then U$ = U$ + " FROM 5 FEET"
    If RN = 37 Then U$ = U$ + " DRIVES TO THE HOLE"
    If RN = 38 Then U$ = U$ + " GLIDES TO THE HOLE"
    If RN = 39 Then U$ = U$ + " GETS INSIDE FOR THE SHOT"
    If RN = 40 Then U$ = U$ + " GOES TO THE HOLE"
    If RN = 41 Then U$ = U$ + " CUTS INSIDE"
    If RN = 42 Then U$ = U$ + " SPINS INSIDE"
    If RN = 43 Then U$ = U$ + " BREAKS INSIDE"
    If RN = 44 Then U$ = U$ + " SNEAKS INSIDE"
    If RN = 45 Then U$ = U$ + " SCOOTS INSIDE"
    If RN = 46 Then U$ = U$ + " WORKS IT INSIDE"
    If RN = 47 Then U$ = U$ + " KNIFES INSIDE"
    If RN = 48 Then U$ = U$ + " SCISSORS THROUGH"
    If RN = 49 Then U$ = U$ + " BANGS INSIDE"
    If RN = 50 Then U$ = U$ + " SLICES INSIDE"
    If RN = 51 Then U$ = U$ + " PUMPS AND SHOOTS"
    If RN = 52 Then U$ = U$ + " DOUBLE PUMPS AND SHOOTS"
    If RN = 53 Then U$ = U$ + "...ONE HANDED FADE AWAY"
    If RN = 54 Then U$ = U$ + "...LEFT HANDED FADE AWAY"
    If RN = 55 Then U$ = U$ + "...HIGH ARCHING SHOT"
    If RN = 56 Then U$ = U$ + " WITH A RUNNING LEFT HANDER"
    If RN = 57 Then U$ = U$ + " WITH A RUNNING SCOOP SHOT"
    If RN = 58 Then U$ = U$ + " WITH THE SCOOP SHOT"
    If RN = 59 Then U$ = U$ + " WITH THE FINGER ROLL"

    GoTo 1495

    1436 DK% = 0
    RN = Int(Rnd(1) * 59) + 1
    If RN = 1 Then U$ = U$ + " POPS FROM OUTSIDE"
    If RN = 2 Then U$ = U$ + " DECIDES TO SHOOT IT"
    If RN = 3 Then U$ = U$ + " FIRES THE FADE-AWAY"
    If RN = 4 Then U$ = U$ + " LETS IT FLY"
    If RN = 5 Then U$ = U$ + " LAUNCHES ONE"
    If RN = 6 Then U$ = U$ + " WITH THE OUTSIDE SHOT"
    If RN = 7 Then U$ = U$ + " OUT OF THE CORNER"
    If RN = 8 Then U$ = U$ + " PUTS IT UP FROM 18"
    If RN = 9 Then U$ = U$ + " SPOTS UP OUTSIDE"
    If RN = 10 Then U$ = U$ + " RELEASES THE JUMPER"
    If RN = 11 Then U$ = U$ + " FROM THE TOP OF THE KEY"
    If RN = 12 Then U$ = U$ + " FIRES FROM THE LEFT WING"
    If RN = 13 Then U$ = U$ + " TRIES THE 12 FOOTER"
    If RN = 14 Then U$ = U$ + " PULLS UP AND POPS"
    If RN = 15 Then U$ = U$ + " FIRES FROM 15"
    If RN = 16 Then U$ = U$ + " FROM THE PERIMETER"
    If RN = 17 Then U$ = U$ + " TURNS AND SHOOTS"
    If RN = 18 Then U$ = U$ + " SHOOTS OFF THE SCREEN"
    If RN = 19 Then U$ = U$ + " FAKES AND SHOOTS"
    If RN = 20 Then U$ = U$ + " FIRES AWAY"
    If RN = 21 Then U$ = U$ + " POPS FROM THE BASELINE"
    If RN = 22 Then U$ = U$ + " DRIVES THE LANE"
    If RN = 23 Then U$ = U$ + " TAKES THE SHOT"
    If RN = 24 Then U$ = U$ + " WORKS FREE AND FIRES"
    If RN = 25 Then U$ = U$ + " SHOOTS OVER HIS MAN"
    If RN = 26 Then U$ = U$ + " SHOOTS FALLING AWAY"
    If RN = 27 Then U$ = U$ + " FAKES AND SHOOTS"
    If RN = 28 Then U$ = U$ + " FIRES AWAY"
    If RN = 29 Then U$ = U$ + " POPS FROM THE BASELINE"
    If RN = 30 Then U$ = U$ + " DRIVES THE LANE"
    If RN = 31 Then U$ = U$ + " TAKES THE SHOT"
    If RN = 32 Then U$ = U$ + " WORKS FREE AND FIRES"
    If RN = 33 Then U$ = U$ + " SHOOTS FROM 15"
    If RN = 34 Then U$ = U$ + " SHOOTS"
    If RN = 35 Then U$ = U$ + " TAKES THE JUMPER"
    If RN = 36 Then U$ = U$ + " FROM THE RIGHT CORNER"
    If RN = 37 Then U$ = U$ + " FROM THE LEFT CORNER"
    If RN = 38 Then U$ = U$ + " FROM THE KEY"
    If RN = 39 Then U$ = U$ + " FIRES FROM OUTSIDE"
    If RN = 40 Then U$ = U$ + " PUTS IT UP FROM 16"
    If RN = 41 Then U$ = U$ + " FROM THE PERIMETER"
    If RN = 42 Then U$ = U$ + " FAKES AND SHOOTS"
    If RN = 43 Then U$ = U$ + " FROM AROUND 16 FEET"
    If RN = 44 Then U$ = U$ + " PUTS IT UP"
    If RN = 45 Then U$ = U$ + " WITH A RUNNING ONE-HANDER"
    If RN = 46 Then U$ = U$ + " PULLS UP"
    If RN = 47 Then U$ = U$ + " KISSES THE GLASS"
    If RN = 48 Then U$ = U$ + " SPINS AND SHOOTS"
    If RN = 49 Then U$ = U$ + " PUTS IT UP FROM 16"
    If RN = 50 Then U$ = U$ + " WITH A LONG JUMPER"
    If RN = 51 Then U$ = U$ + " PUMPS AND SHOOTS"
    If RN = 52 Then U$ = U$ + " DOUBLE PUMPS AND SHOOTS"
    If RN = 53 Then U$ = U$ + "...ONE HANDED FADE AWAY"
    If RN = 54 Then U$ = U$ + "...LEFT HANDED FADE AWAY"
    If RN = 55 Then U$ = U$ + "...HIGH ARCHING SHOT"
    If RN = 56 Then U$ = U$ + " WITH A RUNNING LEFT HANDER"
    If RN = 57 Then U$ = U$ + " WITH A RUNNING SCOOP SHOT"
    If RN = 58 Then U$ = U$ + " WITH THE SCOOP SHOT"
    If RN = 59 Then U$ = U$ + " WITH THE FINGER ROLL"

    GoTo 1495

    1437 DK% = 0
    If RN <= 10 Then U$ = U$ + " FIRES FROM THE PERIMETER": GoTo 1495
    If RN <= 20 Then U$ = U$ + " SHOOTS FROM 17": GoTo 1495
    If RN <= 30 Then U$ = U$ + " PUTS IT UP FROM 17": GoTo 1495
    If RN <= 40 Then U$ = U$ + " PUTS IT UP": GoTo 1495
    If RN <= 45 Then U$ = U$ + " FROM INSIDE THE THE KEY": GoTo 1495
    If RN <= 50 Then U$ = U$ + " FIRES FROM THE CORNER": GoTo 1495
    If RN <= 60 Then U$ = U$ + " SHOOTS FROM OUTSIDE": GoTo 1495
    If RN <= 70 Then U$ = U$ + " PUMPS AND SHOOTS": GoTo 1495
    If RN <= 80 Then U$ = U$ + " FIRES THE FADE-AWAY": GoTo 1495
    If RN <= 90 And X3% = 1 Then U$ = U$ + " FROM INSIDE THE 3-POINT LINE": GoTo 1495
    If RN <= 91 And X3% = 1 Then U$ = U$ + " FOR THREE!": Call PBP: U$ = "NO!": Call PBP: U$ = "HIS FOOT IS ON THE 3-POINT LINE": GoTo 1495
    U$ = U$ + " SHOOTS FROM THE WING"
    GoTo 1495

    1438 If RN <= 20 Then U$ = "HE SAILS TO THE HOOP": GoTo 1495
    If RN <= 23 Then U$ = "HE TAKES IT TO THE BUCKET": GoTo 1495
    If RN <= 26 Then U$ = "HE TAKES IT TO THE HOLE": GoTo 1495
    If RN <= 29 Then U$ = "HE SLASHES TO THE HOLE": GoTo 1495
    If RN <= 32 Then U$ = "HE SLASHES TO THE BUCKET": GoTo 1495
    If RN <= 35 Then U$ = "HE SOARS TO THE BUCKET": GoTo 1495
    If RN <= 38 Then U$ = "HE SOARS TO THE HOLE": GoTo 1495
    If RN <= 40 Then U$ = "HE GLIDES TO THE HOLE": GoTo 1495
    If RN <= 60 Then U$ = "HE GOES IN STRONG": GoTo 1495
    If RN <= 80 Then U$ = "HE TAKES IT IN": GoTo 1495
    If RN <= 90 Then U$ = "HE TAKES A DEFENDER WITH HIM": GoTo 1495
    U$ = "HE GOES UP WITH IT": GoTo 1495

    1439 'BACK AFTER REB
    U$ = "HE "
    RN = Int(Rnd(1) * 11) + 1
    If RN = 1 Then U$ = U$ + "TAKES IT BACK UP"
    If RN = 2 Then U$ = U$ + "GOES BACK UP WITH IT"
    If RN = 3 Then U$ = U$ + "POWERS HIS WAY UP WITH IT"
    If RN = 4 Then U$ = U$ + "GOES STRONG TO THE BASKET"
    If RN = 5 Then U$ = U$ + "MUSCLES IT UP"
    If RN = 6 Then U$ = U$ + "SHOOTS"
    If RN = 7 Then U$ = U$ + "TAKES IT TO THE GOAL"
    If RN = 8 Then U$ = U$ + "PUTS IT UP AGAIN"
    If RN = 9 Then U$ = U$ + "SHOOTS IT AGAIN"
    If RN = 10 Then U$ = U$ + "CLAWS HIS WAY UP"
    If RN = 11 Then U$ = U$ + "SHOOTS IN TRAFFIC"

    If B = 2 Or B = 3 And Int(Rnd(1) * 100) + 1 <= 7 Then DK% = 1
    If B = 4 And Int(Rnd(1) * 100) + 1 <= 15 Then DK% = 1
    GoTo 1495

    1446 RN = Int(Rnd(1) * 100) + 1
    DK% = 0
    If RN <= 20 Then U$ = U$ + " SHOOTS FROM 15": GoTo 1495
    If RN <= 40 Then U$ = U$ + " SHOOTS": GoTo 1495
    If RN <= 60 Then U$ = U$ + " TAKES THE JUMPER": GoTo 1495
    If RN <= 80 Then U$ = U$ + " PUTS IT UP FROM 16": GoTo 1495
    U$ = U$ + " FAKES AND SHOOTS": GoTo 1495

    1440 'THREE GOOD
    U$ = "HE "
    If RN <= 6 Then U$ = U$ + " DRAINS THE THREE!": Call PBP: U$ = "NO!": Call PBP: U$ = "HIS FOOT IS ON THE 3-POINT LINE": XX% = 1: GoTo 1495
    If RN <= 10 Then U$ = U$ + "FILLS UP THE BASKET!!!": GoTo 1495
    If RN <= 20 Then U$ = U$ + "HITS THE TREY!!!": GoTo 1495
    If RN <= 30 Then U$ = "IT'S GOOOODD!!": GoTo 1495
    If RN <= 40 Then U$ = U$ + "GETS IT!!!": GoTo 1495
    If RN <= 45 Then U$ = "NOTHING BUT THE BOTTOM OF THE NET": GoTo 1495
    If RN <= 50 Then U$ = "COUNT IT!!": GoTo 1495
    If RN <= 55 Then U$ = "WAY DOWNTOWN BANG!!!": GoTo 1495
    If RN <= 60 Then U$ = U$ + "RINGS IT UP!!!": GoTo 1495
    If RN <= 80 Then U$ = U$ + "DRAINS IT!!!": GoTo 1495
    If RN <= 90 Then U$ = "HE BURIES IT!!!": GoTo 1495
    U$ = "IT'S ALL NET!!!"
    GoTo 1495

    1445 'THREE BAD
    U$ = "HE "
    If RN <= 20 Then U$ = U$ + "GETS NONE": GoTo 1495
    If RN <= 25 Then U$ = "IT NEVER HAD A CHANCE": GoTo 1495
    If RN <= 35 Then U$ = "AIRRRR BALLLL!!": GoTo 1495
    If RN <= 40 Then U$ = U$ + "MISFIRES": GoTo 1495
    If RN <= 60 Then U$ = "IT'S OFF THE RIM...NO GOOD!": GoTo 1495
    If RN <= 80 Then U$ = "IT'S SHORT": GoTo 1495
    U$ = U$ + "MISSES": GoTo 1495

    1450 'FAST BREAK
    U$ = AA$(p, B1(p, B))
    RN = Int(Rnd(1) * 38) + 1
    If RN = 1 Then U$ = U$ + " HAS IT ON THE FAST BREAK!"
    If RN = 2 Then U$ = U$ + " IS IN TRANSITION!"
    If RN = 3 Then U$ = U$ + " STARTS THE BREAK!"
    If RN = 4 Then U$ = U$ + " IS DOWN COURT IN A HURRY!"
    If RN = 5 Then U$ = U$ + " IS ON THE RUN!"
    If RN = 6 Then U$ = U$ + " GOES COAST TO COAST!"
    If RN = 7 Then U$ = U$ + " ON THE BREAK!"
    If RN = 8 Then U$ = U$ + " ON THE RUN!"
    If RN = 9 Then U$ = U$ + " ON THE FAST BREAK!"
    If RN = 10 Then U$ = U$ + " HAS THE FAST BREAK!"
    If RN = 11 Then U$ = U$ + " BREAKS INTO THE FORECOURT!"
    If RN = 12 Then U$ = U$ + " FLIES UPCOURT!"
    If RN = 13 Then U$ = U$ + " STARTS THE FASTBREAK!"
    If RN = 14 Then U$ = U$ + " ON THE MOVE!"
    If RN = 15 Then U$ = U$ + " IN TRANSITION!"
    If RN = 16 Then U$ = U$ + " DOWN COURT IN A HURRY!"
    If RN = 17 Then U$ = U$ + " CHARGES ACROSS THE LINE!"
    If RN = 18 Then U$ = U$ + " BREAKS QUICKLY!"
    If RN = 19 Then U$ = U$ + " BREAKS DOWN COURT!"
    If RN = 20 Then U$ = U$ + " STARTS THE RUNNING GAME!"
    If RN = 21 Then U$ = U$ + " FLIES DOWN THE FLOOR!"
    If RN = 22 Then U$ = U$ + " GOES RIGHT TO WORK!"
    If RN = 23 Then U$ = U$ + " BACK THE OTHER WAY!"
    If RN = 24 Then U$ = U$ + " QUICKLY UP THE FLOOR!"
    If RN = 25 Then U$ = U$ + " PUSHES IT UP COURT!"
    If RN = 26 Then U$ = U$ + " WASTES NO TIME!"
    If RN = 27 Then U$ = U$ + " UPS THE TEMPO!"
    If RN = 28 Then U$ = U$ + " HUSTLES DOWN COURT!"
    If RN = 29 Then U$ = U$ + " RUSHES UP THE FLOOR!"
    If RN = 30 Then U$ = U$ + " ON THE MOVE!"
    If RN = 31 Then U$ = U$ + " STREAKS UP THE COURT!"
    If RN = 32 Then U$ = U$ + " FLIES DOWN THE COURT!"
    If RN = 33 Then U$ = U$ + " JETS DOWN COURT!"
    If RN = 34 Then U$ = U$ + " QUICKLY GETS IT UP COURT!"
    If RN = 35 Then U$ = U$ + " TAKES IT COAST TO COAST!"
    If RN = 36 Then U$ = U$ + " IS RUNNING AND GUNNING!"
    If RN = 37 Then U$ = U$ + " FLIES OVER THE TIME LINE!"
    If RN = 38 Then U$ = U$ + " RUSHES IT UP THE FLOOR!"
    Call SHOTSTRING
    GoTo 1495

    1460 'FAST BREAK SCORE
    FB% = 0
    RN = Int(Rnd(1) * 39) + 1
    If RN = 1 Then U$ = "DIPSY-DOO DUNK-A-ROO!!!"
    If RN = 2 Then U$ = AA$(p, B1(p, B)) + " MAKES IT HAMMER TIME!!!"
    If RN = 3 Then U$ = AA$(p, B1(p, B)) + " SLAMS IT IN!!!"
    If RN = 4 Then U$ = AA$(p, B1(p, B)) + " WITH THE 2-HANDED SLAM!!!"
    If RN = 5 Then U$ = AA$(p, B1(p, B)) + " SLAMS IT HOME!!!"
    If RN = 6 Then U$ = AA$(p, B1(p, B)) + " JAMS IT HOME!!!"
    If RN = 7 Then U$ = AA$(p, B1(p, B)) + " THROWS IT DOWN!!!"
    If RN = 8 Then U$ = AA$(p, B1(p, B)) + " THUNDERSLAM!!!"
    If RN = 9 Then U$ = "RIM ROCKING JAM!!!"
    If RN = 10 Then U$ = AA$(p, B1(p, B)) + " WITH THE MONSTER SLAM!!"
    If RN = 11 Then U$ = AA$(p, B1(p, B)) + "POWER SLAMS IT!!!"
    If RN = 12 Then U$ = AA$(p, B1(p, B)) + " WITH THE 2-HANDED STUFF!!!"
    If RN = 13 Then U$ = AA$(p, B1(p, B)) + " BACKWARDS JAM!!!"
    If RN = 14 Then U$ = AA$(p, B1(p, B)) + " WITH THE 1-HANDED SLAM!!!"
    If RN = 15 Then U$ = AA$(p, B1(p, B)) + " WITH THE REVERSE SLAM!!!"
    If RN = 16 Then U$ = AA$(p, B1(p, B)) + " WITH THE REVERSE JAM!!!"
    If RN = 17 Then U$ = AA$(p, B1(p, B)) + " WITH THE WINDMILL JAM!!!"
    If RN = 18 Then U$ = AA$(p, B1(p, B)) + " WITH THE WINDMILL SLAM!!!"
    If RN = 19 Then U$ = AA$(p, B1(p, B)) + " WITH THE TOMAHAWK SLAM!!!"
    If RN = 20 Then U$ = AA$(p, B1(p, B)) + " WITH THE 360 SLAM!!!"
    If RN = 21 Then U$ = AA$(p, B1(p, B)) + " WITH THE SPINNING SLAM!!!"
    If RN = 22 Then U$ = AA$(p, B1(p, B)) + " WITH THE LEFT HANDED DUNK!!!"
    If RN = 23 Then U$ = "DOUBLE CLUTCH JAM!!!"
    If RN = 24 Then U$ = AA$(p, B1(p, B)) + " GETS THE FLUSH!!!"
    If RN = 25 Then U$ = AA$(p, B1(p, B)) + " PUNCHES IT HOME!!!"
    If RN = 26 Then U$ = AA$(p, B1(p, B)) + "...DOUBLE PUMP SLAM!!!"
    If RN = 27 Then U$ = AA$(p, B1(p, B)) + "...IN YOUR FACE SLAM!!!"
    If RN = 28 Then U$ = AA$(p, B1(p, B)) + " IS UP": Call PBP: U$ = "HE SPINS": Call PBP: U$ = "BEHIND THE BACK SLAM!!!"
    If RN = 29 Then U$ = "HE SLAMS IT HOME!!"
    If RN = 30 Then U$ = "HE LAYS IT UP AND IN!!"
    If RN = 31 Then U$ = "HE BANGS IT IN!!"
    If RN = 32 Then U$ = "HE RAMS IT IN!!"
    If RN = 33 Then U$ = "HE SLAMS IT IN!!!"
    If RN = 34 Then U$ = "DIPSY-DOO DUNK-A-ROO!!!"
    If RN = 35 Then U$ = "HE MAKES IT HAMMER TIME!!!"
    If RN = 36 Then U$ = AA$(p, B1(p, B)) + " WITH THE 2-HANDED SLAM!!!"
    If RN = 37 Then U$ = AA$(p, B1(p, B)) + " IS UP": Call PBP: U$ = "HE SPINS": Call PBP: U$ = "BEHIND THE BACK SLAM!!!"
    If RN = 38 Then U$ = "HE JAMS IT HOME!!!"
    If RN = 39 Then U$ = "HE HAMMERS IT HOME!"
    GoTo 1495

    1470 'MISS
    If PZ% = 8 Then PZ% = 0: FB% = 0: GoTo 1475
    RN = Int(Rnd(1) * 50) + 1
    If RN = 1 Then U$ = "IT RATTLES IN AND OUT...NO GOOD"
    If RN = 2 Then U$ = "OFF THE IRON"
    If RN = 3 Then U$ = "IT SPINS IN AND OUT"
    If RN = 4 Then U$ = "IT'S OFF THE GLASS"
    If RN = 5 Then U$ = "IT'S NO GOOD"
    If RN = 6 Then U$ = "IT'S OFF THE MARK"
    If RN = 7 Then U$ = "IT'S A BRICK"
    If RN = 8 Then U$ = "OFF THE RIM"
    If RN = 9 Then U$ = "NOT EVEN CLOSE!"
    If RN = 10 Then U$ = "NO GOOD"
    If RN = 11 Then U$ = "HE MISSES"
    If RN = 12 Then U$ = "DOESN'T GET THE ROLL"
    If RN = 13 Then U$ = "OFF THE GLASS"
    If RN = 14 Then U$ = "FALLS SHORT"
    If RN = 15 Then U$ = "NO GOOD"
    If RN = 16 Then U$ = "NO"
    If RN = 17 Then U$ = "HE MISSES"
    If RN = 18 Then U$ = "IN AND OUT"
    If RN = 19 Then U$ = "OFF THE RIM"
    If RN = 20 Then U$ = "OFF THE IRON"
    If RN = 21 Then U$ = "IT'S NOT THERE"
    If RN = 22 Then U$ = "OFF TARGET"
    If RN = 23 Then U$ = "IT RATTLES OUT"
    If RN = 24 Then U$ = "IT DOESN'T FALL"
    If RN = 25 Then U$ = "IT BOUNCES OFF"
    If RN = 26 Then U$ = "IT ROLLS OFF"
    If RN = 27 Then U$ = "SHORT"
    If RN = 28 Then U$ = "IT DOESN'T GO DOWN"
    If RN = 29 Then U$ = "IT'S NO GOOD"
    If RN = 30 Then U$ = "NO GOOD"
    If RN = 31 Then U$ = "HE MISSES"
    If RN = 32 Then U$ = "IT'S A BRICK"
    If RN = 33 Then U$ = "HE FAILS TO CONNECT"
    If RN = 34 Then U$ = "OFF THE SIDE OF THE RIM"
    If RN = 35 Then U$ = "IT COMES UP SHORT"
    If RN = 36 Then U$ = "IT ROLLS OUT"
    If RN = 37 Then U$ = "AROUND AND OUT"
    If RN = 38 Then U$ = "OFF THE BACK OF THE IRON"
    If RN = 39 Then U$ = "ON THE FRONT RIM...ROLLS OFF"
    If RN = 40 Then U$ = "IN THE AIR...NO GOOD"
    If RN = 41 Then U$ = "TOO SHORT"
    If RN = 42 Then U$ = "NO GOOD"
    If RN = 43 Then U$ = "IT COMES UP SHORT"
    If RN = 44 Then U$ = "IT ROLLS OUT"
    If RN = 45 Then U$ = "IN AND OUT"
    If RN = 46 Then U$ = "OFF THE FRONT OF THE IRON"
    If RN = 47 Then U$ = "ON THE RIM...ROLLS OFF"
    If RN = 48 Then U$ = "OFF THE FRONT OF THE RIM"
    If RN = 49 Then U$ = "TOO SHORT"
    If RN = 50 Then U$ = "TOO STRONG"

    GoTo 1495

    1475 U$ = "HE "
    If RN <= 20 Then U$ = U$ + "MISSES THE EASY BASKET!": GoTo 1495
    If RN <= 40 Then U$ = U$ + "JAMS IT TOO HARD!!": GoTo 1495
    If RN <= 60 Then U$ = U$ + "SLAMS IT OFF THE IRON...NO GOOD!": GoTo 1495
    If RN <= 80 Then U$ = U$ + "CAN'T GET IT DOWN!": GoTo 1495
    U$ = U$ + "BLOWS THE EASY BASKET!!": GoTo 1495

    1480 'NOPASS
    U$ = AA$(p, B1(p, B)) + " "
    RN = Int(Rnd(1) * 56) + 1

    If RN = 1 Then U$ = U$ + "LOOKS INSIDE"
    If RN = 2 Then U$ = U$ + "TRIES TO WORK FREE"
    If RN = 3 Then U$ = U$ + "SETS UP THE PLAY"
    If RN = 4 Then U$ = U$ + "LOOKS FOR AN OPENING"
    If RN = 5 Then U$ = U$ + "DRIBBLES AROUND HIS MAN"
    If RN = 6 Then U$ = U$ + "SIGNALS THE PLAY"
    If RN = 7 Then U$ = U$ + "IS IN CONTROL"
    If RN = 8 Then U$ = U$ + "IS IN POSSESSION"
    If RN = 9 Then U$ = U$ + "TRIES TO GET OPEN"
    If RN = 10 Then U$ = U$ + "YO-YO'S OUT TOP"
    If RN = 11 Then U$ = U$ + "GOES TO WORK"
    If RN = 12 Then U$ = U$ + "WORKS OFF THE PICK"
    If RN = 13 Then U$ = U$ + "ON THE DRIBBLE"
    If RN = 14 Then U$ = U$ + "CUTS TO THE LEFT"
    If RN = 15 Then U$ = U$ + "CUTS TO THE RIGHT"
    If RN = 16 Then U$ = U$ + "WHEELS AND DEALS"
    If RN = 17 Then U$ = U$ + "SETS UP ON TOP"
    If RN = 18 Then U$ = U$ + "SLIDES RIGHT"
    If RN = 19 Then U$ = U$ + "SLIDES LEFT"
    If RN = 20 Then U$ = U$ + "GOES TO WORK"
    If RN = 21 Then U$ = U$ + "MOVES IT LEFT-SIDE"
    If RN = 22 Then U$ = U$ + "MOVES IT RIGHT-SIDE"
    If RN = 23 Then U$ = U$ + "REVERSES THE DRIBBLE"
    If RN = 24 Then U$ = U$ + "TRIES TO GET OPEN"
    If RN = 25 Then U$ = U$ + "DRIBBLES TO THE MIDDLE"
    If RN = 26 Then U$ = U$ + "FAKES THE PASS"
    If RN = 27 Then U$ = U$ + "SETS UP ON TOP"
    If RN = 28 Then U$ = U$ + "SLIDES RIGHT"
    If RN = 29 Then U$ = U$ + "SLIDES LEFT"
    If RN = 30 Then U$ = U$ + "DRIBBLES IT BEHIND THE BACK"
    If RN = 31 Then U$ = U$ + "FAKES LEFT...GOES RIGHT"
    If RN = 32 Then U$ = U$ + "FAKES RIGHT...GOES LEFT"
    If RN = 33 Then U$ = U$ + "SPINS HARD TO THE LEFT"
    If RN = 34 Then U$ = U$ + "SPINS HARD TO THE RIGHT"
    If RN = 35 Then U$ = U$ + "LEANS LEFT...GOES RIGHT"
    If RN = 36 Then U$ = U$ + "LEANS RIGHT...GOES LEFT"
    If RN = 37 Then U$ = U$ + "LEANS LEFT...CUTS RIGHT"
    If RN = 38 Then U$ = U$ + "LEANS RIGHT...CUTS LEFT"
    If RN = 39 Then U$ = U$ + "DOUBLE REVERSES HIS DRIBBLE"
    If RN = 40 Then U$ = U$ + "BACKS IN"
    If RN = 41 Then U$ = U$ + "LEANS FORWARD"
    If RN = 42 Then U$ = U$ + "CROSSES OVER...SPINS"
    If RN = 43 Then U$ = U$ + "CUTS LEFT"
    If RN = 44 Then U$ = U$ + "CUTS RIGHT"
    If RN = 45 Then U$ = U$ + "WITH THE HESITATION DRIBBLE": Call PBP: U$ = "HE SPINS LEFT"
                                  
    If RN = 46 Then U$ = U$ + "WITH THE HESITATION DRIBBLE": Call PBP: U$ = "HE SPINS RIGHT"
                                   
    If RN = 47 Then U$ = U$ + "WITH THE HESITATION DRIBBLE": Call PBP: U$ = "HE CUTS LEFT"
                                 
    If RN = 48 Then U$ = U$ + "WITH THE HESITATION DRIBBLE": Call PBP: U$ = "HE CUTS RIGHT"
                                  
    If RN = 49 Then U$ = U$ + "STUTTER STEPS": Call PBP: U$ = "HE MOVES LEFT"

    If RN = 50 Then U$ = U$ + "MAINTAINS HIS DRIBBLE"
    If RN = 51 Then U$ = U$ + "FAKE SPINS LEFT...GOES RIGHT"
    If RN = 52 Then U$ = U$ + "FAKE SPINS RIGHT...GOES LEFT"
    If RN = 53 Then U$ = U$ + "PALMS THE BALL": Call PBP: U$ = "HE TAKES IT AND SPINS"
                                          
    If RN = 54 Then U$ = U$ + "SHAKES AND BAKES"
    If RN = 55 Then U$ = U$ + "DRIBBLES WITH THE LEFT HAND"
    If RN = 56 Then U$ = U$ + "DRIBBLES WITH THE RIGHT HAND"
    GoTo 1495

    1490 'PASS PB$=PREV HANDLER
    V$ = AA$(p, B1(p, B))
    RN = Int(Rnd(1) * 60) + 1
    If RN = 1 Then U$ = PB$ + " TO " + V$
    If RN = 2 Then U$ = PB$ + " KICKS IT OUT": Call PBP: U$ = V$ + " TAKES THE PASS"
                                         
    If RN = 3 Then U$ = PB$ + " FEEDS " + V$
    If RN = 4 Then U$ = PB$ + " PASSES TO " + V$
    If RN = 4 Then U$ = PB$ + " DISHES TO " + V$
    If RN = 5 Then U$ = PB$ + " GIVES IT TO " + V$
    If RN = 6 Then U$ = PB$ + " WITH A NICE GIVE TO " + V$
                                      
    If RN = 7 Then U$ = PB$ + " LOOKS": Call PBP: U$ = "HE GETS IT TO " + V$

    If RN = 8 Then U$ = PB$ + " SENDS IT TO " + V$
    If RN = 9 Then U$ = PB$ + " TO " + V$
    If RN = 10 Then U$ = PB$ + " GETS IT TO " + V$
    If RN = 11 Then U$ = PB$ + " OVER TO " + V$
    If RN = 12 Then U$ = PB$ + " ZIPS IT TO " + V$
    If RN = 13 Then U$ = PB$ + " FINDS " + V$
    If RN = 14 Then U$ = PB$ + " LOOKS FOR " + V$
    If RN = 14 Then U$ = PB$ + " THROWS IT TO " + V$
    If RN = 15 Then U$ = PB$ + " GOES TO " + V$
    If RN = 16 Then U$ = PB$ + " SETS UP " + V$
    If RN = 17 Then U$ = PB$ + " OUT TO " + V$
    If RN = 18 Then U$ = PB$ + " FLIPS IT TO " + V$
    If RN = 19 Then U$ = "BOUNCE PASS TO " + V$
    If RN = 20 Then U$ = PB$ + " LEAVES IT FOR " + V$
    If RN = 21 Then U$ = PB$ + " LOOPS IT TO " + V$
    If RN = 22 Then U$ = PB$ + " SHOVELS IT OVER TO " + V$
    If RN = 23 Then U$ = PB$ + " GIVES IT BACK TO " + V$
    If RN = 24 Then U$ = PB$ + " FEEDS IT TO " + V$
    If RN = 25 Then U$ = PB$ + " GIVES IT OFF TO " + V$
    If RN = 26 Then U$ = "PASSES TO " + V$
    If RN = 27 Then U$ = "DISHES TO " + V$
    If RN = 28 Then U$ = "GIVES TO " + V$
    If RN = 29 Then U$ = "OVER TO " + V$
    If RN = 30 Then U$ = "SENDS IT BACK TO " + V$
    If RN = 31 Then U$ = "NO-LOOK PASS TO " + V$
    If RN = 32 Then U$ = "NO-LOOK BOUNCE PASS TO " + V$
    If RN = 33 Then U$ = "BEHIND THE BACK TO " + V$
    If RN = 34 Then U$ = PB$ + " SWINGS IT TO " + V$
    If RN = 35 Then U$ = PB$ + " FIRES A PASS TO " + V$
    If RN = 36 Then U$ = PB$ + " SNAPS A PASS TO " + V$
    If RN = 37 Then U$ = PB$ + " SWINGS A PASS TO " + V$
    If RN = 38 Then U$ = "TWO-HANDED PASS TO " + V$
    If RN = 39 Then U$ = "WRAP AROUND PASS TO " + V$
    If RN = 40 Then U$ = "OVER THE SHOULDER PASS TO " + V$
    If RN = 41 Then U$ = "BACKHAND PASS TO " + V$
    If RN = 42 Then U$ = "BACKHAND DISH TO " + V$
    If RN = 43 Then U$ = "PASS CONNECTED TO " + V$
    If RN = 44 Then U$ = PB$ + " WIRES IT TO " + V$
    If RN = 45 Then U$ = PB$ + " WIRES A PASS TO " + V$
    If RN = 46 Then U$ = PB$ + " SNAPS A PASS TO " + V$
    If RN = 47 Then U$ = PB$ + " FAKES..PASSES TO " + V$
    If RN = 48 Then U$ = PB$ + " SHUFFLES IT TO " + V$
    If RN = 49 Then U$ = PB$ + " SLINGS IT TO " + V$
    If RN = 50 Then U$ = PB$ + " FLIPS IT TO " + V$
    If RN = 51 Then U$ = PB$ + " WHIPS IT OVER TO " + V$
    If RN = 52 Then U$ = PB$ + " SLINGS A BOUNCE PASS TO " + V$
    If RN = 53 Then U$ = PB$ + " SHOVELS IT TO " + V$
    If RN = 54 Then U$ = PB$ + " PUMP FAKES A SHOT": Call PBP: U$ = "HE SWINGS IT TO " + V$

    If RN = 55 Then U$ = PB$ + " PUMP FAKES A SHOT": Call PBP: U$ = "HE BOUNCES IT TO " + V$

    If RN = 56 Then U$ = PB$ + " WITH A FOOT FAKE": Call PBP: U$ = "HE SPINS AND DUMPS IT TO " + V$
    If RN = 57 Then U$ = PB$ + " CUTS ACROSS THE LANE": Call PBP: U$ = "HE DISHES INSIDE TO " + V$

    If RN = 58 Then U$ = PB$ + " DRIVES THE BASELINE": Call PBP: U$ = "DISHES UNDERNEATH TO " + V$

    If RN = 59 Then U$ = PB$ + " FAKES RIGHT...CUTS LEFT": Call PBP: U$ = "SHUFFLES IT TO " + V$

    If RN = 60 Then U$ = PB$ + " HEAD FAKES": Call PBP: U$ = "HE PUSHES IT TO " + V$

1495 End Sub


'------------------------------
'      PBP Subroutine
'------------------------------
' ...explanation...
Sub PBP ()
    i$ = InKey$
    If UCase$(i$) = "Q" Then End
    2245 If displayPause = 0 Or PB% = 1 Then 2247
    While Len(U$) < 38
        U$ = U$ + " "
    Wend
    U$ = Left$(U$, 38)

    Locate 8, 1
    If NL% = 1 Then A1$ = U$: vg%(1) = TR%(p, 0): vh%(1) = TR%(p, 1)
    If NL% = 2 Then B1$ = U$: vg%(2) = TR%(p, 0): vh%(2) = TR%(p, 1)
    If NL% = 3 Then C1$ = U$: vg%(3) = TR%(p, 0): vh%(3) = TR%(p, 1)
    If NL% = 4 Then D1$ = U$: vg%(4) = TR%(p, 0): vh%(4) = TR%(p, 1)
    If NL% = 5 Then E1$ = U$: vg%(5) = TR%(p, 0): vh%(5) = TR%(p, 1)
    If NL% = 6 Then F1$ = U$: vg%(6) = TR%(p, 0): vh%(6) = TR%(p, 1)
    If NL% = 7 Then G1$ = U$: vg%(7) = TR%(p, 0): vh%(7) = TR%(p, 1)
    If NL% = 8 Then H1$ = U$: vg%(8) = TR%(p, 0): vh%(8) = TR%(p, 1)
    If NL% > 8 Then
        A1$ = B1$
        B1$ = C1$
        C1$ = D1$
        D1$ = E1$
        E1$ = F1$
        F1$ = G1$
        G1$ = H1$
        H1$ = U$
        For I = 1 To 7
            vg%(I) = vg%(I + 1)
            vh%(I) = vh%(I + 1)
        Next
        vg%(8) = TR%(p, 0)
        vh%(8) = TR%(p, 1)
    End If

    NL% = NL% + 1
    Color vg%(1), vh%(1): Print A1$
    Color vg%(2), vh%(2): Print B1$
    Color vg%(3), vh%(3): Print C1$
    Color vg%(4), vh%(4): Print D1$
    Color vg%(5), vh%(5): Print E1$
    Color vg%(6), vh%(6): Print F1$
    Color vg%(7), vh%(7): Print G1$
    Color vg%(8), vh%(8): Print H1$
    Color 15, 0
    Call DELAY
    2247 '
End Sub


'------------------------------
'    SHOTSTRING Subroutine
'------------------------------
' ...explanation...
Sub SHOTSTRING ()
    U$ = U$ + Str$(Int(F + .5)) + "*"
End Sub


'------------------------------
'      TEAMSETUP Subroutine
'------------------------------
' ...explanation...
Sub TEAMSETUP ()

    'TEAM SETUP INITIALIZES THE TEAM DATA FILES, LEAGUE ADJUSTMENTS
    'AND SETS UP THE PLAYER DATA

    F5%(P9, 5) = L%(P9, 1): If F5%(P9, 5) = 0 Then F5%(P9, 5) = 4
    F5%(P9, 6) = L%(P9, 2): If F5%(P9, 6) > 20 Then F5%(P9, 6) = F5%(P9, 6) - 100
    F5%(P9, 7) = L%(P9, 3): If F5%(P9, 7) > 20 Then F5%(P9, 7) = F5%(P9, 7) - 100
    F5%(P9, 4) = L%(P9, 0):: If F5%(P9, 4) > 20 Then F5%(P9, 4) = F5%(P9, 4) - 100
    F5%(P9, 8) = L%(P9, 4)
    If F5%(P9, 8) <> 99 Then F5%(P9, 4) = 0: F5%(P9, 5) = 4: F5%(P9, 6) = 0: F5%(P9, 7) = 0
    For I = 0 To 13
        A(P9, I, 16) = C(P9, I, 6)
        If A(P9, I, 16) = 0 Then A(P9, I, 16) = 6
        A(P9, I, 17) = C(P9, I, 5)
        If A(P9, I, 17) = 0 Then A(P9, I, 17) = 40
        A(P9, I, 19) = C(P9, I, 3)
        A(P9, I, 20) = C(P9, I, 2)
        A(P9, I, 21) = C(P9, I, 4)
        A(P9, I, 2) = A(P9, I, 2)
    Next I
    F5%(P9, 2) = V1%(P9)
    If F5%(P9, 2) > 20 Then F5%(P9, 2) = F5%(P9, 2) - 100
    F5%(P9, 3) = V2%(P9)
    If F5%(P9, 3) > 20 Then F5%(P9, 3) = F5%(P9, 3) - 100
End Sub


'------------------------------
'      TICKER Subroutine
'------------------------------
' ...explanation...
Sub TICKER ()
    2850 '*** TICKER ***
    If AP = 1 And U5 = 2 Then 2884
    Color 14, 0
    Cls

    RN = Int(Rnd(1) * 100) + 1
    If RN <= 50 And HT% = 1 And G9 = 0 Then Locate , 23: Print "PRUDENTIAL AT THE HALF SCOREBOARD": GoTo 2851
    If RN > 50 And HT% = 1 And G9 = 0 Then Locate , 28: Print "RED MAN HALF TIME REPORT": GoTo 2851
    If HT% = 1 And RN <= 50 Then Locate , 29: Print "TOYOTA HALFTIME REPORT": GoTo 2851
    If HT% = 1 And RN > 50 Then Locate , 29: Print "HONDA HALFTIME REPORT": GoTo 2851
    If EG% = 1 And G9 = 0 Then Locate , 27: Print "CHEVROLET FINAL SCOREBOARD": GoTo 2851
    If EG% = 1 And RN <= 33 Then Locate , 27: Print "USA TODAY FINAL SCOREBOARD": GoTo 2851
    If EG% = 1 And RN > 33 And RN <= 67 Then Locate , 24: Print "FEDERAL EXPRESS FINAL SCOREBOARD": GoTo 2851
    If EG% = 1 And RN > 67 Then Locate , 23: Print "ESPN SPORTSCENTER FINAL SCOREBOARD": GoTo 2851
    If HT% = 0 Then Locate , 30: Print "LANCE HAFFNER GAMES SCOREBOARD": GoTo 2851

    2851 Color 15: If TMT = 0 Then Locate 10, 28: Print "NO OTHER GAMES SCHEDULED": GoTo 2883
    If H = 1 And T < 1200 And T >= 960 Then TE = 1
    If H = 1 And T < 960 And T >= 720 Then TE = 2
    If H = 1 And T < 720 And T >= 480 Then TE = 3
    If H = 1 And T < 480 And T >= 240 Then TE = 4
    If H = 1 And T < 240 And T >= 120 Then TE = 5
    If H = 1 And T < 120 Then TE = 6
    If H = 2 And T < 1200 And T >= 960 Then TE = 9
    If H = 2 And T < 960 And T >= 720 Then TE = 10
    If H = 2 And T < 720 And T >= 480 Then TE = 11
    If H = 2 And T < 480 And T >= 240 Then TE = 12
    If H = 2 And T < 240 And T >= 120 Then TE = 13
    If H = 2 And T < 120 Then TE = 14
    If H = 3 And T < 720 And T >= 480 Then TE = 15
    If H = 3 And T < 480 And T >= 240 Then TE = 16
    If H = 3 And T < 240 Then TE = 17
    If HT% = 1 Then TE = 8
    If Left$(SX$(1, 0), 3) = "XXX" Then 2852
    Locate 2, 1: X = 1: GoSub 2885: Print Left$(SX$(1, 0), 13);: Locate , 15
    Print Using "### "; SX%(1, 0, TS%);: Print R$(TS%)
    Print Left$(SX$(1, 1), 13);: Locate , 15: Print Using "### #"; SX%(1, 1, TS%)
    2852 If Left$(SX$(2, 0), 3) = "XXX" Then 2853
    Locate 5, 1: X = 2: GoSub 2885: Print Left$(SX$(2, 0), 13);: Locate , 15: Print Using "### "; SX%(2, 0, TS%);: Print R$(TS%)
    Print Left$(SX$(2, 1), 13);: Locate , 15: Print Using "### #"; SX%(2, 1, TS%)
    2853 If Left$(SX$(3, 0), 3) = "XXX" Then 2854
    Locate 8, 1: X = 3: GoSub 2885: Print Left$(SX$(3, 0), 13);: Locate , 15: Print Using "### "; SX%(3, 0, TS%);: Print R$(TS%)
    Print Left$(SX$(3, 1), 13);: Locate , 15: Print Using "### #"; SX%(3, 1, TS%)
    2854 If Left$(SX$(4, 0), 3) = "XXX" Then 2855
    Locate 11, 1: X = 4: GoSub 2885: Print Left$(SX$(4, 0), 13);: Locate , 15: Print Using "### "; SX%(4, 0, TS%);: Print R$(TS%)
    Print Left$(SX$(4, 1), 13);: Locate , 15: Print Using "### #"; SX%(4, 1, TS%)
    2855 If Left$(SX$(5, 0), 3) = "XXX" Then 2856
    Locate 14, 1: X = 5: GoSub 2885: Print Left$(SX$(5, 0), 13);: Locate , 15: Print Using "### "; SX%(5, 0, TS%);: Print R$(TS%)
    Print Left$(SX$(5, 1), 13);: Locate , 15: Print Using "### #"; SX%(5, 1, TS%)
    2856 If Left$(SX$(6, 0), 3) = "XXX" Then 2857
    Locate 17, 1: X = 6: GoSub 2885: Print Left$(SX$(6, 0), 13);: Locate , 15: Print Using "### "; SX%(6, 0, TS%);: Print R$(TS%)
    Print Left$(SX$(6, 1), 13);: Locate , 15: Print Using "### #"; SX%(6, 1, TS%)
    2857 If Left$(SX$(7, 0), 3) = "XXX" Then 2858
    Locate 20, 1: X = 7: GoSub 2885: Print Left$(SX$(7, 0), 13);: Locate , 15: Print Using "### "; SX%(7, 0, TS%);: Print R$(TS%)
    Print Left$(SX$(7, 1), 13);: Locate , 15: Print Using "### #"; SX%(7, 1, TS%)
    2858 If Left$(SX$(8, 0), 3) = "XXX" Then 2859
    Locate 23, 1: X = 8: GoSub 2885: Print Left$(SX$(8, 0), 13);: Locate , 15: Print Using "### "; SX%(8, 0, TS%);: Print R$(TS%)
    Print Left$(SX$(8, 1), 13);: Locate , 15: Print Using "### #"; SX%(8, 1, TS%);
    2859 If Left$(SX$(9, 0), 3) = "XXX" Then 2860
    Locate 2, 21: X = 9: GoSub 2885: Print Left$(SX$(9, 0), 13);: Locate , 35: Print Using "### "; SX%(9, 0, TS%);: Print R$(TS%)
    Locate , 21: Print Left$(SX$(9, 1), 13);: Locate , 35: Print Using "### #"; SX%(9, 1, TS%)
    2860 If Left$(SX$(10, 0), 3) = "XXX" Then 2861
    Locate 5, 21: X = 10: GoSub 2885: Print Left$(SX$(10, 0), 13);: Locate , 35: Print Using "### "; SX%(10, 0, TS%);: Print R$(TS%)
    Locate , 21: Print Left$(SX$(10, 1), 13);: Locate , 35: Print Using "### #"; SX%(10, 1, TS%)
    2861 If Left$(SX$(11, 0), 3) = "XXX" Then 2862
    Locate 8, 21: X = 11: GoSub 2885: Print Left$(SX$(11, 0), 13);: Locate , 35: Print Using "### "; SX%(11, 0, TS%);: Print R$(TS%)
    Locate , 21: Print Left$(SX$(11, 1), 13);: Locate , 35: Print Using "### #"; SX%(11, 1, TS%)
    2862 If Left$(SX$(12, 0), 3) = "XXX" Then 2863
    Locate 11, 21: X = 12: GoSub 2885: Print Left$(SX$(12, 0), 13);: Locate , 35: Print Using "### "; SX%(12, 0, TS%);: Print R$(TS%)
    Locate , 21: Print Left$(SX$(12, 1), 13);: Locate , 35: Print Using "### #"; SX%(12, 1, TS%)
    2863 If Left$(SX$(13, 0), 3) = "XXX" Then 2864
    Locate 14, 21: X = 13: GoSub 2885: Print Left$(SX$(13, 0), 13);: Locate , 35: Print Using "### "; SX%(13, 0, TS%);: Print R$(TS%)
    Locate , 21: Print Left$(SX$(13, 1), 13);: Locate , 35: Print Using "### #"; SX%(13, 1, TS%)
    2864 If Left$(SX$(14, 0), 3) = "XXX" Then 2865
    Locate 17, 21: X = 14: GoSub 2885: Print Left$(SX$(14, 0), 13);: Locate , 35: Print Using "### "; SX%(14, 0, TS%);: Print R$(TS%)
    Locate , 21: Print Left$(SX$(14, 1), 13);: Locate , 35: Print Using "### #"; SX%(14, 1, TS%)
    2865 If Left$(SX$(15, 0), 3) = "XXX" Then 2866
    Locate 20, 21: X = 15: GoSub 2885: Print Left$(SX$(15, 0), 13);: Locate , 35: Print Using "### "; SX%(15, 0, TS%);: Print R$(TS%)
    Locate , 21: Print Left$(SX$(15, 1), 13);: Locate , 35: Print Using "### #"; SX%(15, 1, TS%)
    2866 If Left$(SX$(16, 0), 3) = "XXX" Then 2867
    Locate 23, 21: X = 16: GoSub 2885: Print Left$(SX$(16, 0), 13);: Locate , 35: Print Using "### "; SX%(16, 0, TS%);: Print R$(TS%)
    Locate , 21: Print Left$(SX$(16, 1), 13);: Locate , 35: Print Using "### #"; SX%(16, 1, TS%);
    2867 If Left$(SX$(17, 0), 3) = "XXX" Then 2868
    Locate 2, 41: X = 17: GoSub 2885: Print Left$(SX$(17, 0), 13);: Locate , 55: Print Using "### "; SX%(17, 0, TS%);: Print R$(TS%)
    Locate , 41: Print Left$(SX$(17, 1), 13);: Locate , 55: Print Using "### #"; SX%(17, 1, TS%)
    2868 If Left$(SX$(18, 0), 3) = "XXX" Then 2869
    Locate 5, 41: X = 18: GoSub 2885: Print Left$(SX$(18, 0), 13);: Locate , 55: Print Using "### "; SX%(18, 0, TS%);: Print R$(TS%)
    Locate , 41: Print Left$(SX$(18, 1), 13);: Locate , 55: Print Using "### #"; SX%(18, 1, TS%)
    2869 If Left$(SX$(19, 0), 3) = "XXX" Then 2870
    Locate 8, 41: X = 19: GoSub 2885: Print Left$(SX$(19, 0), 13);: Locate , 55: Print Using "### "; SX%(19, 0, TS%);: Print R$(TS%)
    Locate , 41: Print Left$(SX$(19, 1), 13);: Locate , 55: Print Using "### #"; SX%(19, 1, TS%)
    2870 If Left$(SX$(20, 0), 3) = "XXX" Then 2871
    Locate 11, 41: X = 20: GoSub 2885: Print Left$(SX$(20, 0), 13);: Locate , 55: Print Using "### "; SX%(20, 0, TS%);: Print R$(TS%)
    Locate , 41: Print Left$(SX$(20, 1), 13);: Locate , 55: Print Using "### #"; SX%(20, 1, TS%)
    2871 If Left$(SX$(21, 0), 3) = "XXX" Then 2872
    Locate 14, 41: X = 21: GoSub 2885: Print Left$(SX$(21, 0), 13);: Locate , 55: Print Using "### "; SX%(21, 0, TS%);: Print R$(TS%)
    Locate , 41: Print Left$(SX$(21, 1), 13);: Locate , 55: Print Using "### #"; SX%(21, 1, TS%)
    2872 If Left$(SX$(22, 0), 3) = "XXX" Then 2873
    Locate 17, 41: X = 22: GoSub 2885: Print Left$(SX$(22, 0), 13);: Locate , 55: Print Using "### "; SX%(22, 0, TS%);: Print R$(TS%)
    Locate , 41: Print Left$(SX$(22, 1), 13);: Locate , 55: Print Using "### #"; SX%(22, 1, TS%)
    2873 If Left$(SX$(23, 0), 3) = "XXX" Then 2874
    Locate 20, 41: X = 23: GoSub 2885: Print Left$(SX$(23, 0), 13);: Locate , 55: Print Using "### "; SX%(23, 0, TS%);: Print R$(TS%)
    Locate , 41: Print Left$(SX$(23, 1), 13);: Locate , 55: Print Using "### #"; SX%(23, 1, TS%)
    2874 If Left$(SX$(24, 0), 3) = "XXX" Then 2875
    Locate 23, 41: X = 24: GoSub 2885: Print Left$(SX$(24, 0), 13);: Locate , 55: Print Using "### "; SX%(24, 0, TS%);: Print R$(TS%)
    Locate , 41: Print Left$(SX$(24, 1), 13);: Locate , 55: Print Using "### #"; SX%(24, 1, TS%);
    2875 If Left$(SX$(25, 0), 3) = "XXX" Then 2876
    Locate 2, 61: X = 25: GoSub 2885: Print Left$(SX$(25, 0), 13);: Locate , 75: Print Using "### "; SX%(25, 0, TS%);: Print R$(TS%)
    Locate , 61: Print Left$(SX$(25, 1), 13);: Locate , 75: Print Using "### #"; SX%(25, 1, TS%)
    2876 If Left$(SX$(26, 0), 3) = "XXX" Then 2877
    Locate 5, 61: X = 26: GoSub 2885: Print Left$(SX$(26, 0), 13);: Locate , 75: Print Using "### "; SX%(26, 0, TS%);: Print R$(TS%)
    Locate , 61: Print Left$(SX$(26, 1), 13);: Locate , 75: Print Using "### #"; SX%(26, 1, TS%)
    2877 If Left$(SX$(27, 0), 3) = "XXX" Then 2878
    Locate 8, 61: X = 27: GoSub 2885: Print Left$(SX$(27, 0), 13);: Locate , 75: Print Using "### "; SX%(27, 0, TS%);: Print R$(TS%)
    Locate , 61: Print Left$(SX$(27, 1), 13);: Locate , 75: Print Using "### #"; SX%(27, 1, TS%)
    2878 If Left$(SX$(28, 0), 3) = "XXX" Then 2879
    Locate 11, 61: X = 28: GoSub 2885: Print Left$(SX$(28, 0), 13);: Locate , 75: Print Using "### "; SX%(28, 0, TS%);: Print R$(TS%)
    Locate , 61: Print Left$(SX$(28, 1), 13);: Locate , 75: Print Using "### #"; SX%(28, 1, TS%)
    2879 If Left$(SX$(29, 0), 3) = "XXX" Then 2880
    Locate 14, 61: X = 29: GoSub 2885: Print Left$(SX$(29, 0), 13);: Locate , 75: Print Using "### "; SX%(29, 0, TS%);: Print R$(TS%)
    Locate , 61: Print Left$(SX$(29, 1), 13);: Locate , 75: Print Using "### #"; SX%(29, 1, TS%)
    2880 If Left$(SX$(30, 0), 3) = "XXX" Then 2881
    Locate 17, 61: X = 30: GoSub 2885: Print Left$(SX$(30, 0), 13);: Locate , 75: Print Using "### "; SX%(30, 0, TS%);: Print R$(TS%)
    Locate , 61: Print Left$(SX$(30, 1), 13);: Locate , 75: Print Using "### #"; SX%(30, 1, TS%)
    2881 If Left$(SX$(31, 0), 3) = "XXX" Then 2882
    Locate 20, 61: X = 31: GoSub 2885: Print Left$(SX$(31, 0), 13);: Locate , 75: Print Using "### "; SX%(31, 0, TS%);: Print R$(TS%)
    Locate , 61: Print Left$(SX$(31, 1), 13);: Locate , 75: Print Using "### #"; SX%(31, 1, TS%)
    2882 If Left$(SX$(32, 0), 3) = "XXX" Then 2883
    Locate 23, 61: X = 32: GoSub 2885: Print Left$(SX$(32, 0), 13);: Locate , 75: Print Using "### "; SX%(32, 0, TS%);: Print R$(TS%)
    Locate , 61: Print Left$(SX$(32, 1), 13);: Locate , 75: Print Using "### #"; SX%(32, 1, TS%);
    2883 I$ = GetKeyPress$
    GoTo 2884
    2885 TS% = TE + (ST% - ST%(X)) * 3
    If TS% < 0 Then TS% = 0
    If TS% > 14 Then TS% = 14
    If EG% = 1 Then TS% = 14
    Return

2884 End Sub



'------------------------------
'      BOX2FILE Subroutine
'------------------------------
' Prints BoxScore to file
Sub BOX2FILE
    Cls
    If AP = 1 Then
        NM$ = "BOX" + Mid$(Str$(AP%(0) + AP%(2)), 2, Len(Str$(AP%(0) + AP%(2))))
    Else
        Input "ENTER FILENAME TO SAVE GAME SUMMARY UNDER "; NM$: NM$ = NM$ + ".SPC"
    End If

    8951 Open p$(1) + NM$ For Output As #2

    If N <> 2 Then Print #2, "@ "; B$(N): Print #2,

    For I = 0 To 1
        Print #2, B$(I); Tab(17); Str$(S(I, 1)); Tab(20); Str$(S(I, 2));
        If S(0, 1) + S(0, 2) = S(1, 1) + S(1, 2) Then Print #2, ; Tab(23); Str$(S(I, 3));
        If S(0, 1) + S(0, 2) <> S(1, 1) + S(1, 2) Then Print #2, ; Tab(23); "- "; S(I, 0): GoTo 8963
        If S(0, 1) + S(0, 2) + S(0, 3) = S(1, 1) + S(1, 2) + S(1, 3) Then Print #2, ; Tab(26); Str$(S(I, 4));
        If S(0, 1) + S(0, 2) + S(0, 3) <> S(1, 1) + S(1, 2) + S(1, 3) Then Print #2, ; Tab(26); "- "; S(I, 0): GoTo 8963
        If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) = S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) Then Print #2, ; Tab(29); Str$(S(I, 5));
        If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) <> S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) Then Print #2, ; Tab(29); "- "; Str$(S(I, 0)): GoTo 8963
        If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) + S(0, 5) = S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) + S(1, 5) Then Print #2, ; Tab(32); Str$(S(I, 6));
        If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) + S(0, 5) <> S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) + S(1, 5) Then Print #2, ; Tab(32); "- "; Str$(S(I, 0)): GoTo 8963
        If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) + S(0, 5) + S(0, 6) = S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) + S(1, 5) + S(1, 6) Then Print #2, ; Tab(35); Str$(S(I, 7)); Tab(38); Str$(S(I, 0)): GoTo 8963
        If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) + S(0, 5) + S(0, 6) <> S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) + S(1, 5) + S(1, 6) Then Print #2, ; Tab(35); "- "; Str$(S(I, 0)): GoTo 8963
    8963 Next I

    For I = 0 To 1: Print #2,: Print #2, B$(I)
        Print #2, "NAME"; Tab(14); " MIN   FG- A 3FG- A  FT- A ORB  RB  PT  F   A   B   S  TO"
        For I1 = 0 To 13
            If AA$(I, I1) = "XXX" Then 8970

            Print #2, AA$(I, I1); Tab(14);

            If ZZ(I, I1) = 0 Then Print #2, ; Tab(15); "DNP";: GoTo 8968

            Print #2, Using "##"; Int(ZZ(I, I1) / 60);: Print #2, ":";

            If ZZ(I, I1) - Int(ZZ(I, I1) / 60) * 60 > 9 Then Print #2, Using "##"; ZZ(I, I1) - Int(ZZ(I, I1) / 60) * 60; Else Print #2, "0";: Print #2, Using "#"; ZZ(I, I1) - Int(ZZ(I, I1) / 60) * 60;

            8968 Print #2, ; Tab(21);: Print #2, Using "##"; A(I, I1, 8);
            Print #2, "-";: Print #2, Using "##"; A(I, I1, 7);: Print #2, Using "  ##"; W%(I, I1, 0);: Print #2, "-";: Print #2, Using "##"; W%(I, I1, 1);: Print #2, Using "  ##"; A(I, I1, 10);: Print #2, "-";: Print #2, Using "##"; A(I, I1, 9);
            Print #2, Using "  ##  ## ###  #  ##  ##  ##  ##  ##  ##"; A(I, I1, 24), A(I, I1, 11), A(I, I1, 8) * 2 + A(I, I1, 10) + W%(I, I1, 0), A(I, I1, 12), A(I, I1, 14), A(I, I1, 15), A(I, I1, 13), A(I, I1, 23)

            V9(0) = V9(0) + A(I, I1, 8)
            V9(1) = V9(1) + A(I, I1, 7)
            V9(2) = V9(2) + A(I, I1, 10)
            V9(3) = V9(3) + A(I, I1, 9)
            V9(4) = V9(4) + A(I, I1, 11)
            V9(5) = V9(5) + (A(I, I1, 8) * 2) + A(I, I1, 10) + W%(I, I1, 0)
            V9(13) = V9(13) + W%(I, I1, 0): V9(14) = V9(14) + W%(I, I1, 1)
            V9(6) = V9(6) + A(I, I1, 12)
            V9(7) = V9(7) + A(I, I1, 14)
            V9(8) = V9(8) + A(I, I1, 15)
            V9(9) = V9(9) + A(I, I1, 13)
            V9(11) = V9(11) + A(I, I1, 24)
            V9(12) = V9(12) + A(I, I1, 23)
            V9(10) = V9(10) + ZZ(I, I1)

        8970 Next I1

        If V9(3) = 0 Then V9(3) = 1

        Print #2,: Print #2, "TOTALS"; Tab(14); Int(V9(10) / 60); Tab(21);: Print #2, Using "##"; V9(0);: Print #2, "-";: Print #2, Using "###"; V9(1);
        Print #2, ; Tab(28);: Print Using "##"; V9(13);: Print #2, "-";: Print #2, Using "##"; V9(14);
        Print #2, ; Tab(35);: Print Using "##"; V9(2);: Print #2, "-";: Print #2, Using "##"; V9(3);
        Print #2, ; Tab(42);: Print Using "##  ## ### ##  ##  ##  ##  ##"; V9(11), V9(4), V9(5), V9(6), V9(7), V9(8), V9(9), V9(12)
        Print #2, ; Tab(22); Int(((V9(0) / V9(1)) * 100) + .5); "%"; Tab(35); Int(((V9(2) / V9(3)) * 100) + .5); "%": Print #2,: Print #2, "TOTAL TURNOVERS -"; T0(I): Print #2, "TOTAL REBOUNDS  -"; R3(I)

        For I2 = 0 To 14: V9(I2) = 0: Next I2

    Next I

    Print #2, Chr$(12)

    Close 2
End Sub


'------------------------------
'      BOX2PRINTER Subroutine
'------------------------------
' Prints BoxScore to printer
Sub BOX2PRINTER
    8900 If N <> 2 Then LPrint "@ "; B$(N): LPrint

    For I = 0 To 1
        LPrint B$(I); Tab(17); Str$(S(I, 1)); Tab(20); Str$(S(I, 2));
        If S(0, 1) + S(0, 2) = S(1, 1) + S(1, 2) Then LPrint ; Tab(23); Str$(S(I, 3));
        If S(0, 1) + S(0, 2) <> S(1, 1) + S(1, 2) Then LPrint ; Tab(23); "- "; S(I, 0): GoTo 8913
        If S(0, 1) + S(0, 2) + S(0, 3) = S(1, 1) + S(1, 2) + S(1, 3) Then LPrint ; Tab(26); Str$(S(I, 4));
        If S(0, 1) + S(0, 2) + S(0, 3) <> S(1, 1) + S(1, 2) + S(1, 3) Then LPrint ; Tab(26); "- "; S(I, 0): GoTo 8913
        If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) = S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) Then LPrint ; Tab(29); Str$(S(I, 5));
        If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) <> S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) Then LPrint ; Tab(29); "- "; Str$(S(I, 0)): GoTo 8913
        If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) + S(0, 5) = S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) + S(1, 5) Then LPrint ; Tab(32); Str$(S(I, 6));
        If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) + S(0, 5) <> S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) + S(1, 5) Then LPrint ; Tab(32); "- "; Str$(S(I, 0)): GoTo 8913
        If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) + S(0, 5) + S(0, 6) = S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) + S(1, 5) + S(1, 6) Then LPrint ; Tab(35); Str$(S(I, 7)); Tab(38); Str$(S(I, 0)): GoTo 8913
        If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) + S(0, 5) + S(0, 6) <> S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) + S(1, 5) + S(1, 6) Then LPrint ; Tab(35); "- "; Str$(S(I, 0)): GoTo 8913
    8913 Next I

    For I = 0 To 1: LPrint: LPrint B$(I)
        LPrint "NAME"; Tab(14); " MIN   FG- A 3FG- A  FT- A ORB  RB  PT  F   A   B   S  TO"

        For I1 = 0 To 13
            If AA$(I, I1) = "XXX" Then 8920

            LPrint AA$(I, I1); Tab(14);

            If ZZ(I, I1) = 0 Then LPrint ; Tab(15); "DNP";: GoTo 8918

            LPrint Using "##"; Int(ZZ(I, I1) / 60);: LPrint ":";: If ZZ(I, I1) - Int(ZZ(I, I1) / 60) * 60 > 9 Then LPrint Using "##"; ZZ(I, I1) - Int(ZZ(I, I1) / 60) * 60; Else LPrint "0";: LPrint Using "#"; ZZ(I, I1) - Int(ZZ(I, I1) / 60) * 60;

            8918 LPrint ; Tab(21);: LPrint Using "##"; A(I, I1, 8);: LPrint "-";: LPrint Using "##"; A(I, I1, 7);: LPrint Using "  ##"; W%(I, I1, 0);: LPrint "-";: LPrint Using "##"; W%(I, I1, 1);: LPrint Using "  ##"; A(I, I1, 10);: LPrint "-";: LPrint Using "##"; A(I, I1, 9);

            LPrint Using "  ##  ## ###  #  ##  ##  ##  ##  ##  ##"; A(I, I1, 24), A(I, I1, 11), A(I, I1, 8) * 2 + A(I, I1, 10) + W%(I, I1, 0), A(I, I1, 12), A(I, I1, 14), A(I, I1, 15), A(I, I1, 13), A(I, I1, 23)

            V9(0) = V9(0) + A(I, I1, 8)
            V9(1) = V9(1) + A(I, I1, 7)
            V9(2) = V9(2) + A(I, I1, 10)
            V9(3) = V9(3) + A(I, I1, 9)
            V9(4) = V9(4) + A(I, I1, 11)
            V9(5) = V9(5) + (A(I, I1, 8) * 2) + A(I, I1, 10) + W%(I, I1, 0)
            V9(13) = V9(13) + W%(I, I1, 0)
            V9(14) = V9(14) + W%(I, I1, 1)
            V9(6) = V9(6) + A(I, I1, 12)
            V9(7) = V9(7) + A(I, I1, 14)
            V9(8) = V9(8) + A(I, I1, 15)
            V9(9) = V9(9) + A(I, I1, 13)
            V9(11) = V9(11) + A(I, I1, 24)
            V9(12) = V9(12) + A(I, I1, 23)
            V9(10) = V9(10) + ZZ(I, I1)
        8920 Next I1

        If V9(3) = 0 Then V9(3) = 1

        LPrint: LPrint "TOTALS"; Tab(14); Int(V9(10) / 60); Tab(21);: LPrint Using "##"; V9(0);: LPrint "-";: LPrint Using "###"; V9(1);
        LPrint ; Tab(28);: LPrint Using "##"; V9(13);: LPrint "-";: LPrint Using "##"; V9(14);
        LPrint ; Tab(35);: LPrint Using "##"; V9(2);: LPrint "-";: LPrint Using "##"; V9(3);
        LPrint ; Tab(42);: LPrint Using "##  ## ### ##  ##  ##  ##  ##"; V9(11), V9(4), V9(5), V9(6), V9(7), V9(8), V9(9), V9(12)
        LPrint ; Tab(22); Int(((V9(0) / V9(1)) * 100) + .5); "%"; Tab(35); Int(((V9(2) / V9(3)) * 100) + .5); "%": LPrint: LPrint "TOTAL TURNOVERS -"; T0(I): LPrint "TOTAL REBOUNDS  -"; R3(I)

        For I2 = 0 To 14: V9(I2) = 0: Next I2

    Next I

    LPrint Chr$(12)

End Sub


'------------------------------
'    BOX2SPC Subroutine
'------------------------------
' Creates box score file for
' the event that any "special"
' (SPC) milestones were reached
Sub BOX2SPC (NM$)
    Open p$(1) + NM$ For Output As #2
    If H - 3 > E%(1) Then Print #2, Using "MORE THAN ## OVERTIMES"; E%(1)
    If S(1, 0) + S(0, 0) > E%(2) Then Print #2, Using "MORE THAN ### COMBINED POINTS"; E%(2)
    If R3(0) + R3(1) > E%(3) Then Print #2, Using "MORE THAN ### COMBINED REBOUNDS"; E%(3)
    If S(0, 0) > E%(4) Then Print #2, B$(0);: Print #2, Using " SCORED MORE THAN ### POINTS"; E%(4)
    If S(1, 0) > E%(4) Then Print #2, B$(1);: Print #2, Using " SCORED MORE THAN ### POINTS"; E%(4)
    If R3(0) > E%(5) Then Print #2, B$(0);: Print #2, Using " HAD MORE THAN ### REBOUNDS"; E%(5)
    If R3(1) > E%(5) Then Print #2, B$(1);: Print #2, Using " HAD MORE THAN ### REBOUNDS"; E%(5)

    For I = 0 To 1
        For I1 = 0 To 13
            If A(I, I1, 8) * 2 + A(I, I1, 10) + W%(I, I1, 0) > E%(6) Then Print #2, AA$(I, I1);: Print #2, Using " SCORED MORE THAN ### POINTS"; E%(6)
            If A(I, I1, 11) > E%(7) Then Print #2, AA$(I, I1);: Print #2, Using " HAD MORE THAN ### REBOUNDS"; E%(7)
            If A(I, I1, 14) > E%(8) Then Print #2, AA$(I, I1);: Print #2, Using " HAD MORE THAN ### ASSISTS"; E%(8)
            If A(I, I1, 15) > E%(9) Then Print #2, AA$(I, I1);: Print #2, Using " BLOCKED MORE THAN ### SHOTS"; E%(9)
            If A(I, I1, 13) > E%(10) Then Print #2, AA$(I, I1);: Print #2, Using " HAD MORE THAN ### STEALS"; E%(10)
            If W%(I, I1, 0) > E%(11) Then Print #2, AA$(I, I1);: Print #2, Using " MADE MORE THAN ### 3-PT FIELD GOALS"; E%(11)
        Next
    Next
    If N <> 2 Then Print #2, "@ "; B$(N): Print #2,
    For I = 0 To 1
        Print #2, B$(I); Tab(17); Str$(S(I, 1)); Tab(20); Str$(S(I, 2));
        If S(0, 1) + S(0, 2) = S(1, 1) + S(1, 2) Then Print #2, ; Tab(23); Str$(S(I, 3));
        If S(0, 1) + S(0, 2) <> S(1, 1) + S(1, 2) Then Print #2, ; Tab(23); "- "; S(I, 0): GoTo 8963
        If S(0, 1) + S(0, 2) + S(0, 3) = S(1, 1) + S(1, 2) + S(1, 3) Then Print #2, ; Tab(26); Str$(S(I, 4));
        If S(0, 1) + S(0, 2) + S(0, 3) <> S(1, 1) + S(1, 2) + S(1, 3) Then Print #2, ; Tab(26); "- "; S(I, 0): GoTo 8963
        If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) = S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) Then Print #2, ; Tab(29); Str$(S(I, 5));
        If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) <> S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) Then Print #2, ; Tab(29); "- "; Str$(S(I, 0)): GoTo 8963
        If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) + S(0, 5) = S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) + S(1, 5) Then Print #2, ; Tab(32); Str$(S(I, 6));
        If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) + S(0, 5) <> S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) + S(1, 5) Then Print #2, ; Tab(32); "- "; Str$(S(I, 0)): GoTo 8963
        If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) + S(0, 5) + S(0, 6) = S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) + S(1, 5) + S(1, 6) Then Print #2, ; Tab(35); Str$(S(I, 7)); Tab(38); Str$(S(I, 0)): GoTo 8963
        If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) + S(0, 5) + S(0, 6) <> S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) + S(1, 5) + S(1, 6) Then Print #2, ; Tab(35); "- "; Str$(S(I, 0)): GoTo 8963
    8963 Next I
    For I = 0 To 1: Print #2,: Print #2, B$(I)
        Print #2, "NAME"; Tab(14); " MIN   FG- A 3FG- A  FT- A ORB  RB  PT  F   A   B   S  TO"
        For I1 = 0 To 13
            If AA$(I, I1) = "XXX" Then 8970
            Print #2, AA$(I, I1); Tab(14)
            If ZZ(I, I1) = 0 Then Print #2, ; Tab(15); "DNP";: GoTo 8968
            Print #2, Using "##"; Int(ZZ(I, I1) / 60);: Print #2, ":";
            If ZZ(I, I1) - Int(ZZ(I, I1) / 60) * 60 > 9 Then Print #2, Using "##"; ZZ(I, I1) - Int(ZZ(I, I1) / 60) * 60; Else Print #2, "0";: Print #2, Using "#"; ZZ(I, I1) - Int(ZZ(I, I1) / 60) * 60;
            8968 Print #2, ; Tab(21);: Print #2, Using "##"; A(I, I1, 8);
            Print #2, "-";: Print #2, Using "##"; A(I, I1, 7);: Print #2, Using "  ##"; W%(I, I1, 0);: Print #2, "-";: Print #2, Using "##"; W%(I, I1, 1);: Print #2, Using "  ##"; A(I, I1, 10);: Print #2, "-";: Print #2, Using "##"; A(I, I1, 9);
            Print #2, Using "  ##  ## ###  #  ##  ##  ##  ##  ##  ##"; A(I, I1, 24), A(I, I1, 11), A(I, I1, 8) * 2 + A(I, I1, 10) + W%(I, I1, 0), A(I, I1, 12), A(I, I1, 14), A(I, I1, 15), A(I, I1, 13), A(I, I1, 23)
            V9(0) = V9(0) + A(I, I1, 8): V9(1) = V9(1) + A(I, I1, 7): V9(2) = V9(2) + A(I, I1, 10): V9(3) = V9(3) + A(I, I1, 9): V9(4) = V9(4) + A(I, I1, 11): V9(5) = V9(5) + (A(I, I1, 8) * 2) + A(I, I1, 10) + W%(I, I1, 0): V9(13) = V9(13) + W%(I, I1, 0): V9(14) = V9(14) + W%(I, I1, 1)
            V9(6) = V9(6) + A(I, I1, 12)
            V9(7) = V9(7) + A(I, I1, 14): V9(8) = V9(8) + A(I, I1, 15): V9(9) = V9(9) + A(I, I1, 13): V9(11) = V9(11) + A(I, I1, 24): V9(12) = V9(12) + A(I, I1, 23)
            V9(10) = V9(10) + ZZ(I, I1)
        8970 Next I1: If V9(3) = 0 Then V9(3) = 1
        Print #2,: Print #2, "TOTALS"; Tab(14); Int(V9(10) / 60); Tab(21);: Print #2, Using "##"; V9(0);: Print #2, "-";: Print #2, Using "###"; V9(1);
        Print #2, Tab(28); Using "##"; V9(13);: Print #2, "-";: Print #2, Using "##"; V9(14);
        Print #2, ; Tab(35); Using "##"; V9(2);: Print #2, "-";: Print #2, Using "##"; V9(3);
        Print #2, ; Tab(42); Using "##  ## ### ##  ##  ##  ##  ##"; V9(11), V9(4), V9(5), V9(6), V9(7), V9(8), V9(9), V9(12)
        Print #2, ; Tab(22); Int(((V9(0) / V9(1)) * 100) + .5); "%"; Tab(35); Int(((V9(2) / V9(3)) * 100) + .5); "%": Print #2,: Print #2, "TOTAL TURNOVERS -"; T0(I): Print #2, "TOTAL REBOUNDS  -"; R3(I)
        For I2 = 0 To 14: V9(I2) = 0: Next I2
    Next I
    Print #2, Chr$(12): Close 2
End Sub



'------------------------------
'    STAMINASORT Subroutine
'------------------------------
' ...explanation...
Sub STAMINASORT
    8800 M = 13
    L = 12
    8802 S6 = 0: S7 = 0
    8803 H1 = 0: H2 = 0
    If S3(S6) <= S3(S6 + 1) Then 8809
    H1 = S3(S6): H2 = F1(S6)
    S3(S6) = S3(S6 + 1): F1(S6) = F1(S6 + 1)
    S3(S6 + 1) = H1: F1(S6 + 1) = H2
    S7 = S6
    8809 S6 = S6 + 1
    If S6 > L Then 8812
    GoTo 8803
    8812 If S7 = 0 Then 8813
    L = S7 - 1: GoTo 8802

8813 End Sub


'------------------------------
'    REBOUNDSORT Subroutine
'------------------------------
' ...explanation...
Sub REBOUNDSORT
    8825 M = 4
    L = 3
    8827 S6 = 0: S7 = 0
    8828 H1 = 0: H2 = 0
    If G5(S6) <= G5(S6 + 1) Then 8834
    H1 = G5(S6): H2 = G4(S6)
    G5(S6) = G5(S6 + 1): G4(S6) = G4(S6 + 1)
    G5(S6 + 1) = H1: G4(S6 + 1) = H2
    S7 = S6
    8834 S6 = S6 + 1
    If S6 > L Then 8837
    GoTo 8828
    8837 If S7 = 0 Then 8838
    L = S7 - 1
    GoTo 8827

8838 End Sub


'------------------------------
'    COMCOACH Subroutine
'------------------------------
' ...explanation...
Sub COMCOACH
    'COMPUTER COACH FOR SUBSTITUTIONS AND AI

    For I = 0 To 13
        F1(I) = I
        S3(I) = A(P9, I, 3) - A(P9, I, 7) - A(P9, I, 11) - A(P9, I, 12)
        If H > 2 Then S3(I) = A(P9, I, 3)
        If H = 2 And T <= 120 Then S3(I) = A(P9, I, 3)
        If A(P9, I, 12) >= NF% Then S3(I) = 0
    Next I
    Call STAMINASORT
    I = 14
    For I1 = 0 To 4
        8794 I = I - 1
        If I < 0 Then 8700
        If B%(P9, F1(I), 18) <> 0 Or AA$(P9, F1(I)) = "XXX" Or A(P9, F1(I), 12) >= NF% Or A(P9, F1(I), 12) >= NF% - 2 And H = 1 Or A(P9, F1(I), 12) >= NF% - 1 And H = 2 And T > 720 Then 8794
        If H = 1 And S3(I) / A(P9, F1(I), 3) < .5 Then 8794
        If H = 1 And T >= 600 And S3(I) / A(P9, F1(I), 3) < .75 Then 8794
        If H = 2 And T >= 600 And S3(I) / A(P9, F1(I), 3) < .25 Then 8794
        If H = 2 And T >= 300 And T < 600 And S3(I) / A(P9, F1(I), 3) < .13 Then 8794
        If H = 2 And T < 300 And S3(I) / A(P9, F1(I), 3) < T / 2400 Then 8794
        If S3(I) <= 0 And H < 2 Or S3(I) <= 0 And H = 2 And T > 120 Then 8794
        G4(I1) = F1(I): G5(I1) = A(P9, F1(I), 2)
    Next I1
    8798 Call REBOUNDSORT
    For I = 0 To 4
        B1(P9, I) = G4(I)
    Next I
    Call AICOMCOACH
    GoTo 8799

    8700 I = 14
    For I1 = 0 To 4
        8705 I = I - 1
        If I < 0 Then 8690
        If B%(P9, F1(I), 18) <> 0 Or AA$(P9, F1(I)) = "XXX" Or A(P9, F1(I), 12) >= NF% Or A(P9, F1(I), 12) >= NF% - 1 And (H = 1 Or H = 2 And T > 720) Then 8705
        G4(I1) = F1(I): G5(I1) = A(P9, F1(I), 2)
    Next I1
    GoTo 8798
    8690 I = 14
    For I1 = 0 To 4
        8695 I = I - 1
        If B%(P9, F1(I), 18) <> 0 Or AA$(P9, F1(I)) = "XXX" Then 8695
        G4(I1) = F1(I): G5(I1) = A(P9, F1(I), 2)
    Next I1
    GoTo 8798

8799 End Sub


'------------------------------
'    TOURNEY Subroutine
'------------------------------
' ...explanation...
Sub TOURNEY
    'CHECK FOR FILLING IN TOURNEY SCHEDULE
    Open p$(0) + FF$ + ".TOU" As #1 Len = 90
    Y2 = LOF(1)
    Close 1

    NT = Y2 / 90
    For I = 1 To NT
        Open p$(0) + FF$ + ".TOU" As #1 Len = 90
        For X = 0 To 14
            Field #1, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
        Next
        Field #1, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
        Field #1, 64 As X$, 3 As Q$(20), 3 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
        Field #1, 84 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)
        Get #1, I
        VS1% = CVI(Q$(16)): HS1% = CVI(Q$(17))
        VT1$ = Q$(18): W1$ = Q$(19)
        For X = 0 To 14: NG1%(X) = CVI(Q$(X + 1)): Next
        For X = 0 To 3: YN1$(X) = Q$(20 + X): Next
        For X = 1 To 3: NG1%(X + 14) = CVI(Q$(X + 23)): Next
        Close 1

        If NG1%(15) <> NG%(15) Or NG%(15) = 17 Then 6108

        If S(0, 0) > S(1, 0) And NG1%(0) - 900 = NG%(17) Then NG1%(0) = NG%(0): YN1$(0) = YN$(0): VT1$ = VT$: GoSub 6105
        If S(1, 0) > S(0, 0) And NG1%(0) - 900 = NG%(17) Then NG1%(0) = NG%(1): YN1$(0) = YN$(1): VT1$ = W$: GoSub 6105
        If S(0, 0) > S(1, 0) And NG1%(1) - 900 = NG%(17) Then NG1%(1) = NG%(0): YN1$(1) = YN$(0): W1$ = VT$: GoSub 6105
        If S(1, 0) > S(0, 0) And NG1%(1) - 900 = NG%(17) Then NG1%(1) = NG%(1): YN1$(1) = YN$(1): W1$ = W$: GoSub 6105
        If S(0, 0) < S(1, 0) And NG1%(0) - 800 = NG%(17) Then NG1%(0) = NG%(0): YN1$(0) = YN$(0): VT1$ = VT$: GoSub 6105
        If S(1, 0) < S(0, 0) And NG1%(0) - 800 = NG%(17) Then NG1%(0) = NG%(1): YN1$(0) = YN$(1): VT1$ = W$: GoSub 6105
        If S(0, 0) < S(1, 0) And NG1%(1) - 800 = NG%(17) Then NG1%(1) = NG%(0): YN1$(1) = YN$(0): W1$ = VT$: GoSub 6105
        If S(1, 0) < S(0, 0) And NG1%(1) - 800 = NG%(17) Then NG1%(1) = NG%(1): YN1$(1) = YN$(1): W1$ = W$: GoSub 6105

        6108 If NG%(15) = 17 Then 6109
        If S(0, 0) > S(1, 0) And NG%(16) = 89 And NG1%(15) = 17 And NG1%(0) - 900 = NG%(15) Then NG1%(0) = NG%(0): YN1$(0) = YN$(0): VT1$ = VT$: GoSub 6105
        If S(1, 0) > S(0, 0) And NG%(16) = 89 And NG1%(15) = 17 And NG1%(0) - 900 = NG%(15) Then NG1%(0) = NG%(1): YN1$(0) = YN$(1): VT1$ = W$: GoSub 6105
        If S(0, 0) > S(1, 0) And NG%(16) = 89 And NG1%(15) = 17 And NG1%(1) - 900 = NG%(15) Then NG1%(1) = NG%(0): YN1$(1) = YN$(0): W1$ = VT$: GoSub 6105
        If S(1, 0) > S(0, 0) And NG%(16) = 89 And NG1%(15) = 17 And NG1%(1) - 900 = NG%(15) Then NG1%(1) = NG%(1): YN1$(1) = YN$(1): W1$ = W$: GoSub 6105

        6109 If S(0, 0) > S(1, 0) And NG%(15) = 17 And NG1%(0) - 1100 = NG%(17) Then NG1%(0) = NG%(0): YN1$(0) = YN$(0): VT1$ = VT$: GoSub 6105
        If S(1, 0) > S(0, 0) And NG%(15) = 17 And NG1%(0) - 1100 = NG%(17) Then NG1%(0) = NG%(1): YN1$(0) = YN$(1): VT1$ = W$: GoSub 6105
        If S(0, 0) > S(1, 0) And NG%(15) = 17 And NG1%(1) - 1100 = NG%(17) Then NG1%(1) = NG%(0): YN1$(1) = YN$(0): W1$ = VT$: GoSub 6105
        If S(1, 0) > S(0, 0) And NG%(15) = 17 And NG1%(1) - 1100 = NG%(17) Then NG1%(1) = NG%(1): YN1$(1) = YN$(1): W1$ = W$: GoSub 6105
        If S(0, 0) < S(1, 0) And NG%(15) = 17 And NG1%(0) - 1000 = NG%(17) Then NG1%(0) = NG%(0): YN1$(0) = YN$(0): VT1$ = VT$: GoSub 6105
        If S(1, 0) < S(0, 0) And NG%(15) = 17 And NG1%(0) - 1000 = NG%(17) Then NG1%(0) = NG%(1): YN1$(0) = YN$(1): VT1$ = W$: GoSub 6105
        If S(0, 0) < S(1, 0) And NG%(15) = 17 And NG1%(1) - 1000 = NG%(17) Then NG1%(1) = NG%(0): YN1$(1) = YN$(0): W1$ = VT$: GoSub 6105
        If S(1, 0) < S(0, 0) And NG%(15) = 17 And NG1%(1) - 1000 = NG%(17) Then NG1%(1) = NG%(1): YN1$(1) = YN$(1): W1$ = W$: GoSub 6105
        If S(0, 0) < S(1, 0) And NG%(15) = 17 And NG1%(0) - 900 = NG%(17) Then NG1%(0) = NG%(0): YN1$(0) = YN$(0): VT1$ = VT$: GoSub 6105
        If S(1, 0) < S(0, 0) And NG%(15) = 17 And NG1%(0) - 900 = NG%(17) Then NG1%(0) = NG%(1): YN1$(0) = YN$(1): VT1$ = W$: GoSub 6105
        If S(0, 0) < S(1, 0) And NG%(15) = 17 And NG1%(1) - 900 = NG%(17) Then NG1%(1) = NG%(0): YN1$(1) = YN$(0): W1$ = VT$: GoSub 6105
        If S(1, 0) < S(0, 0) And NG%(15) = 17 And NG1%(1) - 900 = NG%(17) Then NG1%(1) = NG%(1): YN1$(1) = YN$(1): W1$ = W$: GoSub 6105
    Next
    GoTo 6106

    6105 'UPDATE FILE
    Open p$(0) + FF$ + ".TOU" As #1 Len = 90
    For X = 0 To 14
        Field #1, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
    Next
    Field #1, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
    Field #1, 64 As X$, 3 As Q$(20), 3 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
    Field #1, 84 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)
    For X = 0 To 14: LSet Q$(X + 1) = MKI$(NG1%(X)): Next
    For X = 0 To 3: LSet Q$(20 + X) = YN1$(X): Next
    For X = 1 To 3: LSet Q$(X + 23) = MKI$(NG1%(X + 14)): Next
    LSet Q$(16) = MKI$(0): LSet Q$(17) = MKI$(0)
    LSet Q$(18) = VT1$: LSet Q$(19) = W1$
    Put #1, I
    Close 1

    Return

6106 End Sub


'------------------------------
'    STATCONVERT Subroutine
'------------------------------
' ...explanation...
Sub STATCONVERT (Q, QQ)
    For XX = 4 To 11
        If Mid$(B$(Q), XX, 1) = " " Or Mid$(B$(Q), XX, 1) = "." Then Mid$(B$(Q), XX, 1) = "_"
    Next XX
    For XX = 12 To 14
        If Mid$(B$(Q), XX, 2) = "ST" Then Mid$(B$(Q), 9, 1) = "_": Mid$(B$(Q), 10, 2) = "ST"
    Next XX
    QQ = 8
    For XX = 11 To 4 Step -1
        If Mid$(B$(Q), XX, 1) = "_" Then QQ = XX - 4: GoTo 6615
        GoTo 6620
    6615 Next XX

6620 End Sub


'------------------------------
'    TICKERCHECK Subroutine
'------------------------------
' ...explanation...
Sub TICKERCHECK ()
    If AP <> 1 Or AP%(2) > 31 Then 2581
    If H = 1 And T <= 960 And SX%(AP%(2) + 1, 0, 1) = -1 Then SX%(AP%(2) + 1, 0, 1) = S(0, 0): SX%(AP%(2) + 1, 1, 1) = S(1, 0): GoTo 2581
    If H = 1 And T <= 720 And SX%(AP%(2) + 1, 0, 2) = -1 Then SX%(AP%(2) + 1, 0, 2) = S(0, 0): SX%(AP%(2) + 1, 1, 2) = S(1, 0): GoTo 2581
    If H = 1 And T <= 480 And SX%(AP%(2) + 1, 0, 3) = -1 Then SX%(AP%(2) + 1, 0, 3) = S(0, 1): SX%(AP%(2) + 1, 1, 3) = S(1, 1): GoTo 2581
    If H = 1 And T <= 240 And SX%(AP%(2) + 1, 0, 4) = -1 Then SX%(AP%(2) + 1, 0, 4) = S(0, 0): SX%(AP%(2) + 1, 1, 4) = S(1, 0): GoTo 2581
    If H = 1 And T <= 120 And SX%(AP%(2) + 1, 0, 5) = -1 Then SX%(AP%(2) + 1, 0, 5) = S(0, 0): SX%(AP%(2) + 1, 1, 5) = S(1, 0): GoTo 2581
    If H > 1 And SX%(AP%(2) + 1, 0, 6) = -1 Then For I2 = 6 To 8: SX%(AP%(2) + 1, 0, I2) = S(0, 2) + S(0, 1): SX%(AP%(2) + 1, 1, I2) = S(1, 2) + S(1, 1): Next: GoTo 2581
    If H = 2 And T <= 960 And SX%(AP%(2) + 1, 0, 9) = -1 Then SX%(AP%(2) + 1, 0, 9) = S(0, 0): SX%(AP%(2) + 1, 1, 9) = S(1, 0): GoTo 2581
    If H = 2 And T <= 720 And SX%(AP%(2) + 1, 0, 10) = -1 Then SX%(AP%(2) + 1, 0, 10) = S(0, 0): SX%(AP%(2) + 1, 1, 10) = S(1, 0): GoTo 2581
    If H = 2 And T <= 480 And SX%(AP%(2) + 1, 0, 11) = -1 Then SX%(AP%(2) + 1, 0, 11) = S(0, 3) + S(0, 2) + S(0, 1): SX%(AP%(2) + 1, 1, 11) = S(1, 3) + S(1, 2) + S(1, 1): GoTo 2581
    If H = 2 And T <= 240 And SX%(AP%(2) + 1, 0, 12) = -1 Then SX%(AP%(2) + 1, 0, 12) = S(0, 0): SX%(AP%(2) + 1, 1, 12) = S(1, 0): GoTo 2581
    If H = 2 And T <= 120 And SX%(AP%(2) + 1, 0, 13) = -1 Then SX%(AP%(2) + 1, 0, 13) = S(0, 0): SX%(AP%(2) + 1, 1, 13) = S(1, 0): GoTo 2581
    SX%(AP%(2) + 1, 0, 14) = S(0, 0): SX%(AP%(2) + 1, 1, 14) = S(1, 0)
    2581 '
End Sub


'------------------------------
'    MAINSCREEN Subroutine
'------------------------------
' ...explanation...
Sub MAINSCREEN ()
    'M A I N   S C R E E N
    Call TICKERCHECK
    Locate 2, 1: Locate , 9: Color TR%(0, 0), TR%(0, 1): Print B$(0);
    Locate 1, 37: Color 15, 0: Print Using "##"; Int(T / 60);: Print ":";
    If T <= 5 Then Print "0";: Print Using "#.#"; T - Int(T / 60) * 60;: GoTo 2171
    If T - Int(T / 60) * 60 < 10 Then Print "0";: Print Using "#"; T - Int(T / 60) * 60;: GoTo 2171
    Print Using "##"; T - Int(T / 60) * 60;
    2171 Locate 2, 56: Color TR%(1, 0), TR%(1, 1): Print B$(1)
    Locate 4, 13: Color 14, 0: Print Using "###"; S(0, 0);
    Locate 2, 33: Print " ";: If PA% = 0 Then Locate , 33: Print Chr$(17);
    Locate 2, 35: Print Using "  HALF #"; H;: Locate , 44: Print " ";
    If PA% = 1 Then Locate , 44: Print Chr$(16);
    If H > 2 Then Locate , 35: Print Using "OVERTIME #"; H - 2;: Locate , 46: Print " ";: If PA% = 1 Then Locate , 46: Print Chr$(16)
    Locate 4, 60: Color 14, 0: Print Using "###"; S(1, 0)
    Color 7: Locate 3, 35: Print Using "##  "; T0(0);: Color 12: Print "TO  ";: Color 7: Print Using "##"; T0(1)
    Locate , 35: Print Using "##  "; R3(0);: Color 12: Print "RB  ";: Color 7: Print Using "##"; R3(1)
    Locate , 35: Print Using "##  "; P2(0);: Color 12: Print "TF  ";: Color 7: Print Using "##"; P2(1);
    If P2(0) > 5 Then Locate , 46: Color 0, 2: Print "BONUS";: Color 7, 0
    If P2(1) > 5 Then Locate , 29: Color 0, 2: Print "BONUS": Color 7, 0
    Locate 6, 9: Print "TIMEOUTS:"; T1(0);: Locate , 56: Print "TIMEOUTS:"; T1(1)
    Color 14, 0: Locate 6, 37: Print Using " :##"; X8
    If C7 = 0 Or T < 45 And X8 >= T And C7 = 1 Or T < 35 And X8 >= T And C7 = 2 Or (C7 = 1 Or C7 = 2) And H > 2 And T = 300 Then Locate 6, 37: Print " :  "

    If displayPause = 0 Then 2172
    Locate 9, 41
    Color TR%(D, 0), TR%(D, 1): Print "DEF-"; DB$(DT%(D))
    Locate , 41: Color TR%(p, 0), TR%(p, 1): Print "OFF-"; OB$(OF%(p))
    Color 15, 0
    Locate 11, 41: Color 14: Print "ENTER ";: Color 15: Print "PASS"
    Locate , 41: Color 14: Print "(1)  ";: Color 15: Print "SHOOT"
    Locate , 41: Color 14: Print "(2)  ";: Color 15: Print B$(p); " CALL TIMEOUT     "
    Locate , 41: Color 14: Print "(T)  ";: Color 15: Print "10 MIN TICKER"
    Locate , 41: Color 14: Print "(D)  ";: Color 15: Print "CHANGE DEFENSE FROM 2-3 ZONE"
    Color 15, 0
    Locate 19, 1
    For I1 = 0 To 4: Color 2, 0: Print Using "# "; I1;
        Color 3
        Color 15: Print Left$(AA$(0, B1(0, I1)), 12); Tab(15);
        If A(0, B1(0, I1), 19) >= 7 Then Print "*";
        Print ; Tab(16);
        J = (A(0, B1(0, I1), 3)) - A(0, B1(0, I1), 7) - A(0, B1(0, I1), 11) - A(0, B1(0, I1), 12): If J <= 0 Then J = 0
        Print Using "## ## "; A(0, B1(0, I1), 0), A(0, B1(0, I1), 1);
        If p = 0 Then Print Using "##"; A(0, B1(0, I1), 21) * 2 / 10; Else Print Using "##"; A(0, B1(0, I1), 2) * 2 / 10;
        If J <= 0 And (H = 1 Or H = 2 And T > 120) Then Color 12
        Print Using "### "; J;
        Color 15
        If FA%(0, B1(0, I1)) * -1 >= 5 And (H = 1 Or H = 2 And T > 120) Then Color 12
        Print Using "## "; FA%(0, B1(0, I1)) * -1;
        Color 15
        Print Using "##"; A(0, B1(0, I1), 4);
        Print Using "###  "; A(0, B1(0, I1), 8) * 2 + A(0, B1(0, I1), 10) + W%(0, B1(0, I1), 0);
        If A(0, B1(0, I1), 12) >= 4 Then Color 12
        Print Using "#"; A(0, B1(0, I1), 12)
        Color 15
    Next I1
    Locate 19, 40
    For I1 = 0 To 4: Locate , 40: Color 2, 0: Print Using "# "; I1;
        Color 15: Print Left$(AA$(1, B1(1, I1)), 12); Tab(55);
        If A(1, B1(1, I1), 19) >= 7 Then Print "*";
        Print ; Tab(56);
        J = (A(1, B1(1, I1), 3)) - A(1, B1(1, I1), 7) - A(1, B1(1, I1), 11) - A(1, B1(1, I1), 12): If J <= 0 Then J = 0
        Print Using "## ## "; A(1, B1(1, I1), 0), A(1, B1(1, I1), 1);
        If p = 1 Then Print Using "##"; A(1, B1(1, I1), 21) * 2 / 10; Else Print Using "##"; A(1, B1(1, I1), 2) * 2 / 10;
        If J <= 0 And (H = 1 Or H = 2 And T > 120) Then Color 12
        Print Using "### "; J;
        Color 15
        If FA%(1, B1(1, I1)) * -1 >= 5 And (H = 1 Or H = 2 And T > 120) Then Color 12
        Print Using "## "; FA%(1, B1(1, I1)) * -1;
        Color 15
        Print Using "##"; A(1, B1(1, I1), 4);
        Print Using "###  "; A(1, B1(1, I1), 8) * 2 + A(1, B1(1, I1), 10) + W%(1, B1(1, I1), 0);
        If A(1, B1(1, I1), 12) >= 4 Then Color 12
        Print Using "#"; A(1, B1(1, I1), 12)
        Color 15
    Next I1
    2172 '
End Sub


'------------------------------
'    SCOREBOARD Subroutine
'------------------------------
' ...explanation...
Sub SCOREBOARD ()
    '*** SCREEN UPDATING ***
    Cls
    Call TICKERCHECK
    Color 9, 0
    Locate 3, 11: Print Chr$(218); String$(5, 196); Chr$(191)
    Locate , 11: Print Chr$(179);: Locate , 17: Print Chr$(179)
    Locate , 11: Print Chr$(192); String$(5, 196); Chr$(217)
    Locate 3, 58: Print Chr$(218); String$(5, 196); Chr$(191)
    Locate , 58: Print Chr$(179);: Locate , 64: Print Chr$(179)
    Locate , 58: Print Chr$(192); String$(5, 196); Chr$(217)
    Locate 2, 1: Locate , 9: Color TR%(0, 0), TR%(0, 1): Print B$(0);
    Locate 1, 37: Color 15, 0: Print Using "##"; Int(T / 60);: Print ":";
    If T - Int(T / 60) * 60 < 10 Then Print "0";: Print Using "#"; T - Int(T / 60) * 60;: GoTo 2505
    Print Using "##"; T - Int(T / 60) * 60;
    2505 Locate 2, 56: Color TR%(1, 0), TR%(1, 1): Print B$(1)
    Locate 4, 13: Color 14, 0: Print Using "###"; S(0, 0);
    Locate 2, 35: Print Using "  HALF #"; H;
    If H > 2 Then Locate , 35: Print Using "OVERTIME #"; H - 2
    Locate 4, 60: Color 14, 0: Print Using "###"; S(1, 0)
    Color 7: Locate 3, 35: Print Using "##  "; T0(0);: Color 12: Print "TO  ";: Color 7: Print Using "##"; T0(1)
    Locate , 35: Print Using "##  "; R3(0);: Color 12: Print "RB  ";: Color 7: Print Using "##"; R3(1)
    Locate , 35: Print Using "##  "; P2(0);: Color 12: Print "TF  ";: Color 7: Print Using "##"; P2(1);
    If P2(0) > 5 Then Locate , 46: Color 0, 2: Print "BONUS";: Color 7, 0
    If P2(1) > 5 Then Locate , 29: Color 0, 2: Print "BONUS": Color 7, 0
    Locate 6, 9: Print "TIMEOUTS:"; T1(0);: Locate , 56: Print "TIMEOUTS:"; T1(1)
    Locate 9, 41
    Color TR%(D, 0), TR%(D, 1): Print "DEF-"; DB$(DT%(D))
    Locate , 41: Color TR%(p, 0), TR%(p, 1): Print "OFF-"; OB$(OF%(p))
    Color 15, 0
    Locate 11, 41: Color 14: Print "ENTER";: Color 14: Print "-PASS"
    Locate , 41: Color 14: Print "(1)  ";: Color 15: Print "SHOOT"
    Locate , 41: Color 14: Print "(2)  ";: Color 15: Print B$(p); " CALL TIMEOUT     "
    Locate , 41: Color 14: Print "(T)  ";: Color 15: Print "10 MIN TICKER"
    Locate , 41: Color 14: Print "(D)  ";: Color 15: Print "CHANGE DEFENSE FROM 2-3 ZONE"
    Locate 7, 1
    Color 9, 0: Print String$(38, 205); Chr$(203); String$(38, 205)
    Locate 8, 1
    For I = 1 To 8
        Locate , 39
        Print Chr$(186)
    Next
    Locate 17, 1
    Color TR%(0, 0), TR%(0, 1)
    Print B$(0);

    Print Using " [##"; Z5(0);: Print "-";: Print Using "##]"; Z6(0)
    Color 14, 0: Print "# NAME         FG FT RB CX EF PA PT  F"
    For I1 = 0 To 4
        Color 2: Print Using "# "; I1;
        Color 3
        Color 15: Print Left$(AA$(0, B1(0, I1)), 12);: Locate , 15
        If A(0, B1(0, I1), 19) >= 7 Then Print "*";
        Locate , 16
        J = (A(0, B1(0, I1), 3)) - A(0, B1(0, I1), 7) - A(0, B1(0, I1), 11) - A(0, B1(0, I1), 12): If J <= 0 Then J = 0
        Print Using "##"; A(0, B1(0, I1), 0);
        Print Using "### "; A(0, B1(0, I1), 1);
        If p = 0 Then Print Using "##"; A(0, B1(0, I1), 21) * 2 / 10; Else Print Using "##"; A(0, B1(0, I1), 2) * 2 / 10;
        If J <= 0 And (H = 1 Or H = 2 And T > 120) Then Color 12
        Print Using "### "; J;
        Color 15
        If FA%(0, B1(0, I1)) * -1 >= 5 And (H = 1 Or H = 2 And T > 120) Then Color 12
        Print Using "## "; FA%(0, B1(0, I1)) * -1;
        Color 15
        Print Using "##"; A(0, B1(0, I1), 4);
        Print Using "###  "; A(0, B1(0, I1), 8) * 2 + A(0, B1(0, I1), 10) + W%(0, B1(0, I1), 0);
        If A(0, B1(0, I1), 12) >= 4 Then Color 12
        Print Using "#"; A(0, B1(0, I1), 12)
        Color 15
    Next I1
    Locate 17, 40: Color TR%(1, 0), TR%(1, 1): Print B$(1);: Print Using " [##"; Z5(1);: Print "-";: Print Using "##]"; Z6(1)
    Color 14, 0: Locate , 40: Print "# NAME          FG FT RB CX EF PA PT  F"
    For I1 = 0 To 4: Locate , 40: Color 2: Print Using "# "; I1;
        Color 15: Print Left$(AA$(1, B1(1, I1)), 12);: Locate , 55
        If A(1, B1(1, I1), 19) >= 7 Then Print "*";
        Locate , 56
        J = (A(1, B1(1, I1), 3)) - A(1, B1(1, I1), 7) - A(1, B1(1, I1), 11) - A(1, B1(1, I1), 12): If J <= 0 Then J = 0
        Print Using "##"; A(1, B1(1, I1), 0);
        Print Using "### "; A(1, B1(1, I1), 1);
        If p = 1 Then Print Using "##"; A(1, B1(1, I1), 21) * 2 / 10; Else Print Using "##"; A(1, B1(1, I1), 2) * 2 / 10;
        If J <= 0 And (H = 1 Or H = 2 And T > 120) Then Color 12
        Print Using "### "; J;
        Color 15
        If FA%(1, B1(1, I1)) * -1 >= 5 And (H = 1 Or H = 2 And T > 120) Then Color 12
        Print Using "## "; FA%(1, B1(1, I1)) * -1;
        Color 15
        Print Using "##"; A(1, B1(1, I1), 4);
        Print Using "###  "; A(1, B1(1, I1), 8) * 2 + A(1, B1(1, I1), 10) + W%(1, B1(1, I1), 0);
        If A(1, B1(1, I1), 12) >= 4 Then Color 12
        Print Using "#"; A(1, B1(1, I1), 12)
        Color 15
    Next I1
    Color 9, 0
    For I = 1 To 9: Locate 14 + I, 39: Print Chr$(186): Next
    Color 9, 0
    Locate 16, 1: Print String$(79, 205)
    Locate 16, 39: Print Chr$(206): Color 7, 0
End Sub


'------------------------------
'    COLCNG Subroutine
'------------------------------
' ...explanation...
Sub COLCNG ()
    If TR%(1, 0) = 15 And TR%(1, 1) = 4 Then TR%(1, 0) = 4: TR%(1, 1) = 7
    If TR%(1, 0) = 15 And TR%(1, 1) = 1 Then TR%(1, 0) = 1: TR%(1, 1) = 7
    If TR%(1, 0) = 0 And TR%(1, 1) = 4 Then TR%(1, 0) = 12: TR%(1, 1) = 0
    If TR%(1, 0) = 1 And TR%(1, 1) = 7 Then TR%(1, 0) = 15: TR%(1, 1) = 1
    If TR%(1, 0) = 4 And TR%(1, 1) = 7 Then TR%(1, 0) = 15: TR%(1, 1) = 4
End Sub


'------------------------------
'    LOADSTATS Subroutine
'------------------------------
' ...explanation...
Sub LOADSTATS
    If TR%(1, 0) = TR%(0, 0) And TR%(1, 1) = TR%(0, 1) Then Call COLCNG
    Color 15, 0
    For I = 0 To 1
        Cls
        Call DRAWBOX
        64 Color 15, 0
        Y$(I) = B$(I)
        Q = I
        Call STATCONVERT(Q, QQ)
        M$ = Mid$(B$(I), 4, QQ)
        R$ = M$ + "." + Left$(B$(I), 2)
        R$ = R$ + "S"
        If Not _FileExists(p$(1) + R$) Then 66
        Open p$(1) + R$ For Input As #2
        Input #2, YY%
        For I1 = 1 To 100: Input #2, O%(I1): Next
        For I1 = 1 To 100: Input #2, HO$(I1): Next
        For I1 = 1 To 100: Input #2, HT%(I1): Next
        For I1 = 1 To 100: Input #2, HO$(I1): Next
        For I1 = 1 To 100
            For I2 = 0 To 1
                Input #2, APT%(I1, I2)
            Next
        Next
        For I1 = 1 To 100: Input #2, CRD(I1): Next I1
        For I1 = 1 To 100: Input #2, SITE$(I1): Next I1
        Close 2

        For I1 = 1 To YY%
            If O%(I1) > HT%(I1) Then Z5(I) = Z5(I) + 1
            If O%(I1) < HT%(I1) Then Z6(I) = Z6(I) + 1
        Next
        66 B$(I) = Y$(I)
    Next
End Sub


'------------------------------
'    OFFREBOUND Subroutine
'------------------------------
' ...explanation...
Sub OFFREBOUND ()
    RB = 0
    For I = 0 To 4: RB(I) = 0
        XZ = A(p, B1(p, I), 21)
        If H > 2 Or H = 2 And T <= 120 Then 793
        If A(p, B1(p, I), 3) - A(p, B1(p, I), 7) - A(p, B1(p, I), 11) - A(p, B1(p, I), 12) <= 0 Then XZ = Int(XZ * .5 + .5)
        793 If FA%(p, B1(p, I)) <= -5 Then XZ = Int(XZ * .9 + .5)
        RB(I) = RB(I) + XZ: RB = RB + XZ
    Next
    RN = Rnd(1) * RB
    For B = 0 To 4
        If RN <= RB(B) Then 794
        RB(B + 1) = RB(B + 1) + RB(B)
    Next
    B = Int(Rnd(1) * 5)

794 End Sub


'------------------------------
'    DEFREBOUND Subroutine
'------------------------------
' ...explanation...
Sub DEFREBOUND ()
    RB = 0
    For I = 0 To 4: RB(I) = 0
        XZ = A(1 - p, B1(1 - p, I), 2)
        If H > 2 Or H = 2 And T <= 120 Then 798
        If A(1 - p, B1(1 - p, I), 3) - A(1 - p, B1(1 - p, I), 7) - A(1 - p, B1(1 - p, I), 11) - A(1 - p, B1(1 - p, I), 12) <= 0 Then XZ = Int(XZ * .5 + .5)
        798 If FA%(1 - p, B1(1 - p, I)) <= -5 Then XZ = Int(XZ * .9 + .5)
        RB(I) = RB(I) + XZ: RB = RB + XZ
    Next
    RN = Rnd(1) * RB
    For B = 0 To 4
        If RN <= RB(B) Then 799
        RB(B + 1) = RB(B + 1) + RB(B)
    Next
    B = Int(Rnd(1) * 5)

799 End Sub


'------------------------------
'    EDITCX Subroutine
'------------------------------
' ...explanation...
Sub EDITCX ()

    9300 If AP = 1 And U5 = 2 Then 9499
    Color 15, 0: Cls
    Print "EDIT CONTRIBUTION PERCENTAGES OF PLAYERS FROM:"
    Print
    Color 14: Print "(0) - ";: Color TR%(0, 0), TR%(0, 1): Print B$(0): Color , 0
    Print
    Color 14: Print "(1) - ";: Color TR%(1, 0), TR%(1, 1): Print B$(1): Color , 0
    Print
    Color 14: Print "(2) - ";: Color 3: Print "PROCEED TO GAME": Color 15, 0
    Print
    Print "NOTE:: THE CONTRIBUTION RATING CONSISTS OF THE PLAYER'S"
    Print "       FREQUENCY TO SHOOT, REBOUND, AND COMMIT FOULS."

    9301 I$ = GetKeyPress$
    I = Val(I$): If I < 0 Or I > 2 Then 9301
    If I = 2 Then 9499
    Cls
    TX = 0
    For I1 = 0 To 13: TX = TX + A(I, I1, 3): Next
    For I1 = 0 To 13: A(I, I1, 3) = A(I, I1, 3) / TX * 100: Next

    9305 Locate 2, 1: Print "CONTRIBUTION RATINGS FOR ";: Color TR%(I, 0), TR%(I, 1): Print B$(I): Color 15, 0
    Print: Print " #  PLAYER             CX"
    TY% = 0
    For I1 = 0 To 13: TY% = TY% + A(I, I1, 3)
        If B%(I, I1, 18) = -1 Then Color TR%(I, 0), TR%(I, 1)
        Print Using "## "; I1 + 1;: Print AA$(I, I1);: Locate , 24: Print Using "##"; A(I, I1, 3)
        Color 15, 0
    Next
    Locate , 23: Print Using "###"; TY%
    Input "SELECT PLAYER TO CHANGE (0 TO CONTINUE)"; I1: I1 = I1 - 1
    If I1 > 13 Or I1 < 0 Then 9310
    Locate 22, 1: Print "ENTER NEW CX RATING FOR "; AA$(I, I1); " ";: Input A(I, I1, 3)
    9306 Locate 22, 1: Print "                                                    ": GoTo 9305
    9310 If TY% = 100 Then 9320
    Locate 22, 1: Print "TOTAL MUST ADD UP TO 100%! PRESS ANY KEY"
    I$ = GetKeyPress$
    GoTo 9306
    9320 For I1 = 0 To 13: A(I, I1, 3) = A(I, I1, 3) / 100 * TX: Next
    GoTo 9300
    9499 X = 0
End Sub


'------------------------------
'    DELAY Subroutine
'------------------------------
' ...explanation...
Sub DELAY ()
    'We don't really need the If statement here
    'If displayPause <> 0 Then
    _Delay displayPause
    'End If
End Sub


'------------------------------
'    CLEARPBP Subroutine
'------------------------------
' ...explanation...
Sub CLEARPBP ()
    If displayPause = 0 Or PB% = 1 Then 9750
    NL% = 1: A1$ = "": B1$ = "": C1$ = "": D1$ = "": E1$ = "": F1$ = "": G1$ = "": H1$ = "": J$ = ""
    9750 '
End Sub


'------------------------------
'    ZONECHANGE Subroutine
'------------------------------
' ...explanation...
Sub ZONECHANGE ()
    If U5 = 1 And CT = D Or U5 = 2 Then 8155
    If DT%(D) <> 2 Or DT%(D) <> 7 Or DT%(D) <> 12 Then 8155
    If OF%(p) > 8 Or OF%(p) < 4 Then 8155

    JJ = 8
    Call FRAME

    Print B$(D); " SELECT STRATEGY:"
    Locate 12, 14
    Locate , 14: Print "0 - SOLID MAN TO MAN"
    Locate , 14: Print "1 - PRESSURE MAN TO MAN"
    Locate , 14: Print "2 - 1-3-1 TRAPPING ZONE"
    8103 I$ = GetKeyPress$
    I1 = Val(I$): If I1 < 0 Or I1 > 2 Then 8103
    If I1 = 2 Then I1 = 3
    If DT%(D) <= 3 Then DT%(D) = I1
    If DT%(D) = 7 Then DT%(D) = I + 8
    If DT%(D) = 10 Then DT%(D) = 3
    If DT%(D) = 12 Then DT%(D) = I + 10
    If DT%(D) = 12 Then DT%(D) = 13
    P9 = D
    If DT%(P9) <= 3 Then P7(P9) = 0
    If DT%(P9) = 4 Then P7(P9) = 1
    If DT%(P9) = 5 Or DT%(P9) = 6 Then P7(P9) = 2
    If DT%(P9) > 6 And DT%(P9) <= 10 Then P7(P9) = 3
    If DT%(P9) > 9 And DT%(P9) < 14 Then P7(P9) = 4
    If DT%(P9) = 14 Then P7(P9) = 5
    If DT%(P9) = 0 Or DT%(P9) = 4 Or DT%(P9) = 5 Or DT%(P9) = 8 Or DT%(P9) = 10 Then D2(P9) = 0
    If DT%(P9) = 1 Or DT%(P9) = 6 Or DT%(P9) = 9 Or DT%(P9) = 11 Then D2(P9) = 1
    If DT%(P9) = 2 Or DT%(P9) = 7 Or DT%(P9) = 12 Then D2(P9) = 2
    If DT%(P9) = 3 Or DT%(P9) = 13 Then D2(P9) = 3
    Color 15, 0
    8155 Color 7, 0

End Sub


'------------------------------
'    DEFENSEMENU Subroutine
'------------------------------
' ...explanation...
Sub DEFENSEMENU ()
    110 Cls
    Print ; Tab(33); "DEFENSIVE STYLE": Print
    For I = 0 To 14
        Locate , 5: Print Using "# "; I;
        If I = 10 Then Locate , 5: Print "A ";
        If I = 11 Then Locate , 5: Print "B ";
        If I = 12 Then Locate , 5: Print "C ";
        If I = 13 Then Locate , 5: Print "D ";
        If I = 14 Then Locate , 5: Print "E ";
        Print D$(I)
    Next
    Print
    Locate 19, 5: Print "SELECT DEFENSIVE STYLE FOR "; B$(P9)
    107 I$ = GetKeyPress$
    I1 = Val(I$)
    If UCase$(I$) = "A" Then I1 = 10
    If UCase$(I$) = "B" Then I1 = 11
    If UCase$(I$) = "C" Then I1 = 12
    If UCase$(I$) = "D" Then I1 = 13
    If UCase$(I$) = "E" Then I1 = 14
    If I1 < 0 Or I1 > 14 Then 107

    DT%(P9) = I1
    If DT%(P9) <= 3 Then P7(P9) = 0
    If DT%(P9) = 4 Then P7(P9) = 1
    If DT%(P9) = 5 Or DT%(P9) = 6 Then P7(P9) = 2
    If DT%(P9) > 6 And DT%(P9) <= 10 Then P7(P9) = 3
    If DT%(P9) > 9 And DT%(P9) < 14 Then P7(P9) = 4
    If DT%(P9) = 14 Then P7(P9) = 5
    If DT%(P9) = 14 And (H < 2 Or H >= 2 And T > 180) Then Beep: Locate 23, 1: Print "CAN ONLY USE LAST 3 MIN OF 2ND HALF OR OT": I$ = GetKeyPress$: GoTo 110
    If DT%(P9) = 0 Or DT%(P9) = 4 Or DT%(P9) = 5 Or DT%(P9) = 8 Or DT%(P9) = 10 Then D2(P9) = 0
    If DT%(P9) = 1 Or DT%(P9) = 6 Or DT%(P9) = 9 Or DT%(P9) = 11 Then D2(P9) = 1
    If DT%(P9) = 2 Or DT%(P9) = 7 Or DT%(P9) = 12 Then D2(P9) = 2
    If DT%(P9) = 3 Or DT%(P9) = 13 Then D2(P9) = 3
    Color 15, 0
End Sub


'------------------------------
'    SETSTAMINA Subroutine
'------------------------------
' ...explanation...
Sub SETSTAMINA ()
    SQ% = 0
    For I = 0 To 13
        If TR%(P9, 5) = 0 And C(P9, I, 0) = 0 And B%(P9, I, 18) = 0 Then SQ% = SQ% + A(P9, I, 3)
        108 If TR%(P9, 5) <> 0 And C(P9, I, 0) <> 0 And B%(P9, I, 18) = 0 Then SQ% = SQ% + A(P9, I, 3) * TR%(P9, 5) / C(P9, I, 0): A(P9, I, 3) = A(P9, I, 3) * TR%(P9, 5) / C(P9, I, 0)
    117 Next
    For I = 0 To 13
        116 If TR%(P9, 5) = 0 And C(P9, I, 0) = 0 And B%(P9, I, 18) = 0 Then A(P9, I, 3) = Int(A(P9, I, 3) / SQ% * S9 + .5)
        118 If TR%(P9, 5) <> 0 And C(P9, I, 0) <> 0 And B%(P9, I, 18) = 0 Then A(P9, I, 3) = Int(A(P9, I, 3) / SQ% * S9 + .5)
        112 If A(P9, I, 3) < 1 Then A(P9, I, 3) = 1
    Next I
    SQ% = 0
    For I = 0 To 13
        113 If B%(P9, I, 18) = 0 Then SQ% = SQ% + A(P9, I, 3)
    Next
    For I = 0 To 13
        114 If S9 > 120 And F5%(P9, 8) <> 99 Then A(P9, I, 16) = A(P9, I, 16) * (120 / S9): A(P9, I, 16) = Int(A(P9, I, 16) + .5)
    Next I
    DY = 7
    115 If S9 > 114 Then DY = (114 / S9) * DY: DY = Int(DY + .5)
End Sub


'------------------------------
'       FRAME Subroutine
'------------------------------
' ...explanation...
Sub FRAME ()
    SN = 1
    Locate 8, 8

    Color TR%(TS%, 0), TR%(TS%, 1)

    'Print box top
    Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

    'Print box sides
    For II = 1 To JJ
        Locate 8 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
    Next

    'Print box bottom
    Locate 9 + JJ, 8
    Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

    Locate 10, 15
End Sub


'------------------------------
'   COMPUTERSUB Subroutine
'------------------------------
' ...explanation...
Sub COMPUTERSUB ()
    930 If H >= 2 And T <= 60 And X3% = 1 And (S(P9, 0) - S(1 - P9, 0) = -3 Or S(P9, 0) - S(1 - P9, 0) = -6) Then OF%(P9) = 9
    M1 = 0: M2 = 0: M3 = 0: M4 = 0
    If U5 = 1 And P9 = CT And X = 50 Or U5 = 2 And P9 = p And X = 50 Then 939
    For I1 = 0 To 13
        M6 = A(P9, I1, 3) - A(P9, I1, 7) - A(P9, I1, 11) - A(P9, I1, 12)
        If H < 2 And M6 <= 0 Or H = 2 And T > 120 And M6 <= 0 Then M1 = M1 + 1
        If A(P9, I1, 12) >= NF% Or AA$(P9, I1) = "XXX" Or B%(P9, I1, 18) <> 0 Then M2 = M2 + 1
        If I1 > 4 Then 937
        M6 = A(P9, B1(P9, I1), 3) - A(P9, B1(P9, I1), 7) - A(P9, B1(P9, I1), 11) - A(P9, B1(P9, I1), 12)
        If H = 1 And M6 / A(P9, B1(P9, I1), 3) < .5 Then M3 = M3 + 1
        If H = 1 And T >= 600 And M6 / A(P9, B1(P9, I1), 3) < .75 Then M3 = M3 + 1
        If H = 2 And T >= 600 And M6 / A(P9, B1(P9, I1), 3) < .25 Then M3 = M3 + 1
        If H = 2 And T >= 300 And T < 600 And M6 / A(P9, B1(P9, I1), 3) < .13 Then M3 = M3 + 1
        If H = 2 And T < 300 And M6 / A(P9, B1(P9, I1), 3) < T / 2400 Then M3 = M3 + 1
        If M6 <= 0 And H < 2 Or M6 <= 0 And H = 2 And T > 120 Then M3 = M3 + 1
        If A(P9, B1(P9, I1), 12) >= NF% Or A(P9, B1(P9, I1), 12) >= NF% - 2 And H = 1 Or H = 2 And T > 720 And A(P9, B1(P9, I1), 12) >= NF% - 1 Then M4 = M4 + 1
    937 Next
    If M3 >= 1 And 5 - M3 < 14 - M1 Or H = 2 And T < 120 And XS = 0 And G9%(P9) = 0 Or M4 >= 1 And M2 < 13 Or CTO% = 1 Then CTO% = 0: XM = 1: Call COMCOACH: GoTo 940
    939 If H = 2 And T <= 120 Then G9%(P9) = 1

940 End Sub


'------------------------------
'    FATIGUE Subroutine
'------------------------------
' ...explanation...
Sub FATIGUE ()
    'COMPUTER DEFENSE FOR FATIGUE
    FD% = 0: FQ% = 0
    For I = 0 To 4
        FD% = FD% + FA%(P9, B1(P9, I))
        If FA%(P9, B1(P9, I)) <= -4 Then FQ% = 1
    Next
    If FD% < -15 Or FQ% = 1 Then D2(P9) = 2: P7(P9) = 0: DT%(P9) = 2
End Sub


'------------------------------
'    AUTOSETUP Subroutine
'------------------------------
' ...explanation...
Sub AUTOSETUP ()
    X3% = NG%(3): U5 = NG%(8): NF% = NG%(5): C7 = NG%(7): N = NG%(2): displayPause = NG%(4) / 10: CM% = NG%(9)
    ST% = NG%(12)
    TB% = NG%(6): TF% = NG%(10): N7% = NG%(14)
    For P9 = 0 To 1: I1 = NG%(P9): YN$ = YN$(P9)
        Call GETATEAM(I1, YN$)
    Call TEAMSETUP: Next P9
    If TOU <> 1 Then 1305
    1302 If Right$(FF$, 1) = " " Then FF$ = Left$(FF$, Len(FF$) - 1): GoTo 1302
    1303 If Right$(YN$(0), 1) = " " Then YN$(0) = Left$(YN$(0), Len(YN$(0)) - 1): GoTo 1303
    1304 If Right$(YN$(1), 1) = " " Then YN$(1) = Left$(YN$(1), Len(YN$(1)) - 1): GoTo 1304

    Open p$(0) + FF$ + ".INF" For Input As #2
    Input #2, X$
    Input #2, X$: ' NUMBER OF REGIONS
    For I = 1 To 16
        Input #2, X$: ' NUMBER OF TEAMS IN REGION
        Input #2, X$: ' NAME OF REGION
    Next
    For I = 1 To 16
        For I1 = 1 To 16
            For I2 = 0 To 3
                Input #2, N$(I, I1, I2): ' SEED SLOT NAME
                Input #2, N%(I, I1, I2): ' REGION, SEED NUMBER, MODE PLAY/TM#
            Next
        Next
    Next

    Close 2

    TOU = 0
    For I = 1 To 16
        For I1 = 1 To 16
            'PRINT LEN(YN$(0)),LEN(YN$(1)),LEN( N$(I,I1,1) )
            If NG%(0) = N%(I, I1, 0) And YN$(0) = N$(I, I1, 1) And N%(I, I1, 1) = 0 Then TOU = 1: GoTo 1305
            If NG%(1) = N%(I, I1, 0) And YN$(1) = N$(I, I1, 1) And N%(I, I1, 1) = 0 Then TOU = 1: GoTo 1305
        Next
    Next

1305 End Sub


'------------------------------
'    LINEUPS Subroutine
'------------------------------
' ...explanation...
Sub LINEUPS ()
    Cls
    Color TR%(P9, 0), TR%(P9, 1): Print B$(P9);: Color 15, 0: Print " TEAM ROSTER    COACH: "; C$(P9);
    Print "  RECORD: ";: Print Using "##"; TR%(P9, 7);: Print "-";: Print Using "##"; TR%(P9, 8)
    Color 14: Print "# NAME         POS FG  FT 3FG %3A ORB DRB RB PA  S  B CX EF  PT  F  FC  PTS/G"
    Locate 3, 1
    For R1 = 0 To 13
        If B%(P9, R1, 18) <> 0 Or AA$(P9, R1) = "XXX" Or AA$(P9, R1) = "" Then 71
        Color 2
        If R1 <= 9 Then Print Using "# "; R1;
        If R1 > 9 Then Print X$(R1 - 10);
        Color 15
        Print Left$(AA$(P9, R1), 12);
        Locate , 16: Print PO$(P9, R1);
        Locate , 20
        L = 0: L = A(P9, R1, 3) - A(P9, R1, 7) - A(P9, R1, 11) - A(P9, R1, 12): If L <= 0 Then L = 0
        Print Using "## ###  ##  ##  ##  ## ## "; A(P9, R1, 0), A(P9, R1, 1), A(P9, R1, 20), A(P9, R1, 19), A(P9, R1, 21) * 2 / 10, A(P9, R1, 2) * 2 / 10, (A(P9, R1, 2) + A(P9, R1, 21)) / 5;
        Print Using "## ## ##"; A(P9, R1, 4), A(P9, R1, 5), A(P9, R1, 6);
        If L <= 0 And (H = 1 Or H = 2 And T > 120) Then Color 12
        Print Using "### "; L;
        Color 15
        If FA%(P9, R1) * -1 >= 5 And (H = 1 Or H = 2 And T > 120) Then Color 12
        Print Using "##"; FA%(P9, R1) * -1;
        Color 15
        Locate , 61: Print Using "###  "; A(P9, R1, 8) * 2 + A(P9, R1, 10) + W%(P9, R1, 0);
        If A(P9, R1, 12) >= 4 Then Color 12
        Print Using "# "; A(P9, R1, 12);
        Color 15
        Print Using "###   "; A(P9, R1, 17);

        If RS(P9, R1, 0) = 0 Then
            Print "--.-"
        Else
            Print Using "##.#"; (RS(P9, R1, 3) * 2 + RS(P9, R1, 5) + RS(P9, R1, 13)) / RS(P9, R1, 0)
        End If

    71 Next R1
    Locate 18, 1: Color 9, 0: Print String$(27, 205); Chr$(203); String$(51, 205)
    For R1 = 1 To 7: Locate R1 + 18, 28: Print Chr$(186);: Next
    Locate 19, 1: Color 14, 0: Print "STARTING LINEUP"
    If C1 = 1 Then
        For M = 0 To 4
            Locate M + 20, 1
            Color 2, 0
            Print Using "# "; M;
            Color 15, 0
            Print "ENTER "; PS$(M); " # ";
            Z = B1(P9, M)
            GoSub 90
            Print AA$(P9, Z);: Locate Z + 3 - CV%, 3
            Color TR%(P9, 0), TR%(P9, 1)
            Print Left$(AA$(P9, Z), 12)
            Color 15, 0
        Next
        GoTo 73
    End If

    For I = 0 To 4
        Locate I + 20, 1
        Color 2, 0: Print Using "# "; I;: Color 15: Print "ENTER "; PS$(I); " # ";
        72 I$ = GetKeyPress$
        Call ALPHANUM(I$, J)
        If J < 0 Or J > 13 Or A(P9, J, 12) >= NF% Or AA$(P9, J) = "XXX" Or B%(P9, J, 18) <> 0 Then 72

        For I2 = 0 To 4
            If B1(P9, I2) = J Then 72
        Next I2

        B1(P9, I) = J
        Z = J
        GoSub 90
        Print AA$(P9, J);
        Locate J + 3 - CV%, 3: Color TR%(P9, 0), TR%(P9, 1)
        Print Left$(AA$(P9, J), 12)
        Color 15, 0
    Next

    73 Locate 25, 1
    Color 12, 0
    Print "# TO CHANGE (0-4) 6=NONE";
    Locate 1, 1
    I$ = GetKeyPress$
    I1 = Val(I$): If I1 = 6 Then 100
    If I1 < 0 Or I1 > 4 Then 73
    If X = 50 And I1 = B And P9 = p Then Locate 20, 29: Color 14, 0: Print AA$(P9, B1(P9, I1)); " IS AT THE FT LINE": GoTo 73
    Locate 25, 1: Print "                         ";
    J = B1(P9, I1): Z = J: TM%(P9, J) = T: GoSub 90: Locate J + 3 - CV%, 3: Color 15, 0: Print Left$(AA$(P9, J), 12)
    Locate I1 + 20, 1: Print "                          ";
    B1(P9, I1) = -1
    Locate , 1: Color 2: Print Using "# "; I1;: Color 15: Print "ENTER "; PS$(I1); " # ";

    75 I$ = GetKeyPress$
    Call ALPHANUM(I$, J)
    If J < 0 Or J > 13 Or A(P9, J, 12) >= NF% Or AA$(P9, J) = "XXX" Or B%(P9, J, 18) <> 0 Then 75
    For I2 = 0 To 4
        If B1(P9, I2) = J Then 75
    Next I2

    B1(P9, I1) = J: Z = J: GoSub FATIGUE: GoSub 90: Print AA$(P9, J);: Locate J + 3 - CV%, 3: Color TR%(P9, 0), TR%(P9, 1): Print Left$(AA$(P9, J), 12): Color 15, 0
    GoTo 73

    FATIGUE:
    If TM%(P9, J) - T > 60 And TM%(P9, J) < 120 Then FA%(P9, J) = FA%(P9, J) + 1
    If TM%(P9, J) - T >= 120 Then FA%(P9, J) = FA%(P9, J) + 4
    If FA%(P9, J) > 0 Then FA%(P9, J) = 0
    Return

    90 CV% = 0
    For IP% = 0 To Z
        If B%(P9, IP%, 18) <> 0 Or AA$(P9, IP%) = "" Or AA$(P9, IP%) = "XXX" Then CV% = CV% + 1
    Next
    Return

    100 CV% = 0
    For I1 = 0 To 13
        If B%(P9, I1, 18) <> 0 Or AA$(P9, I1) = "XXX" Or A(P9, I1, 12) >= NF% Then CV% = CV% + 1
    Next
    If 14 - CV% < 5 Then GoTo MENUS
    For I1 = 0 To 4
        If A(P9, B1(P9, I1), 12) >= NF% Then Locate 20, 29: Color 14, 0: Print AA$(P9, B1(P9, I1)); " HAS FOULED OUT": GoTo 73
    Next
    MENUS:
    Call OFFENSE
End Sub


'------------------------------
'    OFFENSE Subroutine
'------------------------------
' ...explanation...
Sub OFFENSE ()
    'This label was not being referenced anywhere else
    'All it was doing for a modern compiler was confusing
    'the call references with the Subroutine name.
    '    OFFENSE:
    109 Color TR%(P9, 0), TR%(P9, 1): Cls
    Print ; Tab(33); "OFFENSIVE STYLE": Print
    For I = 0 To 9
        Locate , 5: Print Using "# "; I;: Print O$(I)
    Next
    Print
    Locate 14, 5: Print "SELECT OFFENSIVE STYLE FOR "; B$(P9)
    106 I$ = GetKeyPress$
    I1 = Val(I$): If I1 < 0 Or I1 > 9 Then 106

    If (I1 = 3 Or I1 = 4 Or I1 = 7 Or I1 = 9) And (H < 2 Or H >= 2 And T > 180) Then Beep: Locate 18, 1: Print "CAN ONLY USE LAST 3 MIN OF 2ND HALF OR OT": I$ = GetKeyPress$: GoTo 109

    If I1 = 8 And D2(1 - P9) <> 2 Then Beep: Locate 18, 1: Print "DEFENSE NOT IN PASSIVE ZONE": I$ = GetKeyPress$: GoTo 109

    If I1 = 4 And S(P9, 0) - S(1 - P9, 0) > -11 Then Beep: Locate 18, 1: Print "ILLEGAL SELECTION": I$ = GetKeyPress$: GoTo 109

    If I1 = 9 And X3% = 0 Then Beep: Locate 18, 1: Print "NOT USING 3 POINT SHOT": I$ = GetKeyPress$: GoTo 109

    If I1 = 5 And C7 = 0 Then Beep: Locate 18, 1: Print "NOT USING SHOT CLOCK": I$ = GetKeyPress$: GoTo 109

    If I1 = 7 And S(P9, 0) - S(1 - P9, 0) <= 0 Then Beep: Locate 18, 1: Print "ILLEGAL SELECTION": I$ = GetKeyPress$: GoTo 109
    OF%(P9) = I1
    CZ%(P9) = 0
    If H < 2 Or H >= 2 And T > 120 Then GoTo DEFENSE
    Print "CRASH OFFENSIVE BOARDS (LAST 2 MIN) (YN)";
    I$ = GetKeyPress$
    If UCase$(I$) = "Y" Then CZ%(P9) = 1

    DEFENSE:
    Call DEFENSEMENU
End Sub


'------------------------------
'    FOULSAFE Subroutine
'------------------------------
' ...explanation...
Sub FOULSAFE ()
    1200 Cls: Color 15: A = 0: S2 = 1: S3 = 1: SW = 0
    For ZZ = 0 To 4
        If A(P9, B1(P9, ZZ), 22) = 50 Then A = A + 1: If SW = 0 Then Print: Print "THE FOLLOWING PLAYER(S) ARE PLAYING SAFE BUT CAN NOW PLAY NORMAL IF YOU WISH": SW = 1
        If A(P9, B1(P9, ZZ), 22) = 50 And SW = 1 And S2 < 6 Then Locate S2 + 3, 1: Print S2; "-"; AA$(P9, B1(P9, ZZ)): P5(A) = S2: P4(A) = ZZ: S2 = S2 + 1: S3 = S3 + 1
    Next ZZ
    If SW = 0 Then 1214
    A = 1: Print: Print "Z - CONTINUE"

    1206 I$ = GetKeyPress$
    If UCase$(I$) = "Z" And MJ = 0 Then 1214
    If UCase$(I$) = "Z" And MJ = 25 Then 1230
    P6 = Val(I$): If P6 < 1 Or P6 > (S2 + 1) Then 1206

    A = P6: If P6 = P5(P6) Then A(P9, B1(P9, P4(A)), 17) = A(P9, B1(P9, P4(A)), 17) * 3: A(P9, B1(P9, P4(A)), 22) = 0: Locate P5(A) + 3, 20: Print AA$(P9, B1(P9, P4(A))); " WILL NOW PLAY NORMAL DEFENSE!": Call STEALADJ: D1(P9) = D1(P9) - 1: A = A + 1: MJ = 25: GoTo 1206

    1214 A = 0: S2 = 1
    For ZZ = 0 To 4
        If H <= 1 And A(P9, B1(P9, ZZ), 12) >= NF% - 3 And A(P9, B1(P9, ZZ), 22) = 0 Or A(P9, B1(P9, ZZ), 12) = NF% - 1 And A(P9, B1(P9, ZZ), 22) = 0 Then A = A + 1: If S2 = 1 Then Cls: Print: Print "THE FOLLOWING PLAYER(S) CAN ME MADE TO PLAY SAFE IF YOU WISH": Print
        If A = 0 And ZZ > 3 Then 1230
        If S2 = A And S2 < 6 Then Print S2; "- "; AA$(P9, B1(P9, ZZ)): P5(A) = S2: P4(A) = ZZ: S2 = S2 + 1: S3 = S3 + 1
    Next ZZ
    A = 1: Print: Print "Z - CONTINUE"
    1220 I$ = GetKeyPress$
    If UCase$(I$) = "Z" Then 1230
    P6 = Val(I$): If P6 < 1 Or P6 > (S2 + 1) Then 1220

    A = P6: If P6 = P5(P6) Then A(P9, B1(P9, P4(A)), 17) = Int((A(P9, B1(P9, P4(A)), 17) + 1) / 3): A(P9, B1(P9, P4(A)), 22) = 50: Locate P5(A) + 3, 20: Print AA$(P9, B1(P9, P4(A))); " WILL NOW PLAY SAFE!": Call STEALADJ: D1(P9) = D1(P9) + 1: A = A + 1: S3 = S3 + 1: GoTo 1220
    1230 MJ = 0
    FO = 99: If H = 1 And T = 1200 Then FO = 0
End Sub


'------------------------------
'    FOULHARD Subroutine
'------------------------------
' ...explanation...
Sub FOULHARD ()
    For I = 0 To 1
        For I1 = 0 To 13
            If A(I, I1, 22) = 50 Then A(I, I1, 22) = 0: A(I, I1, 17) = A(I, I1, 17) * 3: D1(I) = D1(I) - 1: MJ = 50
        Next I1
        If MJ = 50 Then
            MJ = 0
            F5%(I, 0) = 0
            For I1 = 0 To 4: F5%(I, 0) = F5%(I, 0) + A(I, B1(I, I1), 17): Next I1
        End If
    Next I
End Sub


'------------------------------
'    STEALADJ Subroutine
'------------------------------
' ...explanation...
Sub STEALADJ ()
    For I = 0 To 1
        F5%(I, 0) = 0
        F5%(I, 1) = 0
        For I1 = 0 To 4
            F5%(I, 0) = F5%(I, 0) + A(I, B1(I, I1), 17)
            F5%(I, 1) = F5%(I, 1) + A(I, B1(I, I1), 18)
        Next I1
    Next I
End Sub


'------------------------------
'    ALPHANUM Subroutine
'------------------------------
' ...explanation...
Sub ALPHANUM (I$, J)
    J = Val(I$)
    If UCase$(I$) = "A" Then J = 10
    If UCase$(I$) = "B" Then J = 11
    If UCase$(I$) = "C" Then J = 12
    If UCase$(I$) = "D" Then J = 13
    If UCase$(I$) = "E" Then J = 14
End Sub


'------------------------------
'    BOXSCREEN Subroutine
'------------------------------
' Prints BoxScore to Screen
Sub BOXSCREEN ()
    For I = 0 To 1: Color TR%(I, 0), TR%(I, 1)
        Print B$(I): Color 14, 0
        Print "NAME"; Tab(14); " MIN   FG- A 3FG- A  FT- A ORB  RB  PT  F   A   B   S  TO"

        For I1 = 0 To 13
            If AA$(I, I1) = "XXX" Or ZZ(I, I1) = 0 Then 8529

            Color 15

            Print AA$(I, I1); Tab(14);

            If ZZ(I, I1) <> 0 Then Print Using "##"; Int(ZZ(I, I1) / 60);: Print ":";: If ZZ(I, I1) - Int(ZZ(I, I1) / 60) * 60 > 9 Then Print Using "##"; ZZ(I, I1) - Int(ZZ(I, I1) / 60) * 60; Else Print "0";: Print Using "#"; ZZ(I, I1) - Int(ZZ(I, I1) / 60) * 60;
            Locate , 21: Print Using "##"; A(I, I1, 8);: Print "-";: Print Using "##"; A(I, I1, 7);: Print Using "  ##"; W%(I, I1, 0);: Print "-";: Print Using "##"; W%(I, I1, 1);: Print Using "  ##"; A(I, I1, 10);: Print "-";: Print Using "##"; A(I, I1, 9);
            Print Using "  ##  ## ###  #  ##  ##  ##  ##  ##  ##"; A(I, I1, 24), A(I, I1, 11), A(I, I1, 8) * 2 + A(I, I1, 10) + W%(I, I1, 0), A(I, I1, 12), A(I, I1, 14), A(I, I1, 15), A(I, I1, 13), A(I, I1, 23);

            If ZZ(I, I1) = 0 Then Locate , 15: Print "DNP" Else Print

            V9(0) = V9(0) + A(I, I1, 8)
            V9(1) = V9(1) + A(I, I1, 7)
            V9(2) = V9(2) + A(I, I1, 10)
            V9(3) = V9(3) + A(I, I1, 9)
            V9(4) = V9(4) + A(I, I1, 11)
            V9(5) = V9(5) + (A(I, I1, 8) * 2) + A(I, I1, 10) + W%(I, I1, 0)
            V9(13) = V9(13) + W%(I, I1, 0)
            V9(14) = V9(14) + W%(I, I1, 1)
            V9(6) = V9(6) + A(I, I1, 12)
            V9(7) = V9(7) + A(I, I1, 14)
            V9(8) = V9(8) + A(I, I1, 15)
            V9(9) = V9(9) + A(I, I1, 13)
            V9(11) = V9(11) + A(I, I1, 24)
            V9(12) = V9(12) + A(I, I1, 23)
            V9(10) = V9(10) + ZZ(I, I1)
        8529 Next I1

        If V9(3) = 0 Then V9(3) = 1
        Print
        Color 14
        Print "TOTALS";
        Color 15
        Print ; Tab(14); Int(V9(10) / 60);
        Locate , 21: Print Using "##"; V9(0);: Print "-";: Print Using "###"; V9(1);
        Locate , 28: Print Using "##"; V9(13);: Print "-";: Print Using "##"; V9(14);
        Locate , 35: Print Using "##"; V9(2);: Print "-";: Print Using "##"; V9(3);
        Locate , 42: Print Using "##  ## ### ##  ##  ##  ##  ##"; V9(11), V9(4), V9(5), V9(6), V9(7), V9(8), V9(9), V9(12)

        Print ; Tab(22);
        If V9(1) <> 0 Then Print Int(((V9(0) / V9(1)) * 100) + .5); "%";

        Print ; Tab(35);
        If V9(3) <> 0 Then Print Int(((V9(2) / V9(3)) * 100) + .5); "%"

        Print
        Color 14: Print "TOTAL TURNOVERS -";: Color 15: Print T0(I)
        Color 14: Print "TOTAL REBOUNDS  -";: Color 15: Print R3(I)

        For I2 = 0 To 14: V9(I2) = 0: Next I2

        If AP = 1 And U5 = 2 Then
            '
        Else
            I$ = GetKeyPress
        End If

        Cls

    Next I

End Sub


'------------------------------
'    SHOTPERCENT Subroutine
'------------------------------
' ...explanation...
Sub SHOTPERCENT ()
    FX% = A(p, B1(p, B), 0) + TP%(D) + TP%(p)
    TP%(D) = 0: TP%(p) = 0
    If FA%(p, B1(p, B)) <= -5 Then FX% = FX% - 10
    F = FX% + D1(D) + D8(0, D2(D)): J8% = 0
    If N = p Then F = F + HF%(0, TR%(p, 6))
    If N <> p And N <> 2 Then F = F + HF%(1, TR%(p, 6))
    F3S% = Int(Rnd(1) * 100) + 1
    TPA% = 0
    If DT%(D) = 2 Or DT%(D) = 7 Or DT%(D) = 12 Then TPA% = 3
    If DT%(D) = 1 Or DT%(D) = 6 Or DT%(D) = 9 Or DT%(D) = 11 Then TPA% = -5
    If OF%(p) = 9 And (F3S% > A(p, B1(p, B), 19) + F5%(D, 3) + TPA% Or A(p, B1(p, B), 19) = 0) Then F3S% = Int(Rnd(1) * 5) + 11: J8% = 1: GoTo 925
    If OF%(p) = 9 Then 924
    If F3S% > A(p, B1(p, B), 19) + F5%(D, 3) + TPA% Or A(p, B1(p, B), 19) = 0 Then F3S% = Int(Rnd(1) * 5) + 3: GoTo 925
    924 FX% = A(p, B1(p, B), 20)
    If FA%(p, B1(p, B)) <= -5 Then FX% = Int(FX% * .67 + .5)
    F3S% = FX% + F5%(D, 2): J8% = 1
    If A(p, B1(p, B), 3) - A(p, B1(p, B), 7) - A(p, B1(p, B), 11) - A(p, B1(p, B), 12) <= 0 And (H = 1 Or H = 2 And T > 120) Then F3S% = Int(Rnd(1) * 5) + 3: J8% = 0
    925 '
End Sub


'------------------------------
'    THREEFT Subroutine
'------------------------------
' ...explanation...
Sub THREEFT ()
    A(p, B1(p, B), 10) = A(p, B1(p, B), 10) + 1
    RN = Int(Rnd(1) * 100) + 1
    If F3 = 1 And RN <= 50 Then U$ = "FREE THROW IS GOOD"
    If F3 = 1 And RN > 50 Then U$ = "FREE THROW IS GOOD"
    RN = Int(Rnd(1) * 100) + 1
    If F3 = 2 And RN <= 50 Then U$ = "SECOND FREE THROW IS GOOD"
    If F3 = 2 And RN > 50 Then U$ = "HE HITS THE SECOND ONE"
    RN = Int(Rnd(1) * 100) + 1
    If F3 = 3 And RN <= 50 Then U$ = "THIRD FREE THROW IS GOOD"
    If F3 = 3 And RN > 50 Then U$ = "HE HITS THE LAST ONE"
    Call PBP: S(p, H) = S(p, H) + 1: S(p, 0) = S(p, 0) + 1
    Call MAINSCREEN
End Sub


'------------------------------
'    MADEFT Subroutine
'------------------------------
' ...explanation...
Sub MADEFT ()
    A(p, B1(p, B), 10) = A(p, B1(p, B), 10) + 1
    If displayPause <> 0 Then Locate F3 + 9, 1: Call DELAY: Print F3; " GOOD"
    S(p, H) = S(p, H) + 1: S(p, 0) = S(p, 0) + 1
    Call MAINSCREEN
    Call DELAY
End Sub


'------------------------------
'    MISSFT Subroutine
'------------------------------
' ...explanation...
Sub MISSFT ()
    If displayPause = 0 Then 3699
    Call DELAY
    Locate F3 + 9, 1
    Print F3; " MISSED"
    Call DELAY
    3699 '
End Sub


'------------------------------
'    BADFT Subroutine
'------------------------------
' ...explanation...
Sub BADFT ()
    If F3 = 1 Then U$ = "FIRST FREE THROW IS MISSED"
    If F3 = 2 Then U$ = "SECOND FREE THROW IS MISSED"
    If F3 = 3 Then U$ = "THIRD FREE THROW IS MISSED"
    Call PBP
End Sub


'------------------------------
'    PRELOAD Subroutine
'------------------------------
' ...explanation...
Sub PRELOAD ()
    1100 NT = 0
    Open p$(0) + "BASK." + YN$ For Random As #1 Len = 655
    Y2 = LOF(1)
    Field #1, 15 As Q$(0)
    For I1 = 1 To Y2 / 655
        Get #1, I1
        For I2 = 1 To 15
            If Asc(Mid$(Q$(0), I2, 1)) < 32 Or Asc(Mid$(Q$(0), I2, 1)) > 123 Or UCase$(Left$(Q$(0), 3)) = "NOT" Or UCase$(Left$(Q$(0), 3)) = "UN" Then 1105
        Next I2
        NT = NT + 1
        V$(NT) = Q$(0): V%(NT) = I1
    1105 Next I1
    Close 1

    Call TEAMSELECT
End Sub


'------------------------------
'    TEAMSELECT Subroutine
'------------------------------
' ...explanation...
Sub TEAMSELECT ()
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
    _MouseShow "Default"
    20 MAMenu V$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
    _MouseHide
    If EXITCODE% = 27 Then BO% = 1: GoTo 21
    If EXITCODE% = -71 Then Start% = 1: EXITCODE% = 3: SELECTION% = 1: GoTo 20
    If EXITCODE% = -79 Then Start% = NT: EXITCODE% = 3: SELECTION% = NT: GoTo 20
    If EXITCODE% = -73 Then Start% = Start% - 80: If Start% < 1 Then Start% = 1
    If EXITCODE% = -81 Then Start% = Start% + 80: If Start% > NT Then Start% = NT
    If EXITCODE% = -81 Or EXITCODE% = -73 Then EXITCODE% = 3: SELECTION% = Start%: GoTo 20
    I1 = V%(SELECTION%)
    Call GETATEAM(I1, YN$)

21 End Sub


'------------------------------
'    SELECT ID Subroutine
'------------------------------
' ...explanation...
Sub SELECTID ()
    56 Color 15, 0
    Cls
    SN = 1
    Locate 6, 8
    Color 15, 4

    'Print box top
    Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

    'Print box sides
    JJ = 9
    For II = 1 To JJ
        Locate 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
    Next
    Locate 7 + JJ, 8

    'Print box bottom
    Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

    Locate 7, 32
    Locate , 12: Print "ENTER DISK ID: ": Print
    Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
    Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
    Print
    Locate , 12: Print "MAKE SURE NUM LOCK KEY IS OFF TO USE ARROW KEYS"
    Locate , 12: Print "AND PG UP/PG DN KEYS"
    Print
    Locate , 12: Print "(JUST HIT ENTER TO USE PREVIOUS ID, IF SAME)"
    Locate 7, 27: Input NY$
    Color 15, 0
    61 If NY$ = "" Then Call TEAMSELECT: GoTo 60
    YN$ = NY$
    For I = 1 To 500: V$(I) = "": Next
    Call PRELOAD
    60 If BO% = 1 Then BO% = 0: GoTo 56
    58 Locate 2, 33: Color TR%(P9, 0), TR%(P9, 1): Print B$(P9): Color 15, 4: Locate , 33: Print "ANY CHANGE (YN)";: I$ = GetKeyPress$
    If UCase$(I$) = "Y" Then 56
    Color 15, 0
    Call TEAMSETUP
    Call DISABLEPLAYER
End Sub


'------------------------------
'    PRDFAT Subroutine
'------------------------------
' ...explanation...
Sub PRDFAT ()
    'FATIGUE DUE TO PRESSURE DEFENSE ON THE DEFENSE
    For I = 0 To 4
        R0 = Int(Rnd(1) * 100) + 1
        If R0 <= X0% Then FA%(D, B1(D, I)) = FA%(D, B1(D, I)) + X1%
        If R0 > X0% Then FA%(D, B1(D, I)) = FA%(D, B1(D, I)) + X1% + 1
        If FA%(D, B1(D, I)) > 0 Then FA%(D, B1(D, I)) = 0
        If FA%(D, B1(D, I)) < -5 Then FA%(D, B1(D, I)) = -5
    Next
End Sub


'------------------------------
'    PRPFAT Subroutine
'------------------------------
' ...explanation...
Sub PRPFAT ()
    'FATIGUE AGAINST OFFENSIVE PLAYERS SUBJECT TO PRESS
    For I = 0 To 4
        R0 = Int(Rnd(1) * 100) + 1
        If R0 <= X0% Then FA%(p, B1(p, I)) = FA%(p, B1(p, I)) + X1%
        If R0 > X0% Then FA%(p, B1(p, I)) = FA%(p, B1(p, I)) + X1% + 1
        If FA%(p, B1(p, I)) > 0 Then FA%(p, B1(p, I)) = 0
        If FA%(p, B1(p, I)) < -5 Then FA%(p, B1(p, I)) = -5
    Next
End Sub


'------------------------------
'    WHOGESTBALL Subroutine
'------------------------------
' ...explanation...
Sub WHOGETSBALL ()
    1040 cnt% = 0
    1041 cnt% = cnt% + 1
    If cnt% = 4 Then 1047
    M9 = 0: B = Int(Rnd(1) * 5): X = Int(Rnd(1) * 100) + 1
    If X <= 25 Then 1046
    For I = 0 To 4
        M8 = Int(C(p, B1(p, I), 1) / C(p, B1(p, I), 0) + .5) - A(p, B1(p, I), 7)
        If M8 <= 0 Then M8 = 0
        'IF M8<=0 AND A(P,B1(P,I),3)-A(P,B1(P,I),7)-A(P,B1(P,I),11)-A(P,B1(P,I),12)>0 THEN M8=A(P,B1(P,I),3)-A(P,B1(P,I),7)-A(P,B1(P,I),11)-A(P,B1(P,I),12)
        If M8 >= M9 Then B = I: M9 = M8
    Next I
    1046 M8 = A(p, B1(p, B), 3) - A(p, B1(p, B), 7) - A(p, B1(p, B), 11) - A(p, B1(p, B), 12)
    If A(p, B1(p, B), 4) >= 5 And M8 / A(p, B1(p, B), 3) <= 0.4 And H = 1 Then 1041
    If A(p, B1(p, B), 4) >= 5 And M8 / A(p, B1(p, B), 3) <= 0.2 And H = 2 And T > 120 Then 1041
    If A(p, B1(p, B), 3) < 10 And A(p, B1(p, B), 4) >= 5 And M8 / A(p, B1(p, B), 3) <= 0.75 And H = 1 Then 1041
    If A(p, B1(p, B), 3) < 10 And A(p, B1(p, B), 4) >= 5 And M8 / A(p, B1(p, B), 3) <= 0.5 And H = 2 And T > 120 Then 1041
    GoTo 1041
    1047 '
End Sub


'------------------------------
'  BALLDISTRIBUTE Subroutine
'------------------------------
' ...explanation...
Sub BALLDISTRIBUTE ()
    R0 = Int(Rnd(1) * 100) + 1
    If U5 = 0 And CM% = 1 Or U5 = 1 And D = CT And CM% = 1 Or U5 = 1 And p = CT Or U5 = 2 Then Call WHOGETSBALL: GoTo 427
    If D2(D) = 2 Or D2(D) = 3 Then 415
    If S9 <= 90 And R0 <= 15 Then 424
    If S9 <= 100 And R0 <= 20 Then 424
    If S9 <= 110 And R0 <= 25 Then 424
    If S9 <= 120 And R0 <= 30 Then 424
    If S9 <= 130 And R0 <= 38 Then 424
    If S9 <= 140 And R0 <= 53 Then 424
    If S9 <= 150 And R0 <= 60 Then 424
    If S9 > 150 And R0 <= 67 Then 424
    GoTo 423
    415 If S9 <= 90 And R0 <= 8 Then 424
    If S9 <= 100 And R0 <= 10 Then 424
    If S9 <= 110 And R0 <= 13 Then 424
    If S9 <= 120 And R0 <= 15 Then 424
    If S9 <= 130 And R0 <= 18 Then 424
    If S9 <= 140 And R0 <= 53 Then 424
    If S9 <= 150 And R0 <= 60 Then 424
    If S9 > 150 And R0 <= 67 Then 424
    423 B = Int(Rnd(1) * 5): GoTo 427
    424 If displayPause <> 0 And PB% = 0 Then U$ = "PASS TO CHOICE (0-4)": Call PBP

    If displayPause > 0 And PB% = 1 Then
        'IF AP=1 AND U5=2 OR displayPause=0 THEN 425
        For I = 8 To 15
            Locate I, 1
            Call BLANKLINE
        Next I
        Locate 8, 1
        Print "PASS TO CHOICE (0-4)"
        Call DELAY
    End If

    425 I$ = GetKeyPress$
    If UCase$(I$) = "T" Then Call TICKER: Call SCOREBOARD: GoTo 425

    B = Val(I$): If B < 0 Or B > 4 Then 424

427 End Sub


'------------------------------
'    PLAYINGTIME Subroutine
'------------------------------
' ...explanation...
Sub PLAYINGTIME ()
    '*** INCREMENT MINUTES ***
    For I3 = 0 To 1
        For I4 = 0 To 4
            ZZ(I3, B1(I3, I4)) = ZZ(I3, B1(I3, I4)) + TE
        Next
    Next
End Sub


'------------------------------
'    CLOCKOVER Subroutine
'------------------------------
' ...explanation...
Sub CLOCKOVER ()
    If T <= 0 Then TE = T: Call PLAYINGTIME: T = 0
End Sub


'------------------------------
'    BLANKLINE Subroutine
'------------------------------
' ...explanation...
Sub BLANKLINE ()
    Print "                                      "
End Sub


'------------------------------
'    CLEARPBPBOX Subroutine
'------------------------------
' ...explanation...
Sub CLEARPBPBOX ()
    'IF AP=1 AND U5=2 OR displayPause=0 THEN 4096
    If displayPause = 0 Then 4096
    For I = 8 To 15: Locate I, 1: Call BLANKLINE: Next I

    4096 Locate 10
End Sub


'------------------------------
'  DEADBALLOPTIONS Subroutine
'------------------------------
' ...explanation...
Sub DEADBALLOPTIONS ()
    220 PB$ = ""
    Call CLEARPBP
    If U5 = 2 Then P9 = 0: Call COMPUTERSUB: Call FATIGUE: P9 = 1: Call COMPUTERSUB: Call FATIGUE

    If U5 = 1 Then P9 = CT: Call COMPUTERSUB: Call FATIGUE

    FO = 0
    XM = 0
    Call CLEARPBPBOX
    If U5 = 2 Then S2 = 0: P9 = p: C1 = 2: Call CLOCKOVER: Call STEALADJ: GoTo 222

    Locate 9, 1: Color 14, 0
    Print "<ENTER>-CONTINUE"
    Print "1-SUB "; B$(p)
    Print "2-SUB "; B$(D)
    Print "3-CHANGE STRATEGY "; B$(p)
    Print "4-CHANGE STRATEGY "; B$(D)
    Print "5-TIME OUT "; B$(p)
    Print "6-TIME OUT "; B$(D)
    221 I$ = GetKeyPress$

    P3 = Val(I$)
    If P3 < 1 Or P3 > 6 Or U5 = 1 And (P3 = 1 Or P3 = 3 Or P3 = 5 And T1(p) <= 0) And p = CT Or U5 = 1 And (P3 = 2 Or P3 = 4 Or P3 = 6 And T1(D) <= 0) And D = CT Then S2 = 0: P9 = p: C1 = 2: Call CLOCKOVER: Call STEALADJ: GoTo 227
    If P3 = 5 And T1(p) <= 0 Or P3 = 6 And T1(D) <= 0 Then 221

    If P3 = 1 Then S2 = p
    If P3 = 2 Then S2 = D

    If P3 = 3 Then P9 = p: Call OFFENSE: Call SCOREBOARD: GoTo 220

    If P3 = 4 Then P9 = D: Call OFFENSE: Call SCOREBOARD: GoTo 220
    If P3 = 5 Then T1(p) = T1(p) - 1: Call RESETFATIGUE: Call SCOREBOARD: GoTo 220

    If P3 = 6 Then T1(D) = T1(D) - 1: Call RESETFATIGUE: Call SCOREBOARD: GoTo 220

    C1 = 1: P9 = S2: Call LINEUPS: Call FOULSAFE: P9 = p: LC% = 1: GoTo 220
    227 If LC% = 1 Then LC% = 0: Call CLOCKOVER: Call SCOREBOARD

    'IF AP=1 AND U5=2 OR displayPause=0 THEN 222
    If displayPause = 0 Then 222
    For I = 8 To 15: Locate I, 1: Call BLANKLINE: Next I

222 End Sub


'------------------------------
'    LASTFIVEMENU Subroutine
'------------------------------
' ...explanation...
Sub LASTFIVEMENU ()
    Locate 8, 8
    Color TR%(p, 0), TR%(p, 1)

    'Print box top
    Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

    'Print box sides
    JJ = 12
    For II = 1 To JJ
        Locate 8 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
    Next

    'Print box bottom
    Locate 9 + JJ, 8
    Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

    Locate 10, 15
    Print B$(p); " LAST 5 SEC STRATEGY:"
    Locate 12, 14
    Locate , 14: Print Using "TIME REMAINING #.#"; T
    Locate , 14: Print "                                     TIME REQD"
    Locate , 14: Print "0 - FULL COURT PASS (SHOOT A 2)      0.1 - 3.0"
    Locate , 14: Print "1 - FULL COURT PASS (SHOOT A 3)      0.1 - 3.0"
    Locate , 14: Print "2 - SHOOT FROM BACKCOURT             0.1 - 0.5"
    Locate , 14: Print "3 - GET TO HALF COURT, CALL TIME OUT 1.5 - 2.5"
    Locate , 14: Print "4 - WORK DOWN THE FLOOR FOR A 3      3.0 - 4.0"
    Locate , 14: Print "5 - WORK DOWN THE FLOOR FOR A 2      3.0 - 5.0"
End Sub


'------------------------------
'    RESETFATIGUE Subroutine
'------------------------------
' ...explanation...
Sub RESETFATIGUE ()
    295 For I = 0 To 1
        For I1 = 0 To 1
            PR%(I, I1) = 0
        Next
    Next
    For I = 0 To 1
        For I1 = 0 To 13
            FA%(I, I1) = FA%(I, I1) + 1
            If FA%(I, I1) > 0 Then FA%(I, I1) = 0
        Next
    Next
End Sub


'------------------------------
'    WINDEX Subroutine
'------------------------------
' ...explanation...
Sub WINDEX ()
    '    If displayPause > 0 Then
    For I = 8 To 15
        Locate I, 1
        Print Space$(38);
    Next I
    '   End If
    Locate 8, 1
End Sub


'------------------------------
'      BOXTOP Subroutine
'------------------------------
' ...explanation...
Sub BOXTOP ()
    If N <> 2 Then Color TR%(N, 0), TR%(N, 1): Print "@ "; B$(N): Print: Color 14, 0
    For I = 0 To 1: Print B$(I); Tab(17); Str$(S(I, 1)); Tab(20); Str$(S(I, 2));
        If S(0, 1) + S(0, 2) = S(1, 1) + S(1, 2) Then Print ; Tab(23); Str$(S(I, 3));
        If S(0, 1) + S(0, 2) <> S(1, 1) + S(1, 2) Then Print ; Tab(23); "- "; S(I, 0): GoTo 8513
        If S(0, 1) + S(0, 2) + S(0, 3) = S(1, 1) + S(1, 2) + S(1, 3) Then Print ; Tab(26); Str$(S(I, 4));
        If S(0, 1) + S(0, 2) + S(0, 3) <> S(1, 1) + S(1, 2) + S(1, 3) Then Print ; Tab(26); "- "; S(I, 0): GoTo 8513
        If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) = S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) Then Print ; Tab(29); Str$(S(I, 5));
        If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) <> S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) Then Print ; Tab(29); "- "; Str$(S(I, 0)): GoTo 8513
        If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) + S(0, 5) = S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) + S(1, 5) Then LPrint ; Tab(32); Str$(S(I, 6));
        If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) + S(0, 5) <> S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) + S(1, 5) Then Print ; Tab(32); "- "; Str$(S(I, 0)): GoTo 8513
        If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) + S(0, 5) + S(0, 6) = S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) + S(1, 5) + S(1, 6) Then Print ; Tab(35); Str$(S(I, 7)); Tab(38); Str$(S(I, 0)): GoTo 8513
        If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) + S(0, 5) + S(0, 6) <> S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) + S(1, 5) + S(1, 6) Then Print ; Tab(35); "- "; Str$(S(I, 0)): GoTo 8513
    8513 Next I
End Sub


'------------------------------
'    STOPAGE Subroutine
'------------------------------
' ...explanation...
Sub STOPPAGE ()
    810 Call CLEARPBP
    If U5 = 0 Then 811
    If U5 = 2 Then P9 = 0: Call COMPUTERSUB: Call FATIGUE: P9 = 1: Call COMPUTERSUB: Call FATIGUE

    If U5 = 1 Then P9 = CT: Call COMPUTERSUB: Call FATIGUE

    XM = 0
    811 If U5 = 2 Then S2 = 0: P9 = p: C1 = 2: Call CLOCKOVER: GoTo 820

    If PB% = 0 Then Call CLEARPBPBOX
    If PB% = 1 And displayPause <> 0 Then Locate 11, 1
    Color 14, 0
    Print "<ENTER>-CONTINUE"
    Print "1-TIME OUT "; B$(p)
    Print "2-TIME OUT "; B$(D)
    Print "3-CHANGE STRATEGY "; B$(p)
    Print "4-CHANGE STRATEGY "; B$(D)

    813 I$ = GetKeyPress$
    P3 = Val(I$): If P3 < 1 Or P3 > 4 Then S2 = 0: P9 = p: C1 = 2: Call CLOCKOVER: GoTo 818
    If P3 = 1 And T1(p) <= 0 Or P3 = 2 And T1(D) <= 0 Or (P3 = 1 Or P3 = 3) And p = CT And U5 = 1 Or (P3 = 2 Or P3 = 4) And D = CT And U5 = 1 Then 813

    If P3 = 3 Then P9 = p: Call OFFENSE: Call SCOREBOARD: GoTo 810

    If P3 = 4 Then P9 = D: Call OFFENSE: Call SCOREBOARD: GoTo 810
    For I = 0 To 1
        For I1 = 0 To 1
            PR%(I, I1) = 0
        Next
    Next

    If P3 = 1 Then T1(p) = T1(p) - 1: Color 14, 0: Call SCOREBOARD: Call DEADBALLOPTIONS

    If P3 = 2 Then T1(D) = T1(D) - 1: Color 14, 0: Call SCOREBOARD: Call DEADBALLOPTIONS

    818 Call CLEARPBPBOX

820 End Sub


'------------------------------
'    LASTSHOT Subroutine
'------------------------------
' ...explanation...
Sub LASTSHOT ()
    210 M5 = 0: If T <= 2 Or IN = 99 Then M5 = 1: GoTo 214
    If T > 2 And T <= 4 Then M5 = 2
    If T > 4 And T <= 16 Then M5 = Int(Rnd(1) * 2) + 3
    If T > 16 Then M5 = Int(Rnd(1) * 3) + 5
    214 TE = M5
    Call PLAYINGTIME: T = T - M5: IN = 0: If C7 >= 1 Then X8 = X8 - M5
    Call ZEROCHECK
End Sub
    

'------------------------------
'    ZEROCHECK Subroutine
'------------------------------
' ...explanation...
Sub ZEROCHECK ()
    215 If T <= .001 Then TE = T: Call PLAYINGTIME: T = 0
    If X8 <= 0 Then X8 = X8 - X8: T = T - X8: TE = X8: Call PLAYINGTIME
    Call MAINSCREEN
End Sub


'------------------------------
'    SELECTPBP Subroutine
'------------------------------
' ...explanation...
Sub SELECTPBP ()
    If PB% = 0 Then Call PBP
    If PB% = 1 And displayPause <> 0 Then Call WINDEX: Print U$: Call DELAY
End Sub


'------------------------------
'    PRESSGUARD Subroutine
'------------------------------
' ...explanation...
Sub PRESSGUARD ()
    260 Call WINDEX
    If displayPause = 0 Then 264
    If P7(D) > 0 Then U$ = B$(D) + " IS PRESSING": Call SELECTPBP
    264 If P7(D) = 0 Or P7(D) > 2 Then 263
    If U5 = 2 Or U5 = 1 And p = CT Then
        ' COMPUTER GUARD FOR PRESS
        BU% = 1
        If A(p, B1(p, 0), 4) >= A(p, B1(p, 1), 4) And FA%(p, B1(p, 0)) <= 3 Then BU% = 0
        GoTo 263
    End If
    TS% = p

    JJ = 8
    Call FRAME

    Print B$(p); " SELECT GUARD TO HANDLE BALL:"
    Locate 12, 14
    Locate , 14: Print "#   NAME"; Tab(35); "PA"; Tab(38); "FA"
    Locate , 14: Print "0 - "; AA$(p, B1(p, 0)); Tab(35); A(p, B1(p, 0), 4); Tab(38); Abs(FA%(p, B1(p, 0)))
    Locate , 14: Print "1 - "; AA$(p, B1(p, 1)); Tab(35); A(p, B1(p, 1), 4); Tab(38); Abs(FA%(p, B1(p, 1)))

    262 I$ = GetKeyPress$
    I1 = Val(I$): If I1 < 0 Or I1 > 1 Then 262

    BU% = I1
    Color 15, 0
    Call SCOREBOARD

263 End Sub


'------------------------------
'    SAVESTATS Subroutine
'------------------------------
' ...explanation...
Sub SAVESTATS
    6000 If _FileExists("GAME82") Then
        Open "GAME82" For Input As #2
        Input #2, FF$
        For I = 0 To 2: Input #2, AP%(I): Next: Close 2
        'AP(0) = Start Game
        'AP(1) = End Game
        'AP(2) = games played???

        If AP%(2) > 31 Then 6011
        For X = 0 To 1: SX$(AP%(2) + 1, X) = B$(X): Next

        Open "TICKER" For Output As #2
        For I = 1 To 32
            For I1 = 0 To 1
                For I2 = 1 To 14
                    Print #2, SX%(I, I1, I2):
                Next
            Next
        Next
        For I = 1 To 32
            For I1 = 0 To 1
                Print #2, SX$(I, I1)
            Next
        Next
        For I = 1 To 32: Print #2, ST%(I): Next
        Close 2

        6011 Open p$(0) + FF$ For Random As #1 Len = 48
        For X = 0 To 18
            Field #1, X * 2 As X$, 2 As Q$(X + 1), 48 - 2 - 2 * X As X$
        Next
        Field #1, 38 As X$, 2 As Q$(22), 2 As Q$(23), 3 As Q$(20), 3 As Q$(21)
        NG%(13) = 1
        For X = 0 To 18: LSet Q$(X + 1) = MKI$(NG%(X)): Next
        For X = 0 To 1: LSet Q$(20 + X) = YN$(X): Next
        LSet Q$(22) = MKI$(S(0, 0)): LSet Q$(23) = MKI$(S(1, 0))
        Put #1, AP%(0) + AP%(2)
        Close 1
        AP%(2) = AP%(2) + 1
        AP = 1

        'AD = AllDone?
        If AP%(1) < AP%(0) + AP%(2) Then Kill "GAME82": AD = 1: GoTo 6065

        Open "GAME82" For Output As #2
        Print #2, FF$
        For I = 0 To 2
            Print #2, AP%(I)
        Next
        Close 2
        GoTo 6065
    End If
    
    6035 If _FileExists("TOURNEY") Then
        Open "TOURNEY" For Input As #2
        Input #2, FF$
        For I = 0 To 2
            'AP(0) = Start Game
            'AP(1) = End Game
            'AP(2) = games played???
            Input #2, AP%(I)
        Next
        Close 2

        Open p$(0) + FF$ + ".TOU" As #1 Len = 90
        For X = 0 To 14
            Field #1, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
        Next
        Field #1, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
        Field #1, 64 As X$, 3 As Q$(20), 3 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
        Field #1, 84 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)
        NG%(13) = 1
        For X = 0 To 14: LSet Q$(X + 1) = MKI$(NG%(X)): Next
        For X = 0 To 3: LSet Q$(20 + X) = YN$(X): Next
        For X = 1 To 3: LSet Q$(X + 23) = MKI$(NG%(X + 14)): Next
        LSet Q$(16) = MKI$(S(0, 0)): LSet Q$(17) = MKI$(S(1, 0))
        LSet Q$(18) = VT$: LSet Q$(19) = W$
        Put #1, AP%(0) + AP%(2)
        Close 1

        AP%(2) = AP%(2) + 1
        AP = 1

        Open "TOURNEY" For Output As #2
        Print #2, FF$
        For I = 0 To 2
            Print #2, AP%(I)
        Next
        Close 2

        Call TOURNEY
        'AD = AllDone?
        If AP%(1) < AP%(0) + AP%(2) Then Kill "TOURNEY": AD = 1: GoTo 6065
        If AP%(1) < AP%(0) + AP%(2) Then End
    End If
    
    6065 Cls
    H% = N
    For PX = 0 To 1
        For J = 0 To 13
            A(PX, J, 16) = ZZ(PX, J)
            A(PX, J, 17) = A(PX, J, 23)
            A(PX, J, 18) = A(PX, J, 24)
        Next
    Next

    For PX = 0 To 1
        For I = 0 To 13
            For J = 1 To 12
                A(PX, I, J) = A(PX, I, J + 6)
            Next
            W0(PX, I) = W%(PX, I, 0): W1(PX, I) = W%(PX, I, 1)
        Next
        Y$(PX) = B$(PX)
    Next

    Q = 0
    6115 C% = 0
    Cls
    Color 15, 0
    Cls
    I = Q

    Call DRAWBOX
    U$ = "COMPILE STATS FOR " + B$(Q) + " (YN)"
    Locate , (80 - Len(U$)) / 2
    Print U$
    Color 15, 0
    If AP = 1 Then 6355

    6325 I$ = GetKeyPress$
    If UCase$(I$) = "N" Then C% = 1: GoTo 6355

    6355 If C% = 1 Then 6290
    Y$(Q) = B$(Q)
    Call STATCONVERT(Q, QQ): FG$ = Mid$(B$(Q), 4, QQ): R$ = FG$ + "." + Left$(B$(Q), 2)

    If Not _FileExists(p$(1) + R$) Then 6185
    Open p$(1) + R$ For Input As #2
    Input #2, B$(Q)
    For I = 0 To 13: Input #2, AA$(Q, I)
        For J = 0 To 13: Input #2, Z2(I, J): Next J
    Input #2, W2(Q, I): Input #2, W3(Q, I): Next I
    For L = 0 To 15: Input #2, Z(L): Next L
    For L = 0 To 15: Input #2, Z1(L): Next L
    Close #2

    6185 TG = 0
    For I = 0 To 13
        If A(Q, I, 2) * 2 + A(Q, I, 4) + W0(Q, I) > Z2(I, 13) Then Z2(I, 13) = A(Q, I, 2) * 2 + A(Q, I, 4) + W0(Q, I)
        If A(Q, I, 10) > 0 Then Z2(I, 0) = Z2(I, 0) + 1
        For J = 1 To 12
            Z2(I, J) = Z2(I, J) + A(Q, I, J)
            If J = 5 Or J = 10 Or J = 11 Then 6215
            Z(J) = Z(J) + A(Q, I, J): Z1(J) = Z1(J) + A(1 - Q, I, J)
        6215 Next J
        Z(0) = Z(0) + A(Q, I, 10)
        TG = TG + A(Q, I, 10)
    Next I
    For I = 0 To 1
        V9(I) = 0
        For I1 = 0 To 13
            V9(I) = V9(I) + A(I, I1, 5)
        Next
    Next
    Z(15) = Z(15) + R3(Q) - V9(Q): Z1(15) = Z1(15) + R3(1 - Q) - V9(1 - Q)
    Z(5) = Z(5) + R3(Q): Z1(5) = Z1(5) + R3(1 - Q): Z(11) = Z(11) + T0(Q): Z1(11) = Z1(11) + T0(1 - Q)
    For J = 0 To 13: Z(13) = Z(13) + W0(Q, J): Z(14) = Z(14) + W1(Q, J)
        Z1(13) = Z1(13) + W0(1 - Q, J): Z1(14) = Z1(14) + W1(1 - Q, J)
    W2(Q, J) = W2(Q, J) + W0(Q, J): W3(Q, J) = W3(Q, J) + W1(Q, J): Next J
    Z(10) = Z(10) + S(Q, 0): Z1(10) = Z1(10) + S(1 - Q, 0)
    I = Q
    Call DRAWBOX
    U$ = "SAVING STATS FOR " + B$(Q)
    Locate , (80 - Len(U$)) / 2
    Print U$
    Color 15, 0

    Open p$(1) + R$ For Output As #1
    Print #1, Y$(Q)
    For I = 0 To 13: Print #1, AA$(Q, I)
        For J = 0 To 13: Print #1, Z2(I, J): Next J
    Print #1, W2(Q, I): Print #1, W3(Q, I): Next I
    For L = 0 To 15: Print #1, Z(L): Next L
    For L = 0 To 15: Print #1, Z1(L): Next L
    Close #1

    Print
    YY% = 0: R$ = R$ + "S"

    If Not _FileExists(p$(1) + R$) Then 6405
    Open p$(1) + R$ For Input As #1
    Input #1, YY%
    For I = 1 To 100: Input #1, O%(I): Next I
    For I = 1 To 100: Input #1, HO$(I): Next I
    For I = 1 To 100: Input #1, HT%(I): Next I
    For I = 1 To 100: Input #1, H$(I): Next I
    For I = 1 To 100
        For I1 = 0 To 1
            Input #1, APT%(I, I1)
        Next
    Next
    For I = 1 To 100: Input #1, CRD(I): Next I
    For I = 1 To 100: Input #1, SITE$(I): Next I
    Close #1
    
    6405 If YY% < 100 Then YY% = YY% + 1
    If H% = 2 And TG < 12100 Then H$(YY%) = "N": GoTo 6540
    If H% <> Q And TG < 12100 Then H$(YY%) = "V": GoTo 6540
    If H% = Q And TG < 12100 Then H$(YY%) = "H": GoTo 6540
    If H% = Q And TG < 13600 Then H$(YY%) = "H1": GoTo 6540
    If H% = Q And TG < 15100 Then H$(YY%) = "H2": GoTo 6540
    If H% = Q And TG < 16600 Then H$(YY%) = "H3": GoTo 6540
    If H% = Q And TG < 18100 Then H$(YY%) = "H4": GoTo 6540
    If H% = 2 And TG < 13600 Then H$(YY%) = "N1": GoTo 6540
    If H% = 2 And TG < 15100 Then H$(YY%) = "N2": GoTo 6540
    If H% = 2 And TG < 16600 Then H$(YY%) = "N3": GoTo 6540
    If H% = 2 And TG < 18100 Then H$(YY%) = "N4": GoTo 6540
    If H% <> Q And TG < 13600 Then H$(YY%) = "V1": GoTo 6540
    If H% <> Q And TG < 15100 Then H$(YY%) = "V2": GoTo 6540
    If H% <> Q And TG < 16600 Then H$(YY%) = "V3": GoTo 6540
    If H% <> Q And TG < 18100 Then H$(YY%) = "V4": GoTo 6540
    6540 O%(YY%) = S(Q, 0)
    HO$(YY%) = Y$(1 - Q)
    HT%(YY%) = S(1 - Q, 0)
    CRD(YY%) = Y3
    If TR%(0, 9) = 0 Then TR%(0, 9) = 78
    If TR%(1, 9) = 0 Then TR%(1, 9) = 78
    APT%(YY%, 0) = TR%(Q, 9)
    APT%(YY%, 1) = TR%(1 - Q, 9)
    If S$(1) = "" Then S$(1) = "MEMORIAL COLISEUM"
    If N = 2 Then
        SITE$(YY%) = "NEUTRAL SITE"
    Else
        SITE$(YY%) = S$(1)
    End If

    Open p$(1) + R$ For Output As #1
    Print #1, YY%

    For I = 1 To 100: Print #1, O%(I): Next I
    For I = 1 To 100: Print #1, HO$(I): Next I
    For I = 1 To 100: Print #1, HT%(I): Next I
    For I = 1 To 100: Print #1, H$(I): Next I
    For I = 1 To 100
        For I1 = 0 To 1
            Print #1, APT%(I, I1)
        Next
    Next
    For I = 1 To 100: Print #1, CRD(I): Next I
    For I = 1 To 100: Print #1, SITE$(I): Next I
    Close #1

    For I = 1 To 100
        O%(I) = 0: HT%(I) = 0: HO$(I) = "": H$(I) = "": CRD(I) = 0: SITE$(I) = ""
        For I1 = 0 To 1
            APT%(I, I1) = 0
        Next
    Next I

    For J = 0 To 13
        For K = 0 To 13
            Z2(J, K) = 0
        Next K
    Next J

    For L = 0 To 15: Z(L) = 0: Z1(L) = 0: Next L

    6290 Q = Q + 1: Z(12) = 0
    If Q < 2 Then 6115
    If AP = 1 And AD <> 1 Then 6292
    'If AD = 1 Then End
    'If we've played our last scheduled game
    If AD = 1 Then Run "HELLO.EXE"

6292 End Sub


'------------------------------
'    EVENTS Subroutine
'------------------------------
' ...explanation...
Sub EVENTS (COUNT, SPECIAL)
    If Not _FileExists("EVENTS.BOX") Then 8450
    Open "EVENTS.BOX" For Input As #1
    For X = 1 To 13
        Input #1, E%(X)
    Next
    Close 1
    102 If H - 3 > E%(1) Then GoSub 7100: GoTo 8450
    If S(1, 0) + S(0, 0) > E%(2) Then GoSub 7100: GoTo 8450
    If R3(0) + R3(1) > E%(3) Then GoSub 7100: GoTo 8450
    If S(1, 0) > E%(4) Or S(0, 0) > E%(4) Then GoSub 7100: GoTo 8450
    If R3(1) > E%(5) Or R3(0) > E%(5) Then GoSub 7100: GoTo 8450

    For I = 0 To 1
        For I1 = 0 To 13
            If A(I, I1, 8) * 2 + A(I, I1, 10) + W%(I, I1, 0) > E%(6) Then GoSub 7100: GoTo 8450
            If A(I, I1, 11) > E%(7) Then GoSub 7100: GoTo 8450
            If A(I, I1, 14) > E%(8) Then GoSub 7100: GoTo 8450
            If A(I, I1, 15) > E%(9) Then GoSub 7100: GoTo 8450
            If A(I, I1, 13) > E%(10) Then GoSub 7100: GoTo 8450
            If W%(I, I1, 0) > E%(11) Then GoSub 7100: GoTo 8450
        Next
    Next
    GoTo 8450
    7100 Spec$ = p$(1) + "*.SPC"
    COUNT = FCount%(Spec$)
    COUNT = COUNT + 1
    SPECIAL = 1
    Return

8450 End Sub



'------------------------------
'    QUARTERTOTAL Subroutine
'------------------------------
' ...explanation...
Sub QUARTERTOTAL ()

    For I = 0 To 1
        For I2 = 0 To 14: V9(I2) = 0: Next
        For I1 = 0 To 13
            V9(0) = V9(0) + A(I, I1, 8)
            V9(1) = V9(1) + A(I, I1, 7)
            V9(2) = V9(2) + A(I, I1, 10)
            V9(3) = V9(3) + A(I, I1, 9)
            V9(4) = V9(4) + A(I, I1, 11)
            V9(5) = V9(5) + (A(I, I1, 8) * 2) + A(I, I1, 10) + W%(I, I1, 0)
            V9(13) = V9(13) + W%(I, I1, 0)
            V9(14) = V9(14) + W%(I, I1, 1)
            V9(6) = V9(6) + A(I, I1, 12)
            V9(7) = V9(7) + A(I, I1, 14)
            V9(8) = V9(8) + A(I, I1, 15)
            V9(9) = V9(9) + A(I, I1, 13)
            V9(11) = V9(11) + A(I, I1, 24)
            V9(12) = V9(12) + A(I, I1, 23)
        Next

        If H = 1 Or H = 2 Then
            For I2 = 0 To 14
                QR(I, H, I2) = V9(I2) - QR(I, H - 1, I2)
            Next

            For I1 = 0 To 13
                For I2 = 7 To 15
                    QQ(I, H, I1, I2 - 6) = A(I, I1, I2) - QQ(I, H - 1, I1, I2 - 6)
                Next
                QQ(I, H, I1, 10) = W%(I, I1, 0) - QQ(I, H - 1, I1, 10)
                QQ(I, H, I1, 11) = W%(I, I1, 1) - QQ(I, H - 1, I1, 11)
                QQ(I, H, I1, 12) = A(I, I1, 24) - QQ(I, H - 1, I1, 12)
                QQ(I, H, I1, 13) = A(I, I1, 8) * 2 + A(I, I1, 10) + W%(I, I1, 0) - QQ(I, H - 1, I1, 13)
                QQ(I, H, I1, 14) = A(I, I1, 23) - QQ(I, H - 1, I1, 14)
            Next
        End If

        If H = 3 Then
            For I2 = 0 To 14
                QR(I, H, I2) = V9(I2) - QR(I, H - 1, I2) - QR(I, H - 2, I2)
            Next
            For I1 = 0 To 13
                For I2 = 7 To 15
                    QQ(I, H, I1, I2 - 6) = A(I, I1, I2) - QQ(I, H - 1, I1, I2 - 6) - QQ(I, H - 2, I1, I2 - 6)
                Next
                QQ(I, H, I1, 10) = W%(I, I1, 0) - QQ(I, H - 1, I1, 10) - QQ(I, H - 2, I1, 10)
                QQ(I, H, I1, 11) = W%(I, I1, 1) - QQ(I, H - 1, I1, 11) - QQ(I, H - 2, I1, 11)
                QQ(I, H, I1, 12) = A(I, I1, 24) - QQ(I, H - 1, I1, 12) - QQ(I, H - 2, I1, 12)
                QQ(I, H, I1, 13) = A(I, I1, 8) * 2 + A(I, I1, 10) + W%(I, I1, 0) - QQ(I, H - 1, I1, 13) - QQ(I, H - 2, I1, 13)
                QQ(I, H, I1, 14) = A(I, I1, 23) - QQ(I, H - 1, I1, 14) - QQ(I, H - 2, I1, 14)
            Next
        End If

        If H = 4 Then
            For I2 = 0 To 14
                QR(I, H, I2) = V9(I2) - QR(I, H - 1, I2) - QR(I, H - 2, I2) - QR(I, H - 3, I2)
            Next
            For I1 = 0 To 13
                For I2 = 7 To 15
                    QQ(I, H, I1, I2 - 6) = A(I, I1, I2) - QQ(I, H - 1, I1, I2 - 6) - QQ(I, H - 2, I1, I2 - 6) - QQ(I, H - 3, I1, I2 - 6)
                Next
                QQ(I, H, I1, 10) = W%(I, I1, 0) - QQ(I, H - 1, I1, 10) - QQ(I, H - 2, I1, 10) - QQ(I, H - 3, I1, 10)
                QQ(I, H, I1, 11) = W%(I, I1, 1) - QQ(I, H - 1, I1, 11) - QQ(I, H - 2, I1, 11) - QQ(I, H - 3, I1, 11)
                QQ(I, H, I1, 12) = A(I, I1, 24) - QQ(I, H - 1, I1, 12) - QQ(I, H - 2, I1, 12) - QQ(I, H - 3, I1, 12)
                QQ(I, H, I1, 13) = A(I, I1, 8) * 2 + A(I, I1, 10) + W%(I, I1, 0) - QQ(I, H - 1, I1, 13) - QQ(I, H - 2, I1, 13) - QQ(I, H - 3, I1, 13)
                QQ(I, H, I1, 14) = A(I, I1, 23) - QQ(I, H - 1, I1, 14) - QQ(I, H - 2, I1, 14) - QQ(I, H - 3, I1, 14)
            Next
        End If

        If H = 5 Then
            For I2 = 0 To 14
                QR(I, H, I2) = V9(I2) - QR(I, H - 1, I2) - QR(I, H - 2, I2) - QR(I, H - 3, I2) - QR(I, H - 4, I2)
            Next
            For I1 = 0 To 13
                For I2 = 7 To 15
                    QQ(I, H, I1, I2 - 6) = A(I, I1, I2) - QQ(I, H - 1, I1, I2 - 6) - QQ(I, H - 2, I1, I2 - 6) - QQ(I, H - 3, I1, I2 - 6) - QQ(I, H - 4, I1, I2 - 6)
                Next
                QQ(I, H, I1, 10) = W%(I, I1, 0) - QQ(I, H - 1, I1, 10) - QQ(I, H - 2, I1, 10) - QQ(I, H - 3, I1, 10) - QQ(I, H - 4, I1, 10)
                QQ(I, H, I1, 11) = W%(I, I1, 1) - QQ(I, H - 1, I1, 11) - QQ(I, H - 2, I1, 11) - QQ(I, H - 3, I1, 11) - QQ(I, H - 4, I1, 11)
                QQ(I, H, I1, 12) = A(I, I1, 24) - QQ(I, H - 1, I1, 12) - QQ(I, H - 2, I1, 12) - QQ(I, H - 3, I1, 12) - QQ(I, H - 4, I1, 12)
                QQ(I, H, I1, 13) = A(I, I1, 8) * 2 + A(I, I1, 10) + W%(I, I1, 0) - QQ(I, H - 1, I1, 13) - QQ(I, H - 2, I1, 13) - QQ(I, H - 3, I1, 13) - QQ(I, H - 4, I1, 13)
                QQ(I, H, I1, 14) = A(I, I1, 23) - QQ(I, H - 1, I1, 14) - QQ(I, H - 2, I1, 14) - QQ(I, H - 3, I1, 14) - QQ(I, H - 4, I1, 14)
            Next
        End If

        If H = 6 Then
            For I2 = 0 To 14
                QR(I, H, I2) = V9(I2) - QR(I, H - 1, I2) - QR(I, H - 2, I2) - QR(I, H - 3, I2) - QR(I, H - 4, I2) - QR(I, H - 5, I2)
            Next
            For I1 = 0 To 13
                For I2 = 7 To 15
                    QQ(I, H, I1, I2 - 6) = A(I, I1, I2) - QQ(I, H - 1, I1, I2 - 6) - QQ(I, H - 2, I1, I2 - 6) - QQ(I, H - 3, I1, I2 - 6) - QQ(I, H - 4, I1, I2 - 6) - QQ(I, H - 5, I1, I2 - 6)
                Next
                QQ(I, H, I1, 10) = W%(I, I1, 0) - QQ(I, H - 1, I1, 10) - QQ(I, H - 2, I1, 10) - QQ(I, H - 3, I1, 10) - QQ(I, H - 4, I1, 10) - QQ(I, H - 5, I1, 10)
                QQ(I, H, I1, 11) = W%(I, I1, 1) - QQ(I, H - 1, I1, 11) - QQ(I, H - 2, I1, 11) - QQ(I, H - 3, I1, 11) - QQ(I, H - 4, I1, 11) - QQ(I, H - 5, I1, 11)
                QQ(I, H, I1, 12) = A(I, I1, 24) - QQ(I, H - 1, I1, 12) - QQ(I, H - 2, I1, 12) - QQ(I, H - 3, I1, 12) - QQ(I, H - 4, I1, 12) - QQ(I, H - 5, I1, 12)
                QQ(I, H, I1, 13) = A(I, I1, 8) * 2 + A(I, I1, 10) + W%(I, I1, 0) - QQ(I, H - 1, I1, 13) - QQ(I, H - 2, I1, 13) - QQ(I, H - 3, I1, 13) - QQ(I, H - 4, I1, 13) - QQ(I, H - 5, I1, 13)
                QQ(I, H, I1, 14) = A(I, I1, 23) - QQ(I, H - 1, I1, 14) - QQ(I, H - 2, I1, 14) - QQ(I, H - 3, I1, 14) - QQ(I, H - 4, I1, 14) - QQ(I, H - 5, I1, 14)
            Next
        End If

        If H = 7 Then
            For I2 = 0 To 14
                QR(I, H, I2) = V9(I2) - QR(I, H - 1, I2) - QR(I, H - 2, I2) - QR(I, H - 3, I2) - QR(I, H - 4, I2) - QR(I, H - 5, I2) - QR(I, H - 6, I2)
            Next
            For I1 = 0 To 13
                For I2 = 7 To 15
                    QQ(I, H, I1, I2 - 6) = A(I, I1, I2) - QQ(I, H - 1, I1, I2 - 6) - QQ(I, H - 2, I1, I2 - 6) - QQ(I, H - 3, I1, I2 - 6) - QQ(I, H - 4, I1, I2 - 6) - QQ(I, H - 5, I1, I2 - 6) - QQ(I, H - 6, I1, I2 - 6)
                Next
                QQ(I, H, I1, 10) = W%(I, I1, 0) - QQ(I, H - 1, I1, 10) - QQ(I, H - 2, I1, 10) - QQ(I, H - 3, I1, 10) - QQ(I, H - 4, I1, 10) - QQ(I, H - 5, I1, 10) - QQ(I, H - 6, I1, 10)
                QQ(I, H, I1, 11) = W%(I, I1, 1) - QQ(I, H - 1, I1, 11) - QQ(I, H - 2, I1, 11) - QQ(I, H - 3, I1, 11) - QQ(I, H - 4, I1, 11) - QQ(I, H - 5, I1, 11) - QQ(I, H - 6, I1, 11)
                QQ(I, H, I1, 12) = A(I, I1, 24) - QQ(I, H - 1, I1, 12) - QQ(I, H - 2, I1, 12) - QQ(I, H - 3, I1, 12) - QQ(I, H - 4, I1, 12) - QQ(I, H - 5, I1, 12) - QQ(I, H - 6, I1, 12)
                QQ(I, H, I1, 13) = A(I, I1, 8) * 2 + A(I, I1, 10) + W%(I, I1, 0) - QQ(I, H - 1, I1, 13) - QQ(I, H - 2, I1, 13) - QQ(I, H - 3, I1, 13) - QQ(I, H - 4, I1, 13) - QQ(I, H - 5, I1, 13) - QQ(I, H - 6, I1, 13)
                QQ(I, H, I1, 14) = A(I, I1, 23) - QQ(I, H - 1, I1, 14) - QQ(I, H - 2, I1, 14) - QQ(I, H - 3, I1, 14) - QQ(I, H - 4, I1, 14) - QQ(I, H - 5, I1, 14) - QQ(I, H - 6, I1, 14)
            Next
        End If

    Next
    For I = 0 To 14: V9(I) = 0: Next
End Sub


'------------------------------
'    AICOMCOACH Subroutine
'------------------------------
' ...explanation...
Sub AICOMCOACH
    If P9 = 0 Then I6 = 1
    If P9 = 1 Then I6 = 0
    If S(P9, 0) - S(I6, 0) < 0 Then I5 = S(I6, 0) - S(P9, 0): GoTo 8867
    R0 = Int(Rnd(1) * 100) + 1: DT%(P9) = 0: D2(P9) = 0: P7(P9) = 0
    If R0 >= 37 Then DT%(P9) = 0: D2(P9) = 0: P7(P9) = 0
    If R0 > 10 And R0 <= 36 Then D2(P9) = 2: DT%(P9) = 2: P7(P9) = 0
    If R0 <= 10 Then D2(P9) = 1: DT%(P9) = 1: P7(P9) = 0
    R0 = Int(Rnd(1) * 100) + 1
    If R0 <= 5 Then P7(P9) = 1: DT%(P9) = 4: D2(P9) = 0
    If R0 > 5 And R0 <= 9 Then P7(P9) = 3: DT%(P9) = 8: D2(P9) = 0
    If R0 = 10 Then P7(P9) = 3: DT%(P9) = 7: D2(P9) = 2

    GoTo 8880
    8867 R0 = Int(Rnd(1) * 100) + 1
    If H = 1 And R0 > 37 Then D2(P9) = 0: P7(P9) = 0: DT%(P9) = 0
    If H = 1 And R0 > 10 And R0 <= 36 Then D2(P9) = 2: P7(P9) = 0: DT%(P9) = 2
    If H = 1 And R0 > 5 And R0 <= 10 Then D2(P9) = 3: P7(P9) = 0: DT%(P9) = 3
    If H = 1 And R0 <= 5 Then D2(P9) = 1: P7(P9) = 0: DT%(P9) = 1
    R0 = Int(Rnd(1) * 100) + 1
    If H = 1 And R0 <= 15 Then D2(P9) = 0: P7(P9) = 1: DT%(P9) = 4
    If H = 1 And R0 > 9 And R0 <= 12 Then D2(P9) = 0: P7(P9) = 3: DT%(P9) = 8
    If H = 1 And R0 > 6 And R0 <= 9 Then D2(P9) = 2: P7(P9) = 3: DT%(P9) = 7
    If H = 1 And R0 > 3 And R0 <= 6 Then D2(P9) = 1: P7(P9) = 3: DT%(P9) = 9
    If H = 1 And R0 <= 3 Then D2(P9) = 0: P7(P9) = 4: DT%(P9) = 10
    R0 = Int(Rnd(1) * 100) + 1
    If H = 2 And T >= 600 And I5 > 20 And R0 <= 60 Then D2(P9) = 0: P7(P9) = 0: DT%(P9) = 0
    If H = 2 And T >= 600 And I5 > 20 And R0 > 60 And R0 <= 80 Then D2(P9) = 1: P7(P9) = 0: DT%(P9) = 1
    If H = 2 And T >= 600 And I5 > 20 And R0 > 80 And R0 <= 90 Then D2(P9) = 1: P7(P9) = 2: DT%(P9) = 6
    If H = 2 And T >= 600 And I5 > 20 And R0 > 90 And R0 <= 95 Then D2(P9) = 0: P7(P9) = 4: DT%(P9) = 10
    If H = 2 And T >= 600 And I5 > 20 And R0 > 95 Then D2(P9) = 1: P7(P9) = 4: DT%(P9) = 11
    If H = 2 And T >= 600 And I5 >= 10 And I5 < 20 And R0 <= 75 Then D2(P9) = 0: P7(P9) = 0: DT%(P9) = 0
    If H = 2 And T >= 600 And I5 >= 10 And I5 < 20 And R0 > 75 And R0 <= 83 Then D2(P9) = 0: P7(P9) = 1: DT%(P9) = 4
    If H = 2 And T >= 600 And I5 >= 10 And I5 < 20 And R0 > 83 And R0 <= 91 Then D2(P9) = 1: P7(P9) = 0: DT%(P9) = 1
    If H = 2 And T >= 600 And I5 >= 10 And I5 < 20 And R0 > 91 And R0 <= 96 Then D2(P9) = 0: P7(P9) = 4: DT%(P9) = 10
    If H = 2 And T >= 600 And I5 >= 10 And I5 < 20 And R0 > 96 Then D2(P9) = 1: P7(P9) = 4: DT%(P9) = 11
    If H = 2 And T >= 600 And I5 < 10 And R0 <= 70 Then D2(P9) = 0: P7(P9) = 0: DT%(P9) = 0
    If H = 2 And T >= 600 And I5 < 10 And R0 > 70 And R0 <= 75 Then D2(P9) = 1: P7(P9) = 0: DT%(P9) = 1
    If H = 2 And T >= 600 And I5 < 10 And R0 > 76 And R0 <= 85 Then D2(P9) = 2: P7(P9) = 0: DT%(P9) = 2
    If H = 2 And T >= 600 And I5 < 10 And R0 > 85 And R0 <= 93 Then D2(P9) = 0: P7(P9) = 2: DT%(P9) = 5
    If H = 2 And T >= 600 And I5 < 10 And R0 > 93 And R0 <= 97 Then D2(P9) = 3: P7(P9) = 0: DT%(P9) = 3
    If H = 2 And T >= 600 And I5 < 10 And R0 > 97 Then D2(P9) = 2: P7(P9) = 4: DT%(P9) = 12
    If H = 2 And T >= 120 And T < 600 And I5 > 20 And R0 <= 70 Then D2(P9) = 0: P7(P9) = 0: DT%(P9) = 0
    If H = 2 And T >= 120 And T < 600 And I5 > 20 And R0 > 70 And R0 <= 80 Then D2(P9) = 0: P7(P9) = 2: DT%(P9) = 5
    If H = 2 And T >= 120 And T < 600 And I5 > 20 And R0 > 80 And R0 <= 90 Then D2(P9) = 0: P7(P9) = 4: DT%(P9) = 10
    If H = 2 And T >= 120 And T < 600 And I5 > 20 And R0 > 90 Then D2(P9) = 3: P7(P9) = 4: DT%(P9) = 13
    If H = 2 And T >= 120 And T < 600 And I5 > 10 And I5 < 20 And R0 <= 70 Then D2(P9) = 0: P7(P9) = 0: DT%(P9) = 0
    If H = 2 And T >= 120 And T < 600 And I5 > 10 And I5 < 20 And R0 > 70 And R0 <= 80 Then D2(P9) = 0: P7(P9) = 2: DT%(P9) = 5
    If H = 2 And T >= 120 And T < 600 And I5 > 10 And I5 < 20 And R0 > 80 And R0 <= 90 Then D2(P9) = 1: P7(P9) = 4: DT%(P9) = 12
    If H = 2 And T >= 120 And T < 600 And I5 > 10 And I5 < 20 And R0 > 90 Then D2(P9) = 1: P7(P9) = 0: DT%(P9) = 1
    If H = 2 And T >= 120 And T < 600 And I5 <= 10 And R0 <= 70 Then D2(P9) = 0: P7(P9) = 0: DT%(P9) = 0
    If H = 2 And T >= 120 And T < 600 And I5 <= 10 And R0 > 70 And R0 <= 90 Then D2(P9) = 2: P7(P9) = 0: DT%(P9) = 2
    If H = 2 And T >= 120 And T < 600 And I5 <= 10 And R0 > 90 And R0 <= 96 Then D2(P9) = 1: P7(P9) = 0: DT%(P9) = 1
    If H = 2 And T >= 120 And T < 600 And I5 <= 10 And R0 > 96 Then D2(P9) = 0: P7(P9) = 4: DT%(P9) = 10
    If H = 2 And T < 120 And I5 > 10 And R0 <= 50 Then D2(P9) = 1: P7(P9) = 4: DT%(P9) = 11
    If H = 2 And T < 120 And I5 > 10 And R0 > 50 Then D2(P9) = 3: P7(P9) = 4: DT%(P9) = 13
    If H = 2 And T < 120 And I5 <= 10 And I5 > 4 Then D2(P9) = 3: P7(P9) = 5: DT%(P9) = 14
    If H = 2 And T < 120 And I5 <= 4 And R0 <= 25 Then D2(P9) = 0: P7(P9) = 2: DT%(P9) = 5
    If H = 2 And T < 120 And I5 <= 4 And R0 > 25 And R0 <= 50 Then D2(P9) = 1: P7(P9) = 2: DT%(P9) = 6
    If H = 2 And T < 120 And I5 <= 4 And R0 > 50 And R0 <= 75 Then D2(P9) = 1: P7(P9) = 4: DT%(P9) = 11
    If H = 2 And T < 120 And I5 <= 4 And R0 > 75 Then D2(P9) = 3: P7(P9) = 4: DT%(P9) = 13
    If H > 2 And R0 <= 50 Then D2(P9) = 0: P7(P9) = 0: DT%(P9) = 0
    If H > 2 And R0 > 50 Then D2(P9) = 2: P7(P9) = 0: DT%(P9) = 2
    8880 R0 = Int(Rnd(1) * 100) + 1
    If H = 1 And R0 <= 80 Then OF%(P9) = 0
    If H = 1 And R0 > 80 And R0 <= 90 Then OF%(P9) = 1
    If H = 1 And R0 > 90 Then OF%(P9) = 2
    If H >= 2 And S(P9, 0) - S(I6, 0) < 0 And R0 <= 80 Then OF%(P9) = 0
    If H >= 2 And S(P9, 0) - S(I6, 0) < 0 And R0 > 80 Then OF%(P9) = 1
    If H >= 2 And S(P9, 0) - S(I6, 0) >= 0 And R0 <= 80 Then OF%(P9) = 0
    If H >= 2 And S(P9, 0) - S(I6, 0) >= 0 And R0 > 80 Then OF%(P9) = 2
    If H >= 2 And T <= 180 And S(P9, 0) - S(I6, 0) < 0 And R0 <= 30 Then OF%(P9) = 3
    If H >= 2 And T <= 180 And S(P9, 0) - S(I6, 0) >= 0 And R0 <= 30 Then OF%(P9) = 4
    If H >= 2 And T <= 60 And X3% = 1 And (S(P9, 0) - S(1 - P9, 0) = -3 Or S(P9, 0) - S(1 - P9, 0) = -6) Then OF%(P9) = 9
End Sub


'------------------------------
'   RECORDSCHECK Subroutine
'------------------------------
' ...explanation...
Sub RECORDSCHECK ()
    Dim RC$(50, 4), REC(50, 2) As Single, TRC$(125, 3), TREC(125, 2) As Single
    Dim V8(1, 14)
    Dim VERSUS$(1)
    VERSUS$(0) = "AT": VERSUS$(1) = "VS."
    If Not _FileExists(p$(1) + "RECORDS.GAM") Then
        TREC(2, 0) = 999
        TREC(4, 0) = 999
        TREC(7, 0) = 999
        TREC(9, 0) = 999
        TREC(11, 0) = 999
        TREC(13, 0) = 999
        TREC(15, 0) = 999
        TREC(17, 0) = 999
        TREC(19, 0) = 999
        TREC(21, 0) = 999
        TREC(23, 0) = 999
        TREC(25, 0) = 999
        TREC(27, 0) = 999
        TREC(29, 0) = 999
        TREC(31, 0) = 999
        TREC(33, 0) = 999
        TREC(35, 0) = 999
        TREC(37, 0) = 999
        TREC(43, 0) = 999
        TREC(45, 0) = 999
        TREC(61, 0) = 999
        TREC(63, 0) = 999
        TREC(65, 0) = 999
        TREC(67, 0) = 999
        TREC(73, 0) = 999
        TREC(75, 0) = 999
        TREC(81, 0) = 999
        TREC(83, 0) = 999
        TREC(87, 0) = 999
        TREC(89, 0) = 999
        TREC(92, 0) = 999
        TREC(94, 0) = 999
        TREC(97, 0) = 999
        TREC(99, 0) = 999
        TREC(105, 0) = 999
        TREC(107, 0) = 999
        TREC(113, 0) = 999
        TREC(115, 0) = 999
        TREC(120, 0) = 999
        TREC(123, 0) = 999
        TREC(125, 0) = 999
        GoTo 505
    End If
    Open p$(1) + "RECORDS.GAM" For Input As #1
    For I = 0 To 48
        For I1 = 0 To 2
            Input #1, REC(I, I1)
        Next
        For I1 = 0 To 4
            Input #1, RC$(I, I1)
        Next
    Next
    For I = 0 To 125
        For I1 = 0 To 2
            Input #1, TREC(I, I1)
        Next
        For I1 = 0 To 3
            Input #1, TRC$(I, I1)
        Next
    Next
    Close 1

    505 'CHECK THE INDIVIDUAL RECORDS

    For I = 0 To 1
        For I1 = 0 To 13

            ' SCORING RECORDS FOR POINTS IN A GAME, HALF, QUARTER AND OVERTIME PERIOD
            If A(I, I1, 8) * 2 + A(I, I1, 10) + W%(I, I1, 0) > REC(0, 0) Then
                REC(0, 0) = A(I, I1, 8) * 2 + A(I, I1, 10) + W%(I, I1, 0) ' POINTS
                REC(0, 1) = 1 ' TIMES DONE
                REC(0, 2) = 0 ' GAME
                RC$(0, 0) = AA$(I, I1) ' NAME
                RC$(0, 1) = B$(I) ' TEAM
                RC$(0, 2) = VERSUS$(I) ' AT/VS
                RC$(0, 3) = B$(1 - I) ' OPPONENT
                RC$(0, 4) = Date$ ' DATE
            End If
            If QQ(I, 1, I1, 13) > REC(1, 0) Then
                REC(1, 0) = QQ(I, 1, I1, 13) ' POINTS
                REC(1, 1) = 1 ' TIMES DONE
                REC(1, 2) = 1 ' HALF
                RC$(1, 0) = AA$(I, I1) ' NAME
                RC$(1, 1) = B$(I) ' TEAM
                RC$(1, 2) = VERSUS$(I) ' AT/VS
                RC$(1, 3) = B$(1 - I) ' OPPONENT
                RC$(1, 4) = Date$ ' DATE
            End If

            If QQ(I, 2, I1, 13) > REC(1, 0) Then
                REC(1, 0) = QQ(I, 2, I1, 13) ' POINTS
                REC(1, 1) = 1 ' TIMES DONE
                REC(1, 2) = 2 ' HALF
                RC$(1, 0) = AA$(I, I1) ' NAME
                RC$(1, 1) = B$(I) ' TEAM
                RC$(1, 2) = VERSUS$(I) ' AT/VS
                RC$(1, 3) = B$(1 - I) ' OPPONENT
                RC$(1, 4) = Date$ ' DATE
            End If

            For I2 = 3 To 8
                If QQ(I, I2, I1, 13) > REC(3, 0) Then
                    REC(3, 0) = QQ(I, I2, I1, 13) ' POINTS
                    REC(3, 1) = 1 ' TIMES DONE
                    REC(3, 2) = I2 ' QTR
                    RC$(3, 0) = AA$(I, I1) ' NAME
                    RC$(3, 1) = B$(I) ' TEAM
                    RC$(3, 2) = VERSUS$(I) ' AT/VS
                    RC$(3, 3) = B$(1 - I) ' OPPONENT
                    RC$(3, 4) = Date$ ' DATE
                End If
            Next I2

            ' FIELD GOAL PERCENTAGE RECORDS FOR HIGH %, MOST W/NO MISSES, MOST W/NONE MADE
            If A(I, I1, 8) < 15 Or A(I, I1, 7) = 0 Then 900
            If A(I, I1, 8) / A(I, I1, 7) > REC(4, 0) Then
                REC(4, 0) = A(I, I1, 8) / A(I, I1, 7) ' PCTG
                REC(4, 1) = A(I, I1, 8) ' FGM
                REC(4, 2) = A(I, I1, 7) ' FGA
                RC$(4, 0) = AA$(I, I1) ' NAME
                RC$(4, 1) = B$(I) ' TEAM
                RC$(4, 2) = VERSUS$(I) ' AT/VS
                RC$(4, 3) = B$(1 - I) ' OPPONENT
                RC$(4, 4) = Date$ ' DATE
            End If
            900
            If A(I, I1, 8) = A(I, I1, 7) And A(I, I1, 7) > REC(5, 0) Then
                REC(5, 0) = A(I, I1, 7) ' MOST FGM
                RC$(5, 0) = AA$(I, I1) ' NAME
                RC$(5, 1) = B$(I) ' TEAM
                RC$(5, 2) = VERSUS$(I) ' AT/VS
                RC$(5, 3) = B$(1 - I) ' OPPONENT
                RC$(5, 4) = Date$ ' DATE
            End If
            If A(I, I1, 8) = 0 And A(I, I1, 7) > REC(6, 0) Then
                REC(6, 0) = A(I, I1, 7) ' 0 FGM /FGA
                RC$(6, 0) = AA$(I, I1) ' NAME
                RC$(6, 1) = B$(I) ' TEAM
                RC$(6, 2) = VERSUS$(I) ' AT/VS
                RC$(6, 3) = B$(1 - I) ' OPPONENT
                RC$(6, 4) = Date$ ' DATE
            End If

            ' FIELD GOALS MADE IN A GAME, HALF, QUARTER AND OVERTIME PERIOD
            If A(I, I1, 8) > REC(7, 0) Then
                REC(7, 0) = A(I, I1, 8) ' FGM
                REC(7, 1) = 1 ' TIMES DONE
                REC(7, 2) = 0 ' GAME
                RC$(7, 0) = AA$(I, I1) ' NAME
                RC$(7, 1) = B$(I) ' TEAM
                RC$(7, 2) = VERSUS$(I) ' AT/VS
                RC$(7, 3) = B$(1 - I) ' OPPONENT
                RC$(7, 4) = Date$ ' DATE
            End If
            If QQ(I, 1, I1, 2) > REC(8, 0) Then
                REC(8, 0) = QQ(I, 1, I1, 2) ' FGM
                REC(8, 1) = 1 ' TIMES DONE
                REC(8, 2) = 1 ' HALF
                RC$(8, 0) = AA$(I, I1) ' NAME
                RC$(8, 1) = B$(I) ' TEAM
                RC$(8, 2) = VERSUS$(I) ' AT/VS
                RC$(8, 3) = B$(1 - I) ' OPPONENT
                RC$(8, 4) = Date$ ' DATE
            End If
            If QQ(I, 2, I1, 2) > REC(8, 0) Then
                REC(8, 0) = QQ(I, 2, I1, 2) ' FGM
                REC(8, 1) = 1 ' TIMES DONE
                REC(8, 2) = 2 ' HALF
                RC$(8, 0) = AA$(I, I1) ' NAME
                RC$(8, 1) = B$(I) ' TEAM
                RC$(8, 2) = VERSUS$(I) ' AT/VS
                RC$(8, 3) = B$(1 - I) ' OPPONENT
                RC$(8, 4) = Date$ ' DATE
            End If

            For I2 = 3 To 8
                If QQ(I, I2, I1, 2) > REC(10, 0) Then
                    REC(10, 0) = QQ(I, I2, I1, 2) ' FGM
                    REC(10, 1) = 1 ' TIMES DONE
                    REC(10, 2) = I2 ' QTR
                    RC$(10, 0) = AA$(I, I1) ' NAME
                    RC$(10, 1) = B$(I) ' TEAM
                    RC$(10, 2) = VERSUS$(I) ' AT/VS
                    RC$(10, 3) = B$(1 - I) ' OPPONENT
                    RC$(10, 4) = Date$ ' DATE
                End If
            Next I2

            ' FIELD GOAL ATTEMPTS MADE IN A GAME, HALF, QUARTER AND OVERTIME PERIOD
            If A(I, I1, 7) > REC(11, 0) Then
                REC(11, 0) = A(I, I1, 7) ' FGA
                REC(11, 1) = 1 ' TIMES DONE
                REC(11, 2) = 0 ' GAME
                RC$(11, 0) = AA$(I, I1) ' NAME
                RC$(11, 1) = B$(I) ' TEAM
                RC$(11, 2) = VERSUS$(I) ' AT/VS
                RC$(11, 3) = B$(1 - I) ' OPPONENT
                RC$(11, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 1) > REC(12, 0) Then
                REC(12, 0) = QQ(I, 1, I1, 1) ' FGA
                REC(12, 1) = 1 ' TIMES DONE
                REC(12, 2) = 1 ' HALF
                RC$(12, 0) = AA$(I, I1) ' NAME
                RC$(12, 1) = B$(I) ' TEAM
                RC$(12, 2) = VERSUS$(I) ' AT/VS
                RC$(12, 3) = B$(1 - I) ' OPPONENT
                RC$(12, 4) = Date$ ' DATE
            End If
            If QQ(I, 2, I1, 1) > REC(12, 0) Then
                REC(12, 0) = QQ(I, 2, I1, 1) ' FGA
                REC(12, 1) = 1 ' TIMES DONE
                REC(12, 2) = 2 ' HALF
                RC$(12, 0) = AA$(I, I1) ' NAME
                RC$(12, 1) = B$(I) ' TEAM
                RC$(12, 2) = VERSUS$(I) ' AT/VS
                RC$(12, 3) = B$(1 - I) ' OPPONENT
                RC$(12, 4) = Date$ ' DATE
            End If

            For I2 = 3 To 8
                If QQ(I, I2, I1, 1) > REC(14, 0) Then
                    REC(14, 0) = QQ(I, I2, I1, 1) ' FGA
                    REC(14, 1) = 1 ' TIMES DONE
                    REC(14, 2) = I2 ' QTR
                    RC$(14, 0) = AA$(I, I1) ' NAME
                    RC$(14, 1) = B$(I) ' TEAM
                    RC$(14, 2) = VERSUS$(I) ' AT/VS
                    RC$(14, 3) = B$(1 - I) ' OPPONENT
                    RC$(14, 4) = Date$ ' DATE
                End If
            Next I2

            ' THREE-POINT FIELD GOALS MOST MADE NO MISSES, MOST ATT NONE MADE
            If W%(I, I1, 0) = W%(I, I1, 1) And W%(I, I1, 0) > REC(15, 0) Then
                REC(15, 0) = W%(I, I1, 0) ' MOST 3-FGM
                RC$(15, 0) = AA$(I, I1) ' NAME
                RC$(15, 1) = B$(I) ' TEAM
                RC$(15, 2) = VERSUS$(I) ' AT/VS
                RC$(15, 3) = B$(1 - I) ' OPPONENT
                RC$(15, 4) = Date$ ' DATE
            End If
            If W%(I, I1, 0) = 0 And W%(I, I1, 1) > REC(16, 0) Then
                REC(16, 0) = W%(I, I1, 1) ' 0 3FGM /3FGA
                RC$(16, 0) = AA$(I, I1) ' NAME
                RC$(16, 1) = B$(I) ' TEAM
                RC$(16, 2) = VERSUS$(I) ' AT/VS
                RC$(16, 3) = B$(1 - I) ' OPPONENT
                RC$(16, 4) = Date$ ' DATE
            End If

            ' 3-FIELD GOALS MADE IN A GAME, HALF, QUARTER AND OVERTIME PERIOD
            If W%(I, I1, 0) > REC(17, 0) Then
                REC(17, 0) = W%(I, I1, 0) ' 3FGM
                REC(17, 1) = 1 ' TIMES DONE
                REC(17, 2) = 0 ' GAME
                RC$(17, 0) = AA$(I, I1) ' NAME
                RC$(17, 1) = B$(I) ' TEAM
                RC$(17, 2) = VERSUS$(I) ' AT/VS
                RC$(17, 3) = B$(1 - I) ' OPPONENT
                RC$(17, 4) = Date$ ' DATE
            End If
            If QQ(I, 1, I1, 10) > REC(18, 0) Then
                REC(18, 0) = QQ(I, 1, I1, 10) ' 3FGM
                REC(18, 1) = 1 ' TIMES DONE
                REC(18, 2) = 1 ' HALF
                RC$(18, 0) = AA$(I, I1) ' NAME
                RC$(18, 1) = B$(I) ' TEAM
                RC$(18, 2) = VERSUS$(I) ' AT/VS
                RC$(18, 3) = B$(1 - I) ' OPPONENT
                RC$(18, 4) = Date$ ' DATE
            End If
            If QQ(I, 2, I1, 10) > REC(18, 0) Then
                REC(18, 0) = QQ(I, 2, I1, 10) ' 3FGM
                REC(18, 1) = 1 ' TIMES DONE
                REC(18, 2) = 2 ' HALF
                RC$(18, 0) = AA$(I, I1) ' NAME
                RC$(18, 1) = B$(I) ' TEAM
                RC$(18, 2) = VERSUS$(I) ' AT/VS
                RC$(18, 3) = B$(1 - I) ' OPPONENT
                RC$(18, 4) = Date$ ' DATE
            End If

            For I2 = 3 To 8
                If QQ(I, I2, I1, 10) > REC(20, 0) Then
                    REC(20, 0) = QQ(I, I2, I1, 10) ' 3FGM
                    REC(20, 1) = 1 ' TIMES DONE
                    REC(20, 2) = I2 ' QTR
                    RC$(20, 0) = AA$(I, I1) ' NAME
                    RC$(20, 1) = B$(I) ' TEAM
                    RC$(20, 2) = VERSUS$(I) ' AT/VS
                    RC$(20, 3) = B$(1 - I) ' OPPONENT
                    RC$(20, 4) = Date$ ' DATE
                End If
            Next I2

            ' 3-FIELD GOALS ATT IN A GAME, HALF
            If W%(I, I1, 1) > REC(21, 0) Then
                REC(21, 0) = W%(I, I1, 1) ' 3FGA
                REC(21, 1) = 1 ' TIMES DONE
                REC(21, 2) = 0 ' GAME
                RC$(21, 0) = AA$(I, I1) ' NAME
                RC$(21, 1) = B$(I) ' TEAM
                RC$(21, 2) = VERSUS$(I) ' AT/VS
                RC$(21, 3) = B$(1 - I) ' OPPONENT
                RC$(21, 4) = Date$ ' DATE
            End If
            If QQ(I, 1, I1, 11) > REC(22, 0) Then
                REC(22, 0) = QQ(I, 1, I1, 11) ' 3FGA
                REC(22, 1) = 1 ' TIMES DONE
                REC(22, 2) = 1 ' HALF
                RC$(22, 0) = AA$(I, I1) ' NAME
                RC$(22, 1) = B$(I) ' TEAM
                RC$(22, 2) = VERSUS$(I) ' AT/VS
                RC$(22, 3) = B$(1 - I) ' OPPONENT
                RC$(22, 4) = Date$ ' DATE
            End If
            If QQ(I, 2, I1, 11) > REC(22, 0) Then
                REC(22, 0) = QQ(I, 2, I1, 10) ' 3FGA
                REC(22, 1) = 1 ' TIMES DONE
                REC(22, 2) = 2 ' HALF
                RC$(22, 0) = AA$(I, I1) ' NAME
                RC$(22, 1) = B$(I) ' TEAM
                RC$(22, 2) = VERSUS$(I) ' AT/VS
                RC$(22, 3) = B$(1 - I) ' OPPONENT
                RC$(22, 4) = Date$ ' DATE
            End If
            ' FREE THROWS -  MOST MADE NO MISSES, MOST ATT NONE MADE
            If A(I, I1, 9) = A(I, I1, 10) And A(I, I1, 10) > REC(23, 0) Then
                REC(23, 0) = A(I, I1, 10) ' MOST FTM
                RC$(23, 0) = AA$(I, I1) ' NAME
                RC$(23, 1) = B$(I) ' TEAM
                RC$(23, 2) = VERSUS$(I) ' AT/VS
                RC$(23, 3) = B$(1 - I) ' OPPONENT
                RC$(23, 4) = Date$ ' DATE
            End If
            If A(I, I1, 10) = 0 And A(I, I1, 9) > REC(24, 0) Then
                REC(24, 0) = A(I, I1, 9) ' 0 FTM /FTA
                RC$(24, 0) = AA$(I, I1) ' NAME
                RC$(24, 1) = B$(I) ' TEAM
                RC$(24, 2) = VERSUS$(I) ' AT/VS
                RC$(24, 3) = B$(1 - I) ' OPPONENT
                RC$(24, 4) = Date$ ' DATE
            End If

            ' MADE FREE THROWS IN A GAME, HALF, QUARTER
            If A(I, I1, 10) > REC(25, 0) Then
                REC(25, 0) = A(I, I1, 10) ' FTM
                REC(25, 1) = 1 ' TIMES DONE
                REC(25, 2) = 0 ' GAME
                RC$(25, 0) = AA$(I, I1) ' NAME
                RC$(25, 1) = B$(I) ' TEAM
                RC$(25, 2) = VERSUS$(I) ' AT/VS
                RC$(25, 3) = B$(1 - I) ' OPPONENT
                RC$(25, 4) = Date$ ' DATE
            End If
            If QQ(I, 1, I1, 4) > REC(26, 0) Then
                REC(26, 0) = QQ(I, 1, I1, 4) ' FTM
                REC(26, 1) = 1 ' TIMES DONE
                REC(26, 2) = 1 ' HALF
                RC$(26, 0) = AA$(I, I1) ' NAME
                RC$(26, 1) = B$(I) ' TEAM
                RC$(26, 2) = VERSUS$(I) ' AT/VS
                RC$(26, 3) = B$(1 - I) ' OPPONENT
                RC$(26, 4) = Date$ ' DATE
            End If
            If QQ(I, 2, I1, 4) > REC(26, 0) Then
                REC(26, 0) = QQ(I, 2, I1, 4) ' FTM
                REC(26, 1) = 1 ' TIMES DONE
                REC(26, 2) = 2 ' HALF
                RC$(26, 0) = AA$(I, I1) ' NAME
                RC$(26, 1) = B$(I) ' TEAM
                RC$(26, 2) = VERSUS$(I) ' AT/VS
                RC$(26, 3) = B$(1 - I) ' OPPONENT
                RC$(26, 4) = Date$ ' DATE
            End If

            ' FREE THROWS ATTEMPTS IN A GAME, HALF, QUARTER
            If A(I, I1, 9) > REC(28, 0) Then
                REC(28, 0) = A(I, I1, 9) ' FTA
                REC(28, 1) = 1 ' TIMES DONE
                REC(28, 2) = 0 ' GAME
                RC$(28, 0) = AA$(I, I1) ' NAME
                RC$(28, 1) = B$(I) ' TEAM
                RC$(28, 2) = VERSUS$(I) ' AT/VS
                RC$(28, 3) = B$(1 - I) ' OPPONENT
                RC$(28, 4) = Date$ ' DATE
            End If
            If QQ(I, 1, I1, 3) > REC(29, 0) Then
                REC(29, 0) = QQ(I, 1, I1, 3) ' FTA
                REC(29, 1) = 1 ' TIMES DONE
                REC(29, 2) = 1 ' HALF
                RC$(29, 0) = AA$(I, I1) ' NAME
                RC$(29, 1) = B$(I) ' TEAM
                RC$(29, 2) = VERSUS$(I) ' AT/VS
                RC$(29, 3) = B$(1 - I) ' OPPONENT
                RC$(29, 4) = Date$ ' DATE
            End If
            If QQ(I, 2, I1, 3) > REC(29, 0) Then
                REC(29, 0) = QQ(I, 2, I1, 3) ' FTM
                REC(29, 1) = 1 ' TIMES DONE
                REC(29, 2) = 2 ' HALF
                RC$(29, 0) = AA$(I, I1) ' NAME
                RC$(29, 1) = B$(I) ' TEAM
                RC$(29, 2) = VERSUS$(I) ' AT/VS
                RC$(29, 3) = B$(1 - I) ' OPPONENT
                RC$(29, 4) = Date$ ' DATE
            End If

            ' TOTAL REBOUNDS IN A GAME, HALF, QUARTER
            If A(I, I1, 11) > REC(31, 0) Then
                REC(31, 0) = A(I, I1, 11) ' FTA
                REC(31, 1) = 1 ' TIMES DONE
                REC(31, 2) = 0 ' GAME
                RC$(31, 0) = AA$(I, I1) ' NAME
                RC$(31, 1) = B$(I) ' TEAM
                RC$(31, 2) = VERSUS$(I) ' AT/VS
                RC$(31, 3) = B$(1 - I) ' OPPONENT
                RC$(31, 4) = Date$ ' DATE
            End If
            If QQ(I, 1, I1, 5) > REC(32, 0) Then
                REC(32, 0) = QQ(I, 1, I1, 5) ' RB
                REC(32, 1) = 1 ' TIMES DONE
                REC(32, 2) = 1 ' HALF
                RC$(32, 0) = AA$(I, I1) ' NAME
                RC$(32, 1) = B$(I) ' TEAM
                RC$(32, 2) = VERSUS$(I) ' AT/VS
                RC$(32, 3) = B$(1 - I) ' OPPONENT
                RC$(32, 4) = Date$ ' DATE
            End If
            If QQ(I, 2, I1, 5) > REC(32, 0) Then
                REC(32, 0) = QQ(I, 2, I1, 5) ' RB
                REC(32, 1) = 1 ' TIMES DONE
                REC(32, 2) = 2 ' HALF
                RC$(32, 0) = AA$(I, I1) ' NAME
                RC$(32, 1) = B$(I) ' TEAM
                RC$(32, 2) = VERSUS$(I) ' AT/VS
                RC$(32, 3) = B$(1 - I) ' OPPONENT
                RC$(32, 4) = Date$ ' DATE
            End If

            ' TOTAL OFFENSIVE REBOUNDS IN A GAME, HALF, QUARTER
            If A(I, I1, 24) > REC(34, 0) Then
                REC(34, 0) = A(I, I1, 24) ' FTA
                REC(34, 1) = 1 ' TIMES DONE
                REC(34, 2) = 0 ' GAME
                RC$(34, 0) = AA$(I, I1) ' NAME
                RC$(34, 1) = B$(I) ' TEAM
                RC$(34, 2) = VERSUS$(I) ' AT/VS
                RC$(34, 3) = B$(1 - I) ' OPPONENT
                RC$(34, 4) = Date$ ' DATE
            End If
            If QQ(I, 1, I1, 12) > REC(35, 0) Then
                REC(35, 0) = QQ(I, 1, I1, 12) ' RB
                REC(35, 1) = 1 ' TIMES DONE
                REC(35, 2) = 1 ' HALF
                RC$(35, 0) = AA$(I, I1) ' NAME
                RC$(35, 1) = B$(I) ' TEAM
                RC$(35, 2) = VERSUS$(I) ' AT/VS
                RC$(35, 3) = B$(1 - I) ' OPPONENT
                RC$(35, 4) = Date$ ' DATE
            End If
            If QQ(I, 2, I1, 12) > REC(35, 0) Then
                REC(35, 0) = QQ(I, 2, I1, 12) ' RB
                REC(35, 1) = 1 ' TIMES DONE
                REC(35, 2) = 2 ' HALF
                RC$(35, 0) = AA$(I, I1) ' NAME
                RC$(35, 1) = B$(I) ' TEAM
                RC$(35, 2) = VERSUS$(I) ' AT/VS
                RC$(35, 3) = B$(1 - I) ' OPPONENT
                RC$(35, 4) = Date$ ' DATE
            End If

            ' TOTAL DEFENSIVE REBOUNDS IN A GAME, HALF, QUARTER
            If A(I, I1, 11) - A(I, I1, 24) > REC(37, 0) Then
                REC(37, 0) = A(I, I1, 11) - A(I, I1, 24) ' FTA
                REC(37, 1) = 1 ' TIMES DONE
                REC(37, 2) = 0 ' GAME
                RC$(37, 0) = AA$(I, I1) ' NAME
                RC$(37, 1) = B$(I) ' TEAM
                RC$(37, 2) = VERSUS$(I) ' AT/VS
                RC$(37, 3) = B$(1 - I) ' OPPONENT
                RC$(37, 4) = Date$ ' DATE
            End If
            If QQ(I, 1, I1, 5) - QQ(I, 1, I1, 12) > REC(38, 0) Then
                REC(38, 0) = QQ(I, 1, I1, 5) - QQ(I, 1, I1, 12) ' RB
                REC(38, 1) = 1 ' TIMES DONE
                REC(38, 2) = 1 ' HALF
                RC$(38, 0) = AA$(I, I1) ' NAME
                RC$(38, 1) = B$(I) ' TEAM
                RC$(38, 2) = VERSUS$(I) ' AT/VS
                RC$(38, 3) = B$(1 - I) ' OPPONENT
                RC$(38, 4) = Date$ ' DATE
            End If
            If QQ(I, 2, I1, 5) - QQ(I, 2, I1, 12) > REC(38, 0) Then
                REC(38, 0) = QQ(I, 2, I1, 5) - QQ(I, 2, I1, 12) ' RB
                REC(38, 1) = 1 ' TIMES DONE
                REC(38, 2) = 2 ' HALF
                RC$(38, 0) = AA$(I, I1) ' NAME
                RC$(38, 1) = B$(I) ' TEAM
                RC$(38, 2) = VERSUS$(I) ' AT/VS
                RC$(38, 3) = B$(1 - I) ' OPPONENT
                RC$(38, 4) = Date$ ' DATE
            End If

            ' ASSISTS IN A GAME, HALF, QUARTER
            If A(I, I1, 14) > REC(40, 0) Then
                REC(40, 0) = A(I, I1, 14) ' FTA
                REC(40, 1) = 1 ' TIMES DONE
                REC(40, 2) = 0 ' GAME
                RC$(40, 0) = AA$(I, I1) ' NAME
                RC$(40, 1) = B$(I) ' TEAM
                RC$(40, 2) = VERSUS$(I) ' AT/VS
                RC$(40, 3) = B$(1 - I) ' OPPONENT
                RC$(40, 4) = Date$ ' DATE
            End If
            If QQ(I, 1, I1, 8) > REC(41, 0) Then
                REC(41, 0) = QQ(I, 1, I1, 8) ' FTA
                REC(41, 1) = 1 ' TIMES DONE
                REC(41, 2) = 1 ' HALF
                RC$(41, 0) = AA$(I, I1) ' NAME
                RC$(41, 1) = B$(I) ' TEAM
                RC$(41, 2) = VERSUS$(I) ' AT/VS
                RC$(41, 3) = B$(1 - I) ' OPPONENT
                RC$(41, 4) = Date$ ' DATE
            End If
            If QQ(I, 2, I1, 8) > REC(41, 0) Then
                REC(41, 0) = QQ(I, 2, I1, 8) ' FTM
                REC(41, 1) = 1 ' TIMES DONE
                REC(41, 2) = 2 ' HALF
                RC$(41, 0) = AA$(I, I1) ' NAME
                RC$(41, 1) = B$(I) ' TEAM
                RC$(41, 2) = VERSUS$(I) ' AT/VS
                RC$(41, 3) = B$(1 - I) ' OPPONENT
                RC$(41, 4) = Date$ ' DATE
            End If

            ' STEALS IN A GAME, HALF, QUARTER
            If A(I, I1, 13) > REC(43, 0) Then
                REC(43, 0) = A(I, I1, 13) ' FTA
                REC(43, 1) = 1 ' TIMES DONE
                REC(43, 2) = 0 ' GAME
                RC$(43, 0) = AA$(I, I1) ' NAME
                RC$(43, 1) = B$(I) ' TEAM
                RC$(43, 2) = VERSUS$(I) ' AT/VS
                RC$(43, 3) = B$(1 - I) ' OPPONENT
                RC$(43, 4) = Date$ ' DATE
            End If
            If QQ(I, 1, I1, 7) > REC(44, 0) Then
                REC(44, 0) = QQ(I, 1, I1, 7) ' FTA
                REC(44, 1) = 1 ' TIMES DONE
                REC(44, 2) = 1 ' HALF
                RC$(44, 0) = AA$(I, I1) ' NAME
                RC$(44, 1) = B$(I) ' TEAM
                RC$(44, 2) = VERSUS$(I) ' AT/VS
                RC$(44, 3) = B$(1 - I) ' OPPONENT
                RC$(44, 4) = Date$ ' DATE
            End If
            If QQ(I, 2, I1, 7) > REC(44, 0) Then
                REC(44, 0) = QQ(I, 2, I1, 7) ' FTM
                REC(44, 1) = 1 ' TIMES DONE
                REC(44, 2) = 2 ' HALF
                RC$(44, 0) = AA$(I, I1) ' NAME
                RC$(44, 1) = B$(I) ' TEAM
                RC$(44, 2) = VERSUS$(I) ' AT/VS
                RC$(44, 3) = B$(1 - I) ' OPPONENT
                RC$(44, 4) = Date$ ' DATE
            End If

            ' BLOCKS IN A GAME, HALF, QUARTER
            If A(I, I1, 15) > REC(46, 0) Then
                REC(46, 0) = A(I, I1, 15) ' FTA
                REC(46, 1) = 1 ' TIMES DONE
                REC(46, 2) = 0 ' GAME
                RC$(46, 0) = AA$(I, I1) ' NAME
                RC$(46, 1) = B$(I) ' TEAM
                RC$(46, 2) = VERSUS$(I) ' AT/VS
                RC$(46, 3) = B$(1 - I) ' OPPONENT
                RC$(46, 4) = Date$ ' DATE
            End If
            If QQ(I, 1, I1, 9) > REC(47, 0) Then
                REC(47, 0) = QQ(I, 1, I1, 9) ' FTA
                REC(47, 1) = 1 ' TIMES DONE
                REC(47, 2) = 1 ' HALF
                RC$(47, 0) = AA$(I, I1) ' NAME
                RC$(47, 1) = B$(I) ' TEAM
                RC$(47, 2) = VERSUS$(I) ' AT/VS
                RC$(47, 3) = B$(1 - I) ' OPPONENT
                RC$(47, 4) = Date$ ' DATE
            End If
            If QQ(I, 2, I1, 9) > REC(47, 0) Then
                REC(47, 0) = QQ(I, 2, I1, 9) ' FTM
                REC(47, 1) = 1 ' TIMES DONE
                REC(47, 2) = 2 ' HALF
                RC$(47, 0) = AA$(I, I1) ' NAME
                RC$(47, 1) = B$(I) ' TEAM
                RC$(47, 2) = VERSUS$(I) ' AT/VS
                RC$(47, 3) = B$(1 - I) ' OPPONENT
                RC$(47, 4) = Date$ ' DATE
            End If

        Next
    Next

    ' MOST OVERTIME PERIODS
    If H - 3 > TREC(0, 0) Then
        TREC(0, 0) = H - 3
        TREC(0, 1) = S(0, 0)
        TREC(0, 2) = S(1, 0)
        TRC$(0, 0) = B$(0)
        TRC$(0, 1) = B$(1)
        TRC$(0, 2) = Date$
    End If

    For I = 0 To 1

        If S(I, 0) > TREC(1, 0) Then
            TREC(1, 0) = S(I, 0)
            TREC(1, 1) = 1
            TREC(1, 2) = 0
            TRC$(1, 0) = B$(I)
            TRC$(1, 1) = VERSUS$(I)
            TRC$(1, 2) = B$(1 - I)
            TRC$(1, 3) = Date$
        End If

        If S(I, 0) < TREC(2, 0) Then
            TREC(2, 0) = S(I, 0)
            TREC(2, 1) = 1
            TREC(2, 2) = 0
            TRC$(2, 0) = B$(I)
            TRC$(2, 1) = VERSUS$(I)
            TRC$(2, 2) = B$(1 - I)
            TRC$(2, 3) = Date$
        End If

        If S(I, 0) + S(1 - I, 0) > TREC(3, 0) Then
            TREC(3, 0) = S(I, 0) + S(1 - I, 0)
            TREC(3, 1) = S(I, 0)
            TREC(3, 2) = S(1 - I, 0)
            TRC$(3, 0) = B$(I)
            TRC$(3, 1) = VERSUS$(I)
            TRC$(3, 2) = B$(1 - I)
            TRC$(3, 3) = Date$
        End If

        If S(I, 0) + S(1 - I, 0) < TREC(4, 0) Then
            TREC(4, 0) = S(I, 0) + S(1 - I, 0)
            TREC(4, 1) = S(I, 0)
            TREC(4, 2) = S(1 - I, 0)
            TRC$(4, 0) = B$(I)
            TRC$(4, 1) = VERSUS$(I)
            TRC$(4, 2) = B$(1 - I)
            TRC$(4, 3) = Date$
        End If

        If S(I, 1) > TREC(6, 0) Then
            TREC(6, 0) = S(I, 1)
            TREC(6, 1) = 1
            TREC(6, 2) = 0
            TRC$(6, 0) = B$(I)
            TRC$(6, 1) = VERSUS$(I)
            TRC$(6, 2) = B$(1 - I)
            TRC$(6, 3) = Date$
        End If

        If S(I, 1) < TREC(7, 0) Then
            TREC(7, 0) = S(I, 1)
            TREC(7, 1) = 1
            TREC(7, 2) = 0
            TRC$(7, 0) = B$(I)
            TRC$(7, 1) = VERSUS$(I)
            TRC$(7, 2) = B$(1 - I)
            TRC$(7, 3) = Date$
        End If

        If S(I, 1) + S(1 - I, 1) > TREC(8, 0) Then
            TREC(8, 0) = S(I, 1) + S(1 - I, 1)
            TREC(8, 1) = S(I, 1)
            TREC(8, 2) = S(1 - I, 1)
            TRC$(8, 0) = B$(I)
            TRC$(8, 1) = VERSUS$(I)
            TRC$(8, 2) = B$(1 - I)
            TRC$(8, 3) = Date$
        End If

        If S(I, 1) + S(1 - I, 1) < TREC(9, 0) Then
            TREC(9, 0) = S(I, 1) + S(1 - I, 1)
            TREC(9, 1) = S(I, 1)
            TREC(9, 2) = S(1 - I, 1)
            TRC$(9, 0) = B$(I)
            TRC$(9, 1) = VERSUS$(I)
            TRC$(9, 2) = B$(1 - I)
            TRC$(9, 3) = Date$
        End If

        If S(I, 2) > TREC(10, 0) Then
            TREC(10, 0) = S(I, 2)
            TREC(10, 1) = 1
            TREC(10, 2) = 0
            TRC$(10, 0) = B$(I)
            TRC$(10, 1) = VERSUS$(I)
            TRC$(10, 2) = B$(1 - I)
            TRC$(10, 3) = Date$
        End If

        If S(I, 2) < TREC(11, 0) Then
            TREC(11, 0) = S(I, 2)
            TREC(11, 1) = 1
            TREC(11, 2) = 0
            TRC$(11, 0) = B$(I)
            TRC$(11, 1) = VERSUS$(I)
            TRC$(11, 2) = B$(1 - I)
            TRC$(11, 3) = Date$
        End If

        If S(I, 2) + S(1 - I, 2) > TREC(12, 0) Then
            TREC(12, 0) = S(I, 2) + S(1 - I, 2)
            TREC(12, 1) = S(I, 2)
            TREC(12, 2) = S(1 - I, 2)
            TRC$(12, 0) = B$(I)
            TRC$(12, 1) = VERSUS$(I)
            TRC$(12, 2) = B$(1 - I)
            TRC$(12, 3) = Date$
        End If

        If S(I, 2) + S(1 - I, 2) < TREC(13, 0) Then
            TREC(13, 0) = S(I, 2) + S(1 - I, 2)
            TREC(13, 1) = S(I, 2)
            TREC(13, 2) = S(1 - I, 2)
            TRC$(13, 0) = B$(I)
            TRC$(13, 1) = VERSUS$(I)
            TRC$(13, 2) = B$(1 - I)
            TRC$(13, 3) = Date$
        End If

        For I2 = 3 To 8
            If S(I, I2) > TREC(14, 0) Then
                TREC(14, 0) = S(I, I2)
                TREC(14, 1) = I2
                TREC(14, 2) = 0
                TRC$(14, 0) = B$(I)
                TRC$(14, 1) = VERSUS$(I)
                TRC$(14, 2) = B$(1 - I)
                TRC$(14, 3) = Date$
            End If

            If S(I, I2) < TREC(15, 0) Then
                TREC(15, 0) = S(I, I2)
                TREC(15, 1) = I2
                TREC(15, 2) = 0
                TRC$(15, 0) = B$(I)
                TRC$(15, 1) = VERSUS$(I)
                TRC$(15, 2) = B$(1 - I)
                TRC$(15, 3) = Date$
            End If

            If S(I, I2) + S(1 - I, I2) > TREC(16, 0) Then
                TREC(16, 0) = S(I, I2) + S(1 - I, I2)
                TREC(16, 1) = S(I, I2)
                TREC(16, 2) = S(1 - I, I2)
                TRC$(16, 0) = B$(I)
                TRC$(16, 1) = VERSUS$(I)
                TRC$(16, 2) = B$(1 - I)
                TRC$(16, 3) = Date$
            End If

            If S(I, I2) + S(1 - I, I2) < TREC(17, 0) Then
                TREC(17, 0) = S(I, I2) + S(1 - I, I2)
                TREC(17, 1) = S(I, I2)
                TREC(17, 2) = S(1 - I, I2)
                TRC$(17, 0) = B$(I)
                TRC$(17, 1) = VERSUS$(I)
                TRC$(17, 2) = B$(1 - I)
                TRC$(17, 3) = Date$
            End If
        Next I2

        ' TEAM FIELD GOAL PERCENTAGES
        For I1 = 0 To 14: V8(I, I1) = 0: V8(1 - I, I1) = 0: Next
        For I2 = 0 To 1
            For I1 = 0 To 13
                V8(I2, 0) = V8(I2, 0) + A(I2, I1, 8)
                V8(I2, 1) = V8(I2, 1) + A(I2, I1, 7)
                V8(I2, 2) = V8(I2, 2) + A(I2, I1, 10)
                V8(I2, 3) = V8(I2, 3) + A(I2, I1, 9)
                V8(I2, 4) = V8(I2, 4) + A(I2, I1, 11)
                V8(I2, 5) = V8(I2, 5) + (A(I2, I1, 8) * 2) + A(I2, I1, 10) + W%(I2, I1, 0)
                V8(I2, 13) = V8(I2, 13) + W%(I2, I1, 0)
                V8(I2, 14) = V8(I2, 14) + W%(I2, I1, 1)
                V8(I2, 6) = V8(I2, 6) + A(I2, I1, 12)
                V8(I2, 7) = V8(I2, 7) + A(I2, I1, 14)
                V8(I2, 8) = V8(I2, 8) + A(I2, I1, 15)
                V8(I2, 9) = V8(I2, 9) + A(I2, I1, 13)
                V8(I2, 11) = V8(I2, 11) + A(I2, I1, 24)
                V8(I2, 12) = V8(I2, 12) + A(I2, I1, 23)
                V8(I2, 10) = V8(I2, 10) + ZZ(I2, I1)
            Next
        Next

        If V8(I, 1) = 0 Then 101
        If V8(I, 0) / V8(I, 1) > TREC(30, 0) Then
            TREC(30, 0) = V8(I, 0) / V8(I, 1)
            TREC(30, 1) = V8(I, 0)
            TREC(30, 2) = V8(I, 1)
            TRC$(30, 0) = B$(I)
            TRC$(30, 1) = VERSUS$(I)
            TRC$(30, 2) = B$(1 - I)
            TRC$(30, 3) = Date$
        End If

        If V8(I, 0) / V8(I, 1) < TREC(31, 0) Then
            TREC(31, 0) = V8(I, 0) / V8(I, 1)
            TREC(31, 1) = V8(I, 0)
            TREC(31, 2) = V8(I, 1)
            TRC$(31, 0) = B$(I)
            TRC$(31, 1) = VERSUS$(I)
            TRC$(31, 2) = B$(1 - I)
            TRC$(31, 3) = Date$
        End If

        101 If V8(I, 1) + V8(1 - I, 1) = 0 Then 102
        If (V8(I, 0) + V8(1 - I, 0)) / (V8(I, 1) + V8(1 - I, 1)) > TREC(32, 0) Then
            TREC(32, 0) = (V8(I, 0) + V8(1 - I, 0)) / (V8(I, 1) + V8(1 - I, 1))
            TREC(32, 1) = V8(I, 0) / V8(I, 1)
            TREC(32, 2) = V8(1 - I, 0) / (V8(1 - I, 1) + 0.01)
            TRC$(32, 0) = B$(I)
            TRC$(32, 1) = VERSUS$(I)
            TRC$(32, 2) = B$(1 - I)
            TRC$(32, 3) = Date$
        End If

        If (V8(I, 0) + V8(1 - I, 0)) / (V8(I, 1) + V8(1 - I, 1)) < TREC(33, 0) Then
            TREC(33, 0) = (V8(I, 0) + V8(1 - I, 0)) / (V8(I, 1) + V8(1 - I, 1))
            TREC(33, 1) = V8(I, 0) / V8(I, 1)
            TREC(33, 2) = V8(1 - I, 0) / (V8(1 - I, 1) + 0.01)
            TRC$(33, 0) = B$(I)
            TRC$(33, 1) = VERSUS$(I)
            TRC$(33, 2) = B$(1 - I)
            TRC$(33, 3) = Date$
        End If

        102 ' TEAM FIELD GOALS MADE
        If V8(I, 0) > TREC(34, 0) Then
            TREC(34, 0) = V8(I, 0)
            TREC(34, 1) = 0
            TREC(34, 2) = 0
            TRC$(34, 0) = B$(I)
            TRC$(34, 1) = VERSUS$(I)
            TRC$(34, 2) = B$(1 - I)
            TRC$(34, 3) = Date$
        End If

        If V8(I, 0) < TREC(35, 0) Then
            TREC(35, 0) = V8(I, 0)
            TREC(35, 1) = 0
            TREC(35, 2) = 0
            TRC$(35, 0) = B$(I)
            TRC$(35, 1) = VERSUS$(I)
            TRC$(35, 2) = B$(1 - I)
            TRC$(35, 3) = Date$
        End If

        If V8(I, 0) + V8(1 - I, 0) > TREC(36, 0) Then
            TREC(36, 0) = V8(I, 0) + V8(1 - I, 0)
            TREC(36, 1) = V8(I, 0)
            TREC(36, 2) = V8(1 - I, 0)
            TRC$(36, 0) = B$(I)
            TRC$(36, 1) = VERSUS$(I)
            TRC$(36, 2) = B$(1 - I)
            TRC$(36, 3) = Date$
        End If

        If V8(I, 0) + V8(1 - I, 0) < TREC(37, 0) Then
            TREC(37, 0) = V8(I, 0) + V8(1 - I, 0)
            TREC(37, 1) = V8(I, 0)
            TREC(37, 2) = V8(1 - I, 0)
            TRC$(37, 0) = B$(I)
            TRC$(37, 1) = VERSUS$(I)
            TRC$(37, 2) = B$(1 - I)
            TRC$(37, 3) = Date$
        End If

        If QR(I, 1, 0) > TREC(38, 0) Then
            TREC(38, 0) = QR(I, 1, 0)
            TREC(38, 1) = 1
            TREC(38, 2) = 0
            TRC$(38, 0) = B$(I)
            TRC$(38, 1) = VERSUS$(I)
            TRC$(38, 2) = B$(1 - I)
            TRC$(38, 3) = Date$
        End If

        If QR(I, 2, 0) > TREC(38, 0) Then
            TREC(38, 0) = QR(I, 2, 0)
            TREC(38, 1) = 2
            TREC(38, 2) = 0
            TRC$(38, 0) = B$(I)
            TRC$(38, 1) = VERSUS$(I)
            TRC$(38, 2) = B$(1 - I)
            TRC$(38, 3) = Date$
        End If

        If QR(I, 1, 0) + QR(1 - I, 1, 0) > TREC(39, 0) Then
            TREC(39, 0) = QR(I, 1, 0) + QR(1 - I, 1, 0)
            TREC(39, 1) = 1
            TREC(39, 2) = 0
            TRC$(39, 0) = B$(I)
            TRC$(39, 1) = VERSUS$(I)
            TRC$(39, 2) = B$(1 - I)
            TRC$(39, 3) = Date$
        End If

        If QR(I, 2, 0) + QR(1 - I, 2, 0) > TREC(39, 0) Then
            TREC(39, 0) = QR(I, 2, 0) + QR(1 - I, 2, 0)
            TREC(39, 1) = 2
            TREC(39, 2) = 0
            TRC$(39, 0) = B$(I)
            TRC$(39, 1) = VERSUS$(I)
            TRC$(39, 2) = B$(1 - I)
            TRC$(39, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS
        If V8(I, 1) > TREC(42, 0) Then
            TREC(42, 0) = V8(I, 1)
            TREC(42, 1) = 0
            TREC(42, 2) = 0
            TRC$(42, 0) = B$(I)
            TRC$(42, 1) = VERSUS$(I)
            TRC$(42, 2) = B$(1 - I)
            TRC$(42, 3) = Date$
        End If

        If V8(I, 1) < TREC(43, 0) Then
            TREC(43, 0) = V8(I, 1)
            TREC(43, 1) = 0
            TREC(43, 2) = 0
            TRC$(43, 0) = B$(I)
            TRC$(43, 1) = VERSUS$(I)
            TRC$(43, 2) = B$(1 - I)
            TRC$(43, 3) = Date$
        End If

        If V8(I, 1) + V8(1 - I, 1) > TREC(44, 0) Then
            TREC(44, 0) = V8(I, 1) + V8(1 - I, 1)
            TREC(44, 1) = V8(I, 1)
            TREC(44, 2) = V8(1 - I, 1)
            TRC$(44, 0) = B$(I)
            TRC$(44, 1) = VERSUS$(I)
            TRC$(44, 2) = B$(1 - I)
            TRC$(44, 3) = Date$
        End If

        If V8(I, 1) + V8(1 - I, 1) < TREC(45, 0) Then
            TREC(45, 0) = V8(I, 1) + V8(1 - I, 1)
            TREC(45, 1) = V8(I, 1)
            TREC(45, 2) = V8(1 - I, 1)
            TRC$(45, 0) = B$(I)
            TRC$(45, 1) = VERSUS$(I)
            TRC$(45, 2) = B$(1 - I)
            TRC$(45, 3) = Date$
        End If

        If QR(I, 1, 1) > TREC(46, 0) Then
            TREC(46, 0) = QR(I, 1, 1)
            TREC(46, 1) = 1
            TREC(46, 2) = 0
            TRC$(46, 0) = B$(I)
            TRC$(46, 1) = VERSUS$(I)
            TRC$(46, 2) = B$(1 - I)
            TRC$(46, 3) = Date$
        End If

        If QR(I, 2, 1) > TREC(46, 0) Then
            TREC(46, 0) = QR(I, 2, 1)
            TREC(46, 1) = 2
            TREC(46, 2) = 0
            TRC$(46, 0) = B$(I)
            TRC$(46, 1) = VERSUS$(I)
            TRC$(46, 2) = B$(1 - I)
            TRC$(46, 3) = Date$
        End If

        If QR(I, 1, 1) + QR(1 - I, 1, 1) > TREC(47, 0) Then
            TREC(47, 0) = QR(I, 1, 1) + QR(1 - I, 1, 1)
            TREC(47, 1) = 1
            TREC(47, 2) = QR(I, 1, 1)
            TRC$(47, 0) = B$(I)
            TRC$(47, 1) = VERSUS$(I)
            TRC$(47, 2) = B$(1 - I)
            TRC$(47, 3) = Date$
        End If

        If QR(I, 2, 1) + QR(1 - I, 2, 1) > TREC(47, 0) Then
            TREC(47, 0) = QR(I, 2, 1) + QR(1 - I, 2, 1)
            TREC(47, 1) = 2
            TREC(47, 2) = QR(I, 2, 1)
            TRC$(47, 0) = B$(I)
            TRC$(47, 1) = VERSUS$(I)
            TRC$(47, 2) = B$(1 - I)
            TRC$(47, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS
        If V8(I, 13) = V8(I, 14) And V8(I, 14) > TREC(50, 0) Then
            TREC(50, 0) = V8(I, 13)
            TREC(50, 1) = 0
            TREC(50, 2) = 0
            TRC$(50, 0) = B$(I)
            TRC$(50, 1) = VERSUS$(I)
            TRC$(50, 2) = B$(1 - I)
            TRC$(50, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS MOST MADE
        If V8(I, 13) = V8(I, 14) And V8(1 - I, 13) = V8(1 - I, 14) And V8(I, 14) + V8(1 - I, 14) > TREC(51, 0) Then
            TREC(51, 0) = V8(I, 13)
            TREC(51, 1) = V8(1 - I, 13)
            TREC(51, 2) = 0
            TRC$(51, 0) = B$(I)
            TRC$(51, 1) = VERSUS$(I)
            TRC$(51, 2) = B$(1 - I)
            TRC$(51, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS NONE MADE
        If V8(I, 13) = 0 And V8(I, 14) > TREC(52, 0) Then
            TREC(52, 0) = V8(I, 14)
            TREC(52, 1) = 0
            TREC(52, 2) = 0
            TRC$(52, 0) = B$(I)
            TRC$(52, 1) = VERSUS$(I)
            TRC$(52, 2) = B$(1 - I)
            TRC$(52, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS NONE MADE
        If V8(I, 13) > TREC(53, 0) Then
            TREC(53, 0) = V8(I, 13)
            TREC(53, 1) = 0
            TREC(53, 2) = 0
            TRC$(53, 0) = B$(I)
            TRC$(53, 1) = VERSUS$(I)
            TRC$(53, 2) = B$(1 - I)
            TRC$(53, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS NONE MADE
        If V8(I, 13) + V8(1 - I, 13) > TREC(54, 0) Then
            TREC(54, 0) = V8(I, 13) + V8(1 - I, 13)
            TREC(54, 1) = V8(I, 13)
            TREC(54, 2) = V8(1 - I, 13)
            TRC$(54, 0) = B$(I)
            TRC$(54, 1) = VERSUS$(I)
            TRC$(54, 2) = B$(1 - I)
            TRC$(54, 3) = Date$
        End If

        If QR(I, 1, 13) > TREC(55, 0) Then
            TREC(55, 0) = QR(I, 1, 13)
            TREC(55, 1) = 1
            TREC(55, 2) = 0
            TRC$(55, 0) = B$(I)
            TRC$(55, 1) = VERSUS$(I)
            TRC$(55, 2) = B$(1 - I)
            TRC$(55, 3) = Date$
        End If

        If QR(I, 2, 13) > TREC(55, 0) Then
            TREC(55, 0) = QR(I, 2, 13)
            TREC(55, 1) = 2
            TREC(55, 2) = 0
            TRC$(55, 0) = B$(I)
            TRC$(55, 1) = VERSUS$(I)
            TRC$(55, 2) = B$(1 - I)
            TRC$(55, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS NONE MADE
        If V8(I, 14) > TREC(57, 0) Then
            TREC(57, 0) = V8(I, 14)
            TREC(57, 1) = 0
            TREC(57, 2) = 0
            TRC$(57, 0) = B$(I)
            TRC$(57, 1) = VERSUS$(I)
            TRC$(57, 2) = B$(1 - I)
            TRC$(57, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS NONE MADE
        If V8(I, 14) + V8(1 - I, 14) > TREC(58, 0) Then
            TREC(58, 0) = V8(I, 14) + V8(1 - I, 14)
            TREC(58, 1) = V8(I, 14)
            TREC(58, 2) = V8(1 - I, 14)
            TRC$(58, 0) = B$(I)
            TRC$(58, 1) = VERSUS$(I)
            TRC$(58, 2) = B$(1 - I)
            TRC$(58, 3) = Date$
        End If

        If QR(I, 1, 14) > TREC(59, 0) Then
            TREC(59, 0) = QR(I, 1, 14)
            TREC(59, 1) = 1
            TREC(59, 2) = 0
            TRC$(59, 0) = B$(I)
            TRC$(59, 1) = VERSUS$(I)
            TRC$(59, 2) = B$(1 - I)
            TRC$(59, 3) = Date$
        End If

        If QR(I, 2, 14) > TREC(59, 0) Then
            TREC(59, 0) = QR(I, 2, 14)
            TREC(59, 1) = 2
            TREC(59, 2) = 0
            TRC$(59, 0) = B$(I)
            TRC$(59, 1) = VERSUS$(I)
            TRC$(59, 2) = B$(1 - I)
            TRC$(59, 3) = Date$
        End If

        ' FREE THROW ATTEMPTS NO MISS
        If V8(I, 2) = V8(I, 3) And V8(I, 2) > TREC(60, 0) Then
            TREC(60, 0) = V8(I, 2)
            TREC(60, 1) = 0
            TREC(60, 2) = 0
            TRC$(60, 0) = B$(I)
            TRC$(60, 1) = VERSUS$(I)
            TRC$(60, 2) = B$(1 - I)
            TRC$(60, 3) = Date$
        End If

        ' FREE THROW ATTEMPTS LOW %
        If V8(I, 3) = 0 Then 103
        If V8(I, 2) / V8(I, 3) < TREC(61, 0) Then
            TREC(61, 0) = V8(I, 2) / V8(I, 3)
            TREC(61, 1) = V8(I, 2)
            TREC(61, 2) = V8(I, 3)
            TRC$(61, 0) = B$(I)
            TRC$(61, 1) = VERSUS$(I)
            TRC$(61, 2) = B$(1 - I)
            TRC$(61, 3) = Date$
        End If

        103 ' FREE THROW ATTEMPTS COMBINED HIGH %
        If V8(I, 3) + V8(1 - I, 3) = 0 Then 104
        If (V8(I, 2) + V8(1 - I, 2)) / (V8(I, 3) + V8(1 - I, 3)) > TREC(62, 0) Then
            TREC(62, 0) = (V8(I, 2) + V8(1 - I, 2)) / (V8(I, 3) + V8(1 - I, 3))
            TREC(62, 1) = V8(I, 2) / V8(I, 3)
            TREC(62, 2) = V8(1 - I, 2) / (V8(1 - I, 3) + 0.01)
            TRC$(62, 0) = B$(I)
            TRC$(62, 1) = VERSUS$(I)
            TRC$(62, 2) = B$(1 - I)
            TRC$(62, 3) = Date$
        End If

        ' FREE THROW ATTEMPTS COMBINED LOW %
        If (V8(I, 2) + V8(1 - I, 2)) / (V8(I, 3) + V8(1 - I, 3)) < TREC(63, 0) Then
            TREC(63, 0) = (V8(I, 2) + V8(1 - I, 2)) / (V8(I, 3) + V8(1 - I, 3))
            TREC(63, 1) = V8(I, 2) / V8(I, 3)
            TREC(63, 2) = V8(1 - I, 2) / (V8(1 - I, 3) + 0.01)
            TRC$(63, 0) = B$(I)
            TRC$(63, 1) = VERSUS$(I)
            TRC$(63, 2) = B$(1 - I)
            TRC$(63, 3) = Date$
        End If

        ' TEAM FREE THROWS MADE
        104 If V8(I, 2) > TREC(64, 0) Then
            TREC(64, 0) = V8(I, 2)
            TREC(64, 1) = 0
            TREC(64, 2) = 0
            TRC$(64, 0) = B$(I)
            TRC$(64, 1) = VERSUS$(I)
            TRC$(64, 2) = B$(1 - I)
            TRC$(64, 3) = Date$
        End If

        If V8(I, 2) < TREC(65, 0) Then
            TREC(65, 0) = V8(I, 2)
            TREC(65, 1) = 0
            TREC(65, 2) = 0
            TRC$(65, 0) = B$(I)
            TRC$(65, 1) = VERSUS$(I)
            TRC$(65, 2) = B$(1 - I)
            TRC$(65, 3) = Date$
        End If

        If V8(I, 2) + V8(1 - I, 2) > TREC(66, 0) Then
            TREC(66, 0) = V8(I, 2) + V8(1 - I, 2)
            TREC(66, 1) = V8(I, 2)
            TREC(66, 2) = V8(1 - I, 2)
            TRC$(66, 0) = B$(I)
            TRC$(66, 1) = VERSUS$(I)
            TRC$(66, 2) = B$(1 - I)
            TRC$(66, 3) = Date$
        End If

        If V8(I, 2) + V8(1 - I, 2) < TREC(67, 0) Then
            TREC(67, 0) = V8(I, 2) + V8(1 - I, 2)
            TREC(67, 1) = V8(I, 2)
            TREC(67, 2) = V8(1 - I, 2)
            TRC$(67, 0) = B$(I)
            TRC$(67, 1) = VERSUS$(I)
            TRC$(67, 2) = B$(1 - I)
            TRC$(67, 3) = Date$
        End If

        If QR(I, 1, 2) > TREC(68, 0) Then
            TREC(68, 0) = QR(I, 1, 2)
            TREC(68, 1) = 1
            TREC(68, 2) = 0
            TRC$(68, 0) = B$(I)
            TRC$(68, 1) = VERSUS$(I)
            TRC$(68, 2) = B$(1 - I)
            TRC$(68, 3) = Date$
        End If

        If QR(I, 2, 2) > TREC(68, 0) Then
            TREC(68, 0) = QR(I, 2, 2)
            TREC(68, 1) = 2
            TREC(68, 2) = 0
            TRC$(68, 0) = B$(I)
            TRC$(68, 1) = VERSUS$(I)
            TRC$(68, 2) = B$(1 - I)
            TRC$(68, 3) = Date$
        End If

        ' TEAM FREE THROWS ATTEMPTED
        If V8(I, 3) > TREC(72, 0) Then
            TREC(72, 0) = V8(I, 3)
            TREC(72, 1) = 0
            TREC(72, 2) = 0
            TRC$(72, 0) = B$(I)
            TRC$(72, 1) = VERSUS$(I)
            TRC$(72, 2) = B$(1 - I)
            TRC$(72, 3) = Date$
        End If

        If V8(I, 3) < TREC(73, 0) Then
            TREC(73, 0) = V8(I, 3)
            TREC(73, 1) = 0
            TREC(73, 2) = 0
            TRC$(73, 0) = B$(I)
            TRC$(73, 1) = VERSUS$(I)
            TRC$(73, 2) = B$(1 - I)
            TRC$(73, 3) = Date$
        End If

        If V8(I, 3) + V8(1 - I, 3) > TREC(74, 0) Then
            TREC(74, 0) = V8(I, 3) + V8(1 - I, 3)
            TREC(74, 1) = V8(I, 3)
            TREC(74, 2) = V8(1 - I, 3)
            TRC$(74, 0) = B$(I)
            TRC$(74, 1) = VERSUS$(I)
            TRC$(74, 2) = B$(1 - I)
            TRC$(74, 3) = Date$
        End If

        If V8(I, 3) + V8(1 - I, 3) < TREC(75, 0) Then
            TREC(75, 0) = V8(I, 3) + V8(1 - I, 3)
            TREC(75, 1) = V8(I, 3)
            TREC(75, 2) = V8(1 - I, 3)
            TRC$(75, 0) = B$(I)
            TRC$(75, 1) = VERSUS$(I)
            TRC$(75, 2) = B$(1 - I)
            TRC$(75, 3) = Date$
        End If

        If QR(I, 1, 3) > TREC(76, 0) Then
            TREC(76, 0) = QR(I, 1, 3)
            TREC(76, 1) = 1
            TREC(76, 2) = 0
            TRC$(76, 0) = B$(I)
            TRC$(76, 1) = VERSUS$(I)
            TRC$(76, 2) = B$(1 - I)
            TRC$(76, 3) = Date$
        End If

        If QR(I, 2, 3) > TREC(76, 0) Then
            TREC(76, 0) = QR(I, 2, 3)
            TREC(76, 1) = 2
            TREC(76, 2) = 0
            TRC$(76, 0) = B$(I)
            TRC$(76, 1) = VERSUS$(I)
            TRC$(76, 2) = B$(1 - I)
            TRC$(76, 3) = Date$
        End If

        If QR(I, 1, 3) + QR(1 - I, 1, 3) > TREC(77, 0) Then
            TREC(77, 0) = QR(I, 1, 3) + QR(1 - I, 1, 3)
            TREC(77, 1) = 1
            TREC(77, 2) = QR(I, 1, 3)
            TRC$(77, 0) = B$(I)
            TRC$(77, 1) = VERSUS$(I)
            TRC$(77, 2) = B$(1 - I)
            TRC$(77, 3) = Date$
        End If

        If QR(I, 2, 3) + QR(1 - I, 2, 3) > TREC(77, 0) Then
            TREC(77, 0) = QR(I, 2, 3) + QR(1 - I, 2, 3)
            TREC(77, 1) = 2
            TREC(77, 2) = QR(I, 2, 3)
            TRC$(77, 0) = B$(I)
            TRC$(77, 1) = VERSUS$(I)
            TRC$(77, 2) = B$(1 - I)
            TRC$(77, 3) = Date$
        End If

        If R3(I) > TREC(80, 0) Then
            TREC(80, 0) = R3(I)
            TREC(80, 1) = 0
            TREC(80, 2) = 0
            TRC$(80, 0) = B$(I)
            TRC$(80, 1) = VERSUS$(I)
            TRC$(80, 2) = B$(1 - I)
            TRC$(80, 3) = Date$
        End If

        If R3(I) < TREC(81, 0) Then
            TREC(81, 0) = R3(I)
            TREC(81, 1) = 0
            TREC(81, 2) = 0
            TRC$(81, 0) = B$(I)
            TRC$(81, 1) = VERSUS$(I)
            TRC$(81, 2) = B$(1 - I)
            TRC$(81, 3) = Date$
        End If

        If R3(I) + R3(1 - I) > TREC(82, 0) Then
            TREC(82, 0) = R3(I) + R3(1 - I)
            TREC(82, 1) = R3(I)
            TREC(82, 2) = R3(1 - I)
            TRC$(82, 0) = B$(I)
            TRC$(82, 1) = VERSUS$(I)
            TRC$(82, 2) = B$(1 - I)
            TRC$(82, 3) = Date$
        End If

        If R3(I) + R3(1 - I) < TREC(83, 0) Then
            TREC(83, 0) = R3(I) + R3(1 - I)
            TREC(83, 1) = R3(I)
            TREC(83, 2) = R3(1 - I)
            TRC$(83, 0) = B$(I)
            TRC$(83, 1) = VERSUS$(I)
            TRC$(83, 2) = B$(1 - I)
            TRC$(83, 3) = Date$
        End If

        If QR(I, 1, 4) > TREC(84, 0) Then
            TREC(84, 0) = QR(I, 1, 4)
            TREC(84, 1) = 1
            TREC(84, 2) = 0
            TRC$(84, 0) = B$(I)
            TRC$(84, 1) = VERSUS$(I)
            TRC$(84, 2) = B$(1 - I)
            TRC$(84, 3) = Date$
        End If

        If QR(I, 2, 4) > TREC(84, 0) Then
            TREC(84, 0) = QR(I, 2, 4)
            TREC(84, 1) = 2
            TREC(84, 2) = 0
            TRC$(84, 0) = B$(I)
            TRC$(84, 1) = VERSUS$(I)
            TRC$(84, 2) = B$(1 - I)
            TRC$(84, 3) = Date$
        End If

        If V8(I, 11) > TREC(86, 0) Then
            TREC(86, 0) = V8(I, 11)
            TREC(86, 1) = 0
            TREC(86, 2) = 0
            TRC$(86, 0) = B$(I)
            TRC$(86, 1) = VERSUS$(I)
            TRC$(86, 2) = B$(1 - I)
            TRC$(86, 3) = Date$
        End If

        If V8(I, 11) < TREC(87, 0) Then
            TREC(87, 0) = V8(I, 11)
            TREC(87, 1) = 0
            TREC(87, 2) = 0
            TRC$(87, 0) = B$(I)
            TRC$(87, 1) = VERSUS$(I)
            TRC$(87, 2) = B$(1 - I)
            TRC$(87, 3) = Date$
        End If

        If V8(I, 11) + V8(1 - I, 11) > TREC(88, 0) Then
            TREC(88, 0) = V8(I, 11) + V8(1 - I, 11)
            TREC(88, 1) = V8(I, 11)
            TREC(88, 2) = V8(1 - I, 11)
            TRC$(88, 0) = B$(I)
            TRC$(88, 1) = VERSUS$(I)
            TRC$(88, 2) = B$(1 - I)
            TRC$(88, 3) = Date$
        End If

        If V8(I, 11) + V8(1 - I, 11) < TREC(89, 0) Then
            TREC(89, 0) = V8(I, 11) + V8(1 - I, 11)
            TREC(89, 1) = V8(I, 11)
            TREC(89, 2) = V8(1 - I, 11)
            TRC$(89, 0) = B$(I)
            TRC$(89, 1) = VERSUS$(I)
            TRC$(89, 2) = B$(1 - I)
            TRC$(89, 3) = Date$
        End If

        If QR(I, 1, 11) > TREC(90, 0) Then
            TREC(90, 0) = QR(I, 1, 11)
            TREC(90, 1) = 1
            TREC(90, 2) = 0
            TRC$(90, 0) = B$(I)
            TRC$(90, 1) = VERSUS$(I)
            TRC$(90, 2) = B$(1 - I)
            TRC$(90, 3) = Date$
        End If

        If QR(I, 2, 11) > TREC(90, 0) Then
            TREC(90, 0) = QR(I, 2, 11)
            TREC(90, 1) = 2
            TREC(90, 2) = 0
            TRC$(90, 0) = B$(I)
            TRC$(90, 1) = VERSUS$(I)
            TRC$(90, 2) = B$(1 - I)
            TRC$(90, 3) = Date$
        End If

        ' ASSISTS
        If V8(I, 7) > TREC(96, 0) Then
            TREC(96, 0) = V8(I, 7)
            TREC(96, 1) = 0
            TREC(96, 2) = 0
            TRC$(96, 0) = B$(I)
            TRC$(96, 1) = VERSUS$(I)
            TRC$(96, 2) = B$(1 - I)
            TRC$(96, 3) = Date$
        End If

        If V8(I, 7) < TREC(97, 0) Then
            TREC(97, 0) = V8(I, 7)
            TREC(97, 1) = 0
            TREC(97, 2) = 0
            TRC$(97, 0) = B$(I)
            TRC$(97, 1) = VERSUS$(I)
            TRC$(97, 2) = B$(1 - I)
            TRC$(97, 3) = Date$
        1 End If

        If V8(I, 7) + V8(1 - I, 7) > TREC(98, 0) Then
            TREC(98, 0) = V8(I, 7) + V8(1 - I, 7)
            TREC(98, 1) = V8(I, 7)
            TREC(98, 2) = V8(1 - I, 7)
            TRC$(98, 0) = B$(I)
            TRC$(98, 1) = VERSUS$(I)
            TRC$(98, 2) = B$(1 - I)
            TRC$(98, 3) = Date$
        2 End If

        If V8(I, 7) + V8(1 - I, 7) < TREC(99, 0) Then
            TREC(99, 0) = V8(I, 7) + V8(1 - I, 7)
            TREC(99, 1) = V8(I, 7)
            TREC(99, 2) = V8(1 - I, 7)
            TRC$(99, 0) = B$(I)
            TRC$(99, 1) = VERSUS$(I)
            TRC$(99, 2) = B$(1 - I)
            TRC$(99, 3) = Date$
        3 End If

        If QR(I, 1, 7) > TREC(100, 0) Then
            TREC(100, 0) = QR(I, 1, 7)
            TREC(100, 1) = 1
            TREC(100, 2) = 0
            TRC$(100, 0) = B$(I)
            TRC$(100, 1) = VERSUS$(I)
            TRC$(100, 2) = B$(1 - I)
            TRC$(100, 3) = Date$
        4 End If

        If QR(I, 2, 7) > TREC(100, 0) Then
            TREC(100, 0) = QR(I, 2, 7)
            TREC(100, 1) = 2
            TREC(100, 2) = 0
            TRC$(100, 0) = B$(I)
            TRC$(100, 1) = VERSUS$(I)
            TRC$(100, 2) = B$(1 - I)
            TRC$(100, 3) = Date$
        End If

        If QR(I, 2, 7) + QR(1 - I, 2, 7) > TREC(101, 0) Then
            TREC(101, 0) = QR(I, 2, 7) + QR(1 - I, 2, 7)
            TREC(101, 1) = 2
            TREC(101, 2) = 0
            TRC$(101, 0) = B$(I)
            TRC$(101, 1) = VERSUS$(I)
            TRC$(101, 2) = B$(1 - I)
            TRC$(101, 3) = Date$
        5 End If

        ' FOULS
        If V8(I, 6) > TREC(104, 0) Then
            TREC(104, 0) = V8(I, 6)
            TREC(104, 1) = 0
            TREC(104, 2) = 0
            TRC$(104, 0) = B$(I)
            TRC$(104, 1) = VERSUS$(I)
            TRC$(104, 2) = B$(1 - I)
            TRC$(104, 3) = Date$
        10 End If

        If V8(I, 6) < TREC(105, 0) Then
            TREC(105, 0) = V8(I, 6)
            TREC(105, 1) = 0
            TREC(105, 2) = 0
            TRC$(105, 0) = B$(I)
            TRC$(105, 1) = VERSUS$(I)
            TRC$(105, 2) = B$(1 - I)
            TRC$(105, 3) = Date$
        11 End If

        If V8(I, 6) + V8(1 - I, 6) > TREC(106, 0) Then
            TREC(106, 0) = V8(I, 6) + V8(1 - I, 6)
            TREC(106, 1) = V8(I, 6)
            TREC(106, 2) = V8(1 - I, 6)
            TRC$(106, 0) = B$(I)
            TRC$(106, 1) = VERSUS$(I)
            TRC$(106, 2) = B$(1 - I)
            TRC$(106, 3) = Date$
        12 End If

        If V8(I, 6) + V8(1 - I, 6) < TREC(107, 0) Then
            TREC(107, 0) = V8(I, 6) + V8(1 - I, 6)
            TREC(107, 1) = V8(I, 6)
            TREC(107, 2) = V8(1 - I, 6)
            TRC$(107, 0) = B$(I)
            TRC$(107, 1) = VERSUS$(I)
            TRC$(107, 2) = B$(1 - I)
            TRC$(107, 3) = Date$
        13 End If

        If QR(I, 1, 6) > TREC(108, 0) Then
            TREC(108, 0) = QR(I, 1, 6)
            TREC(108, 1) = 1
            TREC(108, 2) = 0
            TRC$(108, 0) = B$(I)
            TRC$(108, 1) = VERSUS$(I)
            TRC$(108, 2) = B$(1 - I)
            TRC$(108, 3) = Date$
        14 End If

        If QR(I, 2, 6) > TREC(108, 0) Then
            TREC(108, 0) = QR(I, 2, 6)
            TREC(108, 1) = 2
            TREC(108, 2) = 0
            TRC$(108, 0) = B$(I)
            TRC$(108, 1) = VERSUS$(I)
            TRC$(108, 2) = B$(1 - I)
            TRC$(108, 3) = Date$
        15 End If

        If QR(I, 1, 6) + QR(1 - I, 1, 6) > TREC(109, 0) Then
            TREC(109, 0) = QR(I, 1, 6) + QR(1 - I, 1, 6)
            TREC(109, 1) = 1
            TREC(109, 2) = QR(I, 1, 6)
            TRC$(109, 0) = B$(I)
            TRC$(109, 1) = VERSUS$(I)
            TRC$(109, 2) = B$(1 - I)
            TRC$(109, 3) = Date$
        16 End If

        If QR(I, 2, 6) + QR(1 - I, 2, 6) > TREC(109, 0) Then
            TREC(109, 0) = QR(I, 2, 6) + QR(1 - I, 2, 6)
            TREC(109, 1) = 2
            TREC(109, 2) = QR(I, 2, 6)
            TRC$(109, 0) = B$(I)
            TRC$(109, 1) = VERSUS$(I)
            TRC$(109, 2) = B$(1 - I)
            TRC$(109, 3) = Date$
        17 End If

        ' STEALS
        If V8(I, 9) > TREC(112, 0) Then
            TREC(112, 0) = V8(I, 9)
            TREC(112, 1) = 0
            TREC(112, 2) = 0
            TRC$(112, 0) = B$(I)
            TRC$(112, 1) = VERSUS$(I)
            TRC$(112, 2) = B$(1 - I)
            TRC$(112, 3) = Date$
        20 End If

        If V8(I, 9) + V8(1 - I, 9) > TREC(114, 0) Then
            TREC(114, 0) = V8(I, 9) + V8(1 - I, 9)
            TREC(114, 1) = V8(I, 9)
            TREC(114, 2) = V8(1 - I, 9)
            TRC$(114, 0) = B$(I)
            TRC$(114, 1) = VERSUS$(I)
            TRC$(114, 2) = B$(1 - I)
            TRC$(114, 3) = Date$
        21 End If

        If QR(I, 1, 9) > TREC(116, 0) Then
            TREC(116, 0) = QR(I, 1, 9)
            TREC(116, 1) = 1
            TREC(116, 2) = 0
            TRC$(116, 0) = B$(I)
            TRC$(116, 1) = VERSUS$(I)
            TRC$(116, 2) = B$(1 - I)
            TRC$(116, 3) = Date$
        End If

        If QR(I, 2, 9) > TREC(116, 0) Then
            TREC(116, 0) = QR(I, 2, 9)
            TREC(116, 1) = 2
            TREC(116, 2) = 0
            TRC$(116, 0) = B$(I)
            TRC$(116, 1) = VERSUS$(I)
            TRC$(116, 2) = B$(1 - I)
            TRC$(116, 3) = Date$
        23 End If

        ' BLOCKS
        If V8(I, 8) > TREC(118, 0) Then
            TREC(118, 0) = V8(I, 8)
            TREC(118, 1) = 0
            TREC(118, 2) = 0
            TRC$(118, 0) = B$(I)
            TRC$(118, 1) = VERSUS$(I)
            TRC$(118, 2) = B$(1 - I)
            TRC$(118, 3) = Date$
        25 End If

        If V8(I, 8) + V8(1 - I, 8) > TREC(119, 0) Then
            TREC(119, 0) = V8(I, 8) + V8(1 - I, 8)
            TREC(119, 1) = V8(I, 8)
            TREC(119, 2) = V8(1 - I, 8)
            TRC$(119, 0) = B$(I)
            TRC$(119, 1) = VERSUS$(I)
            TRC$(119, 2) = B$(1 - I)
            TRC$(119, 3) = Date$
        26 End If

        If QR(I, 1, 8) > TREC(121, 0) Then
            TREC(121, 0) = QR(I, 1, 8)
            TREC(121, 1) = 1
            TREC(121, 2) = 0
            TRC$(121, 0) = B$(I)
            TRC$(121, 1) = VERSUS$(I)
            TRC$(121, 2) = B$(1 - I)
            TRC$(121, 3) = Date$
        22 End If

        If QR(I, 2, 8) > TREC(121, 0) Then
            TREC(121, 0) = QR(I, 2, 8)
            TREC(121, 1) = 1
            TREC(121, 2) = 0
            TRC$(121, 0) = B$(I)
            TRC$(121, 1) = VERSUS$(I)
            TRC$(121, 2) = B$(1 - I)
            TRC$(121, 3) = Date$
        End If

        ' TURNOVERS
        If V8(I, 12) > TREC(122, 0) Then
            TREC(122, 0) = V8(I, 12)
            TREC(122, 1) = 0
            TREC(122, 2) = 0
            TRC$(122, 0) = B$(I)
            TRC$(122, 1) = VERSUS$(I)
            TRC$(122, 2) = B$(1 - I)
            TRC$(122, 3) = Date$
        29 End If

        If V8(I, 12) < TREC(123, 0) Then
            TREC(123, 0) = V8(I, 12)
            TREC(123, 1) = 0
            TREC(123, 2) = 0
            TRC$(123, 0) = B$(I)
            TRC$(123, 1) = VERSUS$(I)
            TRC$(123, 2) = B$(1 - I)
            TRC$(123, 3) = Date$
        30 End If

        If V8(I, 12) + V8(1 - I, 12) > TREC(124, 0) Then
            TREC(124, 0) = V8(I, 12) + V8(1 - I, 12)
            TREC(124, 1) = V8(I, 12)
            TREC(124, 2) = V8(1 - I, 12)
            TRC$(124, 0) = B$(I)
            TRC$(124, 1) = VERSUS$(I)
            TRC$(124, 2) = B$(1 - I)
            TRC$(124, 3) = Date$
        31 End If

        If V8(I, 12) + V8(1 - I, 12) < TREC(125, 0) Then
            TREC(125, 0) = V8(I, 12) + V8(1 - I, 12)
            TREC(125, 1) = V8(I, 12)
            TREC(125, 2) = V8(1 - I, 12)
            TRC$(125, 0) = B$(I)
            TRC$(125, 1) = VERSUS$(I)
            TRC$(125, 2) = B$(1 - I)
            TRC$(125, 3) = Date$
        32 End If

    Next

    Open p$(1) + "RECORDS.GAM" For Output As #1
    For I = 0 To 48
        For I1 = 0 To 2
            Write #1, REC(I, I1)
        Next
        For I1 = 0 To 4
            Write #1, RC$(I, I1)
        Next
    Next
    For I = 0 To 125
        For I1 = 0 To 2
            Write #1, TREC(I, I1)
        Next
        For I1 = 0 To 3
            Write #1, TRC$(I, I1)
        Next
    Next
    Close 1
End Sub

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
