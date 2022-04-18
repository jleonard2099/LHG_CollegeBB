'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Courtside College Basketball - Update Stats"
''$ExeIcon:'./lhg_ccb.ico'
'_Icon
_AllowFullScreen

Dim O1(30), Z1$(30), W0(15), W1(15), DL$(40), OL$(40), OL(40, 20), DL(40, 20), N$(14)
Dim O3(30), O4(30), O5(30), O6(30)
Dim O2(30), Z2$(250), Z3$(250), O%(100), O$(100), T%(100), H1$(40)
'Dim QM$(40), PS$(40), Z4$(40)
Dim LD(250, 2), S(14, 26), OT(21), DT(21)
'Dim H$(100), M1$(40), HA$(40), ZA$(40)
Dim D$(1), BRC(32), BRC$(32, 1), TRC(21), TRC$(21), TRC1(21), TRC1$(21)

Open "DEV.BAS" For Input As #2
Input #2, D$(0): Input #2, D$(1)
Close 2

Cls

4001 TRC(14) = 19999: TRC(18) = 19999
For I = 1 To 13: TRC1(I) = 19999: Next
For I = 15 To 17: TRC1(I) = 19999: Next
For I = 19 To 20: TRC1(I) = 19999: Next
If Not _FileExists(D$(1) + "RECORDS.CBB") Then 4003
    
Open D$(1) + "RECORDS.CBB" For Input As #1
For I = 1 To 25
    Input #1, BRC(I)
    Input #1, BRC$(I, 0)
    Input #1, BRC$(I, 1)
Next
For I = 1 To 21
    Input #1, TRC(I)
    Input #1, TRC$(I)
Next
For I = 1 To 21
    Input #1, TRC1(I)
    Input #1, TRC1$(I)
Next
Close 1
GoSub 4600

4003 Spec$ = D$(1) + "*.LIB"
COUNT% = FCount%(Spec$)
If COUNT% = 0 Then 4500
ReDim leagueFiles$(0 To COUNT%)
For X = 1 To COUNT%
    leagueFiles$(X) = Space$(12)
Next
leagueFiles$(0) = Spec$
Call ReadFile(leagueFiles$())
'CALL SortStr(VARPTR(leagueFiles$(1)), COUNT%, 0)

1310 Cls
Color , 1
Cls
Locate , 28
Color 14
Print "SELECT LEAGUE TO UPDATE": Color 15
SELECTION% = 1
START% = 1
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
Print "-SELECTS LEAGUE";

_MouseShow "DEFAULT"
20 MAMenu leagueFiles$(), SELECTION%, START%, COUNT%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% = 27 Then End
If EXITCODE% = -71 Then START% = 1: EXITCODE% = 3: SELECTION% = 1: GoTo 20
If EXITCODE% = -79 Then START% = COUNT%: EXITCODE% = 3: SELECTION% = COUNT%: GoTo 20
If EXITCODE% = -73 Then START% = START% - 80: If START% < 1 Then START% = 1
If EXITCODE% = -81 Then START% = START% + 80: If START% > COUNT% Then START% = COUNT%
If EXITCODE% = -81 Or EXITCODE% = -73 Then EXITCODE% = 3: SELECTION% = START%: GoTo 20
targetFile$ = leagueFiles$(SELECTION%)
Locate 2, 33: Print targetFile$
Locate , 33: Print "ANY CHANGE (YN)";
Color 15, 0
3910 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 1310

3911 targetFile$ = RTrim$(targetFile$)
WE$ = Left$(targetFile$, Len(targetFile$) - 4)
LH$ = WE$
Cls: Print "IS THIS THE FINAL UPDATE FOR THE SEASON (YN)";
Color 15, 0
3912 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then UP$ = "Y"
Open D$(1) + WE$ + ".TMS" For Input As #2
Input #2, LG$
Input #2, D1$: Input #2, D2$
For XW = 1 To 30
    Input #2, H1$(XW)
    Z1$(XW) = H1$(XW)
Next
Close 2
For X = 1 To 40
    DL(X, 19) = 9999
Next
For XW = 1 To 30
    GoSub 700
    GoSub 900
    Close 3
Next
AA = 1: ZZ = 15: GoSub 950
AA = 16: ZZ = 30: GoSub 950
GoSub 4000

Cls

2905 Run "HELLO.EXE 3"
End

700 WN = 0: LS = 0: CW = 0: CL = 0
If H1$(XW) = Chr$(42) Then Return
GoSub 9131
If Not _FileExists(D$(1) + H1$(XW) + "S") Then 1030
Open D$(1) + H1$(XW) + "S" For Input As #2

801 Input #2, Y%
For I = 1 To 100: Input #2, O%(I): Next I
For I = 1 To 100: Input #2, O$(I): Next I

802 For I = 1 To 100: Input #2, T%(I): Next I
Close 2

803 WN = 0: LS = 0: CW = 0: CL = 0

804 GoSub 5500
Cls
Locate 4, 5
Color 14: Print "FINDING ";
Color 15: Print H1$(XW);: Color 14
Print " STATS            "
Return

811 For I = 0 To 13: For J = 0 To 22: S(I, J) = 0: Next: Next
If Not _FileExists(D$(1) + H1$(XW)) Then 1030
Open D$(1) + H1$(XW) For Input As #2: Input #2, TN$

812 For I = 0 To 13
    Input #2, N$(I)
    For J = 0 To 13: Input #2, S(I, J): Next
    Input #2, W0(I): Input #2, W1(I)
Next: For L = 1 To 16: Input #2, OT(L): Next: For L = 1 To 16: Input #2, DT(L): Next: Close 2
For I = 0 To 13
    S(I, 25) = S(I, 13)
    S(I, 26) = S(I, 12)
    S(I, 12) = S(I, 2) * 2 + W0(I) + S(I, 4): ' *** PTS ***
    If S(I, 0) = 0 Then 835
    S(I, 13) = S(I, 5) / S(I, 0): ' *** RB/G ***
    S(I, 14) = S(I, 6) / S(I, 0): ' *** FOULS/G ***
    S(I, 15) = S(I, 7) / S(I, 0): ' *** STEALS/G ***
    S(I, 16) = S(I, 8) / S(I, 0): ' *** ASSISTS/G ***
    S(I, 17) = S(I, 9) / S(I, 0): ' *** BLOCKS/G ***
    S(I, 24) = S(I, 5) + S(I, 12) - S(I, 6) + S(I, 8) + S(I, 9) + S(I, 7) - 2 * S(I, 11)
    S(I, 18) = S(I, 10) / 60 / S(I, 0): ' *** MIN/G ***
    If S(I, 1) = 0 Or (S(I, 2) / Y% < 5) Then 826
    S(I, 19) = S(I, 2) / S(I, 1): ' *** FG% ***

    826 If S(I, 3) = 0 Or (S(I, 4) / Y%) < 2.5 Then 828
    S(I, 20) = S(I, 4) / S(I, 3): ' *** FT% ***

    828 If W1(I) = 0 Or W0(I) / Y% < 1.5 Then 831
    S(I, 21) = W0(I) / W1(I): ' *** 3FG% ***

    831 S(I, 22) = S(I, 12) / S(I, 0): ' *** PTS/G ***
    If S(I, 0) > BRC(1) Then BRC(1) = S(I, 0): BRC$(1, 0) = N$(I): BRC$(1, 1) = TN$
    If S(I, 1) > BRC(2) Then BRC(2) = S(I, 1): BRC$(2, 0) = N$(I): BRC$(2, 1) = TN$
    If S(I, 2) > BRC(3) Then BRC(3) = S(I, 2): BRC$(3, 0) = N$(I): BRC$(3, 1) = TN$
    If S(I, 19) > BRC(4) And UCase$(UP$) = "Y" And S(I, 2) / Y% >= 5 Then BRC(4) = S(I, 19): BRC$(4, 0) = N$(I): BRC$(4, 1) = TN$
    If W1(I) > BRC(5) Then BRC(5) = W1(I): BRC$(5, 0) = N$(I): BRC$(5, 1) = TN$
    If W0(I) > BRC(6) Then BRC(6) = W0(I): BRC$(6, 0) = N$(I): BRC$(6, 1) = TN$
    If S(I, 21) > BRC(7) And UCase$(UP$) = "Y" And W0(I) / Y% >= 1.5 Then BRC(7) = S(I, 21): BRC$(7, 0) = N$(I): BRC$(7, 1) = TN$
    If S(I, 3) > BRC(8) Then BRC(8) = S(I, 3): BRC$(8, 0) = N$(I): BRC$(8, 1) = TN$
    If S(I, 4) > BRC(9) Then BRC(9) = S(I, 4): BRC$(9, 0) = N$(I): BRC$(9, 1) = TN$
    If S(I, 20) > BRC(10) And UCase$(UP$) = "Y" And S(I, 4) / Y% >= 2.5 Then BRC(10) = S(I, 20): BRC$(10, 0) = N$(I): BRC$(10, 1) = TN$
    If S(I, 10) / 60 > BRC(11) Then BRC(11) = S(I, 10) / 60: BRC$(11, 0) = N$(I): BRC$(11, 1) = TN$
    If S(I, 12) > BRC(12) Then BRC(12) = S(I, 12): BRC$(12, 0) = N$(I): BRC$(12, 1) = TN$
    If S(I, 22) > BRC(13) And UCase$(UP$) = "Y" Then BRC(13) = S(I, 22): BRC$(13, 0) = N$(I): BRC$(13, 1) = TN$
    If S(I, 25) > BRC(14) Then BRC(14) = S(I, 25): BRC$(14, 0) = N$(I): BRC$(14, 1) = TN$
    If S(I, 26) > BRC(15) Then BRC(15) = S(I, 26): BRC$(15, 0) = N$(I): BRC$(15, 1) = TN$
    If S(I, 5) - S(I, 26) > BRC(16) Then BRC(16) = S(I, 5) - S(I, 26): BRC$(16, 0) = N$(I): BRC$(16, 1) = TN$
    If S(I, 5) > BRC(17) Then BRC(17) = S(I, 5): BRC$(17, 0) = N$(I): BRC$(17, 1) = TN$
    If S(I, 13) > BRC(18) And UCase$(UP$) = "Y" Then BRC(18) = S(I, 13): BRC$(18, 0) = N$(I): BRC$(18, 1) = TN$
    If S(I, 6) > BRC(19) Then BRC(19) = S(I, 6): BRC$(19, 0) = N$(I): BRC$(19, 1) = TN$
    If S(I, 7) > BRC(20) Then BRC(20) = S(I, 7): BRC$(20, 0) = N$(I): BRC$(20, 1) = TN$
    If S(I, 8) > BRC(21) Then BRC(21) = S(I, 8): BRC$(21, 0) = N$(I): BRC$(21, 1) = TN$
    If S(I, 9) > BRC(22) Then BRC(22) = S(I, 9): BRC$(22, 0) = N$(I): BRC$(22, 1) = TN$
    If S(I, 11) > BRC(23) Then BRC(23) = S(I, 11): BRC$(23, 0) = N$(I): BRC$(23, 1) = TN$
    If S(I, 18) > BRC(24) And UCase$(UP$) = "Y" Then BRC(24) = S(I, 18): BRC$(24, 0) = N$(I): BRC$(24, 1) = TN$
    If S(I, 24) > BRC(25) Then BRC(25) = S(I, 24): BRC$(25, 0) = N$(I): BRC$(25, 1) = TN$

835 Next
OT(16) = Y%: DT(16) = Y%: OT(17) = OT(3) / OT(2): DT(17) = DT(3) / DT(2)
OT(18) = OT(5) / OT(4): DT(18) = DT(5) / DT(4)
OT(20) = OT(11): DT(20) = DT(11)
If OT(15) <> 0 Then OT(21) = OT(14) / OT(15) Else OT(21) = 0
If DT(15) <> 0 Then DT(21) = DT(14) / DT(15) Else DT(21) = 999
OT(19) = OT(20) / Y%: DT(19) = DT(20) / Y%
If OT(2) > TRC(1) Then TRC(1) = OT(2): TRC$(1) = TN$
If OT(3) > TRC(2) Then TRC(2) = OT(3): TRC$(2) = TN$
If OT(17) > TRC(3) And UCase$(UP$) = "Y" Then TRC(3) = OT(17): TRC$(3) = TN$
If OT(15) > TRC(4) Then TRC(4) = OT(15): TRC$(4) = TN$
If OT(14) > TRC(5) Then TRC(5) = OT(14): TRC$(5) = TN$
If OT(21) > TRC(6) And UCase$(UP$) = "Y" Then TRC(6) = OT(21): TRC$(6) = TN$
If OT(4) > TRC(7) Then TRC(7) = OT(4): TRC$(7) = TN$
If OT(5) > TRC(8) Then TRC(8) = OT(5): TRC$(8) = TN$
If OT(18) > TRC(9) And UCase$(UP$) = "Y" Then TRC(9) = OT(18): TRC$(9) = TN$
If OT(13) > TRC(10) Then TRC(10) = OT(13): TRC$(10) = TN$
If OT(6) - OT(13) > TRC(11) Then TRC(11) = OT(6) - OT(13): TRC$(11) = TN$
If OT(6) > TRC(12) Then TRC(12) = OT(6): TRC$(12) = TN$
If OT(6) / Y% > TRC(13) And UCase$(UP$) = "Y" Then TRC(13) = OT(6) / Y%: TRC$(13) = TN$
If OT(7) < TRC(14) And UCase$(UP$) = "Y" Then TRC(14) = OT(7): TRC$(14) = TN$
If OT(8) > TRC(15) Then TRC(15) = OT(8): TRC$(15) = TN$
If OT(9) > TRC(16) Then TRC(16) = OT(9): TRC$(16) = TN$
If OT(10) > TRC(17) Then TRC(17) = OT(10): TRC$(17) = TN$
If OT(12) < TRC(18) And UCase$(UP$) = "Y" Then TRC(18) = OT(12): TRC$(18) = TN$
If OT(11) > TRC(19) Then TRC(19) = OT(11): TRC$(19) = TN$
If OT(11) / Y% > TRC(20) And UCase$(UP$) = "Y" Then TRC(20) = OT(11) / Y%: TRC$(20) = TN$
If WN > TRC(21) Then TRC(21) = WN: TRC$(21) = TN$
If DT(2) < TRC1(1) Then TRC1(1) = DT(2): TRC1$(1) = TN$
If DT(3) < TRC1(2) Then TRC1(2) = DT(3): TRC1$(2) = TN$
If DT(17) < TRC1(3) And UCase$(UP$) = "Y" Then TRC1(3) = DT(17): TRC1$(3) = TN$
If DT(15) < TRC1(4) Then TRC1(4) = DT(15): TRC1$(4) = TN$
If DT(14) < TRC1(5) Then TRC1(5) = DT(14): TRC1$(5) = TN$
If DT(21) < TRC1(6) And DT(15) <> 0 And UCase$(UP$) = "Y" Then TRC1(6) = DT(21): TRC1$(6) = TN$
If DT(4) < TRC1(7) Then TRC1(7) = DT(4): TRC1$(7) = TN$
If DT(5) < TRC1(8) Then TRC1(8) = DT(5): TRC1$(8) = TN$
If DT(18) < TRC1(9) And UCase$(UP$) = "Y" Then TRC1(9) = DT(18): TRC1$(9) = TN$
If DT(13) < TRC1(10) Then TRC1(10) = DT(13): TRC1$(10) = TN$
If DT(6) - DT(13) < TRC1(11) Then TRC1(11) = DT(6) - DT(13): TRC1$(11) = TN$
If DT(6) < TRC1(12) Then TRC1(12) = DT(6): TRC1$(12) = TN$
If DT(6) / Y% < TRC1(13) And UCase$(UP$) = "Y" Then TRC1(13) = DT(6) / Y%: TRC1$(13) = TN$
If DT(7) > TRC1(14) And UCase$(UP$) = "Y" Then TRC1(14) = DT(7): TRC1$(14) = TN$
If DT(8) < TRC1(15) Then TRC1(15) = DT(8): TRC1$(15) = TN$
If DT(9) < TRC1(16) Then TRC1(16) = DT(9): TRC1$(16) = TN$
If DT(10) < TRC1(17) Then TRC1(17) = DT(10): TRC1$(17) = TN$
If DT(12) > TRC1(18) And UCase$(UP$) = "Y" Then TRC1(18) = DT(12): TRC1$(18) = TN$
If DT(11) < TRC1(19) Then TRC1(19) = DT(11): TRC1$(19) = TN$
If DT(11) / Y% < TRC1(20) And UCase$(UP$) = "Y" Then TRC1(20) = DT(11) / Y%: TRC1$(20) = TN$
If LS > TRC1(21) Then TRC1(21) = LS: TRC1$(21) = TN$

1030 Return

900 L = Len(H1$(XW)): WT$ = ""
For X = 1 To L
    A = Asc(Mid$(H1$(XW), X, 1))
WT$ = WT$ + Chr$(A): Next X
For X = 1 To 30
    If Z1$(X) <> WT$ Then 945
    If O1(X) = WN And O2(X) = LS Then 948
    O1(X) = WN
    O2(X) = LS
    O4(X) = CW
    O5(X) = CL
    If O1(X) + O2(X) = 0 Then
        O3(X) = 0
    Else
        O3(X) = O1(X) / (O1(X) + O2(X))
    End If
    If O4(X) + O5(X) = 0 Then
        O6(X) = 0
    Else
        O6(X) = O4(X) / (O4(X) + O5(X))
    End If

945 Next X
GoSub 811
GoSub 3100

948 Return

950 For X = AA To ZZ - 1: For Y = X + 1 To ZZ: If O1(X) - O2(X) > O1(Y) - O2(Y) Then 985
    If O1(X) - O2(X) = O1(Y) - O2(Y) And O1(X) <= O1(Y) And O1(X) > O2(X) Then 985
        If O1(X) - O2(X) = O1(Y) - O2(Y) And O1(X) >= O1(Y) And O1(X) < O2(X) Then 985
        Swap Z1$(X), Z1$(Y)
        Swap O1(X), O1(Y)
        Swap O2(X), O2(Y)
        Swap O3(X), O3(Y)
        Swap O4(X), O4(Y)
        Swap O5(X), O5(Y)
        Swap O6(X), O6(Y)

985 Next Y: Next X

991 Return

3100 'CLS:PRINT "CHECKING ";H1$(XW);" STATS"
For X = 0 To 14: Color 14: Print "CHECKING STATS OF ";: Color 15: Print N$(X)
    If S(X, 22) <= LD(20, 1) Then 3320
    LD(20, 1) = S(X, 22): LD(20, 2) = 0: Z2$(20) = N$(X): Z3$(20) = TN$
    For Z = 1 To 19: For Z1 = Z + 1 To 20: If LD(Z, 1) >= LD(Z1, 1) Then 3317
        Swap Z2$(Z1), Z2$(Z)
        Swap Z3$(Z1), Z3$(Z)
        For M = 1 To 2: Swap LD(Z1, M), LD(Z, M): Next

    3317 Next Z1: Next Z

    3320 If S(X, 12) <= LD(40, 1) Then 3340
    LD(40, 1) = S(X, 12): LD(40, 2) = 0: Z2$(40) = N$(X): Z3$(40) = TN$
    For Z = 21 To 39: For Z1 = Z + 1 To 40: If LD(Z, 1) >= LD(Z1, 1) Then 3337
        Swap Z2$(Z1), Z2$(Z)
        Swap Z3$(Z1), Z3$(Z)
        For M = 1 To 2: Swap LD(Z1, M), LD(Z, M): Next

    3337 Next Z1: Next Z

    3340 If S(X, 19) <= LD(60, 1) Then 3360
    LD(60, 1) = S(X, 19): LD(60, 2) = S(X, 1): Z2$(60) = N$(X): Z3$(60) = TN$
    For Z = 41 To 59: For Z1 = Z + 1 To 60: If LD(Z, 1) >= LD(Z1, 1) Then 3357
        Swap Z2$(Z1), Z2$(Z)
        Swap Z3$(Z1), Z3$(Z)
        For M = 1 To 2: Swap LD(Z1, M), LD(Z, M): Next

    3357 Next Z1: Next Z

    3360 If S(X, 20) <= LD(80, 1) Then 3380
    LD(80, 1) = S(X, 20): LD(80, 2) = S(X, 3): Z2$(80) = N$(X): Z3$(80) = TN$
    For Z = 61 To 79: For Z1 = Z + 1 To 80: If LD(Z, 1) >= LD(Z1, 1) Then 3377
        Swap Z2$(Z1), Z2$(Z)
        Swap Z3$(Z1), Z3$(Z)
        For M = 1 To 2: Swap LD(Z1, M), LD(Z, M): Next

    3377 Next Z1: Next Z

    3380 If S(X, 15) <= LD(90, 1) Then 3400
    LD(90, 1) = S(X, 15): LD(90, 2) = S(X, 7): Z2$(90) = N$(X): Z3$(90) = TN$
    For Z = 81 To 89: For Z1 = Z + 1 To 90: If LD(Z, 1) >= LD(Z1, 1) Then 3397
        Swap Z2$(Z1), Z2$(Z)
        Swap Z3$(Z1), Z3$(Z)
        For M = 1 To 2: Swap LD(Z1, M), LD(Z, M): Next

    3397 Next Z1: Next Z

    3400 If S(X, 7) <= LD(100, 1) Then 3420
    LD(100, 1) = S(X, 7): LD(100, 2) = 0: Z2$(100) = N$(X): Z3$(100) = TN$
    For Z = 91 To 99: For Z1 = Z + 1 To 100: If LD(Z, 1) >= LD(Z1, 1) Then 3417
        Swap Z2$(Z1), Z2$(Z)
        Swap Z3$(Z1), Z3$(Z)
        For M = 1 To 2: Swap LD(Z1, M), LD(Z, M): Next

    3417 Next Z1: Next Z

    3420 If S(X, 16) <= LD(110, 1) Then 3440
    LD(110, 1) = S(X, 16): LD(110, 2) = S(X, 8): Z2$(110) = N$(X): Z3$(110) = TN$
    For Z = 101 To 109: For Z1 = Z + 1 To 110: If LD(Z, 1) >= LD(Z1, 1) Then 3437
        Swap Z2$(Z1), Z2$(Z)
        Swap Z3$(Z1), Z3$(Z)
        For M = 1 To 2: Swap LD(Z1, M), LD(Z, M): Next

    3437 Next Z1: Next Z

    3440 'FOR Z=111 TO 120
    If S(X, 8) <= LD(120, 1) Then 3460
    LD(120, 1) = S(X, 8): LD(120, 2) = S(X, 8): Z2$(120) = N$(X): Z3$(120) = TN$
    For Z = 111 To 119: For Z1 = Z + 1 To 120: If LD(Z, 1) >= LD(Z1, 1) Then 3457
        Swap Z2$(Z1), Z2$(Z)
        Swap Z3$(Z1), Z3$(Z)
        For M = 1 To 2: Swap LD(Z1, M), LD(Z, M): Next

    3457 Next Z1: Next Z

    3460 'FOR Z=121 TO 130
    If S(X, 21) <= LD(130, 1) Then 3480
    LD(130, 1) = S(X, 21): LD(130, 2) = W1(X): Z2$(130) = N$(X): Z3$(130) = TN$
    For Z = 121 To 129: For Z1 = Z + 1 To 130: If LD(Z, 1) >= LD(Z1, 1) Then 3477
        Swap Z2$(Z1), Z2$(Z)
        Swap Z3$(Z1), Z3$(Z)
        For M = 1 To 2: Swap LD(Z1, M), LD(Z, M): Next

    3477 Next Z1: Next Z

    3480 'FOR Z=131 TO 140
    If W0(X) <= LD(140, 1) Then 3500
    LD(140, 1) = W0(X): LD(140, 2) = 0: Z2$(140) = N$(X): Z3$(140) = TN$
    For Z = 131 To 139: For Z1 = Z + 1 To 140: If LD(Z, 1) >= LD(Z1, 1) Then 3497
        Swap Z2$(Z1), Z2$(Z)
        Swap Z3$(Z1), Z3$(Z)
        For M = 1 To 2: Swap LD(Z1, M), LD(Z, M): Next

    3497 Next Z1: Next Z

    3500 'FOR Z=141 TO 150
    If S(X, 18) <= LD(150, 1) Then 3520
    LD(150, 1) = S(X, 18): LD(150, 2) = 0: Z2$(150) = N$(X): Z3$(150) = TN$
    For Z = 141 To 149: For Z1 = Z + 1 To 150: If LD(Z, 1) >= LD(Z1, 1) Then 3517
        Swap Z2$(Z1), Z2$(Z)
        Swap Z3$(Z1), Z3$(Z)
        For M = 1 To 2: Swap LD(Z1, M), LD(Z, M): Next

    3517 Next Z1: Next Z

    3520 'FOR Z=151 TO 160
    If S(X, 10) / 60 <= LD(160, 1) Then 3540
    LD(160, 1) = S(X, 10) / 60: LD(160, 2) = 0: Z2$(160) = N$(X): Z3$(160) = TN$
    For Z = 151 To 159: For Z1 = Z + 1 To 160: If LD(Z, 1) >= LD(Z1, 1) Then 3537
        Swap Z2$(Z1), Z2$(Z)
        Swap Z3$(Z1), Z3$(Z)
        For M = 1 To 2: Swap LD(Z1, M), LD(Z, M): Next

    3537 Next Z1: Next Z

    3540 'FOR Z=161 TO 170
    If S(X, 13) <= LD(170, 1) Then 3560
    LD(170, 1) = S(X, 13): LD(170, 2) = S(X, 5): Z2$(170) = N$(X): Z3$(170) = TN$
    For Z = 161 To 169: For Z1 = Z + 1 To 170: If LD(Z, 1) >= LD(Z1, 1) Then 3557
        Swap Z2$(Z1), Z2$(Z)
        Swap Z3$(Z1), Z3$(Z)
        For M = 1 To 2: Swap LD(Z1, M), LD(Z, M): Next

    3557 Next Z1: Next Z

    3560 'FOR Z=171 TO 180
    If S(X, 5) <= LD(180, 1) Then 3580
    LD(180, 1) = S(X, 5): LD(18, 2) = 0: Z2$(180) = N$(X): Z3$(180) = TN$
    For Z = 171 To 179: For Z1 = Z + 1 To 180: If LD(Z, 1) >= LD(Z1, 1) Then 3577
        Swap Z2$(Z1), Z2$(Z)
        Swap Z3$(Z1), Z3$(Z)
        For M = 1 To 2: Swap LD(Z1, M), LD(Z, M): Next

    3577 Next Z1: Next Z

    3580 'FOR Z=181 TO 190
    If S(X, 17) <= LD(190, 1) Then 3600
    LD(190, 1) = S(X, 17): LD(190, 2) = 0: Z2$(190) = N$(X): Z3$(190) = TN$
    For Z = 181 To 189: For Z1 = Z + 1 To 190: If LD(Z, 1) >= LD(Z1, 1) Then 3597
        Swap Z2$(Z1), Z2$(Z)
        Swap Z3$(Z1), Z3$(Z)
        For M = 1 To 2: Swap LD(Z1, M), LD(Z, M): Next

    3597 Next Z1: Next Z

    3600 'FOR Z=191 TO 200
    If S(X, 9) <= LD(200, 1) Then 3620
    LD(200, 1) = S(X, 9): LD(200, 2) = 0: Z2$(200) = N$(X): Z3$(200) = TN$
    For Z = 191 To 199: For Z1 = Z + 1 To 200: If LD(Z, 1) >= LD(Z1, 1) Then 3617
        Swap Z2$(Z1), Z2$(Z)
        Swap Z3$(Z1), Z3$(Z)
        For M = 1 To 2: Swap LD(Z1, M), LD(Z, M): Next

    3617 Next Z1: Next Z

    3620 'FOR Z=201 TO 210
    If S(X, 2) <= LD(210, 1) Then 3640
    LD(210, 1) = S(X, 2): LD(210, 2) = 0: Z2$(210) = N$(X): Z3$(210) = TN$
    For Z = 201 To 209: For Z1 = Z + 1 To 210: If LD(Z, 1) >= LD(Z1, 1) Then 3637
        Swap Z2$(Z1), Z2$(Z)
        Swap Z3$(Z1), Z3$(Z)
        For M = 1 To 2: Swap LD(Z1, M), LD(Z, M): Next

    3637 Next Z1: Next Z

    3640 'FOR Z=211 TO 220
    If S(X, 4) <= LD(220, 1) Then 3660
    LD(220, 1) = S(X, 4): LD(220, 2) = 0: Z2$(220) = N$(X): Z3$(220) = TN$
    For Z = 211 To 219: For Z1 = Z + 1 To 220: If LD(Z, 1) >= LD(Z1, 1) Then 3657
        Swap Z2$(Z1), Z2$(Z)
        Swap Z3$(Z1), Z3$(Z)
        For M = 1 To 2: Swap LD(Z1, M), LD(Z, M): Next

    3657 Next Z1: Next Z

    3660 'FOR Z=221 TO 230
    If S(X, 14) <= LD(230, 1) Then 3680
    LD(230, 1) = S(X, 14): LD(230, 2) = 0: Z2$(230) = N$(X): Z3$(230) = TN$
    For Z = 221 To 229: For Z1 = Z + 1 To 230: If LD(Z, 1) >= LD(Z1, 1) Then 3677
        Swap Z2$(Z1), Z2$(Z)
        Swap Z3$(Z1), Z3$(Z)
        For M = 1 To 2: Swap LD(Z1, M), LD(Z, M): Next

    3677 Next Z1: Next Z

    3680 'FOR Z=231 TO 240
    If S(X, 6) <= LD(240, 1) Then 3690
    LD(240, 1) = S(X, 6): LD(240, 2) = 0: Z2$(240) = N$(X): Z3$(240) = TN$
    For Z = 231 To 239: For Z1 = Z + 1 To 240: If LD(Z, 1) >= LD(Z1, 1) Then 3697
        Swap Z2$(Z1), Z2$(Z)
        Swap Z3$(Z1), Z3$(Z)
        For M = 1 To 2: Swap LD(Z1, M), LD(Z, M): Next

    3697 Next Z1: Next Z

    3690 'FOR Z=241 TO 250
    If S(X, 24) <= LD(250, 1) Then 3700
    LD(250, 1) = S(X, 24): LD(250, 2) = 0: Z2$(250) = N$(X): Z3$(250) = TN$
    For Z = 241 To 249: For Z1 = Z + 1 To 250: If LD(Z, 1) >= LD(Z1, 1) Then 3698
        Swap Z2$(Z1), Z2$(Z)
        Swap Z3$(Z1), Z3$(Z)
        For M = 1 To 2: Swap LD(Z1, M), LD(Z, M): Next

    3698 Next Z1: Next Z

3700 Next X
'FOR X=1 TO 40
If OT(19) <= OL(40, 19) Then 3820
For M = 1 To 20: OL(40, M) = OT(M): Next: OL$(40) = TN$
For X = 1 To 39: For X1 = 1 + X To 40: If OL(X, 19) >= OL(X1, 19) Then 3817
    Swap OL$(X1), OL$(X)
    For M = 1 To 20: J = OL(X1, M): OL(X1, M) = OL(X, M): OL(X, M) = J: Next

3817 Next X1: Next X

3820 'FOR X=1 TO 15
If DT(19) >= DL(40, 19) Then 3840
For M = 1 To 20: DL(40, M) = DT(M): Next: DL$(40) = TN$
For X = 1 To 39: For X1 = 1 + X To 40: If DL(X, 19) <= DL(X1, 19) Then 3837
    Swap DL$(X1), DL$(X)
    For M = 1 To 20: J = DL(X1, M): DL(X1, M) = DL(X, M): DL(X, M) = J: Next

3837 Next X1: Next X

3840 Return

4000 Cls
Print: Open D$(1) + WE$ + ".LDR" For Output As #2: Print "    POSTING NEW "; WE$; " CONFERENCE LEADERS"
For X = 1 To 30: Write #2, Z1$(X), O1(X), O2(X), O3(X), O4(X), O5(X), O6(X): Next
For X = 1 To 250
Write #2, Z2$(X), Z3$(X), LD(X, 1), LD(X, 2): Next X
For X = 1 To 40: Write #2, OL$(X): For Y = 1 To 20: Write #2, OL(X, Y): Next: Next
For X = 1 To 40: Write #2, DL$(X): For Y = 1 To 20: Write #2, DL(X, Y): Next: Next
Close 2
Open D$(1) + "RECORDS.CBB" For Output As #1
For I = 1 To 25
    Print #1, BRC(I)
    Print #1, BRC$(I, 0)
    Print #1, BRC$(I, 1)
Next
For I = 1 To 21
    Print #1, TRC(I)
    Print #1, TRC$(I)
Next
For I = 1 To 21
    Print #1, TRC1(I)
    Print #1, TRC1$(I)
Next
Close 1
Return
    

5005 L = Len(TN$): WX$ = "": For X = 1 To (L - 3): A = Asc(Mid$(TN$, X, 1))

5006 WT$ = WT$ + Chr$(A): Next X

5007 TN$ = WT$: Return

5500 For I = 1 To Y%

    5501 GoSub 5600

    5505 For EC = 1 To 30
        If O$(I) = H1$(EC) Then 5510

    5506 Next EC

    5507 If O%(I) > T%(I) Then WN = WN + 1

    5508 If O%(I) < T%(I) Then LS = LS + 1

    5509 GoTo 5520

    5510 If O%(I) > T%(I) Then CW = CW + 1: WN = WN + 1

    5511 If O%(I) < T%(I) Then CL = CL + 1: LS = LS + 1

    5520 EC = 0
Next I

5530 Return

5600 For P = 4 To 11: If Mid$(O$(I), P, 1) = " " Or Mid$(O$(I), P, 1) = "." Then Mid$(O$(I), P, 1) = "_"

5601 Next P

5602 For P = 12 To 14: If Mid$(O$(I), P, 2) = "ST" Then Mid$(O$(I), 9, 1) = "_": Mid$(O$(I), 10, 2) = "ST"

5603 Next P

5604 QQ = 8: For P = 11 To 4 Step -1: If Mid$(O$(I), P, 1) = "_" Then QQ = P - 4: GoTo 5606

    5605 GoTo 5608

5606 Next P

5608 f$ = Mid$(O$(I), 4, QQ): R$ = f$ + "." + Left$(O$(I), 2)

5609 O$(I) = R$

5610 Return

4600 JJ = 4
Locate 6, 8: Color 15, 1
Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";
For II = 1 To JJ: Locate 6 + II, 8
Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
Next
Locate 7 + JJ, 8
Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";: Locate 7
Return

4500 Locate 10, 8
GoSub 4600
Locate , 14: Print "NO LEAGUE FILES WERE FOUND"
Locate , 14: Print "PRESS ANY KEY TO RETURN TO MAIN MENU"
3805 I$ = GetKeyPress$
End

9131 H1$(XW) = Right$(H1$(XW), 2) + " " + Left$(H1$(XW), Len(H1$(XW)) - 3): GoSub 9900: Return

9900 For XX = 4 To 11: If Mid$(H1$(XW), XX, 1) = " " Or Mid$(H1$(XW), XX, 1) = "." Then Mid$(H1$(XW), XX, 1) = "_"
Next XX
For XX = 12 To 14: If Mid$(H1$(XW), XX, 2) = "ST" Then Mid$(H1$(XW), 9, 1) = "_": Mid$(H1$(XW), 10, 2) = "ST"
Next XX
QQ = 8: For XX = 11 To 4 Step -1: If Mid$(H1$(XW), XX, 1) = "_" Then QQ = XX - 4: GoTo 9906
    GoTo 9907

9906 Next XX

9907 G$ = H1$(XW): H1$(XW) = Right$(H1$(XW), Len(H1$(XW)) - 3): YR$ = Left$(G$, 2)
If Len(H1$(XW)) > 8 Then H1$(XW) = Left$(H1$(XW), 8)
H1$(XW) = H1$(XW) + "." + YR$: Return


'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
