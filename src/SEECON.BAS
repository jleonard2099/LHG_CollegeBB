'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Courtside College Basketball - Conference Stats"
''$ExeIcon:'./lhg_ccb.ico'
'_Icon
_AllowFullScreen

DECLARE SUB TITLE ()

Dim T$(30), O1(30), O2(30)
'Dim P(60), BA$(120), ER$(20), P$(260), S$(60), TT(120, 4)
Dim O3(30), O4(30), O5(30), O6(30)
Dim Z2$(260), Z3$(260), Z(260), Z1(260), OL$(60), OL(60, 20), DL$(60), DL(60, 20)
Dim D$(1)

H$ = " 1/2"

Open "DEV.BAS" For Input As #1: For I = 0 To 1: Input #1, D$(I): Next: Close 1

4000 Spec$ = D$(1) + "*.LDR"
COUNT% = FCount%(Spec$)
If COUNT% = 0 Then 4500
ReDim F$(0 To COUNT%)
For X = 1 To COUNT%
    F$(X) = Space$(12)
Next
F$(0) = Spec$
Call ReadFile(F$())
'CALL SortStr(VARPTR(F$(1)), COUNT%, 0)

1310 Cls
Color , 1
Cls
Locate , 28
Color 14
Print "SELECT LEAGUE TO VIEW": Color 15
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
Print "-SELECTS LEAGUE  ESC - EXIT PROGRAM";

_MouseShow "DEFAULT"
20 MAMenu F$(), SELECTION%, START%, COUNT%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% = 27 Then 2900
If EXITCODE% = -71 Then START% = 1: EXITCODE% = 3: SELECTION% = 1: GoTo 20
If EXITCODE% = -79 Then START% = COUNT%: EXITCODE% = 3: SELECTION% = COUNT%: GoTo 20
If EXITCODE% = -73 Then START% = START% - 80: If START% < 1 Then START% = 1
If EXITCODE% = -81 Then START% = START% + 80: If START% > COUNT% Then START% = COUNT%
If EXITCODE% = -81 Or EXITCODE% = -73 Then EXITCODE% = 3: SELECTION% = START%: GoTo 20
F$ = F$(SELECTION%)
Locate 2, 33: Print F$
Locate , 33: Print "ANY CHANGE (YN)";
Color 15, 0
3910 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 1310
F$ = RTrim$(F$)
WE$ = Left$(F$, Len(F$) - 4)
GoTo 7

4500 GoSub 4600
Locate , 12: Print "NO LEAGUE FILES WERE FOUND"
Locate , 12: Print "PRESS ANY KEY TO RETURN TO MAIN MENU"
3805 I$ = GetKeyPress$
GoTo 2900

7 WE$ = Left$(WE$, 8)
Open D$(1) + WE$ + ".TMS" For Input As #2
Input #2, FF$, D1$, D2$
Close 2
GoSub 200
Cls
Locate 9, 1: Print "      PLEASE WAIT WHILE THE LEADER FILES ARE LOADING!!"
Open D$(1) + WE$ + ".LDR" For Input As #2
For X = 1 To 30: Input #2, T$(X), O1(X), O2(X), O3(X), O4(X), O5(X), O6(X): Next
For X = 1 To 250: Input #2, Z2$(X), Z3$(X), Z(X), Z1(X): Next
For X = 1 To 40: Input #2, OL$(X): For M = 1 To 20: Input #2, OL(X, M): Next: Next
For X = 1 To 40: Input #2, DL$(X): For M = 1 To 20: Input #2, DL(X, M): Next: Next
Close #2

1202 Cls: Call TITLE
J = 5: GoSub 2075: Color 7
Locate , 30: Print "STATS TO ";: Color 15: Print "S";: Color 7: Print "CREEN"
Locate , 30: Print "STATS TO ";: Color 15: Print "P";: Color 7: Print "RINTER"
Locate , 30: Print "STATS TO ";: Color 15: Print "F";: Color 7: Print "ILE"
8651 I$ = GetKeyPress$
Color 14, 0
If UCase$(I$) = "P" Then 3200
If UCase$(I$) = "F" Then X1 = 1: GoTo 3100
GoTo 1200

200 L = 8: If Len(D1$) < L Then D1$ = D1$ + " ": GoTo 200

201 L = 8: If Len(D2$) < L Then D2$ = D2$ + " ": GoTo 201

204 Return

1200 Cls
Color 12: Print FF$ + " CONFERENCE"
If D1$ <> "" Then Print D1$; " DIVISION"
Print "                 CONFERENCE                         FULL SEASON"
Color 14
Print "                W    L     PCT                          W    L     PCT"
Color 15
For P = 1 To 15
    If T$(P) = "" Or T$(P) = "*" Then 1256

    1255 Print T$(P); Tab(16);
    Print Using "##   ##   "; O4(P); O5(P);
    If O6(P) = 0 And O4(P) = 0 And O5(P) = 0 Then Print " .---";
    If O6(P) = 0 And (O4(P) <> 0 Or O5(P) <> 0) Then Print " .000";
    If O6(P) = 1 Then Print "1.000";
    If O6(P) <> 0 And O6(P) <> 1 Then Print Using " .###"; O6(P);
    Locate , 41
    Print T$(P); Tab(56);
    Print Using "##   ##   "; O1(P); O2(P);
    If O3(P) = 0 And O1(P) = 0 And O2(P) = 0 Then Print " .---"
    If O3(P) = 0 And (O1(P) <> 0 Or O2(P) <> 0) Then Print " .000"
    If O3(P) = 1 Then Print "1.000"
    If O3(P) <> 0 And O3(P) <> 1 Then Print Using " .###"; O3(P)

1256 Next P

1277 GoSub 7900
Cls
Color 12: Print FF$ + " CONFERENCE"
If D2$ <> "" Then Print D2$; " DIVISION"
Print "                 CONFERENCE                         FULL SEASON"
Color 14
Print "                W    L     PCT                          W    L     PCT"
Color 15
For P = 16 To 30
    If T$(P) = "" Or T$(P) = "*" Then 1258
    Print T$(P); Tab(16);
    Print Using "##   ##   "; O4(P); O5(P);
    If O6(P) = 0 And O4(P) = 0 And O5(P) = 0 Then Print " .---";
    If O6(P) = 0 And (O4(P) <> 0 Or O5(P) <> 0) Then Print " .000";
    If O6(P) = 1 Then Print "1.000";
    If O6(P) <> 0 And O6(P) <> 1 Then Print Using " .###"; O6(P);
    Locate , 41
    Print T$(P); Tab(56);
    Print Using "##   ##   "; O1(P); O2(P);
    If O3(P) = 0 And O1(P) = 0 And O2(P) = 0 Then Print " .---"
    If O3(P) = 0 And (O1(P) <> 0 Or O2(P) <> 0) Then Print " .000"
    If O3(P) = 1 Then Print "1.000"
    If O3(P) <> 0 And O3(P) <> 1 Then Print Using " .###"; O3(P)

1258 Next P
GoSub 7900
Cls: Color 14
Print "FIELD GOAL % (MIN 5 FGM/G)"; Tab(41); "FREE THROW % (MIN 2.5 FTM/G)": Print: Color 15
Print "                            PCT   FGA"; Tab(41); "                            PCT   FTA"
For P = 41 To 60
    If Z2$(P) = "" Then 1305
    Print Left$(Z2$(P), 10) + ", " + Z3$(P);: Locate , 28
    Print Using "##.#  ####"; Z(P) * 100; Z1(P)

1305 Next
Locate 4
For P = 61 To 80
    If Z2$(P) = "" Then 1313
    Locate , 41: Print Left$(Z2$(P), 10) + ", " + Z3$(P);: Locate , 67
    Print Using "###.#  ####"; Z(P) * 100; Z1(P)

1313 Next
GoSub 7900
Cls
Color 14: Print "SCORING"
Color 14: Print "                                PTS/G": Color 15
For P = 1 To 20
    If Z2$(P) = "" Then 1330
    Print Z2$(P) + ", " + Z3$(P);: Locate , 34
    Print Using "##.#"; Z(P)

1330 Next
Locate 1, 41: Color 14: Print "TOTAL POINTS"
Locate , 41: Print "                                  PTS": Color 15
For P = 21 To 40
    If Z2$(P) = "" Then 1333
    Locate , 41: Print Z2$(P) + ", " + Z3$(P);: Locate , 74
    Print Using "####"; Z(P)

1333 Next
GoSub 7900
Cls
Color 14: Print "STEALS PER GAME": Color 15
For P = 81 To 90
    If Z2$(P) = "" Then 1420
    Print Z2$(P) + ", " + Z3$(P);: Locate , 34
    Print Using "##.#"; Z(P)

1420 Next
Locate 1, 41: Color 14: Print "STEALS": Color 15
For P = 91 To 100
    If Z2$(P) = "" Then 1426
    Locate , 41: Print Z2$(P) + ", " + Z3$(P);: Locate , 74
    Print Using "####"; Z(P)

1426 Next
Locate 13: Color 14: Print "ASSISTS PER GAME": Color 15
For P = 101 To 110
    If Z2$(P) = "" Then 1432
    Print Z2$(P) + ", " + Z3$(P);: Locate , 34
    Print Using "##.#"; Z(P)

1432 Next
Locate 13, 41: Color 14: Print "ASSISTS": Color 15
For P = 111 To 120
    If Z2$(P) = "" Then 1433
    Locate , 41: Print Z2$(P) + ", " + Z3$(P);: Locate , 74
    Print Using "####"; Z(P)

1433 Next
GoSub 7900
Cls
Color 14: Print "THREE FIELD GOAL% (MIN 1.5 3FGM/G)": Color 15
For P = 121 To 130
    If Z2$(P) = "" Then 1443
    Print Z2$(P) + ", " + Z3$(P);: Locate , 34
    Print Using "##.#"; Z(P) * 100

1443 Next P
Locate 1, 41
Color 14: Print "MINUTES PER GAME": Color 15
For P = 141 To 150
    If Z2$(P) = "" Then 1449
    Locate , 41: Print Z2$(P) + ", " + Z3$(P);: Locate , 74
    Print Using "##.#"; Z(P)

1449 Next P
Locate 13
Color 14: Print "THREE POINT FIELD GOALS MADE": Color 15
For P = 131 To 140
    If Z2$(P) = "" Then 1450
    Print Z2$(P) + ", " + Z3$(P);: Locate , 34
    Print Using "####"; Z(P)

1450 Next
Locate 13, 41: Color 14: Print "TOTAL MINUTES": Color 15
For P = 151 To 160
    If Z2$(P) = "" Then 1451
    Locate , 41: Print Z2$(P) + ", " + Z3$(P);: Locate , 74
    Print Using "####"; Z(P)

1451 Next
GoSub 7900
Cls
Color 14: Print "REBOUNDS PER GAME   ": Color 15
For P = 161 To 170
    If Z2$(P) = "" Then 1452
    Print Z2$(P) + ", " + Z3$(P);: Locate , 34
    Print Using "##.#"; Z(P)

1452 Next
Locate 1, 41: Color 14: Print "REBOUNDS": Color 15
For P = 171 To 180
    If Z2$(P) = "" Then 1453
    Locate , 41: Print Z2$(P) + ", " + Z3$(P);: Locate , 74
    Print Using "####"; Z(P)

1453 Next
Locate 13: Color 14: Print "BLOCKS PER GAME": Color 15
For P = 181 To 190
    If Z2$(P) = "" Then 1454
    Print Z2$(P) + ", " + Z3$(P);: Locate , 34
    Print Using "##.#"; Z(P)

1454 Next
Locate 13, 41: Color 14: Print "BLOCKS": Color 15
For P = 191 To 200
    If Z2$(P) = "" Then 1455
    Locate , 41: Print Z2$(P) + ", " + Z3$(P);: Locate , 74
    Print Using "####"; Z(P)

1455 Next
GoSub 7900
Cls
Color 14: Print "FIELD GOALS MADE ": Color 15
For P = 201 To 210
    If Z2$(P) = "" Then 1456
    Print Z2$(P) + ", " + Z3$(P);: Locate , 34
    Print Using "####"; Z(P)

1456 Next
Locate 1, 41: Color 14: Print "FREE THROWS MADE": Color 15
For P = 211 To 220
    If Z2$(P) = "" Then 1457
    Locate , 41: Print Z2$(P) + ", " + Z3$(P);: Locate , 74
    Print Using "####"; Z(P)

1457 Next
Locate 13: Color 14: Print "FOULS PER GAME": Color 15
For P = 221 To 230
    If Z2$(P) = "" Then 1458
    Print Z2$(P) + ", " + Z3$(P);: Locate , 34
    Print Using "##.#"; Z(P)

1458 Next
Locate 13, 41: Color 14: Print "FOULS": Color 15
For P = 231 To 240
    If Z2$(P) = "" Then 1459
    Locate , 41: Print Z2$(P) + ", " + Z3$(P);: Locate , 74
    Print Using "####"; Z(P)

1459 Next
GoSub 7900
Cls
Color 14: Print "MOST VALUABLE PLAYER POINTS": Color 15
For P = 241 To 250
    If Z2$(P) = "" Then 1460
    Print Z2$(P) + ", " + Z3$(P);: Locate , 34
    Print Using "####"; Z(P)

1460 Next
GoSub 7900

1900 Cls: Color 12: Print "                         OFFENSIVE TOTALS"

1901 Color 14: Print "             G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG": Color 15

1905 For X = 1 To 20

    1910 If OL$(X) = "" Or OL$(X) = "*" Or OL$(X) = " " Then 1980

    1915 Print Left$(OL$(X), 11); Tab(13);: Print Using "##"; OL(X, 16);: Print Tab(16);: Print Using "####"; OL(X, 2);: Print Tab(21);: Print Using "####"; OL(X, 3);: Print Tab(26);

    1920 Print Using ".###"; OL(X, 17);: Print Tab(31);: Print Using "#### #### .###"; OL(X, 4); OL(X, 5); OL(X, 18);

    1930 Print Tab(46);: Print Using "####"; OL(X, 6);: Print Tab(51);: Print Using "####"; OL(X, 9);: Print Tab(56);: Print Using "####"; OL(X, 7);: Print Tab(61);

    1935 Print Using "###"; OL(X, 8);: Print Tab(65);: Print Using "###"; OL(X, 10);: Print Tab(69);: Print Using "#####"; OL(X, 20);

    1940 Print Tab(75);: Print Using "###.#"; OL(X, 19)

1980 Next: GoSub 7900
Cls: Color 12: Print "                         DEFENSIVE TOTALS"

2901 Color 14: Print "             G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG": Color 15
For X = 1 To 20

    2910 If DL$(X) = "" Or DL$(X) = "*" Or DL$(X) = " " Then 2980

    2915 Print Left$(DL$(X), 11); Tab(13);: Print Using "##"; DL(X, 16);: Print Tab(16);: Print Using "####"; DL(X, 2);: Print Tab(21);: Print Using "####"; DL(X, 3);: Print Tab(26);

    2920 Print Using ".###"; DL(X, 17);: Print Tab(31);: Print Using "#### #### .###"; DL(X, 4); DL(X, 5); DL(X, 18);

    2930 Print Tab(46);: Print Using "#### ####"; DL(X, 6); DL(X, 9);: Print Tab(56);: Print Using "####"; DL(X, 7);: Print Tab(61);

    2935 Print Using "###"; DL(X, 8);: Print Tab(65);: Print Using "###"; DL(X, 10);: Print Tab(69);: Print Using "#####"; DL(X, 20);

    2940 Print Tab(75);: Print Using "###.#"; DL(X, 19)

2980 Next: GoSub 7900

3900 GoSub 4600
U$ = "VIEW LEAGUE STATS AGAIN (YN)"
Locate , (80 - Len(U$)) / 2
Print U$
Color 15, 0

1820 A$ = GetKeyPress$
If UCase$(A$) = "Y" Then 1202
GoTo 1310

2900 Run "HELLO.EXE 3"

3100 Color 15, 0: Cls
JJ = 7
Locate 6, 8: Color 15, 1
Print " Ö"; String$(61, 196); "· ";: For II = 1 To JJ: Locate 6 + II, 8
Print " º"; String$(61, 32); "º ";: Next
Locate 7 + JJ, 8
Print " Ó"; String$(61, 196); "½ ";: Locate 7, 32
Locate , 12: Print "ENTER NAME FOR FILE: ": Print
Locate , 12: Print "THE NAME IS 8 CHARACTERS LONG WITH NO PERIODS OR"
Locate , 12: Print "SPACES. DO NOT ADD A FILE EXTENSION, THE PROGRAM"
Locate , 12: Print "WILL AUTOMATICALLY ADD .RPT"
Locate 7, 37: Input SF$
If SF$ = "" Then 3100
Locate 13, 12: Print "ANY CHANGE (YN) "
705 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 3100
Color 15, 0

3200 Cls
If X1 <> 1 Then Open "REPORT" For Output As #2 Else Open SF$ + ".RPT" For Output As #2
Print #2, FF$ + " CONFERENCE"
If D1$ <> "" Then Print #2, D1$; " DIVISION"
Print #2, "                 CONFERENCE                         FULL SEASON"
Print #2, "                W    L     PCT                          W    L     PCT"
For P = 1 To 15
    If T$(P) = "" Or T$(P) = "*" Then 3256
    Print #2, T$(P); Tab(16);
    Print #2, Using "##   ##   "; O4(P); O5(P);
    If O6(P) = 0 And O4(P) = 0 And O5(P) = 0 Then Print #2, " .---";
    If O6(P) = 0 And (O4(P) <> 0 Or O5(P) <> 0) Then Print #2, " .000";
    If O6(P) = 1 Then Print #2, "1.000";
    If O6(P) <> 0 And O6(P) <> 1 Then Print #2, Using " .###"; O6(P);
    Print #2, Tab(41);
    Print #2, T$(P); Tab(56);
    Print #2, Using "##   ##   "; O1(P); O2(P);
    If O3(P) = 0 And O1(P) = 0 And O2(P) = 0 Then Print #2, " .---"
    If O3(P) = 0 And (O1(P) <> 0 Or O2(P) <> 0) Then Print #2, " .000"
    If O3(P) = 1 Then Print #2, "1.000"
    If O3(P) <> 0 And O3(P) <> 1 Then Print #2, Using " .###"; O3(P)

3256 Next P
Print #2,
Print #2, FF$ + " CONFERENCE"
If D2$ <> "" Then Print #2, D2$; " DIVISION"
Print #2, "                 CONFERENCE                         FULL SEASON"
Print #2, "                W    L     PCT                          W    L     PCT"
For P = 16 To 30
    If T$(P) = "" Or T$(P) = "*" Then 3258
    Print #2, T$(P); Tab(16);
    Print #2, Using "##   ##   "; O4(P); O5(P);
    If O6(P) = 0 And O4(P) = 0 And O5(P) = 0 Then Print #2, " .---";
    If O6(P) = 0 And (O4(P) <> 0 Or O5(P) <> 0) Then Print #2, " .000";
    If O6(P) = 1 Then Print #2, "1.000";
    If O6(P) <> 0 And O6(P) <> 1 Then Print #2, Using " .###"; O6(P);
    Print #2, Tab(41);
    Print #2, T$(P); Tab(56);
    Print #2, Using "##   ##   "; O1(P); O2(P);
    If O3(P) = 0 And O1(P) = 0 And O2(P) = 0 Then Print #2, " .---"
    If O3(P) = 0 And (O1(P) <> 0 Or O2(P) <> 0) Then Print #2, " .000"
    If O3(P) = 1 Then Print #2, "1.000"
    If O3(P) <> 0 And O3(P) <> 1 Then Print #2, Using " .###"; O3(P)

3258 Next P
Print #2, Chr$(12)
Print #2, "FIELD GOAL % (MIN 5 FGM/G)"; Tab(41); "FREE THROW % (MIN 2.5 FTM/G)"
Print #2,
Print #2, "                            PCT   FGA"; Tab(41); "                            PCT   FTA"
For P = 41 To 60
    If Z2$(P) = "" Then
        Print #2, Tab(41);
    Else
        Print #2, Left$(Z2$(P), 10) + ", " + Z3$(P);
        Print #2, Tab(28);
        Print #2, Using "##.#  ####"; Z(P) * 100; Z1(P);
        Print #2, Tab(41);
    End If
    If Z2$(P + 20) = "" Then
        Print #2,
        GoTo 3320
    Else
        Print #2, Left$(Z2$(P + 20), 10) + ", " + Z3$(P + 20);
        Print #2, Tab(67);
        Print #2, Using "###.#  ####"; Z(P + 20) * 100; Z1(P)
    End If

3320 Next
Print #2,
Print #2, "SCORING"; Tab(41); "TOTAL POINTS"
For P = 1 To 20
    If Z2$(P) = "" Then
        Print #2, Tab(41);
    Else
        Print #2, Z2$(P) + ", " + Z3$(P);
        Print #2, Tab(34);
        Print #2, Using "##.#"; Z(P);
        Print #2, Tab(41);
    End If
    If Z2$(P + 20) = "" Then
        Print #2,
        GoTo 3340
    Else
        Print #2, Z2$(P + 20) + ", " + Z3$(P + 20);
        Print #2, Tab(74);
        Print #2, Using "####"; Z(P + 20)
    End If

3340 Next
Print #2, Chr$(12)
Print #2, "STEALS PER GAME"; Tab(41); "STEALS"
For P = 81 To 90
    If Z2$(P) = "" Then
        Print #2, Tab(41);
    Else
        Print #2, Z2$(P) + ", " + Z3$(P);
        Print #2, Tab(34);
        Print #2, Using "##.#"; Z(P);
        Print #2, Tab(41);
    End If
    If Z2$(P + 10) = "" Then
        Print #2,
        GoTo 3360
    Else
        Print #2, Z2$(P + 10) + ", " + Z3$(P + 10);
        Print #2, Tab(74);
        Print #2, Using "####"; Z(P + 10)
    End If

3360 Next
Print #2,
Print #2, "ASSISTS PER GAME"; Tab(41); "ASSISTS"
For P = 101 To 110
    If Z2$(P) = "" Then
        Print #2, Tab(41);
    Else
        Print #2, Z2$(P) + ", " + Z3$(P);
        Print #2, Tab(34);
        Print #2, Using "##.#"; Z(P);
        Print #2, Tab(41);
    End If
    If Z2$(P + 10) = "" Then
        Print #2,
        GoTo 3370
    Else
        Print #2, Z2$(P + 10) + ", " + Z3$(P + 10);
        Print #2, Tab(74);
        Print #2, Using "####"; Z(P + 10)
    End If

3370 Next
Print #2,
Print #2, "THREE FIELD GOAL % (MIN 1.5 3FGM/G)"; Tab(41); "MINUTES PER GAME"
For P = 121 To 130
    If Z2$(P) = "" Then
        Print #2, Tab(41);
    Else
        Print #2, Z2$(P) + ", " + Z3$(P);
        Print #2, Tab(34);
        Print #2, Using "##.#"; Z(P) * 100;
        Print #2, Tab(41);
    End If
    If Z2$(P + 20) = "" Then
        Print #2,
        GoTo 3361
    Else
        Print #2, Z2$(P + 20) + ", " + Z3$(P + 20);
        Print #2, Tab(74);
        Print #2, Using "##.#"; Z(P + 20)
    End If

3361 Next
Print #2,
Print #2, "THREE POINT FIELD GOALS MADE"; Tab(41); "MINUTES"
For P = 131 To 140
    If Z2$(P) = "" Then
        Print #2, Tab(41);
    Else
        Print #2, Z2$(P) + ", " + Z3$(P);
        Print #2, Tab(34);
        Print #2, Using "####"; Z(P);
        Print #2, Tab(41);
    End If
    If Z2$(P + 20) = "" Then
        Print #2,
        GoTo 3371
    Else
        Print #2, Z2$(P + 20) + ", " + Z3$(P + 20);
        Print #2, Tab(74);
        Print #2, Using "####"; Z(P + 20)
    End If

3371 Next
Print #2,
Print #2, Chr$(12)
Print #2, "REBOUNDS PER GAME"; Tab(41); "REBOUNDS"
For P = 161 To 170
    If Z2$(P) = "" Then
        Print #2, Tab(41);
    Else
        Print #2, Z2$(P) + ", " + Z3$(P);
        Print #2, Tab(34);
        Print #2, Using "##.#"; Z(P);
        Print #2, Tab(41);
    End If
    If Z2$(P + 10) = "" Then
        Print #2,
        GoTo 3362
    Else
        Print #2, Z2$(P + 10) + ", " + Z3$(P + 10);
        Print #2, Tab(74);
        Print #2, Using "####"; Z(P + 10)
    End If

3362 Next
Print #2,
Print #2, "BLOCKS PER GAME"; Tab(41); "BLOCKS"
For P = 181 To 190
    If Z2$(P) = "" Then
        Print #2, Tab(41);
    Else
        Print #2, Z2$(P) + ", " + Z3$(P);
        Print #2, Tab(34);
        Print #2, Using "##.#"; Z(P);
        Print #2, Tab(41);
    End If
    If Z2$(P + 10) = "" Then
        Print #2,
        GoTo 3372
    Else
        Print #2, Z2$(P + 10) + ", " + Z3$(P + 10);
        Print #2, Tab(74);
        Print #2, Using "####"; Z(P + 20)
    End If

3372 Next
Print #2,
Print #2, "FIELD GOALS MADE"; Tab(41); "FREE THROWS MADE"
For P = 201 To 210
    If Z2$(P) = "" Then
        Print #2, Tab(41);
    Else
        Print #2, Z2$(P) + ", " + Z3$(P);
        Print #2, Tab(34);
        Print #2, Using "####"; Z(P);
        Print #2, Tab(41);
    End If
    If Z2$(P + 10) = "" Then
        Print #2,
        GoTo 3363
    Else
        Print #2, Z2$(P + 10) + ", " + Z3$(P + 10);
        Print #2, Tab(74);
        Print #2, Using "####"; Z(P + 10)
    End If

3363 Next
Print #2,
Print #2, "FOULS PER GAME"; Tab(41); "FOULS"
For P = 221 To 230
    If Z2$(P) = "" Then
        Print #2, Tab(41);
    Else
        Print #2, Z2$(P) + ", " + Z3$(P);
        Print #2, Tab(34);
        Print #2, Using "##.#"; Z(P);
        Print #2, Tab(41);
    End If
    If Z2$(P + 10) = "" Then
        Print #2,
        GoTo 3373
    Else
        Print #2, Z2$(P + 10) + ", " + Z3$(P + 10);
        Print #2, Tab(74);
        Print #2, Using "####"; Z(P + 10)
    End If

3373 Next
Print #2,
Print #2, "MOST VALUABLE PLAYER"
For P = 241 To 250
    If Z2$(P) = "" Then 3364
    Print #2, Z2$(P) + ", " + Z3$(P);
    Print #2, Tab(34);
    Print #2, Using "####"; Z(P)

3364 Next
Print #2,
Print #2, Chr$(12)
Print #2, "                         OFFENSIVE TOTALS"
Print #2, "             G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG"
For X = 1 To 20
    If OL$(X) = "" Or OL$(X) = "*" Or OL$(X) = " " Then 1981
    Print #2, Left$(OL$(X), 11); Tab(13);: Print #2, Using "##"; OL(X, 16);: Print #2, Tab(16);: Print #2, Using "####"; OL(X, 2);: Print #2, Tab(21);: Print #2, Using "####"; OL(X, 3);: Print #2, Tab(26);
    Print #2, Using ".###"; OL(X, 17);: Print #2, Tab(31);: Print #2, Using "#### #### .###"; OL(X, 4); OL(X, 5); OL(X, 18);
    Print #2, Tab(46);: Print #2, Using "####"; OL(X, 6);: Print #2, Tab(51);: Print #2, Using "####"; OL(X, 9);: Print #2, Tab(56);: Print #2, Using "####"; OL(X, 7);: Print #2, Tab(61);
    Print #2, Using "###"; OL(X, 8);: Print #2, Tab(65);: Print #2, Using "###"; OL(X, 10);: Print #2, Tab(69);: Print #2, Using "#####"; OL(X, 20);
    Print #2, Tab(75);: Print #2, Using "###.#"; OL(X, 19)

1981 Next
Print #2,
Print #2, "                         DEFENSIVE TOTALS"
Print #2, "             G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG": Color 15
For X = 1 To 20
    If DL$(X) = "" Or DL$(X) = "*" Or DL$(X) = " " Then 2981
    Print #2, Left$(DL$(X), 11); Tab(13);: Print #2, Using "##"; DL(X, 16);: Print #2, Tab(16);: Print #2, Using "####"; DL(X, 2);: Print #2, Tab(21);: Print #2, Using "####"; DL(X, 3);: Print #2, Tab(26);
    Print #2, Using ".###"; DL(X, 17);: Print #2, Tab(31);: Print #2, Using "#### #### .###"; DL(X, 4); DL(X, 5); DL(X, 18);
    Print #2, Tab(46);: Print #2, Using "#### ####"; DL(X, 6); DL(X, 9);: Print #2, Tab(56);: Print #2, Using "####"; DL(X, 7);: Print #2, Tab(61);
    Print #2, Using "###"; DL(X, 8);: Print #2, Tab(65);: Print #2, Using "###"; DL(X, 10);: Print #2, Tab(69);: Print #2, Using "#####"; DL(X, 20);
    Print #2, Tab(75);: Print #2, Using "###.#"; DL(X, 19)

2981 Next
Close 2
If X1 = 1 Then 3900
Open "REPORT" For Input As #2
Do While Not EOF(2)
    Line Input #2, X$
    LPrint X$
Loop
Close 2
LPrint Chr$(12)
GoTo 3900

7900 HZ = 1: VT = 24
Locate VT, HZ: Print "                          ";

9005 QK$ = GetKeyPress$
Return

4600 JJ = 4
Locate 6, 8: Color 15, 1
Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";
For II = 1 To JJ: Locate 6 + II, 8
Print " º"; String$(61, 32); "º ";: Next
Locate 7 + JJ, 8
Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";: Locate 7
Return

2075 '
Locate 8, 8: Color 15, 1: Print " Ö"; String$(61, 196); "· ";
For II = 1 To J: Locate 8 + II, 8: Print " º"; String$(61, 32); "º ";: Next
Locate 9 + J, 8: Print " Ó"; String$(61, 196); "½ ";: Locate 10, 15: Return


'------------------------------
'      TITLE Subroutine
'------------------------------
' Displays window title / game copyright
Sub TITLE ()
    Color , 2
    Locate 1, 1: Print Tab(80);: Locate , 26: Color 0
    Print "COURTSIDE COLLEGE BASKETBALL STATISTICS"
    Locate 24, 1: Print Tab(80);: Locate , 1
    Print "(C) LANCE HAFFNER/SHANNON LYNN 1991"; Tab(70); Left$(Time$, 5);
    Locate 4, 1: Color 2, 0
End Sub

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
