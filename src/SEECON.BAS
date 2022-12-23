Sub SEECON ()

    _Title "Courtside College Basketball - Conference Stats"

    'Dim seeconP(60), BA$(120), ER$(20), P$(260), S$(60), TT(120, 4)
    Shared DL!(), OL!()
    Shared O1!(), O2!(), O3!(), O4!(), O5!(), O6!()
    Shared seenatZ!(), seenatZ1!()
    Shared OL$(), DL$()
    Shared seenatZ2$(), seenatZ3$()
    Shared T$()

    H$ = " 1/2"

    4000 Spec$ = diskPaths$(1) + "*.LDR"
    Count% = FCount%(Spec$)

    If Count% = 0 Then
        Call CSMsgBox("NO CONFERENCE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)
    Else

        ReDim conferenceLeaderFiles$(0 To Count%)
        For X = 1 To Count%
            conferenceLeaderFiles$(X) = Space$(12)
        Next
        conferenceLeaderFiles$(0) = Spec$
        Call ReadFile(conferenceLeaderFiles$())

        Do

            Cls
            Color , 1
            Cls
            Locate , 28
            Color 14
            Print "SELECT CONFERENCE TO VIEW": Color 15
            Selection% = 1
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
            Print "ENTER";
			Color 15
            Print "-SELECTS CONFERENCE";

            _MouseShow "DEFAULT"
            MAMenu conferenceLeaderFiles$(), Selection%, START%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
            _MouseHide

            If ExitCode% <> 27 Then

                Color 15, 1
                targetFile$ = conferenceLeaderFiles$(Selection%)
                Locate 2, 33: Print targetFile$

				Do
					Locate , 33: Print "ANY CHANGE (YN)";
					I$ = GetKeyPress$
				Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

				Color 15, 0

            End If

        Loop Until UCase$(I$) = "N" Or ExitCode% = 27

        If ExitCode% <> 27 Then
		
            targetFile$ = RTrim$(targetFile$)
            WE$ = Left$(targetFile$, Len(targetFile$) - 4)
            targetFile$ = Left$(WE$, 8)

            Cls
            Locate 9, 1: Print "      PLEASE WAIT WHILE THE LEADER FILES ARE LOADING!!"

            Open diskPaths$(1) + WE$ + ".LDR" For Input As #2
            For X = 1 To 30: Input #2, T$(X), O1!(X), O2!(X), O3!(X), O4!(X), O5!(X), O6!(X): Next
            For X = 1 To 250: Input #2, seenatZ2$(X), seenatZ3$(X), seenatZ!(X), seenatZ1!(X): Next
            For X = 1 To 40: Input #2, OL$(X): For M = 1 To 20: Input #2, OL!(X, M): Next: Next
            For X = 1 To 40: Input #2, DL$(X): For M = 1 To 20: Input #2, DL!(X, M): Next: Next
            Close #2

            Do

                Cls
                Call TITLE_STATS
                J = 5
                Locate 8, 8
                Color 15, 1
                Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";
                For II = 1 To J
                    Locate 8 + II, 8: Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
                Next
                Locate 9 + J, 8
                Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";
                Locate 10, 15

                Color 7
                Locate , 30: Print "STATS TO ";: Color 15: Print "S";: Color 7: Print "CREEN"
                Locate , 30: Print "STATS TO ";: Color 15: Print "P";: Color 7: Print "RINTER"
                Locate , 30: Print "STATS TO ";: Color 15: Print "F";: Color 7: Print "ILE"
                I$ = GetKeyPress$
                Color 14, 0
                Select Case UCase$(I$)
                    Case "P"
                        Call PrintReport_SEECON(targetFile$, SF$, 0)
                    Case "F"
                        Call GetReportFileName(SF$)
                        Call PrintReport_SEECON(targetFile$, SF$, 1)
                    Case "S"
                        Call DisplayConToScreen(targetFile$)
                End Select

                Cls
                JJ = 4
                Locate 6, 8: Color 15, 1
                Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";
                For II = 1 To JJ: 
                    Locate 6 + II, 8
                    Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
                Next
                Locate 7 + JJ, 8
                Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";

				Do
					Locate 7
					U$ = "VIEW LEAGUE STATS AGAIN (YN)"
					Locate , (80 - Len(U$)) / 2
					Print U$
					Color 15, 0
					A$ = GetKeyPress$
				Loop Until UCase$(A$) = "Y" Or UCase$(A$) = "N"

                'pressing ANY key at this point goes to the first menu
                'need to break this up so that only Y/N is possible???

            Loop Until UCase$(A$) = "N"

        End If 'Done checking for ESC

    End If 'Done checking file count

End Sub

'----------------------------------------
'       DisplayConToScreen Subroutine
'----------------------------------------
'...explanation...
Sub DisplayConToScreen (confFile$)

    Shared DL!(), OL!()
    Shared O1!(), O2!(), O3!(), O4!(), O5!(), O6!()
    Shared seenatZ!(), seenatZ1!()
    Shared OL$(), DL$()
    Shared seenatZ2$(), seenatZ3$()
    Shared T$()

    Open diskPaths$(1) + confFile$ + ".TMS" For Input As #2
    Input #2, FF$, D1$, D2$
    Close 2

    L = 8

    'so, this is basically a pad right function
    While Len(D1$) < L
        D1$ = D1$ + " "
    Wend

    'so, this is basically a pad right function
    While Len(D2$) < L
        D2$ = D2$ + " "
    Wend

    Cls
    'FF$ ---> name of the conference
    'where do we get it from?
    Color 12: Print FF$ + " CONFERENCE"
    If D1$ <> "" Then Print D1$; " DIVISION"
    Print "                 CONFERENCE                         FULL SEASON"
    Color 14
    Print "                W    L     PCT                          W    L     PCT"
    Color 15
    For P = 1 To 15
        If T$(P) = "" Or T$(P) = "*" Then 1256

        1255 Print T$(P); Tab(16);
        Print Using "##   ##   "; O4!(P); O5!(P);

        If O6!(P) = 0 And O4!(P) = 0 And O5!(P) = 0 Then Print " .---";
        If O6!(P) = 0 And (O4!(P) <> 0 Or O5!(P) <> 0) Then Print " .000";
        If O6!(P) = 1 Then Print "1.000";
        If O6!(P) <> 0 And O6!(P) <> 1 Then Print Using " .###"; O6!(P);

        Locate , 41
        Print T$(P); Tab(56);
        Print Using "##   ##   "; O1!(P); O2!(P);

        If O3!(P) = 0 And O1!(P) = 0 And O2!(P) = 0 Then Print " .---"
        If O3!(P) = 0 And (O1!(P) <> 0 Or O2!(P) <> 0) Then Print " .000"
        If O3!(P) = 1 Then Print "1.000"
        If O3!(P) <> 0 And O3!(P) <> 1 Then Print Using " .###"; O3!(P)

    1256 Next P

    1277 '
    HZ = 1: VT = 24
    Locate VT, HZ
    Print Space$(26);
    QK$ = GetKeyPress$

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
        Print Using "##   ##   "; O4!(P); O5!(P);

        If O6!(P) = 0 And O4!(P) = 0 And O5!(P) = 0 Then Print " .---";
        If O6!(P) = 0 And (O4!(P) <> 0 Or O5!(P) <> 0) Then Print " .000";
        If O6!(P) = 1 Then Print "1.000";
        If O6!(P) <> 0 And O6!(P) <> 1 Then Print Using " .###"; O6!(P);

        Locate , 41
        Print T$(P); Tab(56);
        Print Using "##   ##   "; O1!(P); O2!(P);

        If O3!(P) = 0 And O1!(P) = 0 And O2!(P) = 0 Then Print " .---"
        If O3!(P) = 0 And (O1!(P) <> 0 Or O2!(P) <> 0) Then Print " .000"
        If O3!(P) = 1 Then Print "1.000"
        If O3!(P) <> 0 And O3!(P) <> 1 Then Print Using " .###"; O3!(P)

    1258 Next P
    HZ = 1: VT = 24
    Locate VT, HZ
    Print Space$(26);
    QK$ = GetKeyPress$

    Cls
    Color 14
    Print "FIELD GOAL % (MIN 5 FGM/G)"; Tab(41); "FREE THROW % (MIN 2.5 FTM/G)": Print: Color 15
    Print "                            PCT   FGA"; Tab(41); "                            PCT   FTA"
    For P = 41 To 60
        If seenatZ2$(P) <> "" Then
            Print Left$(seenatZ2$(P), 10) + ", " + seenatZ3$(P);: Locate , 28
            'Print Using "##.#  ####"; seenatZ!(P) * 100; seenatZ1!(P)
            Print Using "###.#  ####"; seenatZ!(P) * 100; seenatZ1!(P)
        End If
    Next

    Locate 4
    For P = 61 To 80
        If seenatZ2$(P) <> "" Then
            Locate , 41: Print Left$(seenatZ2$(P), 10) + ", " + seenatZ3$(P);: Locate , 67
            Print Using "###.#  ####"; seenatZ!(P) * 100; seenatZ1!(P)
        End If
    Next

    HZ = 1: VT = 24
    Locate VT, HZ
    Print Space$(26);
    QK$ = GetKeyPress$
    Cls
    Color 14: Print "SCORING"
    Color 14: Print "                                PTS/G": Color 15
    For P = 1 To 20
        If seenatZ2$(P) = "" Then 1330
        Print seenatZ2$(P) + ", " + seenatZ3$(P);: Locate , 34
        Print Using "##.#"; seenatZ!(P)

    1330 Next
    Locate 1, 41: Color 14: Print "TOTAL POINTS"
    Locate , 41: Print "                                  PTS": Color 15
    For P = 21 To 40
        If seenatZ2$(P) = "" Then 1333
        Locate , 41: Print seenatZ2$(P) + ", " + seenatZ3$(P);: Locate , 74
        Print Using "####"; seenatZ!(P)

    1333 Next
    HZ = 1: VT = 24
    Locate VT, HZ
    Print Space$(26);
    QK$ = GetKeyPress$
    Cls
    Color 14: Print "STEALS PER GAME": Color 15
    For P = 81 To 90
        If seenatZ2$(P) = "" Then 1420
        Print seenatZ2$(P) + ", " + seenatZ3$(P);: Locate , 34
        Print Using "##.#"; seenatZ!(P)

    1420 Next
    Locate 1, 41: Color 14: Print "STEALS": Color 15
    For P = 91 To 100
        If seenatZ2$(P) = "" Then 1426
        Locate , 41: Print seenatZ2$(P) + ", " + seenatZ3$(P);: Locate , 74
        Print Using "####"; seenatZ!(P)

    1426 Next
    Locate 13: Color 14: Print "ASSISTS PER GAME": Color 15
    For P = 101 To 110
        If seenatZ2$(P) = "" Then 1432
        Print seenatZ2$(P) + ", " + seenatZ3$(P);: Locate , 34
        Print Using "##.#"; seenatZ!(P)

    1432 Next
    Locate 13, 41: Color 14: Print "ASSISTS": Color 15
    For P = 111 To 120
        If seenatZ2$(P) = "" Then 1433
        Locate , 41: Print seenatZ2$(P) + ", " + seenatZ3$(P);: Locate , 74
        Print Using "####"; seenatZ!(P)

    1433 Next
    HZ = 1: VT = 24
    Locate VT, HZ
    Print Space$(26);
    QK$ = GetKeyPress$
    Cls
    Color 14
    Print "THREE FIELD GOAL% (MIN 1.5 3FGM/G)": Color 15

    For P = 121 To 130
        If seenatZ2$(P) <> "" Then
            Print seenatZ2$(P) + ", " + seenatZ3$(P);: Locate , 34
            Print Using "###.#"; seenatZ!(P) * 100
            'Print Using "##.#"; seenatZ!(P) * 100
        End If
    Next P

    Locate 1, 41
    Color 14: Print "MINUTES PER GAME": Color 15
    For P = 141 To 150
        If seenatZ2$(P) = "" Then 1449
        Locate , 41: Print seenatZ2$(P) + ", " + seenatZ3$(P);: Locate , 74
        Print Using "##.#"; seenatZ!(P)

    1449 Next P
    Locate 13
    Color 14: Print "THREE POINT FIELD GOALS MADE": Color 15
    For P = 131 To 140
        If seenatZ2$(P) = "" Then 1450
        Print seenatZ2$(P) + ", " + seenatZ3$(P);: Locate , 34
        Print Using "####"; seenatZ!(P)

    1450 Next
    Locate 13, 41: Color 14: Print "TOTAL MINUTES": Color 15
    For P = 151 To 160
        If seenatZ2$(P) = "" Then 1451
        Locate , 41: Print seenatZ2$(P) + ", " + seenatZ3$(P);: Locate , 74
        Print Using "####"; seenatZ!(P)

    1451 Next
    HZ = 1: VT = 24
    Locate VT, HZ
    Print Space$(26);
    QK$ = GetKeyPress$
    Cls
    Color 14: Print "REBOUNDS PER GAME   ": Color 15
    For P = 161 To 170
        If seenatZ2$(P) = "" Then 1452
        Print seenatZ2$(P) + ", " + seenatZ3$(P);: Locate , 34
        Print Using "##.#"; seenatZ!(P)

    1452 Next
    Locate 1, 41: Color 14: Print "REBOUNDS": Color 15
    For P = 171 To 180
        If seenatZ2$(P) = "" Then 1453
        Locate , 41: Print seenatZ2$(P) + ", " + seenatZ3$(P);: Locate , 74
        Print Using "####"; seenatZ!(P)

    1453 Next
    Locate 13: Color 14: Print "BLOCKS PER GAME": Color 15
    For P = 181 To 190
        If seenatZ2$(P) = "" Then 1454
        Print seenatZ2$(P) + ", " + seenatZ3$(P);: Locate , 34
        Print Using "##.#"; seenatZ!(P)

    1454 Next
    Locate 13, 41: Color 14: Print "BLOCKS": Color 15
    For P = 191 To 200
        If seenatZ2$(P) = "" Then 1455
        Locate , 41: Print seenatZ2$(P) + ", " + seenatZ3$(P);: Locate , 74
        Print Using "####"; seenatZ!(P)

    1455 Next
    HZ = 1: VT = 24
    Locate VT, HZ
    Print Space$(26);
    QK$ = GetKeyPress$
    Cls
    Color 14: Print "FIELD GOALS MADE ": Color 15
    For P = 201 To 210
        If seenatZ2$(P) = "" Then 1456
        Print seenatZ2$(P) + ", " + seenatZ3$(P);: Locate , 34
        Print Using "####"; seenatZ!(P)

    1456 Next
    Locate 1, 41: Color 14: Print "FREE THROWS MADE": Color 15
    For P = 211 To 220
        If seenatZ2$(P) = "" Then 1457
        Locate , 41: Print seenatZ2$(P) + ", " + seenatZ3$(P);: Locate , 74
        Print Using "####"; seenatZ!(P)

    1457 Next
    Locate 13: Color 14: Print "FOULS PER GAME": Color 15
    For P = 221 To 230
        If seenatZ2$(P) = "" Then 1458
        Print seenatZ2$(P) + ", " + seenatZ3$(P);: Locate , 34
        Print Using "##.#"; seenatZ!(P)

    1458 Next
    Locate 13, 41: Color 14: Print "FOULS": Color 15
    For P = 231 To 240
        If seenatZ2$(P) = "" Then 1459
        Locate , 41: Print seenatZ2$(P) + ", " + seenatZ3$(P);: Locate , 74
        Print Using "####"; seenatZ!(P)

    1459 Next
    HZ = 1: VT = 24
    Locate VT, HZ
    Print Space$(26);
    QK$ = GetKeyPress$
    Cls
    Color 14: Print "MOST VALUABLE PLAYER POINTS": Color 15
    For P = 241 To 250
        If seenatZ2$(P) = "" Then 1460
        Print seenatZ2$(P) + ", " + seenatZ3$(P);: Locate , 34
        Print Using "####"; seenatZ!(P)

    1460 Next
    HZ = 1: VT = 24
    Locate VT, HZ
    Print Space$(26);
    QK$ = GetKeyPress$

    1900 Cls: Color 12: Print "                         OFFENSIVE TOTALS"

    1901 Color 14: Print "             G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG": Color 15

    1905 For X = 1 To 20

        1910 If OL$(X) = "" Or OL$(X) = "*" Or OL$(X) = " " Then 1980

        1915 Print Left$(OL$(X), 11); Tab(13);: Print Using "##"; OL!(X, 16);: Print Tab(16);: Print Using "####"; OL!(X, 2);: Print Tab(21);: Print Using "####"; OL!(X, 3);: Print Tab(26);

        1920 Print Using ".###"; OL!(X, 17);: Print Tab(31);: Print Using "#### #### .###"; OL!(X, 4); OL!(X, 5); OL!(X, 18);

        1930 Print Tab(46);: Print Using "####"; OL!(X, 6);: Print Tab(51);: Print Using "####"; OL!(X, 9);: Print Tab(56);: Print Using "####"; OL!(X, 7);: Print Tab(61);

        1935 Print Using "###"; OL!(X, 8);: Print Tab(65);: Print Using "###"; OL!(X, 10);: Print Tab(69);: Print Using "#####"; OL!(X, 20);

        1940 Print Tab(75);: Print Using "###.#"; OL!(X, 19)

    1980 Next
    HZ = 1: VT = 24
    Locate VT, HZ
    Print Space$(26);
    QK$ = GetKeyPress$

    Cls
    Color 12: Print "                         DEFENSIVE TOTALS"

    2901 Color 14: Print "             G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG": Color 15

    For X = 1 To 20

        2910 If DL$(X) <> "" And DL$(X) <> "*" And DL$(X) <> " " Then
            Print Left$(DL$(X), 11); Tab(13);
            'G
            Print Using "##"; DL!(X, 16);
            Print Tab(16);
            'FGA
            Print Using "####"; DL!(X, 2);
            Print Tab(21);
            'FGM
            Print Using "####"; DL!(X, 3);
            Print Tab(26);
            'FG PCT
            Print Using ".###"; DL!(X, 17);
            Print Tab(31);
            'FTA / FTM / FT PCT
            Print Using "#### #### .###"; DL!(X, 4); DL!(X, 5); DL!(X, 18);
            Print Tab(46);
            'REB / AST
            Print Using "#### ####"; DL!(X, 6); DL!(X, 9);
            Print Tab(56);
            'PF
            Print Using "####"; DL!(X, 7);
            Print Tab(61);
            'STL
            Print Using "###"; DL!(X, 8);
            Print Tab(65);
            'BLK
            Print Using "###"; DL!(X, 10);
            Print Tab(69);
            'PTS
            Print Using "#####"; DL!(X, 20);
            Print Tab(75);
            'AVG
            Print Using "###.#"; DL!(X, 19)
        End If

    2980 Next
    HZ = 1: VT = 24
    Locate VT, HZ
    Print Space$(26);
    QK$ = GetKeyPress$

    'Loop Until UCase$(A$) = "N"

End Sub

'----------------------------------------
'      GetReportFileName Subroutine
'----------------------------------------
'...explanation...
Sub GetReportFileName (reportFile$)

	Color 15, 0
	Cls
	JJ = 7
	Locate 6, 8: Color 15, 1
	Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";: For II = 1 To JJ: Locate 6 + II, 8
	Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";: Next
	Locate 7 + JJ, 8
	Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

	Locate 7, 32

    Do
        Locate , 12: Print "ENTER NAME FOR FILE: ": Print
        Locate , 12: Print "THE NAME IS 8 CHARACTERS LONG WITH NO PERIODS OR"
        Locate , 12: Print "SPACES. DO NOT ADD A FILE EXTENSION, THE PROGRAM"
        Locate , 12: Print "WILL AUTOMATICALLY ADD .RPT"
        Locate 7, 37: Input reportFile$
        
		If SF$ <> "" Then

			Do
				Locate 13, 12: Print "ANY CHANGE (YN) "
				I$ = GetKeyPress$
			Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

		End If

    Loop Until UCase$(I$) = "N" Or SF$ <> ""

    Color 15, 0

End Sub

'----------------------------------------
'     PrintReport_SEECON Subroutine
'----------------------------------------
'This routine opens/creates the target
'   report file and writes data to it.
'   At the end, if specified, this file
'   is sent to the printer.
'
'This saves the effort of needing to
'   have an LPrint and Print for
'   everything
'
'We should do this with other routines
'   and simply use a temp file

Sub PrintReport_SEECON (confFile$, reportFile$, X1)

    Shared DL!(), OL!()
    Shared O1!(), O2!(), O3!(), O4!(), O5!(), O6!()
    Shared seenatZ!(), seenatZ1!()
    Shared OL$(), DL$()
    Shared seenatZ2$(), seenatZ3$()
    Shared T$()

    Open diskPaths$(1) + confFile$ + ".TMS" For Input As #2
    Input #2, FF$, D1$, D2$
    Close 2

    Cls

    If X1 <> 1 Then
        Open "REPORT" For Output As #2
    Else
        Open reportFile$ + ".RPT" For Output As #2
    End If

    Print #2, FF$ + " CONFERENCE"
    If D1$ <> "" Then Print #2, D1$; " DIVISION"
    Print #2, "                 CONFERENCE                         FULL SEASON"
    Print #2, "                W    L     PCT                          W    L     PCT"

    For P = 1 To 15
        If T$(P) = "" Or T$(P) = "*" Then 3256
        Print #2, T$(P); Tab(16);
        Print #2, Using "##   ##   "; O4!(P); O5!(P);
        If O6!(P) = 0 And O4!(P) = 0 And O5!(P) = 0 Then Print #2, " .---";
        If O6!(P) = 0 And (O4!(P) <> 0 Or O5!(P) <> 0) Then Print #2, " .000";
        If O6!(P) = 1 Then Print #2, "1.000";
        If O6!(P) <> 0 And O6!(P) <> 1 Then Print #2, Using " .###"; O6!(P);
        Print #2, Tab(41);
        Print #2, T$(P); Tab(56);
        Print #2, Using "##   ##   "; O1!(P); O2!(P);
        If O3!(P) = 0 And O1!(P) = 0 And O2!(P) = 0 Then Print #2, " .---"
        If O3!(P) = 0 And (O1!(P) <> 0 Or O2!(P) <> 0) Then Print #2, " .000"
        If O3!(P) = 1 Then Print #2, "1.000"
        If O3!(P) <> 0 And O3!(P) <> 1 Then Print #2, Using " .###"; O3!(P)

    3256 Next P

    Print #2,
    Print #2, FF$ + " CONFERENCE"
    If D2$ <> "" Then Print #2, D2$; " DIVISION"
    Print #2, "                 CONFERENCE                         FULL SEASON"
    Print #2, "                W    L     PCT                          W    L     PCT"

    For P = 16 To 30
        If T$(P) = "" Or T$(P) = "*" Then 3258
        Print #2, T$(P); Tab(16);
        Print #2, Using "##   ##   "; O4!(P); O5!(P);
        If O6!(P) = 0 And O4!(P) = 0 And O5!(P) = 0 Then Print #2, " .---";
        If O6!(P) = 0 And (O4!(P) <> 0 Or O5!(P) <> 0) Then Print #2, " .000";
        If O6!(P) = 1 Then Print #2, "1.000";
        If O6!(P) <> 0 And O6!(P) <> 1 Then Print #2, Using " .###"; O6!(P);
        Print #2, Tab(41);
        Print #2, T$(P); Tab(56);
        Print #2, Using "##   ##   "; O1!(P); O2!(P);
        If O3!(P) = 0 And O1!(P) = 0 And O2!(P) = 0 Then Print #2, " .---"
        If O3!(P) = 0 And (O1!(P) <> 0 Or O2!(P) <> 0) Then Print #2, " .000"
        If O3!(P) = 1 Then Print #2, "1.000"
        If O3!(P) <> 0 And O3!(P) <> 1 Then Print #2, Using " .###"; O3!(P)

    3258 Next P

    Print #2, Chr$(12)
    Print #2, "FIELD GOAL % (MIN 5 FGM/G)"; Tab(41); "FREE THROW % (MIN 2.5 FTM/G)"
    Print #2,
    Print #2, "                            PCT   FGA"; Tab(41); "                            PCT   FTA"

    For P = 41 To 60
        If seenatZ2$(P) = "" Then
            Print #2, Tab(41);
        Else
            Print #2, Left$(seenatZ2$(P), 10) + ", " + seenatZ3$(P);
            Print #2, Tab(28);
            Print #2, Using "##.#  ####"; seenatZ!(P) * 100; seenatZ1!(P);
            Print #2, Tab(41);
        End If
        If seenatZ2$(P + 20) = "" Then
            Print #2,
        Else
            Print #2, Left$(seenatZ2$(P + 20), 10) + ", " + seenatZ3$(P + 20);
            Print #2, Tab(67);
            Print #2, Using "###.#  ####"; seenatZ!(P + 20) * 100; seenatZ1!(P)
        End If

    3320 Next

    Print #2,
    Print #2, "SCORING"; Tab(41); "TOTAL POINTS"

    For P = 1 To 20
        If seenatZ2$(P) = "" Then
            Print #2, Tab(41);
        Else
            Print #2, seenatZ2$(P) + ", " + seenatZ3$(P);
            Print #2, Tab(34);
            Print #2, Using "##.#"; seenatZ!(P);
            Print #2, Tab(41);
        End If
        If seenatZ2$(P + 20) = "" Then
            Print #2,
        Else
            Print #2, seenatZ2$(P + 20) + ", " + seenatZ3$(P + 20);
            Print #2, Tab(74);
            Print #2, Using "####"; seenatZ!(P + 20)
        End If

    Next

    Print #2, Chr$(12)
    Print #2, "STEALS PER GAME"; Tab(41); "STEALS"

    For P = 81 To 90
        If seenatZ2$(P) = "" Then
            Print #2, Tab(41);
        Else
            Print #2, seenatZ2$(P) + ", " + seenatZ3$(P);
            Print #2, Tab(34);
            Print #2, Using "##.#"; seenatZ!(P);
            Print #2, Tab(41);
        End If
        If seenatZ2$(P + 10) = "" Then
            Print #2,
        Else
            Print #2, seenatZ2$(P + 10) + ", " + seenatZ3$(P + 10);
            Print #2, Tab(74);
            Print #2, Using "####"; seenatZ!(P + 10)
        End If

    3360 Next

    Print #2,
    Print #2, "ASSISTS PER GAME"; Tab(41); "ASSISTS"

    For P = 101 To 110
        If seenatZ2$(P) = "" Then
            Print #2, Tab(41);
        Else
            Print #2, seenatZ2$(P) + ", " + seenatZ3$(P);
            Print #2, Tab(34);
            Print #2, Using "##.#"; seenatZ!(P);
            Print #2, Tab(41);
        End If
        If seenatZ2$(P + 10) = "" Then
            Print #2,
        Else
            Print #2, seenatZ2$(P + 10) + ", " + seenatZ3$(P + 10);
            Print #2, Tab(74);
            Print #2, Using "####"; seenatZ!(P + 10)
        End If

    3370 Next

    Print #2,
    Print #2, "THREE FIELD GOAL % (MIN 1.5 3FGM/G)"; Tab(41); "MINUTES PER GAME"

    For P = 121 To 130
        If seenatZ2$(P) = "" Then
            Print #2, Tab(41);
        Else
            Print #2, seenatZ2$(P) + ", " + seenatZ3$(P);
            Print #2, Tab(34);
            Print #2, Using "##.#"; seenatZ!(P) * 100;
            Print #2, Tab(41);
        End If
        If seenatZ2$(P + 20) = "" Then
            Print #2,
        Else
            Print #2, seenatZ2$(P + 20) + ", " + seenatZ3$(P + 20);
            Print #2, Tab(74);
            Print #2, Using "##.#"; seenatZ!(P + 20)
        End If

    3361 Next

    Print #2,
    Print #2, "THREE POINT FIELD GOALS MADE"; Tab(41); "MINUTES"

    For P = 131 To 140
        If seenatZ2$(P) = "" Then
            Print #2, Tab(41);
        Else
            Print #2, seenatZ2$(P) + ", " + seenatZ3$(P);
            Print #2, Tab(34);
            Print #2, Using "####"; seenatZ!(P);
            Print #2, Tab(41);
        End If
        If seenatZ2$(P + 20) = "" Then
            Print #2,
        Else
            Print #2, seenatZ2$(P + 20) + ", " + seenatZ3$(P + 20);
            Print #2, Tab(74);
            Print #2, Using "####"; seenatZ!(P + 20)
        End If

    3371 Next

    Print #2,
    Print #2, Chr$(12)
    Print #2, "REBOUNDS PER GAME"; Tab(41); "REBOUNDS"

    For P = 161 To 170
        If seenatZ2$(P) = "" Then
            Print #2, Tab(41);
        Else
            Print #2, seenatZ2$(P) + ", " + seenatZ3$(P);
            Print #2, Tab(34);
            Print #2, Using "##.#"; seenatZ!(P);
            Print #2, Tab(41);
        End If
        If seenatZ2$(P + 10) = "" Then
            Print #2,
        Else
            Print #2, seenatZ2$(P + 10) + ", " + seenatZ3$(P + 10);
            Print #2, Tab(74);
            Print #2, Using "####"; seenatZ!(P + 10)
        End If

    3362 Next

    Print #2,
    Print #2, "BLOCKS PER GAME"; Tab(41); "BLOCKS"

    For P = 181 To 190
        If seenatZ2$(P) = "" Then
            Print #2, Tab(41);
        Else
            Print #2, seenatZ2$(P) + ", " + seenatZ3$(P);
            Print #2, Tab(34);
            Print #2, Using "##.#"; seenatZ!(P);
            Print #2, Tab(41);
        End If
        If seenatZ2$(P + 10) = "" Then
            Print #2,
        Else
            Print #2, seenatZ2$(P + 10) + ", " + seenatZ3$(P + 10);
            Print #2, Tab(74);
            Print #2, Using "####"; seenatZ!(P + 20)
        End If

    3372 Next

    Print #2,
    Print #2, "FIELD GOALS MADE"; Tab(41); "FREE THROWS MADE"

    For P = 201 To 210
        If seenatZ2$(P) = "" Then
            Print #2, Tab(41);
        Else
            Print #2, seenatZ2$(P) + ", " + seenatZ3$(P);
            Print #2, Tab(34);
            Print #2, Using "####"; seenatZ!(P);
            Print #2, Tab(41);
        End If
        If seenatZ2$(P + 10) = "" Then
            Print #2,
        Else
            Print #2, seenatZ2$(P + 10) + ", " + seenatZ3$(P + 10);
            Print #2, Tab(74);
            Print #2, Using "####"; seenatZ!(P + 10)
        End If

    3363 Next
    Print #2,
    Print #2, "FOULS PER GAME"; Tab(41); "FOULS"

    For P = 221 To 230
        If seenatZ2$(P) = "" Then
            Print #2, Tab(41);
        Else
            Print #2, seenatZ2$(P) + ", " + seenatZ3$(P);
            Print #2, Tab(34);
            Print #2, Using "##.#"; seenatZ!(P);
            Print #2, Tab(41);
        End If
        If seenatZ2$(P + 10) = "" Then
            Print #2,
        Else
            Print #2, seenatZ2$(P + 10) + ", " + seenatZ3$(P + 10);
            Print #2, Tab(74);
            Print #2, Using "####"; seenatZ!(P + 10)
        End If

    3373 Next

    Print #2,
    Print #2, "MOST VALUABLE PLAYER"

    For P = 241 To 250
        If seenatZ2$(P) <> "" Then
            Print #2, seenatZ2$(P) + ", " + seenatZ3$(P);
            Print #2, Tab(34);
            Print #2, Using "####"; seenatZ!(P)
        End If
    3364 Next

    Print #2,
    Print #2, Chr$(12)
    Print #2, "                         OFFENSIVE TOTALS"
    Print #2, "             G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG"
    For X = 1 To 20
        If OL$(X) = "" Or OL$(X) = "*" Or OL$(X) = " " Then 1981
        Print #2, Left$(OL$(X), 11); Tab(13);: Print #2, Using "##"; OL!(X, 16);: Print #2, Tab(16);: Print #2, Using "####"; OL!(X, 2);: Print #2, Tab(21);: Print #2, Using "####"; OL!(X, 3);: Print #2, Tab(26);
        Print #2, Using ".###"; OL!(X, 17);: Print #2, Tab(31);: Print #2, Using "#### #### .###"; OL!(X, 4); OL!(X, 5); OL!(X, 18);
        Print #2, Tab(46);: Print #2, Using "####"; OL!(X, 6);: Print #2, Tab(51);: Print #2, Using "####"; OL!(X, 9);: Print #2, Tab(56);: Print #2, Using "####"; OL!(X, 7);: Print #2, Tab(61);
        Print #2, Using "###"; OL!(X, 8);: Print #2, Tab(65);: Print #2, Using "###"; OL!(X, 10);: Print #2, Tab(69);: Print #2, Using "#####"; OL!(X, 20);
        Print #2, Tab(75);: Print #2, Using "###.#"; OL!(X, 19)

    1981 Next

    Print #2,
    Print #2, "                         DEFENSIVE TOTALS"
    Print #2, "             G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG": Color 15

    For X = 1 To 20
        If DL$(X) <> "" And DL$(X) <> "*" And DL$(X) <> " " Then
            Print #2, Left$(DL$(X), 11); Tab(13);: Print #2, Using "##"; DL!(X, 16);: Print #2, Tab(16);: Print #2, Using "####"; DL!(X, 2);: Print #2, Tab(21);: Print #2, Using "####"; DL!(X, 3);: Print #2, Tab(26);
            Print #2, Using ".###"; DL!(X, 17);: Print #2, Tab(31);: Print #2, Using "#### #### .###"; DL!(X, 4); DL!(X, 5); DL!(X, 18);
            Print #2, Tab(46);: Print #2, Using "#### ####"; DL!(X, 6); DL!(X, 9);: Print #2, Tab(56);: Print #2, Using "####"; DL!(X, 7);: Print #2, Tab(61);
            Print #2, Using "###"; DL!(X, 8);: Print #2, Tab(65);: Print #2, Using "###"; DL!(X, 10);: Print #2, Tab(69);: Print #2, Using "#####"; DL!(X, 20);
            Print #2, Tab(75);: Print #2, Using "###.#"; DL!(X, 19)
        End If
    2981 Next

    Close 2

    'At this point, we just print the entire file
    'ASSUMED:
    '   X1 = 1 means we're not sending to printer
    If X1 <> 1 Then
        Open "REPORT" For Input As #2
        Do While Not EOF(2)
            Line Input #2, X$
            LPrint X$
        Loop
        Close 2
        LPrint Chr$(12)
    End If

End Sub
