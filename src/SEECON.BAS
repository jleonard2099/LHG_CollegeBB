'----------------------------------------
'            SEECON Subroutine
'----------------------------------------
'This subroutine lets the user choose a
'conference stat file to view, then calls
'other routines to both load the Stats
'and then provide options for display
Sub SEECON ()

    _Title "Courtside College Basketball - View Conference Stats"

    leaderFile$ = ChooseFile$(diskPaths$(2) + "*.LDR", "LEADER", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Call LoadLeaderFiles_Coll(leaderFile$, FF$, D1$, D2$)
        Call StatOptions_Con(FF$, D1$, D2$)

    End If

End Sub


'----------------------------------------
'     LoadLeaderFiles_Coll Subroutine
'----------------------------------------
'This subroutine loads the conference stats
'from the file into several arrays
Sub LoadLeaderFiles_Coll (targetFile$, FF$, D1$, D2$)

    Shared defLeaders!(), offLeaders!()
    Shared O1!(), O2!(), O3!(), O4!(), O5!(), O6!()
    Shared seeZ!(), seeZ1!()
    Shared offLeaderNames$(), deffLeaderNames$()
    Shared seeZ2$(), seeZ3$()
    Shared seeT$()

    Cls

    Locate 9, 1: Print "      PLEASE WAIT WHILE THE LEADER FILES ARE LOADING!!"

    Open diskPaths$(2) + targetFile$ + ".LDR" For Input As #1

    For X = 1 To TEAMS_PER_CONFERENCE:
        Input #1, seeT$(X), O1!(X), O2!(X), O3!(X), O4!(X), O5!(X), O6!(X)
    Next

    For X = 1 To 250:
        Input #1, seeZ2$(X), seeZ3$(X), seeZ!(X), seeZ1!(X)
    Next

    For X = 1 To MAX_CONFERENCES:
        Input #1, offLeaderNames$(X)
        For M = 1 To 20:
            Input #1, offLeaders!(X, M)
        Next
    Next

    For X = 1 To MAX_CONFERENCES:
        Input #1, deffLeaderNames$(X)
        For M = 1 To 20:
            Input #1, defLeaders!(X, M)
        Next
    Next

    Close #1

    Open diskPaths$(2) + targetFile$ + ".TMS" For Input As #2
    Input #2, FF$, D1$, D2$
    Close #2

    D1$ = PadRight$(D1$, 8)
    D2$ = PadRight$(D2$, 8)

    Cls
    Locate 9, 1: Print "      PLEASE WAIT WHILE THE LEADER FILES ARE LOADING!!"

End Sub


'----------------------------------------
'       StatOptions_Con Subroutine
'----------------------------------------
'This subroutine gives the user various
'options for viewing the previously loaded
'stats, and then calls routines for each
Sub StatOptions_Con (confName$, D1$, D2$)

    Do

        Cls
        Call TITLE_STATS

        Call MenuBox(8, 25, 24, 5, 15, 1)

        Do

            Locate 10
            Color 7, 1

            Locate , 32: Print "SEND TO ";
            Color 15: Print "F";
            Color 7: Print "ILE"
            Locate , 32: Print "SEND TO ";
            Color 15: Print "P";
            Color 7: Print "RINTER"
            Locate , 32: Print "SEND TO ";
            Color 15: Print "S";
            Color 7: Print "CREEN"
            I$ = GetKeyPress$

            Color 14, 0

        Loop Until UCase$(I$) = "F" Or UCase$(I$) = "P" Or UCase$(I$) = "S" Or I$ = Chr$(27)

        If I$ <> Chr$(27) Then

            Select Case UCase$(I$)

                Case "F"
                    userFile$ = _SAVEFILEDIALOG$("Save Conference Report",_CWD$ + "\Report","*.txt","Text File")
                    If userFile$ <> "" Then
                        Call LeaderStatsCon_Report(confName$, D1$, D2$, 0, userFile$)
                    End If

                Case "P"
                    Call LeaderStatsCon_Report(confName$, D1$, D2$, 1, "REPORT")

                Case "S"
                    Call LeaderStatsCon_Screen(confName$, D1$, D2$)

            End Select

            Color 15, 0
            Cls

            Call box0(6, 8, 11, 70, 2, 15, 1)

            Do
                Color 15, 1
                Locate 8
                U$ = "VIEW CONFERENCE STATS AGAIN? (Y/N)"
                Locate , (80 - Len(U$)) / 2
                Print U$
                Color 15, 0
                A$ = GetKeyPress$
            Loop Until UCase$(A$) = "Y" Or UCase$(A$) = "N"

        End If

    Loop Until UCase$(A$) = "N" Or A$ = Chr$(27) Or I$ = Chr$(27)

End Sub


'----------------------------------------
'       LeaderStatsCon_Screen Subroutine
'----------------------------------------
'This subroutine formats the previously
'loaded stats and outputs them to the end
'user display.
Sub LeaderStatsCon_Screen (confName$, D1$, D2$)

    Shared defLeaders!(), offLeaders!()
    Shared O1!(), O2!(), O3!(), O4!(), O5!(), O6!()
    Shared seeZ!(), seeZ1!()
    Shared offLeaderNames$(), deffLeaderNames$()
    Shared seeZ2$(), seeZ3$()
    Shared seeT$()

    Color 15, 0
    Cls

    Color 12
    Locate , Int((80 - Len(confName$) - 11) / 2)
    Print confName$ + " CONFERENCE"

    Color 2
    If D1$ <> "" Then Print RTrim$(D1$); " DIVISION"
    Color 12
    Print "                 CONFERENCE                         FULL SEASON"
    Color 14
    Print "                W    L     PCT                          W    L     PCT"
    Color 15

    For P = 1 To 15

        If seeT$(P) <> "" And seeT$(P) <> "*" Then

            Print seeT$(P); Tab(16);

            'Conference Record
            Print Using "##   ##   "; O4!(P); O5!(P);

            If O6!(P) = 0 And O4!(P) = 0 And O5!(P) = 0 Then Print " .---";
            If O6!(P) = 0 And (O4!(P) <> 0 Or O5!(P) <> 0) Then Print " .000";
            If O6!(P) = 1 Then Print "1.000";
            If O6!(P) <> 0 And O6!(P) <> 1 Then Print Using " .###"; O6!(P);

            'Season Record
            Locate , 41
            Print seeT$(P); Tab(56);
            Print Using "##   ##   "; O1!(P); O2!(P);

            If O3!(P) = 0 And O1!(P) = 0 And O2!(P) = 0 Then Print " .---"
            If O3!(P) = 0 And (O1!(P) <> 0 Or O2!(P) <> 0) Then Print " .000"
            If O3!(P) = 1 Then Print "1.000"
            If O3!(P) <> 0 And O3!(P) <> 1 Then Print Using " .###"; O3!(P)

        End If

    Next P

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 12
    Locate , Int((80 - Len(confName$) - 11) / 2)
    Print confName$ + " CONFERENCE"

    Color 2
    If D2$ <> "" Then Print RTrim$(D2$); " DIVISION"
    Print "                 CONFERENCE                         FULL SEASON"
    Color 14
    Print "                W    L     PCT                          W    L     PCT"
    Color 15

    For P = 16 To 30

        If seeT$(P) <> "" And seeT$(P) <> "*" Then
            Print seeT$(P); Tab(16);
            Print Using "##   ##   "; O4!(P); O5!(P);

            If O6!(P) = 0 And O4!(P) = 0 And O5!(P) = 0 Then Print " .---";
            If O6!(P) = 0 And (O4!(P) <> 0 Or O5!(P) <> 0) Then Print " .000";
            If O6!(P) = 1 Then Print "1.000";
            If O6!(P) <> 0 And O6!(P) <> 1 Then Print Using " .###"; O6!(P);

            Locate , 41
            Print seeT$(P); Tab(56);
            Print Using "##   ##   "; O1!(P); O2!(P);

            If O3!(P) = 0 And O1!(P) = 0 And O2!(P) = 0 Then Print " .---"
            If O3!(P) = 0 And (O1!(P) <> 0 Or O2!(P) <> 0) Then Print " .000"
            If O3!(P) = 1 Then Print "1.000"
            If O3!(P) <> 0 And O3!(P) <> 1 Then Print Using " .###"; O3!(P)
        End If

    Next P

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "FIELD GOAL % (MIN 5 FGA/G)"
    Print "                            PCT   FGA"
    Color 15

    For P = 41 To 60
        If seeZ2$(P) <> "" Then
            Print Left$(seeZ2$(P), 10) + ", " + seeZ3$(P);
            Locate , 28
            Print Using "###.#  ####"; seeZ!(P) * 100; seeZ1!(P)
        End If
    Next

    Color 14
    Locate 1, 41
    Print "FREE THROW % (MIN 2 FTA/G)"
    Locate , 41
    Print "                            PCT   FTA"
    Color 15

    For P = 61 To 80
        If seeZ2$(P) <> "" Then
            Locate , 41
            Print Left$(seeZ2$(P), 10) + ", " + seeZ3$(P);
            Locate , 68
            Print Using "###.#  ####"; seeZ!(P) * 100; seeZ1!(P)
        End If
    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "SCORING"
    Print "                                PTS/G"
    Color 15

    For P = 1 To 20
        If seeZ2$(P) <> "" Then
            Print seeZ2$(P) + ", " + seeZ3$(P);
            Locate , 34
            Print Using "##.#"; seeZ!(P)
        End If
    Next

    Locate 1, 41
    Color 14
    Print "TOTAL POINTS"
    Locate , 41
    Print "                                  PTS"
    Color 15

    For P = 21 To 40
        If seeZ2$(P) <> "" Then
            Locate , 41: Print seeZ2$(P) + ", " + seeZ3$(P);
            Locate , 74
            Print Using "####"; seeZ!(P)
        End If
    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "STEALS PER GAME"
    Color 15

    For P = 81 To 90
        If seeZ2$(P) <> "" Then
            Print seeZ2$(P) + ", " + seeZ3$(P);
            Locate , 34
            Print Using "##.#"; seeZ!(P)
        End If
    Next

    Locate 1, 41
    Color 14
    Print "STEALS"
    Color 15

    For P = 91 To 100

        If seeZ2$(P) <> "" Then
            Locate , 41: Print seeZ2$(P) + ", " + seeZ3$(P);
            Locate , 74
            Print Using "####"; seeZ!(P)
        End If

    Next

    Color 14
    Locate 13: Print "ASSISTS PER GAME"
    Color 15

    For P = 101 To 110
        If seeZ2$(P) <> "" Then
            Print seeZ2$(P) + ", " + seeZ3$(P);
            Locate , 34
            Print Using "##.#"; seeZ!(P)
        End If

    Next

    Locate 13, 41
    Color 14
    Print "ASSISTS"
    Color 15

    For P = 111 To 120
        If seeZ2$(P) <> "" Then
            Locate , 41: Print seeZ2$(P) + ", " + seeZ3$(P);
            Locate , 74
            Print Using "####"; seeZ!(P)
        End If

    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "THREE FIELD GOAL% (MIN 1.5 3FGM/G)"
    Color 15

    For P = 121 To 130
        If seeZ2$(P) <> "" Then
            Print seeZ2$(P) + ", " + seeZ3$(P);
            Locate , 34
            Print Using "###.#"; seeZ!(P) * 100
        End If
    Next P

    Locate 1, 41
    Color 14
    Print "MINUTES PER GAME"
    Color 15

    For P = 141 To 150
        If seeZ2$(P) <> "" Then
            Locate , 41: Print seeZ2$(P) + ", " + seeZ3$(P);
            Locate , 74
            Print Using "##.#"; seeZ!(P)
        End If

    Next P

    Locate 13
    Color 14: Print "THREE POINT FIELD GOALS MADE"
    Color 15

    For P = 131 To 140
        If seeZ2$(P) <> "" Then
            Print seeZ2$(P) + ", " + seeZ3$(P);
            Locate , 34
            Print Using "####"; seeZ!(P)
        End If

    Next

    Locate 13, 41
    Color 14
    Print "TOTAL MINUTES"
    Color 15

    For P = 151 To 160
        If seeZ2$(P) <> "" Then
            Locate , 41: Print seeZ2$(P) + ", " + seeZ3$(P);
            Locate , 74
            Print Using "####"; seeZ!(P)
        End If
    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "REBOUNDS PER GAME   "
    Color 15

    For P = 161 To 170
        If seeZ2$(P) <> "" Then
            Print seeZ2$(P) + ", " + seeZ3$(P);
            Locate , 34
            Print Using "##.#"; seeZ!(P)
        End If

    Next

    Locate 1, 41
    Color 14
    Print "REBOUNDS"
    Color 15

    For P = 171 To 180
        If seeZ2$(P) <> "" Then
            Locate , 41: Print seeZ2$(P) + ", " + seeZ3$(P);: Locate , 74
            Print Using "####"; seeZ!(P)
        End If

    Next

    Locate 13: Color 14: Print "BLOCKS PER GAME"
    Color 15

    For P = 181 To 190
        If seeZ2$(P) <> "" Then
            Print seeZ2$(P) + ", " + seeZ3$(P);
            Locate , 34
            Print Using "##.#"; seeZ!(P)
        End If
    Next

    Locate 13, 41
    Color 14
    Print "BLOCKS"
    Color 15

    For P = 191 To 200
        If seeZ2$(P) <> "" Then
            Locate , 41: Print seeZ2$(P) + ", " + seeZ3$(P);
            Locate , 74
            Print Using "####"; seeZ!(P)
        End If

    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "FIELD GOALS MADE "
    Color 15

    For P = 201 To 210
        If seeZ2$(P) <> "" Then
            Print seeZ2$(P) + ", " + seeZ3$(P);
            Locate , 34
            Print Using "####"; seeZ!(P)
        End If

    Next

    Locate 1, 41
    Color 14
    Print "FREE THROWS MADE"
    Color 15

    For P = 211 To 220
        If seeZ2$(P) <> "" Then
            Locate , 41: Print seeZ2$(P) + ", " + seeZ3$(P);
            Locate , 74
            Print Using "####"; seeZ!(P)
        End If
    Next

    Locate 13
    Color 14
    Print "FOULS PER GAME"
    Color 15

    For P = 221 To 230
        If seeZ2$(P) <> "" Then
            Print seeZ2$(P) + ", " + seeZ3$(P);
            Locate , 34
            Print Using "##.#"; seeZ!(P)
        End If

    Next

    Locate 13, 41
    Color 14
    Print "FOULS"
    Color 15

    For P = 231 To 240
        If seeZ2$(P) <> "" Then
            Locate , 41: Print seeZ2$(P) + ", " + seeZ3$(P);
            Locate , 74
            Print Using "####"; seeZ!(P)
        End If

    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "MOST VALUABLE PLAYER POINTS"
    Color 15

    For P = 241 To 250
        If seeZ2$(P) <> "" Then
            Print seeZ2$(P) + ", " + seeZ3$(P);
            Locate , 34
            Print Using "####"; seeZ!(P)
        End If

    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 12
    Print "                         OFFENSIVE TOTALS"
    Color 14
    Print "             G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG"
    Color 15

    For X = 1 To 20

        If offLeaderNames$(X) <> "" And offLeaderNames$(X) <> "*" And offLeaderNames$(X) <> " " Then

            Print Left$(offLeaderNames$(X), 11); Tab(13);: Print Using "##"; offLeaders!(X, 16);: Print Tab(16);: Print Using "####"; offLeaders!(X, 2);: Print Tab(21);: Print Using "####"; offLeaders!(X, 3);: Print Tab(26);

            Print Using ".###"; offLeaders!(X, 17);: Print Tab(31);: Print Using "#### #### .###"; offLeaders!(X, 4); offLeaders!(X, 5); offLeaders!(X, 18);

            Print Tab(46);: Print Using "####"; offLeaders!(X, 6);: Print Tab(51);: Print Using "####"; offLeaders!(X, 9);: Print Tab(56);: Print Using "####"; offLeaders!(X, 7);: Print Tab(61);

            Print Using "###"; offLeaders!(X, 8);: Print Tab(65);: Print Using "###"; offLeaders!(X, 10);: Print Tab(69);: Print Using "#####"; offLeaders!(X, 20);

            Print Tab(75);: Print Using "###.#"; offLeaders!(X, 19)

        End If

    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 12
    Print "                         DEFENSIVE TOTALS"
    Color 14
    Print "             G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG"
    Color 15

    For X = 1 To 20

        If deffLeaderNames$(X) <> "" And deffLeaderNames$(X) <> "*" And deffLeaderNames$(X) <> " " Then
            Print Left$(deffLeaderNames$(X), 11); Tab(13);
            'G
            Print Using "##"; defLeaders!(X, 16);
            Print Tab(16);
            'FGA
            Print Using "####"; defLeaders!(X, 2);
            Print Tab(21);
            'FGM
            Print Using "####"; defLeaders!(X, 3);
            Print Tab(26);
            'FG PCT
            Print Using ".###"; defLeaders!(X, 17);
            Print Tab(31);
            'FTA / FTM / FT PCT
            Print Using "#### #### .###"; defLeaders!(X, 4); defLeaders!(X, 5); defLeaders!(X, 18);
            Print Tab(46);
            'REB / AST
            Print Using "#### ####"; defLeaders!(X, 6); defLeaders!(X, 9);
            Print Tab(56);
            'PF
            Print Using "####"; defLeaders!(X, 7);
            Print Tab(61);
            'STL
            Print Using "###"; defLeaders!(X, 8);
            Print Tab(65);
            'BLK
            Print Using "###"; defLeaders!(X, 10);
            Print Tab(69);
            'PTS
            Print Using "#####"; defLeaders!(X, 20);
            Print Tab(75);
            'AVG
            Print Using "###.#"; defLeaders!(X, 19)
        End If

    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

End Sub


'----------------------------------------
'     LeaderStatsCon_Report Subroutine
'----------------------------------------
'This subroutine formats the previously
'loaded stats and outputs them to a file.
'This file can then be looped through to
'send to the default printer
Sub LeaderStatsCon_Report (confName$, D1$, D2$, printDest, destFile$)

    Shared defLeaders!(), offLeaders!()
    Shared O1!(), O2!(), O3!(), O4!(), O5!(), O6!()
    Shared seeZ!(), seeZ1!()

    Shared offLeaderNames$(), deffLeaderNames$()
    Shared seeZ2$(), seeZ3$()
    Shared seeT$()

	Color 15, 0
	Cls

	Open destFile$ For Output As #1

    Print #1, confName$ + " CONFERENCE"

    If D1$ <> "" Then Print #1, D1$; " DIVISION"
    Print #1, "                 CONFERENCE                         FULL SEASON"
    Print #1, "                W    L     PCT                          W    L     PCT"

    For P = 1 To 15

        If seeT$(P) <> "" And seeT$(P) <> "*" Then
            Print #1, seeT$(P); Tab(16);
            Print #1, Using "##   ##   "; O4!(P); O5!(P);

            If O6!(P) = 0 And O4!(P) = 0 And O5!(P) = 0 Then Print #1, " .---";
            If O6!(P) = 0 And (O4!(P) <> 0 Or O5!(P) <> 0) Then Print #1, " .000";
            If O6!(P) = 1 Then Print #1, "1.000";
            If O6!(P) <> 0 And O6!(P) <> 1 Then Print #1, Using " .###"; O6!(P);

            Print #1, Tab(41);
            Print #1, seeT$(P); Tab(56);
            Print #1, Using "##   ##   "; O1!(P); O2!(P);

            If O3!(P) = 0 And O1!(P) = 0 And O2!(P) = 0 Then Print #1, " .---"
            If O3!(P) = 0 And (O1!(P) <> 0 Or O2!(P) <> 0) Then Print #1, " .000"
            If O3!(P) = 1 Then Print #1, "1.000"
            If O3!(P) <> 0 And O3!(P) <> 1 Then Print #1, Using " .###"; O3!(P)
        End If

    Next P

    Print #1,
    Print #1, confName$ + " CONFERENCE"

    If D2$ <> "" Then Print #1, D2$; " DIVISION"

    Print #1, "                 CONFERENCE                         FULL SEASON"
    Print #1, "                W    L     PCT                          W    L     PCT"

    For P = 16 To 30

        If seeT$(P) <> "" And seeT$(P) <> "*" Then
            Print #1, seeT$(P); Tab(16);
            Print #1, Using "##   ##   "; O4!(P); O5!(P);

            If O6!(P) = 0 And O4!(P) = 0 And O5!(P) = 0 Then Print #1, " .---";
            If O6!(P) = 0 And (O4!(P) <> 0 Or O5!(P) <> 0) Then Print #1, " .000";
            If O6!(P) = 1 Then Print #1, "1.000";
            If O6!(P) <> 0 And O6!(P) <> 1 Then Print #1, Using " .###"; O6!(P);

            Print #1, Tab(41);
            Print #1, seeT$(P); Tab(56);
            Print #1, Using "##   ##   "; O1!(P); O2!(P);

            If O3!(P) = 0 And O1!(P) = 0 And O2!(P) = 0 Then Print #1, " .---"
            If O3!(P) = 0 And (O1!(P) <> 0 Or O2!(P) <> 0) Then Print #1, " .000"
            If O3!(P) = 1 Then Print #1, "1.000"
            If O3!(P) <> 0 And O3!(P) <> 1 Then Print #1, Using " .###"; O3!(P)
        End If

    Next P

    Print #1, Chr$(12)
    Print #1, "FIELD GOAL % (MIN 5 FGM/G)"; Tab(41); "FREE THROW % (MIN 2.5 FTM/G)"
    Print #1,
    Print #1, "                            PCT   FGA"; Tab(41); "                            PCT   FTA"

    For P = 41 To 60

        If seeZ2$(P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, Left$(seeZ2$(P), 10) + ", " + seeZ3$(P);
            Print #1, Tab(28);
            Print #1, Using "##.#  ####"; seeZ!(P) * 100; seeZ1!(P);
            Print #1, Tab(41);
        End If

        If seeZ2$(P + 20) = "" Then
            Print #1,
        Else
            Print #1, Left$(seeZ2$(P + 20), 10) + ", " + seeZ3$(P + 20);
            Print #1, Tab(67);
            Print #1, Using "###.#  ####"; seeZ!(P + 20) * 100; seeZ1!(P)
        End If

    Next

    Print #1,
    Print #1, "SCORING"; Tab(41); "TOTAL POINTS"

    For P = 1 To 20

        If seeZ2$(P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, seeZ2$(P) + ", " + seeZ3$(P);
            Print #1, Tab(34);
            Print #1, Using "##.#"; seeZ!(P);
            Print #1, Tab(41);
        End If
        If seeZ2$(P + 20) = "" Then
            Print #1,
        Else
            Print #1, seeZ2$(P + 20) + ", " + seeZ3$(P + 20);
            Print #1, Tab(74);
            Print #1, Using "####"; seeZ!(P + 20)
        End If

    Next

    Print #1, Chr$(12)
    Print #1, "STEALS PER GAME"; Tab(41); "STEALS"

    For P = 81 To 90

        If seeZ2$(P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, seeZ2$(P) + ", " + seeZ3$(P);
            Print #1, Tab(34);
            Print #1, Using "##.#"; seeZ!(P);
            Print #1, Tab(41);
        End If

        If seeZ2$(P + 10) = "" Then
            Print #1,
        Else
            Print #1, seeZ2$(P + 10) + ", " + seeZ3$(P + 10);
            Print #1, Tab(74);
            Print #1, Using "####"; seeZ!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "ASSISTS PER GAME"; Tab(41); "ASSISTS"

    For P = 101 To 110

        If seeZ2$(P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, seeZ2$(P) + ", " + seeZ3$(P);
            Print #1, Tab(34);
            Print #1, Using "##.#"; seeZ!(P);
            Print #1, Tab(41);
        End If

        If seeZ2$(P + 10) = "" Then
            Print #1,
        Else
            Print #1, seeZ2$(P + 10) + ", " + seeZ3$(P + 10);
            Print #1, Tab(74);
            Print #1, Using "####"; seeZ!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "THREE FIELD GOAL % (MIN 1.5 3FGM/G)"; Tab(41); "MINUTES PER GAME"

    For P = 121 To 130

        If seeZ2$(P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, seeZ2$(P) + ", " + seeZ3$(P);
            Print #1, Tab(34);
            Print #1, Using "##.#"; seeZ!(P) * 100;
            Print #1, Tab(41);
        End If

        If seeZ2$(P + 20) = "" Then
            Print #1,
        Else
            Print #1, seeZ2$(P + 20) + ", " + seeZ3$(P + 20);
            Print #1, Tab(74);
            Print #1, Using "##.#"; seeZ!(P + 20)
        End If

    Next

    Print #1,
    Print #1, "THREE POINT FIELD GOALS MADE"; Tab(41); "MINUTES"

    For P = 131 To 140

        If seeZ2$(P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, seeZ2$(P) + ", " + seeZ3$(P);
            Print #1, Tab(34);
            Print #1, Using "####"; seeZ!(P);
            Print #1, Tab(41);
        End If

        If seeZ2$(P + 20) = "" Then
            Print #1,
        Else
            Print #1, seeZ2$(P + 20) + ", " + seeZ3$(P + 20);
            Print #1, Tab(74);
            Print #1, Using "####"; seeZ!(P + 20)
        End If

    Next

    Print #1,
    Print #1, Chr$(12)
    Print #1, "REBOUNDS PER GAME"; Tab(41); "REBOUNDS"

    For P = 161 To 170

        If seeZ2$(P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, seeZ2$(P) + ", " + seeZ3$(P);
            Print #1, Tab(34);
            Print #1, Using "##.#"; seeZ!(P);
            Print #1, Tab(41);
        End If

        If seeZ2$(P + 10) = "" Then
            Print #1,
        Else
            Print #1, seeZ2$(P + 10) + ", " + seeZ3$(P + 10);
            Print #1, Tab(74);
            Print #1, Using "####"; seeZ!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "BLOCKS PER GAME"; Tab(41); "BLOCKS"

    For P = 181 To 190

        If seeZ2$(P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, seeZ2$(P) + ", " + seeZ3$(P);
            Print #1, Tab(34);
            Print #1, Using "##.#"; seeZ!(P);
            Print #1, Tab(41);
        End If
        If seeZ2$(P + 10) = "" Then
            Print #1,
        Else
            Print #1, seeZ2$(P + 10) + ", " + seeZ3$(P + 10);
            Print #1, Tab(74);
            Print #1, Using "####"; seeZ!(P + 20)
        End If

    Next

    Print #1,
    Print #1, "FIELD GOALS MADE"; Tab(41); "FREE THROWS MADE"

    For P = 201 To 210

        If seeZ2$(P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, seeZ2$(P) + ", " + seeZ3$(P);
            Print #1, Tab(34);
            Print #1, Using "####"; seeZ!(P);
            Print #1, Tab(41);
        End If
        If seeZ2$(P + 10) = "" Then
            Print #1,
        Else
            Print #1, seeZ2$(P + 10) + ", " + seeZ3$(P + 10);
            Print #1, Tab(74);
            Print #1, Using "####"; seeZ!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "FOULS PER GAME"; Tab(41); "FOULS"

    For P = 221 To 230

        If seeZ2$(P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, seeZ2$(P) + ", " + seeZ3$(P);
            Print #1, Tab(34);
            Print #1, Using "##.#"; seeZ!(P);
            Print #1, Tab(41);
        End If

        If seeZ2$(P + 10) = "" Then
            Print #1,
        Else
            Print #1, seeZ2$(P + 10) + ", " + seeZ3$(P + 10);
            Print #1, Tab(74);
            Print #1, Using "####"; seeZ!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "MOST VALUABLE PLAYER"

    For P = 241 To 250

        If seeZ2$(P) <> "" Then
            Print #1, seeZ2$(P) + ", " + seeZ3$(P);
            Print #1, Tab(34);
            Print #1, Using "####"; seeZ!(P)
        End If

    Next

    Print #1,
    Print #1, Chr$(12)
    Print #1, "                         OFFENSIVE TOTALS"
    Print #1, "             G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG"

    For X = 1 To 20

        If offLeaderNames$(X) <> "" And offLeaderNames$(X) <> "*" And offLeaderNames$(X) <> " " Then
            Print #1, Left$(offLeaderNames$(X), 11); Tab(13);
            Print #1, Using "##"; offLeaders!(X, 16);
            Print #1, Tab(16);
            Print #1, Using "####"; offLeaders!(X, 2);
            Print #1, Tab(21);
            Print #1, Using "####"; offLeaders!(X, 3);
            Print #1, Tab(26);
            Print #1, Using ".###"; offLeaders!(X, 17);
            Print #1, Tab(31);
            Print #1, Using "#### #### .###"; offLeaders!(X, 4); offLeaders!(X, 5); offLeaders!(X, 18);
            Print #1, Tab(46);
            Print #1, Using "####"; offLeaders!(X, 6);
            Print #1, Tab(51);
            Print #1, Using "####"; offLeaders!(X, 9);
            Print #1, Tab(56);
            Print #1, Using "####"; offLeaders!(X, 7);
            Print #1, Tab(61);
            Print #1, Using "###"; offLeaders!(X, 8);
            Print #1, Tab(65);
            Print #1, Using "###"; offLeaders!(X, 10);
            Print #1, Tab(69);
            Print #1, Using "#####"; offLeaders!(X, 20);
            Print #1, Tab(75);
            Print #1, Using "###.#"; offLeaders!(X, 19)
        End If

    Next

    Print #1,
    Print #1, "                         DEFENSIVE TOTALS"
    Print #1, "             G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG": Color 15

    For X = 1 To 20

        If deffLeaderNames$(X) <> "" And deffLeaderNames$(X) <> "*" And deffLeaderNames$(X) <> " " Then
            Print #1, Left$(deffLeaderNames$(X), 11); Tab(13);
            Print #1, Using "##"; defLeaders!(X, 16);
            Print #1, Tab(16);
            Print #1, Using "####"; defLeaders!(X, 2);
            Print #1, Tab(21);
            Print #1, Using "####"; defLeaders!(X, 3);
            Print #1, Tab(26);
            Print #1, Using ".###"; defLeaders!(X, 17);
            Print #1, Tab(31);
            Print #1, Using "#### #### .###"; defLeaders!(X, 4); defLeaders!(X, 5); defLeaders!(X, 18);
            Print #1, Tab(46);
            Print #1, Using "#### ####"; defLeaders!(X, 6); defLeaders!(X, 9);
            Print #1, Tab(56);
            Print #1, Using "####"; defLeaders!(X, 7);
            Print #1, Tab(61);
            Print #1, Using "###"; defLeaders!(X, 8);
            Print #1, Tab(65);
            Print #1, Using "###"; defLeaders!(X, 10);
            Print #1, Tab(69);
            Print #1, Using "#####"; defLeaders!(X, 20);
            Print #1, Tab(75);
            Print #1, Using "###.#"; defLeaders!(X, 19)
        End If

    Next

    Close #1

	If printDest = 1 Then

		Open destFile$ For Input As #2

		Do While Not EOF(2)
			Line Input #2, X$
			LPrint X$
		Loop

		Close #2

		LPrint Chr$(12)

		Kill destFile$

	End If

End Sub

