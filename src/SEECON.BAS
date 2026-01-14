'----------------------------------------
'           SEECON Subroutine
'----------------------------------------
'This subroutine lets the user choose a
'conference stat file to view, then calls
'other routines to both load the Stats
'and then provide options for display
Sub SEECON ()

    _Title "Courtside College Basketball - View Conference Stats"

    leaderFile$ = _OpenFileDialog$("Choose Conference Stats file", diskPaths$(2), "*.LDR", "Conference Stats files [.LDR]")
    'ChooseFile$(diskPaths$(2) + "*.LDR", "LEADER", ExitCode%)

    If leaderFile$ <> "" Then

        Call LoadLeaderFiles_Coll(leaderFile$, conf$, D1$, D2$)
        Call StatOptions_Coll(conf$, D1$, D2$)

    End If

End Sub


'----------------------------------------
'     LoadLeaderFiles_Coll Subroutine
'----------------------------------------
'This subroutine loads the conference stats
'from the file into several arrays
Sub LoadLeaderFiles_Coll (targetFile$, conf$, div1$, div2$)

    Shared conferenceName$
    Shared div1Name$, div2Name$

    Shared defLeaders!(), offLeaders!()
    Shared seasonWins(), seasonLosses(), seasonWinPct!()
    Shared divWins(), divLosses(), divWinPct!()
    Shared confLdrVal!(), confLdrAtt!()
    Shared offLeaderTeams$(), defLeaderTeams$()
    Shared leaderName$(), leaderTeam$()
    Shared confLdrName$()

    Cls

    Locate 9, 6
    Print "PLEASE WAIT WHILE THE LEADER FILES ARE LOADING!!"

    Open targetFile$ For Input As #2

    For X = 1 To TEAMS_PER_CONFERENCE:
        Input #2, confLdrName$(X), seasonWins(X), seasonLosses(X), seasonWinPct!(X), divWins(X), divLosses(X), divWinPct!(X)
    Next

    For X = 1 To 250:
        Input #2, leaderName$(X), leaderTeam$(X), confLdrVal!(X), confLdrAtt!(X)
    Next

    For X = 1 To MAX_CONFERENCES:
        Input #2, offLeaderTeams$(X)
        For M = 1 To 20:
            Input #2, offLeaders!(X, M)
        Next
    Next

    For X = 1 To MAX_CONFERENCES:
        Input #2, defLeaderTeams$(X)
        For M = 1 To 20:
            Input #2, defLeaders!(X, M)
        Next
    Next

    Close #2

    Call ReadConferenceStats(diskPaths$(2) + GetFileBaseName$(targetFile$) + ".TMS")

    conf$ = conferenceName$
    div1$ = PadRight$(div1Name$, 8)
    div2$ = PadRight$(div2Name$, 8)

    Cls
    Locate 9, 1: Print "      PLEASE WAIT WHILE THE LEADER FILES ARE LOADING!!"

End Sub


'----------------------------------------
'       StatOptions_Coll Subroutine
'----------------------------------------
'This subroutine gives the user various
'options for viewing the previously loaded
'stats, and then calls routines for each
Sub StatOptions_Coll (confName$, div1$, div2$)

    Do
        Cls
        Call TITLE_STATS

        userKey$ = GetOutputOptions$

        If userKey$ <> Chr$(27) Then

            Select Case userKey$

                Case "F":
                    userFile$ = _SaveFileDialog$("Save Conference Leaders Report", _CWD$ + "\Conference Leaders Report", "*.txt", "Text File")
                    If userFile$ <> "" Then
                        Call CollLeaders_Report(confName$, div1$, div2$, 0, userFile$)
                    End If

                Case "P":
                    Call CollLeaders_Report(confName$, div1$, div2$, 1, "REPORT")

                Case "S":
                    Call CollegeStdgs_Screen(confName$, div1$, div2$)
                    Call CollLeaders_Screen(confName$, div1$, div2$)

            End Select

            Color 15, 0
            Cls

            Call box0(6, 18, 10, 62, 2, 15, 1)

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

    Loop Until UCase$(A$) = "N" Or A$ = Chr$(27) Or userKey$ = Chr$(27)

End Sub


'----------------------------------------
'     CollLeaders_Report Subroutine
'----------------------------------------
'This subroutine formats the previously
'loaded stats and outputs them to a file.
'This file can then be looped through to
'send to the default printer
Sub CollLeaders_Report (confName$, div1$, div2$, printDest, destFile$)

    Shared defLeaders!(), offLeaders!()
    Shared seasonWins(), seasonLosses(), seasonWinPct!(), divWins(), divLosses(), divWinPct!()
    Shared confLdrVal!(), confLdrAtt!()

    Shared offLeaderTeams$(), defLeaderTeams$()
    Shared leaderName$(), leaderTeam$()
    Shared confLdrName$()

    Dim confLdrBackup$(30)

    Color 15, 0
    Cls

    Open destFile$ For Output As #1

    'If division = 1 Or (division = 2 And divisionName$ <> "") Then
    XCoord = Int((80 - Len(confName$) - 11) / 2)
    Print #1, 
    Print #1, Tab(XCoord); confName$ + " CONFERENCE"
    Print #1, 

    For division = 1 To 2

        If division = 1 Then
            divisionName$ = RTrim$(div1$)
        Else
            divisionName$ = RTrim$(div2$)
        End If

        XCoord = Int((80 - Len(divisionName$)) / 2)
        Print #1, Tab(XCoord); divisionName$
        Print #1,

        first = ((division - 1) * 15) + 1
        last = division * 15

        Call SortStandingsConference(0)

        For X = 1 to 30
            confLdrBackup$(X) = confLdrName$(X)
        Next X

        Call SortStandingsConference(1)

        Print #1, "                 CONFERENCE                             FULL SEASON"
        Print #1, "                W    L     PCT                          W    L     PCT"

        For P = first To last

            If confLdrName$(P) <> "" And confLdrName$(P) <> "*" Then

                'Conference Record
                Print #1, confLdrName$(P); Tab(16);

                Print #1, Using "##   ##   "; divWins(P); divLosses(P);

                If divWinPct!(P) = 0 And divWins(P) = 0 And divLosses(P) = 0 Then
                    Print #1, " .---";
                End If
                If divWinPct!(P) = 0 And (divWins(P) <> 0 Or divLosses(P) <> 0) Then
                    Print #1, " .000";
                End If
                If divWinPct!(P) = 1 Then
                    Print #1, "1.000";
                End If
                If divWinPct!(P) <> 0 And divWinPct!(P) <> 1 Then
                    Print #1, Using " .###"; divWinPct!(P);
                End If

                'Season Record
                Print #1, Tab(41); 
				Print #1, confLdrName$(P); Tab(56);

                Print #1, Using "##   ##   "; seasonWins(P); seasonLosses(P);

                If seasonWinPct!(P) = 0 And seasonWins(P) = 0 And seasonLosses(P) = 0 Then
                    Print #1, " .---"
                End If
                If seasonWinPct!(P) = 0 And (seasonWins(P) <> 0 Or seasonLosses(P) <> 0) Then
                    Print #1, " .000"
                End If
                If seasonWinPct!(P) = 1 Then
                    Print #1, "1.000"
                End If
                If seasonWinPct!(P) <> 0 And seasonWinPct!(P) <> 1 Then
                    Print #1, Using " .###"; seasonWinPct!(P)
                End If

            End If

        Next P

        'End If

    Next division

    Print #1,
    If printDest = 1 Then Print #1, Chr$(12)
    Print #1,

    Print #1, "FIELD GOAL % (MIN 5 FGM/G)"; Tab(41); "FREE THROW % (MIN 2.5 FTM/G)"
    Print #1,
    Print #1, "                            PCT   FGA"; Tab(41); "                            PCT   FTA"

    For P = 41 To 60

        If leaderName$(P) <> "" Then
            Print #1, Left$(leaderName$(P), 10) + ", " + leaderTeam$(P);
            Print #1, Tab(28);
            Print #1, Using "##.#  ####"; confLdrVal!(P) * 100; confLdrAtt!(P);
        End If

        Print #1, Tab(41);
        If leaderName$(P + 20) = "" Then
            Print #1,
        Else
            Print #1, Left$(leaderName$(P + 20), 10) + ", " + leaderTeam$(P + 20);
            Print #1, Tab(67);
            Print #1, Using "###.#  ####"; confLdrVal!(P + 20) * 100; confLdrAtt!(P)
        End If

    Next

    Print #1,
    Print #1, "SCORING"; Tab(41); "TOTAL POINTS"

    For P = 1 To 20

        If leaderName$(P) <> "" Then
            Print #1, leaderName$(P) + ", " + leaderTeam$(P);
            Print #1, Tab(34);
            Print #1, Using "##.#"; confLdrVal!(P);
        End If

        Print #1, Tab(41);
        If leaderName$(P + 20) = "" Then
            Print #1,
        Else
            Print #1, leaderName$(P + 20) + ", " + leaderTeam$(P + 20);
            Print #1, Tab(74);
            Print #1, Using "####"; confLdrVal!(P + 20)
        End If

    Next

    Print #1, 
    If printDest = 1 Then Print #1, Chr$(12)
    Print #1, 
    
    Print #1, "STEALS PER GAME"; Tab(41); "STEALS"

    For P = 81 To 90

        If leaderName$(P) <> "" Then
            Print #1, leaderName$(P) + ", " + leaderTeam$(P);
            Print #1, Tab(34);
            Print #1, Using "##.#"; confLdrVal!(P);
        End If

        Print #1, Tab(41);
        If leaderName$(P + 10) = "" Then
            Print #1,
        Else
            Print #1, leaderName$(P + 10) + ", " + leaderTeam$(P + 10);
            Print #1, Tab(74);
            Print #1, Using "####"; confLdrVal!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "ASSISTS PER GAME"; Tab(41); "ASSISTS"

    For P = 101 To 110

        If leaderName$(P) <> "" Then
            Print #1, leaderName$(P) + ", " + leaderTeam$(P);
            Print #1, Tab(34);
            Print #1, Using "##.#"; confLdrVal!(P);
        End If

        Print #1, Tab(41);
        If leaderName$(P + 10) = "" Then
            Print #1,
        Else
            Print #1, leaderName$(P + 10) + ", " + leaderTeam$(P + 10);
            Print #1, Tab(74);
            Print #1, Using "####"; confLdrVal!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "THREE FIELD GOAL % (MIN 1.5 3FGM/G)"; Tab(41); "MINUTES PER GAME"

    For P = 121 To 130

        If leaderName$(P) <> "" Then
            Print #1, leaderName$(P) + ", " + leaderTeam$(P);
            Print #1, Tab(34);
            Print #1, Using "##.#"; confLdrVal!(P) * 100;
        End If

        Print #1, Tab(41);
        If leaderName$(P + 20) = "" Then
            Print #1,
        Else
            Print #1, leaderName$(P + 20) + ", " + leaderTeam$(P + 20);
            Print #1, Tab(74);
            Print #1, Using "##.#"; confLdrVal!(P + 20)
        End If

    Next

    Print #1,
    Print #1, "THREE POINT FIELD GOALS MADE"; Tab(41); "MINUTES"

    For P = 131 To 140

        If leaderName$(P) <> "" Then
            Print #1, leaderName$(P) + ", " + leaderTeam$(P);
            Print #1, Tab(34);
            Print #1, Using "####"; confLdrVal!(P);
        End If

        Print #1, Tab(41);
        If leaderName$(P + 20) = "" Then
            Print #1,
        Else
            Print #1, leaderName$(P + 20) + ", " + leaderTeam$(P + 20);
            Print #1, Tab(74);
            Print #1, Using "####"; confLdrVal!(P + 20)
        End If

    Next

    Print #1,
    If printDest = 1 Then Print #1, Chr$(12)
    Print #1, 
    
    Print #1, "REBOUNDS PER GAME"; Tab(41); "REBOUNDS"

    For P = 161 To 170

        If leaderName$(P) <> "" Then
            Print #1, leaderName$(P) + ", " + leaderTeam$(P);
            Print #1, Tab(34);
            Print #1, Using "##.#"; confLdrVal!(P);
        End If

        Print #1, Tab(41);
        If leaderName$(P + 10) = "" Then
            Print #1,
        Else
            Print #1, leaderName$(P + 10) + ", " + leaderTeam$(P + 10);
            Print #1, Tab(74);
            Print #1, Using "####"; confLdrVal!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "BLOCKS PER GAME"; Tab(41); "BLOCKS"

    For P = 181 To 190

        If leaderName$(P) <> "" Then
            Print #1, leaderName$(P) + ", " + leaderTeam$(P);
            Print #1, Tab(34);
            Print #1, Using "##.#"; confLdrVal!(P);
        End If

        Print #1, Tab(41);
        If leaderName$(P + 10) = "" Then
            Print #1,
        Else
            Print #1, leaderName$(P + 10) + ", " + leaderTeam$(P + 10);
            Print #1, Tab(74);
            Print #1, Using "####"; confLdrVal!(P + 20)
        End If

    Next

    Print #1,
    Print #1, "FIELD GOALS MADE"; Tab(41); "FREE THROWS MADE"

    For P = 201 To 210

        If leaderName$(P) <> "" Then
            Print #1, leaderName$(P) + ", " + leaderTeam$(P);
            Print #1, Tab(34);
            Print #1, Using "####"; confLdrVal!(P);
        End If

        Print #1, Tab(41);
        If leaderName$(P + 10) = "" Then
            Print #1,
        Else
            Print #1, leaderName$(P + 10) + ", " + leaderTeam$(P + 10);
            Print #1, Tab(74);
            Print #1, Using "####"; confLdrVal!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "FOULS PER GAME"; Tab(41); "FOULS"

    For P = 221 To 230

        If leaderName$(P) <> "" Then
            Print #1, leaderName$(P) + ", " + leaderTeam$(P);
            Print #1, Tab(34);
            Print #1, Using "##.#"; confLdrVal!(P);
        End If

        Print #1, Tab(41);
        If leaderName$(P + 10) = "" Then
            Print #1,
        Else
            Print #1, leaderName$(P + 10) + ", " + leaderTeam$(P + 10);
            Print #1, Tab(74);
            Print #1, Using "####"; confLdrVal!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "MOST VALUABLE PLAYER"

    For P = 241 To 250

        If leaderName$(P) <> "" Then
            Print #1, leaderName$(P) + ", " + leaderTeam$(P);
            Print #1, Tab(34);
            Print #1, Using "####"; confLdrVal!(P)
        End If

    Next

    Print #1,
    If printDest = 1 Then Print #1, Chr$(12)
    Print #1, 

    Print #1, Tab(32); "OFFENSIVE TOTALS"
    Print #1, Tab(16); "G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG"

    For X = 1 To 20

        If offLeaderTeams$(X) <> "" And offLeaderTeams$(X) <> "*" And offLeaderTeams$(X) <> " " Then

            'Player
            Print #1, offLeaderTeams$(X);

            'Games
            Print #1, Tab(16);
            Print #1, Using "##"; offLeaders!(X, 16);

            'FGA
            Print #1, Tab(19);
            Print #1, Using "####"; offLeaders!(X, 1);

            'FGM
            Print #1, Tab(24);
            Print #1, Using "####"; offLeaders!(X, 2);

            'FG %
            Print #1, Tab(29);
            Print #1, Using ".###"; offLeaders!(X, 17);

            'FTA / FTM / FT%
            Print #1, Tab(34);
            Print #1, Using "#### #### .###"; offLeaders!(X, 3); offLeaders!(X, 4); offLeaders!(X, 18);

            'Rebounds
            Print #1, Tab(49);
            Print #1, Using "####"; offLeaders!(X, 5);

            'Assists
            Print #1, Tab(54);
            Print #1, Using "####"; offLeaders!(X, 8);

            'Personal Fouls
            Print #1, Tab(59);
            Print #1, Using "####"; offLeaders!(X, 6);

            'Steals
            Print #1, Tab(64);
            Print #1, Using "###"; offLeaders!(X, 7);

            'Blocks
            Print #1, Tab(68);
            Print #1, Using "###"; offLeaders!(X, 9);

            'Points
            Print #1, Tab(72);
            Print #1, Using "#####"; offLeaders!(X, 20);

            'Average
            Print #1, Tab(78);
            Print #1, Using "###.#"; offLeaders!(X, 19)

        End If

    Next

    Print #1,
    Print #1, Tab(32); "DEFENSIVE TOTALS"
    Print #1, Tab(16); "G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG"

    For X = 1 To 20

        If defLeaderTeams$(X) <> "" And defLeaderTeams$(X) <> "*" And defLeaderTeams$(X) <> " " Then

            'Player
            Print #1, defLeaderTeams$(X); 

            'Games
            Print #1, Tab(16);
            Print #1, Using "##"; defLeaders!(X, 16);

            'FGA
            Print #1, Tab(19);
            Print #1, Using "####"; defLeaders!(X, 1);

            'FGM
            Print #1, Tab(24);
            Print #1, Using "####"; defLeaders!(X, 2);

            'FG %
            Print #1, Tab(29);
            Print #1, Using ".###"; defLeaders!(X, 17);

            'FTA / FTM / FT%
            Print #1, Tab(34);
            Print #1, Using "#### #### .###"; defLeaders!(X, 3); defLeaders!(X, 4); defLeaders!(X, 18);

            'Rebounds
            Print #1, Tab(49);
            Print #1, Using "####"; defLeaders!(X, 5);

            'Assists
            Print #1, Tab(54);
            Print #1, Using "####"; defLeaders!(X, 8);

            'Personal Fouls
            Print #1, Tab(59);
            Print #1, Using "####"; defLeaders!(X, 6);

            'Steals
            Print #1, Tab(64);
            Print #1, Using "###"; defLeaders!(X, 7);

            'Blocks
            Print #1, Tab(68);
            Print #1, Using "###"; defLeaders!(X, 9);

            'Points
            Print #1, Tab(72);
            Print #1, Using "#####"; defLeaders!(X, 20);

            'Average
            Print #1, Tab(78);
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


'----------------------------------------
'    CollLeaders_Screen Subroutine
'----------------------------------------
'This subroutine formats the previously
'loaded stats and outputs them to the end
'user display.
Sub CollLeaders_Screen (confName$, div1$, div2$)

    Shared defLeaders!(), offLeaders!()
    Shared confLdrVal!(), confLdrAtt!()
    Shared offLeaderTeams$(), defLeaderTeams$()
    Shared leaderName$(), leaderTeam$()
    Shared confLdrName$()

    Cls

    Locate 2, 1
    Color 14
    Print "FIELD GOAL % (MIN 5 FGA/G)"
    Print "                            PCT    FGA"
    Color 15

    For P = 41 To 60
        If leaderName$(P) <> "" Then
            Print Left$(leaderName$(P), 10) + ", " + leaderTeam$(P);
            Locate , 28
            Print Using "###.#  ####"; confLdrVal!(P) * 100; confLdrAtt!(P)
        End If
    Next

    Color 14
    Locate 2, 41
    Print "FREE THROW % (MIN 2 FTA/G)"
    Locate , 41
    Print "                            PCT   FTA"
    Color 15

    For P = 61 To 80
        If leaderName$(P) <> "" Then
            Locate , 41
            Print Left$(leaderName$(P), 10) + ", " + leaderTeam$(P);
            Locate , 68
            Print Using "###.#  ####"; confLdrVal!(P) * 100; confLdrAtt!(P)
        End If
    Next

    '-----
    Color 12
    Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Locate 2, 1
    Print "SCORING"
    Print Tab(33); "PTS/G"
    Color 15

    For P = 1 To 20
        If leaderName$(P) <> "" Then
            Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 34
            Print Using "##.#"; confLdrVal!(P)
        End If
    Next

    Locate 2, 41
    Color 14
    Print "TOTAL POINTS"
    Locate , 75
    Print "PTS"
    Color 15

    For P = 21 To 40
        If leaderName$(P) <> "" Then
            Locate , 41: Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 74
            Print Using "####"; confLdrVal!(P)
        End If
    Next

    '-----
    Color 12
    Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Locate 2, 1
    Color 14
    Print "STEALS PER GAME"
    Color 15

    For P = 81 To 90
        If leaderName$(P) <> "" Then
            Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 34
            Print Using "##.#"; confLdrVal!(P)
        End If
    Next

    Locate 2, 41
    Color 14
    Print "STEALS"
    Color 15

    For P = 91 To 100
        If leaderName$(P) <> "" Then
            Locate , 41: Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 74
            Print Using "####"; confLdrVal!(P)
        End If
    Next

    Color 14
    Locate 14: Print "ASSISTS PER GAME"
    Color 15

    For P = 101 To 110
        If leaderName$(P) <> "" Then
            Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 34
            Print Using "##.#"; confLdrVal!(P)
        End If
    Next

    Locate 14, 41
    Color 14
    Print "ASSISTS"
    Color 15

    For P = 111 To 120
        If leaderName$(P) <> "" Then
            Locate , 41: Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 74
            Print Using "####"; confLdrVal!(P)
        End If
    Next

    '-----
    Color 12
    Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Locate 2, 1
    Color 14
    Print "THREE FIELD GOAL% (MIN 1.5 3FGM/G)"
    Color 15

    For P = 121 To 130
        If leaderName$(P) <> "" Then
            Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 34
            Print Using "###.#"; confLdrVal!(P) * 100
        End If
    Next P

    Locate 2, 41
    Color 14
    Print "MINUTES PER GAME"
    Color 15

    For P = 141 To 150
        If leaderName$(P) <> "" Then
            Locate , 41: Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 74
            Print Using "##.#"; confLdrVal!(P)
        End If
    Next P

    Locate 14
    Color 14
    Print "THREE POINT FIELD GOALS MADE"
    Color 15

    For P = 131 To 140
        If leaderName$(P) <> "" Then
            Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 34
            Print Using "####"; confLdrVal!(P)
        End If
    Next

    Locate 14, 41
    Color 14
    Print "TOTAL MINUTES"
    Color 15

    For P = 151 To 160
        If leaderName$(P) <> "" Then
            Locate , 41: Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 74
            Print Using "####"; confLdrVal!(P)
        End If
    Next

    '-----
    Color 12
    Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Locate 2, 1
    Color 14
    Print "REBOUNDS PER GAME   "
    Color 15

    For P = 161 To 170
        If leaderName$(P) <> "" Then
            Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 34
            Print Using "##.#"; confLdrVal!(P)
        End If

    Next

    Locate 2, 41
    Color 14
    Print "REBOUNDS"
    Color 15

    For P = 171 To 180
        If leaderName$(P) <> "" Then
            Locate , 41: Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 74
            Print Using "####"; confLdrVal!(P)
        End If
    Next

    Locate 14
    Color 14
    Print "BLOCKS PER GAME"
    Color 15

    For P = 181 To 190
        If leaderName$(P) <> "" Then
            Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 34
            Print Using "##.#"; confLdrVal!(P)
        End If
    Next

    Locate 14, 41
    Color 14
    Print "BLOCKS"
    Color 15

    For P = 191 To 200
        If leaderName$(P) <> "" Then
            Locate , 41: Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 74
            Print Using "####"; confLdrVal!(P)
        End If
    Next

    '-----
    Color 12
    Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Locate 2, 1
    Color 14
    Print "FIELD GOALS MADE "
    Color 15

    For P = 201 To 210
        If leaderName$(P) <> "" Then
            Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 34
            Print Using "####"; confLdrVal!(P)
        End If
    Next

    Locate 2, 41
    Color 14
    Print "FREE THROWS MADE"
    Color 15

    For P = 211 To 220
        If leaderName$(P) <> "" Then
            Locate , 41: Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 74
            Print Using "####"; confLdrVal!(P)
        End If
    Next

    Locate 14
    Color 14
    Print "FOULS PER GAME"
    Color 15

    For P = 221 To 230
        If leaderName$(P) <> "" Then
            Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 34
            Print Using "##.#"; confLdrVal!(P)
        End If
    Next

    Locate 14, 41
    Color 14
    Print "FOULS"
    Color 15

    For P = 231 To 240
        If leaderName$(P) <> "" Then
            Locate , 41: Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 74
            Print Using "####"; confLdrVal!(P)
        End If
    Next

    '-----
    Color 12
    Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Locate 2, 1
    Color 14
    Print "MOST VALUABLE PLAYER POINTS"
    Color 15

    For P = 241 To 250
        If leaderName$(P) <> "" Then
            Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 34
            Print Using "####"; confLdrVal!(P)
        End If
    Next

    '-----
    Color 12
    Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Locate 2, 1
    Color 12
    Print Tab(32); "OFFENSIVE TOTALS"

    Locate 4, 1
    Color 14
    'Print Tab(13); "G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG"
    Print Tab(13); "G  FGA  FGM  PCT  FTA  FTM  PCT   REB  AST   PF STL BLK   PTS  AVG"
    Color 15

    For X = 1 To 20

        If offLeaderTeams$(X) <> "" And offLeaderTeams$(X) <> "*" And offLeaderTeams$(X) <> " " Then

            Print Left$(offLeaderTeams$(X), 11); Tab(13);

            'Games
            Print Using "##"; offLeaders!(X, 16);
            Print Tab(16);

            'FGA
            Print Using "####"; offLeaders!(X, 1);
            Print Tab(21);

            'FGM
            Print Using "####"; offLeaders!(X, 2);
            Print Tab(26);

            'FG %
            Print Using ".###"; offLeaders!(X, 17);
            Print Tab(31);

            'FTA / FTM / FT%
            Print Using "#### #### .###"; offLeaders!(X, 3); offLeaders!(X, 4); offLeaders!(X, 18);
            Print Tab(46);

            'Rebounds
            Print Using "####"; offLeaders!(X, 5);
            Print Tab(51);

            'Assists
            Print Using "####"; offLeaders!(X, 8);
            Print Tab(56);

            'Personal Fouls
            Print Using "####"; offLeaders!(X, 6);
            Print Tab(61);

            'Steals
            Print Using "###"; offLeaders!(X, 7);
            Print Tab(65);

            'Blocks
            Print Using "###"; offLeaders!(X, 9);
            Print Tab(69);

            'Points
            Print Using "#####"; offLeaders!(X, 20);
            Print Tab(75);

            'Average
            Print Using "###.#"; offLeaders!(X, 19)

        End If

    Next

    '-----
    Color 12
    Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Locate 2, 1
    Color 12
    Print Tab(32); "DEFENSIVE TOTALS"

    Locate 4, 1
    Color 14
    Print Tab(13); "G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG"
    Color 15

    For X = 1 To 20

        If defLeaderTeams$(X) <> "" And defLeaderTeams$(X) <> "*" And defLeaderTeams$(X) <> " " Then

            Print Left$(defLeaderTeams$(X), 11); Tab(13);

            'Games
            Print Using "##"; defLeaders!(X, 16);
            Print Tab(16);

            'FGA
            Print Using "####"; defLeaders!(X, 1);
            Print Tab(21);

            'FGM
            Print Using "####"; defLeaders!(X, 2);
            Print Tab(26);

            'FG PCT
            Print Using ".###"; defLeaders!(X, 17);
            Print Tab(31);

            'FTA / FTM / FT %
            Print Using "#### #### .###"; defLeaders!(X, 3); defLeaders!(X, 4); defLeaders!(X, 18);
            Print Tab(46);

            'Rebounds
            Print Using "####"; defLeaders!(X, 5);
            Print Tab(51);

            'Assists
            Print Using "####"; defLeaders!(X, 8);
            Print Tab(56);

            'Personal FOuls
            Print Using "####"; defLeaders!(X, 6);
            Print Tab(61);

            'Steals
            Print Using "###"; defLeaders!(X, 7);
            Print Tab(65);

            'Blocks
            Print Using "###"; defLeaders!(X, 9);
            Print Tab(69);

            'Points
            Print Using "#####"; defLeaders!(X, 20);
            Print Tab(75);

            'Average
            Print Using "###.#"; defLeaders!(X, 19)

        End If

    Next

    '-----
    Color 12
    Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
    Color 15
    QK$ = GetKeyPress$
    '-----

End Sub


'----------------------------------------
'    CollegeStdgs_Screen Subroutine
'----------------------------------------
'This subroutine formats the previously
'loaded stats and outputs them to the end
'user display.
Sub CollegeStdgs_Screen (confName$, div1$, div2$)

    Shared divWins(), divLosses(), divWinPct!()
    Shared seasonWins(), seasonLosses(), seasonWinPct!()
    Shared confLdrName$(), memberName$()

    For division = 1 To 2

        If division = 1 Then
            divisionName$ = RTrim$(div1$)
        Else
            divisionName$ = RTrim$(div2$)
        End If

        'If division = 1 Or (division = 2 And divisionName$ <> "") Then

        Color 15, 0

        Cls

        Color 2

        Locate 2, Int((80 - Len(confName$) - 11) / 2)
        Print confName$ + " CONFERENCE"
        Print

        Locate , Int((80 - Len(divisionName$)) / 2)
        Print divisionName$
        Print

        Color 12
        Print "                 CONFERENCE                             FULL SEASON"
        Color 14
        Print "                W    L     PCT                          W    L     PCT"
        Color 15

        minIdx = ((division - 1) * 15) + 1
        maxIdx = division * 15

        Call SortStandingsConference(0)

        For P = minIdx To maxIdx

            If confLdrName$(P) <> "" And confLdrName$(P) <> "*" Then

                Print confLdrName$(P); Tab(16);

                'Conference Record
                Print Using "##   ##   "; divWins(P); divLosses(P);

                If divWinPct!(P) = 0 And divWins(P) = 0 And divLosses(P) = 0 Then
                    Print " .---"
                End If
                If divWinPct!(P) = 0 And (divWins(P) <> 0 Or divLosses(P) <> 0) Then
                    Print " .000"
                End If
                If divWinPct!(P) = 1 Then
                    Print "1.000"
                End If
                If divWinPct!(P) <> 0 And divWinPct!(P) <> 1 Then
                    Print Using " .###"; divWinPct!(P)
                End If

            End If

        Next P

        Call SortStandingsConference(1)

        Locate 7

        For P = minIdx To maxIdx

            If confLdrName$(P) <> "" And confLdrName$(P) <> "*" Then

                'Season Record
                Locate , 41
                Print confLdrName$(P); Tab(56);
                Print Using "##   ##   "; seasonWins(P); seasonLosses(P);

                If seasonWinPct!(P) = 0 And seasonWins(P) = 0 And seasonLosses(P) = 0 Then
                    Print " .---"
                End If
                If seasonWinPct!(P) = 0 And (seasonWins(P) <> 0 Or seasonLosses(P) <> 0) Then
                    Print " .000"
                End If
                If seasonWinPct!(P) = 1 Then
                    Print "1.000"
                End If
                If seasonWinPct!(P) <> 0 And seasonWinPct!(P) <> 1 Then
                    Print Using " .###"; seasonWinPct!(P)
                End If

            End If

        Next P

        'End If

        '-----
        Color 12
        Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
        Color 15

        QK$ = GetKeyPress$
        '-----

    Next division

    '-----

End Sub
