'----------------------------------------
'           LOOKY Subroutine
'----------------------------------------
'This subroutine guides the user through
'choosing a team file and then calls other
'routines to load and view the stats
Sub LOOKY ()

    _Title "Courtside College Basketball - Stats Viewer"

    Do

        menuExit = 0

        teamNbr = 0
        Call ChooseTeam(YN$, NTMS, teamNbr, team$)

        If teamNbr > 0 Then

            Color 15, 0

            statFile$ = nameStatFile$(team$)

            If Not _FileExists(diskPaths$(1) + statFile$) Then

                MsgText$ = "NO COMPILED STATS COULD BE FOUND FOR THIS TEAM. PRESS ANY KEY TO RETURN TO THE MAIN MENU"
                BoxWidth = 24
                BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                Call CSMsgBox(MsgText$, BoxWidth, PASS)
                I$ = GetKeyPress$
                Call CSMsgBox("", 0, PASS)

            Else

                Call LoadTeamStats(statFile$, team$, totalGames)
                Call ViewScheduleSummary(statFile$, team$, totalGames, menuExit)

            End If

        End If

    Loop Until teamNbr = 0

End Sub


'----------------------------------------
'        LoadTeamStats Subroutine
'----------------------------------------
'This subroutine loads the selected team's
'stats into arrays
Sub LoadTeamStats (statFile$, teamName$, totalGames)

    Dim totalScore(0 To 1)

    'Do these need shared based on the below???
    'All are assigned 0s and read in from file
    Shared homeLosses, homeWins, neutralLosses, neutralWins
    Shared roadLosses, roadWins, totalLosses, totalWins

    Shared powerRating(), teamScore(), oppScore()

    Shared gameAttendance&()

    Shared stats3FGM!(), stats3FGA!()
    Shared statsOffTot!(), statsDefTot!()
    Shared statPlyrVal!()

    Shared gameSite$(), locIndicator$(), oppName$()
    Shared statPlyrNames$()

    ' *** READ IN TEAM STATS ***
    'Erase totalScore

    totalLosses = 0
    totalWins = 0
    tiedGames = 0

    homeWins = 0
    roadWins = 0
    neutralWins = 0

    homeLosses = 0
    roadLosses = 0
    neutralLosses = 0

    Call ReadScheduleStats(statFile$, totalGames)

    For I = 1 To totalGames

        If teamScore(I) > oppScore(I) Then
            totalWins = totalWins + 1
        End If
        If teamScore(I) > oppScore(I) And Left$(locIndicator$(I), 1) = "H" Then
            homeWins = homeWins + 1
        End If
        If teamScore(I) > oppScore(I) And Left$(locIndicator$(I), 1) = "V" Then
            roadWins = roadWins + 1
        End If
        If teamScore(I) > oppScore(I) And Left$(locIndicator$(I), 1) = "N" Then
            neutralWins = neutralWins + 1
        End If

        If teamScore(I) < oppScore(I) Then
            totalLosses = totalLosses + 1
        End If
        If teamScore(I) < oppScore(I) And Left$(locIndicator$(I), 1) = "H" Then
            homeLosses = homeLosses + 1
        End If
        If teamScore(I) < oppScore(I) And Left$(locIndicator$(I), 1) = "V" Then
            roadLosses = roadLosses + 1
        End If
        If teamScore(I) < oppScore(I) And Left$(locIndicator$(I), 1) = "N" Then
            neutralLosses = neutralLosses + 1
        End If

        'If teamScore(I) = oppScore(I) Then
        '    tiedGames = tiedGames + 1
        'End If

        totalScore(0) = totalScore(0) + teamScore(I)
        totalScore(1) = totalScore(1) + oppScore(I)

    Next

    Call ReadTeamStats(teamName$, statFile$)

    For I = 0 To 13

        If statPlyrVal!(I, 0) <> 0 Then
            statPlyrVal!(I, 14) = statPlyrVal!(I, 10) / statPlyrVal!(I, 0)
            statPlyrVal!(I, 20) = (statPlyrVal!(I, 2) * 2 + statPlyrVal!(I, 4) + stats3FGM!(I)) / statPlyrVal!(I, 0)
            statPlyrVal!(I, 21) = statPlyrVal!(I, 5) / statPlyrVal!(I, 0)
        End If

        statPlyrVal!(I, 22) = statPlyrVal!(I, 2) * 2 + statPlyrVal!(I, 4) + stats3FGM!(I)
        statPlyrVal!(I, 23) = statPlyrVal!(I, 5) - statPlyrVal!(I, 12)

        If statPlyrVal!(I, 1) <> 0 Then
            statPlyrVal!(I, 15) = statPlyrVal!(I, 2) / statPlyrVal!(I, 1)
        End If

        statPlyrVal!(I, 18) = stats3FGM!(I)
        statPlyrVal!(I, 19) = stats3FGA!(I)

        If statPlyrVal!(I, 19) <> 0 Then
            statPlyrVal!(I, 16) = statPlyrVal!(I, 18) / statPlyrVal!(I, 19)
        End If

        If statPlyrVal!(I, 3) <> 0 Then
            statPlyrVal!(I, 17) = statPlyrVal!(I, 4) / statPlyrVal!(I, 3)
        End If

    Next

End Sub


'----------------------------------------
'       ViewScheduleSummary Subroutine
'----------------------------------------
'This subroutine formats and outputs the
'selected team's record to the display,
'then provides routines for output of
'the other stat categories
Sub ViewScheduleSummary (statFile$, teamName$, totalGames, menuExit)

    Shared teamRatings()

    'Do these need shared based on the below???
    'All are assigned 0s and read in from file
    Shared homeLosses, homeWins, neutralLosses, neutralWins
    Shared roadLosses, roadWins, totalLosses, totalWins

    Shared teamScore(), oppScore()

    Shared gameAttendance&()

    Shared gameSite$(), locIndicator$(), oppName$()

    Do

        totalWins = 0
        totalLosses = 0
        'tiedGames = 0

        Color 15, 0
        Cls

        Color 14, 1

        Locate 1, 1
        Print Tab(80); " ";

        header$ = "GAME-BY-GAME REPORT FOR " + teamName$
        Xcoord = Len(header$)
        Xcoord = Int((80 - Xcoord) / 2)
        Locate , Xcoord

        Print "GAME-BY-GAME REPORT FOR ";
        Color teamRatings(0), teamRatings(1)
        Print teamName$

        Color 14, 1
        Print "OPPONENT            SCORE  W/L  ATTEND   SITE"; Space$(35)

        'Locate LAST_ROW - 1, 27: Print "PRESS ANY KEY TO CONTINUE";
        Locate 3, 1
        Color 15, 0

        For I = 1 To totalGames

            Color 7

            If (I Mod 2) = 0 Then Color 15

            Print Left$(oppName$(I), 14);

            Locate , 19: Print Using "###"; teamScore(I);
            Print "-";
            Print Using "###   "; oppScore(I);

            If teamScore(I) > oppScore(I) Then
                totalWins = totalWins + 1
                Print "W";
            End If

            If teamScore(I) < oppScore(I) Then
                totalLosses = totalLosses + 1
                Print "L";
            End If

            'If teamScore(I) = oppScore(I) Then
            'tiedGames = tiedGames + 1
            'Print "T";
            'End If

            Locate , 31: Print Using "####,###"; gameAttendance&(I);
            Locate , 43: Print locIndicator$(I); "  "; Left$(gameSite$(I), 24)

            If (I Mod 18 = 0) Then

                I$ = GetKeyPress$
                ' *** ADD IN CLEAR SCREEN AND TITLE BAR ***

                If I$ <> Chr$(27) Then

                    Cls

                    Locate 1, 1
                    Color 14, 1
                    Print Tab(80); " ";

                    Locate , Xcoord
                    Print "GAME-BY-GAME REPORT FOR ";
                    Color teamRatings(0), teamRatings(1)
                    Print teamName$

                    Color 14, 1
                    Print "OPPONENT            SCORE  W/L  ATTEND   SITE"; Space$(35)

                    Locate LAST_ROW, 1: Print Tab(80); " ";
                    Locate LAST_ROW - 1, 1: Print Tab(80); " ";
                    Locate LAST_ROW - 1, 27: Print "PRESS ANY KEY TO CONTINUE";
                    Locate 3, 1
                    Color 15, 0
                Else
                    Exit For
                End If

            End If

        Next I

        If I$ <> Chr$(27) Then

            Print

            Locate LAST_ROW - 5, 27
            Color 14: Print "OVERALL RECORD ";
            Color 15: Print "("; totalWins; "-"; totalLosses; ")"
            Locate , 27
            Color 14: Print "HOME RECORD    ";
            Color 15: Print "("; homeWins; "-"; homeLosses; ")"
            Locate , 27
            Color 14: Print "ROAD RECORD    ";
            Color 15: Print "("; roadWins; "-"; roadLosses; ")"
            Locate , 27
            Color 14: Print "NEUTRAL RECORD ";
            Color 15: Print "("; neutralWins; "-"; neutralLosses; ")";

            Color 3
            Locate LAST_ROW, 1
            Color 15, 1: Print "VIEW STATS ";
            Color 14: Print "[Pg DN]  ";
            Color 15: Print "SELECT ANOTHER TEAM ";
            Color 14: Print "[ESC]  ";
            Color 15: Print "PRINT STATS ";
            Color 14: Print "[P]";
            Print Space$(18);

            Locate 3, 1

            I$ = GetKeyPress$

            Select Case UCase$(I$)

                Case Chr$(0) + Chr$(73), Chr$(0) + Chr$(75)
                    'Page Up / Left Arrow

                Case Chr$(0) + Chr$(81), Chr$(0) + Chr$(73)
                    'Page Down / RIGHT Arrow
                    Color , 0
                    Cls
                    Call ViewOffenseStats(statFile$, teamName$, categoryPick, totalGames, menuExit)
                    'Call ExtendedStatSummary(statFile$, teamName$, totalGames, menuExit)

                Case "D":
                    Kill diskPaths$(1) + statFile$
                    Kill diskPaths$(1) + statFile$ + "S"

                Case "P":
                    userKey$ = GetPrintOptions$

                    If userKey$ <> Chr$(27) Then

                        Select Case userKey$

                            Case "F"
                                userFile$ = _SaveFileDialog$("Save Report File", _CWD$ + "\Report", "*.txt", "Text File")
                                If userFile$ <> "" Then
                                    Call StatsSchedule_Report(0, userFile$, teamName$, totalGames)
                                    Call StatsSummary_Report(0, userFile$, teamName$, totalGames)
                                End If

                            Case "P"
                                Call StatsSchedule_Report(1, "REPORT", teamName$, totalGames)
                                Call StatsSummary_Report(1, "REPORT", teamName$, totalGames)

                        End Select

                    End If

            End Select

        End If

    Loop Until I$ = Chr$(0) + Chr$(81) Or I$ = Chr$(27) 'Or I$ = Chr$(0) + Chr$(73)

    If I$ = Chr$(27) Then menuExit = 1

End Sub


'----------------------------------------
'       ViewOffenseStats Subroutine
'----------------------------------------
'This subroutine formats and outputs the
'team's player stats to the user display
Sub ViewOffenseStats (statFile$, teamName$, categoryPick, totalGames, menuExit)

    Shared stats3FGM!(), stats3FGA!()
    Shared statsOffTot!(), statsDefTot!()
    Shared statPlyrVal!()

    Shared statCategoryAbbr$(), statPlyrNames$()

    categoryPick = 0

    Do

        Call TITLE_STATS

        Color , 1
        Locate LAST_ROW - 1, 1

        Color 15, 1: Print "SCHEDULE ";
        Color 14: Print "[Pg UP]  ";
        Color 15: Print "DEF STATS ";
        Color 14: Print "[Pg DN]  ";
        Color 15: Print "NEW TEAM ";
        Color 14: Print "[ESC]  ";
        Color 15: Print "SORT ";
        Color 14: Print "<-,-> and [ENTER]";
        Print Space$(5);
        Locate LAST_ROW, 1
        Color 15: Print "PRINT ";
        Color 14: Print "[P]  ";
        Color 15: Print "DELETE STATS ";
        Color 14: Print "[D]";
        Print Space$(53);

        Color 12, 0
        Locate 2, 1: Print "SORT BY: "; statCategoryAbbr$(categoryPick); Tab(40)

        Locate 4, 1
        Color 14: Print teamName$

        Locate 6, 1
        Print "NAME"; Tab(15);
        Print " G  MIN AVG   FGM  FGA  PCT  3FGM 3FGA  PCT   FTM  FTA  PCT"

        For I = 0 To 13

            If statPlyrNames$(I) <> "XXX" Then

                Color 7

                If (I Mod 2) = 0 Then Color 15

                Print Left$(statPlyrNames$(I), 13); Tab(15);

                'Games
                Print Using "## "; statPlyrVal!(I, 0);

                'Minutes
                If statPlyrVal!(I, 10) > 0 Then
                    Locate , 18: Print Using "####"; Int((statPlyrVal!(I, 10) + 30) / 60);
                    Locate , 24: Print Using "##"; CInt(((statPlyrVal!(I, 10) / 60) / statPlyrVal!(I, 0)));
                End If

                'FGM, FGA, FG PCT
                Locate , 28: Print Using "#### ####"; statPlyrVal!(I, 2); statPlyrVal!(I, 1);

                Locate , 38
                If statPlyrVal!(I, 1) = 0 Then
                    Print ".---";
                Else
                    If statPlyrVal!(I, 2) = statPlyrVal!(I, 1) Then
                        Print "1.00";
                    Else
                        If statPlyrVal!(I, 2) = 0 Then
                            Print ".000";
                        Else
                            fgPCT! = statPlyrVal!(I, 2) / statPlyrVal!(I, 1)
                            Print Using ".###"; fgPCT!;
                        End If
                    End If
                End If

                '3FGM, 3FGA
                Locate , 44: Print Using "#### ####"; stats3FGM!(I); stats3FGA!(I);

                '3FG PCT
                Locate , 54
                If stats3FGA!(I) = 0 Then
                    Print ".---";
                Else
                    If stats3FGA!(I) = stats3FGM!(I) Then
                        Print "1.00";
                    Else
                        If stats3FGM!(I) = 0 Then
                            Print ".000";
                        Else
                            threeFGPCT! = stats3FGM!(I) / stats3FGA!(I)
                            Print Using ".###"; threeFGPCT!;
                        End If
                    End If
                End If

                'FTM, FTA
                Locate , 60: Print Using "#### ####"; statPlyrVal!(I, 4); statPlyrVal!(I, 3);

                'FT PCT
                Locate , 70
                If statPlyrVal!(I, 3) = 0 Then
                    Print ".---"
                Else
                    If statPlyrVal!(I, 3) = statPlyrVal!(I, 4) Then
                        Print "1.00"
                    Else
                        If statPlyrVal!(I, 4) = 0 Then
                            Print ".000"
                        Else
                            ftPCT! = statPlyrVal!(I, 4) / statPlyrVal!(I, 3)
                            Print Using ".###"; ftPCT!
                        End If
                    End If
                End If
            End If

        Next 'done looping through all players

        ' *** Team ***

        Color 12
        Print
        Print "TEAM TOTALS";

        'Games
        Locate , 15: Print Using "##"; totalGames;

        'FGM / FGA
        Locate , 28: Print Using "#### ####"; statsOffTot!(2); statsOffTot!(1);
        If statsOffTot!(1) <> 0 Then
            fgPCT! = statsOffTot!(2) / statsOffTot!(1)
            Locate , 38: Print Using ".###"; fgPCT!;
        End If

        '3FGM / 3FGA
        Locate , 44: Print Using "#### ####"; statsOffTot!(13); statsOffTot!(14);
        If statsOffTot!(14) <> 0 Then
            threeFGPCT! = statsOffTot!(13) / statsOffTot!(14)
            Locate , 54: Print Using ".###"; threeFGPCT!;
        End If

        'FTM / FTA
        Locate , 60: Print Using "#### ####"; statsOffTot!(4); statsOffTot!(3);
        If statsOffTot!(3) <> 0 Then
            ftPCT! = statsOffTot!(4) / statsOffTot!(3)
            Locate , 70: Print Using ".###"; ftPCT!
        End If

        Color 6: Print "PER GAME";

        'FGM / FGA
        Locate , 28: Print Using "##.# ##.#"; statsOffTot!(2) / totalGames; statsOffTot!(1) / totalGames;

        '3FGM / 3FGA
        Locate , 44: Print Using "##.# ##.#"; statsOffTot!(13) / totalGames; statsOffTot!(14) / totalGames;

        'FTM / FTA
        Locate , 60: Print Using "##.# ##.#"; statsOffTot!(4) / totalGames; statsOffTot!(3) / totalGames


        ' *** Opponents ***
        Color 12
        Print "OPP TOTALS";

        'Games
        Locate , 15: Print Using "##"; totalGames;

        'FGM / FGA
        Locate , 28: Print Using "#### ####"; statsDefTot!(2); statsDefTot!(1);
        If statsDefTot!(1) <> 0 Then
            fgPCT! = statsDefTot!(2) / statsDefTot!(1)
            Locate , 38: Print Using ".###"; fgPCT!;
        End If

        '3FGM / 3FGA
        Locate , 44: Print Using "#### ####"; statsDefTot!(13); statsDefTot!(14);
        If statsDefTot!(14) <> 0 Then
            threeFGPCT! = statsDefTot!(13) / statsDefTot!(14)
            Locate , 54: Print Using ".###"; threeFGPCT!;
        End If

        'FTM / FTA
        Locate , 60: Print Using "#### ####"; statsDefTot!(4); statsDefTot!(3);
        If statsDefTot!(3) <> 0 Then
            ftPCT! = statsDefTot!(4) / statsDefTot!(3)
            Locate , 70: Print Using ".###"; ftPCT!
        End If

        Locate , 1
        Color 6: Print "PER GAME";

        'FGM / FGA
        Locate , 28: Print Using "##.# ##.#"; statsDefTot!(2) / totalGames; statsDefTot!(1) / totalGames;

        '3FGM / 3FGA
        Locate , 44: Print Using "##.# ##.#"; statsDefTot!(13) / totalGames; statsDefTot!(14) / totalGames;

        'FTM / FTA
        Locate , 60: Print Using "##.# ##.#"; statsDefTot!(4) / totalGames; statsDefTot!(3) / totalGames

        Do

            I$ = GetKeyPress$

            Select Case UCase$(I$)

                Case Chr$(0) + Chr$(81)
                    'Pg Down
                    Cls
                    Call ViewDefenseStats(statFile$, teamName$, categoryPick, totalGames, menuExit)

                Case Chr$(0) + Chr$(73)
                    'Pg Up
                    Cls
                    Call ViewScheduleSummary(statFile$, teamName$, totalGames, menuExit)
                    'Call ExtendedStatSummary(statFile$, teamName$, totalGames, menuExit)

                Case Chr$(0) + Chr$(77)
                    'Right Arrow
                    categoryPick = categoryPick + 1
                    If categoryPick > 12 Then categoryPick = 0
                    Color 12
                    Locate 2, 1: Print "SORT BY: "; statCategoryAbbr$(categoryPick); Tab(40);

                Case Chr$(0) + Chr$(75)
                    'Left Arrow
                    categoryPick = categoryPick - 1
                    If categoryPick < 0 Then categoryPick = 12
                    Color 12
                    Locate 2, 1: Print "SORT BY: "; statCategoryAbbr$(categoryPick); Tab(40);

                Case Chr$(13)
                    'Enter
                    Call SortStats(categoryPick)

                Case "D"
                    'Delete
                    Kill diskPaths$(1) + statFile$
                    Kill diskPaths$(1) + statFile$ + "S"

                Case "P"
                    'Print
                    userKey$ = GetPrintOptions$

                    If userKey$ <> Chr$(27) Then

                        Select Case userKey$

                            Case "F"
                                userFile$ = _SaveFileDialog$("Save Report File", _CWD$ + "\Report", "*.txt", "Text File")
                                If userFile$ <> "" Then
                                    Call StatsSchedule_Report(0, userFile$, teamName$, totalGames)
                                    Call StatsSummary_Report(0, userFile$, teamName$, totalGames)
                                End If

                            Case "P"
                                Call StatsSchedule_Report(1, "REPORT", teamName$, totalGames)
                                Call StatsSummary_Report(1, "REPORT", teamName$, totalGames)

                        End Select

                    End If

            End Select

        Loop Until I$ = Chr$(0) + Chr$(81) Or I$ = Chr$(27) Or I$ = Chr$(13)

        If I$ = Chr$(27) Then menuExit = 1

    Loop Until I$ <> Chr$(13)

End Sub


'----------------------------------------
'       ViewDefenseStats Subroutine
'----------------------------------------
'This subroutine formats and outputs the
'team's OFF/DEF stats to the user display
Sub ViewDefenseStats (statFile$, teamName$, categoryPick, totalGames, menuExit)

    Shared stats3FGM!(), stats3FGA!()
    Shared statsOffTot!(), statsDefTot!()
    Shared statPlyrVal!()

    Shared statCategoryAbbr$(), statPlyrNames$()

    categoryPick = 13

    Do

        Cls

        Call TITLE_STATS

        Color , 1
        Locate LAST_ROW - 1, 1

        Color 15: Print "OFF STATS ";
        Color 14: Print "[Pg UP]  ";
        Color 15: Print "SELECT NEW TEAM ";
        Color 14: Print "[ESC]  ";
        Color 15: Print "SORT ";
        Color 14: Print "<-,-> and [ENTER]";
        Print Space$(16);
        Locate LAST_ROW, 1
        Color 15: Print "PRINT ";
        Color 14: Print "[P]    ";
        Color 15: Print "DELETE STATS ";
        Color 14: Print "[D]";
        Print Space$(51);

        Color 3, 0
        Locate 2, 1: Print "SORT BY: "; statCategoryAbbr$(categoryPick); Tab(40);

        'Player Stats
        Locate 4, 1
        Color 14: Print teamName$

        Print Space$(29); "OFF  DEF  TOT"
        Print "NAME"; Tab(15);
        Print " PTS   AVG HI  REB  REB  REB   AVG   PF  AST  BLK  STL   TO"

        For I = 0 To 13

            If statPlyrNames$(I) <> "XXX" Then

                Color 7
                If (I Mod 2) = 0 Then Color 15

                'Name
                Print Left$(statPlyrNames$(I), 13);: Locate , 15

                'Points
                Print Using "####"; statPlyrVal!(I, 2) * 2 + statPlyrVal!(I, 4) + stats3FGM!(I);

                'Average
                If statPlyrVal!(I, 0) > 0 Then Locate , 21: Print Using "##.#"; (statPlyrVal!(I, 2) * 2 + statPlyrVal!(I, 4) + stats3FGM!(I)) / statPlyrVal!(I, 0);

                'Highest Points, Rebounds
                Locate , 26: Print Using "## #### #### ####"; statPlyrVal!(I, 13); statPlyrVal!(I, 12); statPlyrVal!(I, 5) - statPlyrVal!(I, 12); statPlyrVal!(I, 5);

                'Average
                If statPlyrVal!(I, 0) > 0 Then Locate , 45: Print Using "##.#"; statPlyrVal!(I, 5) / statPlyrVal!(I, 0);

                'PF AST   BLK  STL  TO
                Locate , 51: Print Using "### ####  ###  ###  ###"; statPlyrVal!(I, 6); statPlyrVal!(I, 8); statPlyrVal!(I, 9); statPlyrVal!(I, 7); statPlyrVal!(I, 11)

            End If

        Next I

        Print "TEAM";: Locate , 39: Print Using "####"; statsOffTot!(15)

        ' *** Team ***
        Color 12
        Print "TEAM TOTALS";

        'Points / Average
        If totalGames > 0 Then ptsCalc! = statsOffTot!(10) / totalGames Else ptsCalc! = 0
        Locate , 14: Print Using "##### ###.#"; statsOffTot!(10); ptsCalc!;

        'Rebounds
        If totalGames > 0 Then rebCalc! = statsOffTot!(5) / totalGames Else rebCalc! = 0
        Locate , 29: Print Using "#### #### ####  ##.#"; statsOffTot!(12); statsOffTot!(5) - statsOffTot!(12) - statsOffTot!(15); statsOffTot!(5); rebCalc!;

        'Fouls  AST  BLK  STL  TO
        Print Using " #### #### #### #### ####"; statsOffTot!(6); statsOffTot!(8); statsOffTot!(9); statsOffTot!(7); statsOffTot!(11)

        Color 6: Print "PER GAME";

        'Fouls  AST  BLK  STL  TO
        Locate , 49: Print Using " ##.# ##.# ##.# ##.# ##.#"; statsOffTot!(6) / totalGames; statsOffTot!(8) / totalGames; statsOffTot!(9) / totalGames; statsOffTot!(7) / totalGames; statsOffTot!(11) / totalGames


        ' *** Opponents ***
        Color 12
        Print "OPP TOTALS";

        'Points / Average
        If totalGames > 0 Then ptsCalc! = statsDefTot!(10) / totalGames Else ptsCalc! = 0
        Locate , 14: Print Using "##### ###.#"; statsDefTot!(10); ptsCalc!;

        'Rebounds
        If totalGames > 0 Then rebCalc! = statsDefTot!(5) / totalGames Else rebCalc! = 0
        Locate , 29: Print Using "#### #### ####  ##.#"; statsDefTot!(12); statsDefTot!(5) - statsDefTot!(12) - statsDefTot!(15); statsDefTot!(5); rebCalc!;

        'Fouls  AST  BLK  STL  TO
        Print Using " #### #### #### #### ####"; statsDefTot!(6); statsDefTot!(8); statsDefTot!(9); statsDefTot!(7); statsDefTot!(11)

        Color 6: Print "PER GAME";

        'Fouls  AST  BLK  STL  TO
        Locate , 49: Print Using " ##.# ##.# ##.# ##.# ##.#"; statsDefTot!(6) / totalGames; statsDefTot!(8) / totalGames; statsDefTot!(9) / totalGames; statsDefTot!(7) / totalGames; statsDefTot!(11) / totalGames

        Do

            J$ = GetKeyPress$

            Select Case UCase$(J$)

                Case Chr$(0) + Chr$(73)
                    'Page Up
                    Cls
                    Call ViewOffenseStats(statFile$, teamName$, categoryPick, totalGames, menuExit)

                Case Chr$(0) + Chr$(77)
                    'Right Arrow
                    categoryPick = categoryPick + 1
                    If categoryPick > 25 Then categoryPick = 13
                    Color 3
                    Locate 2, 1: Print "SORT BY: "; statCategoryAbbr$(categoryPick); Tab(40);

                Case Chr$(0) + Chr$(75)
                    'Left Arrow
                    categoryPick = categoryPick - 1
                    If categoryPick < 13 Then categoryPick = 25
                    Color 3
                    Locate 2, 1: Print "SORT BY: "; statCategoryAbbr$(categoryPick); Tab(40);

                Case Chr$(13)
                    'Enter
                    Call SortStats(categoryPick)

                Case "D"
                    Kill diskPaths$(1) + statFile$
                    Kill diskPaths$(1) + statFile$ + "S"

                Case "P"
                    userKey$ = GetPrintOptions$

                    If userKey$ <> Chr$(27) Then

                        Select Case userKey$

                            Case "F"
                                userFile$ = _SaveFileDialog$("Save Report File", _CWD$ + "\Report", "*.txt", "Text File")
                                If userFile$ <> "" Then
                                    Call StatsSchedule_Report(0, userFile$, teamName$, totalGames)
                                    Call StatsSummary_Report(0, userFile$, teamName$, totalGames)
                                End If

                            Case "P"
                                Call StatsSchedule_Report(1, "REPORT", teamName$, totalGames)
                                Call StatsSummary_Report(1, "REPORT", teamName$, totalGames)

                        End Select

                    End If

            End Select

        Loop Until J$ = Chr$(0) + Chr$(73) Or J$ = Chr$(27) Or J$ = Chr$(13)

        If J$ = Chr$(27) Then menuExit = 1

    Loop Until J$ = Chr$(27) Or J$ = Chr$(0) + Chr$(73)

End Sub


'----------------------------------------
'     ExtendedStatSummary Subroutine
'----------------------------------------
'This subroutine formats and outputs the
'team's schedule stats to the  user's display
Sub ExtendedStatSummary (statFile$, teamName$, totalGames, menuExit)

    Shared homeLosses, homeWins, neutralLosses, neutralWins
    Shared roadLosses, roadWins, totalLosses, totalWins

    Shared oppScore(), teamScore()

    Shared locIndicator$(), oppName$()

    Color , 0
    Cls

    Locate 1, 1
    Color 14, 1

    Print Tab(80); " ";
    Xcoord = Len(teamName$)
    Xcoord = Int((80 - Xcoord) / 2)
    Locate , Xcoord
    Print teamName$

    Color 15, 0

    Locate LAST_ROW, 1: Print Tab(80); " ";
    Locate 2, 1

    For I = 1 To totalGames

        Color 7

        If (I Mod 2) = 0 Then Color 15

        If I >= 55 Then

            If I <= 72 Then

                Print Using "###"; teamScore(I);
                Print Tab(5); Left$(oppName$(I), 14);
                Print Tab(20); Using "###"; oppScore(I);
                Print Tab(24); locIndicator$(I)

            Else

                Locate I - 71, 27
                Print Using "###"; teamScore(I);
                Print Tab(31); Left$(oppName$(I), 14);
                Print Tab(46); Using "###"; oppScore(I);
                Print Tab(50); locIndicator$(I)

            End If

        End If

    Next I

    Print
    Locate 21, 27
    Color 14: Print "OVERALL RECORD ";
    Color 15: Print "("; totalWins; "-"; totalLosses; ")"
    Locate , 27: Color 14: Print "HOME RECORD    ";
    Color 15: Print "("; homeWins; "-"; homeLosses; ")"
    Locate , 27: Color 14: Print "ROAD RECORD    ";
    Color 15: Print "("; roadWins; "-"; roadLosses; ")"
    Locate , 27: Color 14: Print "NEUTRAL RECORD ";
    Color 15: Print "("; neutralWins; "-"; neutralLosses; ")";
    Color 3
    Locate LAST_ROW, 1
    Color 15, 1: Print "INDIVIDUAL STATS ";
    Color 14: Print "[Pg DN]  ";
    Color 15: Print "SELECT ANOTHER TEAM ";
    Color 14: Print "[ESC]  ";
    Color 15: Print "PRINT STATS ";
    Color 14: Print "[P]           ";
    Locate 3, 1

    Do

        I$ = GetKeyPress$

        Select Case UCase$(I$)

            Case Chr$(0) + Chr$(81), Chr$(0) + Chr$(73)
                ' *** PAGE DOWN | RIGHT ARROW ***
                Color , 0
                Cls
                Call ViewOffenseStats(statFile$, teamName$, categoryPick, totalGames, menuExit)

            Case Chr$(0) + Chr$(73), Chr$(0) + Chr$(75)
                ' *** PAGE UP | LEFT ARROW ***
                Color , 0
                Cls
                Call ViewScheduleSummary(statFile$, teamName$, totalGames, menuExit)

            Case "D":
                'Delete
                Kill diskPaths$(1) + statFile$
                Kill diskPaths$(1) + statFile$ + "S"

            Case "P":
                'Print
                userKey$ = GetPrintOptions$

                If userKey$ <> Chr$(27) Then

                    Select Case userKey$

                        Case "F"
                            userFile$ = _SaveFileDialog$("Save Report File", _CWD$ + "\Report", "*.txt", "Text File")
                            If userFile$ <> "" Then
                                Call StatsSchedule_Report(0, userFile$, teamName$, totalGames)
                                Call StatsSummary_Report(0, userFile$, teamName$, totalGames)
                            End If

                        Case "P"
                            Call StatsSchedule_Report(1, "REPORT", teamName$, totalGames)
                            Call StatsSummary_Report(1, "REPORT", teamName$, totalGames)

                    End Select

                End If

        End Select

    Loop Until I$ = Chr$(27) Or I$ = Chr$(0) + "Q" Or menuExit = 1

    If I$ = Chr$(27) Then menuExit = 1

End Sub


'----------------------------------------
'     StatsSchedule_Report Subroutine
'----------------------------------------
'This subroutine formats and outputs the
'team's schedule results to the default printer
Sub StatsSchedule_Report (printDest, destFile$, teamName$, totalGames)

    Shared homeLosses, homeWins, neutralLosses, neutralWins
    Shared roadLosses, roadWins, totalLosses, totalWins

    Shared teamScore(), oppScore()

    Shared locIndicator$(), oppName$()

    Color 15, 0
    Cls

    Open destFile$ For Output As #1

    Print #1, "SCHEDULE RESULTS FOR "; teamName$
    Print #1,

    For I = 1 To 28

        Print #1, Using "###"; teamScore(I);
        Print #1, Tab(5); Left$(oppName$(I), 14);
        Print #1, Tab(20); Using "###"; oppScore(I);
        Print #1, Tab(24); locIndicator$(I); Tab(27);

        If I + 28 <= totalGames Then
            Print #1, Using "###"; teamScore(I + 28);
            Print #1, Tab(31); Left$(oppName$(I + 28), 14);
            Print #1, Tab(46); Using "###"; oppScore(I + 28);
            Print #1, Tab(50); locIndicator$(I + 28); Tab(53);

            If I + 56 <= totalGames Then
                Print #1, Using "###"; teamScore(I + 56);
                Print #1, Tab(57); Left$(oppName$(I + 56), 14);
                Print #1, Tab(72); Using "###"; oppScore(I + 56);
                Print #1, Tab(76); locIndicator$(I + 56)
            Else
                Print #1,
            End If
        Else
            Print #1,
        End If

    Next I

    Print #1, Chr$(12)

    Print #1,
    Print #1, Tab(27); "OVERALL RECORD ("; totalWins; "-"; totalLosses; ")"
    Print #1, Tab(27); "HOME RECORD    ("; homeWins; "-"; homeLosses; ")"
    Print #1, Tab(27); "ROAD RECORD    ("; roadWins; "-"; roadLosses; ")"
    Print #1, Tab(27); "NEUTRAL RECORD ("; neutralWins; "-"; neutralLosses; ")"

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
'     StatsSummary_Report Subroutine
'----------------------------------------
'This subroutine formats and outputs the
'team's stats to the default printer
Sub StatsSummary_Report (printDest, destFile$, teamName$, totalGames)

    Shared statsOffTot!(), statsDefTot!()
    Shared stats3FGM!(), stats3FGA!()
    Shared statPlyrVal!()

    Shared statPlyrNames$()

    Color 15, 0
    Cls

    Open destFile$ For Output As #1

    Print #1, "STATISTICS SUMMARY FOR "; teamName$
    Print #1, "NAME"; Tab(15);
    Print #1, " G  MIN AVG   FGM  FGA  PCT  3FGM 3FGA  PCT   FTM  FTA  PCT"

    For I = 0 To 13
        Print #1, Left$(statPlyrNames$(I), 13); Tab(15);

        'Games
        Print #1, Using "## "; statPlyrVal!(I, 0);

        'Min, Avg
        If statPlyrVal!(I, 10) > 0 Then
            Print #1, Tab(18); Using "####"; Int((statPlyrVal!(I, 10) + 30) / 60);
            Print #1, Tab(24); Using "##"; CInt(((statPlyrVal!(I, 10) / 60) / statPlyrVal!(I, 0)));
        End If

        'FGM, FGA, FG PCT
        Print #1, Tab(28); Using "#### ####"; statPlyrVal!(I, 2); statPlyrVal!(I, 1);
        Print #1, Tab(38);

        If statPlyrVal!(I, 1) = 0 Then
            Print #1, ".---";
        Else
            If statPlyrVal!(I, 2) = statPlyrVal!(I, 1) Then
                Print #1, "1.00";
            Else
                If statPlyrVal!(I, 2) = 0 Then
                    Print #1, ".000";
                Else
                    fgPCT! = statPlyrVal!(I, 2) / statPlyrVal!(I, 1)
                    Print #1, Using ".###"; fgPCT!;
                End If
            End If
        End If

        '3FGM, 3FGA, 3FG PCT
        Print #1, Tab(44); Using "#### ####"; stats3FGM!(I); stats3FGA!(I);: Print #1, Tab(54);

        If stats3FGA!(I) = 0 Then
            Print #1, ".---";
        Else
            If stats3FGA!(I) = stats3FGM!(I) Then
                Print #1, "1.00";
            Else
                If stats3FGM!(I) = 0 Then
                    Print #1, ".000";
                Else
                    threeFGPCT! = stats3FGM!(I) / stats3FGA!(I)
                    Print #1, Using ".###"; threeFGPCT!;
                End If
            End If
        End If

        'FTM, FTA, FT PCT
        Print #1, Tab(60); Using "#### ####"; statPlyrVal!(I, 4); statPlyrVal!(I, 3);
        Print #1, Tab(70);

        If statPlyrVal!(I, 3) = 0 Then
            Print #1, ".---"
        Else
            If statPlyrVal!(I, 3) = statPlyrVal!(I, 4) Then
                Print #1, "1.00"
            Else
                If statPlyrVal!(I, 4) = 0 Then
                    Print #1, ".000"
                Else
                    ftPCT! = statPlyrVal!(I, 4) / statPlyrVal!(I, 3)
                    Print #1, Using ".###"; ftPCT!
                End If
            End If
        End If

    Next 'done looping through players

    Print #1,
    Print #1, "TOTALS"; Tab(15);

    'Games
    Print #1, Using "##"; totalGames;

    'FGM / FGA / FG PCT
    Print #1, Tab(28); Using "#### ####"; statsOffTot!(2); statsOffTot!(1);
    If statsOffTot!(1) <> 0 Then
        fgPCT! = statsOffTot!(2) / statsOffTot!(1)
        Print #1, Tab(38); Using ".###"; fgPCT!;
    End If

    '3FGM / 3FGA / 3FG PCT
    Print #1, Tab(44); Using "#### ####"; statsOffTot!(13); statsOffTot!(14);
    If statsOffTot!(14) <> 0 Then
        threeFGPCT! = statsOffTot!(13) / statsOffTot!(14)
        Print #1, Tab(54); Using ".###"; threeFGPCT!;
    End If

    'FTM / FTA / FT PCT
    Print #1, Tab(60); Using "#### ####"; statsOffTot!(4); statsOffTot!(3);
    If statsOffTot!(3) <> 0 Then
        ftPCT! = statsOffTot!(4) / statsOffTot!(3)
        Print #1, Tab(70); Using ".###"; ftPCT!
    End If

    'Opponents

    'Games
    Print #1, Tab(15); Using "##"; totalGames;

    'FGM / FGA / FG PCT
    Print #1, Tab(28); Using "#### ####"; statsDefTot!(2); statsDefTot!(1);
    If statsDefTot!(1) <> 0 Then
        fgPCT! = statsDefTot!(2) / statsDefTot!(1)
        Print #1, Tab(38); Using ".###"; fgPCT!;
    End If

    '3FGM / 3FGA / 3FG PCT
    Print #1, Tab(44); Using "#### ####"; statsDefTot!(13); statsDefTot!(14);
    If statsDefTot!(14) <> 0 Then
        threeFGPCT! = statsDefTot!(13) / statsDefTot!(14)
        Print #1, Tab(54); Using ".###"; threeFGPCT!;
    End If

    'FTM / FTA / FT PCT
    Print #1, Tab(60); Using "#### ####"; statsDefTot!(4); statsDefTot!(3);
    If statsDefTot!(3) <> 0 Then
        ftPCT! = statsDefTot!(4) / statsDefTot!(3)
        Print #1, Tab(70); Using ".###"; ftPCT!
    End If

    'Player Stats
    Print #1,
    Print #1, Tab(15);
    Print #1, "               OFF  DEF  TOT": Print #1, Tab(15);
    Print #1, " PTS   AVG HI  REB  REB  REB   AVG   PF  AST  BLK  STL   TO"

    For I = 0 To 13

        'Player Name
        Print #1, Left$(statPlyrNames$(I), 13);: Print #1, Tab(15);

        'Points
        Print #1, Using "####"; statPlyrVal!(I, 2) * 2 + statPlyrVal!(I, 4) + stats3FGM!(I);

        'Average
        If statPlyrVal!(I, 0) > 0 Then
            Print #1, Tab(21); Using "##.#"; (statPlyrVal!(I, 2) * 2 + statPlyrVal!(I, 4) + stats3FGM!(I)) / statPlyrVal!(I, 0);
        End If

        'Highest Points, Rebounds
        Print #1, Tab(26); Using "## #### #### ####"; statPlyrVal!(I, 13); statPlyrVal!(I, 12); statPlyrVal!(I, 5) - statPlyrVal!(I, 12); statPlyrVal!(I, 5);

        'Average Rebounds
        If statPlyrVal!(I, 0) > 0 Then
            Print #1, Tab(45); Using "##.#"; statPlyrVal!(I, 5) / statPlyrVal!(I, 0);
        End If

        'Fouls / AST / BLK / STL / TO
        Print #1, Tab(51); Using "### ####  ###  ###  ###"; statPlyrVal!(I, 6); statPlyrVal!(I, 8); statPlyrVal!(I, 9); statPlyrVal!(I, 7); statPlyrVal!(I, 11)

    Next I

    Print #1, "TEAM";: Print #1, Tab(39); Using "####"; statsOffTot!(15)

    Print #1,
    Print #1, "TOTALS";

    'Points
    If totalGames > 0 Then ptsCalc! = statsOffTot!(10) / totalGames Else ptsCalc! = 0
    Print #1, Tab(14); Using "##### ###.#"; statsOffTot!(10); ptsCalc!;

    'Rebounds
    If totalGames > 0 Then rebCalc! = statsOffTot!(5) / totalGames Else rebCalc! = 0
    Print #1, Tab(29); Using "#### #### ####  ##.#"; statsOffTot!(12); statsOffTot!(5) - statsOffTot!(12) - statsOffTot!(15); statsOffTot!(5); rebCalc!;

    'Fouls  AST  BLK  STL  TO
    Print #1, Using " #### #### #### #### ####"; statsOffTot!(6); statsOffTot!(8); statsOffTot!(9); statsOffTot!(7); statsOffTot!(11)

    'Points
    If totalGames > 0 Then ptsCalc! = statsDefTot!(10) / totalGames Else ptsCalc! = 0
    Print #1, Tab(14); Using "##### ###.#"; statsDefTot!(10); ptsCalc!;

    'Rebounds
    If totalGames > 0 Then rebCalc! = statsDefTot!(5) / totalGames Else rebCalc! = 0
    Print #1, Tab(29); Using "#### #### ####  ##.#"; statsDefTot!(12); statsDefTot!(5) - statsDefTot!(12) - statsDefTot!(15); statsDefTot!(5); rebCalc!;

    'Fouls  AST  BLK  STL  TO
    Print #1, Using " #### #### #### #### ####"; statsDefTot!(6); statsDefTot!(8); statsDefTot!(9); statsDefTot!(7); statsDefTot!(11)

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
'          SortStats Subroutine
'----------------------------------------
'This subroutine sorts the associate stats
'based on the chosen category
Sub SortStats (categoryPick)

    Shared stats3FGM!(), stats3FGA!()
    Shared statPlyrVal!()

    Shared statPlyrNames$()

    'SORT BASED ON CATEGORY
    Select Case categoryPick

        Case 1: SG% = 0
        Case 2: SG% = 10
        Case 3: SG% = 14
        Case 4: SG% = 2
        Case 5: SG% = 1
        Case 6: SG% = 15
        Case 7: SG% = 18
        Case 8: SG% = 19
        Case 9: SG% = 16
        Case 10: SG% = 4
        Case 11: SG% = 3
        Case 12: SG% = 17
        Case 14: SG% = 22
        Case 15: SG% = 20
        Case 16: SG% = 13
        Case 17: SG% = 12
        Case 18: SG% = 23
        Case 19: SG% = 5
        Case 20: SG% = 21
        Case 21: SG% = 6
        Case 22: SG% = 8
        Case 23: SG% = 9
        Case 24: SG% = 7
        Case 25: SG% = 11

    End Select

    If categoryPick = 0 Or categoryPick = 13 Then

        For Z = 0 To 12

            For Z1 = Z + 1 To 13

                If _StriCmp(statPlyrNames$(Z), statPlyrNames$(Z1)) = 1 Then

                    Swap statPlyrNames$(Z), statPlyrNames$(Z1)

                    For I = 0 To 24:
                        Swap statPlyrVal!(Z, I), statPlyrVal!(Z1, I)
                    Next

                    Swap stats3FGM!(Z), stats3FGM!(Z1)
                    Swap stats3FGA!(Z), stats3FGA!(Z1)

                End If

            Next Z1

        Next Z

    Else

        For Z = 0 To 12

            For Z1 = Z + 1 To 13

                If statPlyrVal!(Z, SG%) < statPlyrVal!(Z1, SG%) Then

                    For I = 0 To 24:
                        Swap statPlyrVal!(Z, I), statPlyrVal!(Z1, I)
                    Next

                    Swap stats3FGM!(Z), stats3FGM!(Z1)
                    Swap stats3FGA!(Z), stats3FGA!(Z1)
                    Swap statPlyrNames$(Z), statPlyrNames$(Z1)

                End If

            Next Z1

        Next Z

    End If

End Sub


'----------------------------------------
'       ResetStatPrompt Subroutine
'----------------------------------------
'This subroutine prompts the user with
'a choice to reset stats. If they chose,
'and confirm, it will then proceed to
'delete the files.
Sub ResetStatPrompt (statFile$)

    Cls

    Call MenuBox(6, 8, 61, 1, 15, 1)

    Do
        Locate 7
        U$ = "RESET COMPILED STATS TO 0? (Y/N)"
        Locate , (80 - Len(U$)) / 2
        Print U$
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Color 15, 0

    If UCase$(I$) = "Y" Then

        Call MenuBox(6, 8, 61, 1, 15, 1)

        Locate 7

        Do
            U$ = "ARE YOU SURE? (Y/N)"
            Locate , (80 - Len(U$)) / 2
            Print U$
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        Color 15, 0

        If UCase$(I$) = "Y" Then
            Kill diskPaths$(1) + statFile$
            Kill diskPaths$(1) + statFile$ + "S"
            If _FileExists(diskPaths$(1) + statFile$ + "R") Then Kill diskPaths$(1) + statFile$ + "R"
        End If

    End If 'Done checking if reset stats

End Sub
