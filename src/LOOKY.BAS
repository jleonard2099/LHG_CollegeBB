'----------------------------------------
'            LOOKY Subroutine
'----------------------------------------
'This subroutine guides the user through
'choosing a team file and then calls other
'routines to load and view the stats
Sub LOOKY ()

    _Title "Courtside College Basketball - Stats Viewer"

    Shared teamRatings%()

    Do

        menuExit = 0

        Call ChooseTeam(YN$, NTMS, teamNbr, team$)

        If teamNbr > 0 Then

            statFile$ = nameStatFile$(team$)

            If Not _FileExists(diskPaths$(1) + statFile$) And teamNbr > 0 Then

                MsgText$ = "NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU"
                BoxWidth = 24
                BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                Call CSMsgBox(MsgText$, BoxWidth, PASS)
                I$ = GetKeyPress$
                Call CSMsgBox("", 0, PASS)

            Else

                Call LoadTeamStats(statFile$, team$, totalGames%)
                Call ViewStatSummary(statFile$, team$, totalGames%, menuExit)

            End If

        End If

    Loop Until menuExit = 0 Or teamNbr = 0

End Sub


'----------------------------------------
'        LoadTeamStats Subroutine
'----------------------------------------
'This subroutine loads the selected team's
'stats into arrays
Sub LoadTeamStats (statFile$, teamName$, totalGames%)

    Dim P%(0 to 1)

    'Do these need shared based on the below???
    'All are assigned 0s and read in from file
    Shared homeLosses, homeWins, neutralLosses, neutralWins
    Shared roadLosses, roadWins, totalLosses, totalWins

    Shared gameAttendance&()

    Shared powerRating(), teamScore(), oppScore()
    
    Shared statsW0!(), statsW1!()
    Shared statsZ!(), statsZ1!()
    Shared values!()

    Shared gameSite$()
    Shared statPlyrNames$(), locIndicator$(), oppName$()

    ' *** READ IN TEAM STATS ***

    P%(0) = 0
    P%(1) = 0

    totalLosses = 0
    totalWins = 0
    tiedGames = 0

    homeWins = 0
    roadWins = 0
    neutralWins = 0

    homeLosses = 0
    roadLosses = 0
    neutralLosses = 0

    Open diskPaths$(1) + statFile$ + "S" For Input As #1

    Input #1, totalGames%

    For I = 1 To MAX_SCHED_STATS: 
        Input #1, teamScore(I)
    Next I

    For I = 1 To MAX_SCHED_STATS: 
        Input #1, oppName$(I)
    Next I

    For I = 1 To MAX_SCHED_STATS: 
        Input #1, oppScore(I)
    Next I

    For I = 1 To MAX_SCHED_STATS: 
        Input #1, locIndicator$(I)
    Next I

    For I = 1 To MAX_SCHED_STATS
        For I1 = 0 To 1: 
            Input #1, powerRating(I, I1)
        Next
    Next

    For I = 1 To MAX_SCHED_STATS: 
        Input #1, gameAttendance&(I)
    Next I

    For I = 1 To MAX_SCHED_STATS: 
        Input #1, gameSite$(I)
    Next I

    Close #1

    For I = 1 To totalGames%
        If teamScore(I) > oppScore(I) Then totalWins = totalWins + 1
        If teamScore(I) > oppScore(I) And Left$(locIndicator$(I), 1) = "H" Then homeWins = homeWins + 1
        If teamScore(I) > oppScore(I) And Left$(locIndicator$(I), 1) = "V" Then roadWins = roadWins + 1
        If teamScore(I) > oppScore(I) And Left$(locIndicator$(I), 1) = "N" Then neutralWins = neutralWins + 1

        If teamScore(I) < oppScore(I) Then totalLosses = totalLosses + 1
        If teamScore(I) < oppScore(I) And Left$(locIndicator$(I), 1) = "H" Then homeLosses = homeLosses + 1
        If teamScore(I) < oppScore(I) And Left$(locIndicator$(I), 1) = "V" Then roadLosses = roadLosses + 1
        If teamScore(I) < oppScore(I) And Left$(locIndicator$(I), 1) = "N" Then neutralLosses = neutralLosses + 1

        If teamScore(I) = oppScore(I) Then tiedGames = tiedGames + 1
        P%(0) = P%(0) + teamScore(I)
        P%(1) = P%(1) + oppScore(I)
    Next

    Open diskPaths$(1) + statFile$ For Input As #2

    Input #2, teamName$

    For I = 0 To 13:

        Input #2, statPlyrNames$(I)

        For J = 0 To 13:
            Input #2, values!(I, J)
        Next

        Input #2, statsW0!(I)
        Input #2, statsW1!(I)

    Next

    For K = 0 To 15: Input #2, statsZ!(K): Next

    For M = 0 To 15: Input #2, statsZ1!(M): Next

    Close #2

    For I = 0 To 13
        If values!(I, 0) <> 0 Then values!(I, 14) = values!(I, 10) / values!(I, 0): values!(I, 20) = (values!(I, 2) * 2 + values!(I, 4) + statsW0!(I)) / values!(I, 0): values!(I, 21) = values!(I, 5) / values!(I, 0)
        values!(I, 22) = values!(I, 2) * 2 + values!(I, 4) + statsW0!(I)
        values!(I, 23) = values!(I, 5) - values!(I, 12)
        If values!(I, 1) <> 0 Then values!(I, 15) = values!(I, 2) / values!(I, 1)
        values!(I, 18) = statsW0!(I)
        values!(I, 19) = statsW1!(I)
        If values!(I, 19) <> 0 Then values!(I, 16) = values!(I, 18) / values!(I, 19)
        If values!(I, 3) <> 0 Then values!(I, 17) = values!(I, 4) / values!(I, 3)
    Next

End Sub


'----------------------------------------
'       ViewStatSummary Subroutine
'----------------------------------------
'This subroutine formats and outputs the
'selected team's record to the display,
'then provides routines for output of
'the other stat categories
Sub ViewStatSummary (statFile$, teamName$, totalGames%, menuExit)

    'Do these need shared based on the below???
    'All are assigned 0s and read in from file
    Shared homeLosses, homeWins, neutralLosses, neutralWins
    Shared roadLosses, roadWins, totalLosses, totalWins

    Shared teamScore(), oppScore()

    Shared statsZ!(), statsZ1!()
    Shared values!()

    Shared statPlyrNames$(), locIndicator$(), oppName$()

    Color , 0
    Cls

    Locate 1, 1
    Color 14, 1

    Print Tab(80);
    Xcoord = Len(teamName$)
    Xcoord = Int((80 - Xcoord) / 2)
    Locate , Xcoord
    Print teamName$

    Locate 25, 1: Print Tab(80);
    Locate 2, 1
    Color 15, 0

    For I = 1 To totalGames%

        Color 7

        If I / 2 = Int(I / 2) Then Color 15

        If I > 54 Then Exit For

        If I > 36 Then
            Locate I - 35, 53: Print Using "###"; teamScore(I);
            Print Tab(57); Left$(oppName$(I), 14);
            Print Tab(72); Using "###"; oppScore(I);
            Print Tab(76); locIndicator$(I)

        Else

            If I > 18 Then
                Locate I - 17, 27: Print Using "###"; teamScore(I);
                Print Tab(31); Left$(oppName$(I), 14);
                Print Tab(46); Using "###"; oppScore(I);
                Print Tab(50); locIndicator$(I)
            Else
                Print Using "###"; teamScore(I);
                Print Tab(5); Left$(oppName$(I), 14);
                Print Tab(20); Using "###"; oppScore(I);
                Print Tab(24); locIndicator$(I)
            End If

        End If

    Next I

    Print

    Locate 21, 27
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
    Locate 25, 1
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

            'Page Down
            Case Chr$(0) + "Q":
                Color , 0
                Cls
                Call ExtendedStatSummary(statFile$, teamName$, totalGames%, menuExit)

            Case "D":
                Kill diskPaths$(1) + statFile$
                Kill diskPaths$(1) + statFile$ + "S"

            Case "P":
                'Color 0, 15

                Call PrintStatsSchedule(teamName$, totalGames%)
                Call PrintStatsSummary(teamName$, totalGames%)

        End Select

    Loop Until I$ = Chr$(0) + "Q" Or I$ = Chr$(27)

    If I$ = Chr$(27) Then menuExit = 1

End Sub


'----------------------------------------
'       ViewPlayerStats Subroutine
'----------------------------------------
'This subroutine formats and outputs the
'team's player stats to the user display
Sub ViewPlayerStats (statFile$, teamName$, categoryPick, totalGames%, menuExit)

    Shared statsW0!(), statsW1!()
    Shared statsZ!(), statsZ1!()
    Shared values!()

    Shared statCategoryAbbr$(), statPlyrNames$()

    categoryPick = 1

    Do

        Call TITLE_STATS

        Color , 1
        Locate 24, 1

        Color 15, 1: Print "SCHEDULE ";
        Color 14: Print "[Pg UP]  ";
        Color 15: Print "MORE STATS ";
        Color 14: Print "[Pg DN]  ";
        Color 15: Print "NEW TEAM ";
        Color 14: Print "[ESC]  ";
        Color 15: Print "SORT ";
        Color 14: Print "<-,-> and [ENTER]";
        Print Space$(4);
        Locate 25, 1
        Color 15: Print "PRINT ";
        Color 14: Print "[P]  ";
        Color 15: Print "DELETE STATS ";
        Color 14: Print "[D]";
        Print Space$(53);

        Color 12, 0
        Locate 2, 1: Print "SORT BY: "; statCategoryAbbr$(categoryPick); Tab(40)

        Color 14: Print teamName$

        Print "NAME"; Tab(15);
        Print " G  MIN AVG   FGM  FGA  PCT  3FGM 3FGA  PCT   FTM  FTA  PCT"

        For I = 0 To 13

            If statPlyrNames$(I) <> "XXX" Then

                Color 7

                If I / 2 = Int(I / 2) Then Color 15

                Print Left$(statPlyrNames$(I), 13); Tab(15);

                'Games
                Print Using "## "; values!(I, 0);

                'Minutes
                If values!(I, 10) > 0 Then 
                    Locate , 18: Print Using "####"; Int((values!(I, 10) + 30) / 60);
                    Locate , 24: Print Using "##"; Int(((values!(I, 10) / 60) / values!(I, 0)) + .5);
                End If

                'FGM, FGA, FG PCT
                Locate , 28: Print Using "#### ####"; values!(I, 2); values!(I, 1);

                Locate , 38
                If values!(I, 1) = 0 Then
                    Print ".---";
                Else
                    If values!(I, 2) = values!(I, 1) Then
                        Print "1.00";
                    Else
                        If values!(I, 2) = 0 Then
                            Print ".000";
                        Else
                            statPCT! = values!(I, 2) / values!(I, 1)
                            Print Using ".###"; statPCT!;
                        End If
                    End If
                End If

                '3FGM, 3FGA, 3FG PCT
                Locate , 44: Print Using "#### ####"; statsW0!(I); statsW1!(I);
                
                Locate , 54
                If statsW1!(I) = 0 Then
                    Print ".---";
                Else
                    If statsW1!(I) = statsW0!(I) Then
                        Print "1.00";
                    Else
                        If statsW0!(I) = 0 Then
                            Print ".000";
                        Else
                            statPCT! = statsW0!(I) / statsW1!(I)
                            Print Using ".###"; statPCT!;
                        End If
                    End If
                End If

                'FTM, FTA, FT PCT
                Locate , 60: Print Using "#### ####"; values!(I, 4); values!(I, 3);
                
                Locate , 70
                If values!(I, 3) = 0 Then
                    Print ".---"
                Else
                    If values!(I, 3) = values!(I, 4) Then
                        Print "1.00"
                    Else
                        If values!(I, 4) = 0 Then
                            Print ".000"
                        Else
                            statPCT! = values!(I, 4) / values!(I, 3)
                            Print Using ".###"; statPCT!
                        End If
                    End If
                End If
            End If

        Next 'done looping through all players

        Color 12
        Print
        Print "TOTALS";
        Locate , 15: Print Using "##"; totalGames%;

        'FGM / FGA
        Locate , 28: Print Using "#### ####"; statsZ!(2); statsZ!(1);
        If statsZ!(1) <> 0 Then 
            statPCT! = statsZ!(2) / statsZ!(1)
            Locate , 38: Print Using ".###"; statPCT!;
        End If

        '3FGM / 3FGA
        Locate , 44: Print Using "#### ####"; statsZ!(13); statsZ!(14);
        If statsZ!(14) <> 0 Then 
            statPCT! = statsZ!(13) / statsZ!(14)
            Locate , 54: Print Using ".###"; statPCT!;
        End If

        'FTM / FTA
        Locate , 60: Print Using "#### ####"; statsZ!(4); statsZ!(3);
        If statsZ!(3) <> 0 Then 
            statPCT! = statsZ!(4) / statsZ!(3)
            Locate , 70: Print Using ".###"; statPCT!
        End If

        'Opponents

        'Games
        Color 12
        Locate , 15: Print Using "##"; totalGames%;

        'FGM / FGA
        Locate , 28: Print Using "#### ####"; statsZ1!(2); statsZ1!(1);
        If statsZ1!(1) <> 0 Then 
            statPCT! = statsZ1!(2) / statsZ1!(1)
            Locate , 38: Print Using ".###"; statPCT!;
        End If

        '3FGM / 3FGA
        Locate , 44: Print Using "#### ####"; statsZ1!(13); statsZ1!(14);
        If statsZ1!(14) <> 0 Then 
            statPCT! = statsZ1!(13) / statsZ1!(14)
            Locate , 54: Print Using ".###"; statPCT!;
        End If

        'FTM / FTA
        Locate , 60: Print Using "#### ####"; statsZ1!(4); statsZ1!(3);
        If statsZ1!(3) <> 0 Then 
            statPCT! = statsZ1!(4) / statsZ1!(3)
            Locate , 70: Print Using ".###"; statPCT!
        End If

        Do
            I$ = GetKeyPress$

            Select Case I$

                'Pg Down
                Case Chr$(0) + "Q"
                    Call ViewTeamStats(statFile$, teamName$, categoryPick, totalGames%, menuExit)

                    'Pg Up
                Case Chr$(0) + "I"
                    Cls
                    Call ExtendedStatSummary(statFile$, teamName$, totalGames%, menuExit)

                    'Right Arrow
                Case Chr$(0) + "M"
                    categoryPick = categoryPick + 1
                    If categoryPick > 13 Then categoryPick = 1
                    Color 12
                    Locate 2, 1: Print "SORT BY: "; statCategoryAbbr$(categoryPick); Tab(40);

                    'Left Arrow
                Case Chr$(0) + "K"
                    categoryPick = categoryPick - 1
                    If categoryPick < 1 Then categoryPick = 13
                    Color 12
                    Locate 2, 1: Print "SORT BY: "; statCategoryAbbr$(categoryPick); Tab(40);

                    'Enter
                Case Chr$(13)
                    Call SortStats(categoryPick)

                    'Delete
                Case "d", "D"
                    Kill diskPaths$(1) + statFile$
                    Kill diskPaths$(1) + statFile$ + "S"

                    'Print
                Case "p", "P"
                    Call PrintStatsSchedule(teamName$, totalGames%)
                    Call PrintStatsSummary(teamName$, totalGames%)

            End Select

        Loop Until I$ = Chr$(0) + "Q" Or I$ = Chr$(27) Or I$ = Chr$(13)

        If I$ = Chr$(27) Then menuExit = 1

    Loop Until I$ <> Chr$(13)

End Sub


'----------------------------------------
'       ViewTeamStats Subroutine
'----------------------------------------
'This subroutine formats and outputs the
'team's OFF/DEF stats to the user display
Sub ViewTeamStats (statFile$, teamName$, categoryPick, totalGames%, menuExit)


    Shared statsW0!(), statsW1!()
    Shared statsZ!(), statsZ1!()
    Shared values!()

    Shared statCategoryAbbr$(), statPlyrNames$()

    categoryPick = 14

    Do

        Cls

        Call TITLE_STATS

        Color , 1
        Locate 24, 1

        Color 15: Print "MORE STATS ";
        Color 14: Print "[Pg UP]  ";
        Color 15: Print "SELECT NEW TEAM ";
        Color 14: Print "[ESC]  ";
        Color 15: Print "SORT ";
        Color 14: Print "<-,-> and [ENTER]";
        Print Space$(15);
        Locate 25, 1
        Color 15: Print "PRINT ";
        Color 14: Print "[P]    ";
        Color 15: Print "DELETE STATS ";
        Color 14: Print "[D]";
        Print Space$(51);

        Color 3, 0
        Locate 2, 1: Print "SORT BY: "; statCategoryAbbr$(categoryPick); Tab(40);

        'Player Stats
        Locate 3, 1
        Color 14, 0: Print teamName$;
        Locate , 15
        Print "               OFF  DEF  TOT": Print "NAME"; Tab(15);
        Print " PTS   AVG HI  REB  REB  REB   AVG   PF  AST  BLK  STL   TO"

        For I = 0 To 13

            If statPlyrNames$(I) <> "XXX" Then

                Color 7
                If I / 2 = Int(I / 2) Then Color 15

                'Name
                Print Left$(statPlyrNames$(I), 13);: Locate , 15

                'Points
                Print Using "####"; values!(I, 2) * 2 + values!(I, 4) + statsW0!(I);

                'Average
                If values!(I, 0) > 0 Then Locate , 21: Print Using "##.#"; (values!(I, 2) * 2 + values!(I, 4) + statsW0!(I)) / values!(I, 0);

                'Highest Points, Rebounds
                Locate , 26: Print Using "## #### #### ####"; values!(I, 13); values!(I, 12); values!(I, 5) - values!(I, 12); values!(I, 5);

                'Average
                If values!(I, 0) > 0 Then Locate , 45: Print Using "##.#"; values!(I, 5) / values!(I, 0);

                'PF AST   BLK  STL  TO
                Locate , 51: Print Using "### ####  ###  ###  ###"; values!(I, 6); values!(I, 8); values!(I, 9); values!(I, 7); values!(I, 11)

            End If

        Next I

        Print "TEAM";: Locate , 39: Print Using "####"; statsZ!(15)

        Color 12: Print "TOTALS";

        'Team

        'Points
        If totalGames% > 0 Then statVal! = statsZ!(10) / totalGames% Else statVal! = 0
        Locate , 14: Print Using "##### ###.#"; statsZ!(10); statVal!;

        'Rebounds
        If totalGames% > 0 Then statVal! = statsZ!(5) / totalGames%
        Locate , 29: Print Using "#### #### ####  ##.#"; statsZ!(12); statsZ!(5) - statsZ!(12) - statsZ!(15); statsZ!(5); statVal!;

        'Fouls  AST  BLK  STL  TO
        Print Using " #### #### #### #### ####"; statsZ!(6); statsZ!(8); statsZ!(9); statsZ!(7); statsZ!(11)

        'Opponents

        'Points
        If totalGames% > 0 Then statVal = statsZ!(10) / totalGames% Else statVal! = 0
        Locate , 14: Print Using "##### ###.#"; statsZ1!(10); statVal;

        'Rebounds
        If totalGames% > 0 Then statVal! = statsZ1!(5) / totalGames% Else statVal! = 0
        Locate , 29: Print Using "#### #### ####  ##.#"; statsZ1!(12); statsZ1!(5) - statsZ1!(12) - statsZ1!(15); statsZ1!(5); statVal!;

        'Fouls  AST  BLK  STL  TO
        Print Using " #### #### #### #### ####"; statsZ1!(6); statsZ1!(8); statsZ1!(9); statsZ1!(7); statsZ1!(11)

        Do

            I$ = GetKeyPress$

            Select Case I$

                'Page Up
                Case Chr$(0) + "I"
                    Cls
                    Call ViewPlayerStats(statFile$, teamName$, categoryPick, totalGames%, menuExit)

                    'Right Arrow
                Case Chr$(0) + "M"
                    categoryPick = categoryPick + 1
                    If categoryPick > 25 Then categoryPick = 14
                    Color 3
                    Locate 2, 1: Print "SORT BY: "; statCategoryAbbr$(categoryPick); Tab(40);

                    'Left Arrow
                Case Chr$(0) + "K"
                    categoryPick = categoryPick - 1
                    If categoryPick < 14 Then categoryPick = 25
                    Color 3
                    Locate 2, 1: Print "SORT BY: "; statCategoryAbbr$(categoryPick); Tab(40);

                Case Chr$(13)
                    Call SortStats(categoryPick)

                Case "d", "D"
                    Kill diskPaths$(1) + statFile$
                    Kill diskPaths$(1) + statFile$ + "S"

                Case "p", "P"
                    Call PrintStatsSchedule(teamName$, totalGames%)
                    Call PrintStatsSummary(teamName$, totalGames%)

            End Select

        Loop Until I$ = Chr$(27) Or I$ = Chr$(13)

        If I$ = Chr$(27) Then menuExit = 1

    Loop Until I$ = Chr$(27) Or I$ = Chr$(0) + "I"
    'Loop Until I$ <> Chr$(13)

End Sub


'----------------------------------------
'     ExtendedStatSummary Subroutine
'----------------------------------------
'This subroutine formats and outputs the
'team's schedule stats to the  user's display
Sub ExtendedStatSummary (statFile$, teamName$, totalGames%, menuExit)

    Shared homeLosses, homeWins, neutralLosses, neutralWins
    Shared roadLosses, roadWins, totalLosses, totalWins

    Shared teamScore(), oppScore()

    Shared statsW0!(), statsW1!()
    Shared statsZ!(), statsZ1!()

    Shared locIndicator$(), oppName$()

    Color , 0
    Cls

    Locate 1, 1
    Color 14, 1

    Print Tab(80);
    Xcoord = Len(teamName$)
    Xcoord = Int((80 - Xcoord) / 2)
    Locate , Xcoord
    Print teamName$

    Color 15, 0

    Locate 25, 1: Print Tab(80);
    Locate 2, 1

    For I = 1 To totalGames%

        Color 7

        If I / 2 = Int(I / 2) Then Color 15

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
    Locate 25, 1
    Color 15, 1: Print "INDIVIDUAL STATS ";
    Color 14: Print "[Pg DN]  ";
    Color 15: Print "SELECT ANOTHER TEAM ";
    Color 14: Print "[ESC]  ";
    Color 15: Print "PRINT STATS ";
    Color 14: Print "[P]           ";
    Locate 3, 1

    Do
        I$ = GetKeyPress$

        Select Case I$

            'Page Down
            Case Chr$(0) + "Q"
                Color , 0
                Cls
                Call ViewPlayerStats(statFile$, teamName$, categoryPick, totalGames%, menuExit)

                'Page Up
            Case Chr$(0) + "I"
                Color , 0
                Cls
                Call ViewStatSummary(statFile$, teamName$, totalGames%, menuExit)

                'Delete
            Case "d", "D":
                Kill diskPaths$(1) + statFile$
                Kill diskPaths$(1) + statFile$ + "S"

                'Print
            Case "p", "P":
                Call PrintStatsSchedule(teamName$, totalGames%)
                Call PrintStatsSummary(teamName$, totalGames%)

        End Select

    Loop Until I$ = Chr$(27) Or I$ = Chr$(0) + "Q" Or menuExit = 1

    If I$ = Chr$(27) Then menuExit = 1

End Sub


'----------------------------------------
'     PrintStatsSchedule Subroutine
'----------------------------------------
'This subroutine formats and outputs the
'team's schedule results to the default printer
Sub PrintStatsSchedule (teamName$, totalGames%)

    Shared homeLosses, homeWins, neutralLosses, neutralWins
    Shared roadLosses, roadWins, totalLosses, totalWins

    Shared teamScore(), oppScore()

    Shared statsW0!(), statsW1!(), statsZ!(), statsZ1!()
    Shared values!()

    Shared statPlyrNames$(), locIndicator$(), oppName$()

    LPrint "SCHEDULE RESULTS FOR "; teamName$
    LPrint

    For I = 1 To 28

        LPrint Using "###"; teamScore(I);
        LPrint Tab(5); Left$(oppName$(I), 14);
        LPrint Tab(20); Using "###"; oppScore(I);
        LPrint Tab(24); locIndicator$(I); Tab(27);

        If I + 28 <= totalGames% Then
            LPrint Using "###"; teamScore(I + 28);
            LPrint Tab(31); Left$(oppName$(I + 28), 14);
            LPrint Tab(46); Using "###"; oppScore(I + 28);
            LPrint Tab(50); locIndicator$(I + 28); Tab(53);

            If I + 56 <= totalGames% Then
                LPrint Using "###"; teamScore(I + 56);
                LPrint Tab(57); Left$(oppName$(I + 56), 14);
                LPrint Tab(72); Using "###"; oppScore(I + 56);
                LPrint Tab(76); locIndicator$(I + 56)
            Else
                LPrint
            End If
        Else
            LPrint
        End If

    Next I

    LPrint Chr$(12)

    LPrint
    LPrint Tab(27); "OVERALL RECORD ("; totalWins; "-"; totalLosses; ")"
    LPrint Tab(27); "HOME RECORD    ("; homeWins; "-"; homeLosses; ")"
    LPrint Tab(27); "ROAD RECORD    ("; roadWins; "-"; roadLosses; ")"
    LPrint Tab(27); "NEUTRAL RECORD ("; neutralWins; "-"; neutralLosses; ")"

    LPrint Chr$(12)

End Sub


'----------------------------------------
'      PrintStatsSummary Subroutine
'----------------------------------------
'This subroutine formats and outputs the
'team's stats to the default printer
Sub PrintStatsSummary (teamName$, totalGames%)

    Shared teamScore(), oppScore()

    Shared statsW0!(), statsW1!(), statsZ!(), statsZ1!()
    Shared values!()

    Shared statPlyrNames$(), locIndicator$(), oppName$()

    LPrint "STATISTICS SUMMARY FOR "; teamName$
    LPrint "NAME"; Tab(15);
    LPrint " G  MIN AVG   FGM  FGA  PCT  3FGM 3FGA  PCT   FTM  FTA  PCT"

    For I = 0 To 13
        LPrint Left$(statPlyrNames$(I), 13); Tab(15);

        'Games
        LPrint Using "## "; values!(I, 0);

        'Min, Avg
        If values!(I, 10) > 0 Then 
            LPrint Tab(18); Using "####"; Int((values!(I, 10) + 30) / 60);
            LPrint Tab(24); Using "##"; Int(((values!(I, 10) / 60) / values!(I, 0)) + .5);
        End If

        'FGM, FGA, FG PCT
        LPrint Tab(28); Using "#### ####"; values!(I, 2); values!(I, 1);
        LPrint Tab(38);

        If values!(I, 1) = 0 Then
            LPrint ".---";
        Else
            If values!(I, 2) = values!(I, 1) Then
                LPrint "1.00";
            Else
                If values!(I, 2) = 0 Then
                    LPrint ".000";
                Else
                    fgPCT! = values!(I, 2) / values!(I, 1)
                    LPrint Using ".###"; fgPCT!;
                End If
            End If
        End If

        '3FGM, 3FGA, 3FG PCT
        LPrint Tab(44); Using "#### ####"; statsW0!(I); statsW1!(I);: LPrint Tab(54);

        If statsW1!(I) = 0 Then
            LPrint ".---";
        Else
            If statsW1!(I) = statsW0!(I) Then
                LPrint "1.00";
            Else
                If statsW0!(I) = 0 Then
                    LPrint ".000";
                Else
                    statPCT! = statsW0!(I) / statsW1!(I)
                    LPrint Using ".###"; statPCT!;
                End If
            End If
        End If

        'FTM, FTA, FT PCT
        LPrint Tab(60); Using "#### ####"; values!(I, 4); values!(I, 3);
        LPrint Tab(70);

        If values!(I, 3) = 0 Then
            LPrint ".---"
        Else
            If values!(I, 3) = values!(I, 4) Then
                LPrint "1.00"
            Else
                If values!(I, 4) = 0 Then
                    LPrint ".000"
                Else
                    statPCT! = values!(I, 4) / values!(I, 3)
                    LPrint Using ".###"; statPCT!
                End If
            End If
        End If

    Next 'done looping through players

    LPrint
    LPrint "TOTALS"; Tab(15);

    'Games
    LPrint Using "##"; totalGames%;

    'FGM / FGA / FGPCT
    LPrint Tab(28); Using "#### ####"; statsZ!(2); statsZ!(1);
    If statsZ!(1) <> 0 Then 
        statPCT! = statsZ!(2) / statsZ!(1)
        LPrint Tab(38); Using ".###"; statPCT!;
    End If

    '3FGM / 3FGA / 3FGPCT
    LPrint Tab(44); Using "#### ####"; statsZ!(13); statsZ!(14);
    If statsZ!(14) <> 0 Then 
        statPCT! = statsZ!(13) / statsZ!(14)
        LPrint Tab(54); Using ".###"; statPCT!;
    End If

    'FTM / FTA / FTPCT
    LPrint Tab(60); Using "#### ####"; statsZ!(4); statsZ!(3);
    If statsZ!(3) <> 0 Then 
        statPCT! = statsZ!(4) / statsZ!(3)
        LPrint Tab(70); Using ".###"; statPCT!
    End If

    'Opponents

    'Games
    LPrint Tab(15); Using "##"; totalGames%;

    'FGM / FGA / FGPCT
    LPrint Tab(28); Using "#### ####"; statsZ1!(2); statsZ1!(1);
    If statsZ1!(1) <> 0 Then 
        statPCT! = statsZ1!(2) / statsZ1!(1)
        LPrint Tab(38); Using ".###"; statPCT!;
    End If

    '3FGM / 3FGA / 3FGPCT
    LPrint Tab(44); Using "#### ####"; statsZ1!(13); statsZ1!(14);
    If statsZ1!(14) <> 0 Then 
        statPCT! = statsZ1!(13) / statsZ1!(14)
        LPrint Tab(54); Using ".###"; statPCT!;
    End If

    'FTM / FTA / FTPCT
    LPrint Tab(60); Using "#### ####"; statsZ1!(4); statsZ1!(3);
    If statsZ1!(3) <> 0 Then 
        statPCT! = statsZ1!(4) / statsZ1!(3)
        LPrint Tab(70); Using ".###"; statPCT!
    End If

    'Player Stats
    LPrint
    LPrint Tab(15);
    LPrint "               OFF  DEF  TOT": LPrint Tab(15);
    LPrint " PTS   AVG HI  REB  REB  REB   AVG   PF  AST  BLK  STL   TO"

    For I = 0 To 13

        'Player Name
        LPrint Left$(statPlyrNames$(I), 13);: LPrint Tab(15);

        'Points
        LPrint Using "####"; values!(I, 2) * 2 + values!(I, 4) + statsW0!(I);

        'Average
        If values!(I, 0) > 0 Then 
            LPrint Tab(21); Using "##.#"; (values!(I, 2) * 2 + values!(I, 4) + statsW0!(I)) / values!(I, 0);
        End If

        'Highest Points, Rebounds
        LPrint Tab(26); Using "## #### #### ####"; values!(I, 13); values!(I, 12); values!(I, 5) - values!(I, 12); values!(I, 5);

        'Average Rebounds
        If values!(I, 0) > 0 Then 
            LPrint Tab(45); Using "##.#"; values!(I, 5) / values!(I, 0);
        End If

        'Fouls / AST / BLK / STL / TO
        LPrint Tab(51); Using "### ####  ###  ###  ###"; values!(I, 6); values!(I, 8); values!(I, 9); values!(I, 7); values!(I, 11)

    Next I

    LPrint "TEAM";: LPrint Tab(39); Using "####"; statsZ!(15)

    LPrint
    LPrint "TOTALS";

    'Points
    If totalGames% > 0 Then ptsCalc! = statsZ!(10) / totalGames% Else ptsCalc! = 0
    LPrint Tab(14); Using "##### ###.#"; statsZ!(10); ptsCalc!;

    'Rebounds
    If totalGames% > 0 Then rebCalc! = statsZ!(5) / totalGames% Else rebCalc! = 0
    LPrint Tab(29); Using "#### #### ####  ##.#"; statsZ!(12); statsZ!(5) - statsZ!(12) - statsZ!(15); statsZ!(5); rebCalc!;

    'Fouls  AST  BLK  STL  TO
    LPrint Using " #### #### #### #### ####"; statsZ!(6); statsZ!(8); statsZ!(9); statsZ!(7); statsZ!(11)

    'Points
    If totalGames% > 0 Then ptsCalc! = statsZ1!(10) / totalGames% Else ptsCalc! = 0
    LPrint Tab(14); Using "##### ###.#"; statsZ1!(10); ptsCalc!;

    'Rebounds
    If totalGames% > 0 Then rebCalc! = statsZ1!(5) / totalGames% Else rebCalc! = 0
    LPrint Tab(29); Using "#### #### ####  ##.#"; statsZ1!(12); statsZ1!(5) - statsZ1!(12) - statsZ1!(15); statsZ1!(5); rebCalc!;

    'Fouls  AST  BLK  STL  TO
    LPrint Using " #### #### #### #### ####"; statsZ1!(6); statsZ1!(8); statsZ1!(9); statsZ1!(7); statsZ1!(11)

    LPrint Chr$(12)

End Sub


'----------------------------------------
'       SortStats Subroutine
'----------------------------------------
'This subroutine sorts the associate stats
'based on the chosen category
Sub SortStats (categoryPick)

    Shared statsW0!(), statsW0!()
    Shared statsW1!(), statsW1!()
    Shared values!()

    Shared statPlyrNames$()

    If categoryPick = 1 Then SG% = 0
    If categoryPick = 2 Then SG% = 10
    If categoryPick = 3 Then SG% = 14
    If categoryPick = 4 Then SG% = 2
    If categoryPick = 5 Then SG% = 1
    If categoryPick = 6 Then SG% = 15
    If categoryPick = 7 Then SG% = 18
    If categoryPick = 8 Then SG% = 19
    If categoryPick = 9 Then SG% = 16
    If categoryPick = 10 Then SG% = 4
    If categoryPick = 11 Then SG% = 3
    If categoryPick = 12 Then SG% = 17
    If categoryPick = 14 Then SG% = 22
    If categoryPick = 15 Then SG% = 20
    If categoryPick = 17 Then SG% = 12
    If categoryPick = 16 Then SG% = 13
    If categoryPick = 18 Then SG% = 23
    If categoryPick = 19 Then SG% = 5
    If categoryPick = 20 Then SG% = 21
    If categoryPick = 21 Then SG% = 6
    If categoryPick = 22 Then SG% = 8
    If categoryPick = 23 Then SG% = 9
    If categoryPick = 24 Then SG% = 7
    If categoryPick = 25 Then SG% = 11

    For Z = 0 To 12

        For Z1 = Z + 1 To 13

            If values!(Z, SG%) < values!(Z1, SG%) Then

                For I = 0 To 24: Swap values!(Z, I), values!(Z1, I): Next

                Swap statsW0!(Z), statsW0!(Z1)
                Swap statsW1!(Z), statsW1!(Z1)
                Swap statPlyrNames$(Z), statPlyrNames$(Z1)

            End If

        Next Z1

    Next Z

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
