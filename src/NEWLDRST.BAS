'----------------------------------------
'    UpdateExpandedStandings Subroutine
'----------------------------------------
'This subroutine takes the stats read in
'for a league, then calculates the standings
'for that league and writes a  corresponding
'.STD file.
Sub UpdateExpandedStandings ()

    Shared lastLoseStreak(), lastWinStreak()
    Shared lastTenLosses(), lastTenWins()
    Shared teamLoseStreak(), teamWinStreak()
    Shared totHomeLosses(), totHomeWins()
    Shared totAwayLosses(), totAwayWins()
    Shared totTeamLosses(), totTeamWins()

    Shared teamScore(), oppScore()

    Shared locIndicator$(), oppName$()
    Shared memberName$(), expStdLeagName$()

    Shared div1Name$, div2Name$, conferenceName$

    Erase lastLoseStreak, lastWinStreak
    Erase lastTenWins, lastTenLosses
    Erase teamLoseStreak, teamWinStreak
    Erase totHomeLosses, totHomeWins
    Erase totAwayLosses, totAwayWins
    Erase totTeamLosses, totTeamWins

    TMR = TEAMS_PER_CONFERENCE
    currRecord = 0

    For KK = 1 To TEAMS_PER_CONFERENCE

        currRecord = currRecord + 1
        Color 14
        Locate 7, 39
        TMR = TMR - 1
        Print Using "##"; TMR

        If memberName$(KK) <> "*" Then

            expStdLeagName$(currRecord) = memberName$(KK)

            Color 15, 0
            Cls

            Locate 5: Print "COMPUTING RECORDS . . . . . . PLEASE WAIT"

            If _FileExists(diskPaths$(1) + memberName$(KK) + "S") Then

                Call ReadScheduleStats(memberName$(KK), nbrGames)

                lastWinStreak(currRecord) = 0
                lastLoseStreak(currRecord) = 0
                teamWinStreak(currRecord) = 0
                teamLoseStreak(currRecord) = 0

                For I = 1 To nbrGames

                    If teamScore(I) > oppScore(I) Then

                        totTeamWins(currRecord) = totTeamWins(currRecord) + 1
                        lastWinStreak(currRecord) = lastWinStreak(currRecord) + 1
                        lastLoseStreak(currRecord) = 0

                        If locIndicator$(I) = "V" Then
                            totAwayWins(currRecord) = totAwayWins(currRecord) + 1
                        End If
                        If locIndicator$(I) = "V1" Then
                            totAwayWins(currRecord) = totAwayWins(currRecord) + 1
                        End If
                        If locIndicator$(I) = "V2" Then
                            totAwayWins(currRecord) = totAwayWins(currRecord) + 1
                        End If
                        If locIndicator$(I) = "H" Then
                            totHomeWins(currRecord) = totHomeWins(currRecord) + 1
                        End If
                        If locIndicator$(I) = "H1" Then
                            totHomeWins(currRecord) = totHomeWins(currRecord) + 1
                        End If
                        If locIndicator$(I) = "H2" Then
                            totHomeWins(currRecord) = totHomeWins(currRecord) + 1
                        End If

                    End If

                    If teamScore(I) < oppScore(I) Then

                        totTeamLosses(currRecord) = totTeamLosses(currRecord) + 1
                        lastLoseStreak(currRecord) = lastLoseStreak(currRecord) + 1
                        lastWinStreak(currRecord) = 0

                        If locIndicator$(I) = "V" Then
                            totAwayLosses(currRecord) = totAwayLosses(currRecord) + 1
                        End If
                        If locIndicator$(I) = "V1" Then
                            totAwayLosses(currRecord) = totAwayLosses(currRecord) + 1
                        End If
                        If locIndicator$(I) = "V2" Then
                            totAwayLosses(currRecord) = totAwayLosses(currRecord) + 1
                        End If
                        If locIndicator$(I) = "H" Then
                            totHomeLosses(currRecord) = totHomeLosses(currRecord) + 1
                        End If
                        If locIndicator$(I) = "H1" Then
                            totHomeLosses(currRecord) = totHomeLosses(currRecord) + 1
                        End If
                        If locIndicator$(I) = "H2" Then
                            totHomeLosses(currRecord) = totHomeLosses(currRecord) + 1
                        End If

                    End If

                    If lastWinStreak(currRecord) > teamWinStreak(currRecord) Then
                        teamWinStreak(currRecord) = lastWinStreak(currRecord)
                    End If
                    If lastLoseStreak(currRecord) > teamLoseStreak(currRecord) Then
                        teamLoseStreak(currRecord) = lastLoseStreak(currRecord)
                    End If

                Next I

                If nbrGames <= 10 Then
                    lastTenLosses(currRecord) = totTeamLosses(currRecord)
                    lastTenWins(currRecord) = totTeamWins(currRecord)
                Else
                    M = nbrGames - 9

                    For J = M To nbrGames
                        If teamScore(J) > oppScore(J) Then
                            lastTenWins(currRecord) = lastTenWins(currRecord) + 1
                        End If
                        If teamScore(J) < oppScore(J) Then
                            lastTenLosses(currRecord) = lastTenLosses(currRecord) + 1
                        End If
                    Next J
                End If

            End If

        End If 'Done checking for *

    Next KK

    Open diskPaths$(2) + conferenceName$ + ".STD" For Output As #3

    Write #3, conferenceName$, div1Name$, div2Name$

    For I = 1 To TEAMS_PER_CONFERENCE
        Write #3, expStdLeagName$(I), totTeamWins(I), totHomeWins(I), totAwayWins(I), lastTenWins(I), totTeamLosses(I), totHomeLosses(I), totAwayLosses(I), lastTenLosses(I), teamWinStreak(I), teamLoseStreak(I)
    Next

    Close #3

End Sub

'----------------------------------------
'     NewStandings_Screen Subroutine
'----------------------------------------
'This subroutine sorts the expanded league
'standings and outputs them to the user
'display in a formatted view
Sub NewStandings_Screen (league$)

    Shared lastTenLosses(), lastTenWins(), lastWinStreak()
    Shared totHomeLosses(), totHomeWins()
    Shared totTeamLosses(), lastLoseStreak()
    Shared totAwayLosses(), totAwayWins()
    Shared teamLoseStreak(), teamWinStreak(), totTeamWins()

    Shared expStdLeagName$()

    Shared div1Name$, div2Name$

    MJ = 0

    For NT = 1 To DIVS_PER_CONFERENCE

        AA = ((NT - 1) * 10) + 1
        ZZ = NT * 10

        Call SortStandings(AA, ZZ)

        Cls

        Select Case NT

            Case 1
                targetDiv$ = div1Name$

            Case 2
                targetDiv$ = div2Name$

        End Select

        'If NT > 1 And targetDiv$ <> ""

        Color 11
        Locate 2, 35: Print league$

        Color 14
        Locate 4: Print Tab(20); "                     MAJ          HOME    ROAD    LAST 10"

        Print RTrim$(targetDiv$)
        Print Tab(20); "G   W   L   PCT   GB  #  WS  LS   W- L    W- L    W- L "

        For I = AA To ZZ

            If expStdLeagName$(I) <> "" And expStdLeagName$(I) <> "*" Then

                Color 3
                Print expStdLeagName$(I);
                Color 15

                'G / W / L
                Print Tab(18); Using "### ### ###"; totTeamWins(I) + totTeamLosses(I); totTeamWins(I); totTeamLosses(I); Tab(30);

                ' PCT
                If totTeamLosses(I) = 0 And totTeamLosses(I) + totTeamWins(I) <> 0 Then
                    Print "1.000";
                Else
                    If totTeamWins(I) = 0 Then
                        Print " .000";
                    Else
                        If totTeamWins(I) + totTeamLosses(I) <> 0 Then
                            winPCT! = totTeamWins(I) / (totTeamWins(I) + totTeamLosses(I))
                            Print Using " .###"; winPCT!;
                        Else
                            Print " .000";
                        End If
                    End If
                End If

                ' GB
                calcGB! = ((totTeamWins(AA) - totTeamWins(I)) + (totTeamLosses(I) - totTeamLosses(AA))) / 2
                If calcGB! = 0 Then
                    Print Tab(36); " ---";
                Else
                    Print Tab(36); Using "##.#"; calcGB!;
                End If

                Print Tab(45); Using "##"; teamWinStreak(I); Tab(49); teamLoseStreak(I);

                ' Records: Home / Road / Last 10
                Print Tab(53); Using "##"; totHomeWins(I); Tab(56); totHomeLosses(I); Tab(61); totAwayWins(I); Tab(64); totAwayLosses(I); Tab(69); lastTenWins(I); Tab(72); lastTenLosses(I)
            End If

        Next

        'MJ #
        MJ = 83 - totTeamWins(AA) - totTeamLosses(AA + 1)
        If MJ < 0 Then MJ = 0

        Locate 6, 42
        Print Using "##"; MJ;

        Color 14
        Locate LAST_ROW - 1: Print "HIT ANY KEY TO CONTINUE";
        I$ = GetKeyPress$
            
        'End If

    Next NT

End Sub


'----------------------------------------
'       NewStandings_Report Subroutine
'----------------------------------------
'This subroutine sorts the expanded league
'standings and outputs them to the default
'printer in a formatted view
Sub NewStandings_Report (printDest, destFile$, league$)

    Shared lastTenLosses(), lastTenWins(), lastWinStreak()
    Shared totHomeLosses(), totHomeWins()
    Shared totTeamLosses(), lastLoseStreak()
    Shared totAwayLosses(), totAwayWins()
    Shared teamLoseStreak(), teamWinStreak(), totTeamWins()

    Shared expStdLeagName$()

    Shared div1Name$, div2Name$

    MJ = 0

    Color 15, 0
    Cls

    Open destFile$ For Output As #1

    For NT = 1 To DIVS_PER_CONFERENCE

        AA = ((NT - 1) * 10) + 1
        ZZ = NT * 10

        Call SortStandings(AA, ZZ)

        Print #1, league$
        Print #1, Tab(20); "                     MAJ          HOME    ROAD   LAST 10"

        Select Case NT

            Case 1
                targetDiv$ = div1Name$

            Case 2
                targetDiv$ = div2Name$

        End Select

        Print #1, RTrim$(targetDiv$);
        Print #1, Tab(20); "G   W   L   PCT   GB  #  WS  LS   W- L    W- L    W- L "
        Print #1, String$(75,45)

        For I = AA To ZZ

            If expStdLeagName$(I) <> "" And expStdLeagName$(I) <> "*" Then

                Print #1, expStdLeagName$(I);
                Print #1, Tab(18); Using "### ### ###"; totTeamWins(I) + totTeamLosses(I); totTeamWins(I); totTeamLosses(I); Tab(30);

                'PCT
                If totTeamLosses(I) = 0 And totTeamLosses(I) + totTeamWins(I) <> 0 Then
                    Print #1, "1.000";
                Else
                    If totTeamWins(I) = 0 Then
                        Print #1, " .000";
                    Else
                        If totTeamWins(I) + totTeamLosses(I) = 0 Then
                            Print #1, " .000";
                        Else
                            winPCT! = totTeamWins(I) / (totTeamWins(I) + totTeamLosses(I))
                            Print #1, Using " .###"; winPCT!;
                        End If
                    End If
                End If

                'Games Behind?
                If ((totTeamWins(AA) - totTeamWins(I)) + (totTeamLosses(I) - totTeamLosses(AA))) / 2 = 0 Then
                    Print #1, Tab(36); " ---";
                Else
                    Print #1, Tab(36); Using "##.#"; ((totTeamWins(AA) - totTeamWins(I)) + (totTeamLosses(I) - totTeamLosses(AA))) / 2;
                End If

                'Magic Number?
                If I = AA Then
                    MJ = 83 - totTeamWins(AA) - totTeamLosses(AA + 1)
                    If MJ < 0 Then MJ = 0
                    Print #1, Tab(42); Using "##"; MJ;
                End If

                Print #1, Tab(45); Using "##"; teamWinStreak(I); Tab(49); teamLoseStreak(I);
                Print #1, Tab(53); Using "##"; totHomeWins(I); Tab(56); totHomeLosses(I); Tab(61); totAwayWins(I); Tab(64); totAwayLosses(I); Tab(69); lastTenWins(I); Tab(72); lastTenLosses(I)

            End If

        Next

        Print #1,
        Print #1,

    Next NT

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
'         SortStandings Subroutine
'----------------------------------------
'This subroutine simply sorts the league
'standings records so that teams that are
'leading can be displayed first.
'This sorting is only done on the arrays
'holding the standing data, so no changes
'are permanent to the underlying files.
Sub SortStandings (AA, ZZ)

    Shared lastTenLosses(), lastTenWins(), lastWinStreak()
    Shared totHomeLosses(), totHomeWins()
    Shared totTeamLosses(), lastLoseStreak()
    Shared totAwayLosses(), totAwayWins()
    Shared teamLoseStreak(), teamWinStreak(), totTeamWins()

    Shared expStdLeagName$()

    For X = AA To ZZ - 1

        For Y = X + 1 To ZZ

            diffX = totTeamWins(X) - totTeamLosses(X)
            diffY = totTeamWins(Y) - totTeamLosses(Y)

            If diffX <= diffY Then

                If diffX <> diffY Then

                    If Not (totTeamWins(X) <= totTeamWins(Y) And totTeamWins(X) > totTeamLosses(X)) Or Not (totTeamWins(X) >= totTeamWins(Y) And totTeamWins(X) < totTeamLosses(X)) Then

                        Swap expStdLeagName$(X), expStdLeagName$(Y)
                        Swap totTeamWins(X), totTeamWins(Y)
                        Swap totTeamLosses(X), totTeamLosses(Y)
                        Swap totHomeLosses(X), totHomeLosses(Y)
                        Swap totAwayLosses(X), totAwayLosses(Y)
                        Swap totHomeWins(X), totHomeWins(Y)
                        Swap totAwayWins(X), totAwayWins(Y)
                        Swap lastTenLosses(X), lastTenLosses(Y)
                        Swap lastTenWins(X), lastTenWins(Y)
                        Swap teamWinStreak(X), teamWinStreak(Y)
                        Swap teamLoseStreak(X), teamLoseStreak(Y)

                    End If

                End If

            End If

        Next Y

    Next X

End Sub
