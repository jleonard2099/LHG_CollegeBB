'----------------------------------------
'           NEWLDR Subroutine
'----------------------------------------
'This subroutine provides the user a menu
'for viewing stats compiled for the new
'"Expanded" Stats/Standings screens
Sub NEWLDR ()

    _Title "Courtside College Basketball - Expanded Leaders / Standings"

    ReDim menuChoices$(1 To 5)

    Dim printOrView

    Shared div1Name$, div2Name$, conferenceName$
    Shared memberName$()

    'Needed for Expanded Standings
    Shared totHomeLosses(), totHomeWins()
    Shared totTeamLosses(), lastLoseStreak()
    Shared lastTenLosses(), lastTenWins(), lastWinStreak()
    Shared totAwayLosses(), totAwayWins()
    Shared teamLoseStreak(), teamWinStreak(), totTeamWins()

    Shared expStdLeagName$()

    Do

        Cls
        Color 15, 0

        Call TITLE

        menuChoices$(1) = "UPDATE EXPANDED CONFERENCE LEADERS        "
        menuChoices$(2) = "VIEW EXPANDED CONFERENCE LEADER FILES     "
        menuChoices$(3) = "UPDATE EXPANDED CONFERENCE STANDINGS      "
        menuChoices$(4) = "VIEW / PRINT EXPANDED CONFERENCE STANDINGS"
        menuChoices$(5) = "RETURN TO CONFERENCE LEADER MAIN MENU     "
        SELECTION% = 1
        Count% = 5
        Start% = 1
        ExitCode% = 5
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            printOrView = 0

            Select Case SELECTION%

                Case 1:
                    Do
                        confStatFile$ = ChooseFile$(diskPaths$(2) + "*.TMS", "CONFERENCE STATS", FileExit%)

                        If FileExit% <> 27 And FileExit% <> 0 Then

                            For X = 1 To TEAMS_PER_CONFERENCE
                                memberName$(X) = "XXX"
                            Next

                            Call ReadConferenceStats(confStatFile$)
                            Call UpdateExpandedLeaders(JB, PTS, TTS)
                            Call SaveLeaderStdg(confStatFile$, JB, PTS, TTS)

                        End If

                    Loop Until FileExit% = 27 Or FileExit% = 0

                Case 2:
                    Do
                        leaderFile$ = ChooseFile$(diskPaths$(2) + "*.LDS", "CONFERENCE LEADERS", FileExit%)

                        If FileExit% <> 27 And FileExit% <> 0 Then
                            Call ReadLeaderStdg(leaderFile$, JB, PTS, TTS)
                            Call ExpandedLeaders_Menu(printOrView, JB, PTS, TTS)
                        End If

                    Loop Until FileExit% = 27 Or FileExit% = 0

                Case 3:
                    Do
                        confStatFile$ = ChooseFile$(diskPaths$(2) + "*.TMS", "CONFERENCE STATS", FileExit%)

                        If FileExit% <> 27 And FileExit% <> 0 Then

                            Color 15, 0
                            Cls

                            Call ReadConferenceStats(confStatFile$)
                            Call UpdateExpandedStandings

                        End If

                    Loop Until FileExit% = 27 Or FileExit% = 0

                Case 4:
                    Do
                        standingsFile$ = ChooseFile$(diskPaths$(2) + "*.STD", "CONFERENCE STANDINGS", FileExit%)

                        If FileExit% <> 27 And FileExit% <> 0 Then

                            Open diskPaths$(2) + standingsFile$ + ".STD" For Input As #2

                            Input #2, leagueName$, div1Name$, div2Name$

                            For I = 1 To TEAMS_PER_CONFERENCE
                                Input #2, expStdLeagName$(I), totTeamWins(I), totHomeWins(I), totAwayWins(I), lastTenWins(I), totTeamLosses(I), totHomeLosses(I), totAwayLosses(I), lastTenLosses(I), teamWinStreak(I), teamLoseStreak(I)
                            Next

                            Close #2

                            userKey$ = GetOutputOptions$

                            If userKey$ <> Chr$(27) Then

                                Select Case userKey$

                                    Case "F"
                                        userFile$ = _SaveFileDialog$("Save Report File", _CWD$ + "\Report", "*.txt", "Text File")
                                        If userFile$ <> "" Then
                                            Call NewStandings_Report(0, userFile$, leagueName$)
                                        End If

                                    Case "P"
                                        Call NewStandings_Report(1, "REPORT", leagueName$)

                                    Case "S":
                                        Call NewStandings_Screen(leagueName$)

                                End Select

                            End If

                        End If

                    Loop Until FileExit% = 27 Or FileExit% = 0

            End Select

        End If

    Loop Until SELECTION% = 5 Or ExitCode% = 27

End Sub


'----------------------------------------
'    UpdateExpandedLeaders Subroutine
'----------------------------------------
'This subroutine calculates the expanded
'stats from those previously read in to
'a variety of arrays.
Sub UpdateExpandedLeaders (JB, plyrTotalStats, teamTotalStats)

    Shared expPlyrLdrName$(), expPlyrLdrTeam$()
    Shared memberName$(), plyLeaderTeams$()
    Shared statPlyrNames$(), tmLeaderTeams$()

    Shared plyrLdrStats!(), plyrLdrVal!()
    Shared plyrLeaderGames!(), statPlyrVal!()
    Shared teamLdrVal!(), totGames!(), tmLeaderGames!()

    Shared leader3FGM!(), leader3FGA!()
    Shared stats3FGM!(), stats3FGA!()
    Shared statsOffTot!(), statsDefTot!()

    Erase expPlyrLdrName$, expPlyrLdrTeam$
    Erase plyLeaderTeams$, tmLeaderTeams$
 
    Erase plyrLdrStats!, plyrLdrVal!
    Erase plyrLeaderGames!
    Erase teamLdrVal!, totGames!, tmLeaderGames!

    Erase leader3FGM!, leader3FGA!
 
    Color 15, 0
    Cls

    JB = 0

    For KK = 1 To TEAMS_PER_CONFERENCE

        YY = YY + 1

        If memberName$(KK) <> "*" And memberName$(KK) <> "XXX" And memberName$(KK) <> "" Then

            Print "CONVERTING STAT FILE OF "; memberName$(KK)

            If _FileExists(diskPaths$(1) + memberName$(KK)) Then

                Call ReadTeamStats(teamName$, memberName$(KK))

                Open diskPaths$(1) + memberName$(KK) + "S" For Input As #2
                Input #2, year
                Close #2

                plyrTotalStats = plyrTotalStats + 1

                For I = 0 To 14
                    plyrLdrVal!(plyrTotalStats, I) = statsOffTot!(I)
                    plyLeaderTeams$(plyrTotalStats, I) = teamName$
                    plyrLeaderGames!(plyrTotalStats) = year
                Next

                teamTotalStats = teamTotalStats + 1

                For I = 0 To 14:
                    teamLdrVal!(teamTotalStats, I) = statsDefTot!(I)
                    tmLeaderTeams$(teamTotalStats, I) = teamName$
                    tmLeaderGames!(teamTotalStats) = year
                Next

                For I = 0 To 14

                    If statPlyrNames$(I) <> "XXX" Then

                        JB = JB + 1
                        expPlyrLdrName$(JB) = statPlyrNames$(I)
                        TBN = TBN + 1
                        expPlyrLdrTeam$(TBN) = teamName$
                        totGames!(JB) = year
                        K1 = 0

                        For I1 = 0 To 13
                            K1 = K1 + 1
                            plyrLdrStats!(JB, K1) = statPlyrVal!(I, I1)
                        Next I1

                        leader3FGM!(JB) = stats3FGM!(I)
                        leader3FGA!(JB) = stats3FGA!(I)

                    End If

                Next I

            End If

        End If

    Next KK

End Sub


'----------------------------------------
'        SaveLeaderStdg Subroutine
'----------------------------------------
'This subroutine compiles the leader files
'(.LDS) for the expanded standings modules
Sub SaveLeaderStdg (statFile$, JB, plyrTotalStats, teamTotalStats)

    Shared expPlyrLdrName$(), plyLeaderTeams$()
    Shared expPlyrLdrTeam$(), tmLeaderTeams$()

    Shared plyrLdrStats!()
    Shared plyrLeaderGames!(), totGames!(), tmLeaderGames!()
    Shared plyrLdrVal!(), teamLdrVal!()
    Shared leader3FGM!(), leader3FGA!()

    'Our first set of stats is 600 entries
    'Our next 2 sets are loops around each
    'conference
    TMR = 600 + (2 * TEAMS_PER_CONFERENCE)

    Print "SAVING LEADER FILES . . . . PLEASE WAIT"

    Open diskPaths$(2) + statFile$ + ".LDS" For Output As #1

    Write #1, JB, plyrTotalStats, teamTotalStats

    For I = 1 To 600:

        Write #1, expPlyrLdrName$(I), expPlyrLdrTeam$(I), totGames!(I)

        Color 14
        Locate 18, 38
        TMR = TMR - 1
        Print Using "#####"; TMR

        For I1 = 1 To 14:
            Write #1, plyrLdrStats!(I, I1)
        Next I1

        Write #1, leader3FGM!(I)
        Write #1, leader3FGA!(I)

        Color 14
        Locate 18, 38
        TMR = TMR - 1
        Print Using "#####"; TMR

    Next I

    For I = 1 To TEAMS_PER_CONFERENCE

        For I1 = 0 To 14
            Write #1, plyrLdrVal!(I, I1), plyLeaderTeams$(I, I1)
        Next

        Color 14
        Locate 18, 38
        TMR = TMR - 1
        Print Using "#####"; TMR
        Write #1, plyrLeaderGames!(I)

    Next

    For I = 1 To TEAMS_PER_CONFERENCE

        For I1 = 0 To 14
            Write #1, teamLdrVal!(I, I1), tmLeaderTeams$(I, I1)
        Next

        Color 14
        Locate 18, 38
        TMR = TMR - 1
        Print Using "#####"; TMR
        Write #1, tmLeaderGames!(I)

    Next

    Close #1

End Sub


'----------------------------------------
'        ReadLeaderStdg Subroutine
'----------------------------------------
'This subroutine reads in the leader files
'(.LDS) for the expanded standings modules
Sub ReadLeaderStdg (statFile$, JB, plyrTotalStats, teamTotalStats)

    Shared expPlyrLdrName$(), expPlyrLdrTeam$()
    Shared plyLeaderTeams$(), tmLeaderTeams$()

    Shared plyrLdrStats!()
    Shared plyrLeaderGames!(), tmLeaderGames!(), totGames!()
    Shared plyrLdrVal!(), teamLdrVal!()
    Shared leader3FGM!(), leader3FGA!()

    'Our first set of stats is 600 entries
    'Our next 2 sets are loops around each
    'conference
    TMR = 600 + (2 * TEAMS_PER_CONFERENCE)

    'Print "READING LEADER FILES . . . . PLEASE WAIT"

    If Not _FileExists(diskPaths$(2) + statFile$ + ".LDS") Then

        MsgText$ = "NO COMPILED LEADER FILES WERE FOUND. PRESS ANY KEY TO RETURN TO MAIN MENU"
        BoxWidth = 20
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, PASS)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, PASS)

    Else

        Open diskPaths$(2) + statFile$ + ".LDS" For Input As #1

        Input #1, JB, plyrTotalStats, teamTotalStats

        For I = 1 To 600:

            Input #1, expPlyrLdrName$(I), expPlyrLdrTeam$(I), totGames!(I)

            Color 14
            Locate 18, 38
            TMR = TMR - 1
            Print Using "#####"; TMR

            For I1 = 1 To 14:
                Input #1, plyrLdrStats!(I, I1)
            Next I1

            Input #1, leader3FGM!(I)
            Input #1, leader3FGA!(I)

            Color 14
            Locate 18, 38
            TMR = TMR - 1
            Print Using "#####"; TMR

        Next I

        For I = 1 To TEAMS_PER_CONFERENCE

            For I1 = 0 To 14
                Input #1, plyrLdrVal!(I, I1), plyLeaderTeams$(I, I1)
            Next

            Color 14
            Locate 18, 38
            TMR = TMR - 1
            Print Using "#####"; TMR
            Input #1, plyrLeaderGames!(I)

        Next

        For I = 1 To TEAMS_PER_CONFERENCE

            For I1 = 0 To 14
                Input #1, teamLdrVal!(I, I1), tmLeaderTeams$(I, I1)
            Next

            Color 14
            Locate 18, 38
            TMR = TMR - 1
            Print Using "#####"; TMR
            Input #1, tmLeaderGames!(I)

        Next

        Close #1

    End If

End Sub


'----------------------------------------
'    ExpandedLeaders_Menu Subroutine
'----------------------------------------
'This subroutine presents a categorized
'menu for viewing expanded leaders. It then
'relies on various subroutines for the end
'user display of those statistics.
Sub ExpandedLeaders_Menu (printOrView, JB, plyrTotalStats, teamTotalStats)

    ReDim menuChoices$(1 To 3)

    Do
        Cls

        Call TITLE
        menuChoices$(1) = "VIEW INDIVIDUAL PLAYER LEADERS"
        menuChoices$(2) = "VIEW TEAM TOTALS              "
        menuChoices$(3) = "RETURN TO LEAGUE LEADER MENU  "
        SELECTION% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case SELECTION%

                Case 1:
                    Call ViewIndividualLeaders(printOrView, JB)

                Case 2:
                    Call ViewTeamLeaders(printOrView, plyrTotalStats, teamTotalStats)

            End Select

        End If

    Loop Until SELECTION% = 3 Or ExitCode% = 27

End Sub


'----------------------------------------
'    ViewIndividualLeaders Subroutine
'----------------------------------------
'This subroutine lets the user choose a
'individual player stat categories and
'relies on other routines to sort and
'display the results.
Sub ViewIndividualLeaders (printOrView, JB)

    Shared totGames!()
    Shared plyrLdrStats!(), expLdrVal!()
    Shared leader3FGM!(), leader3FGA!()

    Shared expPlyrLdrName$(), expIndCategory$()
    Shared expPlyrLdrTeam$(), expLdrTeam$(), expLdrPlyrName$()

    Do

        Color 15, 0
        Cls

        ZZ = 1

        Call ShadowBox(2, 4, 66, 20, 15, 12)
        Locate 10, 15

        Do

            For I = 0 To 15
                Color 14
                Locate I + 3, 8: Print Using "##"; I;
                Print " ";
                Color 0: Print expIndCategory$(I);

                Color 14
                Print Tab(40); Using "##"; I + 16;
                Print " ";
                Color 0
                Print expIndCategory$(I + 16)
            Next

            Print
            Locate , 44: Print Space$(10);
            Locate , 23: Input "SELECT NUMBER TO VIEW"; categoryId

            stpl = 0
            newCategory = 0

            Select Case categoryId

                Case 0: X0 = 1
                Case 1: X0 = 11
                Case 2: X0 = 3
                Case 3: X0 = 2
                Case 4: X0 = 3: X1 = 2
                Case 5 To 7: '
                Case 8: X0 = 5
                Case 9: X0 = 4
                Case 10: X0 = 5: X1 = 4
                Case 11: X0 = 3: X1 = 5
                Case 12: X0 = 3: X1 = 5: X2 = 1
                Case 13: X0 = 14
                Case 14: X0 = 13
                Case 15: X0 = 6: X1 = 13
                Case 16: X0 = 6
                Case 17: X0 = 6: X1 = 1
                Case 18: X0 = 7
                Case 19: X0 = 9
                Case 20: X0 = 10
                Case 21: X0 = 8
                Case 22: X0 = 12
                Case 23: X0 = 3: X1 = 5: X2 = 11
                Case 24: X0 = 6: X1 = 11
                Case 25: X0 = 7: X1 = 11
                Case 26: X0 = 9: X1 = 11
                Case 27: X0 = 10: X1 = 11
                Case 28: X0 = 8: X1 = 11
                Case 29: X0 = 12: X1 = 11
                Case 30: X0 = 11: X1 = 1
                Case 31: Exit Sub

            End Select

        Loop Until categoryId >= 0 And categoryId <= 31

        If categoryId <> 31 And newCategory = 0 Then

            Call SortOptions(printOrView, sortType, newCategory)

            Cls

            PPP = 0

            Select Case categoryId

                Case 0, 2 To 3, 8 To 9, 13 To 14, 16, 18 To 22:

                    For I = 1 To JB

                        If plyrLdrStats!(I, X0) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                            expLdrPlyrName$(PPP) = expPlyrLdrName$(I)
                            expLdrTeam$(PPP) = expPlyrLdrTeam$(I)
                            expLdrVal!(PPP) = plyrLdrStats!(I, X0)
                        End If

                    Next I

                    Call SortPlayer(PPP, stpl, sortType)
                    TVN = 1

                Case 1:

                    For I = 1 To JB

                        If plyrLdrStats!(I, X0) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                            expLdrPlyrName$(PPP) = expPlyrLdrName$(I)
                            expLdrTeam$(PPP) = expPlyrLdrTeam$(I)
                            expLdrVal!(PPP) = (plyrLdrStats!(I, X0) + 30) / 60
                        End If

                    Next I

                    Call SortPlayer(PPP, stpl, sortType)
                    TVN = 1

                Case 4:

                    For I = 1 To JB

                        If plyrLdrStats!(I, X0) >= totGames!(I) * 3.65 Then
                            PPP = PPP + 1
                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                            expLdrPlyrName$(PPP) = expPlyrLdrName$(I)
                            expLdrTeam$(PPP) = expPlyrLdrTeam$(I)
                            expLdrVal!(PPP) = plyrLdrStats!(I, X0) / plyrLdrStats!(I, X1)
                        End If

                    Next I

                    Call SortPlayer(PPP, stpl, sortType)
                    TVN = 2

                Case 5:

                    For I = 1 To JB

                        If leader3FGM!(I) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                            expLdrPlyrName$(PPP) = expPlyrLdrName$(I)
                            expLdrTeam$(PPP) = expPlyrLdrTeam$(I)
                            expLdrVal!(PPP) = leader3FGM!(I)
                        End If

                    Next I

                    Call SortPlayer(PPP, stpl, sortType)
                    TVN = 1

                Case 6:

                    For I = 1 To JB

                        If leader3FGA!(I) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                            expLdrPlyrName$(PPP) = expPlyrLdrName$(I)
                            expLdrTeam$(PPP) = expPlyrLdrTeam$(I)
                            expLdrVal!(PPP) = leader3FGA!(I)
                        End If

                    Next I

                    Call SortPlayer(PPP, stpl, sortType)
                    TVN = 1

                Case 7:

                    For I = 1 To JB

                        If leader3FGM!(I) >= totGames!(I) * .3 Then
                            PPP = PPP + 1
                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                            expLdrPlyrName$(PPP) = expPlyrLdrName$(I)
                            expLdrTeam$(PPP) = expPlyrLdrTeam$(I)
                            expLdrVal!(PPP) = leader3FGM!(I) / leader3FGA!(I)
                        End If

                    Next I

                    Call SortPlayer(PPP, stpl, sortType)
                    TVN = 2

                Case 10:

                    For I = 1 To JB

                        If plyrLdrStats!(I, X0) >= totGames!(I) * 1.52 Then
                            PPP = PPP + 1
                            Locate 1, 5
                            Print "CHANGING PLAYER "; PPP
                            expLdrPlyrName$(PPP) = expPlyrLdrName$(I)
                            expLdrTeam$(PPP) = expPlyrLdrTeam$(I)
                            expLdrVal!(PPP) = plyrLdrStats!(I, X0) / plyrLdrStats!(I, X1)
                        End If

                    Next I

                    Call SortPlayer(PPP, stpl, sortType)
                    TVN = 2

                Case 11:

                    For I = 1 To JB

                        If (plyrLdrStats!(I, X0) * 2) + plyrLdrStats!(I, X1) + leader3FGM!(I) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5
                            Print "CHANGING PLAYER "; PPP
                            expLdrPlyrName$(PPP) = expPlyrLdrName$(I)
                            expLdrTeam$(PPP) = expPlyrLdrTeam$(I)
                            expLdrVal!(PPP) = (plyrLdrStats!(I, X0) * 2) + plyrLdrStats!(I, X1) + leader3FGM!(I)
                        End If

                    Next I

                    Call SortPlayer(PPP, stpl, sortType)
                    TVN = 1

                Case 12:

                    For I = 1 To JB

                        If (plyrLdrStats!(I, X0) * 2) + plyrLdrStats!(I, X1) + leader3FGM!(I) <> 0 And plyrLdrStats!(I, X2) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5
                            Print "CHANGING PLAYER "; PPP
                            expLdrPlyrName$(PPP) = expPlyrLdrName$(I)
                            expLdrTeam$(PPP) = expPlyrLdrTeam$(I)
                            expLdrVal!(PPP) = ((plyrLdrStats!(I, X0) * 2) + plyrLdrStats!(I, X1) + leader3FGM!(I)) / plyrLdrStats!(I, X2)
                        End If

                    Next I

                    Call SortPlayer(PPP, stpl, sortType)
                    TVN = 3

                Case 15:

                    For I = 1 To JB

                        If plyrLdrStats!(I, X0) - plyrLdrStats!(I, X1) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5
                            Print "CHANGING PLAYER "; PPP
                            expLdrPlyrName$(PPP) = expPlyrLdrName$(I)
                            expLdrTeam$(PPP) = expPlyrLdrTeam$(I)
                            expLdrVal!(PPP) = plyrLdrStats!(I, X0) - plyrLdrStats!(I, X1)
                        End If

                    Next I

                    Call SortPlayer(PPP, stpl, sortType)
                    TVN = 1

                Case 17:

                    For I = 1 To JB

                        If plyrLdrStats!(I, X0) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5
                            Print "CHANGING PLAYER "; PPP
                            expLdrPlyrName$(PPP) = expPlyrLdrName$(I)
                            expLdrTeam$(PPP) = expPlyrLdrTeam$(I)
                            expLdrVal!(PPP) = plyrLdrStats!(I, X0) / plyrLdrStats!(I, X1)
                        End If

                    Next I

                    Call SortPlayer(PPP, stpl, sortType)
                    TVN = 3

                Case 23:

                    For I = 1 To JB

                        If (plyrLdrStats!(I, X0) * 2) + plyrLdrStats!(I, X1) + leader3FGM!(I) <> 0 And plyrLdrStats!(I, X2) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5
                            Print "CHANGING PLAYER "; PPP
                            expLdrPlyrName$(PPP) = expPlyrLdrName$(I)
                            expLdrTeam$(PPP) = expPlyrLdrTeam$(I)
                            expLdrVal!(PPP) = (((plyrLdrStats!(I, X0) * 2) + plyrLdrStats!(I, X1) + leader3FGM!(I))) * 48 / ((plyrLdrStats!(I, X2) + 30) / 60)
                        End If

                    Next I

                    Call SortPlayer(PPP, stpl, sortType)
                    TVN = 3

                Case 24 To 29:

                    For I = 1 To JB

                        If plyrLdrStats!(I, X0) <> 0 And plyrLdrStats!(I, X1) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5
                            Print "CHANGING PLAYER "; PPP
                            expLdrPlyrName$(PPP) = expPlyrLdrName$(I)
                            expLdrTeam$(PPP) = expPlyrLdrTeam$(I)
                            expLdrVal!(PPP) = (plyrLdrStats!(I, X0)) * 48 / ((plyrLdrStats!(I, X1) + 30) / 60)
                        End If

                    Next I

                    Call SortPlayer(PPP, stpl, sortType)
                    TVN = 3

                Case 30:

                    For I = 1 To JB

                        If plyrLdrStats!(I, X0) <> 0 And plyrLdrStats!(I, X1) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5
                            Print "CHANGING PLAYER "; PPP
                            expLdrPlyrName$(PPP) = expPlyrLdrName$(I)
                            expLdrTeam$(PPP) = expPlyrLdrTeam$(I)
                            expLdrVal!(PPP) = ((plyrLdrStats!(I, X0) + 30) / 60) / plyrLdrStats!(I, X1)
                        End If

                    Next I

                    Call SortPlayer(PPP, stpl, sortType)
                    TVN = 3

            End Select

            QW = PPP

            Select Case printOrView

                Case 1
                    Call DisplayLeaders(TVN, QW, categoryId)

                Case 2
                    Call GetListLength(printOrView, LG, QW)

                    userKey$ = GetPrintOptions$

                    If userKey$ <> Chr$(27) Then

                        Select Case userKey$

                            Case "F"
                                userFile$ = _SaveFileDialog$("Save Report File", _CWD$ + "\Report", "*.txt", "Text File")
                                If userFile$ <> "" Then
                                    Call Leaders_Report(0, userFile$, TVN, LG, categoryId)
                                End If

                            Case "P"
                                Call Leaders_Report(1, "REPORT", TVN, LG, categoryId)

                        End Select

                    End If

            End Select

        End If

    Loop Until categoryId = 31 'Done selecting number

End Sub


'----------------------------------------
'       ViewTeamLeaders Subroutine
'----------------------------------------
'This subroutine lets the user choose team
'player stat categories and relies on other
'routines to sort and display the results.
Sub ViewTeamLeaders (printOrView, plyrTotalStats, teamTotalStats)

    Shared plyrLeaderGames!(), tmLeaderGames!()
    Shared plyrLdrVal!(), expLdrVal!(), teamLdrVal!()

    Shared expTeamCategory$(), plyLeaderTeams$()
    Shared expLdrTeam$(), tmLeaderTeams$()

    Do

        Color , 0
        Cls

        ZZ = 3

        Call ShadowBox(1, 4, 66, 21, 7, 6)
        Locate 10, 15

        For I = 0 To 19:
            Color 11
            Locate I + 2, 8
            Print Using "##"; I;
            Print " ";
            Color 0: Print expTeamCategory$(I)
        Next

        For I = 20 To 38:
            Color 11
            Locate I - 18, 38
            Print Using "##"; I;
            Print " ";
            Color 0: Print expTeamCategory$(I)
        Next

        Do
            Color 14
            Locate 22, 44: Print Space$(10)
            Locate 22, 23: Input "SELECT NUMBER TO VIEW"; categoryId

            PPP = 0
            stpl = 0

            Select Case categoryId

                Case 0: X0 = 1
                Case 1: X0 = 2
                Case 2: X0 = 2: X1 = 1
                Case 3: X0 = 14
                Case 4: X0 = 13
                Case 5: X0 = 13: X1 = 14
                Case 6: X0 = 3
                Case 7: X0 = 4
                Case 8: X0 = 4: X1 = 3
                Case 9: X0 = 12
                Case 10: X0 = 5: X1 = 12
                Case 11: X0 = 5
                Case 12: X0 = 5
                Case 13: X0 = 6
                Case 14: X0 = 8
                Case 15: X0 = 9
                Case 16: X0 = 7
                Case 17: X0 = 11
                Case 18: X0 = 2: X1 = 13: X2 = 4
                Case 19: X0 = 1
                Case 20: X0 = 2
                Case 21: X0 = 2: X1 = 1
                Case 22: X0 = 14
                Case 23: X0 = 13
                Case 24: X0 = 13: X1 = 14
                Case 25: X0 = 3
                Case 26: X0 = 4
                Case 27: X0 = 4: X1 = 3
                Case 28: X0 = 12
                Case 29: X0 = 5: X1 = 12
                Case 30: X0 = 5
                Case 31: X0 = 5
                Case 32: X0 = 6
                Case 33: X0 = 8
                Case 34: X0 = 9
                Case 35: X0 = 7
                Case 36: X0 = 11
                Case 37: X0 = 2: X1 = 13: X2 = 4

            End Select

            If categoryId < 38 And categoryId >= 0 Then
                Call SortOptions(printOrView, sortType, newCategory)
                Cls
            End If

            Select Case categoryId

                Case 0 To 1, 3 To 4, 6 To 7, 9, 11, 13 To 17:

                    For I = 1 To plyrTotalStats
                        expLdrTeam$(I) = plyLeaderTeams$(I, X0)
                        expLdrVal!(I) = plyrLdrVal!(I, X0)
                    Next

                    Call SortTeam(plyrTotalStats, stpl, sortType)
                    TVN = 1
                    maxRecords = plyrTotalStats

                Case 2, 5, 8:

                    For I = 1 To plyrTotalStats
                        If plyrLdrVal!(I, X1) = 0 Then
                            expLdrTeam$(I) = plyLeaderTeams$(I, X0)
                            expLdrVal!(I) = 0
                        Else
                            expLdrTeam$(I) = plyLeaderTeams$(I, X0)
                            expLdrVal!(I) = plyrLdrVal!(I, X0) / plyrLdrVal!(I, X1)
                        End If
                    Next

                    Call SortTeam(plyrTotalStats, stpl, sortType)
                    TVN = 2
                    maxRecords = plyrTotalStats

                Case 10:
                    For I = 1 To plyrTotalStats
                        expLdrTeam$(I) = plyLeaderTeams$(I, X0)
                        expLdrVal!(I) = plyrLdrVal!(I, X0) - plyrLdrVal!(I, X1)
                    Next

                    Call SortTeam(plyrTotalStats, stpl, sortType)
                    TVN = 1
                    maxRecords = plyrTotalStats

                Case 12:
                    For I = 1 To plyrTotalStats
                        expLdrTeam$(I) = plyLeaderTeams$(I, X0)
                        expLdrVal!(I) = plyrLdrVal!(I, X0) / plyrLeaderGames!(I)
                    Next

                    Call SortTeam(plyrTotalStats, stpl, sortType)
                    TVN = 3
                    maxRecords = plyrTotalStats

                Case 18:
                    For I = 1 To plyrTotalStats
                        expLdrTeam$(I) = plyLeaderTeams$(I, X0)
                        expLdrVal!(I) = ((plyrLdrVal!(I, X0) * 2) + plyrLdrVal!(I, X1) + plyrLdrVal!(I, X2)) / plyrLeaderGames!(I)
                    Next

                    Call SortTeam(plyrTotalStats, stpl, sortType)
                    TVN = 3
                    maxRecords = plyrTotalStats

                Case 19 To 20, 22 To 23, 25 To 26, 28, 30, 32 To 36:

                    For I = 1 To teamTotalStats
                        expLdrTeam$(I) = tmLeaderTeams$(I, X0)
                        expLdrVal!(I) = teamLdrVal!(I, X0)
                    Next

                    Call SortTeam(teamTotalStats, stpl, sortType)
                    TVN = 1
                    maxRecords = teamTotalStats

                Case 21, 24, 27:

                    For I = 1 To teamTotalStats
                        If teamLdrVal!(I, X1) = 0 Then
                            expLdrTeam$(I) = tmLeaderTeams$(I, X0)
                            expLdrVal!(I) = 0
                        Else
                            expLdrTeam$(I) = tmLeaderTeams$(I, X0)
                            expLdrVal!(I) = teamLdrVal!(I, X0) / teamLdrVal!(I, X1)
                        End If
                    Next

                    Call SortTeam(teamTotalStats, stpl, sortType)
                    TVN = 2
                    maxRecords = teamTotalStats

                Case 29:
                    For I = 1 To teamTotalStats
                        expLdrTeam$(I) = tmLeaderTeams$(I, X0)
                        expLdrVal!(I) = teamLdrVal!(I, X0) - teamLdrVal!(I, X1)
                    Next

                    Call SortTeam(teamTotalStats, stpl, sortType)
                    TVN = 1
                    maxRecords = teamTotalStats

                Case 31:
                    For I = 1 To teamTotalStats
                        expLdrTeam$(I) = tmLeaderTeams$(I, X0)
                        expLdrVal!(I) = teamLdrVal!(I, X0) / tmLeaderGames!(I)
                    Next

                    Call SortTeam(teamTotalStats, stpl, sortType)
                    TVN = 3
                    maxRecords = teamTotalStats

                Case 37:
                    For I = 1 To teamTotalStats
                        expLdrTeam$(I) = tmLeaderTeams$(I, X0)
                        expLdrVal!(I) = ((teamLdrVal!(I, X0) * 2) + teamLdrVal!(I, X1) + teamLdrVal!(I, X2)) / tmLeaderGames!(I)
                    Next

                    Call SortTeam(teamTotalStats, stpl, sortType)
                    TVN = 3
                    maxRecords = teamTotalStats

            End Select

            If categoryId < 38 And categoryId >= 0 Then
                Call DisplayTeamView(printOrView, TVN, maxRecords, categoryId)
            End If

        Loop Until categoryId >= 0 And categoryId <= 38

    Loop Until categoryId = 38

End Sub


'----------------------------------------
'         SortOptions Subroutine
'----------------------------------------
'This subroutine prompts the users for the
'different sort options available in the
'"expanded" stats menus.

'sortType
'   1: Sort HIGH to LOW
'   2: Sort LOW to HIGH
Sub SortOptions (printOrView, sortType, reselect)

    Do

        Call MenuBox(8, 8, 57, 6, 15, 1)
        Locate 10, 15

        Color 3

        Locate , 30: Print "SELECT OPTION:"
        Locate , 30: Color 15: Print "H";
        Color 7: Print "IGHEST TO LOWEST"
        Locate , 30: Color 15: Print "L";
        Color 7: Print "OWEST TO HIGHEST"
        Locate , 30: Color 15: Print "R";
        Color 7: Print "E-SELECT CATEGORY"

        Do
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "H" Or UCase$(I$) = "L" Or UCase$(I$) = "R"

        Select Case UCase$(I$)

            Case "H":
                Call PrintViewReselect(I2$)

                Select Case UCase$(I2$)
                    Case "P":
                        printOrView = 2
                    Case "V":
                        printOrView = 1
                    Case "R":
                        reselect = 1
                End Select

                sortType = 1

            Case "L"
                Call PrintViewReselect(I2$)

                Select Case UCase$(I2$)
                    Case "P":
                        printOrView = 2
                    Case "V":
                        printOrView = 1
                    Case "R":
                        reselect = 1
                End Select

                sortType = 2

            Case "R", Chr$(27):
                reselect = 1

        End Select

    Loop Until UCase$(I2$) <> "R" Or UCase$(I$) = "R"

End Sub


'----------------------------------------
'     PrintViewReselect Subroutine
'----------------------------------------
'This subroutine presents the end user
'with a choice to Print or View the exist-
'ing category, or select a new category
Sub PrintViewReselect (userOpt$)

    Call MenuBox(9, 15, 40, 4, 15, 0)
    Locate 10, 10

    Color 12
    Locate , 30: Print "SELECT OPTION:"
    Locate , 30: Color 14: Print "P";
    Color 7: Print "RINT"
    Locate , 30: Color 14: Print "V";
    Color 7: Print "IEW"
    Locate , 30: Color 14: Print "R";
    Color 7: Print "E-SELECT OPTION"

    Do
        userOpt$ = GetKeyPress$
    Loop Until UCase$(userOpt$) = "P" Or UCase$(userOpt$) = "V" Or UCase$(userOpt$) = "R"

End Sub


'----------------------------------------
'       SortPlayer Subroutine
'----------------------------------------
'This subroutine sorts the players involved
'in the chosen stat category
'sortType = 1: High to Low
'sortType = 2: Low to High
Sub SortPlayer (PPP, stpl, sortType)

    Shared expLdrVal!()
    Shared expLdrTeam$(), expLdrPlyrName$()

    For I = 2 To PPP

        stpl = stpl + 1
        Locate 5, 5: Print "sorting player "; stpl

        For J = PPP To I Step -1

            If sortType = 1 Then
                If expLdrVal!(J - 1) < expLdrVal!(J) Then
                    Swap expLdrPlyrName$(J - 1), expLdrPlyrName$(J)
                    Swap expLdrTeam$(J - 1), expLdrTeam$(J)
                    Swap expLdrVal!(J - 1), expLdrVal!(J)
                End If
            End If

            If sortType = 2 Then
                If expLdrVal!(J - 1) > expLdrVal!(J) Then
                    Swap expLdrPlyrName$(J - 1), expLdrPlyrName$(J)
                    Swap expLdrTeam$(J - 1), expLdrTeam$(J)
                    Swap expLdrVal!(J - 1), expLdrVal!(J)
                End If
            End If

        Next J

    Next I

End Sub


'----------------------------------------
'           SortTeam Subroutine
'----------------------------------------
'This subroutine sorts the teams involved
'in the chosen stat category
'sortType = 1: High to Low
'sortType = 2: Low to High
Sub SortTeam (sortMax, stpl, sortType)

    Shared expLdrVal!()
    Shared expLdrTeam$()

    For I = 2 To sortMax

        stpl = stpl + 1
        Locate 5, 5: Print "sorting team "; stpl

        For J = sortMax To I Step -1

            If sortType = 1 Then
                If expLdrVal!(J - 1) < expLdrVal!(J) Then
                    Swap expLdrTeam$(J - 1), expLdrTeam$(J)
                    Swap expLdrVal!(J - 1), expLdrVal!(J)
                End If
            End If

            If sortType = 2 Then
                If expLdrVal!(J - 1) > expLdrVal!(J) Then
                    Swap expLdrTeam$(J - 1), expLdrTeam$(J)
                    Swap expLdrVal!(J - 1), expLdrVal!(J)
                End If
            End If

        Next J

    Next I

End Sub


'----------------------------------------
'       DisplayLeaders Subroutine
'----------------------------------------
'This subroutine displays individual player
'stat categories to the user display
Sub DisplayLeaders (TVN, maxRecords, categoryId)

    Shared expLdrVal!()
    Shared expIndCategory$(), expLdrTeam$(), expLdrPlyrName$()

    userExit% = 0

    Do

        Color 14, 0
        Cls

        For W = 1 To maxRecords

            ' Every 25 records ? new page (but skip prompt on the first page)
            If (W - 1) Mod 25 = 0 Then

                If W > 1 Then

                    Color 14
                    Locate LAST_ROW - 1, 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)";
                    Color 15

                    I$ = GetKeyPress$

                    If I$ = Chr$(27) Then
                        userExit% = 1
                        Exit For
                    End If

                    Color 14, 0
                    Cls

                End If

                ' Print page header
                Locate 2, 1
                Print "     PLAYER                  TEAM                     "; expIndCategory$(categoryId)

            End If

            If userExit% <> 1 Then

                Color 12
                Print Using "### "; W;
                Color 3: Print expLdrPlyrName$(W);
                Color 15
                Print Tab(25); expLdrTeam$(W);

                Select Case TVN

                    Case 1
                        Print Tab(58); Using "#####"; expLdrVal!(W)

                    Case 2
                        Print Tab(58); Using ".###"; expLdrVal!(W)

                    Case 3
                        Print Tab(58); Using "###.#"; expLdrVal!(W)

                End Select

            Else
                Exit Do
            End If

        Next W

        If userExit% <> 1 Then

            Color 14
            Locate LAST_ROW - 1, 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)";
            Color 15

            I$ = GetKeyPress$

            If I$ <> Chr$(27) Then

                Color 14
                Locate LAST_ROW - 1, 15
                Print Space$(50);
                Locate LAST_ROW - 1, 32
                Print "VIEW AGAIN? (Y/N)";

                Do
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "N" Or UCase$(I$) = "Y"

            End If

        End If

    Loop Until UCase$(I$) = "N" Or I$ = Chr$(27)

End Sub


'----------------------------------------
'       DisplayTeamView Subroutine
'----------------------------------------
'This subroutine displays the chosen team
'stat category to the user display
Sub DisplayTeamView (printOrView, TVN, maxRecords, categoryID)

    Shared expLdrVal!()
    Shared expLdrTeam$(), expTeamCategory$()

    userExit% = 0

    Select Case printOrView

        Case 1:

            Do

                Color 14, 0
                Cls

                For W = 1 To maxRecords

                    ' Every 25 records ? new page (but skip prompt on the first page)
                    If (W - 1) Mod 25 = 0 Then

                        If W > 1 Then

                            Color 14
                            Locate LAST_ROW - 2: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
                            Color 15

                            I$ = GetKeyPress$

                            If I$ = Chr$(27) Then
                                userExit% = 1
                                Exit For
                            End If

                            Color 14, 0
                            Cls


                        End If

                        ' Print page header
                        Locate 2, 1
                        Print "     TEAM                     "; expTeamCategory$(categoryID)
                        
                    End If

                    If userExit% <> 1 Then

                        Color 12
                        Print Using "### "; W;
                        Color 3: Print expLdrTeam$(W);
                        Color 15

                        Select Case TVN

                            Case 1:
                                Print Tab(32); Using "#####"; expLdrVal!(W)

                            Case 2:
                                Print Tab(33); Using ".###"; expLdrVal!(W)

                            Case 3:
                                Print Tab(33); Using "###.#"; expLdrVal!(W)

                        End Select

                    Else

                        Exit Do

                    End If

                Next W

                If userExit% <> 1 Then

                    Color 14
                    Locate LAST_ROW - 1, 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
                    Color 15

                    I$ = GetKeyPress$

                    If I$ <> Chr$(27) Then

                        Color 14
                        Locate LAST_ROW - 1, 15
                        Print Space$(50);
                        Locate LAST_ROW - 1, 32
                        Print "VIEW AGAIN? (Y/N)";
                        
                        Do
                            A$ = GetKeyPress$
                        Loop Until UCase$(A$) = "Y" Or UCase$(A$) = "N"

                    Else
                        I$ = "N"
                    End If

                End If

            Loop Until UCase$(I$) = "N" Or I$ = Chr$(27) Or UCase$(A$) = "N"

        Case 2:
            userKey$ = GetPrintOptions$

            If userKey$ <> Chr$(27) Then

                Select Case userKey$

                    Case "F"
                        userFile$ = _SaveFileDialog$("Save Report File", _CWD$ + "\Report", "*.txt", "Text File")
                        If userFile$ <> "" Then
                            printDest = 0
                        End If

                    Case "P"
                        printDest = 1
                        userFile$ = "REPORT"

                End Select

                Color 15, 0
                Cls

                Open destFile$ For Output As #1

                Print #1, "     TEAM                     "; expTeamCategory$(categoryID)

                For W = 1 To maxRecords

                    Print #1, Using "### "; W;
                    Print #1, expLdrTeam$(W);

                    Select Case TVN

                        Case 1:
                            Print #1, Tab(32); Using "#####"; expLdrVal!(W)

                        Case 2:
                            Print #1, Tab(33); Using ".###"; expLdrVal!(W)

                        Case 3:
                            Print #1, Tab(33); Using "###.#"; expLdrVal!(W)

                    End Select

                Next W

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

            End If

    End Select

End Sub


'----------------------------------------
'        Leaders_Report Subroutine
'----------------------------------------
'This subroutine displays individual player
'stat categories to the default printer
Sub Leaders_Report (printDest, destFile$, TVN, maxRecords, categoryID)

    Shared expLdrVal!()
    Shared expIndCategory$(), expLdrTeam$(), expLdrPlyrName$()

    Color 15, 0
    Cls

    Open destFile$ For Output As #1

    For W = 1 To maxRecords

        If (W - 1) Mod 55 = 0 Then
            Print #1, Chr$(12)
            Print #1, "     PLAYER                  TEAM                     "; expIndCategory$(categoryID)
            Print #1, "     ------                  ----                     ";
            Print #1, String$(Len(expIndCategory$(categoryID)),45)
        End If

        Print #1, Using "### "; W;
        Print #1, expLdrPlyrName$(W);
        Print #1, Tab(25); expLdrTeam$(W);

        Select Case TVN

            Case 1:
                Print #1, Tab(58); Using "#####"; expLdrVal!(W)

            Case 2:
                Print #1, Tab(58); Using ".###"; expLdrVal!(W)

            Case 3:
                Print #1, Tab(58); Using "###.#"; expLdrVal!(W)

        End Select

    Next W

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
'        GetListLength Subroutine
'----------------------------------------
'This subroutine prompts the user for the
'maximum number of items to be displayed
'or returned by the standings view. This
'helps the user have a custom view of the
'categories without needing to page through
'every result.
Sub GetListLength (printOrView, listLength, maxItems)

    If printOrView <> 1 Then
        'If printing...

        Do
            Color , 0
            Cls

            Call MenuBox(10, 20, 30, 2, 4, 15)
            Locate 11

            Color 1
            Locate , 25: Print "MAXIMUM = "; maxItems
            Locate , 25: Input "ENTER LENGTH OF LIST"; listLength

        Loop Until listLength <= maxItems

    End If

End Sub
