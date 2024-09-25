'----------------------------------------
'           NEWLDR Subroutine
'----------------------------------------
'This subroutine provides the user a menu
'for viewing stats compiled for the new
'"Expanded" Stats/Standings screens
Sub NEWLDR ()

    _Title "Courtside College Basketball - Expanded Leaders / Standings"

    ReDim menuChoices$(1 To 3)

    Dim printOrView

    Shared statFiles$()

    Do

        Cls
        Color 15, 0

        Call TITLE

        menuChoices$(1) = "UPDATE EXPANDED LEAGUE LEADER FILES"
        menuChoices$(2) = "VIEW EXPANDED LEAGUE STATISTICS    "
        menuChoices$(3) = "RETURN TO LEAGUE LEADER MENU       "
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

            printOrView = 0

            Select Case SELECTION%

                Case 1:
                    Do
                        confStatFile$ = ChooseFile$(diskPaths$(2) + "*.TMS", "CONFERENCE STATS", FileExit%)

                        If FileExit% <> 27 And FileExit% <> 0 Then

                            For X = 1 To MAX_CONFERENCES
                                statFiles$(X) = "XXX"
                            Next

                            Open diskPaths$(2) + confStatFile$ + ".TMS" For Input As #1

                            Input #1, conf$, div1$, div2$

                            For X = 1 To TEAMS_PER_CONFERENCE
                                Input #1, statFiles$(X)
                            Next

                            Close #1

                            Call UpdateExpandedLeaders(JB, PTS, TTS)
                            Call SAVELEADER(confStatFile$, JB, PTS, TTS)

                        End If

                    Loop Until FileExit% = 27 Or FileExit% = 0

                Case 2:
                    Do
                        leaderFile$ = ChooseFile$(diskPaths$(2) + "*.LDS", "LEAGUE LEADERS", FileExit%)

                        If FileExit% <> 27 And FileExit% <> 0 Then
                            Call READLEADER(leaderFile$, JB, PTS, TTS)
                            Call ExpandedLeaders_Menu(printOrView, JB, PTS, TTS)
                        End If

                    Loop Until FileExit% = 27 Or FileExit% = 0

            End Select

        End If

    Loop Until SELECTION% = 3 Or ExitCode% = 27

End Sub


'----------------------------------------
'      ViewExpandedStats Subroutine
'----------------------------------------
'This subroutine reads in the league stats
'from the .TMS file. It then relies on other
'routines to calculate the expanded stats,
'write them to a file and present a categ-
'orized menu to use for reviewing them
' XXXX NO LONGER USED WITH NEW STRUCTURE XXXX
Sub ViewExpandedStats ()

    Dim printOrView
    Shared statFiles$()

    printOrView = 0

    For X = 1 To MAX_CONFERENCES
        statFiles$(X) = "XXX"
    Next

    conferenceFile$ = ChooseFile$(diskPaths$(2) + "*.TMS", "CONFERENCE STATS", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Open diskPaths$(2) + conferenceFile$ + ".TMS" For Input As #1

        Input #1, conf$, div1$, div2$

        For X = 1 To TEAMS_PER_CONFERENCE
            Input #1, statFiles$(X)
        Next

        Close #1

        Call READLEADER(conferenceFile$, JB, PTS, TTS)
        Call UpdateExpandedLeaders(JB, PTS, TTS)
        Call SAVELEADER(leagStatFile$, JB, PTS, TTS)
        Call ExpandedLeaders_Menu(printOrView, JB, PTS, TTS)

    End If 'Done checking for ESC


End Sub


'----------------------------------------
'    UpdateExpandedLeaders Subroutine
'----------------------------------------
'This subroutine calculates the expanded
'stats from those previously read in to
'a variety of arrays.
Sub UpdateExpandedLeaders (JB, plyrTotalStats, teamTotalStats)

    Shared AL$(), plyLeaderTeams$()
    Shared statFiles$(), statPlyrNames$()
    Shared TMA$(), tmLeaderTeams$()

    Shared plyrStatLeaders!()

    Shared plyrLeaderYears!(), GMA!(), tmLeaderYears!()
    Shared TT!(), TT1!()

    Shared statsW0!(), statsW1!()
    Shared statsZ!(), statsZ1!()
    Shared leader3FGM!(), leader3FGA!()

    Shared playerOff!()

    Color 15, 0
    Cls

    JB = 0

    For KK = 1 To TEAMS_PER_CONFERENCE

        YY = YY + 1

        If statFiles$(KK) <> "*" And statFiles$(KK) <> "XXX" And statFiles$(KK) <> "" Then

            Print "CONVERTING STAT FILE OF "; statFiles$(KK)

            If _FileExists(diskPaths$(1) + statFiles$(KK)) Then

                Open diskPaths$(1) + statFiles$(KK) For Input As #1

                Input #1, teamName$

                For I = 0 To 13

                    Input #1, statPlyrNames$(I)

                    For J = 0 To 13:
                        Input #1, playerOff!(I, J)
                    Next

                    Input #1, statsW0!(I)
                    Input #1, statsW1!(I)

                Next

                For L = 0 To 15:
                    Input #1, statsZ!(L)
                Next

                For L = 0 To 15:
                    Input #1, statsZ1!(L)
                Next

                Close #1

                Open diskPaths$(1) + statFiles$(KK) + "S" For Input As #2
                Input #2, year
                Close #2

                plyrTotalStats = plyrTotalStats + 1

                For I = 0 To 14
                    TT!(plyrTotalStats, I) = statsZ!(I)
                    plyLeaderTeams$(plyrTotalStats, I) = teamName$
                    plyrLeaderYears!(plyrTotalStats) = year
                Next

                teamTotalStats = teamTotalStats + 1

                For I = 0 To 14:
                    TT1!(teamTotalStats, I) = statsZ1!(I)
                    tmLeaderTeams$(teamTotalStats, I) = teamName$
                    tmLeaderYears!(teamTotalStats) = year
                Next

                For I = 0 To 14

                    If statPlyrNames$(I) <> "XXX" Then

                        JB = JB + 1
                        AL$(JB) = statPlyrNames$(I)
                        TBN = TBN + 1
                        TMA$(TBN) = teamName$
                        GMA!(JB) = year
                        K1 = 0

                        For I1 = 0 To 13
                            K1 = K1 + 1
                            plyrStatLeaders!(JB, K1) = playerOff!(I, I1)
                        Next I1

                        leader3FGM!(JB) = statsW0!(I)
                        leader3FGA!(JB) = statsW1!(I)
                    
                    End If

                Next I

            End If

        End If

    Next KK

End Sub


'----------------------------------------
'        SAVELEADER Subroutine
'----------------------------------------
'This subroutine compiles the leader files
'(.LDS) for the expanded standings modules
Sub SAVELEADER (statFile$, JB, plyrTotalStats, teamTotalStats)

    Shared AL$(), plyLeaderTeams$()
    Shared TMA$(), tmLeaderTeams$()

    Shared plyrStatLeaders!()
    Shared plyrLeaderYears!(), GMA!(), tmLeaderYears!()
    Shared TT!(), TT1!()
    Shared leader3FGM!(), leader3FGA!()

    'Our first set of stats is 600 entries
    'Our next 2 sets are loops around each
    'conference
    TMR = 600 + (2*MAX_CONFERENCES)

    Print "SAVING LEADER FILES . . . . PLEASE WAIT"

    Open diskPaths$(2) + statFile$ + ".LDS" For Output As #1

    Write #1, JB, plyrTotalStats, teamTotalStats

    For I = 1 To 600:

        Write #1, AL$(I), TMA$(I), GMA!(I)

        Color 14
        Locate 18, 38
        TMR = TMR - 1
        Print Using "#####"; TMR

        For I1 = 1 To 14:
            Write #1, plyrStatLeaders!(I, I1)
        Next I1

        Write #1, leader3FGM!(I)
        Write #1, leader3FGA!(I)

        Color 14
        Locate 18, 38
        TMR = TMR - 1
        Print Using "#####"; TMR

    Next I

    For I = 1 To MAX_CONFERENCES

        For I1 = 0 To 14
            Write #1, TT!(I, I1), plyLeaderTeams$(I, I1)
        Next

        Color 14
        Locate 18, 38
        TMR = TMR - 1
        Print Using "#####"; TMR
        Write #1, plyrLeaderYears!(I)

    Next

    For I = 1 To MAX_CONFERENCES

        For I1 = 0 To 14
            Write #1, TT1!(I, I1), tmLeaderTeams$(I, I1)
        Next

        Color 14
        Locate 18, 45
        TMR = TMR - 1
        Print Using "#####"; TMR
        Write #1, tmLeaderYears!(I)

    Next

    Close #1

End Sub


'----------------------------------------
'        READLEADER Subroutine
'----------------------------------------
'This subroutine reads in the leader files
'(.LDS) for the expanded standings modules
Sub READLEADER (statFile$, JB, plyrTotalStats, teamTotalStats)

    Shared statPlyrNames$(), AL$(), plyLeaderTeams$()
    Shared TMA$(), tmLeaderTeams$()

    Shared plyrStatLeaders!()
    Shared plyrLeaderYears!(), GMA!(), tmLeaderYears!()
    Shared TT!(), TT1!()
    Shared leader3FGM!(), leader3FGA!()

    'Our first set of stats is 600 entries
    'Our next 2 sets are loops around each
    'conference
    TMR = 600 + (2*MAX_CONFERENCES)

    'Print "READING LEADER FILES . . . . PLEASE WAIT"

    If Not _FileExists(diskPaths$(2) + statFile$ + ".LDS") Then

        MsgText$ = "NO COMPILED LEADER FILES WERE FOUND. PRESS ANY KEY TO RETURN TO MAIN MENU"
        BoxWidth = 20
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, PASS)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, PASS)

    Else

        Open diskPaths$(2) + statFile$ + ".LDS" For Input As #1

        Input #1, JB, plyrTotalStats, teamTotalStats

        For I = 1 To 600:

            Input #1, AL$(I), TMA$(I), GMA!(I)

            Color 14
            Locate 18, 38
            TMR = TMR - 1
            Print Using "#####"; TMR

            For I1 = 1 To 14:
                Input #1, plyrStatLeaders!(I, I1)
            Next I1

            Input #1, leader3FGM!(I)
            Input #1, leader3FGA!(I)

            Color 14
            Locate 18, 38
            TMR = TMR - 1
            Print Using "#####"; TMR

        Next I

        For I = 1 To MAX_CONFERENCES

            For I1 = 0 To 14
                Input #1, TT!(I, I1), plyLeaderTeams$(I, I1)
            Next

            Color 14
            Locate 18, 38
            TMR = TMR - 1
            Print Using "#####"; TMR
            Input #1, plyrLeaderYears!(I)

        Next

        For I = 1 To MAX_CONFERENCES

            For I1 = 0 To 14
                Input #1, TT1!(I, I1), tmLeaderTeams$(I, I1)
            Next

            Color 14
            Locate 18, 45
            TMR = TMR - 1
            Print Using "#####"; TMR
            Input #1, tmLeaderYears!(I)

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

    Shared GMA!()
    Shared plyrStatLeaders!(), TYP!()
    Shared leader3FGM!(), leader3FGA!()

    Shared AL$(), expIndCategory$()
    Shared TMA$(), TMM$(), TPP$()

    Do

        Color 15, 0
        Cls

        ZZ = 1

        Call MenuBox (2, 4, 66, 20, 15, 12)
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

                Case 0: X = 1
                Case 1: X = 11
                Case 2: X = 3
                Case 3: X = 2
                Case 4: X = 3: X1 = 2
                Case 5 To 7: '
                Case 8: X = 5
                Case 9: X = 4
                Case 10: X = 5: X1 = 4
                Case 11: X = 3: X1 = 5
                Case 12: X = 3: X1 = 5: X2 = 1
                Case 13: X = 14
                Case 14: X = 13
                Case 15: X = 6: X1 = 13
                Case 16: X = 6
                Case 17: X = 6: X1 = 1
                Case 18: X = 7
                Case 19: X = 9
                Case 20: X = 10
                Case 21: X = 8
                Case 22: X = 12
                Case 23: X = 3: X1 = 5: X2 = 11
                Case 24: X = 6: X1 = 11
                Case 25: X = 7: X1 = 11
                Case 26: X = 9: X1 = 11
                Case 27: X = 10: X1 = 11
                Case 28: X = 8: X1 = 11
                Case 29: X = 12: X1 = 11
                Case 30: X = 11: X1 = 1
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

                        If plyrStatLeaders!(I, X) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                            TPP$(PPP) = AL$(I)
                            TMM$(PPP) = TMA$(I)
                            TYP!(PPP) = plyrStatLeaders!(I, X)
                        End If

                    Next I

                    Call SortPlayer(PPP, stpl, sortType)
                    TVN = 1

                Case 1:

                    For I = 1 To JB

                        If plyrStatLeaders!(I, X) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                            TPP$(PPP) = AL$(I)
                            TMM$(PPP) = TMA$(I)
                            TYP!(PPP) = (plyrStatLeaders!(I, X) + 30) / 60
                        End If

                    Next I

                    Call SortPlayer(PPP, stpl, sortType)
                    TVN = 1

                Case 4:

                    For I = 1 To JB

                        If plyrStatLeaders!(I, X) >= GMA!(I) * 3.65 Then
                            PPP = PPP + 1
                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                            TPP$(PPP) = AL$(I)
                            TMM$(PPP) = TMA$(I)
                            TYP!(PPP) = plyrStatLeaders!(I, X) / plyrStatLeaders!(I, X1)
                        End If

                    Next I

                    Call SortPlayer(PPP, stpl, sortType)
                    TVN = 2

                Case 5:
 
                    For I = 1 To JB

                        If leader3FGM!(I) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                            TPP$(PPP) = AL$(I)
                            TMM$(PPP) = TMA$(I)
                            TYP!(PPP) = leader3FGM!(I)
                        End If

                    Next I

                    Call SortPlayer(PPP, stpl, sortType)
                    TVN = 1

                Case 6:
                    
                    For I = 1 To JB

                        If leader3FGA!(I) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                            TPP$(PPP) = AL$(I)
                            TMM$(PPP) = TMA$(I)
                            TYP!(PPP) = leader3FGA!(I)
                        End If

                    Next I

                    Call SortPlayer(PPP, stpl, sortType)
                    TVN = 1

                Case 7:

                    For I = 1 To JB

                        If leader3FGM!(I) >= GMA!(I) * .3 Then
                            PPP = PPP + 1
                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                            TPP$(PPP) = AL$(I)
                            TMM$(PPP) = TMA$(I)
                            TYP!(PPP) = leader3FGM!(I) / leader3FGA!(I)
                        End If

                    Next I

                    Call SortPlayer(PPP, stpl, sortType)
                    TVN = 2

                Case 10:

                    For I = 1 To JB

                        If plyrStatLeaders!(I, X) >= GMA!(I) * 1.52 Then
                            PPP = PPP + 1
                            Locate 1, 5
                            Print "CHANGING PLAYER "; PPP
                            TPP$(PPP) = AL$(I)
                            TMM$(PPP) = TMA$(I)
                            TYP!(PPP) = plyrStatLeaders!(I, X) / plyrStatLeaders!(I, X1)
                        End If

                    Next I

                    Call SortPlayer(PPP, stpl, sortType)
                    TVN = 2

                Case 11:

                    For I = 1 To JB

                        If (plyrStatLeaders!(I, X) * 2) + plyrStatLeaders!(I, X1) + leader3FGM!(I) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5
                            Print "CHANGING PLAYER "; PPP
                            TPP$(PPP) = AL$(I)
                            TMM$(PPP) = TMA$(I)
                            TYP!(PPP) = (plyrStatLeaders!(I, X) * 2) + plyrStatLeaders!(I, X1) + leader3FGM!(I)
                        End If

                    Next I

                    Call SortPlayer(PPP, stpl, sortType)
                    TVN = 1

                Case 12:

                    For I = 1 To JB

                        If (plyrStatLeaders!(I, X) * 2) + plyrStatLeaders!(I, X1) + leader3FGM!(I) <> 0 And plyrStatLeaders!(I, X2) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5
                            Print "CHANGING PLAYER "; PPP
                            TPP$(PPP) = AL$(I)
                            TMM$(PPP) = TMA$(I)
                            TYP!(PPP) = ((plyrStatLeaders!(I, X) * 2) + plyrStatLeaders!(I, X1) + leader3FGM!(I)) / plyrStatLeaders!(I, X2)
                        End If

                    Next I

                    Call SortPlayer(PPP, stpl, sortType)
                    TVN = 3

                Case 15:

                    For I = 1 To JB

                        If plyrStatLeaders!(I, X) - plyrStatLeaders!(I, X1) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5
                            Print "CHANGING PLAYER "; PPP
                            TPP$(PPP) = AL$(I)
                            TMM$(PPP) = TMA$(I)
                            TYP!(PPP) = plyrStatLeaders!(I, X) - plyrStatLeaders!(I, X1)
                        End If

                    Next I

                    Call SortPlayer(PPP, stpl, sortType)
                    TVN = 1

                Case 17:

                    For I = 1 To JB

                        If plyrStatLeaders!(I, X) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5
                            Print "CHANGING PLAYER "; PPP
                            TPP$(PPP) = AL$(I)
                            TMM$(PPP) = TMA$(I)
                            TYP!(PPP) = plyrStatLeaders!(I, X) / plyrStatLeaders!(I, X1)
                        End If

                    Next I

                    Call SortPlayer(PPP, stpl, sortType)
                    TVN = 3

                Case 23:

                    For I = 1 To JB

                        If (plyrStatLeaders!(I, X) * 2) + plyrStatLeaders!(I, X1) + leader3FGM!(I) <> 0 And plyrStatLeaders!(I, X2) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5
                            Print "CHANGING PLAYER "; PPP
                            TPP$(PPP) = AL$(I)
                            TMM$(PPP) = TMA$(I)
                            TYP!(PPP) = (((plyrStatLeaders!(I, X) * 2) + plyrStatLeaders!(I, X1) + leader3FGM!(I))) * 48 / ((plyrStatLeaders!(I, X2) + 30) / 60)
                        End If

                    Next I

                    Call SortPlayer(PPP, stpl, sortType)
                    TVN = 3

                Case 24 To 29:

                    For I = 1 To JB

                        If plyrStatLeaders!(I, X) <> 0 And plyrStatLeaders!(I, X1) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5
                            Print "CHANGING PLAYER "; PPP
                            TPP$(PPP) = AL$(I)
                            TMM$(PPP) = TMA$(I)
                            TYP!(PPP) = (plyrStatLeaders!(I, X)) * 48 / ((plyrStatLeaders!(I, X1) + 30) / 60)
                        End If

                    Next I

                    Call SortPlayer(PPP, stpl, sortType)
                    TVN = 3

                Case 30:

                    For I = 1 To JB

                        If plyrStatLeaders!(I, X) <> 0 And plyrStatLeaders!(I, X1) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5
                            Print "CHANGING PLAYER "; PPP
                            TPP$(PPP) = AL$(I)
                            TMM$(PPP) = TMA$(I)
                            TYP!(PPP) = ((plyrStatLeaders!(I, X) + 30) / 60) / plyrStatLeaders!(I, X1)
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
                                userFile$ = _SaveFileDialog$("Save Report File",_CWD$ + "\Report","*.txt","Text File")
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

    Shared plyrLeaderYears!(), tmLeaderYears!()
    Shared TT!(), TYP!(), TT1!()

    Shared expTeamCategory$(), plyLeaderTeams$()
    Shared TMM$(), tmLeaderTeams$()

    Do

        Color , 0
        Cls
        Print
        Print
        Print
        Print

        ZZ = 3

        Call MenuBox (1, 4, 66, 21, 15, 6)
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

                Case 0: X = 1
                Case 1: X = 2
                Case 2: X = 2: X1 = 1
                Case 3: X = 14
                Case 4: X = 13
                Case 5: X = 13: X1 = 14
                Case 6: X = 3
                Case 7: X = 4
                Case 8: X = 4: X1 = 3
                Case 9: X = 12
                Case 10: X = 5: X1 = 12
                Case 11: X = 5
                Case 12: X = 5
                Case 13: X = 6
                Case 14: X = 8
                Case 15: X = 9
                Case 16: X = 7
                Case 17: X = 11
                Case 18: X = 2: X1 = 13: X2 = 4
                Case 19: X = 1
                Case 20: X = 2
                Case 21: X = 2: X1 = 1
                Case 22: X = 14
                Case 23: X = 13
                Case 24: X = 13: X1 = 14
                Case 25: X = 3
                Case 26: X = 4
                Case 27: X = 4: X1 = 3
                Case 28: X = 12
                Case 29: X = 5: X1 = 12
                Case 30: X = 5
                Case 31: X = 5
                Case 32: X = 6
                Case 33: X = 8
                Case 34: X = 9
                Case 35: X = 7
                Case 36: X = 11
                Case 37: X = 2: X1 = 13: X2 = 4

            End Select

            If categoryId < 38 And categoryID >= 0 Then
                Call SortOptions(printOrView, sortType, newCategory)
                Cls
            End If

            Select Case categoryId

                Case 0 To 1, 3 To 4, 6 To 7, 9, 11, 13 To 17:

                    For I = 1 To plyrTotalStats
                        TMM$(I) = plyLeaderTeams$(I, X)
                        TYP!(I) = TT!(I, X)
                    Next

                    Call SortTeam(plyrTotalStats, stpl, sortType)
                    TVN = 1
                    maxRecords = plyrTotalStats

                Case 2, 5, 8:

                    For I = 1 To plyrTotalStats
                        If TT!(I, X1) = 0 Then
                            TMM$(I) = plyLeaderTeams$(I, X)
                            TYP!(I) = 0
                        Else
                            TMM$(I) = plyLeaderTeams$(I, X)
                            TYP!(I) = TT!(I, X) / TT!(I, X1)
                        End If
                    Next

                    Call SortTeam(plyrTotalStats, stpl, sortType)
                    TVN = 2
                    maxRecords = plyrTotalStats

                Case 10:
                    For I = 1 To plyrTotalStats
                        TMM$(I) = plyLeaderTeams$(I, X)
                        TYP!(I) = TT!(I, X) - TT!(I, X1)
                    Next

                    Call SortTeam(plyrTotalStats, stpl, sortType)
                    TVN = 1
                    maxRecords = plyrTotalStats

                Case 12:
                    For I = 1 To plyrTotalStats
                        TMM$(I) = plyLeaderTeams$(I, X)
                        TYP!(I) = TT!(I, X) / plyrLeaderYears!(I)
                    Next

                    Call SortTeam(plyrTotalStats, stpl, sortType)
                    TVN = 3
                    maxRecords = plyrTotalStats

                Case 18:
                    For I = 1 To plyrTotalStats
                        TMM$(I) = plyLeaderTeams$(I, X)
                        TYP!(I) = ((TT!(I, X) * 2) + TT!(I, X1) + TT!(I, X2)) / plyrLeaderYears!(I)
                    Next

                    Call SortTeam(plyrTotalStats, stpl, sortType)
                    TVN = 3
                    maxRecords = plyrTotalStats

                Case 19 To 20, 22 To 23, 25 To 26, 28, 30, 32 To 36:
 
                    For I = 1 To teamTotalStats
                        TMM$(I) = tmLeaderTeams$(I, X)
                        TYP!(I) = TT1!(I, X)
                    Next

                    Call SortTeam(teamTotalStats, stpl, sortType)
                    TVN = 1
                    maxRecords = teamTotalStats

                Case 21, 24, 27:
                
                    For I = 1 To teamTotalStats
                        If TT1!(I, X1) = 0 Then
                            TMM$(I) = tmLeaderTeams$(I, X)
                            TYP!(I) = 0
                        Else
                            TMM$(I) = tmLeaderTeams$(I, X)
                            TYP!(I) = TT1!(I, X) / TT1!(I, X1)
                        End If
                    Next

                    Call SortTeam(teamTotalStats, stpl, sortType)
                    TVN = 2
                    maxRecords = teamTotalStats

                Case 29:
                    For I = 1 To teamTotalStats
                        TMM$(I) = tmLeaderTeams$(I, X)
                        TYP!(I) = TT1!(I, X) - TT1!(I, X1)
                    Next

                    Call SortTeam(teamTotalStats, stpl, sortType)
                    TVN = 1
                    maxRecords = teamTotalStats

                Case 31:
                    For I = 1 To teamTotalStats
                        TMM$(I) = tmLeaderTeams$(I, X)
                        TYP!(I) = TT1!(I, X) / tmLeaderYears!(I)
                    Next

                    Call SortTeam(teamTotalStats, stpl, sortType)
                    TVN = 3
                    maxRecords = teamTotalStats

                Case 37:
                    For I = 1 To teamTotalStats
                        TMM$(I) = tmLeaderTeams$(I, X)
                        TYP!(I) = ((TT1!(I, X) * 2) + TT1!(I, X1) + TT1!(I, X2)) / tmLeaderYears!(I)
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

        Call MenuBox (8, 8, 57, 6, 15, 1)
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

    Call MenuBox (9, 15, 40, 4, 15, 0)
    Locate 10, 10

    Color 12
    Locate , 30: Print "SELECT OPTION:"
    Locate , 30: Color 14: Print "P";: Color 7: Print "RINT"
    Locate , 30: Color 14: Print "V";: Color 7: Print "IEW"
    Locate , 30: Color 14: Print "R";: Color 7: Print "E-SELECT OPTION"

    Do
        userOpt$ = GetKeyPress$
    Loop Until UCase$(userOpt$) = "P" Or UCase$(userOpt$) = "V" Or UCase$(userOpt$) = "R"

End Sub


'----------------------------------------
'       SortPlayer Subroutine
'----------------------------------------
'This subroutine sorts the players involved
'in the chosen stat category
Sub SortPlayer (PPP, stpl, sortType)

    Shared TYP!()
    Shared TMM$(), TPP$()

    For I = 2 To PPP

        stpl = stpl + 1
        Locate 5, 5: Print "sorting player "; stpl

        For J = PPP To I Step -1

            If sortType = 1 Then
                If TYP!(J - 1) < TYP!(J) Then
                    Swap TPP$(J - 1), TPP$(J)
                    Swap TMM$(J - 1), TMM$(J)
                    Swap TYP!(J - 1), TYP!(J)
                End If
            End If

            If sortType = 2 Then
                If TYP!(J - 1) > TYP!(J) Then
                    Swap TPP$(J - 1), TPP$(J)
                    Swap TMM$(J - 1), TMM$(J)
                    Swap TYP!(J - 1), TYP!(J)
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
Sub SortTeam (sortMax, stpl, sortType)

    Shared TYP!()
    Shared TMM$()

    For I = 2 To sortMax

        stpl = stpl + 1

        Locate 5, 5: Print "sorting team "; stpl

        For J = sortMax To I Step -1

            If sortType = 1 Then
                If TYP!(J - 1) < TYP!(J) Then
                    Swap TMM$(J - 1), TMM$(J)
                    Swap TYP!(J - 1), TYP!(J)
                End If
            End If

            If sortType = 2 Then
                If TYP!(J - 1) > TYP!(J) Then
                    Swap TMM$(J - 1), TMM$(J)
                    Swap TYP!(J - 1), TYP!(J)
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

    Shared TYP!()
    Shared expIndCategory$(), TMM$(), TPP$()

    userExit% = 0

    Do

        Color 14, 0
        Cls
        Print "     PLAYER                  TEAM                     "; expIndCategory$(categoryId)

        For W = 1 To maxRecords

            If W <> 0 And (W Mod 20 = 0) Then

                Color 14
                Locate 22: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
                Color 15

                I$ = GetKeyPress$

                If I$ = Chr$(27) Then
                    userExit% = 1
                Else
                    Color 14, 0
                    Cls
                    Print "     PLAYER                  TEAM                     "; expIndCategory$(categoryId)
                End If

            End If

            If userExit% <> 1 Then

                Color 12
                Print Using "###"; W;
                Print " ";
                Color 3: Print TPP$(W);
                Color 15
                Print Tab(25); TMM$(W);

                Select Case TVN

                    Case 1
                        Print Tab(58); Using "#####"; TYP!(W)

                    Case 2
                        Print Tab(58); Using ".###"; TYP!(W)

                    Case 3
                        Print Tab(58); Using "###.#"; TYP!(W)

                End Select

            Else
                Exit Do
            End If

        Next W

        If userExit% <> 1 Then

            Color 14
            Locate 22: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
            Color 15

            I$ = GetKeyPress$

            If I$ <> Chr$(27) Then

                Locate 22: Print Space$(64)
                Color 14

                Do
                    Locate 22
                    Print "VIEW AGAIN? (Y/N)"
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

    Shared TYP!()
    Shared TMM$(), expTeamCategory$()

    userExit% = 0

    Select Case printOrView

        Case 1:

            Do

                Color 14, 0
                Cls

                Print "     TEAM                     "; expTeamCategory$(categoryID)

                For W = 1 To maxRecords

                    If W <> 1 And (W Mod 20 = 1) Then

                        Color 14
                        Locate 22: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
                        Color 15

                        I$ = GetKeyPress$

                        If I$ = Chr$(27) Then
                            userExit% = 1
                        Else

                            Color 14, 0
                            Cls

                            Print "     TEAM                     "; expTeamCategory$(categoryId)

                        End If

                    End If

                    If userExit% <> 1 Then

                        Color 12
                        Print Using "###"; W;
                        Print " ";
                        Color 3: Print TMM$(W);
                        Color 15

                        Select Case TVN

                            Case 1:
                                Print Tab(32); Using "#####"; TYP!(W)

                            Case 2:
                                Print Tab(33); Using ".###"; TYP!(W)

                            Case 3:
                                Print Tab(33); Using "###.#"; TYP!(W)

                        End Select

                    End If

                Next W

                If userExit% <> 1 Then

                    Color 14
                    Locate 22: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
                    Color 15

                    I$ = GetKeyPress$

                    If I$ <> Chr$(27) Then

                        Do
                            Locate 22: Print Space$(64)
                            Color 14
                            Locate 22: Print "VIEW AGAIN? (Y/N)"
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
                        userFile$ = _SaveFileDialog$("Save Report File",_CWD$ + "\Report","*.txt","Text File")
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

                    Print #1, Using "###"; W;
                    Print #1, " "; TMM$(W);
                    Color 15

                    Select Case TVN

                        Case 1:
                            Print #1, Tab(32); Using "#####"; TYP!(W)

                        Case 2:
                            Print #1, Tab(33); Using ".###"; TYP!(W)

                        Case 3:
                            Print #1, Tab(33); Using "###.#"; TYP!(W)

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

    Shared TYP!()
    Shared expIndCategory$(), TMM$(), TPP$()

    Color 15, 0
    Cls

    Open destFile$ For Output As #1

    Print #1, "     PLAYER                  TEAM                     "; expIndCategory$(categoryID)

    For W = 1 To maxRecords

        If (W <> 0 And W Mod 50 = 0) Then
            Print #1, Chr$(12)
            Print #1, "     PLAYER                  TEAM                     "; expIndCategory$(categoryID)
        End If

        Print #1, Using "###"; W;
        Print #1, " ";
        Print #1, TPP$(W);
        Print #1, Tab(25); TMM$(W);

        Select Case TVN

            Case 1:
                Print #1, Tab(58); Using "#####"; TYP!(W)

            Case 2:
                Print #1, Tab(58); Using ".###"; TYP!(W)

            Case 3:
                Print #1, Tab(58); Using "###.#"; TYP!(W)

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

            Call MenuBox (10, 20, 30, 2, 4, 15)
            Locate 11

            Color 1
            Locate , 25: Print "MAXIMUM = "; maxItems
            Locate , 25: Input "ENTER LENGTH OF LIST"; listLength

        Loop Until listLength <= maxItems

    End If

End Sub
