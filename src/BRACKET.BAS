'----------------------------------------
'           BRACKET Subroutine
'----------------------------------------
'This subroutine gives the user a menu for
'working with seedings / brackets of pre-
'existing tournament files.
Sub BRACKET ()

    _Title "Courtside College Basketball - View Brackets/Seeding"

    Shared scheduleNG%(), nbrRegionalTeams()

    Shared consolationSetting()
    Shared homeTeam$(), visitingTeam$()

    ' TOURNAMENT DEFAULTS
    ' 0 - CONSOLATION IN REGIONS
    ' 1 - CONSOLATION IN CHAMPIONSHIP

    Do

        Color , 0
        Call TITLE_TOURN

        ReDim menuChoices$(1 To 3)
        menuChoices$(1) = "VIEW BRACKETS            "
        menuChoices$(2) = "VIEW SEEDINGS            "
        menuChoices$(3) = "RETURN TO TOURNAMENT MENU"
        SELECTION% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 10
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case SELECTION%

                Case 1, 2:
                    Call ViewTourneyInfo(SELECTION%)

            End Select
        End If

    Loop Until SELECTION% = 3 Or ExitCode% = 27

End Sub


'----------------------------------------
'       ViewTourneyInfo Subroutine
'----------------------------------------
'This subroutine will let the user choose
'a pre-existing tournament file and Then
'present a chance to modify seedings or
'brackets, depending on the menu choice.
Sub ViewTourneyInfo (ACTION%)

    Shared BS%, NS%

    Shared consolationSetting(), tourneyDefaults()
    Shared tourneyParams(), scheduleNG%(), nbrRegionalTeams()
    Shared homeScores(), visitorScores()
    Shared scheduleAP%()

    Shared homeTeam$(), visitingTeam$()
    Shared regionNames$(), slotName$()
    Shared yearNumber$()

    tourneyFile$ = _OpenFileDialog$("Choose Tournament file", diskPaths$(1), "*.TOU", "Tournament files [.TOU]")
    'ChooseFile$(diskPaths$(1) + "*.TOU", "TOURNAMENT", ExitCode%)

    If tourneyFile$ <> "" Then

        baseFile$ = GetFileBaseName$(tourneyFile$)

        Open diskPaths$(1) + baseFile$ + ".INF" For Input As #1

        Input #1, tournName$

        Input #1, numReg ' NUMBER OF REGIONS

        For I = 0 To 16
            Input #1, nbrRegionalTeams(I) ' NUMBER OF TEAMS IN REGION
            Input #1, regionNames$(I) ' NAME OF REGION
        Next

        For I = 0 To 16
            For I1 = 1 To 16
                For I2 = 0 To 3
                    Input #1, slotName$(I, I1, I2) ' SEED SLOT NAME
                    Input #1, tourneyParams(I, I1, I2) ' REGION, SEED NUMBER, MODE PLAY/TM#
                Next
            Next
        Next

        Input #1, consolationSetting(0)
        Input #1, consolationSetting(1) ' CONSOLATION GAME

        Close #1

        Open diskPaths$(1) + baseFile$ + ".TNY" For Input As #2

        For I = 0 To 14
            Input #2, tourneyDefaults(I)
        Next

        Close #2

        Open tourneyFile$ For Random As #3 Len = TOURNEY_SIZE_BYTES
        fileLength& = LOF(3)
        Close #3

        scheduleAP%(0) = fileLength& / 90
        BS% = Int((scheduleAP%(0) - 1) / 25)

        For I = 1 To scheduleAP%(0)

            Open tourneyFile$ For Random As #4 Len = TOURNEY_SIZE_BYTES

            For X = 0 To 14
                Field #4, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
            Next

            Field #4, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
            Field #4, 64 As X$, 5 As Q$(20), 5 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
            Field #4, 88 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)

            Get #4, I

            For X = 0 To 14:
                scheduleNG%(I, X) = CVI(Q$(X + 1))
            Next

            visitorScores(I) = CVI(Q$(16))
            homeScores(I) = CVI(Q$(17))
            visitingTeam$(I) = Q$(18)
            homeTeam$(I) = Q$(19)

            For X = 0 To 3
                yearNumber$(I, X) = Q$(20 + X)
            Next

            For X = 1 To 3:
                scheduleNG%(I, X + 14) = CVI(Q$(X + 23))
            Next

            For X = 2 To 3
                'Why not for 0 / 1 index?
                yearNumber$(I, X) = RTrim$(yearNumber$(I, X))
            Next

            Close #4

        Next

        Call ViewBracket(numReg, ACTION%)

    End If

End Sub


'----------------------------------------
'  PrintFourTeamBracket Subroutine
'----------------------------------------
'This subroutine uses special characters
'to print a semi-bracket on the screen
Sub PrintFourTeamBracket ()

    Color 15

    For I = 0 To 3

        If (I Mod 2) = 0 Then Locate 6 * I + 5, 55: Print Chr$(179)
        If (I Mod 2) = 0 Then Locate 6 * I + 6, 55: Print Chr$(179)
        If (I Mod 2) = 0 Then Locate 6 * I + 7, 55: Print Chr$(195)
        If (I Mod 2) = 0 Then Locate 6 * I + 8, 55: Print Chr$(179)
        If (I Mod 2) = 0 Then Locate 6 * I + 9, 55: Print Chr$(179)
        If (I Mod 2) = 0 Then Locate 6 * I + 4, 38: Print String$(17, Chr$(196)); Chr$(191)
        If I / 2 <> Int(I / 2) Then
            Locate 6 * I + 4, 38: Print String$(17, Chr$(196)); Chr$(217)
        End If

    Next

    For I = 0 To 1
        If I = 0 Then
            Locate 7, 56: Print String$(13, Chr$(196)); Chr$(191)
        End If

        For I1 = 7 To 17:
            Locate I1 + 1, 69: Print Chr$(179)
        Next

        Locate 13, 69: Print Chr$(195); String$(9, Chr$(196))

        If I = 1 Then
            Locate 19, 56: Print String$(13, Chr$(196)); Chr$(217)
        End If
    Next

    Color 7

End Sub


'----------------------------------------
'      TeamColors Subroutine
'----------------------------------------
'This subroutine tries to determine the
'team colors for the current bracket and
'prints the team name and score using the
'corresponding colors.
Sub TeamColors (index, currentTeam$, currentScore%)

    Shared slotName$(), tourneyParams(), scheduleNG%()

    Dim found

    found = 0

    If Left$(currentTeam$, 3) = "REG" Then

        Print

    Else

        printTeam$ = _Trim$(currentTeam$)

        For X = 0 To 16

            For X1 = 1 To 16
                If printTeam$ = slotName$(X, X1, 0) Then
                    Color tourneyParams(X, X1, 2), tourneyParams(X, X1, 3)
                    found = 1
                    Exit For
                End If
            Next

            If found = 1 Then Exit For

        Next

        If scheduleNG%(index, 13) = 1 Then
            'Completed game
            Print printTeam$;
            Color 14, 0
            'XCoord = Pos(0) + (15 - Len(printTeam$))
            'Locate , XCoord
            'Print Space$(15 - Len(printTeam$));
            Print Using " ###"; currentScore%
        Else
            Print _Trim$(currentTeam$)
        End If

    End If 'Done checking for Region

    Color 7, 0

End Sub

'----------------------------------------
'        ViewBracket Subroutine
'----------------------------------------
'This subroutine will either display the
'tournament seedings or brackets, depending
'on what was chosen in the menu.
Sub ViewBracket (numberRegions, ACTION%)

    Dim bracketA$(9)

    Shared tourneyParams(), scheduleNG%(), nbrRegionalTeams()
    Shared scheduleAP%()
    Shared consolationSetting()
    Shared regionNames$(), slotName$()
    Shared homeScores(), visitorScores()
    Shared homeTeam$(), visitingTeam$()

    ReDim menuChoices$(0 To numberRegions + 2)

    Do

        'EDIT A GIVEN BRACKET
        Color , 0
        Cls

        Color , 6
        Locate 1, 1: Print Tab(80); " ";

        Locate , 23
        Color 15
        Print "TOURNAMENT  - REGION/BRACKET EDITOR"
        Locate LAST_ROW, 1: Print Tab(80); " ";

        Locate , 1
        Color , 0

        For I = 1 To numberRegions
            If regionNames$(I) = "" Then
                regionNames$(I) = "TO BE NAMED"
            End If
            menuChoices$(I) = regionNames$(I)
        Next

        SELECTION% = 1
        Count% = numberRegions + 2
        Start% = 1
        ExitCode% = numberRegions + 2
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 4
        Column% = 32

        If regionNames$(0) <> "" Then
            menuChoices$(0) = regionNames$(0)
            Start% = 0
            Count% = Count% + 1
        End If

        menuChoices$(numberRegions + 1) = "CHAMPIONSHIP BRACKET"
        menuChoices$(numberRegions + 2) = "EXIT                "

        '4 + Count% = top/bottom row,
        '   rows for last 2 brackets
        Call box0(3, 31, 4 + Count%, 52, 2, 15, 1)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 And ExitCode% <> 0 Then

            regionNbr = SELECTION%

            'Championship Bracket
            If regionNbr = numberRegions + 1 Then regionNbr = 17

            'Return to Menu
            If regionNbr = numberRegions + 2 Then Exit Sub

            If ACTION% = 2 Then

                'View Seedings
                Color , 0
                Cls

                Color , 6
                Locate 1, 1: Print Tab(80); " ";

                Locate , 23
                Color 15
                Print "TOURNAMENT  - REGION/BRACKET EDITOR"
                Locate LAST_ROW, 1: Print Tab(80); " ";

                Locate , 1
                Color , 0

                MAXSEED = nbrRegionalTeams(regionNbr) + 1 + 4
                Call box0(3, 24, MAXSEED, 54, 2, 15, 1)

                Locate 4, 25: Print "SEED  TEAM"

                For I1 = 1 To nbrRegionalTeams(regionNbr)
                    Color 2
                    Locate , 25
                    Print Using "##    "; I1;
                    Color 15: Print slotName$(regionNbr, I1, 0)
                Next I1

                I$ = GetKeyPress$

            Else
                'ACTION% = 1
                'View Brackets
                currRegion = regionNbr

                Color , 0

                Cls

                currRegionName$ = regionNames$(currRegion)
                Xcoord = Int((80 - Len(currRegionName$)) / 2)
                Locate 1, Xcoord: Print currRegionName$
                Locate 2

                If currRegion = 17 Then
                    bracketType = 1
                Else
                    bracketType = nbrRegionalTeams(regionNbr) - 3
                End If

                Select Case bracketType

                    Case 1: '4 Teams
                        Call PrintFourTeamBracket

                        REG = regionNbr
                        GAME = 1

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 3, 39: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 2

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 3 + consolationSetting(0)

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 6, 56: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 18, 56: Call TeamColors(I, homeTeam$(I), homeScores(I))

                    Case 2: '3 Teams
                        ' 5 TEAM BRACKET
                        For I = 0 To 1
                            If (I Mod 2) = 0 Then Locate 3 * I + 2, 22: Print bracketA$(I + 1)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 1, 22: Print bracketA$(I + 1)
                            If (I Mod 2) = 0 Then Locate 3 * I + 3, 21: Print String$(16, Chr$(196)); Chr$(191)
                            If (I Mod 2) = 0 Then Locate 3 * I + 4, 37: Print Chr$(195)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 2, 21: Print String$(16, Chr$(196)); Chr$(217)
                        Next

                        Call PrintFourTeamBracket

                        REG = regionNbr
                        GAME = 1

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 2, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 4, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 2

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 3, 39: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 3

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 4 + consolationSetting(0)

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 6, 56: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 18, 56: Call TeamColors(I, homeTeam$(I), homeScores(I))

                    Case 3: '5 Teams
                        ' 6 TEAM BRACKET
                        For I = 0 To 7
                            If (I Mod 2) = 0 Then Locate 3 * I + 2, 22: Print bracketA$(I + 1)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 1, 22: Print bracketA$(I + 1)
                            If I / 2 = Int(I / 2) And (I <> 4 And I <> 2) Then Locate 3 * I + 3, 21: Print String$(16, Chr$(196)); Chr$(191)
                            If I / 2 = Int(I / 2) And (I <> 4 And I <> 2) Then Locate 3 * I + 4, 37: Print Chr$(195)
                            If I / 2 <> Int(I / 2) And (I <> 5 And I <> 3) Then Locate 3 * I + 2, 21: Print String$(16, Chr$(196)); Chr$(217)
                        Next

                        Call PrintFourTeamBracket

                        REG = regionNbr
                        GAME = 1

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 2, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 4, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 2

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 20, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 22, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 3

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 3, 39: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 4

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 5 + consolationSetting(0)

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 6, 56: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 18, 56: Call TeamColors(I, homeTeam$(I), homeScores(I))

                    Case 4: '6 Teams
                        ' 7 TEAM BRACKET
                        For I = 0 To 7
                            If (I Mod 2) = 0 Then Locate 3 * I + 2, 22: Print bracketA$(I + 1)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 1, 22: Print bracketA$(I + 1)
                            If I / 2 = Int(I / 2) And I <> 4 Then Locate 3 * I + 3, 21: Print String$(16, Chr$(196)); Chr$(191)
                            If I / 2 = Int(I / 2) And I <> 4 Then Locate 3 * I + 4, 37: Print Chr$(195)
                            If I / 2 <> Int(I / 2) And I <> 5 Then Locate 3 * I + 2, 21: Print String$(16, Chr$(196)); Chr$(217)
                        Next

                        Call PrintFourTeamBracket

                        REG = regionNbr
                        GAME = 1

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 2, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 4, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 2

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 8, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 10, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 3

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 20, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 22, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 4

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 3, 39: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 5

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 6 + consolationSetting(0)

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 6, 56: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 18, 56: Call TeamColors(I, homeTeam$(I), homeScores(I))

                    Case 5: '7 Teams
                        ' 8 TEAM BRACKET
                        For I = 0 To 7
                            If (I Mod 2) = 0 Then Locate 3 * I + 2, 22: Print bracketA$(I + 1)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 1, 22: Print bracketA$(I + 1)
                            If (I Mod 2) = 0 Then Locate 3 * I + 3, 21: Print String$(16, Chr$(196)); Chr$(191)
                            If (I Mod 2) = 0 Then Locate 3 * I + 4, 37: Print Chr$(195)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 2, 21: Print String$(16, Chr$(196)); Chr$(217)
                        Next

                        Call PrintFourTeamBracket

                        REG = regionNbr
                        GAME = 1

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 2, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 4, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 2

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 8, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 10, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 3

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 14, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 16, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 4

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 20, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 22, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 5

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 3, 39: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 6

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 7 + consolationSetting(0)

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 6, 56: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 18, 56: Call TeamColors(I, homeTeam$(I), homeScores(I))

                    Case 6: '8 Teams
                        '9 TEAM BRACKET
                        Locate 22, 1

                        REG = regionNbr
                        GAME = 1

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Call TeamColors(I, homeTeam$(I), homeScores(I))

                        For I = 0 To 7
                            If (I Mod 2) = 0 Then Locate 3 * I + 2, 22: Print bracketA$(I + 1)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 1, 22: Print bracketA$(I + 1)
                            If (I Mod 2) = 0 Then Locate 3 * I + 3, 21: Print String$(16, Chr$(196)); Chr$(191)
                            If (I Mod 2) = 0 Then Locate 3 * I + 4, 37: Print Chr$(195)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 2, 21: Print String$(16, Chr$(196)); Chr$(217)
                        Next

                        Call PrintFourTeamBracket

                        REG = regionNbr
                        GAME = 2

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 2, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 4, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 3

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 8, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 10, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 4

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 14, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 16, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 5

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 20, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 22, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 6

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 3, 39: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 7

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 8 + consolationSetting(0)

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 6, 56: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 18, 56: Call TeamColors(I, homeTeam$(I), homeScores(I))

                    Case 7: '10 Teams
                        '10 TEAM BRACKET

                        REG = regionNbr
                        GAME = 1

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Call TeamColors(I, homeTeam$(I), homeScores(I))

                        Print
                        Print
                        Print
                        Print
                        Print
                        Print
                        Print
                        Print
                        Print
                        Print
                        Print
                        Print
                        Print
                        Print
                        Print
                        Print

                        REG = regionNbr
                        GAME = 2

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Call TeamColors(I, homeTeam$(I), homeScores(I))

                        For I = 0 To 7
                            If (I Mod 2) = 0 Then Locate 3 * I + 2, 22: Print bracketA$(I + 1)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 1, 22: Print bracketA$(I + 1)
                            If (I Mod 2) = 0 Then Locate 3 * I + 3, 21: Print String$(16, Chr$(196)); Chr$(191)
                            If (I Mod 2) = 0 Then Locate 3 * I + 4, 37: Print Chr$(195)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 2, 21: Print String$(16, Chr$(196)); Chr$(217)
                        Next

                        Call PrintFourTeamBracket

                        REG = regionNbr
                        GAME = 3

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 2, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 4, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 4

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 8, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 10, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 5

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 14, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 16, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 6

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 20, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 22, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 7

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 3, 39: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 8

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 9 + consolationSetting(0)

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 6, 56: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 18, 56: Call TeamColors(I, homeTeam$(I), homeScores(I))

                    Case 8: '11 Teams
                        '11 TEAM BRACKET

                        REG = regionNbr
                        GAME = 1

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Call TeamColors(I, homeTeam$(I), homeScores(I))

                        Print
                        Print
                        Print
                        Print

                        REG = regionNbr
                        GAME = 2
                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next
                        Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Call TeamColors(I, homeTeam$(I), homeScores(I))
                        Print
                        Print
                        Print
                        Print
                        Print
                        Print
                        Print
                        Print
                        Print
                        Print

                        REG = regionNbr
                        GAME = 3

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Call TeamColors(I, homeTeam$(I), homeScores(I))

                        For I = 0 To 7
                            If (I Mod 2) = 0 Then Locate 3 * I + 2, 22: Print bracketA$(I + 1)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 1, 22: Print bracketA$(I + 1)
                            If (I Mod 2) = 0 Then Locate 3 * I + 3, 21: Print String$(16, Chr$(196)); Chr$(191)
                            If (I Mod 2) = 0 Then Locate 3 * I + 4, 37: Print Chr$(195)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 2, 21: Print String$(16, Chr$(196)); Chr$(217)
                        Next

                        Call PrintFourTeamBracket

                        REG = regionNbr
                        GAME = 4

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 2, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 4, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 5

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 8, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 10, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 6

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 14, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 16, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 7

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 20, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 22, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 8

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 3, 39: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 9

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 10 + consolationSetting(0)

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 6, 56: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 18, 56: Call TeamColors(I, homeTeam$(I), homeScores(I))

                    Case 9: '12 Teams
                        '12 TEAM BRACKET

                        REG = regionNbr
                        GAME = 1

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Call TeamColors(I, homeTeam$(I), homeScores(I))

                        Print
                        Print
                        Print
                        Print

                        REG = regionNbr
                        GAME = 2

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Call TeamColors(I, homeTeam$(I), homeScores(I))

                        Print
                        Print
                        Print
                        Print

                        REG = regionNbr
                        GAME = 3

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Call TeamColors(I, homeTeam$(I), homeScores(I))

                        Print
                        Print
                        Print
                        Print

                        REG = regionNbr
                        GAME = 4

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Call TeamColors(I, homeTeam$(I), homeScores(I))

                        Print
                        Print
                        Print

                        Color 15

                        For I = 0 To 7
                            If (I Mod 2) = 0 Then Locate 3 * I + 2, 22: Print bracketA$(I + 1)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 1, 22: Print bracketA$(I + 1)
                            If (I Mod 2) = 0 Then Locate 3 * I + 3, 21: Print String$(16, Chr$(196)); Chr$(191)
                            If (I Mod 2) = 0 Then Locate 3 * I + 4, 37: Print Chr$(195)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 2, 21: Print String$(16, Chr$(196)); Chr$(217)
                        Next

                        Color 7

                        Call PrintFourTeamBracket

                        REG = regionNbr
                        GAME = 5

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 2, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 4, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 6

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 8, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 10, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 7

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 14, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 16, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 8

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 20, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 22, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 9

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 3, 39: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 10

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 11 + consolationSetting(0)

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 6, 56: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 18, 56: Call TeamColors(I, homeTeam$(I), homeScores(I))
                        Locate 12, 64

                    Case 10: '13 Teams
                        '13 TEAM BRACKET

                        REG = regionNbr
                        GAME = 1

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Call TeamColors(I, homeTeam$(I), homeScores(I))

                        Print

                        REG = regionNbr
                        GAME = 2

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Call TeamColors(I, homeTeam$(I), homeScores(I))

                        Print

                        REG = regionNbr
                        GAME = 3

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Call TeamColors(I, homeTeam$(I), homeScores(I))

                        Print
                        Print
                        Print
                        Print

                        REG = regionNbr
                        GAME = 4

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Call TeamColors(I, homeTeam$(I), homeScores(I))

                        Print
                        Print
                        Print
                        Print

                        REG = regionNbr
                        GAME = 5

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Call TeamColors(I, homeTeam$(I), homeScores(I))

                        Print
                        Print
                        Print

                        For I = 0 To 7
                            If (I Mod 2) = 0 Then Locate 3 * I + 2, 22: Print bracketA$(I + 1)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 1, 22: Print bracketA$(I + 1)
                            If (I Mod 2) = 0 Then Locate 3 * I + 3, 21: Print String$(16, Chr$(196)); Chr$(191)
                            If (I Mod 2) = 0 Then Locate 3 * I + 4, 37: Print Chr$(195)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 2, 21: Print String$(16, Chr$(196)); Chr$(217)
                        Next

                        Call PrintFourTeamBracket

                        REG = regionNbr
                        GAME = 6

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 2, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 4, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 7

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 8, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 10, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 8

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 14, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 16, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 9

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 20, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 22, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 10

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 3, 39: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 11

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 12 + consolationSetting(0)

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 6, 56: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 18, 56: Call TeamColors(I, homeTeam$(I), homeScores(I))

                    Case 11: '14 Teams
                        '14 TEAM BRACKET

                        REG = regionNbr
                        GAME = 1

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Call TeamColors(I, homeTeam$(I), homeScores(I))

                        Print

                        REG = regionNbr
                        GAME = 4

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Call TeamColors(I, homeTeam$(I), homeScores(I))

                        Print

                        REG = regionNbr
                        GAME = 5

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Call TeamColors(I, homeTeam$(I), homeScores(I))

                        Print
                        Print
                        Print
                        Print

                        REG = regionNbr
                        GAME = 2

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Call TeamColors(I, homeTeam$(I), homeScores(I))

                        Print

                        REG = regionNbr
                        GAME = 3

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Call TeamColors(I, homeTeam$(I), homeScores(I))

                        Print

                        REG = regionNbr
                        GAME = 6

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Call TeamColors(I, homeTeam$(I), homeScores(I))

                        For I = 0 To 7
                            If (I Mod 2) = 0 Then Locate 3 * I + 2, 22: Print bracketA$(I + 1)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 1, 22: Print bracketA$(I + 1)
                            If (I Mod 2) = 0 Then Locate 3 * I + 3, 21: Print String$(16, Chr$(196)); Chr$(191)
                            If (I Mod 2) = 0 Then Locate 3 * I + 4, 37: Print Chr$(195)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 2, 21: Print String$(16, Chr$(196)); Chr$(217)
                        Next

                        Call PrintFourTeamBracket

                        REG = regionNbr
                        GAME = 7

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 2, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 4, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 8

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 8, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 10, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 9

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 14, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 16, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 10

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 20, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 22, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 11

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 3, 39: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 12

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 13 + consolationSetting(0)

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 6, 56: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 18, 56: Call TeamColors(I, homeTeam$(I), homeScores(I))

                    Case 12: '15 Teams
                        '15 TEAM BRACKET

                        REG = regionNbr
                        GAME = 1
                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Call TeamColors(I, homeTeam$(I), homeScores(I))

                        Print

                        REG = regionNbr
                        GAME = 6

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Call TeamColors(I, homeTeam$(I), homeScores(I))

                        Print

                        REG = regionNbr
                        GAME = 2

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Call TeamColors(I, homeTeam$(I), homeScores(I))

                        Print

                        REG = regionNbr
                        GAME = 5
                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Call TeamColors(I, homeTeam$(I), homeScores(I))

                        Print

                        REG = regionNbr
                        GAME = 3

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Call TeamColors(I, homeTeam$(I), homeScores(I))

                        Print

                        REG = regionNbr
                        GAME = 4

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Call TeamColors(I, homeTeam$(I), homeScores(I))

                        Print

                        REG = regionNbr
                        GAME = 7

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Call TeamColors(I, homeTeam$(I), homeScores(I))

                        For I = 0 To 7
                            If (I Mod 2) = 0 Then Locate 3 * I + 2, 22: Print bracketA$(I + 1)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 1, 22: Print bracketA$(I + 1)
                            If (I Mod 2) = 0 Then Locate 3 * I + 3, 21: Print String$(16, Chr$(196)); Chr$(191)
                            If (I Mod 2) = 0 Then Locate 3 * I + 4, 37: Print Chr$(195)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 2, 21: Print String$(16, Chr$(196)); Chr$(217)
                        Next

                        Call PrintFourTeamBracket

                        REG = regionNbr
                        GAME = 8

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 2, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 4, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 9

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 8, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 10, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 10
                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 14, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 16, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 11

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 20, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 22, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 12

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 3, 39: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 13

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 14 + consolationSetting(0)

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 6, 56: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 18, 56: Call TeamColors(I, homeTeam$(I), homeScores(I))

                    Case 13: '16 Teams
                        '16 TEAM BRACKET
                        REG = regionNbr
                        GAME = 1

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Call TeamColors(I, homeTeam$(I), homeScores(I))

                        Print

                        REG = regionNbr
                        GAME = 8

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Call TeamColors(I, homeTeam$(I), homeScores(I))

                        Print

                        REG = regionNbr
                        GAME = 4

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Call TeamColors(I, homeTeam$(I), homeScores(I))

                        Print

                        REG = regionNbr
                        GAME = 5

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Call TeamColors(I, homeTeam$(I), homeScores(I))

                        Print

                        REG = regionNbr
                        GAME = 3

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Call TeamColors(I, homeTeam$(I), homeScores(I))

                        Print

                        REG = regionNbr
                        GAME = 6

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Call TeamColors(I, homeTeam$(I), homeScores(I))

                        Print

                        REG = regionNbr
                        GAME = 2

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Call TeamColors(I, homeTeam$(I), homeScores(I))

                        Print

                        REG = regionNbr
                        GAME = 7

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Call TeamColors(I, homeTeam$(I), homeScores(I))

                        Color 15

                        For I = 0 To 7
                            If (I Mod 2) = 0 Then Locate 3 * I + 2, 22: Print bracketA$(I + 1)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 1, 22: Print bracketA$(I + 1)
                            If (I Mod 2) = 0 Then Locate 3 * I + 3, 21: Print String$(16, Chr$(196)); Chr$(191)
                            If (I Mod 2) = 0 Then Locate 3 * I + 4, 37: Print Chr$(195)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 2, 21: Print String$(16, Chr$(196)); Chr$(217)
                        Next

                        Color 7

                        Call PrintFourTeamBracket

                        REG = regionNbr
                        GAME = 9

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 2, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 4, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 10

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 8, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 10, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 11

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 14, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 16, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 12

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 20, 21: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 22, 21: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 13

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 3, 39: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 14

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(I, homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 15 + consolationSetting(0)

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 6, 56: Call TeamColors(I, visitingTeam$(I), visitorScores(I))
                        Locate 18, 56: Call TeamColors(I, homeTeam$(I), homeScores(I))

                End Select

                'Print team and wait for keypress
                Locate 12, 64

                If scheduleNG%(I, 13) = 1 Then

                    'Determine winner
                    If homeScores(I) > visitorScores(I) Then
                        printTeam$ = _Trim$(homeTeam$(I))
                    Else
                        printTeam$ = _Trim$(visitingTeam$(I))
                    End If

                    found = 0

                    'Determine team color
                    For X = 0 To 16

                        For X1 = 1 To 16
                            If printTeam$ = slotName$(X, X1, 0) Then
                                Color tourneyParams(X, X1, 2), tourneyParams(X, X1, 3)
                                found = 1
                                Exit For
                            End If
                        Next

                        If found = 1 Then Exit For

                    Next

                    'Print winner
                    Print printTeam$

                End If

                I$ = GetKeyPress$

            End If 'Done determining our ACTION%

        End If 'Done checking for ESC

        'menu until they choose exit
    Loop Until ExitCode% = 27 Or SELECTION% = numberRegions + 2 'ACTION% = 2

End Sub


'----------------------------------------
'       BracketsNotUsed Subroutine
'----------------------------------------
' This code was in the original but is not
'referenced. It appears some thought was
'given here to use this for printing 2-team
'or 3-team brackets.
Sub BracketsNotUsed ()

    Shared scheduleAP%()

    '3 TEAM BRACKET
    For I = 0 To 1
        If (I Mod 2) = 0 Then Locate 6 * I + 5, 47: Print Chr$(179)
        If (I Mod 2) = 0 Then Locate 6 * I + 6, 47: Print Chr$(179)
        If (I Mod 2) = 0 Then Locate 6 * I + 7, 47: Print Chr$(195)
        If (I Mod 2) = 0 Then Locate 6 * I + 8, 47: Print Chr$(179)
        If (I Mod 2) = 0 Then Locate 6 * I + 9, 47: Print Chr$(179)

        If (I Mod 2) = 0 Then Locate 6 * I + 4, 32: Print String$(15, Chr$(196)); Chr$(191)
        If I / 2 <> Int(I / 2) Then Locate 6 * I + 4, 32: Print String$(15, Chr$(196)); Chr$(217)
    Next

    For I = 0 To 1
        If I = 0 Then Locate 7, 48: Print String$(15, Chr$(196)); Chr$(191)
        For I1 = 7 To 17: Locate I1 + 1, 63: Print Chr$(179): Next
        Locate 13, 63: Print Chr$(195); String$(15, Chr$(196))
        If I = 1 Then Locate 19, 48: Print String$(15, Chr$(196)); Chr$(217)
    Next

    Locate 3, 32: Print visitingTeam$(1); Str$(visitorScores(1))
    Locate 9, 32: Print homeTeam$(1); Str$(homeScores(1))
    Locate 6, 48: Print Left$(visitingTeam$(2), 13); Str$(visitorScores(2))
    Locate 18, 48: Print Left$(homeTeam$(2), 13); Str$(homeScores(2))

    '--------------------

    'TWO TEAM BRACKET
    For I = 0 To 1
        If I = 0 Then Locate 7, 48: Print String$(15, Chr$(196)); Chr$(191)
        For I1 = 7 To 17: Locate I1 + 1, 63: Print Chr$(179): Next
        Locate 13, 63: Print Chr$(195); String$(15, Chr$(196))
        If I = 1 Then Locate 19, 48: Print String$(15, Chr$(196)); Chr$(217)
        Locate 6, 48: Print visitingTeam$(4); Str$(visitorScores(4))
        Locate 18, 48: Print homeTeam$(4); Str$(homeScores(4))
    Next

End Sub
