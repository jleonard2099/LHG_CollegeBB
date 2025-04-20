'----------------------------------------
'           BRACKET Subroutine
'----------------------------------------
'This subroutine gives the user a menu for
'working with seedings / brackets of pre-
'existing tournament files.
Sub BRACKET ()

    _Title "Courtside College Basketball - View Brackets/Seeding"

    Shared scheduleNG%(), nbrRegionalTeams()

    Shared consolationSetting%(), tourneyDefaults%()
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

    Shared consolationSetting%(), tourneyDefaults%()
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

        Input #1, FF$

        Input #1, R% ' NUMBER OF REGIONS

        For I = 1 To 16
            Input #1, nbrRegionalTeams(I) ' NUMBER OF TEAMS IN REGION
            Input #1, regionNames$(I) ' NAME OF REGION
        Next

        For I = 1 To 16
            For I1 = 1 To 16
                For I2 = 0 To 3
                    Input #1, slotName$(I, I1, I2) ' SEED SLOT NAME
                    Input #1, tourneyParams(I, I1, I2) ' REGION, SEED NUMBER, MODE PLAY/TM#
                Next
            Next
        Next

        Input #1, consolationSetting%(0)
        Input #1, consolationSetting%(1) ' CONSOLATION GAME

        Close #1

        Open diskPaths$(1) + baseFile$ + ".TNY" For Input As #2

        For I = 0 To 14
            Input #2, tourneyDefaults%(I)
        Next

        Close #2

        If consolationSetting%(0) = 1 Then C$ = "Y" Else C$ = "N"
        NR$ = Str$(R%)

        Open tourneyFile$ For Random As #3 Len = 94
        fileLength& = LOF(3)
        Close #3

        scheduleAP%(0) = fileLength& / 90
        BS% = Int((scheduleAP%(0) - 1) / 20)

        For I = 1 To scheduleAP%(0)

            Open tourneyFile$ For Random As #4 Len = 94

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

        Call EditBracket(R%, ACTION%)

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
Sub TeamColors (currentTeam$, currentScore%)

    Shared slotName$(), tourneyParams(), scheduleNG%()

    Dim found

    found = 0

    If Left$(currentTeam$, 3) = "REG" Then

        Print

    Else

        X$ = _Trim$(currentTeam$)

        For X = 1 To 16

            For X1 = 1 To 16
                If X$ = slotName$(X, X1, 0) Then
                    Color tourneyParams(X, X1, 2), tourneyParams(X, X1, 3)
                    found = 1
                    Exit For
                End If
            Next

            If found = 1 Then Exit For

        Next

        'Color 7, 0

        If Not (scheduleNG%(I, 13) = 0) Then
            Print _Trim$(currentTeam$);
            Color 14, 0: Print Using " ###"; currentScore%
        Else
            Print _Trim$(currentTeam$)
        End If

    End If 'Done checking for Region

    Color 7, 0

End Sub

'----------------------------------------
'        EditBracket Subroutine
'----------------------------------------
'This subroutine will either display the
'tournament seedings or brackets, depending
'on what was chosen in the menu.
Sub EditBracket (numberRegions, ACTION%)

    Dim bracketA$(9)

    Shared tourneyParams(), scheduleNG%(), nbrRegionalTeams()
    Shared scheduleAP%()
    Shared consolationSetting%(), tourneyDefaults%()
    Shared regionNames$(), slotName$()
    Shared homeScores(), visitorScores()
    Shared homeTeam$(), visitingTeam$()

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
        Locate , 1: Color , 0

        ReDim menuChoices$(1 To numberRegions + 2)

        For I = 1 To numberRegions
            If regionNames$(I) = "" Then regionNames$(I) = "TO BE NAMED"
            menuChoices$(I) = regionNames$(I)
        Next

        SELECTION% = 1
        Count% = numberRegions + 2
        Start% = 1
        ExitCode% = numberRegions + 2
        FileColr% = 15
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 4
        Column% = 32
        menuChoices$(numberRegions + 1) = "CHAMPIONSHIP BRACKET"
        menuChoices$(numberRegions + 2) = "EXIT                "

        Call box0(3, 31, 3 + numberRegions + 3, 52, 2, 15, 1)

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
                Locate , 1: Color , 0

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

                        Locate 3, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 2

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 3 + consolationSetting%(0)

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 6, 56: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 18, 56: Call TeamColors(homeTeam$(I), homeScores(I))

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

                        Locate 2, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 4, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 2

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 3, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 3

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 4 + consolationSetting%(0)

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 6, 56: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 18, 56: Call TeamColors(homeTeam$(I), homeScores(I))

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

                        Locate 2, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 4, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 2

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 20, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 22, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 3

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 3, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 4

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 5 + consolationSetting%(0)

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 6, 56: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 18, 56: Call TeamColors(homeTeam$(I), homeScores(I))

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

                        Locate 2, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 4, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 2

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 8, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 10, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 3

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 20, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 22, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 4

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 3, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 5

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 6 + consolationSetting%(0)

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 6, 56: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 18, 56: Call TeamColors(homeTeam$(I), homeScores(I))

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

                        Locate 2, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 4, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 2

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 8, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 10, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 3

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 14, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 16, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 4

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 20, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 22, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 5

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 3, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 6

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 7 + consolationSetting%(0)

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 6, 56: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 18, 56: Call TeamColors(homeTeam$(I), homeScores(I))

                    Case 6: '8 Teams
                        '9 TEAM BRACKET
                        Locate 22, 1

                        REG = regionNbr
                        GAME = 1

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

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

                        Locate 2, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 4, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 3

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 8, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 10, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 4

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 14, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 16, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 5

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 20, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 22, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 6

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 3, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 7

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 8 + consolationSetting%(0)

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 6, 56: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 18, 56: Call TeamColors(homeTeam$(I), homeScores(I))

                    Case 7: '10 Teams
                        '10 TEAM BRACKET

                        REG = regionNbr
                        GAME = 1

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

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

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

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

                        Locate 2, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 4, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 4

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 8, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 10, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 5

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 14, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 16, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 6

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 20, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 22, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 7

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 3, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 8

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 9 + consolationSetting%(0)

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 6, 56: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 18, 56: Call TeamColors(homeTeam$(I), homeScores(I))

                    Case 8: '11 Teams
                        '11 TEAM BRACKET

                        REG = regionNbr
                        GAME = 1

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print
                        Print
                        Print
                        Print

                        REG = regionNbr
                        GAME = 2
                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next
                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))
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

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

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

                        Locate 2, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 4, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 5

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 8, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 10, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 6

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 14, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 16, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 7

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 20, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 22, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 8

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 3, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 9

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 10 + consolationSetting%(0)

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 6, 56: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 18, 56: Call TeamColors(homeTeam$(I), homeScores(I))

                    Case 9: '12 Teams
                        '12 TEAM BRACKET

                        REG = regionNbr
                        GAME = 1

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print
                        Print
                        Print
                        Print

                        REG = regionNbr
                        GAME = 2

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print
                        Print
                        Print
                        Print

                        REG = regionNbr
                        GAME = 3

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print
                        Print
                        Print
                        Print

                        REG = regionNbr
                        GAME = 4

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

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

                        Locate 2, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 4, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 6

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 8, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 10, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 7

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 14, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 16, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 8

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 20, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 22, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 9

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 3, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 10

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 11 + consolationSetting%(0)

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 6, 56: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 18, 56: Call TeamColors(homeTeam$(I), homeScores(I))
                        Locate 12, 64

                    Case 10: '13 Teams
                        '13 TEAM BRACKET

                        REG = regionNbr
                        GAME = 1

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print

                        REG = regionNbr
                        GAME = 2

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print

                        REG = regionNbr
                        GAME = 3

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print
                        Print
                        Print
                        Print

                        REG = regionNbr
                        GAME = 4

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print
                        Print
                        Print
                        Print

                        REG = regionNbr
                        GAME = 5

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

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

                        Locate 2, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 4, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 7

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 8, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 10, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 8

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 14, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 16, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 9

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 20, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 22, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 10

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 3, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 11

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 12 + consolationSetting%(0)

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 6, 56: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 18, 56: Call TeamColors(homeTeam$(I), homeScores(I))

                    Case 11: '14 Teams
                        '14 TEAM BRACKET

                        REG = regionNbr
                        GAME = 1

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print

                        REG = regionNbr
                        GAME = 4

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print

                        REG = regionNbr
                        GAME = 5

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print
                        Print
                        Print
                        Print

                        REG = regionNbr
                        GAME = 2

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print

                        REG = regionNbr
                        GAME = 3

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print

                        REG = regionNbr
                        GAME = 6

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

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

                        Locate 2, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 4, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 8

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 8, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 10, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 9

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 14, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 16, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 10

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 20, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 22, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 11

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 3, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 12

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 13 + consolationSetting%(0)

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 6, 56: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 18, 56: Call TeamColors(homeTeam$(I), homeScores(I))

                    Case 12: '15 Teams
                        '15 TEAM BRACKET

                        REG = regionNbr
                        GAME = 1
                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print

                        REG = regionNbr
                        GAME = 6

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print

                        REG = regionNbr
                        GAME = 2

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print

                        REG = regionNbr
                        GAME = 5
                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print

                        REG = regionNbr
                        GAME = 3

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print

                        REG = regionNbr
                        GAME = 4

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print

                        REG = regionNbr
                        GAME = 7

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

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

                        Locate 2, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 4, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 9

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 8, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 10, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 10
                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 14, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 16, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 11

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 20, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 22, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 12

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 3, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 13

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 14 + consolationSetting%(0)

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 6, 56: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 18, 56: Call TeamColors(homeTeam$(I), homeScores(I))

                    Case 13: '16 Teams
                        '16 TEAM BRACKET
                        REG = regionNbr
                        GAME = 1

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print

                        REG = regionNbr
                        GAME = 8

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print

                        REG = regionNbr
                        GAME = 4

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print

                        REG = regionNbr
                        GAME = 5

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print

                        REG = regionNbr
                        GAME = 3

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print

                        REG = regionNbr
                        GAME = 6

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print

                        REG = regionNbr
                        GAME = 2

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print

                        REG = regionNbr
                        GAME = 7

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

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

                        Locate 2, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 4, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 10

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 8, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 10, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 11

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 14, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 16, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 12

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 20, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 22, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 13

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 3, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 14

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = regionNbr
                        GAME = 15 + consolationSetting%(0)

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = regionNbr And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 6, 56: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 18, 56: Call TeamColors(homeTeam$(I), homeScores(I))

                End Select

                'Print team and wait for keypress
                Locate 12, 64

                If scheduleNG%(I, 13) <> 0 Then
                    'Determine winner
                    If homeScores(I) > visitorScores(I) Then
                        X$ = _Trim$(homeTeam$(I))
                    Else
                        X$ = _Trim$(visitingTeam$(I))
                    End If

                    found = 0
                    'Determine team color
                    For X = 1 To 16
                        For X1 = 1 To 16
                            If X$ = slotName$(X, X1, 0) Then
                                Color tourneyParams(X, X1, 2), tourneyParams(X, X1, 3)
                                found = 1
                                Exit For
                            End If
                        Next
                        If found = 1 Then Exit For
                    Next

                    'Print winner
                    Color 7, 0
                    Print X$

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


'$Include: 'CAREER.BAS'
'$Include: 'CAREDIT.BAS'
'$Include: 'CARLDR.BAS'
'$Include: 'CARVIEW.BAS'
'$Include: 'CNGTM.BAS'
'$Include: 'COMPARE.BAS'
'$Include: 'COMPCON.BAS'
'$Include: 'COMPNATL.BAS'
'$Include: 'CREATCON.BAS'
'$Include: 'CREATNATL.BAS'
'$Include: 'DRAFT.BAS'
'$Include: 'HD2HDCON.BAS'
'$Include: 'LOOKY.BAS'
'$Include: 'MERGE.BAS'
'$Include: 'NEWLDR.BAS'
'Include: 'NEWLDRST.BAS'


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
        Print RTrim$(targetDiv$)

        Color 14
        Locate 4: Print Tab(41); "MAG          HOME    ROAD    LAST 10"
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

        Color 15
        Locate 6, 42
        Print Using "##"; MJ;

        Color 14
        Locate LAST_ROW - 1, 28: Print "HIT ANY KEY TO CONTINUE";
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
        Print #1, String$(75, 45)

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
