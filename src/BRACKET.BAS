Sub BRACKET ()

    _Title "Courtside College Basketball - View Brackets/Seeding"

    Shared scheduleNG%(), NN%()

    Shared consolationSetting%(), tourneyDefaults%()
    Shared homeScores(), visitorScores()
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
        Selection% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 10
        Column% = 25

        Call box0(9, 24, 13, 50, 2, 15, 1)
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then
            Select Case Selection%
                Case 1 To 2: Call ViewTourneyInfo(Selection%)
                Case 3: Cls
            End Select
        End If

    Loop Until Selection% = 3 Or ExitCode% = 27

End Sub

'------------------------------
'  ViewTourneyInfo Subroutine
'------------------------------
' ...explanation...
Sub ViewTourneyInfo (ACTION%)

    Shared consolationSetting%(), tourneyDefaults%()
    Shared tourneyN%(), scheduleNG%(), NN%()
    Shared homeScores(), visitorScores()
    Shared scheduleAP%()

    Shared homeTeam$(), visitingTeam$()
    Shared regionNames$(), slotName$()
    Shared yearNumber$()

    Do
        ' *** LOAD IN A SAVED TOURNAMENT ***
		Spec$ = diskPaths$(1) + "*.TOU"
		Count% = FCount%(Spec$)

        If Count% = 0 Then
            Locate 10, 8
            Call CSMsgBox("NO TOURNAMENT FILES WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)
            'Should go back to selection instead
        Else

            ReDim tournamentFiles$(0 To Count%)
            For X = 1 To Count%
                tournamentFiles$(X) = Space$(12)
            Next
			
            tournamentFiles$(0) = Spec$
            Call ReadFile(tournamentFiles$())

            Cls

            Color , 1
            Cls
            Locate , 28: Color 14
            Print "SELECT TOURNAMENT TO VIEW": Color 15
            Selection% = 1
            Start% = 1
            FileFGColr% = 15: FileBGColr% = 1
            HiLiteFGColr% = 0: HiLiteBGColr% = 7
            Rows% = 20
            Columns% = 4
            Spacing% = 6
            Row% = 4
            Column% = 1
            Locate 25, 1: Color 14: Print "ESC";: Color 15
            Print "-SELECT NEW ID  PGDN  PGUP  ";: Color 14
            Print "ENTER";: Color 15
            Print "-SELECTS TOURNAMENT";

            _MouseShow "DEFAULT"
            MAMenu tournamentFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
            _MouseHide

            If ExitCode% <> 27 Then

                Color 15, 1
                targetFile$ = tournamentFiles$(Selection%)
                Locate 2, 33: Print targetFile$

                Do
                    Locate 3, 33: Print "ANY CHANGE (YN)";
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                Color 15, 0

                If UCase$(I$) <> "Y" Then

                    targetFile$ = _Trim$(targetFile$)
                    targetFile$ = Left$(targetFile$, Len(targetFile$) - 4)
                End If

                Open diskPaths$(1) + targetFile$ + ".INF" For Input As #2
                Input #2, FF$
                Input #2, R% ' NUMBER OF REGIONS
                For I = 1 To 16
                    Input #2, NN%(I) ' NUMBER OF TEAMS IN REGION
                    Input #2, regionNames$(I) ' NAME OF REGION
                Next
                For I = 1 To 16
                    For I1 = 1 To 16
                        For I2 = 0 To 3
                            Input #2, slotName$(I, I1, I2) ' SEED SLOT NAME
                            Input #2, tourneyN%(I, I1, I2) ' REGION, SEED NUMBER, MODE PLAY/TM#
                        Next
                    Next
                Next

                Input #2, consolationSetting%(0)
                Input #2, consolationSetting%(1): ' CONSOLATION GAME
                Close 2

                Open diskPaths$(1) + targetFile$ + ".TNY" For Input As #1
                For I = 0 To 14
                    Input #1, tourneyDefaults%(I)
                Next
                Close 1

                If consolationSetting%(0) = 1 Then C$ = "Y" Else C$ = "N"
                NR$ = Str$(R%)

                Open diskPaths$(1) + targetFile$ + ".TOU" For Random As #1 Len = 90
                fileLength& = LOF(1)
                Close 1

                scheduleAP%(0) = fileLength& / 90
                BS% = Int((scheduleAP%(0) - 1) / 20)

                For I = 1 To scheduleAP%(0)
                    Open diskPaths$(1) + targetFile$ + ".TOU" For Random As #1 Len = 90

                    For X = 0 To 14
                        Field #1, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
                    Next

                    Field #1, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
                    Field #1, 64 As X$, 3 As Q$(20), 3 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
                    Field #1, 84 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)
                    Get #1, I

                    For X = 0 To 14: scheduleNG%(I, X) = CVI(Q$(X + 1)): Next
                    For X = 0 To 3: yearNumber$(I, X) = Q$(20 + X): Next

                    visitorScores(I) = CVI(Q$(16))
                    homeScores(I) = CVI(Q$(17))
                    visitingTeam$(I) = Q$(18)
                    homeTeam$(I) = Q$(19)

                    For X = 1 To 3: scheduleNG%(I, X + 14) = CVI(Q$(X + 23)): Next
                    For X = 2 To 3
                        While Right$(yearNumber$(I, X), 1) = " "
                            yearNumber$(I, X) = Left$(yearNumber$(I, X), Len(yearNumber$(I, X)) - 1)
                        Wend
                    Next

                    Close 1
                Next

                Call EditBracket(R%, ACTION%)

            End If 'Done checking for ESC

        End If 'Done checking for no files

    Loop Until Count% = 0 Or ExitCode% = 27 Or UCase$(I$) = "N"

End Sub

'------------------------------
'  PrintFourTeamBracket Subroutine
'------------------------------
' Uses special characters to print
' a semi-bracket on the screen
Sub PrintFourTeamBracket ()

    Color 15
    For I = 0 To 3
        If I / 2 = Int(I / 2) Then Locate 6 * I + 5, 55: Print Chr$(179)
        If I / 2 = Int(I / 2) Then Locate 6 * I + 6, 55: Print Chr$(179)
        If I / 2 = Int(I / 2) Then Locate 6 * I + 7, 55: Print Chr$(195)
        If I / 2 = Int(I / 2) Then Locate 6 * I + 8, 55: Print Chr$(179)
        If I / 2 = Int(I / 2) Then Locate 6 * I + 9, 55: Print Chr$(179)
        If I / 2 = Int(I / 2) Then Locate 6 * I + 4, 38: Print String$(17, Chr$(196)); Chr$(191)
        If I / 2 <> Int(I / 2) Then Locate 6 * I + 4, 38: Print String$(17, Chr$(196)); Chr$(217)
    Next
    For I = 0 To 1
        If I = 0 Then Locate 7, 56: Print String$(13, Chr$(196)); Chr$(191)
        For I1 = 7 To 17: Locate I1 + 1, 69: Print Chr$(179): Next
        Locate 13, 69: Print Chr$(195); String$(9, Chr$(196))
        If I = 1 Then Locate 19, 56: Print String$(13, Chr$(196)); Chr$(217)
    Next
    Color 7

End Sub

'------------------------------
'      TeamColors Subroutine
'------------------------------
' ...explanation...
Sub TeamColors (currentTeam$, currentScore%)

    'teamName$(teamIndex), teamScore%(teamIndex)
    'need to Dim these in Hello.BAS
    Shared slotName$(), tourneyN%(), scheduleNG%()

    Dim found

    found = 0

    If Left$(currentTeam$, 3) = "REG" Then
        Print
    Else

        X$ = _Trim$(currentTeam$)

        For X = 1 To 16
            For X1 = 1 To 16
                If X$ = slotName$(X, X1, 0) Then
                    Color tourneyN%(X, X1, 2), tourneyN%(X, X1, 3)
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

'------------------------------
'    EditBracket Subroutine
'------------------------------
'...explanation...
Sub EditBracket (numberRegions, ACTION%)

    Dim bracketA$(9)

    Shared tourneyN%(), scheduleNG%(), NN%()
    Shared scheduleAP%()
    Shared consolationSetting%(), tourneyDefaults%()
    Shared regionNames$(), slotName$()
    Shared homeScores(), visitorScores()
    Shared homeTeam$(), visitingTeam$()

    Do
        35 'EDIT A GIVEN BRACKET
        Color , 0

        Cls

        Color , 6

        Locate 1, 1: Print Tab(80);

        Locate , 23
        Color 15
        Print "TOURNAMENT  - REGION/BRACKET EDITOR"
        Locate 25, 1: Print Tab(80);
        Locate , 1: Color , 0

        ReDim menuChoices$(1 To numberRegions + 2)

        'Erase menuChoices$
        For I = 1 To numberRegions
            If regionNames$(I) = "" Then regionNames$(I) = "TO BE NAMED"
            menuChoices$(I) = regionNames$(I)
        Next

        Selection% = 1
        Count% = numberRegions + 2
        Start% = 1
        ExitCode% = numberRegions + 2
        FileColr% = 15
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 4
        Column% = 32
        menuChoices$(numberRegions + 1) = "CHAMPIONSHIP BRACKET"
        menuChoices$(numberRegions + 2) = "EXIT                "

        Call box0(3, 31, 3 + numberRegions + 3, 52, 2, 15, 1)
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            NR = Selection%
            If NR = numberRegions + 1 Then NR = 17
            If NR = numberRegions + 2 Then Exit Sub

            If ACTION% = 2 Then
                'View Seedings
                Color , 0
                Cls

                Color , 6
                Locate 1, 1: Print Tab(80);
                Locate , 23
                Color 15
                Print "TOURNAMENT  - REGION/BRACKET EDITOR"
                Locate 25, 1: Print Tab(80);
                Locate , 1: Color , 0

                MAXSEED = NN%(NR) + 1 + 4
                Call box0(3, 24, MAXSEED, 54, 2, 15, 1)

                Locate 4, 25: Print "SEED  TEAM"

                For I1 = 1 To NN%(NR)
                    Color 2
                    Locate , 25
                    Print Using "##    "; I1;
                    Color 15: Print slotName$(NR, I1, 0)
                Next I1

                I$ = GetKeyPress$

            Else
                'ACTION% = 1
                'View Brackets
                N = NR

                Color , 0

                Cls

                R$ = regionNames$(N)
                Ycoord = Int((80 - Len(R$)) / 2)
                Locate 1, Ycoord: Print R$
                Locate 2

                If N = 17 Then
                    caseNN = 1
                Else
                    caseNN = NN%(N) - 3
                End If

                Select Case caseNN

                    Case 1: '4 Teams
                        Call PrintFourTeamBracket

                        REG = N
                        GAME = 1

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 3, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N
                        GAME = 2

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N
                        GAME = 3 + consolationSetting%(0)

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 6, 56: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 18, 56: Call TeamColors(homeTeam$(I), homeScores(I))

                    Case 2: '3 Teams
                        ' 5 TEAM BRACKET
                        For I = 0 To 1
                            If I / 2 = Int(I / 2) Then Locate 3 * I + 2, 22: Print bracketA$(I + 1)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 1, 22: Print bracketA$(I + 1)
                            If I / 2 = Int(I / 2) Then Locate 3 * I + 3, 21: Print String$(16, Chr$(196)); Chr$(191)
                            If I / 2 = Int(I / 2) Then Locate 3 * I + 4, 37: Print Chr$(195)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 2, 21: Print String$(16, Chr$(196)); Chr$(217)
                        Next

                        Call PrintFourTeamBracket

                        REG = N: GAME = 1

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 2, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 4, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 2

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 3, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 3

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 4 + consolationSetting%(0)

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 6, 56: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 18, 56: Call TeamColors(homeTeam$(I), homeScores(I))

                    Case 3: '5 Teams
                        ' 6 TEAM BRACKET
                        For I = 0 To 7
                            If I / 2 = Int(I / 2) Then Locate 3 * I + 2, 22: Print bracketA$(I + 1)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 1, 22: Print bracketA$(I + 1)
                            If I / 2 = Int(I / 2) And (I <> 4 And I <> 2) Then Locate 3 * I + 3, 21: Print String$(16, Chr$(196)); Chr$(191)
                            If I / 2 = Int(I / 2) And (I <> 4 And I <> 2) Then Locate 3 * I + 4, 37: Print Chr$(195)
                            If I / 2 <> Int(I / 2) And (I <> 5 And I <> 3) Then Locate 3 * I + 2, 21: Print String$(16, Chr$(196)); Chr$(217)
                        Next

                        Call PrintFourTeamBracket

                        REG = N: GAME = 1

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 2, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 4, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 2

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 20, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 22, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 3

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 3, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 4

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 5 + consolationSetting%(0)

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 6, 56: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 18, 56: Call TeamColors(homeTeam$(I), homeScores(I))

                    Case 4: '6 Teams
                        ' 7 TEAM BRACKET
                        For I = 0 To 7
                            If I / 2 = Int(I / 2) Then Locate 3 * I + 2, 22: Print bracketA$(I + 1)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 1, 22: Print bracketA$(I + 1)
                            If I / 2 = Int(I / 2) And I <> 4 Then Locate 3 * I + 3, 21: Print String$(16, Chr$(196)); Chr$(191)
                            If I / 2 = Int(I / 2) And I <> 4 Then Locate 3 * I + 4, 37: Print Chr$(195)
                            If I / 2 <> Int(I / 2) And I <> 5 Then Locate 3 * I + 2, 21: Print String$(16, Chr$(196)); Chr$(217)
                        Next

                        Call PrintFourTeamBracket

                        REG = N: GAME = 1

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 2, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 4, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 2

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 8, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 10, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 3

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 20, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 22, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 4

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 3, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 5

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 6 + consolationSetting%(0)

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 6, 56: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 18, 56: Call TeamColors(homeTeam$(I), homeScores(I))

                    Case 5: '7 Teams
                        ' 8 TEAM BRACKET
                        For I = 0 To 7
                            If I / 2 = Int(I / 2) Then Locate 3 * I + 2, 22: Print bracketA$(I + 1)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 1, 22: Print bracketA$(I + 1)
                            If I / 2 = Int(I / 2) Then Locate 3 * I + 3, 21: Print String$(16, Chr$(196)); Chr$(191)
                            If I / 2 = Int(I / 2) Then Locate 3 * I + 4, 37: Print Chr$(195)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 2, 21: Print String$(16, Chr$(196)); Chr$(217)
                        Next

                        Call PrintFourTeamBracket

                        REG = N: GAME = 1

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 2, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 4, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 2
                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 8, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 10, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 3
                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 14, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 16, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 4
                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 20, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 22, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 5
                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 3, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 6
                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 7 + consolationSetting%(0)
                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next
                        Locate 6, 56: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 18, 56: Call TeamColors(homeTeam$(I), homeScores(I))

                    Case 6: '8 Teams
                        '9 TEAM BRACKET
                        Locate 22, 1

                        REG = N: GAME = 1
                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        For I = 0 To 7
                            If I / 2 = Int(I / 2) Then Locate 3 * I + 2, 22: Print bracketA$(I + 1)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 1, 22: Print bracketA$(I + 1)
                            If I / 2 = Int(I / 2) Then Locate 3 * I + 3, 21: Print String$(16, Chr$(196)); Chr$(191)
                            If I / 2 = Int(I / 2) Then Locate 3 * I + 4, 37: Print Chr$(195)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 2, 21: Print String$(16, Chr$(196)); Chr$(217)
                        Next

                        Call PrintFourTeamBracket

                        REG = N: GAME = 2

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 2, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 4, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 3

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 8, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 10, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 4

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 14, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 16, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 5

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 20, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 22, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 6

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 3, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 7

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 8 + consolationSetting%(0)

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 6, 56: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 18, 56: Call TeamColors(homeTeam$(I), homeScores(I))

                    Case 7: '10 Teams
                        '10 TEAM BRACKET

                        REG = N: GAME = 1

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
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

                        REG = N: GAME = 2

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        For I = 0 To 7
                            If I / 2 = Int(I / 2) Then Locate 3 * I + 2, 22: Print bracketA$(I + 1)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 1, 22: Print bracketA$(I + 1)
                            If I / 2 = Int(I / 2) Then Locate 3 * I + 3, 21: Print String$(16, Chr$(196)); Chr$(191)
                            If I / 2 = Int(I / 2) Then Locate 3 * I + 4, 37: Print Chr$(195)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 2, 21: Print String$(16, Chr$(196)); Chr$(217)
                        Next

                        Call PrintFourTeamBracket

                        REG = N: GAME = 3

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 2, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 4, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 4

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 8, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 10, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 5

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 14, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 16, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 6

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 20, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 22, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 7

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 3, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 8

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 9 + consolationSetting%(0)

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 6, 56: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 18, 56: Call TeamColors(homeTeam$(I), homeScores(I))

                    Case 8: '11 Teams
                        '11 TEAM BRACKET

                        REG = N: GAME = 1

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print
                        Print
                        Print
                        Print

                        REG = N: GAME = 2
                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
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

                        REG = N: GAME = 3

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        For I = 0 To 7
                            If I / 2 = Int(I / 2) Then Locate 3 * I + 2, 22: Print bracketA$(I + 1)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 1, 22: Print bracketA$(I + 1)
                            If I / 2 = Int(I / 2) Then Locate 3 * I + 3, 21: Print String$(16, Chr$(196)); Chr$(191)
                            If I / 2 = Int(I / 2) Then Locate 3 * I + 4, 37: Print Chr$(195)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 2, 21: Print String$(16, Chr$(196)); Chr$(217)
                        Next

                        Call PrintFourTeamBracket

                        REG = N: GAME = 4

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 2, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 4, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 5

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 8, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 10, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 6

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 14, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 16, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 7

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 20, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 22, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 8

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 3, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 9

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 10 + consolationSetting%(0)

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 6, 56: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 18, 56: Call TeamColors(homeTeam$(I), homeScores(I))

                    Case 9: '12 Teams
                        '12 TEAM BRACKET

                        REG = N: GAME = 1

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print
                        Print
                        Print
                        Print

                        REG = N: GAME = 2

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print
                        Print
                        Print
                        Print

                        REG = N: GAME = 3

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print
                        Print
                        Print
                        Print

                        REG = N: GAME = 4

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print
                        Print
                        Print

                        Color 15

                        For I = 0 To 7
                            If I / 2 = Int(I / 2) Then Locate 3 * I + 2, 22: Print bracketA$(I + 1)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 1, 22: Print bracketA$(I + 1)
                            If I / 2 = Int(I / 2) Then Locate 3 * I + 3, 21: Print String$(16, Chr$(196)); Chr$(191)
                            If I / 2 = Int(I / 2) Then Locate 3 * I + 4, 37: Print Chr$(195)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 2, 21: Print String$(16, Chr$(196)); Chr$(217)
                        Next

                        Color 7

                        Call PrintFourTeamBracket

                        REG = N: GAME = 5

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 2, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 4, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 6

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 8, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 10, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 7

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 14, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 16, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 8

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 20, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 22, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 9

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 3, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 10

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 11 + consolationSetting%(0)

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 6, 56: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 18, 56: Call TeamColors(homeTeam$(I), homeScores(I))
                        Locate 12, 64

                    Case 10: '13 Teams
                        '13 TEAM BRACKET

                        REG = N: GAME = 1

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print

                        REG = N: GAME = 2

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print

                        REG = N: GAME = 3

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print
                        Print
                        Print
                        Print

                        REG = N: GAME = 4

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print
                        Print
                        Print
                        Print

                        REG = N: GAME = 5

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print
                        Print
                        Print

                        For I = 0 To 7
                            If I / 2 = Int(I / 2) Then Locate 3 * I + 2, 22: Print bracketA$(I + 1)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 1, 22: Print bracketA$(I + 1)
                            If I / 2 = Int(I / 2) Then Locate 3 * I + 3, 21: Print String$(16, Chr$(196)); Chr$(191)
                            If I / 2 = Int(I / 2) Then Locate 3 * I + 4, 37: Print Chr$(195)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 2, 21: Print String$(16, Chr$(196)); Chr$(217)
                        Next

                        Call PrintFourTeamBracket

                        REG = N: GAME = 6

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 2, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 4, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 7

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 8, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 10, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 8

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 14, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 16, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 9

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 20, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 22, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 10

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 3, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 11

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 12 + consolationSetting%(0)

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 6, 56: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 18, 56: Call TeamColors(homeTeam$(I), homeScores(I))

                    Case 11: '14 Teams
                        '14 TEAM BRACKET

                        REG = N: GAME = 1

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print

                        REG = N: GAME = 4

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print

                        REG = N: GAME = 5

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print
                        Print
                        Print
                        Print

                        REG = N: GAME = 2

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print

                        REG = N: GAME = 3

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print

                        REG = N: GAME = 6

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        For I = 0 To 7
                            If I / 2 = Int(I / 2) Then Locate 3 * I + 2, 22: Print bracketA$(I + 1)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 1, 22: Print bracketA$(I + 1)
                            If I / 2 = Int(I / 2) Then Locate 3 * I + 3, 21: Print String$(16, Chr$(196)); Chr$(191)
                            If I / 2 = Int(I / 2) Then Locate 3 * I + 4, 37: Print Chr$(195)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 2, 21: Print String$(16, Chr$(196)); Chr$(217)
                        Next

                        Call PrintFourTeamBracket

                        REG = N: GAME = 7

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 2, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 4, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 8

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 8, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 10, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 9

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 14, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 16, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 10

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 20, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 22, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 11

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 3, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 12

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 13 + consolationSetting%(0)

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 6, 56: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 18, 56: Call TeamColors(homeTeam$(I), homeScores(I))

                    Case 12: '15 Teams
                        '15 TEAM BRACKET

                        REG = N: GAME = 1
                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print

                        REG = N: GAME = 6

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print

                        REG = N: GAME = 2

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print

                        REG = N: GAME = 5
                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print

                        REG = N: GAME = 3

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print

                        REG = N: GAME = 4

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print

                        REG = N: GAME = 7

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        For I = 0 To 7
                            If I / 2 = Int(I / 2) Then Locate 3 * I + 2, 22: Print bracketA$(I + 1)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 1, 22: Print bracketA$(I + 1)
                            If I / 2 = Int(I / 2) Then Locate 3 * I + 3, 21: Print String$(16, Chr$(196)); Chr$(191)
                            If I / 2 = Int(I / 2) Then Locate 3 * I + 4, 37: Print Chr$(195)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 2, 21: Print String$(16, Chr$(196)); Chr$(217)
                        Next

                        Call PrintFourTeamBracket

                        REG = N: GAME = 8

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 2, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 4, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 9

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 8, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 10, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 10
                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 14, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 16, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 11

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 20, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 22, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 12

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 3, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 13

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 14 + consolationSetting%(0)

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 6, 56: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 18, 56: Call TeamColors(homeTeam$(I), homeScores(I))

                    Case 13: '16 Teams
                        '16 TEAM BRACKET
                        REG = N: GAME = 1

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print

                        REG = N: GAME = 8

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print

                        REG = N: GAME = 4

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print

                        REG = N: GAME = 5

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print

                        REG = N: GAME = 3

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print

                        REG = N: GAME = 6

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print

                        REG = N: GAME = 2

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Print

                        REG = N: GAME = 7

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Call TeamColors(homeTeam$(I), homeScores(I))

                        Color 15

                        For I = 0 To 7
                            If I / 2 = Int(I / 2) Then Locate 3 * I + 2, 22: Print bracketA$(I + 1)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 1, 22: Print bracketA$(I + 1)
                            If I / 2 = Int(I / 2) Then Locate 3 * I + 3, 21: Print String$(16, Chr$(196)); Chr$(191)
                            If I / 2 = Int(I / 2) Then Locate 3 * I + 4, 37: Print Chr$(195)
                            If I / 2 <> Int(I / 2) Then Locate 3 * I + 2, 21: Print String$(16, Chr$(196)); Chr$(217)
                        Next

                        Color 7

                        Call PrintFourTeamBracket

                        REG = N: GAME = 9

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 2, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 4, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 10

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 8, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 10, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 11

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 14, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 16, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 12

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 20, 21: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 22, 21: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 13

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 3, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 9, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 14

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
                        Next

                        Locate 15, 39: Call TeamColors(visitingTeam$(I), visitorScores(I))
                        Locate 21, 39: Call TeamColors(homeTeam$(I), homeScores(I))

                        REG = N: GAME = 15 + consolationSetting%(0)

                        For I = 1 To scheduleAP%(0)
                            If scheduleNG%(I, 15) = N And scheduleNG%(I, 17) = GAME Then Exit For
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
                                Color tourneyN%(X, X1, 2), tourneyN%(X, X1, 3)
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
    Loop Until ExitCode% = 27 Or Selection% = numberRegions + 2 'ACTION% = 2

End Sub


'------------------------------
'  BracketsNotUsed Subroutine
'------------------------------
' This code was in the original
' but is not referenced.
' It appears some thought was
' given to different bracets.
Sub BracketsNotUsed ()

    Shared scheduleAP%()

    55 '3 TEAM BRACKET
    For I = 0 To 1
        If I / 2 = Int(I / 2) Then Locate 6 * I + 5, 47: Print Chr$(179)
        If I / 2 = Int(I / 2) Then Locate 6 * I + 6, 47: Print Chr$(179)
        If I / 2 = Int(I / 2) Then Locate 6 * I + 7, 47: Print Chr$(195)
        If I / 2 = Int(I / 2) Then Locate 6 * I + 8, 47: Print Chr$(179)
        If I / 2 = Int(I / 2) Then Locate 6 * I + 9, 47: Print Chr$(179)

        If I / 2 = Int(I / 2) Then Locate 6 * I + 4, 32: Print String$(15, Chr$(196)); Chr$(191)
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
    60 'TWO TEAM BRACKET
    For I = 0 To 1
        If I = 0 Then Locate 7, 48: Print String$(15, Chr$(196)); Chr$(191)
        For I1 = 7 To 17: Locate I1 + 1, 63: Print Chr$(179): Next
        Locate 13, 63: Print Chr$(195); String$(15, Chr$(196))
        If I = 1 Then Locate 19, 48: Print String$(15, Chr$(196)); Chr$(217)
        Locate 6, 48: Print visitingTeam$(4); Str$(visitorScores(4))
        Locate 18, 48: Print homeTeam$(4); Str$(homeScores(4))
    Next

End Sub
