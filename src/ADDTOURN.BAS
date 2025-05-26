'----------------------------------------
'           ADDTOURN Subroutine
'----------------------------------------
'This subroutine provides the main menu
'for users to interface with the various
'asepcts of tournament handling.
Sub ADDTOURN ()

    _Title "Courtside College Basketball - Create Tournament"

    Shared regionNames$(), slotName$()

    Shared consolationSetting(), nbrRegionalTeams()
    Shared tourneyDefaults(), tourneyParams()

    Dim NR$

    ReDim menuChoices$(1 To 6)

    tourneyDefaults(2) = 2 'Neutral Site
    tourneyDefaults(3) = 1 '3-Pt Opt
    tourneyDefaults(5) = 5 'Fouls to DQ
    tourneyDefaults(6) = 1 'FTs on 10th PF
    tourneyDefaults(7) = 3 'Shot Clock
    tourneyDefaults(8) = 3 'Play Mode (PC vs PC)
    tourneyDefaults(9) = 0 'Human Coaches
    tourneyDefaults(10) = 1 '3FT's on 3FGA
    tourneyDefaults(11) = 0 'Play-in Mode
    tourneyDefaults(14) = 0 'FT on 1st 6 Floor PF

    Do
        Color , 0

        Cls

        Call TITLE_TOURN

        '--------------------
        '   MENU OPTIONS
        '--------------------
        menuChoices$(1) = "CREATE TOURNAMENT SEEDS      "
        menuChoices$(2) = "EDIT EXISTING SEEDS          "
        menuChoices$(3) = "VIEW BRACKETS / SEEDS        "
        menuChoices$(4) = "PLAY TOURNAMENT GAMES        "
        menuChoices$(5) = "RESET A TOURNAMENT           "
        menuChoices$(6) = "RETURN TO COURTSIDE MAIN MENU"
        SELECTION% = 1
        Count% = 6
        Start% = 1
        ExitCode% = 6
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

                Case 1:
                    newTourneyName$ = ""
                    NR$ = ""
                    tourneyFile$ = ""
                    Erase regionNames$
                    Erase slotName$
                    'Call SetupTourney(SELECTION%, NR$, newTourneyName$, tourneyFile$)
                    Call SetupTourney(SELECTION%, NR%, newTourneyName$, tourneyFile$)

                Case 2, 5:
                    Erase nbrRegionalTeams, tourneyParams
                    Erase regionNames$, slotName$
                    Erase consolationSetting

                    Call LoadTourney(NR%, SELECTION%)

                Case 3:
                    Call BRACKET

                Case 4:
                    Call TOURNEY

            End Select

        End If

    Loop Until SELECTION% = 6 Or ExitCode% = 27

End Sub


'----------------------------------------
'        SetupTourney Subroutine
'----------------------------------------
'This subroutine presents the user the
'general options for the tournament being
'setup. It then steps the user through
'other screens for tournament setup - which
'all rely on separate routine.
Sub SetupTourney (ACTION%, nbrRegions, tourneyName$, sourceFile$)

    Dim pbpDelayTourn!

    Shared coachMode$(), foulShotOpt$()
    Shared playMode$()
    Shared sClockOpt$(), yesNoText$()

    Shared consolationSetting()
    Shared tourneyDefaults()

    If consolationSetting(0) = 1 Then
        consolation$ = "Y"
    Else
        consolation$ = "N"
    End If

    'GLOBAL SETUP TOURNEY DEFAULTS
    Cls

    Color , 4
    Locate 1, 1
    Print Tab(80); " ";

    Color 0
    Locate , 23: Print "SELECT GENERAL TOURNAMENT OPTIONS"

    Locate LAST_ROW, 1
    Print Tab(80); " ";

    Locate , 1
    Print "    LANCE HAFFNER/SHANNON LYNN/JASON LEONARD 1987-2025";

    Locate , 1
    Color , 0

    Do

        Locate 4, 2: Color 14
        Print " (1) ";
        Color 15: Print playMode$(tourneyDefaults(8))
        Locate 5, 2: Color 14
        Print " (2) ";
        Color 15: Print "SHOT CLOCK: "; sClockOpt$(tourneyDefaults(7))
        Locate 6, 2: Color 14
        Print " (3) ";
        Color 15: Print "3-POINT SHOT: "; yesNoText$(tourneyDefaults(3))
        Locate 7, 2: Color 14
        Print " (4) ";
        Color 15: Print "FOULS TO DISQUALIFY: "; tourneyDefaults(5)
        Locate 8, 2: Color 14
        Print " (5) ";
        Color 15: Print "FREE THROWS ON 10TH FOUL: "; foulShotOpt$(tourneyDefaults(6))
        Locate 9, 2: Color 14
        Print " (6) ";
        Color 15: Print Using "PLAY-BY-PLAY DELAY (SECS): ##.#  "; tourneyDefaults(4) / 10
        Locate 10, 2: Color 14
        Print " (7) ";
        Color 15: Print "3-FT WHEN FOULED ON 3-PT ATTEMPT: "; yesNoText$(tourneyDefaults(10))
        Locate 11, 2: Color 14
        Print " (8) ";
        Color 15: Print "1 FT ON 1ST 6 FLOOR FOULS (PRIOR TO 1973): "; yesNoText$(tourneyDefaults(14))
        Locate 12, 2: Color 14
        Print " (9) ";
        Color 15: Print "HUMAN COACHES: "; coachMode$(tourneyDefaults(9))
        Locate 13, 2: Color 14
        Print " (0) ";
        Color 15: Print "ACCEPT SETTINGS"
        Locate 14, 2: Color 14
        Print " (A) ";
        Color 15: Print "PLAY-IN GAMES: "; yesNoText$(tourneyDefaults(11))

        Locate 16, 2: Color 14
        Print "NOTE:: "
        Color 15

        Locate , 3: Print "* NO SHOT CLOCK BEFORE 1984-1985; 45 SEC CLOCK ADOPTED (NOT FOR TOURNAMENT)"
        Locate , 3: Print "* 35 SEC SHOT CLOCK IN 1993-94; 30 SEC SHOT CLOCK IN 2015-16"
        Locate , 3: Print "* 3 PT SHOT ADOPTED IN 1986-87"
        Locate , 3: Print "* NCAA ALLOWS 5 FOULS BEFORE A PLAYER IS DQ'D; SOME CONF'S TESTED 6"
        Locate , 3: Print "* 10TH FOUL ALLOWED 1+1 FT BEFORE 90-91; NOW IT IS 2 FT"
        Locate , 3: Print "* 3 FT AWARDED WHEN FOULED ON A 3 FGA STARTING 90-91, "
        Locate , 3: Print "* 1 FT ON 1ST 6 FLOOR FOULS THROUGH 1972-73 SEASON"

        Do
            I$ = GetKeyPress$

            If I$ <> Chr$(27) Then
                Call LetterToNumber(I$, setting)
            End If

        Loop Until setting >= 0 Or setting <= 10

        If I$ = Chr$(27) Then Exit Sub

        Select Case setting

            Case 1:
                tourneyDefaults(8) = tourneyDefaults(8) + 1
                If tourneyDefaults(8) > 3 Then tourneyDefaults(8) = 3

            Case 2:
                tourneyDefaults(7) = tourneyDefaults(7) + 1
                If tourneyDefaults(7) > 3 Then tourneyDefaults(7) = 0

            Case 3:
                tourneyDefaults(3) = 1 - tourneyDefaults(3)

            Case 4:
                tourneyDefaults(5) = tourneyDefaults(5) + 1
                If tourneyDefaults(5) > 6 Then tourneyDefaults(5) = 5

            Case 5:
                tourneyDefaults(6) = 1 - tourneyDefaults(6)

            Case 6:
                Locate 9, 32
                Input pbpDelayTourn!
                tourneyDefaults(4) = pbpDelayTourn! * 10

            Case 7:
                tourneyDefaults(10) = 1 - tourneyDefaults(10)

            Case 8: '
                tourneyDefaults(14) = 1 - tourneyDefaults(14)

            Case 9: '
                tourneyDefaults(9) = 1 - tourneyDefaults(9)

            Case 0: '

            Case 10:
                tourneyDefaults(11) = 1 - tourneyDefaults(11)

        End Select

    Loop Until setting = 0

    Color 15, 1
    FG = 0
    BG = 7

    GetData:
    Cls
    Color , 6
    Locate 1, 1: Print Tab(80); " ";
    Color 15
    Locate , 22: Print "TOURNAMENT  - GENERAL INFORMATION"

    Locate LAST_ROW, 1: Print Tab(80); " ";
    Locate , 1: Color , 0

    Locate 7, 1: QPrint "FILE NAME (48 CHARACTERS) ", 14, 1, -1
    Locate , 27: QPrint PadRight$(sourceFile$, 48), FG, BG, -1

    Locate 9, 1: QPrint "NAME OF TOURNAMENT ", 14, 1, -1
    Locate , 20: QPrint PadRight$(tourneyName$, 55), FG, BG, -1

    Locate 11, 1: QPrint "CONSOLATION GAMES IN REGIONALS/FINALS ", 14, 1, -1
    Locate , 40: QPrint PadRight$(consolation$, 1), FG, BG, -1

    Locate 13, 1: QPrint "NUMBER OF BRACKETS/REGIONS (1-16) ", 14, 1, -1
    Locate , 35: QPrint PadRight$(Str$(nbrRegions), 2), FG, BG, -1

    GetDiv1:
    Locate 7, 26 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 32 'specify field parameters
    Call TextIn(sourceFile$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data
    sourceFile$ = _Trim$(sourceFile$)

    GetDiv2:
    Locate 9, 20 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 40 'specify field parameters
    Call TextIn(tourneyName$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data
    tourneyName$ = _Trim$(tourneyName$)

    GetDiv3:
    Locate 11, 40 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 1 'specify field parameters
    Call TextIn(consolation$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv2: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data
    consolation$ = UCase$(_Trim$(consolation$))

    GetDiv4:
    Locate 13, 35 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 2 'specify field parameters
    regionCount# = nbrRegions
    Call NumIn(regionCount#, Max, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv3: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data

    Done:
    Color 14, 1
    Locate LAST_ROW - 2, 1
    Print Space$(35)
    Print "Is this okay? (Yes/No) ";
    X$ = ""

    Call YesNoB(X$, XCode, FG, BG)
    Locate , 1
    ClearEOL 14, 1

    If XCode = 1 GoTo GetDiv4

    If Not (XCode = 2) Then

        If X$ = "N" GoTo GetDiv1
        If X$ <> "Y" GoTo Done 'insist on either Y or N or up-arrow

        If UCase$(consolation$) = "Y" Then consolationSetting(0) = 1 Else consolationSetting(0) = 0

        consolationSetting(1) = consolationSetting(0)

        If sourceFile$ = "" Then

            MsgText$ = "TOURNAMENT FILE NAME IS BLANK PRESS ANY KEY TO CONTINUE SELECTING"
            BoxWidth = 18
            BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
            Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
            Call CSMsgBox(MsgText$, BoxWidth, PASS)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, PASS)

            GoTo GetData

        Else

            nbrRegions = Int(regionCount#)

            If nbrRegions < 1 Or nbrRegions > 16 Then GoTo GetDiv4

            '.TNY file = tournament settings
            Open diskPaths$(1) + sourceFile$ + ".TNY" For Output As #1

            For I = 0 To 14
                Print #1, tourneyDefaults(I)
            Next

            Close #1

            If ACTION% = 2 Then

                Call EditRegionSeeds(nbrRegions, nbrTeams, sourceFile$, tourneyName$)

            Else

                If tourneyDefaults(11) = 1 Then
                    'Play-in games are handled as "region 0"
                    startRegion = 0
                Else
                    startRegion = 1
                End If

                For currRegion = startRegion To nbrRegions
                    Call SeedBrackets(currRegion, nbrRegions, nbrTeams, sourceFile$, tourneyName$)
                Next

                Call ScheduleNextRoundSeeds(nbrRegions, tourneyDefaults(11), sourceFile$, tourneyName$)

                Call CHAMPIONSHIP(nbrRegions, sourceFile$, tourneyName$)

                Call SaveTourneyInfo(nbrRegions, sourceFile$, tourneyName$)

            End If 'Done checking for Edit Bracket

        End If 'Done checking sourceFile

    End If 'Done checking for ESC

End Sub


'----------------------------------------
'        SeedBrackets Subroutine
'----------------------------------------
'This subroutine lets the user review and
'make changes to the Tournament Regions
'and their corresponding seedings and
'brackets.
Sub SeedBrackets (currRegion, nbrRegions, teamCount, sourceFile$, tourneyName$)

    Shared regionNames$(), slotName$()

    Shared nbrRegionalTeams()
    Shared teamRatings()
    Shared tourneyParams()

    Cls
    Color 15, 1
    FG = 0
    BG = 7

    Cls
    Color , 6
    Locate 1, 1: Print Tab(80); " ";
    Color 15

    Locate , 26: Print "REGIONAL/BRACKET INFORMATION"

    Locate LAST_ROW, 1: Print Tab(80); " ";

    Color , 0
    Locate , 1
    X$ = "NAME OF REGION" + Str$(currRegion) + " "

    If currRegion = 0 Then
        regionNames$(0) = "PLAY-IN TEAMS"
    End If

    Locate 7, 1: QPrint X$, 14, 1, -1
    Locate , 26: QPrint PadRight$(regionNames$(currRegion), 16), FG, BG, -1
    Locate 9, 1: QPrint "NUMBER OF TEAMS IN REGION (4-16) ", 14, 1, -1
    Locate , 34: QPrint PadRight$(Str$(teamCount), 2), FG, BG, -1

    NameRegion:
    If currRegion > 0 Then
        Locate 7, 26 'location of beginning of field
        CapsOn = 0: NumOnly = 0: Max = 16 'specify field parameters
        Call TextIn(regionNames$(currRegion), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
        If XCode = 1 GoTo REGIONSIZE: 'can't back up any more, stay here
        If XCode = 2 GoTo Done1 'all done entering data
        regionNames$(currRegion) = _Trim$(regionNames$(currRegion))
    End If

    REGIONSIZE:
    Locate 9, 34 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 2 'specify field parameters
    teamQty# = teamCount
    Call NumIn(teamQty#, Max, XCode, FG, BG) 'get input
    If XCode = 1 GoTo NameRegion: 'can't back up any more, stay here

    teamCount = Int(teamQty#)

    If currRegion = 0 And teamCount <> 4 And teamCount <> 8 Then
        Locate LAST_ROW - 2, 1
        Print "NOT A VALID NUMBER OF PLAY-IN TEAMS!!!";
        GoTo REGIONSIZE
    End If

    Done1:
    Color 14, 1
    Locate LAST_ROW - 2, 1
    Print Space$(40)
    Print "Is this okay? (Yes/No) ";
    X$ = ""

    Call YesNoB(X$, XCode, FG, BG)
    Locate , 1
    ClearEOL 14, 1
    If XCode = 1 GoTo REGIONSIZE

    If X$ <> Chr$(27) Then

        If X$ = "N" GoTo NameRegion
        If X$ <> "Y" GoTo Done1 'insist on either Y or N or up-arrow

        nbrRegionalTeams(currRegion) = teamCount
        If nbrRegionalTeams(currRegion) < 4 Or nbrRegionalTeams(currRegion) > 16 Then GoTo REGIONSIZE

        playInSeeds = (nbrRegionalTeams(0) / 2)
        slotsUsed = 0

        'Seed Play-in Teams
        'Currently this defaults to assigning 1 winner to each region
        'It also assigns them sequentially.
        '-- This would need changed eventually
        If currRegion > 0 And currRegion <= playInSeeds Then

            slotsUsed = slotsUsed + 1

            '-- Update here to use different slots for seeding
            '-- Currently only uses the last seed
            seedSlot = nbrRegionalTeams(currRegion) 'last seed

            slotName$(currRegion, seedSlot, 0) = "PLAY-IN GM" + Str$(currRegion) + " WINNER"
            slotName$(currRegion, seedSlot, 1) = "XXX"
            tourneyParams(currRegion, seedSlot, 0) = 1700 + currRegion
            tourneyParams(currRegion, seedSlot, 1) = 1

        End If

        Do
            Color , 0

            Cls

            Color , 6
            Locate 1, 1: Print Tab(80); " ";
            Color 15: Locate , 26

            Xcoord = Len(regionNames$(currRegion) + " REGIONAL/CONFERENCE SEEDING")
            Xcoord = (80 - Xcoord) / 2
            Locate , Xcoord
            Print regionNames$(currRegion) + " REGIONAL/CONFERENCE SEEDING"

            Locate LAST_ROW, 1: Print Tab(80); " ";
            Locate , 1
            Color 12, 0

            MAXSEED = nbrRegionalTeams(currRegion) + 1 + 4
            Call box0(3, 24, MAXSEED, 54, 2, 15, 1)

            Locate 4, 25: Print "SEED  TEAM"

            For I1 = 1 To nbrRegionalTeams(currRegion)
                Color 2
                Locate , 25
                Print Using "##    "; I1;
                Color 15: Print slotName$(currRegion, I1, 0)
            Next I1

            Locate LAST_ROW, 1
            Color 0, 6: Print "A-";
            Color 15: Print "ADD A TEAM    ";
            Color 0, 6: Print "R-";
            Color 15: Print "REMOVE A TEAM    ";
            Color 0, 6: Print "S-";
            Color 15: Print "SAVE SEEDING ";
            Color , 0: Locate 1, 1

            Do
                I$ = GetKeyPress$
                If UCase$(I$) = "A" Then I$ = "1"
                If UCase$(I$) = "R" Then I$ = "2"
                If UCase$(I$) = "S" Then I$ = "3"

                If I$ <> Chr$(27) Then

                    'Save Seeding
                    If Val(I$) = 3 Then

                        For I = 1 To nbrRegionalTeams(currRegion)

                            If slotName$(currRegion, I, 0) = "" Then

                                MsgText$ = "NOT ALL TOURNAMENT SEEDS ARE FILLED. PRESS ANY KEY TO CONTINUE SELECTING"
                                BoxWidth = 18
                                BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                                Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                                Call CSMsgBox(MsgText$, BoxWidth, PASS)
                                I$ = GetKeyPress$
                                Call CSMsgBox("", 0, PASS)

                                notFilled = 1
                                'Return
                                Exit For
                            End If

                        Next

                        If notFilled = 1 Then
                            Call EditRegionSeeds(nbrRegions, teamCount, sourceFile$, tourneyName$)
                            _Continue
                        End If

                    End If 'Done for value 3

                    If notFilled = 1 Then notFilled = 0

                End If

            Loop Until Val(I$) >= 1 And Val(I$) <= 3

            If I$ <> Chr$(27) Then

                If Val(I$) <> 3 Then

                    'Add / Remove Teams from Seeding
                    Do
                        Locate 22, 30
                        Print "SEED # (1-"; Str$(nbrRegionalTeams(currRegion)); ") ";
                        Input seed
                    Loop Until seed >= 1 And seed <= nbrRegionalTeams(currRegion)

                    If Val(I$) = 1 Then

                        teamNbr = 0
                        Call ChooseTeam(YN$, NTMS, teamNbr, team$)

                        If teamNbr <> 0 Then

                            slotName$(currRegion, seed, 0) = team$
                            slotName$(currRegion, seed, 1) = YN$
                            tourneyParams(currRegion, seed, 0) = teamNbr
                            tourneyParams(currRegion, seed, 1) = 1
                            tourneyParams(currRegion, seed, 2) = teamRatings(0)
                            tourneyParams(currRegion, seed, 3) = teamRatings(1)

                        End If

                    Else
                        'Clear a slot
                        slotName$(currRegion, seed, 0) = ""
                    End If

                End If 'Done checking for value 3

            End If 'Done checking for ESC

        Loop Until Val(I$) = 3

    End If 'Done checking for ESC

End Sub


'----------------------------------------
'      EditRegionSeeds Subroutine
'----------------------------------------
'This subroutine gives the user a menu of
'regions that were named previously. For
'any region chosen, it calls a  separate
'subroutine to assist with populating the
'seedings for the region. When done, it
'then calls a routine to create the schedule
'for the tournament and save the file.
Sub EditRegionSeeds (nbrRegions, teamCount, saveFile$, tourneyName$)

    Shared TC
    Shared regionNames$()
    Shared nbrRegionalTeams()

    ReDim menuChoices$(0 To nbrRegions + 1)

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

        For I = 0 To nbrRegions
            If I > 0 And regionNames$(I) = "" Then
                regionNames$(I) = "TO BE NAMED"
            End If
            menuChoices$(I) = regionNames$(I)
        Next

        SELECTION% = 1
        Count% = nbrRegions + 1
        Start% = 1
        ExitCode% = nbrRegions + 1
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 4
        Column% = 32

        menuChoices$(nbrRegions + 1) = "SAVE AND EXIT"

        nbrRows = 3 + nbrRegions + 2

        If regionNames$(0) <> "" Then
            'menuChoices$(0) = regionNames$(0)
            SELECTION% = 0
            Count% = Count% + 1
            Start% = 0
            nbrRows = nbrRows + 1
        End If

        Call box0(3, 31, nbrRows, 48, 2, 15, 1)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 And ExitCode% <> 0 Then

            'Save and Exit
            If SELECTION% < nbrRegions + 1 Then
                teamCount = nbrRegionalTeams(SELECTION%)
                Call SeedBrackets(SELECTION%, nbrRegions, teamCount, saveFile$, tourneyName$)
            End If

        End If

    Loop Until SELECTION% = nbrRegions + 1 Or ExitCode% = 27

    If ExitCode% <> 27 Then
        Call ScheduleNextRoundSeeds(nbrRegions, 0, saveFile$, tourneyName$)
        Call CHAMPIONSHIP(nbrRegions, saveFile$, tourneyName$)
        Call SaveTourneyInfo(nbrRegions, saveFile$, tourneyName$)
    End If

End Sub


'----------------------------------------
'   ScheduleNextRoundSeeds Subroutine
'----------------------------------------
'This subroutine uses the data loaded so
'far for the tournament, then calls routines
'to generate the appropriate schedule.
Sub ScheduleNextRoundSeeds (nbrRegions, playIn, saveFile$, tourneyName$)

    Shared nbrRegionalTeams()

    'Delete existing files containing seedings
    If _FileExists(diskPaths$(1) + saveFile$ + ".TMP") Then Kill diskPaths$(1) + saveFile$ + ".TMP"
    If _FileExists(diskPaths$(1) + saveFile$ + ".TCX") Then Kill diskPaths$(1) + saveFile$ + ".TCX"
    If _FileExists(diskPaths$(1) + saveFile$ + ".TOU") Then Kill diskPaths$(1) + saveFile$ + ".TOU"

    For currRegion = 0 To nbrRegions

        If nbrRegionalTeams(currRegion) >= 4 Then
            nxtRndGames = nbrRegionalTeams(currRegion) - 3
            Call ScheduleWinners(nxtRndGames, currRegion, playIn, saveFile$)
        End If

    Next

End Sub


'----------------------------------------
'         LoadTourney Subroutine
'----------------------------------------
'This file opens a previously saved set of
'tournament files and reads in the data.
'It then proceeds to call the routine for
'editing tournament files.
Sub LoadTourney (nbrRegions, ACTION%)

    Shared regionNames$(), slotName$()

    Shared consolationSetting(), nbrRegionalTeams()
    Shared tourneyParams(), tourneyDefaults()

    tourneyFile$ = _OpenFileDialog$("Choose Tournament file", diskPaths$(1), "*.TOU", "Tournament files [.TOU]")
    'ChooseFile$(diskPaths$(1) + "*.TOU", "TOURNAMENT", ExitCode%)

    baseFile$ = GetFileBaseName$(tourneyFile$)

    If tourneyFile$ <> "" Then

        If ACTION% = 5 Then
            'Reset tournament

            'Create TOU file as backup of TCX
            'Call FCopy(diskPaths$(1) + baseFile$ + ".TCX", diskPaths$(1) + baseFile$ + ".TOU", Buff$, copyErr%)
            result$ = CopyFile$(diskPaths$(1) + baseFile$ + ".TCX", diskPaths$(1) + baseFile$ + ".TOU")

            If result$ = "" Then
                MsgText$ = "ALL FILES SAVED...PRESS ANY KEY TO RETURN TO TOURNAMENT OPTIONS"
            Else
                MsgText$ = "THERE WAS AN ERROR SAVING FILES...PRESS ANY KEY TO RETURN TO TOURNAMENT OPTIONS"
            End If

            BoxWidth = 18
            BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
            Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
            Call CSMsgBox(MsgText$, BoxWidth, PASS)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, PASS)

            ACTION% = 1

        Else

            Open diskPaths$(1) + baseFile$ + ".INF" For Input As #1

            Input #1, tourneyName$

            Input #1, nbrRegions ' NUMBER OF REGIONS

            For I = 0 To 16
                Input #1, nbrRegionalTeams(I): ' NUMBER OF TEAMS IN REGION
                Input #1, regionNames$(I) ' NAME OF REGION
            Next

            For I = 0 To 16
                For I1 = 0 To 16
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

            Call SetupTourney(ACTION%, nbrRegions, tourneyName$, baseFile$)

        End If 'Done checking ACTION%

    End If

End Sub


'----------------------------------------
'       ScheduleWinners Subroutine
'----------------------------------------
'This subroutine loads the regular bracket
'games for the tournament, depending on the
'number of regions.
Sub ScheduleWinners (newRndGames, currRegion, playIn, saveFile$)

    Shared TC

    Shared homeTeam$(), visitingTeam$()
    Shared regionNames$(), slotName$(), yearNumber$()

    Shared consolationSetting(), nbrRegionalTeams()
    Shared scheduleNG%(), tourneyParams()

    Select Case newRndGames

        Case 1:
            '4 TEAM REGION
            TC = TC + 1
            REGION = currRegion
            GAME = 1
            ROUND = 1

            Call AssignGameSettings

            '3rd seed vs 2nd seed
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 3, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 2, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 3, 0)
            homeTeam$(TC) = slotName$(currRegion, 2, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 3, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 2, 1)

            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 2
            ROUND = 1

            Call AssignGameSettings

            '4th seed vs 1st seed
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 4, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 1, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 4, 0)
            homeTeam$(TC) = slotName$(currRegion, 1, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 4, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 1, 1)

            Call WRITEGAME(saveFile$)

            REGION = currRegion

            'Only pit "winners" against each other if
            'they are not play-in games
            If currRegion > 0 Then

                If consolationSetting(0) = 1 Then

                    TC = TC + 1
                    GAME = 3
                    ROUND = 88
                    Call AssignGameSettings
                    scheduleNG%(TC, 0) = 1801
                    scheduleNG%(TC, 1) = 1802
                    scheduleNG%(TC, 15) = REGION
                    scheduleNG%(TC, 16) = ROUND
                    scheduleNG%(TC, 17) = GAME
                    visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 1 LOSER"
                    homeTeam$(TC) = "REG" + Str$(REGION) + " GM 2 LOSER"
                    yearNumber$(TC, 0) = "XXX  "
                    yearNumber$(TC, 1) = "XXX  "
                    Call WRITEGAME(saveFile$)

                    TC = TC + 1
                    REGION = currRegion
                    GAME = 4
                    ROUND = 89
                    Call AssignGameSettings
                    scheduleNG%(TC, 0) = 1901
                    scheduleNG%(TC, 1) = 1902
                    scheduleNG%(TC, 15) = REGION
                    scheduleNG%(TC, 16) = ROUND
                    scheduleNG%(TC, 17) = GAME
                    visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 1 WINNER"
                    homeTeam$(TC) = "REG" + Str$(REGION) + " GM 2 WINNER"
                    yearNumber$(TC, 0) = "XXX  "
                    yearNumber$(TC, 1) = "XXX  "
                    Call WRITEGAME(saveFile$)

                Else

                    TC = TC + 1
                    REGION = currRegion
                    GAME = 3
                    ROUND = 89
                    Call AssignGameSettings
                    scheduleNG%(TC, 0) = 1901
                    scheduleNG%(TC, 1) = 1902
                    scheduleNG%(TC, 15) = REGION
                    scheduleNG%(TC, 16) = ROUND
                    scheduleNG%(TC, 17) = GAME
                    visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 1 WINNER"
                    homeTeam$(TC) = "REG" + Str$(REGION) + " GM 2 WINNER"
                    yearNumber$(TC, 0) = "XXX  "
                    yearNumber$(TC, 1) = "XXX  "
                    Call WRITEGAME(saveFile$)

                End If

            End If

        Case 2:
            '5 TEAM REGION
            TC = TC + 1
            REGION = currRegion
            GAME = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 5, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 4, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 5, 0)
            homeTeam$(TC) = slotName$(currRegion, 4, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 5, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 4, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 2
            ROUND = 2
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1901
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 1, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 1 WINNER"
            homeTeam$(TC) = slotName$(currRegion, 1, 0)
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = slotName$(currRegion, 1, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 3
            ROUND = 2
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 3, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 2, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 3, 0)
            homeTeam$(TC) = slotName$(currRegion, 2, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 3, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 2, 1)
            Call WRITEGAME(saveFile$)

            REGION = currRegion
            If consolationSetting(0) = 1 Then

                TC = TC + 1
                GAME = 4
                ROUND = 88
                Call AssignGameSettings
                scheduleNG%(TC, 0) = 1802
                scheduleNG%(TC, 1) = 1803
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 2 LOSER"
                homeTeam$(TC) = "REG" + Str$(REGION) + " GM 3 LOSER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

                TC = TC + 1
                REGION = currRegion
                GAME = 5
                ROUND = 89
                Call AssignGameSettings
                scheduleNG%(TC, 0) = 1902
                scheduleNG%(TC, 1) = 1903
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 2 WINNER"
                homeTeam$(TC) = "REG" + Str$(REGION) + " GM 3 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            Else

                TC = TC + 1
                REGION = currRegion
                GAME = 4
                ROUND = 89
                Call AssignGameSettings
                scheduleNG%(TC, 0) = 1902
                scheduleNG%(TC, 1) = 1903
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 2 WINNER"
                homeTeam$(TC) = "REG" + Str$(REGION) + " GM 3 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            End If

        Case 3:
            '6 TEAM REGION
            TC = TC + 1
            REGION = currRegion
            GAME = 1
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 5, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 4, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 5, 0)
            homeTeam$(TC) = slotName$(currRegion, 4, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 5, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 4, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 2
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 6, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 3, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 6, 0)
            homeTeam$(TC) = slotName$(currRegion, 3, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 6, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 3, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 3
            ROUND = 2
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1901
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 1, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 1 WINNER"
            homeTeam$(TC) = slotName$(currRegion, 1, 0)
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = slotName$(currRegion, 1, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 4
            ROUND = 2
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1902
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 2, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 2 WINNER"
            homeTeam$(TC) = slotName$(currRegion, 2, 0)
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = slotName$(currRegion, 2, 1)
            Call WRITEGAME(saveFile$)

            REGION = currRegion
            If consolationSetting(0) = 1 Then

                TC = TC + 1
                GAME = 5
                ROUND = 88
                Call AssignGameSettings
                scheduleNG%(TC, 0) = 1803
                scheduleNG%(TC, 1) = 1804
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 3 LOSER"
                homeTeam$(TC) = "REG" + Str$(REGION) + " GM 4 LOSER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

                TC = TC + 1
                REGION = currRegion
                GAME = 6
                ROUND = 89
                Call AssignGameSettings
                scheduleNG%(TC, 0) = 1903
                scheduleNG%(TC, 1) = 1904
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 3 WINNER"
                homeTeam$(TC) = "REG" + Str$(REGION) + " GM 4 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            Else

                TC = TC + 1
                REGION = currRegion
                GAME = 5
                ROUND = 89
                Call AssignGameSettings
                scheduleNG%(TC, 0) = 1903
                scheduleNG%(TC, 1) = 1904
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 3 WINNER"
                homeTeam$(TC) = "REG" + Str$(REGION) + " GM 4 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            End If

        Case 4:
            '7 TEAM REGION
            TC = TC + 1
            REGION = currRegion
            GAME = 1
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 6, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 3, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 6, 0)
            homeTeam$(TC) = slotName$(currRegion, 3, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 6, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 3, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 2
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 7, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 2, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 7, 0)
            homeTeam$(TC) = slotName$(currRegion, 2, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 7, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 2, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 3
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 5, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 4, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 5, 0)
            homeTeam$(TC) = slotName$(currRegion, 4, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 5, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 4, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 4
            ROUND = 2
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1901
            scheduleNG%(TC, 1) = 1902
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 1 WINNER"
            homeTeam$(TC) = "REG" + Str$(REGION) + " GM 2 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 5
            ROUND = 2
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1903
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 1, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 3 WINNER"
            homeTeam$(TC) = slotName$(currRegion, 1, 0)
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = slotName$(currRegion, 1, 1)
            Call WRITEGAME(saveFile$)

            REGION = currRegion
            If consolationSetting(0) = 1 Then

                TC = TC + 1
                GAME = 6
                ROUND = 88
                Call AssignGameSettings
                scheduleNG%(TC, 0) = 1804
                scheduleNG%(TC, 1) = 1805
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 4 LOSER"
                homeTeam$(TC) = "REG" + Str$(REGION) + " GM 5 LOSER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

                TC = TC + 1
                REGION = currRegion
                GAME = 7
                ROUND = 89
                Call AssignGameSettings
                scheduleNG%(TC, 0) = 1904
                scheduleNG%(TC, 1) = 1905
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 4 WINNER"
                homeTeam$(TC) = "REG" + Str$(REGION) + " GM 5 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            Else

                TC = TC + 1
                REGION = currRegion
                GAME = 6
                ROUND = 89

                Call AssignGameSettings

                scheduleNG%(TC, 0) = 1904
                scheduleNG%(TC, 1) = 1905
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 4 WINNER"
                homeTeam$(TC) = "REG" + Str$(REGION) + " GM 5 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "

                Call WRITEGAME(saveFile$)

            End If

        Case 5:
            '8 TEAM REGION
            TC = TC + 1
            REGION = currRegion
            GAME = 1
            ROUND = 1

            Call AssignGameSettings

            scheduleNG%(TC, 0) = tourneyParams(currRegion, 7, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 2, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 7, 0)
            homeTeam$(TC) = slotName$(currRegion, 2, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 7, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 2, 1)

            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 2
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 6, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 3, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 6, 0)
            homeTeam$(TC) = slotName$(currRegion, 3, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 6, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 3, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 3
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 5, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 4, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 5, 0)
            homeTeam$(TC) = slotName$(currRegion, 4, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 5, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 4, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 4
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 8, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 1, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 8, 0)
            homeTeam$(TC) = slotName$(currRegion, 1, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 8, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 1, 1)
            Call WRITEGAME(saveFile$)

            'Only pit "winners" against each other if
            'they are not play-in games
            If currRegion > 0 Then

                TC = TC + 1
                REGION = currRegion
                GAME = 5
                ROUND = 2
                Call AssignGameSettings
                scheduleNG%(TC, 0) = 1901
                scheduleNG%(TC, 1) = 1902
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 1 WINNER"
                homeTeam$(TC) = "REG" + Str$(REGION) + " GM 2 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

                TC = TC + 1
                REGION = currRegion
                GAME = 6
                ROUND = 2
                Call AssignGameSettings
                scheduleNG%(TC, 0) = 1903
                scheduleNG%(TC, 1) = 1904
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 3 WINNER"
                homeTeam$(TC) = "REG" + Str$(REGION) + " GM 4 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

                REGION = currRegion
                If consolationSetting(0) = 1 Then

                    TC = TC + 1
                    GAME = 7
                    ROUND = 88
                    Call AssignGameSettings
                    scheduleNG%(TC, 0) = 1805
                    scheduleNG%(TC, 1) = 1806
                    scheduleNG%(TC, 15) = REGION
                    scheduleNG%(TC, 16) = ROUND
                    scheduleNG%(TC, 17) = GAME
                    visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 5 LOSER"
                    homeTeam$(TC) = "REG" + Str$(REGION) + " GM 6 LOSER"
                    yearNumber$(TC, 0) = "XXX  "
                    yearNumber$(TC, 1) = "XXX  "
                    Call WRITEGAME(saveFile$)

                    TC = TC + 1
                    REGION = currRegion
                    GAME = 8
                    ROUND = 89
                    Call AssignGameSettings
                    scheduleNG%(TC, 0) = 1905
                    scheduleNG%(TC, 1) = 1906
                    scheduleNG%(TC, 15) = REGION
                    scheduleNG%(TC, 16) = ROUND
                    scheduleNG%(TC, 17) = GAME
                    visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 5 WINNER"
                    homeTeam$(TC) = "REG" + Str$(REGION) + " GM 6 WINNER"
                    yearNumber$(TC, 0) = "XXX  "
                    yearNumber$(TC, 1) = "XXX  "
                    Call WRITEGAME(saveFile$)

                Else

                    TC = TC + 1
                    REGION = currRegion
                    GAME = 7
                    ROUND = 89
                    Call AssignGameSettings
                    scheduleNG%(TC, 0) = 1905
                    scheduleNG%(TC, 1) = 1906
                    scheduleNG%(TC, 15) = REGION
                    scheduleNG%(TC, 16) = ROUND
                    scheduleNG%(TC, 17) = GAME
                    visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 5 WINNER"
                    homeTeam$(TC) = "REG" + Str$(REGION) + " GM 6 WINNER"
                    yearNumber$(TC, 0) = "XXX  "
                    yearNumber$(TC, 1) = "XXX  "
                    Call WRITEGAME(saveFile$)

                End If

            End If

        Case 6:
            '9 TEAM REGION
            TC = TC + 1
            REGION = currRegion
            GAME = 1
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 9, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 8, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 9, 0)
            homeTeam$(TC) = slotName$(currRegion, 8, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 9, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 8, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 2
            ROUND = 2
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 7, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 2, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 7, 0)
            homeTeam$(TC) = slotName$(currRegion, 2, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 7, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 2, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 3
            ROUND = 2
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 6, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 3, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 6, 0)
            homeTeam$(TC) = slotName$(currRegion, 3, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 6, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 3, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 4
            ROUND = 2
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 5, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 4, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 5, 0)
            homeTeam$(TC) = slotName$(currRegion, 4, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 5, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 4, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 5
            ROUND = 2
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1901
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 1, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 1 WINNER"
            homeTeam$(TC) = slotName$(currRegion, 1, 0)
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = slotName$(currRegion, 1, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 6
            ROUND = 3
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1902
            scheduleNG%(TC, 1) = 1903
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 2 WINNER"
            homeTeam$(TC) = "REG" + Str$(REGION) + " GM 3 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 7
            ROUND = 3
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1904
            scheduleNG%(TC, 1) = 1905
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 4 WINNER"
            homeTeam$(TC) = "REG" + Str$(REGION) + " GM 5 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            REGION = currRegion
            If consolationSetting(0) = 1 Then

                TC = TC + 1
                GAME = 8
                ROUND = 88
                Call AssignGameSettings
                scheduleNG%(TC, 0) = 1806
                scheduleNG%(TC, 1) = 1807
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 6 LOSER"
                homeTeam$(TC) = "REG" + Str$(REGION) + " GM 7 LOSER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

                TC = TC + 1
                REGION = currRegion
                GAME = 9
                ROUND = 89
                Call AssignGameSettings
                scheduleNG%(TC, 0) = 1906
                scheduleNG%(TC, 1) = 1907
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 6 WINNER"
                homeTeam$(TC) = "REG" + Str$(REGION) + " GM 7 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            Else

                TC = TC + 1
                REGION = currRegion
                GAME = 8
                ROUND = 89
                Call AssignGameSettings
                scheduleNG%(TC, 0) = 1906
                scheduleNG%(TC, 1) = 1907
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 6 WINNER"
                homeTeam$(TC) = "REG" + Str$(REGION) + " GM 7 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            End If

        Case 7:
            '10 TEAM REGION
            TC = TC + 1
            REGION = currRegion
            GAME = 1
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 10, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 7, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 10, 0)
            homeTeam$(TC) = slotName$(currRegion, 7, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 10, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 7, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 2
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 9, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 8, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 9, 0)
            homeTeam$(TC) = slotName$(currRegion, 8, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 9, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 8, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 3
            ROUND = 2
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1901
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 2, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 1 WINNER"
            homeTeam$(TC) = slotName$(currRegion, 2, 0)
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = slotName$(currRegion, 2, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 4
            ROUND = 2
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 6, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 3, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 6, 0)
            homeTeam$(TC) = slotName$(currRegion, 3, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 6, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 3, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 5
            ROUND = 2
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 5, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 4, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 5, 0)
            homeTeam$(TC) = slotName$(currRegion, 4, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 5, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 4, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 6
            ROUND = 2
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1902
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 1, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 2 WINNER"
            homeTeam$(TC) = slotName$(currRegion, 1, 0)
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = slotName$(currRegion, 1, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 7
            ROUND = 3
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1903
            scheduleNG%(TC, 1) = 1904
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 3 WINNER"
            homeTeam$(TC) = "REG" + Str$(REGION) + " GM 4 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 8
            ROUND = 3
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1905
            scheduleNG%(TC, 1) = 1906
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 5 WINNER"
            homeTeam$(TC) = "REG" + Str$(REGION) + " GM 6 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            REGION = currRegion
            If consolationSetting(0) = 1 Then

                TC = TC + 1
                GAME = 9
                ROUND = 88
                Call AssignGameSettings
                scheduleNG%(TC, 0) = 1807
                scheduleNG%(TC, 1) = 1808
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 7 LOSER"
                homeTeam$(TC) = "REG" + Str$(REGION) + " GM 8 LOSER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

                TC = TC + 1
                REGION = currRegion
                GAME = 10
                ROUND = 89
                Call AssignGameSettings
                scheduleNG%(TC, 0) = 1907
                scheduleNG%(TC, 1) = 1908
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 7 WINNER"
                homeTeam$(TC) = "REG" + Str$(REGION) + " GM 8 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            Else

                TC = TC + 1
                REGION = currRegion
                GAME = 9
                ROUND = 89
                Call AssignGameSettings
                scheduleNG%(TC, 0) = 1907
                scheduleNG%(TC, 1) = 1908
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 7 WINNER"
                homeTeam$(TC) = "REG" + Str$(REGION) + " GM 8 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            End If

        Case 8:
            '11 TEAM REGION
            TC = TC + 1
            REGION = currRegion
            GAME = 1
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 10, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 7, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 10, 0)
            homeTeam$(TC) = slotName$(currRegion, 7, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 10, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 7, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 2
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 11, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 6, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 11, 0)
            homeTeam$(TC) = slotName$(currRegion, 6, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 11, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 6, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 3
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 9, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 8, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 9, 0)
            homeTeam$(TC) = slotName$(currRegion, 8, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 9, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 8, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 4
            ROUND = 2
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1901
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 2, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 1 WINNER"
            homeTeam$(TC) = slotName$(currRegion, 2, 0)
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = slotName$(currRegion, 2, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 5
            ROUND = 2
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1902
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 3, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 2 WINNER"
            homeTeam$(TC) = slotName$(currRegion, 3, 0)
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = slotName$(currRegion, 3, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 6
            ROUND = 2
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 5, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 4, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 5, 0)
            homeTeam$(TC) = slotName$(currRegion, 4, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 5, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 4, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 7
            ROUND = 2
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1903
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 1, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 3 WINNER"
            homeTeam$(TC) = slotName$(currRegion, 1, 0)
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = slotName$(currRegion, 1, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 8
            ROUND = 3
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1904
            scheduleNG%(TC, 1) = 1905
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 4 WINNER"
            homeTeam$(TC) = "REG" + Str$(REGION) + " GM 5 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 9
            ROUND = 3
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1906
            scheduleNG%(TC, 1) = 1907
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 6 WINNER"
            homeTeam$(TC) = "REG" + Str$(REGION) + " GM 7 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            REGION = currRegion
            If consolationSetting(0) = 1 Then

                TC = TC + 1
                GAME = 10
                ROUND = 88
                Call AssignGameSettings
                scheduleNG%(TC, 0) = 1808
                scheduleNG%(TC, 1) = 1809
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 8 LOSER"
                homeTeam$(TC) = "REG" + Str$(REGION) + " GM 9 LOSER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

                TC = TC + 1
                REGION = currRegion
                GAME = 11
                ROUND = 89
                Call AssignGameSettings
                scheduleNG%(TC, 0) = 1908
                scheduleNG%(TC, 1) = 1909
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 8 WINNER"
                homeTeam$(TC) = "REG" + Str$(REGION) + " GM 9 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            Else

                TC = TC + 1
                REGION = currRegion
                GAME = 10
                ROUND = 89
                Call AssignGameSettings
                scheduleNG%(TC, 0) = 1908
                scheduleNG%(TC, 1) = 1909
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 8 WINNER"
                homeTeam$(TC) = "REG" + Str$(REGION) + " GM 9 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            End If

        Case 9:
            '12 TEAM REGION
            TC = TC + 1
            REGION = currRegion
            GAME = 1
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 10, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 7, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 10, 0)
            homeTeam$(TC) = slotName$(currRegion, 7, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 10, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 7, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 2
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 11, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 6, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 11, 0)
            homeTeam$(TC) = slotName$(currRegion, 6, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 11, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 6, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 3
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 12, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 5, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 12, 0)
            homeTeam$(TC) = slotName$(currRegion, 5, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 12, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 5, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 4
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 9, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 8, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 9, 0)
            homeTeam$(TC) = slotName$(currRegion, 8, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 9, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 8, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 5
            ROUND = 2
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1901
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 2, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 1 WINNER"
            homeTeam$(TC) = slotName$(currRegion, 2, 0)
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = slotName$(currRegion, 2, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 6
            ROUND = 2
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1902
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 3, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 2 WINNER"
            homeTeam$(TC) = slotName$(currRegion, 3, 0)
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = slotName$(currRegion, 3, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 7
            ROUND = 2
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1903
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 4, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 3 WINNER"
            homeTeam$(TC) = slotName$(currRegion, 4, 0)
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = slotName$(currRegion, 4, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 8
            ROUND = 2
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1904
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 1, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 4 WINNER"
            homeTeam$(TC) = slotName$(currRegion, 1, 0)
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = slotName$(currRegion, 1, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 9
            ROUND = 3
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1905
            scheduleNG%(TC, 1) = 1906
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 5 WINNER"
            homeTeam$(TC) = "REG" + Str$(REGION) + " GM 6 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 10
            ROUND = 3
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1907
            scheduleNG%(TC, 1) = 1908
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 7 WINNER"
            homeTeam$(TC) = "REG" + Str$(REGION) + " GM 8 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            REGION = currRegion
            If consolationSetting(0) = 1 Then

                TC = TC + 1
                GAME = 11
                ROUND = 88
                Call AssignGameSettings
                scheduleNG%(TC, 0) = 1809
                scheduleNG%(TC, 1) = 1810
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 9 LOSER"
                homeTeam$(TC) = "REG" + Str$(REGION) + " GM 10 LOSER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

                TC = TC + 1
                REGION = currRegion
                GAME = 12
                ROUND = 89
                Call AssignGameSettings
                scheduleNG%(TC, 0) = 1909
                scheduleNG%(TC, 1) = 1910
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 9 WINNER"
                homeTeam$(TC) = "REG" + Str$(REGION) + " GM 10 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            Else

                TC = TC + 1
                REGION = currRegion
                GAME = 11
                ROUND = 89
                Call AssignGameSettings
                scheduleNG%(TC, 0) = 1909
                scheduleNG%(TC, 1) = 1910
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 9 WINNER"
                homeTeam$(TC) = "REG" + Str$(REGION) + " GM 10 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            End If

        Case 10:
            '13 TEAM REGION
            TC = TC + 1
            REGION = currRegion
            GAME = 1
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 13, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 4, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 13, 0)
            homeTeam$(TC) = slotName$(currRegion, 4, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 13, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 4, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 2
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 12, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 5, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 12, 0)
            homeTeam$(TC) = slotName$(currRegion, 5, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 12, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 5, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 3
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 9, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 8, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 9, 0)
            homeTeam$(TC) = slotName$(currRegion, 8, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 9, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 8, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 4
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 10, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 7, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 10, 0)
            homeTeam$(TC) = slotName$(currRegion, 7, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 10, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 7, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 5
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 11, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 6, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 11, 0)
            homeTeam$(TC) = slotName$(currRegion, 6, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 11, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 6, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 6
            ROUND = 2
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1901
            scheduleNG%(TC, 1) = 1902
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 1 WINNER"
            homeTeam$(TC) = "REG" + Str$(REGION) + " GM 2 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 7
            ROUND = 2
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1903
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 1, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 3 WINNER"
            homeTeam$(TC) = slotName$(currRegion, 1, 0)
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = slotName$(currRegion, 1, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 8
            ROUND = 2
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1904
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 2, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 4 WINNER"
            homeTeam$(TC) = slotName$(currRegion, 2, 0)
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = slotName$(currRegion, 2, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 9
            ROUND = 2
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1905
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 3, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 5 WINNER"
            homeTeam$(TC) = slotName$(currRegion, 3, 0)
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = slotName$(currRegion, 3, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 10
            ROUND = 3
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1906
            scheduleNG%(TC, 1) = 1907
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 6 WINNER"
            homeTeam$(TC) = "REG" + Str$(REGION) + " GM 7 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 11
            ROUND = 3
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1908
            scheduleNG%(TC, 1) = 1909
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 8 WINNER"
            homeTeam$(TC) = "REG" + Str$(REGION) + " GM 9 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            REGION = currRegion
            If consolationSetting(0) = 1 Then

                TC = TC + 1
                GAME = 12
                ROUND = 88
                Call AssignGameSettings
                scheduleNG%(TC, 0) = 1810
                scheduleNG%(TC, 1) = 1811
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 10 LOSER"
                homeTeam$(TC) = "REG" + Str$(REGION) + " GM 11 LOSER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

                TC = TC + 1
                REGION = currRegion
                GAME = 13
                ROUND = 89
                Call AssignGameSettings
                scheduleNG%(TC, 0) = 1910
                scheduleNG%(TC, 1) = 1911
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 10 WINNER"
                homeTeam$(TC) = "REG" + Str$(REGION) + " GM 11 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            Else

                TC = TC + 1
                REGION = currRegion
                GAME = 12
                ROUND = 89
                Call AssignGameSettings
                scheduleNG%(TC, 0) = 1910
                scheduleNG%(TC, 1) = 1911
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 10 WINNER"
                homeTeam$(TC) = "REG" + Str$(REGION) + " GM 11 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            End If

        Case 11:
            '14 TEAM REGION
            TC = TC + 1
            REGION = currRegion
            GAME = 1
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 14, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 3, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 14, 0)
            homeTeam$(TC) = slotName$(currRegion, 3, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 14, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 3, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 2
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 13, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 4, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 13, 0)
            homeTeam$(TC) = slotName$(currRegion, 4, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 13, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 4, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 3
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 12, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 5, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 12, 0)
            homeTeam$(TC) = slotName$(currRegion, 5, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 12, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 5, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 4
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 11, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 6, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 11, 0)
            homeTeam$(TC) = slotName$(currRegion, 6, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 11, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 6, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 5
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 10, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 7, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 10, 0)
            homeTeam$(TC) = slotName$(currRegion, 7, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 10, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 7, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 6
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 9, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 8, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 9, 0)
            homeTeam$(TC) = slotName$(currRegion, 8, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 9, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 8, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 7
            ROUND = 2
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1901
            scheduleNG%(TC, 1) = 1904
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 1 WINNER"
            homeTeam$(TC) = "REG" + Str$(REGION) + " GM 4 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 8
            ROUND = 2
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1905
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 2, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 5 WINNER"
            homeTeam$(TC) = slotName$(currRegion, 2, 0)
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = slotName$(currRegion, 2, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 9
            ROUND = 2
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1902
            scheduleNG%(TC, 1) = 1903
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 2 WINNER"
            homeTeam$(TC) = "REG" + Str$(REGION) + " GM 3 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 10
            ROUND = 2
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1906
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 1, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 6 WINNER"
            homeTeam$(TC) = slotName$(currRegion, 1, 0)
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = slotName$(currRegion, 1, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 11
            ROUND = 3
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1907
            scheduleNG%(TC, 1) = 1908
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 7 WINNER"
            homeTeam$(TC) = "REG" + Str$(REGION) + " GM 8 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 12
            ROUND = 3
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1909
            scheduleNG%(TC, 1) = 1910
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 9 WINNER"
            homeTeam$(TC) = "REG" + Str$(REGION) + " GM 10 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            REGION = currRegion
            If consolationSetting(0) = 1 Then

                TC = TC + 1
                GAME = 13
                ROUND = 88
                Call AssignGameSettings
                scheduleNG%(TC, 0) = 1811
                scheduleNG%(TC, 1) = 1812
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 11 LOSER"
                homeTeam$(TC) = "REG" + Str$(REGION) + " GM 12 LOSER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

                TC = TC + 1
                REGION = currRegion
                GAME = 14
                ROUND = 89
                Call AssignGameSettings
                scheduleNG%(TC, 0) = 1911
                scheduleNG%(TC, 1) = 1912
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 11 WINNER"
                homeTeam$(TC) = "REG" + Str$(REGION) + " GM 12 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            Else

                TC = TC + 1
                REGION = currRegion
                GAME = 13
                ROUND = 89
                Call AssignGameSettings
                scheduleNG%(TC, 0) = 1911
                scheduleNG%(TC, 1) = 1912
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 11 WINNER"
                homeTeam$(TC) = "REG" + Str$(REGION) + " GM 12 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            End If

        Case 12:
            '15 TEAM REGION
            TC = TC + 1
            REGION = currRegion
            GAME = 1
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 15, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 2, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 15, 0)
            homeTeam$(TC) = slotName$(currRegion, 2, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 15, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 2, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 2
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 14, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 3, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 14, 0)
            homeTeam$(TC) = slotName$(currRegion, 3, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 14, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 3, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 3
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 13, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 4, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 13, 0)
            homeTeam$(TC) = slotName$(currRegion, 4, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 13, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 4, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 4
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 12, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 5, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 12, 0)
            homeTeam$(TC) = slotName$(currRegion, 5, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 12, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 5, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 5
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 11, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 6, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 11, 0)
            homeTeam$(TC) = slotName$(currRegion, 6, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 11, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 6, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 6
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 10, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 7, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 10, 0)
            homeTeam$(TC) = slotName$(currRegion, 7, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 10, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 7, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 7
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 9, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 8, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 9, 0)
            homeTeam$(TC) = slotName$(currRegion, 8, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 9, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 8, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 8
            ROUND = 2
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1901
            scheduleNG%(TC, 1) = 1906
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 1 WINNER"
            homeTeam$(TC) = "REG" + Str$(REGION) + " GM 6 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 9
            ROUND = 2
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1902
            scheduleNG%(TC, 1) = 1905
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 2 WINNER"
            homeTeam$(TC) = "REG" + Str$(REGION) + " GM 5 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 10
            ROUND = 2
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1903
            scheduleNG%(TC, 1) = 1904
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 3 WINNER"
            homeTeam$(TC) = "REG" + Str$(REGION) + " GM 4 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 11
            ROUND = 2
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1907
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 1, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 7 WINNER"
            homeTeam$(TC) = slotName$(currRegion, 1, 0)
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = slotName$(currRegion, 1, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 12
            ROUND = 3
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1908
            scheduleNG%(TC, 1) = 1909
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 8 WINNER"
            homeTeam$(TC) = "REG" + Str$(REGION) + " GM 9 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 13
            ROUND = 3
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1910
            scheduleNG%(TC, 1) = 1911
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 10 WINNER"
            homeTeam$(TC) = "REG" + Str$(REGION) + " GM 11 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            REGION = currRegion
            If consolationSetting(0) = 1 Then

                TC = TC + 1
                GAME = 14
                ROUND = 88
                Call AssignGameSettings
                scheduleNG%(TC, 0) = 1812
                scheduleNG%(TC, 1) = 1813
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 12 LOSER"
                homeTeam$(TC) = "REG" + Str$(REGION) + " GM 13 LOSER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

                TC = TC + 1
                REGION = currRegion
                GAME = 15
                ROUND = 89
                Call AssignGameSettings
                scheduleNG%(TC, 0) = 1912
                scheduleNG%(TC, 1) = 1913
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 12 WINNER"
                homeTeam$(TC) = "REG" + Str$(REGION) + " GM 13 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            Else

                TC = TC + 1
                REGION = currRegion
                GAME = 14
                ROUND = 89
                Call AssignGameSettings
                scheduleNG%(TC, 0) = 1912
                scheduleNG%(TC, 1) = 1913
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 12 WINNER"
                homeTeam$(TC) = "REG" + Str$(REGION) + " GM 13 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            End If

        Case 13:
            '16 TEAM REGION
            TC = TC + 1
            REGION = currRegion
            GAME = 1
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 16, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 1, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 16, 0)
            homeTeam$(TC) = slotName$(currRegion, 1, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 16, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 1, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 2
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 15, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 2, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 15, 0)
            homeTeam$(TC) = slotName$(currRegion, 2, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 15, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 2, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 3
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 14, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 3, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 14, 0)
            homeTeam$(TC) = slotName$(currRegion, 3, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 14, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 3, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 4
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 13, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 4, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 13, 0)
            homeTeam$(TC) = slotName$(currRegion, 4, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 13, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 4, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 5
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 12, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 5, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 12, 0)
            homeTeam$(TC) = slotName$(currRegion, 5, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 12, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 5, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 6
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 11, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 6, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 11, 0)
            homeTeam$(TC) = slotName$(currRegion, 6, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 11, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 6, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 7
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 10, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 7, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 10, 0)
            homeTeam$(TC) = slotName$(currRegion, 7, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 10, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 7, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 8
            ROUND = 1
            Call AssignGameSettings
            scheduleNG%(TC, 0) = tourneyParams(currRegion, 9, 0)
            scheduleNG%(TC, 1) = tourneyParams(currRegion, 8, 0)
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = slotName$(currRegion, 9, 0)
            homeTeam$(TC) = slotName$(currRegion, 8, 0)
            yearNumber$(TC, 0) = slotName$(currRegion, 9, 1)
            yearNumber$(TC, 1) = slotName$(currRegion, 8, 1)
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 9
            ROUND = 2
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1901
            scheduleNG%(TC, 1) = 1908
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 1 WINNER"
            homeTeam$(TC) = "REG" + Str$(REGION) + " GM 8 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 10
            ROUND = 2
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1904
            scheduleNG%(TC, 1) = 1905
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 4 WINNER"
            homeTeam$(TC) = "REG" + Str$(REGION) + " GM 5 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 11
            ROUND = 2
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1903
            scheduleNG%(TC, 1) = 1906
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 3 WINNER"
            homeTeam$(TC) = "REG" + Str$(REGION) + " GM 6 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 12
            ROUND = 2
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1902
            scheduleNG%(TC, 1) = 1907
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 2 WINNER"
            homeTeam$(TC) = "REG" + Str$(REGION) + " GM 7 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 13
            ROUND = 3
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1909
            scheduleNG%(TC, 1) = 1910
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 9 WINNER"
            homeTeam$(TC) = "REG" + Str$(REGION) + " GM 10 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = currRegion
            GAME = 14
            ROUND = 3
            Call AssignGameSettings
            scheduleNG%(TC, 0) = 1911
            scheduleNG%(TC, 1) = 1912
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 11 WINNER"
            homeTeam$(TC) = "REG" + Str$(REGION) + " GM 12 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            REGION = currRegion
            If consolationSetting(0) = 1 Then

                TC = TC + 1
                GAME = 15
                ROUND = 88
                Call AssignGameSettings
                scheduleNG%(TC, 0) = 1813
                scheduleNG%(TC, 1) = 1814
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 13 LOSER"
                homeTeam$(TC) = "REG" + Str$(REGION) + " GM 14 LOSER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

                TC = TC + 1
                REGION = currRegion
                GAME = 16
                ROUND = 89
                Call AssignGameSettings
                scheduleNG%(TC, 0) = 1913
                scheduleNG%(TC, 1) = 1914
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 13 WINNER"
                homeTeam$(TC) = "REG" + Str$(REGION) + " GM 14 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            Else

                TC = TC + 1
                REGION = currRegion
                GAME = 15
                ROUND = 89
                Call AssignGameSettings
                scheduleNG%(TC, 0) = 1913
                scheduleNG%(TC, 1) = 1914
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "REG" + Str$(REGION) + " GM 13 WINNER"
                homeTeam$(TC) = "REG" + Str$(REGION) + " GM 14 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            End If

    End Select

End Sub

'----------------------------------------
'        CHAMPIONSHIP Subroutine
'----------------------------------------
'This subroutine configures the championship
'bracket / seed for the tournament, depending
'on the number of regions.
Sub CHAMPIONSHIP (nbrRegions, saveFile$, tournName$)

    Shared TC

    Shared homeTeam$(), visitingTeam$(), yearNumber$()
    Shared consolationSetting(), scheduleNG%()

    Select Case nbrRegions

        Case 1:

        Case 2:
            '2 REGIONS
            TC = TC + 1
            REGION = 17
            GAME = 1
            ROUND = 99
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1901
            scheduleNG%(TC, 1) = 1902
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 1 WINNER"
            homeTeam$(TC) = "REGION 2 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

        Case 3:
            '3 REGIONS
            TC = TC + 1
            REGION = 17
            GAME = 1
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1903
            scheduleNG%(TC, 1) = 1902
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 3 WINNER"
            homeTeam$(TC) = "REGION 2 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 2
            ROUND = 99
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2101
            scheduleNG%(TC, 1) = 1901
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 1 WINNER"
            homeTeam$(TC) = "REGION 1 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

        Case 4:
            '4 REGIONS
            TC = TC + 1
            REGION = 17
            GAME = 1
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1901
            scheduleNG%(TC, 1) = 1904
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 1 WINNER"
            homeTeam$(TC) = "REGION 4 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 2
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1902
            scheduleNG%(TC, 1) = 1903
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 2 WINNER"
            homeTeam$(TC) = "REGION 3 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            If consolationSetting(1) <> 0 Then
                TC = TC + 1
                REGION = 17
                GAME = 3
                ROUND = 98
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 2001
                scheduleNG%(TC, 1) = 2002
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 1 LOSER"
                homeTeam$(TC) = "GAME 2 LOSER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

                TC = TC + 1
                REGION = 17
                GAME = 4
                ROUND = 99
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 2101
                scheduleNG%(TC, 1) = 2102
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 1 WINNER"
                homeTeam$(TC) = "GAME 2 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            Else

                TC = TC + 1
                REGION = 17
                GAME = 3
                ROUND = 99
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 2101
                scheduleNG%(TC, 1) = 2102
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 1 WINNER"
                homeTeam$(TC) = "GAME 2 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            End If

        Case 5:
            '5 REGIONS
            TC = TC + 1
            REGION = 17
            GAME = 1
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1905
            scheduleNG%(TC, 1) = 1904
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 5 WINNER"
            homeTeam$(TC) = "REGION 4 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 2
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2101
            scheduleNG%(TC, 1) = 1901
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 1 WINNER"
            homeTeam$(TC) = "REGION 1 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 3
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1902
            scheduleNG%(TC, 1) = 1903
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 2 WINNER"
            homeTeam$(TC) = "REGION 3 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            If consolationSetting(1) <> 0 Then
                TC = TC + 1
                REGION = 17
                GAME = 4
                ROUND = 98
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 2003
                scheduleNG%(TC, 1) = 2002
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 3 LOSER"
                homeTeam$(TC) = "GAME 2 LOSER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

                TC = TC + 1
                REGION = 17
                GAME = 5
                ROUND = 99
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 2103
                scheduleNG%(TC, 1) = 2102
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 3 WINNER"
                homeTeam$(TC) = "GAME 2 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            Else

                TC = TC + 1
                REGION = 17
                GAME = 4
                ROUND = 99
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 2103
                scheduleNG%(TC, 1) = 2102
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 3 WINNER"
                homeTeam$(TC) = "GAME 2 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            End If

        Case 6:
            '6 REGIONS
            TC = TC + 1
            REGION = 17
            GAME = 1
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1905
            scheduleNG%(TC, 1) = 1904
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 5 WINNER"
            homeTeam$(TC) = "REGION 4 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 2
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1903
            scheduleNG%(TC, 1) = 1906
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 3 WINNER"
            homeTeam$(TC) = "REGION 6 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 3
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2101
            scheduleNG%(TC, 1) = 1901
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 1 WINNER"
            homeTeam$(TC) = "REGION 1 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 4
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2102
            scheduleNG%(TC, 1) = 1902
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 2 WINNER"
            homeTeam$(TC) = "REGION 2 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            If consolationSetting(1) <> 0 Then
                TC = TC + 1
                REGION = 17
                GAME = 5
                ROUND = 98
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 2003
                scheduleNG%(TC, 1) = 2004
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 3 LOSER"
                homeTeam$(TC) = "GAME 4 LOSER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

                TC = TC + 1
                REGION = 17
                GAME = 6
                ROUND = 99
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 2103
                scheduleNG%(TC, 1) = 2104
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 3 WINNER"
                homeTeam$(TC) = "GAME 4 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            Else

                TC = TC + 1
                REGION = 17
                GAME = 5
                ROUND = 99
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 2103
                scheduleNG%(TC, 1) = 2104
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 3 WINNER"
                homeTeam$(TC) = "GAME 4 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            End If

        Case 7:
            '7 REGIONS
            TC = TC + 1
            REGION = 17
            GAME = 1
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1903
            scheduleNG%(TC, 1) = 1906
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 3 WINNER"
            homeTeam$(TC) = "REGION 6 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 2
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1902
            scheduleNG%(TC, 1) = 1907
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 2 WINNER"
            homeTeam$(TC) = "REGION 7 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 3
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1904
            scheduleNG%(TC, 1) = 1905
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 4 WINNER"
            homeTeam$(TC) = "REGION 5 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 4
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2102
            scheduleNG%(TC, 1) = 2101
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 2 WINNER"
            homeTeam$(TC) = "GAME 1 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 5
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2103
            scheduleNG%(TC, 1) = 1901
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 3 WINNER"
            homeTeam$(TC) = "REGION 1 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            If consolationSetting(1) <> 0 Then
                TC = TC + 1
                REGION = 17
                GAME = 6
                ROUND = 98
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 2005
                scheduleNG%(TC, 1) = 2004
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 5 LOSER"
                homeTeam$(TC) = "GAME 4 LOSER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

                TC = TC + 1
                REGION = 17
                GAME = 7
                ROUND = 99
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 2105
                scheduleNG%(TC, 1) = 2104
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 5 WINNER"
                homeTeam$(TC) = "GAME 4 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            Else

                TC = TC + 1
                REGION = 17
                GAME = 6
                ROUND = 99
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 2105
                scheduleNG%(TC, 1) = 2104
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 5 WINNER"
                homeTeam$(TC) = "GAME 4 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            End If

        Case 8:
            '8 REGIONS
            TC = TC + 1
            REGION = 17
            GAME = 1
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1902
            scheduleNG%(TC, 1) = 1907
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 2 WINNER"
            homeTeam$(TC) = "REGION 7 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 2
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1903
            scheduleNG%(TC, 1) = 1906
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 3 WINNER"
            homeTeam$(TC) = "REGION 6 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 3
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1904
            scheduleNG%(TC, 1) = 1905
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 4 WINNER"
            homeTeam$(TC) = "REGION 5 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 4
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1901
            scheduleNG%(TC, 1) = 1908
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 1 WINNER"
            homeTeam$(TC) = "REGION 8 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 5
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2101
            scheduleNG%(TC, 1) = 2102
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 1 WINNER"
            homeTeam$(TC) = "GAME 2 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 6
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2103
            scheduleNG%(TC, 1) = 2104
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 3 WINNER"
            homeTeam$(TC) = "GAME 4 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            If consolationSetting(1) <> 0 Then
                TC = TC + 1
                REGION = 17
                GAME = 7
                ROUND = 98
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 2005
                scheduleNG%(TC, 1) = 2006
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 5 LOSER"
                homeTeam$(TC) = "GAME 6 LOSER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

                TC = TC + 1
                REGION = 17
                GAME = 8
                ROUND = 99
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 2105
                scheduleNG%(TC, 1) = 2106
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 5 WINNER"
                homeTeam$(TC) = "GAME 6 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            Else

                TC = TC + 1
                REGION = 17
                GAME = 7
                ROUND = 99
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 2105
                scheduleNG%(TC, 1) = 2106
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 5 WINNER"
                homeTeam$(TC) = "GAME 6 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            End If

        Case 9:
            '9 REGIONS
            TC = TC + 1
            REGION = 17
            GAME = 1
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1909
            scheduleNG%(TC, 1) = 1908
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 9 WINNER"
            homeTeam$(TC) = "REGION 8 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 2
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1902
            scheduleNG%(TC, 1) = 1907
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 2 WINNER"
            homeTeam$(TC) = "REGION 7 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 3
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1903
            scheduleNG%(TC, 1) = 1906
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 3 WINNER"
            homeTeam$(TC) = "REGION 6 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 4
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1904
            scheduleNG%(TC, 1) = 1905
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 4 WINNER"
            homeTeam$(TC) = "REGION 5 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 5
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1901
            scheduleNG%(TC, 1) = 2101
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 1 WINNER"
            homeTeam$(TC) = "GAME 1 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 6
            ROUND = 3
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2102
            scheduleNG%(TC, 1) = 2103
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 2 WINNER"
            homeTeam$(TC) = "GAME 3 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 7
            ROUND = 3
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2104
            scheduleNG%(TC, 1) = 2105
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 4 WINNER"
            homeTeam$(TC) = "GAME 5 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            If consolationSetting(1) <> 0 Then
                TC = TC + 1
                REGION = 17
                GAME = 8
                ROUND = 98
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 2007
                scheduleNG%(TC, 1) = 2006
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 7 LOSER"
                homeTeam$(TC) = "GAME 6 LOSER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

                TC = TC + 1
                REGION = 17
                GAME = 9
                ROUND = 99
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 2107
                scheduleNG%(TC, 1) = 2106
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 7 WINNER"
                homeTeam$(TC) = "GAME 6 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            Else

                TC = TC + 1
                REGION = 17
                GAME = 8
                ROUND = 99
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 2107
                scheduleNG%(TC, 1) = 2106
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 7 WINNER"
                homeTeam$(TC) = "GAME 6 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            End If

        Case 10:
            '10 REGIONS
            TC = TC + 1
            REGION = 17
            GAME = 1
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1910
            scheduleNG%(TC, 1) = 1907
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 10 WINNER"
            homeTeam$(TC) = "REGION 7 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 2
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1908
            scheduleNG%(TC, 1) = 1909
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 8 WINNER"
            homeTeam$(TC) = "REGION 9 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 3
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2101
            scheduleNG%(TC, 1) = 1902
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 1 WINNER"
            homeTeam$(TC) = "REGION 2 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 4
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1903
            scheduleNG%(TC, 1) = 1906
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 3 WINNER"
            homeTeam$(TC) = "REGION 6 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 5
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1904
            scheduleNG%(TC, 1) = 1905
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 4 WINNER"
            homeTeam$(TC) = "REGION 5 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 6
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1901
            scheduleNG%(TC, 1) = 2102
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 1 WINNER"
            homeTeam$(TC) = "GAME 2 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 7
            ROUND = 3
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2103
            scheduleNG%(TC, 1) = 2104
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 3 WINNER"
            homeTeam$(TC) = "GAME 4 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 8
            ROUND = 3
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2105
            scheduleNG%(TC, 1) = 2106
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 5 WINNER"
            homeTeam$(TC) = "GAME 6 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            If consolationSetting(1) <> 0 Then
                TC = TC + 1
                REGION = 17
                GAME = 9
                ROUND = 98
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 2007
                scheduleNG%(TC, 1) = 2008
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 7 LOSER"
                homeTeam$(TC) = "GAME 8 LOSER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

                TC = TC + 1
                REGION = 17
                GAME = 10
                ROUND = 99
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 2107
                scheduleNG%(TC, 1) = 2108
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 7 WINNER"
                homeTeam$(TC) = "GAME 8 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            Else
                TC = TC + 1
                REGION = 17
                GAME = 9
                ROUND = 99
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 2107
                scheduleNG%(TC, 1) = 2108
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 7 WINNER"
                homeTeam$(TC) = "GAME 8 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            End If

        Case 11:
            '11 REGIONS
            TC = TC + 1
            REGION = 17
            GAME = 1
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1910
            scheduleNG%(TC, 1) = 1907
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 10 WINNER"
            homeTeam$(TC) = "REGION 7 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 2
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1911
            scheduleNG%(TC, 1) = 1906
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 11 WINNER"
            homeTeam$(TC) = "REGION 6 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 3
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1909
            scheduleNG%(TC, 1) = 1908
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 9 WINNER"
            homeTeam$(TC) = "REGION 8 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 4
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2101
            scheduleNG%(TC, 1) = 1902
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 1 WINNER"
            homeTeam$(TC) = "REGION 2 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 5
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1903
            scheduleNG%(TC, 1) = 2102
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 3 WINNER"
            homeTeam$(TC) = "GAME 2 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 6
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1904
            scheduleNG%(TC, 1) = 1905
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 4 WINNER"
            homeTeam$(TC) = "REGION 5 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 7
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1901
            scheduleNG%(TC, 1) = 2103
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 1 WINNER"
            homeTeam$(TC) = "GAME 3 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 8
            ROUND = 3
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2104
            scheduleNG%(TC, 1) = 2105
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 4 WINNER"
            homeTeam$(TC) = "GAME 5 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 9
            ROUND = 3
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2106
            scheduleNG%(TC, 1) = 2107
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 6 WINNER"
            homeTeam$(TC) = "GAME 7 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            If consolationSetting(1) <> 0 Then
                TC = TC + 1
                REGION = 17
                GAME = 10
                ROUND = 98
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 2009
                scheduleNG%(TC, 1) = 2008
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 9 LOSER"
                homeTeam$(TC) = "GAME 8 LOSER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

                TC = TC + 1
                REGION = 17
                GAME = 11
                ROUND = 99
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 2109
                scheduleNG%(TC, 1) = 2108
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 9 WINNER"
                homeTeam$(TC) = "GAME 8 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            Else

                TC = TC + 1
                REGION = 17
                GAME = 10
                ROUND = 99
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 2109
                scheduleNG%(TC, 1) = 2108
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 7 WINNER"
                homeTeam$(TC) = "GAME 8 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            End If

        Case 12:
            '12 REGIONS
            TC = TC + 1
            REGION = 17
            GAME = 1
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1910
            scheduleNG%(TC, 1) = 1907
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 10 WINNER"
            homeTeam$(TC) = "REGION 7 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 2
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1911
            scheduleNG%(TC, 1) = 1906
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 11 WINNER"
            homeTeam$(TC) = "REGION 6 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 3
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1912
            scheduleNG%(TC, 1) = 1905
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 12 WINNER"
            homeTeam$(TC) = "REGION 5 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 4
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1909
            scheduleNG%(TC, 1) = 1908
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 9 WINNER"
            homeTeam$(TC) = "REGION 8 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 5
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1902
            scheduleNG%(TC, 1) = 2101
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 2 WINNER"
            homeTeam$(TC) = "GAME 1 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 6
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2102
            scheduleNG%(TC, 1) = 1903
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 2 WINNER"
            homeTeam$(TC) = "REGION 3 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 7
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1904
            scheduleNG%(TC, 1) = 2103
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 4 WINNER"
            homeTeam$(TC) = "GAME 3 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 8
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2104
            scheduleNG%(TC, 1) = 1901
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 4 WINNER"
            homeTeam$(TC) = "REGION 1 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 9
            ROUND = 3
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2106
            scheduleNG%(TC, 1) = 2105
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 6 WINNER"
            homeTeam$(TC) = "GAME 5 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 10
            ROUND = 3
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2107
            scheduleNG%(TC, 1) = 2108
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 7 WINNER"
            homeTeam$(TC) = "GAME 8 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            If consolationSetting(1) <> 0 Then
                TC = TC + 1
                REGION = 17
                GAME = 11
                ROUND = 98
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 2009
                scheduleNG%(TC, 1) = 2010
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 9 LOSER"
                homeTeam$(TC) = "GAME 10 LOSER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

                TC = TC + 1
                REGION = 17
                GAME = 12
                ROUND = 99
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 2109
                scheduleNG%(TC, 1) = 2110
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 9 WINNER"
                homeTeam$(TC) = "GAME 10 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            Else

                TC = TC + 1
                REGION = 17
                GAME = 11
                ROUND = 99
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 2109
                scheduleNG%(TC, 1) = 2110
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 9 WINNER"
                homeTeam$(TC) = "GAME 10 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            End If

        Case 13:
            '13 REGIONS
            TC = TC + 1
            REGION = 17
            GAME = 1
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1913
            scheduleNG%(TC, 1) = 1904
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 13 WINNER"
            homeTeam$(TC) = "REGION 4 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 2
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1912
            scheduleNG%(TC, 1) = 1905
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 12 WINNER"
            homeTeam$(TC) = "REGION 5 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 3
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1909
            scheduleNG%(TC, 1) = 1908
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 9 WINNER"
            homeTeam$(TC) = "REGION 8 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 4
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1910
            scheduleNG%(TC, 1) = 1907
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 10 WINNER"
            homeTeam$(TC) = "REGION 7 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 5
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1906
            scheduleNG%(TC, 1) = 1911
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 6 WINNER"
            homeTeam$(TC) = "REGION 11 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 6
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2101
            scheduleNG%(TC, 1) = 2102
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 1 WINNER"
            homeTeam$(TC) = "GAME 2 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 7
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1901
            scheduleNG%(TC, 1) = 2103
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 1 WINNER"
            homeTeam$(TC) = "GAME 3 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 8
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2104
            scheduleNG%(TC, 1) = 1902
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 4 WINNER"
            homeTeam$(TC) = "REGION 2 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 9
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1903
            scheduleNG%(TC, 1) = 2105
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 3 WINNER"
            homeTeam$(TC) = "GAME 5 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 10
            ROUND = 3
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2107
            scheduleNG%(TC, 1) = 2106
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 7 WINNER"
            homeTeam$(TC) = "GAME 6 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 11
            ROUND = 3
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2103
            scheduleNG%(TC, 1) = 2109
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 3 WINNER"
            homeTeam$(TC) = "GAME 9 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            If consolationSetting(1) <> 0 Then
                TC = TC + 1
                REGION = 17
                GAME = 12
                ROUND = 98
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 2011
                scheduleNG%(TC, 1) = 2010
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 11 LOSER"
                homeTeam$(TC) = "GAME 10 LOSER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

                TC = TC + 1
                REGION = 17
                GAME = 13
                ROUND = 99
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 2111
                scheduleNG%(TC, 1) = 2110
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 11 WINNER"
                homeTeam$(TC) = "GAME 10 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            Else

                TC = TC + 1
                REGION = 17
                GAME = 12
                ROUND = 99
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 2111
                scheduleNG%(TC, 1) = 2110
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 11 WINNER"
                homeTeam$(TC) = "GAME 10 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            End If

        Case 14:

            '14 REGIONS
            TC = TC + 1
            REGION = 17
            GAME = 1
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1914
            scheduleNG%(TC, 1) = 1903
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 14 WINNER"
            homeTeam$(TC) = "REGION 3 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 2
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1913
            scheduleNG%(TC, 1) = 1904
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 13 WINNER"
            homeTeam$(TC) = "REGION 4 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 3
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1912
            scheduleNG%(TC, 1) = 1905
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 12 WINNER"
            homeTeam$(TC) = "REGION 5 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 4
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1911
            scheduleNG%(TC, 1) = 1906
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 11 WINNER"
            homeTeam$(TC) = "REGION 6 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 5
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1910
            scheduleNG%(TC, 1) = 1907
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 10 WINNER"
            homeTeam$(TC) = "REGION 7 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 6
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1909
            scheduleNG%(TC, 1) = 1909
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 9 WINNER"
            homeTeam$(TC) = "REGION 8 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 7
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2104
            scheduleNG%(TC, 1) = 2101
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 4 WINNER"
            homeTeam$(TC) = "GAME 1 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 8
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2105
            scheduleNG%(TC, 1) = 1902
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 5 WINNER"
            homeTeam$(TC) = "REGION 2 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 9
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2102
            scheduleNG%(TC, 1) = 2103
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 2 WINNER"
            homeTeam$(TC) = "GAME 3 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 10
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2106
            scheduleNG%(TC, 1) = 1901
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 6 WINNER"
            homeTeam$(TC) = "REGION 1 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 11
            ROUND = 3
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2107
            scheduleNG%(TC, 1) = 2108
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 7 WINNER"
            homeTeam$(TC) = "GAME 8 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 12
            ROUND = 3
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2109
            scheduleNG%(TC, 1) = 1110
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 9 WINNER"
            homeTeam$(TC) = "GAME 10 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            If consolationSetting(1) <> 0 Then
                TC = TC + 1
                REGION = 17
                GAME = 13
                ROUND = 98
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 1012
                scheduleNG%(TC, 1) = 1011
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 12 LOSER"
                homeTeam$(TC) = "GAME 11 LOSER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

                TC = TC + 1
                REGION = 17
                GAME = 14
                ROUND = 99
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 2112
                scheduleNG%(TC, 1) = 2111
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 12 WINNER"
                homeTeam$(TC) = "GAME 11 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            Else

                TC = TC + 1
                REGION = 17
                GAME = 13
                ROUND = 99
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 2111
                scheduleNG%(TC, 1) = 2112
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 11 WINNER"
                homeTeam$(TC) = "GAME 12 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            End If

        Case 15:

            '15 REGIONS
            TC = TC + 1
            REGION = 17
            GAME = 1
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1915
            scheduleNG%(TC, 1) = 1902
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 15 WINNER"
            homeTeam$(TC) = "REGION 2 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 2
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1914
            scheduleNG%(TC, 1) = 1903
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 14 WINNER"
            homeTeam$(TC) = "REGION 3 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 3
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1913
            scheduleNG%(TC, 1) = 1904
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 13 WINNER"
            homeTeam$(TC) = "REGION 4 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 4
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1912
            scheduleNG%(TC, 1) = 1905
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 12 WINNER"
            homeTeam$(TC) = "REGION 5 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 5
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1911
            scheduleNG%(TC, 1) = 1906
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 11 WINNER"
            homeTeam$(TC) = "REGION 6 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 6
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1910
            scheduleNG%(TC, 1) = 1907
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 10 WINNER"
            homeTeam$(TC) = "REGION 7 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 7
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1909
            scheduleNG%(TC, 1) = 1908
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 9 WINNER"
            homeTeam$(TC) = "REGION 8 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 8
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2101
            scheduleNG%(TC, 1) = 2106
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 1 WINNER"
            homeTeam$(TC) = "GAME 6 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 9
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2102
            scheduleNG%(TC, 1) = 2105
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 2 WINNER"
            homeTeam$(TC) = "GAME 5 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 10
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2103
            scheduleNG%(TC, 1) = 2104
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 3 WINNER"
            homeTeam$(TC) = "GAME 4 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 11
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2107
            scheduleNG%(TC, 1) = 1901
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 7 WINNER"
            homeTeam$(TC) = "REGION 1 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 12
            ROUND = 3
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2109
            scheduleNG%(TC, 1) = 2108
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 9 WINNER"
            homeTeam$(TC) = "GAME 8 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 13
            ROUND = 3
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2110
            scheduleNG%(TC, 1) = 2111
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 10 WINNER"
            homeTeam$(TC) = "GAME 11 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            If consolationSetting(1) <> 0 Then
                TC = TC + 1
                REGION = 17
                GAME = 14
                ROUND = 98
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 2012
                scheduleNG%(TC, 1) = 2013
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 12 LOSER"
                homeTeam$(TC) = "GAME 13 LOSER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

                TC = TC + 1
                REGION = 17
                GAME = 15
                ROUND = 99
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 2112
                scheduleNG%(TC, 1) = 2113
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 12 WINNER"
                homeTeam$(TC) = "GAME 13 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            Else
                TC = TC + 1
                REGION = 17
                GAME = 14
                ROUND = 99
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 2112
                scheduleNG%(TC, 1) = 2113
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 12 WINNER"
                homeTeam$(TC) = "GAME 13 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            End If

        Case 16:

            '16 REGIONS
            TC = TC + 1
            REGION = 17
            GAME = 1
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1916
            scheduleNG%(TC, 1) = 1901
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 16 WINNER"
            homeTeam$(TC) = "REGION 1 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 2
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1915
            scheduleNG%(TC, 1) = 1902
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 15 WINNER"
            homeTeam$(TC) = "REGION 2 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 3
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1914
            scheduleNG%(TC, 1) = 1903
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 14 WINNER"
            homeTeam$(TC) = "REGION 3 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 4
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1913
            scheduleNG%(TC, 1) = 1904
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 13 WINNER"
            homeTeam$(TC) = "REGION 4 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 5
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1912
            scheduleNG%(TC, 1) = 1905
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 12 WINNER"
            homeTeam$(TC) = "REGION 5 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 6
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1911
            scheduleNG%(TC, 1) = 1906
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 11 WINNER"
            homeTeam$(TC) = "REGION 6 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 7
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1910
            scheduleNG%(TC, 1) = 1907
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 10 WINNER"
            homeTeam$(TC) = "REGION 7 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 8
            ROUND = 1
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 1909
            scheduleNG%(TC, 1) = 1908
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "REGION 9 WINNER"
            homeTeam$(TC) = "REGION 8 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 9
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2101
            scheduleNG%(TC, 1) = 2108
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 1 WINNER"
            homeTeam$(TC) = "GAME 8 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 10
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2104
            scheduleNG%(TC, 1) = 2105
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 4 WINNER"
            homeTeam$(TC) = "GAME 5 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 11
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2103
            scheduleNG%(TC, 1) = 2106
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 3 WINNER"
            homeTeam$(TC) = "GAME 6 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 12
            ROUND = 2
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2102
            scheduleNG%(TC, 1) = 2107
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 2 WINNER"
            homeTeam$(TC) = "GAME 7 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 13
            ROUND = 3
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2109
            scheduleNG%(TC, 1) = 2110
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 9 WINNER"
            homeTeam$(TC) = "GAME 10 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            TC = TC + 1
            REGION = 17
            GAME = 14
            ROUND = 3
            Call AssignGameSettings

            scheduleNG%(TC, 0) = 2111
            scheduleNG%(TC, 1) = 2112
            scheduleNG%(TC, 15) = REGION
            scheduleNG%(TC, 16) = ROUND
            scheduleNG%(TC, 17) = GAME
            visitingTeam$(TC) = "GAME 11 WINNER"
            homeTeam$(TC) = "GAME 12 WINNER"
            yearNumber$(TC, 0) = "XXX  "
            yearNumber$(TC, 1) = "XXX  "
            Call WRITEGAME(saveFile$)

            If consolationSetting(1) <> 0 Then
                TC = TC + 1
                REGION = 17
                GAME = 15
                ROUND = 98
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 2014
                scheduleNG%(TC, 1) = 2013
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 14 LOSER"
                homeTeam$(TC) = "GAME 13 LOSER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

                TC = TC + 1
                REGION = 17
                GAME = 16
                ROUND = 99
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 2114
                scheduleNG%(TC, 1) = 2113
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 14 WINNER"
                homeTeam$(TC) = "GAME 13 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            Else

                TC = TC + 1
                REGION = 17
                GAME = 15
                ROUND = 99
                Call AssignGameSettings

                scheduleNG%(TC, 0) = 2114
                scheduleNG%(TC, 1) = 2113
                scheduleNG%(TC, 15) = REGION
                scheduleNG%(TC, 16) = ROUND
                scheduleNG%(TC, 17) = GAME
                visitingTeam$(TC) = "GAME 14 WINNER"
                homeTeam$(TC) = "GAME 13 WINNER"
                yearNumber$(TC, 0) = "XXX  "
                yearNumber$(TC, 1) = "XXX  "
                Call WRITEGAME(saveFile$)

            End If

    End Select

End Sub


'----------------------------------------
'     AssignGameSettings Subroutine
'----------------------------------------
'This subroutine assigns the tournament
'settings to the current game, using the
'defaults set earlier.
Sub AssignGameSettings ()

    Shared TC
    Shared scheduleNG%(), tourneyDefaults()

    For I = 0 To 14
        scheduleNG%(TC, I) = tourneyDefaults(I)
    Next

End Sub

'----------------------------------------
'         WRITEGAME Subroutine
'----------------------------------------
'This subroutine writes the current game
'being considered to temporary files.
'Both .TMP and .TCX files are used, which
'serve as "schedule" files of sorts for
'tournaments.
'The .TMP file becomes the final .TOU
'file for play later, while the .TCX file
'is a backup of initial settings if the
'tournament is reset.

Sub WRITEGAME (outputFile$)

    Shared TC

    Shared homeScores(), scheduleNG%(), visitorScores()
    Shared homeTeam$(), visitingTeam$(), yearNumber$()

    'WRITE OUT A GAME
    'TMP file
    Open diskPaths$(1) + outputFile$ + ".TMP" For Random As #1 Len = TOURNEY_SIZE_BYTES

    For X = 0 To 14
        Field #1, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
    Next

    Field #1, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
    Field #1, 64 As X$, 5 As Q$(20), 5 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
    Field #1, 88 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)

    For X = 0 To 14:
        LSet Q$(X + 1) = MKI$(scheduleNG%(TC, X))
    Next

    LSet Q$(16) = MKI$(visitorScores(TC))
    LSet Q$(17) = MKI$(homeScores(TC))
    LSet Q$(18) = visitingTeam$(TC)
    LSet Q$(19) = homeTeam$(TC)

    For X = 0 To 3:
        LSet Q$(20 + X) = yearNumber$(TC, X)
    Next

    For X = 24 To 26
        LSet Q$(X) = MKI$(scheduleNG%(TC, X - 9))
    Next

    Put #1, TC

    Close #1

    'TCX file
    Open diskPaths$(1) + outputFile$ + ".TCX" For Random As #2 Len = TOURNEY_SIZE_BYTES

    For X = 0 To 14
        Field #2, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
    Next

    Field #2, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
    Field #2, 64 As X$, 5 As Q$(20), 5 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
    Field #2, 88 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)

    For X = 0 To 14:
        LSet Q$(X + 1) = MKI$(scheduleNG%(TC, X))
    Next

    For X = 0 To 3:
        LSet Q$(20 + X) = yearNumber$(TC, X)
    Next

    LSet Q$(16) = MKI$(visitorScores(TC))
    LSet Q$(17) = MKI$(homeScores(TC))
    LSet Q$(18) = visitingTeam$(TC)
    LSet Q$(19) = homeTeam$(TC)

    For X = 24 To 26
        LSet Q$(X) = MKI$(scheduleNG%(TC, X - 9))
    Next

    Put #2, TC

    Close #2

End Sub

'----------------------------------------
'       SaveTourneyInfo Subroutine
'----------------------------------------
'This subroutine saves the tournament
'"INFO" file (.INF), then relies on a
'separate routine to sort the games and
'save them to the .TOU file.
Sub SaveTourneyInfo (nbrRegions, saveFile$, tourneyName$)

    Shared TC

    Shared regionNames$(), slotName$()

    Shared consolationSetting(), nbrRegionalTeams(), tourneyParams()

    'SAVE TOURNAMENT INFO AND QUIT
    Open diskPaths$(1) + saveFile$ + ".INF" For Output As #2

    Print #2, tourneyName$
    Print #2, nbrRegions ' NUMBER OF REGIONS

    For I = 0 To 16
        Print #2, nbrRegionalTeams(I) ' NUMBER OF TEAMS IN REGION
        Print #2, regionNames$(I) ' NAME OF REGION
    Next

    For I = 0 To 16
        For I1 = 0 To 16
            For I2 = 0 To 3
                Print #2, slotName$(I, I1, I2) ' SEED SLOT NAME
                Print #2, tourneyParams(I, I1, I2) ' REGION, SEED NUMBER, MODE PLAY/TM#
            Next
        Next
    Next

    Print #2, consolationSetting(0)
    Print #2, consolationSetting(1): ' CONSOLATION GAME

    Close #2

    Call SortTourney

    Call SaveTourneyGames(nbrRegions, saveFile$)

    Locate 22, 25: Print Space$(34)

    MsgText$ = "ALL FILES SAVED...PRESS ANY KEY TO RETURN TO TOURNAMENT OPTIONS"
    BoxWidth = 18
    BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
    Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
    Call CSMsgBox(MsgText$, BoxWidth, PASS)
    I$ = GetKeyPress$
    Call CSMsgBox("", 0, PASS)

End Sub


'----------------------------------------
'      SaveTourneyGames Subroutine
'----------------------------------------
'This subroutine reads data from a .TMP
'file created earlier in the process. If
'then performs a sort on all games to be
'scheduled and creates the final .TOU file.
Sub SaveTourneyGames (nbrRegions, tourneyFile$)

    Shared TC

    Shared homeTeam$(), visitingTeam$(), yearNumber$()

    Shared homeScores(), scheduleNG%(), visitorScores()

    For RD = 1 To 89

        If Not (RD > 5 And RD < 88 Or RD > 89) Then

            Locate 22, 25: Print "SCHEDULING GAMES FOR BRACKET"; nbrRegions

            For currRegion = 0 To 16

                If currRegion <= nbrRegions Then

                    For AP = 1 To TC

                        'Read data from .TMP file
                        Open diskPaths$(1) + tourneyFile$ + ".TMP" For Random As #1 Len = TOURNEY_SIZE_BYTES

                        For X = 0 To 14
                            Field #1, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
                        Next

                        Field #1, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
                        Field #1, 64 As X$, 5 As Q$(20), 5 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
                        Field #1, 88 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)

                        Get #1, AP

                        For X = 0 To 14:
                            scheduleNG%(0, X) = CVI(Q$(X + 1))
                        Next
                        For X = 0 To 3:
                            yearNumber$(0, X) = Q$(20 + X)
                        Next

                        visitorScores(0) = CVI(Q$(16))
                        homeScores(0) = CVI(Q$(17))
                        visitingTeam$(0) = Q$(18)
                        homeTeam$(0) = Q$(19)

                        For X = 1 To 3
                            scheduleNG%(0, X + 14) = CVI(Q$(X + 23))
                        Next

                        Close #1

                        'Create matches in .TOU file
                        If scheduleNG%(0, 16) = RD And scheduleNG%(0, 15) = currRegion Then

                            MATCH = MATCH + 1

                            Open diskPaths$(1) + tourneyFile$ + ".TOU" For Random As #2 Len = TOURNEY_SIZE_BYTES

                            For X = 0 To 14
                                Field #2, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
                            Next

                            Field #2, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
                            Field #2, 64 As X$, 5 As Q$(20), 5 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
                            Field #2, 88 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)

                            For X = 0 To 14:
                                LSet Q$(X + 1) = MKI$(scheduleNG%(0, X))
                            Next
                            For X = 0 To 3:
                                LSet Q$(20 + X) = yearNumber$(0, X)
                            Next

                            LSet Q$(16) = MKI$(visitorScores(0))
                            LSet Q$(17) = MKI$(homeScores(0))
                            LSet Q$(18) = visitingTeam$(0)
                            LSet Q$(19) = homeTeam$(0)

                            For X = 24 To 26
                                LSet Q$(X) = MKI$(scheduleNG%(0, X - 9))
                            Next

                            Put #2, MATCH

                            Close #2

                        End If

                    Next AP 'Done looping through all games

                End If 'Done checking if we're still within a valid region

            Next currRegion 'Done processing all regions

        End If

    Next RD

    currRegion = 17

    tempFile$ = diskPaths$(1) + tourneyFile$ + ".TMP"

    For RD = 1 To 99

        If Not (RD > 5 And RD < 88 Or RD > 89 And RD < 98) Then

            For AP = 1 To TC

                Open tempFile$ For Random As #1 Len = TOURNEY_SIZE_BYTES

                For X = 0 To 14
                    Field #1, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
                Next

                Field #1, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
                Field #1, 64 As X$, 5 As Q$(20), 5 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
                Field #1, 88 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)

                Get #1, AP

                For X = 0 To 14:
                    scheduleNG%(0, X) = CVI(Q$(X + 1))
                Next

                For X = 0 To 3:
                    yearNumber$(0, X) = Q$(20 + X)
                Next

                visitorScores(0) = CVI(Q$(16))
                homeScores(0) = CVI(Q$(17))
                visitingTeam$(0) = Q$(18)
                homeTeam$(0) = Q$(19)

                For X = 1 To 3:
                    scheduleNG%(0, X + 14) = CVI(Q$(X + 23))
                Next

                Close #1

                If scheduleNG%(0, 16) = RD And scheduleNG%(0, 15) = currRegion Then

                    MATCH = MATCH + 1

                    Open diskPaths$(1) + tourneyFile$ + ".TOU" For Random As #2 Len = TOURNEY_SIZE_BYTES

                    For X = 0 To 14
                        Field #2, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
                    Next

                    Field #2, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
                    Field #2, 64 As X$, 5 As Q$(20), 5 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
                    Field #2, 88 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)

                    For X = 0 To 14:
                        LSet Q$(X + 1) = MKI$(scheduleNG%(0, X))
                    Next

                    For X = 0 To 3:
                        LSet Q$(20 + X) = yearNumber$(0, X)
                    Next

                    LSet Q$(16) = MKI$(visitorScores(0))
                    LSet Q$(17) = MKI$(homeScores(0))
                    LSet Q$(18) = visitingTeam$(0)
                    LSet Q$(19) = homeTeam$(0)

                    For X = 24 To 26:
                        LSet Q$(X) = MKI$(scheduleNG%(0, X - 9))
                    Next

                    Put #2, MATCH

                    Close #2

                End If

            Next AP

        End If

    Next RD

    If _FileExists(tempFile$) Then Kill tempFile$

End Sub


'----------------------------------------
'       SortTourney SubRoutine
'----------------------------------------
'This subroutine is used to automatically
'sort all games in the schedule by their
'date / time.
Sub SortTourney ()

    Shared scheduleAP%(), scheduleNG%()
    Shared homeScores(), visitorScores()
    Shared homeTeam$(), visitingTeam$()
    Shared yearNumber$()


    For Z = 1 To scheduleAP%(0) - 1

        For Z1 = Z + 1 To scheduleAP%(0)

            Locate 1, 1: Print Z, Z1

            If scheduleNG%(Z, 16) > scheduleNG%(Z1, 16) Then

                For I = 0 To 14:
                    Swap scheduleNG%(Z, I), scheduleNG%(Z1, I)
                Next

                For I = 0 To 3:
                    Swap yearNumber$(Z, I), yearNumber$(Z1, I)
                Next

                Swap homeTeam$(Z), homeTeam$(Z1)
                Swap visitingTeam$(Z), visitingTeam$(Z1)
                Swap homeScores(Z), homeScores(Z1)
                Swap visitorScores(Z), visitorScores(Z1)

                For I = 1 To 3:
                    Swap scheduleNG%(Z, I + 14), scheduleNG%(Z1, I + 14)
                Next

            End If

        Next Z1

    Next Z

    For Z = 1 To scheduleAP%(0) - 1

        For Z1 = Z + 1 To scheduleAP%(0)

            If scheduleNG%(Z, 15) > scheduleNG%(Z1, 15) Then

                For I = 0 To 14:
                    Swap scheduleNG%(Z, I), scheduleNG%(Z1, I)
                Next

                For I = 0 To 3:
                    Swap yearNumber$(Z, I), yearNumber$(Z1, I)
                Next

                Swap homeTeam$(Z), homeTeam$(Z1)
                Swap visitingTeam$(Z), visitingTeam$(Z1)
                Swap homeScores(Z), homeScores(Z1)
                Swap visitorScores(Z), visitorScores(Z1)

                For X = 0 To 14:
                    scheduleNG%(0, X) = CVI(Q$(X + 1))
                Next

                For X = 0 To 3:
                    yearNumber$(0, X) = Q$(20 + X)
                Next

                For I = 1 To 3:
                    Swap scheduleNG%(Z, I + 14), scheduleNG%(Z1, I + 14)
                Next

            End If

        Next Z1

    Next Z

End Sub
