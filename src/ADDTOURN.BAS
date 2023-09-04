'----------------------------------------
'           ADDTOURN Subroutine
'----------------------------------------
'This subroutine provides the main menu
'for users to interface with the various
'asepcts of tournament handling.
Sub ADDTOURN ()

    _Title "Courtside College Basketball - Create Tournament"

    Shared slotName$(), regionNames$()

    Shared consolationSetting%()
    Shared homeScores(), visitorScores()
    Shared tourneyN%(), scheduleNG%(), NN%()

    Dim NR$

    ReDim menuChoices$(1 To 6)

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
        menuChoices$(4) = "RESET A TOURNAMENT           "
        menuChoices$(5) = "PLAY TOURNAMENT GAMES        "
        menuChoices$(6) = "RETURN TO COURTSIDE MAIN MENU"
        Selection% = 1
        Count% = 6
        Start% = 1
        ExitCode% = 6
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 10
        Column% = 25

        Call box0(9, 24, 16, 54, 2, 15, 1)
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case Selection%

                Case 1:
                    newTourneyName$ = ""
                    NR$ = ""
                    tourneyFile$ = ""
                    Erase regionNames$
                    Erase slotName$
                    Call SetupTourney(NR$, Selection%, newTourneyName$, tourneyFile$)

                Case 2, 4:
                    For I = 1 To 16
                        NN%(I) = 0
                        regionNames$(I) = ""
                    Next

                    For I = 1 To 16
                        For I1 = 1 To 16
                            For I2 = 0 To 3
                                slotName$(I, I1, I2) = ""
                                tourneyN%(I, I1, I2) = 0
                            Next
                        Next
                    Next

                    consolationSetting%(0) = 0
                    consolationSetting%(1) = 0

                    Call LoadTourney(NR%, Selection%, tn$)

                Case 3: Call BRACKET
                Case 5: Call TOURNEY
                Case 6: Cls

            End Select

        End If

    Loop Until Selection% = 6 Or ExitCode% = 27

End Sub


'----------------------------------------
'        SetupTourney Subroutine
'----------------------------------------
'This subroutine presents the user the
'general options for the tournament being
'setup. It then steps the user through
'other screens for tournament setup - which
'all rely on separate routine.
Sub SetupTourney (numberRegions$, ACTION%, tourneyName$, sourceFile$)

    Dim HC$(2)
    Dim SC$(2), TE$(1), TF$(1)

    Dim tourneyCM$(1), tourneyTS$(1)

    Shared tourneyDefaults%()

    Dim U6!

    Shared slotName$(), regionNames$()
    Shared consolationSetting%(), teamRatings%()
    Shared tourneyN%(), scheduleNG%(), NN%()

    tourneyDefaults%(3) = 1
    tourneyDefaults%(7) = 2
    tourneyDefaults%(5) = 5
    tourneyDefaults%(6) = 1
    tourneyDefaults%(10) = 1
    tourneyDefaults%(14) = 0

    tourneyTS$(0) = "N"
    tourneyTS$(1) = "Y"
    SC$(0) = "NO SHOT CLOCK"
    SC$(1) = "45 SEC CLOCK "
    SC$(2) = "35 SEC CLOCK "
    'SC$(3) = "30 SEC CLOCK"
    HC$(1) = "Y"
    HC$(2) = "N"
    TF$(0) = "N"
    TF$(1) = "Y"
    TE$(0) = "SHOOT 1+1"
    TE$(1) = "SHOOT 2  "
    tourneyCM$(0) = "COACH AND SELECT SHOTS           "
    tourneyCM$(1) = "COACH ONLY AND LET COMPUTER SHOOT"

    If consolationSetting%(0) = 1 Then C$ = "Y" Else C$ = "N"

    'GLOBAL SETUP TOURNEY DEFAULTS
    Cls

    Color , 4
    Locate 1, 1: Print Tab(80);

    Color 0
    Locate , 19: Print "SELECT GENERAL TOURNAMENT OPTIONS"

    Locate 25, 1: Print Tab(80);
    Locate , 1: Color , 0

    Do
        Locate 5, 
        Color 14: Print " (1) ";
        Color 15: Print "3-POINT SHOT: "; tourneyTS$(tourneyDefaults%(3))
        Color 14: Print " (2) ";
        Color 15: Print SC$(tourneyDefaults%(7))
        Color 14: Print " (3) ";
        Color 15: Print Using "DISPLAY PAUSE IN SECONDS: ##.#  "; tourneyDefaults%(4) / 10
        Color 14: Print " (4) ";
        Color 15: Print "FOULS TO DISQUALIFY: "; tourneyDefaults%(5)
        Color 14: Print " (5) ";
        Color 15: Print "FREE THROWS ON 10TH FOUL "; TE$(tourneyDefaults%(6))
        Color 14: Print " (6) ";
        Color 15: Print "3-FT WHEN FOULED ON 3-PT ATTEMPT "; TF$(tourneyDefaults%(10))
        Color 14: Print " (7) ";
        Color 15: Print "1 FT ON 1ST 6 FLOOR FOULS (PRIOR TO 1973): "; TF$(tourneyDefaults%(14))
        Color 14: Print " (8) ";
        Color 15: Print "ACCEPT SETTINGS"

        Do
            I$ = GetKeyPress$

            If I$ <> Chr$(27) Then
                I = Val(I$)
            End If

        Loop Until I >= 1 Or I <= 8

        If I$ = Chr$(27) Then Exit Sub

        Select Case I
            Case 1: tourneyDefaults%(3) = 1 - tourneyDefaults%(3)
            Case 2: tourneyDefaults%(7) = tourneyDefaults%(7) + 1: If tourneyDefaults%(7) > 2 Then tourneyDefaults%(7) = 0
            Case 3:
                Locate 7, 32
                Input U6!
                tourneyDefaults%(4) = U6! * 10
            Case 4: tourneyDefaults%(5) = tourneyDefaults%(5) + 1: If tourneyDefaults%(5) > 6 Then tourneyDefaults%(5) = 5
            Case 5: tourneyDefaults%(6) = 1 - tourneyDefaults%(6)
            Case 6: tourneyDefaults%(10) = 1 - tourneyDefaults%(10)
            Case 7: tourneyDefaults%(14) = 1 - tourneyDefaults%(14)
            Case 8: '
        End Select

    Loop Until I = 8

    Color 15, 1
    FG = 0
    BG = 7

    GetData:
    Cls
    Color , 6
    Locate 1, 1: Print Tab(80);
    Color 15
    Locate , 22: Print "TOURNAMENT  - GENERAL INFORMATION"

    Locate 25, 1: Print Tab(80);
    Locate , 1: Color , 0

    Locate 7, 1: QPrint "FILE NAME (48 CHARACTERS) ", 14, 1, -1
    Locate , 27: QPrint PadRight$(sourceFile$, 48), FG, BG, -1

    Locate 9, 1: QPrint "NAME OF TOURNAMENT ", 14, 1, -1
    Locate , 20: QPrint PadRight$(tourneyName$, 55), FG, BG, -1

    Locate 11, 1: QPrint "CONSOLATION GAMES IN REGIONALS/FINALS ", 14, 1, -1
    Locate , 40: QPrint PadRight$(C$, 1), FG, BG, -1

    Locate 13, 1: QPrint "NUMBER OF BRACKETS/REGIONS (1-16) ", 14, 1, -1
    Locate , 35: QPrint PadRight$(numberRegions$, 2), FG, BG, -1

    GETDIV1:
    Locate 7, 26 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 32 'specify field parameters
    Call TextIn(sourceFile$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GETDIV1: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data
    sourceFile$ = _Trim$(sourceFile$)

    GetDiv2:
    Locate 9, 20 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 40 'specify field parameters
    Call TextIn(tourneyName$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GETDIV1: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data
    tourneyName$ = _Trim$(tourneyName$)

    GetDiv3:
    Locate 11, 40 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 1 'specify field parameters
    Call TextIn(C$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv2: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data
    C$ = _Trim$(C$)

    GETDIV4:
    Locate 13, 35 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 2 'specify field parameters
    Call TextIn(numberRegions$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv3: 'can't back up any more, stay here
    numberRegions$ = _Trim$(numberRegions$)

    Done:
    Color 14, 1
    Locate 23, 1
    Print "Is this okay? (Yes/No) ";
    X$ = ""
    Call YesNoB(X$, XCode, FG, BG)
    Locate , 1
    ClearEOL 14, 1
    If XCode = 1 GoTo GETDIV4

    If Not (XCode = 2) Then

        If X$ = "N" GoTo GETDIV1
        If X$ <> "Y" GoTo Done 'insist on either Y or N or up-arrow

        If UCase$(C$) = "Y" Then consolationSetting%(0) = 1 Else consolationSetting%(0) = 0

        consolationSetting%(1) = consolationSetting%(0)

        If sourceFile$ = "" Then

            MsgText$ = "TOURNAMENT FILE NAME IS BLANK PRESS ANY KEY TO CONTINUE SELECTING"
            BoxWidth = 18
            BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
            Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
            Call CSMsgBox(MsgText$, BoxWidth, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)

            GoTo GetData
            
        Else

            numberRegions = Val(numberRegions$)

            If numberRegions < 1 Or numberRegions > 16 Then GoTo GETDIV4

            Open diskPaths$(1) + sourceFile$ + ".TNY" For Output As #1

            For I = 0 To 14
                Print #1, tourneyDefaults%(I)
            Next

            Close #1

            If ACTION% = 2 Then

                Call EditBracket_TOURN(NT$, sourceFile$, numberRegions, tourneyName$)

            Else

                For NR = 1 To numberRegions

                    Call SeedBrackets(NR, NT$, sourceFile$, numberRegions, tourneyName$)

                Next

                Call ScheduleAndSaveTourney(sourceFile$, numberRegions, tourneyName$)

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
Sub SeedBrackets (NR, NT$, sourceFile$, numberRegions, tourneyName$)

    Shared slotName$(), regionNames$()
    Shared consolationSetting%(), teamRatings%()
    Shared tourneyN%(), scheduleNG%(), NN%()

    Cls
    Color 15, 1
    FG = 0
    BG = 7
    'Colr = OneColor%(FG, BG) 'pack FG and BG into a single byte
    Cls
    Color , 6
    Locate 1, 1: Print Tab(80);
    Color 15
    Locate , 26: Print "REGIONAL/BRACKET INFORMATION"
    Locate 25, 1: Print Tab(80);

    Color , 0
    Locate , 1
    X$ = "NAME OF REGION" + Str$(NR) + " "

    Locate 7, 1: QPrint X$, 14, 1, -1
    Locate , 26: QPrint PadRight$(regionNames$(NR), 16), FG, BG, -1
    Locate 9, 1: QPrint "NUMBER OF TEAMS IN REGION (4-16) ", 14, 1, -1
    Locate , 34: QPrint PadRight$(NT$, 2), FG, BG, -1

    NameRegion:
    Locate 7, 26 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 16 'specify field parameters
    Call TextIn(regionNames$(NR), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo REGIONSIZE: 'can't back up any more, stay here
    If XCode = 2 GoTo Done1 'all done entering data
    regionNames$(NR) = _Trim$(regionNames$(NR))

    REGIONSIZE:
    Locate 9, 34 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 2 'specify field parameters
    Call TextIn(NT$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo NameRegion: 'can't back up any more, stay here
    NT$ = _Trim$(NT$)

    Done1:
    Color 14, 1
    Locate 23, 1
    Print "Is this okay? (Yes/No) ";
    X$ = ""

    Call YesNoB(X$, XCode, FG, BG)
    Locate , 1
    ClearEOL 14, 1
    If XCode = 1 GoTo REGIONSIZE

    If Not (X$ = Chr$(27)) Then

        If X$ = "N" GoTo NameRegion
        If X$ <> "Y" GoTo Done1 'insist on either Y or N or up-arrow

        NN%(NR) = Val(NT$)
        If NN%(NR) < 4 Or NN%(NR) > 16 Then GoTo REGIONSIZE

        Do
            Color , 0

            Cls

            Color , 6
            Locate 1, 1: Print Tab(80);
            Color 15: Locate , 26

            Xcoord = Len(regionNames$(NR) + " REGIONAL/CONFERENCE SEEDING")
            Xcoord = (80 - Xcoord) / 2
            Locate , Xcoord
            Print regionNames$(NR) + " REGIONAL/CONFERENCE SEEDING"

            Locate 25, 1: Print Tab(80);
            Locate , 1
            Color 12, 0

            MAXSEED = NN%(NR) + 1 + 4
            Call box0(3, 24, MAXSEED, 54, 2, 15, 1)

            Locate 4, 25: Print "SEED  TEAM"

            For I1 = 1 To NN%(NR)
                Color 2
                Locate , 25
                Print Using "##    "; I1;
                Color 15: Print slotName$(NR, I1, 0)
            Next I1

            Locate 25, 1
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

                        For I = 1 To NN%(NR)

                            If slotName$(NR, I, 0) = "" Then

                                MsgText$ = "ALL SEEDS HAVE NOT BEEN FILLED PRESS ANY KEY TO CONTINUE SELECTING"
                                BoxWidth = 18
                                BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                                Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                                Call CSMsgBox(MsgText$, BoxWidth, Pass)
                                I$ = GetKeyPress$
                                Call CSMsgBox("", 0, Pass)

                                NF = 1
                                'Return
                                Exit For
                            End If

                        Next

                        If NF = 1 Then
                            Call EditBracket_TOURN(NT$, sourceFile$, numberRegions, tourneyName$)
                            _Continue
                        End If

                    End If 'Done for value 3

                    If NF = 1 Then NF = 0

                End If

            Loop Until Val(I$) >= 1 And Val(I$) <= 3

            If I$ <> Chr$(27) Then

                If Val(I$) <> 3 Then

                    'Add / Remove Teams from Seeding
                    Do
                        Locate 22, 30
                        Print "SEED # (1-"; Str$(NN%(NR)); ") ";
                        Input B
                    Loop Until B >= 1 And B <= NN%(NR)

                    If Val(I$) = 1 Then

                        Call ChooseTeam (YN$, NTMS, teamNbr, team$)

                        If teamNbr <> 0 Then

                            slotName$(NR, B, 0) = team$
                            slotName$(NR, B, 1) = YN$
                            tourneyN%(NR, B, 0) = teamNbr
                            tourneyN%(NR, B, 1) = 3
                            tourneyN%(NR, B, 2) = teamRatings%(0)
                            tourneyN%(NR, B, 3) = teamRatings%(1)
                            tourneyN%(NR, B, 1) = 1
                                                        
                        End If

                    Else
                        'Clear a slot
                        slotName$(NR, B, 0) = ""

                    End If

                End If 'Done checking for value 3

            End If 'Done checking for ESC

        Loop Until Val(I$) = 3

    End If 'Done checking for ESC

End Sub


'----------------------------------------
'      EditBracket_TOURN Subroutine
'----------------------------------------
'This subroutine gives the user a menu of
'regions that were named previously. For 
'any region chosen, it calls a  separate 
'subroutine to assist with populating the
'seedings for the region. When done, it 
'then calls a routine to create the schedule 
'for the tournament and save the file.
Sub EditBracket_TOURN (numberTeams$, saveFile$, numberRegions, tourneyName$)

    Shared TC!

    Shared regionNames$()
    Shared NN%()

    ReDim menuChoices$(1 To numberRegions + 1)

    Do

        'EDIT A GIVEN BRACKET
        Color , 0
        Cls

        Color , 6
        Locate 1, 1: Print Tab(80);

        Locate , 23: Color 15
        Print "TOURNAMENT  - REGION/BRACKET EDITOR"

        Locate 25, 1: Print Tab(80);

        Locate , 1
        Color , 0

        For I = 1 To numberRegions
            If regionNames$(I) = "" Then regionNames$(I) = "TO BE NAMED"
            menuChoices$(I) = regionNames$(I)
        Next

        Selection% = 1
        Count% = numberRegions + 1
        Start% = 1
        ExitCode% = numberRegions + 1
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 4
        Column% = 32

        menuChoices$(numberRegions + 1) = "SAVE AND EXIT"
        Call box0(3, 31, 3 + numberRegions + 2, 48, 2, 15, 1)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0
        If ExitCode% <> 27 And ExitCode% <> 0 Then

            'Save and Exit
            If Selection% < numberRegions + 1 Then
                numberTeams$ = _Trim$(Str$(NN%(Selection%)))
                Call SeedBrackets(Selection%, numberTeams$, saveFile$, numberRegions, tourneyName$)

            End If

        End If

    Loop Until Selection% = numberRegions + 1

    Call ScheduleAndSaveTourney(saveFile$, numberRegions, tourneyName$)

End Sub


'----------------------------------------
'   ScheduleAndSaveTourney Subroutine
'----------------------------------------
'This subroutine uses the data loaded so
'far for the tournament, then calls routines
'to generate the appropriate schedule.
Sub ScheduleAndSaveTourney (saveFile$, numberRegions, tourneyName$)

    Shared NN%()

    If _FileExists(diskPaths$(1) + saveFile$ + ".TMP") Then Kill diskPaths$(1) + saveFile$ + ".TMP"
    If _FileExists(diskPaths$(1) + saveFile$ + ".TCX") Then Kill diskPaths$(1) + saveFile$ + ".TCX"
    If _FileExists(diskPaths$(1) + saveFile$ + ".TOU") Then Kill diskPaths$(1) + saveFile$ + ".TOU"

    For NR = 1 To numberRegions
        If NN%(NR) >= 4 Then
            var% = NN%(NR) - 3
            Call PickIt(var%, NR, saveFile$)
        End If
    Next

    Call CHAMPIONSHIP(saveFile$, numberRegions, tourneyName$)

End Sub


'----------------------------------------
'         LoadTourney Subroutine
'----------------------------------------
'This file opens a previously saved set of
'tournament files and reads in the data.
'It then proceeds to call the routine for
'editing tournament files.
Sub LoadTourney (numberRegions, ACTION%, tourneyName$)

    Dim numberRegions$

    Shared slotName$(), regionNames$()
    Shared consolationSetting%()
    Shared tourneyN%(), NN%()
    Shared tourneyDefaults%()
    
    tourneyFile$ = ChooseFile$ (diskPaths$(1) + "*.TOU", "TOURNAMENT", ExitCode%)

    If ExitCode% <> 27 Then

        If ACTION% = 4 Then
            'Reset tournament

            'Create TOU file as backup of TCX
            Call FCopy(diskPaths$(1) + tourneyFile$ + ".TCX", diskPaths$(1) + tourneyFile$ + ".TOU", BUFFER$, ERRCODE%)

            MsgText$ = "ALL FILES SAVED...PRESS ANY KEY TO RETURN TO TOURNAMENT OPTIONS"
            BoxWidth = 18
            BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
            Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
            Call CSMsgBox(MsgText$, BoxWidth, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)

            ACTION% = 1

        Else

            Open diskPaths$(1) + tourneyFile$ + ".INF" For Input As #1

            Input #1, tourneyName$

            Input #1, numberRegions ' NUMBER OF REGIONS

            For I = 1 To 16
                Input #1, NN%(I): ' NUMBER OF TEAMS IN REGION
                Input #1, regionNames$(I) ' NAME OF REGION
            Next

            For I = 1 To 16
                For I1 = 1 To 16
                    For I2 = 0 To 3
                        Input #1, slotName$(I, I1, I2) ' SEED SLOT NAME
                        Input #1, tourneyN%(I, I1, I2) ' REGION, SEED NUMBER, MODE PLAY/TM#
                    Next
                Next
            Next

            Input #1, consolationSetting%(0): Input #1, consolationSetting%(1): ' CONSOLATION GAME

            Close #1

            Open diskPaths$(1) + tourneyFile$ + ".TNY" For Input As #2

            For I = 0 To 14
                Input #2, tourneyDefaults%(I)
            Next

            Close #2

            If consolationSetting%(0) = 1 Then C$ = "Y" Else C$ = "N"

            numberRegions$ = Str$(numberRegions)

            Call SetupTourney(numberRegions$, ACTION%, tourneyName$, tourneyFile$)

        End If 'Done checking ACTION%

    End If

End Sub


'----------------------------------------
'       PickIt Subroutine
'----------------------------------------
'This subroutine loads the regular bracket
'games for the tournament, depending on the
'number of regions.
Sub PickIt (var%, currRegion, saveFile$)

    Shared TC!

    Shared homeTeam$(), visitingTeam$()
    Shared regionNames$(), slotName$(), yearNumber$()
    Shared consolationSetting%()
    Shared tourneyN%(), scheduleNG%(), NN%()

    Select Case var%

        Case 1:
            '4 TEAM REGION

            TC! = TC! + 1
            REGION = currRegion
            GAME = 1
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 3, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 2, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 3, 0)
            homeTeam$(TC!) = slotName$(currRegion, 2, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 3, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 2, 1)

            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 2
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 4, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 1, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 4, 0)
            homeTeam$(TC!) = slotName$(currRegion, 1, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 4, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 1, 1)
            Call WRITEGAME(saveFile$)

            REGION = currRegion

            If consolationSetting%(0) <> 0 Then '1-YES, 0-NO CONSOLATION
                TC! = TC! + 1
                GAME = 3
                ROUND = 88
                Call SETTINGS
                scheduleNG%(TC!, 0) = 801
                scheduleNG%(TC!, 1) = 802
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 1 LOSER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 2 LOSER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                TC! = TC! + 1
                REGION = currRegion
                GAME = 4
                ROUND = 89
                Call SETTINGS
                scheduleNG%(TC!, 0) = 901
                scheduleNG%(TC!, 1) = 902
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 1 WINNER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 2 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

            Else
                TC! = TC! + 1
                REGION = currRegion
                GAME = 3
                ROUND = 89
                Call SETTINGS
                scheduleNG%(TC!, 0) = 901
                scheduleNG%(TC!, 1) = 902
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 1 WINNER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 2 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

            End If

        Case 2:
            '5 TEAM REGION
            TC! = TC! + 1
            REGION = currRegion
            GAME = 1
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 5, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 4, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 5, 0)
            homeTeam$(TC!) = slotName$(currRegion, 4, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 5, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 4, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 2
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = 901
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 1, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 1 WINNER"
            homeTeam$(TC!) = slotName$(currRegion, 1, 0)
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = slotName$(currRegion, 1, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 3
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 3, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 2, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 3, 0)
            homeTeam$(TC!) = slotName$(currRegion, 2, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 3, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 2, 1)
            Call WRITEGAME(saveFile$)

            REGION = currRegion
            If consolationSetting%(0) <> 0 Then '1-YES, 0-NO CONSOLATION
                TC! = TC! + 1
                GAME = 4
                ROUND = 88
                Call SETTINGS
                scheduleNG%(TC!, 0) = 802
                scheduleNG%(TC!, 1) = 803
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 2 LOSER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 3 LOSER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                TC! = TC! + 1
                REGION = currRegion
                GAME = 5
                ROUND = 89
                Call SETTINGS
                scheduleNG%(TC!, 0) = 902
                scheduleNG%(TC!, 1) = 903
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 2 WINNER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 3 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

            Else

                TC! = TC! + 1
                REGION = currRegion
                GAME = 4
                ROUND = 89
                Call SETTINGS
                scheduleNG%(TC!, 0) = 902
                scheduleNG%(TC!, 1) = 903
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 2 WINNER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 3 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

            End If

        Case 3:
            '6 TEAM REGION
            TC! = TC! + 1
            REGION = currRegion
            GAME = 1
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 5, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 4, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 5, 0)
            homeTeam$(TC!) = slotName$(currRegion, 4, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 5, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 4, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 2
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 6, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 3, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 6, 0)
            homeTeam$(TC!) = slotName$(currRegion, 3, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 6, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 3, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 3
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = 901
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 1, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 1 WINNER"
            homeTeam$(TC!) = slotName$(currRegion, 1, 0)
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = slotName$(currRegion, 1, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 4
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = 902
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 2, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 2 WINNER"
            homeTeam$(TC!) = slotName$(currRegion, 2, 0)
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = slotName$(currRegion, 2, 1)
            Call WRITEGAME(saveFile$)

            REGION = currRegion
            If consolationSetting%(0) <> 0 Then '1-YES, 0-NO CONSOLATION
                TC! = TC! + 1
                GAME = 5
                ROUND = 88
                Call SETTINGS
                scheduleNG%(TC!, 0) = 803
                scheduleNG%(TC!, 1) = 804
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 3 LOSER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 4 LOSER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                TC! = TC! + 1
                REGION = currRegion
                GAME = 6
                ROUND = 89
                Call SETTINGS
                scheduleNG%(TC!, 0) = 903
                scheduleNG%(TC!, 1) = 904
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 3 WINNER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 4 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

            Else
                TC! = TC! + 1
                REGION = currRegion
                GAME = 5
                ROUND = 89
                Call SETTINGS
                scheduleNG%(TC!, 0) = 903
                scheduleNG%(TC!, 1) = 904
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 3 WINNER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 4 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

            End If

        Case 4:
            '7 TEAM REGION
            TC! = TC! + 1
            REGION = currRegion
            GAME = 1
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 6, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 3, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 6, 0)
            homeTeam$(TC!) = slotName$(currRegion, 3, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 6, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 3, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 2
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 7, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 2, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 7, 0)
            homeTeam$(TC!) = slotName$(currRegion, 2, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 7, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 2, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 3
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 5, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 4, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 5, 0)
            homeTeam$(TC!) = slotName$(currRegion, 4, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 5, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 4, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 4
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = 901
            scheduleNG%(TC!, 1) = 902
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 1 WINNER"
            homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 2 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 5
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = 903
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 1, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 3 WINNER"
            homeTeam$(TC!) = slotName$(currRegion, 1, 0)
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = slotName$(currRegion, 1, 1)
            Call WRITEGAME(saveFile$)

            REGION = currRegion
            If consolationSetting%(0) <> 0 Then '1-YES, 0-NO CONSOLATION
                TC! = TC! + 1
                GAME = 6
                ROUND = 88
                Call SETTINGS
                scheduleNG%(TC!, 0) = 804
                scheduleNG%(TC!, 1) = 805
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 4 LOSER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 5 LOSER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                TC! = TC! + 1
                REGION = currRegion
                GAME = 7
                ROUND = 89
                Call SETTINGS
                scheduleNG%(TC!, 0) = 904
                scheduleNG%(TC!, 1) = 905
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 4 WINNER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 5 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

            Else
                TC! = TC! + 1
                REGION = currRegion
                GAME = 6
                ROUND = 89
                Call SETTINGS
                scheduleNG%(TC!, 0) = 904
                scheduleNG%(TC!, 1) = 905
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 4 WINNER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 5 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

            End If

        Case 5:
            '8 TEAM REGION
            TC! = TC! + 1
            REGION = currRegion
            GAME = 1
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 7, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 2, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 7, 0)
            homeTeam$(TC!) = slotName$(currRegion, 2, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 7, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 2, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 2
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 6, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 3, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 6, 0)
            homeTeam$(TC!) = slotName$(currRegion, 3, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 6, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 3, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 3
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 5, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 4, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 5, 0)
            homeTeam$(TC!) = slotName$(currRegion, 4, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 5, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 4, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 4
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 8, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 1, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 8, 0)
            homeTeam$(TC!) = slotName$(currRegion, 1, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 8, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 1, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 5
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = 901
            scheduleNG%(TC!, 1) = 902
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 1 WINNER"
            homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 2 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 6
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = 903
            scheduleNG%(TC!, 1) = 904
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 3 WINNER"
            homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 4 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            REGION = currRegion
            If consolationSetting%(0) <> 0 Then '1-YES, 0-NO CONSOLATION
                TC! = TC! + 1
                GAME = 7
                ROUND = 88
                Call SETTINGS
                scheduleNG%(TC!, 0) = 805
                scheduleNG%(TC!, 1) = 806
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 5 LOSER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 6 LOSER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                TC! = TC! + 1
                REGION = currRegion
                GAME = 8
                ROUND = 89
                Call SETTINGS
                scheduleNG%(TC!, 0) = 905
                scheduleNG%(TC!, 1) = 906
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 5 WINNER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 6 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

            Else
                TC! = TC! + 1
                REGION = currRegion
                GAME = 7
                ROUND = 89
                Call SETTINGS
                scheduleNG%(TC!, 0) = 905
                scheduleNG%(TC!, 1) = 906
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 5 WINNER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 6 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

            End If

        Case 6:
            '9 TEAM REGION
            TC! = TC! + 1
            REGION = currRegion
            GAME = 1
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 9, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 8, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 9, 0)
            homeTeam$(TC!) = slotName$(currRegion, 8, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 9, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 8, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 2
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 7, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 2, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 7, 0)
            homeTeam$(TC!) = slotName$(currRegion, 2, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 7, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 2, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 3
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 6, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 3, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 6, 0)
            homeTeam$(TC!) = slotName$(currRegion, 3, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 6, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 3, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 4
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 5, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 4, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 5, 0)
            homeTeam$(TC!) = slotName$(currRegion, 4, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 5, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 4, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 5
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = 901
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 1, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 1 WINNER"
            homeTeam$(TC!) = slotName$(currRegion, 1, 0)
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = slotName$(currRegion, 1, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 6
            ROUND = 3
            Call SETTINGS
            scheduleNG%(TC!, 0) = 902
            scheduleNG%(TC!, 1) = 903
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 2 WINNER"
            homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 3 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 7
            ROUND = 3
            Call SETTINGS
            scheduleNG%(TC!, 0) = 904
            scheduleNG%(TC!, 1) = 905
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 4 WINNER"
            homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 5 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            REGION = currRegion
            If consolationSetting%(0) <> 0 Then '1-YES, 0-NO CONSOLATION
                TC! = TC! + 1
                GAME = 8
                ROUND = 88
                Call SETTINGS
                scheduleNG%(TC!, 0) = 806
                scheduleNG%(TC!, 1) = 807
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 6 LOSER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 7 LOSER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                TC! = TC! + 1
                REGION = currRegion
                GAME = 9
                ROUND = 89
                Call SETTINGS
                scheduleNG%(TC!, 0) = 906
                scheduleNG%(TC!, 1) = 907
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 6 WINNER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 7 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

            Else
                TC! = TC! + 1
                REGION = currRegion
                GAME = 8
                ROUND = 89
                Call SETTINGS
                scheduleNG%(TC!, 0) = 906
                scheduleNG%(TC!, 1) = 907
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 6 WINNER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 7 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

            End If

        Case 7:
            '10 TEAM REGION
            TC! = TC! + 1
            REGION = currRegion
            GAME = 1
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 10, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 7, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 10, 0)
            homeTeam$(TC!) = slotName$(currRegion, 7, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 10, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 7, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 2
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 9, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 8, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 9, 0)
            homeTeam$(TC!) = slotName$(currRegion, 8, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 9, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 8, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 3
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = 901
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 2, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 1 WINNER"
            homeTeam$(TC!) = slotName$(currRegion, 2, 0)
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = slotName$(currRegion, 2, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 4
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 6, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 3, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 6, 0)
            homeTeam$(TC!) = slotName$(currRegion, 3, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 6, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 3, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 5
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 5, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 4, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 5, 0)
            homeTeam$(TC!) = slotName$(currRegion, 4, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 5, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 4, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 6
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = 902
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 1, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 2 WINNER"
            homeTeam$(TC!) = slotName$(currRegion, 1, 0)
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = slotName$(currRegion, 1, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 7
            ROUND = 3
            Call SETTINGS
            scheduleNG%(TC!, 0) = 903
            scheduleNG%(TC!, 1) = 904
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 3 WINNER"
            homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 4 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 8
            ROUND = 3
            Call SETTINGS
            scheduleNG%(TC!, 0) = 905
            scheduleNG%(TC!, 1) = 906
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 5 WINNER"
            homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 6 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            REGION = currRegion
            If consolationSetting%(0) <> 0 Then '1-YES, 0-NO CONSOLATION
                TC! = TC! + 1
                GAME = 9
                ROUND = 88
                Call SETTINGS
                scheduleNG%(TC!, 0) = 807
                scheduleNG%(TC!, 1) = 808
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 7 LOSER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 8 LOSER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                TC! = TC! + 1
                REGION = currRegion
                GAME = 10
                ROUND = 89
                Call SETTINGS
                scheduleNG%(TC!, 0) = 907
                scheduleNG%(TC!, 1) = 908
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 7 WINNER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 8 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

            Else
                TC! = TC! + 1
                REGION = currRegion
                GAME = 9
                ROUND = 89
                Call SETTINGS
                scheduleNG%(TC!, 0) = 907
                scheduleNG%(TC!, 1) = 908
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 7 WINNER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 8 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

            End If

        Case 8:
            '11 TEAM REGION
            TC! = TC! + 1
            REGION = currRegion
            GAME = 1
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 10, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 7, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 10, 0)
            homeTeam$(TC!) = slotName$(currRegion, 7, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 10, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 7, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 2
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 11, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 6, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 11, 0)
            homeTeam$(TC!) = slotName$(currRegion, 6, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 11, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 6, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 3
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 9, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 8, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 9, 0)
            homeTeam$(TC!) = slotName$(currRegion, 8, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 9, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 8, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 4
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = 901
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 2, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 1 WINNER"
            homeTeam$(TC!) = slotName$(currRegion, 2, 0)
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = slotName$(currRegion, 2, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 5
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = 902
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 3, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 2 WINNER"
            homeTeam$(TC!) = slotName$(currRegion, 3, 0)
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = slotName$(currRegion, 3, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 6
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 5, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 4, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 5, 0)
            homeTeam$(TC!) = slotName$(currRegion, 4, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 5, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 4, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 7
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = 903
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 1, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 3 WINNER"
            homeTeam$(TC!) = slotName$(currRegion, 1, 0)
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = slotName$(currRegion, 1, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 8
            ROUND = 3
            Call SETTINGS
            scheduleNG%(TC!, 0) = 904
            scheduleNG%(TC!, 1) = 905
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 4 WINNER"
            homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 5 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 9
            ROUND = 3
            Call SETTINGS
            scheduleNG%(TC!, 0) = 906
            scheduleNG%(TC!, 1) = 907
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 6 WINNER"
            homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 7 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            REGION = currRegion
            If consolationSetting%(0) <> 0 Then '1-YES, 0-NO CONSOLATION
                TC! = TC! + 1
                GAME = 10
                ROUND = 88
                Call SETTINGS
                scheduleNG%(TC!, 0) = 808
                scheduleNG%(TC!, 1) = 809
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 8 LOSER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 9 LOSER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                TC! = TC! + 1
                REGION = currRegion
                GAME = 11
                ROUND = 89
                Call SETTINGS
                scheduleNG%(TC!, 0) = 908
                scheduleNG%(TC!, 1) = 909
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 8 WINNER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 9 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

            Else
                TC! = TC! + 1
                REGION = currRegion
                GAME = 10
                ROUND = 89
                Call SETTINGS
                scheduleNG%(TC!, 0) = 908
                scheduleNG%(TC!, 1) = 909
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 8 WINNER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 9 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

            End If

        Case 9:
            '12 TEAM REGION
            TC! = TC! + 1
            REGION = currRegion
            GAME = 1
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 10, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 7, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 10, 0)
            homeTeam$(TC!) = slotName$(currRegion, 7, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 10, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 7, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 2
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 11, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 6, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 11, 0)
            homeTeam$(TC!) = slotName$(currRegion, 6, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 11, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 6, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 3
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 12, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 5, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 12, 0)
            homeTeam$(TC!) = slotName$(currRegion, 5, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 12, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 5, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 4
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 9, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 8, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 9, 0)
            homeTeam$(TC!) = slotName$(currRegion, 8, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 9, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 8, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 5
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = 901
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 2, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 1 WINNER"
            homeTeam$(TC!) = slotName$(currRegion, 2, 0)
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = slotName$(currRegion, 2, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 6
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = 902
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 3, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 2 WINNER"
            homeTeam$(TC!) = slotName$(currRegion, 3, 0)
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = slotName$(currRegion, 3, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 7
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = 903
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 4, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 3 WINNER"
            homeTeam$(TC!) = slotName$(currRegion, 4, 0)
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = slotName$(currRegion, 4, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 8
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = 904
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 1, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 4 WINNER"
            homeTeam$(TC!) = slotName$(currRegion, 1, 0)
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = slotName$(currRegion, 1, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 9
            ROUND = 3
            Call SETTINGS
            scheduleNG%(TC!, 0) = 905
            scheduleNG%(TC!, 1) = 906
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 5 WINNER"
            homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 6 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 10
            ROUND = 3
            Call SETTINGS
            scheduleNG%(TC!, 0) = 907
            scheduleNG%(TC!, 1) = 908
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 7 WINNER"
            homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 8 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            REGION = currRegion
            If consolationSetting%(0) <> 0 Then '1-YES, 0-NO CONSOLATION
                TC! = TC! + 1
                GAME = 11
                ROUND = 88
                Call SETTINGS
                scheduleNG%(TC!, 0) = 809
                scheduleNG%(TC!, 1) = 810
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 9 LOSER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 10 LOSER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                TC! = TC! + 1
                REGION = currRegion
                GAME = 12
                ROUND = 89
                Call SETTINGS
                scheduleNG%(TC!, 0) = 909
                scheduleNG%(TC!, 1) = 910
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 9 WINNER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 10 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

            Else
                TC! = TC! + 1
                REGION = currRegion
                GAME = 11
                ROUND = 89
                Call SETTINGS
                scheduleNG%(TC!, 0) = 909
                scheduleNG%(TC!, 1) = 910
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 9 WINNER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 10 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

            End If

        Case 10:
            '13 TEAM REGION
            TC! = TC! + 1
            REGION = currRegion
            GAME = 1
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 13, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 4, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 13, 0)
            homeTeam$(TC!) = slotName$(currRegion, 4, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 13, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 4, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 2
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 12, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 5, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 12, 0)
            homeTeam$(TC!) = slotName$(currRegion, 5, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 12, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 5, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 3
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 9, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 8, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 9, 0)
            homeTeam$(TC!) = slotName$(currRegion, 8, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 9, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 8, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 4
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 10, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 7, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 10, 0)
            homeTeam$(TC!) = slotName$(currRegion, 7, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 10, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 7, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 5
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 11, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 6, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 11, 0)
            homeTeam$(TC!) = slotName$(currRegion, 6, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 11, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 6, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 6
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = 901
            scheduleNG%(TC!, 1) = 902
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 1 WINNER"
            homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 2 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 7
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = 903
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 1, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 3 WINNER"
            homeTeam$(TC!) = slotName$(currRegion, 1, 0)
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = slotName$(currRegion, 1, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 8
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = 904
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 2, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 4 WINNER"
            homeTeam$(TC!) = slotName$(currRegion, 2, 0)
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = slotName$(currRegion, 2, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 9
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = 905
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 3, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 5 WINNER"
            homeTeam$(TC!) = slotName$(currRegion, 3, 0)
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = slotName$(currRegion, 3, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 10
            ROUND = 3
            Call SETTINGS
            scheduleNG%(TC!, 0) = 906
            scheduleNG%(TC!, 1) = 907
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 6 WINNER"
            homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 7 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 11
            ROUND = 3
            Call SETTINGS
            scheduleNG%(TC!, 0) = 908
            scheduleNG%(TC!, 1) = 909
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 8 WINNER"
            homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 9 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            REGION = currRegion
            If consolationSetting%(0) <> 0 Then '1-YES, 0-NO CONSOLATION
                TC! = TC! + 1
                GAME = 12
                ROUND = 88
                Call SETTINGS
                scheduleNG%(TC!, 0) = 810
                scheduleNG%(TC!, 1) = 811
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 10 LOSER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 11 LOSER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                TC! = TC! + 1
                REGION = currRegion
                GAME = 13
                ROUND = 89
                Call SETTINGS
                scheduleNG%(TC!, 0) = 910
                scheduleNG%(TC!, 1) = 911
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 10 WINNER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 11 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

            Else
                TC! = TC! + 1
                REGION = currRegion
                GAME = 12
                ROUND = 89
                Call SETTINGS
                scheduleNG%(TC!, 0) = 910
                scheduleNG%(TC!, 1) = 911
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 10 WINNER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 11 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

            End If

        Case 11:
            '14 TEAM REGION
            TC! = TC! + 1
            REGION = currRegion
            GAME = 1
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 14, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 3, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 14, 0)
            homeTeam$(TC!) = slotName$(currRegion, 3, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 14, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 3, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 2
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 13, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 4, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 13, 0)
            homeTeam$(TC!) = slotName$(currRegion, 4, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 13, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 4, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 3
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 12, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 5, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 12, 0)
            homeTeam$(TC!) = slotName$(currRegion, 5, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 12, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 5, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 4
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 11, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 6, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 11, 0)
            homeTeam$(TC!) = slotName$(currRegion, 6, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 11, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 6, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 5
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 10, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 7, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 10, 0)
            homeTeam$(TC!) = slotName$(currRegion, 7, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 10, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 7, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 6
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 9, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 8, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 9, 0)
            homeTeam$(TC!) = slotName$(currRegion, 8, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 9, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 8, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 7
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = 901
            scheduleNG%(TC!, 1) = 904
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 1 WINNER"
            homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 4 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 8
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = 905
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 2, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 5 WINNER"
            homeTeam$(TC!) = slotName$(currRegion, 2, 0)
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = slotName$(currRegion, 2, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 9
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = 902
            scheduleNG%(TC!, 1) = 903
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 2 WINNER"
            homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 3 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 10
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = 906
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 1, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 6 WINNER"
            homeTeam$(TC!) = slotName$(currRegion, 1, 0)
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = slotName$(currRegion, 1, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 11
            ROUND = 3
            Call SETTINGS
            scheduleNG%(TC!, 0) = 907
            scheduleNG%(TC!, 1) = 908
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 7 WINNER"
            homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 8 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 12
            ROUND = 3
            Call SETTINGS
            scheduleNG%(TC!, 0) = 909
            scheduleNG%(TC!, 1) = 910
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 9 WINNER"
            homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 10 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            REGION = currRegion
            If consolationSetting%(0) <> 0 Then '1-YES, 0-NO CONSOLATION
                TC! = TC! + 1
                GAME = 13
                ROUND = 88
                Call SETTINGS
                scheduleNG%(TC!, 0) = 811
                scheduleNG%(TC!, 1) = 812
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 11 LOSER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 12 LOSER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                TC! = TC! + 1
                REGION = currRegion
                GAME = 14
                ROUND = 89
                Call SETTINGS
                scheduleNG%(TC!, 0) = 911
                scheduleNG%(TC!, 1) = 912
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 11 WINNER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 12 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

            Else
                TC! = TC! + 1
                REGION = currRegion
                GAME = 13
                ROUND = 89
                Call SETTINGS
                scheduleNG%(TC!, 0) = 911
                scheduleNG%(TC!, 1) = 912
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 11 WINNER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 12 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

            End If

        Case 12:
            '15 TEAM REGION
            TC! = TC! + 1
            REGION = currRegion
            GAME = 1
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 15, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 2, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 15, 0)
            homeTeam$(TC!) = slotName$(currRegion, 2, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 15, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 2, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 2
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 14, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 3, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 14, 0)
            homeTeam$(TC!) = slotName$(currRegion, 3, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 14, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 3, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 3
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 13, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 4, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 13, 0)
            homeTeam$(TC!) = slotName$(currRegion, 4, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 13, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 4, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 4
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 12, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 5, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 12, 0)
            homeTeam$(TC!) = slotName$(currRegion, 5, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 12, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 5, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 5
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 11, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 6, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 11, 0)
            homeTeam$(TC!) = slotName$(currRegion, 6, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 11, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 6, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 6
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 10, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 7, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 10, 0)
            homeTeam$(TC!) = slotName$(currRegion, 7, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 10, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 7, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 7
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 9, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 8, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 9, 0)
            homeTeam$(TC!) = slotName$(currRegion, 8, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 9, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 8, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 8
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = 901
            scheduleNG%(TC!, 1) = 906
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 1 WINNER"
            homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 6 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 9
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = 902
            scheduleNG%(TC!, 1) = 905
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 2 WINNER"
            homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 5 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 10
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = 903
            scheduleNG%(TC!, 1) = 904
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 3 WINNER"
            homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 4 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 11
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = 907
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 1, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 7 WINNER"
            homeTeam$(TC!) = slotName$(currRegion, 1, 0)
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = slotName$(currRegion, 1, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 12
            ROUND = 3
            Call SETTINGS
            scheduleNG%(TC!, 0) = 908
            scheduleNG%(TC!, 1) = 909
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 8 WINNER"
            homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 9 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 13
            ROUND = 3
            Call SETTINGS
            scheduleNG%(TC!, 0) = 910
            scheduleNG%(TC!, 1) = 911
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 10 WINNER"
            homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 11 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            REGION = currRegion
            If consolationSetting%(0) <> 0 Then '1-YES, 0-NO CONSOLATION
                TC! = TC! + 1
                GAME = 14
                ROUND = 88
                Call SETTINGS
                scheduleNG%(TC!, 0) = 812
                scheduleNG%(TC!, 1) = 813
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 12 LOSER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 13 LOSER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                TC! = TC! + 1
                REGION = currRegion
                GAME = 15
                ROUND = 89
                Call SETTINGS
                scheduleNG%(TC!, 0) = 912
                scheduleNG%(TC!, 1) = 913
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 12 WINNER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 13 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

            Else
                TC! = TC! + 1
                REGION = currRegion
                GAME = 14
                ROUND = 89
                Call SETTINGS
                scheduleNG%(TC!, 0) = 912
                scheduleNG%(TC!, 1) = 913
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 12 WINNER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 13 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

            End If

        Case 13:
            '16 TEAM REGION
            TC! = TC! + 1
            REGION = currRegion
            GAME = 1
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 16, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 1, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 16, 0)
            homeTeam$(TC!) = slotName$(currRegion, 1, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 16, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 1, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 2
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 15, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 2, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 15, 0)
            homeTeam$(TC!) = slotName$(currRegion, 2, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 15, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 2, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 3
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 14, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 3, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 14, 0)
            homeTeam$(TC!) = slotName$(currRegion, 3, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 14, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 3, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 4
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 13, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 4, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 13, 0)
            homeTeam$(TC!) = slotName$(currRegion, 4, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 13, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 4, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 5
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 12, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 5, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 12, 0)
            homeTeam$(TC!) = slotName$(currRegion, 5, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 12, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 5, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 6
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 11, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 6, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 11, 0)
            homeTeam$(TC!) = slotName$(currRegion, 6, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 11, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 6, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 7
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 10, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 7, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 10, 0)
            homeTeam$(TC!) = slotName$(currRegion, 7, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 10, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 7, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 8
            ROUND = 1
            Call SETTINGS
            scheduleNG%(TC!, 0) = tourneyN%(currRegion, 9, 0)
            scheduleNG%(TC!, 1) = tourneyN%(currRegion, 8, 0)
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = slotName$(currRegion, 9, 0)
            homeTeam$(TC!) = slotName$(currRegion, 8, 0)
            yearNumber$(TC!, 0) = slotName$(currRegion, 9, 1)
            yearNumber$(TC!, 1) = slotName$(currRegion, 8, 1)
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 9
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = 901
            scheduleNG%(TC!, 1) = 908
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 1 WINNER"
            homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 8 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 10
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = 904
            scheduleNG%(TC!, 1) = 905
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 4 WINNER"
            homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 5 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 11
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = 903
            scheduleNG%(TC!, 1) = 906
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 3 WINNER"
            homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 6 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 12
            ROUND = 2
            Call SETTINGS
            scheduleNG%(TC!, 0) = 902
            scheduleNG%(TC!, 1) = 907
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 2 WINNER"
            homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 7 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 13
            ROUND = 3
            Call SETTINGS
            scheduleNG%(TC!, 0) = 909
            scheduleNG%(TC!, 1) = 910
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 9 WINNER"
            homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 10 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = currRegion
            GAME = 14
            ROUND = 3
            Call SETTINGS
            scheduleNG%(TC!, 0) = 911
            scheduleNG%(TC!, 1) = 912
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 11 WINNER"
            homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 12 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            REGION = currRegion
            If consolationSetting%(0) <> 0 Then '1-YES, 0-NO CONSOLATION
                TC! = TC! + 1
                GAME = 15
                ROUND = 88
                Call SETTINGS
                scheduleNG%(TC!, 0) = 813
                scheduleNG%(TC!, 1) = 814
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 13 LOSER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 14 LOSER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                TC! = TC! + 1
                REGION = currRegion
                GAME = 16
                ROUND = 89
                Call SETTINGS
                scheduleNG%(TC!, 0) = 913
                scheduleNG%(TC!, 1) = 914
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 13 WINNER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 14 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

            Else
                TC! = TC! + 1
                REGION = currRegion
                GAME = 15
                ROUND = 89
                Call SETTINGS
                scheduleNG%(TC!, 0) = 913
                scheduleNG%(TC!, 1) = 914
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "REG" + Str$(REGION) + " GM 13 WINNER"
                homeTeam$(TC!) = "REG" + Str$(REGION) + " GM 14 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
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
Sub CHAMPIONSHIP (saveFile$, numberRegions, tournName$)

    Shared TC!

    Shared consolationSetting%(), scheduleNG%()
    ' Shared homeScores(), visitorScores()
    Shared homeTeam$(), visitingTeam$(), yearNumber$()

    Select Case numberRegions

        Case 1:
            Call SaveTourneyInfo(saveFile$, numberRegions, tournName$)

        Case 2:
            '2 REGIONS
            TC! = TC! + 1
            REGION = 17
            GAME = 1
            ROUND = 99
            Call SETTINGS

            scheduleNG%(TC!, 0) = 901
            scheduleNG%(TC!, 1) = 902
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 1 WINNER"
            homeTeam$(TC!) = "REGION 2 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            Call SaveTourneyInfo(saveFile$, numberRegions, tournName$)

        Case 3:
            '3 REGIONS
            TC! = TC! + 1
            REGION = 17
            GAME = 1
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 903
            scheduleNG%(TC!, 1) = 902
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 3 WINNER"
            homeTeam$(TC!) = "REGION 2 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 2
            ROUND = 99
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1101
            scheduleNG%(TC!, 1) = 901
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 1 WINNER"
            homeTeam$(TC!) = "REGION 1 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            Call SaveTourneyInfo(saveFile$, numberRegions, tournName$)

        Case 4:
            '4 REGIONS
            TC! = TC! + 1
            REGION = 17
            GAME = 1
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 901
            scheduleNG%(TC!, 1) = 904
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 1 WINNER"
            homeTeam$(TC!) = "REGION 4 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 2
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 902
            scheduleNG%(TC!, 1) = 903
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 2 WINNER"
            homeTeam$(TC!) = "REGION 3 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            If consolationSetting%(1) <> 0 Then
                TC! = TC! + 1
                REGION = 17
                GAME = 3
                ROUND = 98
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1001
                scheduleNG%(TC!, 1) = 1002
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 1 LOSER"
                homeTeam$(TC!) = "GAME 2 LOSER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                TC! = TC! + 1
                REGION = 17
                GAME = 4
                ROUND = 99
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1101
                scheduleNG%(TC!, 1) = 1102
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 1 WINNER"
                homeTeam$(TC!) = "GAME 2 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                Call SaveTourneyInfo(saveFile$, numberRegions, tournName$)
            Else

                TC! = TC! + 1
                REGION = 17
                GAME = 3
                ROUND = 99
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1101
                scheduleNG%(TC!, 1) = 1102
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 1 WINNER"
                homeTeam$(TC!) = "GAME 2 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                Call SaveTourneyInfo(saveFile$, numberRegions, tournName$)

            End If


        Case 5:
            '5 REGIONS
            TC! = TC! + 1
            REGION = 17
            GAME = 1
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 905
            scheduleNG%(TC!, 1) = 904
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 5 WINNER"
            homeTeam$(TC!) = "REGION 4 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 2
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1101
            scheduleNG%(TC!, 1) = 901
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 1 WINNER"
            homeTeam$(TC!) = "REGION 1 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 3
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 902
            scheduleNG%(TC!, 1) = 903
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 2 WINNER"
            homeTeam$(TC!) = "REGION 3 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            If consolationSetting%(1) <> 0 Then
                TC! = TC! + 1
                REGION = 17
                GAME = 4
                ROUND = 98
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1003
                scheduleNG%(TC!, 1) = 1002
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 3 LOSER"
                homeTeam$(TC!) = "GAME 2 LOSER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                TC! = TC! + 1
                REGION = 17
                GAME = 5
                ROUND = 99
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1103
                scheduleNG%(TC!, 1) = 1102
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 3 WINNER"
                homeTeam$(TC!) = "GAME 2 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                Call SaveTourneyInfo(saveFile$, numberRegions, tournName$)
            Else
                TC! = TC! + 1
                REGION = 17
                GAME = 4
                ROUND = 99
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1103
                scheduleNG%(TC!, 1) = 1102
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 3 WINNER"
                homeTeam$(TC!) = "GAME 2 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                Call SaveTourneyInfo(saveFile$, numberRegions, tournName$)
            End If

        Case 6:
            '6 REGIONS
            TC! = TC! + 1
            REGION = 17
            GAME = 1
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 905
            scheduleNG%(TC!, 1) = 904
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 5 WINNER"
            homeTeam$(TC!) = "REGION 4 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 2
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 903
            scheduleNG%(TC!, 1) = 906
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 3 WINNER"
            homeTeam$(TC!) = "REGION 6 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 3
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1101
            scheduleNG%(TC!, 1) = 901
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 1 WINNER"
            homeTeam$(TC!) = "REGION 1 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 4
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1102
            scheduleNG%(TC!, 1) = 902
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 2 WINNER"
            homeTeam$(TC!) = "REGION 2 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            If consolationSetting%(1) <> 0 Then
                TC! = TC! + 1
                REGION = 17
                GAME = 5
                ROUND = 98
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1003
                scheduleNG%(TC!, 1) = 1004
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 3 LOSER"
                homeTeam$(TC!) = "GAME 4 LOSER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                TC! = TC! + 1
                REGION = 17
                GAME = 6
                ROUND = 99
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1103
                scheduleNG%(TC!, 1) = 1104
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 3 WINNER"
                homeTeam$(TC!) = "GAME 4 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                Call SaveTourneyInfo(saveFile$, numberRegions, tournName$)
            Else

                TC! = TC! + 1
                REGION = 17
                GAME = 5
                ROUND = 99
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1103
                scheduleNG%(TC!, 1) = 1104
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 3 WINNER"
                homeTeam$(TC!) = "GAME 4 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                Call SaveTourneyInfo(saveFile$, numberRegions, tournName$)

            End If

        Case 7:
            '7 REGIONS
            TC! = TC! + 1
            REGION = 17
            GAME = 1
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 903
            scheduleNG%(TC!, 1) = 906
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 3 WINNER"
            homeTeam$(TC!) = "REGION 6 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 2
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 902
            scheduleNG%(TC!, 1) = 907
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 2 WINNER"
            homeTeam$(TC!) = "REGION 7 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 3
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 904
            scheduleNG%(TC!, 1) = 905
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 4 WINNER"
            homeTeam$(TC!) = "REGION 5 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 4
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1102
            scheduleNG%(TC!, 1) = 1101
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 2 WINNER"
            homeTeam$(TC!) = "GAME 1 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 5
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1103
            scheduleNG%(TC!, 1) = 901
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 3 WINNER"
            homeTeam$(TC!) = "REGION 1 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            If consolationSetting%(1) <> 0 Then
                TC! = TC! + 1
                REGION = 17
                GAME = 6
                ROUND = 98
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1005
                scheduleNG%(TC!, 1) = 1004
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 5 LOSER"
                homeTeam$(TC!) = "GAME 4 LOSER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                TC! = TC! + 1
                REGION = 17
                GAME = 7
                ROUND = 99
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1105
                scheduleNG%(TC!, 1) = 1104
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 5 WINNER"
                homeTeam$(TC!) = "GAME 4 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                Call SaveTourneyInfo(saveFile$, numberRegions, tournName$)

            Else
                TC! = TC! + 1
                REGION = 17
                GAME = 6
                ROUND = 99
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1105
                scheduleNG%(TC!, 1) = 1104
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 5 WINNER"
                homeTeam$(TC!) = "GAME 4 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                Call SaveTourneyInfo(saveFile$, numberRegions, tournName$)
            End If

        Case 8:
            '8 REGIONS
            TC! = TC! + 1
            REGION = 17
            GAME = 1
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 902
            scheduleNG%(TC!, 1) = 907
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 2 WINNER"
            homeTeam$(TC!) = "REGION 7 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 2
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 903
            scheduleNG%(TC!, 1) = 906
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 3 WINNER"
            homeTeam$(TC!) = "REGION 6 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 3
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 904
            scheduleNG%(TC!, 1) = 905
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 4 WINNER"
            homeTeam$(TC!) = "REGION 5 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 4
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 901
            scheduleNG%(TC!, 1) = 908
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 1 WINNER"
            homeTeam$(TC!) = "REGION 8 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 5
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1101
            scheduleNG%(TC!, 1) = 1102
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 1 WINNER"
            homeTeam$(TC!) = "GAME 2 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 6
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1103
            scheduleNG%(TC!, 1) = 1104
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 3 WINNER"
            homeTeam$(TC!) = "GAME 4 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            If consolationSetting%(1) <> 0 Then
                TC! = TC! + 1
                REGION = 17
                GAME = 7
                ROUND = 98
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1005
                scheduleNG%(TC!, 1) = 1006
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 5 LOSER"
                homeTeam$(TC!) = "GAME 6 LOSER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                TC! = TC! + 1
                REGION = 17
                GAME = 8
                ROUND = 99
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1105
                scheduleNG%(TC!, 1) = 1106
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 5 WINNER"
                homeTeam$(TC!) = "GAME 6 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                Call SaveTourneyInfo(saveFile$, numberRegions, tournName$)

            Else

                TC! = TC! + 1
                REGION = 17
                GAME = 7
                ROUND = 99
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1105
                scheduleNG%(TC!, 1) = 1106
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 5 WINNER"
                homeTeam$(TC!) = "GAME 6 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                Call SaveTourneyInfo(saveFile$, numberRegions, tournName$)

            End If

        Case 9:
            '9 REGIONS
            TC! = TC! + 1
            REGION = 17
            GAME = 1
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 909
            scheduleNG%(TC!, 1) = 908
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 9 WINNER"
            homeTeam$(TC!) = "REGION 8 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 2
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 902
            scheduleNG%(TC!, 1) = 907
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 2 WINNER"
            homeTeam$(TC!) = "REGION 7 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 3
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 903
            scheduleNG%(TC!, 1) = 906
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 3 WINNER"
            homeTeam$(TC!) = "REGION 6 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 4
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 904
            scheduleNG%(TC!, 1) = 905
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 4 WINNER"
            homeTeam$(TC!) = "REGION 5 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 5
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 901
            scheduleNG%(TC!, 1) = 1101
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 1 WINNER"
            homeTeam$(TC!) = "GAME 1 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 6
            ROUND = 3
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1102
            scheduleNG%(TC!, 1) = 1103
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 2 WINNER"
            homeTeam$(TC!) = "GAME 3 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 7
            ROUND = 3
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1104
            scheduleNG%(TC!, 1) = 1105
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 4 WINNER"
            homeTeam$(TC!) = "GAME 5 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            If consolationSetting%(1) <> 0 Then
                TC! = TC! + 1
                REGION = 17
                GAME = 8
                ROUND = 98
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1007
                scheduleNG%(TC!, 1) = 1006
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 7 LOSER"
                homeTeam$(TC!) = "GAME 6 LOSER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                TC! = TC! + 1
                REGION = 17
                GAME = 9
                ROUND = 99
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1107
                scheduleNG%(TC!, 1) = 1106
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 7 WINNER"
                homeTeam$(TC!) = "GAME 6 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                Call SaveTourneyInfo(saveFile$, numberRegions, tournName$)

            Else
                TC! = TC! + 1
                REGION = 17
                GAME = 8
                ROUND = 99
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1107
                scheduleNG%(TC!, 1) = 1106
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 7 WINNER"
                homeTeam$(TC!) = "GAME 6 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                Call SaveTourneyInfo(saveFile$, numberRegions, tournName$)
            End If

        Case 10:
            '10 REGIONS
            TC! = TC! + 1
            REGION = 17
            GAME = 1
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 910
            scheduleNG%(TC!, 1) = 907
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 10 WINNER"
            homeTeam$(TC!) = "REGION 7 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 2
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 908
            scheduleNG%(TC!, 1) = 909
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 8 WINNER"
            homeTeam$(TC!) = "REGION 9 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 3
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1101
            scheduleNG%(TC!, 1) = 902
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 1 WINNER"
            homeTeam$(TC!) = "REGION 2 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 4
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 903
            scheduleNG%(TC!, 1) = 906
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 3 WINNER"
            homeTeam$(TC!) = "REGION 6 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 5
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 904
            scheduleNG%(TC!, 1) = 905
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 4 WINNER"
            homeTeam$(TC!) = "REGION 5 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 6
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 901
            scheduleNG%(TC!, 1) = 1102
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 1 WINNER"
            homeTeam$(TC!) = "GAME 2 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 7
            ROUND = 3
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1103
            scheduleNG%(TC!, 1) = 1104
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 3 WINNER"
            homeTeam$(TC!) = "GAME 4 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 8
            ROUND = 3
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1105
            scheduleNG%(TC!, 1) = 1106
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 5 WINNER"
            homeTeam$(TC!) = "GAME 6 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            If consolationSetting%(1) <> 0 Then
                TC! = TC! + 1
                REGION = 17
                GAME = 9
                ROUND = 98
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1007
                scheduleNG%(TC!, 1) = 1008
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 7 LOSER"
                homeTeam$(TC!) = "GAME 8 LOSER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                TC! = TC! + 1
                REGION = 17
                GAME = 10
                ROUND = 99
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1107
                scheduleNG%(TC!, 1) = 1108
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 7 WINNER"
                homeTeam$(TC!) = "GAME 8 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                Call SaveTourneyInfo(saveFile$, numberRegions, tournName$)

            Else
                TC! = TC! + 1
                REGION = 17
                GAME = 9
                ROUND = 99
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1107
                scheduleNG%(TC!, 1) = 1108
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 7 WINNER"
                homeTeam$(TC!) = "GAME 8 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                Call SaveTourneyInfo(saveFile$, numberRegions, tournName$)

            End If

        Case 11:
            '11 REGIONS
            TC! = TC! + 1
            REGION = 17
            GAME = 1
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 910
            scheduleNG%(TC!, 1) = 907
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 10 WINNER"
            homeTeam$(TC!) = "REGION 7 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 2
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 911
            scheduleNG%(TC!, 1) = 906
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 11 WINNER"
            homeTeam$(TC!) = "REGION 6 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 3
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 909
            scheduleNG%(TC!, 1) = 908
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 9 WINNER"
            homeTeam$(TC!) = "REGION 8 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 4
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1101
            scheduleNG%(TC!, 1) = 902
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 1 WINNER"
            homeTeam$(TC!) = "REGION 2 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 5
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 903
            scheduleNG%(TC!, 1) = 1102
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 3 WINNER"
            homeTeam$(TC!) = "GAME 2 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 6
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 904
            scheduleNG%(TC!, 1) = 905
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 4 WINNER"
            homeTeam$(TC!) = "REGION 5 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 7
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 901
            scheduleNG%(TC!, 1) = 1103
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 1 WINNER"
            homeTeam$(TC!) = "GAME 3 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 8
            ROUND = 3
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1104
            scheduleNG%(TC!, 1) = 1105
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 4 WINNER"
            homeTeam$(TC!) = "GAME 5 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 9
            ROUND = 3
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1106
            scheduleNG%(TC!, 1) = 1107
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 6 WINNER"
            homeTeam$(TC!) = "GAME 7 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            If consolationSetting%(1) <> 0 Then
                TC! = TC! + 1
                REGION = 17
                GAME = 10
                ROUND = 98
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1009
                scheduleNG%(TC!, 1) = 1008
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 9 LOSER"
                homeTeam$(TC!) = "GAME 8 LOSER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                TC! = TC! + 1
                REGION = 17
                GAME = 11
                ROUND = 99
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1109
                scheduleNG%(TC!, 1) = 1108
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 9 WINNER"
                homeTeam$(TC!) = "GAME 8 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                Call SaveTourneyInfo(saveFile$, numberRegions, tournName$)
            Else
                TC! = TC! + 1
                REGION = 17
                GAME = 10
                ROUND = 99
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1109
                scheduleNG%(TC!, 1) = 1108
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 7 WINNER"
                homeTeam$(TC!) = "GAME 8 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                Call SaveTourneyInfo(saveFile$, numberRegions, tournName$)
            End If

        Case 12:
            '12 REGIONS
            TC! = TC! + 1
            REGION = 17
            GAME = 1
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 910
            scheduleNG%(TC!, 1) = 907
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 10 WINNER"
            homeTeam$(TC!) = "REGION 7 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 2
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 911
            scheduleNG%(TC!, 1) = 906
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 11 WINNER"
            homeTeam$(TC!) = "REGION 6 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 3
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 912
            scheduleNG%(TC!, 1) = 905
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 12 WINNER"
            homeTeam$(TC!) = "REGION 5 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 4
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 909
            scheduleNG%(TC!, 1) = 908
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 9 WINNER"
            homeTeam$(TC!) = "REGION 8 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 5
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 902
            scheduleNG%(TC!, 1) = 1101
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 2 WINNER"
            homeTeam$(TC!) = "GAME 1 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 6
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1102
            scheduleNG%(TC!, 1) = 903
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 2 WINNER"
            homeTeam$(TC!) = "REGION 3 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 7
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 904
            scheduleNG%(TC!, 1) = 1103
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 4 WINNER"
            homeTeam$(TC!) = "GAME 3 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 8
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1104
            scheduleNG%(TC!, 1) = 901
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 4 WINNER"
            homeTeam$(TC!) = "REGION 1 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 9
            ROUND = 3
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1106
            scheduleNG%(TC!, 1) = 1105
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 6 WINNER"
            homeTeam$(TC!) = "GAME 5 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 10
            ROUND = 3
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1107
            scheduleNG%(TC!, 1) = 1108
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 7 WINNER"
            homeTeam$(TC!) = "GAME 8 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            If consolationSetting%(1) <> 0 Then
                TC! = TC! + 1
                REGION = 17
                GAME = 11
                ROUND = 98
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1009
                scheduleNG%(TC!, 1) = 1010
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 9 LOSER"
                homeTeam$(TC!) = "GAME 10 LOSER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                TC! = TC! + 1
                REGION = 17
                GAME = 12
                ROUND = 99
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1109
                scheduleNG%(TC!, 1) = 1110
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 9 WINNER"
                homeTeam$(TC!) = "GAME 10 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                Call SaveTourneyInfo(saveFile$, numberRegions, tournName$)

            Else

                TC! = TC! + 1
                REGION = 17
                GAME = 11
                ROUND = 99
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1109
                scheduleNG%(TC!, 1) = 1110
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 9 WINNER"
                homeTeam$(TC!) = "GAME 10 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                Call SaveTourneyInfo(saveFile$, numberRegions, tournName$)

            End If

        Case 13:
            '13 REGIONS
            TC! = TC! + 1
            REGION = 17
            GAME = 1
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 913
            scheduleNG%(TC!, 1) = 904
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 13 WINNER"
            homeTeam$(TC!) = "REGION 4 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 2
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 912
            scheduleNG%(TC!, 1) = 905
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 12 WINNER"
            homeTeam$(TC!) = "REGION 5 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 3
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 909
            scheduleNG%(TC!, 1) = 908
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 9 WINNER"
            homeTeam$(TC!) = "REGION 8 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 4
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 910
            scheduleNG%(TC!, 1) = 907
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 10 WINNER"
            homeTeam$(TC!) = "REGION 7 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 5
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 906
            scheduleNG%(TC!, 1) = 911
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 6 WINNER"
            homeTeam$(TC!) = "REGION 11 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 6
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1101
            scheduleNG%(TC!, 1) = 1102
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 1 WINNER"
            homeTeam$(TC!) = "GAME 2 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 7
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 901
            scheduleNG%(TC!, 1) = 1103
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 1 WINNER"
            homeTeam$(TC!) = "GAME 3 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 8
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1104
            scheduleNG%(TC!, 1) = 902
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 4 WINNER"
            homeTeam$(TC!) = "REGION 2 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 9
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 903
            scheduleNG%(TC!, 1) = 1105
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 3 WINNER"
            homeTeam$(TC!) = "GAME 5 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 10
            ROUND = 3
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1107
            scheduleNG%(TC!, 1) = 1106
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 7 WINNER"
            homeTeam$(TC!) = "GAME 6 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 11
            ROUND = 3
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1103
            scheduleNG%(TC!, 1) = 1109
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 3 WINNER"
            homeTeam$(TC!) = "GAME 9 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            If consolationSetting%(1) <> 0 Then
                TC! = TC! + 1
                REGION = 17
                GAME = 12
                ROUND = 98
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1011
                scheduleNG%(TC!, 1) = 1010
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 11 LOSER"
                homeTeam$(TC!) = "GAME 10 LOSER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                TC! = TC! + 1
                REGION = 17
                GAME = 13
                ROUND = 99
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1111
                scheduleNG%(TC!, 1) = 1110
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 11 WINNER"
                homeTeam$(TC!) = "GAME 10 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                Call SaveTourneyInfo(saveFile$, numberRegions, tournName$)

            Else
                TC! = TC! + 1
                REGION = 17
                GAME = 12
                ROUND = 99
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1111
                scheduleNG%(TC!, 1) = 1110
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 11 WINNER"
                homeTeam$(TC!) = "GAME 10 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                Call SaveTourneyInfo(saveFile$, numberRegions, tournName$)
            End If

        Case 14:

            '14 REGIONS
            TC! = TC! + 1
            REGION = 17
            GAME = 1
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 914
            scheduleNG%(TC!, 1) = 903
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 14 WINNER"
            homeTeam$(TC!) = "REGION 3 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 2
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 913
            scheduleNG%(TC!, 1) = 904
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 13 WINNER"
            homeTeam$(TC!) = "REGION 4 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 3
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 912
            scheduleNG%(TC!, 1) = 905
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 12 WINNER"
            homeTeam$(TC!) = "REGION 5 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 4
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 911
            scheduleNG%(TC!, 1) = 906
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 11 WINNER"
            homeTeam$(TC!) = "REGION 6 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 5
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 910
            scheduleNG%(TC!, 1) = 907
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 10 WINNER"
            homeTeam$(TC!) = "REGION 7 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 6
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 909
            scheduleNG%(TC!, 1) = 909
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 9 WINNER"
            homeTeam$(TC!) = "REGION 8 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 7
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1104
            scheduleNG%(TC!, 1) = 1101
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 4 WINNER"
            homeTeam$(TC!) = "GAME 1 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 8
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1105
            scheduleNG%(TC!, 1) = 902
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 5 WINNER"
            homeTeam$(TC!) = "REGION 2 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 9
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1102
            scheduleNG%(TC!, 1) = 1103
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 2 WINNER"
            homeTeam$(TC!) = "GAME 3 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 10
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1106
            scheduleNG%(TC!, 1) = 901
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 6 WINNER"
            homeTeam$(TC!) = "REGION 1 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 11
            ROUND = 3
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1107
            scheduleNG%(TC!, 1) = 1108
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 7 WINNER"
            homeTeam$(TC!) = "GAME 8 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 12
            ROUND = 3
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1109
            scheduleNG%(TC!, 1) = 1110
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 9 WINNER"
            homeTeam$(TC!) = "GAME 10 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            If consolationSetting%(1) <> 0 Then
                TC! = TC! + 1
                REGION = 17
                GAME = 13
                ROUND = 98
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1012
                scheduleNG%(TC!, 1) = 1011
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 12 LOSER"
                homeTeam$(TC!) = "GAME 11 LOSER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                TC! = TC! + 1
                REGION = 17
                GAME = 14
                ROUND = 99
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1112
                scheduleNG%(TC!, 1) = 1111
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 12 WINNER"
                homeTeam$(TC!) = "GAME 11 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                Call SaveTourneyInfo(saveFile$, numberRegions, tournName$)

            Else
                TC! = TC! + 1
                REGION = 17
                GAME = 13
                ROUND = 99
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1111
                scheduleNG%(TC!, 1) = 1112
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 11 WINNER"
                homeTeam$(TC!) = "GAME 12 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                Call SaveTourneyInfo(saveFile$, numberRegions, tournName$)
            End If

        Case 15:

            '15 REGIONS
            TC! = TC! + 1
            REGION = 17
            GAME = 1
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 915
            scheduleNG%(TC!, 1) = 902
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 15 WINNER"
            homeTeam$(TC!) = "REGION 2 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 2
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 914
            scheduleNG%(TC!, 1) = 903
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 14 WINNER"
            homeTeam$(TC!) = "REGION 3 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 3
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 913
            scheduleNG%(TC!, 1) = 904
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 13 WINNER"
            homeTeam$(TC!) = "REGION 4 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 4
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 912
            scheduleNG%(TC!, 1) = 905
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 12 WINNER"
            homeTeam$(TC!) = "REGION 5 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 5
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 911
            scheduleNG%(TC!, 1) = 906
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 11 WINNER"
            homeTeam$(TC!) = "REGION 6 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 6
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 910
            scheduleNG%(TC!, 1) = 907
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 10 WINNER"
            homeTeam$(TC!) = "REGION 7 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 7
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 909
            scheduleNG%(TC!, 1) = 908
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 9 WINNER"
            homeTeam$(TC!) = "REGION 8 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 8
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1101
            scheduleNG%(TC!, 1) = 1106
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 1 WINNER"
            homeTeam$(TC!) = "GAME 6 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 9
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1102
            scheduleNG%(TC!, 1) = 1105
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 2 WINNER"
            homeTeam$(TC!) = "GAME 5 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 10
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1103
            scheduleNG%(TC!, 1) = 1104
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 3 WINNER"
            homeTeam$(TC!) = "GAME 4 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 11
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1107
            scheduleNG%(TC!, 1) = 901
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 7 WINNER"
            homeTeam$(TC!) = "REGION 1 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 12
            ROUND = 3
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1109
            scheduleNG%(TC!, 1) = 1108
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 9 WINNER"
            homeTeam$(TC!) = "GAME 8 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 13
            ROUND = 3
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1110
            scheduleNG%(TC!, 1) = 1111
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 10 WINNER"
            homeTeam$(TC!) = "GAME 11 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            If consolationSetting%(1) <> 0 Then
                TC! = TC! + 1
                REGION = 17
                GAME = 14
                ROUND = 98
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1012
                scheduleNG%(TC!, 1) = 1013
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 12 LOSER"
                homeTeam$(TC!) = "GAME 13 LOSER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                TC! = TC! + 1
                REGION = 17
                GAME = 15
                ROUND = 99
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1112
                scheduleNG%(TC!, 1) = 1113
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 12 WINNER"
                homeTeam$(TC!) = "GAME 13 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                Call SaveTourneyInfo(saveFile$, numberRegions, tournName$)

            Else
                TC! = TC! + 1
                REGION = 17
                GAME = 14
                ROUND = 99
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1112
                scheduleNG%(TC!, 1) = 1113
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 12 WINNER"
                homeTeam$(TC!) = "GAME 13 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                Call SaveTourneyInfo(saveFile$, numberRegions, tournName$)
            End If

        Case 16:

            '16 REGIONS
            TC! = TC! + 1
            REGION = 17
            GAME = 1
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 916
            scheduleNG%(TC!, 1) = 901
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 16 WINNER"
            homeTeam$(TC!) = "REGION 1 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 2
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 915
            scheduleNG%(TC!, 1) = 902
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 15 WINNER"
            homeTeam$(TC!) = "REGION 2 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 3
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 914
            scheduleNG%(TC!, 1) = 903
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 14 WINNER"
            homeTeam$(TC!) = "REGION 3 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 4
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 913
            scheduleNG%(TC!, 1) = 904
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 13 WINNER"
            homeTeam$(TC!) = "REGION 4 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 5
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 912
            scheduleNG%(TC!, 1) = 905
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 12 WINNER"
            homeTeam$(TC!) = "REGION 5 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 6
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 911
            scheduleNG%(TC!, 1) = 906
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 11 WINNER"
            homeTeam$(TC!) = "REGION 6 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 7
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 910
            scheduleNG%(TC!, 1) = 907
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 10 WINNER"
            homeTeam$(TC!) = "REGION 7 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 8
            ROUND = 1
            Call SETTINGS

            scheduleNG%(TC!, 0) = 909
            scheduleNG%(TC!, 1) = 908
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "REGION 9 WINNER"
            homeTeam$(TC!) = "REGION 8 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 9
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1101
            scheduleNG%(TC!, 1) = 1108
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 1 WINNER"
            homeTeam$(TC!) = "GAME 8 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 10
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1104
            scheduleNG%(TC!, 1) = 1105
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 4 WINNER"
            homeTeam$(TC!) = "GAME 5 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 11
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1103
            scheduleNG%(TC!, 1) = 1106
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 3 WINNER"
            homeTeam$(TC!) = "GAME 6 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 12
            ROUND = 2
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1102
            scheduleNG%(TC!, 1) = 1107
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 2 WINNER"
            homeTeam$(TC!) = "GAME 7 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 13
            ROUND = 3
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1109
            scheduleNG%(TC!, 1) = 1110
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 9 WINNER"
            homeTeam$(TC!) = "GAME 10 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            TC! = TC! + 1
            REGION = 17
            GAME = 14
            ROUND = 3
            Call SETTINGS

            scheduleNG%(TC!, 0) = 1111
            scheduleNG%(TC!, 1) = 1112
            scheduleNG%(TC!, 15) = REGION
            scheduleNG%(TC!, 16) = ROUND
            scheduleNG%(TC!, 17) = GAME
            visitingTeam$(TC!) = "GAME 11 WINNER"
            homeTeam$(TC!) = "GAME 12 WINNER"
            yearNumber$(TC!, 0) = "XXX"
            yearNumber$(TC!, 1) = "XXX"
            Call WRITEGAME(saveFile$)

            If consolationSetting%(1) <> 0 Then
                TC! = TC! + 1
                REGION = 17
                GAME = 15
                ROUND = 98
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1014
                scheduleNG%(TC!, 1) = 1013
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 14 LOSER"
                homeTeam$(TC!) = "GAME 13 LOSER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                TC! = TC! + 1
                REGION = 17
                GAME = 16
                ROUND = 99
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1114
                scheduleNG%(TC!, 1) = 1113
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 14 WINNER"
                homeTeam$(TC!) = "GAME 13 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

                Call SaveTourneyInfo(saveFile$, numberRegions, tournName$)

            Else
                TC! = TC! + 1
                REGION = 17
                GAME = 15
                ROUND = 99
                Call SETTINGS

                scheduleNG%(TC!, 0) = 1114
                scheduleNG%(TC!, 1) = 1113
                scheduleNG%(TC!, 15) = REGION
                scheduleNG%(TC!, 16) = ROUND
                scheduleNG%(TC!, 17) = GAME
                visitingTeam$(TC!) = "GAME 14 WINNER"
                homeTeam$(TC!) = "GAME 13 WINNER"
                yearNumber$(TC!, 0) = "XXX"
                yearNumber$(TC!, 1) = "XXX"
                Call WRITEGAME(saveFile$)

            End If

    End Select

End Sub


'----------------------------------------
'    SETTINGS Subroutine
'----------------------------------------
'This subroutine assigns the tournament
'settings to the current game, using the 
'defaults set earlier.
Sub SETTINGS ()

    Shared TC!
    Shared scheduleNG%(), tourneyDefaults%()

    For I = 0 To 14
        scheduleNG%(TC!, I) = tourneyDefaults%(I)
    Next

    scheduleNG%(TC!, 8) = 3
    scheduleNG%(TC!, 2) = 2

End Sub

'----------------------------------------
'    WRITEGAME Subroutine
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

    Shared TC!

    Shared homeScores(), scheduleNG%(), visitorScores()
    Shared homeTeam$(), visitingTeam$(), yearNumber$()

    'WRITE OUT A GAME
    'TMP file
    Open diskPaths$(1) + outputFile$ + ".TMP" For Random As #1 Len = 90

    For X = 0 To 14
        Field #1, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
    Next

    Field #1, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
    Field #1, 64 As X$, 3 As Q$(20), 3 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
    Field #1, 84 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)

    For X = 0 To 14: LSet Q$(X + 1) = MKI$(scheduleNG%(TC!, X)): Next
    For X = 0 To 3: LSet Q$(20 + X) = yearNumber$(TC!, X): Next

    LSet Q$(16) = MKI$(visitorScores(TC!))
    LSet Q$(17) = MKI$(homeScores(TC!))
    LSet Q$(18) = visitingTeam$(TC!)
    LSet Q$(19) = homeTeam$(TC!)

    For X = 24 To 26
        LSet Q$(X) = MKI$(scheduleNG%(TC!, X - 9))
    Next

    Put #1, TC!

    Close #1

    'TCX file
    Open diskPaths$(1) + outputFile$ + ".TCX" For Random As #2 Len = 90

    For X = 0 To 14
        Field #2, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
    Next

    Field #2, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
    Field #2, 64 As X$, 3 As Q$(20), 3 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
    Field #2, 84 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)

    For X = 0 To 14: 
        LSet Q$(X + 1) = MKI$(scheduleNG%(TC!, X))
    Next

    For X = 0 To 3: 
        LSet Q$(20 + X) = yearNumber$(TC!, X)
    Next

    LSet Q$(16) = MKI$(visitorScores(TC!))
    LSet Q$(17) = MKI$(homeScores(TC!))
    LSet Q$(18) = visitingTeam$(TC!)
    LSet Q$(19) = homeTeam$(TC!)

    For X = 24 To 26
        LSet Q$(X) = MKI$(scheduleNG%(TC!, X - 9))
    Next

    Put #2, TC!

    Close #2

End Sub

'----------------------------------------
'         SaveTourneyInfo Subroutine
'----------------------------------------
'This subroutine saves the tournament
'"INFO" file (.INF), then relies on a
'separate routine to sort the games and
'save them to the .TOU file. 
Sub SaveTourneyInfo (saveFile$, numberRegions, tourneyName$)

    Shared TC!

    Shared consolationSetting%(), tourneyN%(), NN%()
    Shared slotName$(), regionNames$()

    'SAVE TOURNAMENT INFO AND QUIT
    Open diskPaths$(1) + saveFile$ + ".INF" For Output As #2

    Print #2, tourneyName$
    Print #2, numberRegions ' NUMBER OF REGIONS

    For I = 1 To 16
        Print #2, NN%(I) ' NUMBER OF TEAMS IN REGION
        Print #2, regionNames$(I) ' NAME OF REGION
    Next

    For I = 1 To 16
        For I1 = 1 To 16
            For I2 = 0 To 3
                Print #2, slotName$(I, I1, I2) ' SEED SLOT NAME
                Print #2, tourneyN%(I, I1, I2) ' REGION, SEED NUMBER, MODE PLAY/TM#
            Next
        Next
    Next

    Print #2, consolationSetting%(0)
    Print #2, consolationSetting%(1): ' CONSOLATION GAME

    Close #2

    Call SortAndSaveGames(saveFile$, numberRegions)

    Locate 22, 25: Print Space$(34)

    MsgText$ = "ALL FILES SAVED...PRESS ANY KEY TO RETURN TO TOURNAMENT OPTIONS"
    BoxWidth = 18
    BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
    Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
    Call CSMsgBox(MsgText$, BoxWidth, Pass)
    I$ = GetKeyPress$
    Call CSMsgBox("", 0, Pass)

End Sub


'----------------------------------------
'      SortAndSaveGames Subroutine
'----------------------------------------
'This subroutine reads data from a .TMP 
'file created earlier in the process. If
'then performs a sort on all games to be
'scheduled and creates the final .TOU file.
Sub SortAndSaveGames (tourneyFile$, numberRegions)

    Shared TC!

    Shared scheduleNG%()
    Shared homeTeam$(), visitingTeam$(), yearNumber$()
    Shared homeScores(), visitorScores()

    For RD = 1 To 89

        If Not (RD > 5 And RD < 88 Or RD > 89) Then

            Locate 22, 25: Print "SCHEDULING GAMES FOR BRACKET"; numberRegions

            For NR = 1 To 16

                If NR <= numberRegions Then

                    For AP = 1 To TC!

                        'Read data from .TMP file
                        Open diskPaths$(1) + tourneyFile$ + ".TMP" For Random As #1 Len = 90

                        For X = 0 To 14
                            Field #1, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
                        Next

                        Field #1, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
                        Field #1, 64 As X$, 3 As Q$(20), 3 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
                        Field #1, 84 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)

                        Get #1, AP

                        For X = 0 To 14: scheduleNG%(0, X) = CVI(Q$(X + 1)): Next
                        For X = 0 To 3: yearNumber$(0, X) = Q$(20 + X): Next

                        visitorScores(0) = CVI(Q$(16))
                        homeScores(0) = CVI(Q$(17))
                        visitingTeam$(0) = Q$(18)
                        homeTeam$(0) = Q$(19)

                        For X = 1 To 3
                            scheduleNG%(0, X + 14) = CVI(Q$(X + 23))
                        Next

                        Close #1

                        'Create matches in .TOU file
                        If scheduleNG%(0, 16) = RD And scheduleNG%(0, 15) = NR Then

                            MATCH = MATCH + 1

                            Open diskPaths$(1) + tourneyFile$ + ".TOU" For Random As #2 Len = 90

                            For X = 0 To 14
                                Field #2, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
                            Next

                            Field #2, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
                            Field #2, 64 As X$, 3 As Q$(20), 3 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
                            Field #2, 84 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)

                            For X = 0 To 14: LSet Q$(X + 1) = MKI$(scheduleNG%(0, X)): Next
                            For X = 0 To 3: LSet Q$(20 + X) = yearNumber$(0, X): Next

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

            Next NR 'Done processing all regions

        End If
    Next RD

    NR = 17

    tempFile$ = diskPaths$(1) + tourneyFile$ + ".TMP"

    For RD = 1 To 99

        If Not (RD > 5 And RD < 88 Or RD > 89 And RD < 98) Then

            For AP = 1 To TC!

                Open tempFile$ For Random As #1 Len = 90

                For X = 0 To 14
                    Field #1, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
                Next

                Field #1, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
                Field #1, 64 As X$, 3 As Q$(20), 3 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
                Field #1, 84 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)

                Get #1, AP

                For X = 0 To 14: scheduleNG%(0, X) = CVI(Q$(X + 1)): Next
                For X = 0 To 3: yearNumber$(0, X) = Q$(20 + X): Next

                visitorScores(0) = CVI(Q$(16))
                homeScores(0) = CVI(Q$(17))
                visitingTeam$(0) = Q$(18)
                homeTeam$(0) = Q$(19)

                For X = 1 To 3: scheduleNG%(0, X + 14) = CVI(Q$(X + 23)): Next

                Close #1

                If scheduleNG%(0, 16) = RD And scheduleNG%(0, 15) = NR Then

                    MATCH = MATCH + 1

                    Open diskPaths$(1) + tourneyFile$ + ".TOU" For Random As #2 Len = 90

                    For X = 0 To 14
                        Field #2, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
                    Next

                    Field #2, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
                    Field #2, 64 As X$, 3 As Q$(20), 3 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
                    Field #2, 84 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)

                    For X = 0 To 14: LSet Q$(X + 1) = MKI$(scheduleNG%(0, X)): Next
                    For X = 0 To 3: LSet Q$(20 + X) = yearNumber$(0, X): Next

                    LSet Q$(16) = MKI$(visitorScores(0)): LSet Q$(17) = MKI$(homeScores(0))
                    LSet Q$(18) = visitingTeam$(0): LSet Q$(19) = homeTeam$(0)

                    For X = 24 To 26: LSet Q$(X) = MKI$(scheduleNG%(0, X - 9)): Next

                    Put #2, MATCH

                    Close #2

                End If

            Next AP

        End If

    Next RD

    If _FileExists(tempFile$) Then Kill tempFile$

End Sub
