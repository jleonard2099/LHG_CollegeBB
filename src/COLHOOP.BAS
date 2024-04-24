'----------------------------------------
'           COLHOOP Subroutine
'----------------------------------------
'This subroutine serves as the main "source"
'for the action in the game. It steps the
'user through choosing game parameters and
'teams, as well as determining players to
'disable, etc.
Sub COLHOOP ()

    _Title "Courtside College Basketball v" + GAME_VERSION$ + " - HOOPS!"

    Shared actualAttendance&
    Shared avgAttendance&()

    Shared scheduleFile$, tourneyFile$

    Shared tickerStart
    Shared FY%()

    Shared coachMode$(), foulShotOpt$()
    Shared location$()
    Shared playMode$()
    Shared sClockOpt$(), yesNoText$()

    Color 7, 0
    Cls

    Randomize Timer

    Call InitVar

    '----------------------------------------
    '         DETERMINE GAME OPTIONS
    '----------------------------------------

    If Not _FileExists("DEV.BAS") Then

        Open "DEV.BAS" For Output As #1

        For I = 0 To 1
            Print #1, _CWD$ + "\"
        Next

        Close #1

    End If

    Open "DEV.BAS" For Input As #2

    For I = 0 To 1
        Input #2, diskPaths$(I)
    Next

    Close #2

    If Not _FileExists("DEFAULT") Then

        Open "DEFAULT" For Output As #3

        For I = 1 To 3:
            Print #3, 0
        Next

        'Location
        Print #3, 1
        Print #3, 5

        For I = 6 To 11:
            Print #3, 0
        Next

        Close #3

    End If

    Open "DEFAULT" For Input As #4
    Input #4, playerOpt
    Input #4, threePtOpt
    Input #4, sClockVal
    Input #4, gameLoc
    Input #4, foulsToDQ
    Input #4, pbpDelay!
    Input #4, coachOpt
    Input #4, ft10thFoul
    Input #4, ft3PtFoul
    Input #4, ft6FloorFouls
    Input #4, pbpOpt
    Close #4

    If _FileExists("TICKER") Then

        Open "TICKER" For Input As #5

        For I = 1 To 32
            For I1 = 0 To 1
                For I2 = 1 To 14
                    Input #5, SX%(I, I1, I2)
                Next
            Next
        Next

        For I = 1 To 32
            For I1 = 0 To 1
                Input #5, SX$(I, I1)
            Next
        Next

        For I = 1 To 32:
            Input #5, ST%(I)
        Next

        Close #5

        TMT = 1
    Else
        TMT = 0
    End If

    'Load game in-progress, if there is one
    If _FileExists("GAME82") Then

        Open "GAME82" For Input As #6

        Input #6, scheduleFile$

        For I = 0 To 2:
            Input #6, schedGame(I)
        Next

        Close #6

        currGame = schedGame(0) + schedGame(2)

        'Open "pbplog" + Str$(currGame) For Append As #11

        Open diskPaths$(3) + scheduleFile$ For Random As #7 Len = 48

        For X = 0 To 18
            Field #7, X * 2 As X$, 2 As Q$(X + 1), 48 - 2 - 2 * X As X$
        Next

        Field #7, 38 As X$, 2 As Q$(22), 2 As Q$(23), 3 As Q$(20), 3 As Q$(21)

        Get #7, schedGame(0) + schedGame(2)

        For X = 0 To 18:
            NG%(X) = CVI(Q$(X + 1))
        Next

        For X = 0 To 1:
            diskIDs$(X) = Q$(20 + X)
        Next

        VS% = CVI(Q$(22))
        HS% = CVI(Q$(23))

        Close #7

        autoPlay = 1

    Else

        'Open "pbplog" For Append As #11

    End If

    If _FileExists("TOURNEY") Then

        Open "TOURNEY" For Input As #8

        Input #8, tourneyFile$

        For I = 0 To 2:
            Input #8, schedGame(I)
        Next

        Close #8

        'Other areas of code, like creating box files,
        'will use scheduleFile$. We would check for
        'TOU = 1 to distinguish but TOU = 0 after most
        'of the initial AUTOSETUP process
        scheduleFile$ = tourneyFile$

        'If start game = end game,
        'we're all done!
        If schedGame(2) = schedGame(1) Then
            Kill "TOURNEY"
            'System
            'End
            Exit Sub

        Else

            Open diskPaths$(1) + tourneyFile$ As #9 Len = 90

            For X = 0 To 14
                Field #9, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
            Next

            Field #9, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
            Field #9, 64 As X$, 3 As Q$(20), 3 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
            Field #9, 84 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)

            Get #9, schedGame(0) + schedGame(2)

            VS% = CVI(Q$(16))
            HS% = CVI(Q$(17))
            schedVisTm$ = Q$(18)
            schedHomeTm$ = Q$(19)

            For X = 0 To 14:
                NG%(X) = CVI(Q$(X + 1))
            Next

            For X = 0 To 3:
                diskIDs$(X) = Q$(20 + X)
            Next

            For X = 1 To 3:
                NG%(X + 14) = CVI(Q$(X + 23))
            Next

            Close #9

            autoPlay = 1
            TOU = 1

        End If

    End If

    If autoPlay = 1 Then Call AUTOSETUP

    'AUTOSETUP will reset TOU = 0 in many circumstances
    If autoPlay <> 1 And TOU <> 1 Then

        ' USER SELECTION OF OPTIONS
        'gameLoc = 1 is neutral site
        If gameLoc = 0 Then gameLoc = 1

        Do

            Color 15, 0
            Cls

            Color 2
            Print Tab(29); "SELECT OPENING OPTIONS"

            Locate 2, 2: Color 14
            Print "(1) ";: Color 15: Print playMode$(playerOpt)
            Locate 3, 2: Color 14
            Print "(2) ";
            Color 15: Print "VISITING TEAM: ";
            Color teamRat_GAME(0, 0), teamRat_GAME(0, 1): Print gameTeams$(0)
            Color , 0
            Locate 4, 2: Color 14, 0
            Print "(3) ";
            Color 15: Print "HOME TEAM: ";
            Color teamRat_GAME(1, 0), teamRat_GAME(1, 1): Print gameTeams$(1)
            Locate 5, 2: Color 14, 0
            Print "(4) ";
            Color 15: Print "GAME LOCATION: "; location$(gameLoc)
            Locate 6, 2: Color 14
            Print "(5) ";
            Color 15: Print "SHOT CLOCK: "; sClockOpt$(sClockVal)
            Locate 7, 2: Color 14
            Print "(6) ";
            Color 15: Print "3-POINT SHOT: "; yesNoText$(threePtOpt)
            Locate 8, 2: Color 14
            Print "(7) ";
            Color 15: Print "FOULS TO DISQUALIFY: "; foulsToDQ
            Locate 9, 2: Color 14
            Print "(8) ";
            Color 15: Print "FREE THROWS ON 10TH FOUL: "; foulShotOpt$(ft10thFoul)
            Locate 10, 2: Color 14
            Print "(9) ";
            Color 15: Print "START GAME"
            Locate 11, 2: Color 14
            Print "(0) ";
            Color 15: Print "SAVE SETTINGS"
            Locate 12, 2: Color 14
            Print "(A) ";
            Color 15: Print Using "PLAY-BY-PLAY DELAY (SECS): ##.#"; pbpDelay!
            Locate 13, 2: Color 14
            Print "(B) ";
            Color 15: Print "HUMAN COACHES: "; coachMode$(coachOpt);
            Locate 1, 1
            Locate 14, 2: Color 14
            Print "(C) ";
            Color 15: Print "3 FT WHEN FOULED ON 3-PT ATT: "; yesNoText$(ft3PtFoul)
            Locate 15, 2: Color 14
            Print "(D) ";
            Color 15: Print "1 FT ON 1ST 6 FLOOR FOULS : "; yesNoText$(ft6FloorFouls)
            Locate 16, 2: Color 14
            Print "(E) ";
            Color 15: Print "PLAY-BY-PLAY STYLE: "; pbpType$(pbpOpt)

            'Locate 19, 2: Print "NOTE::"
            'Print "        1 FT ON 1ST 6 FLOOR FOULS THROUGH 1972-73 SEASON"
            'Print "        45 SEC SHOT CLOCK ADOPTED 1984-85, BUT NOT FOR POST-SEASON"
            'Print "        3 PT SHOT ADOPTED IN 1986-87"
            'Print "        2 FT INSTEAD OF 1+1 AFTER 10TH FOUL ADOPTED 1990-91"
            'Print "        3 FT WHEN FOULED ON A 3 PT ATT ADOPTED 1990-91"
            'Print "        35 SEC SHOT CLOCK IN 1993-94; 30 SEC SHOT CLOCK IN 2015-16";

            Locate 18, 2: Color 14
            Print "NOTE:: "
            Color 15

            Locate , 3: Print "* 1 FT ON 1ST 6 FLOOR FOULS THROUGH 1972-73 SEASON"
            'Locate , 3: Print "* 45 SEC SHOT CLOCK ADOPTED 1984-85, BUT NOT IN TOURNAMENTS"
            Locate , 3: Print "* NO SHOT CLOCK UNTIL 1984-1985; 45 SEC CLOCK ADOPTED BUT NOT FOR TOURNAMENT"
            Locate , 3: Print "* 35 SEC SHOT CLOCK IN 1993-94; 30 SEC SHOT CLOCK IN 2015-16"
            Locate , 3: Print "* 3 PT SHOT ADOPTED IN 1986-87"
            Locate , 3: Print "* BEFORE 1990-91, 10TH FOUL ALLOWED 1+1 FT; NOW IT IS 2 FT"
            Locate , 3: Print "* STARTING 1990-91, 3 FT REWARDED WHEN FOULED ON A 3 PT ATT";

            Do
                Color 7

                I$ = GetKeyPress$

                If I$ <> Chr$(27) Then
                    iVal = Val(I$)
                    If UCase$(I$) = "A" Then iVal = 10
                    If UCase$(I$) = "B" Then iVal = 11
                    If UCase$(I$) = "C" Then iVal = 12
                    If UCase$(I$) = "D" Then iVal = 13
                    If UCase$(I$) = "E" Then iVal = 14
                End If

            Loop Until iVal >= 0 And iVal <= 14 Or I$ = Chr$(27) Or UCase$(I$) = "Q"

            If I$ <> Chr$(27) And UCase$(I$) <> "Q" Then

                Select Case iVal

                    Case 0:
                        Open "DEFAULT" For Output As #10
                        Print #10, playerOpt
                        Print #10, threePtOpt
                        Print #10, sClockVal
                        Print #10, gameLoc
                        Print #10, foulsToDQ
                        Print #10, pbpDelay!
                        Print #10, coachOpt
                        Print #10, ft10thFoul
                        Print #10, ft3PtFoul
                        Print #10, ft6FloorFouls
                        Print #10, pbpOpt
                        Close #10

                    Case 1:
                        playerOpt = playerOpt + 1
                        If playerOpt > 3 Then playerOpt = 0

                    Case 2:
                        Call ChooseGameTeam(YN$, numberTeams, tmIdx, gameTeams$(0), 0)
                        If tmIdx <> 0 Then
                            Call TEAMSETUP(0)
                            Call DISABLEPLAYERS(0)
                        End If

                    Case 3:
                        Call ChooseGameTeam(YN$, numberTeams, tmIdx, gameTeams$(1), 1)
                        If tmIdx <> 0 Then
                            Call TEAMSETUP(1)
                            Call DISABLEPLAYERS(1)
                        End If

                    Case 4:
                        gameLoc = gameLoc + 1
                        If gameLoc > 2 Then gameLoc = 1

                    Case 5:
                        sClockVal = sClockVal + 1
                        'If sClockVal > 2 Then sClockVal = 0
                        If sClockVal > 3 Then sClockVal = 0

                    Case 6:
                        threePtOpt = 1 - threePtOpt

                    Case 7:
                        foulsToDQ = foulsToDQ + 1
                        If foulsToDQ > 6 Then foulsToDQ = 5

                    Case 8:
                        ft10thFoul = 1 - ft10thFoul

                    Case 9:
                        '

                    Case 10:
                        Do
                            Locate 12, 32
                            Input pbpDelay!
                        Loop Until pbpDelay! >= 0

                    Case 11:
                        coachOpt = 1 - coachOpt

                    Case 12:
                        ft3PtFoul = 1 - ft3PtFoul

                    Case 13:
                        ft6FloorFouls = 1 - ft6FloorFouls

                    Case 14:
                        pbpOpt = 1 - pbpOpt

                End Select

            End If

        Loop Until (iVal = 9 And gameTeams$(0) <> "" And gameTeams$(1) <> "") Or I$ = Chr$(27)

        If I$ <> Chr$(27) And UCase$(I$) <> "Q" Then

            'TICKER PROMPT
            Cls
            Color 3

            Call TEAMBOX(7, 1)

            Locate 7, 11

            Print "IF YOU ARE USING THE 10 MINUTE TICKER OPTION FOR THIS GAME,"
            Locate , 11: Print "ENTER STARTING TIME FOR GAME IN THIS FORMAT"
            Locate , 11: Print "(EXAMPLES: 1:00P, 11:30A). OTHERWISE, JUST HIT ENTER"
            Print
            Locate , 11: Input "START TIME: "; startTime$

            L = Len(startTime$)

            If L < 4 Then
                startTime$ = "1:00P"
            Else
                If L = 5 Then
                    HR = Val(Left$(startTime$, 1))
                    MIN = Val(Mid$(startTime$, 3, 2))
                End If
                If L = 6 Then
                    HR = Val(Left$(startTime$, 2))
                    MIN = Val(Mid$(startTime$, 4, 2))
                End If
            End If

            tickerMeridian$ = Right$(startTime$, 1)

            If UCase$(tickerMeridian$) = "P" And Left$(startTime$, 2) <> "12" Then HR = HR + 12

            TM = HR * 60 + MIN
            tickerStart = Int(TM / 30) - 18

        Else

            'End
            'System
            Exit Sub

        End If

    End If

    '----------------------------------------
    '    AUTO-DETERMINE OTHER OPTIONS
    '----------------------------------------

    ' Determine computer team
    Select Case playerOpt

        'Case 0:
        'playerMode = 0
        'compTeam = 0

        Case 1:
            playerMode = 1
            compTeam = 0

        Case 2:
            playerMode = 1
            compTeam = 1

        Case 3:
            playerMode = 2
            'compTeam = 0

    End Select

    'playerMode represents the following values:
    '   0 = Human
    '   1 = Computer Team
    '   2 = PC vs. PC

    Call ChangeContributions

    Call LOADSTATS

    If autoPlay = 1 Then gameLoc = NG%(2)

    'Determine attendance
    If gameLoc = 2 Or avgAttendance&(1) = 0 Then
        'Neutral Site
        'Random # 2,000 - 14,000
        '-- Yes, some teams struggle to get 1,500
        '-- but it's rare, or defCoverage, or an outlier
        '-- Original game ranged 9,000 to 11,000
        actualAttendance& = Int(Rnd(1) * 12000) + 2000
    Else
        'Random # +/ 1,000 of team's average attendance
        actualAttendance& = avgAttendance&(1) + Int(Rnd(1) * 2000) - 1000
    End If

    For I = 0 To 1
        For I1 = 0 To 13
            TM%(I, I1) = 1200
        Next
    Next

    Select Case sClockVal
        Case 1: shotClock = 45
        Case 2: shotClock = 35
        Case 3: shotClock = 30
    End Select

    gameAvgTmStamina = (teamStamina(0) + teamStamina(1)) / 2

    For P9 = 0 To 1

        Call SETSTAMINA

        If playerMode = 1 And P9 = compTeam Or playerMode = 2 Then
            Call COMCOACH
        Else
            Call LINEUPS
            Call FOULSAFE
        End If

        If threePtOpt = 1 Then

            For I = 0 To 13

                M8 = 100 - plyrOff_GAME!(P9, I, 19)

                If M8 <> 0 Then
                    M9 = 100 / M8
                End If

                plyrOff_GAME!(P9, I, 16) = Int((plyrOff_GAME!(P9, I, 16) * M9) + .5)

            Next I

        End If

    Next P9

    Call CheckGameClock
    Call SCOREBOARD
    Call STEALADJ

    250 '
    prevBall$ = ""

    'Determine which team gets the ball 1st
    P = Int(Rnd(1) * 2) '0 or 1

    If P = 1 Then
        P = 0
        D = 1
        P9 = 0
        C1 = 2
    Else
        P = 1
        D = 0
        P9 = 1
        C1 = 2
    End If

    Call LASTSHOT
    Call CheckGameClock
    If gameClock! = 0 Then 170

    Call ResetShotClock

    If pbpDelay! <> 0 Then

        If (currHalf = 1 Or currHalf = 3) Then

            If pbpOpt = 0 Then
                pbpString$ = gameTeams$(P) + " GETS THE JUMP"
                possArrow = D
                Call PBP
            Else
                'pbpOpt = 1
                Call WINDEX
                Print gameTeams$(P) + " GETS THE JUMP"
                possArrow = D
                Call DELAY
            End If

        End If

        If (currHalf = 2 Or currHalf >= 4) Then

            P = possArrow
            D = 1 - P
            possArrow = 1 - possArrow

            If pbpOpt = 0 Then
                pbpString$ = gameTeams$(P) + " INBOUNDS THE BALL"
                Call PBP
            Else
                'pbpOpt = 1
                Call WINDEX
                Print gameTeams$(P) + " INBOUNDS THE BALL"
                Call DELAY
            End If

        End If

    Else

        If (currHalf = 1 Or currHalf = 3) Then
            possArrow = D
        Else
            P = possArrow
            D = 1 - P
            possArrow = 1 - possArrow
        End If

    End If

    GoTo 300

    '----- NOODLE -----

    170 '
    Erase defStrat, defCoverage, defCategory

    gameClock! = 0
    Call QUARTERTOTAL
    currHalf = currHalf + 1
    gameClock! = 1200
    XS = 1
    X = 0

    If currHalf = 2 Then
        pbpString$ = "END OF HALF"
        Select Case pbpOpt
            Case 0:
                Color 7, 0
                Call WINDEX
                Print pbpString$
            Case 1:
                If pbpDelay! > 0 Then
                    Call WINDEX
                    Print pbpString$
                End If
        End Select
    End If

    If currHalf > 3 Then
        pbpString$ = "END OF OVERTIME" + Str$(currHalf - 3)
        Select Case pbpOpt
            Case 0:
                Color 7, 0
                Print pbpString$
            Case 1:
                If pbpDelay! > 0 Then
                    Call WINDEX
                    Print pbpString$
                End If
        End Select
    End If

    If currHalf = 2 Then

        pbpString$ = "HALFTIME!"

        Select Case pbpOpt
            Case 0:
                Color 7, 0
                Call WINDEX
                Print pbpString$
            Case 1:
                Call WINDEX
                Print pbpString$
                Call DELAY
        End Select

        halfTime = 1
        Call BOXOPTIONS(0)
        Call TICKER
        halfTime = 0

        Call FOULHARD

    End If

    If currHalf < 3 And playerMode <> 0 Then

        If playerMode = 1 Then
            P9 = compTeam
            Call COMCOACH
        End If

        If playerMode = 2 Then
            P9 = 0
            Call COMCOACH
            P9 = 1
            Call COMCOACH
        End If

    End If

    If currHalf < 3 Then

        teamFouls(0) = 0
        teamFouls(1) = 0

        Call CheckGameClock
        Call SCOREBOARD

        Print
        ballCarrier = -1

        Call DEADBALLOPTIONS

        gameClock! = 1200

        Call UPDATESCREEN

        Erase tmFatigue

        For I = 0 To 1
            For I1 = 0 To 13:
                TM%(I, I1) = 1200
            Next
        Next

        GoTo 250

    End If

    'Considered doing an ELSE here
    'Would have to be careful with
    'all involved conditionals to
    'make sure we reach end game
    Call CheckGameClock
    Call SCOREBOARD

    If currHalf = 3 Then

        pbpString$ = "END OF REGULATION"
        Select Case pbpOpt
            Case 0:
                Color 7, 0
                Call WINDEX
                Print pbpString$
            Case 1:
                If pbpDelay! > 0 Then
                    Call WINDEX
                    Print pbpString$
                End If
        End Select
    End If

    Locate 2, 31
    Print "END OF REGULATION"

    'If scores are tied, it's Overtime!
    If score(0, 0) = score(1, 0) Then

        pbpString$ = "OVERTIME !!!"

        Select Case pbpOpt
            Case 0:
                Color 7, 0
                Call WINDEX
                Print pbpString$
            Case 1:
                Call WINDEX
                Print pbpString$
                Call DELAY
        End Select

        timeouts(0) = timeouts(0) + 1
        timeouts(1) = timeouts(1) + 1
        gameClock! = 300

        Call UPDATESCREEN

        Erase tmFatigue

        For I = 0 To 1
            For I1 = 0 To 13:
                TM%(I, I1) = 300
            Next
        Next

        ballCarrier = -1

        Call DEADBALLOPTIONS
        Call SCOREBOARD

        GoTo 250

    End If

    If Not (autoPlay = 1 And playerMode = 2) Then
        I$ = GetKeyPress
    End If

    '--Start "EndOfGame" routine
    'Close #11

    Call POSTGAME

    ' Prompt to play another game
    Cls

    If autoPlay <> 1 Then

        'Using Home Team's color for the box
        Call TEAMBOX(1, 1)

        Do
            U$ = "PLAY ANOTHER GAME (Y/N)?"
            Locate 7, (80 - Len(U$)) / 2
            Print U$

            Color 15, 0
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    End If

    If UCase$(I$) = "Y" Or (autoPlay = 1 And endAllGames <> 1) Then
        Cls
        Print "PLEASE WAIT ..."
        Call COLHOOP
    End If

    '--End "EndOfGame" routine

    Exit Sub

    '----------------------------------------
    '           END OF GAME!!!
    '----------------------------------------

    '================================================================================
    ' Below are all the different GoTo branches for the gameplay
    ' These have not yet been worked out into a different flow
    ' yet for the game's logic.
    '================================================================================

    TIMEEXP:
    timeElapsed! = gameClock!
    Call AdjustPlayTime
    gameClock! = 0
    Call CheckGameShotClock

    GoTo 170

    '----- NOODLE -----

    300 '

    If gameClock! <= 5 And timeouts(D) <= 0 And score(P, 0) > score(D, 0) Then
        timeElapsed! = gameClock!
        Call AdjustPlayTime
        gameClock! = 0
        Call CheckGameShotClock

        Call WINDEX
        Print gameTeams$(P); " RUNS THE CLOCK OUT!"
        Call DELAY
        Call DELAY

        GoTo 170

    End If

    'Last 5 seconds, down by 1 score!!!
    scoreDiff = score(D, 0) - score(P, 0)
    If gameClock! <= 5 And (threePtOpt = 1 And scoreDiff <= 3 Or threePtOpt = 0 And scoreDiff <= 2) And scoreDiff >= 0 And currHalf >= 2 Then

        If playerMode = 2 Or playerMode = 1 And P = compTeam Then

            ' Computer handle last 5 seconds
            Call Rolld100(RN0)

            scoreDiff = score(P, 0) - score(D, 0)

            If gameClock! >= 3 And gameClock! <= 5 Then

                If threePtOpt = 0 Then
                    If scoreDiff < 3 Then
                        I1% = 5
                    End If
                Else
                    'threePtOpt = 1
                    If scoreDiff = 3 Then
                        I1% = 4
                    End If
                    If scoreDiff = 2 Then
                        If RN0 <= 50 Then
                            I1% = 5
                        Else
                            I1% = 4
                        End If
                    End If

                End If

                If gameClock! < 3 Then
                    If threePtOpt = 0 Then
                        If scoreDiff < 3 Then
                            I1% = 0
                        End If
                    Else
                        'threePtOpt = 1
                        If scoreDiff = 3 Then
                            I1% = 1
                        End If
                        If scoreDiff = 2 Then
                            If RN0 <= 50 Then
                                I1% = 0
                            Else
                                I1% = 1
                            End If
                        End If
                    End If
                End If

            End If

        Else

            Call LASTFIVEMENU

            280 '
            Do
                I$ = GetKeyPress$
                I1% = Val(I$)
            Loop Until I1% >= 0 And I1% <= 5 And Not (I1% = 3 And timeouts(P9) <= 0) And ((I1% = 1 Or I1% = 4) And threePtOpt = 1)

            Color 15, 0
            Call SCOREBOARD

        End If

        ballCarrier = Int(Rnd(1) * 5)

        Call Rolld100(RN0)

        ' 0 | 1 = Full Court Pass [Shoot 2 | Shoot 3]
        '2 = Shoot from Back Court
        '3 = TO from Half Court
        '4 | 5 = Work the Floor [Shoot 2 | Shoot 3]

        If I1% = 0 And RN0 <= 50 Or I1% = 1 And RN0 <= 33 Then

            pbpString$ = "LONG PASS DOWN COURT"
            Call PBP
            Call DELAY
            pbpString$ = "IT'S INTERCEPTED...GAME OVER!!"
            Call PBP
            GoTo TIMEEXP

        End If

        If I1% = 2 Then

            rndFtLine = Int(Rnd(1) * 45) + 45

            pbpString$ = players$(P, B1(P, ballCarrier)) + " HAS THE BALL"
            Call PBP

            pbpString$ = "HE PUTS IT UP FROM" + Str$(rndFtLine) + " FEET"
            Call PBP
            Call DELAY

            If RN0 = 61 Then

                pbpString$ = "GOOD!!"
                Call PBP

                'Reward at least 2 points (depends on if 3-pt shot is supported)
                score(P, currHalf) = score(P, currHalf) + 2
                score(P, 0) = score(P, 0) + 2

                Call CreditAssists

                plyrOff_GAME!(P, B1(P, ballCarrier), 8) = plyrOff_GAME!(P, B1(P, ballCarrier), 8) + 1
                plyrOff_GAME!(P, B1(P, ballCarrier), 7) = plyrOff_GAME!(P, B1(P, ballCarrier), 7) + 1

                If threePtOpt = 1 Then

                    'Add the "3rd" point
                    score(P, currHalf) = score(P, currHalf) + 1
                    score(P, 0) = score(P, 0) + 1

                    'Increase 3FGM + 3FGA
                    threeFG(P, B1(P, ballCarrier), 1) = threeFG(P, B1(P, ballCarrier), 1) + 1
                    threeFG(P, B1(P, ballCarrier), 0) = threeFG(P, B1(P, ballCarrier), 0) + 1

                End If

                Call UPDATESCREEN

                GoTo TIMEEXP

            Else

                pbpString$ = "SHOT MISSES...GAME OVER"
                Call PBP

                plyrOff_GAME!(P, B1(P, ballCarrier), 7) = plyrOff_GAME!(P, B1(P, ballCarrier), 7) + 1

                If threePtOpt = 1 Then
                    threeFG(P, B1(P, ballCarrier), 1) = threeFG(P, B1(P, ballCarrier), 1) + 1
                End If

                GoTo TIMEEXP

            End If

        End If

        If I1% >= 3 Then

            Select Case RN0

                Case Is <= 8
                    pbpString$ = "BALL LOST O.B.  GAME OVER"
                    Call PBP
                    GoTo TIMEEXP

                Case 9 To 18
                    pbpString$ = "PASS INTERCEPTED"
                    Call PBP
                    GoTo TIMEEXP

            End Select

        End If

        If I1% = 3 Then

            If gameClock! < 1.5 Then

                pbpString$ = "INBOUND PASS TO " + players$(P, B1(P, ballCarrier))
                Call PBP

                pbpString$ = "TIME EXPIRES"
                Call PBP

                GoTo TIMEEXP

            Else

                timeElapsed! = (Rnd(1) + 1.5)
                gameClock! = gameClock! - timeElapsed!

                Call AdjustPlayTime
                Call AdjustShotClock

                pbpString$ = gameTeams$(P) + " GETS THE BALL TO HALF COURT"
                Call PBP

                pbpString$ = "THEY QUICKLY CALL FOR A TIMEOUT"
                Call PBP

                timeouts(P) = timeouts(P) - 1

                Call DEADBALLOPTIONS
                Call SCOREBOARD
                Call HalfCourt(I2%)
            End If

        End If


        If I1% >= 4 And gameClock! < 3 Then

            pbpString$ = "INBOUND PASS TO " + players$(P, B1(P, ballCarrier))
            Call PBP

            pbpString$ = "TIME EXPIRES"
            Call PBP

            GoTo TIMEEXP
        End If


        '-- Previously If I1% = 0 Or I1% = 4 Or (I1% = 3 And I2% = 1) Then
        If I1% = 1 Or I1% = 4 Or (I1% = 3 And I2% = 1) Then
            shootFor3 = 1
        Else
            shootFor3 = 0
        End If

        If shootFor3 = 1 And threePtOpt = 1 Then

            ' Shoot for 3!!!
            Call Rolld100(RN0)
            Select Case gameClock!
                Case Is <= .5
                    XX% = 2
                Case .5 To 1.8
                    XX% = 12
                Case Is > 1.8
                    XX% = 20
            End Select

            If RN0 > XX% Then

                pbpString$ = players$(P, B1(P, ballCarrier)) + " MISSES"
                Call PBP

                plyrOff_GAME!(P, B1(P, ballCarrier), 7) = plyrOff_GAME!(P, B1(P, ballCarrier), 7) + 1
                threeFG(P, B1(P, ballCarrier), 1) = threeFG(P, B1(P, ballCarrier), 1) + 1

            Else

                pbpString$ = "BASKET BY " + players$(P, B1(P, ballCarrier)) + "!!"
                Call PBP

                plyrOff_GAME!(P, B1(P, ballCarrier), 7) = plyrOff_GAME!(P, B1(P, ballCarrier), 7) + 1
                plyrOff_GAME!(P, B1(P, ballCarrier), 8) = plyrOff_GAME!(P, B1(P, ballCarrier), 8) + 1

                score(P, currHalf) = score(P, currHalf) + 3
                score(P, 0) = score(P, 0) + 3

                'Increase 3FGM + 3FGA
                threeFG(P, B1(P, ballCarrier), 1) = threeFG(P, B1(P, ballCarrier), 1) + 1
                threeFG(P, B1(P, ballCarrier), 0) = threeFG(P, B1(P, ballCarrier), 0) + 1

                Call CreditAssists
                Call UPDATESCREEN

            End If

        Else

            'Go for 2!!!
            Call Rolld100(RN0)

            Select Case gameClock!
                Case Is <= .5
                    XX% = 3
                Case .5 To 1.8
                    XX% = 16
                Case Is > 1.8
                    XX% = 29
            End Select

            If RN0 > XX% Then

                pbpString$ = players$(P, B1(P, ballCarrier)) + " MISSES"
                Call PBP
                plyrOff_GAME!(P, B1(P, ballCarrier), 7) = plyrOff_GAME!(P, B1(P, ballCarrier), 7) + 1

            Else

                pbpString$ = "BASKET BY " + players$(P, B1(P, ballCarrier)) + "!!"
                Call PBP

                plyrOff_GAME!(P, B1(P, ballCarrier), 7) = plyrOff_GAME!(P, B1(P, ballCarrier), 7) + 1
                plyrOff_GAME!(P, B1(P, ballCarrier), 8) = plyrOff_GAME!(P, B1(P, ballCarrier), 8) + 1

                score(P, currHalf) = score(P, currHalf) + 2
                score(P, 0) = score(P, 0) + 2

                Call CreditAssists
                Call UPDATESCREEN

            End If

        End If

        GoTo TIMEEXP

    End If

    'If we can work the above into a better structure,
    'all branches end the same so we can shortcut this:

    'GoTo TIMEXP

    'Else

    FO = 0
    LS = 0
    XS = 0

    Select Case sClockVal
        Case 1: shotClock = 45
        Case 2: shotClock = 35
        Case 3: shotClock = 30
    End Select

    LS = 0

    If gameClock! < 6 And LT <> 4 Then LS = 4

    playChoice = 0
    D6 = 0
    A4 = 0
    A6 = 0
    PASS = 1
    shotChance! = 0

    If X <> 90 Then X = 0

    If (defCoverage(D) = 1 Or defCoverage(D) = 3) And defCategory(D) = 0 Then
        X0% = 50
        X1% = -1
        Call PRDFAT
    End If

    If defCoverage(D) = 2 And defCategory(D) = 0 Then
        X0% = 100
        X1% = 1
        Call PRDFAT
        Call PRPFAT
    End If

    If defCoverage(D) = 1 And defCategory(D) = 0 Then
        X0% = 25
        X1% = -1
        Call PRPFAT
    End If

    GoTo 400

    'End If

    '----- NOODLE -----

    350 '
    PASS = PASS + 1
    playChoice = 0
    A4 = 0
    D6 = 0
    A6 = 1
    shotChance! = 0
    X = 0

    If PASS = 2 And defCategory(D) > 0 Then

        If (defCoverage(D) = 1 Or defCoverage(D) = 3) Then
            X0% = 50
            X1% = -1
            Call PRDFAT
        End If

        If defCoverage(D) = 2 Then
            X0% = 100
            X1% = 1
            Call PRDFAT
            Call PRPFAT
        End If

        If defCoverage(D) = 1 Then
            X0% = 25
            X1% = -1
            Call PRPFAT
        End If

    End If

    GoTo 400

    '----- NOODLE -----

    400 '
    A4 = 0
    D6 = 0
    P3 = 0
    ballCarrier = Int(Rnd(1) * 5)
    Call BALLDISTRIBUTE

    If defCategory(D) > 0 And PASS = 1 Then

        ' *** FULL COURT PRESS ***

        'Yes it's odd that Case 1, 2 and 4
        'have identical outcome.
        'That is the code as it was written.
        'Leaving as is in case there would
        'be a perceived need to adjust.
        'My guess is they were Previously
        'different in the past.

        Select Case defCategory(D)

            Case 1:
                X0% = 50
                X1% = -1
            Case 2, 4:
                X0% = 50
                X1% = -1
            Case 3:
                X0% = 25
                X1% = -1

        End Select

        Call PRDFAT

        'Check for Denial / Foul
        If currHalf >= 2 And gameClock! <= 180 And defCategory(D) = 5 Then

            Call Rolld100(RN0)

            If RN0 <= 10 Then

                gameClock! = gameClock! - 4
                timeElapsed! = 4
                Call AdjustPlayTime
                Call AdjustShotClock
                Call WHOLAY

                If PASS = 1 Then
                    pbpString$ = players$(P, B1(P, ballCarrier)) + " GETS THE INBOUND PASS"
                Else
                    pbpString$ = players$(P, B1(P, ballCarrier)) + " HAS THE BALL"
                End If

                Call PBP
                pbpString$ = "HE IS STREAKING DOWN THE FLOOR!"
                Call PBP

                LAYUP:
                fastBreak = 1
                shotChance! = 98
                playChoice = 1

                GoTo 520

            End If

            Call Rolld100(RN1)
            Call Rolld100(RN0)

            IN = 99
            C1 = 2
            P9 = D

            pbpString$ = "DESIGNATED FOULER (0-4)"
            Call SELECTPBP

            skipInbound% = 0

            If playerMode = 1 And D = compTeam Or playerMode = 2 Then
                desigFouler = Int(Rnd(1) * 5)
            Else
                Do
                    I$ = GetKeyPress$
                    desigFouler = Val(I$)
                Loop Until desigFouler >= 0 And desigFouler <= 4

                If PASS > 1 Then skipInbound% = 1

            End If

            If skipInbound% = 0 Then

                If RN1 <= 4 And timeouts(P) > 0 Then
                    pbpString$ = gameTeams$(P) + " CAN'T INBOUND THE BALL"
                    Call SELECTPBP

                    pbpString$ = gameTeams$(P) + " CALLS A TIME OUT"

                    If pbpOpt = 1 And pbpDelay! <> 0 Then
                        Print pbpString$
                        Call DELAY
                    End If
                    If pbpOpt = 0 Then Call PBP

                    PASS = 0
                    GoTo 640

                End If

                If RN1 <= 4 And timeouts(P) = 0 Or RN1 > 4 And RN1 <= 7 Then
                    pbpString$ = gameTeams$(P) + " CAN'T INBOUND THE BALL"
                    Call SELECTPBP

                    pbpString$ = "5 SECOND VIOLATION...TURNOVER"

                    SV% = 1
                    GoTo 517

                End If

                CY = 7 + DP(0, 4)
                If RN1 <= CY Then

                    Select Case RN0

                        Case 1 To 80:
                            gameClock! = gameClock! + 4
                            timeElapsed! = -4
                            Call AdjustPlayTime
                            Call AdjustShotClock

                        Case 81 To 90:
                            gameClock! = gameClock! - 3
                            timeElapsed! = 3
                            Call AdjustPlayTime
                            Call AdjustShotClock

                    End Select

                    Call LASTSHOT
                    GoTo 517

                End If

                CY = CY + teamAdj(P, 4) + DP(1, 4)

                If RN1 <= CY Then

                    Select Case RN0

                        Case 1 To 80:
                            gameClock! = gameClock! + 4
                            timeElapsed! = -4
                            Call AdjustPlayTime
                            Call AdjustShotClock

                        Case 81 To 90:
                            gameClock! = gameClock! - 3
                            timeElapsed! = 3
                            Call AdjustPlayTime
                            Call AdjustShotClock

                    End Select

                    'Check for steal
                    GoTo 435

                Else
                    432 '
                    pbpString$ = "PASS TO " + players$(P, B1(P, ballCarrier))
                    Call SELECTPBP

                End If

            End If

            Call Rolld100(RN0)
            If RN0 <= 5 Then intentional = 1

            Call Rolld100(RN1)

            Select Case RN1

                Case 1 To 80:
                    timeElapsed! = Int(Rnd(1) * 3) + 1

                Case 81 To 90:
                    timeElapsed! = Int(Rnd(1) * 3) + 8

                Case 91 To 100:
                    timeElapsed! = Int(Rnd(1) * 3) + 5

            End Select

            gameClock! = gameClock! - timeElapsed!
            Call AdjustPlayTime
            Call AdjustShotClock

            Call Rolld100(RN2)

            '80% chance of foul by designated person
            If RN2 <= 80 Then
                fouler = desigFouler
                Call FoulCalled(0)
            Else
                Call FoulCalled(1)
            End If

            GoTo 9065

        End If

        If defCategory(D) = 1 Then
            X0% = 25
            X1% = -1
            tmFatigue(P, B1(P, BU%)) = tmFatigue(P, B1(P, BU%)) - 1
            Call PRPFAT
        End If

        If defCategory(D) > 1 Then
            X0% = 25
            X1% = -1
            Call PRPFAT
        End If

        Call Rolld100(RN0)

        Select Case defCategory(D)

            Case 1, 3
                gameClock! = gameClock! - 2
                timeElapsed! = 2

            Case 2
                If RN0 <= 75 Then
                    gameClock! = gameClock! - 2
                    timeElapsed! = 2
                Else
                    gameClock! = gameClock! + 2
                    timeElapsed! = -2
                End If

            Case 4
                If RN0 <= 50 Then
                    gameClock! = gameClock! - 2
                    timeElapsed! = 2
                Else
                    gameClock! = gameClock! + 2
                    timeElapsed! = -2
                End If

        End Select

        Call AdjustPlayTime
        Call AdjustShotClock

        Call Rolld100(RN0)

        If RN0 <= DP(2, defCategory(D)) Then

            Call LASTSHOT
            Call ResetShotClock
            Call UPDATESCREEN

            X = 88

            Call FoulCalled(1)

            LT = 4

            GoTo 9065

        End If

        'We have some near identical behavior below,
        'and these are some of the only places these
        'code branches "GoTo". But I don't think it
        'can be condensed at all.
        If RN0 <= DP(2, defCategory(D)) + 1 Then
            X = 0
            GoTo 580
        End If

        CY = DP(2, defCategory(D)) + 1 + teamAdj(P, 5) + teamAdj(D, 6) + DP(0, defCategory(D)) - PR%(D, 0)

        If RN0 <= CY Then
            Call LASTSHOT
            GoTo 517
        End If

        CY = CY + teamAdj(P, 4) + DP(1, defCategory(D)) - PR%(D, 1)
        pctContrib(D) = pctContrib(D) + 1

        If pctContrib(D) > 3 Then
            PR%(D, 0) = 0
            PR%(D, 1) = 0
        End If

        If RN0 <= CY Then
            'Check for steal
            GoTo 435
        End If

    Else

        Call Rolld100(RN0)

        If (offStrat(P) <> 8 And offStrat(P) <> 7) Or defCoverage(D) <> 2 Then
            'Offensive Stall or Defense is not 2-3 Zone

            If RN0 <= D8(4, defCoverage(D)) Then

                Call LASTSHOT
                Call ResetShotClock
                Call UPDATESCREEN

                X = 88

                Call FoulCalled(1)

                LT = 4

                GoTo 9065

            End If

            If RN0 <= D8(4, defCoverage(D)) + 1 Then
                X = 0
                GoTo 580
            End If

            OY% = 0
            If offStrat(P) = 4 Or offStrat(P) = 7 Then
                OY% = Int((D8(2, defCoverage(D)) / 2) + .5) * -1
            End If

            CY = D8(4, defCoverage(D)) + 1 + teamAdj(P, 5) + teamAdj(D, 6) + D8(2, defCoverage(D)) + OY% + 1
            If RN0 <= CY Then
                Call LASTSHOT
                GoTo 517
            End If

            CY = CY + teamAdj(P, 4) + D8(3, defCoverage(D))
            OY% = 0

            If offStrat(P) = 4 Or offStrat(P) = 7 Then
                OY% = Int((D8(3, defCoverage(D)) / 2) + .5) * -1
            End If

            If offStrat(P) = 1 Then
                OY% = 7
            End If

            If offStrat(P) = 2 Then
                OY% = -7
            End If

            If RN0 <= CY + OY% Then
                'Check for steal
                GoTo 435
            End If

        End If

    End If

    '-----

    465 '
    Call Rolld100(RN0)

    'Adjust likelihood of taking a shot baed on defense
    GS% = ShotPacing%

    'Pressure Man-to-Man | 2-3 Zone = Add 7
    '-1 to 12
    If defCoverage(D) = 1 Or defCoverage(D) = 2 Then
        GS% = GS% + 7
    End If

    'Trapping Zone = Subtract 7
    '-15 to -2
    If defCoverage(D) = 3 Then
        GS% = GS% - 7
    End If

    'Quick Off
    '6 to 23
    If offStrat(P) = 3 Then
        GS% = GS% + 21
    End If

    'Delay Off
    '-36 to -23
    If offStrat(P) = 4 Then
        GS% = GS% - 21
    End If

    'Total Stall / Hold Ball / Milk Clock and More than 15 Secs on Shot Clock
    If offStrat(P) = 7 Or offStrat(P) = 8 Or (offStrat(P) = 5 And shotClock > 15 And sClockVal >= 1) Or (RN0 > GS%) Then
        'If offStrat(P) = 7 Or offStrat(P) = 8 Or (offStrat(P) = 5 And shotClock > 15 And sClockVal >= 1) Or (RN0 > 99) Then
        'No Shot / Difficult Shot
        GoTo 437
    Else

        Call Rolld100(RN1)
        If RN1 <= D8(1, defCoverage(D)) Then

            A4 = 0

            If Not (currHalf = 2 And gameClock! <= 120 Or currHalf > 2) Then

                If currHalf <= 2 Then
                    If plyrOff_GAME!(P, B1(P, ballCarrier), 3) - plyrOff_GAME!(P, B1(P, ballCarrier), 7) - plyrOff_GAME!(P, B1(P, ballCarrier), 11) - plyrOff_GAME!(P, B1(P, ballCarrier), 12) <= 0 Then 437
                End If

            End If

            Call SHOTPERCENT

            A4 = Int(Rnd(1) * 28) + 1
            RN2 = Int(Rnd(1) * 9) + 1
            I2 = Int(Rnd(1) * 5)

            If tmFatigue(P, B1(P, ballCarrier)) > -5 Then

                If RN2 <= plyrOff_GAME!(P, B1(P, I2), 4) Then

                    shotChance! = plyrOff_GAME!(P, B1(P, ballCarrier), 0) + dFGPA(D) + A4 + ft3PtFoul(P) + ft3PtFoul(D)
                    ft3PtFoul(P) = 0
                    ft3PtFoul(D) = 0
                    PZ% = 2
                    playChoice = 1
                    X = 0

                    Call Rolld100(RN3)

                    If PZ% = 2 And RN3 <= 50 Then
                        PZ% = 3
                        playChoice = 1
                    End If

                    GoTo 520

                End If

            End If

        End If

        'Delay Offense
        If offStrat(P) = 4 Then
            GoTo 437

        Else

            If currHalf <= 2 Then

                If Not (currHalf = 2 And gameClock! <= 120 Or currHalf > 2) Then
                    If plyrOff_GAME!(P, B1(P, ballCarrier), 3) - plyrOff_GAME!(P, B1(P, ballCarrier), 7) - plyrOff_GAME!(P, B1(P, ballCarrier), 11) - plyrOff_GAME!(P, B1(P, ballCarrier), 12) <= 0 Then
                        GoTo 437
                    End If
                End If

            End If

            playChoice = 1
            Call SHOTPERCENT

            PZ% = Int(Rnd(1) * 3) + 5

            X = 0

            Call Rolld100(RN0)
            If PZ% = 2 And RN0 <= 50 Then
                PZ% = 3
                playChoice = 1
            End If

            GoTo 520

        End If

    End If

    '----- NOODLE -----

    ' ***** CHECK FOR STEALS *****
    435 '
    'Random player (0-4)
    P9 = Int(Rnd(1) * 5)

    'Compare random # 1-4 against that player's steal rating
    '(basically, average steals / game)
    stealRating = plyrOff_GAME!(D, P9, 5)
    RNX = Int(Rnd(1) * 4) + 1

    If RNX <= (stealRating) Then
        'Successful steal!
        plyrOff_GAME!(D, B1(D, P9), 18) = plyrOff_GAME!(D, B1(D, P9), 18) + 1

        If defCategory(D) <= 0 Or PASS <> 1 Then Call LASTSHOT
        Call ResetShotClock

        plyrOff_GAME!(P, B1(P, ballCarrier), 23) = plyrOff_GAME!(P, B1(P, ballCarrier), 23) + 1

        If pbpDelay! <> 0 Then

            pbpString$ = "BALL STOLEN BY " + players$(D, B1(D, P9)) + "!"

            If pbpOpt = 1 Then
                Call WINDEX
                Print pbpString$
                Call DELAY
            Else

                Call Rolld100(RN0)

                Select Case RN0
                    Case 1 To 3
                        pbpString$ = players$(D, B1(D, P9)) + " QUICKLY REACHES IN!"
                        Call PBP
                    Case 4 To 6
                        pbpString$ = players$(D, B1(D, P9)) + " SNEAKS IN FROM BEHIND!"
                        Call PBP
                    Case 7 To 10
                        pbpString$ = players$(D, B1(D, P9)) + " GETS A HAND ON THE BALL!"
                        Call PBP
                        'No case for 11 to 100
                End Select

                RN1 = Int(Rnd(1) * 20) + 1

                Select Case RN1

                    Case 1
                        pbpString$ = "PASS INTERCEPTED BY " + players$(D, B1(D, P9)) + "!"
                    Case 2
                        pbpString$ = "PASS IS PICKED OFF BY " + players$(D, B1(D, P9)) + "!"
                    Case 3
                        pbpString$ = "BALL IS STOLEN BY " + players$(D, B1(D, P9)) + "!"
                    Case 4
                        pbpString$ = players$(D, B1(D, P9)) + " WITH THE STEAL!"
                    Case 5
                        pbpString$ = players$(D, B1(D, P9)) + " TAKES IT AWAY!"
                    Case 6
                        pbpString$ = players$(D, B1(D, P9)) + " STEALS IT!"
                    Case 7
                        pbpString$ = players$(D, B1(D, P9)) + " RIPS IT LOOSE!"
                    Case 8
                        pbpString$ = players$(D, B1(D, P9)) + " TAKES THE BALL AWAY!"
                    Case 9
                        pbpString$ = players$(D, B1(D, P9)) + " STEALS THE BALL!"
                    Case 10
                        pbpString$ = players$(D, B1(D, P9)) + " PICKS IT OFshotChance!"
                    Case 11
                        pbpString$ = players$(D, B1(D, P9)) + " SWIPES THE BALL!"
                    Case 12
                        pbpString$ = players$(D, B1(D, P9)) + " WITH THE STOLEN BALL!"
                    Case 13
                        pbpString$ = players$(D, B1(D, P9)) + " STRIPS IT AWAY!"
                    Case 14
                        pbpString$ = players$(D, B1(D, P9)) + " WITH THE STEAL!"
                    Case 15
                        pbpString$ = players$(D, B1(D, P9)) + " SWIPES AND STEALS IT!"
                    Case 16
                        pbpString$ = players$(D, B1(D, P9)) + " SNATCHES IT AWAY!"
                    Case 17
                        pbpString$ = players$(D, B1(D, P9)) + " QUICKLY REACHES IN!"
                    Case 18
                        pbpString$ = players$(D, B1(D, P9)) + " TAKES IT THE OTHER WAY!"
                    Case 19
                        pbpString$ = players$(D, B1(D, P9)) + " POKES IT LOOSE!"
                    Case 20
                        pbpString$ = players$(D, B1(D, P9)) + " STRIPS IT CLEAN!"

                End Select

                Call PBP

            End If

        End If

        plyrOff_GAME!(D, B1(D, P9), 13) = plyrOff_GAME!(D, B1(D, P9), 13) + 1
        turnovers(P) = turnovers(P) + 1

        Call UPDATESCREEN

        C1 = 2
        P9 = D

        If gameClock! <= 0 Then 170

        prevBall$ = ""
        If P = 1 Then
            P = 0
            D = 1
            P9 = 0
            C1 = 2
        Else
            'P = 0
            P = 1
            D = 0
            P9 = 1
            C1 = 2
        End If

        fastBreak = 1

        Call Rolld100(RN0)

        Select Case RN0
            Case Is <= 38
                ballCarrier = 0
            Case 39 To 75
                ballCarrier = 1
            Case 76 To 83
                ballCarrier = 2
            Case 84 To 91
                ballCarrier = 3
            Case 92 To 100
                ballCarrier = 4
        End Select

        Call Rolld100(RN1)

        If PASS > 1 Then
            Select Case defCoverage(P9)
                Case 0, 1, 3
                    If RN1 <= 15 Then GoTo LAYUP
                Case 2
                    If RN1 <= 5 Then GoTo LAYUP
            End Select
        Else
            'PASS = 1
            If defCategory(P) > 0 Then
                If RN1 <= 33 Or (defCategory(P) <> 3 And RN1 <= 40) Then
                    GoTo LAYUP
                End If
            End If
        End If

        GoTo 300

    Else

        'No steal - layup or take a shot!
        Call Rolld100(RN0)

        Select Case defCategory(D)

            Case 1
                If PASS = 1 And RN0 <= plyrOff_GAME!(P, B1(P, BU%), 4) Then
                    GoTo LAYUP
                End If

                If PASS = 1 And RN0 <= plyrOff_GAME!(P, B1(P, BU%), 4) * 2 Then
                    GoTo GDSHOT
                End If

            Case 2
                If PASS = 1 And RN0 <= plyrOff_GAME!(P, B1(P, BU%), 4) * 2 Then
                    GoTo LAYUP
                End If
                If PASS = 1 And RN0 <= plyrOff_GAME!(P, B1(P, BU%), 4) * 4 Then
                    GoTo GDSHOT
                End If

            Case 3
                If PASS = 1 And RN0 <= 7 Then
                    GoTo LAYUP
                End If
                If PASS = 1 And RN0 <= 14 Then
                    GoTo GDSHOT
                End If

            Case 4
                If PASS = 1 And RN0 <= 15 Then
                    GoTo LAYUP
                End If
                If PASS = 1 And RN0 <= 30 Then
                    GoTo GDSHOT
                End If

            Case 5
                GoTo 432

        End Select

    End If



    ' ***BAD SHOT ***
    437 '
    playChoice = 0
    J8% = 0
    PZ% = 1

    RN0 = Int(Rnd(1) * 3) + 1
    threePtShotChance = Int(Rnd(1) * 7) + 3

    'Adjust shot % based on player's FG%
    If plyrOff_GAME!(P, B1(P, ballCarrier), 0) > 60 Then

        Select Case RN0
            Case 1
                shotChance! = 24
            Case 2
                shotChance! = 30
            Case 3
                shotChance! = 36
        End Select

    Else

        Select Case RN0
            Case 1
                shotChance! = Int(plyrOff_GAME!(P, B1(P, ballCarrier), 0) * .4 + .5)
            Case 2
                shotChance! = Int(plyrOff_GAME!(P, B1(P, ballCarrier), 0) * .5 + .5)
            Case 3
                shotChance! = Int(plyrOff_GAME!(P, B1(P, ballCarrier), 0) * .6 + .5)
        End Select

    End If

    If Int(Rnd(1) * 150) + 1 = 40 Then
        '0.66% chance of TIEUP
        GoTo TIEUP
    Else

        X = 0
        Call Rolld100(RN0)

        If PZ% = 2 And RN0 <= 50 Then
            PZ% = 3
            playChoice = 1
        End If

        GoTo 520

    End If

    '----- NOODLE -----

    517 '
    Call ResetShotClock

    RN0 = Int(Rnd(1) * 25) + 1

    If RN0 = 13 Then

        GoTo TIEUP

    Else

        'Increase player and team TO's
        plyrOff_GAME!(P, B1(P, ballCarrier), 23) = plyrOff_GAME!(P, B1(P, ballCarrier), 23) + 1
        turnovers(P) = turnovers(P) + 1

        If SV% <> 1 Then
            PT% = 1
            Call MAKEPBP
        End If

        SV% = 0

        If pbpOpt = 1 And pbpDelay! <> 0 Then
            Call WINDEX
            Print "TURNOVER BY "; gameTeams$(P)
            Call DELAY
        End If
        If pbpOpt = 0 Then Call PBP

        pbpString$ = "BALL OUT OF BOUNDS TO " + gameTeams$(D)
        If pbpOpt = 1 And pbpDelay! <> 0 Then
            Print pbpString$
            Call DELAY
        End If
        If pbpOpt = 0 Then Call PBP

        Call DELAY
        Call UPDATESCREEN

        Locate 10, 1

        If gameClock! <= 0 Then 170

        prevBall$ = ""
        If P = 1 Then
            P = 0
            D = 1
            P9 = 0
            C1 = 2
        Else
            'P = 0
            P = 1
            D = 0
            P9 = 1
            C1 = 2
        End If

        Call DEADBALLOPTIONS

        For I = 0 To 1
            If defCategory(I) = 0 Then
                PR%(I, 0) = 1
                PR%(I, 1) = 7
                pctContrib(I) = 0
            End If
        Next

        GoTo 300

    End If

    '----- NOODLE -----

    GDSHOT:
    baseShotChance = plyrOff_GAME!(P, B1(P, ballCarrier), 0)

    If tmFatigue(P, B1(P, ballCarrier)) <= -5 Then
        baseShotChance = baseShotChance - 10
    End If

    shotChance! = baseShotChance + Int(Rnd(20)) + 11 + ft3PtFoul(D) + ft3PtFoul(P)
    ft3PtFoul(P) = 0
    ft3PtFoul(D) = 0
    PZ% = Int(Rnd(1) * 3) + 5
    X = 0

    Call Rolld100(RN0)

    If PZ% = 2 And RN0 <= 50 Then
        PZ% = 3
        playChoice = 1
    End If

    520 '
    If J8% = 1 Then PZ% = 4

    Call LASTSHOT
    If gameClock! <= 0 Then 170

    If PZ% = 4 And threePtShotChance <= 0 Then threePtShotChance = 3

    If pbpDelay! <> 0 Then

        If PZ% = 10 Then

            GDSHOTDES:
            If pbpDelay! <> 0 Then

                Call Rolld100(RN0)

                Select Case RN0

                    Case Is <= 25
                        pbpString$ = players$(P, B1(P, ballCarrier)) + " BREAKS IN TO FRONT COURT"

                    Case 25 To 50
                        pbpString$ = players$(P, B1(P, ballCarrier)) + " GETS THE PASS IN FRONT COURT"

                    Case 51 To 75
                        pbpString$ = players$(P, B1(P, ballCarrier)) + " BREAKS THE PRESS"

                    Case 76 To 100
                        pbpString$ = players$(P, B1(P, ballCarrier)) + " DEFEATS THE PRESS"

                End Select

                Call PBP

                Call Rolld100(RN1)

                Select Case RN1

                    Case Is <= 33
                        pbpString$ = "HE HAS AN OPEN JUMPER AT THE FT LINE"

                    Case 34 To 66
                        pbpString$ = "HE'S OPEN ON THE BASELINE"

                    Case 66 To 100
                        pbpString$ = "HE'S OPEN FROM THE WING"

                End Select

                Call SHOTSTRING
                Call PBP

            End If

        Else

            If shotChance! > 70 And fastBreak = 1 And PASS = 1 Then

                PT% = 6
                PZ% = 8
                Call MAKEPBP
                Call PBP

            Else

                If players$(P, B1(P, ballCarrier)) <> prevBall$ Then
                    If prevBall$ = "" Then
                        pbpString$ = "PASS TO " + players$(P, B1(P, ballCarrier))
                        Call PBP
                    Else
                        PT% = 10
                        Call MAKEPBP
                        Call PBP
                    End If
                Else
                    PT% = 9
                    Call MAKEPBP
                    Call PBP
                End If

                If shotClock <= 0 And sClockVal > 0 Then
                    pbpString$ = "SHOT CLOCK HAS EXPIRED!"
                    SV% = 1
                    Call LASTSHOT
                    GoTo 517
                End If

                prevBall$ = players$(P, B1(P, ballCarrier))
                YY$ = "HE"

                Select Case PZ%

                    Case 1:

                        If pbpDelay! <> 0 Then

                            RN0 = Int(Rnd(1) * 12) + 1

                            Select Case RN0

                                Case 1
                                    pbpString$ = YY$ + "'S HEAVILY GUARDED"
                                Case 2
                                    pbpString$ = YY$ + "'S GUARDED"
                                Case 3
                                    pbpString$ = YY$ + "'S GUARDED BY " + players$(D, B1(D, ballCarrier))
                                Case 4
                                    pbpString$ = YY$ + " HAS A MAN ON HIM"
                                Case 5
                                    pbpString$ = YY$ + " HAS A DEFENDER ON HIM"
                                Case 6
                                    pbpString$ = YY$ + "'S COVERED"
                                Case 7
                                    pbpString$ = YY$ + " CAN'T GET AROUND THE DEFENDER"
                                Case 8
                                    pbpString$ = YY$ + "'S PRESSURED"
                                Case 9
                                    pbpString$ = YY$ + " HAS NOWHERE TO GO"
                                Case 10
                                    pbpString$ = YY$ + " HAS A HAND IN HIS FACE"
                                Case 11
                                    pbpString$ = YY$ + "'S WATCHED BY " + players$(D, B1(D, ballCarrier))
                                Case 12
                                    pbpString$ = YY$ + "'S WORKING ON " + players$(D, B1(D, ballCarrier))
                            End Select

                        End If

                        Call PBP

                    Case 2:
                        pbpString$ = YY$ + " WORKS FREE INSIDE"
                        Call PBP
                        pbpString$ = "HE HAS AN OPEN SHOT"
                        Call SHOTSTRING
                        Call PBP

                        playChoice = 1

                    Case 3:

                        If pbpDelay! <> 0 Then

                            playChoice = 1

                            Call Rolld100(RN0)

                            If ballCarrier = 4 Then

                                Select Case RN0
                                    Case Is <= 25
                                        pbpString$ = YY$ + " MAKES A MOVE DOWN LOW"
                                        Call PBP

                                    Case 26 To 50
                                        pbpString$ = YY$ + " POSITIONS HIMSELF DOWN LOW"
                                        Call PBP

                                    Case 51 To 75
                                        pbpString$ = YY$ + " POSTS UP UNDERNEATH"
                                        Call PBP

                                    Case 76 To 100
                                        pbpString$ = YY$ + " BACKS IN AGAINST " + players$(D, B1(D, 4))

                                End Select

                                Call PBP

                                pbpString$ = "HE HAS A GOOD SHOT INSIDE"
                                Call SHOTSTRING
                                Call PBP

                            End If

                            If (ballCarrier = 3 Or ballCarrier = 2) Then

                                Select Case RN0

                                    Case Is <= 17
                                        pbpString$ = YY$ + " DRIVES THE BASELINE"

                                    Case 18 To 34
                                        pbpString$ = YY$ + " PENETRATES THE LANE"

                                    Case 35 To 50
                                        pbpString$ = YY$ + " MOVES INSIDE"

                                    Case 51 To 67
                                        pbpString$ = YY$ + " DRIVES THE LANE"

                                    Case 68 To 84
                                        pbpString$ = YY$ + " MOVES INTO THE PAINT"

                                    Case 85 To 100
                                        pbpString$ = YY$ + " DRIVES UNDERNEATH"

                                End Select

                                Call PBP

                                pbpString$ = "HE'S GOT A GOOD SHOT UNDERNEATH"
                                Call SHOTSTRING
                                Call PBP

                            End If

                            If (ballCarrier = 0 Or ballCarrier = 1) Then

                                Select Case RN0
                                    Case Is <= 25
                                        pbpString$ = YY$ + " DRIVES THE LANE":

                                    Case 26 To 50
                                        pbpString$ = YY$ + " DRIVES THROUGH THE KEY"
                                        Call PBP
                                        pbpString$ = "HE'S INTO THE LANE"

                                    Case 51 To 75
                                        pbpString$ = YY$ + " DRIVES TO THE BASKET"

                                    Case 76 To 100
                                        pbpString$ = YY$ + " DRIVES THROUGH AN OPENING"

                                End Select

                                Call PBP

                                pbpString$ = "HE HAS A GOOD SHOT CLOSE IN"
                                Call SHOTSTRING
                                Call PBP

                            End If

                        End If

                    Case 4:
                        pbpString$ = "HE'S OPEN FROM 3-POINT RANGE!"
                        pbpString$ = pbpString$ + Str$(threePtShotChance) + "*"
                        Call PBP

                        playChoice = 1

                    Case 5:
                        pbpString$ = "HE'S OPEN FROM MEDIUM RANGE"
                        Call SHOTSTRING
                        Call PBP

                        playChoice = 1

                    Case 6:
                        pbpString$ = "HE'S OPEN FROM MEDIUM RANGE"
                        Call SHOTSTRING
                        Call PBP

                        playChoice = 1

                    Case 7:
                        pbpString$ = "HE'S OPEN FROM MEDIUM RANGE"
                        Call SHOTSTRING
                        Call PBP

                        playChoice = 1

                End Select

            End If

        End If

    End If

    529 '
    If shotChance! <= 0 Then shotChance! = 1
    If shotChance! >= 100 Then shotChance! = 99

    skipShotPct% = 0

    If pbpDelay! > 0 Then

        If pbpOpt = 1 Then

            Call WINDEX
            Print "PASS TO "; players$(P, B1(P, ballCarrier)); " OF "; gameTeams$(P)

            Locate 10
            If PZ% = 4 Then
                Print "3 PT SHOT % "; threePtShotChance;: Print " *";
            Else
                Print "SHOT % "; shotChance!;
                If PZ% >= 2 And PZ% <= 8 Then Print "*";
            End If

            Call DELAY

        Else

            Locate 8, 41
            If PZ% = 4 Then
                Print Using "SHOT % ###"; threePtShotChance;: Print " *";
            Else
                Print Using "SHOT % ###   "; shotChance!;
                If PZ% >= 2 And PZ% <= 8 Then Locate , 52: Print "*";
            End If

        End If

    End If

    Print


    ' *** Determine value for playChoice ***
    'playChoice = play type
    '0 = Pass
    '1 = Shoot (2FGA)
    '2 = Timeout
    '3 = Foul
    '4 = Shoot (3FGA)

    'Computer Opponent or "Let Computer Shoot"
    scoreDiff = score(P, 0) - score(D, 0)
    If playerMode <> 0 Or coachOpt = 1 Then

        ' NOT (Overtime, 4 minutes left, Offense Ahead, Computer is Visitor / On Offense)
        If Not (currHalf >= 2 And gameClock! <= 240 And scoreDiff > 0 And playerMode = 1 And P = compTeam) Then

            'PC vs PC, Computer on Defense
            If playerMode = 2 Or playerMode = 1 And D = compTeam Then

                'Human Opponent | PC is Visitor AND Coach Shots
                If (playerMode = 1 Or playerMode = 0) And coachOpt <> 1 Then
                    GoTo 530
                End If

            End If

        End If

        If gameClock! <= 1 Or sClockVal >= 1 And shotClock <= 7 Then
            playChoice = 1
        Else
            If gameClock! <= 8 And scoreDiff <= -3 And currHalf >= 2 Then
                playChoice = 1
            Else
                If gameClock! <= 20 And scoreDiff <= -5 And currHalf >= 2 Then
                    playChoice = 1
                Else

                    baseShotChance = plyrOff_GAME!(P, B1(P, ballCarrier), 0)

                    If tmFatigue(P, B1(P, ballCarrier)) <= -5 Then baseShotChance = baseShotChance - 10

                    'Determine when to shoot
                    If playChoice <> 1 Then

                        'Human Opponent, Computer Shoots | Computer is Visitor AND on Defense, Computer Shoots | Computer is Visitor AND On Offense | Computer is Home Team
                        If (playerMode = 0 And coachOpt = 1 Or playerMode = 1 And D = compTeam And coachOpt = 1 Or playerMode = 1 And P = compTeam Or playerMode = 2) Then

                            If gameClock! <= 660 And gameClock! >= 540 And currHalf = 1 And timeouts(P) >= 5 Then
                                playChoice = 2
                                CTO% = 1
                            Else
                                If gameClock! <= 240 And gameClock! >= 120 And currHalf = 1 And timeouts(P) >= 4 Then
                                    playChoice = 2
                                    CTO% = 1
                                Else
                                    If gameClock! <= 840 And gameClock! >= 720 And currHalf = 2 And timeouts(P) >= 3 Then
                                        playChoice = 2
                                        CTO% = 1
                                    Else
                                        If gameClock! <= 300 And gameClock! >= 180 And currHalf = 2 And timeouts(P) >= 2 Then
                                            playChoice = 2
                                            CTO% = 1
                                        End If
                                    End If
                                End If

                            End If

                        End If

                    End If

                End If

            End If

        End If

    Else

        'Respond to manual input from user for shot options
        530 '
        I$ = GetKeyPress$

        Select Case UCase$(I$)

            Case "B"
                Call BOXOPTIONS(0)
                Call SCOREBOARD

            Case "D"
                Call ZONECHANGE
                Call SCOREBOARD
                GoTo 529

            Case "O"
                Call OPTIONS
                GoTo 529

            Case "T"
                Call TICKER
                Call SCOREBOARD
                GoTo 529

            Case "Q"
                Exit Sub

        End Select

        playChoice = Val(I$)

        If playChoice < 1 Or playChoice > 4 Then playChoice = 0
        If playChoice = 4 Then FT% = 1

    End If

    'Loop Until playerMode <> 0 Or coachOpt = 1 Or (UCase$(I$) <> "T" And UCase$(I$) <> "D" And UCase$(I$) <> "O")

    '----------------------------------------------------
    '--- THIS IS CRITCAL FOR PBP MOVING AUTOMATICALLY ---
    '----------------------------------------------------
    ' essentially it allows the user to press a key
    ' during game play to activate certain functions
    ' or special menus, etc.
    532 '
    I$ = InKey$

    Select Case UCase$(I$)

        Case "B"
            Call BOXOPTIONS(0)
            Call SCOREBOARD

        Case "D"
            Call ZONECHANGE
            Call SCOREBOARD
            GoTo 529

        Case "O"
            Call OPTIONS
            GoTo 529

        Case "T"
            Call TICKER
            Call SCOREBOARD
            GoTo 529

        Case "Q"
            Exit Sub

    End Select

    'Foul was chosen | Computer on offense and last 4 minutes of game
    If Val(I$) = 3 And (playerMode = 0 Or playerMode = 1 And P = compTeam) And currHalf >= 4 And gameClock! < 240 Then
        playChoice = 3
    End If

    'Timeout
    If Val(I$) = 2 And (playerMode = 0 Or playerMode = 1 And D = compTeam) And timeouts(P) > 0 Then
        playChoice = 2
    End If

    P3 = playChoice

    If playChoice = 2 And timeouts(P) = 0 Or playChoice = 3 And currHalf = 1 Or playChoice = 3 And playerMode = 1 And D = compTeam Or playChoice = 3 And currHalf = 2 And gameClock! > 240 Then
        GoTo 530
    End If

    ' *** Decide if 3FGA ***
    If J8% = 1 Then
        If (playerMode = 1 And P = compTeam) Or (playerMode = 2) Then
            playChoice = 4
        End If
    End If

    scoreDiff = score(D, 0) - score(P, 0)
    'Try for 3 near end of game when down by 3
    If currHalf >= 2 And gameClock! < 30 And scoreDiff = 3 And ((playerMode = 1 And P = compTeam) Or playerMode = 2) Then
        If threePtOpt = 1 Then playChoice = 4
    End If

    'Shoot and in 3 pt range
    If (playChoice = 1 Or playChoice = 4) And PZ% = 4 Then
        playChoice = 4
    End If

    ' *** Done determining value for playChoice ***
    playType = playChoice + 1

    Select Case playType

        Case 1
            GoTo 350

        Case 2
            537 '
            ' *** SHOOT !!! ***
            PT% = 3
            Call MAKEPBP
            Call PBP

            shotRoll = 0
            Call Rolld100(foulRoll)

            plyrOff_GAME!(P, B1(P, ballCarrier), 7) = plyrOff_GAME!(P, B1(P, ballCarrier), 7) + 1

            scoreDiff = score(D, 0) - score(P, 0)

            If Not (currHalf > 1 And gameClock! <= 30 And scoreDiff = 2 Or currHalf > 1 And gameClock! <= 30 And scoreDiff = 0 Or currHalf > 1 And gameClock! <= 30 And scoreDiff = -2) Then

                blockRat_TMP = plyrOff_GAME!(P, B1(P, ballCarrier), 16)
                fAdj_TMP = teamAdj(D, 7)

                If foulRoll <= plyrOff_GAME!(P, B1(P, ballCarrier), 16) + teamAdj(D, 7) Then

                    'FOUL!!!
                    plyrOff_GAME!(P, B1(P, ballCarrier), 7) = plyrOff_GAME!(P, B1(P, ballCarrier), 7) - 1

                    Call ResetShotClock
                    gameClock! = gameClock! - 4
                    timeElapsed! = 4
                    Call AdjustPlayTime
                    Call AdjustShotClock

                    LT = 0

                    Call UPDATESCREEN

                    Call FoulCalled(1)

                    '--- BEGINNING OF SUBROUTINE

                    If pbpOpt = 0 Then
                        pbpString$ = "2 FT: " + players$(P, B1(P, ballCarrier)) + Str$(plyrOff_GAME!(P, B1(P, ballCarrier), 1)) + " %"
                        Call PBP
                        Call DELAY
                    Else
                        Call WINDEX
                        Print gameTeams$(P)
                        Print "2 FT: "; players$(P, B1(P, ballCarrier)); "   "; plyrOff_GAME!(P, B1(P, ballCarrier), 1); " %"
                    End If

                    X = 50

                    Call DEADBALLOPTIONS

                    For I = 0 To 1
                        If defCategory(I) = 0 Then
                            PR%(I, 0) = 1
                            PR%(I, 1) = 7
                            pctContrib(I) = 0
                        End If
                    Next

                    X = 0

                    If pbpDelay! <> 0 Then

                        If pbpOpt = 0 Then
                            pbpString$ = "2 FT: " + players$(P, B1(P, ballCarrier)) + Str$(plyrOff_GAME!(P, B1(P, ballCarrier), 1)) + " %"
                            Call PBP
                        Else
                            Call WINDEX
                            Print gameTeams$(P)
                            Print "2 FT: "; players$(P, B1(P, ballCarrier)); "   "; plyrOff_GAME!(P, B1(P, ballCarrier), 1); " %"
                        End If

                    End If

                    'Two FT Attempts
                    For freethrow = 1 To 2:

                        Call Rolld100(shotRoll)

                        'FT Success Chance
                        baseShotChance = plyrOff_GAME!(P, B1(P, ballCarrier), 1)

                        If tmFatigue(P, B1(P, ballCarrier)) <= -5 Then
                            baseShotChance = Int(baseShotChance * .9 + .5)
                        End If

                        If shotRoll <= baseShotChance Then

                            If pbpOpt = 0 Then
                                Call THREEFT(freethrow)
                            Else
                                'pbpOpt = 1
                                Call MADEFT(freethrow)
                            End If

                        Else

                            If pbpOpt = 0 Then
                                Call BADFT(freethrow)
                            Else
                                'pbpOpt = 1
                                Call MISSFT(freethrow)

                            End If
                        End If

                    Next freethrow

                    'Increase FT ATT
                    plyrOff_GAME!(P, B1(P, ballCarrier), 9) = plyrOff_GAME!(P, B1(P, ballCarrier), 9) + 2

                    '--- END OF SUBROUTINE

                    LT = 0

                    If gameClock! <= 0 Then
                        timeElapsed! = gameClock!
                        Call AdjustPlayTime
                        GoTo 170
                    End If

                    If shotRoll > baseShotChance Then
                        'Missed shot
                        GoTo 650
                    Else
                        598 '
                        prevBall$ = ""
                        If P = 1 Then
                            P = 0
                            D = 1
                            P9 = 0
                            C1 = 2
                        Else
                            'P = 0
                            P = 1
                            D = 0
                            P9 = 1
                            C1 = 2
                        End If

                        pbpString$ = gameTeams$(P) + " BALL O.B. AFTER SCORE"
                        Call SELECTPBP

                        Locate 8, 41: Print Space$(12)

                        Call DELAY
                        Call STOPPAGE
                        ballCarrier = 6
                        X = 0
                        Call PRESSGUARD

                        GoTo 300

                    End If

                End If

            End If

            Call Rolld100(shotRoll)

            'So, foul chance is up to 1/10 of the shot chance
            foulChance! = shotChance! * ((DY / 100) + (.03 * threePtOpt))

            If Not (currHalf > 1 And gameClock! < 30 And scoreDiff = 2 Or currHalf > 1 And gameClock! < 30 And scoreDiff = 0 Or currHalf > 1 And gameClock! < 30 And scoreDiff = -2) Then

                If shotRoll <= foulChance! Then

                    'Foul + Stuff
                    Call ResetShotClock
                    gameClock! = gameClock! - 4
                    timeElapsed! = 4
                    Call AdjustPlayTime
                    Call AdjustShotClock

                    LT = 0

                    Call UPDATESCREEN
                    Call CreditAssists

                    If DK% = 1 Then

                        If pbpOpt = 0 Then
                            PT% = 2
                            Call MAKEPBP
                            pbpString$ = "STUFF BY " + players$(P, B1(P, ballCarrier))
                            Call PBP
                            DK% = 0
                        End If
                        If pbpOpt = 1 Then
                            Call WINDEX
                            Print "STUFF BY " + players$(P, B1(P, ballCarrier))
                            Call DELAY
                        End If

                        DK% = 0

                    Else

                        If PZ% = 8 Then
                            PT% = 7
                            Call MAKEPBP
                            PZ% = 0
                            Call PBP

                        Else

                            F3 = 1
                            If pbpOpt = 0 Then
                                pbpString$ = "BASKET GOOD BY " + players$(P, B1(P, ballCarrier))
                                Call PBP
                            Else
                                'pbpOpt = 1
                                If pbpDelay! <> 0 Then Call WINDEX: Print "BASKET GOOD BY " + players$(P, B1(P, ballCarrier)): Call DELAY
                            End If

                        End If

                    End If

                    617 '
                    pbpString$ = "... AND HE'S FOULED !!!"

                    If pbpOpt = 1 And pbpDelay! <> 0 Then
                        Print pbpString$
                        Call DELAY
                    End If
                    If pbpOpt = 0 Then Call PBP

                    Call DELAY

                    'Update score and FGA
                    F3 = 1

                    score(P, currHalf) = score(P, currHalf) + 2
                    score(P, 0) = score(P, 0) + 2
                    plyrOff_GAME!(P, B1(P, ballCarrier), 8) = plyrOff_GAME!(P, B1(P, ballCarrier), 8) + 1

                    Call UPDATESCREEN
                    Call FoulCalled(1)

                    619 '
                    If pbpOpt = 1 And pbpDelay! <> 0 Then
                        Call WINDEX
                        Print gameTeams$(P)
                        Print "1 FT: "; players$(P, B1(P, ballCarrier)); "   "; plyrOff_GAME!(P, B1(P, ballCarrier), 1); " %"
                    End If
                    If pbpOpt = 0 Then
                        pbpString$ = players$(P, B1(P, ballCarrier)) + " WILL SHOOT 1"
                        Call PBP
                    End If

                    X = 50
                    Call DEADBALLOPTIONS

                    For I = 0 To 1
                        If defCategory(I) = 0 Then
                            PR%(I, 0) = 1
                            PR%(I, 1) = 7
                            pctContrib(I) = 0
                        End If
                    Next

                    X = 0

                    Call Rolld100(shotRoll)

                    If pbpOpt = 1 And pbpDelay! <> 0 Then
                        Call WINDEX
                        Print gameTeams$(P)
                        Print "1 FT: "; players$(P, B1(P, ballCarrier)); "   "; plyrOff_GAME!(P, B1(P, ballCarrier), 1); " %"
                    End If

                    baseShotChance = plyrOff_GAME!(P, B1(P, ballCarrier), 1)
                    If tmFatigue(P, B1(P, ballCarrier)) <= -5 Then
                        baseShotChance = Int(baseShotChance * .9 + .5)
                    End If

                    If shotRoll <= baseShotChance And pbpOpt = 0 Then

                        'Increase FTM
                        plyrOff_GAME!(P, B1(P, ballCarrier), 10) = plyrOff_GAME!(P, B1(P, ballCarrier), 10) + 1

                        If pbpDelay! <> 0 Then
                            pbpString$ = "FREE THROW IS GOOD"
                            Call PBP
                        End If

                        score(P, currHalf) = score(P, currHalf) + 1
                        score(P, 0) = score(P, 0) + 1

                        Call UPDATESCREEN

                    End If

                    If shotRoll <= baseShotChance Then
                        If pbpOpt = 1 Then
                            Call MADEFT(1)
                        End If
                    Else
                        If pbpOpt = 0 And pbpDelay! <> 0 Then
                            pbpString$ = "FREE THROW IS MISSED"
                            Call PBP
                        Else
                            Call MISSFT(1)
                        End If
                    End If

                    'Increase FT ATT
                    plyrOff_GAME!(P, B1(P, ballCarrier), 9) = plyrOff_GAME!(P, B1(P, ballCarrier), 9) + 1

                    If gameClock! <= 0 Then
                        timeElapsed! = gameClock!
                        Call AdjustPlayTime
                        GoTo 170
                    End If

                    X = 99

                    If shotRoll > baseShotChance Then 670

                    prevBall$ = ""
                    If P = 1 Then
                        P = 0
                        D = 1
                        P9 = 0
                        C1 = 2
                    Else
                        'P = 0
                        P = 1
                        D = 0
                        P9 = 1
                        C1 = 2
                    End If

                    pbpString$ = gameTeams$(P) + " BALL O.B. AFTER SCORE"
                    Call SELECTPBP

                    Locate 8, 41: Print Space$(12)

                    Call DELAY
                    Call STOPPAGE

                    ballCarrier = 6
                    X = 0
                    Call PRESSGUARD

                    GoTo 300

                End If

            End If

            If shotRoll <= shotChance! Then

                LT = 0

                If DK% = 1 And pbpOpt = 0 Then
                    PT% = 2
                    Call MAKEPBP
                    pbpString$ = "STUFF BY "
                    DK% = 0
                Else
                    If DK% = 1 And pbpOpt = 1 Then
                        pbpString$ = "STUFF BY "
                        DK% = 0
                    Else

                        If pbpOpt = 0 And PZ% = 8 Then
                            PT% = 7
                            Call MAKEPBP
                            Call PBP
                            PZ% = 0

                        Else
                            pbpString$ = "BASKET BY "

                            pbpString$ = pbpString$ + players$(P, B1(P, ballCarrier)) + " !!!"

                            If pbpOpt = 1 And pbpDelay! <> 0 Then
                                Call WINDEX
                                Print pbpString$
                                Call DELAY
                            Else

                                Call Rolld100(RN0)

                                If RN0 > 50 Then

                                    RN1 = Int(Rnd(1) * 22) + 1

                                    Select Case RN1
                                        Case 1
                                            pbpString$ = "NOTHING BUT THE BOTTOM OF THE NET!"
                                        Case 2
                                            pbpString$ = "COUNT IT!"
                                        Case 3
                                            pbpString$ = players$(P, B1(P, ballCarrier)) + " TICKLES THE TWINE!"
                                        Case 4
                                            pbpString$ = "NO RIM, NO BOARD, JUST CORD!"
                                        Case 5
                                            pbpString$ = "GOOD!"
                                        Case 6
                                            pbpString$ = "YES!"
                                        Case 7
                                            pbpString$ = "COUNT IT!"
                                        Case 8
                                            pbpString$ = "IT'S GOOD!"
                                        Case 9
                                            pbpString$ = "BINGO!"
                                        Case 10
                                            pbpString$ = "IT FALLS!"
                                        Case 11
                                            pbpString$ = "HE DRAINS IT!"
                                        Case 12
                                            pbpString$ = "SWISH!"
                                        Case 13
                                            pbpString$ = "ALL NET!"
                                        Case 14
                                            pbpString$ = "IT'S THERE!"
                                        Case 15
                                            pbpString$ = "HE BURIES IT!"
                                        Case 16
                                            pbpString$ = "IT GOES IN!"
                                        Case 17
                                            pbpString$ = "RING IT UP!"
                                        Case 18
                                            pbpString$ = "IT GOES DOWN!"
                                        Case 19
                                            pbpString$ = "BULLSEYE!"
                                        Case 20
                                            pbpString$ = "HE SCORES!"
                                        Case 21
                                            pbpString$ = "HE HAS IT!"
                                        Case 22
                                            pbpString$ = "HE CONNECTS!"
                                    End Select

                                End If

                                If pbpOpt = 0 Then Call PBP

                            End If

                        End If

                    End If

                End If

                560 '
                score(P, currHalf) = score(P, currHalf) + 2
                score(P, 0) = score(P, 0) + 2

                Call CreditAssists

                Call UPDATESCREEN

                plyrOff_GAME!(P, B1(P, ballCarrier), 8) = plyrOff_GAME!(P, B1(P, ballCarrier), 8) + 1

                562 '
                pbpString$ = gameTeams$(D) + " BALL O.B. AFTER SCORE"

                If pbpOpt = 1 And pbpDelay! <> 0 Then
                    Locate 9, 1: Print pbpString$
                End If
                If pbpOpt = 0 Then
                    Locate 8, 41: Print Space$(12)
                    Call PBP
                End If

                Call DELAY
                Call ResetShotClock
                gameClock! = gameClock! - 4
                timeElapsed! = 4
                Call AdjustPlayTime
                Call AdjustShotClock

                If gameClock! <= 0 Then
                    timeElapsed! = gameClock!
                    Call AdjustPlayTime
                    GoTo 170
                End If

                Call UPDATESCREEN

                prevBall$ = ""
                If P = 1 Then
                    P = 0
                    D = 1
                    P9 = 0
                    C1 = 2
                Else
                    'P = 0
                    P = 1
                    D = 0
                    P9 = 1
                    C1 = 2
                End If

                Call STOPPAGE
                Call PRESSGUARD

                GoTo 300

            Else

                Call Rolld100(RN0)

                LT = 0

                I2 = Int(Rnd(1) * 5) + 1
                rndPlayer = Int(Rnd(1) * 5)

                If I2 > plyrOff_GAME!(D, B1(D, rndPlayer), 6) Then

                    If pbpOpt = 1 And pbpDelay! <> 0 Then
                        Call WINDEX
                        Print "SHOT MISSED BY "; players$(P, B1(P, ballCarrier))
                        Call DELAY
                    End If
                    If pbpOpt = 0 Then
                        PT% = 8
                        Call MAKEPBP
                        Call PBP
                    End If

                    Call ResetShotClock
                    gameClock! = gameClock! - 4
                    timeElapsed! = 4
                    Call AdjustPlayTime
                    Call AdjustShotClock

                    If gameClock! <= 0 Then
                        timeElapsed! = gameClock!
                        Call AdjustPlayTime
                        GoTo 170
                    End If

                    Call UPDATESCREEN

                    LT = 0

                    GoTo 670

                End If

                If pbpDelay! <> 0 Then

                    If pbpOpt = 1 Then
                        Call WINDEX
                        Print "SHOT BLOCKED BY "; players$(D, B1(D, rndPlayer))
                        Call DELAY
                    Else

                        Call Rolld100(RN0)

                        Select Case RN0
                            Case 1 To 10
                                pbpString$ = players$(D, B1(D, rndPlayer)) + " STUFFS THE SHOT!!"
                            Case 11 To 15
                                pbpString$ = "DENIED BY " + players$(D, B1(D, rndPlayer)) + "!!"
                            Case 16 To 20
                                pbpString$ = "ERASED BY " + players$(D, B1(D, rndPlayer)) + "!!"
                            Case 21 To 25
                                pbpString$ = "EXPUNGED BY " + players$(D, B1(D, rndPlayer)) + "!!"
                            Case 26 To 30
                                pbpString$ = "REBUKED BY " + players$(D, B1(D, rndPlayer)) + "!!"
                            Case 31 To 35
                                pbpString$ = "SHOT BLOCKED CLEANLY BY " + players$(D, B1(D, rndPlayer)) + "!!"
                            Case 36 To 70
                                pbpString$ = "SHOT BLOCKED BY " + players$(D, B1(D, rndPlayer)) + "!!"
                            Case 71 To 90
                                pbpString$ = "SHOT REJECTED BY " + players$(D, B1(D, rndPlayer)) + "!!"
                            Case 91 To 100
                                pbpString$ = "SHOT SWATTED OUT BY " + players$(D, B1(D, rndPlayer)) + "!!"
                        End Select

                        Call PBP

                    End If

                End If

                plyrOff_GAME!(D, B1(D, rndPlayer), 15) = plyrOff_GAME!(D, B1(D, rndPlayer), 15) + 1
                X = 98

                Call ResetShotClock
                gameClock! = gameClock! - 4
                timeElapsed! = 4
                Call AdjustPlayTime
                Call AdjustShotClock

                If gameClock! <= 0 Then
                    timeElapsed! = gameClock!
                    Call AdjustPlayTime
                    TP% = 0
                    GoTo 170
                End If

                Call UPDATESCREEN

                GoTo 670

            End If

            '--- done with "routines" for taking a shot

        Case 3
            GoTo 640

        Case 4
            IN = 99
            Call LASTSHOT

            If gameClock! <= 0 Then 170

            C1 = 2
            P9 = D
            pbpString$ = "DESIGNATED FOULER (0-4)"

            Call SELECTPBP

            If playerMode = 1 And D = compTeam Or playerMode = 2 Then
                fouler = Int(Rnd(1) * 5)
            Else
                Do
                    I$ = GetKeyPress$
                    fouler = Val(I$)
                Loop Until fouler >= 0 And fouler <= 4
            End If

            Call FoulCalled(0)

            GoTo 9065

        Case 5

            '3FG Attempt
            J8% = 0

            If threePtOpt = 0 Then

                P3 = 1
                playChoice = 1
                GoTo 537

            Else

                Call Rolld100(shotRoll)

                threeFG(P, B1(P, ballCarrier), 1) = threeFG(P, B1(P, ballCarrier), 1) + 1
                plyrOff_GAME!(P, B1(P, ballCarrier), 7) = plyrOff_GAME!(P, B1(P, ballCarrier), 7) + 1
                PT% = 3

                Call MAKEPBP
                Call PBP

                Call Rolld100(foulChance)

                'Essentialy a 2% chance of being fouled on 3-pointer
                If foulChance <= 2 And ft3PtFoul = 1 Then

                    If shotRoll > threePtShotChance Then

                        'FOUL!

                        'Remove the attempt from the record
                        plyrOff_GAME!(P, B1(P, ballCarrier), 7) = plyrOff_GAME!(P, B1(P, ballCarrier), 7) - 1
                        threeFG(P, B1(P, ballCarrier), 1) = threeFG(P, B1(P, ballCarrier), 1) - 1

                        Call ResetShotClock
                        gameClock! = gameClock! - 4
                        timeElapsed! = 4
                        Call AdjustPlayTime
                        Call AdjustShotClock

                        Call UPDATESCREEN

                        Call FoulCalled(1)

                        If pbpDelay! <> 0 Then
                            If pbpOpt = 0 Then
                                pbpString$ = "3 FT: " + players$(P, B1(P, ballCarrier)) + Str$(plyrOff_GAME!(P, B1(P, ballCarrier), 1)) + " %"
                                Call PBP
                                Call DELAY
                            Else
                                Call WINDEX
                                Print gameTeams$(P)
                                Print "3 FT: "; players$(P, B1(P, ballCarrier)); "   "; plyrOff_GAME!(P, B1(P, ballCarrier), 1); " %"
                            End If
                        End If

                        X = 50
                        Call DEADBALLOPTIONS

                        For I = 0 To 1
                            If defCategory(I) = 0 Then
                                PR%(I, 0) = 1
                                PR%(I, 1) = 7
                                pctContrib(I) = 0
                            End If
                        Next

                        X = 0

                        If pbpDelay! <> 0 Then
                            If pbpOpt = 0 Then
                                pbpString$ = "3 FT: " + players$(P, B1(P, ballCarrier)) + Str$(plyrOff_GAME!(P, B1(P, ballCarrier), 1)) + " %"
                                Call PBP
                            Else
                                Call WINDEX
                                Print gameTeams$(P)
                                Print "3 FT: "; players$(P, B1(P, ballCarrier)); "   "; plyrOff_GAME!(P, B1(P, ballCarrier), 1); " %"
                            End If

                        End If

                        'Increase FT ATT
                        plyrOff_GAME!(P, B1(P, ballCarrier), 9) = plyrOff_GAME!(P, B1(P, ballCarrier), 9) + 3

                        '3 FT Attempts
                        For freethrow = 1 To 3:

                            Call Rolld100(shotRoll)

                            'FT Success Chance
                            baseShotChance = plyrOff_GAME!(P, B1(P, ballCarrier), 1)
                            If tmFatigue(P, B1(P, ballCarrier)) <= -5 Then baseShotChance = Int(baseShotChance * .9 + .5)

                            If shotRoll <= baseShotChance Then
                                If pbpOpt = 0 Then
                                    Call THREEFT(freethrow)
                                Else
                                    'pbpOpt = 1
                                    Call MADEFT(freethrow)
                                End If
                            Else
                                'shotRoll > baseShotChance
                                If pbpOpt = 0 Then
                                    Call BADFT(freethrow)
                                Else
                                    Call MISSFT(freethrow)
                                End If
                            End If

                        Next freethrow

                        LT = 0

                        If gameClock! <= 0 Then
                            timeElapsed! = gameClock!
                            Call AdjustPlayTime
                            GoTo 170
                        End If

                        If shotRoll > baseShotChance Then
                            'Missed shot
                            GoTo 650
                        Else
                            GoTo 598
                        End If

                    Else

                        'shotRoll <= threePtShotChance Then

                        gameClock! = gameClock! - 4
                        timeElapsed! = 4
                        Call AdjustPlayTime
                        Call AdjustShotClock
                        Call UPDATESCREEN
                        Call CreditAssists
                        pbpString$ = "3 PT BASKET GOOD BY " + players$(P, B1(P, ballCarrier))
                        Call SELECTPBP

                        Call CreditAssists

                        pbpString$ = "AND HE WAS FOULED !!!"
                        If pbpOpt = 1 And pbpDelay! <> 0 Then
                            Print pbpString$
                            Call DELAY
                        End If
                        If pbpOpt = 0 Then
                            Call PBP
                        End If

                        F3 = 1

                        score(P, currHalf) = score(P, currHalf) + 3
                        score(P, 0) = score(P, 0) + 3

                        plyrOff_GAME!(P, B1(P, ballCarrier), 8) = plyrOff_GAME!(P, B1(P, ballCarrier), 8) + 1
                        threeFG(P, B1(P, ballCarrier), 0) = threeFG(P, B1(P, ballCarrier), 0) + 1

                        Call UPDATESCREEN
                        Call FoulCalled(1)

                        GoTo 619

                    End If

                Else

                    'No foul - normal 3-pointer
                    If shotRoll > threePtShotChance Then

                        If pbpOpt = 1 And pbpDelay! <> 0 Then
                            Call WINDEX
                            Print "3 PT FG MISSED BY "; players$(P, B1(P, ballCarrier))
                            Call DELAY
                        End If

                        If pbpOpt = 0 Then
                            PT% = 5
                            TP% = 1
                            Call MAKEPBP
                            Call PBP
                        End If

                        Call ResetShotClock
                        gameClock! = gameClock! - 4
                        timeElapsed! = 4
                        Call AdjustPlayTime
                        Call AdjustShotClock

                        If gameClock! <= 0 Then
                            timeElapsed! = gameClock!
                            Call AdjustPlayTime
                            TP% = 0
                            GoTo 170
                        End If

                        Call UPDATESCREEN

                        GoTo 670

                    Else

                        If pbpOpt = 1 And pbpDelay! <> 0 Then
                            Call WINDEX
                            Print "3 PT FG GOOD BY "; players$(P, B1(P, ballCarrier)); " !!!"
                            Call DELAY
                        End If

                        If pbpOpt = 0 Then
                            PT% = 4
                            Call MAKEPBP
                            Call PBP
                        End If

                        369 '
                        If XX% = 1 Then
                            XX% = 0
                            score(P, currHalf) = score(P, currHalf) + 2
                            score(P, 0) = score(P, 0) + 2
                            threeFG(P, B1(P, ballCarrier), 1) = threeFG(P, B1(P, ballCarrier), 1) - 1
                        Else
                            score(P, currHalf) = score(P, currHalf) + 3
                            score(P, 0) = score(P, 0) + 3
                            threeFG(P, B1(P, ballCarrier), 0) = threeFG(P, B1(P, ballCarrier), 0) + 1
                        End If

                        plyrOff_GAME!(P, B1(P, ballCarrier), 8) = plyrOff_GAME!(P, B1(P, ballCarrier), 8) + 1
                        Call CreditAssists
                        Call UPDATESCREEN

                        GoTo 562

                    End If

                End If

            End If

    End Select

    '----- NOODLE -----

    ' BONUS!!!
    ' 1 + 1 Free Throw

    '--- BEGINNING OF SUBROUTINE

    570 '
    If pbpDelay! <> 0 Then
        If pbpOpt = 0 Then
            pbpString$ = "1+1 FT: " + players$(P, B1(P, ballCarrier)) + Str$(plyrOff_GAME!(P, B1(P, ballCarrier), 1)) + " %"
            Call PBP
        Else
            Call WINDEX
            Print gameTeams$(P)
            Print "1+1 FT: "; players$(P, B1(P, ballCarrier)); " "; plyrOff_GAME!(P, B1(P, ballCarrier), 1); " %"
        End If

    End If

    X = 50
    Call DEADBALLOPTIONS

    For I = 0 To 1
        If defCategory(I) = 0 Then
            PR%(I, 0) = 1
            PR%(I, 1) = 7
            pctContrib(I) = 0
        End If
    Next

    X = 0

    If pbpDelay! <> 0 Then

        If pbpOpt = 0 Then
            pbpString$ = "1+1 FT: " + players$(P, B1(P, ballCarrier)) + Str$(plyrOff_GAME!(P, B1(P, ballCarrier), 1)) + " %"
            Call PBP
        Else
            Call WINDEX
            Print gameTeams$(P)
            Print "1+1 FT: "; players$(P, B1(P, ballCarrier)); "   "; plyrOff_GAME!(P, B1(P, ballCarrier), 1); " %"
        End If

    End If

    '2 FT Attempts
    For freethrow = 1 To 2

        'Increase FT ATT
        plyrOff_GAME!(P, B1(P, ballCarrier), 9) = plyrOff_GAME!(P, B1(P, ballCarrier), 9) + 1

        Call Rolld100(shotRoll)

        'FT Success Chance
        baseShotChance = plyrOff_GAME!(P, B1(P, ballCarrier), 1)
        If tmFatigue(P, B1(P, ballCarrier)) <= -5 Then baseShotChance = Int(baseShotChance * .9 + .5)

        If shotRoll <= baseShotChance Then

            If pbpOpt = 0 Then
                Call THREEFT(freethrow)
            Else
                'pbpOpt = 1 Then
                Call MADEFT(freethrow)
            End If
        Else
            'shotRoll > baseShotChance And
            If pbpOpt = 0 Then
                Call BADFT(freethrow)
            Else
                'pbpOpt = 1
                Call MISSFT(freethrow)
            End If

        End If

        If freethrow = 1 And shotRoll > plyrOff_GAME!(P, B1(P, ballCarrier), 1) Then
            Exit For
        End If

    Next freethrow

    '--- END OF SUBROUTINE

    '575
    If gameClock! <= 0 Then 170

    If shotRoll > baseShotChance Then
        'Missed Shot
        GoTo 650
    Else
        prevBall$ = ""
        If P = 1 Then
            P = 0
            D = 1
            P9 = 0
            C1 = 2
        Else
            'P = 0
            P = 1
            D = 0
            P9 = 1
            C1 = 2
        End If

        X = 0
        pbpString$ = gameTeams$(P) + " BALL O.B. AFTER SCORE"
        Call SELECTPBP

        Locate 8, 41: Print Space$(12)
        Call DELAY
        Call STOPPAGE
        Call PRESSGUARD

        GoTo 300

    End If

    '----- NOODLE -----

    580 '
    Call LASTSHOT

    Call ResetShotClock

    prevBall$ = ""
    If P = 1 Then
        P = 0
        D = 1
        P9 = 0
        C1 = 2
    Else
        'P = 0
        P = 1
        D = 0
        P9 = 1
        C1 = 2
    End If

    Call FoulCalled(1)

    ballCarrier = fouler

    'Check if in BONUS
    If teamFouls(D) > 6 Then

        GoTo 570

    Else

        If gameClock! <= 0 Then 170
        pbpString$ = "OFFENSIVE FOUL"
        Call SELECTPBP

        pbpString$ = gameTeams$(P) + " BALL O.B. AFTER FOUL"
        If pbpOpt = 1 And pbpDelay! <> 0 Then
            Print pbpString$
            Call DELAY
        End If
        If pbpOpt = 0 Then
            Call PBP
        End If

        Locate 8, 41: Print Space$(12)

        ballCarrier = 6
        Call DELAY

        turnovers(D) = turnovers(D) + 1

        Call UPDATESCREEN
        Call DEADBALLOPTIONS

        For I = 0 To 1
            If defCategory(I) = 0 Then
                PR%(I, 0) = 1
                PR%(I, 1) = 7
                pctContrib(I) = 0
            End If
        Next

        Call PRESSGUARD

        GoTo 300

    End If

    '----- NOODLE -----

    640 '
    prevBall$ = ""

    If timeouts(P) > 0 Then

        timeouts(P) = timeouts(P) - 1
        Call SCOREBOARD

        '--- RESUME ANALYSIS

        'Offense
        Call Rolld100(RN0)
        If RN0 <= 8 Then
            ft3PtFoul(P) = 10
        End If
        If RN0 > 8 And RN0 <= 16 Then
            ft3PtFoul(P) = 5
        End If

        'Defense
        Call Rolld100(RN1)
        If RN1 <= 8 Then
            ft3PtFoul(D) = -10
        End If
        If RN1 > 8 And RN1 <= 16 Then
            ft3PtFoul(D) = -5
        End If

        gameClock! = gameClock! + M5
        timeElapsed! = -M5
        Call AdjustPlayTime

        If sClockVal >= 1 Then
            shotClock = shotClock + M5
        End If

        Erase PR%

        pbpString$ = "TIME OUT " + gameTeams$(P)
        Call SELECTPBP
        Call DELAY
        Call DEADBALLOPTIONS

        For I = 0 To 1
            For I1 = 0 To 13
                tmFatigue(I, I1) = tmFatigue(I, I1) + 1
                If tmFatigue(I, I1) > 0 Then tmFatigue(I, I1) = 0
            Next
        Next

    End If

    If A6 = 0 Then Call PRESSGUARD: GoTo 300

    GoTo 350

    '----- NOODLE -----

    ' ***** CHECK FOR REBOUNDS *****

    650 '
    A6 = 1
    playChoice = 0
    A4 = 0
    D6 = 0
    shotRoll = 0

    Call ResetShotClock

    Call Rolld100(RN0)

    scoreDiff = score(P, 0) - score(D, 0)
    If (currHalf > 1 And gameClock! < 30 And score(D, 0) - score(P, 0) = 2 And RN0 <= 12) Or (currHalf > 1 And gameClock! < 30 And scoreDiff = 0 And RN0 <= 12) Then
        Call OFFREBOUND
        GoTo 760
    End If

    Call Rolld100(RN0)

    Select Case RN0

        Case 1 To 11
            foulOnReb = 1
            GoTo 707

        Case 12 To 19
            Call OFFREBOUND
            GoTo 760

        Case 20 To 100
            Call DEFREBOUND
            GoTo 800

    End Select


    '----- NOODLE -----

    670 '
    A6 = 1
    playChoice = 0
    A4 = 0
    D6 = 0

    Call ResetShotClock

    '--- loop
    700 '
    shotRoll = 0
    A4 = 0
    D6 = 0

    Call Rolld100(RN0)

    scoreDiff = score(D, 0) - score(P, 0)
    If currHalf > 1 And gameClock! < 30 And (scoreDiff = 2 Or scoreDiff = 0 Or scoreDiff = -2) And RN0 <= 12 Then
        GoTo 700
    End If
    '--- loop

    If TP% = 1 And RN0 <= 19 Then
        TP% = 0
    End If

    Select Case RN0

        Case 0 To 1

            725 '
            If X = 98 Then 700
            turnovers(P) = turnovers(P) + 1
            pbpString$ = "OFFENSIVE GOAL TENDING"
            Call SELECTPBP

            pbpString$ = "BALL O.B. TO " + gameTeams$(D)
            If pbpOpt = 1 And pbpDelay! <> 0 Then
                Print pbpString$
                Call DELAY
            End If
            If pbpOpt = 0 Then
                Call PBP
            End If

            Call DELAY

            prevBall$ = ""
            If P = 1 Then
                P = 0
                D = 1
                P9 = 0
                C1 = 2
            Else
                'P = 0
                P = 1
                D = 0
                P9 = 1
                C1 = 2
            End If

            Call DEADBALLOPTIONS

            For I = 0 To 1
                If defCategory(I) = 0 Then
                    PR%(I, 0) = 1
                    PR%(I, 1) = 7
                    pctContrib(I) = 0
                End If
            Next

            GoTo 300

        Case 2 To DY
            foulOnReb = 1

        Case DY + 1 To 17

            Call Rolld100(RN0)

            If RN0 > 35 Then

                totRebounds(D) = totRebounds(D) + 1
                Call UPDATESCREEN
                pbpString$ = "RB KNOCKED O.B. TO " + gameTeams$(D)
                Call SELECTPBP
                Call DELAY
                Call DEADBALLOPTIONS

                For I = 0 To 1
                    If defCategory(I) = 0 Then
                        PR%(I, 0) = 1
                        PR%(I, 1) = 7
                        pctContrib(I) = 0
                    End If
                Next

                prevBall$ = ""
                If P = 1 Then
                    P = 0
                    D = 1
                    P9 = 0
                    C1 = 2
                Else
                    'P = 0
                    P = 1
                    D = 0
                    P9 = 1
                    C1 = 2
                End If

                C1 = 2
                P9 = P

                GoTo 300

            Else

                752 '
                totRebounds(P) = totRebounds(P) + 1
                Call UPDATESCREEN
                pbpString$ = "RB KNOCKED O.B. TO " + gameTeams$(P)
                Call SELECTPBP
                Call DELAY
                Call DEADBALLOPTIONS

                For I = 0 To 1
                    If defCategory(I) = 0 Then
                        PR%(I, 0) = 1
                        PR%(I, 1) = 7
                        pctContrib(I) = 0
                    End If
                Next

                LT = 4

                GoTo 520

            End If

        Case 18 To 19

            If X = 99 Then GoTo 700

            Call OFFREBOUND

            plyrOff_GAME!(P, B1(P, ballCarrier), 7) = plyrOff_GAME!(P, B1(P, ballCarrier), 7) + 1
            plyrOff_GAME!(P, B1(P, ballCarrier), 8) = plyrOff_GAME!(P, B1(P, ballCarrier), 8) + 1
            plyrOff_GAME!(P, B1(P, ballCarrier), 11) = plyrOff_GAME!(P, B1(P, ballCarrier), 11) + 1
            totRebounds(P) = totRebounds(P) + 1
            score(P, currHalf) = score(P, currHalf) + 2
            score(P, 0) = score(P, 0) + 2
            Call UPDATESCREEN

            If pbpOpt = 1 And pbpDelay! <> 0 Then
                Call WINDEX
                Print "TIPPED IN BY "; players$(P, B1(P, ballCarrier))
                Print "   BASKET GOOD !!!"
                Call DELAY
            End If
            If pbpOpt = 0 Then
                pbpString$ = players$(P, B1(P, ballCarrier)) + " TIPS IT IN!!"
                Call PBP
            End If

            prevBall$ = ""
            If P = 1 Then
                P = 0
                D = 1
                P9 = 0
                C1 = 2
            Else
                'P = 0
                P = 1
                D = 0
                P9 = 1
                C1 = 2
            End If

            pbpString$ = gameTeams$(P) + " BALL O.B. AFTER SCORE"
            Locate 8, 41: Print Space$(12)

            If pbpOpt = 1 And pbpDelay! <> 0 Then
                Print pbpString$
                Call DELAY
            End If
            If pbpOpt = 0 Then
                Call PBP
            End If

            Call DELAY
            Call STOPPAGE
            Call PRESSGUARD

            GoTo 300

        Case 20
            If X = 99 Then 700

            pbpString$ = "DEFENSIVE GOAL TENDING"
            Call SELECTPBP

            If TP% = 1 Then

                TP% = 0

                If XX% = 1 Then
                    XX% = 0
                    score(P, currHalf) = score(P, currHalf) + 2
                    score(P, 0) = score(P, 0) + 2
                    threeFG(P, B1(P, ballCarrier), 1) = threeFG(P, B1(P, ballCarrier), 1) - 1
                Else
                    score(P, currHalf) = score(P, currHalf) + 3
                    score(P, 0) = score(P, 0) + 3
                    threeFG(P, B1(P, ballCarrier), 0) = threeFG(P, B1(P, ballCarrier), 0) + 1
                End If

                plyrOff_GAME!(P, B1(P, ballCarrier), 8) = plyrOff_GAME!(P, B1(P, ballCarrier), 8) + 1
                Call CreditAssists
                Call UPDATESCREEN

                GoTo 562

            Else

                GoTo 560

            End If

    End Select

    707 '
    TP% = 0
    RB = 0

    For I = 0 To 9:
        RB(I) = 0
    Next

    For I = 0 To 4
        XZ = plyrOff_GAME!(P, B1(P, I), 21)

        If Not (currHalf > 2 Or currHalf = 2 And gameClock! <= 120) Then
            If plyrOff_GAME!(P, B1(P, I), 3) - plyrOff_GAME!(P, B1(P, I), 7) - plyrOff_GAME!(P, B1(P, I), 11) - plyrOff_GAME!(P, B1(P, I), 12) <= 0 Then
                XZ = Int(XZ * .5 + .5)
            End If
        End If

        If tmFatigue(P, B1(P, I)) <= -5 Then
            XZ = Int(XZ * .9 + .5)
        End If

        RB(I) = RB(I) + XZ
        RB = RB + XZ
    Next

    ORB = RB

    For I = 0 To 4

        XZ = plyrOff_GAME!(1 - P, B1(1 - P, I), 2)

        If Not (currHalf > 2 Or currHalf = 2 And gameClock! <= 120) Then
            If plyrOff_GAME!(1 - P, B1(1 - P, I), 3) - plyrOff_GAME!(1 - P, B1(1 - P, I), 7) - plyrOff_GAME!(1 - P, B1(1 - P, I), 11) - plyrOff_GAME!(1 - P, B1(1 - P, I), 12) <= 0 Then
                XZ = Int(XZ * .5 + .5)
            End If
        End If

        If tmFatigue(1 - P, B1(1 - P, I)) <= -5 Then
            XZ = Int(XZ * .9 + .5)
        End If

        RB(I + 5) = RB(I + 5) + XZ
        RB = RB + XZ

    Next

    DRB = RB - ORB

    If defCoverage(D) = 0 Then XQ = Int((RB * .06) + .5)
    If defCoverage(D) = 1 Then XQ = Int((RB * .12) + .5)
    If defCoverage(D) = 2 Then XQ = Int((RB * .1) + .5)
    If defCoverage(D) = 3 Then XQ = Int((RB * .14) + .5)
    If CZ%(P) = 1 Then XQ = Int((RB * .17) + .5)

    rndReb = Rnd(1) * RB
    If rndReb <= ORB + XQ Then

        rndOReb = Rnd(1) * ORB
        For ballCarrier = 0 To 4
            If rndOReb <= RB(ballCarrier) Then GoTo 760
            RB(ballCarrier + 1) = RB(ballCarrier + 1) + RB(ballCarrier)
        Next

        ballCarrier = Int(Rnd(1) * 5)

        GoTo 760

    End If

    rndDReb = Rnd(1) * DRB
    skipRandom% = 0

    For ballCarrier = 5 To 9

        If rndDReb <= RB(ballCarrier) Then
            ballCarrier = ballCarrier - 5
            skipRandom% = 1
            Exit For
        Else
            RB(ballCarrier + 1) = RB(ballCarrier + 1) + RB(ballCarrier)
        End If

    Next

    If skipRandom% = 0 Then
        ballCarrier = Int(Rnd(1) * 5)
    End If

    800 '
    If foulOnReb = 1 Then

        foulOnReb = 0

        If pbpDelay! <> 0 Then
            If pbpOpt = 1 Then
                Call WINDEX
                Print players$(D, B1(D, ballCarrier)); " HAS THE DEF RB FOR"
                Print gameTeams$(D); " AND IS FOULED !!!"
                Call DELAY
            Else
                pbpString$ = players$(D, B1(D, ballCarrier)) + " HAS THE DEF RB"
                Call PBP
                pbpString$ = "HE'S FOULED !!!"
                Call PBP
            End If
        End If

        totRebounds(D) = totRebounds(D) + 1
        plyrOff_GAME!(D, B1(D, ballCarrier), 11) = plyrOff_GAME!(D, B1(D, ballCarrier), 11) + 1

        Call UPDATESCREEN

        prevBall$ = ""
        If P = 1 Then
            P = 0
            D = 1
            P9 = 0
            C1 = 2
        Else
            'P = 0
            P = 1
            D = 0
            P9 = 1
            C1 = 2
        End If

        Call FoulCalled(1)

        LT = 4

        GoTo 9065

    End If

    plyrOff_GAME!(D, B1(D, ballCarrier), 11) = plyrOff_GAME!(D, B1(D, ballCarrier), 11) + 1
    totRebounds(D) = totRebounds(D) + 1
    Call UPDATESCREEN
    pbpString$ = players$(D, B1(D, ballCarrier)) + " HAS THE DEF RB"

    If pbpDelay! <> 0 Then

        If pbpOpt = 0 Then
            Call PBP
        End If

        If pbpOpt = 1 Then
            Call WINDEX
            Print pbpString$
            Print "FOR " + gameTeams$(D)
            Call DELAY
        End If

    End If

    prevBall$ = players$(D, B1(D, ballCarrier))
    C1 = 2
    P9 = D

    prevBall$ = ""
    If P = 1 Then
        P = 0
        D = 1
        P9 = 0
        C1 = 2
    Else
        'P = 0
        P = 1
        D = 0
        P9 = 1
        C1 = 2
    End If

    fastBreak = 1
    prevBall$ = players$(P, B1(P, ballCarrier))

    RN0 = Int(Rnd(1) * 130) + 1

    If RN0 = 15 Then
        fastBreak = 0
        GoTo RBTIEUP
    End If

    Call Rolld100(RN0)
    If CZ%(D) = 1 And RN0 <= 50 Then

        gameClock! = gameClock! - 4
        timeElapsed! = 4
        Call AdjustPlayTime
        Call AdjustShotClock
        Call WHOLAY

        GoTo LAYUP

    Else

        GoTo 300

    End If

    '----- NOODLE -----

    760 '
    If foulOnReb = 1 Then

        foulOnReb = 0

        pbpString$ = players$(P, B1(P, ballCarrier)) + " HAS THE OFF RB"
        If pbpOpt = 1 And pbpDelay! <> 0 Then
            Call WINDEX
            Print pbpString$
            Print "FOR "; gameTeams$(P)
            Call DELAY
        End If
        If pbpOpt = 0 Then
            Call PBP
        End If

        pbpString$ = "HE'S FOULED !!!"
        If pbpOpt = 1 And pbpDelay! <> 0 Then
            Call WINDEX
            Print pbpString$
            Call DELAY
        End If
        If pbpOpt = 0 Then
            Call PBP
        End If

        plyrOff_GAME!(P, B1(P, ballCarrier), 11) = plyrOff_GAME!(P, B1(P, ballCarrier), 11) + 1
        plyrOff_GAME!(P, B1(P, ballCarrier), 24) = plyrOff_GAME!(P, B1(P, ballCarrier), 24) + 1
        totRebounds(P) = totRebounds(P) + 1

        Call UPDATESCREEN

        Call FoulCalled(1)

        LT = 4

        GoTo 9065

    End If

    plyrOff_GAME!(P, B1(P, ballCarrier), 24) = plyrOff_GAME!(P, B1(P, ballCarrier), 24) + 1
    plyrOff_GAME!(P, B1(P, ballCarrier), 11) = plyrOff_GAME!(P, B1(P, ballCarrier), 11) + 1
    totRebounds(P) = totRebounds(P) + 1
    Call UPDATESCREEN

    pbpString$ = players$(P, B1(P, ballCarrier)) + " HAS THE OFF RB"
    If pbpDelay! <> 0 Then
        If pbpOpt = 0 Then Call PBP
        If pbpOpt = 1 Then
            Call WINDEX
            Print pbpString$
            Print "FOR " + gameTeams$(P)
            Call DELAY
        End If
    End If

    If gameClock! <= 0 Then

        GoTo 170

    Else

        RN0 = Int(Rnd(1) * 130) + 1
        prevBall$ = players$(P, B1(P, ballCarrier))

        If RN0 = 32 Then
            GoTo RBTIEUP
        Else
            RN1 = Int(Rnd(1) * 10) + 1
            Call SHOTPERCENT

            If Not (currHalf > 2 Or currHalf = 2 And gameClock! <= 120) Then
                If plyrOff_GAME!(P, B1(P, ballCarrier), 3) - plyrOff_GAME!(P, B1(P, ballCarrier), 7) - plyrOff_GAME!(P, B1(P, ballCarrier), 11) - plyrOff_GAME!(P, B1(P, ballCarrier), 12) <= 0 Then
                    shotChance! = Int(shotChance! * .6 + .5)
                End If
            End If

            If RN1 <= 8 Then
                PZ% = 9
                playChoice = 1
                GoTo 537
            Else
                GoTo 520
            End If

        End If

    End If

    '----- NOODLE -----

    TIEUP:
    If pbpOpt <> 1 Then

        Call Rolld100(RN0)

        Select Case RN0
            Case 1 To 20
                pbpString$ = "BALL IS SLAPPED AWAY"
            Case 21 To 40
                pbpString$ = "BALL IS LOOSE ON THE FLOOR"
            Case 41 To 60
                pbpString$ = players$(P, B1(P, ballCarrier)) + " IS TRAPPED"
            Case 61 To 80
                pbpString$ = "TRIES TO WORK LOOSE"
            Case 81 To 100
                pbpString$ = players$(P, B1(P, ballCarrier)) + " IS PINNED ON THE LINE"
        End Select

        Call PBP

    End If

    RBTIEUP:
    Call Rolld100(RN0)
    P9 = Int(Rnd(1) * 5)

    If pbpOpt = 1 And pbpDelay! <> 0 Then
        Call WINDEX
        Print "JUMP BALL!"
        Call DELAY
    Else
        Select Case RN0
            Case 1 To 25
                pbpString$ = players$(P, B1(P, ballCarrier)) + " IS TIED UP BY " + players$(D, B1(D, P9))
            Case 26 To 50
                pbpString$ = players$(D, B1(D, P9)) + " TIES UP " + players$(P, B1(P, ballCarrier))
            Case 51 To 75
                pbpString$ = "HELD BALL!"
            Case 76 To 100
                pbpString$ = "JUMP BALL!"
        End Select

        Call PBP

    End If

    pbpString$ = "POSSESSION ARROW TO " + gameTeams$(possArrow)

    If pbpOpt = 1 And pbpDelay! <> 0 Then
        Print pbpString$
        Call DELAY
    End If
    If pbpOpt = 0 Then Call PBP

    Call DELAY

    If possArrow = D Then

        turnovers(P) = turnovers(P) + 1
        Call DEADBALLOPTIONS

        For I = 0 To 1
            If defCategory(I) = 0 Then
                PR%(I, 0) = 1
                PR%(I, 1) = 7
                pctContrib(I) = 0
            End If
        Next

        prevBall$ = ""
        If P = 1 Then
            P = 0
            D = 1
            P9 = 0
            C1 = 2
        Else
            'P = 0
            P = 1
            D = 0
            P9 = 1
            C1 = 2
        End If

        C1 = 2
        P9 = P
        possArrow = 1 - possArrow

        GoTo 300

    Else

        possArrow = 1 - possArrow
        Call DEADBALLOPTIONS

        For I = 0 To 1
            If defCategory(I) = 0 Then
                PR%(I, 0) = 1
                PR%(I, 1) = 7
                pctContrib(I) = 0
            End If
        Next

        LT = 4

        GoTo 350

    End If

    '----- NOODLE -----

    ' *** TEAM FOULS ***
    9065 '

    If intentional = 1 Or (ft10thFoul = 1 And teamFouls(D) >= 10) Then

        'BONUS +

        If pbpDelay! <> 0 Then
            If pbpOpt = 0 Then
                pbpString$ = "2 FT: " + players$(P, B1(P, ballCarrier)) + Str$(plyrOff_GAME!(P, B1(P, ballCarrier), 1)) + " %"
                Call PBP
                Call DELAY
            Else
                Call WINDEX
                Print gameTeams$(P)
                Print "2 FT: "; players$(P, B1(P, ballCarrier)); "   "; plyrOff_GAME!(P, B1(P, ballCarrier), 1); " %"
            End If
        End If

        X = 50
        Call DEADBALLOPTIONS

        For I = 0 To 1
            If defCategory(I) = 0 Then
                PR%(I, 0) = 1
                PR%(I, 1) = 7
                pctContrib(I) = 0
            End If
        Next

        X = 0

        If pbpDelay! <> 0 Then
            If pbpOpt = 0 Then
                pbpString$ = "2 FT: " + players$(P, B1(P, ballCarrier)) + Str$(plyrOff_GAME!(P, B1(P, ballCarrier), 1)) + " %"
                Call PBP
            Else
                Call WINDEX
                Print gameTeams$(P)
                Print "2 FT: "; players$(P, B1(P, ballCarrier)); "   "; plyrOff_GAME!(P, B1(P, ballCarrier), 1); " %"
            End If
        End If

        '2 FT Attempts
        For freethrow = 1 To 2:

            'Increase FT ATT
            plyrOff_GAME!(P, B1(P, ballCarrier), 9) = plyrOff_GAME!(P, B1(P, ballCarrier), 9) + 1

            Call Rolld100(shotRoll)

            'FT % Success Chance
            baseShotChance = plyrOff_GAME!(P, B1(P, ballCarrier), 1)
            If tmFatigue(P, B1(P, ballCarrier)) <= -5 Then
                baseShotChance = Int(baseShotChance * .9 + .5)
            End If

            If shotRoll <= baseShotChance Then

                If pbpOpt = 0 Then
                    Call THREEFT(freethrow)
                Else
                    'pbpOpt = 1
                    Call MADEFT(freethrow)
                End If
            Else
                'shotRoll > baseShotChance And
                If pbpOpt = 0 Then
                    Call BADFT(freethrow)
                Else
                    'pbpOpt = 1
                    Call MISSFT(freethrow)
                End If

            End If

        Next freethrow

        If gameClock! <= 0 Then 170

        Call CLEARPBPBOX

        If shotRoll > baseShotChance And intentional = 0 Then
            'Missed shot (no intentional foul)
            GoTo 650
        End If

        If intentional = 1 Then
            PASS = 1
            intentional = 0
            X = 0
            pbpString$ = gameTeams$(P) + " BALL AFTER INT. FOUL"
            Call SELECTPBP
            Locate 8, 41: Print Space$(12)
            Call DELAY
            Call STOPPAGE
            Call PRESSGUARD
        Else

            prevBall$ = ""

            If P = 1 Then
                P = 0
                D = 1
                P9 = 0
                C1 = 2
            Else
                'P = 0
                P = 1
                D = 0
                P9 = 1
                C1 = 2
            End If

            X = 0

            pbpString$ = gameTeams$(P) + " BALL O.B. AFTER SCORE"
            Locate 8, 41: Print Space$(12)

            Call SELECTPBP
            Call DELAY
            Call STOPPAGE
            Call PRESSGUARD

        End If

        GoTo 300

    Else

        'Check if BONUS
        If teamFouls(D) > 6 Then

            GoTo 570

        Else

            If gameClock! <= 0 Then 170

            Call DEADBALLOPTIONS

            For I = 0 To 1
                If defCategory(I) = 0 Then
                    PR%(I, 0) = 1
                    PR%(I, 1) = 7
                    pctContrib(I) = 0
                End If
            Next

            If ft6FloorFouls = 0 Then

                If pbpOpt = 0 Or pbpDelay! = 0 Then
                    Call WINDEX
                    pbpString$ = gameTeams$(P) + " BALL O.B. AFTER FOUL"
                    Call PBP
                    Locate 8, 41: Print Space$(12)

                Else
                    'pbpOpt = 1
                    Call WINDEX
                    Print gameTeams$(P) + " BALL O.B. AFTER FOUL"
                    Call DELAY
                    Locate 8, 41: Print Space$(12)
                End If

                GoTo 350

            End If

            Call UPDATESCREEN

            pbpString$ = "1 FT: " + players$(P, B1(P, ballCarrier)) + Str$(plyrOff_GAME!(P, B1(P, ballCarrier), 1)) + " %"
            Call PBP

            X = 50

            Call DEADBALLOPTIONS

            For I = 0 To 1
                If defCategory(I) = 0 Then
                    PR%(I, 0) = 1
                    PR%(I, 1) = 7
                    pctContrib(I) = 0
                End If
            Next

            X = 0

            pbpString$ = "1 FT: " + players$(P, B1(P, ballCarrier)) + Str$(plyrOff_GAME!(P, B1(P, ballCarrier), 1)) + " %"

            Call PBP

            F3 = 1

            'Increase FT ATT
            plyrOff_GAME!(P, B1(P, ballCarrier), 9) = plyrOff_GAME!(P, B1(P, ballCarrier), 9) + 1

            baseShotChance = plyrOff_GAME!(P, B1(P, ballCarrier), 1)
            If tmFatigue(P, B1(P, ballCarrier)) <= -5 Then
                baseShotChance = Int(baseShotChance * .9 + .5)
            End If

            Call Rolld100(shotRoll)

            If shotRoll <= baseShotChance Then
                If pbpOpt = 0 Then
                    Call THREEFT(1)
                Else
                    'pbpOpt = 1
                    Call MADEFT(1)
                End If
            Else
                'shotRoll > baseShotChance
                If pbpOpt = 0 Then
                    Call BADFT(1)
                Else
                    'pbpOpt = 1
                    Call MISSFT(1)
                End If
            End If

            LT = 0

            If gameClock! <= 0 Then
                timeElapsed! = gameClock!
                Call AdjustPlayTime
                GoTo 170
            End If

            If shotRoll > baseShotChance Then
                'Missed shot
                GoTo 650
            Else

                prevBall$ = ""
                If P = 1 Then
                    P = 0
                    D = 1
                    P9 = 0
                    C1 = 2
                Else
                    'P = 0
                    P = 1
                    D = 0
                    P9 = 1
                    C1 = 2
                End If

                pbpString$ = gameTeams$(P) + " BALL O.B. AFTER SCORE"
                Locate 8, 41: Print Space$(12)

                Call PBP
                Call DELAY
                Call STOPPAGE

                ballCarrier = 6
                X = 0

                Call PRESSGUARD

                GoTo 300

            End If

        End If

    End If

End Sub
