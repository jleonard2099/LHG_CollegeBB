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

        'currGame = schedGame(0) + schedGame(2)

        'Open "pbplog" + Str$(currGame) For Append As #11

        Open diskPaths$(0) + scheduleFile$ For Random As #7 Len = 48

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
        'gameLoc = 0 is not used ("Visitor")
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
                        If sClockVal > 2 Then sClockVal = 0
                        'If sClockVal > 3 Then sClockVal = 0

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
        '-- but it's rare, or defType, or an outlier
        '-- Original game ranged 9,000 to 11,000
        actualAttendance& = Int(Rnd(1) * 12000) + 2000
    Else
        'Random # +/ 1,000 of team's average attendance
        actualAttendance& = avgAttendance&(1) + Int(Rnd(1) * 2000) - 1000
    End If

    'FY%(0) = 2 * (Visitor Tm Average FGA/G - League FGA / GM)
    'FY%(1) = 2 * (Home Tm Average FGA/G - League FGA / GM)
    'JY = Average of the 2 Ratings
    JY = Int(((FY%(1) + FY%(0)) / 2) + .5)

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

    S9 = (teamStamina(0) + teamStamina(1)) / 2

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
                PA% = D
                Call PBP
            Else
                'pbpOpt = 1
                Call WINDEX
                Print gameTeams$(P) + " GETS THE JUMP"
                PA% = D
                Call DELAY
            End If

        End If

        If (currHalf = 2 Or currHalf >= 4) Then

            P = PA%
            D = 1 - P
            PA% = 1 - PA%

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
            PA% = D
        Else
            P = PA%
            D = 1 - P
            PA% = 1 - PA%
        End If

    End If

    GoTo 300

    '----- NOODLE -----

    170 '
    Erase defStrat, defType, P7

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
                If pbpDelay! > 0 Then Call WINDEX: Print pbpString$
        End Select
    End If

    If currHalf > 3 Then
        pbpString$ = "END OF OVERTIME" + Str$(currHalf - 3)
        Select Case pbpOpt
            Case 0:
                Color 7, 0
                Print pbpString$
            Case 1:
                If pbpDelay! > 0 Then Call WINDEX: Print pbpString$
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

        P2(0) = 0
        P2(1) = 0

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
    If gameClock! <= 5 And (threePtOpt = 1 And score(D, 0) - score(P, 0) <= 3 Or threePtOpt = 0 And score(D, 0) - score(P, 0) <= 2) And score(D, 0) - score(P, 0) >= 0 And currHalf >= 2 Then

        If playerMode = 2 Or playerMode = 1 And P = compTeam Then
            ' Computer handle last 5 seconds
            Call d100(RN0)

            scoreDiff = score(P, 0) - score(D, 0)

            If gameClock! >= 3 And gameClock! <= 5 And threePtOpt = 0 And scoreDiff < 3 Then I1% = 5
            If gameClock! >= 3 And gameClock! <= 5 And threePtOpt = 1 And scoreDiff = 3 Then I1% = 4
            If gameClock! >= 3 And gameClock! <= 5 And threePtOpt = 1 And scoreDiff = 2 And RN0 <= 50 Then I1% = 5
            If gameClock! >= 3 And gameClock! <= 5 And threePtOpt = 1 And scoreDiff = 2 And RN0 > 50 Then I1% = 4
            If gameClock! < 3 And threePtOpt = 0 And scoreDiff < 3 Then I1% = 0
            If gameClock! < 3 And threePtOpt = 1 And scoreDiff = 3 Then I1% = 1
            If gameClock! < 3 And threePtOpt = 1 And scoreDiff = 2 And RN0 <= 50 Then I1% = 0
            If gameClock! < 3 And threePtOpt = 1 And scoreDiff = 2 And RN0 > 50 Then I1% = 1

        Else

            Call LASTFIVEMENU

            280 '
            Do
                I$ = GetKeyPress$
                I1% = Val(I$)
            Loop Until I1% >= 0 And I1% <= 5 And Not (I1% = 3 And timeouts(P9) <= 0) And Not ((I1% = 1 Or I1% = 4) And threePtOpt = 0)

            Color 15, 0
            Call SCOREBOARD

        End If

        ballCarrier = Int(Rnd(1) * 5)

        Call d100(RN0)

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

                Call GoodBasket

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

            End If

        End If

        If I1% = 2 Then

            pbpString$ = "SHOT MISSES...GAME OVER"
            Call PBP

            plyrOff_GAME!(P, B1(P, ballCarrier), 7) = plyrOff_GAME!(P, B1(P, ballCarrier), 7) + 1
            If threePtOpt = 1 Then
                threeFG(P, B1(P, ballCarrier), 1) = threeFG(P, B1(P, ballCarrier), 1) + 1
            End If

            GoTo TIMEEXP
        End If

        If I1% >= 3 And RN0 <= 8 Then
            pbpString$ = "BALL LOST O.B.  GAME OVER"
            Call PBP
            GoTo TIMEEXP
        End If

        If I1% >= 3 And RN0 > 8 And RN0 <= 18 Then
            pbpString$ = "PASS INTERCEPTED"
            Call PBP
            GoTo TIMEEXP
        End If

        If I1% = 3 And gameClock! < 1.5 Or I1% >= 4 And gameClock! < 3 Then

            pbpString$ = "INBOUND PASS TO " + players$(P, B1(P, ballCarrier))
            Call PBP

            pbpString$ = "TIME EXPIRES"
            Call PBP

            GoTo TIMEEXP
        End If

        If I1% = 3 Then

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
            Call HalfCourt

        End If

        If I1% = 0 Or I1% = 4 Or (I1% = 3 And I2% = 1) Then

            ' TAKE3
            Call d100(RN0)
            If gameClock! <= .5 Then XX% = 2
            If gameClock! > .5 And gameClock! <= 1.8 Then XX% = 12
            If gameClock! > 1.8 Then XX% = 20

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

                Call GoodBasket
                Call UPDATESCREEN

            End If

            GoTo TIMEEXP

        End If

        Call d100(RN0)
        If gameClock! <= .5 Then XX% = 3
        If gameClock! > .5 And gameClock! <= 1.8 Then XX% = 16
        If gameClock! > 1.8 Then XX% = 29

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
            Call GoodBasket
            Call UPDATESCREEN
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

    P6 = 0
    D6 = 0
    A4 = 0
    A6 = 0
    PASS% = 1
    F! = 0

    If X <> 90 Then X = 0

    If (defType(D) = 1 Or defType(D) = 3) And P7(D) = 0 Then
        X0% = 50
        X1% = -1
        Call PRDFAT
    End If

    If defType(D) = 2 And P7(D) = 0 Then
        X0% = 100
        X1% = 1
        Call PRDFAT
        Call PRPFAT
    End If

    If defType(D) = 1 And P7(D) = 0 Then
        X0% = 25
        X1% = -1
        Call PRPFAT
    End If

    GoTo 400

    'End If

    '----- NOODLE -----

    350 '
    PASS% = PASS% + 1
    P6 = 0
    A4 = 0
    D6 = 0
    A6 = 1
    F! = 0
    X = 0

    If PASS% = 2 And P7(D) > 0 Then

        If (defType(D) = 1 Or defType(D) = 3) Then
            X0% = 50
            X1% = -1
            Call PRDFAT
        End If

        If defType(D) = 2 Then
            X0% = 100
            X1% = 1
            Call PRDFAT
            Call PRPFAT
        End If

        If defType(D) = 1 Then
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

    If P7(D) > 0 And PASS% = 1 Then

        ' *** FULL COURT PRESS ***

        'Yes it's odd that Case 1, 2 and 4
        'have identical outcome.
        'That is the code as it was written.

        Select Case P7(D)

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

        If currHalf >= 2 And gameClock! <= 180 And P7(D) = 5 Then

            'DENIAL
            Call d100(RN0)

            If RN0 <= 10 Then

                gameClock! = gameClock! - 4
                timeElapsed! = 4
                Call AdjustPlayTime
                Call AdjustShotClock
                Call WHOLAY

                If PASS% = 1 Then
                    pbpString$ = players$(P, B1(P, ballCarrier)) + " GETS THE INBOUND PASS"
                Else
                    pbpString$ = players$(P, B1(P, ballCarrier)) + " HAS THE BALL"
                End If

                Call PBP
                pbpString$ = "HE IS STREAKING DOWN THE FLOOR!"
                Call PBP

                LAYUP:
                FB% = 1
                F! = 98
                P6 = 1

                GoTo 520

            End If

            Call d100(RN1)
            Call d100(RN0)

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

                If PASS% > 1 Then skipInbound% = 1

            End If

            If skipInbound% = 0 Then

                If RN1 <= 4 And timeouts(P) > 0 Then
                    pbpString$ = gameTeams$(P) + " CAN'T INBOUND THE BALL"
                    Call SELECTPBP

                    pbpString$ = gameTeams$(P) + " CALLS A TIME OUT"

                    If pbpOpt = 0 Then Call PBP
                    If pbpOpt = 1 And pbpDelay! <> 0 Then Print pbpString$: Call DELAY

                    PASS% = 0
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

                    GoTo 435

                End If

                432 '
                pbpString$ = "PASS TO " + players$(P, B1(P, ballCarrier))
                Call SELECTPBP

            End If

            Call d100(RN0)
            If RN0 <= 5 Then IN% = 1

            Call d100(RN1)

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

            Call d100(RN2)

            If RN2 <= 80 Then
                fouler = desigFouler
                Call FoulCalled(0)
            Else
                Call FoulCalled(1)
            End If

            GoTo 9065

        End If

        If P7(D) = 1 Then
            X0% = 25
            X1% = -1
            tmFatigue(P, B1(P, BU%)) = tmFatigue(P, B1(P, BU%)) - 1
            Call PRPFAT
        End If

        If P7(D) > 1 Then
            X0% = 25
            X1% = -1
            Call PRPFAT
        End If

        Call d100(RN0)

        Select Case P7(D)

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

        Call d100(RN0)

        If RN0 <= DP(2, P7(D)) Then

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
        If RN0 <= DP(2, P7(D)) + 1 Then
            X = 0
            GoTo 580
        End If

        CY = DP(2, P7(D)) + 1 + teamAdj(P, 5) + teamAdj(D, 6) + DP(0, P7(D)) - PR%(D, 0)

        If RN0 <= CY Then
            Call LASTSHOT
            GoTo 517
        End If

        CY = CY + teamAdj(P, 4) + DP(1, P7(D)) - PR%(D, 1)
        PC%(D) = PC%(D) + 1

        If PC%(D) > 3 Then
            PR%(D, 0) = 0
            PR%(D, 1) = 0
        End If

        If RN0 <= CY Then 435

    Else

        Call d100(RN0)

        If (offStrat(P) <> 8 And offStrat(P) <> 7) Or defType(D) <> 2 Then

            If RN0 <= D8(4, defType(D)) Then

                Call LASTSHOT

                Call ResetShotClock

                Call UPDATESCREEN

                X = 88

                Call FoulCalled(1)
                LT = 4

                GoTo 9065

            End If

            If RN0 <= D8(4, defType(D)) + 1 Then X = 0: GoTo 580

            OY% = 0
            If offStrat(P) = 4 Or offStrat(P) = 7 Then OY% = Int((D8(2, defType(D)) / 2) + .5) * -1

            CY = D8(4, defType(D)) + 1 + teamAdj(P, 5) + teamAdj(D, 6) + D8(2, defType(D)) + OY% + 1
            If RN0 <= CY Then Call LASTSHOT: GoTo 517

            CY = CY + teamAdj(P, 4) + D8(3, defType(D))
            OY% = 0

            If offStrat(P) = 4 Or offStrat(P) = 7 Then OY% = Int((D8(3, defType(D)) / 2) + .5) * -1
            If offStrat(P) = 1 Then OY% = 7
            If offStrat(P) = 2 Then OY% = -7

            If RN0 <= CY + OY% Then 435

        End If

    End If

    '-----

    465 '
    Call d100(RN0)

    'GS% = GoodShot% ???
    'In other words, how does the player's accuracy line up with the shot difficulty ?

    'JY = Average FGA/G Adjustment For Both Teams
    '-- Most values fall in the range of -8 to 8 statistically

    Select Case sClockVal

        Case 0
            'No Shot Clock
            'Will range from -8 to 5 for most teams
            GS% = 39 + Int(((JY - 52) * .8) + .5)

        Case 1
            '45 Sec Shot Clock
            'Will range from -8 to 5 for most teams
            GS% = 39 + Int(((JY - 52) * .8) + .5)

        Case 2
            '35 Sec Shot Clock
            'Will range from -2 to 0 for most teams
            GS% = 39 + Int(((JY - 58) * .8) + .5)

        Case 3
            '30 Sec Shot Clock
            GS% = 39 + Int(((JY - 62) * .8) + .5)

    End Select

    'Pressure Man-to-Man | 2-3 Zone = Add 7
    '-1 to 12
    If defType(D) = 1 Or defType(D) = 2 Then GS% = GS% + 7

    'Trapping Zone = Subtract 7
    '-15 to -2
    If defType(D) = 3 Then GS% = GS% - 7

    'Quick Off
    '6 to 23
    If offStrat(P) = 3 Then GS% = GS% + 21

    'Delay Off
    '-36 to -23
    If offStrat(P) = 4 Then GS% = GS% - 21

    'Total Stall / Hold Ball / Milk Clock and More than 15 Secs on Shot Clock
    If offStrat(P) = 7 Or offStrat(P) = 8 Or (offStrat(P) = 5 And shotClock > 15 And sClockVal >= 1) Then 437

    If RN0 > GS% Then
        'This is the only place GS% is used
        '(about 75-90% chance)
        GoTo 437
    Else
        'If our random # from 1-100 was above GS%...
        '(about 10-25% chance)
        RN0 = Int(Rnd(1) * 100)

        If RN0 <= D8(1, defType(D)) Then

            A4 = 0

            If Not (currHalf = 2 And gameClock! <= 120 Or currHalf > 2) Then

                If currHalf <= 2 Then
                    If plyrOff_GAME!(P, B1(P, ballCarrier), 3) - plyrOff_GAME!(P, B1(P, ballCarrier), 7) - plyrOff_GAME!(P, B1(P, ballCarrier), 11) - plyrOff_GAME!(P, B1(P, ballCarrier), 12) <= 0 Then 437
                End If

            End If

            Call SHOTPERCENT

            A4 = Int(Rnd(1) * 28) + 1
            RN0 = Int(Rnd(1) * 9) + 1
            I2 = Int(Rnd(1) * 5)

            If tmFatigue(P, B1(P, ballCarrier)) > -5 Then

                If RN0 <= plyrOff_GAME!(P, B1(P, I2), 4) Then

                    F! = plyrOff_GAME!(P, B1(P, ballCarrier), 0) + dFGPA(D) + A4 + ft3PtFoul(P) + ft3PtFoul(D)
                    ft3PtFoul(P) = 0
                    ft3PtFoul(D) = 0
                    PZ% = 2
                    P6 = 1
                    X = 0

                    Call d100(RN0)

                    If PZ% = 2 And RN0 <= 50 Then
                        PZ% = 3
                        P6 = 1
                    End If

                    GoTo 520

                End If

            End If

        End If

    End If

    If offStrat(P) = 4 Then
        'Delay Offense
        GoTo 437

    Else

        If currHalf <= 2 Then

            If Not (currHalf = 2 And gameClock! <= 120 Or currHalf > 2) Then
                If plyrOff_GAME!(P, B1(P, ballCarrier), 3) - plyrOff_GAME!(P, B1(P, ballCarrier), 7) - plyrOff_GAME!(P, B1(P, ballCarrier), 11) - plyrOff_GAME!(P, B1(P, ballCarrier), 12) <= 0 Then 437
            End If

        End If

        P6 = 1
        Call SHOTPERCENT

        PZ% = Int(Rnd(1) * 3) + 5

        X = 0

        Call d100(RN0)
        If PZ% = 2 And RN0 <= 50 Then
            PZ% = 3
            P6 = 1
        End If

        GoTo 520

    End If


    '----- NOODLE -----

    435 '
    P9 = Int(Rnd(1) * 5)

    If Int(Rnd(1) * 5) + 1 <= plyrOff_GAME!(D, B1(D, P9), 5) Then

        plyrOff_GAME!(D, B1(D, P9), 18) = plyrOff_GAME!(D, B1(D, P9), 18) + 1

        If P7(D) <= 0 Or PASS% <> 1 Then Call LASTSHOT
        Call ResetShotClock

        plyrOff_GAME!(P, B1(P, ballCarrier), 23) = plyrOff_GAME!(P, B1(P, ballCarrier), 23) + 1

        If pbpDelay! <> 0 Then

            pbpString$ = "BALL STOLEN BY " + players$(D, B1(D, P9)) + "!"

            If pbpOpt = 1 Then
                Call WINDEX
                Print pbpString$
                Call DELAY
            Else

                Call d100(RN0)

                Select Case RN0
                    Case 1 To 3
                        pbpString$ = players$(D, B1(D, P9)) + " QUICKLY REACHES IN!": Call PBP
                    Case 4 To 6
                        pbpString$ = players$(D, B1(D, P9)) + " SNEAKS IN FROM BEHIND!": Call PBP
                    Case 7 To 10
                        pbpString$ = players$(D, B1(D, P9)) + " GETS A HAND ON THE BALL!": Call PBP
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
                        pbpString$ = players$(D, B1(D, P9)) + " PICKS IT OFF!"
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

        FB% = 1

        Call d100(RN0)

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

        Call d100(RN1)
        If PASS% > 1 And (defType(P9) = 0 Or defType(P9) = 1 Or defType(P9) = 3) And RN1 <= 15 Then GoTo LAYUP
        If PASS% > 1 And defType(P9) = 2 And RN1 <= 5 Then GoTo LAYUP
        If PASS% = 1 And P7(P) > 0 And RN1 <= 33 Then GoTo LAYUP
        If PASS% = 1 And P7(P) > 0 And P7(P) <> 3 And RN1 <= 40 Then GoTo LAYUP

        GoTo 300

    End If

    Call d100(RN0)

    Select Case P7(D)

        Case 1
            If PASS% = 1 And RN0 <= plyrOff_GAME!(P, B1(P, BU%), 4) Then GoTo LAYUP
            If PASS% = 1 And RN0 <= plyrOff_GAME!(P, B1(P, BU%), 4) * 2 Then GoTo GDSHOT

        Case 2
            If PASS% = 1 And RN0 <= plyrOff_GAME!(P, B1(P, BU%), 4) * 2 Then GoTo LAYUP
            If PASS% = 1 And RN0 <= plyrOff_GAME!(P, B1(P, BU%), 4) * 4 Then GoTo GDSHOT

        Case 3
            If PASS% = 1 And RN0 <= 7 Then GoTo LAYUP
            If PASS% = 1 And RN0 <= 14 Then GoTo GDSHOT

        Case 4
            If PASS% = 1 And RN0 <= 15 Then GoTo LAYUP
            If PASS% = 1 And RN0 <= 30 Then GoTo GDSHOT

        Case 5
            GoTo 432

    End Select

    ' --- I think this is by itself and the above are separate??? ---

    ' ***BAD SHOT ***
    437 '
    P6 = 0
    J8% = 0
    PZ% = 1

    RN0 = Int(Rnd(1) * 3) + 1
    F3S% = Int(Rnd(1) * 7) + 3

    'Adjust shot % based on player's FG%
    If plyrOff_GAME!(P, B1(P, ballCarrier), 0) > 60 Then

        Select Case RN0
            Case 1
                F! = 24
            Case 2
                F! = 30
            Case 3
                F! = 36
        End Select

    Else

        Select Case RN0
            Case 1
                F! = Int(plyrOff_GAME!(P, B1(P, ballCarrier), 0) * .4 + .5)
            Case 2
                F! = Int(plyrOff_GAME!(P, B1(P, ballCarrier), 0) * .5 + .5)
            Case 3
                F! = Int(plyrOff_GAME!(P, B1(P, ballCarrier), 0) * .6 + .5)
        End Select

    End If

    If Int(Rnd(1) * 150) + 1 = 40 Then
        '0.66% chance of TIEUP
        GoTo TIEUP
    Else

        X = 0
        Call d100(RN0)

        If PZ% = 2 And RN0 <= 50 Then
            PZ% = 3
            P6 = 1
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

        If SV% <> 1 Then PT% = 1: Call MAKEPBP

        SV% = 0

        If pbpOpt = 1 And pbpDelay! <> 0 Then Call WINDEX: Print "TURNOVER BY "; gameTeams$(P): Call DELAY
        If pbpOpt = 0 Then Call PBP

        pbpString$ = "BALL OUT OF BOUNDS TO " + gameTeams$(D)
        If pbpOpt = 0 Then Call PBP
        If pbpOpt = 1 And pbpDelay! <> 0 Then Print pbpString$: Call DELAY

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
            If P7(I) = 0 Then
                PR%(I, 0) = 1
                PR%(I, 1) = 7
                PC%(I) = 0
            End If
        Next

        GoTo 300

    End If

    '----- NOODLE -----

    GDSHOT:
    FX% = plyrOff_GAME!(P, B1(P, ballCarrier), 0)

    If tmFatigue(P, B1(P, ballCarrier)) <= -5 Then FX% = FX% - 10

    F! = FX% + Int(Rnd(20)) + 11 + ft3PtFoul(D) + ft3PtFoul(P)
    ft3PtFoul(P) = 0
    ft3PtFoul(D) = 0
    PZ% = Int(Rnd(1) * 3) + 5
    X = 0

    Call d100(RN0)

    If PZ% = 2 And RN0 <= 50 Then
        PZ% = 3
        P6 = 1
    End If

    520 '
    If J8% = 1 Then PZ% = 4

    Call LASTSHOT
    If gameClock! <= 0 Then 170

    If PZ% = 4 And F3S% <= 0 Then F3S% = 3

    If pbpDelay! <> 0 Then

        If PZ% = 10 Then

            GDSHOTDES:
            If pbpDelay! <> 0 Then

                Call d100(RN0)

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

                Call d100(RN1)

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

            If F! > 70 And FB% = 1 And PASS% = 1 Then

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

                        P6 = 1

                    Case 3:

                        If pbpDelay! <> 0 Then

                            P6 = 1

                            Call d100(RN0)

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
                        pbpString$ = pbpString$ + Str$(F3S%) + "*"
                        Call PBP

                        P6 = 1

                    Case 5:
                        pbpString$ = "HE'S OPEN FROM MEDIUM RANGE"
                        Call SHOTSTRING
                        Call PBP

                        P6 = 1

                    Case 6:
                        pbpString$ = "HE'S OPEN FROM MEDIUM RANGE"
                        Call SHOTSTRING
                        Call PBP

                        P6 = 1

                    Case 7:
                        pbpString$ = "HE'S OPEN FROM MEDIUM RANGE"
                        Call SHOTSTRING
                        Call PBP

                        P6 = 1

                End Select

            End If

        End If

    End If

    529 '
    If F! <= 0 Then F! = 1
    If F! >= 100 Then F! = 99

    skipShotPct% = 0

    If pbpDelay! <> 0 And pbpOpt = 1 Then

        Call WINDEX
        Print "PASS TO "; players$(P, B1(P, ballCarrier)); " OF "; gameTeams$(P)

        If PZ% <> 4 Then
            Locate 10: Print "SHOT % "; F!;
            If PZ% >= 2 And PZ% <= 8 Then Print "*";
        Else
            Locate 10
            Print "3 PT SHOT % "; F3S%;: Print " *";
            skipShotPct% = 1
        End If

    End If

    If skipShotPct% = 0 Then

        If pbpDelay! <> 0 And pbpOpt <> 1 Then

            If PZ% <> 4 Then
                Locate 8, 41: Print Using "SHOT % ###   "; F!;
                If PZ% >= 2 And PZ% <= 8 Then Locate , 52: Print "*";
            Else
                Locate 8, 41
                Print Using "SHOT % ###"; F3S%;: Print " *";
            End If

        End If

    End If

    Print

    If pbpOpt = 1 And pbpDelay! <> 0 Then Call DELAY

    '--- This definitely can be done much better!!!

    ' *** Determine value for P6 ***
    'P6 = play type
    '0 = Pass
    '1 = Shoot (2FGA)
    '2 = Timeout
    '3 = Foul
    '4 = Shoot (3FGA)

    'Computer Opponent or "Let Computer Shoot"
    If playerMode <> 0 Or coachOpt = 1 Then

        ' NOT (Overtime, 4 minutes left, Offense Ahead, Computer is Visitor / On Offense)
        If Not (currHalf >= 2 And gameClock! <= 240 And score(P, 0) - score(D, 0) > 0 And playerMode = 1 And P = compTeam) Then

            'PC vs PC, Computer on Defense
            If playerMode = 2 Or playerMode = 1 And D = compTeam Then

                'Human Opponent | PC is Visitor AND Coach Shots
                If (playerMode = 1 Or playerMode = 0) And coachOpt <> 1 Then
                    GoTo 530
                End If

            End If

        End If

        If gameClock! <= 1 Or sClockVal >= 1 And shotClock <= 7 Then
            P6 = 1
        Else
            If gameClock! <= 8 And score(P, 0) - score(D, 0) <= -3 And currHalf >= 2 Then
                P6 = 1
            Else
                If gameClock! <= 20 And score(P, 0) - score(D, 0) <= -5 And currHalf >= 2 Then
                    P6 = 1
                Else

                    FX% = plyrOff_GAME!(P, B1(P, ballCarrier), 0)

                    If tmFatigue(P, B1(P, ballCarrier)) <= -5 Then FX% = FX% - 10

                    'Determine when to shoot
                    If P6 <> 1 Then

                        'Human Opponent, Computer Shoots | Computer is Visitor AND on Defense, Computer Shoots | Computer is Visitor AND On Offense | Computer is Home Team
                        If (playerMode = 0 And coachOpt = 1 Or playerMode = 1 And D = compTeam And coachOpt = 1 Or playerMode = 1 And P = compTeam Or playerMode = 2) Then

                            If gameClock! <= 660 And gameClock! >= 540 And currHalf = 1 And timeouts(P) >= 5 Then
                                P6 = 2
                                CTO% = 1
                            Else
                                If gameClock! <= 240 And gameClock! >= 120 And currHalf = 1 And timeouts(P) >= 4 Then
                                    P6 = 2
                                    CTO% = 1
                                Else
                                    If gameClock! <= 840 And gameClock! >= 720 And currHalf = 2 And timeouts(P) >= 3 Then
                                        P6 = 2
                                        CTO% = 1
                                    Else
                                        If gameClock! <= 300 And gameClock! >= 180 And currHalf = 2 And timeouts(P) >= 2 Then
                                            P6 = 2
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

        If UCase$(I$) = "T" Then
            Call TICKER
            Call SCOREBOARD
            GoTo 529
        End If

        If UCase$(I$) = "D" Then
            Call ZONECHANGE
            Call SCOREBOARD
            GoTo 529
        End If

        If UCase$(I$) = "O" Then
            Call OPTIONS
            GoTo 529
        End If

        P6 = Val(I$)

        If P6 < 1 Or P6 > 4 Then P6 = 0
        If P6 = 4 Then FT% = 1

    End If

    'Loop Until playerMode <> 0 Or coachOpt = 1 Or (UCase$(I$) <> "T" And UCase$(I$) <> "D" And UCase$(I$) <> "O")

    'Foul
    If Val(I$) = 3 And (playerMode = 0 Or playerMode = 1 And P = compTeam) And currHalf >= 4 And gameClock! < 240 Then
        P6 = 3
    End If

    'Timeout
    If Val(I$) = 2 And (playerMode = 0 Or playerMode = 1 And D = compTeam) And timeouts(P) > 0 Then
        P6 = 2
    End If

    P3 = P6

    If P6 = 2 And timeouts(P) = 0 Or P6 = 3 And currHalf = 1 Or P6 = 3 And playerMode = 1 And D = compTeam Or P6 = 3 And currHalf = 2 And gameClock! > 240 Then
        GoTo 530
    End If

    ' *** Decide if 3FGA ***
    If J8 = 1 Then
        If (playerMode = 1 And P = compTeam) Or (playerMode = 2) Then
            P6 = 4
        End If
    End If

    scoreDiff = score(D, 0) - score(P, 0)
    'Try for 3 near end of game when down by 3
    If currHalf >= 2 And gameClock! < 30 And scoreDiff = 3 And ((playerMode = 1 And P = compTeam) Or playerMode = 2) Then
        If threePtOpt = 1 Then P6 = 4
    End If

    'Shoot and in 3 pt range
    If (P6 = 1 Or P6 = 4) And PZ% = 4 Then
        P6 = 4
    End If

    'If (P6 = 4 And J8% = 1) Then
    'End If

    ' *** Done determining value for P6 ***
    caseVar% = P6 + 1

    Select Case caseVar%

        Case 1
            GoTo 350

        Case 2
            537 '
            ' *** SHOOT !!! ***
            PT% = 3
            Call MAKEPBP
            Call PBP

            S = 0
            SS = 0

            Call d100(RN0)

            plyrOff_GAME!(P, B1(P, ballCarrier), 7) = plyrOff_GAME!(P, B1(P, ballCarrier), 7) + 1
            GG = score(D, 0) - score(P, 0)

            If Not (currHalf > 1 And gameClock! <= 30 And GG = 2 Or currHalf > 1 And gameClock! <= 30 And GG = 0 Or currHalf > 1 And gameClock! <= 30 And GG = -2) Then

                blockRat_TMP = plyrOff_GAME!(P, B1(P, ballCarrier), 16)
                fAdj_TMP = teamAdj(D, 7)

                If RN0 <= plyrOff_GAME!(P, B1(P, ballCarrier), 16) + teamAdj(D, 7) Then

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
                        If P7(I) = 0 Then
                            PR%(I, 0) = 1
                            PR%(I, 1) = 7
                            PC%(I) = 0
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
                    For F3 = 1 To 2:

                        Call d100(S)

                        'FT Success Chance
                        FX% = plyrOff_GAME!(P, B1(P, ballCarrier), 1)

                        If tmFatigue(P, B1(P, ballCarrier)) <= -5 Then FX% = Int(FX% * .9 + .5)

                        If S <= FX% Then

                            If pbpOpt = 0 Then
                                Call THREEFT
                            Else
                                'pbpOpt = 1
                                Call MADEFT
                            End If
                        Else
                            'S > FX% And
                            If pbpOpt = 0 Then
                                Call BADFT
                            Else
                                'pbpOpt = 1
                                Call MISSFT

                            End If
                        End If

                    Next F3

                    'Increase FT ATT
                    plyrOff_GAME!(P, B1(P, ballCarrier), 9) = plyrOff_GAME!(P, B1(P, ballCarrier), 9) + 2
                    LT = 0

                    If gameClock! <= 0 Then
                        timeElapsed! = gameClock!
                        Call AdjustPlayTime
                        GoTo 170
                    End If

                    If S > FX% Then
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

            Call d100(S)
            R0! = F! * ((DY / 100) + (.03 * threePtOpt))

            If Not (currHalf > 1 And gameClock! < 30 And GG = 2 Or currHalf > 1 And gameClock! < 30 And GG = 0 Or currHalf > 1 And gameClock! < 30 And GG = -2) Then

                If S <= R0! Then

                    'Foul + Stuff
                    Call ResetShotClock
                    gameClock! = gameClock! - 4
                    timeElapsed! = 4
                    Call AdjustPlayTime
                    Call AdjustShotClock

                    LT = 0

                    Call UPDATESCREEN
                    Call GoodBasket

                    If DK% = 1 And pbpOpt = 0 Then PT% = 2: Call MAKEPBP: pbpString$ = "STUFF BY " + players$(P, B1(P, ballCarrier)): Call PBP: DK% = 0: GoTo 617
                    If DK% = 1 And pbpOpt = 1 Then Call WINDEX: Print "STUFF BY " + players$(P, B1(P, ballCarrier)): Call DELAY: DK% = 0: GoTo 617
                    If PZ% = 8 Then PT% = 7: Call MAKEPBP: PZ% = 0: Call PBP: GoTo 617

                    F3 = 1
                    If pbpOpt = 0 Then
                        pbpString$ = "BASKET GOOD BY " + players$(P, B1(P, ballCarrier))
                        Call PBP
                    Else
                        'pbpOpt = 1
                        If pbpDelay! <> 0 Then Call WINDEX: Print "BASKET GOOD BY " + players$(P, B1(P, ballCarrier)): Call DELAY
                    End If

                    617 '
                    pbpString$ = "... AND HE'S FOULED !!!"

                    If pbpOpt = 0 Then Call PBP
                    If pbpOpt = 1 And pbpDelay! <> 0 Then Print pbpString$: Call DELAY

                    Call DELAY

                    F3 = 1
                    score(P, currHalf) = score(P, currHalf) + 2
                    score(P, 0) = score(P, 0) + 2
                    plyrOff_GAME!(P, B1(P, ballCarrier), 8) = plyrOff_GAME!(P, B1(P, ballCarrier), 8) + 1
                    Call UPDATESCREEN
                    Call FoulCalled(1)

                    619 '
                    If pbpOpt = 0 Then pbpString$ = players$(P, B1(P, ballCarrier)) + " WILL SHOOT 1": Call PBP
                    If pbpOpt = 1 And pbpDelay! <> 0 Then Call WINDEX: Print gameTeams$(P): Print "1 FT: "; players$(P, B1(P, ballCarrier)); "   "; plyrOff_GAME!(P, B1(P, ballCarrier), 1); " %"

                    X = 50
                    Call DEADBALLOPTIONS

                    For I = 0 To 1
                        If P7(I) = 0 Then
                            PR%(I, 0) = 1
                            PR%(I, 1) = 7
                            PC%(I) = 0
                        End If
                    Next

                    X = 0

                    Call d100(S)

                    If pbpOpt = 1 And pbpDelay! <> 0 Then
                        Call WINDEX
                        Print gameTeams$(P)
                        Print "1 FT: "; players$(P, B1(P, ballCarrier)); "   "; plyrOff_GAME!(P, B1(P, ballCarrier), 1); " %"
                    End If

                    FX% = plyrOff_GAME!(P, B1(P, ballCarrier), 1)
                    If tmFatigue(P, B1(P, ballCarrier)) <= -5 Then FX% = Int(FX% * .9 + .5)

                    If S <= FX% And pbpOpt = 0 Then
                        'Increase FTM
                        plyrOff_GAME!(P, B1(P, ballCarrier), 10) = plyrOff_GAME!(P, B1(P, ballCarrier), 10) + 1

                        If pbpDelay! <> 0 Then pbpString$ = "FREE THROW IS GOOD": Call PBP

                        score(P, currHalf) = score(P, currHalf) + 1
                        score(P, 0) = score(P, 0) + 1

                        Call UPDATESCREEN

                    End If

                    If S <= FX% Then
                        If pbpOpt = 1 Then Call MADEFT
                    Else
                        If pbpOpt = 0 Then If pbpDelay! <> 0 Then pbpString$ = "FREE THROW IS MISSED": Call PBP
                        If pbpOpt = 1 Then Call MISSFT
                    End If

                    'Increase FT ATT
                    plyrOff_GAME!(P, B1(P, ballCarrier), 9) = plyrOff_GAME!(P, B1(P, ballCarrier), 9) + 1

                    If gameClock! <= 0 Then
                        timeElapsed! = gameClock!
                        Call AdjustPlayTime
                        GoTo 170
                    End If

                    X = 99

                    If S > FX% Then 670

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

            If S <= F! Then

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

                                Call d100(RN0)

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

                Call GoodBasket

                Call UPDATESCREEN

                plyrOff_GAME!(P, B1(P, ballCarrier), 8) = plyrOff_GAME!(P, B1(P, ballCarrier), 8) + 1

                562 '
                pbpString$ = gameTeams$(D) + " BALL O.B. AFTER SCORE"

                If pbpOpt = 0 Then Locate 8, 41: Print Space$(12): Call PBP
                If pbpOpt = 1 And pbpDelay! <> 0 Then Locate 9, 1: Print pbpString$

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

                Call d100(RN0)
                LT = 0
                I2 = Int(Rnd(1) * 5) + 1
                rndPlayer = Int(Rnd(1) * 5)

                If I2 > plyrOff_GAME!(D, B1(D, rndPlayer), 6) Then

                    If pbpOpt = 0 Then PT% = 8: Call MAKEPBP: Call PBP
                    If pbpOpt = 1 And pbpDelay! <> 0 Then Call WINDEX: Print "SHOT MISSED BY "; players$(P, B1(P, ballCarrier)): Call DELAY

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

                        Call d100(RN0)

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
                P6 = 1
                GoTo 537

            Else

                Call d100(S)

                threeFG(P, B1(P, ballCarrier), 1) = threeFG(P, B1(P, ballCarrier), 1) + 1
                plyrOff_GAME!(P, B1(P, ballCarrier), 7) = plyrOff_GAME!(P, B1(P, ballCarrier), 7) + 1
                PT% = 3

                Call MAKEPBP
                Call PBP

                I2 = Int(Rnd(1) * 50) + 1

                'I2 = 43 is a random # chosen on which fouls are caused
                'there is a 2% chance of it being reached
                'Much of this logic could be condensed in the S > F3S and S <= F3S logic

                If I2 = 43 And ft3PtFoul = 1 And S > F3S% Then
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
                        If P7(I) = 0 Then
                            PR%(I, 0) = 1
                            PR%(I, 1) = 7
                            PC%(I) = 0
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
                    For F3 = 1 To 3:

                        Call d100(S)

                        'FT Success Chance
                        FX% = plyrOff_GAME!(P, B1(P, ballCarrier), 1)
                        If tmFatigue(P, B1(P, ballCarrier)) <= -5 Then FX% = Int(FX% * .9 + .5)

                        If S <= FX% Then
                            If pbpOpt = 0 Then
                                Call THREEFT
                            Else
                                'pbpOpt = 1
                                Call MADEFT
                            End If
                        Else
                            'S > FX%
                            If pbpOpt = 0 Then
                                Call BADFT
                            Else
                                Call MISSFT
                            End If
                        End If

                    Next F3

                    LT = 0

                    If gameClock! <= 0 Then
                        timeElapsed! = gameClock!
                        Call AdjustPlayTime
                        GoTo 170
                    End If

                    'We already have this exact logic
                    'worked out above !!!
                    If S > FX% Then
                        GoTo 650
                    Else
                        GoTo 598
                    End If

                End If

                If I2 = 43 And ft3PtFoul = 1 And S <= F3S% Then

                    gameClock! = gameClock! - 4
                    timeElapsed! = 4
                    Call AdjustPlayTime
                    Call AdjustShotClock
                    Call UPDATESCREEN
                    Call GoodBasket
                    pbpString$ = "3 PT BASKET GOOD BY " + players$(P, B1(P, ballCarrier))
                    Call SELECTPBP

                    Call GoodBasket

                    pbpString$ = "AND HE WAS FOULED !!!"
                    If pbpOpt = 0 Then Call PBP
                    If pbpOpt = 1 And pbpDelay! <> 0 Then Print pbpString$: Call DELAY

                    F3 = 1

                    score(P, currHalf) = score(P, currHalf) + 3
                    score(P, 0) = score(P, 0) + 3

                    plyrOff_GAME!(P, B1(P, ballCarrier), 8) = plyrOff_GAME!(P, B1(P, ballCarrier), 8) + 1
                    threeFG(P, B1(P, ballCarrier), 0) = threeFG(P, B1(P, ballCarrier), 0) + 1

                    Call UPDATESCREEN
                    Call FoulCalled(1)

                    GoTo 619

                End If

                If S > F3S% Then

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
                    Call GoodBasket
                    Call UPDATESCREEN

                    GoTo 562

                End If

            End If

    End Select

    '----- NOODLE -----

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
        If P7(I) = 0 Then
            PR%(I, 0) = 1
            PR%(I, 1) = 7
            PC%(I) = 0
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
    For F3 = 1 To 2

        'Increase FT ATT
        plyrOff_GAME!(P, B1(P, ballCarrier), 9) = plyrOff_GAME!(P, B1(P, ballCarrier), 9) + 1

        Call d100(S)

        'FT Success Chance
        FX% = plyrOff_GAME!(P, B1(P, ballCarrier), 1)
        If tmFatigue(P, B1(P, ballCarrier)) <= -5 Then FX% = Int(FX% * .9 + .5)

        If S <= FX% Then

            If pbpOpt = 0 Then
                Call THREEFT
            Else
                'pbpOpt = 1 Then
                Call MADEFT
            End If
        Else
            'S > FX% And
            If pbpOpt = 0 Then
                Call BADFT
            Else
                'pbpOpt = 1
                Call MISSFT
            End If

        End If

        If F3 = 1 And S > plyrOff_GAME!(P, B1(P, ballCarrier), 1) Then Exit For

    Next F3

    575 '
    If gameClock! <= 0 Then 170

    If S > FX% Then 650

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

    If P2(D) > 6 Then 570

    If gameClock! <= 0 Then 170
    pbpString$ = "OFFENSIVE FOUL"
    Call SELECTPBP

    pbpString$ = gameTeams$(P) + " BALL O.B. AFTER FOUL"
    If pbpOpt = 0 Then Call PBP
    If pbpOpt = 1 And pbpDelay! <> 0 Then Print pbpString$: Call DELAY

    Locate 8, 41: Print Space$(12)

    ballCarrier = 6
    Call DELAY

    turnovers(D) = turnovers(D) + 1

    Call UPDATESCREEN
    Call DEADBALLOPTIONS

    For I = 0 To 1
        If P7(I) = 0 Then
            PR%(I, 0) = 1
            PR%(I, 1) = 7
            PC%(I) = 0
        End If
    Next

    Call PRESSGUARD

    GoTo 300

    '----- NOODLE -----

    640 '
    prevBall$ = ""

    If timeouts(P) > 0 Then

        timeouts(P) = timeouts(P) - 1
        Call SCOREBOARD

        '--- RESUME ANALYSIS

        'Offense
        RN0 = Int(Rnd(1) * 100)
        If RN0 <= 8 Then
            ft3PtFoul(P) = 10
        Else
            If RN0 <= 16 Then ft3PtFoul(P) = 5
        End If

        'Defense
        RN1 = Int(Rnd(1) * 100)
        If RN1 <= 8 Then
            ft3PtFoul(D) = -10
        Else
            If RN1 <= 16 Then ft3PtFoul(D) = -5
        End If

        gameClock! = gameClock! + M5
        timeElapsed! = -M5
        Call AdjustPlayTime

        If sClockVal >= 1 Then shotClock = shotClock + M5

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

    650 '
    A6 = 1
    P6 = 0
    A4 = 0
    D6 = 0
    S = 0

    Call ResetShotClock

    Call d100(RN0)

    If (currHalf > 1 And gameClock! < 30 And score(D, 0) - score(P, 0) = 2 And RN0 <= 12) Or (currHalf > 1 And gameClock! < 30 And score(P, 0) - score(D, 0) = 0 And RN0 <= 12) Then
        Call OFFREBOUND
        GoTo 760
    End If

    Call d100(RN0)

    Select Case RN0

        Case 1 To 11
            FF% = 1
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
    P6 = 0
    A4 = 0
    D6 = 0

    Call ResetShotClock

    '--- loop
    700 '
    S = 0
    A4 = 0
    D6 = 0

    Call d100(RN0)
    GG = score(D, 0) - score(P, 0)

    If currHalf > 1 And gameClock! < 30 And (GG = 2 Or GG = 0 Or GG = -2) And RN0 <= 12 Then 700
    '--- loop

    If TP% = 1 And RN0 <= 19 Then TP% = 0

    Select Case RN0

        Case 0 To 1

            725 '
            If X = 98 Then 700
            turnovers(P) = turnovers(P) + 1
            pbpString$ = "OFFENSIVE GOAL TENDING"
            Call SELECTPBP

            pbpString$ = "BALL O.B. TO " + gameTeams$(D)
            If pbpOpt = 0 Then Call PBP
            If pbpOpt = 1 And pbpDelay! <> 0 Then Print pbpString$: Call DELAY

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
                If P7(I) = 0 Then
                    PR%(I, 0) = 1
                    PR%(I, 1) = 7
                    PC%(I) = 0
                End If
            Next

            GoTo 300

        Case 2 To DY
            FF% = 1

        Case DY + 1 To 17

            Call d100(RN0)

            If RN0 > 35 Then

                gameR3!(D) = gameR3!(D) + 1
                Call UPDATESCREEN
                pbpString$ = "RB KNOCKED O.B. TO " + gameTeams$(D)
                Call SELECTPBP
                Call DELAY
                Call DEADBALLOPTIONS

                For I = 0 To 1
                    If P7(I) = 0 Then
                        PR%(I, 0) = 1
                        PR%(I, 1) = 7
                        PC%(I) = 0
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
                gameR3!(P) = gameR3!(P) + 1: Call UPDATESCREEN
                pbpString$ = "RB KNOCKED O.B. TO " + gameTeams$(P)
                Call SELECTPBP
                Call DELAY
                Call DEADBALLOPTIONS

                For I = 0 To 1
                    If P7(I) = 0 Then
                        PR%(I, 0) = 1
                        PR%(I, 1) = 7
                        PC%(I) = 0
                    End If
                Next

                LT = 4

                GoTo 520

            End If

        Case 18 To 19

            If X = 99 Then 700

            Call OFFREBOUND

            plyrOff_GAME!(P, B1(P, ballCarrier), 7) = plyrOff_GAME!(P, B1(P, ballCarrier), 7) + 1
            plyrOff_GAME!(P, B1(P, ballCarrier), 8) = plyrOff_GAME!(P, B1(P, ballCarrier), 8) + 1
            plyrOff_GAME!(P, B1(P, ballCarrier), 11) = plyrOff_GAME!(P, B1(P, ballCarrier), 11) + 1
            gameR3!(P) = gameR3!(P) + 1
            score(P, currHalf) = score(P, currHalf) + 2
            score(P, 0) = score(P, 0) + 2
            Call UPDATESCREEN

            If pbpOpt = 1 And pbpDelay! <> 0 Then Call WINDEX: Print "TIPPED IN BY "; players$(P, B1(P, ballCarrier)): Print "   BASKET GOOD !!!": Call DELAY
            If pbpOpt = 0 Then pbpString$ = players$(P, B1(P, ballCarrier)) + " TIPS IT IN!!": Call PBP

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

            If pbpOpt = 0 Then Call PBP
            If pbpOpt = 1 And pbpDelay! <> 0 Then Print pbpString$: Call DELAY

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
                Call GoodBasket
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
            If plyrOff_GAME!(P, B1(P, I), 3) - plyrOff_GAME!(P, B1(P, I), 7) - plyrOff_GAME!(P, B1(P, I), 11) - plyrOff_GAME!(P, B1(P, I), 12) <= 0 Then XZ = Int(XZ * .5 + .5)
        End If

        If tmFatigue(P, B1(P, I)) <= -5 Then XZ = Int(XZ * .9 + .5)

        RB(I) = RB(I) + XZ: RB = RB + XZ
    Next

    ORB = RB

    For I = 0 To 4

        XZ = plyrOff_GAME!(1 - P, B1(1 - P, I), 2)

        If Not (currHalf > 2 Or currHalf = 2 And gameClock! <= 120) Then
            If plyrOff_GAME!(1 - P, B1(1 - P, I), 3) - plyrOff_GAME!(1 - P, B1(1 - P, I), 7) - plyrOff_GAME!(1 - P, B1(1 - P, I), 11) - plyrOff_GAME!(1 - P, B1(1 - P, I), 12) <= 0 Then XZ = Int(XZ * .5 + .5)
        End If

        If tmFatigue(1 - P, B1(1 - P, I)) <= -5 Then XZ = Int(XZ * .9 + .5)
        RB(I + 5) = RB(I + 5) + XZ: RB = RB + XZ

    Next

    DRB = RB - ORB

    If defType(D) = 0 Then XQ = Int((RB * .06) + .5)
    If defType(D) = 1 Then XQ = Int((RB * .12) + .5)
    If defType(D) = 2 Then XQ = Int((RB * .1) + .5)
    If defType(D) = 3 Then XQ = Int((RB * .14) + .5)
    If CZ%(P) = 1 Then XQ = Int((RB * .17) + .5)

    rndReb = Rnd(1) * RB
    If rndReb <= ORB + XQ Then

        rndOReb = Rnd(1) * ORB
        For ballCarrier = 0 To 4
            If rndOReb <= RB(ballCarrier) Then 760
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
    If FF% = 1 Then

        FF% = 0

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

        gameR3!(D) = gameR3!(D) + 1
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
    gameR3!(D) = gameR3!(D) + 1
    Call UPDATESCREEN
    pbpString$ = players$(D, B1(D, ballCarrier)) + " HAS THE DEF RB"

    If pbpOpt = 0 And pbpDelay! <> 0 Then Call PBP
    If pbpOpt = 1 And pbpDelay! <> 0 Then Call WINDEX: Print pbpString$: Print "FOR " + gameTeams$(D): Call DELAY

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

    FB% = 1
    prevBall$ = players$(P, B1(P, ballCarrier))

    RN0 = Int(Rnd(1) * 130) + 1

    If RN0 = 15 Then FB% = 0: GoTo RBTIEUP

    Call d100(RN0)
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
    If FF% = 1 Then

        FF% = 0

        pbpString$ = players$(P, B1(P, ballCarrier)) + " HAS THE OFF RB"
        If pbpOpt = 0 Then Call PBP
        If pbpOpt = 1 And pbpDelay! <> 0 Then Call WINDEX: Print pbpString$: Print "FOR "; gameTeams$(P): Call DELAY

        pbpString$ = "HE'S FOULED !!!"
        If pbpOpt = 0 Then Call PBP
        If pbpOpt = 1 And pbpDelay! <> 0 Then Print pbpString$: Call DELAY

        plyrOff_GAME!(P, B1(P, ballCarrier), 11) = plyrOff_GAME!(P, B1(P, ballCarrier), 11) + 1
        plyrOff_GAME!(P, B1(P, ballCarrier), 24) = plyrOff_GAME!(P, B1(P, ballCarrier), 24) + 1
        gameR3!(P) = gameR3!(P) + 1

        Call UPDATESCREEN

        Call FoulCalled(1)
        LT = 4

        GoTo 9065

    End If

    plyrOff_GAME!(P, B1(P, ballCarrier), 24) = plyrOff_GAME!(P, B1(P, ballCarrier), 24) + 1
    plyrOff_GAME!(P, B1(P, ballCarrier), 11) = plyrOff_GAME!(P, B1(P, ballCarrier), 11) + 1
    gameR3!(P) = gameR3!(P) + 1
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
                If plyrOff_GAME!(P, B1(P, ballCarrier), 3) - plyrOff_GAME!(P, B1(P, ballCarrier), 7) - plyrOff_GAME!(P, B1(P, ballCarrier), 11) - plyrOff_GAME!(P, B1(P, ballCarrier), 12) <= 0 Then F! = Int(F! * .6 + .5)
            End If

            If RN1 <= 8 Then
                PZ% = 9
                P6 = 1
                GoTo 537
            Else
                GoTo 520
            End If

        End If

    End If

    '----- NOODLE -----

    TIEUP:
    If pbpOpt <> 1 Then

        Call d100(RN0)

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
    Call d100(RN0)
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

    pbpString$ = "POSSESSION ARROW TO " + gameTeams$(PA%)
    If pbpOpt = 0 Then Call PBP
    If pbpOpt = 1 And pbpDelay! <> 0 Then Print pbpString$: Call DELAY

    Call DELAY

    If PA% = D Then
        turnovers(P) = turnovers(P) + 1
        Call DEADBALLOPTIONS

        For I = 0 To 1
            If P7(I) = 0 Then
                PR%(I, 0) = 1
                PR%(I, 1) = 7
                PC%(I) = 0
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
        PA% = 1 - PA%

        GoTo 300

    Else

        PA% = 1 - PA%
        Call DEADBALLOPTIONS
        For I = 0 To 1
            If P7(I) = 0 Then
                PR%(I, 0) = 1
                PR%(I, 1) = 7
                PC%(I) = 0
            End If
        Next
        LT = 4

        GoTo 350

    End If

    '----- NOODLE -----

    9065

    If IN% = 1 Or (ft10thFoul = 1 And P2(D) >= 10) Then

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
            If P7(I) = 0 Then
                PR%(I, 0) = 1
                PR%(I, 1) = 7
                PC%(I) = 0
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
        For F3 = 1 To 2:

            'Increase FT ATT
            plyrOff_GAME!(P, B1(P, ballCarrier), 9) = plyrOff_GAME!(P, B1(P, ballCarrier), 9) + 1

            Call d100(S)

            'FT % Success Chance
            FX% = plyrOff_GAME!(P, B1(P, ballCarrier), 1)
            If tmFatigue(P, B1(P, ballCarrier)) <= -5 Then FX% = Int(FX% * .9 + .5)

            If S <= FX% Then

                If pbpOpt = 0 Then
                    Call THREEFT
                Else
                    'pbpOpt = 1
                    Call MADEFT
                End If
            Else
                'S > FX% And
                If pbpOpt = 0 Then
                    Call BADFT
                Else
                    'pbpOpt = 1
                    Call MISSFT
                End If

            End If

        Next F3

        If gameClock! <= 0 Then 170

        Call CLEARPBPBOX

        If S > FX% And IN% <> 1 Then 650

        If IN% = 1 Then
            PASS% = 1
            IN% = 0
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

        If P2(D) > 6 Then 570

        If gameClock! <= 0 Then 170

        Call DEADBALLOPTIONS

        For I = 0 To 1
            If P7(I) = 0 Then
                PR%(I, 0) = 1
                PR%(I, 1) = 7
                PC%(I) = 0
            End If
        Next

        If ft6FloorFouls = 0 And pbpOpt = 0 Or ft6FloorFouls = 0 And pbpDelay! = 0 Then
            Call WINDEX
            pbpString$ = gameTeams$(P) + " BALL O.B. AFTER FOUL": Call PBP: Locate 8, 41: Print Space$(12)
            GoTo 350
        End If

        If ft6FloorFouls = 0 And pbpOpt = 1 Then
            Call WINDEX
            Print gameTeams$(P) + " BALL O.B. AFTER FOUL": Call DELAY: Locate 8, 41: Print Space$(12)
            GoTo 350
        End If

        Call UPDATESCREEN

        pbpString$ = "1 FT: " + players$(P, B1(P, ballCarrier)) + Str$(plyrOff_GAME!(P, B1(P, ballCarrier), 1)) + " %"
        Call PBP

        X = 50

        Call DEADBALLOPTIONS

        For I = 0 To 1
            If P7(I) = 0 Then
                PR%(I, 0) = 1
                PR%(I, 1) = 7
                PC%(I) = 0
            End If
        Next

        X = 0

        pbpString$ = "1 FT: " + players$(P, B1(P, ballCarrier)) + Str$(plyrOff_GAME!(P, B1(P, ballCarrier), 1)) + " %"

        Call PBP

        F3 = 1

        'Increase FT ATT
        plyrOff_GAME!(P, B1(P, ballCarrier), 9) = plyrOff_GAME!(P, B1(P, ballCarrier), 9) + 1


        FX% = plyrOff_GAME!(P, B1(P, ballCarrier), 1)
        If tmFatigue(P, B1(P, ballCarrier)) <= -5 Then FX% = Int(FX% * .9 + .5)

        Call d100(S)

        If S <= FX% Then
            If pbpOpt = 0 Then
                Call THREEFT
            Else
                'pbpOpt = 1
                Call MADEFT
            End If
        Else
            'S > FX%
            If pbpOpt = 0 Then
                Call BADFT
            Else
                'pbpOpt = 1
                Call MISSFT
            End If
        End If

        LT = 0

        If gameClock! <= 0 Then
            timeElapsed! = gameClock!
            Call AdjustPlayTime
            GoTo 170
        End If

        If S > FX% Then 650

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

End Sub