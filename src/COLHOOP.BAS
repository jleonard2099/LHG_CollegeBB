Sub COLHOOP ()

    _Title "Courtside College Basketball v" + GAME_VERSION$ + " - HOOPS!"

    'Open "pbplog" For Output As #7

    Shared scheduleFile$

    Shared tickerStart

    Shared coachMode$(), foulShotOpt$(), location$()
    Shared MO$(), sclockOpt$(), yesNo$()

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
        Close 1
    End If

    Open "DEV.BAS" For Input As #1
    For I = 0 To 1
        Input #1, diskPaths$(I)
    Next
    Close 1

    If Not _FileExists("DEFAULT") Then
        Open "DEFAULT" For Output As #1
        For I = 1 To 11:
            Print #1, 0
        Next
        Close 1
    End If

    Open "DEFAULT" For Input As #1
    Input #1, U5
    Input #1, X3%
    Input #1, sClockVal
    Input #1, N
    Input #1, NF%
    Input #1, pbpDelay!
    Input #1, coachOpt
    Input #1, TB%
    Input #1, TF%
    Input #1, N7%
    Input #1, PB%
    Close 1

    If _FileExists("TICKER") Then

        Open "TICKER" For Input As #2

        For I = 1 To 32
            For I1 = 0 To 1
                For I2 = 1 To 14
                    Input #2, SX%(I, I1, I2)
                Next
            Next
        Next

        For I = 1 To 32
            For I1 = 0 To 1
                Input #2, SX$(I, I1)
            Next
        Next

        For I = 1 To 32:
            Input #2, ST%(I)
        Next

        Close 2

        TMT = 1
    Else
        TMT = 0
    End If

    'Load game in-progress, if there is one
    If _FileExists("GAME82") Then

        Open "GAME82" For Input As #2

        Input #2, scheduleFile$

        For I = 0 To 2:
            Input #2, AP%(I)
        Next

        Close 2

        Open diskPaths$(0) + scheduleFile$ For Random As #1 Len = 48

        For X = 0 To 18
            Field #1, X * 2 As X$, 2 As Q$(X + 1), 48 - 2 - 2 * X As X$
        Next

        Field #1, 38 As X$, 2 As Q$(22), 2 As Q$(23), 3 As Q$(20), 3 As Q$(21)

        Get #1, AP%(0) + AP%(2)

        For X = 0 To 18:
            NG%(X) = CVI(Q$(X + 1))
        Next

        For X = 0 To 1:
            YN$(X) = Q$(20 + X)
        Next

        VS% = CVI(Q$(22))
        HS% = CVI(Q$(23))

        Close 1

        AP = 1

    End If

    If _FileExists("TOURNEY") Then

        Open "TOURNEY" For Input As #2

        Input #2, tourneyFile$

        For I = 0 To 2:
            Input #2, AP%(I)
        Next

        Close 2

        'If start game = end game,
        'we're all done!
        If AP%(2) = AP%(1) Then
            Kill "TOURNEY"
            'System
            'End
            Exit Sub

        Else

            Open diskPaths$(1) + tourneyFile$ + ".TOU" As #1 Len = 90

            For X = 0 To 14
                Field #1, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
            Next

            Field #1, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
            Field #1, 64 As X$, 3 As Q$(20), 3 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
            Field #1, 84 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)

            Get #1, AP%(0) + AP%(2)

            VS% = CVI(Q$(16))
            HS% = CVI(Q$(17))
            VT$ = Q$(18)
            W$ = Q$(19)

            For X = 0 To 14:
                NG%(X) = CVI(Q$(X + 1))
            Next

            For X = 0 To 3:
                YN$(X) = Q$(20 + X)
            Next

            For X = 1 To 3:
                NG%(X + 14) = CVI(Q$(X + 23))
            Next

            Close 1

            AP = 1
            TOU = 1

        End If

    End If

    If AP = 1 Then Call AUTOSETUP

    'AUTOSETUP will reset TOU = 0
    'in many circumstances
    If AP <> 1 And TOU <> 1 Then

        ' USER SELECTION OF OPTIONS
        'N = 1 is neutral site
        N = 1

        Do
            Color 15, 0
            Cls
            Color 2

            Print ; Tab(29); "SELECT OPENING OPTIONS"
            Print
            Locate 2, 2: Color 14
            Print "(1) ";: Color 15: Print MO$(U5)
            Locate 3, 2: Color 14
            Print "(2) ";: Color 15: Print "VISITING TEAM: ";: Color TR%(0, 0), TR%(0, 1): Print B$(0)
            Color , 0
            Locate 4, 2: Color 14
            Print "(3) ";: Color 15: Print "HOME TEAM: ";: Color TR%(1, 0), TR%(1, 1): Print B$(1)
            Color , 0
            Locate 5, 2: Color 14
            Print "(4) ";: Color 15: Print "3-POINT SHOT: "; yesNo$(X3%)
            Locate 6, 2: Color 14
            Print "(5) ";: Color 15: Print sclockOpt$(sClockVal)
            Locate 7, 2: Color 14
            Print "(6) ";: Color 15: Print "LOCATION: "; location$(N)
            Locate 8, 2: Color 14
            Print "(7) ";: Color 15: Print Using "DISPLAY PAUSE IN SECONDS: ##.#"; pbpDelay!
            Locate 9, 2: Color 14
            Print "(8) ";: Color 15: Print "START GAME"
            Locate 10, 2: Color 14
            Print "(9) ";: Color 15: Print "FOULS TO DISQUALIFY: "; NF%
            Locate 11, 2: Color 14
            Print "(0) ";: Color 15: Print "SAVE SETTINGS"
            Locate 12, 2: Color 14
            Print "(A) ";: Color 15: Print "FREE THROWS ON 10TH FOUL: "; foulShotOpt$(TB%)
            Locate 13, 2: Color 14
            Print "(B) ";: Color 15: Print "HUMAN COACHES WILL: "; coachMode$(coachOpt);
            Locate 1, 1
            Locate 14, 2: Color 14
            Print "(C) ";: Color 15: Print "3 FT WHEN FOULED ON 3-PT ATT: "; yesNo$(TF%)
            Locate 15, 2: Color 14
            Print "(D) ";: Color 15: Print "1 FT ON 1ST 6 FLOOR FOULS : "; yesNo$(N7%)
            Locate 16, 2: Color 14
            Print "(E) ";: Color 15: Print "PLAY BY PLAY: "; PB$(PB%)

            Locate 17, 2: Print "NOTE:: 1 FT ON 1ST 6 FLOOR FOULS THROUGH 1972-73 SEASON"
            Locate , 2: Print "       45 SEC SHOT CLOCK WAS ADOPTED IN 1984-85, BUT NOT USED IN"
            Locate , 2: Print "       84-85 POST SEASON TOURNAMENTS"
            Locate , 2: Print "       3 PT SHOT WAS ADOPTED IN 1986-87"
            Locate , 2: Print "       2 FT INSTEAD OF 1+1 AFTER 10TH FOUL ADOPTED IN 1990-91"
            Locate , 2: Print "       3 FT WHEN FOULED ON A 3 PT ATT ADOPTED IN 1990-91"
            Locate , 2: Print "       35 SEC SHOT CLOCK WAS ADOPTED IN 1993-94"

            'Start opening options with Locate 3, 2:
            'Options wil go up to Locate 17, 2 before the below:

            'Locate 19, 2: Print "NOTE::"
            'Locate , 2: Print "       1 FT ON 1ST 6 FLOOR FOULS THROUGH 1972-73 SEASON"
            'Locate , 2: Print "       45 SEC SHOT CLOCK ADOPTED 1984-85, BUT NOT FOR POST-SEASON"
            'Locate , 2: Print "       3 PT SHOT ADOPTED IN 1986-87"
            'Locate , 2: Print "       2 FT INSTEAD OF 1+1 AFTER 10TH FOUL ADOPTED 1990-91"
            'Locate , 2: Print "       3 FT WHEN FOULED ON A 3 PT ATT ADOPTED 1990-91"
            'Locate , 2: Print "       35 SEC SHOT CLOCK IN 1993-94; 30 SEC SHOT CLOCK IN 2015-16"

            Do
                Color 7

                I$ = GetKeyPress$

                If I$ <> Chr$(27) Then
                    iVal = Val(I$)
                    If UCase$(I$) = "A" Then iVal = 11
                    If UCase$(I$) = "B" Then iVal = 12
                    If UCase$(I$) = "C" Then iVal = 13
                    If UCase$(I$) = "D" Then iVal = 14
                    If UCase$(I$) = "E" Then iVal = 15
                    If iVal = 0 Then iVal = 10
                End If

            Loop Until iVal >= 0 And iVal <= 15 Or I$ = Chr$(27) Or UCase$(I$) = "Q"

            If I$ <> Chr$(27) And UCase$(I$) <> "Q" Then

                Select Case iVal

                    Case 1:
                        U5 = U5 + 1
                        If U5 > 3 Then U5 = 0

                    Case 2:
                        P9 = 0
                        Call SELECTID

                    Case 3:
                        P9 = 1
                        Call SELECTID

                    Case 4:
                        X3% = 1 - X3%

                    Case 5:
                        sClockVal = sClockVal + 1
                        '-- for 30 second shot clock option
                        '-- If sClockVal > 3 Then sClockVal = 0
                        If sClockVal > 2 Then sClockVal = 0

                    Case 6:
                        N = N + 1
                        If N > 2 Then N = 1

                    Case 7:
                        Do
                            Locate 8, 32
                            Input pbpDelay!
                        Loop Until pbpDelay! >= 0

                    Case 8:
                        If B$(0) <> "" And B$(1) <> "" Then Exit Do

                    Case 9:
                        NF% = NF% + 1
                        If NF% > 6 Then NF% = 5

                    Case 10:
                        Open "DEFAULT" For Output As #1
                        Print #1, U5
                        Print #1, X3%
                        Print #1, sClockVal
                        Print #1, N
                        Print #1, NF%
                        Print #1, pbpDelay!
                        Print #1, coachOpt
                        Print #1, TB%
                        Print #1, TF%
                        Print #1, N7%
                        Print #1, PB%
                        Close 1

                    Case 11:
                        TB% = 1 - TB%

                    Case 12:
                        coachOpt = 1 - coachOpt

                    Case 13:
                        TF% = 1 - TF%

                    Case 14:
                        N7% = 1 - N7%

                    Case 15:
                        PB% = 1 - PB%

                End Select

            End If

        Loop Until (iVal = 8 And B$(0) <> "" And B$(1) <> "") Or I$ = Chr$(27)

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
                If L = 5 Then HR = Val(Left$(startTime$, 1)): MIN = Val(Mid$(startTime$, 3, 2))
                If L = 6 Then HR = Val(Left$(startTime$, 2)): MIN = Val(Mid$(startTime$, 4, 2))
            End If

            tickerMeridian$ = Right$(startTime$, 1)

            If UCase$(tickerMeridian$) = "P" And Left$(startTime$, 2) <> "12" Then HR = HR + 12

            TM = HR * 60 + MIN
            tickerStart = Int(TM / 30) - 18

            Call SETUPRATINGS

        Else

            'End
            'System
            Exit Sub

        End If

    End If

    FY%(2) = Int(((FY%(1) + FY%(0)) / 2) + .5)

    For I = 0 To 1
        For I1 = 0 To 13
            TM%(I, I1) = 1200
        Next
    Next

    'Determine attendance
    ' N = 2 is a non-neutral site
    If N = 2 Or avgAttendance&(1) = 0 Then
        actualAttendance& = Int(Rnd(1) * 2000) + 9000
    Else
        actualAttendance& = Int(Rnd(1) * 2000) + avgAttendance&(1) - 1000
    End If

    '----------------------------------------
    '    AUTO-DETERMINE OTHER OPTIONS
    '----------------------------------------

    ' Determine computer team
    If U5 = 1 Then
        CT = 0
    Else
        If U5 = 2 Then
            U5 = 1
            CT = 1
        Else
            If U5 = 3 Then U5 = 2
        End If
    End If

    Call LOADSTATS_Game

    If AP = 1 Then N = NG%(2)

    Select Case sClockVal
        Case 1: shotClock = 45
        Case 2: shotClock = 35
        Case 3: shotClock = 30
    End Select

    S9 = (S1(0) + S1(1)) / 2

    For P9 = 0 To 1

        If U5 = 1 And P9 = CT Or U5 = 2 Then
            Call SETSTAMINA
            Call COMCOACH
        Else

            Call SETSTAMINA
            Call LINEUPS
            Call FOULSAFE
        End If

        If X3% <> 0 Then

            For I = 0 To 13

                M8 = 100 - A!(P9, I, 19)

                If M8 <> 0 Then
                    M9 = 100 / M8
                End If

                A!(P9, I, 16) = Int((A!(P9, I, 16) * M9) + .5)

            Next I

        End If

    Next P9

    Call CheckZeroTime
    Call SCOREBOARD
    Call STEALADJ

    250 P = Int(Rnd(1) * 2)
    'P has value of 0 or 1
    If P = 0 Then
        D = 1
    Else
        'P = 1
        D = 0
    End If

    PB$ = ""
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

    Call LASTSHOT
    Call CheckZeroTime
    If gameClock! = 0 Then 170

    Call ResetShotClock

    If pbpDelay! <> 0 Then

        If (H = 1 Or H = 3) And PB% = 0 Then U$ = B$(P) + " GETS THE JUMP": PA% = D: Call PBP
        If (H = 1 Or H = 3) And PB% = 1 Then Call WINDEX: Print B$(P) + " GETS THE JUMP": PA% = D: Call DELAY

        If (H <> 1 And H <> 3 Or H > 3) And PB% = 0 Then P = PA%: D = 1 - P: PA% = 1 - PA%: U$ = B$(P) + " INBOUNDS THE BALL": Call PBP
        If (H <> 1 And H <> 3 Or H > 3) And PB% = 1 Then P = PA%: D = 1 - P: PA% = 1 - PA%: Call WINDEX: Print B$(P) + " INBOUNDS THE BALL": Call DELAY

    Else

        If (H = 1 Or H = 3) Then
            PA% = D
        Else
            P = PA%
            D = 1 - P
            PA% = 1 - PA%
        End If

    End If

    GoTo 300

    '----- NOODLE -----

    170 For I = 0 To 1
        DT%(I) = 0
        D2(I) = 0
        P7(I) = 0
    Next

    gameClock! = 0
    Call QUARTERTOTAL
    H = H + 1
    gameClock! = 1200
    XS = 1
    X = 0

    If H = 2 Then
        U$ = "END OF HALF"
        Select Case PB%
            Case 0:
                Color 7, 0
                Call WINDEX
                Print U$
            Case 1:
                If pbpDelay! > 0 Then Call WINDEX: Print U$
        End Select
    End If

    If H > 3 Then
        U$ = "END OF OVERTIME" + Str$(H - 3)
        Select Case PB%
            Case 0:
                Color 7, 0
                Print U$
            Case 1:
                If pbpDelay! > 0 Then Call WINDEX: Print U$
        End Select
    End If

    If H = 2 Then

        U$ = "HALFTIME!"

        Select Case PB%
            Case 0:
                Color 7, 0
                Call WINDEX
                Print U$
            Case 1:
                Call WINDEX
                Print U$
                Call DELAY
        End Select

        halfTime = 1
        Call BOXOPTIONS(0)
        Call TICKER
        halfTime = 0

        Call FOULHARD

    End If

    If H < 3 And U5 <> 0 Then
        If U5 = 1 Then P9 = CT: Call COMCOACH
        If U5 = 2 Then P9 = 0: Call COMCOACH: P9 = 1: Call COMCOACH
    End If

    If H < 3 Then
        P2(0) = 0
        P2(1) = 0

        Call CheckZeroTime
        Call SCOREBOARD

        Print
        B = -1

        Call DEADBALLOPTIONS

        gameClock! = 1200

        Call MAINSCREEN

        For I = 0 To 1
            For I1 = 0 To 13:
                TM%(I, I1) = 1200
                FA%(I, I1) = 0
            Next
        Next

        GoTo 250

    End If

    'Considered doing an ELSE here
    'Would have to be careful with
    'all involved conditionals to
    'make sure we reach end game
    Call CheckZeroTime
    Call SCOREBOARD

    If H = 3 Then
        U$ = "END OF REGULATION"
        Select Case PB%
            Case 0:
                Color 7, 0
                Call WINDEX
                Print U$
            Case 1:
                If pbpDelay! > 0 Then Call WINDEX: Print U$
        End Select
    End If

    Locate 2, 31
    Print "END OF REGULATION"

    'If scores are tied, it's Overtime!
    If S!(0, 0) = S!(1, 0) Then

        U$ = "OVERTIME !!!"

        Select Case PB%
            Case 0:
                Color 7, 0
                Call WINDEX
                Print U$
            Case 1:
                Call WINDEX
                Print U$
                Call DELAY
        End Select

        190 '
        T1(0) = T1(0) + 1
        T1(1) = T1(1) + 1
        gameClock! = 300

        Call MAINSCREEN

        For I = 0 To 1
            For I1 = 0 To 13:
                TM%(I, I1) = 300
                FA%(I, I1) = 0
            Next
        Next

        B = -1

        Call DEADBALLOPTIONS
        Call SCOREBOARD

        GoTo 250

    End If

    If Not (AP = 1 And U5 = 2) Then
        I$ = GetKeyPress
    End If

    '--Start "EndOfGame" routine
    Call POSTGAME

    ' Prompt to play another game
    Cls

    If AP <> 1 Then

        'Using Home Team's color for the box
        Call TEAMBOX(1, 1)

        Do
            U$ = "PLAY ANOTHER GAME (YN)"
            Locate 7, (80 - Len(U$)) / 2
            Print U$

            Color 15, 0
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    End If

    If UCase$(I$) = "Y" Or (AP = 1 And endAllGames <> 1) Then
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
    Call ZeroCheck

    GoTo 170

    '----- NOODLE -----

    300 '

    If gameClock! <= 5 And T1(D) <= 0 And S!(P, 0) > S!(D, 0) Then
        timeElapsed! = gameClock!
        Call AdjustPlayTime
        gameClock! = 0
        Call ZeroCheck

        Call WINDEX
        Print B$(P); " RUNS THE CLOCK OUT!"
        Call DELAY
        Call DELAY

        GoTo 170

    End If

    If gameClock! <= 5 And (X3% = 1 And S!(D, 0) - S!(P, 0) <= 3 Or X3% = 0 And S!(D, 0) - S!(P, 0) <= 2) And S!(D, 0) - S!(P, 0) >= 0 And H >= 2 Then

        'Last 5 seconds!!!
        If U5 = 2 Or U5 = 1 And P = CT Then
            ' Computer handle last 5 seconds
            RN = Int(Rnd(1) * 100) + 1
            R0! = S!(P, 0) - S!(D, 0)
            If gameClock! >= 3 And gameClock! <= 5 And X3% = 0 And R0! < 3 Then I1% = 5
            If gameClock! >= 3 And gameClock! <= 5 And X3% = 1 And R0! = 3 Then I1% = 4
            If gameClock! >= 3 And gameClock! <= 5 And X3% = 1 And R0! = 2 And RN <= 50 Then I1% = 5
            If gameClock! >= 3 And gameClock! <= 5 And X3% = 1 And R0! = 2 And RN > 50 Then I1% = 4
            If gameClock! < 3 And X3% = 0 And R0! < 3 Then I1% = 0
            If gameClock! < 3 And X3% = 1 And R0! = 3 Then I1% = 1
            If gameClock! < 3 And X3% = 1 And R0! = 2 And RN <= 50 Then I1% = 0
            If gameClock! < 3 And X3% = 1 And R0! = 2 And RN > 50 Then I1% = 1

        Else

            Call LASTFIVEMENU

            280 I$ = GetKeyPress$
            I1% = Val(I$): If I1% < 0 Or I1% > 5 Then 280
            If I1% = 3 And T1(P9) <= 0 Then 280
            If (I1% = 1 Or I1% = 4) And X3% = 0 Then 280

            Color 15, 0
            Call SCOREBOARD

        End If

        281 '
        B = Int(Rnd(1) * 5)

        RN = Int(Rnd(1) * 100) + 1

        If I1% = 0 And RN <= 50 Or I1% = 1 And RN <= 33 Then
            U$ = "LONG PASS DOWN COURT"
            Call PBP
            Call DELAY
            U$ = "IT'S INTERCEPTED...GAME OVER!!"
            Call PBP
            GoTo TIMEEXP
        End If

        If I1% = 2 Then
            I3% = Int(Rnd(1) * 45) + 45
            U$ = AA$(P, B1(P, B)) + " HAS THE BALL"
            Call PBP
            U$ = "HE PUTS IT UP FROM" + Str$(I3%) + " FEET"
            Call PBP
            Call DELAY

            If RN = 61 Then

                U$ = "GOOD!!"
                Call PBP

                S!(P, H) = S!(P, H) + 2
                S!(P, 0) = S!(P, 0) + 2

                Call GoodBasket

                A!(P, B1(P, B), 8) = A!(P, B1(P, B), 8) + 1
                A!(P, B1(P, B), 7) = A!(P, B1(P, B), 7) + 1

                If X3% <> 0 Then
                    S!(P, H) = S!(P, H) + 1: S!(P, 0) = S!(P, 0) + 1
                    W%(P, B1(P, B), 1) = W%(P, B1(P, B), 1) + 1
                    W%(P, B1(P, B), 0) = W%(P, B1(P, B), 0) + 1
                End If

                Call MAINSCREEN

                GoTo TIMEEXP

            End If

        End If

        If I1% = 2 Then
            U$ = "SHOT MISSES...GAME OVER"
            Call PBP

            A!(P, B1(P, B), 7) = A!(P, B1(P, B), 7) + 1
            If X3% <> 0 Then
                W%(P, B1(P, B), 1) = W%(P, B1(P, B), 1) + 1
            End If

            GoTo TIMEEXP
        End If

        If I1% >= 3 And RN <= 8 Then
            U$ = "BALL LOST O.B.  GAME OVER"
            Call PBP
            GoTo TIMEEXP
        End If

        If I1% >= 3 And RN > 8 And RN <= 18 Then
            U$ = "PASS INTERCEPTED"
            Call PBP
            GoTo TIMEEXP
        End If

        If I1% = 3 And gameClock! < 1.5 Or I1% >= 4 And gameClock! < 3 Then
            U$ = "INBOUND PASS TO " + AA$(P, B1(P, B))
            Call PBP

            U$ = "TIME EXPIRES"
            Call PBP

            GoTo TIMEEXP
        End If

        If I1% = 3 Then

            timeElapsed! = (Rnd(1) + 1.5)
            gameClock! = gameClock! - timeElapsed!

            Call AdjustPlayTime
            Call AdjustShotClock

            U$ = B$(P) + " GETS THE BALL TO HALF COURT"
            Call PBP

            U$ = "THEY QUICKLY CALL FOR A TIMEOUT"
            Call PBP

            T1(P) = T1(P) - 1

            Call DEADBALLOPTIONS
            Call SCOREBOARD
            Call HalfCourt

        End If

        If I1% = 0 Or I1% = 4 Or (I1% = 3 And I2% = 1) Then

            ' TAKE3
            RN = Int(Rnd(1) * 100) + 1
            If gameClock! <= .5 Then XX% = 2
            If gameClock! > .5 And gameClock! <= 1.8 Then XX% = 12
            If gameClock! > 1.8 Then XX% = 20

            If RN > XX% Then
                U$ = AA$(P, B1(P, B)) + " MISSES"
                Call PBP
                A!(P, B1(P, B), 7) = A!(P, B1(P, B), 7) + 1
                W%(P, B1(P, B), 1) = W%(P, B1(P, B), 1) + 1
            Else
                U$ = "BASKET BY " + AA$(P, B1(P, B)) + "!!"
                Call PBP
                A!(P, B1(P, B), 7) = A!(P, B1(P, B), 7) + 1
                A!(P, B1(P, B), 8) = A!(P, B1(P, B), 8) + 1
                S!(P, H) = S!(P, H) + 3
                S!(P, 0) = S!(P, 0) + 3
                W%(P, B1(P, B), 1) = W%(P, B1(P, B), 1) + 1
                W%(P, B1(P, B), 0) = W%(P, B1(P, B), 0) + 1
                Call GoodBasket
                Call MAINSCREEN
            End If

            GoTo TIMEEXP

        End If

        RN = Int(Rnd(1) * 100) + 1
        If gameClock! <= .5 Then XX% = 3
        If gameClock! > .5 And gameClock! <= 1.8 Then XX% = 16
        If gameClock! > 1.8 Then XX% = 29

        If RN > XX% Then
            U$ = AA$(P, B1(P, B)) + " MISSES"
            Call PBP
            A!(P, B1(P, B), 7) = A!(P, B1(P, B), 7) + 1
        Else
            U$ = "BASKET BY " + AA$(P, B1(P, B)) + "!!"
            Call PBP
            A!(P, B1(P, B), 7) = A!(P, B1(P, B), 7) + 1
            A!(P, B1(P, B), 8) = A!(P, B1(P, B), 8) + 1
            S!(P, H) = S!(P, H) + 2
            S!(P, 0) = S!(P, 0) + 2
            Call GoodBasket
            Call MAINSCREEN
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
    If (D2(D) = 1 Or D2(D) = 3) And P7(D) = 0 Then X0% = 50: X1% = -1: Call PRDFAT
    If D2(D) = 2 And P7(D) = 0 Then X0% = 100: X1% = 1: Call PRDFAT: Call PRPFAT
    If D2(D) = 1 And P7(D) = 0 Then X0% = 25: X1% = -1: Call PRPFAT

    GoTo 400

    'End If

    '----- NOODLE -----

    350 PASS% = PASS% + 1
    P6 = 0
    A4 = 0
    D6 = 0
    A6 = 1
    F! = 0
    X = 0

    If (D2(D) = 1 Or D2(D) = 3) And PASS% = 2 And P7(D) > 0 Then X0% = 50: X1% = -1: Call PRDFAT
    If D2(D) = 2 And P7(D) > 0 And PASS% = 2 Then X0% = 100: X1% = 1: Call PRDFAT: Call PRPFAT
    If D2(D) = 1 And P7(D) > 0 And PASS% = 2 Then X0% = 25: X1% = -1: Call PRPFAT

    GoTo 400

    '----- NOODLE -----

    400 '
    A4 = 0
    D6 = 0
    P3 = 0
    B = Int(Rnd(1) * 5)
    Call BALLDISTRIBUTE

    If P7(D) > 0 And PASS% = 1 Then

        ' *** FULL COURT PRESS ***

        'Yes I see it odd that Case 1, 2 and 4
        'all have identical outcome.
        'That is the code as it was written.
        'I wonder if 2,4 should be a value
        'between 25 and 50.

        Select Case P7(D)
            Case 1:
                X0% = 50: X1% = -1
            Case 2, 4:
                X0% = 50: X1% = -1
            Case 3:
                X0% = 25: X1% = -1
        End Select

        Call PRDFAT

        If H >= 2 And gameClock! <= 180 And P7(D) = 5 Then

            'DENIAL
            If Int(Rnd(1) * 100) + 1 <= 10 Then

                gameClock! = gameClock! - 4
                timeElapsed! = 4
                Call AdjustPlayTime
                Call AdjustShotClock
                Call WHOLAY

                If PASS% <> 1 Then U$ = AA$(P, B1(P, B)) + " HAS THE BALL"
                If PASS% = 1 Then U$ = AA$(P, B1(P, B)) + " GETS THE INBOUND PASS"
                Call PBP
                U$ = "HE IS STREAKING DOWN THE FLOOR!"
                Call PBP

                LAYUP:
                FB% = 1
                F! = 98
                P6 = 1

                GoTo 520

            End If

            RN = Int(Rnd(1) * 100) + 1
            R0! = Int(Rnd(1) * 100) + 1
            IN = 99
            C1 = 2: P9 = D
            U$ = "DESIGNATED FOULER (0-4)"
            Call SELECTPBP

            skipInbound% = 0

            If U5 = 1 And D = CT Or U5 = 2 Then
                DF% = Int(Rnd(1) * 5)
            Else
                Do
                    I$ = GetKeyPress$
                    DF% = Val(I$)
                Loop Until DF% >= 0 And DF% <= 4

                If PASS% > 1 Then skipInbound% = 1

            End If

            If skipInbound% = 0 Then

                If RN <= 4 And T1(P) > 0 Then
                    U$ = B$(P) + " CAN'T INBOUND THE BALL": Call SELECTPBP: U$ = B$(P) + " CALLS A TIME OUT"
                    If PB% = 0 Then Call PBP
                    If PB% = 1 And pbpDelay! <> 0 Then Print U$: Call DELAY
                    PASS% = 0
                    GoTo 640
                End If

                If RN <= 4 And T1(P) = 0 Or RN > 4 And RN <= 7 Then
                    U$ = B$(P) + " CAN'T INBOUND THE BALL"
                    Call SELECTPBP
                    U$ = "5 SECOND VIOLATION...TURNOVER"
                    SV% = 1
                    GoTo 517
                End If

                CY = 7 + DP(0, 4)
                If RN <= CY Then
                    Select Case R0!
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

                CY = CY + F5%(P, 4) + DP(1, 4)
                If RN <= CY Then

                    Select Case R0!
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
                U$ = "PASS TO " + AA$(P, B1(P, B))
                Call SELECTPBP
            End If

            434 If Int(Rnd(1) * 100) + 1 <= 5 Then IN% = 1

            RN = Int(Rnd(1) * 100) + 1
            Select Case RN
                Case 1 To 80: timeElapsed! = Int(Rnd(1) * 3) + 1
                Case 81 To 90: timeElapsed! = Int(Rnd(1) * 3) + 8
                Case 91 To 100: timeElapsed! = Int(Rnd(1) * 3) + 5
            End Select
            gameClock! = gameClock! - timeElapsed!
            Call AdjustPlayTime
            Call AdjustShotClock

            RN = Int(Rnd(1) * 100) + 1
            If RN <= 80 Then
                W5 = DF%
                Call FoulCalled(0)
            Else
                Call FoulCalled(1)
            End If

            GoTo 9065

        End If

        If P7(D) = 1 Then X0% = 25: X1% = -1: FA%(P, B1(P, BU%)) = FA%(P, B1(P, BU%)) - 1: Call PRPFAT
        If P7(D) > 1 Then X0% = 25: X1% = -1: Call PRPFAT

        R0! = Int(Rnd(1) * 100) + 1
        If P7(D) = 1 Or P7(D) = 3 Then gameClock! = gameClock! - 2: timeElapsed! = 2
        If P7(D) = 2 And R0! <= 75 Then gameClock! = gameClock! - 2: timeElapsed! = 2
        If P7(D) = 2 And R0! > 75 Then gameClock! = gameClock! + 2: timeElapsed! = -2
        If P7(D) = 4 And R0! <= 50 Then gameClock! = gameClock! - 2: timeElapsed! = 2
        If P7(D) = 4 And R0! > 50 Then gameClock! = gameClock! + 2: timeElapsed! = -2

        Call AdjustPlayTime
        Call AdjustShotClock

        R0! = Int(Rnd(1) * 100) + 1

        If R0! <= DP(2, P7(D)) Then
            Call LASTSHOT
            Call ResetShotClock
            Call MAINSCREEN
            X = 88

            Call FoulCalled(1)
            LT = 4

            GoTo 9065
        End If

        'We have some near identical behavior below,
        'and these are some of the only places these
        'code branches "GoTo". But I don't think it
        'can be condensed at all.
        If R0! <= DP(2, P7(D)) + 1 Then X = 0: GoTo 580

        CY = DP(2, P7(D)) + 1 + F5%(P, 5) + F5%(D, 6) + DP(0, P7(D)) - PR%(D, 0)
        If R0! <= CY Then Call LASTSHOT: GoTo 517

        CY = CY + F5%(P, 4) + DP(1, P7(D)) - PR%(D, 1)
        PC%(D) = PC%(D) + 1
        If PC%(D) > 3 Then PR%(D, 0) = 0: PR%(D, 1) = 0

        If R0! <= CY Then 435

    Else

        R0! = Int(Rnd(1) * 100) + 1

        If (OF%(P) <> 8 And OF%(P) <> 7) Or D2(D) <> 2 Then

            If R0! <= D8(4, D2(D)) Then
                Call LASTSHOT
                Call ResetShotClock
                Call MAINSCREEN
                X = 88

                Call FoulCalled(1)
                LT = 4

                GoTo 9065
            End If

            If R0! <= D8(4, D2(D)) + 1 Then X = 0: GoTo 580

            OY% = 0
            If OF%(P) = 4 Or OF%(P) = 7 Then OY% = Int((D8(2, D2(D)) / 2) + .5) * -1

            CY = D8(4, D2(D)) + 1 + F5%(P, 5) + F5%(D, 6) + D8(2, D2(D)) + OY% + 1
            If R0! <= CY Then Call LASTSHOT: GoTo 517

            CY = CY + F5%(P, 4) + D8(3, D2(D))
            OY% = 0
            If OF%(P) = 4 Or OF%(P) = 7 Then OY% = Int((D8(3, D2(D)) / 2) + .5) * -1
            If OF%(P) = 1 Then OY% = 7
            If OF%(P) = 2 Then OY% = -7

            If R0! <= CY + OY% Then 435

        End If

    End If

    '-----

    465 R0! = Int(Rnd(1) * 100) + 1

    '--- What to do for 30 second shot clock?
    '   What is GS%?
    '   What is this doing?
    'If sClockVal = 3 Then GS% = 39 + Int(((FY%(2) - 58) * .8) + .5)
    If sClockVal = 2 Then GS% = 39 + Int(((FY%(2) - 58) * .8) + .5)

    If sClockVal = 1 Then GS% = 39 + Int(((FY%(2) - 52) * .8) + .5)

    If sClockVal = 0 Then GS% = 39 + Int(((FY%(2) - 52) * .8) + .5)
    'The "no shot clock" was previously - 40 (not - 52)

    If D2(D) = 1 Or D2(D) = 2 Then GS% = GS% + 7
    If D2(D) = 3 Then GS% = GS% - 7
    If OF%(P) = 3 Then GS% = GS% + 21
    If OF%(P) = 4 Then GS% = GS% - 21

    If OF%(P) = 7 Or OF%(P) = 8 Then 437
    If OF%(P) = 5 And shotClock > 15 And sClockVal >= 1 Then 437

    If R0! > GS% Then
        GoTo 437
    Else

        R0! = Int(Rnd(1) * 100)
        If R0! <= D8(1, D2(D)) Then

            A4 = 0
            If Not (H = 2 And gameClock! <= 120 Or H > 2) Then
                If H <= 2 Then
                    If A!(P, B1(P, B), 3) - A!(P, B1(P, B), 7) - A!(P, B1(P, B), 11) - A!(P, B1(P, B), 12) <= 0 Then 437
                End If
            End If

            Call SHOTPERCENT

            A4 = Int(Rnd(1) * 28) + 1
            R0! = Int(Rnd(1) * 9) + 1
            I2 = Int(Rnd(1) * 5)

            If FA%(P, B1(P, B)) > -5 Then

                If R0! <= A!(P, B1(P, I2), 4) Then

                    F! = A!(P, B1(P, B), 0) + D1(D) + A4 + TF%(P) + TF%(D)
                    TF%(P) = 0
                    TF%(D) = 0
                    PZ% = 2
                    P6 = 1
                    X = 0

                    If PZ% = 2 And Int(Rnd(1) * 100) + 1 <= 50 Then
                        PZ% = 3
                        P6 = 1
                    End If

                    GoTo 520

                End If

            End If

        End If

    End If

    445 '
    If OF%(P) = 4 Then
        GoTo 437
    Else

        If H <= 2 Then

            If Not (H = 2 And gameClock! <= 120 Or H > 2) Then
                If A!(P, B1(P, B), 3) - A!(P, B1(P, B), 7) - A!(P, B1(P, B), 11) - A!(P, B1(P, B), 12) <= 0 Then 437
            End If

        End If

        P6 = 1
        Call SHOTPERCENT

        PZ% = Int(Rnd(1) * 3) + 5

        X = 0

        If PZ% = 2 And Int(Rnd(1) * 100) + 1 <= 50 Then
            PZ% = 3
            P6 = 1
        End If

        GoTo 520

    End If


    '----- NOODLE -----

    435 P9 = Int(Rnd(1) * 5)

    If Int(Rnd(1) * 5) + 1 <= A!(D, B1(D, P9), 5) Then

        A!(D, B1(D, P9), 18) = A!(D, B1(D, P9), 18) + 1

        513 '
        If P7(D) <= 0 Or PASS% <> 1 Then
            Call LASTSHOT
        End If

        Call ResetShotClock
        A!(P, B1(P, B), 23) = A!(P, B1(P, B), 23) + 1

        If pbpDelay! <> 0 Then

            U$ = "BALL STOLEN BY " + AA$(D, B1(D, P9)) + "!"

            If PB% = 1 Then
                Call WINDEX
                Print U$
                Call DELAY
            Else

                RN = Int(Rnd(1) * 100) + 1
                Select Case RN
                    Case 1 To 3
                        U$ = AA$(D, B1(D, P9)) + " QUICKLY REACHES IN!": Call PBP
                    Case 4 To 6
                        U$ = AA$(D, B1(D, P9)) + " SNEAKS IN FROM BEHIND!": Call PBP
                    Case 7 To 10
                        U$ = AA$(D, B1(D, P9)) + " GETS A HAND ON THE BALL!": Call PBP
                        'No case for 11 to 100
                End Select

                RN = Int(Rnd(1) * 20) + 1
                Select Case RN

                    Case 1
                        U$ = "PASS INTERCEPTED BY " + AA$(D, B1(D, P9)) + "!"
                    Case 2
                        U$ = "PASS IS PICKED OFF BY " + AA$(D, B1(D, P9)) + "!"
                    Case 3
                        U$ = "BALL IS STOLEN BY " + AA$(D, B1(D, P9)) + "!"
                    Case 4
                        U$ = AA$(D, B1(D, P9)) + " WITH THE STEAL!"
                    Case 5
                        U$ = AA$(D, B1(D, P9)) + " TAKES IT AWAY!"
                    Case 6
                        U$ = AA$(D, B1(D, P9)) + " STEALS IT!"
                    Case 7
                        U$ = AA$(D, B1(D, P9)) + " RIPS IT LOOSE!"
                    Case 8
                        U$ = AA$(D, B1(D, P9)) + " TAKES THE BALL AWAY!"
                    Case 9
                        U$ = AA$(D, B1(D, P9)) + " STEALS THE BALL!"
                    Case 10
                        U$ = AA$(D, B1(D, P9)) + " PICKS IT OFF!"
                    Case 11
                        U$ = AA$(D, B1(D, P9)) + " SWIPES THE BALL!"
                    Case 12
                        U$ = AA$(D, B1(D, P9)) + " WITH THE STOLEN BALL!"
                    Case 13
                        U$ = AA$(D, B1(D, P9)) + " STRIPS IT AWAY!"
                    Case 14
                        U$ = AA$(D, B1(D, P9)) + " WITH THE STEAL!"
                    Case 15
                        U$ = AA$(D, B1(D, P9)) + " SWIPES AND STEALS IT!"
                    Case 16
                        U$ = AA$(D, B1(D, P9)) + " SNATCHES IT AWAY!"
                    Case 17
                        U$ = AA$(D, B1(D, P9)) + " QUICKLY REACHES IN!"
                    Case 18
                        U$ = AA$(D, B1(D, P9)) + " TAKES IT THE OTHER WAY!"
                    Case 19
                        U$ = AA$(D, B1(D, P9)) + " POKES IT LOOSE!"
                    Case 20
                        U$ = AA$(D, B1(D, P9)) + " STRIPS IT CLEAN!"

                End Select

                Call PBP

            End If

        End If

        505 '
        A!(D, B1(D, P9), 13) = A!(D, B1(D, P9), 13) + 1
        T0(P) = T0(P) + 1

        Call MAINSCREEN

        C1 = 2
        P9 = D

        If gameClock! <= 0 Then 170

        PB$ = ""
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

        RN = Int(Rnd(1) * 100) + 1
        If RN <= 38 Then B = 0
        If RN > 38 And RN <= 75 Then B = 1
        If RN > 75 And RN <= 83 Then B = 2
        If RN > 83 And RN <= 91 Then B = 3
        If RN > 91 Then B = 4

        RN = Int(Rnd(1) * 100) + 1
        If PASS% > 1 And (D2(P9) = 0 Or D2(P9) = 1 Or D2(P9) = 3) And RN <= 15 Then GoTo LAYUP
        If PASS% > 1 And D2(P9) = 2 And RN <= 5 Then GoTo LAYUP
        If PASS% = 1 And P7(P) > 0 And RN <= 33 Then GoTo LAYUP
        If PASS% = 1 And P7(P) > 0 And P7(P) <> 3 And RN <= 40 Then GoTo LAYUP

        GoTo 300

    End If

    R0! = Int(Rnd(1) * 100) + 1

    If P7(D) = 1 And PASS% = 1 And R0! <= A!(P, B1(P, BU%), 4) Then GoTo LAYUP
    If P7(D) = 2 And PASS% = 1 And R0! <= A!(P, B1(P, BU%), 4) * 2 Then GoTo LAYUP
    If P7(D) = 3 And PASS% = 1 And R0! <= 7 Then GoTo LAYUP
    If P7(D) = 4 And PASS% = 1 And R0! <= 15 Then GoTo LAYUP

    If P7(D) = 1 And PASS% = 1 And R0! <= A!(P, B1(P, BU%), 4) * 2 Then GoTo GDSHOT
    If P7(D) = 2 And PASS% = 1 And R0! <= A!(P, B1(P, BU%), 4) * 4 Then GoTo GDSHOT
    If P7(D) = 3 And PASS% = 1 And R0! <= 14 Then GoTo GDSHOT
    If P7(D) = 4 And PASS% = 1 And R0! <= 30 Then GoTo GDSHOT
    If P7(D) = 5 Then 432

    ' --- I think this is by itself and the above are separate??? ---

    437 P6 = 0: R0! = Int(Rnd(1) * 3) + 1: J8% = 0: F3S% = Int(Rnd(1) * 7) + 3
    If A!(P, B1(P, B), 0) > 60 And R0! = 1 Then F! = 24
    If A!(P, B1(P, B), 0) > 60 And R0! = 2 Then F! = 30
    If A!(P, B1(P, B), 0) > 60 And R0! = 3 Then F! = 36
    If A!(P, B1(P, B), 0) <= 60 And R0! = 1 Then F! = Int(A!(P, B1(P, B), 0) * .4 + .5)
    If A!(P, B1(P, B), 0) <= 60 And R0! = 2 Then F! = Int(A!(P, B1(P, B), 0) * .5 + .5)
    If A!(P, B1(P, B), 0) <= 60 And R0! = 3 Then F! = Int(A!(P, B1(P, B), 0) * .6 + .5)
    PZ% = 1

    If Int(Rnd(1) * 150) + 1 = 40 Then
        GoTo TIEUP
    Else
        X = 0
        If PZ% = 2 And Int(Rnd(1) * 100) + 1 <= 50 Then PZ% = 3: P6 = 1
        GoTo 520
    End If

    '----- NOODLE -----

    517 Call ResetShotClock

    R0! = Int(Rnd(1) * 25) + 1

    If R0! = 13 Then

        GoTo TIEUP

    Else

        A!(P, B1(P, B), 23) = A!(P, B1(P, B), 23) + 1
        T0(P) = T0(P) + 1

        If SV% <> 1 Then PT% = 1: Call MAKEPBP

        SV% = 0

        If PB% = 1 And pbpDelay! <> 0 Then Call WINDEX: Print "TURNOVER BY "; B$(P): Call DELAY
        If PB% = 0 Then Call PBP

        U$ = "BALL OUT OF BOUNDS TO " + B$(D)
        If PB% = 0 Then Call PBP
        If PB% = 1 And pbpDelay! <> 0 Then Print U$: Call DELAY

        Call DELAY
        Call MAINSCREEN

        Locate 10, 1

        If gameClock! <= 0 Then 170

        PB$ = ""
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
            If P7(I) = 0 Then PR%(I, 0) = 1: PR%(I, 1) = 7: PC%(I) = 0
        Next

        GoTo 300

    End If

    '----- NOODLE -----

    GDSHOT:
    FX% = A!(P, B1(P, B), 0)

    If FA%(P, B1(P, B)) <= -5 Then FX% = FX% - 10

    F! = FX% + Int(Rnd(20)) + 11 + TF%(D) + TF%(P)
    TF%(P) = 0
    TF%(D) = 0
    PZ% = 10

    PZ% = Int(Rnd(1) * 3) + 5
    X = 0

    If PZ% = 2 And Int(Rnd(1) * 100) + 1 <= 50 Then PZ% = 3: P6 = 1

    520 '
    If J8% = 1 Then PZ% = 4

    Call LASTSHOT: If gameClock! <= 0 Then 170

    If PZ% = 4 And F3S% <= 0 Then F3S% = 3

    If pbpDelay! <> 0 Then

        If PZ% = 10 Then

            GDSHOTDES:
            If pbpDelay! <> 0 Then

                RN = Int(Rnd(1) * 100) + 1
                If RN <= 25 Then U$ = AA$(P, B1(P, B)) + " BREAKS IN TO FRONT COURT"
                If RN > 25 And RN <= 50 Then U$ = AA$(P, B1(P, B)) + " GETS THE PASS IN FRONT COURT"
                If RN > 50 And RN <= 75 Then U$ = AA$(P, B1(P, B)) + " BREAKS THE PRESS"
                If RN > 75 Then U$ = AA$(P, B1(P, B)) + " DEFEATS THE PRESS"
                Call PBP

                RN = Int(Rnd(1) * 100) + 1
                If RN <= 33 Then U$ = "HE HAS AN OPEN JUMPER AT THE FT LINE"
                If RN > 33 And RN <= 66 Then U$ = "HE'S OPEN ON THE BASELINE"
                If RN > 66 Then U$ = "HE'S OPEN FROM THE WING"
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

                If AA$(P, B1(P, B)) <> PB$ And PB$ = "" Then U$ = "PASS TO " + AA$(P, B1(P, B)): Call PBP
                If AA$(P, B1(P, B)) <> PB$ And PB$ <> "" Then PT% = 10: Call MAKEPBP: Call PBP
                If AA$(P, B1(P, B)) = PB$ Then PT% = 9: Call MAKEPBP: Call PBP

                If shotClock <= 0 And sClockVal > 0 Then
                    U$ = "SHOT CLOCK HAS EXPIRED!"
                    SV% = 1
                    Call LASTSHOT
                    GoTo 517
                End If

                PB$ = AA$(P, B1(P, B))
                YY$ = "HE"

                Select Case PZ%
                    Case 1:
                        If pbpDelay! <> 0 Then

                            RN = Int(Rnd(1) * 12) + 1
                            Select Case RN
                                Case 1
                                    U$ = YY$ + "'S HEAVILY GUARDED"
                                Case 2
                                    U$ = YY$ + "'S GUARDED"
                                Case 3
                                    U$ = YY$ + "'S GUARDED BY " + AA$(D, B1(D, B))
                                Case 4
                                    U$ = YY$ + " HAS A MAN ON HIM"
                                Case 5
                                    U$ = YY$ + " HAS A DEFENDER ON HIM"
                                Case 6
                                    U$ = YY$ + "'S COVERED"
                                Case 7
                                    U$ = YY$ + " CAN'T GET AROUND THE DEFENDER"
                                Case 8
                                    U$ = YY$ + "'S PRESSURED"
                                Case 9
                                    U$ = YY$ + " HAS NOWHERE TO GO"
                                Case 10
                                    U$ = YY$ + " HAS A HAND IN HIS FACE"
                                Case 11
                                    U$ = YY$ + "'S WATCHED BY " + AA$(D, B1(D, B))
                                Case 12
                                    U$ = YY$ + "'S WORKING ON " + AA$(D, B1(D, B))
                            End Select

                        End If

                        Call PBP

                    Case 2:
                        U$ = YY$ + " WORKS FREE INSIDE"
                        Call PBP
                        U$ = "HE HAS AN OPEN SHOT"
                        Call SHOTSTRING
                        Call PBP

                        P6 = 1

                    Case 3:
                        If pbpDelay! <> 0 Then

                            P6 = 1

                            RN = Int(Rnd(1) * 100) + 1
                            If B = 4 And RN <= 25 Then U$ = YY$ + " MAKES A MOVE DOWN LOW": Call PBP: GoTo 602
                            If B = 4 And RN <= 50 Then U$ = YY$ + " POSITIONS HIMSELF DOWN LOW": Call PBP: GoTo 602
                            If B = 4 And RN <= 75 Then U$ = YY$ + " POSTS UP UNDERNEATH": Call PBP: GoTo 602
                            If B = 4 Then U$ = YY$ + " BACKS IN AGAINST " + AA$(D, B1(D, 4)): Call PBP

                            602 If B = 4 Then U$ = "HE HAS A GOOD SHOT INSIDE": Call SHOTSTRING: Call PBP: GoTo 529
                            If (B = 3 Or B = 2) And RN <= 17 Then U$ = YY$ + " DRIVES THE BASELINE": Call PBP: GoTo 603
                            If (B = 3 Or B = 2) And RN <= 34 Then U$ = YY$ + " PENETRATES THE LANE": Call PBP: GoTo 603
                            If (B = 3 Or B = 2) And RN <= 50 Then U$ = YY$ + " MOVES INSIDE": Call PBP: GoTo 603
                            If (B = 3 Or B = 2) And RN <= 67 Then U$ = YY$ + " DRIVES THE LANE": Call PBP: GoTo 603
                            If (B = 3 Or B = 2) And RN <= 84 Then U$ = YY$ + " MOVES INTO THE PAINT": Call PBP: GoTo 603
                            If (B = 3 Or B = 2) Then U$ = YY$ + " DRIVES UNDERNEATH": Call PBP

                            603 If (B = 3 Or B = 2) Then U$ = "HE'S GOT A GOOD SHOT UNDERNEATH": Call SHOTSTRING: Call PBP: GoTo 529
                            If (B = 0 Or B = 1) And RN <= 25 Then U$ = YY$ + " DRIVES THE LANE": Call PBP: GoTo 604
                            If (B = 0 Or B = 1) And RN <= 50 Then U$ = YY$ + " DRIVES THROUGH THE KEY": Call PBP: U$ = "HE'S INTO THE LANE": Call PBP: GoTo 604
                            If (B = 0 Or B = 1) And RN <= 75 Then U$ = YY$ + " DRIVES TO THE BASKET": Call PBP: GoTo 604
                            U$ = YY$ + " DRIVES THROUGH AN OPENING": Call PBP

                            604 U$ = "HE HAS A GOOD SHOT CLOSE IN": Call SHOTSTRING: Call PBP
                        End If

                    Case 4:
                        U$ = "HE'S OPEN FROM 3-POINT RANGE!"
                        U$ = U$ + Str$(F3S%) + "*"
                        Call PBP

                        P6 = 1

                    Case 5:
                        U$ = "HE'S OPEN FROM MEDIUM RANGE"
                        Call SHOTSTRING
                        Call PBP

                        P6 = 1

                    Case 6:
                        U$ = "HE'S OPEN FROM MEDIUM RANGE"
                        Call SHOTSTRING
                        Call PBP

                        P6 = 1

                    Case 7:
                        U$ = "HE'S OPEN FROM MEDIUM RANGE"
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

    If pbpDelay! <> 0 Then

        If PB% = 1 Then Call WINDEX: Print "PASS TO "; AA$(P, B1(P, B)); " OF "; B$(P)

        If PB% = 1 And PZ% <> 4 Then Locate 10: Print "SHOT % "; F!;

        If PB% = 1 And PZ% = 4 Then
            Locate 10
            Print "3 PT SHOT % "; F3S%;: Print " *";
            skipShotPct% = 1
        Else
            If PB% = 1 And PZ% >= 2 And PZ% <= 8 Then Print "*";
        End If

    End If

    If skipShotPct% = 0 Then
        If pbpDelay! <> 0 And PB% <> 1 Then

            If PZ% <> 4 Then Locate 8, 41: Print Using "SHOT % ###   "; F!;

            If PZ% = 4 Then
                Locate 8, 41
                Print Using "SHOT % ###"; F3S%;: Print " *";
            Else
                If PZ% >= 2 And PZ% <= 8 Then Locate , 52: Print "*";
            End If
        End If

    End If

    528 '
    Print
    If PB% = 1 And pbpDelay! <> 0 Then Call DELAY

    If U5 <> 0 Or coachOpt = 1 Then
        '949
        If Not (H >= 2 And gameClock! <= 240 And S!(P, 0) - S!(D, 0) > 0 And U5 = 1 And P = CT) Then
            If U5 = 2 Or U5 = 1 And D = CT Then
                If (U5 = 1 Or U5 = 0) And coachOpt <> 1 Then
                    GoTo 530
                End If
            End If

        End If

        951 '
        If gameClock! <= 1 Or sClockVal >= 1 And shotClock <= 7 Then
            P6 = 1
        Else
            If gameClock! <= 8 And S!(P, 0) - S!(D, 0) <= -3 And H >= 2 Then
                P6 = 1
            Else
                If gameClock! <= 20 And S!(P, 0) - S!(D, 0) <= -5 And H >= 2 Then
                    P6 = 1
                Else

                    FX% = A!(P, B1(P, B), 0)

                    If FA%(P, B1(P, B)) <= -5 Then FX% = FX% - 10

                    If P6 <> 1 Then

                        If (U5 = 0 And coachOpt = 1 Or U5 = 1 And D = CT And coachOpt = 1 Or U5 = 1 And P = CT Or U5 = 2) Then

                            If gameClock! <= 660 And gameClock! >= 540 And H = 1 And T1(P) >= 5 Then
                                P6 = 2: CTO% = 1
                            Else

                                If gameClock! <= 240 And gameClock! >= 120 And H = 1 And T1(P) >= 4 Then

                                    P6 = 2: CTO% = 1

                                Else

                                    If gameClock! <= 840 And gameClock! >= 720 And H = 2 And T1(P) >= 3 Then
                                        P6 = 2: CTO% = 1
                                    Else

                                        If gameClock! <= 300 And gameClock! >= 180 And H = 2 And T1(P) >= 2 Then
                                            P6 = 2: CTO% = 1
                                        End If

                                    End If

                                End If

                            End If

                        End If

                    End If

                End If

            End If

        End If

        'GoTo 532

    Else

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

        P6 = Val(I$)

        If P6 < 1 Or P6 > 4 Then P6 = 0
        If P6 = 4 Then FT% = 1

    End If

    532 '
    If Val(I$) = 3 And (U5 = 0 Or U5 = 1 And P = CT) And H >= 4 And gameClock! < 240 Then P6 = 3
    If Val(I$) = 2 And (U5 = 0 Or U5 = 1 And D = CT) And T1(P) > 0 Then P6 = 2
    P3 = P6

    If P6 = 2 And T1(P) = 0 Or P6 = 3 And H = 1 Or P6 = 3 And U5 = 1 And D = CT Or P6 = 3 And H = 2 And gameClock! > 240 Then 530

    If U5 = 1 And P = CT And J8% = 1 Or U5 = 2 And J8% = 1 Then P6 = 4
    If U5 = 1 And P = CT And H >= 2 And gameClock! < 30 And S!(D, 0) - S!(P, 0) = 3 Or U5 = 2 And H >= 2 And gameClock! < 30 And S!(D, 0) - S!(P, 0) = 3 Then P6 = 4
    If (P6 = 1 Or P6 = 4) And PZ% = 4 Then P6 = 4

    caseVar% = P6 + 1

    Select Case caseVar%

        'On caseVar% GOTO 350, 537, 640,

        Case 1
            GoTo 350

        Case 2
            'WARNING - this is a long "routine"

            537
            ' *** SHOOT !!! ***
            PT% = 3
            Call MAKEPBP
            Call PBP

            S = 0
            SS = 0

            R0! = Int(Rnd(1) * 100) + 1

            A!(P, B1(P, B), 7) = A!(P, B1(P, B), 7) + 1
            GG = S!(D, 0) - S!(P, 0)

            If Not (H > 1 And gameClock! <= 30 And GG = 2 Or H > 1 And gameClock! <= 30 And GG = 0 Or H > 1 And gameClock! <= 30 And GG = -2) Then

                If R0! <= A!(P, B1(P, B), 16) + F5%(D, 7) Then
                    A!(P, B1(P, B), 7) = A!(P, B1(P, B), 7) - 1
                    Call ResetShotClock
                    LT = 0

                    gameClock! = gameClock! - 4
                    timeElapsed! = 4
                    Call AdjustPlayTime
                    Call AdjustShotClock
                    Call MAINSCREEN

                    Call FoulCalled(1)

                    If PB% = 0 Then
                        U$ = "2 FT: " + AA$(P, B1(P, B)) + Str$(A!(P, B1(P, B), 1)) + " %"
                        Call PBP
                        Call DELAY
                    Else
                        Call WINDEX
                        Print B$(P)
                        Print "2 FT: "; AA$(P, B1(P, B)); "   "; A!(P, B1(P, B), 1); " %"
                    End If

                    X = 50

                    Call DEADBALLOPTIONS

                    For I = 0 To 1
                        If P7(I) = 0 Then PR%(I, 0) = 1: PR%(I, 1) = 7: PC%(I) = 0
                    Next

                    X = 0

                    If pbpDelay! <> 0 Then

                        If PB% = 0 Then
                            U$ = "2 FT: " + AA$(P, B1(P, B)) + Str$(A!(P, B1(P, B), 1)) + " %"
                            Call PBP
                        Else
                            Call WINDEX
                            Print B$(P)
                            Print "2 FT: "; AA$(P, B1(P, B)); "   "; A!(P, B1(P, B), 1); " %"
                        End If

                    End If

                    'Two FT Attempts
                    For F3 = 1 To 2:

                        S = Int(Rnd(1) * 100) + 1

                        'FT Success Chance
                        FX% = A!(P, B1(P, B), 1)

                        If FA%(P, B1(P, B)) <= -5 Then FX% = Int(FX% * .9 + .5)

                        If S <= FX% Then

                            If PB% = 0 Then
                                Call THREEFT
                            Else
                                'PB% = 1
                                Call MADEFT
                            End If
                        Else
                            'S > FX% And
                            If PB% = 0 Then
                                Call BADFT
                            Else
                                'PB% = 1
                                Call MISSFT

                            End If
                        End If

                    Next F3

                    'Increase FT ATT
                    A!(P, B1(P, B), 9) = A!(P, B1(P, B), 9) + 2

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
                        PB$ = ""
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

                        U$ = B$(P) + " BALL O.B. AFTER SCORE"
                        Call SELECTPBP

                        Locate 8, 41: Print Space$(12)

                        Call DELAY
                        Call STOPPAGE
                        B = 6
                        X = 0
                        Call PRESSGUARD

                        GoTo 300

                    End If

                End If

            End If

            540 '
            S = Int(Rnd(1) * 100) + 1
            R0! = F! * ((DY / 100) + (.03 * X3%))

            If H > 1 And gameClock! < 30 And GG = 2 Or H > 1 And gameClock! < 30 And GG = 0 Or H > 1 And gameClock! < 30 And GG = -2 Then 542

            If S <= R0! Then
                Call ResetShotClock
                LT = 0

                610 '
                gameClock! = gameClock! - 4
                timeElapsed! = 4
                Call AdjustPlayTime
                Call AdjustShotClock
                Call MAINSCREEN
                Call GoodBasket

                If DK% = 1 And PB% = 0 Then PT% = 2: Call MAKEPBP: U$ = "STUFF BY " + AA$(P, B1(P, B)): Call PBP: DK% = 0: GoTo 617
                If DK% = 1 And PB% = 1 Then Call WINDEX: Print "STUFF BY " + AA$(P, B1(P, B)): Call DELAY: DK% = 0: GoTo 617
                If PZ% = 8 Then PT% = 7: Call MAKEPBP: PZ% = 0: Call PBP: GoTo 617

                F3 = 1
                If PB% = 0 Then
                    U$ = "BASKET GOOD BY " + AA$(P, B1(P, B))
                    Call PBP
                Else
                    'PB% = 1
                    If pbpDelay! <> 0 Then Call WINDEX: Print "BASKET GOOD BY " + AA$(P, B1(P, B)): Call DELAY
                End If

                617 U$ = "... AND HE'S FOULED !!!"

                If PB% = 0 Then Call PBP
                If PB% = 1 And pbpDelay! <> 0 Then Print U$: Call DELAY

                Call DELAY

                F3 = 1
                S!(P, H) = S!(P, H) + 2
                S!(P, 0) = S!(P, 0) + 2
                A!(P, B1(P, B), 8) = A!(P, B1(P, B), 8) + 1
                Call MAINSCREEN
                Call FoulCalled(1)

                619 '
                If PB% = 1 And pbpDelay! <> 0 Then Call WINDEX: Print B$(P): Print "1 FT: "; AA$(P, B1(P, B)); "   "; A!(P, B1(P, B), 1); " %"
                If PB% = 0 Then U$ = AA$(P, B1(P, B)) + " WILL SHOOT 1": Call PBP

                X = 50
                Call DEADBALLOPTIONS

                For I = 0 To 1
                    If P7(I) = 0 Then PR%(I, 0) = 1: PR%(I, 1) = 7: PC%(I) = 0
                Next

                X = 0

                S = Int(Rnd(1) * 100) + 1

                If PB% = 1 And pbpDelay! <> 0 Then
                    Call WINDEX
                    Print B$(P)
                    Print "1 FT: "; AA$(P, B1(P, B)); "   "; A!(P, B1(P, B), 1); " %"
                End If

                FX% = A!(P, B1(P, B), 1)
                If FA%(P, B1(P, B)) <= -5 Then FX% = Int(FX% * .9 + .5)

                If S <= FX% And PB% = 0 Then
                    'Increase FTM
                    A!(P, B1(P, B), 10) = A!(P, B1(P, B), 10) + 1

                    If pbpDelay! <> 0 Then U$ = "FREE THROW IS GOOD": Call PBP

                    S!(P, H) = S!(P, H) + 1
                    S!(P, 0) = S!(P, 0) + 1

                    Call MAINSCREEN

                End If

                If S <= FX% And PB% = 1 Then Call MADEFT
                If S > FX% And PB% = 0 Then If pbpDelay! <> 0 Then U$ = "FREE THROW IS MISSED": Call PBP
                If S > FX% And PB% = 1 Then Call MISSFT

                'Increase FT ATT
                A!(P, B1(P, B), 9) = A!(P, B1(P, B), 9) + 1

                If gameClock! <= 0 Then
                    timeElapsed! = gameClock!
                    Call AdjustPlayTime
                    GoTo 170
                End If

                X = 99

                If S > FX% Then 670

                PB$ = ""
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

                U$ = B$(P) + " BALL O.B. AFTER SCORE"
                Call SELECTPBP
                Locate 8, 41: Print Space$(12)
                Call DELAY
                Call STOPPAGE
                B = 6
                X = 0
                Call PRESSGUARD

                GoTo 300

            End If

            542 '
            If S <= F! Then
                LT = 0

                If DK% = 1 And PB% = 0 Then
                    PT% = 2
                    Call MAKEPBP
                    U$ = "STUFF BY "
                    DK% = 0
                Else
                    If DK% = 1 And PB% = 1 Then
                        U$ = "STUFF BY "
                        DK% = 0
                    Else

                        If PB% = 0 And PZ% = 8 Then
                            PT% = 7
                            Call MAKEPBP
                            Call PBP
                            PZ% = 0

                        Else
                            U$ = "BASKET BY "

                            U$ = U$ + AA$(P, B1(P, B)) + " !!!"

                            If PB% = 1 And pbpDelay! <> 0 Then
                                Call WINDEX: Print U$: Call DELAY
                            Else

                                RN = Int(Rnd(1) * 100) + 1

                                If RN > 50 Then

                                    RN = Int(Rnd(1) * 22) + 1
                                    Select Case RN
                                        Case 1
                                            U$ = "NOTHING BUT THE BOTTOM OF THE NET!"
                                        Case 2
                                            U$ = "COUNT IT!"
                                        Case 3
                                            U$ = AA$(P, B1(P, B)) + " TICKLES THE TWINE!"
                                        Case 4
                                            U$ = "NO RIM, NO BOARD, JUST CORD!"
                                        Case 5
                                            U$ = "GOOD!"
                                        Case 6
                                            U$ = "YES!"
                                        Case 7
                                            U$ = "COUNT IT!"
                                        Case 8
                                            U$ = "IT'S GOOD!"
                                        Case 9
                                            U$ = "BINGO!"
                                        Case 10
                                            U$ = "IT FALLS!"
                                        Case 11
                                            U$ = "HE DRAINS IT!"
                                        Case 12
                                            U$ = "SWISH!"
                                        Case 13
                                            U$ = "ALL NET!"
                                        Case 14
                                            U$ = "IT'S THERE!"
                                        Case 15
                                            U$ = "HE BURIES IT!"
                                        Case 16
                                            U$ = "IT GOES IN!"
                                        Case 17
                                            U$ = "RING IT UP!"
                                        Case 18
                                            U$ = "IT GOES DOWN!"
                                        Case 19
                                            U$ = "BULLSEYE!"
                                        Case 20
                                            U$ = "HE SCORES!"
                                        Case 21
                                            U$ = "HE HAS IT!"
                                        Case 22
                                            U$ = "HE CONNECTS!"
                                    End Select

                                End If

                                If PB% = 0 Then Call PBP

                            End If

                        End If

                    End If

                End If

                560 S!(P, H) = S!(P, H) + 2
                S!(P, 0) = S!(P, 0) + 2

                Call GoodBasket

                Call MAINSCREEN

                A!(P, B1(P, B), 8) = A!(P, B1(P, B), 8) + 1

                562 U$ = B$(D) + " BALL O.B. AFTER SCORE"
                If PB% = 1 And pbpDelay! <> 0 Then Locate 9, 1: Print U$
                If PB% = 0 Then Locate 8, 41: Print Space$(12): Call PBP

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

                Call MAINSCREEN

                PB$ = ""
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
                R0! = Int(Rnd(1) * 100) + 1
                LT = 0

                I2 = Int(Rnd(1) * 5) + 1
                I3 = Int(Rnd(1) * 5)

                If I2 > A!(D, B1(D, I3), 6) Then

                    545 '
                    If PB% = 1 And pbpDelay! <> 0 Then Call WINDEX: Print "SHOT MISSED BY "; AA$(P, B1(P, B)): Call DELAY
                    If PB% = 0 Then PT% = 8: Call MAKEPBP: Call PBP

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

                    Call MAINSCREEN

                    LT = 0

                    GoTo 670

                End If

                If pbpDelay! <> 0 Then

                    If PB% = 1 Then
                        Call WINDEX
                        Print "SHOT BLOCKED BY "; AA$(D, B1(D, I3))
                        Call DELAY
                    Else

                        RN = Int(Rnd(1) * 100) + 1
                        Select Case RN
                            Case 1 To 10
                                U$ = AA$(D, B1(D, I3)) + " STUFFS THE SHOT!!"
                            Case 11 To 15
                                U$ = "DENIED BY " + AA$(D, B1(D, I3)) + "!!"
                            Case 16 To 20
                                U$ = "ERASED BY " + AA$(D, B1(D, I3)) + "!!"
                            Case 21 To 25
                                U$ = "EXPUNGED BY " + AA$(D, B1(D, I3)) + "!!"
                            Case 26 To 30
                                U$ = "REBUKED BY " + AA$(D, B1(D, I3)) + "!!"
                            Case 31 To 35
                                U$ = "SHOT BLOCKED CLEANLY BY " + AA$(D, B1(D, I3)) + "!!"
                            Case 36 To 70
                                U$ = "SHOT BLOCKED BY " + AA$(D, B1(D, I3)) + "!!"
                            Case 71 To 90
                                U$ = "SHOT REJECTED BY " + AA$(D, B1(D, I3)) + "!!"
                            Case 91 To 100
                                U$ = "SHOT SWATTED OUT BY " + AA$(D, B1(D, I3)) + "!!"
                        End Select

                        Call PBP

                    End If

                End If

                A!(D, B1(D, I3), 15) = A!(D, B1(D, I3), 15) + 1
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

                Call MAINSCREEN

                GoTo 670

            End If

            '--- done with "routines" for taking a shot

        Case 3
            GoTo 640

        Case 4
            584 '
            IN = 99
            Call LASTSHOT

            If gameClock! <= 0 Then 170

            C1 = 2: P9 = D
            U$ = "DESIGNATED FOULER (0-4)"

            Call SELECTPBP

            If U5 = 1 And D = CT Or U5 = 2 Then
                W5 = Int(Rnd(1) * 5)
            Else
                Do
                    I$ = GetKeyPress$
                    W5 = Val(I$)
                Loop Until W5 >= 0 And W5 <= 4
            End If

            Call FoulCalled(0)

            GoTo 9065

        Case 5
            J8% = 0
            If X3% = 0 Then
                P3 = 1
                P6 = 1
                GoTo 537
            Else
                S = Int(Rnd(1) * 100) + 1
                W%(P, B1(P, B), 1) = W%(P, B1(P, B), 1) + 1
                A!(P, B1(P, B), 7) = A!(P, B1(P, B), 7) + 1
                PT% = 3

                Call MAKEPBP
                Call PBP

                I2 = Int(Rnd(1) * 50) + 1

                If I2 = 43 And TF% = 1 And S > F3S% Then

                    A!(P, B1(P, B), 7) = A!(P, B1(P, B), 7) - 1
                    W%(P, B1(P, B), 1) = W%(P, B1(P, B), 1) - 1

                    Call ResetShotClock

                    9055 '
                    gameClock! = gameClock! - 4
                    timeElapsed! = 4
                    Call AdjustPlayTime
                    Call AdjustShotClock
                    Call MAINSCREEN

                    Call FoulCalled(1)

                    If pbpDelay! <> 0 Then
                        If PB% = 0 Then
                            U$ = "3 FT: " + AA$(P, B1(P, B)) + Str$(A!(P, B1(P, B), 1)) + " %"
                            Call PBP
                            Call DELAY
                        Else
                            Call WINDEX
                            Print B$(P)
                            Print "3 FT: "; AA$(P, B1(P, B)); "   "; A!(P, B1(P, B), 1); " %"
                        End If
                    End If

                    X = 50
                    Call DEADBALLOPTIONS

                    For I = 0 To 1
                        If P7(I) = 0 Then PR%(I, 0) = 1: PR%(I, 1) = 7: PC%(I) = 0
                    Next

                    X = 0

                    If pbpDelay! <> 0 Then
                        If PB% = 0 Then
                            U$ = "3 FT: " + AA$(P, B1(P, B)) + Str$(A!(P, B1(P, B), 1)) + " %"
                            Call PBP
                        Else
                            Call WINDEX
                            Print B$(P)
                            Print "3 FT: "; AA$(P, B1(P, B)); "   "; A!(P, B1(P, B), 1); " %"
                        End If

                    End If

                    'Increase FT ATT
                    A!(P, B1(P, B), 9) = A!(P, B1(P, B), 9) + 3

                    '3 FT Attempts
                    For F3 = 1 To 3:

                        S = Int(Rnd(1) * 100) + 1

                        'FT Success Chance
                        FX% = A!(P, B1(P, B), 1)
                        If FA%(P, B1(P, B)) <= -5 Then FX% = Int(FX% * .9 + .5)

                        If S <= FX% Then
                            If PB% = 0 Then
                                Call THREEFT
                            Else
                                'PB% = 1
                                Call MADEFT
                            End If
                        Else
                            'S > FX%
                            If PB% = 0 Then
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

                If I2 = 43 And TF% = 1 And S <= F3S% Then

                    gameClock! = gameClock! - 4
                    timeElapsed! = 4
                    Call AdjustPlayTime
                    Call AdjustShotClock
                    Call MAINSCREEN
                    Call GoodBasket
                    U$ = "3 PT BASKET GOOD BY " + AA$(P, B1(P, B))
                    Call SELECTPBP

                    Call GoodBasket

                    U$ = "AND HE WAS FOULED !!!"
                    If PB% = 0 Then Call PBP
                    If PB% = 1 And pbpDelay! <> 0 Then Print U$: Call DELAY

                    F3 = 1
                    S!(P, H) = S!(P, H) + 3
                    S!(P, 0) = S!(P, 0) + 3
                    A!(P, B1(P, B), 8) = A!(P, B1(P, B), 8) + 1
                    W%(P, B1(P, B), 0) = W%(P, B1(P, B), 0) + 1
                    Call MAINSCREEN

                    Call FoulCalled(1)

                    GoTo 619

                End If

                If S > F3S% Then

                    If PB% = 1 And pbpDelay! <> 0 Then Call WINDEX: Print "3 PT FG MISSED BY "; AA$(P, B1(P, B)): Call DELAY
                    If PB% = 0 Then PT% = 5: TP% = 1: Call MAKEPBP: Call PBP

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

                    Call MAINSCREEN

                    GoTo 670

                Else

                    If PB% = 1 And pbpDelay! <> 0 Then Call WINDEX: Print "3 PT FG GOOD BY "; AA$(P, B1(P, B)); " !!!": Call DELAY
                    If PB% = 0 Then PT% = 4: Call MAKEPBP: Call PBP

                    369 If XX% = 1 Then
                        XX% = 0: S!(P, H) = S!(P, H) + 2: S!(P, 0) = S!(P, 0) + 2: W%(P, B1(P, B), 1) = W%(P, B1(P, B), 1) - 1
                    Else
                        S!(P, H) = S!(P, H) + 3: S!(P, 0) = S!(P, 0) + 3: W%(P, B1(P, B), 0) = W%(P, B1(P, B), 0) + 1
                    End If

                    A!(P, B1(P, B), 8) = A!(P, B1(P, B), 8) + 1
                    Call GoodBasket
                    Call MAINSCREEN

                    GoTo 562

                End If

            End If

    End Select

    '----- NOODLE -----

    570 '
    If pbpDelay! <> 0 Then
        If PB% = 0 Then
            U$ = "1+1 FT: " + AA$(P, B1(P, B)) + Str$(A!(P, B1(P, B), 1)) + " %"
            Call PBP
        Else
            Call WINDEX
            Print B$(P)
            Print "1+1 FT: "; AA$(P, B1(P, B)); " "; A!(P, B1(P, B), 1); " %"
        End If

    End If

    571 '
    X = 50
    Call DEADBALLOPTIONS

    For I = 0 To 1
        If P7(I) = 0 Then PR%(I, 0) = 1: PR%(I, 1) = 7: PC%(I) = 0
    Next

    X = 0

    If pbpDelay! <> 0 Then

        If PB% = 0 Then
            U$ = "1+1 FT: " + AA$(P, B1(P, B)) + Str$(A!(P, B1(P, B), 1)) + " %"
            Call PBP
        Else
            Call WINDEX
            Print B$(P)
            Print "1+1 FT: "; AA$(P, B1(P, B)); "   "; A!(P, B1(P, B), 1); " %"
        End If

    End If

    572 '
    '2 FT Attempts
    For F3 = 1 To 2

        'Increase FT ATT
        A!(P, B1(P, B), 9) = A!(P, B1(P, B), 9) + 1

        S = Int(Rnd(1) * 100) + 1

        'FT Success Chance
        FX% = A!(P, B1(P, B), 1)
        If FA%(P, B1(P, B)) <= -5 Then FX% = Int(FX% * .9 + .5)

        If S <= FX% Then

            If PB% = 0 Then
                Call THREEFT
            Else
                'PB% = 1 Then
                Call MADEFT
            End If
        Else
            'S > FX% And
            If PB% = 0 Then
                Call BADFT
            Else
                'PB% = 1
                Call MISSFT
            End If

        End If

        If F3 = 1 And S > A!(P, B1(P, B), 1) Then Exit For

    Next F3

    575 If gameClock! <= 0 Then 170

    If S > FX% Then 650

    PB$ = ""
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
    U$ = B$(P) + " BALL O.B. AFTER SCORE"
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

    PB$ = ""
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

    B = W5

    If P2(D) > 6 Then 570

    If gameClock! <= 0 Then 170
    U$ = "OFFENSIVE FOUL"
    Call SELECTPBP

    U$ = B$(P) + " BALL O.B. AFTER FOUL"
    If PB% = 0 Then Call PBP
    If PB% = 1 And pbpDelay! <> 0 Then Print U$: Call DELAY

    Locate 8, 41: Print Space$(12): B = 6
    Call DELAY
    T0(D) = T0(D) + 1
    Call MAINSCREEN
    Call DEADBALLOPTIONS

    For I = 0 To 1
        If P7(I) = 0 Then PR%(I, 0) = 1: PR%(I, 1) = 7: PC%(I) = 0
    Next

    Call PRESSGUARD

    GoTo 300

    '----- NOODLE -----

    640 '
    PB$ = ""

    If T1(P) > 0 Then
        T1(P) = T1(P) - 1
        Call SCOREBOARD

        RN = Int(Rnd(1) * 100)
        If RN <= 8 Then TF%(P) = 10
        If RN > 8 And RN <= 16 Then TF%(P) = 5

        RN = Int(Rnd(1) * 100)
        If RN <= 8 Then
            TF%(D) = -10
        Else
            If RN <= 16 Then TF%(D) = -5
        End If

        gameClock! = gameClock! + M5
        timeElapsed! = -M5
        Call AdjustPlayTime
        If sClockVal >= 1 Then shotClock = shotClock + M5
        '
        For I = 0 To 1
            For I1 = 0 To 1
                PR%(I, I1) = 0
            Next
        Next

        U$ = "TIME OUT " + B$(P)
        Call SELECTPBP
        Call DELAY
        Call DEADBALLOPTIONS

        For I = 0 To 1
            For I1 = 0 To 13
                FA%(I, I1) = FA%(I, I1) + 1
                If FA%(I, I1) > 0 Then FA%(I, I1) = 0
            Next
        Next

    End If

    If A6 = 0 Then Call PRESSGUARD: GoTo 300

    GoTo 350

    '----- NOODLE -----

    650 A6 = 1
    P6 = 0
    A4 = 0
    D6 = 0
    Call ResetShotClock
    S = 0

    R0! = Int(Rnd(1) * 100) + 1

    If H > 1 And gameClock! < 30 And S!(D, 0) - S!(P, 0) = 2 And R0! <= 12 Or H > 1 And gameClock! < 30 And S!(P, 0) - S!(D, 0) = 0 And R0! <= 12 Then
        Call OFFREBOUND
        GoTo 760
    End If

    R0! = Int(Rnd(1) * 100) + 1

    Select Case R0!
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

    670 A6 = 1
    P6 = 0
    A4 = 0
    D6 = 0
    Call ResetShotClock

    '--- loop
    700 S = 0
    A4 = 0
    D6 = 0

    R0! = Int(Rnd(1) * 100) + 1
    GG = S!(D, 0) - S!(P, 0)

    If H > 1 And gameClock! < 30 And (GG = 2 Or GG = 0 Or GG = -2) And R0! <= 12 Then 700
    '--- loop

    If TP% = 1 And R0! <= 19 Then TP% = 0

    Select Case R0!
        Case 0 To 1

            725 If X = 98 Then 700
            T0(P) = T0(P) + 1
            U$ = "OFFENSIVE GOAL TENDING"
            Call SELECTPBP

            U$ = "BALL O.B. TO " + B$(D)
            If PB% = 0 Then Call PBP
            If PB% = 1 And pbpDelay! <> 0 Then Print U$: Call DELAY

            Call DELAY

            PB$ = ""
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
                If P7(I) = 0 Then PR%(I, 0) = 1: PR%(I, 1) = 7: PC%(I) = 0
            Next

            GoTo 300

        Case 2 To DY
            FF% = 1

        Case DY + 1 To 17
            750 '
            R0! = Int(Rnd(1) * 100) + 1

            If R0! > 35 Then

                R3!(D) = R3!(D) + 1: Call MAINSCREEN
                U$ = "RB KNOCKED O.B. TO " + B$(D)
                Call SELECTPBP
                Call DELAY
                Call DEADBALLOPTIONS

                For I = 0 To 1
                    If P7(I) = 0 Then PR%(I, 0) = 1: PR%(I, 1) = 7: PC%(I) = 0
                Next

                PB$ = ""
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

                752 R3!(P) = R3!(P) + 1: Call MAINSCREEN
                U$ = "RB KNOCKED O.B. TO " + B$(P)
                Call SELECTPBP
                Call DELAY
                Call DEADBALLOPTIONS

                For I = 0 To 1
                    If P7(I) = 0 Then PR%(I, 0) = 1: PR%(I, 1) = 7: PC%(I) = 0
                Next

                LT = 4

                GoTo 520

            End If

        Case 18 To 19
            730 '
            If X = 99 Then 700

            Call OFFREBOUND

            A!(P, B1(P, B), 7) = A!(P, B1(P, B), 7) + 1
            A!(P, B1(P, B), 8) = A!(P, B1(P, B), 8) + 1
            A!(P, B1(P, B), 11) = A!(P, B1(P, B), 11) + 1
            R3!(P) = R3!(P) + 1: S!(P, H) = S!(P, H) + 2
            S!(P, 0) = S!(P, 0) + 2
            Call MAINSCREEN

            If PB% = 1 And pbpDelay! <> 0 Then Call WINDEX: Print "TIPPED IN BY "; AA$(P, B1(P, B)): Print "   BASKET GOOD !!!": Call DELAY
            If PB% = 0 Then U$ = AA$(P, B1(P, B)) + " TIPS IT IN!!": Call PBP

            PB$ = ""
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

            U$ = B$(P) + " BALL O.B. AFTER SCORE"
            Locate 8, 41: Print Space$(12)

            If PB% = 0 Then Call PBP
            If PB% = 1 And pbpDelay! <> 0 Then Print U$: Call DELAY

            Call DELAY
            Call STOPPAGE
            Call PRESSGUARD

            GoTo 300

        Case 20
            If X = 99 Then 700

            U$ = "DEFENSIVE GOAL TENDING"
            Call SELECTPBP

            If TP% = 1 Then
                TP% = 0

                If XX% = 1 Then
                    XX% = 0
                    S!(P, H) = S!(P, H) + 2
                    S!(P, 0) = S!(P, 0) + 2
                    W%(P, B1(P, B), 1) = W%(P, B1(P, B), 1) - 1
                Else
                    S!(P, H) = S!(P, H) + 3
                    S!(P, 0) = S!(P, 0) + 3
                    W%(P, B1(P, B), 0) = W%(P, B1(P, B), 0) + 1
                End If

                A!(P, B1(P, B), 8) = A!(P, B1(P, B), 8) + 1
                Call GoodBasket
                Call MAINSCREEN

                GoTo 562

            Else

                GoTo 560

            End If

    End Select

    'Essentially this is Case 21 to 100
    '   It's not written as such because
    '   case 2 to DY will end up here too
    707 '
    TP% = 0
    RB = 0

    For I = 0 To 9: RB(I) = 0: Next

    For I = 0 To 4
        XZ = A!(P, B1(P, I), 21)

        If Not (H > 2 Or H = 2 And gameClock! <= 120) Then
            If A!(P, B1(P, I), 3) - A!(P, B1(P, I), 7) - A!(P, B1(P, I), 11) - A!(P, B1(P, I), 12) <= 0 Then XZ = Int(XZ * .5 + .5)
        End If

        If FA%(P, B1(P, I)) <= -5 Then XZ = Int(XZ * .9 + .5)

        RB(I) = RB(I) + XZ: RB = RB + XZ
    Next

    ORB = RB

    For I = 0 To 4
        XZ = A!(1 - P, B1(1 - P, I), 2)
        If H > 2 Or H = 2 And gameClock! <= 120 Then 778
        If A!(1 - P, B1(1 - P, I), 3) - A!(1 - P, B1(1 - P, I), 7) - A!(1 - P, B1(1 - P, I), 11) - A!(1 - P, B1(1 - P, I), 12) <= 0 Then XZ = Int(XZ * .5 + .5)
        778 If FA%(1 - P, B1(1 - P, I)) <= -5 Then XZ = Int(XZ * .9 + .5)
        RB(I + 5) = RB(I + 5) + XZ: RB = RB + XZ
    Next

    DRB = RB - ORB

    If D2(D) = 0 Then XQ = Int((RB * .06) + .5)
    If D2(D) = 1 Then XQ = Int((RB * .12) + .5)
    If D2(D) = 2 Then XQ = Int((RB * .1) + .5)
    If D2(D) = 3 Then XQ = Int((RB * .14) + .5)
    If CZ%(P) = 1 Then XQ = Int((RB * .17) + .5)

    RN = Rnd(1) * RB
    If RN <= ORB + XQ Then

        RN = Rnd(1) * ORB
        For B = 0 To 4
            If RN <= RB(B) Then 760
            RB(B + 1) = RB(B + 1) + RB(B)
        Next

        B = Int(Rnd(1) * 5)

        GoTo 760

    End If

    RN = Rnd(1) * DRB
    skipRandom% = 0

    For B = 5 To 9

        If RN <= RB(B) Then
            B = B - 5
            skipRandom% = 1
            Exit For
        Else
            RB(B + 1) = RB(B + 1) + RB(B)
        End If

    Next

    If skipRandom% = 0 Then
        B = Int(Rnd(1) * 5)
    End If

    800 '
    If FF% = 1 Then

        FF% = 0

        710 '
        If pbpDelay! <> 0 Then
            If PB% = 1 Then
                Call WINDEX
                Print AA$(D, B1(D, B)); " HAS THE DEF RB FOR"
                Print B$(D); " AND IS FOULED !!!"
                Call DELAY
            Else
                U$ = AA$(D, B1(D, B)) + " HAS THE DEF RB"
                Call PBP
                U$ = "HE'S FOULED !!!"
                Call PBP
            End If
        End If

        R3!(D) = R3!(D) + 1
        A!(D, B1(D, B), 11) = A!(D, B1(D, B), 11) + 1

        Call MAINSCREEN

        PB$ = ""
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

    A!(D, B1(D, B), 11) = A!(D, B1(D, B), 11) + 1
    R3!(D) = R3!(D) + 1
    Call MAINSCREEN
    U$ = AA$(D, B1(D, B)) + " HAS THE DEF RB"

    If PB% = 0 And pbpDelay! <> 0 Then Call PBP
    If PB% = 1 And pbpDelay! <> 0 Then Call WINDEX: Print U$: Print "FOR " + B$(D): Call DELAY

    PB$ = AA$(D, B1(D, B))
    C1 = 2
    P9 = D

    PB$ = ""
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
    PB$ = AA$(P, B1(P, B))

    R0! = Int(Rnd(1) * 130) + 1

    If R0! = 15 Then FB% = 0: GoTo RBTIEUP

    If CZ%(D) = 1 And Int(Rnd(1) * 100) + 1 <= 50 Then

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

        U$ = AA$(P, B1(P, B)) + " HAS THE OFF RB"
        If PB% = 0 Then Call PBP
        If PB% = 1 And pbpDelay! <> 0 Then Call WINDEX: Print U$: Print "FOR "; B$(P): Call DELAY

        U$ = "HE'S FOULED !!!"
        If PB% = 0 Then Call PBP
        If PB% = 1 And pbpDelay! <> 0 Then Print U$: Call DELAY

        A!(P, B1(P, B), 11) = A!(P, B1(P, B), 11) + 1
        A!(P, B1(P, B), 24) = A!(P, B1(P, B), 24) + 1
        R3!(P) = R3!(P) + 1

        Call MAINSCREEN
        Call FoulCalled(1)
        LT = 4

        GoTo 9065

    End If

    A!(P, B1(P, B), 24) = A!(P, B1(P, B), 24) + 1
    A!(P, B1(P, B), 11) = A!(P, B1(P, B), 11) + 1
    R3!(P) = R3!(P) + 1
    Call MAINSCREEN

    U$ = AA$(P, B1(P, B)) + " HAS THE OFF RB"
    If PB% = 0 And pbpDelay! <> 0 Then Call PBP
    If PB% = 1 And pbpDelay! <> 0 Then Call WINDEX: Print U$: Print "FOR " + B$(P): Call DELAY

    If gameClock! <= 0 Then

        GoTo 170

    Else

        R0! = Int(Rnd(1) * 130) + 1
        PB$ = AA$(P, B1(P, B))

        If R0! = 32 Then
            GoTo RBTIEUP
        Else
            R0! = Int(Rnd(1) * 10) + 1
            Call SHOTPERCENT

            If Not (H > 2 Or H = 2 And gameClock! <= 120) Then
                If A!(P, B1(P, B), 3) - A!(P, B1(P, B), 7) - A!(P, B1(P, B), 11) - A!(P, B1(P, B), 12) <= 0 Then F! = Int(F! * .6 + .5)
            End If

            If R0! <= 8 Then
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
    If PB% <> 1 Then

        R0! = Int(Rnd(1) * 100) + 1

        Select Case R0!
            Case 1 To 20
                U$ = "BALL IS SLAPPED AWAY"
            Case 21 To 40
                U$ = "BALL IS LOOSE ON THE FLOOR"
            Case 41 To 60
                U$ = AA$(P, B1(P, B)) + " IS TRAPPED"
            Case 61 To 80
                U$ = "TRIES TO WORK LOOSE"
            Case 81 To 100
                U$ = AA$(P, B1(P, B)) + " IS PINNED ON THE LINE"
        End Select

        Call PBP

    End If

    RBTIEUP:
    R0! = Int(Rnd(1) * 100) + 1
    P9 = Int(Rnd(1) * 5)

    If PB% = 1 And pbpDelay! <> 0 Then
        Call WINDEX
        Print "JUMP BALL!"
        Call DELAY
    Else
        Select Case R0!
            Case 1 To 25
                U$ = AA$(P, B1(P, B)) + " IS TIED UP BY " + AA$(D, B1(D, P9))
            Case 26 To 50
                U$ = AA$(D, B1(D, P9)) + " TIES UP " + AA$(P, B1(P, B))
            Case 51 To 75
                U$ = "HELD BALL!"
            Case 76 To 100
                U$ = "JUMP BALL!"
        End Select

        Call PBP

    End If

    U$ = "POSSESSION ARROW TO " + B$(PA%)
    If PB% = 0 Then Call PBP
    If PB% = 1 And pbpDelay! <> 0 Then Print U$: Call DELAY

    Call DELAY

    If PA% = D Then
        T0(P) = T0(P) + 1
        Call DEADBALLOPTIONS

        For I = 0 To 1
            If P7(I) = 0 Then PR%(I, 0) = 1: PR%(I, 1) = 7: PC%(I) = 0
        Next

        PB$ = ""
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
            If P7(I) = 0 Then PR%(I, 0) = 1: PR%(I, 1) = 7: PC%(I) = 0
        Next
        LT = 4

        GoTo 350

    End If

    '----- NOODLE -----

    9065

    If IN% = 1 Or (TB% = 1 And P2(D) >= 10) Then

        If pbpDelay! <> 0 Then
            If PB% = 0 Then
                U$ = "2 FT: " + AA$(P, B1(P, B)) + Str$(A!(P, B1(P, B), 1)) + " %"
                Call PBP
                Call DELAY
            Else
                Call WINDEX
                Print B$(P)
                Print "2 FT: "; AA$(P, B1(P, B)); "   "; A!(P, B1(P, B), 1); " %"
            End If
        End If

        X = 50
        Call DEADBALLOPTIONS

        For I = 0 To 1
            If P7(I) = 0 Then PR%(I, 0) = 1: PR%(I, 1) = 7: PC%(I) = 0
        Next

        X = 0

        If pbpDelay! <> 0 Then
            If PB% = 0 Then
                U$ = "2 FT: " + AA$(P, B1(P, B)) + Str$(A!(P, B1(P, B), 1)) + " %"
                Call PBP
            Else
                Call WINDEX
                Print B$(P)
                Print "2 FT: "; AA$(P, B1(P, B)); "   "; A!(P, B1(P, B), 1); " %"
            End If
        End If

        '2 FT Attempts
        For F3 = 1 To 2:

            'Increase FT ATT
            A!(P, B1(P, B), 9) = A!(P, B1(P, B), 9) + 1

            S = Int(Rnd(1) * 100) + 1

            'FT Success Chance
            FX% = A!(P, B1(P, B), 1)
            If FA%(P, B1(P, B)) <= -5 Then FX% = Int(FX% * .9 + .5)

            If S <= FX% Then

                If PB% = 0 Then
                    Call THREEFT
                Else
                    'PB% = 1
                    Call MADEFT
                End If
            Else
                'S > FX% And
                If PB% = 0 Then
                    Call BADFT
                Else
                    'PB% = 1
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
            U$ = B$(P) + " BALL AFTER INT. FOUL"
            Call SELECTPBP
            Locate 8, 41: Print Space$(12)
            Call DELAY
            Call STOPPAGE
            Call PRESSGUARD
        Else

            PB$ = ""

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

            U$ = B$(P) + " BALL O.B. AFTER SCORE"
            Locate 8, 41: Print Space$(12)

            Call SELECTPBP
            Call DELAY
            Call STOPPAGE
            Call PRESSGUARD
        End If

        GoTo 300

    Else

        If P2(D) > 6 Then 570

        567 '
        If gameClock! <= 0 Then 170

        Call DEADBALLOPTIONS

        For I = 0 To 1
            If P7(I) = 0 Then PR%(I, 0) = 1: PR%(I, 1) = 7: PC%(I) = 0
        Next

        If N7% = 0 And PB% = 0 Or N7% = 0 And pbpDelay! = 0 Then
            Call WINDEX
            U$ = B$(P) + " BALL O.B. AFTER FOUL": Call PBP: Locate 8, 41: Print Space$(12)
            GoTo 350
        End If

        If N7% = 0 And PB% = 1 Then
            Call WINDEX
            Print B$(P) + " BALL O.B. AFTER FOUL": Call DELAY: Locate 8, 41: Print Space$(12)
            GoTo 350
        End If

        Call MAINSCREEN

        U$ = "1 FT: " + AA$(P, B1(P, B)) + Str$(A!(P, B1(P, B), 1)) + " %"
        Call PBP

        X = 50

        Call DEADBALLOPTIONS

        For I = 0 To 1
            If P7(I) = 0 Then PR%(I, 0) = 1: PR%(I, 1) = 7: PC%(I) = 0
        Next

        X = 0

        U$ = "1 FT: " + AA$(P, B1(P, B)) + Str$(A!(P, B1(P, B), 1)) + " %"

        Call PBP

        F3 = 1

        'Increase FT ATT
        A!(P, B1(P, B), 9) = A!(P, B1(P, B), 9) + 1

        S = Int(Rnd(1) * 100) + 1
        FX% = A!(P, B1(P, B), 1)
        If FA%(P, B1(P, B)) <= -5 Then FX% = Int(FX% * .9 + .5)

        If S <= FX% Then
            If PB% = 0 Then
                Call THREEFT
            Else
                'PB% = 1
                Call MADEFT
            End If
        Else
            'S > FX%
            If PB% = 0 Then
                Call BADFT
            Else
                'PB% = 1
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

        PB$ = ""
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

        U$ = B$(P) + " BALL O.B. AFTER SCORE"
        Locate 8, 41: Print Space$(12)

        Call PBP
        Call DELAY
        Call STOPPAGE

        B = 6
        X = 0

        Call PRESSGUARD

        GoTo 300

    End If

End Sub

