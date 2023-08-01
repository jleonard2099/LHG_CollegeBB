Sub TOURNEY ()

    _Title "Courtside College Basketball - Tournament"

    Shared homeTeam$(), visitingTeam$(), yearNumber$()
    Shared homeScores(), visitorScores()
    Shared scheduleAP%(), scheduleNG%()

    Dim NGG(MAX_TEAMS)

    Dim DY%(MAX_TEAMS), MON%(MAX_TEAMS), YR%(MAX_TEAMS)

    Shared coachMode$(), foulShotOpt$(), location$()
    Shared MO$(), sclockOpt$(), yesNo$()

    yearNumber$(0, 3) = "10-01-91"
    yearNumber$(0, 2) = "07:30P"

    tourneyFile$ = ChooseFile$ (diskPaths$(1) + "*.TOU", "TOURNAMENT", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Color 7, 0
        Cls

        scheduleNG%(0, 8) = 0
        scheduleNG%(0, 3) = 1
        scheduleNG%(0, 7) = 0
        scheduleNG%(0, 2) = 1
        scheduleNG%(0, 5) = 5
        yearNumber$(0, 0) = ""
        yearNumber$(0, 1) = ""

        For I = 1 To 500
            visitingTeam$(I) = "": scheduleNG%(I, 13) = 0
            homeTeam$(I) = ""
        Next

        Call ReadTourneyFile(tourneyFile$)

        Do

            Cls

            Locate 23, 1
            Color , 1: Print Tab(80);
            
            Locate 23, 1
            Color 13: Print "SC";
            Color 14: Print "-SHOT CLOCK  ";
            Color 13: Print "HC";
            Color 14: Print "-HOME COURT ADVANTAGE  ";
            Color 13: Print "3";
            Color 14: Print "-USE 3-POINT SHOT  ";
            Locate 24, 1: Color , 1: Print Tab(80);: Locate 24, 1
            Color 11: Print "PGUP  PGDN  ";
            Color 13: Print "REG";
            Color 14: Print "-REGION  ";
            Color 13: Print "RND";
            Color 14: Print "-ROUND  ";
            Color 13: Print "GM";
            Color 14: Print "-GAME  ";
            Locate 25, 1: Color , 1: Print Tab(80);: Locate 25, 1
            Color 11: Print "P";
            Color 14: Print "-PLAY TOURNAMENT   ";
            Color 11: Print "Q";
            Color 14: Print "-QUIT  ";
            Color 11: Print "E";
            Color 14: Print "-EDIT A GAME  ";
            Locate 1, 1: Print Tab(80);: Locate 1, 1: Print tourneyFile$
            Locate 1, 33
            Color 14: Print "AUTO SCHEDULER"
            Color , 0

            Locate 2, 1
            Color 14
            Print "GM ID#  REG  RND  GM    VISITING TEAM        HOME TEAM            HC  3  SC": Color 15
            For X = 1 To 20
                Locate X + 2, 1
                If X + NS% * 20 > scheduleAP%(0) Then Exit For
                Print Using "###      ##   ##   ##"; X + NS% * 20; scheduleNG%(X + NS% * 20, 15); scheduleNG%(X + NS% * 20, 16); scheduleNG%(X + NS% * 20, 17);
                'LOCATE , 17: PRINT yearNumber$(X + NS% * 20, 2);
                Locate , 25: Print visitingTeam$(X + NS% * 20); " ";
                If scheduleNG%(X + NS% * 20, 13) = 1 Then Print Using "###"; visitorScores(X + NS% * 20);
                Locate , 46: Print homeTeam$(X + NS% * 20); " ";
                If scheduleNG%(X + NS% * 20, 13) = 1 Then Print Using "###"; homeScores(X + NS% * 20);
                Locate , 67: If scheduleNG%(X + NS% * 20, 2) = 1 Then Print "Y";: Else Print "N";
                Locate , 71: Print yesNo$(scheduleNG%(X + NS% * 20, 3));
                Locate , 74: Print Left$(sclockOpt$(scheduleNG%(X + NS% * 20, 7)), 2)
            Next

            I$ = GetKeyPress$

            Select Case UCase$(I$)
                Case Chr$(0) + "Q":
                    130 ' *** PAGE DOWN ***
                    If NS% >= BS% Then
                        NS% = BS%
                    Else
                        NS% = NS% + 1
                    End If

                Case Chr$(0) + "I":
                    135 ' *** PAGE UP ***
                    If Not (NS% = 0) Then
                        NS% = NS% - 1
                    End If

                Case "E":
                    Call EditGame_TOURNEY

                Case "P":
                    ' *** SAVE SCHEDULE FILE ***
                    '    IF _FILEEXISTS(diskPaths$(1) + tourneyFile$ + ".TNY") THEN
                    '       KILL diskPaths$(1) + tourneyFile$ + ".TNY"
                    '    END IF

                    Open diskPaths$(1) + tourneyFile$ + ".TOU" For Random As #1 Len = 90

                    For X = 0 To 14
                        Field #1, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
                    Next

                    Field #1, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
                    Field #1, 64 As X$, 3 As Q$(20), 3 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
                    Field #1, 84 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)

                    For I = 1 To scheduleAP%(0)

                        For X = 0 To 14:
                            LSet Q$(X + 1) = MKI$(scheduleNG%(I, X))
                        Next

                        For X = 0 To 3:
                            LSet Q$(20 + X) = yearNumber$(I, X)
                        Next

                        LSet Q$(16) = MKI$(visitorScores(I))
                        LSet Q$(17) = MKI$(homeScores(I))
                        LSet Q$(18) = visitingTeam$(I)
                        LSet Q$(19) = homeTeam$(I)

                        For X = 1 To 3:
                            LSet Q$(X + 23) = MKI$(scheduleNG%(I, X + 14))
                        Next

                        Put #1, I

                    Next

                    Close 1

                    ' *** PLAY GAMES ***
                    Color , 0
                    Cls

                    JJ = 7
                    Locate 6, 8: Color 15, 4
                    Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";: For II = 1 To JJ: Locate 6 + II, 8
                    Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";: Next
                    Locate 7 + JJ, 8
                    Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

                    Do
                        'Locate 7, 32
                        Locate 8, 12: Print "TOTAL GAMES ="; scheduleAP%(0)
                        Locate 9, 12: Print "SELECT GAME # TO START AND END WITH": Print
                        Locate 10, 12: Input "ENTER START GAME #: "; SG%: Print
                        Locate 11, 12: Input "ENTER END GAME #:"; EG%
                        Locate 13, 12: Print "ANY CHANGE (YN)"
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "N"

                    If SG% <= 0 Then SG% = 1
                    If EG% > scheduleAP%(0) Then EG% = scheduleAP%(0)

                    Open "TOURNEY" For Output As #2
                    Print #2, tourneyFile$ ' + ".TNY"
                    Print #2, SG%
                    Print #2, EG%
                    Print #2, 0
                    Close 2

                    For I = 1 To scheduleAP%(0)
                        L = Len(yearNumber$(I, 2))
                        If L = 5 Then HR = Val(Left$(yearNumber$(I, 2), 1)): MIN = Val(Mid$(yearNumber$(I, 2), 3, 2))
                        If L = 6 Then HR = Val(Left$(yearNumber$(I, 2), 2)): MIN = Val(Mid$(yearNumber$(I, 2), 4, 2))
                        MON%(I) = Val(Left$(yearNumber$(I, 3), 2))
                        DY%(I) = Val(Mid$(yearNumber$(I, 3), 4, 2))
                        YR%(I) = Val(Mid$(yearNumber$(I, 3), 7, 2))
                        T$ = Right$(yearNumber$(I, 2), 1)
                        If UCase$(T$) = "P" And Left$(yearNumber$(I, 2), 2) <> "12" Then HR = HR + 12
                        TM = HR * 60 + MIN
                        NGG(I) = Int(TM / 30 - 18): scheduleNG%(I, 12) = NGG(I)
                    Next

                    Open "TICKER" For Output As #2

                    For I = 1 To 32:
                        For I1 = 0 To 1:
                            For I2 = 1 To 14
                                Print #2, -1
                            Next
                        Next
                    Next

                    For I = 1 To 32:
                        For I1 = 0 To 1:
                            Print #2, "XXX"
                        Next
                    Next

                    For I = SG% To SG% + 31
                        Print #2, scheduleNG%(I, 12)
                    Next

                    Close 2

                    Call COLHOOP

                    Call ReadTourneyFile(tourneyFile$)

                Case "Q":
                    'Exit Sub
            End Select

        Loop Until UCase$(I$) = "Q" Or Count% = 0

    End If 'Done checking for ESC

End Sub


'----------------------------------------
'       ReadTourneyFile Subroutine
'----------------------------------------
'...explanation...
Sub ReadTourneyFile (tourneyFile$)

    Shared homeTeam$(), visitingTeam$(), yearNumber$()
    Shared homeScores(), visitorScores()
    Shared scheduleAP%(), scheduleNG%()

    Open diskPaths$(1) + tourneyFile$ + ".TOU" For Random As #1 Len = 90
    fileLength& = LOF(1)
    Close 1

    scheduleAP%(0) = fileLength& / 90
    BS% = Int((scheduleAP%(0) - 1) / 20)

    For I = 1 To scheduleAP%(0)

        Open diskPaths$(1) + tourneyFile$ + ".TOU" For Random As #1 Len = 90

        For X = 0 To 14
            Field #1, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
        Next

        Field #1, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
        Field #1, 64 As X$, 3 As Q$(20), 3 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
        Field #1, 84 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)

        Get #1, I

        For X = 0 To 14:
            scheduleNG%(I, X) = CVI(Q$(X + 1))
        Next

        For X = 0 To 3:
            yearNumber$(I, X) = Q$(20 + X)
        Next

        visitorScores(I) = CVI(Q$(16))
        homeScores(I) = CVI(Q$(17))
        visitingTeam$(I) = Q$(18)
        homeTeam$(I) = Q$(19)

        For X = 1 To 3:
            scheduleNG%(I, X + 14) = CVI(Q$(X + 23))
        Next

        For X = 2 To 3
            yearNumber$(I, X) = RTrim$(yearNumber$(I, X))
        Next

        Close 1

    Next

End Sub


'----------------------------------------
'       EditGame_TOURNEY Subroutine
'----------------------------------------
'...explanation...
Sub EditGame_TOURNEY ()

    Shared homeTeam$(), visitingTeam$(), yearNumber$()
    Shared scheduleNG%(), scheduleAP%()

    Shared coachMode$(), foulShotOpt$(), location$()
    Shared MO$(), sclockOpt$(), yesNo$()

    Dim displayPause!

    yearNumber$(0, 3) = "10-01-91"
    yearNumber$(0, 2) = "07:30P"

    Do
        140 ' *** EDIT A GAME ***
        Locate 23, 1: Print "ENTER GAME # TO EDIT        "
        Locate 23, 1: Input "ENTER GAME # TO EDIT "; NG%
        'If NG% < 1 Or NG% > scheduleAP%(0) Then 140
    Loop Until NG% >= 1 And NG% <= scheduleAP%(0)

    V$ = visitingTeam$(NG%)
    H$ = homeTeam$(NG%)

    Do

        149 '
        Cls
        Color 2: Print Tab(29); "SELECT GAME OPTIONS"
        Print

        151 '
        Locate 2, 2
		Color 14: Print "     ";
        Color 15: Print "VISITING TEAM: "; V$
        152 '
		Color 14: Print "     ";
        Color 15: Print "HOME TEAM: "; H$
        153 '
		Color 14: Print " (1) ";:
		Color 15: Print MO$(scheduleNG%(NG%, 8))
        154 '
		Color 14: Print " (2) ";:
		Color 15: Print "3-POINT SHOT: "; yesNo$(scheduleNG%(NG%, 3))
        155 '
		Color 14: Print " (3) ";:
		Color 15: Print "SHOT CLOCK "; sclockOpt$(scheduleNG%(NG%, 7))
        156  '
		Color 14: Print " (4) ";:
		Color 15: Print "HOME COURT ADVANTAGE: "; location$(scheduleNG%(NG%, 2))
        157  '
        Color 14: Print " (5) ";:
		Color 15: Print Using "DISPLAY PAUSE IN SECONDS: ##.#  "; scheduleNG%(NG%, 4) / 10
        158  '
		Color 14: Print " (6) ";:
		Color 15: Print "FOULS TO DISQUALIFY: "; scheduleNG%(NG%, 5)
        159  '
		Color 14: Print " (7) ";:
		Color 15: Print "HUMAN COACHES WILL: "; coachMode$(scheduleNG%(NG%, 9))
        162  '
		Color 14: Print " (8) ";:
		Color 15: Print "FREE THROWS ON 10TH FOUL "; foulShotOpt$(scheduleNG%(NG%, 6))
        164  '
		Color 14: Print " (9) ";:
		Color 15: Print "3-FT WHEN FOULED ON 3-PT ATTEMPT "; yesNo$(scheduleNG%(NG%, 10))
        165  '
		Color 14: Print " (A) ";:
		Color 15: Print "1 FT ON 1ST 6 FLOOR FOULS (PRIOR TO 1973): "; yesNo$(scheduleNG%(NG%, 14))
        163  '
        Color 14: Print " (0) ";:
		Color 15: Print "ACCEPT SETTINGS"

        Do
            I$ = GetKeyPress$
            If UCase$(I$) = "A" Then I$ = "10"
            I8% = Val(I$)
        Loop Until I8% >= 0 And I8% <= 10

        Select Case I8%
            
            Case 1:
                scheduleNG%(NG%, 8) = scheduleNG%(NG%, 8) + 1
                If scheduleNG%(NG%, 8) > 3 Then scheduleNG%(NG%, 8) = 0
                GoTo 151

            Case 2:
                scheduleNG%(NG%, 3) = 1 - scheduleNG%(NG%, 3)
                GoTo 154

            Case 3:
                scheduleNG%(NG%, 7) = 1 + scheduleNG%(NG%, 7)
                If scheduleNG%(NG%, 7) > 2 Then scheduleNG%(NG%, 7) = 0
                GoTo 155

            Case 4:
                N = 1 - (scheduleNG%(NG%, 2) - 1)
                scheduleNG%(NG%, 2) = N + 1
                GoTo 156

            Case 5:
                Locate 8, 32: Input displayPause!
                scheduleNG%(NG%, 4) = displayPause! * 10
                GoTo 157

            Case 6:
                scheduleNG%(NG%, 5) = 1 + scheduleNG%(NG%, 5)
                If scheduleNG%(NG%, 5) > 6 Then scheduleNG%(NG%, 5) = 5
                GoTo 158

            Case 7:
                scheduleNG%(NG%, 9) = 1 - scheduleNG%(NG%, 9)
                GoTo 159

            Case 8:
                scheduleNG%(NG%, 6) = 1 - scheduleNG%(NG%, 6)
                GoTo 162

            Case 9:
                scheduleNG%(NG%, 10) = 1 - scheduleNG%(NG%, 10)
                GoTo 149

            Case 10:
                scheduleNG%(NG%, 14) = 1 - scheduleNG%(NG%, 14)
                GoTo 149

        End Select

    Loop Until I8% = 0

End Sub
