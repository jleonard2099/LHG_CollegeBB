'----------------------------------------
'           TOURNEY Subroutine
'----------------------------------------
'This subroutine lets the user select an
'existing tournament file, reads the data
'in and then gives the user a chance to
'play or edit games. It uses a view similar
'to that of the SCHEDULE routine.
Sub TOURNEY ()

    _Title "Courtside College Basketball - Tournament"

    Shared homeTeam$(), visitingTeam$()
    Shared yearNumber$()
    Shared homeScores(), visitorScores()
    Shared scheduleAP%(), scheduleNG%()

    Shared BS%, NS%

    Dim gameDay(MAX_TEAMS), gameMonth(MAX_TEAMS), gameYear(MAX_TEAMS)

    Shared coachMode$(), foulShotOpt$(), location$()
    Shared modeAbbrev$(), playMode$(), sClockOpt$(), yesNo$()

    'ReDim homeTeam$(MAX_SCHEDULE_GAMES), visitingTeam$(MAX_SCHEDULE_GAMES)

    yearNumber$(0, 3) = "03-01-25"
    yearNumber$(0, 2) = "07:30P"

    tourneyFile$ = _OpenFileDialog$("Choose Tournament file", diskPaths$(1), "*.TOU", "Tournament files [.TOU]")
    'ChooseFile$(diskPaths$(1) + "*.TOU", "TOURNAMENT", ExitCode%)

    If tourneyFile$ <> "" Then

        Color 7, 0
        Cls

        scheduleNG%(0, 2) = 1
        scheduleNG%(0, 3) = 1
        scheduleNG%(0, 5) = 5
        scheduleNG%(0, 7) = 0
        scheduleNG%(0, 8) = 0
        yearNumber$(0, 0) = ""
        yearNumber$(0, 1) = ""

        Erase homeTeam$, visitingTeam$

        For I = 1 To 500
            scheduleNG%(I, 13) = 0
        Next

        Call ReadTourneyFile(tourneyFile$)

        tourneyName$ = GetFileBaseName$(tourneyFile$)

        Do

            Color 15, 0
            Cls

            Locate LAST_ROW - 2, 1
            Color , 1: Print Tab(80); " ";
            Locate LAST_ROW - 2, 1
            Color 13: Print "SC";
            Color 14: Print "-SHOT CLOCK  ";
            Color 13: Print "MA";
            Color 14: Print "-MODE OF PLAY  ";
            Color 11: Print " G";
            Color 14: Print "-GOTO GAME";
            Color 11: Print " B";
            Color 14: Print "-BOXSCORE OPTIONS"

            Locate LAST_ROW - 1, 1
            Color , 1: Print Tab(80); " ";
            Locate LAST_ROW - 1, 1
            Color 13: Print "HC";
            Color 14: Print "-HOME COURT  ";
            Color 13: Print "3";
            Color 14: Print "-USE 3-PT SHOT  ";
            Color 13: Print "REG";
            Color 14: Print "-REGION  ";
            Color 13: Print "RND";
            Color 14: Print "-ROUND  ";
            Color 13: Print "GM";
            Color 14: Print "-GAME  ";
            Color 11: Print "   PGUP  PGDN  ";

            Locate LAST_ROW, 1
            Color , 1: Print Tab(80); " ";
            Locate LAST_ROW, 1
            Color 11: Print "P";
            Color 14: Print "-PLAY TOURNAMENT   ";
            Color 11: Print "E";
            Color 14: Print "-EDIT A GAME  ";
            Color 11: Print "Q";
            Color 14: Print "-QUIT  ";

            Locate 1, 1: Print Tab(80); " ";
            Locate 1, 1: Print tourneyName$
            Locate 1, 33

            Color 14: Print "AUTO SCHEDULER"
            Color , 0

            Locate 2, 1
            Color 14
            Print "GM ID#  REG  RND  GM    VISITING TEAM        HOME TEAM            MA  HC  3  SC": Color 15

            'Show 20 schedule entries on the screen at once
            '-- we could update this to 25
            For X = 1 To 20

                Locate X + 2, 1

                If X + NS% * 20 <= scheduleAP%(0) Then

                    Print Using "###      ##   ##   ##"; X + NS% * 20; scheduleNG%(X + NS% * 20, 15); scheduleNG%(X + NS% * 20, 16); scheduleNG%(X + NS% * 20, 17);

                    'Visiting Team
                    Locate , 25: Print visitingTeam$(X + NS% * 20); " ";
                    If scheduleNG%(X + NS% * 20, 13) = 1 Then Print Using "###"; visitorScores(X + NS% * 20);

                    'Home Team
                    Locate , 46: Print homeTeam$(X + NS% * 20); " ";
                    If scheduleNG%(X + NS% * 20, 13) = 1 Then Print Using "###"; homeScores(X + NS% * 20);

                    'Mode of Play
                    Locate , 67: Print modeAbbrev$(scheduleNG%(X + NS% * 20, 8));

                    'Home Court
                    Locate , 72: If scheduleNG%(X + NS% * 20, 2) = 1 Then Print "Y";: Else Print "N";

                    '3 Pt Option
                    Locate , 75: Print yesNo$(scheduleNG%(X + NS% * 20, 3));

                    'Shot Clock
                    Locate , 78: Print Left$(sClockOpt$(scheduleNG%(X + NS% * 20, 7)), 2)

                End If

            Next

            I$ = GetKeyPress$

            Select Case UCase$(I$)

                Case "B":
                    Do
                        Locate LAST_ROW - 3, 1: Print "ENTER GAME # TO GO TO        "
                        Locate LAST_ROW - 3, 1: Input "ENTER GAME # TO GO TO "; targetGame%
                    Loop Until targetGame% >= 1 And targetGame% <= scheduleAP%(0)
                    NS% = Int((targetGame% - 1) / 20)

                    'boxName$ = diskPaths$(1) + "BOX" + Mid$(Str$(targetGame%), 2, Len(Str$(targetGame%)))
                    boxName$ = diskPaths$(1) + scheduleFile$ + "_GAME" + Mid$(Str$(targetGame%), 2, Len(Str$(targetGame%))) + ".BS"

                    Call EditBoxscore(boxName$)
                    Color 15, 0

                Case "E":
                    Call EditTournamentGame

                Case "G":
                    'GOTO A SPECIFIC PAGE
                    Do
                        Locate LAST_ROW - 3, 1: Print "ENTER GAME # TO GO TO        "
                        Locate LAST_ROW - 3, 1: Input "ENTER GAME # TO GO TO "; targetGame%
                    Loop Until targetGame% >= 1 And targetGame% <= scheduleAP%(0)

                    NS% = Int((targetGame% - 1) / 20)

                Case "P":
                    ' *** SAVE TOURNAMENT SCHEDULE ***
                    Open tourneyFile$ For Random As #1 Len = TOURNEY_SIZE_BYTES

                    For X = 0 To 14
                        Field #1, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
                    Next

                    Field #1, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
                    Field #1, 64 As X$, 5 As Q$(20), 5 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
                    Field #1, 88 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)

                    For I = 1 To scheduleAP%(0)

                        For X = 0 To 14:
                            LSet Q$(X + 1) = MKI$(scheduleNG%(I, X))
                        Next

                        LSet Q$(16) = MKI$(visitorScores(I))
                        LSet Q$(17) = MKI$(homeScores(I))
                        LSet Q$(18) = visitingTeam$(I)
                        LSet Q$(19) = homeTeam$(I)

                        For X = 0 To 3:
                            LSet Q$(20 + X) = yearNumber$(I, X)
                        Next

                        For X = 1 To 3:
                            LSet Q$(X + 23) = MKI$(scheduleNG%(I, X + 14))
                        Next

                        Put #1, I

                    Next

                    Close #1

                    ' *** PLAY GAMES ***
                    Color , 0
                    Cls

                    Call MenuBox(6, 8, 61, 8, 15, 4)

                    Do

                        Locate 8, 12: Print "TOTAL GAMES ="; scheduleAP%(0)
                        Locate 9, 12: Print "SELECT GAME # TO START AND END WITH": Print
                        Locate 10, 12: Input "ENTER START GAME #: "; SG%: Print
                        Locate 11, 12: Input "ENTER END GAME #:"; EG%
                        Locate 13, 12: Print "ANY CHANGE? (Y/N)"

                        I$ = GetKeyPress$

                    Loop Until UCase$(I$) = "N"

                    If SG% <= 0 Then SG% = 1
                    If EG% > scheduleAP%(0) Then EG% = scheduleAP%(0)

                    'Create the Tournament file (equivalent to GAME82)
                    Open "TOURNEY" For Output As #2
                    Print #2, tourneyName$ + ".TOU"
                    Print #2, SG%
                    Print #2, EG%
                    Print #2, 0
                    Close #2

                    For I = 1 To scheduleAP%(0)

                        L = Len(yearNumber$(I, 2))
                        If L = 5 Then HR = Val(Left$(yearNumber$(I, 2), 1)): MIN = Val(Mid$(yearNumber$(I, 2), 3, 2))
                        If L = 6 Then HR = Val(Left$(yearNumber$(I, 2), 2)): MIN = Val(Mid$(yearNumber$(I, 2), 4, 2))

                        gameMonth(I) = Val(Left$(yearNumber$(I, 3), 2))
                        gameDay(I) = Val(Mid$(yearNumber$(I, 3), 4, 2))
                        gameYear(I) = Val(Mid$(yearNumber$(I, 3), 7, 2))

                        T$ = Right$(yearNumber$(I, 2), 1)
                        If UCase$(T$) = "P" And Left$(yearNumber$(I, 2), 2) <> "12" Then HR = HR + 12
                        TM = HR * 60 + MIN
                        scheduleNG%(I, 12) = Int(TM / 30 - 18)

                    Next

                    Open "TICKER" For Output As #3

                    For I = 1 To 32:
                        For I1 = 0 To 1:
                            For I2 = 1 To 14
                                Print #3, -1
                            Next
                        Next
                    Next

                    For I = 1 To 32:
                        For I1 = 0 To 1:
                            Print #3, "XXX"
                        Next
                    Next

                    For I = SG% To SG% + 31
                        Print #3, scheduleNG%(I, 12)
                    Next

                    Close #3

                    Do
                        Call COLHOOP
                    Loop Until schedGame(0) + schedGame(2) > schedGame(1)

                    Call ReadTourneyFile(tourneyFile$)

                Case "Q":
                    'Exit Sub

                Case Chr$(0) + Chr$(81), Chr$(0) + Chr$(77)
                    ' *** PAGE DOWN | RIGHT ARROW ***
                    If NS% >= BS% Then
                        NS% = BS%
                    Else
                        NS% = NS% + 1
                    End If

                Case Chr$(0) + Chr$(73), Chr$(0) + Chr$(75)
                    ' *** PAGE UP | LEFT ARROW ***
                    If NS% <> 0 Then
                        NS% = NS% - 1
                    End If

            End Select

        Loop Until UCase$(I$) = "Q"

    End If 'Done checking for ESC

End Sub


'----------------------------------------
'       ReadTourneyFile Subroutine
'----------------------------------------
'This subroutine actually reads in all of
'the parameters and information for the
'tournament from the chosen file (.TOU)
Sub ReadTourneyFile (tourneyFile$)

    Shared BS%, NS%

    Shared homeTeam$(), visitingTeam$(), yearNumber$()
    Shared homeScores(), visitorScores()
    Shared scheduleAP%(), scheduleNG%()

    Open tourneyFile$ For Random As #1 Len = TOURNEY_SIZE_BYTES
    fileLength& = LOF(1)
    Close #1

    scheduleAP%(0) = fileLength& / TOURNEY_SIZE_BYTES
    BS% = Int((scheduleAP%(0) - 1) / 20)

    For I = 1 To scheduleAP%(0)

        Open tourneyFile$ For Random As #2 Len = TOURNEY_SIZE_BYTES

        For X = 0 To 14
            Field #2, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
        Next

        Field #2, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
        Field #2, 64 As X$, 5 As Q$(20), 5 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
        Field #2, 88 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)

        Get #2, I

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

        Close #2

    Next

End Sub


'----------------------------------------
'       EditTournamentGame Subroutine
'----------------------------------------
'This Subroutine lets the user edit any of
'the existing games for a tournament schedule.
Sub EditTournamentGame ()

    Shared homeTeam$(), visitingTeam$(), yearNumber$()
    Shared scheduleNG%(), scheduleAP%()

    Shared coachMode$(), foulShotOpt$(), location$()
    Shared playMode$(), sClockOpt$(), yesNoText$()

    Dim displayPause!

    yearNumber$(0, 3) = "03-01-25"
    yearNumber$(0, 2) = "07:30P"

    Do
        ' *** EDIT A GAME ***
        Locate LAST_ROW - 3, 1: Print "ENTER GAME # TO EDIT        "
        Locate LAST_ROW - 3, 1: Input "ENTER GAME # TO EDIT "; NG%
    Loop Until NG% >= 1 And NG% <= scheduleAP%(0)

    visitor$ = visitingTeam$(NG%)
    home$ = homeTeam$(NG%)

    Do

        Do

            Cls
            Color 2: Print Tab(29); "SELECT GAME OPTIONS"
            Print

            Locate 2, 2
            Color 14: Print "     ";
            Color 15: Print "VISITING TEAM: "; visitor$

            Color 14: Print "     ";
            Color 15: Print "HOME TEAM: "; home$

            Color 14: Print " (1) ";:
            Color 15: Print playMode$(scheduleNG%(NG%, 8))

            Color 14: Print " (2) ";:
            Color 15: Print "HOME COURT ADVANTAGE: "; location$(scheduleNG%(NG%, 2))

            Color 14: Print " (3) ";:
            Color 15: Print Using "PLAY-BY-PLAY DELAY (SECS): ##.#  "; scheduleNG%(NG%, 4) / 10

            Color 14: Print " (4) ";:
            Color 15: Print "SHOT CLOCK "; sClockOpt$(scheduleNG%(NG%, 7))

            Color 14: Print " (5) ";:
            Color 15: Print "3-POINT SHOT: "; yesNoText$(scheduleNG%(NG%, 3))

            Color 14: Print " (6) ";:
            Color 15: Print "FOULS TO DISQUALIFY: "; scheduleNG%(NG%, 5)

            Color 14: Print " (7) ";:
            Color 15: Print "FREE THROWS ON 10TH FOUL "; foulShotOpt$(scheduleNG%(NG%, 6))

            Color 14: Print " (8) ";:
            Color 15: Print "HUMAN COACHES: "; coachMode$(scheduleNG%(NG%, 9))

            Color 14: Print " (9) ";:
            Color 15: Print "3-FT WHEN FOULED ON 3-PT ATTEMPT "; yesNoText$(scheduleNG%(NG%, 10))

            Color 14: Print " (A) ";:
            Color 15: Print "1 FT ON 1ST 6 FLOOR FOULS: "; yesNoText$(scheduleNG%(NG%, 14))

            Color 14: Print " (0) ";:
            Color 15: Print "ACCEPT SETTINGS"

            I$ = GetKeyPress$
            If UCase$(I$) = "A" Then I$ = "10"
            userKey = Val(I$)

            Select Case userKey

                Case 1:
                    scheduleNG%(NG%, 8) = scheduleNG%(NG%, 8) + 1
                    If scheduleNG%(NG%, 8) > 3 Then scheduleNG%(NG%, 8) = 0

                Case 2:
                    N = 1 - (scheduleNG%(NG%, 2) - 1)
                    scheduleNG%(NG%, 2) = N + 1

                Case 3:
                    Locate 8, 32: Input displayPause!
                    scheduleNG%(NG%, 4) = displayPause! * 10

                Case 4:
                    scheduleNG%(NG%, 7) = 1 + scheduleNG%(NG%, 7)
                    If scheduleNG%(NG%, 7) > 2 Then scheduleNG%(NG%, 7) = 0

                Case 5:
                    scheduleNG%(NG%, 3) = 1 - scheduleNG%(NG%, 3)

                Case 6:
                    scheduleNG%(NG%, 5) = 1 + scheduleNG%(NG%, 5)
                    If scheduleNG%(NG%, 5) > 6 Then scheduleNG%(NG%, 5) = 5

                Case 7:
                    scheduleNG%(NG%, 6) = 1 - scheduleNG%(NG%, 6)

                Case 8:
                    scheduleNG%(NG%, 9) = 1 - scheduleNG%(NG%, 9)

                Case 9:
                    scheduleNG%(NG%, 10) = 1 - scheduleNG%(NG%, 10)

                Case 10:
                    scheduleNG%(NG%, 14) = 1 - scheduleNG%(NG%, 14)

            End Select

        Loop Until userKey >= 0 And userKey <= 10

    Loop Until userKey = 0

End Sub
