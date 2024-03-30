'----------------------------------------
'        CompileCollege SubRoutine
'----------------------------------------
'This subroutine reads in existing records,
'looks for stats matching each team in the
'conference, and then relies on subroutines
'to read stats and compare to any existing
'leaders and records
Sub CompileCollege ()

    _Title "Courtside College Basketball - Update Conference Stats"

    Shared leaderVals!(), plyrRecords!()
    Shared defLeaders!(), defRecords!()
    Shared offLeaders!(), offRecords!()
    Shared O1!(), O2!(), O3!(), O4!(), O5!(), O6!()

    Shared deffLeaderNames$(), offLeaderNames$(), statFiles$()
    Shared defRecDesc$(), offRecDesc$()
    Shared plyrRecDesc$()
    Shared Z0$(), Z1$(), Z3$()

    Shared confWins, confLosses, fullWins, fullLosses

    'Read in any existing records first
    If _FileExists(diskPaths$(1) + "RECORDS.CBB") Then

        Open diskPaths$(1) + "RECORDS.CBB" For Input As #1

        For I = 1 To 25
            Input #1, plyrRecords!(I)
            Input #1, plyrRecDesc$(I, 0)
            Input #1, plyrRecDesc$(I, 1)
        Next

        For I = 1 To 21
            Input #1, offRecords!(I)
            Input #1, offRecDesc$(I)
        Next

        For I = 1 To 21
            Input #1, defRecords!(I)
            Input #1, defRecDesc$(I)
        Next

        Close #1

    Else

        Erase plyrRecords!, offRecords!, defRecords!
        Erase plyrRecDesc$, offRecDesc$, defRecDesc$

    End If

    'Initialize values first

    'These are defensive record values, so they should be really high
    'To determine if a "record" was broken, the stat values will be
    'compared to see if they are lower than this.
    'If we initialized to 0, the "record" would always be 0
    offRecords!(14) = 19999
    offRecords!(18) = 19999

    For I = 1 To 13:
        defRecords!(I) = 19999
    Next

    For I = 15 To 17:
        defRecords!(I) = 19999
    Next

    For I = 19 To 20:
        defRecords!(I) = 19999
    Next

    'These are defensive record values, so they should be really high
    'To determine if a "record" was broken, the stat values will be
    'compared to see if they are lower than this.
    'If we initialized to 0, the "record" would always be 0
    Erase defLeaders!, offLeaders!

    For X = 1 To MAX_CONFERENCES
        defLeaders!(X, 19) = 9999
    Next

    Erase leaderVals!
    Erase O1!, O2!, O3!, O4!, O5!, O6!
    Erase deffLeaderNames$, offLeaderNames$
    Erase Z0$, Z1$, Z3$

    C$ = ","

    conferenceFile$ = ChooseFile$(diskPaths$(2) + "*.LIB", "CONFERENCE", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Do
            Color 15, 0
            Cls
            Print "WOULD YOU LIKE TO POST SEASON RECORDS? (Y/N)";
            I$ = GetKeyPress$
            finalUpdate$ = UCase$(I$)
        Loop Until finalUpdate$ = "Y" Or finalUpdate$ = "N"

        Open diskPaths$(2) + conferenceFile$ + ".TMS" For Input As #2

        Input #2, LG$
        Input #2, div1$
        Input #2, div2$

        For XW = 1 To TEAMS_PER_CONFERENCE
            Input #2, statFiles$(XW)
            Z1$(XW) = statFiles$(XW)
        Next

        Close #2

        For XW = 1 To TEAMS_PER_CONFERENCE

            'Look for * as terminating char
            If statFiles$(XW) <> Chr$(42) Then

                haveStats = FindStatsCon%(statFiles$(XW), numberGames)

                If (haveStats) Then

                    L = Len(statFiles$(XW))
                    WT$ = ""

                    For X = 1 To L
                        A = Asc(Mid$(statFiles$(XW), X, 1))
                        WT$ = WT$ + Chr$(A)
                    Next X

                    nonConfGame = 0

                    For X = 1 To TEAMS_PER_CONFERENCE

                        If Z1$(X) = WT$ Then

                            If O1!(X) = fullWins And O2!(X) = fullLosses Then

                                nonConfGame = 1
                                Exit For

                            Else

                                O1!(X) = fullWins
                                O2!(X) = fullLosses

                                O4!(X) = confWins
                                O5!(X) = confLosses

                                If O1!(X) + O2!(X) = 0 Then
                                    O3!(X) = 0
                                Else
                                    O3!(X) = O1!(X) / (O1!(X) + O2!(X))
                                End If

                                If O4!(X) + O5!(X) = 0 Then
                                    O6!(X) = 0
                                Else
                                    O6!(X) = O4!(X) / (O4!(X) + O5!(X))
                                End If

                            End If

                        End If

                    Next X

                    If nonConfGame <> 1 And _FileExists(diskPaths$(1) + statFiles$(XW)) Then

                        Call ReadStatsConf(statFiles$(XW), TN$)
                        Call CalcStatTotalsConf(numberGames, TN$, finalUpdate$)
                        Call CalcStatLeadersConf(TN$)

                    End If

                End If 'Done checking if we have stats

            End If 'Done checking valid conference

        Next XW 'Done "checking stats"

        Call SortStandingsConference
        Call PostLeadersConf(conferenceFile$)
        Call PostRecordsColl

    End If

End Sub


'----------------------------------------
'       ReadStatsConf Subroutine
'----------------------------------------
'This subroutine reads stats in for a
'single team in the conference
Sub ReadStatsConf (statFile$, teamName$)

    Shared compS!(), defTotals!(), offTotals!()
    Shared statsW0!(), statsW1!()

    Shared statPlyrNames$()

    Erase compS!, defTotals!, offTotals!
    Erase statsW0!, statsW1!
    Erase statPlyrNames$

    Open diskPaths$(1) + statFile$ For Input As #1

    Input #1, teamName$

    For I = 0 To 13

        Input #1, statPlyrNames$(I)

        For J = 0 To 13:
            Input #1, compS!(I, J)
        Next

        Input #1, statsW0!(I)

        Input #1, statsW1!(I)

    Next

    For L = 1 To 16:
        Input #1, offTotals!(L)
    Next

    For L = 1 To 16:
        Input #1, defTotals!(L)
    Next

    Close #1

End Sub


'----------------------------------------
'        CalcStatLeadersConf Subroutine
'----------------------------------------
'This subroutine compares pre-loaded stats
'to existing leaders and updates as needed
Sub CalcStatLeadersConf (teamName$)

    Shared defTotals!(), offTotals!()
    Shared defLeaders!(), offLeaders!()
    Shared leaderVals!(), compS!()
    Shared statsW0!(), statsW1!()

    Shared deffLeaderNames$(), offLeaderNames$()
    Shared statPlyrNames$()
    Shared Z0$(), Z3$()

    For X = 0 To 14:

        'Color 14
        Print "CHECKING STATS OF ";
        Color 15
        Print statPlyrNames$(X)

        'Division 1, 2 Record
        If compS!(X, 22) > leaderVals!(20, 1) Then

            leaderVals!(20, 1) = compS!(X, 22)
            leaderVals!(20, 2) = 0
            Z0$(20) = statPlyrNames$(X)
            Z3$(20) = teamName$

            For Z = 1 To 19:
                For Z1 = Z + 1 To 20:

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Division 3, 4 Record
        If compS!(X, 12) > leaderVals!(40, 1) Then

            leaderVals!(40, 1) = compS!(X, 12)
            leaderVals!(40, 2) = 0
            Z0$(40) = statPlyrNames$(X)
            Z3$(40) = teamName$

            For Z = 21 To 39
                For Z1 = Z + 1 To 40

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Field Goal %
        If compS!(X, 19) > leaderVals!(60, 1) Then

            leaderVals!(60, 1) = compS!(X, 19)
            leaderVals!(60, 2) = compS!(X, 1)
            Z0$(60) = statPlyrNames$(X)
            Z3$(60) = teamName$

            For Z = 41 To 59
                For Z1 = Z + 1 To 60

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Free Throw %
        If compS!(X, 20) > leaderVals!(80, 1) Then

            leaderVals!(80, 1) = compS!(X, 20)
            leaderVals!(80, 2) = compS!(X, 3)
            Z0$(80) = statPlyrNames$(X)
            Z3$(80) = teamName$

            For Z = 61 To 79
                For Z1 = Z + 1 To 80

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Steals Per Game
        If compS!(X, 15) > leaderVals!(90, 1) Then

            leaderVals!(90, 1) = compS!(X, 15)
            leaderVals!(90, 2) = compS!(X, 7)
            Z0$(90) = statPlyrNames$(X)
            Z3$(90) = teamName$

            For Z = 81 To 89
                For Z1 = Z + 1 To 90

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Steals
        If compS!(X, 7) > leaderVals!(100, 1) Then

            leaderVals!(100, 1) = compS!(X, 7)
            leaderVals!(100, 2) = 0
            Z0$(100) = statPlyrNames$(X)
            Z3$(100) = teamName$

            For Z = 91 To 99
                For Z1 = Z + 1 To 100

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Assist Per Game
        If compS!(X, 16) > leaderVals!(110, 1) Then

            leaderVals!(110, 1) = compS!(X, 16)
            leaderVals!(110, 2) = compS!(X, 8)
            Z0$(110) = statPlyrNames$(X)
            Z3$(110) = teamName$

            For Z = 101 To 109
                For Z1 = Z + 1 To 110

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Assists
        If compS!(X, 8) > leaderVals!(120, 1) Then

            leaderVals!(120, 1) = compS!(X, 8)
            leaderVals!(120, 2) = compS!(X, 8)
            Z0$(120) = statPlyrNames$(X)
            Z3$(120) = teamName$

            For Z = 111 To 119
                For Z1 = Z + 1 To 120

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        '3 Field Goal %
        If compS!(X, 21) > leaderVals!(130, 1) Then

            leaderVals!(130, 1) = compS!(X, 21)
            leaderVals!(130, 2) = statsW1!(X)
            Z0$(130) = statPlyrNames$(X)
            Z3$(130) = teamName$

            For Z = 121 To 129
                For Z1 = Z + 1 To 130

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        '3 Field Goals Made
        If statsW0!(X) > leaderVals!(140, 1) Then

            leaderVals!(140, 1) = statsW0!(X)
            leaderVals!(140, 2) = 0
            Z0$(140) = statPlyrNames$(X)
            Z3$(140) = teamName$

            For Z = 131 To 139
                For Z1 = Z + 1 To 140

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Minutes Per Game
        If compS!(X, 18) > leaderVals!(150, 1) Then

            leaderVals!(150, 1) = compS!(X, 18)
            leaderVals!(150, 2) = 0
            Z0$(150) = statPlyrNames$(X)
            Z3$(150) = teamName$

            For Z = 141 To 149
                For Z1 = Z + 1 To 150

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Total Minutes
        If compS!(X, 10) / 60 > leaderVals!(160, 1) Then

            leaderVals!(160, 1) = compS!(X, 10) / 60
            leaderVals!(160, 2) = 0
            Z0$(160) = statPlyrNames$(X)
            Z3$(160) = teamName$

            For Z = 151 To 159
                For Z1 = Z + 1 To 160

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Rebounds Per Game
        If compS!(X, 13) > leaderVals!(170, 1) Then

            leaderVals!(170, 1) = compS!(X, 13)
            leaderVals!(170, 2) = compS!(X, 5)
            Z0$(170) = statPlyrNames$(X)
            Z3$(170) = teamName$

            For Z = 161 To 169
                For Z1 = Z + 1 To 170

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Rebounds
        If compS!(X, 5) > leaderVals!(180, 1) Then

            leaderVals!(180, 1) = compS!(X, 5)
            leaderVals!(18, 2) = 0
            Z0$(180) = statPlyrNames$(X)
            Z3$(180) = teamName$

            For Z = 171 To 179
                For Z1 = Z + 1 To 180

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Blocks Per Game
        If compS!(X, 17) > leaderVals!(190, 1) Then

            leaderVals!(190, 1) = compS!(X, 17)
            leaderVals!(190, 2) = 0
            Z0$(190) = statPlyrNames$(X)
            Z3$(190) = teamName$

            For Z = 181 To 189
                For Z1 = Z + 1 To 190

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Blocks
        If compS!(X, 9) > leaderVals!(200, 1) Then

            leaderVals!(200, 1) = compS!(X, 9)
            leaderVals!(200, 2) = 0
            Z0$(200) = statPlyrNames$(X)
            Z3$(200) = teamName$

            For Z = 191 To 199
                For Z1 = Z + 1 To 200

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Field Goals Made
        If compS!(X, 2) > leaderVals!(210, 1) Then

            leaderVals!(210, 1) = compS!(X, 2)
            leaderVals!(210, 2) = 0
            Z0$(210) = statPlyrNames$(X)
            Z3$(210) = teamName$

            For Z = 201 To 209
                For Z1 = Z + 1 To 210

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Free Throws Made
        If compS!(X, 4) > leaderVals!(220, 1) Then

            leaderVals!(220, 1) = compS!(X, 4)
            leaderVals!(220, 2) = 0
            Z0$(220) = statPlyrNames$(X)
            Z3$(220) = teamName$

            For Z = 211 To 219
                For Z1 = Z + 1 To 220

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Fouls Per Game
        If compS!(X, 14) > leaderVals!(230, 1) Then

            leaderVals!(230, 1) = compS!(X, 14)
            leaderVals!(230, 2) = 0
            Z0$(230) = statPlyrNames$(X)
            Z3$(230) = teamName$

            For Z = 221 To 229
                For Z1 = Z + 1 To 230

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Fouls
        If compS!(X, 6) > leaderVals!(240, 1) Then

            leaderVals!(240, 1) = compS!(X, 6)
            leaderVals!(240, 2) = 0
            Z0$(240) = statPlyrNames$(X)
            Z3$(240) = teamName$

            For Z = 231 To 239
                For Z1 = Z + 1 To 240

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'MVP Points
        If compS!(X, 24) > leaderVals!(250, 1) Then

            leaderVals!(250, 1) = compS!(X, 24)
            leaderVals!(250, 2) = 0
            Z0$(250) = statPlyrNames$(X)
            Z3$(250) = teamName$

            For Z = 241 To 249
                For Z1 = Z + 1 To 250

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

    Next X

    'Offensive Totals
    If offTotals!(19) > offLeaders!(40, 19) Then

        For M = 1 To 20:
            offLeaders!(40, M) = offTotals!(M)
        Next

        offLeaderNames$(40) = teamName$

        For X = 1 To 39
            For X1 = 1 + X To 40:

                If offLeaders!(X, 19) < offLeaders!(X1, 19) Then
                    Swap offLeaderNames$(X1), offLeaderNames$(X)
                    For M = 1 To 20:
                        Swap offLeaders!(X1, M), offLeaders!(X, M)
                    Next
                End If

            Next X1
        Next X

    End If

    'Defensive Totals
    If defTotals!(19) < defLeaders!(40, 19) Then

        For M = 1 To 20:
            defLeaders!(40, M) = defTotals!(M)
        Next

        deffLeaderNames$(40) = teamName$

        For X = 1 To 39
            For X1 = 1 + X To 40

                If defLeaders!(X, 19) > defLeaders!(X1, 19) Then
                    Swap deffLeaderNames$(X1), deffLeaderNames$(X)
                    For M = 1 To 20:
                        Swap defLeaders!(X1, M), defLeaders!(X, M)
                    Next
                End If

            Next X1
        Next X

    End If

End Sub


'----------------------------------------
'     CalcStatTotalsConf Subroutine
'----------------------------------------
'This subroutine compares all pre-loaded
'stats to each of the record categories to
'see if any should be updated
Sub CalcStatTotalsConf (numberGames, teamName$, finalUpdate$)

    Shared plyrRecords!(), compS!(), defTotals!(), offTotals!()
    Shared offRecords!(), defRecords!()
    Shared statsW0!(), statsW1!()

    Shared plyrRecDesc$(), statPlyrNames$()
    Shared offRecDesc$(), defRecDesc$()

    Shared fullWins, fullLosses

    For I = 0 To 13

        compS!(I, 25) = compS!(I, 13)
        compS!(I, 26) = compS!(I, 12)
        compS!(I, 12) = compS!(I, 2) * 2 + statsW0!(I) + compS!(I, 4) ' *** PTS ***

        If compS!(I, 0) <> 0 Then
            compS!(I, 13) = compS!(I, 5) / compS!(I, 0) ' *** RB/G ***
            compS!(I, 14) = compS!(I, 6) / compS!(I, 0) ' *** FOULS/G ***
            compS!(I, 15) = compS!(I, 7) / compS!(I, 0) ' *** STEALS/G ***
            compS!(I, 16) = compS!(I, 8) / compS!(I, 0) ' *** ASSISTS/G ***
            compS!(I, 17) = compS!(I, 9) / compS!(I, 0) ' *** BLOCKS/G ***
            compS!(I, 24) = compS!(I, 5) + compS!(I, 12) - compS!(I, 6) + compS!(I, 8) + compS!(I, 9) + compS!(I, 7) - 2 * compS!(I, 11)
            compS!(I, 18) = compS!(I, 10) / 60 / compS!(I, 0) ' *** MIN/G ***

            If Not (compS!(I, 1) = 0 Or (compS!(I, 2) / numberGames < 5)) Then
                compS!(I, 19) = compS!(I, 2) / compS!(I, 1) ' *** FG% ***
            End If

            If Not (compS!(I, 3) = 0 Or (compS!(I, 4) / numberGames) < 2.5) Then
                compS!(I, 20) = compS!(I, 4) / compS!(I, 3) ' *** FT% ***
            End If

            If Not (statsW1!(I) = 0 Or statsW0!(I) / numberGames < 1.5) Then
                compS!(I, 21) = statsW0!(I) / statsW1!(I) ' *** 3FG% ***
            End If

            compS!(I, 22) = compS!(I, 12) / compS!(I, 0) ' *** PTS/G ***

            If compS!(I, 0) > plyrRecords!(1) Then plyrRecords!(1) = compS!(I, 0): plyrRecDesc$(1, 0) = statPlyrNames$(I): plyrRecDesc$(1, 1) = teamName$
            If compS!(I, 1) > plyrRecords!(2) Then plyrRecords!(2) = compS!(I, 1): plyrRecDesc$(2, 0) = statPlyrNames$(I): plyrRecDesc$(2, 1) = teamName$
            If compS!(I, 2) > plyrRecords!(3) Then plyrRecords!(3) = compS!(I, 2): plyrRecDesc$(3, 0) = statPlyrNames$(I): plyrRecDesc$(3, 1) = teamName$
            If compS!(I, 19) > plyrRecords!(4) And UCase$(finalUpdate$) = "Y" And compS!(I, 2) / numberGames >= 5 Then plyrRecords!(4) = compS!(I, 19): plyrRecDesc$(4, 0) = statPlyrNames$(I): plyrRecDesc$(4, 1) = teamName$
            If statsW1!(I) > plyrRecords!(5) Then plyrRecords!(5) = statsW1!(I): plyrRecDesc$(5, 0) = statPlyrNames$(I): plyrRecDesc$(5, 1) = teamName$
            If statsW0!(I) > plyrRecords!(6) Then plyrRecords!(6) = statsW0!(I): plyrRecDesc$(6, 0) = statPlyrNames$(I): plyrRecDesc$(6, 1) = teamName$
            If compS!(I, 21) > plyrRecords!(7) And UCase$(finalUpdate$) = "Y" And statsW0!(I) / numberGames >= 1.5 Then plyrRecords!(7) = compS!(I, 21): plyrRecDesc$(7, 0) = statPlyrNames$(I): plyrRecDesc$(7, 1) = teamName$
            If compS!(I, 3) > plyrRecords!(8) Then plyrRecords!(8) = compS!(I, 3): plyrRecDesc$(8, 0) = statPlyrNames$(I): plyrRecDesc$(8, 1) = teamName$
            If compS!(I, 4) > plyrRecords!(9) Then plyrRecords!(9) = compS!(I, 4): plyrRecDesc$(9, 0) = statPlyrNames$(I): plyrRecDesc$(9, 1) = teamName$
            If compS!(I, 20) > plyrRecords!(10) And UCase$(finalUpdate$) = "Y" And compS!(I, 4) / numberGames >= 2.5 Then plyrRecords!(10) = compS!(I, 20): plyrRecDesc$(10, 0) = statPlyrNames$(I): plyrRecDesc$(10, 1) = teamName$
            If compS!(I, 10) / 60 > plyrRecords!(11) Then plyrRecords!(11) = compS!(I, 10) / 60: plyrRecDesc$(11, 0) = statPlyrNames$(I): plyrRecDesc$(11, 1) = teamName$
            If compS!(I, 12) > plyrRecords!(12) Then plyrRecords!(12) = compS!(I, 12): plyrRecDesc$(12, 0) = statPlyrNames$(I): plyrRecDesc$(12, 1) = teamName$
            If compS!(I, 22) > plyrRecords!(13) And UCase$(finalUpdate$) = "Y" Then plyrRecords!(13) = compS!(I, 22): plyrRecDesc$(13, 0) = statPlyrNames$(I): plyrRecDesc$(13, 1) = teamName$
            If compS!(I, 25) > plyrRecords!(14) Then plyrRecords!(14) = compS!(I, 25): plyrRecDesc$(14, 0) = statPlyrNames$(I): plyrRecDesc$(14, 1) = teamName$
            If compS!(I, 26) > plyrRecords!(15) Then plyrRecords!(15) = compS!(I, 26): plyrRecDesc$(15, 0) = statPlyrNames$(I): plyrRecDesc$(15, 1) = teamName$
            If compS!(I, 5) - compS!(I, 26) > plyrRecords!(16) Then plyrRecords!(16) = compS!(I, 5) - compS!(I, 26): plyrRecDesc$(16, 0) = statPlyrNames$(I): plyrRecDesc$(16, 1) = teamName$
            If compS!(I, 5) > plyrRecords!(17) Then plyrRecords!(17) = compS!(I, 5): plyrRecDesc$(17, 0) = statPlyrNames$(I): plyrRecDesc$(17, 1) = teamName$
            If compS!(I, 13) > plyrRecords!(18) And UCase$(finalUpdate$) = "Y" Then plyrRecords!(18) = compS!(I, 13): plyrRecDesc$(18, 0) = statPlyrNames$(I): plyrRecDesc$(18, 1) = teamName$
            If compS!(I, 6) > plyrRecords!(19) Then plyrRecords!(19) = compS!(I, 6): plyrRecDesc$(19, 0) = statPlyrNames$(I): plyrRecDesc$(19, 1) = teamName$
            If compS!(I, 7) > plyrRecords!(20) Then plyrRecords!(20) = compS!(I, 7): plyrRecDesc$(20, 0) = statPlyrNames$(I): plyrRecDesc$(20, 1) = teamName$
            If compS!(I, 8) > plyrRecords!(21) Then plyrRecords!(21) = compS!(I, 8): plyrRecDesc$(21, 0) = statPlyrNames$(I): plyrRecDesc$(21, 1) = teamName$
            If compS!(I, 9) > plyrRecords!(22) Then plyrRecords!(22) = compS!(I, 9): plyrRecDesc$(22, 0) = statPlyrNames$(I): plyrRecDesc$(22, 1) = teamName$
            If compS!(I, 11) > plyrRecords!(23) Then plyrRecords!(23) = compS!(I, 11): plyrRecDesc$(23, 0) = statPlyrNames$(I): plyrRecDesc$(23, 1) = teamName$
            If compS!(I, 18) > plyrRecords!(24) And UCase$(finalUpdate$) = "Y" Then plyrRecords!(24) = compS!(I, 18): plyrRecDesc$(24, 0) = statPlyrNames$(I): plyrRecDesc$(24, 1) = teamName$
            If compS!(I, 24) > plyrRecords!(25) Then plyrRecords!(25) = compS!(I, 24): plyrRecDesc$(25, 0) = statPlyrNames$(I): plyrRecDesc$(25, 1) = teamName$

        End If

    Next

    offTotals!(16) = numberGames
    defTotals!(16) = numberGames
    offTotals!(17) = offTotals!(3) / offTotals!(2)
    defTotals!(17) = defTotals!(3) / defTotals!(2)
    offTotals!(18) = offTotals!(5) / offTotals!(4)
    defTotals!(18) = defTotals!(5) / defTotals!(4)
    offTotals!(20) = offTotals!(11)
    defTotals!(20) = defTotals!(11)

    If offTotals!(15) <> 0 Then offTotals!(21) = offTotals!(14) / offTotals!(15) Else offTotals!(21) = 0
    If defTotals!(15) <> 0 Then defTotals!(21) = defTotals!(14) / defTotals!(15) Else defTotals!(21) = 999

    offTotals!(19) = offTotals!(20) / numberGames
    defTotals!(19) = defTotals!(20) / numberGames

    If offTotals!(2) > offRecords!(1) Then offRecords!(1) = offTotals!(2): offRecDesc$(1) = teamName$
    If offTotals!(3) > offRecords!(2) Then offRecords!(2) = offTotals!(3): offRecDesc$(2) = teamName$
    If offTotals!(17) > offRecords!(3) And UCase$(finalUpdate$) = "Y" Then offRecords!(3) = offTotals!(17): offRecDesc$(3) = teamName$
    If offTotals!(15) > offRecords!(4) Then offRecords!(4) = offTotals!(15): offRecDesc$(4) = teamName$
    If offTotals!(14) > offRecords!(5) Then offRecords!(5) = offTotals!(14): offRecDesc$(5) = teamName$
    If offTotals!(21) > offRecords!(6) And UCase$(finalUpdate$) = "Y" Then offRecords!(6) = offTotals!(21): offRecDesc$(6) = teamName$
    If offTotals!(4) > offRecords!(7) Then offRecords!(7) = offTotals!(4): offRecDesc$(7) = teamName$
    If offTotals!(5) > offRecords!(8) Then offRecords!(8) = offTotals!(5): offRecDesc$(8) = teamName$
    If offTotals!(18) > offRecords!(9) And UCase$(finalUpdate$) = "Y" Then offRecords!(9) = offTotals!(18): offRecDesc$(9) = teamName$
    If offTotals!(13) > offRecords!(10) Then offRecords!(10) = offTotals!(13): offRecDesc$(10) = teamName$
    If offTotals!(6) - offTotals!(13) > offRecords!(11) Then offRecords!(11) = offTotals!(6) - offTotals!(13): offRecDesc$(11) = teamName$
    If offTotals!(6) > offRecords!(12) Then offRecords!(12) = offTotals!(6): offRecDesc$(12) = teamName$
    If offTotals!(6) / numberGames > offRecords!(13) And UCase$(finalUpdate$) = "Y" Then offRecords!(13) = offTotals!(6) / numberGames: offRecDesc$(13) = teamName$
    If offTotals!(7) < offRecords!(14) And UCase$(finalUpdate$) = "Y" Then offRecords!(14) = offTotals!(7): offRecDesc$(14) = teamName$
    If offTotals!(8) > offRecords!(15) Then offRecords!(15) = offTotals!(8): offRecDesc$(15) = teamName$
    If offTotals!(9) > offRecords!(16) Then offRecords!(16) = offTotals!(9): offRecDesc$(16) = teamName$
    If offTotals!(10) > offRecords!(17) Then offRecords!(17) = offTotals!(10): offRecDesc$(17) = teamName$
    If offTotals!(12) < offRecords!(18) And UCase$(finalUpdate$) = "Y" Then offRecords!(18) = offTotals!(12): offRecDesc$(18) = teamName$
    If offTotals!(11) > offRecords!(19) Then offRecords!(19) = offTotals!(11): offRecDesc$(19) = teamName$
    If offTotals!(11) / numberGames > offRecords!(20) And UCase$(finalUpdate$) = "Y" Then offRecords!(20) = offTotals!(11) / numberGames: offRecDesc$(20) = teamName$
    If fullWins > offRecords!(21) Then offRecords!(21) = fullWins: offRecDesc$(21) = teamName$

    If defTotals!(2) < defRecords!(1) Then defRecords!(1) = defTotals!(2): defRecDesc$(1) = teamName$
    If defTotals!(3) < defRecords!(2) Then defRecords!(2) = defTotals!(3): defRecDesc$(2) = teamName$
    If defTotals!(17) < defRecords!(3) And UCase$(finalUpdate$) = "Y" Then defRecords!(3) = defTotals!(17): defRecDesc$(3) = teamName$
    If defTotals!(15) < defRecords!(4) Then defRecords!(4) = defTotals!(15): defRecDesc$(4) = teamName$
    If defTotals!(14) < defRecords!(5) Then defRecords!(5) = defTotals!(14): defRecDesc$(5) = teamName$
    If defTotals!(21) < defRecords!(6) And defTotals!(15) <> 0 And UCase$(finalUpdate$) = "Y" Then defRecords!(6) = defTotals!(21): defRecDesc$(6) = teamName$
    If defTotals!(4) < defRecords!(7) Then defRecords!(7) = defTotals!(4): defRecDesc$(7) = teamName$
    If defTotals!(5) < defRecords!(8) Then defRecords!(8) = defTotals!(5): defRecDesc$(8) = teamName$
    If defTotals!(18) < defRecords!(9) And UCase$(finalUpdate$) = "Y" Then defRecords!(9) = defTotals!(18): defRecDesc$(9) = teamName$
    If defTotals!(13) < defRecords!(10) Then defRecords!(10) = defTotals!(13): defRecDesc$(10) = teamName$
    If defTotals!(6) - defTotals!(13) < defRecords!(11) Then defRecords!(11) = defTotals!(6) - defTotals!(13): defRecDesc$(11) = teamName$
    If defTotals!(6) < defRecords!(12) Then defRecords!(12) = defTotals!(6): defRecDesc$(12) = teamName$
    If defTotals!(6) / numberGames < defRecords!(13) And UCase$(finalUpdate$) = "Y" Then defRecords!(13) = defTotals!(6) / numberGames: defRecDesc$(13) = teamName$
    If defTotals!(7) > defRecords!(14) And UCase$(finalUpdate$) = "Y" Then defRecords!(14) = defTotals!(7): defRecDesc$(14) = teamName$
    If defTotals!(8) < defRecords!(15) Then defRecords!(15) = defTotals!(8): defRecDesc$(15) = teamName$
    If defTotals!(9) < defRecords!(16) Then defRecords!(16) = defTotals!(9): defRecDesc$(16) = teamName$
    If defTotals!(10) < defRecords!(17) Then defRecords!(17) = defTotals!(10): defRecDesc$(17) = teamName$
    If defTotals!(12) > defRecords!(18) And UCase$(finalUpdate$) = "Y" Then defRecords!(18) = defTotals!(12): defRecDesc$(18) = teamName$
    If defTotals!(11) < defRecords!(19) Then defRecords!(19) = defTotals!(11): defRecDesc$(19) = teamName$
    If defTotals!(11) / numberGames < defRecords!(20) And UCase$(finalUpdate$) = "Y" Then defRecords!(20) = defTotals!(11) / numberGames: defRecDesc$(20) = teamName$
    If fullLosses > defRecords!(21) Then defRecords!(21) = fullLosses: defRecDesc$(21) = teamName$

End Sub


'----------------------------------------
'   SortStandingsConference Subroutine
'----------------------------------------
'This subroutine sorts the leaders in the
'target conference. Currently setup using
'a bubble sort algorithm.
Sub SortStandingsConference

    Shared O1!(), O2!(), O3!(), O4!(), O5!(), O6!()

    Shared Z1$()

    'Currently sorts by Conference Record
    '   To sort by Season Record, change O4 / O5 to O1 / O2
    '   (not in the Swap statements!)

    For Z = 0 To 1

        AA = (Z * 15) + 1
        ZZ = (Z * 15) + 15

        For X = AA To ZZ - 1

            For Y = X + 1 To ZZ

                diffX = O4!(X) - O5!(X)
                diffY = O4!(Y) - O5!(Y)

                If diffX <= diffY Then
                    If diffX <> diffY Then
                        If Not (O4!(X) <= O4!(Y) And O4!(X) > O5!(X)) Or Not (O4!(X) >= O4!(Y) And O4!(X) < O5!(X)) Then
                            Swap Z1$(X), Z1$(Y)
                            Swap O1!(X), O1!(Y)
                            Swap O2!(X), O2!(Y)
                            Swap O3!(X), O3!(Y)
                            Swap O4!(X), O4!(Y)
                            Swap O5!(X), O5!(Y)
                            Swap O6!(X), O6!(Y)
                        End If
                    End If
                End If

            Next Y

        Next X

    Next Z

End Sub


'----------------------------------------
'        PostLeadersConf Subroutine
'----------------------------------------
'This subroutine opens the existing leader
'and record files and writes the updated
'stats to the file
Sub PostLeadersConf (conferenceName$)

    Shared deffLeaderNames$(), offLeaderNames$()
    Shared defLeaders!(), offLeaders!()
    Shared leaderVals!()
    Shared O1!(), O2!(), O3!(), O4!(), O5!(), O6!()

    Shared Z0$(), Z1$(), Z3$()

    Cls

    Print

    Open diskPaths$(2) + conferenceName$ + ".LDR" For Output As #1

    Print "    POSTING NEW "; conferenceName$; " CONFERENCE LEADERS"

    For X = 1 To TEAMS_PER_CONFERENCE
        Write #1, Z1$(X), O1!(X), O2!(X), O3!(X), O4!(X), O5!(X), O6!(X)
    Next

    For X = 1 To 250
        Write #1, Z0$(X), Z3$(X), leaderVals!(X, 1), leaderVals!(X, 2)
    Next X

    For X = 1 To MAX_CONFERENCES:
        Write #1, offLeaderNames$(X)
        For Y = 1 To 20:
            Write #1, offLeaders!(X, Y)
        Next
    Next

    For X = 1 To MAX_CONFERENCES:
        Write #1, deffLeaderNames$(X):
        For Y = 1 To 20:
            Write #1, defLeaders!(X, Y)
        Next
    Next

    Close #1

End Sub


'----------------------------------------
'         PostRecordsColl Subroutine
'----------------------------------------
'This subroutine writes the calculated
'records to the records file
Sub PostRecordsColl

    Shared defRecords!(), offRecords!(), plyrRecords!()

    Shared plyrRecDesc$(), offRecDesc$(), defRecDesc$()

    Open diskPaths$(1) + "RECORDS.CBB" For Output As #2

    For I = 1 To 25
        Print #2, plyrRecords!(I)
        Print #2, plyrRecDesc$(I, 0)
        Print #2, plyrRecDesc$(I, 1)
    Next

    For I = 1 To 21
        Print #2, offRecords!(I)
        Print #2, offRecDesc$(I)
    Next

    For I = 1 To 21
        Print #2, defRecords!(I)
        Print #2, defRecDesc$(I)
    Next

    Close #2

End Sub


'----------------------------------------
'         FindStatsCon Function
'----------------------------------------
'Looks for schedule stats for a specified
'team in the conference and loads them if
'found, notifying the user
Function FindStatsCon% (statFile$, numberGames)

    Shared oppScore(), teamScore()

    Shared locIndicator$(), oppName$(), statFiles$()

    Shared confWins, confLosses, fullWins, fullLosses

    findFile$ = diskPaths$(1) + statFile$ + "S"
    If Not _FileExists(findFile$) Then

        Print "NO STATS FOUND FOR "; statFile$
        FindStatsCon% = 0

    Else

        Open findFile$ For Input As #1

        Input #1, numberGames

        For I = 1 To MAX_SCHED_STATS:
            Input #1, teamScore(I)
        Next I

        For I = 1 To MAX_SCHED_STATS:
            Input #1, oppName$(I)
        Next I

        For I = 1 To MAX_SCHED_STATS:
            Input #1, oppScore(I)
        Next I

        For I = 1 To MAX_SCHED_STATS:
            Input #1, locIndicator$(I)
        Next I

        Close #1

        confWins = 0
        confLosses = 0

        fullWins = 0
        fullLosses = 0

        'RATINGS
        For I = 1 To numberGames

            statName$ = nameStatFile$(oppName$(I))
            nonConfGame = 0

            For EC = 1 To TEAMS_PER_CONFERENCE
                'If team stat file = opponent of a conference game?
                If statName$ = statFiles$(EC) Then
                    nonConfGame = 1
                    Exit For
                End If

            Next EC

            If nonConfGame <> 1 Then

                If teamScore(I) > oppScore(I) Then
                    fullWins = fullWins + 1
                End If

                If teamScore(I) < oppScore(I) Then
                    fullLosses = fullLosses + 1
                End If

            Else

                If teamScore(I) > oppScore(I) Then
                    confWins = confWins + 1
                    fullWins = fullWins + 1
                End If

                If teamScore(I) < oppScore(I) Then
                    confLosses = confLosses + 1
                    fullLosses = fullLosses + 1
                End If

            End If

            EC = 0

        Next I

        Cls

        Locate 4, 5

        Color 15
        Print "FINDING ";
        Color 14
        Print statFile$;
        Color 15
        Print " STATS            "

        FindStatsCon% = 1

    End If

End Function

