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
    Shared compSeasonWins!(), compSeasonLosses!(), compSeasonWinPct!(), compConfWins!(), compConfLosses!(), compConfWinPct!()

    Shared defLeaderNames$(), offLeaderNames$()
    Shared defRecDesc$(), offRecDesc$()
    Shared plyrRecDesc$(), memberName$()
    Shared leaderName$(), leaderTeam$()

    Shared confWins, confLosses, fullWins, fullLosses

    'Read in any existing records first
    If _FileExists(diskPaths$(1) + "RECORDS.CBB") Then

        Call ReadSeasonRecords ("RECORDS.CBB")

    Else

        Erase plyrRecords!, offRecords!, defRecords!
        Erase plyrRecDesc$, offRecDesc$, defRecDesc$

    End If

    'Initialize values first
    Erase defLeaders!, offLeaders!
    Erase leaderVals!
    Erase compSeasonWins!, compSeasonLosses!, compSeasonWinPct!, compConfWins!, compConfLosses!, compConfWinPct!

    Erase defLeaderNames$, offLeaderNames$
    Erase memberName$, leaderName$, leaderTeam$

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

    For X = 1 To MAX_CONFERENCES
        defLeaders!(X, 19) = 9999
    Next

    C$ = ","

    conferenceFile$ = ChooseFile$(diskPaths$(2) + "*.TMI", "CONFERENCE", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Do
            Color 15, 0
            Cls
            Print "WOULD YOU LIKE TO POST SEASON RECORDS? (Y/N)";
            I$ = GetKeyPress$
            finalUpdate$ = UCase$(I$)
        Loop Until finalUpdate$ = "Y" Or finalUpdate$ = "N"

        Call ReadConferenceStats (conferenceFile$)

        For XW = 1 To TEAMS_PER_CONFERENCE

            'Look for * as terminating char
            If memberName$(XW) <> Chr$(42) Then

                haveStats = FindStatsCon%(memberName$(XW), numberGames)

                If (haveStats) Then

                    L = Len(memberName$(XW))
                    WT$ = ""

                    For X = 1 To L
                        A = Asc(Mid$(memberName$(XW), X, 1))
                        WT$ = WT$ + Chr$(A)
                    Next X

                    nonConfGame = 0

                    For X = 1 To TEAMS_PER_CONFERENCE

                        If memberName$(X) = WT$ Then

                            If compSeasonWins!(X) = fullWins And compSeasonLosses!(X) = fullLosses Then

                                nonConfGame = 1
                                Exit For

                            Else

                                compSeasonWins!(X) = fullWins
                                compSeasonLosses!(X) = fullLosses

                                compConfWins!(X) = confWins
                                compConfLosses!(X) = confLosses

                                If compSeasonWins!(X) + compSeasonLosses!(X) = 0 Then
                                    compSeasonWinPct!(X) = 0
                                Else
                                    compSeasonWinPct!(X) = compSeasonWins!(X) / (compSeasonWins!(X) + compSeasonLosses!(X))
                                End If

                                If compConfWins!(X) + compConfLosses!(X) = 0 Then
                                    compConfWinPct!(X) = 0
                                Else
                                    compConfWinPct!(X) = compConfWins!(X) / (compConfWins!(X) + compConfLosses!(X))
                                End If

                            End If

                        End If

                    Next X

                    If nonConfGame <> 1 And _FileExists(diskPaths$(1) + memberName$(XW)) Then

                        Call ReadStatsConf(memberName$(XW), TN$)
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

    Shared compStats!(), defTotals!(), offTotals!()
    Shared statsW0!(), statsW1!()

    Shared statPlyrNames$()

    Erase compStats!, defTotals!, offTotals!
    Erase statsW0!, statsW1!
    Erase statPlyrNames$

    Open diskPaths$(1) + statFile$ For Input As #1

    Input #1, teamName$

    For I = 0 To 13

        Input #1, statPlyrNames$(I)

        For J = 0 To 13:
            Input #1, compStats!(I, J)
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
'     CalcStatLeadersConf Subroutine
'----------------------------------------
'This subroutine compares pre-loaded stats
'to existing leaders and updates as needed
Sub CalcStatLeadersConf (teamName$)

    Shared defTotals!(), offTotals!()
    Shared defLeaders!(), offLeaders!()
    Shared leaderVals!(), compStats!()
    Shared statsW0!(), statsW1!()

    Shared defLeaderNames$(), offLeaderNames$()
    Shared statPlyrNames$()
    Shared leaderName$(), leaderTeam$()

    For X = 0 To 14:

        'Color 14
        Print "CHECKING STATS OF ";
        Color 15
        Print statPlyrNames$(X)

        'Division 1, 2 Record
        If compStats!(X, 22) > leaderVals!(20, 1) Then

            leaderVals!(20, 1) = compStats!(X, 22)
            leaderVals!(20, 2) = 0
            leaderName$(20) = statPlyrNames$(X)
            leaderTeam$(20) = teamName$

            For Z = 1 To 19:
                For Z1 = Z + 1 To 20:

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap leaderName$(Z1), leaderName$(Z)
                        Swap leaderTeam$(Z1), leaderTeam$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Offensive Rebounds
        If compStats!(X, 12) > leaderVals!(40, 1) Then

            leaderVals!(40, 1) = compStats!(X, 12)
            leaderVals!(40, 2) = 0
            leaderName$(40) = statPlyrNames$(X)
            leaderTeam$(40) = teamName$

            For Z = 21 To 39
                For Z1 = Z + 1 To 40

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap leaderName$(Z1), leaderName$(Z)
                        Swap leaderTeam$(Z1), leaderTeam$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Field Goal %
        If compStats!(X, 19) > leaderVals!(60, 1) Then

            leaderVals!(60, 1) = compStats!(X, 19)
            leaderVals!(60, 2) = compStats!(X, 1)
            leaderName$(60) = statPlyrNames$(X)
            leaderTeam$(60) = teamName$

            For Z = 41 To 59
                For Z1 = Z + 1 To 60

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap leaderName$(Z1), leaderName$(Z)
                        Swap leaderTeam$(Z1), leaderTeam$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Free Throw %
        If compStats!(X, 20) > leaderVals!(80, 1) Then

            leaderVals!(80, 1) = compStats!(X, 20)
            leaderVals!(80, 2) = compStats!(X, 3)
            leaderName$(80) = statPlyrNames$(X)
            leaderTeam$(80) = teamName$

            For Z = 61 To 79
                For Z1 = Z + 1 To 80

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap leaderName$(Z1), leaderName$(Z)
                        Swap leaderTeam$(Z1), leaderTeam$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Steals Per Game
        If compStats!(X, 15) > leaderVals!(90, 1) Then

            leaderVals!(90, 1) = compStats!(X, 15)
            leaderVals!(90, 2) = compStats!(X, 7)
            leaderName$(90) = statPlyrNames$(X)
            leaderTeam$(90) = teamName$

            For Z = 81 To 89
                For Z1 = Z + 1 To 90

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap leaderName$(Z1), leaderName$(Z)
                        Swap leaderTeam$(Z1), leaderTeam$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Steals
        If compStats!(X, 7) > leaderVals!(100, 1) Then

            leaderVals!(100, 1) = compStats!(X, 7)
            leaderVals!(100, 2) = 0
            leaderName$(100) = statPlyrNames$(X)
            leaderTeam$(100) = teamName$

            For Z = 91 To 99
                For Z1 = Z + 1 To 100

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap leaderName$(Z1), leaderName$(Z)
                        Swap leaderTeam$(Z1), leaderTeam$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Assist Per Game
        If compStats!(X, 16) > leaderVals!(110, 1) Then

            leaderVals!(110, 1) = compStats!(X, 16)
            leaderVals!(110, 2) = compStats!(X, 8)
            leaderName$(110) = statPlyrNames$(X)
            leaderTeam$(110) = teamName$

            For Z = 101 To 109
                For Z1 = Z + 1 To 110

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap leaderName$(Z1), leaderName$(Z)
                        Swap leaderTeam$(Z1), leaderTeam$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Assists
        If compStats!(X, 8) > leaderVals!(120, 1) Then

            leaderVals!(120, 1) = compStats!(X, 8)
            leaderVals!(120, 2) = compStats!(X, 8)
            leaderName$(120) = statPlyrNames$(X)
            leaderTeam$(120) = teamName$

            For Z = 111 To 119
                For Z1 = Z + 1 To 120

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap leaderName$(Z1), leaderName$(Z)
                        Swap leaderTeam$(Z1), leaderTeam$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        '3 Field Goal %
        If compStats!(X, 21) > leaderVals!(130, 1) Then

            leaderVals!(130, 1) = compStats!(X, 21)
            leaderVals!(130, 2) = statsW1!(X)
            leaderName$(130) = statPlyrNames$(X)
            leaderTeam$(130) = teamName$

            For Z = 121 To 129
                For Z1 = Z + 1 To 130

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap leaderName$(Z1), leaderName$(Z)
                        Swap leaderTeam$(Z1), leaderTeam$(Z)
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
            leaderName$(140) = statPlyrNames$(X)
            leaderTeam$(140) = teamName$

            For Z = 131 To 139
                For Z1 = Z + 1 To 140

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap leaderName$(Z1), leaderName$(Z)
                        Swap leaderTeam$(Z1), leaderTeam$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Minutes Per Game
        If compStats!(X, 18) > leaderVals!(150, 1) Then

            leaderVals!(150, 1) = compStats!(X, 18)
            leaderVals!(150, 2) = 0
            leaderName$(150) = statPlyrNames$(X)
            leaderTeam$(150) = teamName$

            For Z = 141 To 149
                For Z1 = Z + 1 To 150

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap leaderName$(Z1), leaderName$(Z)
                        Swap leaderTeam$(Z1), leaderTeam$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Total Minutes
        If compStats!(X, 10) / 60 > leaderVals!(160, 1) Then

            leaderVals!(160, 1) = compStats!(X, 10) / 60
            leaderVals!(160, 2) = 0
            leaderName$(160) = statPlyrNames$(X)
            leaderTeam$(160) = teamName$

            For Z = 151 To 159
                For Z1 = Z + 1 To 160

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap leaderName$(Z1), leaderName$(Z)
                        Swap leaderTeam$(Z1), leaderTeam$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Rebounds Per Game
        If compStats!(X, 13) > leaderVals!(170, 1) Then

            leaderVals!(170, 1) = compStats!(X, 13)
            leaderVals!(170, 2) = compStats!(X, 5)
            leaderName$(170) = statPlyrNames$(X)
            leaderTeam$(170) = teamName$

            For Z = 161 To 169
                For Z1 = Z + 1 To 170

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap leaderName$(Z1), leaderName$(Z)
                        Swap leaderTeam$(Z1), leaderTeam$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Rebounds
        If compStats!(X, 5) > leaderVals!(180, 1) Then

            leaderVals!(180, 1) = compStats!(X, 5)
            leaderVals!(18, 2) = 0
            leaderName$(180) = statPlyrNames$(X)
            leaderTeam$(180) = teamName$

            For Z = 171 To 179
                For Z1 = Z + 1 To 180

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap leaderName$(Z1), leaderName$(Z)
                        Swap leaderTeam$(Z1), leaderTeam$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Blocks Per Game
        If compStats!(X, 17) > leaderVals!(190, 1) Then

            leaderVals!(190, 1) = compStats!(X, 17)
            leaderVals!(190, 2) = 0
            leaderName$(190) = statPlyrNames$(X)
            leaderTeam$(190) = teamName$

            For Z = 181 To 189
                For Z1 = Z + 1 To 190

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap leaderName$(Z1), leaderName$(Z)
                        Swap leaderTeam$(Z1), leaderTeam$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Blocks
        If compStats!(X, 9) > leaderVals!(200, 1) Then

            leaderVals!(200, 1) = compStats!(X, 9)
            leaderVals!(200, 2) = 0
            leaderName$(200) = statPlyrNames$(X)
            leaderTeam$(200) = teamName$

            For Z = 191 To 199
                For Z1 = Z + 1 To 200

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap leaderName$(Z1), leaderName$(Z)
                        Swap leaderTeam$(Z1), leaderTeam$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Field Goals Made
        If compStats!(X, 2) > leaderVals!(210, 1) Then

            leaderVals!(210, 1) = compStats!(X, 2)
            leaderVals!(210, 2) = 0
            leaderName$(210) = statPlyrNames$(X)
            leaderTeam$(210) = teamName$

            For Z = 201 To 209
                For Z1 = Z + 1 To 210

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap leaderName$(Z1), leaderName$(Z)
                        Swap leaderTeam$(Z1), leaderTeam$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Free Throws Made
        If compStats!(X, 4) > leaderVals!(220, 1) Then

            leaderVals!(220, 1) = compStats!(X, 4)
            leaderVals!(220, 2) = 0
            leaderName$(220) = statPlyrNames$(X)
            leaderTeam$(220) = teamName$

            For Z = 211 To 219
                For Z1 = Z + 1 To 220

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap leaderName$(Z1), leaderName$(Z)
                        Swap leaderTeam$(Z1), leaderTeam$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Fouls Per Game
        If compStats!(X, 14) > leaderVals!(230, 1) Then

            leaderVals!(230, 1) = compStats!(X, 14)
            leaderVals!(230, 2) = 0
            leaderName$(230) = statPlyrNames$(X)
            leaderTeam$(230) = teamName$

            For Z = 221 To 229
                For Z1 = Z + 1 To 230

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap leaderName$(Z1), leaderName$(Z)
                        Swap leaderTeam$(Z1), leaderTeam$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Fouls
        If compStats!(X, 6) > leaderVals!(240, 1) Then

            leaderVals!(240, 1) = compStats!(X, 6)
            leaderVals!(240, 2) = 0
            leaderName$(240) = statPlyrNames$(X)
            leaderTeam$(240) = teamName$

            For Z = 231 To 239
                For Z1 = Z + 1 To 240

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap leaderName$(Z1), leaderName$(Z)
                        Swap leaderTeam$(Z1), leaderTeam$(Z)
                        For M = 1 To 2:
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'MVP Points
        If compStats!(X, 24) > leaderVals!(250, 1) Then

            leaderVals!(250, 1) = compStats!(X, 24)
            leaderVals!(250, 2) = 0
            leaderName$(250) = statPlyrNames$(X)
            leaderTeam$(250) = teamName$

            For Z = 241 To 249
                For Z1 = Z + 1 To 250

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap leaderName$(Z1), leaderName$(Z)
                        Swap leaderTeam$(Z1), leaderTeam$(Z)
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

        defLeaderNames$(40) = teamName$

        For X = 1 To 39
            For X1 = 1 + X To 40

                If defLeaders!(X, 19) > defLeaders!(X1, 19) Then
                    Swap defLeaderNames$(X1), defLeaderNames$(X)
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

    Shared defTotals!(), offTotals!()
    Shared plyrRecords!(), compStats!()
    Shared offRecords!(), defRecords!()
    Shared statsW0!(), statsW1!()

    Shared plyrRecDesc$(), statPlyrNames$()
    Shared offRecDesc$(), defRecDesc$()

    Shared fullWins, fullLosses

    For I = 0 To 13

        compStats!(I, 25) = compStats!(I, 13)
        compStats!(I, 26) = compStats!(I, 12)
        compStats!(I, 12) = compStats!(I, 2) * 2 + statsW0!(I) + compStats!(I, 4) ' *** PTS ***

        If compStats!(I, 0) <> 0 Then

            compStats!(I, 13) = compStats!(I, 5) / compStats!(I, 0) ' *** RB/G ***
            compStats!(I, 14) = compStats!(I, 6) / compStats!(I, 0) ' *** FOULS/G ***
            compStats!(I, 15) = compStats!(I, 7) / compStats!(I, 0) ' *** STEALS/G ***
            compStats!(I, 16) = compStats!(I, 8) / compStats!(I, 0) ' *** ASSISTS/G ***
            compStats!(I, 17) = compStats!(I, 9) / compStats!(I, 0) ' *** BLOCKS/G ***
            compStats!(I, 24) = compStats!(I, 5) + compStats!(I, 12) - compStats!(I, 6) + compStats!(I, 8) + compStats!(I, 9) + compStats!(I, 7) - 2 * compStats!(I, 11)
            compStats!(I, 18) = compStats!(I, 10) / 60 / compStats!(I, 0) ' *** MIN/G ***

            If Not (compStats!(I, 1) = 0 Or (compStats!(I, 2) / numberGames < 5)) Then
                compStats!(I, 19) = compStats!(I, 2) / compStats!(I, 1) ' *** FG% ***
            End If

            If Not (compStats!(I, 3) = 0 Or (compStats!(I, 4) / numberGames) < 2.5) Then
                compStats!(I, 20) = compStats!(I, 4) / compStats!(I, 3) ' *** FT% ***
            End If

            If Not (statsW1!(I) = 0 Or statsW0!(I) / numberGames < 1.5) Then
                compStats!(I, 21) = statsW0!(I) / statsW1!(I) ' *** 3FG% ***
            End If

            compStats!(I, 22) = compStats!(I, 12) / compStats!(I, 0) ' *** PTS/G ***

            If compStats!(I, 0) > plyrRecords!(1) Then
				plyrRecords!(1) = compStats!(I, 0)
				plyrRecDesc$(1, 0) = statPlyrNames$(I)
				plyrRecDesc$(1, 1) = teamName$
            End If
            If compStats!(I, 1) > plyrRecords!(2) Then
				plyrRecords!(2) = compStats!(I, 1)
				plyrRecDesc$(2, 0) = statPlyrNames$(I)
				plyrRecDesc$(2, 1) = teamName$
            End If
            If compStats!(I, 2) > plyrRecords!(3) Then
				plyrRecords!(3) = compStats!(I, 2)
				plyrRecDesc$(3, 0) = statPlyrNames$(I)
				plyrRecDesc$(3, 1) = teamName$
            End If
            If compStats!(I, 19) > plyrRecords!(4) And UCase$(finalUpdate$) = "Y" And compStats!(I, 2) / numberGames >= 5 Then
				plyrRecords!(4) = compStats!(I, 19)
				plyrRecDesc$(4, 0) = statPlyrNames$(I)
				plyrRecDesc$(4, 1) = teamName$
            End If

            If statsW1!(I) > plyrRecords!(5) Then
				plyrRecords!(5) = statsW1!(I)
				plyrRecDesc$(5, 0) = statPlyrNames$(I)
				plyrRecDesc$(5, 1) = teamName$
            End If
            If statsW0!(I) > plyrRecords!(6) Then
				plyrRecords!(6) = statsW0!(I)
				plyrRecDesc$(6, 0) = statPlyrNames$(I)
				plyrRecDesc$(6, 1) = teamName$
            End If
            If compStats!(I, 21) > plyrRecords!(7) And UCase$(finalUpdate$) = "Y" And statsW0!(I) / numberGames >= 1.5 Then
				plyrRecords!(7) = compStats!(I, 21)
				plyrRecDesc$(7, 0) = statPlyrNames$(I)
				plyrRecDesc$(7, 1) = teamName$
            End If

            If compStats!(I, 3) > plyrRecords!(8) Then
				plyrRecords!(8) = compStats!(I, 3)
				plyrRecDesc$(8, 0) = statPlyrNames$(I)
				plyrRecDesc$(8, 1) = teamName$
            End If
            If compStats!(I, 4) > plyrRecords!(9) Then
				plyrRecords!(9) = compStats!(I, 4)
				plyrRecDesc$(9, 0) = statPlyrNames$(I)
				plyrRecDesc$(9, 1) = teamName$
            End If
            If compStats!(I, 20) > plyrRecords!(10) And UCase$(finalUpdate$) = "Y" And compStats!(I, 4) / numberGames >= 2.5 Then
				plyrRecords!(10) = compStats!(I, 20)
				plyrRecDesc$(10, 0) = statPlyrNames$(I)
				plyrRecDesc$(10, 1) = teamName$
            End If

            If compStats!(I, 10) / 60 > plyrRecords!(11) Then
				plyrRecords!(11) = compStats!(I, 10) / 60
				plyrRecDesc$(11, 0) = statPlyrNames$(I)
				plyrRecDesc$(11, 1) = teamName$
            End If
            If compStats!(I, 12) > plyrRecords!(12) Then
				plyrRecords!(12) = compStats!(I, 12)
				plyrRecDesc$(12, 0) = statPlyrNames$(I)
				plyrRecDesc$(12, 1) = teamName$
            End If
            If compStats!(I, 22) > plyrRecords!(13) And UCase$(finalUpdate$) = "Y" Then
				plyrRecords!(13) = compStats!(I, 22)
				plyrRecDesc$(13, 0) = statPlyrNames$(I)
				plyrRecDesc$(13, 1) = teamName$
            End If

            If compStats!(I, 25) > plyrRecords!(14) Then
				plyrRecords!(14) = compStats!(I, 25)
				plyrRecDesc$(14, 0) = statPlyrNames$(I)
				plyrRecDesc$(14, 1) = teamName$
            End If
            If compStats!(I, 26) > plyrRecords!(15) Then
				plyrRecords!(15) = compStats!(I, 26)
				plyrRecDesc$(15, 0) = statPlyrNames$(I)
				plyrRecDesc$(15, 1) = teamName$
            End If
            If compStats!(I, 5) - compStats!(I, 26) > plyrRecords!(16) Then
				plyrRecords!(16) = compStats!(I, 5) - compStats!(I, 26)
				plyrRecDesc$(16, 0) = statPlyrNames$(I)
				plyrRecDesc$(16, 1) = teamName$
            End If
            If compStats!(I, 5) > plyrRecords!(17) Then
				plyrRecords!(17) = compStats!(I, 5)
				plyrRecDesc$(17, 0) = statPlyrNames$(I)
				plyrRecDesc$(17, 1) = teamName$
            End If
            If compStats!(I, 13) > plyrRecords!(18) And UCase$(finalUpdate$) = "Y" Then
				plyrRecords!(18) = compStats!(I, 13)
				plyrRecDesc$(18, 0) = statPlyrNames$(I)
				plyrRecDesc$(18, 1) = teamName$
            End If

            If compStats!(I, 6) > plyrRecords!(19) Then
				plyrRecords!(19) = compStats!(I, 6)
				plyrRecDesc$(19, 0) = statPlyrNames$(I)
				plyrRecDesc$(19, 1) = teamName$
            End If
            If compStats!(I, 7) > plyrRecords!(20) Then
				plyrRecords!(20) = compStats!(I, 7)
				plyrRecDesc$(20, 0) = statPlyrNames$(I)
				plyrRecDesc$(20, 1) = teamName$
            End If
            If compStats!(I, 8) > plyrRecords!(21) Then
				plyrRecords!(21) = compStats!(I, 8)
				plyrRecDesc$(21, 0) = statPlyrNames$(I)
				plyrRecDesc$(21, 1) = teamName$
            End If
            If compStats!(I, 9) > plyrRecords!(22) Then
				plyrRecords!(22) = compStats!(I, 9)
				plyrRecDesc$(22, 0) = statPlyrNames$(I)
				plyrRecDesc$(22, 1) = teamName$
            End If
            If compStats!(I, 11) > plyrRecords!(23) Then
				plyrRecords!(23) = compStats!(I, 11)
				plyrRecDesc$(23, 0) = statPlyrNames$(I)
				plyrRecDesc$(23, 1) = teamName$
            End If
            If compStats!(I, 18) > plyrRecords!(24) And UCase$(finalUpdate$) = "Y" Then
				plyrRecords!(24) = compStats!(I, 18)
				plyrRecDesc$(24, 0) = statPlyrNames$(I)
				plyrRecDesc$(24, 1) = teamName$
            End If

            If compStats!(I, 24) > plyrRecords!(25) Then
				plyrRecords!(25) = compStats!(I, 24)
				plyrRecDesc$(25, 0) = statPlyrNames$(I)
				plyrRecDesc$(25, 1) = teamName$
            End If

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

    If offTotals!(15) <> 0 Then
		offTotals!(21) = offTotals!(14) / offTotals!(15) 
    Else 
        offTotals!(21) = 0
    End If
    If defTotals!(15) <> 0 Then
		defTotals!(21) = defTotals!(14) / defTotals!(15) 
    Else 
        defTotals!(21) = 999
    End If

    offTotals!(19) = offTotals!(20) / numberGames
    defTotals!(19) = defTotals!(20) / numberGames

    If offTotals!(2) > offRecords!(1) Then
		offRecords!(1) = offTotals!(2)
        offRecDesc$(1) = teamName$
    End If
    If offTotals!(3) > offRecords!(2) Then
		offRecords!(2) = offTotals!(3)
        offRecDesc$(2) = teamName$
    End If
    If offTotals!(17) > offRecords!(3) And UCase$(finalUpdate$) = "Y" Then
		offRecords!(3) = offTotals!(17)
        offRecDesc$(3) = teamName$
    End If

    If offTotals!(15) > offRecords!(4) Then
		offRecords!(4) = offTotals!(15)
        offRecDesc$(4) = teamName$
    End If
    If offTotals!(14) > offRecords!(5) Then
		offRecords!(5) = offTotals!(14)
        offRecDesc$(5) = teamName$
    End If
    If offTotals!(21) > offRecords!(6) And UCase$(finalUpdate$) = "Y" Then
		offRecords!(6) = offTotals!(21)
        offRecDesc$(6) = teamName$
    End If

    If offTotals!(4) > offRecords!(7) Then
		offRecords!(7) = offTotals!(4)
        offRecDesc$(7) = teamName$
    End If
    If offTotals!(5) > offRecords!(8) Then
		offRecords!(8) = offTotals!(5)
        offRecDesc$(8) = teamName$
    End If
    If offTotals!(18) > offRecords!(9) And UCase$(finalUpdate$) = "Y" Then
		offRecords!(9) = offTotals!(18)
        offRecDesc$(9) = teamName$
    End If

    If offTotals!(13) > offRecords!(10) Then
		offRecords!(10) = offTotals!(13)
        offRecDesc$(10) = teamName$
    End If
    If offTotals!(6) - offTotals!(13) > offRecords!(11) Then
		offRecords!(11) = offTotals!(6) - offTotals!(13)
        offRecDesc$(11) = teamName$
    End If
    If offTotals!(6) > offRecords!(12) Then
		offRecords!(12) = offTotals!(6)
        offRecDesc$(12) = teamName$
    End If
    If offTotals!(6) / numberGames > offRecords!(13) And UCase$(finalUpdate$) = "Y" Then
		offRecords!(13) = offTotals!(6) / numberGames
        offRecDesc$(13) = teamName$
    End If

    If offTotals!(7) < offRecords!(14) And UCase$(finalUpdate$) = "Y" Then
		offRecords!(14) = offTotals!(7)
        offRecDesc$(14) = teamName$
    End If
    
    If offTotals!(8) > offRecords!(15) Then
		offRecords!(15) = offTotals!(8)
        offRecDesc$(15) = teamName$
    End If
    If offTotals!(9) > offRecords!(16) Then
		offRecords!(16) = offTotals!(9)
        offRecDesc$(16) = teamName$
    End If
    If offTotals!(10) > offRecords!(17) Then
		offRecords!(17) = offTotals!(10)
        offRecDesc$(17) = teamName$
    End If
    If offTotals!(12) < offRecords!(18) And UCase$(finalUpdate$) = "Y" Then
		offRecords!(18) = offTotals!(12)
        offRecDesc$(18) = teamName$
    End If

    If offTotals!(11) > offRecords!(19) Then
		offRecords!(19) = offTotals!(11)
        offRecDesc$(19) = teamName$
    End If
    If offTotals!(11) / numberGames > offRecords!(20) And UCase$(finalUpdate$) = "Y" Then
		offRecords!(20) = offTotals!(11) / numberGames
        offRecDesc$(20) = teamName$
    End If
    If fullWins > offRecords!(21) Then
		offRecords!(21) = fullWins
        offRecDesc$(21) = teamName$
    End If

    'FG's
    If defTotals!(2) < defRecords!(1) Then
		defRecords!(1) = defTotals!(2): defRecDesc$(1) = teamName$
    End If
    If defTotals!(3) < defRecords!(2) Then
		defRecords!(2) = defTotals!(3): defRecDesc$(2) = teamName$
    End If
    If defTotals!(17) < defRecords!(3) And UCase$(finalUpdate$) = "Y" Then
		defRecords!(3) = defTotals!(17): defRecDesc$(3) = teamName$
    End If

    '3 FGs
    If defTotals!(15) < defRecords!(4) Then
		defRecords!(4) = defTotals!(15): defRecDesc$(4) = teamName$
    End If
    If defTotals!(14) < defRecords!(5) Then
		defRecords!(5) = defTotals!(14): defRecDesc$(5) = teamName$
    End If
    If defTotals!(21) < defRecords!(6) And defTotals!(15) <> 0 And UCase$(finalUpdate$) = "Y" Then
		defRecords!(6) = defTotals!(21): defRecDesc$(6) = teamName$
    End If

    If defTotals!(4) < defRecords!(7) Then
		defRecords!(7) = defTotals!(4): defRecDesc$(7) = teamName$
    End If
    If defTotals!(5) < defRecords!(8) Then
		defRecords!(8) = defTotals!(5): defRecDesc$(8) = teamName$
    End If
    If defTotals!(18) < defRecords!(9) And UCase$(finalUpdate$) = "Y" Then
		defRecords!(9) = defTotals!(18): defRecDesc$(9) = teamName$
    End If

    If defTotals!(13) < defRecords!(10) Then
		defRecords!(10) = defTotals!(13): defRecDesc$(10) = teamName$
    End If
    If defTotals!(6) - defTotals!(13) < defRecords!(11) Then
		defRecords!(11) = defTotals!(6) - defTotals!(13): defRecDesc$(11) = teamName$
    End If
    If defTotals!(6) < defRecords!(12) Then
		defRecords!(12) = defTotals!(6): defRecDesc$(12) = teamName$
    End If
    If defTotals!(6) / numberGames < defRecords!(13) And UCase$(finalUpdate$) = "Y" Then
		defRecords!(13) = defTotals!(6) / numberGames: defRecDesc$(13) = teamName$
    End If

    If defTotals!(7) > defRecords!(14) And UCase$(finalUpdate$) = "Y" Then
		defRecords!(14) = defTotals!(7): defRecDesc$(14) = teamName$
    End If

    If defTotals!(8) < defRecords!(15) Then
		defRecords!(15) = defTotals!(8): defRecDesc$(15) = teamName$
    End If
    If defTotals!(9) < defRecords!(16) Then
		defRecords!(16) = defTotals!(9): defRecDesc$(16) = teamName$
    End If
    If defTotals!(10) < defRecords!(17) Then
		defRecords!(17) = defTotals!(10): defRecDesc$(17) = teamName$
    End If
    If defTotals!(12) > defRecords!(18) And UCase$(finalUpdate$) = "Y" Then
		defRecords!(18) = defTotals!(12): defRecDesc$(18) = teamName$
    End If

    If defTotals!(11) < defRecords!(19) Then
		defRecords!(19) = defTotals!(11): defRecDesc$(19) = teamName$
    End If
    If defTotals!(11) / numberGames < defRecords!(20) And UCase$(finalUpdate$) = "Y" Then
		defRecords!(20) = defTotals!(11) / numberGames: defRecDesc$(20) = teamName$
    End If

    If fullLosses > defRecords!(21) Then
		defRecords!(21) = fullLosses: defRecDesc$(21) = teamName$
    End If

End Sub


'----------------------------------------
'   SortStandingsConference Subroutine
'----------------------------------------
'This subroutine sorts the leaders in the
'target conference. Currently setup using
'a bubble sort algorithm.
Sub SortStandingsConference

    Shared compSeasonWins!(), compSeasonLosses!(), compSeasonWinPct!(), compConfWins!(), compConfLosses!(), compConfWinPct!()

    Shared memberName$()

    'Currently sorts by Conference Record
    '   To sort by Season Record, change O4 / O5 to O1 / O2
    '   (not in the Swap statements!)

    For Z = 0 To 1

        AA = (Z * 15) + 1
        ZZ = (Z * 15) + 15

        For X = AA To ZZ - 1

            For Y = X + 1 To ZZ

                diffX = compConfWins!(X) - compConfLosses!(X)
                diffY = compConfWins!(Y) - compConfLosses!(Y)

                If diffX <= diffY Then
                    If diffX <> diffY Then
                        If Not (compConfWins!(X) <= compConfWins!(Y) And compConfWins!(X) > compConfLosses!(X)) Or Not (compConfWins!(X) >= compConfWins!(Y) And compConfWins!(X) < compConfLosses!(X)) Then
                            Swap memberName$(X), memberName$(Y)
                            Swap compSeasonWins!(X), compSeasonWins!(Y)
                            Swap compSeasonLosses!(X), compSeasonLosses!(Y)
                            Swap compSeasonWinPct!(X), compSeasonWinPct!(Y)
                            Swap compConfWins!(X), compConfWins!(Y)
                            Swap compConfLosses!(X), compConfLosses!(Y)
                            Swap compConfWinPct!(X), compConfWinPct!(Y)
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

    Shared defLeaderNames$(), offLeaderNames$()
    Shared defLeaders!(), offLeaders!()
    Shared leaderVals!()
    Shared compSeasonWins!(), compSeasonLosses!(), compSeasonWinPct!(), compConfWins!(), compConfLosses!(), compConfWinPct!()

    Shared leaderName$(), memberName$(), leaderTeam$()

    Cls

    Print

    Open diskPaths$(2) + conferenceName$ + ".LDR" For Output As #1

    Print "    POSTING NEW "; conferenceName$; " CONFERENCE LEADERS"

    For X = 1 To TEAMS_PER_CONFERENCE
        Write #1, memberName$(X), compSeasonWins!(X), compSeasonLosses!(X), compSeasonWinPct!(X), compConfWins!(X), compConfLosses!(X), compConfWinPct!(X)
    Next

    For X = 1 To 250
        Write #1, leaderName$(X), leaderTeam$(X), leaderVals!(X, 1), leaderVals!(X, 2)
    Next X

    For X = 1 To MAX_CONFERENCES:
        Write #1, offLeaderNames$(X)
        For Y = 1 To 20:
            Write #1, offLeaders!(X, Y)
        Next
    Next

    For X = 1 To MAX_CONFERENCES:
        Write #1, defLeaderNames$(X):
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

    Shared locIndicator$(), oppName$(), memberName$()

    Shared confWins, confLosses, fullWins, fullLosses

    findFile$ = diskPaths$(1) + statFile$ + "S"

    If Not _FileExists(findFile$) Then

        Print "NO STATS FOUND FOR "; statFile$
        FindStatsCon% = 0

    Else

        Call ReadScheduleStats (statFile$, numberGames)

        confWins = 0
        confLosses = 0

        fullWins = 0
        fullLosses = 0

        'RATINGS
        For I = 1 To numberGames

            statName$ = nameStatFile$(oppName$(I))
            nonConfGame = 0

            For EC = 1 To TEAMS_PER_CONFERENCE

                If statName$ = memberName$(EC) Then
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
