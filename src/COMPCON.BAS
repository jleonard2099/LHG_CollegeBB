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
    Shared divWins!(), divLosses!(), divWinPct()
    Shared seasonWins!(), seasonLosses!(), seasonWinPct!()

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
        
        Erase seasonWins!, seasonLosses!, seasonWinPct!
        Erase divWins!, divLosses!, divWinPct

        For XW = 1 To TEAMS_PER_CONFERENCE

            'Look for * as terminating char
            If memberName$(XW) <> Chr$(42) Then

                haveStats = FindStatsCon%(memberName$(XW), numberGames)

                If (haveStats) Then

                    WT$ = memberName$(XW)
                    nonConfGame = 0

                    For X = 1 To TEAMS_PER_CONFERENCE

                        If memberName$(X) = WT$ Then

                            If seasonWins!(X) = fullWins And seasonLosses!(X) = fullLosses Then

                                nonConfGame = 1
                                Exit For

                            Else

                                seasonWins!(X) = fullWins
                                seasonLosses!(X) = fullLosses

                                divWins!(X) = confWins
                                divLosses!(X) = confLosses

                                If seasonWins!(X) + seasonLosses!(X) = 0 Then
                                    seasonWinPct!(X) = 0
                                Else
                                    seasonWinPct!(X) = seasonWins!(X) / (seasonWins!(X) + seasonLosses!(X))
                                End If

                                If divWins!(X) + divLosses!(X) = 0 Then
                                    divWinPct(X) = 0
                                Else
                                    divWinPct(X) = divWins!(X) / (divWins!(X) + divLosses!(X))
                                End If

                            End If

                        End If

                    Next X
                    'alternatively, use a Do / Until loop; nonConfGame = 1 or EC = 30

                    If nonConfGame <> 1 And _FileExists(diskPaths$(1) + memberName$(XW)) Then

                        Call ReadTeamStats(memberName$(XW), TN$)
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
'     CalcStatLeadersConf Subroutine
'----------------------------------------
'This subroutine compares pre-loaded stats
'to existing leaders and updates as needed
Sub CalcStatLeadersConf (teamName$)

    Shared defTotals!(), offTotals!()
    Shared defLeaders!(), offLeaders!()
    Shared leaderVals!(), statPlyrVal!()
    Shared stats3FGM!(), stats3FGA!()

    Shared defLeaderNames$(), offLeaderNames$()
    Shared statPlyrNames$()
    Shared leaderName$(), leaderTeam$()

    For X = 0 To 14:

        'Color 14
        Print "CHECKING STATS OF ";
        Color 15
        Print statPlyrNames$(X)

        'Division 1, 2 Record
        If statPlyrVal!(X, 22) > leaderVals!(20, 1) Then

            leaderVals!(20, 1) = statPlyrVal!(X, 22)
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
        If statPlyrVal!(X, 12) > leaderVals!(40, 1) Then

            leaderVals!(40, 1) = statPlyrVal!(X, 12)
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
        If statPlyrVal!(X, 19) > leaderVals!(60, 1) Then

            leaderVals!(60, 1) = statPlyrVal!(X, 19)
            leaderVals!(60, 2) = statPlyrVal!(X, 1)
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
        If statPlyrVal!(X, 20) > leaderVals!(80, 1) Then

            leaderVals!(80, 1) = statPlyrVal!(X, 20)
            leaderVals!(80, 2) = statPlyrVal!(X, 3)
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
        If statPlyrVal!(X, 15) > leaderVals!(90, 1) Then

            leaderVals!(90, 1) = statPlyrVal!(X, 15)
            leaderVals!(90, 2) = statPlyrVal!(X, 7)
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
        If statPlyrVal!(X, 7) > leaderVals!(100, 1) Then

            leaderVals!(100, 1) = statPlyrVal!(X, 7)
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
        If statPlyrVal!(X, 16) > leaderVals!(110, 1) Then

            leaderVals!(110, 1) = statPlyrVal!(X, 16)
            leaderVals!(110, 2) = statPlyrVal!(X, 8)
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
        If statPlyrVal!(X, 8) > leaderVals!(120, 1) Then

            leaderVals!(120, 1) = statPlyrVal!(X, 8)
            leaderVals!(120, 2) = statPlyrVal!(X, 8)
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
        If statPlyrVal!(X, 21) > leaderVals!(130, 1) Then

            leaderVals!(130, 1) = statPlyrVal!(X, 21)
            leaderVals!(130, 2) = stats3FGA!(X)
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
        If stats3FGM!(X) > leaderVals!(140, 1) Then

            leaderVals!(140, 1) = stats3FGM!(X)
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
        If statPlyrVal!(X, 18) > leaderVals!(150, 1) Then

            leaderVals!(150, 1) = statPlyrVal!(X, 18)
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
        If statPlyrVal!(X, 10) / 60 > leaderVals!(160, 1) Then

            leaderVals!(160, 1) = statPlyrVal!(X, 10) / 60
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
        If statPlyrVal!(X, 13) > leaderVals!(170, 1) Then

            leaderVals!(170, 1) = statPlyrVal!(X, 13)
            leaderVals!(170, 2) = statPlyrVal!(X, 5)
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
        If statPlyrVal!(X, 5) > leaderVals!(180, 1) Then

            leaderVals!(180, 1) = statPlyrVal!(X, 5)
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
        If statPlyrVal!(X, 17) > leaderVals!(190, 1) Then

            leaderVals!(190, 1) = statPlyrVal!(X, 17)
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
        If statPlyrVal!(X, 9) > leaderVals!(200, 1) Then

            leaderVals!(200, 1) = statPlyrVal!(X, 9)
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
        If statPlyrVal!(X, 2) > leaderVals!(210, 1) Then

            leaderVals!(210, 1) = statPlyrVal!(X, 2)
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
        If statPlyrVal!(X, 4) > leaderVals!(220, 1) Then

            leaderVals!(220, 1) = statPlyrVal!(X, 4)
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
        If statPlyrVal!(X, 14) > leaderVals!(230, 1) Then

            leaderVals!(230, 1) = statPlyrVal!(X, 14)
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
        If statPlyrVal!(X, 6) > leaderVals!(240, 1) Then

            leaderVals!(240, 1) = statPlyrVal!(X, 6)
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
        If statPlyrVal!(X, 24) > leaderVals!(250, 1) Then

            leaderVals!(250, 1) = statPlyrVal!(X, 24)
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
    Shared plyrRecords!(), statPlyrVal!()
    Shared offRecords!(), defRecords!()
    Shared stats3FGM!(), stats3FGA!()

    Shared plyrRecDesc$(), statPlyrNames$()
    Shared offRecDesc$(), defRecDesc$()

    Shared fullWins, fullLosses

    For I = 0 To 13

        statPlyrVal!(I, 25) = statPlyrVal!(I, 13)
        statPlyrVal!(I, 26) = statPlyrVal!(I, 12)
        statPlyrVal!(I, 12) = statPlyrVal!(I, 2) * 2 + stats3FGM!(I) + statPlyrVal!(I, 4) ' *** PTS ***

        If statPlyrVal!(I, 0) <> 0 Then

            statPlyrVal!(I, 13) = statPlyrVal!(I, 5) / statPlyrVal!(I, 0) ' *** RB/G ***
            statPlyrVal!(I, 14) = statPlyrVal!(I, 6) / statPlyrVal!(I, 0) ' *** FOULS/G ***
            statPlyrVal!(I, 15) = statPlyrVal!(I, 7) / statPlyrVal!(I, 0) ' *** STEALS/G ***
            statPlyrVal!(I, 16) = statPlyrVal!(I, 8) / statPlyrVal!(I, 0) ' *** ASSISTS/G ***
            statPlyrVal!(I, 17) = statPlyrVal!(I, 9) / statPlyrVal!(I, 0) ' *** BLOCKS/G ***
            statPlyrVal!(I, 24) = statPlyrVal!(I, 5) + statPlyrVal!(I, 12) - statPlyrVal!(I, 6) + statPlyrVal!(I, 8) + statPlyrVal!(I, 9) + statPlyrVal!(I, 7) - 2 * statPlyrVal!(I, 11)
            statPlyrVal!(I, 18) = statPlyrVal!(I, 10) / 60 / statPlyrVal!(I, 0) ' *** MIN/G ***

            If Not (statPlyrVal!(I, 1) = 0 Or (statPlyrVal!(I, 2) / numberGames < 5)) Then
                statPlyrVal!(I, 19) = statPlyrVal!(I, 2) / statPlyrVal!(I, 1) ' *** FG% ***
            End If

            If Not (statPlyrVal!(I, 3) = 0 Or (statPlyrVal!(I, 4) / numberGames) < 2.5) Then
                statPlyrVal!(I, 20) = statPlyrVal!(I, 4) / statPlyrVal!(I, 3) ' *** FT% ***
            End If

            If Not (stats3FGA!(I) = 0 Or stats3FGM!(I) / numberGames < 1.5) Then
                statPlyrVal!(I, 21) = stats3FGM!(I) / stats3FGA!(I) ' *** 3FG% ***
            End If

            statPlyrVal!(I, 22) = statPlyrVal!(I, 12) / statPlyrVal!(I, 0) ' *** PTS/G ***

            If statPlyrVal!(I, 0) > plyrRecords!(1) Then
				plyrRecords!(1) = statPlyrVal!(I, 0)
				plyrRecDesc$(1, 0) = statPlyrNames$(I)
				plyrRecDesc$(1, 1) = teamName$
            End If
            If statPlyrVal!(I, 1) > plyrRecords!(2) Then
				plyrRecords!(2) = statPlyrVal!(I, 1)
				plyrRecDesc$(2, 0) = statPlyrNames$(I)
				plyrRecDesc$(2, 1) = teamName$
            End If
            If statPlyrVal!(I, 2) > plyrRecords!(3) Then
				plyrRecords!(3) = statPlyrVal!(I, 2)
				plyrRecDesc$(3, 0) = statPlyrNames$(I)
				plyrRecDesc$(3, 1) = teamName$
            End If
            If statPlyrVal!(I, 19) > plyrRecords!(4) And UCase$(finalUpdate$) = "Y" And statPlyrVal!(I, 2) / numberGames >= 5 Then
				plyrRecords!(4) = statPlyrVal!(I, 19)
				plyrRecDesc$(4, 0) = statPlyrNames$(I)
				plyrRecDesc$(4, 1) = teamName$
            End If

            If stats3FGA!(I) > plyrRecords!(5) Then
				plyrRecords!(5) = stats3FGA!(I)
				plyrRecDesc$(5, 0) = statPlyrNames$(I)
				plyrRecDesc$(5, 1) = teamName$
            End If
            If stats3FGM!(I) > plyrRecords!(6) Then
				plyrRecords!(6) = stats3FGM!(I)
				plyrRecDesc$(6, 0) = statPlyrNames$(I)
				plyrRecDesc$(6, 1) = teamName$
            End If
            If statPlyrVal!(I, 21) > plyrRecords!(7) And UCase$(finalUpdate$) = "Y" And stats3FGM!(I) / numberGames >= 1.5 Then
				plyrRecords!(7) = statPlyrVal!(I, 21)
				plyrRecDesc$(7, 0) = statPlyrNames$(I)
				plyrRecDesc$(7, 1) = teamName$
            End If

            If statPlyrVal!(I, 3) > plyrRecords!(8) Then
				plyrRecords!(8) = statPlyrVal!(I, 3)
				plyrRecDesc$(8, 0) = statPlyrNames$(I)
				plyrRecDesc$(8, 1) = teamName$
            End If
            If statPlyrVal!(I, 4) > plyrRecords!(9) Then
				plyrRecords!(9) = statPlyrVal!(I, 4)
				plyrRecDesc$(9, 0) = statPlyrNames$(I)
				plyrRecDesc$(9, 1) = teamName$
            End If
            If statPlyrVal!(I, 20) > plyrRecords!(10) And UCase$(finalUpdate$) = "Y" And statPlyrVal!(I, 4) / numberGames >= 2.5 Then
				plyrRecords!(10) = statPlyrVal!(I, 20)
				plyrRecDesc$(10, 0) = statPlyrNames$(I)
				plyrRecDesc$(10, 1) = teamName$
            End If

            If statPlyrVal!(I, 10) / 60 > plyrRecords!(11) Then
				plyrRecords!(11) = statPlyrVal!(I, 10) / 60
				plyrRecDesc$(11, 0) = statPlyrNames$(I)
				plyrRecDesc$(11, 1) = teamName$
            End If
            If statPlyrVal!(I, 12) > plyrRecords!(12) Then
				plyrRecords!(12) = statPlyrVal!(I, 12)
				plyrRecDesc$(12, 0) = statPlyrNames$(I)
				plyrRecDesc$(12, 1) = teamName$
            End If
            If statPlyrVal!(I, 22) > plyrRecords!(13) And UCase$(finalUpdate$) = "Y" Then
				plyrRecords!(13) = statPlyrVal!(I, 22)
				plyrRecDesc$(13, 0) = statPlyrNames$(I)
				plyrRecDesc$(13, 1) = teamName$
            End If

            If statPlyrVal!(I, 25) > plyrRecords!(14) Then
				plyrRecords!(14) = statPlyrVal!(I, 25)
				plyrRecDesc$(14, 0) = statPlyrNames$(I)
				plyrRecDesc$(14, 1) = teamName$
            End If
            If statPlyrVal!(I, 26) > plyrRecords!(15) Then
				plyrRecords!(15) = statPlyrVal!(I, 26)
				plyrRecDesc$(15, 0) = statPlyrNames$(I)
				plyrRecDesc$(15, 1) = teamName$
            End If
            If statPlyrVal!(I, 5) - statPlyrVal!(I, 26) > plyrRecords!(16) Then
				plyrRecords!(16) = statPlyrVal!(I, 5) - statPlyrVal!(I, 26)
				plyrRecDesc$(16, 0) = statPlyrNames$(I)
				plyrRecDesc$(16, 1) = teamName$
            End If
            If statPlyrVal!(I, 5) > plyrRecords!(17) Then
				plyrRecords!(17) = statPlyrVal!(I, 5)
				plyrRecDesc$(17, 0) = statPlyrNames$(I)
				plyrRecDesc$(17, 1) = teamName$
            End If
            If statPlyrVal!(I, 13) > plyrRecords!(18) And UCase$(finalUpdate$) = "Y" Then
				plyrRecords!(18) = statPlyrVal!(I, 13)
				plyrRecDesc$(18, 0) = statPlyrNames$(I)
				plyrRecDesc$(18, 1) = teamName$
            End If

            If statPlyrVal!(I, 6) > plyrRecords!(19) Then
				plyrRecords!(19) = statPlyrVal!(I, 6)
				plyrRecDesc$(19, 0) = statPlyrNames$(I)
				plyrRecDesc$(19, 1) = teamName$
            End If
            If statPlyrVal!(I, 7) > plyrRecords!(20) Then
				plyrRecords!(20) = statPlyrVal!(I, 7)
				plyrRecDesc$(20, 0) = statPlyrNames$(I)
				plyrRecDesc$(20, 1) = teamName$
            End If
            If statPlyrVal!(I, 8) > plyrRecords!(21) Then
				plyrRecords!(21) = statPlyrVal!(I, 8)
				plyrRecDesc$(21, 0) = statPlyrNames$(I)
				plyrRecDesc$(21, 1) = teamName$
            End If
            If statPlyrVal!(I, 9) > plyrRecords!(22) Then
				plyrRecords!(22) = statPlyrVal!(I, 9)
				plyrRecDesc$(22, 0) = statPlyrNames$(I)
				plyrRecDesc$(22, 1) = teamName$
            End If
            If statPlyrVal!(I, 11) > plyrRecords!(23) Then
				plyrRecords!(23) = statPlyrVal!(I, 11)
				plyrRecDesc$(23, 0) = statPlyrNames$(I)
				plyrRecDesc$(23, 1) = teamName$
            End If
            If statPlyrVal!(I, 18) > plyrRecords!(24) And UCase$(finalUpdate$) = "Y" Then
				plyrRecords!(24) = statPlyrVal!(I, 18)
				plyrRecDesc$(24, 0) = statPlyrNames$(I)
				plyrRecDesc$(24, 1) = teamName$
            End If

            If statPlyrVal!(I, 24) > plyrRecords!(25) Then
				plyrRecords!(25) = statPlyrVal!(I, 24)
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

    Shared seasonWins!(), seasonLosses!(), seasonWinPct!(), divWins!(), divLosses!(), divWinPct()

    Shared memberName$()

    'Currently sorts by Conference Record
    '   To sort by Season Record, change O4 / O5 to O1 / O2
    '   (not in the Swap statements!)

    For Z = 0 To 1

        AA = (Z * 15) + 1
        ZZ = (Z * 15) + 15

        For X = AA To ZZ - 1

            For Y = X + 1 To ZZ

                diffX = divWins!(X) - divLosses!(X)
                diffY = divWins!(Y) - divLosses!(Y)

                If diffX <= diffY Then
                    If diffX <> diffY Then
                        If Not (divWins!(X) <= divWins!(Y) And divWins!(X) > divLosses!(X)) Or Not (divWins!(X) >= divWins!(Y) And divWins!(X) < divLosses!(X)) Then
                            Swap memberName$(X), memberName$(Y)
                            Swap seasonWins!(X), seasonWins!(Y)
                            Swap seasonLosses!(X), seasonLosses!(Y)
                            Swap seasonWinPct!(X), seasonWinPct!(Y)
                            Swap divWins!(X), divWins!(Y)
                            Swap divLosses!(X), divLosses!(Y)
                            Swap divWinPct(X), divWinPct(Y)
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
    Shared seasonWins!(), seasonLosses!(), seasonWinPct!(), divWins!(), divLosses!(), divWinPct()

    Shared leaderName$(), memberName$(), leaderTeam$()

    Cls

    Print

    Open diskPaths$(2) + conferenceName$ + ".LDR" For Output As #1

    Print "    POSTING NEW "; conferenceName$; " CONFERENCE LEADERS"

    For X = 1 To TEAMS_PER_CONFERENCE
        Write #1, memberName$(X), seasonWins!(X), seasonLosses!(X), seasonWinPct!(X), divWins!(X), divLosses!(X), divWinPct(X)
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
