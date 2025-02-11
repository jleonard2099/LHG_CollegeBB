'----------------------------------------
'        CompileNatlOrg SubRoutine
'----------------------------------------
'This subroutine reads in existing records,
'looks for stats matching each team in the
'organization, and then relies on subroutines
'to read stats and compare to any existing
'leaders and records
Sub CompileNatlOrg ()

    _Title "Courtside College Basketball - Update National Stats"

    Shared memberConf$()

    Shared leaderVals!(), plyrRecords!()
    Shared defLeaders!(), defRecords!()
    Shared offLeaders!(), offRecords!()
    Shared seasonWins(), seasonLosses(), seasonWinPct!(), divWins(), divLosses(), divWinPct()

    Shared defLeaderNames$(), offLeaderNames$()
    Shared offRecDesc$(), defRecDesc$()
    Shared plyrRecDesc$(), memberName$()
    Shared leaderName$(), leaderTeam$()

    Shared confWins, confLosses, fullWins, fullLosses

    Shared natDefLeaders!(), natLeaderVals!(), natOffLeaders!()
    Shared natDefLdrNames$(), natOffLdrNames$(), natLdrName$(), confLdrName$(), natLdrTeam$()

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

    If _FileExists(diskPaths$(1) + "RECORDS.CBB") Then

        Call ReadSeasonRecords ("RECORDS.CBB")

    Else

        Erase plyrRecords!, offRecords!, defRecords!
        Erase plyrRecDesc$, offRecDesc$, defRecDesc$

    End If

    'Initialize values first
    Erase natDefLeaders!, natLeaderVals!, natOffLeaders!

    Erase natDefLdrNames$, natOffLdrNames$
    Erase natLdrName$, confLdrName$, natLdrTeam$

    For X = 1 To MAX_CONFERENCES
        natDefLeaders!(X, 19) = 9999
    Next


    C$ = ","

    organizationFile$ = ChooseFile$(diskPaths$(2) + "*.CON", "ORGANIZATION", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Do
            Color 15, 0
            Cls
            Print "WOULD YOU LIKE TO POST SEASON RECORDS? (Y/N)";
            I$ = GetKeyPress$
            finalUpdate$ = UCase$(I$)
        Loop Until finalUpdate$ = "Y" Or finalUpdate$ = "N"

        Call ReadOrganizationFile(organizationFile$, orgName$)

        For currConf = 1 To MAX_CONFERENCES

            If memberConf$(currConf) <> "XXX" Then

                Cls

                Erase defLeaders!, offLeaders!
                For X = 1 To MAX_CONFERENCES
                    defLeaders!(X, 19) = 9999
                Next

                Erase leaderVals!
                Erase seasonWins, seasonLosses, seasonWinPct!, divWins, divLosses, divWinPct
                Erase defLeaderNames$, offLeaderNames$
                Erase memberName$, leaderName$, leaderTeam$

                targetConf$ = diskPaths$(2) + memberConf$(currConf) + ".TMS"

                If _FileExists(targetConf$) Then

                    Open targetConf$ For Input As #4
                    Input #4, statType
                    Close #4

                    If statType <> 0 Then

                        MsgText$ = "ERROR!! THIS IS NOT A COLLEGE CONFERENCE. PRESS ANY KEY TO RETURN TO MAIN MENU"
                        BoxWidth = 20
                        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                        Call CSMsgBox(MsgText$, BoxWidth, PASS)
                        I$ = GetKeyPress$
                        Call CSMsgBox("", 0, PASS)
                        Exit Sub

                    Else

                        Open targetConf$ For Input As #3

                        Input #3, LG$
                        Input #3, div1$
                        Input #3, div2$

                        For XW = 1 To TEAMS_PER_CONFERENCE
                            Input #3, memberName$(XW)
                        Next

                        Close #3

                        For XW = 1 To TEAMS_PER_CONFERENCE

                            'Look for * as terminating char
                            If memberName$(XW) <> Chr$(42) Then

                                haveStats = FindStatsNat%(memberName$(XW), numberGames, NRT)

                                If (haveStats) Then

                                    L = Len(memberName$(XW))
                                    statName$ = ""

                                    For X = 1 To L
                                        A = Asc(Mid$(memberName$(XW), X, 1))
                                        statName$ = statName$ + Chr$(A)
                                    Next X

                                    nonConfGame = 0

                                    For X = 1 To TEAMS_PER_CONFERENCE

                                        If memberName$(X) = statName$ Then

                                            If seasonWins(X) = fullWins And seasonLosses(X) = fullLosses Then

                                                nonConfGame = 1
                                                Exit For

                                            Else

                                                seasonWins(X) = fullWins
                                                seasonLosses(X) = fullLosses

                                                divWins(X) = confWins
                                                divLosses(X) = confLosses

                                                If seasonWins(X) + seasonLosses(X) = 0 Then
                                                    seasonWinPct!(X) = 0
                                                Else
                                                    seasonWinPct!(X) = seasonWins(X) / (seasonWins(X) + seasonLosses(X))
                                                End If

                                                If divWins(X) + divLosses(X) = 0 Then
                                                    divWinPct(X) = 0
                                                Else
                                                    divWinPct(X) = divWins(X) / (divWins(X) + divLosses(X))
                                                End If

                                            End If

                                        End If

                                    Next X


                                    If nonConfGame <> 1 And _FileExists(diskPaths$(1) + memberName$(XW)) Then

                                        Call ReadTeamStats(memberName$(XW), TN$)
                                        Call CalcStatTotalsConf(numberGames, TN$, finalUpdate$)
                                        Call CalcStatLeadersConf(TN$)
                                        Call CalcStatLeadersNat(memberName$(XW), TN$)

                                    End If

                                End If 'Done looking for stats

                            End If 'Done checking for valid stat file

                        Next XW

                        Call SortStandingsConference
                        Call PostLeadersConf(memberConf$(currConf))
                        Call PostRecordsColl

                    End If 'Done checking for valid conference

                End If 'Done checking if file exists

            End If

        Next currConf

        Call PostLeadersOrg(organizationFile$, NRT)
        Call PostRecordsColl

    End If

End Sub


'----------------------------------------
'       CalcStatLeadersNat Subroutine
'----------------------------------------
'This subroutine compares pre-loaded stats
'to existing leaders and updates as needed
Sub CalcStatLeadersNat (statFile$, teamName$)

    Shared defTotals!(), offTotals!()
    Shared natDefLeaders!(), natOffLeaders!()
    Shared natLeaderVals!(), statPlyrVal!()
    Shared stats3FGM!(), stats3FGA!()

    Shared natDefLdrNames$(), natOffLdrNames$()
    Shared statPlyrNames$()
    Shared natLdrName$(), natLdrTeam$()

    Cls
    Print "CHECKING "; statFile$; " STATS AGAINST NATIONAL LEADERS"

    For X = 0 To 14

        'Color 14
        Print "CHECKING STATS OF ";
        Color 15
        Print statPlyrNames$(X)

        'Points Per Game
        If statPlyrVal!(X, 22) > natLeaderVals!(20, 1) Then

            natLeaderVals!(20, 1) = statPlyrVal!(X, 22)
            natLeaderVals!(20, 2) = 0
            natLdrName$(20) = statPlyrNames$(X)
            natLdrTeam$(20) = teamName$

            For Z = 1 To 19
                For Z1 = Z + 1 To 20

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natLdrName$(Z1), natLdrName$(Z)
                        Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                        For M = 1 To 2: Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M): Next
                    End If

                Next Z1
            Next Z

        End If

        'Offensive Rebounds
        If statPlyrVal!(X, 12) > natLeaderVals!(40, 1) Then

            natLeaderVals!(40, 1) = statPlyrVal!(X, 12)
            natLeaderVals!(40, 2) = 0
            natLdrName$(40) = statPlyrNames$(X)
            natLdrTeam$(40) = teamName$

            For Z = 21 To 39
                For Z1 = Z + 1 To 40

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natLdrName$(Z1), natLdrName$(Z)
                        Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Field Goal %
        If statPlyrVal!(X, 19) > natLeaderVals!(60, 1) Then

            natLeaderVals!(60, 1) = statPlyrVal!(X, 19)
            natLeaderVals!(60, 2) = statPlyrVal!(X, 1)
            natLdrName$(60) = statPlyrNames$(X)
            natLdrTeam$(60) = teamName$

            For Z = 41 To 59
                For Z1 = Z + 1 To 60

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natLdrName$(Z1), natLdrName$(Z)
                        Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Free Throw %
        If statPlyrVal!(X, 20) > natLeaderVals!(80, 1) Then

            natLeaderVals!(80, 1) = statPlyrVal!(X, 20)
            natLeaderVals!(80, 2) = statPlyrVal!(X, 3)
            natLdrName$(80) = statPlyrNames$(X)
            natLdrTeam$(80) = teamName$

            For Z = 61 To 79
                For Z1 = Z + 1 To 80

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natLdrName$(Z1), natLdrName$(Z)
                        Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Steals Per Game
        If statPlyrVal!(X, 15) > natLeaderVals!(90, 1) Then

            natLeaderVals!(90, 1) = statPlyrVal!(X, 15)
            natLeaderVals!(90, 2) = statPlyrVal!(X, 7)
            natLdrName$(90) = statPlyrNames$(X)
            natLdrTeam$(90) = teamName$

            For Z = 81 To 89
                For Z1 = Z + 1 To 90

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natLdrName$(Z1), natLdrName$(Z)
                        Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Steals
        If statPlyrVal!(X, 7) > natLeaderVals!(100, 1) Then

            natLeaderVals!(100, 1) = statPlyrVal!(X, 7)
            natLeaderVals!(100, 2) = 0
            natLdrName$(100) = statPlyrNames$(X)
            natLdrTeam$(100) = teamName$

            For Z = 91 To 99
                For Z1 = Z + 1 To 100

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natLdrName$(Z1), natLdrName$(Z)
                        Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Assist Per Game
        If statPlyrVal!(X, 16) > natLeaderVals!(110, 1) Then

            natLeaderVals!(110, 1) = statPlyrVal!(X, 16)
            natLeaderVals!(110, 2) = statPlyrVal!(X, 8)
            natLdrName$(110) = statPlyrNames$(X)
            natLdrTeam$(110) = teamName$

            For Z = 101 To 109
                For Z1 = Z + 1 To 110

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natLdrName$(Z1), natLdrName$(Z)
                        Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Assists
        If statPlyrVal!(X, 8) > natLeaderVals!(120, 1) Then

            natLeaderVals!(120, 1) = statPlyrVal!(X, 8)
            natLeaderVals!(120, 2) = statPlyrVal!(X, 8)
            natLdrName$(120) = statPlyrNames$(X)
            natLdrTeam$(120) = teamName$

            For Z = 111 To 119
                For Z1 = Z + 1 To 120

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natLdrName$(Z1), natLdrName$(Z)
                        Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        '3 Field Goal %
        If statPlyrVal!(X, 21) > natLeaderVals!(130, 1) Then

            natLeaderVals!(130, 1) = statPlyrVal!(X, 21)
            natLeaderVals!(130, 2) = stats3FGA!(X)
            natLdrName$(130) = statPlyrNames$(X)
            natLdrTeam$(130) = teamName$

            For Z = 121 To 129
                For Z1 = Z + 1 To 130

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natLdrName$(Z1), natLdrName$(Z)
                        Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        '3 Field Goals Made
        If stats3FGM!(X) > natLeaderVals!(140, 1) Then

            natLeaderVals!(140, 1) = stats3FGM!(X)
            natLeaderVals!(140, 2) = 0
            natLdrName$(140) = statPlyrNames$(X)
            natLdrTeam$(140) = teamName$

            For Z = 131 To 139
                For Z1 = Z + 1 To 140

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natLdrName$(Z1), natLdrName$(Z)
                        Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Minutes Per Game
        If statPlyrVal!(X, 18) > natLeaderVals!(150, 1) Then

            natLeaderVals!(150, 1) = statPlyrVal!(X, 18)
            natLeaderVals!(150, 2) = 0
            natLdrName$(150) = statPlyrNames$(X)
            natLdrTeam$(150) = teamName$

            For Z = 141 To 149
                For Z1 = Z + 1 To 150

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natLdrName$(Z1), natLdrName$(Z)
                        Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Total Minutes
        If statPlyrVal!(X, 10) / 60 > natLeaderVals!(160, 1) Then

            natLeaderVals!(160, 1) = statPlyrVal!(X, 10) / 60
            natLeaderVals!(160, 2) = 0
            natLdrName$(160) = statPlyrNames$(X)
            natLdrTeam$(160) = teamName$

            For Z = 151 To 159
                For Z1 = Z + 1 To 160

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natLdrName$(Z1), natLdrName$(Z)
                        Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Rebounds Per Game
        If statPlyrVal!(X, 13) > natLeaderVals!(170, 1) Then

            natLeaderVals!(170, 1) = statPlyrVal!(X, 13)
            natLeaderVals!(170, 2) = statPlyrVal!(X, 5)
            natLdrName$(170) = statPlyrNames$(X)
            natLdrTeam$(170) = teamName$

            For Z = 161 To 169
                For Z1 = Z + 1 To 170

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natLdrName$(Z1), natLdrName$(Z)
                        Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Avg Rebounds
        If statPlyrVal!(X, 5) > natLeaderVals!(180, 1) Then

            natLeaderVals!(180, 1) = statPlyrVal!(X, 5)
            natLeaderVals!(18, 2) = 0
            natLdrName$(180) = statPlyrNames$(X)
            natLdrTeam$(180) = teamName$

            For Z = 171 To 179
                For Z1 = Z + 1 To 180

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natLdrName$(Z1), natLdrName$(Z)
                        Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Blocks Per Game
        If statPlyrVal!(X, 17) > natLeaderVals!(190, 1) Then

            natLeaderVals!(190, 1) = statPlyrVal!(X, 17)
            natLeaderVals!(190, 2) = 0
            natLdrName$(190) = statPlyrNames$(X)
            natLdrTeam$(190) = teamName$

            For Z = 181 To 189
                For Z1 = Z + 1 To 190

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natLdrName$(Z1), natLdrName$(Z)
                        Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Blocks
        If statPlyrVal!(X, 9) > natLeaderVals!(200, 1) Then

            natLeaderVals!(200, 1) = statPlyrVal!(X, 9)
            natLeaderVals!(200, 2) = 0
            natLdrName$(200) = statPlyrNames$(X)
            natLdrTeam$(200) = teamName$

            For Z = 191 To 199
                For Z1 = Z + 1 To 200

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natLdrName$(Z1), natLdrName$(Z)
                        Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Field Goals Made
        If statPlyrVal!(X, 2) > natLeaderVals!(210, 1) Then

            natLeaderVals!(210, 1) = statPlyrVal!(X, 2)
            natLeaderVals!(210, 2) = 0
            natLdrName$(210) = statPlyrNames$(X)
            natLdrTeam$(210) = teamName$

            For Z = 201 To 209
                For Z1 = Z + 1 To 210

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natLdrName$(Z1), natLdrName$(Z)
                        Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Free Throws Made
        If statPlyrVal!(X, 4) > natLeaderVals!(220, 1) Then

            natLeaderVals!(220, 1) = statPlyrVal!(X, 4)
            natLeaderVals!(220, 2) = 0
            natLdrName$(220) = statPlyrNames$(X)
            natLdrTeam$(220) = teamName$

            For Z = 211 To 219
                For Z1 = Z + 1 To 220

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natLdrName$(Z1), natLdrName$(Z)
                        Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Fouls Per Game
        If statPlyrVal!(X, 14) > natLeaderVals!(230, 1) Then

            natLeaderVals!(230, 1) = statPlyrVal!(X, 14)
            natLeaderVals!(230, 2) = 0
            natLdrName$(230) = statPlyrNames$(X)
            natLdrTeam$(230) = teamName$

            fpg! = statPlyrVal!(X, 14)
            test$ = statPlyrNames$(X)

            For Z = 221 To 229
                For Z1 = Z + 1 To 230

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natLdrName$(Z1), natLdrName$(Z)
                        Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Fouls
        If statPlyrVal!(X, 6) > natLeaderVals!(240, 1) Then

            natLeaderVals!(240, 1) = statPlyrVal!(X, 6)
            natLeaderVals!(240, 2) = 0
            natLdrName$(240) = statPlyrNames$(X)
            natLdrTeam$(240) = teamName$

            For Z = 231 To 239
                For Z1 = Z + 1 To 240

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natLdrName$(Z1), natLdrName$(Z)
                        Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'MVP Points
        If statPlyrVal!(X, 24) > natLeaderVals!(250, 1) Then

            natLeaderVals!(250, 1) = statPlyrVal!(X, 24)
            natLeaderVals!(250, 2) = 0
            natLdrName$(250) = statPlyrNames$(X)
            natLdrTeam$(250) = teamName$

            For Z = 241 To 249
                For Z1 = Z + 1 To 250

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natLdrName$(Z1), natLdrName$(Z)
                        Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

    Next X

    'Offensive Totals
    If offTotals!(19) > natOffLeaders!(40, 19) Then

        For M = 1 To 20
            natOffLeaders!(40, M) = offTotals!(M)
        Next

        natOffLdrNames$(40) = teamName$

        For X = 1 To 39
            For X1 = 1 + X To 40

                If natOffLeaders!(X, 19) < natOffLeaders!(X1, 19) Then
                    Swap natOffLdrNames$(X1), natOffLdrNames$(X)
                    For M = 1 To 20
                        Swap natOffLeaders!(X1, M), natOffLeaders!(X, M)
                    Next
                End If

            Next X1
        Next X

    End If

    'Defensive Totals
    If defTotals!(19) < natDefLeaders!(40, 19) Then

        For M = 1 To 20
            natDefLeaders!(40, M) = defTotals!(M)
        Next

        natDefLdrNames$(40) = teamName$

        For X = 1 To 39
            For X1 = 1 + X To 40:

                If natDefLeaders!(X, 19) > natDefLeaders!(X1, 19) Then
                    Swap natDefLdrNames$(X1), natDefLdrNames$(X)
                    For M = 1 To 20
                        Swap natDefLeaders!(X1, M), natDefLeaders!(X, M)
                    Next
                End If

            Next X1
        Next X

    End If

End Sub


'----------------------------------------
'         FindStatsNat Function
'----------------------------------------
'Looks for schedule stats for a specified
'team in the organization and loads them if
'found, notifying the user
Function FindStatsNat% (statFile$, numberGames, NRT)

    Shared oppScore(), powerRating(), teamScore()

    Shared gameSite$()
    Shared locIndicator$(), oppName$(), memberName$()

    Shared confWins, confLosses, fullWins, fullLosses

    Shared gameAttendance&()

    Shared pollTeam$()
    Shared pollRatings#()

    findFile$ = diskPaths$(1) + statFile$ + "S"

    If Not _FileExists(findFile$) Then

        Locate , 1
        Print "NO STATS FOUND FOR "; statFile$
        FindStatsNat% = 0

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
                'If team stat file = opponent of a conference game?
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

        'STRENGTH OF SCHEDULE
        teamPower = 0
        SS = 0

        For I = 1 To numberGames
            SS = powerRating(I, 1) + SS
        Next

        SS = SS / numberGames

        'INDIVIDUAL GAME POWER RATINGS
        GL = 0
        teamPower = 0

        For I = 1 To numberGames
            RAT = teamScore(I) - oppScore(I) + powerRating(I, 1)
            If Left$(locIndicator$(I), 1) = "H" Then 
                RAT = RAT - 4.5
            End If
            If Left$(locIndicator$(I), 1) = "A" Then 
                RAT = RAT + 4.5
            End If
            teamPower = teamPower + RAT
        Next

        If numberGames <= 4 Then
            GL = 5 - numberGames
            teamPower = teamPower + GL * powerRating(1, 0)
        End If

        teamPower = teamPower / (numberGames + GL)
        NRT = NRT + 1
        pollTeam$(NRT) = statFile$
        pollRatings#(NRT, 1) = teamPower
        pollRatings#(NRT, 2) = fullWins
        pollRatings#(NRT, 3) = fullLosses
        pollRatings#(NRT, 4) = SS

        Cls

        Locate 4, 5

        Color 15
        Print "FINDING ";
        Color 14
        Print statFile$;
        Color 15
        Print " STATS            "

        FindStatsNat% = 1

    End If

End Function


'----------------------------------------
'     PostLeadersOrg Subroutine
'----------------------------------------
'This subroutine opens the existing leader
'and record files for the organization,
'and writes the updated stats to the file
'This includes the national poll.
Sub PostLeadersOrg (organization$, NRT)

    Shared seasonWins(), seasonLosses(), seasonWinPct!(), divWins(), divLosses(), divWinPct()

    Shared plyrRecords!()
    Shared natDefLeaders!(), natLeaderVals!(), natOffLeaders!()
    Shared offRecords!(), defRecords!()

    Shared plyrRecDesc$()
    Shared natDefLdrNames$(), natOffLdrNames$(), natLdrName$(), confLdrName$(), natLdrTeam$()
    Shared offRecDesc$(), defRecDesc$()

    Shared pollTeam$()
    Shared pollRatings#()

    Cls

    Open diskPaths$(2) + organization$ + ".NLD" For Output As #1

    Print "    POSTING NEW "; organization$; " LEADERS"

    For X = 1 To 30
        Write #1, confLdrName$(X), seasonWins(X), seasonLosses(X), seasonWinPct!(X), divWins(X), divLosses(X), divWinPct(X)
    Next

    For X = 1 To 250
        Write #1, natLdrName$(X), natLdrTeam$(X), natLeaderVals!(X, 1), natLeaderVals!(X, 2)
    Next X

    For X = 1 To MAX_CONFERENCES
        Write #1, natOffLdrNames$(X)
        For Y = 1 To 20
            Write #1, natOffLeaders!(X, Y)
        Next
    Next

    For X = 1 To MAX_CONFERENCES
        Write #1, natDefLdrNames$(X)
        For Y = 1 To 20
            Write #1, natDefLeaders!(X, Y)
        Next
    Next

    Close #1

    For Z = 1 To NRT - 1
        For Z1 = Z + 1 To NRT
            If pollRatings#(Z, 1) < pollRatings#(Z1, 1) Then
                Swap pollTeam$(Z1), pollTeam$(Z)
                For MN = 1 To 4
                    Swap pollRatings#(Z1, MN), pollRatings#(Z, MN)
                Next MN
            End If
        Next Z1
    Next Z

    Open diskPaths$(2) + organization$ + ".POL" For Output As #2

    Print "POSTING "; organization$; " POLL"

    Write #2, NRT

    For X = 1 To 1200
        Write #2, pollTeam$(X), pollRatings#(X, 1), pollRatings#(X, 2), pollRatings#(X, 3), pollRatings#(X, 4)
    Next

    Close #2

End Sub
