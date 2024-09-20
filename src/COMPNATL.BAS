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
    Shared O1!(), O2!(), O3!(), O4!(), O5!(), O6!()

    Shared defLeaderNames$(), offLeaderNames$()
    Shared offRecDesc$(), defRecDesc$()
    Shared plyrRecDesc$(), statFiles$()
    Shared Z2$(), Z3$()

    Shared confWins, confLosses, fullWins, fullLosses

    Shared natDefLeaders!(), natLeaderVals!(), natOffLeaders!()
    Shared natDefLdrNames$(), natOffLdrNames$(), natZ1$(), NZ1$(), natZ2$()

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
    Erase natDefLeaders!, natLeaderVals!, natOffLeaders!

    Erase natDefLdrNames$, natOffLdrNames$
    Erase natZ1$, NZ1$, natZ2$

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

        Open diskPaths$(2) + organizationFile$ + ".CON" For Input As #2

        Input #2, X$

        For I = 1 To MAX_CONFERENCES
            Input #2, memberConf$(I)
        Next

        Close #2

        For currConf = 1 To MAX_CONFERENCES

            If memberConf$(currConf) <> "XXX" Then

                Cls

                Erase defLeaders!, offLeaders!
                For X = 1 To MAX_CONFERENCES
                    defLeaders!(X, 19) = 9999
                Next

                Erase leaderVals!
                Erase O1!, O2!, O3!, O4!, O5!, O6!
                Erase defLeaderNames$, offLeaderNames$
                Erase statFiles$, Z2$, Z3$

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
                            Input #3, statFiles$(XW)
                        Next

                        Close #3

                        For XW = 1 To TEAMS_PER_CONFERENCE

                            'Look for * as terminating char
                            If statFiles$(XW) <> Chr$(42) Then

                                haveStats = FindStatsNat%(statFiles$(XW), numberGames, NRT)

                                If (haveStats) Then

                                    L = Len(statFiles$(XW))
                                    WT$ = ""

                                    For X = 1 To L
                                        A = Asc(Mid$(statFiles$(XW), X, 1))
                                        WT$ = WT$ + Chr$(A)
                                    Next X

                                    nonConfGame = 0

                                    For X = 1 To TEAMS_PER_CONFERENCE

                                        If statFiles$(X) = WT$ Then

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
                                        Call CalcStatLeadersNat(statFiles$(XW), TN$)

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
    Shared natLeaderVals!(), compS!()
    Shared statsW0!(), statsW1!()

    Shared natDefLdrNames$(), natOffLdrNames$()
    Shared statPlyrNames$()
    Shared natZ1$(), natZ2$()

    Cls
    Print "CHECKING "; statFile$; " STATS AGAINST NATIONAL LEADERS"

    For X = 0 To 14

        'Color 14
        Print "CHECKING STATS OF ";
        Color 15
        Print statPlyrNames$(X)

        'Points Per Game
        If compS!(X, 22) > natLeaderVals!(20, 1) Then

            natLeaderVals!(20, 1) = compS!(X, 22)
            natLeaderVals!(20, 2) = 0
            natZ1$(20) = statPlyrNames$(X)
            natZ2$(20) = teamName$

            For Z = 1 To 19
                For Z1 = Z + 1 To 20

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natZ1$(Z1), natZ1$(Z)
                        Swap natZ2$(Z1), natZ2$(Z)
                        For M = 1 To 2: Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M): Next
                    End If

                Next Z1
            Next Z

        End If

        'Offensive Rebounds
        If compS!(X, 12) > natLeaderVals!(40, 1) Then

            natLeaderVals!(40, 1) = compS!(X, 12)
            natLeaderVals!(40, 2) = 0
            natZ1$(40) = statPlyrNames$(X)
            natZ2$(40) = teamName$

            For Z = 21 To 39
                For Z1 = Z + 1 To 40

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natZ1$(Z1), natZ1$(Z)
                        Swap natZ2$(Z1), natZ2$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Field Goal %
        If compS!(X, 19) > natLeaderVals!(60, 1) Then

            natLeaderVals!(60, 1) = compS!(X, 19)
            natLeaderVals!(60, 2) = compS!(X, 1)
            natZ1$(60) = statPlyrNames$(X)
            natZ2$(60) = teamName$

            For Z = 41 To 59
                For Z1 = Z + 1 To 60

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natZ1$(Z1), natZ1$(Z)
                        Swap natZ2$(Z1), natZ2$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Free Throw %
        If compS!(X, 20) > natLeaderVals!(80, 1) Then

            natLeaderVals!(80, 1) = compS!(X, 20)
            natLeaderVals!(80, 2) = compS!(X, 3)
            natZ1$(80) = statPlyrNames$(X)
            natZ2$(80) = teamName$

            For Z = 61 To 79
                For Z1 = Z + 1 To 80

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natZ1$(Z1), natZ1$(Z)
                        Swap natZ2$(Z1), natZ2$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Steals Per Game
        If compS!(X, 15) > natLeaderVals!(90, 1) Then

            natLeaderVals!(90, 1) = compS!(X, 15)
            natLeaderVals!(90, 2) = compS!(X, 7)
            natZ1$(90) = statPlyrNames$(X)
            natZ2$(90) = teamName$

            For Z = 81 To 89
                For Z1 = Z + 1 To 90

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natZ1$(Z1), natZ1$(Z)
                        Swap natZ2$(Z1), natZ2$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Steals
        If compS!(X, 7) > natLeaderVals!(100, 1) Then

            natLeaderVals!(100, 1) = compS!(X, 7)
            natLeaderVals!(100, 2) = 0
            natZ1$(100) = statPlyrNames$(X)
            natZ2$(100) = teamName$

            For Z = 91 To 99
                For Z1 = Z + 1 To 100

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natZ1$(Z1), natZ1$(Z)
                        Swap natZ2$(Z1), natZ2$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Assist Per Game
        If compS!(X, 16) > natLeaderVals!(110, 1) Then

            natLeaderVals!(110, 1) = compS!(X, 16)
            natLeaderVals!(110, 2) = compS!(X, 8)
            natZ1$(110) = statPlyrNames$(X)
            natZ2$(110) = teamName$

            For Z = 101 To 109
                For Z1 = Z + 1 To 110

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natZ1$(Z1), natZ1$(Z)
                        Swap natZ2$(Z1), natZ2$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Assists
        If compS!(X, 8) > natLeaderVals!(120, 1) Then

            natLeaderVals!(120, 1) = compS!(X, 8)
            natLeaderVals!(120, 2) = compS!(X, 8)
            natZ1$(120) = statPlyrNames$(X)
            natZ2$(120) = teamName$

            For Z = 111 To 119
                For Z1 = Z + 1 To 120

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natZ1$(Z1), natZ1$(Z)
                        Swap natZ2$(Z1), natZ2$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        '3 Field Goal %
        If compS!(X, 21) > natLeaderVals!(130, 1) Then

            natLeaderVals!(130, 1) = compS!(X, 21)
            natLeaderVals!(130, 2) = statsW1!(X)
            natZ1$(130) = statPlyrNames$(X)
            natZ2$(130) = teamName$

            For Z = 121 To 129
                For Z1 = Z + 1 To 130

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natZ1$(Z1), natZ1$(Z)
                        Swap natZ2$(Z1), natZ2$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        '3 Field Goals Made
        If statsW0!(X) > natLeaderVals!(140, 1) Then

            natLeaderVals!(140, 1) = statsW0!(X)
            natLeaderVals!(140, 2) = 0
            natZ1$(140) = statPlyrNames$(X)
            natZ2$(140) = teamName$

            For Z = 131 To 139
                For Z1 = Z + 1 To 140

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natZ1$(Z1), natZ1$(Z)
                        Swap natZ2$(Z1), natZ2$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Minutes Per Game
        If compS!(X, 18) > natLeaderVals!(150, 1) Then

            natLeaderVals!(150, 1) = compS!(X, 18)
            natLeaderVals!(150, 2) = 0
            natZ1$(150) = statPlyrNames$(X)
            natZ2$(150) = teamName$

            For Z = 141 To 149
                For Z1 = Z + 1 To 150

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natZ1$(Z1), natZ1$(Z)
                        Swap natZ2$(Z1), natZ2$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Total Minutes
        If compS!(X, 10) / 60 > natLeaderVals!(160, 1) Then

            natLeaderVals!(160, 1) = compS!(X, 10) / 60
            natLeaderVals!(160, 2) = 0
            natZ1$(160) = statPlyrNames$(X)
            natZ2$(160) = teamName$

            For Z = 151 To 159
                For Z1 = Z + 1 To 160

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natZ1$(Z1), natZ1$(Z)
                        Swap natZ2$(Z1), natZ2$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Rebounds Per Game
        If compS!(X, 13) > natLeaderVals!(170, 1) Then

            natLeaderVals!(170, 1) = compS!(X, 13)
            natLeaderVals!(170, 2) = compS!(X, 5)
            natZ1$(170) = statPlyrNames$(X)
            natZ2$(170) = teamName$

            For Z = 161 To 169
                For Z1 = Z + 1 To 170

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natZ1$(Z1), natZ1$(Z)
                        Swap natZ2$(Z1), natZ2$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Avg Rebounds
        If compS!(X, 5) > natLeaderVals!(180, 1) Then

            natLeaderVals!(180, 1) = compS!(X, 5)
            natLeaderVals!(18, 2) = 0
            natZ1$(180) = statPlyrNames$(X)
            natZ2$(180) = teamName$

            For Z = 171 To 179
                For Z1 = Z + 1 To 180

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natZ1$(Z1), natZ1$(Z)
                        Swap natZ2$(Z1), natZ2$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Blocks Per Game
        If compS!(X, 17) > natLeaderVals!(190, 1) Then

            natLeaderVals!(190, 1) = compS!(X, 17)
            natLeaderVals!(190, 2) = 0
            natZ1$(190) = statPlyrNames$(X)
            natZ2$(190) = teamName$

            For Z = 181 To 189
                For Z1 = Z + 1 To 190

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natZ1$(Z1), natZ1$(Z)
                        Swap natZ2$(Z1), natZ2$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Blocks
        If compS!(X, 9) > natLeaderVals!(200, 1) Then

            natLeaderVals!(200, 1) = compS!(X, 9)
            natLeaderVals!(200, 2) = 0
            natZ1$(200) = statPlyrNames$(X)
            natZ2$(200) = teamName$

            For Z = 191 To 199
                For Z1 = Z + 1 To 200

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natZ1$(Z1), natZ1$(Z)
                        Swap natZ2$(Z1), natZ2$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Field Goals Made
        If compS!(X, 2) > natLeaderVals!(210, 1) Then

            natLeaderVals!(210, 1) = compS!(X, 2)
            natLeaderVals!(210, 2) = 0
            natZ1$(210) = statPlyrNames$(X)
            natZ2$(210) = teamName$

            For Z = 201 To 209
                For Z1 = Z + 1 To 210

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natZ1$(Z1), natZ1$(Z)
                        Swap natZ2$(Z1), natZ2$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Free Throws Made
        If compS!(X, 4) > natLeaderVals!(220, 1) Then

            natLeaderVals!(220, 1) = compS!(X, 4)
            natLeaderVals!(220, 2) = 0
            natZ1$(220) = statPlyrNames$(X)
            natZ2$(220) = teamName$

            For Z = 211 To 219
                For Z1 = Z + 1 To 220

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natZ1$(Z1), natZ1$(Z)
                        Swap natZ2$(Z1), natZ2$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Fouls Per Game
        If compS!(X, 14) > natLeaderVals!(230, 1) Then

            natLeaderVals!(230, 1) = compS!(X, 14)
            natLeaderVals!(230, 2) = 0
            natZ1$(230) = statPlyrNames$(X)
            natZ2$(230) = teamName$

            fpg! = compS!(X, 14)
            test$ = statPlyrNames$(X)

            For Z = 221 To 229
                For Z1 = Z + 1 To 230

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natZ1$(Z1), natZ1$(Z)
                        Swap natZ2$(Z1), natZ2$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Fouls
        If compS!(X, 6) > natLeaderVals!(240, 1) Then

            natLeaderVals!(240, 1) = compS!(X, 6)
            natLeaderVals!(240, 2) = 0
            natZ1$(240) = statPlyrNames$(X)
            natZ2$(240) = teamName$

            For Z = 231 To 239
                For Z1 = Z + 1 To 240

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natZ1$(Z1), natZ1$(Z)
                        Swap natZ2$(Z1), natZ2$(Z)
                        For M = 1 To 2:
                            Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'MVP Points
        If compS!(X, 24) > natLeaderVals!(250, 1) Then

            natLeaderVals!(250, 1) = compS!(X, 24)
            natLeaderVals!(250, 2) = 0
            natZ1$(250) = statPlyrNames$(X)
            natZ2$(250) = teamName$

            For Z = 241 To 249
                For Z1 = Z + 1 To 250

                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap natZ1$(Z1), natZ1$(Z)
                        Swap natZ2$(Z1), natZ2$(Z)
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

    Shared gameSite$(), PT$()
    Shared locIndicator$(), oppName$(), statFiles$()

    Shared confWins, confLosses, fullWins, fullLosses

    Shared PT#()
    Shared gameAttendance&()

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
        PT$(NRT) = statFile$
        PT#(NRT, 1) = teamPower
        PT#(NRT, 2) = fullWins
        PT#(NRT, 3) = fullLosses
        PT#(NRT, 4) = SS

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

    Dim NO1(30), NO2(30), NO3(30), NO4(30), NO5(30), NO6(30)

    Shared PT#()

    Shared plyrRecords!()
    Shared natDefLeaders!(), natLeaderVals!(), natOffLeaders!()
    Shared offRecords!(), defRecords!()

    Shared plyrRecDesc$()
    Shared natDefLdrNames$(), natOffLdrNames$(), natZ1$(), NZ1$(), natZ2$()
    Shared PT$(), offRecDesc$(), defRecDesc$()

    Cls

    Open diskPaths$(2) + organization$ + ".NLD" For Output As #1

    Print "    POSTING NEW "; organization$; " LEADERS"

    For X = 1 To 30
        Write #1, NZ1$(X), NO1(X), NO2(X), NO3(X), NO4(X), NO5(X), NO6(X)
    Next

    For X = 1 To 250
        Write #1, natZ1$(X), natZ2$(X), natLeaderVals!(X, 1), natLeaderVals!(X, 2)
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
            If PT#(Z, 1) < PT#(Z1, 1) Then
                Swap PT$(Z1), PT$(Z)
                For MN = 1 To 4
                    Swap PT#(Z1, MN), PT#(Z, MN)
                Next MN
            End If
        Next Z1
    Next Z

    Open diskPaths$(2) + organization$ + ".POL" For Output As #2

    Print "POSTING "; organization$; " POLL"

    Write #2, NRT

    For X = 1 To 1200
        Write #2, PT$(X), PT#(X, 1), PT#(X, 2), PT#(X, 3), PT#(X, 4)
    Next

    Close #2

End Sub


'----------------------------------------
'      ReadScheduleStats Subroutine
'----------------------------------------
'This subroutine reads in all data from the
'team's schedule stats file.
Sub ReadScheduleStats (statFile$, numberGames)

    Shared teamScore(), oppScore()
    Shared oppName$()

    findFile$ = diskPaths$(1) + statFile$ + "S"

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

    Close #1

End Sub
