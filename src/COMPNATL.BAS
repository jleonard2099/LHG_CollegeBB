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

    Dim CN$(1 To 40)

    Shared PT#()

    Shared plyrRecords!(), compS!()
    Shared defLeaders!(), defTotals!(), offTotals!()
    Shared leaderVals!(), natDefLeaders!(), natLeaderVals!(), natOffLeaders!()
    Shared O1!(), O2!(), O3!(), O4!(), O5!(), O6!()
    Shared offRecords!(), defRecords!()


    Shared plyrRecDesc$(), locIndicator$(), oppName$(), DL$()
    Shared H1$(), OL$()
    Shared NDL$(), NOL$(), NZ0$(), NZ1$(), NZ3$()
    Shared offRecDesc$(), defRecDesc$()
    Shared Z0$(), Z1$(), Z3$()

    Shared teamScore()

    Shared fullWins, fullLosses

    'Initialize values first
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

    Erase natLeaderVals!

    For X = 1 To 40
        natDefLeaders!(X, 19) = 9999
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

    End If

    organizationFile$ = ChooseFile$ (diskPaths$(1) + "*.CON", "ORGANIZATION", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Do
            Color 15, 0
            Cls
            Print "WOULD YOU LIKE TO POST SEASON RECORDS? (Y/N)";
            I$ = GetKeyPress$
            finalUpdate$ = UCase$(I$)
        Loop Until finalUpdate$ = "Y" Or finalUpdate$ = "N"

        If UCase$(I$) = "Y" Then finalUpdate$ = "Y"

        Open diskPaths$(1) + organizationFile$ + ".CON" For Input As #2

        Input #2, X$

        For I = 1 To 40
            Input #2, CN$(I)
        Next

        Close #2

        For NC = 1 To 40

            If CN$(NC) <> "XXX" Then

                Cls

                Erase Z1$
                Erase defLeaders!, O1!, O2!, O3!, O4!, O5!, O6!

                For X = 1 To 40
                    defLeaders!(X, 19) = 9999
                Next

                For X = 1 To 30
                    O1!(X) = 0
                    O2!(X) = 0
                    O3!(X) = 0
                    O4!(X) = 0
                    O5!(X) = 0
                    O6!(X) = 0
                Next

                Open diskPaths$(1) + CN$(NC) + ".TMS" For Input As #3

                Input #3, LG$
                Input #3, D1$
                Input #3, D2$

                For XW = 1 To 30
                    Input #3, H1$(XW)
                    Z1$(XW) = H1$(XW)
                Next

                Close #3

                For XW = 1 To 30

                    fullWins = 0
                    fullLosses = 0
                    confWins = 0
                    confLosses = 0

                    'Look for * as terminating char
                    If H1$(XW) <> Chr$(42) Then

                        haveStats = FindStatsNat%(H1$(XW), numberGames%, NRT)

                        If (haveStats) Then

                            L = Len(H1$(XW))
                            WT$ = ""

                            For X = 1 To L
                                A = Asc(Mid$(H1$(XW), X, 1))
                                WT$ = WT$ + Chr$(A)
                            Next X

                            confGame = 0

                            For X = 1 To 30

                                If Z1$(X) = WT$ Then

                                    If O1!(X) = fullWins And O2!(X) = fullLosses Then
                                        confGame = 1
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

                                If (confGame) Then Exit For

                            Next X

                            Erase compS!

                            If confGame <> 1 And _FileExists(diskPaths$(1) + H1$(XW)) Then
                                Call ReadStatsNat(H1$(XW), numberGames%, TN$)
                                Call StatsByCategoryTeam(TN$)
                                Call CheckStatsLeaders(H1$(XW), TN$)
                            End If

                        End If

                    End If 'Done looking for * as char

                Next XW

                Call SortLeadersNat
                Call PostConLeaders_Nat(CN$(NC))

            End If

        Next NC

        Call PostOrgLeaders_Nat(organizationFile$, NRT)

    End If

End Sub


'----------------------------------------
'       ReadStatsNat Subroutine
'----------------------------------------
'This subroutine reads stats in for a 
'single team in the organization
Sub ReadStatsNat (statFile$, numberGames%, TN$)

    Shared plyrRecDesc$()
    Shared statPlyrNames$(), OL$(), offRecDesc$(), defRecDesc$()

    Shared plyrRecords!()
    Shared compS!(), defTotals!(), offTotals!()
    Shared statsW0!(), statsW1!()
    Shared offRecords!(), defRecords!()

    Shared fullWins, fullLosses

    If _FileExists(diskPaths$(1) + statFile$) Then

        Open diskPaths$(1) + statFile$ For Input As #1
        Input #1, TN$

        For I = 0 To 13
            Input #1, statPlyrNames$(I)
            For J = 0 To 13
                Input #1, compS!(I, J)
            Next
            Input #1, statsW0!(I)
            Input #1, statsW1!(I)
        Next

        For L = 1 To 16: Input #1, offTotals!(L): Next: For L = 1 To 16: Input #1, defTotals!(L): Next

        Close #1

        For I = 0 To 13

            compS!(I, 25) = compS!(I, 13)
            compS!(I, 26) = compS!(I, 12)
            compS!(I, 12) = compS!(I, 2) * 2 + statsW0!(I) + compS!(I, 4): ' *** PTS ***

            If compS!(I, 0) <> 0 Then

                compS!(I, 13) = compS!(I, 5) / compS!(I, 0): ' *** RB/G ***
                compS!(I, 14) = compS!(I, 6) / compS!(I, 0): ' *** FOULS/G ***
                compS!(I, 15) = compS!(I, 7) / compS!(I, 0): ' *** STEALS/G ***
                compS!(I, 16) = compS!(I, 8) / compS!(I, 0): ' *** ASSISTS/G ***
                compS!(I, 17) = compS!(I, 9) / compS!(I, 0): ' *** BLOCKS/G ***
                compS!(I, 24) = compS!(I, 5) + compS!(I, 12) - compS!(I, 6) + compS!(I, 8) + compS!(I, 9) + compS!(I, 7) - 2 * compS!(I, 11)
                compS!(I, 18) = compS!(I, 10) / 60 / compS!(I, 0): ' *** MIN/G ***

                If compS!(I, 1) <> 0 And (compS!(I, 2) / numberGames% >= 5) Then
                    compS!(I, 19) = compS!(I, 2) / compS!(I, 1): ' *** FG% ***
                End If

                If compS!(I, 3) <> 0 And (compS!(I, 4) / numberGames%) >= 2.5 Then
                    compS!(I, 20) = compS!(I, 4) / compS!(I, 3): ' *** FT% ***
                End If

                If statsW1!(I) <> 0 And statsW0!(I) / numberGames% >= 1.5 Then
                    compS!(I, 21) = statsW0!(I) / statsW1!(I): ' *** 3FG% ***
                End If

                compS!(I, 22) = compS!(I, 12) / compS!(I, 0): ' *** PTS/G ***

                If compS!(I, 0) > plyrRecords!(1) Then plyrRecords!(1) = compS!(I, 0): plyrRecDesc$(1, 0) = statPlyrNames$(I): plyrRecDesc$(1, 1) = TN$
                If compS!(I, 1) > plyrRecords!(2) Then plyrRecords!(2) = compS!(I, 1): plyrRecDesc$(2, 0) = statPlyrNames$(I): plyrRecDesc$(2, 1) = TN$
                If compS!(I, 2) > plyrRecords!(3) Then plyrRecords!(3) = compS!(I, 2): plyrRecDesc$(3, 0) = statPlyrNames$(I): plyrRecDesc$(3, 1) = TN$
                If compS!(I, 19) > plyrRecords!(4) And UCase$(finalUpdate$) = "Y" And compS!(I, 2) / numberGames% >= 5 Then plyrRecords!(4) = compS!(I, 19): plyrRecDesc$(4, 0) = statPlyrNames$(I): plyrRecDesc$(4, 1) = TN$

                If statsW1!(I) > plyrRecords!(5) Then plyrRecords!(5) = statsW1!(I): plyrRecDesc$(5, 0) = statPlyrNames$(I): plyrRecDesc$(5, 1) = TN$
                If statsW0!(I) > plyrRecords!(6) Then plyrRecords!(6) = statsW0!(I): plyrRecDesc$(6, 0) = statPlyrNames$(I): plyrRecDesc$(6, 1) = TN$

                If compS!(I, 21) > plyrRecords!(7) And UCase$(finalUpdate$) = "Y" And statsW0!(I) / numberGames% >= 1.5 Then plyrRecords!(7) = compS!(I, 21): plyrRecDesc$(7, 0) = statPlyrNames$(I): plyrRecDesc$(7, 1) = TN$
                If compS!(I, 3) > plyrRecords!(8) Then plyrRecords!(8) = compS!(I, 3): plyrRecDesc$(8, 0) = statPlyrNames$(I): plyrRecDesc$(8, 1) = TN$
                If compS!(I, 4) > plyrRecords!(9) Then plyrRecords!(9) = compS!(I, 4): plyrRecDesc$(9, 0) = statPlyrNames$(I): plyrRecDesc$(9, 1) = TN$
                If compS!(I, 20) > plyrRecords!(10) And UCase$(finalUpdate$) = "Y" And compS!(I, 4) / numberGames% >= 2.5 Then plyrRecords!(10) = compS!(I, 20): plyrRecDesc$(10, 0) = statPlyrNames$(I): plyrRecDesc$(10, 1) = TN$
                If compS!(I, 10) / 60 > plyrRecords!(11) Then plyrRecords!(11) = compS!(I, 10) / 60: plyrRecDesc$(11, 0) = statPlyrNames$(I): plyrRecDesc$(11, 1) = TN$
                If compS!(I, 12) > plyrRecords!(12) Then plyrRecords!(12) = compS!(I, 12): plyrRecDesc$(12, 0) = statPlyrNames$(I): plyrRecDesc$(12, 1) = TN$
                If compS!(I, 22) > plyrRecords!(13) And UCase$(finalUpdate$) = "Y" Then plyrRecords!(13) = compS!(I, 22): plyrRecDesc$(13, 0) = statPlyrNames$(I): plyrRecDesc$(13, 1) = TN$
                If compS!(I, 25) > plyrRecords!(14) Then plyrRecords!(14) = compS!(I, 25): plyrRecDesc$(14, 0) = statPlyrNames$(I): plyrRecDesc$(14, 1) = TN$
                If compS!(I, 26) > plyrRecords!(15) Then plyrRecords!(15) = compS!(I, 26): plyrRecDesc$(15, 0) = statPlyrNames$(I): plyrRecDesc$(15, 1) = TN$
                If compS!(I, 5) - compS!(I, 26) > plyrRecords!(16) Then plyrRecords!(16) = compS!(I, 5) - compS!(I, 26): plyrRecDesc$(16, 0) = statPlyrNames$(I): plyrRecDesc$(16, 1) = TN$
                If compS!(I, 5) > plyrRecords!(17) Then plyrRecords!(17) = compS!(I, 5): plyrRecDesc$(17, 0) = statPlyrNames$(I): plyrRecDesc$(17, 1) = TN$
                If compS!(I, 13) > plyrRecords!(18) And UCase$(finalUpdate$) = "Y" Then plyrRecords!(18) = compS!(I, 13): plyrRecDesc$(18, 0) = statPlyrNames$(I): plyrRecDesc$(18, 1) = TN$
                If compS!(I, 6) > plyrRecords!(19) Then plyrRecords!(19) = compS!(I, 6): plyrRecDesc$(19, 0) = statPlyrNames$(I): plyrRecDesc$(19, 1) = TN$
                If compS!(I, 7) > plyrRecords!(20) Then plyrRecords!(20) = compS!(I, 7): plyrRecDesc$(20, 0) = statPlyrNames$(I): plyrRecDesc$(20, 1) = TN$
                If compS!(I, 8) > plyrRecords!(21) Then plyrRecords!(21) = compS!(I, 8): plyrRecDesc$(21, 0) = statPlyrNames$(I): plyrRecDesc$(21, 1) = TN$
                If compS!(I, 9) > plyrRecords!(22) Then plyrRecords!(22) = compS!(I, 9): plyrRecDesc$(22, 0) = statPlyrNames$(I): plyrRecDesc$(22, 1) = TN$
                If compS!(I, 11) > plyrRecords!(23) Then plyrRecords!(23) = compS!(I, 11): plyrRecDesc$(23, 0) = statPlyrNames$(I): plyrRecDesc$(23, 1) = TN$
                If compS!(I, 18) > plyrRecords!(24) And UCase$(finalUpdate$) = "Y" Then plyrRecords!(24) = compS!(I, 18): plyrRecDesc$(24, 0) = statPlyrNames$(I): plyrRecDesc$(24, 1) = TN$
                If compS!(I, 24) > plyrRecords!(25) Then plyrRecords!(25) = compS!(I, 24): plyrRecDesc$(25, 0) = statPlyrNames$(I): plyrRecDesc$(25, 1) = TN$

            End If

        Next

        offTotals!(16) = numberGames%
        defTotals!(16) = numberGames%
        offTotals!(17) = offTotals!(3) / offTotals!(2)
        defTotals!(17) = defTotals!(3) / defTotals!(2)
        offTotals!(18) = offTotals!(5) / offTotals!(4)
        defTotals!(18) = defTotals!(5) / defTotals!(4)
        offTotals!(20) = offTotals!(11): defTotals!(20) = defTotals!(11)

        If offTotals!(15) <> 0 Then offTotals!(21) = offTotals!(14) / offTotals!(15) Else offTotals!(21) = 0
        If defTotals!(15) <> 0 Then defTotals!(21) = defTotals!(14) / defTotals!(15) Else defTotals!(21) = 999

        offTotals!(19) = offTotals!(20) / numberGames%
        defTotals!(19) = defTotals!(20) / numberGames%

        If offTotals!(2) > offRecords!(1) Then offRecords!(1) = offTotals!(2): offRecDesc$(1) = TN$
        If offTotals!(3) > offRecords!(2) Then offRecords!(2) = offTotals!(3): offRecDesc$(2) = TN$
        If offTotals!(17) > offRecords!(3) And UCase$(finalUpdate$) = "Y" Then offRecords!(3) = offTotals!(17): offRecDesc$(3) = TN$
        If offTotals!(15) > offRecords!(4) Then offRecords!(4) = offTotals!(15): offRecDesc$(4) = TN$
        If offTotals!(14) > offRecords!(5) Then offRecords!(5) = offTotals!(14): offRecDesc$(5) = TN$
        If offTotals!(21) > offRecords!(6) And UCase$(finalUpdate$) = "Y" Then offRecords!(6) = offTotals!(21): offRecDesc$(6) = TN$
        If offTotals!(4) > offRecords!(7) Then offRecords!(7) = offTotals!(4): offRecDesc$(7) = TN$
        If offTotals!(5) > offRecords!(8) Then offRecords!(8) = offTotals!(5): offRecDesc$(8) = TN$
        If offTotals!(18) > offRecords!(9) And UCase$(finalUpdate$) = "Y" Then offRecords!(9) = offTotals!(18): offRecDesc$(9) = TN$
        If offTotals!(13) > offRecords!(10) Then offRecords!(10) = offTotals!(13): offRecDesc$(10) = TN$
        If offTotals!(6) - offTotals!(13) > offRecords!(11) Then offRecords!(11) = offTotals!(6) - offTotals!(13): offRecDesc$(11) = TN$
        If offTotals!(6) > offRecords!(12) Then offRecords!(12) = offTotals!(6): offRecDesc$(12) = TN$
        If offTotals!(6) / numberGames% > offRecords!(13) And UCase$(finalUpdate$) = "Y" Then offRecords!(13) = offTotals!(6) / numberGames%: offRecDesc$(13) = TN$
        If offTotals!(7) < offRecords!(14) And UCase$(finalUpdate$) = "Y" Then offRecords!(14) = offTotals!(7): offRecDesc$(14) = TN$
        If offTotals!(8) > offRecords!(15) Then offRecords!(15) = offTotals!(8): offRecDesc$(15) = TN$
        If offTotals!(9) > offRecords!(16) Then offRecords!(16) = offTotals!(9): offRecDesc$(16) = TN$
        If offTotals!(10) > offRecords!(17) Then offRecords!(17) = offTotals!(10): offRecDesc$(17) = TN$
        If offTotals!(12) < offRecords!(18) And UCase$(finalUpdate$) = "Y" Then offRecords!(18) = offTotals!(12): offRecDesc$(18) = TN$
        If offTotals!(11) > offRecords!(19) Then offRecords!(19) = offTotals!(11): offRecDesc$(19) = TN$
        If offTotals!(11) / numberGames% > offRecords!(20) And UCase$(finalUpdate$) = "Y" Then offRecords!(20) = offTotals!(11) / numberGames%: offRecDesc$(20) = TN$
        If fullWins > offRecords!(21) Then offRecords!(21) = fullWins%: offRecDesc$(21) = TN$
        If defTotals!(2) < defRecords!(1) Then defRecords!(1) = defTotals!(2): defRecDesc$(1) = TN$
        If defTotals!(3) < defRecords!(2) Then defRecords!(2) = defTotals!(3): defRecDesc$(2) = TN$
        If defTotals!(17) < defRecords!(3) And UCase$(finalUpdate$) = "Y" Then defRecords!(3) = defTotals!(17): defRecDesc$(3) = TN$
        If defTotals!(15) < defRecords!(4) Then defRecords!(4) = defTotals!(15): defRecDesc$(4) = TN$
        If defTotals!(14) < defRecords!(5) Then defRecords!(5) = defTotals!(14): defRecDesc$(5) = TN$
        If defTotals!(21) < defRecords!(6) And defTotals!(15) <> 0 And UCase$(finalUpdate$) = "Y" Then defRecords!(6) = defTotals!(21): defRecDesc$(6) = TN$
        If defTotals!(4) < defRecords!(7) Then defRecords!(7) = defTotals!(4): defRecDesc$(7) = TN$
        If defTotals!(5) < defRecords!(8) Then defRecords!(8) = defTotals!(5): defRecDesc$(8) = TN$
        If defTotals!(18) < defRecords!(9) And UCase$(finalUpdate$) = "Y" Then defRecords!(9) = defTotals!(18): defRecDesc$(9) = TN$
        If defTotals!(13) < defRecords!(10) Then defRecords!(10) = defTotals!(13): defRecDesc$(10) = TN$
        If defTotals!(6) - defTotals!(13) < defRecords!(11) Then defRecords!(11) = defTotals!(6) - defTotals!(13): defRecDesc$(11) = TN$
        If defTotals!(6) < defRecords!(12) Then defRecords!(12) = defTotals!(6): defRecDesc$(12) = TN$
        If defTotals!(6) / numberGames% < defRecords!(13) And UCase$(finalUpdate$) = "Y" Then defRecords!(13) = defTotals!(6) / numberGames%: defRecDesc$(13) = TN$
        If defTotals!(7) > defRecords!(14) And UCase$(finalUpdate$) = "Y" Then defRecords!(14) = defTotals!(7): defRecDesc$(14) = TN$
        If defTotals!(8) < defRecords!(15) Then defRecords!(15) = defTotals!(8): defRecDesc$(15) = TN$
        If defTotals!(9) < defRecords!(16) Then defRecords!(16) = defTotals!(9): defRecDesc$(16) = TN$
        If defTotals!(10) < defRecords!(17) Then defRecords!(17) = defTotals!(10): defRecDesc$(17) = TN$
        If defTotals!(12) > defRecords!(18) And UCase$(finalUpdate$) = "Y" Then defRecords!(18) = defTotals!(12): defRecDesc$(18) = TN$
        If defTotals!(11) < defRecords!(19) Then defRecords!(19) = defTotals!(11): defRecDesc$(19) = TN$
        If defTotals!(11) / numberGames% < defRecords!(20) And UCase$(finalUpdate$) = "Y" Then defRecords!(20) = defTotals!(11) / numberGames%: defRecDesc$(20) = TN$

        If fullLosses > defRecords!(21) Then defRecords!(21) = fullLosses: defRecDesc$(21) = TN$

    End If

End Sub


'----------------------------------------
'       CheckStatsLeaders Subroutine
'----------------------------------------
'This subroutine compares pre-loaded stats
'to existing leaders and updates as needed
Sub CheckStatsLeaders (statFile$, TN$)

    Shared natDefLeaders!(), natOffLeaders!(), natLeaderVals!()
    Shared compS!(), statsW0!(), statsW1!()

    Shared defTotals!(), offTotals!()

    Shared statPlyrNames$()
    Shared NDL$(), NOL$(), NZ0$(), NZ3$()

    Cls
    Print "CHECKING "; statFile$; " STATS AGAINST NATIONAL LEADERS"

    For X = 0 To 14

        Color 14
        Print "CHECKING STATS OF ";
        Color 15
        Print statPlyrNames$(X)

        'Division 1, 2 Record
        If compS!(X, 22) > natLeaderVals!(20, 1) Then

            natLeaderVals!(20, 1) = compS!(X, 22)
            natLeaderVals!(20, 2) = 0
            NZ0$(20) = statPlyrNames$(X)
            NZ3$(20) = TN$

            For Z = 1 To 19
                For Z1 = Z + 1 To 20
                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        'Division 3, 4 Record
        If compS!(X, 12) > natLeaderVals!(40, 1) Then

            natLeaderVals!(40, 1) = compS!(X, 12)
            natLeaderVals!(40, 2) = 0
            NZ0$(40) = statPlyrNames$(X)
            NZ3$(40) = TN$

            For Z = 21 To 39
                For Z1 = Z + 1 To 40
                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        'Field Goal %
        If compS!(X, 19) > natLeaderVals!(60, 1) Then

            natLeaderVals!(60, 1) = compS!(X, 19)
            natLeaderVals!(60, 2) = compS!(X, 1)
            NZ0$(60) = statPlyrNames$(X)
            NZ3$(60) = TN$

            For Z = 41 To 59
                For Z1 = Z + 1 To 60
                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        'Free Throw %
        If compS!(X, 20) > natLeaderVals!(80, 1) Then

            natLeaderVals!(80, 1) = compS!(X, 20)
            natLeaderVals!(80, 2) = compS!(X, 3)
            NZ0$(80) = statPlyrNames$(X)
            NZ3$(80) = TN$

            For Z = 61 To 79
                For Z1 = Z + 1 To 80
                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        'Steals Per Game
        If compS!(X, 15) > natLeaderVals!(90, 1) Then

            natLeaderVals!(90, 1) = compS!(X, 15)
            natLeaderVals!(90, 2) = compS!(X, 7)
            NZ0$(90) = statPlyrNames$(X)
            NZ3$(90) = TN$

            For Z = 81 To 89
                For Z1 = Z + 1 To 90
                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        'Steals
        If compS!(X, 7) > natLeaderVals!(100, 1) Then

            natLeaderVals!(100, 1) = compS!(X, 7)
            natLeaderVals!(100, 2) = 0
            NZ0$(100) = statPlyrNames$(X)
            NZ3$(100) = TN$

            For Z = 91 To 99
                For Z1 = Z + 1 To 100
                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        'Assist Per Game
        If compS!(X, 16) > natLeaderVals!(110, 1) Then

            natLeaderVals!(110, 1) = compS!(X, 16)
            natLeaderVals!(110, 2) = compS!(X, 8)
            NZ0$(110) = statPlyrNames$(X)
            NZ3$(110) = TN$

            For Z = 101 To 109
                For Z1 = Z + 1 To 110
                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        'Assists
        If compS!(X, 8) > natLeaderVals!(120, 1) Then

            natLeaderVals!(120, 1) = compS!(X, 8)
            natLeaderVals!(120, 2) = compS!(X, 8)
            NZ0$(120) = statPlyrNames$(X)
            NZ3$(120) = TN$

            For Z = 111 To 119
                For Z1 = Z + 1 To 120
                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        '3 FG %
        If compS!(X, 21) > natLeaderVals!(130, 1) Then

            natLeaderVals!(130, 1) = compS!(X, 21)
            natLeaderVals!(130, 2) = statsW1!(X)
            NZ0$(130) = statPlyrNames$(X)
            NZ3$(130) = TN$

            For Z = 121 To 129
                For Z1 = Z + 1 To 130
                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        'FOR Z=131 TO 140
        If statsW0!(X) > natLeaderVals!(140, 1) Then

            natLeaderVals!(140, 1) = statsW0!(X)
            natLeaderVals!(140, 2) = 0
            NZ0$(140) = statPlyrNames$(X)
            NZ3$(140) = TN$

            For Z = 131 To 139
                For Z1 = Z + 1 To 140
                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        'Minutes Per Game
        If compS!(X, 18) > natLeaderVals!(150, 1) Then

            natLeaderVals!(150, 1) = compS!(X, 18)
            natLeaderVals!(150, 2) = 0
            NZ0$(150) = statPlyrNames$(X)
            NZ3$(150) = TN$

            For Z = 141 To 149
                For Z1 = Z + 1 To 150
                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        'Total Minutes
        If compS!(X, 10) / 60 > natLeaderVals!(160, 1) Then

            natLeaderVals!(160, 1) = compS!(X, 10) / 60
            natLeaderVals!(160, 2) = 0
            NZ0$(160) = statPlyrNames$(X)
            NZ3$(160) = TN$

            For Z = 151 To 159
                For Z1 = Z + 1 To 160
                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        'FOR Z=161 TO 170
        If compS!(X, 13) > natLeaderVals!(170, 1) Then

            natLeaderVals!(170, 1) = compS!(X, 13)
            natLeaderVals!(170, 2) = compS!(X, 5)
            NZ0$(170) = statPlyrNames$(X)
            NZ3$(170) = TN$

            For Z = 161 To 169
                For Z1 = Z + 1 To 170
                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        'Rebounds
        If compS!(X, 5) > natLeaderVals!(180, 1) Then

            natLeaderVals!(180, 1) = compS!(X, 5)
            natLeaderVals!(18, 2) = 0
            NZ0$(180) = statPlyrNames$(X)
            NZ3$(180) = TN$

            For Z = 171 To 179
                For Z1 = Z + 1 To 180
                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        'Blocks Per Game
        If compS!(X, 17) > natLeaderVals!(190, 1) Then

            natLeaderVals!(190, 1) = compS!(X, 17)
            natLeaderVals!(190, 2) = 0
            NZ0$(190) = statPlyrNames$(X)
            NZ3$(190) = TN$

            For Z = 181 To 189
                For Z1 = Z + 1 To 190
                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        'Blocks
        If compS!(X, 9) > natLeaderVals!(200, 1) Then

            natLeaderVals!(200, 1) = compS!(X, 9)
            natLeaderVals!(200, 2) = 0
            NZ0$(200) = statPlyrNames$(X)
            NZ3$(200) = TN$

            For Z = 191 To 199
                For Z1 = Z + 1 To 200
                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        'FG Made
        If compS!(X, 2) > natLeaderVals!(210, 1) Then

            natLeaderVals!(210, 1) = compS!(X, 2)
            natLeaderVals!(210, 2) = 0
            NZ0$(210) = statPlyrNames$(X)
            NZ3$(210) = TN$

            For Z = 201 To 209
                For Z1 = Z + 1 To 210
                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        'FT Made
        If compS!(X, 4) > natLeaderVals!(220, 1) Then

            natLeaderVals!(220, 1) = compS!(X, 4)
            natLeaderVals!(220, 2) = 0
            NZ0$(220) = statPlyrNames$(X)
            NZ3$(220) = TN$

            For Z = 211 To 219
                For Z1 = Z + 1 To 220
                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        'Fouls Per Game
        If compS!(X, 14) > natLeaderVals!(230, 1) Then

            natLeaderVals!(230, 1) = compS!(X, 14)
            natLeaderVals!(230, 2) = 0
            NZ0$(230) = statPlyrNames$(X)
            NZ3$(230) = TN$

            For Z = 221 To 229
                For Z1 = Z + 1 To 230
                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        'Fouls
        If compS!(X, 6) > natLeaderVals!(240, 1) Then

            natLeaderVals!(240, 1) = compS!(X, 6)
            natLeaderVals!(240, 2) = 0
            NZ0$(240) = statPlyrNames$(X)
            NZ3$(240) = TN$

            For Z = 231 To 239
                For Z1 = Z + 1 To 240
                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        'MVP Points
        If compS!(X, 24) > natLeaderVals!(250, 1) Then

            natLeaderVals!(250, 1) = compS!(X, 24)
            natLeaderVals!(250, 2) = 0
            NZ0$(250) = statPlyrNames$(X)
            NZ3$(250) = TN$

            For Z = 241 To 249
                For Z1 = Z + 1 To 250
                    If natLeaderVals!(Z, 1) < natLeaderVals!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap natLeaderVals!(Z1, M), natLeaderVals!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

    Next X

    'FOR X=1 TO 40
    If offTotals!(19) > natOffLeaders!(40, 19) Then

        For M = 1 To 20
            natOffLeaders!(40, M) = offTotals!(M)
        Next

        NOL$(40) = TN$

        For X = 1 To 39
            For X1 = 1 + X To 40
                If natOffLeaders!(X, 19) < natOffLeaders!(X1, 19) Then
                    Swap NOL$(X1), NOL$(X)
                    For M = 1 To 20
                        Swap natOffLeaders!(X1, M), natOffLeaders!(X, M)
                    Next
                End If
            Next X1
        Next X

    End If

    'FOR X=1 TO 15
    If defTotals!(19) < natDefLeaders!(40, 19) Then

        For M = 1 To 20
            natDefLeaders!(40, M) = defTotals!(M)
        Next

        NDL$(40) = TN$

        For X = 1 To 39
            For X1 = 1 + X To 40:
                If natDefLeaders!(X, 19) > natDefLeaders!(X1, 19) Then
                    Swap NDL$(X1), NDL$(X)
                    For M = 1 To 20
                        Swap natDefLeaders!(X1, M), natDefLeaders!(X, M)
                    Next
                End If
            Next X1
        Next X

    End If

End Sub


'----------------------------------------
'     StatsByCategoryTeam Subroutine
'----------------------------------------
'This subroutine compares all pre-loaded 
'stats to each of the record categories to 
'see if any should be updated
Sub StatsByCategoryTeam (TN$)

    Shared defLeaders!(), leaderVals!()
    Shared natDefLeaders!(), natOffLeaders!(), natLeaderVals!()
    Shared offLeaders!(), compS!(), statsW0!(), statsW1!()

    Shared defTotals!(), offTotals!()

    Shared statPlyrNames$(), DL$()
    Shared NDL$(), NOL$(), NZ0$(), NZ3$()
    Shared OL$(), Z0$(), Z3$()

    For X = 0 To 14:

        Color 14
        Print "CHECKING STATS OF ";
        Color 15: Print statPlyrNames$(X)

        'Division 1, 2 Record
        If compS!(X, 22) > leaderVals!(20, 1) Then

            leaderVals!(20, 1) = compS!(X, 22)
            leaderVals!(20, 2) = 0
            Z0$(20) = statPlyrNames$(X)
            Z3$(20) = TN$

            For Z = 1 To 19:
                For Z1 = Z + 1 To 20
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
            Z3$(40) = TN$

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
            Z3$(60) = TN$

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
            Z3$(80) = TN$

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
            Z3$(90) = TN$

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
            Z3$(100) = TN$

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

        'Assists Per Game
        If compS!(X, 16) > leaderVals!(110, 1) Then

            leaderVals!(110, 1) = compS!(X, 16)
            leaderVals!(110, 2) = compS!(X, 8)
            Z0$(110) = statPlyrNames$(X)
            Z3$(110) = TN$

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
            Z3$(120) = TN$

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

        '3 FG Percent
        If compS!(X, 21) > leaderVals!(130, 1) Then

            leaderVals!(130, 1) = compS!(X, 21)
            leaderVals!(130, 2) = statsW1!(X)
            Z0$(130) = statPlyrNames$(X)
            Z3$(130) = TN$

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

        '3 FG MADE
        If statsW0!(X) > leaderVals!(140, 1) Then

            leaderVals!(140, 1) = statsW0!(X)
            leaderVals!(140, 2) = 0
            Z0$(140) = statPlyrNames$(X)
            Z3$(140) = TN$

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
            Z3$(150) = TN$

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
            Z3$(160) = TN$

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
            Z3$(170) = TN$

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
            Z3$(180) = TN$

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
            Z3$(190) = TN$

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
            Z3$(200) = TN$

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
            Z3$(210) = TN$

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
            Z3$(220) = TN$

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
            Z3$(230) = TN$

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
            Z3$(240) = TN$

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
            Z3$(250) = TN$

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

    'FOR X=1 TO 40
    If offTotals!(19) > offLeaders!(40, 19) Then

        For M = 1 To 20:
            offLeaders!(40, M) = offTotals!(M)
        Next

        OL$(40) = TN$

        For X = 1 To 39:
            For X1 = 1 + X To 40:
                If offLeaders!(X, 19) < offLeaders!(X1, 19) Then
                    Swap OL$(X1), OL$(X)
                    For M = 1 To 20:
                        Swap offLeaders!(X1, M), offLeaders!(X, M)
                    Next
                End If
            Next X1
        Next X

    End If

    'FOR X=1 TO 15
    If defTotals!(19) < defLeaders!(40, 19) Then

        For M = 1 To 20:
            defLeaders!(40, M) = defTotals!(M)
        Next

        DL$(40) = TN$

        For X = 1 To 39
            For X1 = 1 + X To 40:
                If defLeaders!(X, 19) > defLeaders!(X1, 19) Then
                    Swap DL$(X1), DL$(X)
                    For M = 1 To 20:
                        Swap defLeaders!(X1, M), defLeaders!(X, M)
                    Next
                End If
            Next X1
        Next X

    End If

End Sub


'----------------------------------------
'         FindStatsNat Function
'----------------------------------------
'...explanation...
Function FindStatsNat% (statFile$, numberGames%, NRT)

    Shared powerRating(), teamScore(), oppScore()

    Shared PT#()

    Shared H1$(), PT$()
    Shared locIndicator$(), oppName$()

    Shared confWins, confLosses
    Shared fullWins, fullLosses

    If Not _FileExists(diskPaths$(1) + statFile$ + "S") Then

        Print "NO STATS FOUND FOR "; statFile$
        FindStatsNat% = 0

    Else

        Open diskPaths$(1) + statFile$ + "S" For Input As #1

        Input #1, numberGames%

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

        For I = 1 To MAX_SCHED_STATS
            For I1 = 0 To 1
                Input #1, powerRating(I, I1)
            Next
        Next

        For I = 1 To MAX_SCHED_STATS: 
            Input #1, gameAttendance&(I)
        Next I
        
        For I = 1 To MAX_SCHED_STATS: 
            Input #1, gameSite$(I)
        Next I

        Close #1

        fullWins = 0
        fullLosses = 0
        confWins = 0
        confLosses = 0

        For I = 1 To numberGames%

            statName$ = nameStatFile$(oppName$(I))

            confGame = 0

            For EC = 1 To 30

                If statName$ = H1$(EC) Then
                    confGame = 1
                    Exit For
                End If

            Next EC

            If confGame <> 1 Then
                If teamScore(I) > oppScore(I) Then fullWins = fullWins + 1
                If teamScore(I) < oppScore(I) Then fullLosses = fullLosses + 1
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
        PW = 0
        SS = 0

        For I = 1 To numberGames%
            SS = powerRating(I, 1) + SS
        Next

        SS = SS / numberGames%

        'INDIVIDUAL GAME POWER RATINGS
        GL = 0
        PW = 0

        For I = 1 To numberGames%
            RAT = teamScore(I) - oppScore(I) + powerRating(I, 1)
            If Left$(locIndicator$(I), 1) = "H" Then RAT = RAT - 4.5
            If Left$(locIndicator$(I), 1) = "A" Then RAT = RAT + 4.5
            PW = PW + RAT
        Next

        If numberGames% <= 4 Then
            GL = 5 - numberGames%
            PW = PW + GL * powerRating(1, 0)
        End If

        PW = PW / (numberGames% + GL)
        NRT = NRT + 1
        PT$(NRT) = statFile$
        PT#(NRT, 1) = PW
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
'       SortLeadersNat Subroutine
'----------------------------------------
'This subroutine sorts the leaders in the
'target organization
Sub SortLeadersNat

    Shared O1!(), O2!(), O3!(), O4!(), O5!(), O6!()
    Shared Z1$()

    For Z = 0 To 1

        AA = (Z * 15) + 1
        ZZ = (Z * 15) + 10

        For X = AA To ZZ - 1

            For Y = X + 1 To ZZ

                diffX = O1!(X) - O2!(X)
                diffY = O1!(Y) - O2!(Y)

                If diffX <= diffY Then
                    If diffX <> diffY Then
                        If Not (O1!(X) <= O1!(Y) And O1!(X) > O2!(X)) Or Not (O1!(X) >= O1!(Y) And O1!(X) < O2!(X)) Then
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
'     PostConLeaders_Nat Subroutine
'----------------------------------------
'This subroutine opens the existing leader
'and record files for the conferences,
'and writes the updated stats to the file
Sub PostConLeaders_Nat (conferenceName$)

    Shared defLeaders!(), leaderVals!(), offLeaders!()
    Shared O1!(), O2!(), O3!(), O4!(), O5!(), O6!()

    Shared DL$(), OL$()
    Shared Z0$(), Z1$(), Z3$()

    Cls
    Print

    Open diskPaths$(1) + conferenceName$ + ".LDR" For Output As #1

    Print "    POSTING NEW "; conferenceName$; " CONFERENCE LEADERS"

    For X = 1 To 30:
        Write #1, Z1$(X), O1!(X), O2!(X), O3!(X), O4!(X), O5!(X), O6!(X)
    Next

    For X = 1 To 250
        Write #1, Z0$(X), Z3$(X), leaderVals!(X, 1), leaderVals!(X, 2)
    Next X

    For X = 1 To 40:
        Write #1, OL$(X)
        For Y = 1 To 20:
            Write #1, offLeaders!(X, Y)
        Next
    Next

    For X = 1 To 40:
        Write #1, DL$(X)
        For Y = 1 To 20:
            Write #1, defLeaders!(X, Y)
        Next
    Next

    Close #1

End Sub


'----------------------------------------
'     PostOrgLeaders_Nat Subroutine
'----------------------------------------
'This subroutine opens the existing leader
'and record files for the organization,
'and writes the updated stats to the file
'This includes the national poll.
Sub PostOrgLeaders_Nat (organization$, NRT)

    Dim NO1(30), NO2(30), NO3(30), NO4(30), NO5(30), NO6(30)

    Shared PT#()

    Shared plyrRecords!()
    Shared natDefLeaders!(), natLeaderVals!(), natOffLeaders!()
    Shared offRecords!(), defRecords!()

    Shared plyrRecDesc$()
    Shared NDL$(), NOL$(), NZ0$(), NZ1$(), NZ3$()
    Shared PT$(), offRecDesc$(), defRecDesc$()

    Cls

    Open diskPaths$(1) + organization$ + ".NLD" For Output As #1

    Print "    POSTING NEW "; organization$; " LEADERS"

    For X = 1 To 30
        Write #1, NZ1$(X), NO1(X), NO2(X), NO3(X), NO4(X), NO5(X), NO6(X)
    Next

    For X = 1 To 250
        Write #1, NZ0$(X), NZ3$(X), natLeaderVals!(X, 1), natLeaderVals!(X, 2)
    Next X

    For X = 1 To 40
        Write #1, NOL$(X)
        For Y = 1 To 20
            Write #1, natOffLeaders!(X, Y)
        Next
    Next

    For X = 1 To 40
        Write #1, NDL$(X)
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

    Open diskPaths$(1) + organization$ + ".POL" For Output As #2

    Print "POSTING "; organization$; " POLL"

    Write #2, NRT

    For X = 1 To 1200
        Write #2, PT$(X), PT#(X, 1), PT#(X, 2), PT#(X, 3), PT#(X, 4)
    Next

    Close #2

    Open diskPaths$(1) + "RECORDS.CBB" For Output As #3

    For I = 1 To 25
        Print #3, plyrRecords!(I)
        Print #3, plyrRecDesc$(I, 0)
        Print #3, plyrRecDesc$(I, 1)
    Next

    For I = 1 To 21
        Print #3, offRecords!(I)
        Print #3, offRecDesc$(I)
    Next

    For I = 1 To 21
        Print #3, defRecords!(I)
        Print #3, defRecDesc$(I)
    Next

    Close #3

End Sub

