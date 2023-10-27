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

    Shared BRC!(), compS!(), DL!()
    Shared LD!(), NDL!(), NLD!(), NOL!(), OL!()
    Shared O1!(), O2!(), O3!(), O4!(), O5!(), O6!()
    Shared TRC!(), TRC1!()

    Shared DT!(), OT!()

    Shared BRC$(), compH$(), compO$(), DL$()
    Shared H1$(), OL$()
    Shared NDL$(), NOL$(), NZ0$(), NZ1$(), NZ3$()
    Shared recconTRC$(), recconTRC1$()
    Shared Z0$(), Z1$(), Z3$()

    Shared mergeO%()

    Shared fullWins, fullLosses

    'Initialize values first
    TRC!(14) = 19999
    TRC!(18) = 19999

    For I = 1 To 13: 
        TRC1!(I) = 19999
    Next

    For I = 15 To 17: 
        TRC1!(I) = 19999
    Next

    For I = 19 To 20: 
        TRC1!(I) = 19999
    Next

    Erase NLD!

    For X = 1 To 40
        NDL!(X, 19) = 9999
    Next

    If _FileExists(diskPaths$(1) + "RECORDS.CBB") Then

        Open diskPaths$(1) + "RECORDS.CBB" For Input As #1

        For I = 1 To 25
            Input #1, BRC!(I)
            Input #1, BRC$(I, 0)
            Input #1, BRC$(I, 1)
        Next

        For I = 1 To 21
            Input #1, TRC!(I)
            Input #1, recconTRC$(I)
        Next

        For I = 1 To 21
            Input #1, TRC1!(I)
            Input #1, recconTRC1$(I)
        Next

        Close #1

    End If

    organizationFile$ = ChooseFile$ (diskPaths$(1) + "*.CON", "ORGANIZATION", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Do
            Color 15, 0
            Cls
            Print "WOULD YOU LIKE TO POST SEASON LEADERS? (Y/N)";
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
                Erase DL!, O1!, O2!, O3!, O4!, O5!, O6!

                For X = 1 To 40
                    DL!(X, 19) = 9999
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

    Shared BRC$()
    Shared compN$(), OL$(), recconTRC$(), recconTRC1$()

    Shared BRC!()
    Shared compS!(), DT!(), OL!(), OT!()
    Shared statsW0!(), statsW1!()
    Shared TRC!(), TRC1!()

    Shared fullWins, fullLosses

    If _FileExists(diskPaths$(1) + statFile$) Then

        Open diskPaths$(1) + statFile$ For Input As #1
        Input #1, TN$

        For I = 0 To 13
            Input #1, compN$(I)
            For J = 0 To 13
                Input #1, compS!(I, J)
            Next
            Input #1, statsW0!(I)
            Input #1, statsW1!(I)
        Next

        For L = 1 To 16: Input #1, OT!(L): Next: For L = 1 To 16: Input #1, DT!(L): Next

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

                If compS!(I, 0) > BRC!(1) Then BRC!(1) = compS!(I, 0): BRC$(1, 0) = compN$(I): BRC$(1, 1) = TN$
                If compS!(I, 1) > BRC!(2) Then BRC!(2) = compS!(I, 1): BRC$(2, 0) = compN$(I): BRC$(2, 1) = TN$
                If compS!(I, 2) > BRC!(3) Then BRC!(3) = compS!(I, 2): BRC$(3, 0) = compN$(I): BRC$(3, 1) = TN$
                If compS!(I, 19) > BRC!(4) And UCase$(finalUpdate$) = "Y" And compS!(I, 2) / numberGames% >= 5 Then BRC!(4) = compS!(I, 19): BRC$(4, 0) = compN$(I): BRC$(4, 1) = TN$

                If statsW1!(I) > BRC!(5) Then BRC!(5) = statsW1!(I): BRC$(5, 0) = compN$(I): BRC$(5, 1) = TN$
                If statsW0!(I) > BRC!(6) Then BRC!(6) = statsW0!(I): BRC$(6, 0) = compN$(I): BRC$(6, 1) = TN$

                If compS!(I, 21) > BRC!(7) And UCase$(finalUpdate$) = "Y" And statsW0!(I) / numberGames% >= 1.5 Then BRC!(7) = compS!(I, 21): BRC$(7, 0) = compN$(I): BRC$(7, 1) = TN$
                If compS!(I, 3) > BRC!(8) Then BRC!(8) = compS!(I, 3): BRC$(8, 0) = compN$(I): BRC$(8, 1) = TN$
                If compS!(I, 4) > BRC!(9) Then BRC!(9) = compS!(I, 4): BRC$(9, 0) = compN$(I): BRC$(9, 1) = TN$
                If compS!(I, 20) > BRC!(10) And UCase$(finalUpdate$) = "Y" And compS!(I, 4) / numberGames% >= 2.5 Then BRC!(10) = compS!(I, 20): BRC$(10, 0) = compN$(I): BRC$(10, 1) = TN$
                If compS!(I, 10) / 60 > BRC!(11) Then BRC!(11) = compS!(I, 10) / 60: BRC$(11, 0) = compN$(I): BRC$(11, 1) = TN$
                If compS!(I, 12) > BRC!(12) Then BRC!(12) = compS!(I, 12): BRC$(12, 0) = compN$(I): BRC$(12, 1) = TN$
                If compS!(I, 22) > BRC!(13) And UCase$(finalUpdate$) = "Y" Then BRC!(13) = compS!(I, 22): BRC$(13, 0) = compN$(I): BRC$(13, 1) = TN$
                If compS!(I, 25) > BRC!(14) Then BRC!(14) = compS!(I, 25): BRC$(14, 0) = compN$(I): BRC$(14, 1) = TN$
                If compS!(I, 26) > BRC!(15) Then BRC!(15) = compS!(I, 26): BRC$(15, 0) = compN$(I): BRC$(15, 1) = TN$
                If compS!(I, 5) - compS!(I, 26) > BRC!(16) Then BRC!(16) = compS!(I, 5) - compS!(I, 26): BRC$(16, 0) = compN$(I): BRC$(16, 1) = TN$
                If compS!(I, 5) > BRC!(17) Then BRC!(17) = compS!(I, 5): BRC$(17, 0) = compN$(I): BRC$(17, 1) = TN$
                If compS!(I, 13) > BRC!(18) And UCase$(finalUpdate$) = "Y" Then BRC!(18) = compS!(I, 13): BRC$(18, 0) = compN$(I): BRC$(18, 1) = TN$
                If compS!(I, 6) > BRC!(19) Then BRC!(19) = compS!(I, 6): BRC$(19, 0) = compN$(I): BRC$(19, 1) = TN$
                If compS!(I, 7) > BRC!(20) Then BRC!(20) = compS!(I, 7): BRC$(20, 0) = compN$(I): BRC$(20, 1) = TN$
                If compS!(I, 8) > BRC!(21) Then BRC!(21) = compS!(I, 8): BRC$(21, 0) = compN$(I): BRC$(21, 1) = TN$
                If compS!(I, 9) > BRC!(22) Then BRC!(22) = compS!(I, 9): BRC$(22, 0) = compN$(I): BRC$(22, 1) = TN$
                If compS!(I, 11) > BRC!(23) Then BRC!(23) = compS!(I, 11): BRC$(23, 0) = compN$(I): BRC$(23, 1) = TN$
                If compS!(I, 18) > BRC!(24) And UCase$(finalUpdate$) = "Y" Then BRC!(24) = compS!(I, 18): BRC$(24, 0) = compN$(I): BRC$(24, 1) = TN$
                If compS!(I, 24) > BRC!(25) Then BRC!(25) = compS!(I, 24): BRC$(25, 0) = compN$(I): BRC$(25, 1) = TN$

            End If

        Next

        OT!(16) = numberGames%: DT!(16) = numberGames%: OT!(17) = OT!(3) / OT!(2): DT!(17) = DT!(3) / DT!(2)
        OT!(18) = OT!(5) / OT!(4): DT!(18) = DT!(5) / DT!(4)
        OT!(20) = OT!(11): DT!(20) = DT!(11)
        If OT!(15) <> 0 Then OT!(21) = OT!(14) / OT!(15) Else OT!(21) = 0
        If DT!(15) <> 0 Then DT!(21) = DT!(14) / DT!(15) Else DT!(21) = 999
        OT!(19) = OT!(20) / numberGames%: DT!(19) = DT!(20) / numberGames%
        If OT!(2) > TRC!(1) Then TRC!(1) = OT!(2): recconTRC$(1) = TN$
        If OT!(3) > TRC!(2) Then TRC!(2) = OT!(3): recconTRC$(2) = TN$
        If OT!(17) > TRC!(3) And UCase$(finalUpdate$) = "Y" Then TRC!(3) = OT!(17): recconTRC$(3) = TN$
        If OT!(15) > TRC!(4) Then TRC!(4) = OT!(15): recconTRC$(4) = TN$
        If OT!(14) > TRC!(5) Then TRC!(5) = OT!(14): recconTRC$(5) = TN$
        If OT!(21) > TRC!(6) And UCase$(finalUpdate$) = "Y" Then TRC!(6) = OT!(21): recconTRC$(6) = TN$
        If OT!(4) > TRC!(7) Then TRC!(7) = OT!(4): recconTRC$(7) = TN$
        If OT!(5) > TRC!(8) Then TRC!(8) = OT!(5): recconTRC$(8) = TN$
        If OT!(18) > TRC!(9) And UCase$(finalUpdate$) = "Y" Then TRC!(9) = OT!(18): recconTRC$(9) = TN$
        If OT!(13) > TRC!(10) Then TRC!(10) = OT!(13): recconTRC$(10) = TN$
        If OT!(6) - OT!(13) > TRC!(11) Then TRC!(11) = OT!(6) - OT!(13): recconTRC$(11) = TN$
        If OT!(6) > TRC!(12) Then TRC!(12) = OT!(6): recconTRC$(12) = TN$
        If OT!(6) / numberGames% > TRC!(13) And UCase$(finalUpdate$) = "Y" Then TRC!(13) = OT!(6) / numberGames%: recconTRC$(13) = TN$
        If OT!(7) < TRC!(14) And UCase$(finalUpdate$) = "Y" Then TRC!(14) = OT!(7): recconTRC$(14) = TN$
        If OT!(8) > TRC!(15) Then TRC!(15) = OT!(8): recconTRC$(15) = TN$
        If OT!(9) > TRC!(16) Then TRC!(16) = OT!(9): recconTRC$(16) = TN$
        If OT!(10) > TRC!(17) Then TRC!(17) = OT!(10): recconTRC$(17) = TN$
        If OT!(12) < TRC!(18) And UCase$(finalUpdate$) = "Y" Then TRC!(18) = OT!(12): recconTRC$(18) = TN$
        If OT!(11) > TRC!(19) Then TRC!(19) = OT!(11): recconTRC$(19) = TN$
        If OT!(11) / numberGames% > TRC!(20) And UCase$(finalUpdate$) = "Y" Then TRC!(20) = OT!(11) / numberGames%: recconTRC$(20) = TN$
        If fullWins > TRC!(21) Then TRC!(21) = fullWins%: recconTRC$(21) = TN$
        If DT!(2) < TRC1!(1) Then TRC1!(1) = DT!(2): recconTRC1$(1) = TN$
        If DT!(3) < TRC1!(2) Then TRC1!(2) = DT!(3): recconTRC1$(2) = TN$
        If DT!(17) < TRC1!(3) And UCase$(finalUpdate$) = "Y" Then TRC1!(3) = DT!(17): recconTRC1$(3) = TN$
        If DT!(15) < TRC1!(4) Then TRC1!(4) = DT!(15): recconTRC1$(4) = TN$
        If DT!(14) < TRC1!(5) Then TRC1!(5) = DT!(14): recconTRC1$(5) = TN$
        If DT!(21) < TRC1!(6) And DT!(15) <> 0 And UCase$(finalUpdate$) = "Y" Then TRC1!(6) = DT!(21): recconTRC1$(6) = TN$
        If DT!(4) < TRC1!(7) Then TRC1!(7) = DT!(4): recconTRC1$(7) = TN$
        If DT!(5) < TRC1!(8) Then TRC1!(8) = DT!(5): recconTRC1$(8) = TN$
        If DT!(18) < TRC1!(9) And UCase$(finalUpdate$) = "Y" Then TRC1!(9) = DT!(18): recconTRC1$(9) = TN$
        If DT!(13) < TRC1!(10) Then TRC1!(10) = DT!(13): recconTRC1$(10) = TN$
        If DT!(6) - DT!(13) < TRC1!(11) Then TRC1!(11) = DT!(6) - DT!(13): recconTRC1$(11) = TN$
        If DT!(6) < TRC1!(12) Then TRC1!(12) = DT!(6): recconTRC1$(12) = TN$
        If DT!(6) / numberGames% < TRC1!(13) And UCase$(finalUpdate$) = "Y" Then TRC1!(13) = DT!(6) / numberGames%: recconTRC1$(13) = TN$
        If DT!(7) > TRC1!(14) And UCase$(finalUpdate$) = "Y" Then TRC1!(14) = DT!(7): recconTRC1$(14) = TN$
        If DT!(8) < TRC1!(15) Then TRC1!(15) = DT!(8): recconTRC1$(15) = TN$
        If DT!(9) < TRC1!(16) Then TRC1!(16) = DT!(9): recconTRC1$(16) = TN$
        If DT!(10) < TRC1!(17) Then TRC1!(17) = DT!(10): recconTRC1$(17) = TN$
        If DT!(12) > TRC1!(18) And UCase$(finalUpdate$) = "Y" Then TRC1!(18) = DT!(12): recconTRC1$(18) = TN$
        If DT!(11) < TRC1!(19) Then TRC1!(19) = DT!(11): recconTRC1$(19) = TN$
        If DT!(11) / numberGames% < TRC1!(20) And UCase$(finalUpdate$) = "Y" Then TRC1!(20) = DT!(11) / numberGames%: recconTRC1$(20) = TN$
        If fullLosses > TRC1!(21) Then TRC1!(21) = fullLosses: recconTRC1$(21) = TN$

    End If

End Sub


'----------------------------------------
'       CheckStatsLeaders Subroutine
'----------------------------------------
'This subroutine compares pre-loaded stats
'to existing leaders and updates as needed
Sub CheckStatsLeaders (statFile$, TN$)

    Shared NDL!(), NOL!(), NLD!()
    Shared compS!(), statsW0!(), statsW1!()

    Shared DT!(), OT!()

    Shared compN$()
    Shared NDL$(), NOL$(), NZ0$(), NZ3$()

    Cls
    Print "CHECKING "; statFile$; " STATS AGAINST NATIONAL LEADERS"

    For X = 0 To 14

        Color 14
        Print "CHECKING STATS OF ";
        Color 15
        Print compN$(X)

        'Division 1, 2 Record
        If compS!(X, 22) > NLD!(20, 1) Then

            NLD!(20, 1) = compS!(X, 22)
            NLD!(20, 2) = 0
            NZ0$(20) = compN$(X)
            NZ3$(20) = TN$

            For Z = 1 To 19
                For Z1 = Z + 1 To 20
                    If NLD!(Z, 1) < NLD!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap NLD!(Z1, M), NLD!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        'Division 3, 4 Record
        If compS!(X, 12) > NLD!(40, 1) Then

            NLD!(40, 1) = compS!(X, 12)
            NLD!(40, 2) = 0
            NZ0$(40) = compN$(X)
            NZ3$(40) = TN$

            For Z = 21 To 39
                For Z1 = Z + 1 To 40
                    If NLD!(Z, 1) < NLD!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap NLD!(Z1, M), NLD!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        'Field Goal %
        If compS!(X, 19) > NLD!(60, 1) Then

            NLD!(60, 1) = compS!(X, 19)
            NLD!(60, 2) = compS!(X, 1)
            NZ0$(60) = compN$(X)
            NZ3$(60) = TN$

            For Z = 41 To 59
                For Z1 = Z + 1 To 60
                    If NLD!(Z, 1) < NLD!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap NLD!(Z1, M), NLD!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        'Free Throw %
        If compS!(X, 20) > NLD!(80, 1) Then

            NLD!(80, 1) = compS!(X, 20)
            NLD!(80, 2) = compS!(X, 3)
            NZ0$(80) = compN$(X)
            NZ3$(80) = TN$

            For Z = 61 To 79
                For Z1 = Z + 1 To 80
                    If NLD!(Z, 1) < NLD!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap NLD!(Z1, M), NLD!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        'Steals Per Game
        If compS!(X, 15) > NLD!(90, 1) Then

            NLD!(90, 1) = compS!(X, 15)
            NLD!(90, 2) = compS!(X, 7)
            NZ0$(90) = compN$(X)
            NZ3$(90) = TN$

            For Z = 81 To 89
                For Z1 = Z + 1 To 90
                    If NLD!(Z, 1) < NLD!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap NLD!(Z1, M), NLD!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        'Steals
        If compS!(X, 7) > NLD!(100, 1) Then

            NLD!(100, 1) = compS!(X, 7)
            NLD!(100, 2) = 0
            NZ0$(100) = compN$(X)
            NZ3$(100) = TN$

            For Z = 91 To 99
                For Z1 = Z + 1 To 100
                    If NLD!(Z, 1) < NLD!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap NLD!(Z1, M), NLD!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        'Assist Per Game
        If compS!(X, 16) > NLD!(110, 1) Then

            NLD!(110, 1) = compS!(X, 16)
            NLD!(110, 2) = compS!(X, 8)
            NZ0$(110) = compN$(X)
            NZ3$(110) = TN$

            For Z = 101 To 109
                For Z1 = Z + 1 To 110
                    If NLD!(Z, 1) < NLD!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap NLD!(Z1, M), NLD!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        'Assists
        If compS!(X, 8) > NLD!(120, 1) Then

            NLD!(120, 1) = compS!(X, 8)
            NLD!(120, 2) = compS!(X, 8)
            NZ0$(120) = compN$(X)
            NZ3$(120) = TN$

            For Z = 111 To 119
                For Z1 = Z + 1 To 120
                    If NLD!(Z, 1) < NLD!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap NLD!(Z1, M), NLD!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        '3 FG %
        If compS!(X, 21) > NLD!(130, 1) Then

            NLD!(130, 1) = compS!(X, 21)
            NLD!(130, 2) = statsW1!(X)
            NZ0$(130) = compN$(X)
            NZ3$(130) = TN$

            For Z = 121 To 129
                For Z1 = Z + 1 To 130
                    If NLD!(Z, 1) < NLD!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap NLD!(Z1, M), NLD!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        'FOR Z=131 TO 140
        If statsW0!(X) > NLD!(140, 1) Then

            NLD!(140, 1) = statsW0!(X)
            NLD!(140, 2) = 0
            NZ0$(140) = compN$(X)
            NZ3$(140) = TN$

            For Z = 131 To 139
                For Z1 = Z + 1 To 140
                    If NLD!(Z, 1) < NLD!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap NLD!(Z1, M), NLD!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        'Minutes Per Game
        If compS!(X, 18) > NLD!(150, 1) Then

            NLD!(150, 1) = compS!(X, 18)
            NLD!(150, 2) = 0
            NZ0$(150) = compN$(X)
            NZ3$(150) = TN$

            For Z = 141 To 149
                For Z1 = Z + 1 To 150
                    If NLD!(Z, 1) < NLD!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap NLD!(Z1, M), NLD!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        'Total Minutes
        If compS!(X, 10) / 60 > NLD!(160, 1) Then

            NLD!(160, 1) = compS!(X, 10) / 60
            NLD!(160, 2) = 0
            NZ0$(160) = compN$(X)
            NZ3$(160) = TN$

            For Z = 151 To 159
                For Z1 = Z + 1 To 160
                    If NLD!(Z, 1) < NLD!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap NLD!(Z1, M), NLD!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        'FOR Z=161 TO 170
        If compS!(X, 13) > NLD!(170, 1) Then

            NLD!(170, 1) = compS!(X, 13)
            NLD!(170, 2) = compS!(X, 5)
            NZ0$(170) = compN$(X)
            NZ3$(170) = TN$

            For Z = 161 To 169
                For Z1 = Z + 1 To 170
                    If NLD!(Z, 1) < NLD!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap NLD!(Z1, M), NLD!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        'Rebounds
        If compS!(X, 5) > NLD!(180, 1) Then

            NLD!(180, 1) = compS!(X, 5)
            NLD!(18, 2) = 0
            NZ0$(180) = compN$(X)
            NZ3$(180) = TN$

            For Z = 171 To 179
                For Z1 = Z + 1 To 180
                    If NLD!(Z, 1) < NLD!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap NLD!(Z1, M), NLD!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        'Blocks Per Game
        If compS!(X, 17) > NLD!(190, 1) Then

            NLD!(190, 1) = compS!(X, 17)
            NLD!(190, 2) = 0
            NZ0$(190) = compN$(X)
            NZ3$(190) = TN$

            For Z = 181 To 189
                For Z1 = Z + 1 To 190
                    If NLD!(Z, 1) < NLD!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap NLD!(Z1, M), NLD!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        'Blocks
        If compS!(X, 9) > NLD!(200, 1) Then

            NLD!(200, 1) = compS!(X, 9)
            NLD!(200, 2) = 0
            NZ0$(200) = compN$(X)
            NZ3$(200) = TN$

            For Z = 191 To 199
                For Z1 = Z + 1 To 200
                    If NLD!(Z, 1) < NLD!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap NLD!(Z1, M), NLD!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        'FG Made
        If compS!(X, 2) > NLD!(210, 1) Then

            NLD!(210, 1) = compS!(X, 2)
            NLD!(210, 2) = 0
            NZ0$(210) = compN$(X)
            NZ3$(210) = TN$

            For Z = 201 To 209
                For Z1 = Z + 1 To 210
                    If NLD!(Z, 1) < NLD!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap NLD!(Z1, M), NLD!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        'FT Made
        If compS!(X, 4) > NLD!(220, 1) Then

            NLD!(220, 1) = compS!(X, 4)
            NLD!(220, 2) = 0
            NZ0$(220) = compN$(X)
            NZ3$(220) = TN$

            For Z = 211 To 219
                For Z1 = Z + 1 To 220
                    If NLD!(Z, 1) < NLD!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap NLD!(Z1, M), NLD!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        'Fouls Per Game
        If compS!(X, 14) > NLD!(230, 1) Then

            NLD!(230, 1) = compS!(X, 14)
            NLD!(230, 2) = 0
            NZ0$(230) = compN$(X)
            NZ3$(230) = TN$

            For Z = 221 To 229
                For Z1 = Z + 1 To 230
                    If NLD!(Z, 1) < NLD!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap NLD!(Z1, M), NLD!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        'Fouls
        If compS!(X, 6) > NLD!(240, 1) Then

            NLD!(240, 1) = compS!(X, 6)
            NLD!(240, 2) = 0
            NZ0$(240) = compN$(X)
            NZ3$(240) = TN$

            For Z = 231 To 239
                For Z1 = Z + 1 To 240
                    If NLD!(Z, 1) < NLD!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap NLD!(Z1, M), NLD!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

        'MVP Points
        If compS!(X, 24) > NLD!(250, 1) Then

            NLD!(250, 1) = compS!(X, 24)
            NLD!(250, 2) = 0
            NZ0$(250) = compN$(X)
            NZ3$(250) = TN$

            For Z = 241 To 249
                For Z1 = Z + 1 To 250
                    If NLD!(Z, 1) < NLD!(Z1, 1) Then
                        Swap NZ0$(Z1), NZ0$(Z)
                        Swap NZ3$(Z1), NZ3$(Z)
                        For M = 1 To 2: Swap NLD!(Z1, M), NLD!(Z, M): Next
                    End If
                Next Z1
            Next Z

        End If

    Next X

    'FOR X=1 TO 40
    If OT!(19) > NOL!(40, 19) Then

        For M = 1 To 20
            NOL!(40, M) = OT!(M)
        Next

        NOL$(40) = TN$

        For X = 1 To 39
            For X1 = 1 + X To 40
                If NOL!(X, 19) < NOL!(X1, 19) Then
                    Swap NOL$(X1), NOL$(X)
                    For M = 1 To 20
                        Swap NOL!(X1, M), NOL!(X, M)
                    Next
                End If
            Next X1
        Next X

    End If

    'FOR X=1 TO 15
    If DT!(19) < NDL!(40, 19) Then

        For M = 1 To 20
            NDL!(40, M) = DT!(M)
        Next

        NDL$(40) = TN$

        For X = 1 To 39
            For X1 = 1 + X To 40:
                If NDL!(X, 19) > NDL!(X1, 19) Then
                    Swap NDL$(X1), NDL$(X)
                    For M = 1 To 20
                        Swap NDL!(X1, M), NDL!(X, M)
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

    Shared DL!(), LD!()
    Shared NDL!(), NOL!(), NLD!()
    Shared OL!(), compS!(), statsW0!(), statsW1!()

    Shared DT!(), OT!()

    Shared compN$(), DL$()
    Shared NDL$(), NOL$(), NZ0$(), NZ3$()
    Shared OL$(), Z0$(), Z3$()

    For X = 0 To 14:

        Color 14
        Print "CHECKING STATS OF ";
        Color 15: Print compN$(X)

        'Division 1, 2 Record
        If compS!(X, 22) > LD!(20, 1) Then

            LD!(20, 1) = compS!(X, 22)
            LD!(20, 2) = 0
            Z0$(20) = compN$(X)
            Z3$(20) = TN$

            For Z = 1 To 19:
                For Z1 = Z + 1 To 20
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If
                Next Z1
            Next Z

        End If

        'Division 3, 4 Record
        If compS!(X, 12) > LD!(40, 1) Then

            LD!(40, 1) = compS!(X, 12)
            LD!(40, 2) = 0
            Z0$(40) = compN$(X)
            Z3$(40) = TN$

            For Z = 21 To 39
                For Z1 = Z + 1 To 40
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If
                Next Z1
            Next Z

        End If

        'Field Goal %
        If compS!(X, 19) > LD!(60, 1) Then

            LD!(60, 1) = compS!(X, 19)
            LD!(60, 2) = compS!(X, 1)
            Z0$(60) = compN$(X)
            Z3$(60) = TN$

            For Z = 41 To 59
                For Z1 = Z + 1 To 60
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If
                Next Z1
            Next Z

        End If

        'Free Throw %
        If compS!(X, 20) > LD!(80, 1) Then

            LD!(80, 1) = compS!(X, 20)
            LD!(80, 2) = compS!(X, 3)
            Z0$(80) = compN$(X)
            Z3$(80) = TN$

            For Z = 61 To 79
                For Z1 = Z + 1 To 80
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If
                Next Z1
            Next Z

        End If

        'Steals Per Game
        If compS!(X, 15) > LD!(90, 1) Then

            LD!(90, 1) = compS!(X, 15)
            LD!(90, 2) = compS!(X, 7)
            Z0$(90) = compN$(X)
            Z3$(90) = TN$

            For Z = 81 To 89
                For Z1 = Z + 1 To 90
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If
                Next Z1
            Next Z

        End If

        'Steals
        If compS!(X, 7) > LD!(100, 1) Then

            LD!(100, 1) = compS!(X, 7)
            LD!(100, 2) = 0
            Z0$(100) = compN$(X)
            Z3$(100) = TN$

            For Z = 91 To 99
                For Z1 = Z + 1 To 100
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If
                Next Z1
            Next Z

        End If

        'Assists Per Game
        If compS!(X, 16) > LD!(110, 1) Then

            LD!(110, 1) = compS!(X, 16)
            LD!(110, 2) = compS!(X, 8)
            Z0$(110) = compN$(X)
            Z3$(110) = TN$

            For Z = 101 To 109
                For Z1 = Z + 1 To 110
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If
                Next Z1
            Next Z

        End If

        'Assists
        If compS!(X, 8) > LD!(120, 1) Then

            LD!(120, 1) = compS!(X, 8)
            LD!(120, 2) = compS!(X, 8)
            Z0$(120) = compN$(X)
            Z3$(120) = TN$

            For Z = 111 To 119
                For Z1 = Z + 1 To 120
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If
                Next Z1
            Next Z

        End If

        '3 FG Percent
        If compS!(X, 21) > LD!(130, 1) Then

            LD!(130, 1) = compS!(X, 21)
            LD!(130, 2) = statsW1!(X)
            Z0$(130) = compN$(X)
            Z3$(130) = TN$

            For Z = 121 To 129
                For Z1 = Z + 1 To 130
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If
                Next Z1
            Next Z

        End If

        '3 FG MADE
        If statsW0!(X) > LD!(140, 1) Then

            LD!(140, 1) = statsW0!(X)
            LD!(140, 2) = 0
            Z0$(140) = compN$(X)
            Z3$(140) = TN$

            For Z = 131 To 139
                For Z1 = Z + 1 To 140
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If
                Next Z1
            Next Z

        End If

        'Minutes Per Game
        If compS!(X, 18) > LD!(150, 1) Then

            LD!(150, 1) = compS!(X, 18)
            LD!(150, 2) = 0
            Z0$(150) = compN$(X)
            Z3$(150) = TN$

            For Z = 141 To 149
                For Z1 = Z + 1 To 150
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If
                Next Z1
            Next Z

        End If

        'Total Minutes
        If compS!(X, 10) / 60 > LD!(160, 1) Then

            LD!(160, 1) = compS!(X, 10) / 60
            LD!(160, 2) = 0
            Z0$(160) = compN$(X)
            Z3$(160) = TN$

            For Z = 151 To 159
                For Z1 = Z + 1 To 160
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If
                Next Z1
            Next Z

        End If

        'Rebounds Per Game
        If compS!(X, 13) > LD!(170, 1) Then

            LD!(170, 1) = compS!(X, 13)
            LD!(170, 2) = compS!(X, 5)
            Z0$(170) = compN$(X)
            Z3$(170) = TN$

            For Z = 161 To 169
                For Z1 = Z + 1 To 170
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If
                Next Z1
            Next Z

        End If

        'Rebounds
        If compS!(X, 5) > LD!(180, 1) Then

            LD!(180, 1) = compS!(X, 5)
            LD!(18, 2) = 0
            Z0$(180) = compN$(X)
            Z3$(180) = TN$

            For Z = 171 To 179
                For Z1 = Z + 1 To 180
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If
                Next Z1
            Next Z

        End If

        'Blocks Per Game
        If compS!(X, 17) > LD!(190, 1) Then

            LD!(190, 1) = compS!(X, 17)
            LD!(190, 2) = 0
            Z0$(190) = compN$(X)
            Z3$(190) = TN$

            For Z = 181 To 189
                For Z1 = Z + 1 To 190
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If
                Next Z1
            Next Z

        End If

        'Blocks
        If compS!(X, 9) > LD!(200, 1) Then

            LD!(200, 1) = compS!(X, 9)
            LD!(200, 2) = 0
            Z0$(200) = compN$(X)
            Z3$(200) = TN$

            For Z = 191 To 199
                For Z1 = Z + 1 To 200
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If
                Next Z1
            Next Z

        End If

        'Field Goals Made
        If compS!(X, 2) > LD!(210, 1) Then

            LD!(210, 1) = compS!(X, 2)
            LD!(210, 2) = 0
            Z0$(210) = compN$(X)
            Z3$(210) = TN$

            For Z = 201 To 209
                For Z1 = Z + 1 To 210
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If
                Next Z1
            Next Z

        End If

        'Free Throws Made
        If compS!(X, 4) > LD!(220, 1) Then

            LD!(220, 1) = compS!(X, 4)
            LD!(220, 2) = 0
            Z0$(220) = compN$(X)
            Z3$(220) = TN$

            For Z = 211 To 219
                For Z1 = Z + 1 To 220
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If
                Next Z1
            Next Z

        End If

        'Fouls Per Game
        If compS!(X, 14) > LD!(230, 1) Then

            LD!(230, 1) = compS!(X, 14)
            LD!(230, 2) = 0
            Z0$(230) = compN$(X)
            Z3$(230) = TN$

            For Z = 221 To 229
                For Z1 = Z + 1 To 230
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If
                Next Z1
            Next Z

        End If

        'Fouls
        If compS!(X, 6) > LD!(240, 1) Then

            LD!(240, 1) = compS!(X, 6)
            LD!(240, 2) = 0
            Z0$(240) = compN$(X)
            Z3$(240) = TN$

            For Z = 231 To 239
                For Z1 = Z + 1 To 240
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If
                Next Z1
            Next Z

        End If

        'MVP Points
        If compS!(X, 24) > LD!(250, 1) Then

            LD!(250, 1) = compS!(X, 24)
            LD!(250, 2) = 0
            Z0$(250) = compN$(X)
            Z3$(250) = TN$

            For Z = 241 To 249
                For Z1 = Z + 1 To 250
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2:
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If
                Next Z1
            Next Z

        End If

    Next X

    'FOR X=1 TO 40
    If OT!(19) > OL!(40, 19) Then

        For M = 1 To 20:
            OL!(40, M) = OT!(M)
        Next

        OL$(40) = TN$

        For X = 1 To 39:
            For X1 = 1 + X To 40:
                If OL!(X, 19) < OL!(X1, 19) Then
                    Swap OL$(X1), OL$(X)
                    For M = 1 To 20:
                        Swap OL!(X1, M), OL!(X, M)
                    Next
                End If
            Next X1
        Next X

    End If

    'FOR X=1 TO 15
    If DT!(19) < DL!(40, 19) Then

        For M = 1 To 20:
            DL!(40, M) = DT!(M)
        Next

        DL$(40) = TN$

        For X = 1 To 39
            For X1 = 1 + X To 40:
                If DL!(X, 19) > DL!(X1, 19) Then
                    Swap DL$(X1), DL$(X)
                    For M = 1 To 20:
                        Swap DL!(X1, M), DL!(X, M)
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

    Dim compAP%(100, 1), compT%(1 To NUM_STATRECORDS)
    Dim CRD(100)

    Dim gameSite$(100)

    Shared mergeO%()

    Shared PT#()

    Shared compO$(), compH$(), H1$(), PT$()

    Shared confWins, confLosses
    Shared fullWins, fullLosses

    If Not _FileExists(diskPaths$(1) + statFile$ + "S") Then

        Print "NO STATS FOUND FOR "; statFile$
        FindStatsNat% = 0

    Else

        Open diskPaths$(1) + statFile$ + "S" For Input As #1

        Input #1, numberGames%

        For I = 1 To NUM_STATRECORDS: Input #1, mergeO%(I): Next I

        For I = 1 To NUM_STATRECORDS: Input #1, compO$(I): Next I

        For I = 1 To NUM_STATRECORDS: Input #1, compT%(I): Next I

        For I = 1 To NUM_STATRECORDS: Input #1, compH$(I): Next I

        For I = 1 To NUM_STATRECORDS
            For I1 = 0 To 1
                Input #1, compAP%(I, I1)
            Next
        Next

        For I = 1 To NUM_STATRECORDS: Input #1, CRD(I): Next I
        For I = 1 To NUM_STATRECORDS: Input #1, gameSite$(I): Next I

        Close #1

        fullWins = 0
        fullLosses = 0
        confWins = 0
        confLosses = 0

        For I = 1 To numberGames%

            For P = 4 To 11
                If Mid$(compO$(I), P, 1) = " " Or Mid$(compO$(I), P, 1) = "." Then Mid$(compO$(I), P, 1) = "_"
            Next P

            For P = 12 To 14
                If Mid$(compO$(I), P, 2) = "ST" Then Mid$(compO$(I), 9, 1) = "_": Mid$(compO$(I), 10, 2) = "ST"
            Next P

            QQ = 8
            For P = 11 To 4 Step -1:
                If Mid$(compO$(I), P, 1) = "_" Then
                    QQ = P - 4
                Else
                    Exit For
                End If
            Next P

            f$ = Mid$(compO$(I), 4, QQ)
            R$ = f$ + "." + Left$(compO$(I), 2)
            compO$(I) = R$

            confGame = 0

            For EC = 1 To 30

                If compO$(I) = H1$(EC) Then
                    confGame = 1
                    Exit For
                End If

            Next EC

            If confGame <> 1 Then
                If mergeO%(I) > compT%(I) Then fullWins = fullWins + 1
                If mergeO%(I) < compT%(I) Then fullLosses = fullLosses + 1
            Else
                If mergeO%(I) > compT%(I) Then confWins = confWins + 1: fullWins = fullWins + 1
                If mergeO%(I) < compT%(I) Then confLosses = confLosses + 1: fullLosses = fullLosses + 1
            End If

            EC = 0

        Next I

        'STRENGTH OF SCHEDULE
        PW = 0
        SS = 0

        For I = 1 To numberGames%
            SS = compAP%(I, 1) + SS
        Next

        SS = SS / numberGames%

        'INDIVIDUAL GAME POWER RATINGS
        GL = 0
        PW = 0

        For I = 1 To numberGames%
            RAT = mergeO%(I) - compT%(I) + compAP%(I, 1)
            If Left$(compH$(I), 1) = "H" Then RAT = RAT - 4.5
            If Left$(compH$(I), 1) = "A" Then RAT = RAT + 4.5
            PW = PW + RAT
        Next

        If numberGames% <= 4 Then
            GL = 5 - numberGames%
            PW = PW + GL * compAP%(1, 0)
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

    Shared DL!(), LD!(), OL!()
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
        Write #1, Z0$(X), Z3$(X), LD!(X, 1), LD!(X, 2)
    Next X

    For X = 1 To 40:
        Write #1, OL$(X)
        For Y = 1 To 20:
            Write #1, OL!(X, Y)
        Next
    Next

    For X = 1 To 40:
        Write #1, DL$(X)
        For Y = 1 To 20:
            Write #1, DL!(X, Y)
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

    Shared BRC!()
    Shared NDL!(), NLD!(), NOL!()
    Shared TRC!(), TRC1!()

    Shared BRC$()
    Shared NDL$(), NOL$(), NZ0$(), NZ1$(), NZ3$()
    Shared PT$(), recconTRC$(), recconTRC1$()

    Cls

    Open diskPaths$(1) + organization$ + ".NLD" For Output As #1

    Print "    POSTING NEW "; organization$; " LEADERS"

    For X = 1 To 30
        Write #1, NZ1$(X), NO1(X), NO2(X), NO3(X), NO4(X), NO5(X), NO6(X)
    Next

    For X = 1 To 250
        Write #1, NZ0$(X), NZ3$(X), NLD!(X, 1), NLD!(X, 2)
    Next X

    For X = 1 To 40
        Write #1, NOL$(X)
        For Y = 1 To 20
            Write #1, NOL!(X, Y)
        Next
    Next

    For X = 1 To 40
        Write #1, NDL$(X)
        For Y = 1 To 20
            Write #1, NDL!(X, Y)
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
        Print #3, BRC!(I)
        Print #3, BRC$(I, 0)
        Print #3, BRC$(I, 1)
    Next

    For I = 1 To 21
        Print #3, TRC!(I)
        Print #3, recconTRC$(I)
    Next

    For I = 1 To 21
        Print #3, TRC1!(I)
        Print #3, recconTRC1$(I)
    Next

    Close #3

End Sub

