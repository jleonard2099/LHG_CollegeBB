Sub CompileCollege ()

    _Title "Courtside College Basketball - Update Conference Stats"

    Shared mergeT%()

    Shared BRC!(), DL!()
    Shared LD!(), OL!()
    Shared compS!(), TRC!(), TRC1!()
    Shared statsW0!(), statsW1!()
    Shared DT!(), OT!()
    Shared O1!(), O2!(), O3!(), O4!(), O5!(), O6!()

    Shared BRC$(), DL$(), compN$(), compO$()
    Shared H1$(), OL$()
    Shared recconTRC1$(), recconTRC$()
    Shared Z0$(), Z1$(), Z3$()

    Shared mergeO%()

    Shared fullWins, fullLosses

    'Initialize values first
    TRC!(14) = 19999
    TRC!(18) = 19999

    For I = 1 To 13: TRC1!(I) = 19999: Next
    For I = 15 To 17: TRC1!(I) = 19999: Next
    For I = 19 To 20: TRC1!(I) = 19999: Next

    Erase Z1$
    Erase DL!

    For X = 1 To 40
        DL!(X, 19) = 9999
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

        Close 1

    End If

    Cls

    Do
        'Display list of files...
        Spec$ = diskPaths$(1) + "*.LIB"
        Count% = FCount%(Spec$)

        If Count% = 0 Then

            Locate 10, 8
            Call CSMsgBox("NO CONFERENCE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, PASS)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, PASS)

        Else

            ReDim conferenceFiles$(0 To Count%)

            For X = 1 To Count%
                conferenceFiles$(X) = Space$(12)
            Next

            conferenceFiles$(0) = Spec$

            Call ReadFile(conferenceFiles$())

            Do
                Cls
                Color , 1
                Cls
                Locate , 28
                Color 14
                Print "SELECT CONFERENCE TO UPDATE": Color 15
                Selection% = 1
                START% = 1
                FileFGColr% = 15: FileBGColr% = 1
                HiLiteFGColr% = 0: HiLiteBGColr% = 7
                Rows% = 20
                Columns% = 4
                Spacing% = 6
                Row% = 4
                Column% = 1
                Locate 25, 1
                Color 14
                Print "ENTER";: Color 15
                Print "-SELECTS CONFERENCE";

                _MouseShow "DEFAULT"
                MAMenu conferenceFiles$(), Selection%, START%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
                _MouseHide

                If ExitCode% <> 27 Then

                    Color 15, 1
                    targetFile$ = conferenceFiles$(Selection%)
                    Locate 2, 33: Print targetFile$

                    Do
                        Locate 3, 33: Print "ANY CHANGE (YN)";
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                    Color 15, 0
                Else
                    BO% = 1
                End If

            Loop Until UCase$(I$) = "N" Or ExitCode% = 27

            If ExitCode% <> 27 Then

                targetFile$ = RTrim$(targetFile$)
                WE$ = Left$(targetFile$, Len(targetFile$) - 4)

                Do
                    Cls
                    Print "IS THIS THE FINAL UPDATE FOR THE SEASON (YN)";
                    Color 15, 0
                    I$ = GetKeyPress$
                    finalUpdate$ = UCase$(I$)
                Loop Until finalUpdate$ = "Y" Or finalUpdate$ = "N"

                If UCase$(I$) = "Y" Then finalUpdate$ = "Y"

                Open diskPaths$(1) + WE$ + ".TMS" For Input As #2

                Input #2, LG$
                Input #2, D1$
                Input #2, D2$

                For XW = 1 To 30
                    Input #2, H1$(XW)
                    Z1$(XW) = H1$(XW)
                Next

                Close 2

                For XW = 1 To 30

                    'Look for * as terminating char
                    If H1$(XW) <> Chr$(42) Then

                        haveStats = FindStatsCon%(H1$(XW), numberGames%)

                        If (haveStats) Then

                            L = Len(H1$(XW))
                            WT$ = ""

                            For X = 1 To L
                                A = Asc(Mid$(H1$(XW), X, 1))
                                WT$ = WT$ + Chr$(A)
                            Next X

                            confGame = 0

                            For X = 1 To TEAMS_PER_CONFERENCE

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

                            If confGame <> 1 And _FileExists(diskPaths$(1) + H1$(XW)) Then

                                For I = 0 To 13:
                                    For J = 0 To 22
                                        compS!(I, J) = 0
                                    Next
                                Next

                                Call ReadStatsCon(H1$(XW), TN$)
                                Call StatsByCategoryCon(numberGames%, TN$, finalUpdate$)
                                Call CheckStatsCon(TN$)
                            End If

                        End If 'Done checking if we have stats

                    End If 'Done checking valid conference

                Next XW 'Done "checking stats"

                Call SortLeadersConf
                Call PostLeadersConf(WE$)

            End If 'Done checking for ESC

        End If 'Done checking for file count

        '...Until we press ESC
    Loop Until ExitCode% = 27 Or Count% = 0

    Cls

End Sub


'----------------------------------------
'     StatsByCategoryCon Subroutine
'----------------------------------------
Sub StatsByCategoryCon (numberGames%, TN$, finalUpdate$)

    Shared BRC!(), compS!(), DT!(), OT!()
    Shared TRC!(), TRC1!()
    Shared statsW0!(), statsW1!()

    Shared BRC$(), compN$()
    Shared recconTRC$(), recconTRC1$()

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

            If Not (compS!(I, 1) = 0 Or (compS!(I, 2) / numberGames% < 5)) Then
                compS!(I, 19) = compS!(I, 2) / compS!(I, 1) ' *** FG% ***
            End If

            If Not (compS!(I, 3) = 0 Or (compS!(I, 4) / numberGames%) < 2.5) Then
                compS!(I, 20) = compS!(I, 4) / compS!(I, 3) ' *** FT% ***
            End If

            If Not (statsW1!(I) = 0 Or statsW0!(I) / numberGames% < 1.5) Then
                compS!(I, 21) = statsW0!(I) / statsW1!(I) ' *** 3FG% ***
            End If

            compS!(I, 22) = compS!(I, 12) / compS!(I, 0) ' *** PTS/G ***

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

    OT!(16) = numberGames%
    DT!(16) = numberGames%
    OT!(17) = OT!(3) / OT!(2)
    DT!(17) = DT!(3) / DT!(2)
    OT!(18) = OT!(5) / OT!(4)
    DT!(18) = DT!(5) / DT!(4)
    OT!(20) = OT!(11)
    DT!(20) = DT!(11)

    If OT!(15) <> 0 Then OT!(21) = OT!(14) / OT!(15) Else OT!(21) = 0
    If DT!(15) <> 0 Then DT!(21) = DT!(14) / DT!(15) Else DT!(21) = 999

    OT!(19) = OT!(20) / numberGames%
    DT!(19) = DT!(20) / numberGames%

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
    If fullWins > TRC!(21) Then TRC!(21) = fullWins: recconTRC$(21) = TN$
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

End Sub


'----------------------------------------
'     CheckStatsCon Subroutine
'----------------------------------------
Sub CheckStatsCon (TN$)

    Shared compS!(), DL!(), DT!()
    Shared LD!(), OL!(), OT!()
    Shared statsW0!(), statsW1!()

    Shared compN$(), DL$(), OL$()
    Shared Z0$(), Z3$()


    For X = 0 To 14:

        Color 14: Print "CHECKING STATS OF ";
        Color 15: Print compN$(X)

        'Division 1, 2 Record
        If compS!(X, 22) > LD!(20, 1) Then

            LD!(20, 1) = compS!(X, 22)
            LD!(20, 2) = 0
            Z0$(20) = compN$(X)
            Z3$(20) = TN$

            For Z = 1 To 19:
                For Z1 = Z + 1 To 20:
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

        'Assist Per Game
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

        '3 FG %
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

        'FG Made
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

        'FT Made
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

        For X = 1 To 39
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
            For X1 = 1 + X To 40
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
'       SortLeadersConf Subroutine
'----------------------------------------
'Sort leaders in conference
Sub SortLeadersConf

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
'         FindStatsCon Function
'----------------------------------------
'...explanation...
Function FindStatsCon% (teamName$, numberGames%)

    Shared mergeT%(), mergeO%()

    Shared compO$(), H1$()

    Shared fullWins, fullLosses

    If _FileExists(diskPaths$(1) + teamName$ + "S") Then

        Open diskPaths$(1) + teamName$ + "S" For Input As #2

        Input #2, numberGames%

        For I = 1 To 100: Input #2, mergeO%(I): Next I
        For I = 1 To 100: Input #2, compO$(I): Next I
        For I = 1 To 100: Input #2, mergeT%(I): Next I

        Close 2

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
            For P = 11 To 4 Step -1
                If Mid$(compO$(I), P, 1) = "_" Then
                    QQ = P - 4
                Else
                    Exit For '5605 GoTo 5608
                End If
            Next P

            targetFile$ = Mid$(compO$(I), 4, QQ)
            R$ = targetFile$ + "." + Left$(compO$(I), 2)
            compO$(I) = R$

            confGame = 0

            For EC = 1 To 30
                'If team stat file = opponent of a conference game?
                If compO$(I) = H1$(EC) Then
                    confGame = 1
                    Exit For
                End If

            Next EC

            If confGame <> 1 Then
                If mergeO%(I) > mergeT%(I) Then fullWins = fullWins + 1
                If mergeO%(I) < mergeT%(I) Then fullLosses = fullLosses + 1
            Else
                If mergeO%(I) > mergeT%(I) Then confWins = confWins + 1: fullWins = fullWins + 1
                If mergeO%(I) < mergeT%(I) Then confLosses = confLosses + 1: fullLosses = fullLosses + 1
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

    Else

        Print "NO STATS FOUND FOR "; statFile$
        FindStatsCon% = 0

    End If

End Function


'----------------------------------------
'       ReadStatsCon Subroutine
'----------------------------------------
'...explanation...
Sub ReadStatsCon (statFile$, TN$)

    Shared compS!(), DT!(), OT!()
    Shared statsW0!(), statsW1!()

    Shared compN$()

    Open diskPaths$(1) + statFile$ For Input As #2

    Input #2, TN$

    For I = 0 To 13

        Input #2, compN$(I)

        For J = 0 To 13:
            Input #2, compS!(I, J)
        Next

        Input #2, statsW0!(I)

        Input #2, statsW1!(I)

    Next

    For L = 1 To 16:
        Input #2, OT!(L)
    Next

    For L = 1 To 16:
        Input #2, DT!(L)
    Next

    Close 2

End Sub


'------------------------------
'   PostLeadersNat Subroutine
'------------------------------
' ...explanation...
Sub PostLeadersConf (leaderFile$)

    Shared DL$(), OL$()

    Shared BRC!(), DL!(), LD!()
    Shared O1!(), O2!(), O3!(), O4!(), O5!(), O6!()
    Shared BRC!(), OL!(), TRC!(), TRC1!()

    Shared BRC$(), recconTRC$(), recconTRC1$()
    Shared Z0$(), Z1$(), Z3$()

    Cls

    Print

    Open diskPaths$(1) + leaderFile$ + ".LDR" For Output As #2

    Print "    POSTING NEW "; leaderFile$; " CONFERENCE LEADERS"

    For X = 1 To 30
        Write #2, Z1$(X), O1!(X), O2!(X), O3!(X), O4!(X), O5!(X), O6!(X)
    Next

    For X = 1 To 250
        Write #2, Z0$(X), Z3$(X), LD!(X, 1), LD!(X, 2)
    Next X

    For X = 1 To 40:
        Write #2, OL$(X)
        For Y = 1 To 20:
            Write #2, OL!(X, Y)
        Next
    Next

    For X = 1 To 40:
        Write #2, DL$(X):
        For Y = 1 To 20:
            Write #2, DL!(X, Y)
        Next
    Next

    Close 2

    Open diskPaths$(1) + "RECORDS.CBB" For Output As #1

    For I = 1 To 25
        Print #1, BRC!(I)
        Print #1, BRC$(I, 0)
        Print #1, BRC$(I, 1)
    Next

    For I = 1 To 21
        Print #1, TRC!(I)
        Print #1, recconTRC$(I)
    Next

    For I = 1 To 21
        Print #1, TRC1!(I)
        Print #1, recconTRC1$(I)
    Next

    Close 1

End Sub

