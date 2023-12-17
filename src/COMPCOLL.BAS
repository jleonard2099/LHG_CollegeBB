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

    Shared oppScore()

    Shared BRC!(), compS!(), leaderVals!()
    Shared defLeaders!(), defTotals!()
    Shared offLeaders!(), offTotals!()
    Shared O1!(), O2!(), O3!(), O4!(), O5!(), O6!()
    Shared statsW0!(), statsW1!(), TRC!(), TRC1!()

    Shared BRC$(), DL$(), statPlyrNames$(), oppName$()
    Shared H1$(), OL$()
    Shared recconTRC1$(), recconTRC$()
    Shared Z0$(), Z1$(), Z3$()

    Shared teamScore()

    Shared fullWins, fullLosses

    'Read in any existing records first
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

    Else

        Erase BRC!, TRC!, TRC1!
        Erase BRC$, recconTRC$, recconTRC1$

    End If

    'Initialize values first
    'These are defensive record values, so they should be really high
    'To determine if a "record" was broken, the stat values will be
    'compared to see if they are lower than this.
    'If we initialized to 0, the "record" would always be 0
    For X = 1 To 40
        defLeaders!(X, 19) = 9999
    Next

    Erase leaderVals!
    Erase Z0$, Z1$, Z3$

    C$ = ","

    conferenceFile$ = ChooseFile$(diskPaths$(1) + "*.LIB", "CONFERENCE", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Do
            Color 15, 0
            Cls
            Print "WOULD YOU LIKE TO POST SEASON LEADERS? (Y/N)";
            I$ = GetKeyPress$
            finalUpdate$ = UCase$(I$)
        Loop Until finalUpdate$ = "Y" Or finalUpdate$ = "N"

        If UCase$(I$) = "Y" Then finalUpdate$ = "Y"

        Open diskPaths$(1) + conferenceFile$ + ".TMS" For Input As #2

        Input #2, LG$
        Input #2, D1$
        Input #2, D2$

        For XW = 1 To 30
            Input #2, H1$(XW)
            Z1$(XW) = H1$(XW)
        Next

        Close #2

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

                        Erase compS!
                        Erase defTotals!, offTotals!, statsW0!, statsW1!
                        Erase statPlyrNames$

                        Call ReadStatsCon(H1$(XW), TN$)
                        Call StatsByCategoryCon(numberGames%, TN$, finalUpdate$)
                        Call CheckStatsCon(TN$)

                    End If

                End If 'Done checking if we have stats

            End If 'Done checking valid conference

        Next XW 'Done "checking stats"

        Call SortLeadersConf
        Call PostLeadersConf(conferenceFile$)

    End If

End Sub


'----------------------------------------
'       ReadStatsCon Subroutine
'----------------------------------------
'This subroutine reads stats in for a 
'single team in the conference
Sub ReadStatsCon (statFile$, TN$)

    Shared compS!(), defTotals!(), offTotals!()
    Shared statsW0!(), statsW1!()

    Shared statPlyrNames$()

    Open diskPaths$(1) + statFile$ For Input As #1

    Input #1, TN$

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
'        CheckStatsCon Subroutine
'----------------------------------------
'This subroutine compares pre-loaded stats
'to existing leaders and updates as needed
Sub CheckStatsCon (TN$)

    Shared compS!(), defLeaders!(), defTotals!()
    Shared leaderVals!(), offLeaders!(), offTotals!()
    Shared statsW0!(), statsW1!()

    Shared statPlyrNames$(), DL$(), OL$()
    Shared Z0$(), Z3$()


    For X = 0 To 14:

        Color 14: Print "CHECKING STATS OF ";
        Color 15: Print statPlyrNames$(X)

        'Division 1, 2 Record
        If compS!(X, 22) > leaderVals!(20, 1) Then

            leaderVals!(20, 1) = compS!(X, 22)
            leaderVals!(20, 2) = 0
            Z0$(20) = statPlyrNames$(X)
            Z3$(20) = TN$

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

        'Assist Per Game
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

        '3 FG %
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

        'FG Made
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

        'FT Made
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

        For X = 1 To 39
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
            For X1 = 1 + X To 40
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
'     StatsByCategoryCon Subroutine
'----------------------------------------
'This subroutine compares all pre-loaded 
'stats to each of the record categories to 
'see if any should be updated
Sub StatsByCategoryCon (numberGames%, TN$, finalUpdate$)

    Shared BRC!(), compS!(), defTotals!(), offTotals!()
    Shared TRC!(), TRC1!()
    Shared statsW0!(), statsW1!()

    Shared BRC$(), statPlyrNames$()
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

            If compS!(I, 0) > BRC!(1) Then BRC!(1) = compS!(I, 0): BRC$(1, 0) = statPlyrNames$(I): BRC$(1, 1) = TN$
            If compS!(I, 1) > BRC!(2) Then BRC!(2) = compS!(I, 1): BRC$(2, 0) = statPlyrNames$(I): BRC$(2, 1) = TN$
            If compS!(I, 2) > BRC!(3) Then BRC!(3) = compS!(I, 2): BRC$(3, 0) = statPlyrNames$(I): BRC$(3, 1) = TN$
            If compS!(I, 19) > BRC!(4) And UCase$(finalUpdate$) = "Y" And compS!(I, 2) / numberGames% >= 5 Then BRC!(4) = compS!(I, 19): BRC$(4, 0) = statPlyrNames$(I): BRC$(4, 1) = TN$
            If statsW1!(I) > BRC!(5) Then BRC!(5) = statsW1!(I): BRC$(5, 0) = statPlyrNames$(I): BRC$(5, 1) = TN$
            If statsW0!(I) > BRC!(6) Then BRC!(6) = statsW0!(I): BRC$(6, 0) = statPlyrNames$(I): BRC$(6, 1) = TN$
            If compS!(I, 21) > BRC!(7) And UCase$(finalUpdate$) = "Y" And statsW0!(I) / numberGames% >= 1.5 Then BRC!(7) = compS!(I, 21): BRC$(7, 0) = statPlyrNames$(I): BRC$(7, 1) = TN$
            If compS!(I, 3) > BRC!(8) Then BRC!(8) = compS!(I, 3): BRC$(8, 0) = statPlyrNames$(I): BRC$(8, 1) = TN$
            If compS!(I, 4) > BRC!(9) Then BRC!(9) = compS!(I, 4): BRC$(9, 0) = statPlyrNames$(I): BRC$(9, 1) = TN$
            If compS!(I, 20) > BRC!(10) And UCase$(finalUpdate$) = "Y" And compS!(I, 4) / numberGames% >= 2.5 Then BRC!(10) = compS!(I, 20): BRC$(10, 0) = statPlyrNames$(I): BRC$(10, 1) = TN$
            If compS!(I, 10) / 60 > BRC!(11) Then BRC!(11) = compS!(I, 10) / 60: BRC$(11, 0) = statPlyrNames$(I): BRC$(11, 1) = TN$
            If compS!(I, 12) > BRC!(12) Then BRC!(12) = compS!(I, 12): BRC$(12, 0) = statPlyrNames$(I): BRC$(12, 1) = TN$
            If compS!(I, 22) > BRC!(13) And UCase$(finalUpdate$) = "Y" Then BRC!(13) = compS!(I, 22): BRC$(13, 0) = statPlyrNames$(I): BRC$(13, 1) = TN$
            If compS!(I, 25) > BRC!(14) Then BRC!(14) = compS!(I, 25): BRC$(14, 0) = statPlyrNames$(I): BRC$(14, 1) = TN$
            If compS!(I, 26) > BRC!(15) Then BRC!(15) = compS!(I, 26): BRC$(15, 0) = statPlyrNames$(I): BRC$(15, 1) = TN$
            If compS!(I, 5) - compS!(I, 26) > BRC!(16) Then BRC!(16) = compS!(I, 5) - compS!(I, 26): BRC$(16, 0) = statPlyrNames$(I): BRC$(16, 1) = TN$
            If compS!(I, 5) > BRC!(17) Then BRC!(17) = compS!(I, 5): BRC$(17, 0) = statPlyrNames$(I): BRC$(17, 1) = TN$
            If compS!(I, 13) > BRC!(18) And UCase$(finalUpdate$) = "Y" Then BRC!(18) = compS!(I, 13): BRC$(18, 0) = statPlyrNames$(I): BRC$(18, 1) = TN$
            If compS!(I, 6) > BRC!(19) Then BRC!(19) = compS!(I, 6): BRC$(19, 0) = statPlyrNames$(I): BRC$(19, 1) = TN$
            If compS!(I, 7) > BRC!(20) Then BRC!(20) = compS!(I, 7): BRC$(20, 0) = statPlyrNames$(I): BRC$(20, 1) = TN$
            If compS!(I, 8) > BRC!(21) Then BRC!(21) = compS!(I, 8): BRC$(21, 0) = statPlyrNames$(I): BRC$(21, 1) = TN$
            If compS!(I, 9) > BRC!(22) Then BRC!(22) = compS!(I, 9): BRC$(22, 0) = statPlyrNames$(I): BRC$(22, 1) = TN$
            If compS!(I, 11) > BRC!(23) Then BRC!(23) = compS!(I, 11): BRC$(23, 0) = statPlyrNames$(I): BRC$(23, 1) = TN$
            If compS!(I, 18) > BRC!(24) And UCase$(finalUpdate$) = "Y" Then BRC!(24) = compS!(I, 18): BRC$(24, 0) = statPlyrNames$(I): BRC$(24, 1) = TN$
            If compS!(I, 24) > BRC!(25) Then BRC!(25) = compS!(I, 24): BRC$(25, 0) = statPlyrNames$(I): BRC$(25, 1) = TN$

        End If

    Next

    offTotals!(16) = numberGames%
    defTotals!(16) = numberGames%
    offTotals!(17) = offTotals!(3) / offTotals!(2)
    defTotals!(17) = defTotals!(3) / defTotals!(2)
    offTotals!(18) = offTotals!(5) / offTotals!(4)
    defTotals!(18) = defTotals!(5) / defTotals!(4)
    offTotals!(20) = offTotals!(11)
    defTotals!(20) = defTotals!(11)

    If offTotals!(15) <> 0 Then offTotals!(21) = offTotals!(14) / offTotals!(15) Else offTotals!(21) = 0
    If defTotals!(15) <> 0 Then defTotals!(21) = defTotals!(14) / defTotals!(15) Else defTotals!(21) = 999

    offTotals!(19) = offTotals!(20) / numberGames%
    defTotals!(19) = defTotals!(20) / numberGames%

    If offTotals!(2) > TRC!(1) Then TRC!(1) = offTotals!(2): recconTRC$(1) = TN$
    If offTotals!(3) > TRC!(2) Then TRC!(2) = offTotals!(3): recconTRC$(2) = TN$
    If offTotals!(17) > TRC!(3) And UCase$(finalUpdate$) = "Y" Then TRC!(3) = offTotals!(17): recconTRC$(3) = TN$
    If offTotals!(15) > TRC!(4) Then TRC!(4) = offTotals!(15): recconTRC$(4) = TN$
    If offTotals!(14) > TRC!(5) Then TRC!(5) = offTotals!(14): recconTRC$(5) = TN$
    If offTotals!(21) > TRC!(6) And UCase$(finalUpdate$) = "Y" Then TRC!(6) = offTotals!(21): recconTRC$(6) = TN$
    If offTotals!(4) > TRC!(7) Then TRC!(7) = offTotals!(4): recconTRC$(7) = TN$
    If offTotals!(5) > TRC!(8) Then TRC!(8) = offTotals!(5): recconTRC$(8) = TN$
    If offTotals!(18) > TRC!(9) And UCase$(finalUpdate$) = "Y" Then TRC!(9) = offTotals!(18): recconTRC$(9) = TN$
    If offTotals!(13) > TRC!(10) Then TRC!(10) = offTotals!(13): recconTRC$(10) = TN$
    If offTotals!(6) - offTotals!(13) > TRC!(11) Then TRC!(11) = offTotals!(6) - offTotals!(13): recconTRC$(11) = TN$
    If offTotals!(6) > TRC!(12) Then TRC!(12) = offTotals!(6): recconTRC$(12) = TN$
    If offTotals!(6) / numberGames% > TRC!(13) And UCase$(finalUpdate$) = "Y" Then TRC!(13) = offTotals!(6) / numberGames%: recconTRC$(13) = TN$
    If offTotals!(7) < TRC!(14) And UCase$(finalUpdate$) = "Y" Then TRC!(14) = offTotals!(7): recconTRC$(14) = TN$
    If offTotals!(8) > TRC!(15) Then TRC!(15) = offTotals!(8): recconTRC$(15) = TN$
    If offTotals!(9) > TRC!(16) Then TRC!(16) = offTotals!(9): recconTRC$(16) = TN$
    If offTotals!(10) > TRC!(17) Then TRC!(17) = offTotals!(10): recconTRC$(17) = TN$
    If offTotals!(12) < TRC!(18) And UCase$(finalUpdate$) = "Y" Then TRC!(18) = offTotals!(12): recconTRC$(18) = TN$
    If offTotals!(11) > TRC!(19) Then TRC!(19) = offTotals!(11): recconTRC$(19) = TN$
    If offTotals!(11) / numberGames% > TRC!(20) And UCase$(finalUpdate$) = "Y" Then TRC!(20) = offTotals!(11) / numberGames%: recconTRC$(20) = TN$
    If fullWins > TRC!(21) Then TRC!(21) = fullWins: recconTRC$(21) = TN$
    If defTotals!(2) < TRC1!(1) Then TRC1!(1) = defTotals!(2): recconTRC1$(1) = TN$
    If defTotals!(3) < TRC1!(2) Then TRC1!(2) = defTotals!(3): recconTRC1$(2) = TN$
    If defTotals!(17) < TRC1!(3) And UCase$(finalUpdate$) = "Y" Then TRC1!(3) = defTotals!(17): recconTRC1$(3) = TN$
    If defTotals!(15) < TRC1!(4) Then TRC1!(4) = defTotals!(15): recconTRC1$(4) = TN$
    If defTotals!(14) < TRC1!(5) Then TRC1!(5) = defTotals!(14): recconTRC1$(5) = TN$
    If defTotals!(21) < TRC1!(6) And defTotals!(15) <> 0 And UCase$(finalUpdate$) = "Y" Then TRC1!(6) = defTotals!(21): recconTRC1$(6) = TN$
    If defTotals!(4) < TRC1!(7) Then TRC1!(7) = defTotals!(4): recconTRC1$(7) = TN$
    If defTotals!(5) < TRC1!(8) Then TRC1!(8) = defTotals!(5): recconTRC1$(8) = TN$
    If defTotals!(18) < TRC1!(9) And UCase$(finalUpdate$) = "Y" Then TRC1!(9) = defTotals!(18): recconTRC1$(9) = TN$
    If defTotals!(13) < TRC1!(10) Then TRC1!(10) = defTotals!(13): recconTRC1$(10) = TN$
    If defTotals!(6) - defTotals!(13) < TRC1!(11) Then TRC1!(11) = defTotals!(6) - defTotals!(13): recconTRC1$(11) = TN$
    If defTotals!(6) < TRC1!(12) Then TRC1!(12) = defTotals!(6): recconTRC1$(12) = TN$
    If defTotals!(6) / numberGames% < TRC1!(13) And UCase$(finalUpdate$) = "Y" Then TRC1!(13) = defTotals!(6) / numberGames%: recconTRC1$(13) = TN$
    If defTotals!(7) > TRC1!(14) And UCase$(finalUpdate$) = "Y" Then TRC1!(14) = defTotals!(7): recconTRC1$(14) = TN$
    If defTotals!(8) < TRC1!(15) Then TRC1!(15) = defTotals!(8): recconTRC1$(15) = TN$
    If defTotals!(9) < TRC1!(16) Then TRC1!(16) = defTotals!(9): recconTRC1$(16) = TN$
    If defTotals!(10) < TRC1!(17) Then TRC1!(17) = defTotals!(10): recconTRC1$(17) = TN$
    If defTotals!(12) > TRC1!(18) And UCase$(finalUpdate$) = "Y" Then TRC1!(18) = defTotals!(12): recconTRC1$(18) = TN$
    If defTotals!(11) < TRC1!(19) Then TRC1!(19) = defTotals!(11): recconTRC1$(19) = TN$
    If defTotals!(11) / numberGames% < TRC1!(20) And UCase$(finalUpdate$) = "Y" Then TRC1!(20) = defTotals!(11) / numberGames%: recconTRC1$(20) = TN$
    If fullLosses > TRC1!(21) Then TRC1!(21) = fullLosses: recconTRC1$(21) = TN$

End Sub


'----------------------------------------
'       SortLeadersConf Subroutine
'----------------------------------------
'This subroutine sorts the leaders in the
'target conference
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
'        PostLeadersNat Subroutine
'----------------------------------------
'This subroutine opens the existing leader
'and record files and writes the updated 
'stats to the file
Sub PostLeadersConf (leaderFile$)

    Shared DL$(), OL$()

    Shared BRC!(), defLeaders!(), leaderVals!()
    Shared O1!(), O2!(), O3!(), O4!(), O5!(), O6!()
    Shared BRC!(), offLeaders!(), TRC!(), TRC1!()

    Shared BRC$(), recconTRC$(), recconTRC1$()
    Shared Z0$(), Z1$(), Z3$()

    Cls

    Print

    Open diskPaths$(1) + leaderFile$ + ".LDR" For Output As #1

    Print "    POSTING NEW "; leaderFile$; " CONFERENCE LEADERS"

    For X = 1 To 30
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
        Write #1, DL$(X):
        For Y = 1 To 20:
            Write #1, defLeaders!(X, Y)
        Next
    Next

    Close #1

    Open diskPaths$(1) + "RECORDS.CBB" For Output As #2

    For I = 1 To 25
        Print #2, BRC!(I)
        Print #2, BRC$(I, 0)
        Print #2, BRC$(I, 1)
    Next

    For I = 1 To 21
        Print #2, TRC!(I)
        Print #2, recconTRC$(I)
    Next

    For I = 1 To 21
        Print #2, TRC1!(I)
        Print #2, recconTRC1$(I)
    Next

    Close #2

End Sub


'----------------------------------------
'         FindStatsCon Function
'----------------------------------------
'Looks for schedule stats for a specified
'team in the conference and loads them if
'found, notifying the user
Function FindStatsCon% (teamName$, numberGames%)

    Shared teamScore(), oppScore()

    Shared H1$(), locIndicator$(), oppName$()

    Shared fullWins, fullLosses

    If Not _FileExists(diskPaths$(1) + teamName$ + "S") Then

        Print "NO STATS FOUND FOR "; statFile$
        FindStatsCon% = 0

    Else

        Open diskPaths$(1) + teamName$ + "S" For Input As #1

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

        Close #1

        fullWins = 0
        fullLosses = 0
        confWins = 0
        confLosses = 0

        For I = 1 To numberGames%

            For P = 4 To 11
                If Mid$(oppName$(I), P, 1) = " " Or Mid$(oppName$(I), P, 1) = "." Then Mid$(oppName$(I), P, 1) = "_"
            Next P

            For P = 12 To 14
                If Mid$(oppName$(I), P, 2) = "ST" Then Mid$(oppName$(I), 9, 1) = "_": Mid$(oppName$(I), 10, 2) = "ST"
            Next P

            QQ = 8
            For P = 11 To 4 Step -1
                If Mid$(oppName$(I), P, 1) = "_" Then
                    QQ = P - 4
                Else
                    Exit For '5605 GoTo 5608
                End If
            Next P

            targetFile$ = Mid$(oppName$(I), 4, QQ)
            R$ = targetFile$ + "." + Left$(oppName$(I), 2)
            oppName$(I) = R$

            confGame = 0

            For EC = 1 To 30
                'If team stat file = opponent of a conference game?
                If oppName$(I) = H1$(EC) Then
                    confGame = 1
                    Exit For
                End If

            Next EC

            If confGame <> 1 Then

                If teamScore(I) > oppScore(I) Then fullWins = fullWins + 1
                If teamScore(I) < oppScore(I) Then fullLosses = fullLosses + 1

            Else

                If teamScore(I) > oppScore(I) Then 
                    confWins = confWins + 1: fullWins = fullWins + 1
                End If

                If teamScore(I) < oppScore(I) Then 
                    confLosses = confLosses + 1: fullLosses = fullLosses + 1
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
