Sub COMPNAT ()

    _Title "Courtside College Basketball - Update Stats"

    Dim AP%(100, 1), compnatT%(1 To 100)

    Dim CRD(100)
    Dim NO1(30), NO2(30), NO3(30), NO4(30), NO5(30), NO6(30)

    Dim CN$(40), H1$(40)
    Dim NOL$(40)
    Dim SITE$(100)

    ReDim PT(1200, 4)
    ReDim PT$(1200)

    Shared BRC!(), DL!()
    Shared LD!(), NDL!(), NLD!(), NOL!(), OL!()
    Shared O1!(), O2!(), O3!(), O4!(), O5!(), O6!()
    Shared TRC!(), TRC1!()

    Shared DT!(), OT!()

    Shared BRC$(), compnatH$(), DL$(), O$(), OL$()
    Shared NDL$(), NZ0$(), NZ1$(), NZ3$()
    Shared recconTRC$(), recconTRC1$()
    Shared Z0$(), Z1$(), Z3$()

    Shared O%()

    Cls

    For X = 1 To 250
        For Y = 1 To 2
            NLD!(X, Y) = 0
        Next Y
    Next X

    TRC!(14) = 19999: TRC!(18) = 19999
    For I = 1 To 13: TRC1!(I) = 19999: Next
    For I = 15 To 17: TRC1!(I) = 19999: Next
    For I = 19 To 20: TRC1!(I) = 19999: Next

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

    Do
        'Display list of files...

        Spec$ = diskPaths$(1) + "*.CON"
        Count% = FCount%(Spec$)
        If Count% = 0 Then
            Call CSMsgBox("NO ORGANIZATION FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)
        Else

            ReDim organizationFiles$(0 To Count%)
            For X = 1 To Count%
                organizationFiles$(X) = Space$(12)
            Next
            organizationFiles$(0) = Spec$
            Call ReadFile(organizationFiles$())

            Do

                Cls
                Color , 1
                Cls
                Locate , 28
                Color 14
                Print "SELECT ORGANIZATION TO UPDATE": Color 15
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
                Print "-SELECTS ORGANIZATION";

                _MouseShow "DEFAULT"
                MAMenu organizationFiles$(), Selection%, START%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
                _MouseHide

                If ExitCode% <> 27 Then
                    targetFile$ = organizationFiles$(Selection%)
                    Locate 2, 33: Print targetFile$
                    Locate , 33: Print "ANY CHANGE (YN)";
                    Color 15, 0
                    3910 I$ = GetKeyPress$
                End If
            Loop Until UCase$(I$) = "N"

            If ExitCode% <> 27 Then

                3911 targetFile$ = RTrim$(targetFile$)
                WE$ = Left$(targetFile$, Len(targetFile$) - 4)
                LH$ = WE$
                Cls: Print "IS THIS THE FINAL UPDATE FOR THE SEASON (YN)";
                Color 15, 0
                3912 I$ = GetKeyPress$
                If UCase$(I$) = "Y" Then UP$ = "Y"

                Open diskPaths$(1) + WE$ + ".CON" For Input As #2
                Input #2, X$
                For I = 1 To 40
                    Input #2, CN$(I)
                Next
                Close 2

                For X = 1 To 40
                    NDL!(X, 19) = 9999
                Next

                For NC = 1 To 40
                    If CN$(NC) <> "XXX" Then
                        Cls

                        For X = 0 To 40
                            OL$(X) = ""
                            DL$(X) = ""
                            For X1 = 0 To 20
                                DL!(X, X1) = 0
                                OL!(X, X1) = 0
                            Next
                        Next

                        For X = 1 To 40
                            DL!(X, 19) = 9999
                        Next

                        For X = 1 To 250
                            Z0$(X) = "": Z3$(X) = ""
                            For X1 = 0 To 2
                                LD!(X, X1) = 0
                            Next
                        Next

                        For X = 1 To 30
                            Z1$(X) = "": O1!(X) = 0: O2!(X) = 0: O3!(X) = 0: O4!(X) = 0: O5!(X) = 0: O6!(X) = 0
                        Next

                        Open diskPaths$(1) + CN$(NC) + ".TMS" For Input As #2
                        Input #2, LG$
                        Input #2, D1$: Input #2, D2$
                        For XW = 1 To 30
                            Input #2, H1$(XW)
                            Z1$(XW) = H1$(XW)
                        Next
                        Close 2

                        For XW = 1 To 30
                            replayWins% = 0
                            replayLosses% = 0
                            CW = 0
                            CL = 0

                            If H1$(XW) = Chr$(42) Then Exit For 'was Return; but where from?

                            H1$(XW) = Right$(H1$(XW), 2) + " " + Left$(H1$(XW), Len(H1$(XW)) - 3)

                            For XX = 4 To 11: If Mid$(H1$(XW), XX, 1) = " " Or Mid$(H1$(XW), XX, 1) = "." Then Mid$(H1$(XW), XX, 1) = "_"
                            Next XX

                            For XX = 12 To 14: If Mid$(H1$(XW), XX, 2) = "ST" Then Mid$(H1$(XW), 9, 1) = "_": Mid$(H1$(XW), 10, 2) = "ST"
                            Next XX

                            QQ = 8

                            For XX = 11 To 4 Step -1
                                If Mid$(H1$(XW), XX, 1) = "_" Then
                                    QQ = XX - 4
                                Else
                                    Exit For
                                End If
                            Next XX

                            G$ = H1$(XW): H1$(XW) = Right$(H1$(XW), Len(H1$(XW)) - 3): YR$ = Left$(G$, 2)
                            If Len(H1$(XW)) > 8 Then H1$(XW) = Left$(H1$(XW), 8)
                            H1$(XW) = H1$(XW) + "." + YR$

                            If _FileExists(diskPaths$(1) + H1$(XW) + "S") Then

                                Open diskPaths$(1) + H1$(XW) + "S" For Input As #2

                                Input #2, numGames%
                                For I = 1 To 100: Input #2, O%(I): Next I
                                For I = 1 To 100: Input #2, O$(I): Next I

                                For I = 1 To 100: Input #2, compnatT%(I): Next I
                                For I = 1 To 100: Input #2, compnatH$(I): Next I
                                For I = 1 To 100
                                    For I1 = 0 To 1
                                        Input #2, AP%(I, I1)
                                Next: Next
                                For I = 1 To 100: Input #2, CRD(I): Next I
                                For I = 1 To 100: Input #2, SITE$(I): Next I
                                Close 2

                                replayWins% = 0: replayLosses% = 0: CW = 0: CL = 0

                                '804 GoSub 5500
                                For I = 1 To numGames%

                                    'GoSub 5600
                                    For P = 4 To 11
                                        If Mid$(O$(I), P, 1) = " " Or Mid$(O$(I), P, 1) = "." Then Mid$(O$(I), P, 1) = "_"
                                    Next P

                                    For P = 12 To 14
                                        If Mid$(O$(I), P, 2) = "ST" Then Mid$(O$(I), 9, 1) = "_": Mid$(O$(I), 10, 2) = "ST"
                                    Next P

                                    QQ = 8
                                    For P = 11 To 4 Step -1:
                                        If Mid$(O$(I), P, 1) = "_" Then
                                            QQ = P - 4
                                        Else
                                            Exit For
                                        End If
                                    Next P

                                    f$ = Mid$(O$(I), 4, QQ): R$ = f$ + "." + Left$(O$(I), 2)
                                    O$(I) = R$

                                    For EC = 1 To 30
                                        If O$(I) = H1$(EC) Then
                                            skipper=1: Exit For
                                        End If
                                    Next EC

                                    If skipper <> 1 Then
                                        If O%(I) > compnatT%(I) Then replayWins% = replayWins% + 1
                                        If O%(I) < compnatT%(I) Then replayLosses% = replayLosses% + 1
                                    Else
                                        If O%(I) > compnatT%(I) Then CW = CW + 1: replayWins% = replayWins% + 1

                                        If O%(I) < compnatT%(I) Then CL = CL + 1: replayLosses% = replayLosses% + 1
                                    End If

                                    EC = 0
                                Next I

                                'STRENGTH OF SCHEDULE
                                PW = 0: SS = 0

                                For I = 1 To numGames%
                                    SS = AP%(I, 1) + SS
                                Next

                                SS = SS / numGames%
                                'INDIVIDUAL GAME POWER RATINGS
                                GL = 0: PW = 0

                                For I = 1 To numGames%
                                    RAT = O%(I) - compnatT%(I) + AP%(I, 1)
                                    If Left$(compnatH$(I), 1) = "H" Then RAT = RAT - 4.5
                                    If Left$(compnatH$(I), 1) = "A" Then RAT = RAT + 4.5
                                    PW = PW + RAT
                                Next

                                If numGames% <= 4 Then
                                    GL = 5 - numGames%
                                    PW = PW + GL * AP%(1, 0)
                                End If

                                PW = PW / (numGames% + GL)
                                NRT = NRT + 1
                                PT$(NRT) = H1$(XW)
                                PT(NRT, 1) = PW
                                PT(NRT, 2) = replayWins%
                                PT(NRT, 3) = replayLosses%
                                PT(NRT, 4) = SS

                                Cls
                                Locate 4, 5
                                Color 14: Print "FINDING ";
                                Color 15: Print H1$(XW);: Color 14
                                Print " STATS            "

                                L = Len(H1$(XW)): WT$ = ""
                                For X = 1 To L
                                    A = Asc(Mid$(H1$(XW), X, 1))
                                WT$ = WT$ + Chr$(A): Next X
                                For X = 1 To 30
                                    If Z1$(X) = WT$ Then
                                        If O1!(X) = replayWins% And O2!(X) = replayLosses% Then GoTo 948
                                        O1!(X) = replayWins%
                                        O2!(X) = replayLosses%
                                        O4!(X) = CW
                                        O5!(X) = CL
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
                                Next X

                                Call LoadStats(H1$(XW))

                            End If 'Done checkin for stat file

                            Call CheckTeam

                            948 '
                            If H1$(XW) <> "*" And _FileExists(diskPaths$(1) + H1$(XW) + "S") Then
                                Call CheckStatsLeaders(H1$(XW))
                            End If

                            Close 3
                        Next

                        AA = 1: ZZ = 15
                        For X = AA To ZZ - 1
                            For Y = X + 1 To ZZ
                                DiffX = O1!(X) - O2!(X)
                                DiffY = O1!(Y) - O2!(Y)
                                If DiffX <= DiffY Then
                                    If DiffX = DiffY Then
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

                        AA = 16: ZZ = 30

                        For X = AA To ZZ - 1
                            For Y = X + 1 To ZZ
                                DiffX = O1!(X) - O2!(X)
                                DiffY = O1!(Y) - O2!(Y)
                                If DiffX <= DiffY Then
                                    If DiffX = DiffY Then
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

                        Cls
                        Print

                        Open diskPaths$(1) + CN$(NC) + ".LDR" For Output As #2
                        Print "    POSTING NEW "; CN$(NC); " CONFERENCE LEADERS"
                        For X = 1 To 30:
                            Write #2, Z1$(X), O1!(X), O2!(X), O3!(X), O4!(X), O5!(X), O6!(X)
                        Next

                        For X = 1 To 250
                            Write #2, Z0$(X), Z3$(X), LD!(X, 1), LD!(X, 2)
                        Next X

                        For X = 1 To 40: Write #2, OL$(X): For Y = 1 To 20: Write #2, OL!(X, Y): Next: Next
                        For X = 1 To 40: Write #2, DL$(X): For Y = 1 To 20: Write #2, DL!(X, Y): Next: Next
                        Close 2

                    End If

                Next NC

                Cls
                Print

                Open diskPaths$(1) + WE$ + ".NLD" For Output As #2
                Print "    POSTING NEW "; WE$; " LEADERS"

                For X = 1 To 30
                    Write #2, NZ1$(X), NO1(X), NO2(X), NO3(X), NO4(X), NO5(X), NO6(X)
                Next

                For X = 1 To 250
                    Write #2, NZ0$(X), NZ3$(X), NLD!(X, 1), NLD!(X, 2)
                Next X

                For X = 1 To 40
                    Write #2, NOL$(X)
                    For Y = 1 To 20
                        Write #2, NOL!(X, Y)
                    Next
                Next

                For X = 1 To 40
                    Write #2, NDL$(X)
                    For Y = 1 To 20
                        Write #2, NDL!(X, Y)
                    Next
                Next

                Close 2

                For Z = 1 To NRT - 1
                    For Z1 = Z + 1 To NRT
                        If PT(Z, 1) < PT(Z1, 1) Then
                            Swap PT$(Z1), PT$(Z)
                            For MN = 1 To 4
                                Swap PT(Z1, MN), PT(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z

                Open diskPaths$(1) + WE$ + ".POL" For Output As #2
                Print "POSTING "; WE$; " POLL"
                Write #2, NRT
                For X = 1 To 1200
                    Write #2, PT$(X), PT(X, 1), PT(X, 2), PT(X, 3), PT(X, 4)
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

            End If 'Done checking for ESC

        End If 'Done checking for file count

    '...Until we press ESC
    Loop Until ExitCode% = 27 Or Count% = 0 Or I$ = "N"

    99 '

    '2905 Exit Sub
End Sub

'----------------------------------------
'       LoadStats Subroutine
'----------------------------------------
'...explanation...
Sub LoadStats (statFile$)

    Dim BRC$(32, 1)
    Dim TRC!(21), TRC1!(21)

    Shared BRC!()
    Shared S!(), W0!(), W1!()
    Shared DT!(), OT!()
    Shared compnatN$(), recconTRC$(), recconTRC1$()

    811 For I = 0 To 13
        For J = 0 To 26
            S!(I, J) = 0
        Next
    Next

    If _FileExists(diskPaths$(1) + statFile$) Then

        Open diskPaths$(1) + statFile$ For Input As #2
        Input #2, TN$

        812 For I = 0 To 13
            Input #2, compnatN$(I)
            For J = 0 To 13
                Input #2, S!(I, J)
            Next
            Input #2, W0!(I)
            Input #2, W1!(I)
        Next
        For L = 1 To 16: Input #2, OT!(L): Next: For L = 1 To 16: Input #2, DT!(L): Next
        Close 2

        For I = 0 To 13
            S!(I, 25) = S!(I, 13)
            S!(I, 26) = S!(I, 12)
            S!(I, 12) = S!(I, 2) * 2 + W0!(I) + S!(I, 4): ' *** PTS ***
            If S!(I, 0) <> 0 Then
                S!(I, 13) = S!(I, 5) / S!(I, 0): ' *** RB/G ***
                S!(I, 14) = S!(I, 6) / S!(I, 0): ' *** FOULS/G ***
                S!(I, 15) = S!(I, 7) / S!(I, 0): ' *** STEALS/G ***
                S!(I, 16) = S!(I, 8) / S!(I, 0): ' *** ASSISTS/G ***
                S!(I, 17) = S!(I, 9) / S!(I, 0): ' *** BLOCKS/G ***
                S!(I, 24) = S!(I, 5) + S!(I, 12) - S!(I, 6) + S!(I, 8) + S!(I, 9) + S!(I, 7) - 2 * S!(I, 11)
                S!(I, 18) = S!(I, 10) / 60 / S!(I, 0): ' *** MIN/G ***

                If S!(I, 1) <> 0 And (S!(I, 2) / numGames% >= 5) Then
                    S!(I, 19) = S!(I, 2) / S!(I, 1): ' *** FG% ***
                End If

                If S!(I, 3) <> 0 And (S!(I, 4) / numGames%) >= 2.5 Then
                    S!(I, 20) = S!(I, 4) / S!(I, 3): ' *** FT% ***
                End If

                If W1!(I) <> 0 And W0!(I) / numGames% >= 1.5 Then
                    S!(I, 21) = W0!(I) / W1!(I): ' *** 3FG% ***
                End If

                S!(I, 22) = S!(I, 12) / S!(I, 0): ' *** PTS/G ***
                If S!(I, 0) > BRC!(1) Then BRC!(1) = S!(I, 0): BRC$(1, 0) = compnatN$(I): BRC$(1, 1) = TN$
                If S!(I, 1) > BRC!(2) Then BRC!(2) = S!(I, 1): BRC$(2, 0) = compnatN$(I): BRC$(2, 1) = TN$
                If S!(I, 2) > BRC!(3) Then BRC!(3) = S!(I, 2): BRC$(3, 0) = compnatN$(I): BRC$(3, 1) = TN$
                If S!(I, 19) > BRC!(4) And UCase$(UP$) = "Y" And S!(I, 2) / numGames% >= 5 Then BRC!(4) = S!(I, 19): BRC$(4, 0) = compnatN$(I): BRC$(4, 1) = TN$
                If W1!(I) > BRC!(5) Then BRC!(5) = W1!(I): BRC$(5, 0) = compnatN$(I): BRC$(5, 1) = TN$
                If W0!(I) > BRC!(6) Then BRC!(6) = W0!(I): BRC$(6, 0) = compnatN$(I): BRC$(6, 1) = TN$
                If S!(I, 21) > BRC!(7) And UCase$(UP$) = "Y" And W0!(I) / numGames% >= 1.5 Then BRC!(7) = S!(I, 21): BRC$(7, 0) = compnatN$(I): BRC$(7, 1) = TN$
                If S!(I, 3) > BRC!(8) Then BRC!(8) = S!(I, 3): BRC$(8, 0) = compnatN$(I): BRC$(8, 1) = TN$
                If S!(I, 4) > BRC!(9) Then BRC!(9) = S!(I, 4): BRC$(9, 0) = compnatN$(I): BRC$(9, 1) = TN$
                If S!(I, 20) > BRC!(10) And UCase$(UP$) = "Y" And S!(I, 4) / numGames% >= 2.5 Then BRC!(10) = S!(I, 20): BRC$(10, 0) = compnatN$(I): BRC$(10, 1) = TN$
                If S!(I, 10) / 60 > BRC!(11) Then BRC!(11) = S!(I, 10) / 60: BRC$(11, 0) = compnatN$(I): BRC$(11, 1) = TN$
                If S!(I, 12) > BRC!(12) Then BRC!(12) = S!(I, 12): BRC$(12, 0) = compnatN$(I): BRC$(12, 1) = TN$
                If S!(I, 22) > BRC!(13) And UCase$(UP$) = "Y" Then BRC!(13) = S!(I, 22): BRC$(13, 0) = compnatN$(I): BRC$(13, 1) = TN$
                If S!(I, 25) > BRC!(14) Then BRC!(14) = S!(I, 25): BRC$(14, 0) = compnatN$(I): BRC$(14, 1) = TN$
                If S!(I, 26) > BRC!(15) Then BRC!(15) = S!(I, 26): BRC$(15, 0) = compnatN$(I): BRC$(15, 1) = TN$
                If S!(I, 5) - S!(I, 26) > BRC!(16) Then BRC!(16) = S!(I, 5) - S!(I, 26): BRC$(16, 0) = compnatN$(I): BRC$(16, 1) = TN$
                If S!(I, 5) > BRC!(17) Then BRC!(17) = S!(I, 5): BRC$(17, 0) = compnatN$(I): BRC$(17, 1) = TN$
                If S!(I, 13) > BRC!(18) And UCase$(UP$) = "Y" Then BRC!(18) = S!(I, 13): BRC$(18, 0) = compnatN$(I): BRC$(18, 1) = TN$
                If S!(I, 6) > BRC!(19) Then BRC!(19) = S!(I, 6): BRC$(19, 0) = compnatN$(I): BRC$(19, 1) = TN$
                If S!(I, 7) > BRC!(20) Then BRC!(20) = S!(I, 7): BRC$(20, 0) = compnatN$(I): BRC$(20, 1) = TN$
                If S!(I, 8) > BRC!(21) Then BRC!(21) = S!(I, 8): BRC$(21, 0) = compnatN$(I): BRC$(21, 1) = TN$
                If S!(I, 9) > BRC!(22) Then BRC!(22) = S!(I, 9): BRC$(22, 0) = compnatN$(I): BRC$(22, 1) = TN$
                If S!(I, 11) > BRC!(23) Then BRC!(23) = S!(I, 11): BRC$(23, 0) = compnatN$(I): BRC$(23, 1) = TN$
                If S!(I, 18) > BRC!(24) And UCase$(UP$) = "Y" Then BRC!(24) = S!(I, 18): BRC$(24, 0) = compnatN$(I): BRC$(24, 1) = TN$
                If S!(I, 24) > BRC!(25) Then BRC!(25) = S!(I, 24): BRC$(25, 0) = compnatN$(I): BRC$(25, 1) = TN$

            End If

        Next

        OT!(16) = numGames%: DT!(16) = numGames%: OT!(17) = OT!(3) / OT!(2): DT!(17) = DT!(3) / DT!(2)
        OT!(18) = OT!(5) / OT!(4): DT!(18) = DT!(5) / DT!(4)
        OT!(20) = OT!(11): DT!(20) = DT!(11)
        If OT!(15) <> 0 Then OT!(21) = OT!(14) / OT!(15) Else OT!(21) = 0
        If DT!(15) <> 0 Then DT!(21) = DT!(14) / DT!(15) Else DT!(21) = 999
        OT!(19) = OT!(20) / numGames%: DT!(19) = DT!(20) / numGames%
        If OT!(2) > TRC!(1) Then TRC!(1) = OT!(2): recconTRC$(1) = TN$
        If OT!(3) > TRC!(2) Then TRC!(2) = OT!(3): recconTRC$(2) = TN$
        If OT!(17) > TRC!(3) And UCase$(UP$) = "Y" Then TRC!(3) = OT!(17): recconTRC$(3) = TN$
        If OT!(15) > TRC!(4) Then TRC!(4) = OT!(15): recconTRC$(4) = TN$
        If OT!(14) > TRC!(5) Then TRC!(5) = OT!(14): recconTRC$(5) = TN$
        If OT!(21) > TRC!(6) And UCase$(UP$) = "Y" Then TRC!(6) = OT!(21): recconTRC$(6) = TN$
        If OT!(4) > TRC!(7) Then TRC!(7) = OT!(4): recconTRC$(7) = TN$
        If OT!(5) > TRC!(8) Then TRC!(8) = OT!(5): recconTRC$(8) = TN$
        If OT!(18) > TRC!(9) And UCase$(UP$) = "Y" Then TRC!(9) = OT!(18): recconTRC$(9) = TN$
        If OT!(13) > TRC!(10) Then TRC!(10) = OT!(13): recconTRC$(10) = TN$
        If OT!(6) - OT!(13) > TRC!(11) Then TRC!(11) = OT!(6) - OT!(13): recconTRC$(11) = TN$
        If OT!(6) > TRC!(12) Then TRC!(12) = OT!(6): recconTRC$(12) = TN$
        If OT!(6) / numGames% > TRC!(13) And UCase$(UP$) = "Y" Then TRC!(13) = OT!(6) / numGames%: recconTRC$(13) = TN$
        If OT!(7) < TRC!(14) And UCase$(UP$) = "Y" Then TRC!(14) = OT!(7): recconTRC$(14) = TN$
        If OT!(8) > TRC!(15) Then TRC!(15) = OT!(8): recconTRC$(15) = TN$
        If OT!(9) > TRC!(16) Then TRC!(16) = OT!(9): recconTRC$(16) = TN$
        If OT!(10) > TRC!(17) Then TRC!(17) = OT!(10): recconTRC$(17) = TN$
        If OT!(12) < TRC!(18) And UCase$(UP$) = "Y" Then TRC!(18) = OT!(12): recconTRC$(18) = TN$
        If OT!(11) > TRC!(19) Then TRC!(19) = OT!(11): recconTRC$(19) = TN$
        If OT!(11) / numGames% > TRC!(20) And UCase$(UP$) = "Y" Then TRC!(20) = OT!(11) / numGames%: recconTRC$(20) = TN$
        If replayWins% > TRC!(21) Then TRC!(21) = replayWins%: recconTRC$(21) = TN$
        If DT!(2) < TRC1!(1) Then TRC1!(1) = DT!(2): recconTRC1$(1) = TN$
        If DT!(3) < TRC1!(2) Then TRC1!(2) = DT!(3): recconTRC1$(2) = TN$
        If DT!(17) < TRC1!(3) And UCase$(UP$) = "Y" Then TRC1!(3) = DT!(17): recconTRC1$(3) = TN$
        If DT!(15) < TRC1!(4) Then TRC1!(4) = DT!(15): recconTRC1$(4) = TN$
        If DT!(14) < TRC1!(5) Then TRC1!(5) = DT!(14): recconTRC1$(5) = TN$
        If DT!(21) < TRC1!(6) And DT!(15) <> 0 And UCase$(UP$) = "Y" Then TRC1!(6) = DT!(21): recconTRC1$(6) = TN$
        If DT!(4) < TRC1!(7) Then TRC1!(7) = DT!(4): recconTRC1$(7) = TN$
        If DT!(5) < TRC1!(8) Then TRC1!(8) = DT!(5): recconTRC1$(8) = TN$
        If DT!(18) < TRC1!(9) And UCase$(UP$) = "Y" Then TRC1!(9) = DT!(18): recconTRC1$(9) = TN$
        If DT!(13) < TRC1!(10) Then TRC1!(10) = DT!(13): recconTRC1$(10) = TN$
        If DT!(6) - DT!(13) < TRC1!(11) Then TRC1!(11) = DT!(6) - DT!(13): recconTRC1$(11) = TN$
        If DT!(6) < TRC1!(12) Then TRC1!(12) = DT!(6): recconTRC1$(12) = TN$
        If DT!(6) / numGames% < TRC1!(13) And UCase$(UP$) = "Y" Then TRC1!(13) = DT!(6) / numGames%: recconTRC1$(13) = TN$
        If DT!(7) > TRC1!(14) And UCase$(UP$) = "Y" Then TRC1!(14) = DT!(7): recconTRC1$(14) = TN$
        If DT!(8) < TRC1!(15) Then TRC1!(15) = DT!(8): recconTRC1$(15) = TN$
        If DT!(9) < TRC1!(16) Then TRC1!(16) = DT!(9): recconTRC1$(16) = TN$
        If DT!(10) < TRC1!(17) Then TRC1!(17) = DT!(10): recconTRC1$(17) = TN$
        If DT!(12) > TRC1!(18) And UCase$(UP$) = "Y" Then TRC1!(18) = DT!(12): recconTRC1$(18) = TN$
        If DT!(11) < TRC1!(19) Then TRC1!(19) = DT!(11): recconTRC1$(19) = TN$
        If DT!(11) / numGames% < TRC1!(20) And UCase$(UP$) = "Y" Then TRC1!(20) = DT!(11) / numGames%: recconTRC1$(20) = TN$
        If replayLosses% > TRC1!(21) Then TRC1!(21) = LS: recconTRC1$(21) = TN$

    End If
    '1030 Return

End Sub

'----------------------------------------
'       CheckStatsLeaders Subroutine
'----------------------------------------
Sub CheckTeam ()

    Shared DL!(), LD!()
    Shared NDL!(), NOL!(), NLD!()
    Shared OL!(), S!(), W0!(), W1!()

    Shared DT!(), OT!()

    Shared compnatN$(), DL$()
    Shared NDL$(), NZ0$(), NZ3$()
    Shared Z0$(), Z3$()

    For X = 0 To 14:
        Color 14: Print "CHECKING STATS OF ";: Color 15: Print compnatN$(X)

        If S!(X, 22) > LD!(20, 1) Then
            LD!(20, 1) = S!(X, 22)
            LD!(20, 2) = 0
            Z0$(20) = compnatN$(X)
            Z3$(20) = TN$
            For Z = 1 To 19:
                For Z1 = Z + 1 To 20
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2: Swap LD!(Z1, M), LD!(Z, M): Next
                    End If
                Next Z1
            Next Z
        End If

        If S!(X, 12) > LD!(40, 1) Then
            LD!(40, 1) = S!(X, 12)
            LD!(40, 2) = 0
            Z0$(40) = compnatN$(X)
            Z3$(40) = TN$
            For Z = 21 To 39
                For Z1 = Z + 1 To 40
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2: Swap LD!(Z1, M), LD!(Z, M): Next
                    End If
                Next Z1
            Next Z
        End If

        If S!(X, 19) > LD!(60, 1) Then
            LD!(60, 1) = S!(X, 19)
            LD!(60, 2) = S!(X, 1)
            Z0$(60) = compnatN$(X)
            Z3$(60) = TN$
            For Z = 41 To 59
                For Z1 = Z + 1 To 60
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2: Swap LD!(Z1, M), LD!(Z, M): Next
                    End If
                Next Z1
            Next Z
        End If

        If S!(X, 20) > LD!(80, 1) Then
            LD!(80, 1) = S!(X, 20)
            LD!(80, 2) = S!(X, 3)
            Z0$(80) = compnatN$(X)
            Z3$(80) = TN$
            For Z = 61 To 79
                For Z1 = Z + 1 To 80
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2: Swap LD!(Z1, M), LD!(Z, M): Next
                    End If
                Next Z1
            Next Z
        End If

        If S!(X, 15) > LD!(90, 1) Then
            LD!(90, 1) = S!(X, 15)
            LD!(90, 2) = S!(X, 7)
            Z0$(90) = compnatN$(X)
            Z3$(90) = TN$
            For Z = 81 To 89
                For Z1 = Z + 1 To 90
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2: Swap LD!(Z1, M), LD!(Z, M): Next
                    End If
                Next Z1
            Next Z
        End If

        If S!(X, 7) > LD!(100, 1) Then
            LD!(100, 1) = S!(X, 7)
            LD!(100, 2) = 0
            Z0$(100) = compnatN$(X)
            Z3$(100) = TN$
            For Z = 91 To 99
                For Z1 = Z + 1 To 100
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2: Swap LD!(Z1, M), LD!(Z, M): Next
                    End If
                Next Z1
            Next Z
        End If

        If S!(X, 16) > LD!(110, 1) Then
            LD!(110, 1) = S!(X, 16)
            LD!(110, 2) = S!(X, 8)
            Z0$(110) = compnatN$(X)
            Z3$(110) = TN$
            For Z = 101 To 109
                For Z1 = Z + 1 To 110
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2: Swap LD!(Z1, M), LD!(Z, M): Next
                    End If
                Next Z1
            Next Z
        End If

        'FOR Z=111 TO 120
        If S!(X, 8) > LD!(120, 1) Then
            LD!(120, 1) = S!(X, 8)
            LD!(120, 2) = S!(X, 8)
            Z0$(120) = compnatN$(X)
            Z3$(120) = TN$
            For Z = 111 To 119
                For Z1 = Z + 1 To 120
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2: Swap LD!(Z1, M), LD!(Z, M): Next
                    End If
                Next Z1
            Next Z
        End If

        'FOR Z=121 TO 130
        If S!(X, 21) > LD!(130, 1) Then
            LD!(130, 1) = S!(X, 21)
            LD!(130, 2) = W1!(X)
            Z0$(130) = compnatN$(X)
            Z3$(130) = TN$
            For Z = 121 To 129
                For Z1 = Z + 1 To 130
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2: Swap LD!(Z1, M), LD!(Z, M): Next
                    End If
                Next Z1
            Next Z
        End If

        'FOR Z=131 TO 140
        If W0!(X) > LD!(140, 1) Then
            LD!(140, 1) = W0!(X)
            LD!(140, 2) = 0
            Z0$(140) = compnatN$(X)
            Z3$(140) = TN$
            For Z = 131 To 139
                For Z1 = Z + 1 To 140
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2: Swap LD!(Z1, M), LD!(Z, M): Next
                    End If
                Next Z1
            Next Z
        End If

        'FOR Z=141 TO 150
        If S!(X, 18) > LD!(150, 1) Then
            LD!(150, 1) = S!(X, 18)
            LD!(150, 2) = 0
            Z0$(150) = compnatN$(X)
            Z3$(150) = TN$
            For Z = 141 To 149
                For Z1 = Z + 1 To 150
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2: Swap LD!(Z1, M), LD!(Z, M): Next
                    End If
                Next Z1
            Next Z
        End If

        'FOR Z=151 TO 160
        If S!(X, 10) / 60 > LD!(160, 1) Then
            LD!(160, 1) = S!(X, 10) / 60
            LD!(160, 2) = 0
            Z0$(160) = compnatN$(X)
            Z3$(160) = TN$
            For Z = 151 To 159
            For Z1 = Z + 1 To 160
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2: Swap LD!(Z1, M), LD!(Z, M): Next
                    End If
            3537 Next Z1: Next Z
        End If

        'FOR Z=161 TO 170
        If S!(X, 13) > LD!(170, 1) Then
            LD!(170, 1) = S!(X, 13)
            LD!(170, 2) = S!(X, 5)
            Z0$(170) = compnatN$(X)
            Z3$(170) = TN$
            For Z = 161 To 169
            For Z1 = Z + 1 To 170
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2: Swap LD!(Z1, M), LD!(Z, M): Next
                    End If
                Next Z1
            Next Z
        End If

        'FOR Z=171 TO 180
        If S!(X, 5) > LD!(180, 1) Then
            LD!(180, 1) = S!(X, 5)
            LD!(18, 2) = 0
            Z0$(180) = compnatN$(X)
            Z3$(180) = TN$
            For Z = 171 To 179
                For Z1 = Z + 1 To 180
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2: Swap LD!(Z1, M), LD!(Z, M): Next
                    End If
                Next Z1
            Next Z
        End If

        'FOR Z=181 TO 190
        If S!(X, 17) > LD!(190, 1) Then
            LD!(190, 1) = S!(X, 17)
            LD!(190, 2) = 0
            Z0$(190) = compnatN$(X)
            Z3$(190) = TN$
            For Z = 181 To 189
                For Z1 = Z + 1 To 190
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2: Swap LD!(Z1, M), LD!(Z, M): Next
                    End If
                Next Z1
            Next Z
        End If

        'FOR Z=191 TO 200
        If S!(X, 9) > LD!(200, 1) Then
            LD!(200, 1) = S!(X, 9)
            LD!(200, 2) = 0
            Z0$(200) = compnatN$(X)
            Z3$(200) = TN$
            For Z = 191 To 199
                For Z1 = Z + 1 To 200
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2: Swap LD!(Z1, M), LD!(Z, M): Next
                    End If
                Next Z1
            Next Z
        End If

        'FOR Z=201 TO 210
        If S!(X, 2) > LD!(210, 1) Then
            LD!(210, 1) = S!(X, 2)
            LD!(210, 2) = 0
            Z0$(210) = compnatN$(X)
            Z3$(210) = TN$
            For Z = 201 To 209
                For Z1 = Z + 1 To 210
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2: Swap LD!(Z1, M), LD!(Z, M): Next
                    End If
                Next Z1
            Next Z
        End If

        'FOR Z=211 TO 220
        If S!(X, 4) > LD!(220, 1) Then
            LD!(220, 1) = S!(X, 4)
            LD!(220, 2) = 0
            Z0$(220) = compnatN$(X)
            Z3$(220) = TN$
            For Z = 211 To 219
                For Z1 = Z + 1 To 220
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2: Swap LD!(Z1, M), LD!(Z, M): Next
                    End If
                Next Z1
            Next Z
        End If

        'FOR Z=221 TO 230
        If S!(X, 14) > LD!(230, 1) Then
            LD!(230, 1) = S!(X, 14)
            LD!(230, 2) = 0
            Z0$(230) = compnatN$(X)
            Z3$(230) = TN$
            For Z = 221 To 229
                For Z1 = Z + 1 To 230
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2: Swap LD!(Z1, M), LD!(Z, M): Next
                    End If
                Next Z1
            Next Z
        End If

        'FOR Z=231 TO 240
        If S!(X, 6) > LD!(240, 1) Then
            LD!(240, 1) = S!(X, 6)
            LD!(240, 2) = 0
            Z0$(240) = compnatN$(X)
            Z3$(240) = TN$
            For Z = 231 To 239
                For Z1 = Z + 1 To 240
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2: Swap LD!(Z1, M), LD!(Z, M): Next
                    End If
                Next Z1
            Next Z
        End If

        'FOR Z=241 TO 250
        If S!(X, 24) > LD!(250, 1) Then
            LD!(250, 1) = S!(X, 24)
            LD!(250, 2) = 0
            Z0$(250) = compnatN$(X)
            Z3$(250) = TN$
            For Z = 241 To 249
                For Z1 = Z + 1 To 250
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z0$(Z1), Z0$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2: Swap LD!(Z1, M), LD!(Z, M): Next
                    End If
                Next Z1
            Next Z
        End If

    Next X

    'FOR X=1 TO 40
    If OT!(19) > OL!(40, 19) Then
        For M = 1 To 20: OL!(40, M) = OT!(M): Next: OL$(40) = TN$
        For X = 1 To 39: 
            For X1 = 1 + X To 40: 
                If OL!(X, 19) < OL!(X1, 19) Then
                    Swap OL$(X1), OL$(X)
                    For M = 1 To 20: J = OL!(X1, M): OL!(X1, M) = OL!(X, M): OL!(X, M) = J: Next
                End If
            Next X1
        Next X
    End If

    'FOR X=1 TO 15
    If DT!(19) < DL!(40, 19) Then
        For M = 1 To 20: DL!(40, M) = DT!(M): Next:
        DL$(40) = TN$
        For X = 1 To 39
            For X1 = 1 + X To 40:
                If DL!(X, 19) > DL!(X1, 19) Then
                    Swap DL$(X1), DL$(X)
                    For M = 1 To 20: J! = DL!(X1, M): DL!(X1, M) = DL!(X, M): DL!(X, M) = J!: Next
                End If
            Next X1
        Next X
    End If

End Sub

'----------------------------------------
'       CheckStatsLeaders Subroutine
'----------------------------------------
'...explanation...
Sub CheckStatsLeaders (statFile$)

    Shared NDL!(), NOL!(), NLD!()
    Shared S!(), W0!(), W1!()

    Shared DT!(), OT!()

    Shared compnatN$()
    Shared NDL$(), NZ0$(), NZ3$()

    Cls: Print "CHECKING "; statFile$; " STATS AGAINST NATIONAL LEADERS"
    For X = 0 To 14
        Color 14: Print "CHECKING STATS OF ";: Color 15: Print compnatN$(X)
        If S!(X, 22) > NLD!(20, 1) Then
            NLD!(20, 1) = S!(X, 22)
            NLD!(20, 2) = 0
            NZ0$(20) = compnatN$(X)
            NZ3$(20) = TN$
            'Is this logic accurate?
            'It leads to this only be updating occassionally
            'This makes it hard for things to work without
            'Manually assigning values to the arrays
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

        If S!(X, 12) > NLD!(40, 1) Then
            NLD!(40, 1) = S!(X, 12)
            NLD!(40, 2) = 0
            NZ0$(40) = compnatN$(X)
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

        If S!(X, 19) > NLD!(60, 1) Then
            NLD!(60, 1) = S!(X, 19)
            NLD!(60, 2) = S!(X, 1)
            NZ0$(60) = compnatN$(X)
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

        If S!(X, 20) > NLD!(80, 1) Then
            NLD!(80, 1) = S!(X, 20)
            NLD!(80, 2) = S!(X, 3)
            NZ0$(80) = compnatN$(X)
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

        If S!(X, 15) > NLD!(90, 1) Then
            NLD!(90, 1) = S!(X, 15)
            NLD!(90, 2) = S!(X, 7)
            NZ0$(90) = compnatN$(X)
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

        If S!(X, 7) > NLD!(100, 1) Then
            NLD!(100, 1) = S!(X, 7)
            NLD!(100, 2) = 0
            NZ0$(100) = compnatN$(X)
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

        If S!(X, 16) > NLD!(110, 1) Then
            NLD!(110, 1) = S!(X, 16)
            NLD!(110, 2) = S!(X, 8)
            NZ0$(110) = compnatN$(X)
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

        'FOR Z=111 TO 120
        If S!(X, 8) > NLD!(120, 1) Then
            NLD!(120, 1) = S!(X, 8)
            NLD!(120, 2) = S!(X, 8)
            NZ0$(120) = compnatN$(X)
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

        'FOR Z=121 TO 130
        If S!(X, 21) > NLD!(130, 1) Then
            NLD!(130, 1) = S!(X, 21)
            NLD!(130, 2) = W1!(X)
            NZ0$(130) = compnatN$(X)
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
        If W0!(X) > NLD!(140, 1) Then
            NLD!(140, 1) = W0!(X)
            NLD!(140, 2) = 0
            NZ0$(140) = compnatN$(X)
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

        'FOR Z=141 TO 150
        If S!(X, 18) > NLD!(150, 1) Then
            NLD!(150, 1) = S!(X, 18)
            NLD!(150, 2) = 0
            NZ0$(150) = compnatN$(X)
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

        'FOR Z=151 TO 160
            If S!(X, 10) / 60 <= NLD!(160, 1) Then
            NLD!(160, 1) = S!(X, 10) / 60
            NLD!(160, 2) = 0
            NZ0$(160) = compnatN$(X)
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
        If S!(X, 13) > NLD!(170, 1) Then
            NLD!(170, 1) = S!(X, 13)
            NLD!(170, 2) = S!(X, 5)
            NZ0$(170) = compnatN$(X)
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

        'FOR Z=171 TO 180
        If S!(X, 5) > NLD!(180, 1) Then
            NLD!(180, 1) = S!(X, 5)
            NLD!(18, 2) = 0
            NZ0$(180) = compnatN$(X)
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

        'FOR Z=181 TO 190
        If S!(X, 17) > NLD!(190, 1) Then
            NLD!(190, 1) = S!(X, 17)
            NLD!(190, 2) = 0
            NZ0$(190) = compnatN$(X)
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

        'FOR Z=191 TO 200
        If S!(X, 9) > NLD!(200, 1) Then
            NLD!(200, 1) = S!(X, 9)
            NLD!(200, 2) = 0
            NZ0$(200) = compnatN$(X)
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

        'FOR Z=201 TO 210
        If S!(X, 2) > NLD!(210, 1) Then
            NLD!(210, 1) = S!(X, 2)
            NLD!(210, 2) = 0
            NZ0$(210) = compnatN$(X)
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

        'FOR Z=211 TO 220
        If S!(X, 4) > NLD!(220, 1) Then
            NLD!(220, 1) = S!(X, 4)
            NLD!(220, 2) = 0
            NZ0$(220) = compnatN$(X)
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

        'FOR Z=221 TO 230
        If S!(X, 14) > NLD!(230, 1) Then
            NLD!(230, 1) = S!(X, 14)
            NLD!(230, 2) = 0
            NZ0$(230) = compnatN$(X)
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

        'FOR Z=231 TO 240
        If S!(X, 6) > NLD!(240, 1) Then
            NLD!(240, 1) = S!(X, 6)
            NLD!(240, 2) = 0
            NZ0$(240) = compnatN$(X)
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

        'FOR Z=241 TO 250
        If S!(X, 24) > NLD!(250, 1) Then
            NLD!(250, 1) = S!(X, 24)
            NLD!(250, 2) = 0
            NZ0$(250) = compnatN$(X)
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
                        J! = NOL!(X1, M)
                        NOL!(X1, M) = NOL!(X, M)
                        NOL!(X, M) = J!
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
                        J! = NDL!(X1, M)
                        NDL!(X1, M) = NDL!(X, M)
                        NDL!(X, M) = J!
                    Next
                End If
            Next X1
        Next X
    End If
    '4840 Return

End Sub
