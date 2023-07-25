Sub NEWLDR ()

    _Title "Courtside College Basketball - Expanded Leaders"

    ReDim menuChoices$(1 To 2)

    Do

        Cls
        Color 15, 0

        Call TITLE

        menuChoices$(1) = "VIEW EXPANDED LEAGUE STATISTICS "
        menuChoices$(2) = "RETURN TO LEAGUE LEADER MENU    "
        Selection% = 1
        Count% = 2
        Start% = 1
        ExitCode% = 2
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 8
        Column% = 25

        Call box0(7, 24, 10, 57, 2, 15, 1)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case Selection%

                Case 1:
                    Call ViewExpandedStats

            End Select

        End If

    Loop Until Selection% = 2 Or ExitCode% = 27

End Sub

'----------------------------------------
'       ViewExpandedStats Subroutine
'----------------------------------------
'...explanation...
Sub ViewExpandedStats ()

    Dim printOrView%
    Dim expanZ1$(40)

    printOrView% = 0

    For X = 1 To 40
        expanZ1$(X) = "XXX"
    Next

    Spec$ = diskPaths$(1) + "*.TMS"
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

            Color , 1
            Cls
            Locate , 28
            Color 14
            Print "SELECT CONFERENCE": Color 15
            Selection% = 1
            Start% = 1
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
            MAMenu conferenceFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
            _MouseHide

            If ExitCode% <> 27 Then

                Color 15, 1
                targetFile$ = RTrim$(conferenceFiles$(Selection%))
                Locate 2, 33: Print targetFile$

                Do
                    Locate 3, 33: Print "ANY CHANGE (YN)";
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                Color 15, 0

                If UCase$(I$) = "N" Then

                    targetFile$ = RTrim$(targetFile$)
                    targetFile$ = Left$(targetFile$, Len(targetFile$) - 4)

                    Open diskPaths$(1) + targetFile$ + ".TMS" For Input As #2

                    Input #2, LG$, D1$, D2$

                    For X = 1 To 30
                        Input #2, expanZ1$(X)
                    Next

                    Close 2

                    Call UpdateExpandedLeaders(JB, targetFile$, expanZ1$(), TS, TTS)

                    ReDim menuChoices$(1 To 3)

                    Do
                        Cls

                        Call TITLE
                        menuChoices$(1) = "VIEW INDIVIDUAL PLAYER LEADERS  "
                        menuChoices$(2) = "VIEW TEAM TOTALS                "
                        menuChoices$(3) = "RETURN TO LEAGUE LEADER MENU    "
                        Selection% = 1
                        Count% = 3
                        Start% = 1
                        ExitCode% = 3
                        FileFGColr% = 15: FileBGColr% = 1
                        HiLiteFGColr% = 0: HiLiteBGColr% = 7
                        Spacing% = 1
                        Row% = 8
                        Column% = 25

                        Call box0(7, 24, 11, 57, 2, 15, 1)

                        _MouseShow "DEFAULT"
                        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
                        _MouseHide

                        Color , 0

                        If ExitCode% <> 27 Then

                            Select Case Selection%

                                Case 1:
                                    Call ViewIndividualLeaders(JB, printOrView%)

                                Case 2:
                                    Call ViewTeamLeaders(printOrView%, TS, TTS)

                            End Select

                        End If

                    Loop Until Selection% = 3

                End If ' End of checking if Any Change Y/N

            End If 'End of checking return code from menu

        Loop Until UCase$(I$) = "N" Or ExitCode% = 27

    End If 'Done checking file count

End Sub

'----------------------------------------
'    ViewIndividualLeaders Subroutine
'----------------------------------------
'...explanation...
Sub ViewIndividualLeaders (JB, printOrView%)

    Shared AL$()
    Shared TMA$(), TMM$(), TP$(), TPP$()

    Shared statsAL!(), GMA!(), TYP!()
    Shared W0L!(), W1L!()

    Do

        Color , 0
        Cls
        Print
        Print
        Print
        Print

        J = 20
        ZZ = 1

        Locate 2, 4
        Print " " + Chr$(214); String$(66, 196); Chr$(183) + " ";

        For II = 1 To J:
            Locate 1 + II, 4
            Print " " + Chr$(186); String$(66, 32); Chr$(186) + " ";
        Next

        Locate 3 + J, 4
        Print " " + Chr$(211); String$(66, 196); Chr$(189) + " ";
        Locate 10, 15

        Do

            For I = 0 To 15
                Color 14
                Locate I + 3, 8: Print Using "##"; I;
                Print " ";
                Color 0: Print TP$(I);

                Color 14
                Print Tab(40); Using "##"; I + 16;
                Print " ";
                Color 0
                Print TP$(I + 16)
            Next

            Print
            Locate , 23: Input "SELECT NUMBER TO VIEW"; B9

            stpl = 0

            Select Case B9

                Case 0: X = 1
                Case 1: X = 11
                Case 2: X = 3
                Case 3: X = 2
                Case 4: X = 3: X1 = 2
                Case 5 To 7: '
                Case 8: X = 5
                Case 9: X = 4
                Case 10: X = 5: X1 = 4
                Case 11: X = 3: X1 = 5
                Case 12: X = 3: X1 = 5: X2 = 1
                Case 13: X = 14
                Case 14: X = 13
                Case 15: X = 6: X1 = 13
                Case 16: X = 6
                Case 17: X = 6: X1 = 1
                Case 18: X = 7
                Case 19: X = 9
                Case 20: X = 10
                Case 21: X = 8
                Case 22: X = 12
                Case 23: X = 3: X1 = 5: X2 = 11
                Case 24: X = 6: X1 = 11
                Case 25: X = 7: X1 = 11
                Case 26: X = 9: X1 = 11
                Case 27: X = 10: X1 = 11
                Case 28: X = 8: X1 = 11
                Case 29: X = 12: X1 = 11
                Case 30: X = 11: X1 = 1
                Case 31: Exit Sub

            End Select

        Loop Until B9 >= 0 And B9 <= 31

        If B9 <> 31 Then

            Call SortOptions(JB, ZZ, printOrView%, sortType%)

            Cls

            PPP = 0

            Select Case B9

                Case 0, 2 To 3, 8 To 9, 13 To 14, 16, 18 To 22:

                    For I = 1 To JB
                        If statsAL!(I, X) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                            TPP$(PPP) = AL$(I)
                            TMM$(PPP) = TMA$(I)
                            TYP!(PPP) = statsAL!(I, X)
                        End If
                    Next I

                    Call SortPlayer(PPP, stpl, sortType%)
                    TVN = 1

                Case 1:
                    For I = 1 To JB
                        If statsAL!(I, X) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                            TPP$(PPP) = AL$(I)
                            TMM$(PPP) = TMA$(I)
                            TYP!(PPP) = (statsAL!(I, X) + 30) / 60
                        End If
                    Next I

                    Call SortPlayer(PPP, stpl, sortType%)
                    TVN = 1

                Case 4:
                    For I = 1 To JB
                        If statsAL!(I, X) >= GMA!(I) * 3.65 Then
                            PPP = PPP + 1
                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                            TPP$(PPP) = AL$(I)
                            TMM$(PPP) = TMA$(I)
                            TYP!(PPP) = statsAL!(I, X) / statsAL!(I, X1)
                        End If
                    Next I

                    Call SortPlayer(PPP, stpl, sortType%)
                    TVN = 2

                Case 5:
                    For I = 1 To JB
                        If W0L!(I) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                            TPP$(PPP) = AL$(I)
                            TMM$(PPP) = TMA$(I)
                            TYP!(PPP) = W0L!(I)
                        End If
                    Next I

                    Call SortPlayer(PPP, stpl, sortType%)
                    TVN = 1

                Case 6:
                    For I = 1 To JB
                        If W1L!(I) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                            TPP$(PPP) = AL$(I)
                            TMM$(PPP) = TMA$(I)
                            TYP!(PPP) = W1L!(I)
                        End If
                    Next I

                    Call SortPlayer(PPP, stpl, sortType%)
                    TVN = 1

                Case 7:
                    For I = 1 To JB
                        If W0L!(I) >= GMA!(I) * .3 Then
                            PPP = PPP + 1
                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                            TPP$(PPP) = AL$(I)
                            TMM$(PPP) = TMA$(I)
                            TYP!(PPP) = W0L!(I) / W1L!(I)
                        End If
                    Next I

                    Call SortPlayer(PPP, stpl, sortType%)
                    TVN = 2

                Case 10:
                    For I = 1 To JB
                        If statsAL!(I, X) >= GMA!(I) * 1.52 Then
                            PPP = PPP + 1
                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                            TPP$(PPP) = AL$(I)
                            TMM$(PPP) = TMA$(I)
                            TYP!(PPP) = statsAL!(I, X) / statsAL!(I, X1)
                        End If
                    Next I

                    Call SortPlayer(PPP, stpl, sortType%)
                    TVN = 2

                Case 11:
                    For I = 1 To JB
                        If (statsAL!(I, X) * 2) + statsAL!(I, X1) + W0L!(I) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                            TPP$(PPP) = AL$(I)
                            TMM$(PPP) = TMA$(I)
                            TYP!(PPP) = (statsAL!(I, X) * 2) + statsAL!(I, X1) + W0L!(I)
                        End If
                    Next I

                    Call SortPlayer(PPP, stpl, sortType%)
                    TVN = 1

                Case 12:
                    For I = 1 To JB
                        If (statsAL!(I, X) * 2) + statsAL!(I, X1) + W0L!(I) <> 0 And statsAL!(I, X2) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                            TPP$(PPP) = AL$(I)
                            TMM$(PPP) = TMA$(I)
                            TYP!(PPP) = ((statsAL!(I, X) * 2) + statsAL!(I, X1) + W0L!(I)) / statsAL!(I, X2)
                        End If
                    Next I

                    Call SortPlayer(PPP, stpl, sortType%)
                    TVN = 3

                Case 15:
                    For I = 1 To JB
                        If statsAL!(I, X) - statsAL!(I, X1) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                            TPP$(PPP) = AL$(I)
                            TMM$(PPP) = TMA$(I)
                            TYP!(PPP) = statsAL!(I, X) - statsAL!(I, X1)
                        End If
                    Next I

                    Call SortPlayer(PPP, stpl, sortType%)
                    TVN = 1

                Case 17:
                    For I = 1 To JB
                        If statsAL!(I, X) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                            TPP$(PPP) = AL$(I)
                            TMM$(PPP) = TMA$(I)
                            TYP!(PPP) = statsAL!(I, X) / statsAL!(I, X1)
                        End If
                    Next I

                    Call SortPlayer(PPP, stpl, sortType%)
                    TVN = 3

                Case 23:
                    For I = 1 To JB
                        If (statsAL!(I, X) * 2) + statsAL!(I, X1) + W0L!(I) <> 0 And statsAL!(I, X2) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                            TPP$(PPP) = AL$(I)
                            TMM$(PPP) = TMA$(I)
                            TYP!(PPP) = (((statsAL!(I, X) * 2) + statsAL!(I, X1) + W0L!(I))) * 48 / ((statsAL!(I, X2) + 30) / 60)
                        End If
                    Next I

                    Call SortPlayer(PPP, stpl, sortType%)
                    TVN = 3

                Case 24 To 29:
                    For I = 1 To JB
                        If statsAL!(I, X) <> 0 And statsAL!(I, X1) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                            TPP$(PPP) = AL$(I)
                            TMM$(PPP) = TMA$(I)
                            TYP!(PPP) = (statsAL!(I, X)) * 48 / ((statsAL!(I, X1) + 30) / 60)
                        End If
                    Next I

                    Call SortPlayer(PPP, stpl, sortType%)
                    TVN = 3

                Case 30:
                    For I = 1 To JB
                        If statsAL!(I, X) <> 0 And statsAL!(I, X1) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                            TPP$(PPP) = AL$(I)
                            TMM$(PPP) = TMA$(I)
                            TYP!(PPP) = ((statsAL!(I, X) + 30) / 60) / statsAL!(I, X1)
                        End If
                    Next I

                    Call SortPlayer(PPP, stpl, sortType%)
                    TVN = 3

            End Select

            QW = PPP

            Select Case printOrView%

                Case 1
                    Call DisplayLeaders(TVN, QW, B9)

                Case 2
                    Call GetListLength(printOrView%, LG, QW)
                    Call PrintLeaders(TVN, LG, B9)

            End Select

        End If 'Done checking for exit

    Loop Until B9 = 31 'Done selecting number

End Sub


'----------------------------------------
'       DisplayLeaders Subroutine
'----------------------------------------
'... explain here ...
Sub DisplayLeaders (TVN, maxRecords, categoryId)

    Shared TYP!()
    Shared TMM$(), TPP$(), TP$()

    userExit% = 0

    Do

        Color 14, 0
        Cls
        Print "     PLAYER                  TEAM                     "; TP$(categoryId)

        For W = 1 To maxRecords

            If W <> 0 And (W Mod 20 = 0) Then

				Color 14
				Locate 22: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
				Color 15

                I$ = GetKeyPress$

                If I$ <> Chr$(27) Then
                    Cls
                    Color 14: Print "     PLAYER                  TEAM                     "; TP$(categoryId)
                Else
                    userExit% = 1
                End If
            End If

            If userExit% <> 1 Then

                Color 12
                Print Using "###"; W;
                Print " ";
                Color 3: Print TPP$(W);
                Color 15
                Print Tab(25); TMM$(W);

                Select Case TVN

                    Case 1
                        Print Tab(58); Using "#####"; TYP!(W)

                    Case 2
                        Print Tab(58); Using ".###"; TYP!(W)

                    Case 3
                        Print Tab(58); Using "###.#"; TYP!(W)

                End Select

            Else
                Exit Do
            End If

        Next W

        If userExit% <> 1 Then

			Color 14
			Locate 22: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
			Color 15

            I$ = GetKeyPress$

            If I$ <> Chr$(27) Then

                Locate 22: Print Space$(64)
                Color 14

                Do
                    Locate 22
                    Print "VIEW AGAIN (Y/N)"
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "N" Or UCase$(I$) = "Y"

            End If

        End If

    Loop Until UCase$(I$) = "N" Or I$ = Chr$(27)

End Sub


'----------------------------------------
'         PrintLeaders Subroutine
'----------------------------------------
'... explain here ...
Sub PrintLeaders (TVN, maxRecords, categoryID)

    Shared TYP!()
    Shared TMM$(), TP$(), TPP$()

    Color 0, 15
    Cls

    LPrint "     PLAYER                  TEAM                     "; TP$(categoryID)

    For W = 1 To maxRecords

        If (W <> 0 And W Mod 50 = 0) Then
            LPrint Chr$(12)
            LPrint "     PLAYER                  TEAM                     "; TP$(categoryID)
        End If

        LPrint Using "###"; W;
        LPrint " ";
        LPrint TPP$(W);
        LPrint Tab(25); TMM$(W);

        Select Case TVN

            Case 1: 
				LPrint Tab(58); Using "#####"; TYP!(W)

            Case 2: 
				LPrint Tab(58); Using ".###"; TYP!(W)

            Case 3: 
				LPrint Tab(58); Using "###.#"; TYP!(W)

        End Select

    Next W

    LPrint Chr$(12)

End Sub


'----------------------------------------
'       ViewTeamLeaders Subroutine
'----------------------------------------
'...explanation...
Sub ViewTeamLeaders (printOrView%, tStats, ttStats)

    Shared expanTT$(), TMM$(), TP1$(), TT1$()
    Shared GM!(), GM1!(), TT!(), TYP!(), TT1!()

    Do

        Color , 0
        Cls
        Print
        Print
        Print
        Print

        J = 21
        ZZ = 3

        Locate 1, 4
        Color 15, 6
        Print " " + Chr$(214); String$(66, 196); Chr$(183) + " ";

        For II = 1 To J:
            Locate 1 + II, 4
            Print " " + Chr$(186); String$(66, 32); Chr$(186) + " ";
        Next

        Locate 2 + J, 4
        Print " " + Chr$(211); String$(66, 196); Chr$(189) + " ";
        Locate 10, 15

        For I = 0 To 19:
            Color 11
            Locate I + 2, 8
            Print Using "##"; I;
            Print " ";
            Color 0: Print TP1$(I)
        Next

        For I = 20 To 38:
            Color 11
            Locate I - 18, 38
            Print Using "##"; I;
            Print " ";
            Color 0: Print TP1$(I)
        Next

        Do
            Color 14
            Locate 22, 23: Input "SELECT NUMBER TO VIEW"; B9

            PPP = 0
            stpl = 0

            Select Case B9
                Case 0: X = 1
                Case 1: X = 2
                Case 2: X = 2: X1 = 1
                Case 3: X = 14
                Case 4: X = 13
                Case 5: X = 13: X1 = 14
                Case 6: X = 3
                Case 7: X = 4
                Case 8: X = 4: X1 = 3
                Case 9: X = 12
                Case 10: X = 5: X1 = 12
                Case 11: X = 5
                Case 12: X = 5
                Case 13: X = 6
                Case 14: X = 8
                Case 15: X = 9
                Case 16: X = 7
                Case 17: X = 11
                Case 18: X = 2: X1 = 13: X2 = 4
                Case 19: X = 1
                Case 20: X = 2
                Case 21: X = 2: X1 = 1
                Case 22: X = 14
                Case 23: X = 13
                Case 24: X = 13: X1 = 14
                Case 25: X = 3
                Case 26: X = 4
                Case 27: X = 4: X1 = 3
                Case 28: X = 12
                Case 29: X = 5: X1 = 12
                Case 30: X = 5
                Case 31: X = 5
                Case 32: X = 6
                Case 33: X = 8
                Case 34: X = 9
                Case 35: X = 7
                Case 36: X = 11
                Case 37: X = 2: X1 = 13: X2 = 4
            End Select

            If B9 <> 38 Then
                Call SortOptions(JB, ZZ, printOrView%, sortType%)
                Cls
            End If

            Select Case B9

                Case 0 To 1, 3 To 4, 6 To 7, 9, 11, 13 To 17:

                    For I = 1 To tStats
                        TMM$(I) = expanTT$(I, X)
                        TYP!(I) = TT!(I, X)
                    Next

                    Call SortTeam(tStats, stpl, sortType%)
                    TVN = 1
                    maxRecords = tStats

                Case 2, 5, 8:

                    For I = 1 To tStats
                        If TT!(I, X1) = 0 Then
                            TMM$(I) = expanTT$(I, X)
                            TYP!(I) = 0
                        Else
                            TMM$(I) = expanTT$(I, X)
                            TYP!(I) = TT!(I, X) / TT!(I, X1)
                        End If
                    Next

                    Call SortTeam(tStats, stpl, sortType%)
                    TVN = 2
                    maxRecords = tStats

                Case 10:

                    For I = 1 To tStats
                        TMM$(I) = expanTT$(I, X)
                        TYP!(I) = TT!(I, X) - TT!(I, X1)
                    Next

                    Call SortTeam(tStats, stpl, sortType%)
                    TVN = 1
                    maxRecords = tStats

                Case 12:
                    For I = 1 To tStats
                        TMM$(I) = expanTT$(I, X)
                        TYP!(I) = TT!(I, X) / GM!(I)
                    Next

                    Call SortTeam(tStats, stpl, sortType%)
                    TVN = 3
                    maxRecords = tStats

                Case 18:
                    For I = 1 To tStats
                        TMM$(I) = expanTT$(I, X)
                        TYP!(I) = ((TT!(I, X) * 2) + TT!(I, X1) + TT!(I, X2)) / GM!(I)
                    Next

                    Call SortTeam(tStats, stpl, sortType%)
                    TVN = 3
                    maxRecords = tStats

                Case 19 To 20, 22 To 23, 25 To 26, 28, 30:

                    For I = 1 To ttStats
                        TMM$(I) = TT1$(I, X)
                        TYP!(I) = TT1!(I, X)
                    Next

                    Call SortTeam(ttStats, stpl, sortType%)
                    TVN = 1
                    maxRecords = ttStats

                Case 21, 24, 27:

                    For I = 1 To ttStats
                        If TT1!(I, X1) = 0 Then
                            TMM$(I) = TT1$(I, X): TYP!(I) = 0
                        Else
                            TMM$(I) = TT1$(I, X): TYP!(I) = TT1!(I, X) / TT1!(I, X1)
                        End If
                    Next

                    Call SortTeam(ttStats, stpl, sortType%)
                    TVN = 2
                    maxRecords = ttStats

                Case 29:
                    For I = 1 To ttStats
                        TMM$(I) = TT1$(I, X)
                        TYP!(I) = TT1!(I, X) - TT1!(I, X1)
                    Next

                    Call SortTeam(ttStats, stpl, sortType%)
                    TVN = 1
                    maxRecords = ttStats

                Case 31 To 36:
                    For I = 1 To ttStats
                        TMM$(I) = TT1$(I, X)
                        TYP!(I) = TT1!(I, X) / GM1!(I)
                    Next

                    Call SortTeam(ttStats, stpl, sortType%)
                    TVN = 3
                    maxRecords = ttStats

                Case 37:
                    For I = 1 To ttStats
                        TMM$(I) = TT1$(I, X)
                        TYP!(I) = ((TT1!(I, X) * 2) + TT1!(I, X1) + TT1!(I, X2)) / GM1!(I)
                    Next

                    Call SortTeam(ttStats, stpl, sortType%)
                    TVN = 3
                    maxRecords = ttStats

            End Select

            If B9 <> 38 Then
                Call DisplayTeamView(printOrView%, TVN, maxRecords, B9)
            End If

        Loop Until B9 >= 0 And B9 <= 38

    Loop Until B9 = 38

End Sub


'----------------------------------------
'       DisplayTeamView Subroutine
'----------------------------------------
'...explanation...
Sub DisplayTeamView (printOrView%, TVN, maxRecords, categoryID)

    Shared TYP!()
    Shared TMM$(), TP1$()

    userExit% = 0

    Select Case printOrView%

        Case 1:

            Do

                Color 14, 0
                Cls
                Print "     TEAM                     "; TP1$(categoryID)

                For W = 1 To maxRecords

                    If (W Mod 20 = 0) Then

						Color 14
						Locate 22: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
						Color 15

                        I$ = GetKeyPress$
                        If I$ = Chr$(27) Then userExit% = 1

                    End If

                    If userExit% <> 1 Then

                        Color 12
                        Print Using "###"; W;
                        Print " ";
                        Color 3: Print TMM$(W);
                        Color 15

                        Select Case TVN

                            Case 1:
                                Print Tab(32); Using "#####"; TYP!(W)

                            Case 2:
                                Print Tab(33); Using ".###"; TYP!(W)

                            Case 3:
                                Print Tab(33); Using "###.#"; TYP!(W)

                        End Select

                    End If

                Next W

                If userExit% <> 1 Then

					Color 14
					Locate 22: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
					Color 15

                    I$ = GetKeyPress$

                    If I$ <> Chr$(27) Then

                        Do
                            Locate 22: Print Space$(64)
                            Color 14
                            Locate 22: Print "VIEW AGAIN (Y/N)"
                            A$ = GetKeyPress$
                        Loop Until UCase$(A$) = "Y" Or UCase$(A$) = "N"

                    Else
                        I$ = "N"
                    End If

                End If

            Loop Until UCase$(I$) = "N" Or I$ = Chr$(27) Or UCase$(A$) = "N"

        Case 2:

            Color 0, 15

            LPrint "     TEAM                     "; TP1$(categoryID)

            For W = 1 To maxRecords

                LPrint Using "###"; W;
                LPrint " "; TMM$(W);
                Color 15

                Select Case TVN

                    Case 1: 
						LPrint Tab(32); Using "#####"; TYP!(W)

                    Case 2: 
						LPrint Tab(33); Using ".###"; TYP!(W)
                    
					Case 3: 
						LPrint Tab(33); Using "###.#"; TYP!(W)

                End Select

            Next W

            LPrint Chr$(12)

    End Select

End Sub

'----------------------------------------
'         SortOptions Subroutine
'----------------------------------------
'...explanation...
Sub SortOptions (JB, ZZ, printOrView%, sortType%)

    'sortType% = 1: Sort HIGH to LOW
    'sortType% = 2: Sort LOW to HIGH

    Do

        J = 6
        Locate 8, 8
        Color 15, 1: Print " " + Chr$(214); String$(57, 196); Chr$(183) + " ";

        For II = 1 To J
            Locate 8 + II, 8
            Print " " + Chr$(186); String$(57, 32); Chr$(186) + " ";
        Next
        Locate 9 + J, 8
        Print " " + Chr$(211); String$(57, 196); Chr$(189) + " ";
        Locate 10, 15

        Color 3: Locate , 30: Print "SELECT OPTION:"
        Locate , 30: Color 15: Print "H";: Color 7: Print "IGHEST TO LOWEST"
        Locate , 30: Color 15: Print "L";: Color 7: Print "OWEST TO HIGHEST"
        Locate , 30: Color 15: Print "R";: Color 7: Print "E-SELECT CATEGORY"

        I$ = GetKeyPress$

        Select Case UCase$(I$)

            Case "H":
                Call PrintViewReselect(J$)

                Select Case UCase$(J$)
                    Case "P": printOrView% = 2
                    Case "V": printOrView% = 1
                    Case "R":
                        If ZZ = 1 Then Call ViewIndividualLeaders(JB, printOrView%)
                        If ZZ = 3 Then Call ViewTeamLeaders(printOrView%, TS, TTS)
                End Select

                sortType% = 1

            Case "L"
                Call PrintViewReselect(I2$)

                Select Case I2$
                    Case "P": printOrView% = 2
                    Case "V": printOrView% = 1
                    Case "R":
                        'I$ = "R" will also put us into the next "R" statement
                        If ZZ = 1 Then Call ViewIndividualLeaders(JB, printOrView%)
                        If ZZ = 3 Then Call ViewTeamLeaders(printOrView%, TS, TTS)
                End Select

                sortType% = 2

            Case "R":
                If ZZ = 1 Then Call ViewIndividualLeaders(JB, printOrView%)
                If ZZ = 3 Then Call ViewTeamLeaders(printOrView%, TS, TTS)

        End Select

    Loop Until UCase$(I$) = "H" Or UCase$(I$) = "L" Or UCase$(I$) = "R"

End Sub

'----------------------------------------
'     PrintViewReselect Subroutine
'----------------------------------------
'... explain here ...
Sub PrintViewReselect (userOpt$)

    J = 4

    Locate 9, 15
    Color 15, 0

    Print " " + Chr$(214); String$(40, 196); Chr$(183) + " ";

    For II = 1 To J
        Locate 9 + II, 15: Print " " + Chr$(186); String$(40, 32); Chr$(186) + " ";
    Next

    Locate 10 + J, 15
    Print " " + Chr$(211); String$(40, 196); Chr$(189) + " ";
    Locate 10, 10

    Color 12
    Locate , 30: Print "SELECT OPTION:"
    Locate , 30: Color 14: Print "P";: Color 7: Print "RINT"
    Locate , 30: Color 14: Print "V";: Color 7: Print "IEW"
    Locate , 30: Color 14: Print "R";: Color 7: Print "E-SELECT OPTION"

    Do
        userOpt$ = GetKeyPress$
    Loop Until UCase$(userOpt$) = "P" Or UCase$(userOpt$) = "V" Or UCase$(userOpt$) = "R"

End Sub


'----------------------------------------
'       SortPlayer Subroutine
'----------------------------------------
'...explanation...
Sub SortPlayer (PPP, stpl, sortType%)

    Shared TMM$(), TPP$()
    Shared TYP!()

    For I = 2 To PPP

        stpl = stpl + 1
        Locate 5, 5: Print "sorting player "; stpl

        For J = PPP To I Step -1

            If sortType% = 1 Then
                If TYP!(J - 1) < TYP!(J) Then
                    Swap TPP$(J - 1), TPP$(J)
                    Swap TMM$(J - 1), TMM$(J)
                    Swap TYP!(J - 1), TYP!(J)
                End If
            End If

            If sortType% = 2 Then
                If TYP!(J - 1) > TYP!(J) Then
                    Swap TPP$(J - 1), TPP$(J)
                    Swap TMM$(J - 1), TMM$(J)
                    Swap TYP!(J - 1), TYP!(J)
                End If
            End If

        Next J

    Next I

End Sub


'----------------------------------------
'              GetListLength
'----------------------------------------
'...explanation...
Sub GetListLength (printOrView%, listLength, maxItems)

    If printOrView% <> 1 Then
        'If printing...

        Do
            Color , 0
            Cls

            Print
            Print
            Print
            Print
            Print
            J = 2

            Locate 10, 20
            Color 4, 15
            Print " " + Chr$(214); String$(30, 196); Chr$(183) + " ";

            For II = 1 To J
                Locate 10 + II, 20: Print " " + Chr$(186); String$(30, 32); Chr$(186) + " ";
            Next

            Locate 11 + J, 20
            Print " " + Chr$(211); String$(30, 196); Chr$(189) + " "
            Locate 11

            Color 1
            Locate , 25: Print "MAXIMUM = "; maxItems
            Locate , 25: Input "ENTER LENGTH OF LIST"; listLength

        Loop Until listLength <= maxItems

    End If

End Sub


'----------------------------------------
'     UpdateExpandedLeaders Subroutine
'----------------------------------------
' This creates an .LDS file for your league
Sub UpdateExpandedLeaders (JB, statFile$, Z1$(), tStats, ttStats)

    Shared statPlyrNames$(), AL$(), expanTT$()
    Shared TMA$(), TT1$()

    Shared playerOff!(), statsAL!()
    Shared GM!(), GMA!(), GM1!()
    Shared TT!(), TT1!()
    Shared statsW0!(), statsW1!(), W0L!(), W1L!()
    Shared statsZ!(), statsZ1!()

    Color 15, 0
    Cls

    For KK = 1 To 30

        YY = YY + 1

        If Z1$(KK) <> "*" And Z1$(KK) <> "XXX" And Z1$(KK) <> "" Then

            Print "CONVERTING STAT FILE OF "; Z1$(KK)

            If _FileExists(diskPaths$(1) + Z1$(KK)) Then

                Open diskPaths$(1) + Z1$(KK) For Input As #1

                Input #1, B$

                For I = 0 To 13

                    Input #1, statPlyrNames$(I)

                    For J = 0 To 13:
                        Input #1, playerOff!(I, J)
                    Next

                    Input #1, statsW0!(I)
                    Input #1, statsW1!(I)

                Next

                For L = 0 To 15:
                    Input #1, statsZ!(L)
                Next

                For L = 0 To 15:
                    Input #1, statsZ1!(L)
                Next

                Close #1

                Open diskPaths$(1) + Z1$(KK) + "S" For Input As #1

                Input #1, year

                Close #1

                tStats = tStats + 1

                For I = 0 To 14
                    TT!(tStats, I) = statsZ!(I)
                    expanTT$(tStats, I) = B$
                    GM!(tStats) = year
                Next

                ttStats = ttStats + 1

                For I = 0 To 14:
                    TT1!(ttStats, I) = statsZ1!(I)
                    TT1$(ttStats, I) = B$
                    GM1!(ttStats) = year
                Next

                For I = 0 To 14

                    If statPlyrNames$(I) <> "XXX" Then

                        JB = JB + 1
                        AL$(JB) = statPlyrNames$(I)
                        TBN = TBN + 1
                        TMA$(TBN) = B$
                        GMA!(JB) = year
                        K1 = 0

                        For I1 = 0 To 13
                            K1 = K1 + 1
                            statsAL!(JB, K1) = playerOff!(I, I1)
                        Next I1

                    End If

                    W0L!(JB) = statsW0!(I)
                    W1L!(JB) = statsW1!(I)

                Next I

            End If

        End If

    Next KK

    Call SAVELEADER(statFile$, JB, tStats, ttStats)

End Sub

'----------------------------------------
'           SortTeam Subroutine
'----------------------------------------
'...explanation...
Sub SortTeam (sortMax, stpl, sortType%)

    Shared TYP!()
    Shared TMM$()

    For I = 2 To sortMax

        stpl = stpl + 1

        Locate 5, 5: Print "sorting team "; stpl

        For J = sortMax To I Step -1

            If sortType% = 1 Then
                If TYP!(J - 1) < TYP!(J) Then
                    Swap TMM$(J - 1), TMM$(J)
                    Swap TYP!(J - 1), TYP!(J)
                End If
            End If

            If sortType% = 2 Then
                If TYP!(J - 1) > TYP!(J) Then
                    Swap TMM$(J - 1), TMM$(J)
                    Swap TYP!(J - 1), TYP!(J)
                End If
            End If

        Next J

    Next I

End Sub


'----------------------------------------
'        SAVELEADER Subroutine
'----------------------------------------
' Compiles leader files for the expanded
' standings modules (.LDS)
Sub SAVELEADER (statFile$, JB, tStats, ttStats)

    Shared statPlyrNames$(), AL$(), expanTT$()
    Shared TMA$(), TT1$()

    Shared statsAL!()
    Shared GM!(), GMA!(), GM1!()
    Shared TT!(), TT1!()
    Shared W0L!(), W1L!()

    TMR = 600

    Print "SAVING LEADER FILES . . . . PLEASE WAIT"

    Open diskPaths$(1) + statFile$ + ".LDS" For Output As #1

    Write #1, JB, tStats, ttStats

    For I = 1 To 600:

        Write #1, AL$(I), TMA$(I), GMA!(I)

        Color 14
        Locate 18, 45
        TMR% = TMR% - 1
        Print Using "#####"; TMR%

        For I1 = 1 To 14:
            Write #1, statsAL!(I, I1)
        Next I1

        Write #1, W0L!(I)
        Write #1, W1L!(I)
        Color 14
        Locate 18, 45
        TMR% = TMR% - 1
        Print Using "#####"; TMR%
    Next I

    For I = 1 To 40

        For I1 = 0 To 14
            Write #1, TT!(I, I1), expanTT$(I, I1)
        Next

        Color 14: Locate 18, 45
        TMR% = TMR% - 1
        Print Using "#####"; TMR%
        Write #1, GM!(I)

    Next

    For I = 1 To 40

        For I1 = 0 To 14
            Write #1, TT1!(I, I1), TT1$(I, I1)
        Next

        Color 14: Locate 18, 45
        TMR% = TMR% - 1
        Print Using "#####"; TMR%
        Write #1, GM1!(I)

    Next

    Close #1

End Sub

