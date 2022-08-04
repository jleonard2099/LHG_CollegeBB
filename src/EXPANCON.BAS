Sub EXPANCON ()

    _Title "Courtside College Basketball - Expanded Leaders"

    'K = 0
    'K1 = 0
    'PK = 0
    'PK1 = 0
    'JB = 0
    'JP = 0
    'TBN = 0
    'TPN = 0
    'YY = 0
    'TMR = 1280
    'TS = 0
    'PV = 0
    Shared JB, PPP

    Cls

    Do

        Cls

        Call TITLE
        'Erase menuChoices$
        ReDim menuChoices$(1 To 2)
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
                Case 1: Call ViewExpanded
                Case 2: Cls
            End Select
        End If

    Loop Until Selection% = 2 Or ExitCode% = 27

End Sub


'----------------------------------------
'       ViewExpanded Subroutine
'----------------------------------------
'...explanation...
Sub ViewExpanded ()

    Dim expanZ1$(40)

    For X = 1 To 40
        expanZ1$(X) = "XXX"
    Next

    Spec$ = diskPaths$(1) + "*.TMS"
    null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
    Count% = DirCount%

    If Count% = 0 Then
        Locate 10, 8
        Call CSMsgBox("NO LEAGUE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)
    Else
        ReDim leagueFiles$(0 To Count%)
        For X = 1 To Count%
            leagueFiles$(X) = Space$(12)
        Next
        leagueFiles$(0) = Spec$
        Call ReadFile(leagueFiles$())
        'CALL SortStr(VARPTR(leagueFiles$(1)), Count%, 0)

        Do
            Cls
            Color , 1
            Cls
            Locate , 28
            Color 14
            Print "SELECT LEAGUE": Color 15
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
            Print "-SELECTS LEAGUE";

            _MouseShow "DEFAULT"
            MAMenu leagueFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
            _MouseHide

            If ExitCode% = 27 Then Exit Sub

            targetFile$ = leagueFiles$(Selection%)
            Locate 2, 33: Print targetFile$
            Locate , 33: Print "ANY CHANGE (YN)";
            Color 15, 0
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "N"

        targetFile$ = RTrim$(targetFile$)
        targetFile$ = Left$(targetFile$, Len(targetFile$) - 4)

        LG$ = targetFile$
        Open diskPaths$(1) + targetFile$ + ".TMS" For Input As #2
        Input #2, LG$
        Input #2, D1$
        Input #2, D2$
        For X = 1 To 30
            Input #2, expanZ1$(X)
        Next
        Close 2

        Call BUILDSTATS(targetFile$, expanZ1$())

        Do
            Cls

            70 Call TITLE
            'Erase menuChoices$
            ReDim menuChoices$(1 To 3)
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
            If Not (ExitCode% = 27) Then
                Select Case Selection%
                    Case 1: Call PlayerLeaders(printTarget)
                    Case 2: Call TeamTotals(printTarget)
                    Case 3: Cls
                End Select
            Else
                Selection% = 3
            End If

        Loop Until Selection% = 3

    End If

End Sub


'----------------------------------------
'       PlayerLeaders Subroutine
'----------------------------------------
'...explanation...
Sub PlayerLeaders (printTarget)

    Shared JB, PPP

    Shared AL$()
    Shared TMA$(), TMM$(), TP$(), TPP$()

    'Single not INT
    Shared statsAL!(), GMA!(), TYP!()
    Shared W0L!(), W1L!()

    Do

        80 Color , 0
        Cls
        Print
        Print
        Print
        Print
        J = 20
        ZZ = 1
        '
        Locate 2, 4: Color 15, 4
        Print " " + Chr$(214); String$(66, 196); Chr$(183) + " ";
        For II = 1 To J: Locate 2 + II, 4: Print " " + Chr$(186); String$(66, 32); Chr$(186) + " ";: Next
        Locate 3 + J, 4: Print " " + Chr$(211); String$(66, 196); Chr$(189) + " ";
        Locate 10, 15

        For I = 0 To 15
            Locate I + 3, 8
            Color 14: Print Using "##"; I;: Print " ";
            Color 0: Print TP$(I);
            Color 14: Print Tab(40); Using "##"; I + 16;
            Print " ";
            Color 0: Print TP$(I + 16)
        Next

        Print
        Locate , 23: Input "SELECT NUMBER TO VIEW"; B9

        Select Case B9
            Case 0: X = 1: stpl = 0: PPP = 0
            Case 1: X = 11: stpl = 0: PPP = 0
            Case 2: X = 3: stpl = 0: PPP = 0
            Case 3: X = 2: stpl = 0: PPP = 0
            Case 4: X = 3: X1 = 2: stpl = 0: PPP = 0
            Case 5: stpl = 0: PPP = 0
            Case 6: stpl = 0: PPP = 0
            Case 7: stpl = 0: PPP = 0
            Case 8: X = 5: stpl = 0: PPP = 0
            Case 9: X = 4: stpl = 0: PPP = 0
            Case 10: X = 5: X1 = 4: stpl = 0: PPP = 0
            Case 11: X = 3: X1 = 5: stpl = 0: PPP = 0
            Case 12: X = 3: X1 = 5: X2 = 1: stpl = 0: PPP = 0
            Case 13: X = 14: stpl = 0: PPP = 0
            Case 14: X = 13: stpl = 0: PPP = 0
            Case 15: X = 6: X1 = 13: stpl = 0: PPP = 0
            Case 16: X = 6: stpl = 0: PPP = 0
            Case 17: X = 6: X1 = 1: stpl = 0: PPP = 0
            Case 18: X = 7: stpl = 0: PPP = 0
            Case 19: X = 9: stpl = 0: PPP = 0
            Case 20: X = 10: stpl = 0: PPP = 0
            Case 21: X = 8: stpl = 0: PPP = 0
            Case 22: X = 12: stpl = 0: PPP = 0
            Case 23: X = 3: X1 = 5: X2 = 11: stpl = 0: PPP = 0
            Case 24: X = 6: X1 = 11: stpl = 0: PPP = 0
            Case 25: X = 7: X1 = 11: stpl = 0: PPP = 0
            Case 26: X = 9: X1 = 11: stpl = 0: PPP = 0
            Case 27: X = 10: X1 = 11: stpl = 0: PPP = 0
            Case 28: X = 8: X1 = 11: stpl = 0: PPP = 0
            Case 29: X = 12: X1 = 11: stpl = 0: PPP = 0
            Case 30: X = 11: X1 = 1: stpl = 0: PPP = 0
        End Select

        If B9 <> 31 Then Call SortOptions(printTarget, ZZ)

        Select Case B9
            Case 0, 2 TO 3, 8 TO 9, 13 TO 14, 16, 18 TO 22:
                Cls
                PPP = 0
                For I = 1 To JB
                    If statsAL!(I, X) <> 0 Then
                        PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
                        TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP!(PPP) = statsAL!(I, X)
                    End If
                Next I
                Call SortPlayer

                TVN = 1

            Case 1:
                Cls
                PPP = 0
                For I = 1 To JB
                    If statsAL!(I, X) <> 0 Then
                        PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
                        TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP!(PPP) = (statsAL!(I, X) + 30) / 60
                    End If
                Next I
                Call SortPlayer

                TVN = 1

            Case 4:
                Cls: PPP = 0
                For I = 1 To JB
                    If statsAL!(I, X) >= GMA!(I) * 3.65 Then
                        PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
                        TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP!(PPP) = statsAL!(I, X) / statsAL!(I, X1)
                    End If
                Next I
                Call SortPlayer

                TVN = 2

            Case 5:
                Cls: PPP = 0
                For I = 1 To JB
                    If W0L!(I) <> 0 Then
                        PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
                        TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP!(PPP) = W0L!(I)
                    End If
                Next I
                Call SortPlayer

                TVN = 1

            Case 6:
                Cls
                PPP = 0
                For I = 1 To JB
                    If W1L!(I) <> 0 Then
                        PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
                        TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP!(PPP) = W1L!(I)
                    End If
                Next I
                Call SortPlayer

                TVN = 1

            Case 7:
                Cls: PPP = 0
                For I = 1 To JB
                    If W0L!(I) >= GMA!(I) * .3 Then
                        PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
                        TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP!(PPP) = W0L!(I) / W1L!(I)
                    End If
                Next I
                Call SortPlayer

                TVN = 2

            Case 10:
                Cls
                PPP = 0
                For I = 1 To JB
                    If statsAL!(I, X) >= GMA!(I) * 1.52 Then
                        PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
                        TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP!(PPP) = statsAL!(I, X) / statsAL!(I, X1)
                    End If
                Next I
                Call SortPlayer

                TVN = 2

            Case 11:
                Cls
                PPP = 0
                For I = 1 To JB
                    If (statsAL!(I, X) * 2) + statsAL!(I, X1) + W0L!(I) <> 0 Then
                        PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
                        TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP!(PPP) = (statsAL!(I, X) * 2) + statsAL!(I, X1) + W0L!(I)
                        'TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP!(PPP) = 0
                    End If
                Next I
                Call SortPlayer

                TVN = 1

            Case 12:
                Cls: PPP = 0
                For I = 1 To JB
                    If (statsAL!(I, X) * 2) + statsAL!(I, X1) + W0L!(I) <> 0 And statsAL!(I, X2) <> 0 Then
                        PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
                        TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP!(PPP) = ((statsAL!(I, X) * 2) + statsAL!(I, X1) + W0L!(I)) / statsAL!(I, X2)
                    End If
                Next I
                Call SortPlayer

                TVN = 3

            Case 15:
                Cls: PPP = 0
                For I = 1 To JB
                    If statsAL!(I, X) - statsAL!(I, X1) <> 0 Then
                        PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
                        TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP!(PPP) = statsAL!(I, X) - statsAL!(I, X1)
                    End If
                Next I
                Call SortPlayer

                TVN = 1

            Case 17:
                Cls
                PPP = 0
                For I = 1 To JB
                    If statsAL!(I, X) <> 0 Then
                        PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
                        TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP!(PPP) = statsAL!(I, X) / statsAL!(I, X1)
                    End If
                Next I
                Call SortPlayer
                TVN = 3

            Case 23:
                Cls: PPP = 0
                For I = 1 To JB
                    If (statsAL!(I, X) * 2) + statsAL!(I, X1) + W0L!(I) <> 0 And statsAL!(I, X2) <> 0 Then
                        PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
                        TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP!(PPP) = (((statsAL!(I, X) * 2) + statsAL!(I, X1) + W0L!(I))) * 48 / ((statsAL!(I, X2) + 30) / 60)
                    End If
                Next I
                Call SortPlayer
                TVN = 3

            Case 24 TO 29:
                Cls: PPP = 0
                For I = 1 To JB
                    If statsAL!(I, X) <> 0 And statsAL!(I, X1) <> 0 Then
                        PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
                        TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP!(PPP) = (statsAL!(I, X)) * 48 / ((statsAL!(I, X1) + 30) / 60)
                    End If
                Next I
                Call SortPlayer
                TVN = 3

            Case 30:
                Cls: PPP = 0
                For I = 1 To JB
                    If statsAL!(I, X) <> 0 And statsAL!(I, X1) <> 0 Then
                        PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
                        TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP!(PPP) = ((statsAL!(I, X) + 30) / 60) / statsAL!(I, X1)
                    End If
                Next I
                Call SortPlayer
                TVN = 3

        End Select

        If B9 <> 31 Then
            QW = PPP
            Color , 0
            Cls

            If Not (printTarget = 1) Then
                Do
                    Print
                    Print
                    Print
                    Print
                    Print
                    J = 2

                    Locate 10, 20
                    Color 4, 15: Print " " + Chr$(214); String$(30, 196); Chr$(183) + " ";
                    For II = 1 To J
                        Locate 10 + II, 20
                        Print " " + Chr$(186); String$(30, 32); Chr$(186) + " ";
                    Next
                    Locate 11 + J, 20
                    Print " " + Chr$(211); String$(30, 196); Chr$(189) + " "
                    Locate 11

                    'Currently this is showing QW as 0
                    Locate , 25: Color 1: Print "MAXIMUM = "; QW
                    Locate , 25: Input "ENTER LENGTH OF LIST"; LG
                Loop Until LG <= QW
            End If

            Call PrintPlayer(printTarget, QW, TVN, B9)
        End If

    Loop Until B9 = 31 'Done selecting number

End Sub


'----------------------------------------
'       TeamTotals Subroutine
'----------------------------------------
'...explanation...
Sub TeamTotals (printTarget)

    Dim GM!(1 To 40)

    Shared JX, TS, TTS

    Shared expanTT$(), TMM$(), TP1$(), TT1$()
    Shared GM1!(), TT!(), TYP!(), TT1!()

    'JX = 1: Sort HIGH to LOW
    'JX = 2: Sort LOW to HIGH

    Do

        Color , 0
        Cls
        Print
        Print
        Print
        Print
        J = 21
        ZZ = 3
        '
        Locate 1, 4: Color 15, 6: Print " " + Chr$(214); String$(66, 196); Chr$(183) + " ";
        For II = 1 To J: Locate 1 + II, 4: Print " " + Chr$(186); String$(66, 32); Chr$(186) + " ";: Next
        Locate 2 + J, 4: Print " " + Chr$(211); String$(66, 196); Chr$(189) + " ";: Locate 10, 15

        For I = 0 To 19: Locate I + 2, 8: Color 11: Print Using "##"; I;: Print " ";: Color 0: Print TP1$(I): Next
        For I = 20 To 38: Locate I - 18, 38: Color 11: Print Using "##"; I;: Print " ";: Color 0: Print TP1$(I): Next

        3050 Color 14: Locate 22, 23: Input "SELECT NUMBER TO VIEW"; B9

        Select Case B9
            Case 0: X = 1: stpl = 0
            Case 1: X = 2: stpl = 0
            Case 2: X = 2: X1 = 1: stpl = 0
            Case 3: X = 14: stpl = 0
            Case 4: X = 13: stpl = 0
            Case 5: X = 13: X1 = 14: stpl = 0
            Case 6: X = 3: stpl = 0
            Case 7: X = 4: stpl = 0
            Case 8: X = 4: X1 = 3: stpl = 0
            Case 9: X = 12: stpl = 0
            Case 10: X = 5: X1 = 12: stpl = 0
            Case 11: X = 5: stpl = 0
            Case 12: X = 5: stpl = 0
            Case 13: X = 6: stpl = 0
            Case 14: X = 8: stpl = 0
            Case 15: X = 9: stpl = 0
            Case 16: X = 7: stpl = 0
            Case 17: X = 11: stpl = 0
            Case 18: X = 2: X1 = 13: X2 = 4: stpl = 0
            Case 19: X = 1: stpl = 0
            Case 20: X = 2: stpl = 0
            Case 21: X = 2: X1 = 1: stpl = 0
            Case 22: X = 14: stpl = 0
            Case 23: X = 13: stpl = 0
            Case 24: X = 13: X1 = 14: stpl = 0
            Case 25: X = 3: stpl = 0
            Case 26: X = 4: stpl = 0
            Case 27: X = 4: X1 = 3: stpl = 0
            Case 28: X = 12: stpl = 0
            Case 29: X = 5: X1 = 12: stpl = 0
            Case 30: X = 5: stpl = 0
            Case 31: X = 5: stpl = 0
            Case 32: X = 6: stpl = 0
            Case 33: X = 8: stpl = 0
            Case 34: X = 9: stpl = 0
            Case 35: X = 7: stpl = 0
            Case 36: X = 11: stpl = 0
            Case 37: X = 2: X1 = 13: X2 = 4: stpl = 0
        End Select

        If B9 <> 38 Then
            Call SortOptions(printTarget, ZZ)
            Cls
        End If

        Select Case B9
            Case 0 TO 1, 3 TO 4, 6 TO 7, 9, 11, 13 TO 17:
                For I = 1 To TS
					TMM$(I) = expanTT$(I, X)
					TYP!(I) = TT!(I, X)
				Next
				Call SortTeam (TS)
                TVN = 1

            Case 2, 5, 8:
                For I = 1 To TS
                    If TT!(I, X1) = 0 Then
                        TMM$(I) = expanTT$(I, X): TYP!(I) = 0
                    Else
                        TMM$(I) = expanTT$(I, X): TYP!(I) = TT!(I, X) / TT!(I, X1)
                    End If
                Next
				Call SortTeam (TS)
                TVN = 2

            Case 10:
                For I = 1 To TS
					TMM$(I) = expanTT$(I, X)
					TYP!(I) = TT!(I, X) - TT!(I, X1)
				Next
				Call SortTeam (TS)
                TVN = 1

            Case 12:
                For I = 1 To TS
					TMM$(I) = expanTT$(I, X)
					TYP!(I) = TT!(I, X) / GM!(I)
				Next
				Call SortTeam (TS)
                TVN = 3

            Case 18:
                For I = 1 To TS
					TMM$(I) = expanTT$(I, X)
					TYP!(I) = ((TT!(I, X) * 2) + TT!(I, X1) + TT!(I, X2)) / GM!(I)
				Next
				Call SortTeam (TS)
                TVN = 3

            Case 19 TO 20, 22 TO 23, 25 TO 26, 28, 30:
                For I = 1 To TTS
					TMM$(I) = TT1$(I, X)
					TYP!(I) = TT1!(I, X)
				Next
				Call SortTeam (TTS)
                TVN = 1

            Case 21, 24, 27:
                For I = 1 To TTS
                    If TT1!(I, X1) = 0 Then
                        TMM$(I) = TT1$(I, X): TYP!(I) = 0
                    Else
                        TMM$(I) = TT1$(I, X): TYP!(I) = TT1!(I, X) / TT1!(I, X1)
                    End If
                Next
				Call SortTeam (TTS)
                TVN = 2

            Case 29:
                For I = 1 To TTS
					TMM$(I) = TT1$(I, X)
					TYP!(I) = TT1!(I, X) - TT1!(I, X1)
				Next
				Call SortTeam (TTS)
                TVN = 1

            Case 31 TO 36:
                For I = 1 To TTS
                    TMM$(I) = TT1$(I, X)
                    TYP!(I) = TT1!(I, X) / GM1!(I)
                Next
				Call SortTeam (TTS)
                TVN = 3

            Case 37:
                For I = 1 To TTS
                    TMM$(I) = TT1$(I, X)
                    TYP!(I) = ((TT1!(I, X) * 2) + TT1!(I, X1) + TT1!(I, X2)) / GM1!(I)
                Next
				Call SortTeam (TTS)
                TVN = 3

        End Select

        If B9 <> 38 Then
            QW = TTS
            Call PrintIt(printTarget, QW, TVN, B9)
        End If

    Loop Until B9 = 38


End Sub


'----------------------------------------
'         PrintPlayer Subroutine
'----------------------------------------
'...explanation...
Sub PrintPlayer (printTarget, maxRecords, TVN, categoryID)

    Shared TP$(), TMM$(), TPP$()
    Shared TYP!()

    Select Case printTarget
        Case 1:
            Do
                Color 14, 0
                Cls
                Print "     PLAYER                  TEAM                     "; TP$(categoryID)
                For W = 1 To maxRecords
                    '--** If a multiple of 20 we do the same ething no matter what

                    If W = 20 Or W = 40 Or W = 60 Or W = 80 Or W = 100 Or W = 120 Or W = 140 Or W = 160 Or W = 180 Or W = 200 Then
                        Locate 22
                        Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
                        I$ = GetKeyPress$
                        If I$ = Chr$(27) Then Exit Sub
                        Cls
                        Color 14: Print "     PLAYER                  TEAM                     "; TP$(categoryID)
                    End If

                    If W = 220 Or W = 240 Or W = 260 Or W = 280 Or W = 300 Or W = 320 Or W = 340 Or W = 360 Or W = 380 Or W = 400 Then
                        Locate 22
                        Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
                        I$ = GetKeyPress$
                        If I$ = Chr$(27) Then Exit Sub
                        Cls
                        Color 14: Print "     PLAYER                  TEAM                     "; TP$(categoryID)
                    End If

                    If W = 420 Or W = 440 Or W = 460 Or W = 480 Or W = 500 Or W = 520 Or W = 540 Or W = 560 Or W = 580 Or W = 600 Then
                        Locate 22
                        Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
                        I$ = GetKeyPress$
                        If I$ = Chr$(27) Then Exit Sub
                        Cls
                        Color 14: Print "     PLAYER                  TEAM                     "; TP$(categoryID)
                    End If

                    If W = 620 Or W = 640 Or W = 660 Then
                        Locate 22
                        Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
                        I$ = GetKeyPress$
                        If I$ = Chr$(27) Then Exit Sub
                        Cls
                        Color 14: Print "     PLAYER                  TEAM                     "; TP$(categoryID)
                    End If

                    '--** we can replace this really easy by checking W against a 0 remainder when we do mod 20

                    If TVN = 1 Then Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TPP$(W);: Color 15: Print Tab(25); TMM$(W);: Color 15: Print Tab(58); Using "#####"; TYP!(W)
                    If TVN = 2 Then Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TPP$(W);: Color 15: Print Tab(25); TMM$(W);: Color 15: Print Tab(58); Using ".###"; TYP!(W)
                    If TVN = 3 Then Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TPP$(W);: Color 15: Print Tab(25); TMM$(W);: Color 15: Print Tab(58); Using "###.#"; TYP!(W)

                Next W

                Locate 22
                Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"

                I$ = GetKeyPress$
                If I$ <> Chr$(27) Then
                    Locate 22: Print "                                                                "
                    Locate 22: Color 14: Print "VIEW AGAIN (Y/N)"
                    I$ = GetKeyPress$
                End If

            Loop Until UCase$(I$) = "N" Or I$ = Chr$(27)

        Case 2:

            LPrint Chr$(15)
            'This is not supported by QB64
            'WIDTH "LPT1:1", 130
            Color 14, 0:
            Cls
            LPrint "     PLAYER                  TEAM                     "; TP$(categoryID)
            For W = 1 To LG
                '--** If a multiple of 50, we're doing the same thing no matter what

                If W = 50 Or W = 100 Or W = 150 Or W = 200 Then
                    LPrint Chr$(12)
                    LPrint "     PLAYER                  TEAM                     "; TP$(categoryID)
                End If
                If W = 250 Or W = 300 Or W = 350 Or W = 400 Then
                    LPrint Chr$(12)
                    LPrint "     PLAYER                  TEAM                     "; TP$(categoryID)
                End If

                If W = 450 Or W = 500 Or W = 550 Or W = 600 Then
                    LPrint Chr$(12)
                    LPrint "     PLAYER                  TEAM                     "; TP$(categoryID)
                End If

                If W = 650 Or W = 700 Or W = 750 Or W = 800 Then
                    LPrint Chr$(12)
                    LPrint "     PLAYER                  TEAM                     "; TP$(categoryID)
                End If

                If W = 850 Or W = 900 Then
                    LPrint Chr$(12)
                    LPrint "     PLAYER                  TEAM                     "; TP$(categoryID)
                End If

                If TVN = 1 Then LPrint Using "###"; W;: LPrint " ";: LPrint TPP$(W);: LPrint Tab(25); TMM$(W);: LPrint Tab(58); Using "#####"; TYP!(W)
                If TVN = 2 Then LPrint Using "###"; W;: LPrint " ";: LPrint TPP$(W);: LPrint Tab(25); TMM$(W);: LPrint Tab(58); Using ".###"; TYP!(W)
                If TVN = 3 Then LPrint Using "###"; W;: LPrint " ";: LPrint TPP$(W);: LPrint Tab(25); TMM$(W);: LPrint Tab(58); Using "###.#"; TYP!(W)

            Next W

            LPrint Chr$(18)
            'This is not supported in QB64
            'WIDTH "LPT1:1", 80

    End Select

End Sub


'----------------------------------------
'           PrintIt Subroutine
'----------------------------------------
'...explanation...
Sub PrintIt (printTarget, maxRecords, TVN, categoryID)

    Shared TMM$(), TP1$()
    Shared TYP!()

    Select Case printTarget

        Case 1:
            Do
                Color 14, 0
                Cls
                Print "     TEAM                     "; TP1$(categoryID)
                For W = 1 To maxRecords
                    If W = 20 Or W = 40 Then
                        Locate 22: Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
                        I$ = GetKeyPress$
                        If I$ = Chr$(27) Then Exit Sub
                        'How do we break both loops???
                        Cls
                        Color 14: Print "     TEAM                     "; TP1$(categoryID)
                    End If
                    If TVN = 1 Then Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TMM$(W);: Color 15: Print Tab(32); Using "#####"; TYP!(W)
                    If TVN = 2 Then Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TMM$(W);: Color 15: Print Tab(33); Using ".###"; TYP!(W)
                    If TVN = 3 Then Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TMM$(W);: Color 15: Print Tab(33); Using "###.#"; TYP!(W)
                Next W

                Locate 22: Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"

                I$ = GetKeyPress$
                If I$ <> Chr$(27) Then
                    Locate 22: Print "                                                                "
                    Locate 22: Color 14: Print "VIEW AGAIN (Y/N)"
                    I$ = GetKeyPress$
                Else
                    I$ = "N"
                End If

            Loop Until UCase$(I$) = "N"

        Case 2:
            LPrint Chr$(15)
            'WIDTH "LPT1:1", 130
            LPrint "     TEAM                     "; TP1$(categoryID)
            '-- Should these be LPRINT ???
            For W = 1 To maxRecords
                If TVN = 1 Then Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TMM$(W);: Color 15: Print Tab(32); Using "#####"; TYP!(W)
                If TVN = 2 Then Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TMM$(W);: Color 15: Print Tab(33); Using ".###"; TYP!(W)
                If TVN = 3 Then Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TMM$(W);: Color 15: Print Tab(33); Using "###.#"; TYP!(W)
            Next W
            LPrint Chr$(12)
            LPrint Chr$(18)

            'This is not supported in QB64
            'WIDTH "LPT1:1", 80
            'GoTo 70

    End Select

End Sub


'----------------------------------------
'       SortPlayer Subroutine
'----------------------------------------
'...explanation...
Sub SortPlayer

    Shared JX, PPP
    Shared TMM$(), TPP$()
    Shared TYP!()

    For I = 2 To PPP
        stpl = stpl + 1
        Locate 5, 5: Print "sorting player "; stpl
        For J = PPP To I Step -1
            If JX = 1 Then If TYP!(J - 1) < TYP!(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP!(J - 1), TYP!(J)
            If JX = 2 Then If TYP!(J - 1) > TYP!(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP!(J - 1), TYP!(J)
        Next J
    Next I

End Sub


'----------------------------------------
'       SortOptions Subroutine
'----------------------------------------
'...explanation...
Sub SortOptions (printTarget, ZZ)

    Shared JX

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
                Print: Print: Print: Print: Print
                J = 4

                Locate 9, 15: Color 15, 0: Print " " + Chr$(214); String$(40, 196); Chr$(183) + " ";
                For II = 1 To J: Locate 9 + II, 15: Print " " + Chr$(186); String$(40, 32); Chr$(186) + " ";: Next
                Locate 10 + J, 15: Print " " + Chr$(211); String$(40, 196); Chr$(189) + " ": Locate 10, 10

                Do
                    Color 12: Locate , 30: Print "SELECT OPTION:"
                    Locate , 30: Color 14: Print "P";: Color 7: Print "RINT"
                    Locate , 30: Color 14: Print "V";: Color 7: Print "IEW"
                    Locate , 30: Color 14: Print "R";: Color 7: Print "E-SELECT OPTION"
                    J$ = GetKeyPress$
                Loop Until UCase$(J$) = "P" Or UCase$(J$) = "V" Or UCase$(J$) = "R"

                Select Case UCase$(J$)
                    Case "P": printTarget = 2
                    Case "V": printTarget = 1
                    Case "R":
                        If ZZ = 1 Then Call PlayerLeaders(printTarget)
                        If ZZ = 3 Then Call TeamTotals(printTarget)
                End Select

                JX = 1

            Case "L"
                Print: Print: Print: Print: Print
                J = 4

                Locate 9, 15: Color 15, 0: Print " " + Chr$(214); String$(40, 196); Chr$(183) + " ";
                For II = 1 To J: Locate 9 + II, 15: Print " " + Chr$(186); String$(40, 32); Chr$(186) + " ";: Next
                Locate 10 + J, 15: Print " " + Chr$(211); String$(40, 196); Chr$(189) + " ": Locate 10, 10

                Do
                    Color 12: Locate , 30: Print "SELECT OPTION:"
                    Locate , 30: Color 14: Print "P";: Color 7: Print "RINT"
                    Locate , 30: Color 14: Print "V";: Color 7: Print "IEW"
                    Locate , 30: Color 14: Print "R";: Color 7: Print "E-SELECT OPTION"
                    I2$ = GetKeyPress$
                Loop Until I2$ = "P" Or I2$ = "V" Or I2$ = "R"

                Select Case I2$
                    Case "P": printTarget = 2
                    Case "V": printTarget = 1
                    Case "R":
                        'I$ = "R" will also put us into the next "R" statement
                        If ZZ = 1 Then Call PlayerLeaders(printTarget)
                        If ZZ = 3 Then TeamTotals (printTarget)
                End Select

                JX = 2

            Case "R":
                If ZZ = 1 Then Call PlayerLeaders(printTarget)
                If ZZ = 3 Then TeamTotals (printTarget)

        End Select

    Loop Until UCase$(I$) = "H" Or UCase$(I$) = "L" Or UCase$(I$) = "R"

End Sub


'------------------------------
'      BUILDSTATS Subroutine
'------------------------------
' ...explanation...
Sub BUILDSTATS (statFile$, Z1$())

    Dim GM!(1 To 40)

    Shared A$(), AL$(), expanTT$()
    Shared TMA$(), TT1$()

    Shared A(), statsAL!()
    Shared GMA!(), GM1!()
    Shared TT!(), TT1!()
    Shared W0(), W1(), W0L!(), W1L!()
    Shared Z!(), Z1!()

    Shared JB, TS, TMR, TTS

    Cls

    For KK = 1 To 30
        YY = YY + 1
        If Z1$(KK) <> "*" And Z1$(KK) <> "XXX" And Z1$(KK) <> "" Then

            Print "CONVERTING STAT FILE OF "; Z1$(KK)

            If _FileExists(diskPaths$(1) + Z1$(KK)) Then

                Open diskPaths$(1) + Z1$(KK) For Input As #1
                Input #1, B$
                For I = 0 To 13
                    Input #1, A$(I)
                    For J = 0 To 13: Input #1, A(I, J): Next
                    Input #1, W0(I)
                    Input #1, W1(I)
                Next
                For L = 0 To 15: Input #1, Z!(L): Next
                For L = 0 To 15: Input #1, Z1!(L): Next
                Close #1

                Open diskPaths$(1) + Z1$(KK) + "S" For Input As #1
                Input #1, year
                Close #1

                TS = TS + 1: For I = 0 To 14: TT!(TS, I) = Z!(I): expanTT$(TS, I) = B$: GM!(TS) = year: Next
                TTS = TTS + 1: For I = 0 To 14: TT1!(TTS, I) = Z1!(I): TT1$(TTS, I) = B$: GM1!(TTS) = year: Next

                For I = 0 To 14
                    If A$(I) <> "XXX" Then
                        JB = JB + 1
                        AL$(JB) = A$(I)
                        TBN = TBN + 1
                        TMA$(TBN) = B$
                        GMA!(JB) = year
                        K1 = 0
                        For I1 = 0 To 13
                            K1 = K1 + 1
                            statsAL!(JB, K1) = A(I, I1)
                        Next I1
                    End If
                    W0L!(JB) = W0(I)
                    W1L!(JB) = W1(I)
                Next I

            End If

        End If
    Next KK

    Print "SAVING LEADER FILES . . . . PLEASE WAIT"
    Open diskPaths$(1) + statFile$ + ".LDS" For Output As #1
    Write #1, JB, TS, TTS
    For I = 1 To 600:
        Write #1, AL$(I), TMA$(I), GMA!(I)
        Color 14: TMR = TMR - 1
        Locate 18, 45: Print Using "#####"; TMR
        For I1 = 1 To 14:
            Write #1, statsAL!(I, I1)
        Next I1

        Write #1, W0L!(I)
        Write #1, W1L!(I)
        Color 14
        TMR = TMR - 1
        Locate 18, 45: Print Using "#####"; TMR
    Next I

    For I = 1 To 40
        For I1 = 0 To 14
            Write #1, TT!(I, I1), expanTT$(I, I1)
        Next
        Color 14
        TMR = TMR - 1
        Locate 18, 45: Print Using "#####"; TMR
        Write #1, GM!(I)
    Next
    For I = 1 To 40
        For I1 = 0 To 14
            Write #1, TT1!(I, I1), TT1$(I, I1)
        Next
        Color 14
        TMR = TMR - 1
        Locate 18, 45
        Print Using "#####"; TMR
        Write #1, GM1!(I)
    Next

    Close #1
End Sub


'----------------------------------------
'       SortTeam Subroutine
'----------------------------------------
'...explanation...
Sub SortTeam (sortMax)

    Shared JX
    Shared TMM$()
    Shared TYP!()

	For I = 2 To sortMax
		stpl = stpl + 1
		Locate 5, 5: Print "sorting team "; stpl
		For J = sortMax To I Step -1
			If JX = 1 Then If TYP!(J - 1) < TYP!(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP!(J - 1), TYP!(J)
			If JX = 2 Then If TYP!(J - 1) > TYP!(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP!(J - 1), TYP!(J)
		Next J
	Next I

End Sub
