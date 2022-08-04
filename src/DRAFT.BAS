Sub DRAFT ()

    Shared DA(), draftA(), draftX(), SP()
    Shared V1%(), V2%()
    Shared draftL%(), draftT%()
    Shared draftA$(), draftT$()

    _Title "Courtside College Basketball - Team Draft"

    '-----
    ReDim menuChoices$(4)

    For I = 0 To 1
        draftT$(I) = "XXX"
        SP(I) = 0
        DA(I) = 0
        V1%(I) = 0
        V2%(I) = 0
        For I1 = 0 To 4: draftL%(I, I1) = 0: Next
        For I1 = 0 To 9: draftT%(I, I1) = 0: Next
        For I1 = 0 To 13
            draftA$(I, I1) = "XXX"
            For I2 = 0 To 6
                draftA(I, I1, I2) = 0
                draftX(I, I1, I2) = 0
            Next
        Next
    Next


    Do

        Cls

        Call TITLE
        menuChoices$(1) = "DRAFT A NEW TEAM         "
        menuChoices$(2) = "DRAFT TO AN EXISTING TEAM"
        menuChoices$(3) = "EXIT TO MAIN MENU        "
        Selection% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 8
        Column% = 25

        Call box0(7, 24, 11, 50, 2, 15, 1)
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then
            Select Case Selection%
                Case 1:
                    DNT% = 0
                    Call DraftNewTeam(DNT%)
					'Clear player list; alternatively, don't make this so shared
					Erase draftA$
                Case 2:
                    DNT% = 1
                    Call DraftExistingTeam(DNT%)
					'Clear player list; alternatively, don't make this so shared
					Erase draftA$
                Case 3:
                    Cls
            End Select
        End If

    Loop Until Selection% = 3 Or ExitCode% = 27

    '1 Exit Sub
End Sub


'----------------------------------------
'       DraftNewTeam Subroutine
'----------------------------------------
'...explanation...
Sub DraftNewTeam (draftNew%)

    Dim T1N(1)

    Shared draftT%()
    Shared draftT$(), draftYN$()

    Do
        Cls
        Color 14: Print "TEAM INFORMATION": X% = 2: Color 15

        147 Locate 2: Input " 1. TEAM NUMBER "; T1N(0): If C1 = 1 Then 190

        162 Locate 3: Input " 2. DISK ID "; draftYN$(0): If C1 = 1 Then 190

        163 Locate 4: Input " 3. TEAM NAME "; draftT$(0): If C1 = 1 Then 190

        157 Locate 5: Input " 4. TEXT COLOR (0-15) "; draftT%(0, 0): If C1 = 1 Then 190

        158 Locate 6: Input " 5. BACKGROUND (0-7) "; draftT%(0, 1): If C1 = 1 Then 190

        164 Locate 7: Input " 6. HOMECOURT FACTOR (0-6) "; draftT%(0, 6): If C1 = 1 Then 190

        190 Locate 9, 1: Color draftT%(0, 0), draftT%(0, 1)

        Print "TEAM APPEARANCE": Color 15, 0
        C1 = 1
        Locate 11, 1: Print "SELECT # TO CHANGE 1-6 (0 TO CONTINUE) ";
        Input B9

        If (B9 >= 1 And B9 <= 6) Then
            Select Case B9
                Case 1:
                    GoTo 147
                Case 2:
                    GoTo 162
                Case 3:
                    GoTo 163
                Case 4:
                    GoTo 157
                Case 5:
                    GoTo 158
                Case 6:
                    GoTo 164
            End Select
        End If

    Loop Until B9 = 0

    '0 = Continue
    If draftT%(0, 6) < 0 Then draftT%(0, 6) = 0
    If draftT%(0, 6) > 6 Then draftT%(0, 6) = 6
    C1 = 0

    Call DoTheDraft(draftNew%, teamIdx%)

End Sub


'----------------------------------------
'     DraftExistingTeam Subroutine
'----------------------------------------
'...explanation...
Sub DraftExistingTeam (draftNew%)

    Shared draftT$(), draftYN$()
    Shared draftT%()
    Do
        Color 15, 0: Cls
        JJ = 7
        Locate 6, 8: Color 15, 4

        'Print box top
        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

        'Print box sides
        For II = 1 To JJ
            Locate 6 + II, 8
            Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
        Next

        'Print box bottom
        Locate 7 + JJ, 8
        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

        Locate 7, 32
        Locate , 12: Print "ENTER DISK ID: ": Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Locate 7, 27: Input NY$
        If NY$ = "" Then
            Call SelectTeam_DRAFT(NTMS)
        Else
            draftYN$(0) = NY$
            Erase teamNames$
            Call MoreTeam(Index%)
        End If

        If BO% = 1 Then
            BO% = 0
        Else
            Locate 2, 33: Color draftT%(0, 0), draftT%(0, 1): Print draftT$(0): Color 15, 4
            Locate , 33: Print "ANY CHANGE (YN)";
            I$ = GetKeyPress$
        End If

    Loop Until UCase$(I$) = "N"

    Call DoTheDraft(draftNew%, Index%)

End Sub



'----------------------------------------
'     DoTheDraft Subroutine
'----------------------------------------
'...explanation...
Sub DoTheDraft (draftNew%, teamPosition%)

    Shared draftA$(), draftP$(), draftT$(), draftYN$()
    Shared A1$(), C$(), S$()

    Shared draftA(), ATT(), DA(), draftDF()
    Shared draftRS(), draftX(), SP()

    Shared draftL%(), draftT%(), draftT1%()
    Shared TR%(), V1%(), V2%()

    Do

        450 Color 15, 0
        Cls
        Color draftT%(0, 0), draftT%(0, 1): Print draftT$(0);
        Color 14, 0: Print "     (DRAFTING)"

        451 Print
        Print "PLAYERS"

        452 For I = 0 To 13
            Color 2: Print Using "## "; I;: Color 15: Print draftA$(0, I)
        Next

        454 Locate 21, 1
        Color 14: Print "(1) ";: Color 15: Print "DRAFT PLAYER"
        Color 14: Print "(2) ";: Color 15: Print "SAVE TEAM"

        I$ = GetKeyPress$

        Select Case I$

            Case "1"
                Do
                    Locate 21, 20: Print "SLOT # (0-13) ";
                    N = 1
                    Input B
                Loop Until B >= 0 And B <= 14

                Do
                    1440 Color 15, 0: Cls
                    JJ = 7
                    Locate 6, 8: Color 15, 4
                    'Print box top
                    Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

                    'Print box sides
                    For II = 1 To JJ
                        Locate 6 + II, 8
                        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
                    Next

                    'Print box bottom
                    Locate 7 + JJ, 8
                    Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

                    Locate 7, 32
                    Locate , 12: Print "ENTER DISK ID: ": Print
                    Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
                    Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
                    Locate 7, 27: Input NY$

                    If NY$ = "" Then
                        Call SelectTeam_DRAFT(NTMS)
                    Else
                        draftYN$(1) = NY$
                        Erase teamNames$
                        Cls
                        Open diskPaths$(0) + "BASK." + draftYN$(1) For Random As #1 Len = 655
                        Y = LOF(1)
                        Field #1, 15 As Q$(0), 640 As X$
                        For I1 = 1 To Y / 655
                            Get #1, I1
                            teamNames$(I1) = Q$(0)
                            teamIndex%(I1) = I1
                        Next
                        Close 1
                        NTMS = Y / 655

                        Call SelectTeam_DRAFT(NTMS)
                    End If

                    182 If BO% = 1 Then
                        BO% = 0
                    Else
                        Locate 2, 33: Color draftT%(1, 0), draftT%(1, 1): Print draftT$(1): Color 15, 4
                        Locate , 33: Print "ANY CHANGE (YN)";
                        I$ = GetKeyPress$
                    End If

                Loop Until UCase$(I$) = "N"

                Color , 0
                Cls
                Color draftT%(1, 0), draftT%(1, 1): Print draftT$(1): Color 14, 0

                Print: Print "PLAYERS"

                For I = 0 To 13
                    Color 2: Print Using "## "; I;
                    Color 15: Print draftA$(1, I)
                Next

                Do
                    Color 14: Locate 21, 1: Print "SELECT PLAYER # TO DRAFT ";
                    Input B1
                Loop Until B1 >= 0 And B1 <= 13

                draftA$(0, B) = draftA$(1, B1)
                draftDF(0, B) = draftDF(1, B1)
                draftP$(0, B) = draftP$(1, B1)
                For I = 0 To 13
                    draftRS(0, B, I) = draftRS(1, B1, I)
                Next

                For I = 1 To 12
                    TR%(0, B, I) = TR%(1, B1, I)
                Next

                For I = 0 To 6
                    draftA(0, B, I) = draftA(1, B1, I)
                    draftX(0, B, I) = draftX(1, B1, I)
                Next

            Case "2":
                Do
                    Cls
                    Color draftT%(0, 0), draftT%(0, 1): Print A$: Color 14, 0
                    Print "PLAYER          CONTRIBUTION"
                    Print "                PERCENT"
                    Color 15, 0
                    T3 = 0
                    For I = 0 To 13
                        Print Using "## "; I;: Print draftA$(0, I);
                        Locate , 17: Print draftA(0, I, 3): Locate , 1
                        T3 = T3 + draftA(0, I, 3)
                    Next I
                    Print
                    Color 14: Print "TOTAL CP ";
                    Locate , 17
                    Color 15, 0: Print T3

                    Do
                        Locate 21: Print "SELECT PLAYER TO CHANGE 0-13 (22-NO MORE CHANGE) ";
                        Input X9

                        If X9 = 22 Then

                            If T3 <= 98 Or T3 >= 101 Then
                                Cls
                                JJ = 4
                                Locate 6, 8: Color 15, 1
                                Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";
                                For II = 1 To JJ: Locate 6 + II, 8
                                    Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
                                Next
                                Locate 7 + JJ, 8
                                Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";: Locate 7
                                U$ = "TOTAL CONTRIBUTION MUST BE 100 %"
                                Locate , (80 - Len(U$)) / 2
                                Print U$
                                U$ = "HIT ANY KEY TO CONTINUE"
                                Locate , (80 - Len(U$)) / 2
                                Print U$
                                Color 15, 0
                                I$ = GetKeyPress$
                            End If 'End checking total

                        Else

                            Locate 22, 18: Print draftA$(0, X9): Locate , 1
                            Locate 23, 1
                            Input "ENTER CP: "; draftA(0, X9, 3)

                        End If 'End checking X9

                    Loop Until (X9 >= 0 And X9 <= 13) Or X9 = 22

                Loop Until X9 = 22 And (T3 > 98 And T3 < 101)

        End Select

    Loop Until I$ = "1" Or I$ = "2"

    Cls

    NP = 0
    For I = 0 To 13
        If draftA$(0, I) <> "XXX" Then
            NP = NP + 1
            SP(0) = SP(0) + TR%(0, I, 1)
            DA(0) = DA(0) + TR%(0, I, 2)
            V2%(0) = V2%(0) + TR%(0, I, 3)
            V1%(0) = V1%(0) + TR%(0, I, 4)
            draftL%(0, 0) = draftL%(0, 0) + TR%(0, I, 5)
            draftL%(0, 2) = draftL%(0, 2) + TR%(0, I, 6)
            draftL%(0, 3) = draftL%(0, 3) + TR%(0, I, 7)
            draftT%(0, 3) = draftT%(0, 3) + TR%(0, I, 9)
            draftT%(0, 4) = draftT%(0, 4) + TR%(0, I, 10)
            draftT%(0, 5) = draftT%(0, 5) + TR%(0, I, 11)
            draftT%(0, 6) = draftT%(0, 6) + TR%(0, I, 12)
        End If
    Next

    SP(0) = SP(0) / (NP - 1)
    DA(0) = DA(0) / (NP - 1)
    V2%(0) = V2%(0) / (NP - 1)
    If V2%(0) > 20 Then V2%(0) = 100 - V2%(0)
    V1%(0) = V1%(0) / (NP - 1)
    If V1%(0) > 20 Then V1%(0) = 100 - V1%(0)
    draftL%(0, 0) = draftL%(0, 0) / (NP - 1)
    If draftL%(0, 0) > 20 Then draftL%(0, 0) = 100 - draftL%(0, 0)
    draftL%(0, 2) = draftL%(0, 2) / (NP - 1)
    If draftL%(0, 2) > 20 Then draftL%(0, 2) = 100 - draftL%(0, 2)
    draftL%(0, 3) = draftL%(0, 3) / (NP - 1)
    If draftL%(0, 3) > 20 Then draftL%(0, 3) = 100 - draftL%(0, 3)
    draftL%(0, 4) = 99
    draftT%(0, 3) = draftT%(0, 3) / (NP - 1)
    draftT%(0, 4) = draftT%(0, 4) / (NP - 1)
    draftT%(0, 5) = draftT%(0, 5) / (NP - 1)
    draftT%(0, 6) = draftT%(0, 6) / (NP - 1)

    Open diskPaths$(0) + "BASK." + draftYN$(0) For Random As #1 Len = 655
    Y = LOF(1)

    If draftNew% = 0 Then teamPosition% = Y / 655 + 1

    Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 2 As Q$(14), 2 As Q$(15), 2 As Q$(16), 2 As Q$(17), 2 As Q$(18), 2 As Q$(19), 602 As X$
    For I = 0 To 13
        Field #1, 53 + I * 43 As X$, 15 As Q$(I * 15 + 20), 2 As Q$(I * 15 + 21), 2 As Q$(I * 15 + 22), 2 As Q$(I * 15 + 23), 2 As Q$(I * 15 + 24), 2 As Q$(I * 15 + 25), 2 As Q$(I * 15 + 26), 2 As Q$(I * 15 + 27), 2 As Q$(I * 15 + 28), 2 As Q$(I * 15 + 29), 2 As Q$(I * 15 + 30), 2 As Q$(I * 15 + 31), 2 As Q$(I * 15 + 32), 2 As Q$(I * 15 + 33), 2 As Q$(I * 15 + 34), 655 - 53 - 43 - I * 43 As X$
    Next

    LSet Q$(0) = draftT$(0): LSet Q$(1) = MKI$(SP(0)): LSet Q$(2) = MKI$(DA(0)): LSet Q$(3) = MKI$(V1%(0))
    LSet Q$(4) = MKI$(V2%(0)): For I = 0 To 4: LSet Q$(I + 5) = MKI$(draftL%(0, I)): Next
    draftT%(0, 2) = 999

    For I = 0 To 9: LSet Q$(I + 10) = MKI$(draftT%(0, I)): Next
    For I = 0 To 13
        LSet Q$(I * 15 + 20) = draftA$(0, I)
        For I1 = 0 To 6
            LSet Q$(I * 15 + I1 + 21) = MKI$(draftA(0, I, I1))
            LSet Q$(I * 15 + I1 + 28) = MKI$(draftX(0, I, I1))
        Next
    Next
    Put #1, teamPosition%: Close 1


    Open diskPaths$(0) + "COLBBTMS." + draftYN$(0) For Random As #1 Len = 566
    Field #1, 15 As Q$(0), 40 As Q$(1), 15 As Q$(2), 4 As Q$(3)
    For I = 0 To 13
        Field #1, (74 + I * 32) As X$, 2 As Q$(I * 16 + 4), 2 As Q$(I * 16 + 5), 2 As Q$(I * 16 + 6), 2 As Q$(I * 16 + 7), 2 As Q$(I * 16 + 8), 2 As Q$(I * 16 + 9), 2 As Q$(I * 16 + 10), 2 As Q$(I * 16 + 11), 2 As Q$(I * 16 + 12), 2 As Q$(I * 16 + 13), 2 As Q$(I * 16 + 14), 2 As Q$(I * 16 + 15), 2 As Q$(I * 16 + 16), 2 As Q$(I * 16 + 17), 2 As Q$(I * 16 + 18), 2 As Q$(I * 16 + 19)
        Field #1, 522 As X$, 2 As Q$(228), 2 As Q$(229), 2 As Q$(230), 2 As Q$(231), 2 As Q$(232), 2 As Q$(233), 2 As Q$(234), 2 As Q$(235), 2 As Q$(236), 2 As Q$(237), 2 As Q$(238), 2 As Q$(239), 2 As Q$(240), 2 As Q$(241), 2 As Q$(242), 2 As Q$(243), 2 As Q$(244), 2 As Q$(245), 2 As Q$(246), 2 As Q$(247), 2 As Q$(248), 2 As Q$(249)
    Next

    LSet Q$(0) = C$(0)
    LSet Q$(1) = S$(0)
    LSet Q$(2) = A1$(0)
    LSet Q$(3) = MKL$(ATT(0))

    For I = 0 To 13
        LSet Q$(I * 16 + 4) = MKI$(draftDF(0, I))
        LSet Q$(I * 16 + 5) = draftP$(0, I)
        For I1 = 0 To 13
            LSet Q$(I * 16 + I1 + 6) = MKI$(draftRS(0, I, I1))
        Next
    Next
    For I = 0 To 20
        LSet Q$(228 + I) = MKI$(draftT1%(0, I))
    Next
    LSet Q$(249) = MKI$(999)
    Put #1, teamPosition%
    Close 1

    Cls
    JJ = 4
    Locate 6, 8: Color 15, 1
    Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";
    For II = 1 To JJ: Locate 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
    Next
    Locate 7 + JJ, 8
    Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";: Locate 7
    U$ = "DRAFT ANOTHER TEAM (YN)"
    Locate , (80 - Len(U$)) / 2
    Print U$
    Color 15, 0
    I$ = GetKeyPress$

    1010 If UCase$(I$) = "N" Then Cls

End Sub


'----------------------------------------
'       SelectTeam_DRAFT Subroutine
'----------------------------------------
'...explanation...
Sub SelectTeam_DRAFT (numberTeams)

    Shared A1$(), C$(), S$()

    Shared draftA$(), draftP$(), draftT$(), draftYN$()

    Shared CK(), draftA(), draftDF(), draftX()
    Shared ATT(), DA(), draftDF(), draftRS(), SP()

    Shared draftL%(), draftT%(), draftT1%()
    Shared TR%(), V1%(), V2%()

    Selection% = 1
    Count% = numberTeams
    Start% = 1
    FileFGColr% = 15: FileBGColr% = 4
    HiLiteFGColr% = 0: HiLiteBGColr% = 7
    Rows% = 20
    Columns% = 4
    Spacing% = 6
    Row% = 4
    Column% = 1
    Color , 4: Cls: Locate , 33: Color 14
    Print "TEAM SELECTION": Color 15
    Locate 25, 1: Color 14: Print "ESC";: Color 15
    Print "-SELECT NEW ID  PGDN  PGUP  ";: Color 14
    Print "ENTER";: Color 15: Print "-SELECTS TEAM";

    _MouseShow "DEFAULT"
    MAMenu teamNames$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
    _MouseHide

    If ExitCode% <> 27 Then

		D = teamIndex%(Selection%)
		Open diskPaths$(0) + "BASK." + draftYN$(1) For Random As #1 Len = 655
		Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 2 As Q$(14), 2 As Q$(15), 2 As Q$(16), 2 As Q$(17), 2 As Q$(18), 2 As Q$(19), 602 As X$
		For I = 0 To 13
			Field #1, 53 + I * 43 As X$, 15 As Q$(I * 15 + 20), 2 As Q$(I * 15 + 21), 2 As Q$(I * 15 + 22), 2 As Q$(I * 15 + 23), 2 As Q$(I * 15 + 24), 2 As Q$(I * 15 + 25), 2 As Q$(I * 15 + 26), 2 As Q$(I * 15 + 27), 2 As Q$(I * 15 + 28), 2 As Q$(I * 15 + 29), 2 As Q$(I * 15 + 30), 2 As Q$(I * 15 + 31), 2 As Q$(I * 15 + 32), 2 As Q$(I * 15 + 33), 2 As Q$(I * 15 + 34), 655 - 53 - 43 - I * 43 As X$
		Next
		Get #1, D
		draftT$(1) = RTrim$(Q$(0))

		SP(1) = CVI(Q$(1)): DA(1) = CVI(Q$(2))
		V1%(1) = CVI(Q$(3)): V2%(1) = CVI(Q$(4))
		For I = 0 To 4: draftL%(1, I) = CVI(Q$(I + 5)): Next
		For I = 0 To 9: draftT%(1, I) = CVI(Q$(I + 10)): Next
		For I = 0 To 13
			draftA$(1, I) = RTrim$(Q$(I * 15 + 20))

			For I1 = 0 To 6
				draftA(1, I, I1) = CVI(Q$(I * 15 + I1 + 21))
				draftX(1, I, I1) = CVI(Q$(I * 15 + I1 + 28))
			Next
		Next
		Close 1

		If V2%(1) > 20 Then V2%(1) = 100 - V2%(1)
		If V1%(1) > 20 Then V1%(1) = 100 - V1%(1)
		If draftL%(1, 0) > 20 Then draftL%(1, 0) = 100 - draftL%(1, 0)
		If draftL%(1, 2) > 20 Then draftL%(1, 2) = 100 - draftL%(1, 2)
		If draftL%(1, 3) > 20 Then draftL%(1, 3) = 100 - draftL%(1, 3)

		Open diskPaths$(0) + "COLBBTMS." + draftYN$(1) For Random As #1 Len = 566
		Field #1, 15 As Q$(0), 40 As Q$(1), 15 As Q$(2), 4 As Q$(3)
		For I = 0 To 13
			Field #1, 74 + I * 32 As X$, 2 As Q$(I * 16 + 4), 2 As Q$(I * 16 + 5), 2 As Q$(I * 16 + 6), 2 As Q$(I * 16 + 7), 2 As Q$(I * 16 + 8), 2 As Q$(I * 16 + 9), 2 As Q$(I * 16 + 10), 2 As Q$(I * 16 + 11), 2 As Q$(I * 16 + 12), 2 As Q$(I * 16 + 13), 2 As Q$(I * 16 + 14), 2 As Q$(I * 16 + 15), 2 As Q$(I * 16 + 16), 2 As Q$(I * 16 + 17), 2 As Q$(I * 16 + 18), 2 As Q$(I * 16 + 19)
			Field #1, 522 As X$, 2 As Q$(228), 2 As Q$(229), 2 As Q$(230), 2 As Q$(231), 2 As Q$(232), 2 As Q$(233), 2 As Q$(234), 2 As Q$(235), 2 As Q$(236), 2 As Q$(237), 2 As Q$(238), 2 As Q$(239), 2 As Q$(240), 2 As Q$(241), 2 As Q$(242), 2 As Q$(243), 2 As Q$(244), 2 As Q$(245), 2 As Q$(246), 2 As Q$(247), 2 As Q$(248), 2 As Q$(249)
		Next
		Get #1, D

		C$(1) = Q$(0)
		S$(1) = Q$(1)
		A1$(1) = Q$(2)
		ATT(1) = CVL(Q$(3))

		For I = 0 To 13
			draftDF(1, I) = CVI(Q$(I * 16 + 4))
			draftP$(1, I) = Q$(I * 16 + 5)
			For I1 = 0 To 13
				draftRS(1, I, I1) = CVI(Q$(I * 16 + I1 + 6))
			Next
		Next

		For I = 0 To 20
			draftT1%(1, I) = CVI(Q$(228 + I))
		Next
		
		CK(1) = CVI(Q$(249))

		Close 1

		For I = 0 To 13
			TR%(1, I, 1) = SP(1)
			TR%(1, I, 2) = DA(1)
			TR%(1, I, 3) = V2%(1)
			TR%(1, I, 4) = V1%(1)
			TR%(1, I, 5) = draftL%(1, 0)
			TR%(1, I, 6) = draftL%(1, 2)
			TR%(1, I, 7) = draftL%(1, 3)
			TR%(1, I, 8) = 99
			TR%(1, I, 9) = draftT%(1, 3)
			TR%(1, I, 10) = draftT%(1, 4)
			TR%(1, I, 11) = draftT%(1, 5)
			TR%(1, I, 12) = draftT%(1, 6)
		Next

		If CK(1) <> 999 Then

			C$(1) = ""
			S$(1) = ""
			A1$(1) = ""
			ATT(1) = 0

			For I = 0 To 13: draftDF(1, I) = 5: draftP$(1, I) = ""
				For I1 = 0 To 13
					draftRS(1, I, I1) = 0
				Next
			Next

			For I = 0 To 20: draftT1%(1, I) = 0: Next

		End If 'Done comparing to 999
	Else
		BO% = 1
	End If

End Sub


'----------------------------------------
'       MoreTeam Subroutine
'----------------------------------------
Sub MoreTeam (teamPosition%)


    Shared ATT(), CK(), DA(), draftA(), draftDF()
    Shared draftRS(), draftX(), SP()

    Shared TR%(), V1%(), V2%()
    Shared draftL%(), draftT%(), draftT1%()

    Shared draftA$(), draftT$(), draftP$(), draftYN$()
    Shared A1$(), C$(), S$()

    1200 Cls
    Open diskPaths$(0) + "BASK." + draftYN$(0) For Random As #1 Len = 655
    Y = LOF(1)
    Field #1, 15 As Q$(0), 640 As X$
    For I1 = 1 To Y / 655
        Get #1, I1
        teamNames$(I1) = Q$(0): teamIndex%(I1) = I1
    Next
    Close 1
    numberTeams = Y / 655

    1202 Selection% = 1
    Count% = numberTeams
    Start% = 1
    FileFGColr% = 15: FileBGColr% = 4
    HiLiteFGColr% = 0: HiLiteBGColr% = 7
    Rows% = 20
    Columns% = 4
    Spacing% = 6
    Row% = 4
    Column% = 1
    Color , 4: Cls: Locate , 33: Color 14
    Print "TEAM SELECTION": Color 15
    Locate 25, 1: Color 14: Print "ESC";: Color 15
    Print "-SELECT NEW ID  PGDN  PGUP  ";: Color 14
    Print "ENTER";: Color 15: Print "-SELECTS TEAM";

    _MouseShow "DEFAULT"
    MAMenu teamNames$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
    _MouseHide

    If ExitCode% = 27 Then BO% = 1: Exit Sub

    teamPosition% = teamIndex%(Selection%)

    9100 Open diskPaths$(0) + "BASK." + draftYN$(0) For Random As #1 Len = 655
    Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 2 As Q$(14), 2 As Q$(15), 2 As Q$(16), 2 As Q$(17), 2 As Q$(18), 2 As Q$(19), 602 As X$
    For I = 0 To 13
        Field #1, 53 + I * 43 As X$, 15 As Q$(I * 15 + 20), 2 As Q$(I * 15 + 21), 2 As Q$(I * 15 + 22), 2 As Q$(I * 15 + 23), 2 As Q$(I * 15 + 24), 2 As Q$(I * 15 + 25), 2 As Q$(I * 15 + 26), 2 As Q$(I * 15 + 27), 2 As Q$(I * 15 + 28), 2 As Q$(I * 15 + 29), 2 As Q$(I * 15 + 30), 2 As Q$(I * 15 + 31), 2 As Q$(I * 15 + 32), 2 As Q$(I * 15 + 33), 2 As Q$(I * 15 + 34), 655 - 53 - 43 - I * 43 As X$
    Next
    Get #1, teamPosition%
    draftT$(0) = RTrim$(Q$(0))

    SP(0) = CVI(Q$(1)): DA(0) = CVI(Q$(2))
    V1%(0) = CVI(Q$(3)): V2%(0) = CVI(Q$(4))
    For I = 0 To 4: draftL%(0, I) = CVI(Q$(I + 5)): Next
    For I = 0 To 9: draftT%(0, I) = CVI(Q$(I + 10)): Next
    For I = 0 To 13
        draftA$(0, I) = RTrim$(Q$(I * 15 + 20))

        For I1 = 0 To 6
            draftA(0, I, I1) = CVI(Q$(I * 15 + I1 + 21))
            draftX(0, I, I1) = CVI(Q$(I * 15 + I1 + 28))
		Next
	Next
    Close 1

    If V1%(0) > 20 Then V1%(0) = 100 - V1%(0)
    If V2%(0) > 20 Then V2%(0) = 100 - V2%(0)
    If draftL%(0, 0) > 20 Then draftL%(0, 0) = 100 - draftL%(0, 0)
    If draftL%(0, 2) > 20 Then draftL%(0, 2) = 100 - draftL%(0, 2)
    If draftL%(0, 3) > 20 Then draftL%(0, 3) = 100 - draftL%(0, 3)

    Open diskPaths$(0) + "COLBBTMS." + draftYN$(0) For Random As #1 Len = 566
    Field #1, 15 As Q$(0), 40 As Q$(1), 15 As Q$(2), 4 As Q$(3)
    For I = 0 To 13
        Field #1, 74 + I * 32 As X$, 2 As Q$(I * 16 + 4), 2 As Q$(I * 16 + 5), 2 As Q$(I * 16 + 6), 2 As Q$(I * 16 + 7), 2 As Q$(I * 16 + 8), 2 As Q$(I * 16 + 9), 2 As Q$(I * 16 + 10), 2 As Q$(I * 16 + 11), 2 As Q$(I * 16 + 12), 2 As Q$(I * 16 + 13), 2 As Q$(I * 16 + 14), 2 As Q$(I * 16 + 15), 2 As Q$(I * 16 + 16), 2 As Q$(I * 16 + 17), 2 As Q$(I * 16 + 18), 2 As Q$(I * 16 + 19)
        Field #1, 522 As X$, 2 As Q$(228), 2 As Q$(229), 2 As Q$(230), 2 As Q$(231), 2 As Q$(232), 2 As Q$(233), 2 As Q$(234), 2 As Q$(235), 2 As Q$(236), 2 As Q$(237), 2 As Q$(238), 2 As Q$(239), 2 As Q$(240), 2 As Q$(241), 2 As Q$(242), 2 As Q$(243), 2 As Q$(244), 2 As Q$(245), 2 As Q$(246), 2 As Q$(247), 2 As Q$(248), 2 As Q$(249)
    Next
    Get #1, teamPosition%
    C$(0) = Q$(0)
    S$(0) = Q$(1)
    A1$(0) = Q$(2)
    ATT(0) = CVL(Q$(3))
    For I = 0 To 13
        draftDF(0, I) = CVI(Q$(I * 16 + 4))
        draftP$(0, I) = Q$(I * 16 + 5)
        For I1 = 0 To 13
            draftRS(0, I, I1) = CVI(Q$(I * 16 + I1 + 6))
    Next: Next
    For I = 0 To 20
        draftT1%(0, I) = CVI(Q$(228 + I))
    Next
    CK(0) = CVI(Q$(249))
    Close 1

    For I = 0 To 13
        TR%(0, I, 1) = SP(0)
        TR%(0, I, 2) = DA(0)
        TR%(0, I, 3) = V2%(0)
        TR%(0, I, 4) = V1%(0)
        TR%(0, I, 5) = draftL%(0, 0)
        TR%(0, I, 6) = draftL%(0, 2)
        TR%(0, I, 7) = draftL%(0, 3)
        TR%(0, I, 8) = 99
        TR%(0, I, 9) = draftT%(0, 3)
        TR%(0, I, 10) = draftT%(0, 4)
        TR%(0, I, 11) = draftT%(0, 5)
        TR%(0, I, 12) = draftT%(0, 6)
    Next
    If CK(0) <> 999 Then
        C$(0) = "": S$(0) = "": A1$(0) = "": ATT(0) = 0
        For I = 0 To 13: draftDF(0, I) = 5: draftP$(0, I) = ""
            For I1 = 0 To 13
                draftRS(0, I, I1) = 0
            Next
        Next
        For I = 0 To 20: draftT1%(0, I) = 0: Next
    End If

    '9110 Return

End Sub
