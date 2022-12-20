Sub SINPUT ()

    _Title "Courtside College Basketball - Stats Input"

    'Dim A(14, 14)
    Shared A$()
    Shared W0!(), W1!()
    Shared Z!(), Z1!()

    Dim Z2(14, 14)

    Color 7, 0
    KEY Off
    'On Error GoTo 0

    Do
        Cls
        Call Title_SINPUT (1, "")

        Color 15: Print "C";
        Color 3: Print "HANGE/INPUT COMPILED TEAM STATS"
        Print
        Color 15: Print "R";
        Color 3: Print "ETURN TO MAIN MENU"

        Do
            I$ = GetKeyPress$
            If UCase$(I$) <> "R" Then 
                Q = 1
            Else 
                Q = 2
            End If

        Loop Until Q >= 1 And Q <= 2

        If Q = 1 Then

            Call SelectTeam_SINPUT (B$)

            Z$ = Right$(B$, Len(B$) - 3) + "." + Left$(B$, 2)

            Cls
            X$ = "OFFENSIVE TEAM STATS"
            Call Title_SINPUT (2, X$)
            Locate 3, 1
            Print "1. STAT FILE NAME "; Z$

            Open diskPaths$(1) + Z$ For Input As #1
            Input #1, A$
            For I = 0 To 13: Input #1, A$(I)
            For J = 0 To 13: Input #1, Z2(I, J): Next J: Input #1, W0!(I): Input #1, W1!(I): Next I
            For L = 0 To 15: Input #1, Z!(L): Next L
            For L = 0 To 15: Input #1, Z1!(L): Next L
            Close 1

            X% = 2

            Cls
            X$ = "OFFENSIVE TEAM STATS"
            Call Title_SINPUT (2, X$)

            X% = 2

            Cls
            X$ = "OFFENSIVE TEAM STATS"
            Call Title_SINPUT (2, X$)

            175 Locate 3, 1: Input "1. STAT FILE NAME "; Z$: If C1 = 1 Then 210
            177 Locate 4, 1: Input "2. TM MINUTES "; Z!(0): Z!(0) = Z!(0) * 60: If C1 = 1 Then 210
            179 Locate 5, 1: Input "3. TM FGA "; Z!(1): If C1 = 1 Then 210
            181 Locate 6, 1: Input "4. TM FGM "; Z!(2): If C1 = 1 Then 210
            183 Locate 7, 1: Input "5. TM 3FGA "; Z!(14): If C1 = 1 Then 210
            185 Locate 8, 1: Input "6. TM 3FGM "; Z!(13): If C1 = 1 Then 210
            187 Locate 9, 1: Input "7. TM FTA "; Z!(3): If C1 = 1 Then 210
            189 Locate 10, 1: Input "8. TM FTM "; Z!(4): If C1 = 1 Then 210
            191 Locate 11, 1: Input "9. TM OFF RB  "; Z!(12): If C1 = 1 Then 210
            193 Locate 12, 1: Input "10. TM TOT RB "; Z!(5): If C1 = 1 Then 210
            195 Locate 13, 1: Input "11. TM FOULS "; Z!(6): If C1 = 1 Then 210
            197 Locate 14, 1: Input "12. TM STEALS "; Z!(7): If C1 = 1 Then 210
            199 Locate 15, 1: Input "13. TM ASSISTS "; Z!(8): If C1 = 1 Then 210
            201 Locate 16, 1: Input "14. TM BLOCKS "; Z!(9): If C1 = 1 Then 210
            203 Locate 17, 1: Input "15. TM TURNOVERS "; Z!(11): If C1 = 1 Then 210
            205 Locate 18, 1: Input "16. TM POINTS  "; Z!(10): If C1 = 1 Then 210

            210 C1 = 1

            Do
                Locate 20, 1
                Input "# TO CHANGE (0 TO CONTINUE) "; B9

            Loop Until B9 >= 0 And B9 <= 16

            On B9 GOTO 175, 177, 179, 181, 183, 185, 187, 189, 191, 193, 195, 197, 199, 201, 203, 205

            '0 = Continue
            C1 = 0

            If X% <= 0 Then

                365 Cls
                X$ = "DEFENSIVE TEAM STATS"
                Call Title_SINPUT (2, X$)

                379 Locate 3, 1: Input "1. OPP FGA "; Z1!(1): If C1 = 1 Then 410
                381 Locate 4, 1: Input "2. OPP FGM "; Z1!(2): If C1 = 1 Then 410
                383 Locate 5, 1: Input "3. OPP 3FGA "; Z1!(14): If C1 = 1 Then 410
                385 Locate 6, 1: Input "4. OPP 3FGM "; Z1!(13): If C1 = 1 Then 410
                387 Locate 7, 1: Input "5. OPP FTA "; Z1!(3): If C1 = 1 Then 410
                389 Locate 8, 1: Input "6. OPP FTM "; Z1!(4): If C1 = 1 Then 410
                391 Locate 9, 1: Input "7. OPP OFF RB  "; Z1!(12): If C1 = 1 Then 410
                393 Locate 10, 1: Input "8. OPP TOT RB "; Z1!(5): If C1 = 1 Then 410
                395 Locate 11, 1: Input "9. OPP FOULS "; Z1!(6): If C1 = 1 Then 410
                397 Locate 12, 1: Input "10. OPP STEALS "; Z1!(7): If C1 = 1 Then 410
                399 Locate 13, 1: Input "11. OPP ASSISTS "; Z1!(8): If C1 = 1 Then 410
                401 Locate 14, 1: Input "12. OPP BLOCKS "; Z1!(9): If C1 = 1 Then 410
                403 Locate 15, 1: Input "13. OPP TURNOVERS "; Z1!(11): If C1 = 1 Then 410
                405 Locate 16, 1: Input "14. OPP POINTS  "; Z1!(10): If C1 = 1 Then 410

                410 C1 = 1

                Do
                    Locate 20, 1: Input "# TO CHANGE (0 TO CONTINUE) "; B9
                Loop Until B9 >= 0 And B9 <= 14

                413 On B9 GOTO 379, 381, 383, 385, 387, 389, 391, 393, 395, 397, 399, 401, 403, 405

                '0 = Continue
                C1 = 0

                If X% <= 0 Then

                    For I = 0 To 13
                        Cls
                        X$ = "PLAYER -" + Str$(I + 1)
                        Call Title_SINPUT (2, X$)

                        675 Locate 3, 1: Input "1. NAME "; A$(I): If C1 = 1 Then 1225
                        705 Locate 4, 1: Input "2. GAMES "; Z2(I, 0): If C1 = 1 Then 1225
                        745 Locate 5, 1: Input "3. MINUTES "; Z2(I, 10): Z2(I, 10) = Z2(I, 10) * 60: If C1 = 1 Then 1225
                        785 Locate 6, 1: Input "4. FGA "; Z2(I, 1): If C1 = 1 Then 1225
                        825 Locate 7, 1: Input "5. FGM "; Z2(I, 2): If C1 = 1 Then 1225
                        865 Locate 8, 1: Input "6. FTA "; Z2(I, 3): If C1 = 1 Then 1225
                        905 Locate 9, 1: Input "7. FTM "; Z2(I, 4): If C1 = 1 Then 1225
                        906 Locate 10, 1: Input "8. OFF REB "; Z2(I, 12): If C1 = 1 Then 1225
                        945 Locate 11, 1: Input "9. TOT REB  "; Z2(I, 5): If C1 = 1 Then 1225
                        985 Locate 12, 1: Input "10. FOULS "; Z2(I, 6): If C1 = 1 Then 1225
                        1025 Locate 13, 1: Input "11. STEALS "; Z2(I, 7): If C1 = 1 Then 1225
                        1065 Locate 14, 1: Input "12. ASSISTS "; Z2(I, 8): If C1 = 1 Then 1225
                        1105 Locate 15, 1: Input "13. BLOCKS "; Z2(I, 9): If C1 = 1 Then 1225
                        1106 Locate 16, 1: Input "14. TURNOVERS "; Z2(I, 11): If C1 = 1 Then 1225
                        1184 Locate 17, 1: Input "15. 3FGA "; W1!(I): If C1 = 1 Then 1225
                        1185 Locate 18, 1: Input "16. 3FGM "; W0!(I): If C1 = 1 Then 1225
                        1186 Locate 19, 1: Input "17. HI-GAME (PTS) "; Z2(I, 13): If C1 = 1 Then 1225

                        1225 C1 = 1

                        Do
                            Locate 21, 1

                            Input "# TO CHANGE (0 TO CONTINUE) "; B9

                        Loop Until B9 >= 0 And B9 <= 17

                        1265 On B9 GOTO 675, 705, 745, 785, 825, 865, 905, 906, 945, 985, 1025, 1065, 1105, 1106, 1184, 1185, 1186

                        C1 = 0
                        If X% > 0 Then 1605

                    Next I

                End If

                Cls
                X$ = "DEFENSIVE TEAM STATS"
                Call Title_SINPUT (2, X$)

                X% = 2
                Locate 3, 1
                Print "1. OPP FGA "; Z1!(1)
                Print "2. OPP FGM "; Z1!(2)
                Print "3. OPP 3FGA "; Z1!(14)
                Print "4. OPP 3FGM "; Z1!(13)
                Print "5. OPP FTA "; Z1!(3)
                Print "6. OPP FTM "; Z1!(4)
                Print "7. OPP OFF RB  "; Z1!(12)
                Print "8. OPP TOT RB "; Z1!(5)
                Print "9. OPP FOULS "; Z1!(6)
                Print "10. OPP STEALS "; Z1!(7)
                Print "11. OPP ASSISTS "; Z1!(8)
                Print "12. OPP BLOCKS "; Z1!(9)
                Print "13. OPP TURNOVERS "; Z1!(11)
                Print "14. OPP POINTS  "; Z1!(10)
                GoTo 410

            End If

            1605 '
            Cls
            Call Title_SINPUT (1, "")
            Locate 3, 1: Color 14: Print "CHANGE PLAYERS "

            Print

            X% = 2

            For I = 0 To 13
                Color 7
                If I / 2 = Int(I / 2) Then Color 15
                Print (I + 1);
                Print " "; A$(I)
            Next I
            Print
            Print "# TO CHANGE (0 TO CONTINUE) ";

            Input I
            I = I - 1

            If I <= 13 And I >= 0 Then

                Cls
                X$ = "PLAYER -" + Str$(I + 1)
                Call Title_SINPUT (2, X$)

                Locate 3, 1
                Print "1. NAME "; A$(I)
                Print "2. GAMES "; Z2(I, 0)
                Print "3. MINUTES "; Int(Z2(I, 10) / 60 + .5)
                Print "4. FGA "; Z2(I, 1)
                Print "5. FGM "; Z2(I, 2); ""
                Print "6. FTA "; Z2(I, 3)
                Print "7. FTM "; Z2(I, 4)
                Print "8. OFF REB "; Z2(I, 12)
                Print "9. TOT REB  "; Z2(I, 5)
                Print "10. FOULS "; Z2(I, 6)
                Print "11. STEALS "; Z2(I, 7)
                Print "12. ASSISTS "; Z2(I, 8)
                Print "13. BLOCKS "; Z2(I, 9)
                Print "14. TURNOVERS "; Z2(I, 11)
                Print "15. 3FGA "; W1!(I)
                Print "16. 3FGM "; W0!(I)
                Print "17. HI-GAME (PTS) "; Z2(I, 13)
                GoTo 1225
            Else
                X% = 0
            End If

            GoTo 1935

            '---
            1875 Print
            Print "# TO CHANGE (1-12) (0 TO CONTINUE) ";
            Input I
            I = I - 1
            If I > 10 Or I < 0 Then 
                X% = 0  
            Else
                1925 Cls
                Call Title_SINPUT (1, "")
            End if

            1935 Open diskPaths$(1) + Z$ For Output As #1
            Print #1, A$
            For I = 0 To 13: Print #1, A$(I)
            For J = 0 To 13: Print #1, Z2(I, J): Next J: Print #1, W0!(I): Print #1, W1!(I): Next I
            For L = 0 To 15: Print #1, Z!(L): Next L
            For L = 0 To 15: Print #1, Z1!(L): Next L
            Close 1

        End If

    Loop Until I$ = "2"

    Cls

End Sub

'----------------------------------------
'     SelectTeam_SINPUT Subroutine
'----------------------------------------
'...explanation...
Sub SelectTeam_SINPUT (B$)

    Shared L%(), T%()

    Do
        Color 15, 0
        Cls
        JJ = 6
        SN = 1
        Locate 6, 8: Color 15, 4
        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";: For II = 1 To JJ: Locate 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";: Next
        Locate 7 + JJ, 8
        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";
		
		Locate 7, 32
        Locate , 12: Print "ENTER DISK ID: ": Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Print
        Locate , 12: Print "ESC TO RETURN TO MAIN MENU"
        Color 15, 0

        I$ = GetKeyPress$

        If I$ <> Chr$(27) Then

            If I$ = Chr$(13) Then 
                Call GetTeam_SINPUT (YN$, NTMS)
            Else
                Locate 7, 27: Print I$
                Locate 7, 28: Line Input NY$
                NY$ = I$ + NY$
                Color 15, 0

                If NY$ = "" And YN$ <> "" Then 
                    Call GetTeam_SINPUT (YN$, NTMS)
                Else
                    YN$ = NY$
                    Erase teamNames$

                    Open diskPaths$(0) + "BASK." + YN$ As #1 Len = 655
                    Y = LOF(1)
                    Field #1, 15 As Q$(0), 640 As X$
                    For I1 = 1 To Y / 655
                        Get #1, I1
                        teamNames$(I1) = Q$(0)
                    Next
                    Close 1

                    NTMS = Y / 655
                    'TS% = Int((NTMS - 1) / 80)
                    Call GetTeam_SINPUT (YN$, NTMS)
                End If
            End If

            If BO% = 1 Then 
                BO% = 0
            Else
                Locate 2, 33
				Color T%(0), T%(1): Print A$
                
				Color 15, 4
				
				Do
					Locate , 33: Print "ANY CHANGE (YN)";
					I$ = GetKeyPress$
				Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"
            End If
        Else
            BO% = 1
        End If

    Loop Until UCase$(I$) = "N"

    Color 15, 0

End Sub

'----------------------------------------
'       GetTeam_SINPUT Subroutine
'----------------------------------------
'...explanation...
Sub GetTeam_SINPUT (YN$, numberTeams)

    Shared L%(), T%()

    NS% = 0

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

    If I$ <> Chr$(27) Then

        T1 = NS% * 80 + Y - 3 + (X - 1) * 20

        Open diskPaths$(0) + "BASK." + YN$ As #1 Len = 655

        FIELD #1,15 AS Q$(0),2 AS Q$(1),2 AS Q$(2),2 AS Q$(3),2 AS Q$(4),2 AS Q$(5),2 AS Q$(6),2 AS Q$(7),2 AS Q$(8),2 AS Q$(9),2 AS Q$(10),2 AS Q$(11),2 AS Q$(12),2 AS Q$(13),2 AS Q$(14),2 AS Q$(15),2 AS Q$(16),2 AS Q$(17),2 AS Q$(18),2 AS Q$(19),602 AS X$

        For I = 0 To 13
            FIELD #1,53+I*43 AS X$,15 AS Q$(I*15+20),2 AS Q$(I*15+21),2 AS Q$(I*15+22),2 AS Q$(I*15+23),2 AS Q$(I*15+24),2 AS Q$(I*15+25),2 AS Q$(I*15+26),2 AS Q$(I*15+27),2 AS Q$(I*15+28),2 AS Q$(I*15+29),2 AS Q$(I*15+30),2 AS Q$(I*15+31),2 AS Q$(I*15+32),2 AS Q$(I*15+33),2 AS Q$(I*15+34),655-53-43-I*43 AS X$
        Next

        Get #1, T1

        A$ = RTRim$(Q$(0))
        B$ = A$
        staminaRating% = CVI(Q$(1))
        defFGPctAdj% = CVI(Q$(2))
        V1% = CVI(Q$(3))
        V2% = CVI(Q$(4))

        For I = 0 To 4: L%(I) = CVI(Q$(I + 5)): Next
        For I = 0 To 9: T%(I) = CVI(Q$(I + 10)): Next

        Close 1
    Else
        BO% = 1
    End If 'Done checking ExitCode code

END SUB
