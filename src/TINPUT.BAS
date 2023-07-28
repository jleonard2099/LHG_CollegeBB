Sub TINPUT ()

    _Title "Courtside College Basketball - Team Input"

    Shared BE, BF, BG, BI, BJ, BK, BH
    Shared L3, L5, T1, T2, T6, T7, T8, T3, T4, T0, T9
    Shared W0, W1, XX

    Shared def3FG_Adj, def3FGA_Adj
    Shared teamAttendance%, defFGPctAdj%, staminaRating%
    Shared mascot$, coach$, arenaName$

    Dim E0(16)
    Dim inputQQ(3), inputX$(3)

    Shared teamRatings%()

    Shared playerOff!(), playerDef!(), leagueRatings%()
    Shared playerStats!(), playerRatings!()

    Shared playerNames$(), position$()

    inputX$(0) = "COLLEGE BASKETBALL"
    inputX$(1) = "H S BASKETBALL"

    Cls

    Do

        Color 12: Print "SELECT TYPE OF TEAMS YOU ARE ENTERING"

        Print
        Color 14: Print "(0) ";
        Color 15: Print "COLLEGE BASKETBALL"

        Print
        Color 14: Print "(1) ";
        Color 15: Print "HIGH SCHOOL BASKETBALL"

        I$ = GetKeyPress$

        If I$ <> Chr$(27) Then
            H = Val(I$)
        End If

    Loop Until I$ = Chr$(27) Or H = 0 Or H = 1

    If I$ <> Chr$(27) Then

        If H = 0 Then
            'College BB
            L7 = 6
            L3 = 20
            L6 = 14
        Else
            'High School BB
            L7 = 5
            L6 = 11
            L3 = 16
        End If

        '---------------
        ' Season Stats
        '---------------
        Cls

        Color 14
        Print inputX$(H); Tab(31); "LEAGUE INFORMATION"
        Color 15

        Locate 3: Input "1. SEASON FGA/G "; L1
        Locate 4: Input "2. SEASON REBOUNDS/GAME "; L2
        Locate 5: Input "3. SEASON FG % "; L4
        Locate 6: Input "4. SEASON STEALS/GAME "; BA
        Locate 7: Input "5. SEASON TURNOVERS/GAME "; BB
        Locate 8: Input "6. SEASON 3 FGA/FGA % "; Q7
        Locate 9: Input "7. SEASON 3 FG % "; Q8
        teamRatings%(2) = 999

        Do

            Do
                Locate 22
                Input "# TO CHANGE (0 TO CONTINUE ) "; B9
            Loop Until B9 >= 0 And B9 <= 7

            Select Case B9

                Case 1:
                    Locate 3: Input "1. SEASON FGA/G "; L1

                Case 2:
                    Locate 4: Input "2. SEASON REBOUNDS/GAME "; L2

                Case 3:
                    Locate 5: Input "3. SEASON FG % "; L4

                Case 4
                    Locate 6: Input "4. SEASON STEALS/GAME "; BA

                Case 5
                    Locate 7: Input "5. SEASON TURNOVERS/GAME "; BB

                Case 6
                    Locate 8: Input "6. SEASON 3 FGA/FGA % "; Q7

                Case 7
                    Locate 9: Input "7. SEASON 3 FG % "; Q8

            End Select

        Loop Until B9 = 0


        '---------------
        ' Required Stats
        '---------------
        Do

            Cls

            Print "YOU MUST HAVE THE FOLLOWING TEAM &"
            Print "INDIVIDUAL STATS: "
            Print "FGA, FG %, FTA, FT %, REBOUNDS"
            Print
            Print "IF YOU ARE INPUTTING A TEAM WITH ANY "
            Print "OF THE FOLLOWING STATS MISSING,"
            Print "SIMPLY TYPE 'N' AND THE PROGRAM WILL"
            Print "ALLOW YOU TO INPUT RATINGS DIRECTLY"
            Print
            Color 12: Print "DO YOU HAVE THE FOLLOWING INFORMATION :"

            Print
            Color 14: Print "(1) ";
            Color 15: Print "TEAM/INDIVID ASSISTS (Y/N): ";

            Do
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = Chr$(27)

            If I$ <> Chr$(27) Then

                Print UCase$(I$)

                If UCase$(I$) = "Y" Then inputQQ(0) = 0
                If UCase$(I$) = "N" Then inputQQ(0) = 1

                Print
                Color 14: Print "(2) ";
                Color 15: Print "PERSONAL FOULS (Y/N): ";

                Do
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = Chr$(27)

                If I$ <> Chr$(27) Then

                    Print UCase$(I$)

                    If UCase$(I$) = "Y" Then inputQQ(1) = 0
                    If UCase$(I$) = "N" Then inputQQ(1) = 1

                    Print
                    Color 14: Print "(3) ";
                    Color 15: Print "STEALS & TURNOVERS (Y/N): ";

                    Do
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = Chr$(27)

                    If I$ <> Chr$(27) Then

                        Print UCase$(I$)

                        If UCase$(I$) = "Y" Then inputQQ(2) = 0
                        If UCase$(I$) = "N" Then inputQQ(2) = 1

                        Print
                        Color 14: Print "(4) ";
                        Color 15: Print "TEAM / INDIVIDUAL BLOCKS (Y/N): ";

                        Do
                            I$ = GetKeyPress$
                        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = Chr$(27)

                        If I$ <> Chr$(27) Then

                            Print UCase$(I$)

                            If UCase$(I$) = "Y" Then inputQQ(3) = 0
                            If UCase$(I$) = "N" Then inputQQ(3) = 1

                            Print

                            Do

                                Print "ANY CHANGE (Y/N)";
                                I$ = GetKeyPress$

                            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or I$ = Chr$(27)

                        End If

                    End If

                End If

            End If

            If I$ = Chr$(27) Then Exit Sub
            
        Loop Until UCase$(I$) = "N"

        '---------------
        ' Team Stats
        '---------------
        Cls

        Locate 1
        Color 14
        Print inputX$(H); Tab(32); "TEAM INFORMATION"
        Color 15

        Locate 2
        Input " 0. TEAM # (0 TO EXIT)"; teamNumber

        If teamNumber <> 0 Then

            Locate 3: Input " 1. DISK ID "; yearNumber$
            Locate 4: Input " 2. TEAM NAME "; A$
            Locate 5: Input " 3. TEAM NICKNAME "; mascot$
            Locate 6: Input " 4. STRENGTH OF SCHEDULE ADJ "; BC
            Locate 7: Input " 5. HOMECOURT FACTOR (0-6)"; teamRatings%(6)
            Locate 8: Input " 6. TEXT COLOR (0-15) "; teamRatings%(0)
            Locate 9: Input " 7. BACKGROUND COLOR (0-7) "; teamRatings%(1)
            'Locate 8, 37
            'Print "TEAM APPEARANCE: ";
            Locate 9, 37
            Color teamRatings%(0), teamRatings%(1)
            Print A$ + " " + mascot$
            Color 15, 0

            Locate 2, 37: Input "20. GAMES PLAYED "; L5
            Locate 3, 37: Input "21. WINS "; teamRatings%(7)
            Locate 4, 37: Input "22. LOSSES "; teamRatings%(8)
            Locate 5, 37: Input "23. POWER RATING "; teamRatings%(9)
            Locate 6, 37: Input "24. COACH "; coach$
            Locate 7, 37: Input "25. ARENA "; arenaName$
            Locate 8, 37: Input "26. AVG ATT "; teamAttendance%

            Locate 10, 10: Print A$;
            Locate 10, 30: Print "OPPONENTS"
            Locate 11, 1: Print "8. FGA ";
            Locate 11, 15: Input T1
            Locate 11, 30: Input T2
            Locate 12, 1: Print "9. FG % ";
            Locate 12, 15: Input T6
            Locate 12, 30: Input BE
            Locate 13, 1: Print "10. FTA";
            Locate 13, 15: Input T7
            Locate 13, 30: Input BF
            Locate 14, 1: Print "11. FT %";
            Locate 14, 15: Input T8
            Locate 14, 30: Input BG
            Locate 15, 1: Print "12. RB";
            Locate 15, 15: Input T3
            Locate 15, 30: Input T4
            Locate 16, 1: Print "13. AST";
            Locate 16, 15: Input T0
            Locate 16, 30: Input XX
            Locate 17, 1: Print "14. FOULS";
            Locate 17, 15: Input L3
            Locate 17, 30: Input BI
            Locate 18, 1: Print "15. TO";
            Locate 18, 15: Input BJ
            Locate 18, 30: Input BK
            Locate 19, 1: Print "16. STLS";
            Locate 19, 15: Input T9
            Locate 19, 30: Input BH
            Locate 20, 1: Print "17. 3 FGA ";
            Locate 20, 15: Print "---";
            Locate 20, 30: Input W0
            Locate 21, 1: Print "18. 3 FGA % ";
            Locate 21, 15: Print "---";
            Locate 21, 30: Input W1

            Locate 22: Input "19. OPP 2FG% "; OE

            'FOULS
            'Average 20 / game if none entered
            If BI = 0 Then BI = 20 * L5
            If L3 = 0 Then L3 = 20 * L5

            Do

                Do
                    Locate 23
                    Input "# TO CHANGE (0 TO CONTINUE ) "; B9
                Loop Until B9 >= 0 And B9 <= 26

                Select Case B9

                    Case 1
                        Locate 3, 1: Input " 1. DISK ID "; yearNumber$

                    Case 2
                        Locate 4: Input " 2. TEAM NAME "; A$

                    Case 3
                        Locate 5: Input " 3. TEAM NICKNAME "; mascot$

                    Case 4
                        Locate 6: Input " 4. STRENGTH OF SCHEDULE ADJ "; BC

                    Case 5
                        Locate 7: Input " 5. HOMECOURT FACTOR (0-6)"; teamRatings%(6)

                    Case 6
                        Locate 8: Input " 6. TEXT COLOR (0-15) "; teamRatings%(0)
                        If teamRatings%(0) >= 0 And teamRatings%(1) >= 0 Then
                            'Locate 8, 37
                            'Print "TEAM APPEARANCE: ";
                            Locate 9, 37
                            Color teamRatings%(0), teamRatings%(1)
                            Print A$ + " " + mascot$
                            Color 15, 0
                        End If

                    Case 7
                        Locate 9: Input " 7. BACKGROUND COLOR (0-7) "; teamRatings%(1)
                        If teamRatings%(0) >= 0 And teamRatings%(1) >= 0 Then
                            'Locate 8, 37
                            'Print "TEAM APPEARANCE: ";
                            Locate 9, 37
                            Color teamRatings%(0), teamRatings%(1)
                            Print A$ + " " + mascot$
                            Color 15, 0
                        End If

                    Case 8
                        Locate 11, 1: Print "8. FGA ";
                        Locate 11, 15: Input T1
                        Locate 11, 30: Input T2

                    Case 9
                        Locate 12, 1: Print "9. FG % ";
                        Locate 12, 15: Input T6
                        Locate 12, 30: Input BE

                    Case 10
                        Locate 13, 1: Print "10. FTA";
                        Locate 13, 15: Input T7
                        Locate 13, 30: Input BF

                    Case 11
                        Locate 14, 1: Print "11. FT %";
                        Locate 14, 15: Input T8
                        Locate 14, 30: Input BG

                    Case 12
                        Locate 15, 1: Print "12. RB";
                        Locate 15, 15: Input T3
                        Locate 15, 30: Input T4

                    Case 13
                        Locate 16, 1: Print "13. AST";
                        Locate 16, 15: Input T0
                        Locate 16, 30: Input XX

                    Case 14
                        Locate 17, 1: Print "14. FOULS";
                        Locate 17, 15: Input L3
                        Locate 17, 30: Input BI

                    Case 15
                        Locate 18, 1: Print "15. TO";
                        Locate 18, 15: Input BJ
                        Locate 18, 30: Input BK

                    Case 16
                        Locate 19, 1: Print "16. STLS";
                        Locate 19, 15: Input T9
                        Locate 19, 30: Input BH

                    Case 17
                        Locate 20, 1: Print "17. 3 FGA ";
                        Locate 20, 15: Print "---";
                        Locate 20, 30: Input W0

                    Case 18
                        Locate 21, 1: Print "18. 3 FGA % ";
                        Locate 21, 15: Print "---";

                    Case 19
                        Locate 22, 1: Input "OPP 2FG% "; OE

                    Case 20
                        Locate 2, 37: Input " 3. GAMES PLAYED "; L5

                    Case 21
                        Locate 3, 37: Input "20. WINS "; teamRatings%(7)

                    Case 22
                        Locate 4, 37: Input "21. LOSSES "; teamRatings%(8)

                    Case 23
                        Locate 5, 37: Input "22. POWER RATING "; teamRatings%(9)

                    Case 24
                        Locate 6, 37: Input "23. COACH "; coach$

                    Case 25
                        Locate 7, 37: Input "24. ARENA "; arenaName$

                    Case 26
                        Locate 8, 37: Input "25. AVG ATT "; teamAttendance%

                End Select


            Loop Until teamNumber = 0 Or B9 = 0

        End If

        If teamNumber <> 0 Then

            '---------------
            ' Players
            '---------------
            ' *** TM FGA/G
            teamRatings%(3) = Int(((T1 + T2) / (L5 * 2)) + .5)
            teamRatings%(4) = L1
            teamRatings%(5) = L5

            If H = 1 Then
                F3 = 32
            Else
                F3 = 40
            End If

            For I = 0 To 13

                Cls
                Color 14
                Print inputX$(H); Tab(35); "PLAYER - "; I + 1
                Print "ENTER PLAYER STATS"
                Color 15

                Locate 3
                Input " 1. NAME (XXX TO QUIT) "; playerNames$(I)

                If playerNames$(I) <> "XXX" Then

                    Locate 4: Input " 2. POSITION "; position$(I)
                    Locate 5: Input " 3. GAMES "; playerOff!(I, 7)
                    playerRatings!(I, 0) = playerOff!(I, 7)
                    Locate 6: Input " 4. MP "; playerOff!(I, 8)
                    Locate 7: Input " 5. FGM "; playerStats!(I, 3)
                    Locate 8: Input " 6. FGA "; playerOff!(I, 9)
                    playerRatings!(I, 1) = playerOff!(I, 9)
                    Locate 9: Input " 7. FTM "; playerStats!(I, 5)
                    Locate 10: Input " 8. FTA "; playerOff!(I, 16)
                    Locate 11: Input " 9. ORB "; playerOff!(I, 19)
                    Locate 12: Input "10. TOT RB "; playerOff!(I, 10)
                    Locate 13: Input "11. PF "; playerOff!(I, 12)
                    Locate 14: Input "12. AST "; playerOff!(I, 11)
                    Locate 15: Input "13. BLK "; playerOff!(I, 14)
                    Locate 16: Input "14. STL "; playerOff!(I, 13)
                    Locate 17: Input "15. 3 FGM "; playerStats!(I, 13)
                    Locate 18: Input "16. 3 FGA "; playerOff!(I, 17)
                    Locate 19: Input "17. DEFENSE (1-9) "; playerDef!(I)

                    Do

                        Do
                            Locate 22
                            Input "# TO CHANGE (0 TO CONTINUE) "; B9
                        Loop Until B9 >= 0 And B9 <= 17

                        If B9 <> 0 Then

                            Select Case B9

                                Case 1
                                    Locate 3: Input " 1. NAME (XXX TO QUIT) "; playerNames$(I)

                                Case 2
                                    Locate 4: Input "2. POSITION "; position$(I)

                                Case 3
                                    Locate 5: Input " 3. GAMES "; playerOff!(I, 7)
                                    playerRatings!(I, 0) = playerOff!(I, 7)

                                Case 4
                                    Locate 6: Input " 4. MP "; playerOff!(I, 8)

                                Case 5
                                    Locate 7: Input " 5. FGM "; playerStats!(I, 3)

                                Case 6
                                    Locate 8: Input " 6. FGA "; playerOff!(I, 9)
                                    playerRatings!(I, 1) = playerOff!(I, 9)

                                Case 7
                                    Locate 9: Input " 7. FTM "; playerStats!(I, 5)

                                Case 8
                                    Locate 10: Input " 8. FTA "; playerOff!(I, 16)

                                Case 9
                                    Locate 11: Input " 9. ORB "; playerOff!(I, 19)

                                Case 10
                                    Locate 12: Input " 10. TOT RB "; playerOff!(I, 10)

                                Case 11
                                    Locate 13: Input "11. PF "; playerOff!(I, 12)

                                Case 12
                                    Locate 14: Input "12. AST "; playerOff!(I, 11)

                                Case 13
                                    Locate 15: Input "13. BLK "; playerOff!(I, 14)

                                Case 14
                                    Locate 16: Input "14. STL "; playerOff!(I, 13)

                                Case 15
                                    Locate 17: Input "15. 3 FGM "; playerStats!(I, 13)

                                Case 16
                                    Locate 18: Input "16. 3 FGA "; playerOff!(I, 17)

                                Case 17
                                    Locate 19: Input "17. DEFENSE (1-9) "; playerDef!(I)

                            End Select

                            playerStats!(I, 0) = playerOff!(I, 7)
                            playerStats!(I, 1) = playerOff!(I, 8)
                            playerStats!(I, 2) = playerOff!(I, 9)

                            If playerStats!(I, 2) = 0 Then
                                playerOff!(I, 0) = 0
                            Else
                                playerOff!(I, 0) = Int(playerStats!(I, 3) / playerStats!(I, 2) * 100 + .5)
                            End If

                            playerStats!(I, 4) = playerOff!(I, 16)

                            If playerStats!(I, 4) = 0 Then
                                playerOff!(I, 1) = 0
                            Else
                                playerOff!(I, 1) = Int(playerStats!(I, 5) / playerStats!(I, 4) * 100 + .5)
                            End If

                            playerStats!(I, 6) = playerOff!(I, 19)
                            playerStats!(I, 7) = playerOff!(I, 10)
                            playerStats!(I, 8) = playerOff!(I, 12)
                            playerStats!(I, 9) = playerOff!(I, 11)
                            playerStats!(I, 10) = playerOff!(I, 14)
                            playerStats!(I, 11) = playerOff!(I, 13)
                            playerStats!(I, 12) = playerOff!(I, 17)

                            If playerStats!(I, 12) = 0 Then
                                playerOff!(I, 18) = 0
                                playerRatings!(I, 2) = 0
                            Else
                                playerOff!(I, 18) = Int(playerStats!(I, 13) / playerStats!(I, 12) * 100 + .5)
                                playerRatings!(I, 2) = playerOff!(I, 18)
                            End If

                        End If

                    Loop Until B9 = 0

                Else

                    For I1 = I To 13:
                        playerNames$(I1) = "XXX"
                    Next

                    Exit For

                End If 'Done checking for XXX

            Next I

            X1 = Val(Mid$(A$, 1, 2))
            'Adjusts team and player FTA
            'based on FT rules changes
            '-- original code was between 1 and 72
            '-- but this would impact years
            '-- between 2000 and 2052
            If (X1 >= 39 And X1 <= 72) Then

                If X1 >= 53 And X1 <= 57 Then 
                    '1953 - 1957
                    '70% of entered value
                    X2 = .7
                Else
                    '1939 - 1952, 1958 - 1972
                    '83% of entered value
                    X2 = .83
                End If

                T7 = Int((T7 * X2) + .5)
                BF = Int((BF * X2) + .5)

                For I = 0 To 13:
                    playerOff!(I, 16) = Int((playerOff!(I, 16) * X2) + .5)
                Next I

            End If

            X9 = 0

            X1 = T2 - ((BE * T2) / 100) + BF - ((BF * BG) / 100)
            X2 = T1 - ((T1 * T6) / 100) + T7 - ((T7 * T8) / 100)
            RD = (T3 - X1 + X2 - T4) / (L5 * 2): HS = T3 - X1 + X2 - T4
            M7 = ((35 + RD) / L2)

            If H = 2 Then M7 = ((44 + RD) / L2)

            For I = 0 To 13

                If playerNames$(I) <> "XXX" Then
                    X9 = X9 + playerOff!(I, 10) + playerOff!(I, 9)
                End If

            Next I

            For I = 0 To 13

                If playerNames$(I) <> "XXX" Then

                    If playerOff!(I, 8) <> 0 And H = 0 Then M = 40 / (playerOff!(I, 8) / playerOff!(I, 7))
                    If playerOff!(I, 8) <> 0 And H = 1 Then M = 32 / (playerOff!(I, 8) / playerOff!(I, 7))
                    If playerOff!(I, 8) = 0 Then M = (X9 / 5) / (playerOff!(I, 9) + playerOff!(I, 10)): MI = (playerOff!(I, 9) + playerOff!(I, 10)) / (X9 / 5)
                    If playerOff!(I, 8) = 0 And M < 1.1 Then M = 1.1
                    playerOff!(I, 2) = Int((((playerOff!(I, 10) / playerOff!(I, 7)) * M) * M7) + .5)
                    X = Int(((((playerOff!(I, 10) / playerOff!(I, 7)) * M) * M7) * 10) + .5)

                    If playerOff!(I, 19) = 0 Then
                        playerRatings!(I, 4) = Int((X * .35) + .5)
                    Else
                        playerRatings!(I, 4) = Int((X * (playerOff!(I, 19) / playerOff!(I, 10))) + .5)
                    End If

                    playerOff!(I, 2) = X - playerRatings!(I, 4)

                    'Assists / Game
                    If inputQQ(0) = 0 Then playerOff!(I, 4) = Int(((playerOff!(I, 11) / playerOff!(I, 7)) * M) * (L6 / (T0 / L5)) + .5)
                    'Steals / Game
                    If inputQQ(2) = 0 Then playerOff!(I, 5) = Int(((playerOff!(I, 13) / playerOff!(I, 7)) * M) + .5)
                    'Blocks / Game
                    If inputQQ(3) = 0 Then playerOff!(I, 6) = Int (((playerOff!(I, 14) / playerOff!(I, 7)) * M) + .5)

                End If

            Next I

            X = L1 + L2 + 20

            inputY! = (T1 + T2 + T3 + T4 + L3 + BI) / (2 * L5)
            staminaRating% = Int(((2 * inputY!) - X) + .5)

            X1 = (T1 + T2) / (L5 * 2)
            X3 = (BC / 2) / X1
            X3 = Int((X3 * 100) + .5)
            BC = Int(X3 / 2)
            BY = Int(X3 / 2)
            X4 = Int(Rnd(1) * 2)

            If X3 - BC - BY > 0 And X4 = 0 Then BC = BC + 1
            If X3 - BC - BY > 0 And X4 = 1 Then BY = BY + 1

            U7 = Int((46 - L4) / 2)
            defFGPctAdj% = OE - L4 + BY - U7

            For I = 0 To 13

                If playerNames$(I) <> "XXX" Then
                    playerOff!(I, 3) = (playerOff!(I, 9) + playerOff!(I, 10)) / X9
                    playerOff!(I, 3) = Int((100 * playerOff!(I, 3)) + .5)
                End If

            Next I

            V% = Int(((W0 / T2) + .005) * 100)
            def3FGA_Adj = V% - Q7

            'DEF 3FGA/3FGA ADJ
            '-- Here is where 100 is being added to the value
            '-- This is why our team files and viewer later
            '-- will show values as 95-99
            If def3FGA_Adj < 0 Then def3FGA_Adj = 100 + def3FGA_Adj

            def3FG_Adj = W1 - Q8

            'DEF 3FG %
            '-- Here is where 100 is being added to the value
            '-- This is why our team files and viewer later
            '-- will show values as 95-99
            If def3FG_Adj < 0 Then def3FG_Adj = 100 + def3FG_Adj

            For I = 0 To 13

                If playerNames$(I) <> "XXX" Then

                    If playerOff!(I, 9) <> 0 Then playerRatings!(I, 3) = Int(playerOff!(I, 17) / playerOff!(I, 9) * 100 + .5)
                    If playerOff!(I, 8) <> 0 And H = 0 Then M = 40 / (playerOff!(I, 8) / playerOff!(I, 7))
                    If playerOff!(I, 8) <> 0 And H = 1 Then M = 32 / (playerOff!(I, 8) / playerOff!(I, 7))
                    If playerOff!(I, 8) = 0 Then M = (X9 / 5) / (playerOff!(I, 9) + playerOff!(I, 10))
                    If playerOff!(I, 8) = 0 And M < 1 Then M = 1

                    B3 = (playerOff!(I, 12) / playerOff!(I, 7)) * M: B3 = Int((B3 * 10) + .5)
                    playerRatings!(I, 5) = B3
                    B3 = playerOff!(I, 9) * (playerOff!(I, 0) / 100): B4 = playerOff!(I, 17) * (playerOff!(I, 18) / 100)

                    If playerOff!(I, 9) - playerOff!(I, 17) <> 0 Then playerOff!(I, 0) = ((B3 - B4) / (playerOff!(I, 9) - playerOff!(I, 17))) + .005
                    playerOff!(I, 0) = Int((playerOff!(I, 0) * 100) + .5)

                End If

            Next I

            For I = 0 To 13

                If playerNames$(I) <> "XXX" Then
                    playerOff!(I, 0) = playerOff!(I, 0) - BC + U7
                End If

            Next I

            leagueRatings%(4) = 99

            For I = 0 To 13

                If playerNames$(I) <> "XXX" Then

                    X! = ((playerOff!(I, 9) * playerOff!(I, 0)) / 100)
                    MI = (playerOff!(I, 8) / playerOff!(I, 7)) / 40
                    If playerOff!(I, 8) = 0 Then MI = (playerOff!(I, 9) + playerOff!(I, 10)) / (X9 / 5)
                    X! = X! * .1
                    If MI > 1 And playerOff!(I, 8) = 0 Then MI = 1
                    X! = X! + ((playerOff!(I, 7) * 1.64) * MI)
                    X! = ((playerOff!(I, 16) - X!) / 2)
                    X! = Int(((X! / (playerOff!(I, 9) + X!)) * 100) + .5)
                    If X! < 0 Then X! = 0
                    playerRatings!(I, 6) = X!

                End If

            Next I

            inputY! = BH / L5
            X! = (inputY! - 7) / .4
            X! = Int(X! + .5)

            If X! < 0 Then leagueRatings%(0) = 100 + X!
            If X! > 0 Then leagueRatings%(0) = X!

            R! = 1200 - (((T1 + T2) / (L5 * 2)) * 4)
            R! = R! / 6
            inputY! = (BJ - BH) / L5
            X! = (inputY! / R!) - .01
            X! = Int((X! * 100) + .5)
            leagueRatings%(1) = X!

            inputY! = (BK - T9) / L5
            X! = (inputY! / R!) - .01
            X! = Int((X! * 100) + .5)
            X! = X! - 4

            If X! < 0 Then leagueRatings%(2) = 100 + X!
            If X! > 0 Then leagueRatings%(2) = X!

            R1! = Int(((T2 * BE) / 100) + .5)
            X! = (R1! * .1) + (L5 * 8.2)
            X! = (BF - X!) / 2
            X! = (X! / (T2 + X!)) - .09
            X! = Int((X! * 100) + .5)

            If X! > 0 Then leagueRatings%(3) = X!
            If X! < 0 Then leagueRatings%(3) = X! + 100

            '---------------
            ' Assists
            '---------------
            If inputQQ(0) = 1 Then

                Cls
                Color 14
                Print "INPUT ASSIST RATINGS"
                Color 15
                Print "AA SHOULD = APPROX. 3 WHEN FINISHED"
                Print "FORWARDS,CENTERS ARE USUALLY 0,1,2,3 ; GUARDS ARE USUALLY 3,4,5,6"

                Print

                For I = 0 To 13
                    Locate I + 6, 1
                    Print I; " "; playerNames$(I);
                    Locate I + 6, 20
                    Input E0(I)
                Next I

                AA = 0

                For I1 = 0 To 13
                    AA = AA + E0(I1) * Int(playerOff!(I1, 3))
                Next I1

                Locate 20, 1
                Print "AA = "; AA / 100

                Do

                    Do
                        Locate 21, 1
                        Print "ANY CHANGE (0-13) (N=NO CHANGE) "
                        Input I$
                    Loop Until B9 >= 0 And B9 <= 13 Or UCase$(I$) = "N"

                    If UCase$(I$) <> "N" Then
                        I = Val(I$)
                        Locate I + 6, 20
                        Input E0(I)
                    End If

                Loop Until UCase$(I$) = "N"

                For I = 0 To 13
                    playerOff!(I, 4) = playerOff!(I, 4) + E0(I)
                Next I

            End If

            '---------------
            ' Foul Commit
            '---------------
            If inputQQ(1) = 1 Then

                Cls
                Color 14
                Print "INPUT FOUL COMMITING RATINGS"
                Color 15
                Print "PF SHOULD = APPROX. 40 WHEN FINISHED"

                Print "AVG. RATING = 40 ; RANGE = 15 TO 99"
                Print

                For I = 0 To 13
                    Locate I + 6, 1
                    Print I; " "; playerNames$(I);
                    Locate I + 6, 20
                    Input E0(I)
                Next I

                PF = 0
                TP = 0

                For I1 = 0 To 13
                    If playerNames$(I1) <> "XXX" Then
                        TP = TP + 1
                        PF = PF + E0(I1)
                    End If
                Next I1

                Locate 20, 1
                Print "PF = "; PF / TP

                Do

                    Do
                        Locate 21, 1
                        Print "ANY CHANGE (0-13) (N=NO CHANGE) "
                        Input I$
                    Loop Until B9 >= 0 And B9 <= 13 Or UCase$(I$) = "N"

                    If UCase$(I$) <> "N" Then
                        I = Val(I$)
                        Locate I + 6, 20
                        Input E0(I)
                    End If

                Loop Until UCase$(I$) = "N"

                For I = 0 To 13
                    playerRatings!(I, 5) = E0(I)
                Next I

            End If

            '---------------
            ' Steals & TOs
            '---------------
            If inputQQ(2) = 1 Then

                HQ = T1 - T2
                HR = T7 - BF
                HW = HR / 350
                If HW > 1 Then HW = 1
                HW = ((HW * 35) + 32) / 100
                If HW < 0 Then HW = HW * -1
                HR = HR * HW
                HS = HS / 2
                HX = Int((HR + HS - HQ) + .5)
                HX = Int(((HX / L5) * 10) + .5)

                Cls
                leagueRatings%(4) = 99

                Color 14
                Print "INPUT STEALS & TURNOVERS RATINGS."
                Color 15
                Print "THIS TEAMS ST SHOULD = APPROX. "; HX / 10
                Print "IND. STL RATINGS: AVG=1 RANGE = 0 TO 4"
                Print "TM OFF STL ADJ: AVG=0 RANGE=-10 TO 10"
                Print "TM TURNOVR RATING: AVG=4 RANGE = 1 TO 7"
                Print "TM DEF TRNOVR ADJ: AVG=0 RANGE -4 TO 4"

                For I = 0 To 13
                    Locate I + 7, 1
                    Print I; " "; playerNames$(I);
                    Locate I + 7, 20
                    Input E0(I)
                Next I

                Locate 20, 1: Input "X-TM OFF STL ADJ   "; E0(14)
                Locate 21, 1: Input "Y-TM TURNOVER ADJ  "; E0(15)
                Locate 22, 1: Input "Z-TM DEF TURNOVR ADJ"; E0(16)

                ST = 0

                For I = 0 To 13:
                    ST = ST + E0(I) * Int(playerOff!(I, 3))
                Next I

                ST = ST / 20
                SY = 7 + E0(14) * .4
                SV = (R * E0(15)) / 100
                SW = ((4 + E0(16)) * R) / 100
                ST = ST - SY - SV + SW

                Locate 10, 26
                Print "ST = "; ST;

                Do

                    Do
                        Locate 23, 1
                        Print "ANY CHANGE (0-Z) (N = NO CHANGE) ";
                        Input I$
                        I = Val(I$)
                        If UCase$(I$) = "X" Then I = 14
                        If UCase$(I$) = "Y" Then I = 15
                        If UCase$(I$) = "Z" Then I = 16
                    Loop Until I >= 0 And I <= 16 Or UCase$(I$) = "N"

                    If UCase$(I$) <> "N" Then

                        If I <= 13 Then

                            Locate I + 7, 20
                            Input E0(I)

                        Else

                            Select Case UCase$(I$)

                                Case "X":
                                    Locate 20, 1: Input "X-TM OFF STL ADJ   "; E0(14)

                                Case "Y":
                                    Locate 21, 1: Input "Y-TM TURNOVER ADJ  "; E0(15)

                                Case "Z":
                                    Locate 22, 1: Input "Z-TM DEF TURNOVR ADJ"; E0(16)

                            End Select

                        End If

                    End If

                Loop Until UCase$(I$) = "N"

                For I = 0 To 13
                    playerOff!(I, 5) = playerOff!(I, 5) + E0(I)
                Next I

                If E0(14) < 0 Then E0(14) = 100 + E0(14)
                If E0(16) < 0 Then E0(16) = 100 + E0(16)

                For I = 0 To 2
                    leagueRatings%(I) = E0(I + 14)
                Next

                I$ = GetKeyPress$

            End If

            '---------------
            ' Block Ratings
            '---------------
            If inputQQ(3) = 1 Then

                Cls
                Color 14
                Print "INPUT BLOCK RATINGS"
                Color 15
                Print "GUARDS ARE USUALLY 0,1 ; FORWARDS,CENTERS ARE USUALLY 0,1,2,3"
                Print "THE GREAT SHOT BLOCKERS ARE USUALLY 5,6,7"
                Print

                For I = 0 To 13
                    Locate I + 6, 1
                    Print I; " "; playerNames$(I);
                    Locate I + 6, 20
                    Input E0(I)
                Next I

                Do
                    Do
                        Locate 20, 1
                        Print "ANY CHANGE (0-13) (N=NO CHANGE) "
                        Input I$
                    Loop Until B9 >= 0 And B9 <= 13 Or UCase$(I$) = "N"

                    If UCase$(I$) <> "N" Then
                        I = Val(I$)
                        Locate I + 6, 20
                        Input E0(I)
                    End If

                Loop Until UCase$(I$) = "N"

            End If

            For I = 0 To 13:
                playerOff!(I, 6) = playerOff!(I, 6) + E0(I)
            Next I

            Call SaveTeam(1, A$, yearNumber$, teamNumber)

        End If 'Done checking for EXIT

    End If 'We ESC'd out of the first menu

End Sub


'----------------------------------------
'          SaveTeam Subroutine
'----------------------------------------
'...explanation...
Sub SaveTeam (newID%, teamName$, yearNumber$, teamIdx)

    Shared def3FG_Adj, def3FGA_Adj
    Shared teamAttendance%, defFGPctAdj%, staminaRating%
    Shared mascot$, coach$, arenaName$

    Shared BE, BF, BG, BI, BJ, BK, BH
    Shared L3, L5, T1, T2, T6, T7, T8, T3, T4, T0, T9
    Shared W0, W1, XX

    Shared teamRatings%()

    Shared playerOff!(), playerDef!(), leagueRatings%()
    Shared playerStats!(), playerRatings!()

    Shared playerNames$(), position$()

    teamYear$ = RTrim$(yearNumber$)

    Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$ For Random As #1 Len = TEAM_SIZE_BYTES

    fileLength& = LOF(1)

    If newID% = 1 Or teamIdx = 0 Then teamIdx = fileLength& / TEAM_SIZE_BYTES + 1

    Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 2 As Q$(14), 2 As Q$(15), 2 As Q$(16), 2 As Q$(17), 2 As Q$(18), 2 As Q$(19), 602 As X$

    For I = 0 To 13
        Field #1, 53 + I * 43 As X$, 15 As Q$(I * 15 + 20), 2 As Q$(I * 15 + 21), 2 As Q$(I * 15 + 22), 2 As Q$(I * 15 + 23), 2 As Q$(I * 15 + 24), 2 As Q$(I * 15 + 25), 2 As Q$(I * 15 + 26), 2 As Q$(I * 15 + 27), 2 As Q$(I * 15 + 28), 2 As Q$(I * 15 + 29), 2 As Q$(I * 15 + 30), 2 As Q$(I * 15 + 31), 2 As Q$(I * 15 + 32), 2 As Q$(I * 15 + 33), 2 As Q$(I * 15 + 34), TEAM_SIZE_BYTES - 53 - 43 - I * 43 As X$
    Next

    LSet Q$(0) = RTrim$(teamName$)
    LSet Q$(1) = MKI$(staminaRating%)
    LSet Q$(2) = MKI$(defFGPctAdj%)
    LSet Q$(3) = MKI$(def3FG_Adj)
    LSet Q$(4) = MKI$(def3FGA_Adj)

    For I = 0 To 4:
        LSet Q$(I + 5) = MKI$(leagueRatings%(I))
    Next

    teamRatings%(2) = 999

    For I = 0 To 9:
        LSet Q$(I + 10) = MKI$(teamRatings%(I))
    Next

    For I = 0 To 13

        LSet Q$(I * 15 + 20) = playerNames$(I)

        For I1 = 0 To 6
            LSet Q$(I * 15 + I1 + 21) = MKI$(playerOff!(I, I1))
            LSet Q$(I * 15 + I1 + 28) = MKI$(playerRatings!(I, I1))
        Next

    Next

    Put #1, teamIdx

    Close 1

    Open diskPaths$(0) + DATA_FILE_NAME$ + "." + yearNumber$ For Random As #1 Len = DATA_SIZE_BYTES

    Field #1, 15 As Q$(0), 40 As Q$(1), 15 As Q$(2), 4 As Q$(3)

    For I = 0 To 13
        Field #1, 74 + I * 32 As X$, 2 As Q$(I * 16 + 4), 2 As Q$(I * 16 + 5), 2 As Q$(I * 16 + 6), 2 As Q$(I * 16 + 7), 2 As Q$(I * 16 + 8), 2 As Q$(I * 16 + 9), 2 As Q$(I * 16 + 10), 2 As Q$(I * 16 + 11), 2 As Q$(I * 16 + 12), 2 As Q$(I * 16 + 13), 2 As Q$(I * 16 + 14), 2 As Q$(I * 16 + 15), 2 As Q$(I * 16 + 16), 2 As Q$(I * 16 + 17), 2 As Q$(I * 16 + 18), 2 As Q$(I * 16 + 19)
        Field #1, 522 As X$, 2 As Q$(228), 2 As Q$(229), 2 As Q$(230), 2 As Q$(231), 2 As Q$(232), 2 As Q$(233), 2 As Q$(234), 2 As Q$(235), 2 As Q$(236), 2 As Q$(237), 2 As Q$(238), 2 As Q$(239), 2 As Q$(240), 2 As Q$(241), 2 As Q$(242), 2 As Q$(243), 2 As Q$(244), 2 As Q$(245), 2 As Q$(246), 2 As Q$(247), 2 As Q$(248), 2 As Q$(249)
    Next

    LSet Q$(0) = coach$
    LSet Q$(1) = arenaName$
    LSet Q$(2) = mascot$
    LSet Q$(3) = MKL$(teamAttendance%)

    For I = 0 To 13

        LSet Q$(I * 16 + 4) = MKI$(playerDef!(I))
        LSet Q$(I * 16 + 5) = position$(I)

        For I1 = 0 To 13
            LSet Q$(I * 16 + I1 + 6) = MKI$(playerStats!(I, I1))
        Next

    Next

    LSet Q$(228) = MKI$(L5)
    LSet Q$(229) = MKI$(T1)
    LSet Q$(230) = MKI$(T2)
    LSet Q$(231) = MKI$(T6)
    LSet Q$(232) = MKI$(BE)
    LSet Q$(233) = MKI$(T7)
    LSet Q$(234) = MKI$(BF)
    LSet Q$(235) = MKI$(T8)
    LSet Q$(236) = MKI$(BG)
    LSet Q$(237) = MKI$(T3)
    LSet Q$(238) = MKI$(T4)
    LSet Q$(239) = MKI$(T0)
    LSet Q$(240) = MKI$(XX)
    LSet Q$(241) = MKI$(L3)
    LSet Q$(242) = MKI$(BI)
    LSet Q$(243) = MKI$(BJ)
    LSet Q$(244) = MKI$(BK)
    LSet Q$(245) = MKI$(T9)
    LSet Q$(246) = MKI$(BH)
    LSet Q$(247) = MKI$(W0)
    LSet Q$(248) = MKI$(W1)

    LSet Q$(249) = MKI$(999)

    Put #1, teamIdx

    Close 1

    T9 = 0

    For I = 0 To 13

        For I1 = 0 To 19
            playerOff!(I, I1) = 0
        Next I1

        For I1 = 0 To 6
            playerRatings!(I, 0) = 0
        Next I1

        For I1 = 0 To 13
            playerStats!(I, I1) = 0
        Next I1

        playerDef!(I) = 0
        position$(I) = ""

    Next I

    mascot$ = ""
    arenaName$ = ""
    coach$ = ""
    teamAttendance% = 0

    Erase N$

End Sub
