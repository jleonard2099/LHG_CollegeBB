'----------------------------------------
'           InputTeam Subroutine
'----------------------------------------
'This subroutine assists the user with the
'steps for entering a new team.
Sub InputTeam ()

    _Title "Courtside College Basketball - Team Input"

    Shared nbrGmsPlayed!, opp3FGA!, opp3FGAPct!, oppAsst!, oppBlocks!
    Shared oppFGPct!, oppFouls!, oppFGA!, oppFTA!, oppFTPct!
    Shared oppReb!, oppSteal!, oppTO!
    Shared tm3FGA!, tm3FGAPct!, tmAsst!, tmBlocks!, tmFouls!
    Shared tmFGA!, tmFGPct!, tmFTA!, tmFTPct!
    Shared tmReb!, tmSteal!, tmTO!

    Shared def3FG_Adj, defthreeFGA_Adj
    Shared teamAttendance&, defFGPctAdj!, staminaRating!
    Shared mascot$, coach$, arenaName$

    Dim inputRatings(16)
    Dim inputQQ(3), inputX$(3)

    Shared teamRatings()

    Shared playerOff!(), playerDef!(), leagueRatings()
    Shared playerStats!(), playerRatings!()

    Shared playerNames$(), position$()

    inputX$(0) = "COLLEGE BASKETBALL"
    inputX$(1) = "H S BASKETBALL"

    Cls

    Do

        Locate 2
        Color 12: Print "SELECT TYPE OF TEAMS YOU ARE ENTERING"

        Print
        Color 14: Print "(0) ";
        Color 15: Print "COLLEGE BASKETBALL"

        Print
        Color 14: Print "(1) ";
        Color 15: Print "HIGH SCHOOL BASKETBALL"

        I$ = GetKeyPress$

        If I$ <> Chr$(27) Then
            choice = Val(I$)
        End If

    Loop Until I$ = Chr$(27) Or choice = 0 Or choice = 1

    If I$ <> Chr$(27) Then

        'L6 = average assists / team ???
        'L7 is not used; average steals / game?
        If choice = 0 Then
            'College BB
            L7 = 6
            L6 = 14
            tmFouls! = 20
        Else
            'High School BB
            L7 = 5
            L6 = 11
            tmFouls! = 16
        End If

        '---------------
        ' Season Stats
        '---------------
        Cls

        Color 12
        Locate 2
        Print inputX$(choice)

        Locate , 32
        Color 14
        Print "LEAGUE INFORMATION"
        Color 15

        Locate 5, 1: Input "1. SEASON FGA/G "; leagFGAPerGame!
        Locate 5, 17: Print " "

        Locate 6, 1: Input "2. SEASON REBOUNDS/GAME "; leagRebPerGame!
        Locate 6, 25: Print " "

        Locate 7, 1: Input "3. SEASON FG % "; leagFGPct!
        Locate 7, 16: Print " "

        Locate 8, 1: Input "4. SEASON STEALS/GAME "; leagStlsPerGame!
        Locate 8, 23: Print " "

        Locate 9, 1: Input "5. SEASON TURNOVERS/GAME "; leagTOPerGame!
        Locate 9, 26: Print " "

        Locate 10, 1: Input "6. SEASON 3 FGA/FGA % "; leag3FGAPct!
        Locate 10, 23: Print " "

        Locate 11, 1: Input "7. SEASON 3 FG % "; leag3FGPct!
        Locate 11, 18: Print " "

        teamRatings(2) = 999

        Do

            Do
                Locate 22, 1
                Color 11
                Input "# TO CHANGE (0 TO CONTINUE)"; B9
                Color 15
            Loop Until B9 >= 0 And B9 <= 7

            Select Case B9

                Case 1:
                    Locate 5, 17: Print Space$(10)
                    Locate 5, 1: Input "1. SEASON FGA/G "; leagFGAPerGame!
                    Locate 5, 17: Print " "

                Case 2:
                    Locate 6, 25: Print Space$(10)
                    Locate 6, 1: Input "2. SEASON REBOUNDS/GAME "; leagRebPerGame!
                    Locate 6, 25: Print " "

                Case 3:
                    Locate 7, 16: Print Space$(10)
                    Locate 7, 1: Input "3. SEASON FG % "; leagFGPct!
                    Locate 7, 16: Print " "

                Case 4
                    Locate 8, 23: Print Space$(10)
                    Locate 8, 1: Input "4. SEASON STEALS/GAME "; leagStlsPerGame!
                    Locate 8, 23: Print " "

                Case 5
                    Locate 9, 26: Print Space$(10)
                    Locate 9, 1: Input "5. SEASON TURNOVERS/GAME "; leagTOPerGame!
                    Locate 9, 26: Print " "

                Case 6
                    Locate 10, 23: Print Space$(10)
                    Locate 10, 1: Input "6. SEASON 3 FGA/FGA % "; leag3FGAPct!
                    Locate 10, 23: Print " "

                Case 7
                    Locate 11, 18: Print Space$(10)
                    Locate 11, 1: Input "7. SEASON 3 FG % "; leag3FGPct!
                    Locate 11, 18: Print " "

            End Select

            Locate 22, 27
            Print Space$(10)

        Loop Until B9 = 0


        '---------------
        ' Required Stats
        '---------------
        Do

            Cls

            Locate 2,
            Print "YOU MUST HAVE THE FOLLOWING TEAM & INDIVIDUAL STATS: "
            Print "FGA, FG %, FTA, FT %, REBOUNDS"
            Print
            Print "IF YOU ARE INPUTTING A TEAM WITH ANY OF THESE MISSING,"
            Print "TYPE 'N' AT THE CORRESPONDING PROMPT AND THE PROGRAM"
            Print "WILL ALLOW YOU TO INPUT CUSTOM RATINGS"
            Print
            Color 12: Print "DO YOU HAVE THE FOLLOWING INFORMATION :"

            Locate 11, 1
            Color 14: Print "(1) ";
            Color 15: Print "TEAM/INDIVID ASSISTS? (Y/N): ";

            Do
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = Chr$(27)

            If I$ <> Chr$(27) Then

                Print UCase$(I$)

                If UCase$(I$) = "Y" Then
                    inputQQ(0) = 1
                End If
                If UCase$(I$) = "N" Then
                    inputQQ(0) = 0
                End If

                Locate 13, 1
                Color 14: Print "(2) ";
                Color 15: Print "PERSONAL FOULS? (Y/N): ";

                Do
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = Chr$(27)

                If I$ <> Chr$(27) Then

                    Print UCase$(I$)

                    If UCase$(I$) = "Y" Then
                        inputQQ(1) = 1
                    End If
                    If UCase$(I$) = "N" Then
                        inputQQ(1) = 0
                    End If

                    Locate 15, 1
                    Color 14: Print "(3) ";
                    Color 15: Print "STEALS & TURNOVERS? (Y/N): ";

                    Do
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = Chr$(27)

                    If I$ <> Chr$(27) Then

                        Print UCase$(I$)

                        If UCase$(I$) = "Y" Then
                            inputQQ(2) = 1
                        End If
                        If UCase$(I$) = "N" Then
                            inputQQ(2) = 0
                        End If

                        Locate 17, 1
                        Color 14: Print "(4) ";
                        Color 15: Print "TEAM / INDIVIDUAL BLOCKS? (Y/N): ";

                        Do
                            I$ = GetKeyPress$
                        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = Chr$(27)

                        If I$ <> Chr$(27) Then

                            Print UCase$(I$)

                            If UCase$(I$) = "Y" Then
                                inputQQ(3) = 1
                            End If
                            If UCase$(I$) = "N" Then
                                inputQQ(3) = 1
                            End If

                            Do
                                Locate 19, 1
                                Color 11
                                Print "ANY CHANGE?";
                                Color 15: Print " (Y/N)";
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
        Color 12
        Print inputX$(choice)

        Locate 2, 32
        Color 14
        Print "TEAM INFORMATION"

        Color 15
        Locate 3: Input " 0. TEAM # (BLANK TO AUTO-ASSIGN)"; teamNumber
        Locate 3, 34: Print " "
        Locate 4: Input " 1. DISK ID "; yearNumber$
        Locate 4, 13: Print " "
        Locate 5: Input " 2. TEAM NAME "; teamName$
        Locate 5, 15: Print " "
        Locate 6: Input " 3. TEAM NICKNAME "; mascot$
        Locate 6, 19: Print " "
        Locate 7: Input " 4. STRENGTH OF SCHEDULE ADJ "; schedStr
        Locate 7, 30: Print " "
        Locate 8: Input " 5. HOMECOURT FACTOR (0-6) "; teamRatings(6)
        Locate 8, 28: Print " "
        Locate 9: Input " 6. TEXT COLOR (0-15) "; teamRatings(0)
        Locate 9, 23: Print " "
        Locate 10: Input " 7. BACKGROUND COLOR (0-7) "; teamRatings(1)
        Locate 10, 28: Print " "
        'Print "TEAM APPEARANCE: ";
        Locate 11, 5
        Color teamRatings(0), teamRatings(1)
        Print teamName$ + " " + mascot$
        Color 15, 0

        Locate 3, 40: Input "21. GAMES PLAYED"; nbrGmsPlayed!
        Locate 3, 56: Print " "
        Locate 4, 40: Input "22. WINS"; teamRatings(7)
        Locate 4, 48: Print " "
        Locate 5, 40: Input "23. LOSSES"; teamRatings(8)
        Locate 5, 50: Print " "
        Locate 6, 40: Input "24. POWER RATING"; teamRatings(9)
        Locate 6, 56: Print " "
        Locate 7, 40: Input "25. COACH"; coach$
        Locate 7, 49: Print " "
        Locate 8, 40: Input "26. ARENA"; arenaName$
        Locate 8, 49: Print " "
        Locate 9, 40: Input "27. AVG ATTENDANCE"; teamAttendance&
        Locate 9, 58: Print " "

        Color 14
        Locate 13, 15: Print " TEAM"
        Locate 13, 30: Print " OPPONENTS"
        Color 15

        Locate 14, 1: Print " 8. FGA ";
        Locate 14, 14: Input tmFGA!
        Locate 14, 14: Print " "
        Locate 14, 29: Input oppFGA!
        Locate 14, 29: Print " "

        Locate 15, 1: Print " 9. FG % ";
        Locate 15, 14: Input tmFGPct!
        Locate 15, 14: Print " "
        Locate 15, 29: Input oppFGPct!
        Locate 15, 29: Print " "

        Locate 16, 1: Print "10. FTA ";
        Locate 16, 14: Input tmFTA!
        Locate 16, 14: Print " "
        Locate 16, 29: Input oppFTA!
        Locate 16, 29: Print " "

        Locate 17, 1: Print "11. FT % ";
        Locate 17, 14: Input tmFTPct!
        Locate 17, 14: Print " "
        Locate 17, 29: Input oppFTPct!
        Locate 17, 29: Print " "

        Locate 18, 1: Print "12. RB ";
        Locate 18, 14: Input tmReb!
        Locate 18, 14: Print " "
        Locate 18, 29: Input oppReb!
        Locate 18, 29: Print " "

        Locate 19, 1: Print "13. AST ";
        Locate 19, 14: Input tmAsst!
        Locate 19, 14: Print " "
        Locate 19, 29: Input oppAsst!
        Locate 19, 29: Print " "

        Locate 20, 1: Print "14. FOULS ";
        Locate 20, 14: Input tmFouls!
        Locate 20, 14: Print " "
        Locate 20, 29: Input oppFouls!
        Locate 20, 29: Print " "

        Locate 21, 1: Print "15. TO ";
        Locate 21, 14: Input tmTO!
        Locate 21, 14: Print " "
        Locate 21, 29: Input oppTO!
        Locate 21, 29: Print " "

        Locate 22, 1: Print "16. STLS ";
        Locate 22, 14: Input tmSteal!
        Locate 22, 14: Print " "
        Locate 22, 29: Input oppSteal!
        Locate 22, 29: Print " "

        Locate 23, 1: Print "17. BLOCKS ";
        Locate 23, 14: Input tmBlocks!
        Locate 23, 14: Print " "
        Locate 23, 29: Input oppBlocks!
        Locate 23, 29: Print " "

        Locate 24, 1: Print "18. 3 FGA ";
        Locate 24, 14: Input tm3FGA!
        Locate 24, 14: Print " "
        Locate 24, 29: Input opp3FGA!
        Locate 24, 29: Print " "

        Locate 25, 1: Print "19. 3 FG % ";
        Locate 25, 14: Input tm3FGAPct!
        Locate 25, 14: Print " "
        Locate 25, 29: Input opp3FGAPct!
        Locate 25, 29: Print " "

        Locate 26: Input "20. OPP 2FG% "; opp2FGPct!
        Locate 26, 14: Print " "

        'FOULS
        'Average 20 / game if none entered
        If oppFouls! = 0 Then oppFouls! = 20 * nbrGmsPlayed!
        If tmFouls! = 0 Then tmFouls! = 20 * nbrGmsPlayed!

        Do

            Do

                Color 11
                Locate 28, 25
                Input "# TO CHANGE (0 TO CONTINUE)"; B9
                Color 15
            Loop Until B9 >= 0 And B9 <= 27

            Select Case B9

                Case 1
                    Locate 4, 1: Input " 1. DISK ID "; yearNumber$
                    Locate 4, 13: Print " "

                Case 2
                    Locate 5, 1: Input " 2. TEAM NAME "; teamName$
                    Locate 5, 15: Print " "

                Case 3
                    Locate 6, 1: Input " 3. TEAM NICKNAME "; mascot$
                    Locate 6, 19: Print " "

                Case 4
                    Locate 7, 30: Print Space$(10)
                    Locate 7, 1: Input " 4. STRENGTH OF SCHEDULE ADJ "; schedStr
                    Locate 7, 30: Print " "

                Case 5
                    Locate 8, 28: Print Space$(10)
                    Locate 8, 1: Input " 5. HOMECOURT FACTOR (0-6) "; teamRatings(6)
                    Locate 8, 28: Print " "

                Case 6
                    Locate 9, 23: Print Space$(10)
                    Locate 9, 1: Input " 6. TEXT COLOR (0-15) "; teamRatings(0)
                    Locate 9, 23: Print " "

                    If teamRatings(0) >= 0 And teamRatings(1) >= 0 Then
                        'Print "TEAM APPEARANCE: ";
                        Locate 11, 5
                        Color teamRatings(0), teamRatings(1)
                        Print teamName$ + " " + mascot$
                        Color 15, 0
                    End If

                Case 7
                    Locate 10, 28: Print Space$(10)
                    Locate 10, 1: Input " 7. BACKGROUND COLOR (0-7) "; teamRatings(1)
                    Locate 10, 28: Print " "

                    If teamRatings(0) >= 0 And teamRatings(1) >= 0 Then
                        'Print "TEAM APPEARANCE: ";
                        Locate 11, 5
                        Color teamRatings(0), teamRatings(1)
                        Print teamName$ + " " + mascot$
                        Color 15, 0
                    End If

                Case 8
                    Locate 14, 1: Print " 8. FGA";
                    Locate 14, 14: Print Space$(10)
                    Locate 14, 14: Input tmFGA!
                    Locate 14, 14: Print " "
                    Locate 14, 29: Print Space$(10)
                    Locate 14, 29: Input oppFGA!
                    Locate 14, 29: Print " "

                Case 9
                    Locate 15, 1: Print " 9. FG %";
                    Locate 15, 15: Print Space$(10)
                    Locate 15, 14: Input tmFGPct!
                    Locate 15, 15: Print " "
                    Locate 15, 29: Print Space$(10)
                    Locate 15, 29: Input oppFGPct!
                    Locate 15, 29: Print " "

                Case 10
                    Locate 16, 1: Print "10. FTA";
                    Locate 16, 14: Print Space$(10)
                    Locate 16, 14: Input tmFTA!
                    Locate 16, 14: Print " "
                    Locate 16, 29: Print Space$(10)
                    Locate 16, 29: Input oppFTA!
                    Locate 16, 29: Print " "

                Case 11
                    Locate 17, 1: Print "11. FT %";
                    Locate 17, 14: Print Space$(10)
                    Locate 17, 14: Input tmFTPct!
                    Locate 17, 14: Print " "
                    Locate 17, 29: Print Space$(10)
                    Locate 17, 29: Input oppFTPct!
                    Locate 17, 29: Print " "

                Case 12
                    Locate 18, 1: Print "12. RB";
                    Locate 18, 14: Print Space$(10)
                    Locate 18, 14: Input tmReb!
                    Locate 18, 14: Print " "
                    Locate 18, 29: Print Space$(10)
                    Locate 18, 29: Input oppReb!
                    Locate 18, 29: Print " "

                Case 13
                    Locate 19, 1: Print "13. AST";
                    Locate 19, 14: Print Space$(10)
                    Locate 19, 14: Input tmAsst!
                    Locate 19, 14: Print " "
                    Locate 19, 29: Print Space$(10)
                    Locate 19, 29: Input oppAsst!
                    Locate 19, 29: Print " "

                Case 14
                    Locate 20, 1: Print "14. FOULS";
                    Locate 20, 14: Print Space$(10)
                    Locate 20, 14: Input tmFouls!
                    Locate 20, 14: Print " "
                    Locate 20, 29: Print Space$(10)
                    Locate 20, 29: Input oppFouls!
                    Locate 20, 29: Print " "

                Case 15
                    Locate 21, 1: Print "15. TO";
                    Locate 21, 14: Print Space$(10)
                    Locate 21, 14: Input tmTO!
                    Locate 21, 14: Print " "
                    Locate 21, 29: Print Space$(10)
                    Locate 21, 29: Input oppTO!
                    Locate 21, 29: Print " "

                Case 16
                    Locate 22, 1: Print "16. STLS";
                    Locate 22, 14: Print Space$(10)
                    Locate 22, 14: Input tmSteal!
                    Locate 22, 14: Print " "
                    Locate 22, 29: Print Space$(10)
                    Locate 22, 29: Input oppSteal!
                    Locate 22, 29: Print " "

                Case 17
                    Locate 23, 1: Print "17. BLOCKS";
                    Locate 23, 14: Print Space$(10)
                    Locate 23, 14: Input tmBlocks!
                    Locate 23, 14: Print " "
                    Locate 23, 29: Print Space$(10)
                    Locate 23, 29: Input oppBlocks!
                    Locate 23, 29: Print " "

                Case 18
                    Locate 24, 1: Print "18. 3 FGA";
                    Locate 24, 14: Print Space$(10)
                    Locate 24, 14: Input tm3FGA!
                    Locate 24, 14: Print " "
                    Locate 24, 29: Print Space$(10)
                    Locate 24, 29: Input opp3FGA!
                    Locate 24, 29: Print " "

                Case 19
                    Locate 25, 1: Print "19. 3 FG %";
                    Locate 25, 14: Print Space$(10)
                    Locate 25, 14: Input tm3FGAPct!
                    Locate 25, 14: Print " "
                    Locate 25, 29: Input opp3FGAPct!
                    Locate 25, 29: Print Space$(10)
                    Locate 25, 29: Print " "

                Case 20
                    Locate 26, 9: Print Space$(10)
                    Locate 26, 1: Input "OPP 2FG% "; opp2FGPct!
                    Locate 26, 9: Print " "

                Case 21
                    Locate 3, 56: Print Space$(10)
                    Locate 3, 40: Input "21. GAMES PLAYED"; nbrGmsPlayed!
                    Locate 3, 56: Print " "

                Case 22
                    Locate 4, 48: Print Space$(10)
                    Locate 4, 40: Input "22. WINS"; teamRatings(7)
                    Locate 4, 48: Print " "

                Case 23
                    Locate 5, 50: Print Space$(10)
                    Locate 5, 40: Input "23. LOSSES"; teamRatings(8)
                    Locate 5, 50: Print " "

                Case 24
                    Locate 6, 56: Print Space$(10)
                    Locate 6, 40: Input "24. POWER RATING"; teamRatings(9)
                    Locate 6, 56: Print " "

                Case 25
                    Locate 7, 40: Input "25. COACH "; coach$
                    Locate 7, 50: Print " "

                Case 26
                    Locate 8, 40: Input "26. ARENA "; arenaName$
                    Locate 8, 50: Print " "

                Case 27
                    Locate 9, 60: Print Space$(10)
                    Locate 9, 40: Input "27. AVG ATTENDANCE "; teamAttendance&
                    Locate 9, 60: Print " "

            End Select

            Locate 28, 25
            Print Space$(10)

        Loop Until B9 = 0


        '---------------
        ' Players
        '---------------
        'Average FGA/G for this team (Off + Def)
        teamRatings(3) = CInt(((tmFGA! + oppFGA!) / (nbrGmsPlayed! * 2)))
        teamRatings(4) = leagFGAPerGame!
        teamRatings(5) = nbrGmsPlayed!

        If choice = 1 Then
            F3 = 32
        Else
            F3 = 40
        End If

        For I = 0 To 13

            Cls

            Color 12
            Locate 2
            Print inputX$(choice)

            Color 14
            Locate 3, 35: Print "PLAYER - "; I + 1

            Locate 5
            Print "ENTER PLAYER STATS"
            Color 15

            Input " 1. NAME (XXX TO QUIT) "; playerNames$(I)
            Locate 6, 23: Print " "

            If playerNames$(I) <> "XXX" Then

                Locate 7, 1: Input " 2. POSITION"; position$(I)
                Locate 7, 13: Print " "
                Locate 8, 1: Input " 3. GAMES"; playerOff!(I, 7)
                Locate 8, 10: Print " "
                playerRatings!(I, 0) = playerOff!(I, 7)

                Locate 9, 1: Input " 4. MP"; playerOff!(I, 8)
                Locate 9, 7: Print " "
                Locate 10, 1: Input " 5. FGM"; playerStats!(I, 3)
                Locate 10, 8: Print " "

                Locate 11, 1: Input " 6. FGA"; playerOff!(I, 9)
                Locate 11, 8: Print " "
                playerRatings!(I, 1) = playerOff!(I, 9)

                Locate 12, 1: Input " 7. 3 FGM"; playerStats!(I, 13)
                Locate 12, 10: Print " "
                Locate 13, 1: Input " 8. 3 FGA"; playerOff!(I, 17)
                Locate 13, 10: Print " "

                Locate 14, 1: Input " 9. FTM"; playerStats!(I, 5)
                Locate 14, 8: Print " "
                Locate 15, 1: Input "10. FTA"; playerOff!(I, 16)
                Locate 15, 8: Print " "
                Locate 16, 1: Input "11. ORB"; playerOff!(I, 19)
                Locate 16, 8: Print " "
                Locate 17, 1: Input "12. TOT RB"; playerOff!(I, 10)
                Locate 17, 11: Print " "
                Locate 18, 1: Input "13. AST"; playerOff!(I, 11)
                Locate 18, 8: Print " "
                Locate 19, 1: Input "14. STL"; playerOff!(I, 13)
                Locate 19, 8: Print " "
                Locate 20, 1: Input "15. BLK"; playerOff!(I, 14)
                Locate 20, 8: Print " "
                Locate 21, 1: Input "16. PF"; playerOff!(I, 12)
                Locate 21, 7: Print " "
                'Locate 20, 1: Input "17. DEFENSE (1-9)"; playerDef!(I)

                Do

                    Do
                        Color 11
                        Locate 23, 1
                        Input "# TO CHANGE (0 TO CONTINUE) "; B9
                        Color 15
                    Loop Until B9 >= 0 And B9 <= 16

                    If B9 <> 0 Then

                        Select Case B9

                            Case 1
                                Locate 6, 24: Print Space$(20)
                                Locate 6, 1: Input " 1. NAME (XXX TO QUIT) "; playerNames$(I)
                                Locate 6, 24: Print " "

                            Case 2
                                Locate 7, 14: Print Space$(10)
                                Locate 7, 1: Input " 2. POSITION "; position$(I)
                                Locate 7, 14: Print " "

                            Case 3
                                Locate 8, 12: Print Space$(10)
                                Locate 8, 1: Input " 3. GAMES"; playerOff!(I, 7)
                                Locate 8, 12: Print " "

                                playerRatings!(I, 0) = playerOff!(I, 7)

                            Case 4
                                Locate 9, 9: Print Space$(10)
                                Locate 9, 1: Input " 4. MP"; playerOff!(I, 8)
                                Locate 9, 9: Print " "

                            Case 5
                                Locate 10, 10: Print Space$(10)
                                Locate 10, 1: Input " 5. FGM"; playerStats!(I, 3)
                                Locate 10, 10: Print " "

                            Case 6
                                Locate 11, 10: Print Space$(10)
                                Locate 11, 1: Input " 6. FGA"; playerOff!(I, 9)
                                playerRatings!(I, 1) = playerOff!(I, 9)
                                Locate 11, 10: Print " "

                            Case 7
                                Locate 12, 12: Print Space$(10)
                                Locate 12, 1: Input " 7. 3 FGM"; playerStats!(I, 13)
                                Locate 12, 12: Print " "

                            Case 8
                                Locate 13, 12: Print Space$(10)
                                Locate 13, 1: Input " 8. 3 FGA"; playerOff!(I, 17)
                                Locate 13, 12: Print " "

                            Case 9
                                Locate 14, 10: Print Space$(10)
                                Locate 14, 1: Input " 9. FTM"; playerStats!(I, 5)
                                Locate 14, 10: Print " "

                            Case 10
                                Locate 15, 10: Print Space$(10)
                                Locate 15, 1: Input "10. FTA"; playerOff!(I, 16)
                                Locate 15, 10: Print " "

                            Case 11
                                Locate 16, 10: Print Space$(10)
                                Locate 16, 1: Input "11. ORB"; playerOff!(I, 19)
                                Locate 16, 10: Print " "

                            Case 12
                                Locate 17, 13: Print Space$(10)
                                Locate 17, 1: Input "12. TOT RB"; playerOff!(I, 10)
                                Locate 17, 13: Print " "

                            Case 13
                                Locate 18, 10: Print Space$(10)
                                Locate 18, 1: Input "13. AST"; playerOff!(I, 11)
                                Locate 18, 10: Print " "

                            Case 14
                                Locate 19, 10: Print Space$(10)
                                Locate 19, 1: Input "14. STL"; playerOff!(I, 13)
                                Locate 19, 10: Print " "

                            Case 15
                                Locate 20, 10: Print Space$(10)
                                Locate 20, 1: Input "15. BLK"; playerOff!(I, 14)
                                Locate 20, 10: Print " "

                            Case 16
                                Locate 21, 9: Print Space$(10)
                                Locate 21, 1: Input "16. PF"; playerOff!(I, 12)
                                Locate 21, 9: Print " "

                            Case 17
                                'Locate 20, 20: Print Space$(10)
                                'Locate 20, 1: Input "17. DEFENSE (1-9) "; playerDef!(I)
                                'Locate 20, 20: Print " "

                        End Select

                        Locate 23, 29
                        Print Space$(10)

                    End If

                    playerStats!(I, 0) = playerOff!(I, 7)
                    playerStats!(I, 1) = playerOff!(I, 8)
                    playerStats!(I, 2) = playerOff!(I, 9)

                    If playerStats!(I, 2) = 0 Then
                        playerOff!(I, 0) = 0
                    Else
                        playerOff!(I, 0) = CInt(playerStats!(I, 3) / playerStats!(I, 2) * 100)
                    End If

                    playerStats!(I, 4) = playerOff!(I, 16)

                    If playerStats!(I, 4) = 0 Then
                        playerOff!(I, 1) = 0
                    Else
                        playerOff!(I, 1) = CInt(playerStats!(I, 5) / playerStats!(I, 4) * 100)
                    End If

                    playerStats!(I, 6) = playerOff!(I, 19)
                    playerStats!(I, 7) = playerOff!(I, 10)
                    playerStats!(I, 8) = playerOff!(I, 12)
                    playerStats!(I, 9) = playerOff!(I, 11)

                    'Blocks
                    playerStats!(I, 10) = playerOff!(I, 14)

                    'Steals
                    playerStats!(I, 11) = playerOff!(I, 13)

                    '3FGA
                    playerStats!(I, 12) = playerOff!(I, 17)

                    '3FG%
                    If playerStats!(I, 12) = 0 Then
                        playerOff!(I, 18) = 0
                        playerRatings!(I, 2) = 0
                    Else
                        playerOff!(I, 18) = CInt(playerStats!(I, 13) / playerStats!(I, 12) * 100)
                        playerRatings!(I, 2) = playerOff!(I, 18)
                    End If

                Loop Until B9 = 0

            Else

                For I1 = I To 13:
                    playerNames$(I1) = "XXX"
                Next

                Exit For

            End If 'Done checking for XXX

        Next I

        X1 = Val(Mid$(teamName$, 1, 2))

        'Adjusts team and player FTA
        'based on FT rules changes
        '-- original code was between 1 and 72
        '-- but this would impact years
        '-- between 2000 and 2052
        If (X1 >= 39 And X1 <= 72) Then

            If X1 >= 53 And X1 <= 57 Then
                '1953 - 1957
                '70% of entered value
                X2! = .7
            Else
                '1939 - 1952, 1958 - 1972
                '83% of entered value
                X2! = .83
            End If

            tmFTA! = CInt((tmFTA! * X2!))
            oppFTA! = CInt((oppFTA! * X2!))

            For I = 0 To 13:
                playerOff!(I, 16) = CInt((playerOff!(I, 16) * X2))
            Next I

        End If

        X9! = 0

        X1! = oppFGA! - ((oppFGPct! * oppFGA!) / 100) + oppFTA! - ((oppFTA! * oppFTPct!) / 100)
        X2! = tmFGA! - ((tmFGA! * tmFGPct!) / 100) + tmFTA! - ((tmFTA! * tmFTPct!) / 100)
        RD! = (tmReb! - X1! + X2! - oppReb!) / (nbrGmsPlayed! * 2)
        HS! = tmReb! - X1! + X2! - oppReb!
        M7! = ((35 + RD!) / leagRebPerGame!)

        If choice = 2 Then
            M7! = ((44 + RD!) / leagRebPerGame!)
        End If

        For I = 0 To 13

            If playerNames$(I) <> "XXX" Then
                'Sum of all player Blocks + Assists
                X9! = X9! + playerOff!(I, 10) + playerOff!(I, 9)
            End If

        Next I

        For I = 0 To 13

            If playerNames$(I) <> "XXX" Then

                'M is a factor of how often a player
                'contributes in a single game
                If playerOff!(I, 8) <> 0 Then
                    If choice = 0 Then
                        'College
                        M! = 40 / (playerOff!(I, 8) / playerOff!(I, 7))
                    End If
                    If choice = 1 Then
                        'High School
                        M! = 32 / (playerOff!(I, 8) / playerOff!(I, 7))
                    End If
                    plyrMinAdj! = M!
                Else
                    M! = (X9! / 5) / (playerOff!(I, 9) + playerOff!(I, 10))
                    MI! = (playerOff!(I, 9) + playerOff!(I, 10)) / (X9! / 5)
                    If M! < 1.1 Then plyrMinAdj! = 1.1
                End If

                'Tot Reb / 40 Min
                playerOff!(I, 2) = CInt((((playerOff!(I, 10) / playerOff!(I, 7)) * plyrMinAdj!) * M7!))

                '(Tot Reb / 40 Min) * 10
                X = CInt(((((playerOff!(I, 10) / playerOff!(I, 7)) * plyrMinAdj!) * M7!) * 10))

                '(Off Reb / 40 Min) * 10
                If playerOff!(I, 19) = 0 Then
                    playerRatings!(I, 4) = CInt((X * .35))
                Else
                    playerRatings!(I, 4) = CInt((X * (playerOff!(I, 19) / playerOff!(I, 10))))
                End If

                '(Def Reb / 40 Min) x 10
                playerOff!(I, 2) = X - playerRatings!(I, 4)

                'Assist Rating (based on Assits / Game)
                If inputQQ(0) = 1 Then
                    'Adjust entered assists / game based on time contribution and team
                    playerOff!(I, 4) = CInt(((playerOff!(I, 11) / playerOff!(I, 7)) * plyrMinAdj!) * (L6 / (tmAsst! / nbrGmsPlayed!)))
                End If

                'Steal Rating (based on Steals / Game)
                If inputQQ(2) = 1 Then
                    'Adjust entered steals / game based on time contribution
                    playerSteals = CInt(((playerOff!(I, 13) / playerOff!(I, 7)) * plyrMinAdj!))
                    playerOff!(I, 5) = playerSteals
                    playerDef!(I) = playerSteals
                End If

                'Block Rating (based on Blocks / Game)
                If inputQQ(3) = 1 Then
                    playerOff!(I, 6) = CInt(((playerOff!(I, 14) / playerOff!(I, 7)) * plyrMinAdj!))
                End If

            End If

        Next I

        oppShotContrib! = leagFGAPerGame! + leagRebPerGame! + 20
        gamePace! = (tmFGA! + oppFGA! + tmReb! + oppReb! + tmFouls! + oppFouls!) / (nbrGmsPlayed! * 2)
        staminaRating! = CInt(((2 * gamePace!) - oppShotContrib!))

        'Strength of Schedule
        sosAdj = (schedStr / 2) / ((tmFGA! + oppFGA!) / (nbrGmsPlayed! * 2))
        sosAdj = CInt((sosAdj * 100))

        schedStr = Int(sosAdj / 2)
        sosDupe = Int(sosAdj / 2)
        coin = Int(Rnd(1) * 2)

        If sosAdj - schedStr - sosDupe > 0 Then
            If coin = 0 Then
                schedStr = schedStr + 1
            Else
                sosDupe = sosDupe + 1
            End If
        End If

        'Def FG % Adj
        lgFgPctAdj = Int((46 - leagFGPct!) / 2)
        defFGPctAdj! = opp2FGPct! - leagFGPct! + sosDupe - lgFgPctAdj

        'Def 3 FG % Adj
        opp3FGAAdj = Int(((opp3FGA! / oppFGA!) + .005) * 100)
        defthreeFGA_Adj = opp3FGAAdj - leag3FGAPct!

        'DEF 3FGA/3FGA ADJ
        If defthreeFGA_Adj < 0 Then
            defthreeFGA_Adj = 100 + defthreeFGA_Adj
        End If

        def3FG_Adj = opp3FGAPct! - leag3FGPct!

        'DEF 3FG %
        If def3FG_Adj < 0 Then
            def3FG_Adj = 100 + def3FG_Adj
        End If

        For I = 0 To 13

            If playerNames$(I) <> "XXX" Then

                'Contribution Pct
                playerOff!(I, 3) = (playerOff!(I, 9) + playerOff!(I, 10)) / X9!
                playerOff!(I, 3) = CInt((100 * playerOff!(I, 3)))

                '3FGA / TOT FGA
                If playerOff!(I, 9) <> 0 Then
                    playerRatings!(I, 3) = CInt(playerOff!(I, 17) / playerOff!(I, 9) * 100)
                End If

                If M! < 1 Then plyrMinAdj! = 1

                'Fouls Rating (adjusted for player time contrib)
                B3 = (playerOff!(I, 12) / playerOff!(I, 7)) * plyrMinAdj!
                B3 = CInt((B3 * 10))
                playerRatings!(I, 5) = B3

                'FGA * FG%
                B3 = playerOff!(I, 9) * (playerOff!(I, 0) / 100)

                '3FGA * 3FG%
                B4 = playerOff!(I, 17) * (playerOff!(I, 18) / 100)

                'Adjusted FG%
                If playerOff!(I, 9) - playerOff!(I, 17) <> 0 Then
                    playerOff!(I, 0) = ((B3 - B4) / (playerOff!(I, 9) - playerOff!(I, 17))) + .005
                End If

                playerOff!(I, 0) = CInt((playerOff!(I, 0) * 100))

            End If

        Next I

        For I = 0 To 13

            If playerNames$(I) <> "XXX" Then
                'Adjust player FG% by SOS and lgFgPctAdj (how far league FG% is from 46)
                playerOff!(I, 0) = playerOff!(I, 0) - schedStr + lgFgPctAdj
            End If

        Next I

        leagueRatings(4) = 99

        For I = 0 To 13

            If playerNames$(I) <> "XXX" Then

                X! = ((playerOff!(I, 9) * playerOff!(I, 0)) / 100)
                MI = (playerOff!(I, 8) / playerOff!(I, 7)) / 40

                If playerOff!(I, 8) = 0 Then
                    MI = (playerOff!(I, 9) + playerOff!(I, 10)) / (X9! / 5)
                End If

                X! = X! * .1

                If MI > 1 And playerOff!(I, 8) = 0 Then
                    MI = 1
                End If
                X! = X! + ((playerOff!(I, 7) * 1.64) * MI)
                X! = ((playerOff!(I, 16) - X!) / 2)
                X! = CInt(((X! / (playerOff!(I, 9) + X!)) * 100))

                If X! < 0 Then X! = 0

                'Foul Draw rating
                playerRatings!(I, 6) = X!

            End If

        Next I

        'Off Steal Rating
        gameStls! = oppSteal! / nbrGmsPlayed!
        X! = CInt((gameStls! - 7) / .4)

        If X! < 0 Then leagueRatings(0) = 100 + X!
        If X! > 0 Then leagueRatings(0) = X!

        'Off TO Rating
        R! = 1200 - (((tmFGA! + oppFGA!) / (nbrGmsPlayed! * 2)) * 4)
        R! = R! / 6
        toAdj! = (tmTO! - oppSteal!) / nbrGmsPlayed!
        X! = (toAdj! / R!) - .01
        X! = CInt(X! * 100)
        leagueRatings(1) = X!

        'Def TO Adj
        toAdj! = (oppTO! - tmSteal!) / nbrGmsPlayed!
        X! = (toAdj! / R!) - .01
        X! = CInt(X! * 100)
        X! = X! - 4

        If X! < 0 Then leagueRatings(2) = 100 + X!
        If X! > 0 Then leagueRatings(2) = X!

        'Off Foul Adj
        homeScoreTeam! = CInt(((oppFGA! * oppFGPct!) / 100))
        X! = (homeScoreTeam! * .1) + (nbrGmsPlayed! * 8.2)
        X! = (oppFTA! - X!) / 2
        X! = (X! / (oppFGA! + X!)) - .09
        X! = CInt(X! * 100)

        If X! > 0 Then leagueRatings(3) = X!
        If X! < 0 Then leagueRatings(3) = X! + 100

        '---------------
        ' Assists
        '---------------
        If inputQQ(0) = 0 Then

            Cls

            Locate 2, 1
            Color 14
            Print "INPUT ASSIST RATINGS"

            Locate 4, 1
            Color 15
            Print "AA SHOULD BE ~3 WHEN FINISHED"

            Locate 6, 1
            Print "FORWARDS / CENTERS ARE USUALLY 0,1,2,3"
            Print "GUARDS ARE USUALLY 3,4,5,6"

            Print

            For I = 0 To 13
                Locate I + 9, 1
                Print I; " "; playerNames$(I);
                Locate I + 9, 20
                Input inputRatings(I)
            Next I

            AA = 0

            For I1 = 0 To 13
                AA = AA + inputRatings(I1) * Int(playerOff!(I1, 3))
            Next I1

            Locate 24, 1
            Print "AA = "; AA / 100

            Do

                Do
                    Color 11
                    Locate 26, 1
                    Color 15
                    Print "ANY CHANGE (0-13) (N=NO CHANGE) "
                    Input I$
                    B9 = Val(I$)
                Loop Until B9 >= 0 And B9 <= 13 Or UCase$(I$) = "N"

                If UCase$(I$) <> "N" Then
                    rating = B9
                    Locate rating + 9, 20
                    Input inputRatings(rating)
                End If

            Loop Until UCase$(I$) = "N"

            For I = 0 To 13
                playerOff!(I, 4) = playerOff!(I, 4) + inputRatings(I)
            Next I

        End If

        '---------------
        ' Foul Commit
        '---------------
        If inputQQ(1) = 0 Then

            Cls

            Locate 2, 1
            Color 14
            Print "INPUT FOUL COMMITING RATINGS"

            Locate 4, 1
            Color 15
            Print "PF SHOULD BE ~40 WHEN FINISHED"

            Locate 6, 1
            Print "AVG. RATING = 40 ; RANGE = 15 TO 99"
            Print

            For I = 0 To 13
                Locate I + 8, 1
                Print I; " "; playerNames$(I);
                Locate I + 8, 20
                Input inputRatings(I)
            Next I

            PF = 0
            TP = 0

            For I1 = 0 To 13
                If playerNames$(I1) <> "XXX" Then
                    TP = TP + 1
                    PF = PF + inputRatings(I1)
                End If
            Next I1

            Locate 23, 1
            Print "PF = "; PF / TP

            Do

                Do
                    Color 11
                    Locate 25, 1
                    Print "ANY CHANGE (0-13) (N=NO CHANGE) ";
                    Input I$
                    Color 15
                    B9 = Val(I$)
                Loop Until B9 >= 0 And B9 <= 13 Or UCase$(I$) = "N"

                If UCase$(I$) <> "N" Then
                    rating = B9
                    Locate rating + 8, 20
                    Input inputRatings(rating)
                End If

            Loop Until UCase$(I$) = "N"

            For I = 0 To 13
                playerRatings!(I, 5) = inputRatings(I)
            Next I

        End If

        '---------------
        ' Steals & TOs
        '---------------
        If inputQQ(2) = 0 Then

            HQ = tmFGA! - oppFGA!
            HR = tmFTA! - oppFTA!

            HW = HR / 350
            If HW > 1 Then HW = 1

            HW = ((HW * 35) + 32) / 100
            If HW < 0 Then HW = HW * -1

            HR = HR * HW
            HS = HS / 2
            HX = CInt(HR + HS - HQ)
            HX = Int((HX / nbrGmsPlayed!) * 10)

            Cls
            leagueRatings(4) = 99

            Locate 2, 1
            Color 14
            Print "INPUT STEALS & TURNOVERS RATINGS."

            Locate 4, 1
            Color 15
            Print "RATINGS FOR THIS TEAM SHOULD BE ~"; HX / 10
            Print "IND. STL RATINGS:  AVG=1 RANGE =  0 TO  4"
            Print "TM OFF STL ADJ:    AVG=0 RANGE= -10 TO 10"
            Print "TM TURNOVR RATING: AVG=4 RANGE =  1 TO  7"
            Print "TM DEF TRNOVR ADJ: AVG=0 RANGE = -4 TO  4"

            For I = 0 To 13
                Locate I + 10, 1
                Print I; " "; playerNames$(I);
                Locate I + 10, 20
                Input inputRatings(I)
            Next I

            Locate 24, 1: Input "X-TM OFF STL ADJ   "; inputRatings(14)
            Locate 25, 1: Input "Y-TM TURNOVER ADJ  "; inputRatings(15)
            Locate 26, 1: Input "Z-TM DEF TURNOVR ADJ"; inputRatings(16)

            ST = 0

            For I = 0 To 13:
                ST = ST + inputRatings(I) * Int(playerOff!(I, 3))
            Next I

            ST = ST / 20
            SY = 7 + inputRatings(14) * .4
            SV = (R * inputRatings(15)) / 100
            SW = ((4 + inputRatings(16)) * R) / 100
            ST = ST - SY - SV + SW

            Locate 10, 26
            Print "ST = "; ST;

            Do

                Do
                    Color 11
                    Locate 28, 1
                    Print "SELECT # (0-Z) TO CHANGE (N = NO CHANGE) ";
                    Input I$

                    Color 15
                    rating = Val(I$)
                    If UCase$(I$) = "X" Then rating = 14
                    If UCase$(I$) = "Y" Then rating = 15
                    If UCase$(I$) = "Z" Then rating = 16
                Loop Until rating >= 0 And rating <= 16 Or UCase$(I$) = "N"

                If UCase$(I$) <> "N" Then

                    If rating <= 13 Then

                        Locate rating + 7, 20
                        Input inputRatings(rating)

                    Else

                        Select Case UCase$(I$)

                            Case "X":
                                Locate 24, 1: Input "X-TM OFF STL ADJ   "; inputRatings(14)

                            Case "Y":
                                Locate 25, 1: Input "Y-TM TURNOVER ADJ  "; inputRatings(15)

                            Case "Z":
                                Locate 26, 1: Input "Z-TM DEF TURNOVR ADJ"; inputRatings(16)

                        End Select

                    End If

                End If

            Loop Until UCase$(I$) = "N"

            For I = 0 To 13
                playerOff!(I, 5) = playerOff!(I, 5) + inputRatings(I)
            Next I

            If inputRatings(14) < 0 Then inputRatings(14) = 100 + inputRatings(14)
            If inputRatings(16) < 0 Then inputRatings(16) = 100 + inputRatings(16)

            For I = 0 To 2
                leagueRatings(I) = inputRatings(I + 14)
            Next

            I$ = GetKeyPress$

        End If

        '---------------
        ' Block Ratings
        '---------------
        If inputQQ(3) = 0 Then

            Cls

            Locate 2, 1
            Color 14
            Print "INPUT BLOCK RATINGS"

            Locate 4, 1
            Color 15
            Print "GUARDS ARE USUALLY 0,1"
            Print "FORWARDS,CENTERS ARE USUALLY 0,1,2,3"
            Print "EXCELLENT SHOT BLOCKERS ARE USUALLY 5,6,7"

            For I = 0 To 13
                Locate I + 8, 1
                Print I; " "; playerNames$(I);
                Locate I + 8, 20
                Input inputRatings(I)
            Next I

            Do
                Do
                    Color 11
                    Locate 22, 1
                    Print "ANY CHANGE (0-13) (N=NO CHANGE) "
                    Color 15
                    Input I$
                    B9 = Val(I$)
                Loop Until B9 >= 0 And B9 <= 13 Or UCase$(I$) = "N"

                If UCase$(I$) <> "N" Then
                    rating = B9
                    Locate rating + 8, 20
                    Input inputRatings(rating)
                End If

            Loop Until UCase$(I$) = "N"

        End If

        For I = 0 To 13:
            playerOff!(I, 6) = playerOff!(I, 6) + inputRatings(I)
        Next I

        Call SaveTeam(1, teamName$, yearNumber$, teamNumber)

    End If 'We ESC'd out of the first menu

End Sub


'----------------------------------------
'          SaveTeam Subroutine
'----------------------------------------
'This subroutine saves the modifed team
'data (from corresponding arrays) to an
'appropriate spot in the select team file
Sub SaveTeam (newID, teamName$, yearNumber$, teamIdx)

    Shared nbrGmsPlayed!, opp3FGA!, opp3FGAPct!, oppAsst!, oppBlocks!
    Shared oppFGPct!, oppFouls!, oppFGA!, oppFTA!, oppFTPct!
    Shared oppReb!, oppSteal!, oppTO!
    Shared tm3FGA!, tm3FGAPct!, tmAsst!, tmBlocks!, tmFouls!
    Shared tmFGA!, tmFGPct!, tmFTA!, tmFTPct!
    Shared tmReb!, tmSteal!, tmTO!

    Shared def3FG_Adj, defthreeFGA_Adj
    Shared teamAttendance&, defFGPctAdj!, staminaRating!
    Shared mascot$, coach$, arenaName$

    Shared teamRatings()

    Shared playerOff!(), playerDef!(), leagueRatings()
    Shared playerStats!(), playerRatings!()

    Shared playerNames$(), position$()

    Call CSMsgBox("SAVING TEAM FILES", 20, PASS)

    teamYear$ = RTrim$(yearNumber$)

    Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$ For Random As #1 Len = TEAM_SIZE_BYTES

    fileLength& = LOF(1)

    If newID = 1 Or teamIdx = 0 Then
        teamIdx = (fileLength& / TEAM_SIZE_BYTES) + 1
    End If

    Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 2 As Q$(14), 2 As Q$(15), 2 As Q$(16), 2 As Q$(17), 2 As Q$(18), 2 As Q$(19), 602 As X$

    For I = 0 To 13
        Field #1, 53 + I * 43 As X$, 15 As Q$(I * 15 + 20), 2 As Q$(I * 15 + 21), 2 As Q$(I * 15 + 22), 2 As Q$(I * 15 + 23), 2 As Q$(I * 15 + 24), 2 As Q$(I * 15 + 25), 2 As Q$(I * 15 + 26), 2 As Q$(I * 15 + 27), 2 As Q$(I * 15 + 28), 2 As Q$(I * 15 + 29), 2 As Q$(I * 15 + 30), 2 As Q$(I * 15 + 31), 2 As Q$(I * 15 + 32), 2 As Q$(I * 15 + 33), 2 As Q$(I * 15 + 34), TEAM_SIZE_BYTES - 53 - 43 - I * 43 As X$
    Next

    LSet Q$(0) = RTrim$(teamName$)
    LSet Q$(1) = MKI$(staminaRating!)
    LSet Q$(2) = MKI$(defFGPctAdj!)
    LSet Q$(3) = MKI$(def3FG_Adj)
    LSet Q$(4) = MKI$(defthreeFGA_Adj)

    For I = 0 To 4:
        LSet Q$(I + 5) = MKI$(leagueRatings(I))
    Next

    teamRatings(2) = 999

    For I = 0 To 9:
        LSet Q$(I + 10) = MKI$(teamRatings(I))
    Next

    For I = 0 To 13

        LSet Q$(I * 15 + 20) = playerNames$(I)

        For I1 = 0 To 6
            LSet Q$(I * 15 + I1 + 21) = MKI$(playerOff!(I, I1))
            LSet Q$(I * 15 + I1 + 28) = MKI$(playerRatings!(I, I1))
        Next

    Next

    Put #1, teamIdx

    Close #1

    Open diskPaths$(0) + DATA_FILE_NAME$ + "." + yearNumber$ For Random As #2 Len = DATA_SIZE_BYTES

    Field #2, 15 As Q$(0), 40 As Q$(1), 15 As Q$(2), 4 As Q$(3)

    For I = 0 To 13
        Field #2, 74 + I * 32 As X$, 2 As Q$(I * 16 + 4), 2 As Q$(I * 16 + 5), 2 As Q$(I * 16 + 6), 2 As Q$(I * 16 + 7), 2 As Q$(I * 16 + 8), 2 As Q$(I * 16 + 9), 2 As Q$(I * 16 + 10), 2 As Q$(I * 16 + 11), 2 As Q$(I * 16 + 12), 2 As Q$(I * 16 + 13), 2 As Q$(I * 16 + 14), 2 As Q$(I * 16 + 15), 2 As Q$(I * 16 + 16), 2 As Q$(I * 16 + 17), 2 As Q$(I * 16 + 18), 2 As Q$(I * 16 + 19)
    Next

    Field #2, 522 As X$, 2 As Q$(228), 2 As Q$(229), 2 As Q$(230), 2 As Q$(231), 2 As Q$(232), 2 As Q$(233), 2 As Q$(234), 2 As Q$(235), 2 As Q$(236), 2 As Q$(237), 2 As Q$(238), 2 As Q$(239), 2 As Q$(240), 2 As Q$(241), 2 As Q$(242), 2 As Q$(243), 2 As Q$(244), 2 As Q$(245), 2 As Q$(246), 2 As Q$(247), 2 As Q$(248), 2 As Q$(249), 2 As Q$(250), 2 As Q$(251), 2 As Q$(252), 2 As Q$(253)

    LSet Q$(0) = coach$
    LSet Q$(1) = arenaName$
    LSet Q$(2) = mascot$
    LSet Q$(3) = MKL$(teamAttendance&)

    For I = 0 To 13

        LSet Q$(I * 16 + 4) = MKI$(playerDef!(I))
        LSet Q$(I * 16 + 5) = position$(I)

        For I1 = 0 To 13
            LSet Q$(I * 16 + I1 + 6) = MKI$(playerStats!(I, I1))
        Next

    Next

    LSet Q$(228) = MKI$(nbrGmsPlayed!)
    LSet Q$(229) = MKI$(tmFGA!)
    LSet Q$(230) = MKI$(oppFGA!)
    LSet Q$(231) = MKI$(tmFGPct!)
    LSet Q$(232) = MKI$(oppFGPct!)
    LSet Q$(233) = MKI$(tmFTA!)
    LSet Q$(234) = MKI$(oppFTA!)
    LSet Q$(235) = MKI$(tmFTPct!)
    LSet Q$(236) = MKI$(oppFTPct!)
    LSet Q$(237) = MKI$(tmReb!)
    LSet Q$(238) = MKI$(oppReb!)        
    LSet Q$(239) = MKI$(tmAsst!)
    LSet Q$(240) = MKI$(oppAsst!)
    LSet Q$(241) = MKI$(tmFouls!)       
    LSet Q$(242) = MKI$(oppFouls!)      
    LSet Q$(243) = MKI$(tmTO!)          
    LSet Q$(244) = MKI$(oppTO!)         
    LSet Q$(245) = MKI$(tmSteal!)       
    LSet Q$(246) = MKI$(oppSteal!)
    LSet Q$(247) = MKI$(opp3FGA!)
    LSet Q$(248) = MKI$(opp3FGAPct!)
    LSet Q$(249) = MKI$(tm3FGA!)
    LSet Q$(250) = MKI$(tm3FGAPct!)
    LSet Q$(251) = MKI$(tmBlocks!)
    LSet Q$(252) = MKI$(oppBlocks!)

    LSet Q$(253) = MKI$(999)

    Put #2, teamIdx

    Close #2

    Call CSMsgBox("", 0, PASS)

    tmSteal! = 0

    Erase playerDef!, playerOff!, playerRatings!, playerStats!
    Erase position$

    mascot$ = ""
    arenaName$ = ""
    coach$ = ""
    teamAttendance& = 0

End Sub
