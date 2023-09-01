'----------------------------------------
'          EditStats Subroutine
'----------------------------------------
'This subroutine loads in stats for a TEAM
'and gives the user an interface for making
'edits to the stats.
Sub EditStats ()

    _Title "Courtside College Basketball - Stats Input"

    Shared statsW0!(), statsW1!()
    Shared statsZ!(), statsZ1!()

    Shared statPlyrNames$()
    Dim Z2(14, 14)

    Call ChooseTeam (YN$, NTMS, teamNbr, team$)
	statFile$ = nameStatFile$(team$)

    If Not _FileExists(diskPaths$(1) + statFile$) Then

        MsgText$ = "NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU"
        BoxWidth = 24
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

    Else

        Open diskPaths$(1) + statFile$ For Input As #1

        Input #1, A$

        For I = 0 To 13: 
            Input #1, statPlyrNames$(I)

            For J = 0 To 13: 
                Input #1, Z2(I, J)
            Next J

            Input #1, statsW0!(I)

            Input #1, statsW1!(I)

        Next I

        For L = 0 To 15: 
            Input #1, statsZ!(L)
        Next L

        For L = 0 To 15: 
            Input #1, statsZ1!(L)
        Next L

        Close #1

        '---------------
        ' Team Stats
        '---------------
        Color 15, 0
        Cls

        X$ = "OFFENSIVE TEAM STATS"
        Call TITLE_STAT_HEADER(X$)

        Locate 3, 1
        Print "1. STAT FILE NAME "; statFile$
        Print "2. TM MINUTES "; 
        Print Using "####"; statsZ!(0)
        Print "3. TM FGA "; statsZ!(1)
        Print "4. TM FGM "; statsZ!(2)
        Print "5. TM 3FGA "; statsZ!(14)
        Print "6. TM 3FGM "; statsZ!(13)
        Print "7. TM FTA "; statsZ!(3)
        Print "8. TM FTM "; statsZ!(4)
        Print "9. TM OFF RB  "; statsZ!(12)
        Print "10. TM TOT RB "; statsZ!(5)
        Print "11. TM FOULS "; statsZ!(6)
        Print "12. TM STEALS "; statsZ!(7)
        Print "13. TM ASSISTS "; statsZ!(8)
        Print "14. TM BLOCKS "; statsZ!(9)
        Print "15. TM TURNOVERS "; statsZ!(11)
        Print "16. TM POINTS  "; statsZ!(10)

        Do

            Do
                Locate 22, 1
                Input "# TO CHANGE (0 TO CONTINUE) "; B9
            Loop Until B9 >= 0 And B9 <= 16

            Select Case B9

                Case 1
                    Locate 3, 1: Input "1. STAT FILE NAME "; statFile$

                Case 2
                    Locate 4, 1: Input "2. TM MINUTES "; statsZ!(0): statsZ!(0) = statsZ!(0) * 60

                Case 3
                    Locate 5, 1: Input "3. TM FGA "; statsZ!(1)

                Case 4
                    Locate 6, 1: Input "4. TM FGM "; statsZ!(2)

                Case 5
                    Locate 7, 1: Input "5. TM 3FGA "; statsZ!(14)

                Case 6
                    Locate 8, 1: Input "6. TM 3FGM "; statsZ!(13)

                Case 7
                    Locate 9, 1: Input "7. TM FTA "; statsZ!(3)

                Case 8
                    Locate 10, 1: Input "8. TM FTM "; statsZ!(4)

                Case 9
                    Locate 11, 1: Input "9. TM OFF RB  "; statsZ!(12)

                Case 10
                    Locate 12, 1: Input "10. TM TOT RB "; statsZ!(5)

                Case 11
                    Locate 13, 1: Input "11. TM FOULS "; statsZ!(6)

                Case 12
                    Locate 14, 1: Input "12. TM STEALS "; statsZ!(7)

                Case 13
                    Locate 15, 1: Input "13. TM ASSISTS "; statsZ!(8)

                Case 14
                    Locate 16, 1: Input "14. TM BLOCKS "; statsZ!(9)

                Case 15
                    Locate 17, 1: Input "15. TM TURNOVERS "; statsZ!(11)

                Case 16
                    Locate 18, 1: Input "16. TM POINTS  "; statsZ!(10)

            End Select

        Loop Until B9 >= 0

        '---------------
        ' Defense Stats
        '---------------
        Cls
        X$ = "DEFENSIVE TEAM STATS"
        Call TITLE_EDITSTATS(2, X$)

        Locate 3, 1
        Print "1. OPP FGA "; statsZ1!(1)
        Print "2. OPP FGM "; statsZ1!(2)
        Print "3. OPP 3FGA "; statsZ1!(14)
        Print "4. OPP 3FGM "; statsZ1!(13)
        Print "5. OPP FTA "; statsZ1!(3)
        Print "6. OPP FTM "; statsZ1!(4)
        Print "7. OPP OFF RB  "; statsZ1!(12)
        Print "8. OPP TOT RB "; statsZ1!(5)
        Print "9. OPP FOULS "; statsZ1!(6)
        Print "10. OPP STEALS "; statsZ1!(7)
        Print "11. OPP ASSISTS "; statsZ1!(8)
        Print "12. OPP BLOCKS "; statsZ1!(9)
        Print "13. OPP TURNOVERS "; statsZ1!(11)
        Print "14. OPP POINTS  "; statsZ1!(10)

        Do

            Do
                Locate 22, 1
                Input "# TO CHANGE (0 TO CONTINUE) "; B9
            Loop Until B9 >= 0 And B9 <= 14

            Select Case B9

                Case 1
                    Locate 3, 1: Input "1. OPP FGA "; statsZ1!(1)

                Case 2
                    Locate 4, 1: Input "2. OPP FGM "; statsZ1!(2)

                Case 3
                    Locate 5, 1: Input "3. OPP 3FGA "; statsZ1!(14)

                Case 4
                    Locate 6, 1: Input "4. OPP 3FGM "; statsZ1!(13)

                Case 5
                    Locate 7, 1: Input "5. OPP FTA "; statsZ1!(3)

                Case 6
                    Locate 8, 1: Input "6. OPP FTM "; statsZ1!(4)

                Case 7
                    Locate 9, 1: Input "7. OPP OFF RB  "; statsZ1!(12)

                Case 8
                    Locate 10, 1: Input "8. OPP TOT RB "; statsZ1!(5)

                Case 9
                    Locate 11, 1: Input "9. OPP FOULS "; statsZ1!(6)

                Case 10
                    Locate 12, 1: Input "10. OPP STEALS "; statsZ1!(7)

                Case 11
                    Locate 13, 1: Input "11. OPP ASSISTS "; statsZ1!(8)

                Case 12
                    Locate 14, 1: Input "12. OPP BLOCKS "; statsZ1!(9)

                Case 13
                    Locate 15, 1: Input "13. OPP TURNOVERS "; statsZ1!(11)

                Case 14
                    Locate 16, 1: Input "14. OPP POINTS  "; statsZ1!(10)

            End Select

        Loop Until B9 = 0


        '---------------
        ' Player Stats
        '---------------
        Do

            Cls
            X$ = "CHANGE PLAYERS"
            Call TITLE_STAT_HEADER(X$)

            For I = 0 To 13
                Color 7
                If I / 2 = Int(I / 2) Then Color 15
                Print (I + 1);
                Print " "; inputA$(I)
            Next I

            Print

            Do
                Input "# TO CHANGE (1-13) (0 TO CONTINUE) "; I
            Loop Until I >= 0 And I <= 13

            If I > 0 Then

                I = I - 1

                Cls

                Locate 2, 1: PRINT "PLAYER - " + Str$(I + 1)
                Locate 3, 1
                Print "1. NAME "; statPlyrNames$(I)
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
                Print "15. 3FGA "; statsW1!(I)
                Print "16. 3FGM "; statsW0!(I)
                Print "17. HI-GAME (PTS) "; Z2(I, 13)

                Do

                    Do
                        Locate 22, 1
                        Input "# TO CHANGE (0 TO CONTINUE) "; B9
                    Loop Until B9 >= 0 And B9 <= 17

                    Select Case B9

                        Case 1
                            Locate 3, 1: Input "1. NAME "; statPlyrNames$(I)

                        Case 2
                            Locate 4, 1: Input "2. GAMES "; Z2(I, 0)

                        Case 3
                            Locate 5, 1: Input "3. MINUTES "; Z2(I, 10)
                            Z2(I, 10) = Z2(I, 10) * 60

                        Case 4
                            Locate 6, 1: Input "4. FGA "; Z2(I, 1)

                        Case 5
                            Locate 7, 1: Input "5. FGM "; Z2(I, 2)

                        Case 6
                            Locate 8, 1: Input "6. FTA "; Z2(I, 3)

                        Case 7
                            Locate 9, 1: Input "7. FTM "; Z2(I, 4)

                        Case 8
                            Locate 10, 1: Input "8. OFF REB "; Z2(I, 12)

                        Case 9
                            Locate 11, 1: Input "9. TOT REB  "; Z2(I, 5)

                        Case 10
                            Locate 12, 1: Input "10. FOULS "; Z2(I, 6)

                        Case 11
                            Locate 13, 1: Input "11. STEALS "; Z2(I, 7)

                        Case 12
                            Locate 14, 1: Input "12. ASSISTS "; Z2(I, 8)

                        Case 13
                            Locate 15, 1: Input "13. BLOCKS "; Z2(I, 9)

                        Case 14
                            Locate 16, 1: Input "14. TURNOVERS "; Z2(I, 11)

                        Case 15
                            Locate 17, 1: Input "15. 3FGA "; statsW1!(I)

                        Case 16
                            Locate 18, 1: Input "16. 3FGM "; statsW0!(I)

                        Case 17
                            Locate 19, 1: Input "17. HI-GAME (PTS) "; Z2(I, 13)

                    End Select

                Loop Until B9 = 0

            End If

        Loop Until I = 0


        '---------------
        ' Save Edits
        '---------------
        Open diskPaths$(1) + statFile$ For Output As #2

        Print #2, A$

        For I = 0 To 13:

            Print #2, statPlyrNames$(I)

            For J = 0 To 13: 
                Print #2, Z2(I, J)
            Next J

            Print #2, statsW0!(I)

            Print #2, statsW1!(I)

        Next I

        For L = 0 To 15: Print #2, statsZ!(L): Next L

        For L = 0 To 15: Print #2, statsZ1!(L): Next L

        Close #2

    End If

End Sub

