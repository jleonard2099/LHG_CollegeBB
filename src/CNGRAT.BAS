Sub CNGRAT (userChoice)

    _Title "Courtside College Basketball - Change Team"

    Shared V1%, V2%
    Shared defFGPctAdj%, staminaRating%
    Shared A1$, C$, S$

    Shared A!(), DF!(), X!()
    Shared cngratA$(), P$()
    Shared L%(), T%(), T1%()

    Do
        ReDim menuChoices$(1 To 4)

        Cls

        Call TITLE
        menuChoices$(1) = "CHANGE OR EDIT AN EXISTING TEAM"
        menuChoices$(2) = "PRINT A TEAM ROSTER            "
        menuChoices$(3) = "VIEW A TEAM ROSTER             "
        menuChoices$(4) = "EXIT TO MAIN MENU              "
        Selection% = 1
        Count% = 4
        Start% = 1
        ExitCode% = 4
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 8
        Column% = 25

        If (userChoice <> 0) Then
            Selection% = userChoice
        Else
            Call box0(7, 24, 12, 56, 2, 15, 1)

            _MouseShow "DEFAULT"
            MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
            _MouseHide

            Color , 0
        End If

        If ExitCode% <> 27 Then

            Select Case Selection%
                Case 1:
                    Call GetTeam_CNGRAT(YN$, T1, A$)
                    Call EditTeam(YN$, T1, A$)

                Case 2:
                    Call GetTeam_CNGRAT(YN$, T1, A$)
                    'Send team roster to default printer
                    Cls
                    LPrint "TEAM ROSTER FOR "; A$
                    LPrint
                    LPrint "AVG SHOTS PER GAME = "; T%(3); "      DEF FG ADJ = "; defFGPctAdj%
                    LPrint "OPP PCT OF 3FG ATT ADJ = "; V2%; "  OPP 3FG PCT ADJ = "; V1%
                    LPrint
                    LPrint "DEF STEAL ADJ "; L%(0)
                    LPrint "OFF TURNOVER ADJ "; L%(1)
                    LPrint "DEF TURNOVER ADJ "; L%(2)
                    LPrint "OFF FOUL ADJ "; L%(3)
                    LPrint
                    LPrint "NAME           FG  FT ORB DRB  CX PS  D  B FC FD  3FG 3FG%":
                    LPrint

                    For I = 0 To 13
                        If cngratA$(I) <> "XXX" Then
                            LPrint cngratA$(I); Tab(15);
                            LPrint Using "### ### ### ### ### "; A!(I, 0); A!(I, 1); X!(I, 4); A!(I, 2); A!(I, 3);
                            LPrint Using "## ## ## ## ## ### ###"; A!(I, 4); A!(I, 5); A!(I, 6); X!(I, 5); X!(I, 6); X!(I, 3); X!(I, 2)
                        End If
                    Next I

                    LPrint
                    LPrint "FG- FIELD GOAL %                 B- BLOCK SHOT RATING"
                    LPrint "FT- FREE THROW %                FC- FOUL COMMIT RATING"
                    LPrint "RB- REBOUNDING RATING           FD- FOUL DRAWING RATING"
                    LPrint "CX- CONTRIBUTION FACTOR        3FG- 3FGA/TOTAL FGA"
                    LPrint "PS- PASSING RATING            3FG%- THREE POINT GOAL %"
                    LPrint " D- DEFENSIVE RATING"

					LPrint Chr$(12)
					
                Case 3:
                    Call GetTeam_CNGRAT(YN$, T1, A$)

                    'Send team roster to screen
                    Cls
                    Print "TEAM ROSTER FOR ";
                    Color T%(0), T%(1): Print A$
                    Color 14, 0
                    Print "    AVG SHOTS PER GAME = ";
                    Color 15, 0
                    Print T%(3);
                    Color 14, 0
                    Print " DEF FG ADJ = ";
                    Color 15, 0: Print defFGPctAdj%
                    Color 14, 0: Print "OPP PCT OF 3FG ATT ADJ = ";
                    Color 15, 0: Print V2%;
                    Color 14, 0: Print "  OPP 3FG PCT ADJ = ";
                    Color 15, 0: Print V1%;
                    Color 14, 0: Print "   DEF STEAL ADJ ";
                    Color 15, 0: Print L%(0)
                    Color 14, 0: Print "OFF TURNOVER ADJ ";
                    Color 15, 0: Print L%(1);
                    Color 14, 0: Print "    DEF TURNOVER ADJ ";
                    Color 15, 0: Print L%(2);
                    Color 14, 0: Print "    OFF FOUL ADJ ";
                    Color 15, 0: Print L%(3)
                    Print
                    Color 2, 0: Print "NAME           FG% FT% ORB DRB  CX PS  D  B  FC FD  3FG 3FG%"
                    Color 15, 0
                    For I = 0 To 13
                        If cngratA$(I) <> "XXX" Then
                            Print cngratA$(I); Tab(16);
                            Print Using "### ### ### ### ### "; A!(I, 0); A!(I, 1); X!(I, 4); A!(I, 2); A!(I, 3);
                            Print Using "## ## ## ### ##  ###  ###"; A!(I, 4); A!(I, 5); A!(I, 6); X!(I, 5); X!(I, 6); X!(I, 3); X!(I, 2)
                        End If
                    Next I
                    Print " B- BLOCK SHOT RATING           FC- FOUL COMMIT RATING"
                    Print "RB- REBOUNDING RATING           FD- FOUL DRAWING RATING"
                    Print "CX- CONTRIBUTION FACTOR        3FG- 3FGA/TOTAL FGA"
                    Print "PS- PASSING RATING               D- DEFENSIVE RATING"
                    I$ = GetKeyPress$

					'No need to implement a "View Again" here as it's just one page

                Case 4: Cls

            End Select

            userChoice = 0

        End If

        'If RN = 4 Then Cls: Exit Sub
        'If RN = 4 And D% >= 67 Then Exit Sub
        'On D1 GOTO 56, 56, 56, 880

    Loop Until Selection% = 4 Or ExitCode% = 27

End Sub

'----------------------------------------
'     GetTeam_CNGRAT Subroutine
'----------------------------------------
'... explain here ...
Sub GetTeam_CNGRAT (yearNumber$, teamIdx%, teamName$)

    Shared T%()

    Do

        Call GetDiskID_CNGRAT(yearNumber$)

        If yearNumber$ = "" Then
            Call SelectTeam_CNGRAT(teamIdx%, NTMS, yearNumber$, teamName$)
        Else
            Open diskPaths$(0) + "BASK." + yearNumber$ For Random As #1 Len = 655
            Y = LOF(1)
            Field #1, 15 As Q$(0), 640 As X$

            For I1 = 1 To Y / 655
                Get #1, I1
                teamNames$(I1) = Q$(0)
            Next
            Close 1

            NTMS = Y / 655
            TS% = Int((NTMS - 1) / 80)

            Call SelectTeam_CNGRAT(teamIdx%, NTMS, yearNumber$, teamName$)
            'We have at least 1 condition where we have to rewind to 1100
        End If

        If BO% <> 1 Then
            'This is more than calling the subroutine,
            '   it's starting from that point in the code!!!
            Locate 2, 33
            Color T%(0), T%(1): Print teamName$

            Color 15, 4

            Do
                Locate , 33
                Print "ANY CHANGE (YN)";
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        Else

            BO% = 0

        End If

    Loop Until UCase$(I$) = "N"

    Color 15, 0

End Sub

'----------------------------------------
'     EditTeam Subroutine
'----------------------------------------
'... explain here ...
Sub EditTeam (yearNumber$, teamIdx%, teamName$)

    Shared V1%, V2%
    Shared avgAttendance%, defFGPctAdj%, staminaRating%
    Shared A1$, C$, S$

    Shared L%(), T%(), T1%()
    Shared cngratA$(), P$()
    Shared A!(), DF!(), RS!(), X!()

    '-- these singular variables all need values
    '   Do
    Cls
    Color 14
    Print "CHANGE LEAGUE INFORMATION": X% = 2: Color 15
    Print " 1. TEAM NUMBER "; teamIdx%
    Print " 2. DISK ID "; yearNumber$
    Print " 3. TEAM NAME "; teamName$; Tab(40); "20. NICKNAME "; A1$
    Print " 4. TEAM STAMINA RATING "; staminaRating%; Tab(40); "21. COACH "; C$
    Print " 5. DEF FG% ADJ "; defFGPctAdj%
    Print " 6. DEF 3FGA/FGA ADJ "; V2%; Tab(40); "22. AVG ATTENDANCE "; avgAttendance%
    Print " 7. DEF 3FG% ADJ "; V1%; Tab(40); "23. WINS "; T%(7)
    Print " 8. OFF STEAL RATING "; L%(0); Tab(40); "24. LOSSES "; T%(8)
    Print " 9. OFF TURNOVER RATING "; L%(1); Tab(40); "25. POWER RATING "; T%(9)
    Print "10. DEF TURNOVER ADJ "; L%(2)
    Print "11. DEF FOUL ADJ "; L%(3)
    Print "12. ENTER 99 IF USING 6-9 "; L%(4)
    Print "13. TEXT COLOR (0-15) "; T%(0)
    Print "14. BACKGROUND (0-7) "; T%(1)
    Print "15. TEAM FGA/G "; T%(3)
    Print "16. LEAGUE FGA/G "; T%(4)
    Print "17. TEAM GAMES PLAYED "; T%(5)
    Print "18. HOMECOURT FACTOR "; T%(6)
    Print "19. ARENA NAME "; S$

    Do
        C1 = 1
        Locate 23, 1: Print "SELECT # TO CHANGE 1-24 (0 TO CONTINUE) ";
        Input B9

        If B9 <> 0 Then

            On B9 GOTO 147, 162, 163, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 164, 165, 166, 167, 168, 169, 170

            '       Select Case B9
            '           On B9 GOTO 147, 162, 163, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 164, 165, 166, 167, 168, 169, 170
            '

            '       End Select

            '   Loop Until C1 = 0

            '-- is the 1600 part necessary?
            '   isn't that all doing the same as below???
            '   (but without the C1 tracking
            ' IN FACT, it skips #'s 20+
            ' is that part of the deal?
            ' if you don't have the data?
            '145 Cls: Color 14: Print "CHANGE LEAGUE INFORMATION": X% = 2: Color 15

            147 Locate 2: Input " 1. TEAM NUMBER "; teamIdx%: If C1 = 1 Then 190
            162 Locate 3: Input " 2. DISK ID "; yearNumber$: If C1 = 1 Then 190
            163 Locate 4: Input " 3. TEAM NAME "; teamName$: If C1 = 1 Then 190
            148 Locate 5: Input " 4. TEAM STAMINA RATING "; staminaRating%: If C1 = 1 Then 190
            149 Locate 6: Input " 5. DEF FG% ADJ "; defFGPctAdj%: If C1 = 1 Then 190
            150 Locate 7: Input " 6. DEF 3FGA/FGA ADJ "; V2%: If C1 = 1 Then 190
            151 Locate 8: Input " 7. DEF 3FG% ADJ "; V1%: If C1 = 1 Then 190
            152 Locate 9: Input " 8. OFF STEAL RATING "; L%(0): If C1 = 1 Then 190
            153 Locate 10: Input " 9. OFF TURNOVER RATING "; L%(1): If C1 = 1 Then 190
            154 Locate 11: Input "10. DEF TURNOVER ADJ "; L%(2): If C1 = 1 Then 190
            155 Locate 12: Input "11. DEF FOUL ADJ "; L%(3): If C1 = 1 Then 190
            156 Locate 13: Input "12. ENTER 99 IF USING 6-9 "; L%(4): If C1 = 1 Then 190
            157 Locate 14: Input "13. TEXT COLOR (0-15) "; T%(0): If C1 = 1 Then 190
            158 Locate 15: Input "14. BACKGROUND (0-7) "; T%(1): If C1 = 1 Then 190
            159 Locate 16: Input "15. TEAM FGA/G "; T%(3): If C1 = 1 Then 190
            160 Locate 17: Input "16. LEAGUE FGA/G "; T%(4): If C1 = 1 Then 190
            161 Locate 18: Input "17. TEAM GAMES PLAYED "; T%(5): If C1 = 1 Then 190
            164 Locate 19: Input "18. HOMECOURT FACTOR "; T%(6): If C1 = 1 Then 190
            165 Locate 20: Input "19. ARENA NAME "; S$: If C1 = 1 Then 190
            166 Locate 4, 40: Input "20. NICKNAME "; A1$: If C1 = 1 Then 190
            167 Locate 5, 40: Input "21. COACH "; C$: If C1 = 1 Then 190
            168 Locate 7, 40: Input "22. AVG ATTENDANCE "; avgAttendance%: If C1 = 1 Then 190
            169 Locate 8, 40: Input "23. WINS "; T%(7): If C1 = 1 Then 190
            170 Locate 9, 40: Input "24. LOSSES "; T%(8): If C1 = 1 Then 190
            171 Locate 10, 40: Input "25. POWER RATING "; T%(9): If C1 = 1 Then 190

            190 '
            Locate 21, 1: Color T%(0), T%(1)
            Print "TEAM APPEARANCE": Color 15, 0

        End If

    Loop Until B9 = 0


    '---------------
    'Change Players
    '---------------
    200 '
    If T%(6) < 0 Then T%(6) = 0
    If T%(6) > 6 Then T%(6) = 6

    C1 = 0


    Cls

    Print "CHANGE PLAYERS"

    X% = 2

    For I = 0 To 6
        Locate I + 5, 5
        Print Using "##"; I + 1;
        Print " "; cngratA$(I); Tab(40);: Print Using "##"; I + 8;
        Print " "; cngratA$(I + 7)
    Next

    Print

    Print "# TO CHANGE (1-14) (0 TO CONTINUE) ";: Input I

    'to handle array from 0 to 13
    'I = I - 1

    If I >= 1 And I <= 14 Then

        Do

            1885 Cls: Print " 1. NAME "; cngratA$(I); Tab(40); "17. GAMES "; RS!(I, 0)
            Print " 2. POSITION "; P$(I); Tab(40); "18. MINUTES "; RS!(I, 1)
            Print " 3. GAMES PLAYED "; X!(I, 0); Tab(40); "19. FGM "; RS!(I, 3)
            Print " 4. ADJ FG% "; A!(I, 0); Tab(40); "20. FGA "; RS!(I, 2)
            Print " 5. FT% "; A!(I, 1); Tab(40); "21. FTM "; RS!(I, 5)
            Print " 6. OFF RB/40 MIN X 10 "; X!(I, 4); Tab(40); "22. FTA "; RS!(I, 4)
            Print " 7. DEF RB/40 MIN X 10 "; A!(I, 2); Tab(40); "23. OFF REB "; RS!(I, 6)
            Print " 8. ASSIST RATING "; A!(I, 4); Tab(40); "24. TOTAL REB "; RS!(I, 7)
            Print " 9. STEAL RATING "; A!(I, 5); Tab(40); "25. PER FOULS "; RS!(I, 8)
            Print "10. BLOCK RATING "; A!(I, 6); Tab(40); "26. ASSISTS "; RS!(I, 9)
            Print "11. FGA "; X!(I, 1); Tab(40); "27. BLOCKS "; RS!(I, 10)
            Print "12. FOUL COMMIT RATING "; X!(I, 5); Tab(40); "28. STEALS "; RS!(I, 11)
            Print "13. FOUL DRAWING RATING "; X!(I, 6); Tab(40); "29. 3 FGM "; RS!(I, 13)
            Print "14. 3FG% "; X!(I, 2); Tab(40); "30. 3 FGA "; RS!(I, 12)
            Print "15. 3FGA/TOT FGA "; X!(I, 3)
            Print "16. DEFENSIVE RATING (1-9) "; DF!(I)
            GoTo 250

            210 Locate 1: Input " 1. NAME "; cngratA$(I): If C1 = 1 Then 250
            224 Locate 2: Input " 2. POSITION "; P$(I): If C1 = 1 Then 250
            211 Locate 3: Input " 3. GAMES PLAYED "; X!(I, 0): If C1 = 1 Then 250
            212 Locate 4: Input " 4. ADJ FG% "; A!(I, 0): If C1 = 1 Then 250
            213 Locate 5: Input " 5. FT% "; A!(I, 1): If C1 = 1 Then 250
            214 Locate 6: Input " 6. OFF RB/40 MIN X 10 "; X!(I, 4): If C1 = 1 Then 250
            215 Locate 7: Input " 7. DEF RB/40 MIN X 10 "; A!(I, 2): If C1 = 1 Then 250
            216 Locate 8: Input " 8. ASSIST RATING "; A!(I, 4): If C1 = 1 Then 250
            217 Locate 9: Input " 9. STEAL RATING "; A!(I, 5): If C1 = 1 Then 250
            218 Locate 10: Input "10. BLOCK RATING "; A!(I, 6): If C1 = 1 Then 250
            219 Locate 11: Input "11. FGA "; X!(I, 1): If C1 = 1 Then 250
            220 Locate 12: Input "12. FOUL COMMIT RATING "; X!(I, 5): If C1 = 1 Then 250
            221 Locate 13: Input "13. FOUL DRAWING RATING "; X!(I, 6): If C1 = 1 Then 250
            222 Locate 14: Input "14. 3FG% "; X!(I, 2): If C1 = 1 Then 250
            223 Locate 15: Input "15. 3FGA/TOT FGA "; X!(I, 3): If C1 = 1 Then 250
            225 Locate 16: Input "16. DEFENSIVE RATING (1-9) "; DF!(I): If C1 = 1 Then 250
            226 Locate 1, 40: Input "17. GAMES "; RS!(I, 0): If C1 = 1 Then 250
            227 Locate 2, 40: Input "18. MINUTES "; RS!(I, 1): If C1 = 1 Then 250
            228 Locate 3, 40: Input "19. FGM "; RS!(I, 3): If C1 = 1 Then 250
            229 Locate 4, 40: Input "20. FGA "; RS!(I, 2): If C1 = 1 Then 250
            230 Locate 5, 40: Input "21. FTM "; RS!(I, 5): If C1 = 1 Then 250
            231 Locate 6, 40: Input "22. FTA "; RS!(I, 4): If C1 = 1 Then 250
            232 Locate 7, 40: Input "23. OFF REB "; RS!(I, 6): If C1 = 1 Then 250
            233 Locate 8, 40: Input "24. TOTAL REB "; RS!(I, 7): If C1 = 1 Then 250
            234 Locate 9, 40: Input "25. PER FOULS "; RS!(I, 8): If C1 = 1 Then 250
            235 Locate 10, 40: Input "26. ASSISTS "; RS!(I, 9): If C1 = 1 Then 250
            236 Locate 11, 40: Input "27. BLOCKS "; RS!(I, 10): If C1 = 1 Then 250
            237 Locate 12, 40: Input "28. STEALS "; RS!(I, 11): If C1 = 1 Then 250
            238 Locate 13, 40: Input "29. 3 FGM "; RS!(I, 13): If C1 = 1 Then 250
            239 Locate 14, 40: Input "30. 3 FGA "; RS!(I, 12): If C1 = 1 Then 250

            250 C1 = 1: Locate 22, 1: Print "SELECT # TO CHANGE 1-30 (0 TO CONTINUE) ";
            Input B9

        Loop Until B9 >= 1 Or B9 <= 30

        On B9 GOTO 210, 224, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239

        '---------------
        'Disk ID
        '---------------
        Do
            Color 15, 0: Cls
            JJ = 6
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
            Locate , 12: Print "ENTER NEW DISK ID: "
            Print

            Do
                Locate 7, 31: Input NY$
                Locate , 12: Print "IS THIS CORRECT (YN)"
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            If UCase$(I$) = "Y" Then
                newID% = 1
                Call SaveTeam_CNGRAT(NY$, newID%, teamIdx%, teamName$)
                Exit Sub
            End If

        Loop Until UCase$(I$) = "Y"
    Else
        'Continue
        X% = 0
    End If

    '---------------
    'Player Contributions
    '---------------
    Do
        Cls

        Color T%(0), T%(1)
        Print teamName$

        Color 14, 0
        Print "PLAYER          CONTRIBUTION"
        Print "                PERCENT"

        Color 15, 0

        T3 = 0

        For I = 0 To 13
            Print Using "## "; I + 1;
            Print cngratA$(I);
            Locate , 17
            Print A!(I, 3)
            Locate , 1
            T3 = T3 + A!(I, 3)
        Next I
        Print
        Color 14
        Print "TOTAL CP ";
        Locate , 17
        Color 15, 0
        Print T3

        Locate 21
        Print "SELECT PLAYER TO CHANGE 1-14 (0-NO MORE CHANGE) ";
        Input X9

        If X9 < 1 Or X9 > 14 Then
            '0 - no more change
            Cls
            If T3 > 98 And T3 < 102 Then
                'This is where we actually leave
                Call SaveTeam_CNGRAT(yearNumber$, newID%, teamIdx%, teamName$)
            Else
                'We need to warn the user that
                'our total doesn't add up!!
                JJ = 4
                Locate 6, 8: Color 15, 1
                Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";
                For II = 1 To JJ:
                    Locate 6 + II, 8
                    Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
                Next
                Locate 7 + JJ, 8
                Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";
                Locate 7

                U$ = "TOTAL CONTRIBUTION MUST BE 100 %"
                Locate , (80 - Len(U$)) / 2
                Print U$

                U$ = "HIT ANY KEY TO CONTINUE"
                Locate , (80 - Len(U$)) / 2
                Print U$

                Color 15, 0

                I$ = GetKeyPress$
            End If
        Else
            'Subtract one because our array starts at 0
            X9 = X9 - 1

            Locate 22, 18
            Print cngratA$(X9)
            Locate , 1
            Locate 23, 1
            Input "ENTER CP: "; A!(X9, 3)
        End If

    Loop Until X9 = 0

End Sub

'----------------------------------------
'     GetDiskID_CNGRAT Subroutine
'----------------------------------------
'... explain here ...
Sub GetDiskID_CNGRAT (yearNumber$)

    Color 15, 0: Cls
    JJ = 6
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
    Color 15, 0
    Cls

    '-- We need to handle ESC!
    '-- change INPUT to a get key,
    ' then update remaining code
    ' this explains all the places
    ' where ESC outputs the arrow

    yearNumber$ = NY$
    Erase teamNames$

End Sub

'----------------------------------------
'     SelectTeam_CNGRAT Subroutine
'----------------------------------------
'...explanation...
Sub SelectTeam_CNGRAT (teamIdx%, numberTeams%, yearNumber$, selectedTeamName$)

    Shared V1%, V2%
    Shared avgAttendance%, defFGPctAdj%, staminaRating%
    Shared A1$, C$, S$

    Shared L%(), T%(), T1%()
    Shared cngratA$(), P$()
    Shared A!(), DF!(), RS!(), X!()

    NS% = 0

    Selection% = 1
    Count% = numberTeams%
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

        teamIdx% = Selection%

        '-- This could be it's own subroutine
        Open diskPaths$(0) + "BASK." + yearNumber$ For Random As #1 Len = 655

        Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 2 As Q$(14), 2 As Q$(15), 2 As Q$(16), 2 As Q$(17), 2 As Q$(18), 2 As Q$(19), 602 As X$
        For I = 0 To 13
            Field #1, 53 + I * 43 As X$, 15 As Q$(I * 15 + 20), 2 As Q$(I * 15 + 21), 2 As Q$(I * 15 + 22), 2 As Q$(I * 15 + 23), 2 As Q$(I * 15 + 24), 2 As Q$(I * 15 + 25), 2 As Q$(I * 15 + 26), 2 As Q$(I * 15 + 27), 2 As Q$(I * 15 + 28), 2 As Q$(I * 15 + 29), 2 As Q$(I * 15 + 30), 2 As Q$(I * 15 + 31), 2 As Q$(I * 15 + 32), 2 As Q$(I * 15 + 33), 2 As Q$(I * 15 + 34), 655 - 53 - 43 - I * 43 As X$
        Next
        Get #1, teamIdx%

        selectedTeamName$ = RTrim$(Q$(0))

        staminaRating% = CVI(Q$(1))
        defFGPctAdj% = CVI(Q$(2))
        V1% = CVI(Q$(3))
        V2% = CVI(Q$(4))

        For I = 0 To 4: L%(I) = CVI(Q$(I + 5)): Next
        For I = 0 To 9: T%(I) = CVI(Q$(I + 10)): Next
        For I = 0 To 13

            cngratA$(I) = RTrim$(Q$(I * 15 + 20))

            For I1 = 0 To 6
                A!(I, I1) = CVI(Q$(I * 15 + I1 + 21))
                X!(I, I1) = CVI(Q$(I * 15 + I1 + 28))
            Next
        Next

        Close 1

        Open diskPaths$(0) + "COLBBTMS." + yearNumber$ For Random As #1 Len = 566

        Field #1, 15 As Q$(0), 40 As Q$(1), 15 As Q$(2), 4 As Q$(3)

        For I = 0 To 13
            Field #1, 74 + I * 32 As X$, 2 As Q$(I * 16 + 4), 2 As Q$(I * 16 + 5), 2 As Q$(I * 16 + 6), 2 As Q$(I * 16 + 7), 2 As Q$(I * 16 + 8), 2 As Q$(I * 16 + 9), 2 As Q$(I * 16 + 10), 2 As Q$(I * 16 + 11), 2 As Q$(I * 16 + 12), 2 As Q$(I * 16 + 13), 2 As Q$(I * 16 + 14), 2 As Q$(I * 16 + 15), 2 As Q$(I * 16 + 16), 2 As Q$(I * 16 + 17), 2 As Q$(I * 16 + 18), 2 As Q$(I * 16 + 19)
            Field #1, 522 As X$, 2 As Q$(228), 2 As Q$(229), 2 As Q$(230), 2 As Q$(231), 2 As Q$(232), 2 As Q$(233), 2 As Q$(234), 2 As Q$(235), 2 As Q$(236), 2 As Q$(237), 2 As Q$(238), 2 As Q$(239), 2 As Q$(240), 2 As Q$(241), 2 As Q$(242), 2 As Q$(243), 2 As Q$(244), 2 As Q$(245), 2 As Q$(246), 2 As Q$(247), 2 As Q$(248), 2 As Q$(249)
        Next

        Get #1, teamIdx%

        C$ = Q$(0)
        S$ = Q$(1)
        A1$ = Q$(2)
        avgAttendance% = CVL(Q$(3))

        For I = 0 To 13
            DF!(I) = CVI(Q$(I * 16 + 4))
            P$(I) = Q$(I * 16 + 5)
            For I1 = 0 To 13
                RS!(I, I1) = CVI(Q$(I * 16 + I1 + 6))
            Next
        Next

        For I = 0 To 20
            T1%(I) = CVI(Q$(228 + I))
        Next

        CK = CVI(Q$(249))

        Close 1

        If CK <> 999 Then
            C$ = ""
            S$ = ""
            A1$ = ""
            avgAttendance% = 0

            For I = 0 To 13
                DF!(I) = 5
				P$(I) = ""
                For I1 = 0 To 13
                    RS!(I, I1) = 0
                Next
            Next

            For I = 0 To 20
                T1%(I) = 0
            Next

        End If 'done checking for '999
    Else
        BO% = 1
    End If 'Done checking ExitCode

End Sub

'----------------------------------------
'     SaveTeam_CNGRAT Subroutine
'----------------------------------------
'... explain here ...
Sub SaveTeam_CNGRAT (yearNumber$, newID%, teamIdx%, teamName$)

    Shared V1%, V2%
    Shared defFGPctAdj%, staminaRating%
    Shared A1$, C$, S$

    Shared L%(), T%(), T1%()
    Shared cngratA$(), P$()
    Shared A!(), DF!(), RS!(), X!()

    If newID% = 1 Then
        Open diskPaths$(0) + "BASK." + yearNumber$ For Random As #1 Len = 655
        Y = LOF(1) / 655
        Close 1
        teamIdx% = Y + 1
    End If

    Open diskPaths$(0) + "BASK." + yearNumber$ For Random As #1 Len = 655
    Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 2 As Q$(14), 2 As Q$(15), 2 As Q$(16), 2 As Q$(17), 2 As Q$(18), 2 As Q$(19), 602 As X$

    For I = 0 To 13
        Field #1, 53 + I * 43 As X$, 15 As Q$(I * 15 + 20), 2 As Q$(I * 15 + 21), 2 As Q$(I * 15 + 22), 2 As Q$(I * 15 + 23), 2 As Q$(I * 15 + 24), 2 As Q$(I * 15 + 25), 2 As Q$(I * 15 + 26), 2 As Q$(I * 15 + 27), 2 As Q$(I * 15 + 28), 2 As Q$(I * 15 + 29), 2 As Q$(I * 15 + 30), 2 As Q$(I * 15 + 31), 2 As Q$(I * 15 + 32), 2 As Q$(I * 15 + 33), 2 As Q$(I * 15 + 34), 655 - 53 - 43 - I * 43 As X$
    Next

    LSet Q$(0) = teamName$
    LSet Q$(1) = MKI$(staminaRating%)
    LSet Q$(2) = MKI$(defFGPctAdj%)
    LSet Q$(3) = MKI$(V1%)
	LSet Q$(4) = MKI$(V2%)
    For I = 0 To 4: LSet Q$(I + 5) = MKI$(L%(I)): Next
    T%(2) = 999

    For I = 0 To 9: LSet Q$(I + 10) = MKI$(T%(I)): Next

	'Players
    For I = 0 To 13
        LSet Q$(I * 15 + 20) = cngratA$(I)
        For I1 = 0 To 6
            LSet Q$(I * 15 + I1 + 21) = MKI$(A!(I, I1))
            LSet Q$(I * 15 + I1 + 28) = MKI$(X!(I, I1))
        Next
    Next

    Put #1, teamIdx%
    Close 1

    Open diskPaths$(0) + "COLBBTMS." + yearNumber$ For Random As #1 Len = 566
    Field #1, 15 As Q$(0), 40 As Q$(1), 15 As Q$(2), 4 As Q$(3)

    For I = 0 To 13
        Field #1, 74 + I * 32 As X$, 2 As Q$(I * 16 + 4), 2 As Q$(I * 16 + 5), 2 As Q$(I * 16 + 6), 2 As Q$(I * 16 + 7), 2 As Q$(I * 16 + 8), 2 As Q$(I * 16 + 9), 2 As Q$(I * 16 + 10), 2 As Q$(I * 16 + 11), 2 As Q$(I * 16 + 12), 2 As Q$(I * 16 + 13), 2 As Q$(I * 16 + 14), 2 As Q$(I * 16 + 15), 2 As Q$(I * 16 + 16), 2 As Q$(I * 16 + 17), 2 As Q$(I * 16 + 18), 2 As Q$(I * 16 + 19)
        Field #1, 522 As X$, 2 As Q$(228), 2 As Q$(229), 2 As Q$(230), 2 As Q$(231), 2 As Q$(232), 2 As Q$(233), 2 As Q$(234), 2 As Q$(235), 2 As Q$(236), 2 As Q$(237), 2 As Q$(238), 2 As Q$(239), 2 As Q$(240), 2 As Q$(241), 2 As Q$(242), 2 As Q$(243), 2 As Q$(244), 2 As Q$(245), 2 As Q$(246), 2 As Q$(247), 2 As Q$(248), 2 As Q$(249)
    Next

    LSet Q$(0) = C$
    LSet Q$(1) = S$
    LSet Q$(2) = A1$
    LSet Q$(3) = MKL$(avgAttendance%)

    For I = 0 To 13
        LSet Q$(I * 16 + 4) = MKI$(DF!(I))
        LSet Q$(I * 16 + 5) = P$(I)
        For I1 = 0 To 13
            LSet Q$(I * 16 + I1 + 6) = MKI$(RS!(I, I1))
        Next
    Next

    For I = 0 To 20
        LSet Q$(228 + I) = MKI$(T1%(I))
    Next

    LSet Q$(249) = MKI$(999)
    'issue here
    Put #1, teamIdx%
    Close 1

End Sub
