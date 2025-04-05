'----------------------------------------
'            CARLDR Subroutine
'----------------------------------------
'This routine provides a menu for the user
'that allows them to Update and View the
'Career leaders for a specific team
Sub CARLDR ()

    _Title "Courtside College Basketball - Career Leaders"

    Do

        Color 15, 0
        Cls

        Call TITLE

        ReDim menuChoices$(1 To 3)
        menuChoices$(1) = "UPDATE CAREER LEADERS   "
        menuChoices$(2) = "REVIEW CAREER LEADERS   "
        menuChoices$(3) = "RETURN TO UTILITIES MENU"
        SELECTION% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 6
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 Then

            Select Case SELECTION%

                Case 1:
                    Call UpdateCareerLeaders

                Case 2:
                    Call CareerLeaderOptions

            End Select

        End If

    Loop Until SELECTION% = 3 Or ExitCode% = 27

End Sub


'----------------------------------------
'     UpdateCareerLeaders Subroutine
'----------------------------------------
'This routine reads in the current stats,
'calculates the latest values (based on
'the criteria given) and then creates /
'overwrites the TCL file with the newest
'calculated values.
Sub UpdateCareerLeaders ()

    Dim plyrCarVals!(62, 15)
    Dim car3FGMVals!(15, 62)
    Dim car3FGAVals!(15, 62)

    Shared carLdrPlyrName$()
    Shared carLdrVal!()

    'Arrays to store data loaded from files
    Shared careerPlayers$()
    Shared carPlyrStat!(), car3FGM!(), car3FGA!()

    TMR1 = 15 * 62

    Erase carLdrPlyrName$
    Erase carLdrVal!

    careerFile$ = ChooseFile$(diskPaths$(1) + "*.CAR", "CAREER", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Call LoadCareerFile(diskPaths$(1) + careerFile$ + ".CAR")

        Color 14

        Locate 10: Print "COMPUTING LEADER FILE . . . . . . PLEASE WAIT"

        '# Career Years
        For N = 1 To MAX_CAREER_YEARS

            '# Players
            For I = 0 To 61

                If careerPlayers$(I) <> "" And careerPlayers$(I) <> "XXX" And careerPlayers$(I) <> "END" Then

                    '# Stats
                    For J = 0 To 15

                        plyrCarVals!(I, J) = plyrCarVals!(I, J) + carPlyrStat!(N, I, J)

                        car3FGMVals!(J, I) = car3FGMVals!(J, I) + car3FGM!(J, I)
                        car3FGAVals!(J, I) = car3FGAVals!(J, I) + car3FGA!(J, I)

                    Next J
                
                End If

            Next I
            
        Next N

        ' ##############################
        ' #   Update and sort leaders
        ' ##############################

        'GP, FGA/FGM
        For Z = 0 To 2

            For K = 0 To 61

                If plyrCarVals!(K, Z) > carLdrVal!(Z, 19) Then

                    carLdrVal!(Z, 19) = plyrCarVals!(K, Z)
                    carLdrPlyrName$(Z, 19) = careerPlayers$(K)

                    For I = 1 To 20
                        For J = 20 To I Step -1
                            If carLdrVal!(Z, J - 1) < carLdrVal!(Z, J) Then
                                Swap carLdrVal!(Z, J - 1), carLdrVal!(Z, J)
                                Swap carLdrPlyrName$(Z, J - 1), carLdrPlyrName$(Z, J)
                            End If
                        Next J
                    Next I

                End If

            Next K

        Next Z

        'FG %
        For K = 0 To 61

            If plyrCarVals!(K, 2) / plyrCarVals!(K, 1) > carLdrVal!(3, 19) Then

                carLdrVal!(3, 19) = plyrCarVals!(K, 2) / plyrCarVals!(K, 1)
                carLdrPlyrName$(3, 19) = careerPlayers$(K)

                For I = 1 To 20
                    For J = 20 To I Step -1
                        If carLdrVal!(3, J - 1) < carLdrVal!(3, J) Then
                            Swap carLdrVal!(3, J - 1), carLdrVal!(3, J)
                            Swap carLdrPlyrName$(3, J - 1), carLdrPlyrName$(3, J)
                        End If
                    Next J
                Next I

            End If

        Next K

        'For Z = 3 To 4

        '3FGA
        For K = 0 To 61

            If car3FGAVals!(1, K) > carLdrVal!(4, 19) Then

                carLdrVal!(4, 19) = car3FGAVals!(1, K)
                carLdrPlyrName$(4, 19) = careerPlayers$(K)

                For I = 1 To 20
                    For J = 20 To I Step -1
                        If carLdrVal!(4, J - 1) < carLdrVal!(4, J) Then
                            Swap carLdrVal!(4, J - 1), carLdrVal!(4, J)
                            Swap carLdrPlyrName$(4, J - 1), carLdrPlyrName$(4, J)
                        End If
                    Next J
                Next I

            End If

        Next K

        '3FGM
        For K = 0 To 61

            If car3FGMVals!(1, K) > carLdrVal!(5, 19) Then

                carLdrVal!(5, 19) = car3FGMVals!(1, K)
                carLdrPlyrName$(5, 19) = careerPlayers$(K)

                For I = 1 To 20
                    For J = 20 To I Step -1
                        If carLdrVal!(5, J - 1) < carLdrVal!(5, J) Then
                            Swap carLdrVal!(5, J - 1), carLdrVal!(5, J)
                            Swap carLdrPlyrName$(5, J - 1), carLdrPlyrName$(5, J)
                        End If
                    Next J
                Next I

            End If

        Next K

        '3FG %
        For K = 0 To 61

            If car3FGMVals!(1, K) / car3FGAVals!(1, K) > carLdrVal!(6, 19) Then

                carLdrVal!(6, 19) = car3FGMVals!(1, K) / car3FGAVals!(1, K)
                carLdrPlyrName$(6, 19) = careerPlayers$(K)

                For I = 1 To 20
                    For J = 20 To I Step -1
                        If carLdrVal!(6, J - 1) < carLdrVal!(6, J) Then
                            Swap carLdrVal!(6, J - 1), carLdrVal!(6, J)
                            Swap carLdrPlyrName$(6, J - 1), carLdrPlyrName$(6, J)
                        End If
                    Next J
                Next I

            End If

        Next K

        'FTA/FTM
        For Z = 3 To 4

            For K = 0 To 61

                If plyrCarVals!(K, Z) > carLdrVal!(Z, 19) Then

                    carLdrVal!(Z + 4, 19) = plyrCarVals!(K, Z)
                    carLdrPlyrName$(Z + 4, 19) = careerPlayers$(K)

                    For I = 1 To 20
                        For J = 20 To I Step -1
                            If carLdrVal!(Z + 4, J - 1) < carLdrVal!(Z + 4, J) Then
                                Swap carLdrVal!(Z + 4, J - 1), carLdrVal!(Z + 4, J)
                                Swap carLdrPlyrName$(Z + 4, J - 1), carLdrPlyrName$(Z + 4, J)
                            End If
                        Next J
                    Next I

                End If

            Next K

        Next Z

        'FT %
        For K = 0 To 61

            If plyrCarVals!(K, 4) / plyrCarVals!(K, 3) > carLdrVal!(9, 19) Then

                carLdrVal!(9, 19) = plyrCarVals!(K, 4) / plyrCarVals!(K, 3)
                carLdrPlyrName$(9, 19) = careerPlayers$(K)

                For I = 1 To 20
                    For J = 20 To I Step -1
                        If carLdrVal!(9, J - 1) < carLdrVal!(9, J) Then
                            Swap carLdrVal!(9, J - 1), carLdrVal!(9, J)
                            Swap carLdrPlyrName$(9, J - 1), carLdrPlyrName$(9, J)
                        End If
                    Next J
                Next I

            End If

        Next K

        'All Remaining Categories
        For Z = 5 To 13

            For K = 0 To 61

                If plyrCarVals!(K, Z) > carLdrVal!(Z + 5, 19) Then

                    carLdrVal!(Z + 5, 19) = plyrCarVals!(K, Z)
                    carLdrPlyrName$(Z + 5, 19) = careerPlayers$(K)

                    For I = 1 To 20
                        For J = 20 To I Step -1
                            If carLdrVal!(Z + 5, J - 1) < carLdrVal!(Z + 5, J) Then
                                Swap carLdrVal!(Z + 5, J - 1), carLdrVal!(Z + 5, J)
                                Swap carLdrPlyrName$(Z + 5, J - 1), carLdrPlyrName$(Z + 5, J)
                            End If
                        Next J
                    Next I

                End If

            Next K

        Next Z

        'Save leaders
        Open diskPaths$(2) + careerFile$ + ".TCL" For Output As #1

        For I = 0 To 19:
            For I1 = 0 To 19:
                Write #1, carLdrVal!(I, I1), carLdrPlyrName$(I, I1)
            Next
        Next

        Close #1

    End If

End Sub


'----------------------------------------
'     CareerLeaderOptions Subroutine
'----------------------------------------
'This routine reads in previously compiled
'career stats (.TCL file) and then provides
'the user options to print or view the stats.
Sub CareerLeaderOptions ()

    Shared carLdrPlyrName$()
    Shared carLdrVal!()

    Cls

    leaderFile$ = ChooseFile$(diskPaths$(2) + "*.TCL", "LEADER", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Color 15, 0
        Cls

        Open diskPaths$(2) + leaderFile$ + ".TCL" For Input As #1

        For I = 0 To 19:
            For I1 = 0 To 19:
                Input #1, carLdrVal!(I, I1), carLdrPlyrName$(I, I1)
            Next
        Next

        Close #1

        userKey$ = GetOutputOptions$

        If userKey$ <> Chr$(27) Then

            Select Case userKey$

                Case "F"
                    userFile$ = _SaveFileDialog$("Save Report File", _CWD$ + "\Report", "*.txt", "Text File")
                    If userFile$ <> "" Then
                        Call PrintCareerLeaders(0, userFile$)
                    End If

                Case "P"
                    Call PrintCareerLeaders(1, "REPORT")

                Case "S"
                    Call ViewCareerLeaders

            End Select

        End If

    End If 'Done checking for ESC


End Sub


'----------------------------------------
'     PrintCareerLeaders Subroutine
'----------------------------------------
'This routine sends formatted career stats
'(loaded from the .TCL file) to the user's
'default printer
Sub PrintCareerLeaders (printDest, destFile$)

    Shared carLdrPlyrName$()
    Shared carLdrVal!()

    Color 15, 0
    Cls

    Open destFile$ For Output As #1

    Print #1, Tab(5); "GAMES PLAYED"; Tab(34); "FIELD GOALS ATTEMPTED"; Tab(62); "FIELD GOALS MADE"

    For I = 0 To 19
        Print #1, Using "##"; I + 1;
        Print #1, ". ";
        Print #1, carLdrPlyrName$(0, I);
        Print #1, Tab(18); Using "####"; carLdrVal!(0, I);
        Print #1, Tab(28); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(1, I);
        Print #1, Tab(47); Using "####"; carLdrVal!(1, I);
        Print #1, Tab(58); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(2, I);
        Print #1, Tab(76); Using "####"; carLdrVal!(2, I)
    Next

    Print #1,
    Print #1,

    Print #1, Tab(5); "FIELD GOAL %"; Tab(34); "3 POINTERS MADE"; Tab(62); "3 POINTERS ATTEMPTED"

    For I = 0 To 19
        Print #1, Using "##"; I + 1;
        Print #1, ". ";
        Print #1, carLdrPlyrName$(3, I);
        Print #1, Tab(18); Using "##.#"; carLdrVal!(3, I);
        Print #1, Tab(28); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(5, I);
        Print #1, Tab(47); Using "####"; carLdrVal!(5, I);
        Print #1, Tab(58); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(4, I);
        Print #1, Tab(76); Using "####"; carLdrVal!(4, I)
    Next

    Print #1,
    Print #1,

    Print #1, Tab(5); "3 POINTER %"; Tab(34); "FREE THROWS MADE"; Tab(62); "FREE THROWS ATTEMPTED"

    For I = 0 To 19
        Print #1, Using "##"; I + 1;
        Print #1, ". ";
        Print #1, carLdrPlyrName$(6, I);
        Print #1, Tab(18); Using "##.#"; carLdrVal!(6, I);
        Print #1, Tab(28); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(7, I);
        Print #1, Tab(47); Using "####"; carLdrVal!(7, I);
        Print #1, Tab(58); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(8, I);
        Print #1, Tab(76); Using "####"; carLdrVal!(8, I)
    Next

    Print #1,
    Print #1,

    Print #1, Tab(5); "FREE THROW %"; Tab(34); "TOTAL REBOUNDS"; Tab(62); "PERSONAL FOULS"
    For I = 0 To 19
        Print #1, Using "##"; I + 1;
        Print #1, ". ";
        Print #1, carLdrPlyrName$(9, I);
        Print #1, Tab(18); Using "##.#"; carLdrVal!(9, I);
        Print #1, Tab(28); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(10, I);
        Print #1, Tab(47); Using "####"; carLdrVal!(10, I);
        Print #1, Tab(58); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(11, I);
        Print #1, Tab(76); Using "####"; carLdrVal!(11, I)
    Next

    Print #1,
    Print #1,

    Print #1, Tab(5); "STEALS"; Tab(34); "ASSISTS"; Tab(62); "BLOCKS"

    For I = 0 To 19
        Print #1, Using "##"; I + 1;
        Print #1, ". ";
        Print #1, carLdrPlyrName$(12, I);
        Print #1, Tab(18); Using "####"; carLdrVal!(12, I);
        Print #1, Tab(28); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(13, I);
        Print #1, Tab(47); Using "####"; carLdrVal!(13, I);
        Print #1, Tab(58); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(14, I);
        Print #1, Tab(76); Using "####"; carLdrVal!(14, I)
    Next

    Print #1,
    Print #1,

    Print #1, Tab(5); "MINUTES PLAYED"; Tab(34); "POINTS"; Tab(62); "OFFENSIVE REBOUNDS"

    For I = 0 To 19
        Print #1, Using "##"; I + 1;
        Print #1, ". ";
        Print #1, carLdrPlyrName$(15, I);
        Print #1, Tab(18); Using "####"; Int((carLdrVal!(15, I) + 30) / 60);
        Print #1, Tab(28); Using "##"; I + 1;: Print #1, ". ";
        Print #1, carLdrPlyrName$(16, I);
        Print #1, Tab(47); Using "####"; carLdrVal!(16, I);
        Print #1, Tab(58); Using "##"; I + 1;: Print #1, ". ";
        Print #1, carLdrPlyrName$(17, I);
        Print #1, Tab(76); Using "####"; carLdrVal!(17, I)
    Next

    Print #1,
    Print #1,

    Print #1, Tab(5); "HIGHEST POINTS"

    For I = 0 To 19
        Print #1, Using "##"; I + 1;
        Print #1, ". ";
        Print #1, carLdrPlyrName$(18, I);
        Print #1, Tab(18); Using "####"; carLdrVal!(18, I)
    Next

    Print #1,
    Print #1,

    If printDest = 1 Then

        Open destFile$ For Input As #2

        Do While Not EOF(2)
            Line Input #2, X$
            LPrint X$
        Loop

        Close #2

        LPrint Chr$(12)

        Kill destFile$

    End If

End Sub


'----------------------------------------
'      ViewCareerLeaders Subroutine
'----------------------------------------
'This routine displays formatted career
'stats (loaded from the .TCL file) to the
'primary screen
Sub ViewCareerLeaders ()

    Shared carLdrPlyrName$()
    Shared carLdrVal!()

    Color 15, 0

    Cls
    Color 12

    ' Category #1, 2
    Locate 2, 1
    Print Tab(5); "GAMES"; Tab(44); "FGA"

    For I = 0 To 19
        Color 3
        Locate I + 3: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(0, I);
        Print Tab(20); Using "####"; carLdrVal!(0, I);:

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(1, I);
        Print Tab(60); Using "####"; carLdrVal!(1, I)
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Locate 2, 1
    Print Tab(5); "FGM"; Tab(44); "FG %"

    For I = 0 To 19
        Color 3
        Locate I + 3: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(2, I);
        Print Tab(20); Using "####"; carLdrVal!(2, I);:

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(3, I);
        Print Tab(60); Using "##.#"; carLdrVal!(3, I) * 100
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Locate 2, 1
    Print Tab(5); "3FGA"; Tab(44); "3FGM"

    For I = 0 To 19
        Color 3
        Locate I + 3: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(4, I);
        Print Tab(20); Using "####"; carLdrVal!(4, I);:

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(5, I);
        Print Tab(60); Using "####"; carLdrVal!(5, I)
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Locate 2, 1
    Print Tab(5); "3FG %"; Tab(44); "FTA"

    For I = 0 To 19
        Color 3
        Locate I + 3: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(6, I);
        Print Tab(20); Using "##.#"; carLdrVal!(6, I) * 100;

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(7, I);
        Print Tab(60); Using "####"; carLdrVal!(7, I)
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Locate 2, 1
    Print Tab(5); "FTM"; Tab(44); "FT %"

    For I = 0 To 19
        Color 3
        Locate I + 3: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(8, I);
        Print Tab(20); Using "####"; carLdrVal!(8, I);:

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(9, I);
        Print Tab(60); Using "##.#"; carLdrVal!(9, I) * 100
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Locate 2, 1
    Print Tab(5); "TOTAL REBOUNDS"; Tab(44); "PERSONAL FOULS"
    For I = 0 To 19
        Color 3
        Locate I + 3: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(10, I);
        Print Tab(20); Using "####"; carLdrVal!(10, I);

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(11, I);
        Print Tab(60); Using "####"; carLdrVal!(11, I)
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Locate 2, 1
    Print Tab(5); "STEALS"; Tab(44); "ASSISTS"
    For I = 0 To 19
        Color 3
        Locate I + 3: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(12, I);
        Print Tab(20); Using "####"; carLdrVal!(12, I);:

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(13, I);
        Print Tab(60); Using "####"; carLdrVal!(13, I)
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Locate 2, 1
    Print Tab(5); "BLOCKS"; Tab(44); "MINUTES PLAYED"

    For I = 0 To 19
        Color 3
        Locate I + 3: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(14, I);
        Print Tab(20); Using "####"; carLdrVal!(14, I);

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(15, I);
        Print Tab(60); Using "####"; Int((carLdrVal!(15, I) + 30) / 60)
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Locate 2, 1
    Print Tab(5); "POINTS"; Tab(44); "OFFENSIVE REBOUNDS"

    For I = 0 To 19
        Color 3
        Locate I + 3: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(16, I);
        Print Tab(20); Using "####"; carLdrVal!(16, I);:

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(17, I);
        Print Tab(60); Using "####"; carLdrVal!(17, I)
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Locate 2, 1
    Print Tab(5); "HIGHEST POINTS"

    For I = 0 To 19
        Color 3
        Locate I + 3: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(18, I);
        Print Tab(20); Using "####"; carLdrVal!(18, I)
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

End Sub
