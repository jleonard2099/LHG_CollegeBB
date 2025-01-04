'----------------------------------------
'           CAREDIT Subroutine
'----------------------------------------
'This routine allows the user to choose a
'career file, then loads in the stats and
'gives the users options for editing and
'saving the file.
Sub CAREDIT ()

    _Title "Courtside College Basketball - Edit Career Files"

    Color 15, 0
    Cls

    careerFile$ = ChooseFile$(diskPaths$(1) + "*.CAR", "CAREER", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Call LoadCareerFile(careerFile$)

        Do

            Cls

            Color 7: Print "EDIT ";
            Color 12: Print "P";
            Color 7: Print "LAYERS"

            Color 12: Print "U";
            Color 7: Print "PDATE CARER FILE"

            Color 12: Print "E";
            Color 7: Print "XIT WITHOUT UPDATING"

            I$ = GetKeyPress$

            Select Case UCase$(I$)

                Case "P":
                    Call EditPlayers(careerFile$)

                Case "E", "X": '

                Case "U":
                    Call SaveCareerFile(careerFile$)

            End Select

        Loop Until UCase$(I$) = "E" Or UCase$(I$) = "X"

    End If

End Sub


'----------------------------------------
'         EditPlayers Subroutine
'----------------------------------------
'This routine gives the user a choice of
'players to edit from the career stats,
'then presents options for handling them
'before deletions or edits are processed.
Sub EditPlayers (careerFile$)

    Shared careerPlayers$()

    Shared carPlyrStat!(), car3FGM!(), car3FGA!()

    Do

        Cls
        Color 12: Print careerFile$

        For I = 0 To 15:
            Locate I + 2
            Color 11: Print Using "##"; I;
            Print " ";
            Color 7: Print Left$(careerPlayers$(I), 12)
        Next

        For I = 16 To 31:
            Locate I - 14, 20
            Color 11: Print Using "##"; I;
            Print " ";
            Color 7: Print Left$(careerPlayers$(I), 12)
        Next

        For I = 32 To 46:
            Locate I - 30, 40
            Color 11: Print Using "##"; I;
            Print " ";
            Color 7: Print Left$(careerPlayers$(I), 12)
        Next

        For I = 47 To 61:
            Locate I - 45, 60
            Color 11: Print Using "##"; I;
            Print " ";
            Color 7: Print Left$(careerPlayers$(I), 12)
        Next

        Print

        Do
            Locate 22: Color 15
            Input "ENTER NUMBER OF PLAYER TO DELETE (62 TO EXIT)"; B9
        Loop Until B9 >= 0 And B9 <= 62

        If B9 <> 62 Then

            If B9 < 61 Then

                Cls

                Color 15
                Locate 5, 20: Print "PRINT CAREER STATS OF ";
                Color 12: Print careerPlayers$(B9)
                Color 15
                Locate , 20: Print "BEFORE DELETING THEM? (Y/N)"

                Color 14
                Print
                Locate , 20: Print "IF YOU DO NOT WISH TO DELETE THE STATS"
                Locate , 20: Print "OF ";
                Color 12: Print careerPlayers$(B9);
                Color 14: Print " HIT 'E', 'X' or 'Esc'"

                Do
                    I$ = GetKeyPress$

                    Select Case UCase$(I$)

                        Case "Y":
                            userKey$ = GetPrintOptions$

                            If userKey$ <> Chr$(27) Then

                                Select Case userKey$

                                    Case "F"
                                        userFile$ = _SaveFileDialog$("Save Report File", _CWD$ + "\Report", "*.txt", "Text File")
                                        If userFile$ <> "" Then
                                            Call CareerStatsPlayer_Report(0, userFile$, careerFile$, B9)
                                        End If

                                    Case "P"
                                        Call CareerStatsPlayer_Report(1, "REPORT", careerFile$, B9)

                                End Select

                            End If

                        Case "N":
                            Cls

                        Case "E", "X", Chr$(27):
                            Exit Sub

                    End Select

                Loop Until UCase$(I$) = "N"

                Cls
                Locate 5, 15: Print "CHANGING PLAYER'S FILES . . . . . PLEASE WAIT"

                For I = B9 To 60:
                    careerPlayers$(I) = careerPlayers$(I + 1)
                Next

                UPP = (61 - B9) * 15
                I = B9

                For J = 1 To MAX_CAREER_YEARS
                    For I = B9 To 60
                        For I1 = 0 To 15
                            carPlyrStat!(J, I + 1, I1) = carPlyrStat!(J, I + 1, I1)
                        Next I1

                        car3FGM!(N, I) = car3FGM!(N, I + 1)
                        car3FGA!(N, I) = car3FGM!(N, I + 1)

                        Locate 7, 36
                        Color 12: UPP = UPP - 1
                        Print Using "####"; UPP
                    Next
                Next

            End If

            If B9 < 62 Then 'we have to ignore if they entered over 62
                careerPlayers$(61) = ""
                For J = 1 To MAX_CAREER_YEARS
                    For I1 = 0 To 15
                        carPlyrStat!(J, 61, I1) = 0
                        car3FGM!(J, 61) = 0
                        car3FGA!(J, 61) = 0
                    Next
                Next
            End If

        End If

    Loop Until B9 = 62

End Sub


'----------------------------------------
'  CareerStatsPlayer_Report Subroutine
'----------------------------------------
'This routine formats and prints career
'stats associated with the team's players
Sub CareerStatsPlayer_Report (printDest, destFile$, careerFile$, playerIdx)

    Shared careerPlayers$()

    Shared carPlyrStat!(), car3FGM!(), car3FGA!()

    B9 = playerIdx

    Color 15, 0
    Cls

    Open destFile$ For Output As #1

    Print #1, careerFile$

    'Process player
    TT1 = 0
    TT2 = 0
    TT3 = 0
    TT4 = 0
    TT5 = 0
    TT6 = 0
    TT7 = 0
    TT8 = 0
    TT9 = 0
    TT10 = 0
    TT11 = 0
    TT12 = 0
    TT13 = 0
    TT14 = 0
    TT15 = 0
    TT16 = 0
    TT17 = 0
    
    VV = 0

    For Z = 1 To MAX_CAREER_YEARS
        VV = VV + carPlyrStat!(Z, I, 14)
    Next Z

    If 55 - PTN < VV Then
        Print #1, Chr$(12)
        Print #1, Tab(59); careerFile$
        Print #1,
        Print #1, "PLAYER      YEAR   G  FGM   FGA  PCT  FTM   FTA  PCT   PTS  AVG.  3FGM 3FGA  PCT   RB   AVG   PF  AST  BLK   ST  HI   MIN  AVG."
        PTN = 0
    End If

    Print #1, Left$(careerPlayers$(I), 11);

    For V = 1 To VV

        PTN = PTN + 1
        'Year
        Print #1, Tab(12); carPlyrStat!(V, I, 15);
        'G
        Print #1, Tab(19); Using "##"; carPlyrStat!(V, I, 0);
        'FGM
        Print #1, Tab(23); Using "###"; carPlyrStat!(V, I, 2);
        'FGA
        Print #1, Tab(28); Using "####"; carPlyrStat!(V, I, 1);

        'FG Pct
        If carPlyrStat!(V, I, 2) = 0 Then 
            Print #1, Tab(34); "--.-";
        End If
        If carPlyrStat!(V, I, 2) > 0 Then 
            Print #1, Tab(33); Using "###.#"; (carPlyrStat!(V, I, 2) / carPlyrStat!(V, I, 1)) * 100;
        End If

        'FTM
        Print #1, Tab(39); Using "###"; carPlyrStat!(V, I, 4);
        'FTA
        Print #1, Tab(45); Using "###"; carPlyrStat!(V, I, 3);

        'FT %
        If carPlyrStat!(V, I, 4) = 0 Then 
            Print #1, Tab(50); "--.-";
        End If
        If carPlyrStat!(V, I, 4) > 0 Then 
            Print #1, Tab(49); Using "###.#"; (carPlyrStat!(V, I, 4) / carPlyrStat!(V, I, 3)) * 100;
        End If

        'Pts
        Print #1, Tab(55); Using "####"; carPlyrStat!(V, I, 2) * 2 + carPlyrStat!(V, I, 4) + car3FGM!(V, I);

        'Average
        If carPlyrStat!(V, I, 2) * 2 + carPlyrStat!(V, I, 4) + car3FGM!(V, I) = 0 Then 
            Print #1, Tab(61); " 0.0";
        End If
        If carPlyrStat!(V, I, 2) * 2 + carPlyrStat!(V, I, 4) + car3FGM!(V, I) > 0 Then 
            Print #1, Tab(61); Using "##.#"; (carPlyrStat!(V, I, 2) * 2 + carPlyrStat!(V, I, 4) + car3FGM!(V, I)) / carPlyrStat!(V, I, 0);
        End If

        '3FGM
        Print #1, Tab(68); Using "###"; car3FGM!(V, I);
        '3FGA
        Print #1, Tab(73); Using "###"; car3FGA!(V, I);

        '3 FG%
        If car3FGM!(V, I) = 0 Then 
            Print #1, Tab(78); "--.-";
        End If
        If car3FGM!(V, I) > 0 Then 
            Print #1, Tab(77); Using "###.#"; (car3FGM!(V, I) / car3FGA!(V, I)) * 100;
        End If
        
        'RB
        Print #1, Tab(83); Using "###"; carPlyrStat!(V, I, 5);

        'Avg RB
        If carPlyrStat!(V, I, 5) = 0 Then 
            Print #1, Tab(88); " 0.0";
        End If
        If carPlyrStat!(V, I, 5) > 0 Then 
            Print #1, Tab(88); Using "##.#"; carPlyrStat!(V, I, 5) / carPlyrStat!(V, I, 0);
        End If

        'PF
        Print #1, Tab(94); Using "###"; carPlyrStat!(V, I, 6);
        'Ast
        Print #1, Tab(99); Using "###"; carPlyrStat!(V, I, 8);
        'Blk
        Print #1, Tab(104); Using "###"; carPlyrStat!(V, I, 9);
        'Stl
        Print #1, Tab(109); Using "###"; carPlyrStat!(V, I, 7);
        'Highest Points
        Print #1, Tab(113); Using "###"; carPlyrStat!(V, I, 13);

        'Mins
        If carPlyrStat!(V, I, 10) = 0 Then 
            Print #1, Tab(118); "   0";
        End If
        If carPlyrStat!(V, I, 10) > 0 Then 
            Print #1, Tab(118); Using "####"; Int((carPlyrStat!(V, I, 10) + 30) / 60);
        End If

        'Avg Mins
        If carPlyrStat!(V, I, 10) = 0 Then 
            Print #1, Tab(125); " 0"
        End If
        If carPlyrStat!(V, I, 10) > 0 Then 
            Print #1, Tab(125); Using "##"; CInt(((carPlyrStat!(V, I, 10) / 60) / carPlyrStat!(V, I, 0)))
        End If

    Next V

    VV = 0

    For Z = 1 To MAX_CAREER_YEARS1
        VV = VV + carPlyrStat!(Z, I, 14)
    Next Z

    For V = 1 To VV
        TT1 = TT1 + carPlyrStat!(V, I, 0)
        TT2 = TT2 + carPlyrStat!(V, I, 2)
        TT3 = TT3 + carPlyrStat!(V, I, 1)
        TT4 = TT4 + carPlyrStat!(V, I, 4)
        TT5 = TT5 + carPlyrStat!(V, I, 3)
        TT6 = TT6 + (carPlyrStat!(V, I, 2) * 2 + carPlyrStat!(V, I, 4) + car3FGM!(V, I))
        TT7 = TT7 + car3FGM!(V, I)
        TT8 = TT8 + car3FGA!(V, I)
        TT9 = TT9 + carPlyrStat!(V, I, 5)
        TT10 = TT10 + carPlyrStat!(V, I, 6)
        TT11 = TT11 + carPlyrStat!(V, I, 8)
        TT12 = TT12 + carPlyrStat!(V, I, 9)
        TT13 = TT13 + carPlyrStat!(V, I, 7)
        TT14 = TT14 + carPlyrStat!(V, I, 10)
        TT16 = TT16 + carPlyrStat!(V, I, 11)
        TT17 = TT17 + carPlyrStat!(V, I, 12)
        If carPlyrStat!(V, I, 13) > TT15 Then 
            TT15 = carPlyrStat!(V, I, 13)
        End If
    Next V

    Print #1, "  TOTALS";
    Print #1, Tab(15); Using "##"; VV;
    Print #1, Tab(17); Using "####"; TT1;
    Print #1, Tab(21); Using "#####"; TT2;
    Print #1, Tab(27); Using "#####"; TT3;

    If TT2 = 0 Then 
        Print #1, Tab(34); "--.-";
    End If
    If TT2 > 0 Then 
        Print #1, Tab(34); Using "###.#"; (TT2 / TT3) * 100;
    End If

    Print #1, Tab(37); Using "#####"; TT4;
    Print #1, Tab(43); Using "#####"; TT5;

    If TT4 = 0 Then 
        Print #1, Tab(50); "--.-";
    End If
    If TT4 > 0 Then 
        Print #1, Tab(50); Using "###.#"; (TT4 / TT5) * 100;
    End If

    Print #1, Tab(55); Using "####"; TT6;

    If TT6 = 0 Then 
        Print #1, Tab(61); " 0.0";
    End If
    If TT6 > 0 Then 
        Print #1, Tab(61); Using "##.#"; TT6 / TT1;
    End If

    Print #1, Tab(67); Using "####"; TT7;
    Print #1, Tab(72); Using "####"; TT8;

    If TT7 = 0 Then 
        Print #1, Tab(78); "--.-";
    End If
    If TT7 > 0 Then 
        Print #1, Tab(78); Using "###.#"; (TT7 / TT8) * 100;
    End If

    Print #1, Tab(81); Using "#####"; TT9;

    If TT9 = 0 Then 
        Print #1, Tab(88); " 0.0";
    End If
    If TT9 > 0 Then 
        Print #1, Tab(88); Using "##.#"; TT9 / TT1;
    End If

    Print #1, Tab(93); Using "####"; TT10;
    Print #1, Tab(98); Using "####"; TT11;
    Print #1, Tab(103); Using "####"; TT12;
    Print #1, Tab(109); Using "###"; TT13;
    Print #1, Tab(114); Using "##"; TT15;

    If TT14 = 0 Then 
        Print #1, Tab(117); "    0";
    End If
    If TT14 > 0 Then 
        Print #1, Tab(117); Using "#####"; Int((TT14 + 30) / 60);
    End If
    If TT14 = 0 Then 
        Print #1, Tab(125); " 0"
    End If
    If TT14 > 0 Then 
        Print #1, Tab(125); Using "##"; Int((TT14 / 60) / TT1) + 0.5
    End If

    Print #1, Chr$(12)

    Close #1

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
