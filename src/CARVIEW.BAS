'----------------------------------------
'           CARVIEW Subroutine
'----------------------------------------
'This routine loads in an existing career
'file (.CAR) and gives the user options
'for viewing / printing the results
Sub CARVIEW ()

    _Title "Courtside College Basketball - View Careers"

    careerFile$ = _OpenFileDialog$("Choose Career file", diskPaths$(1), "*.CAR", "Career files [.CAR]")
    'ChooseFile$(diskPaths$(1) + "*.CAR", "CAREER", ExitCode%)

    If careerFile$ <> "" Then

        Call LoadCareerFile(careerFile$)

        userKey$ = GetOutputOptions$

        If userKey$ <> Chr$(27) Then

            Select Case userKey$

                Case "F"
                    userFile$ = _SaveFileDialog$("Save Report File", _CWD$ + "\Report", "*.txt", "Text File")
                    If userFile$ <> "" Then
                        Call CareerStats_Report(GetFileBaseName$(careerFile$), 0, userFile$)
                    End If

                Case "P"
                    Call CareerStats_Report(GetFileBaseName$(careerFile$), 1, "CAREER")

                Case "S"
                    Call CareerStats_Screen(GetFileBaseName$(careerFile$))

            End Select

        End If

    End If 'Done checking for ESC

End Sub


'----------------------------------------
'       CareerStats_Report Subroutine
'----------------------------------------
'Formats results of Career stats and sends
'to the user's default printer
Sub CareerStats_Report (careerName$, printDest, destFile$)

    Shared careerPlayers$()

    Shared carPlyrStat!(), car3FGM!(), car3FGA!()

    PTN = 0

    Color 15, 0
    Cls

    Open destFile$ For Output As #1

    Print #1, Ta ,b(59); careerName$
    Print #1,
    Print #1, "PLAYER      YEAR   G  FGM   FGA  PCT  FTM   FTA  PCT   PTS  AVG.  3FGM 3FGA  PCT   RB   AVG   PF  AST  BLK   ST  HI   MIN  AVG."

    For I = 0 To 61

        If careerPlayers$(I) <> "" And careerPlayers$(I) <> "XXX" Then

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

            For Z = 1 To MAX_CAREER_YEARS1
                VV = VV + carPlyrStat!(Z, I, 14)
            Next Z

            If 55 - PTN < VV Then
                Print #1, Chr$(12)
                Print #1, Tab(59); careerName$
                Print #1,
                Print #1, "PLAYER      YEAR   G  FGM   FGA  PCT  FTM   FTA  PCT   PTS  AVG.  3FGM 3FGA  PCT   RB   AVG   PF  AST  BLK   ST  HI   MIN  AVG."
                PTN = 0
            End If

            Print #1, Left$(careerPlayers$(I), 11);

            For V = 1 To VV

                PTN = PTN + 1
                'Year?
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
                'Hi-Game
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

            PTN = PTN + 2

        End If ' Done checking for legitimate player

    Next I

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
'      CareerStats_Screen Subroutine
'----------------------------------------
'This routine gives the user different
'categories of stats related to the roster.
'It then calls a supplemental routine for
'each  that will format those stats and
'send them to the display screen
Sub CareerStats_Screen (careerName$)

    Shared careerPlayers$()

    Do

        Color 15, 0
        Cls

        Locate 4, 10
        Color 7: Print "VIEW ";
        Color 12: Print "P";
        Color 7: Print "LAYERS"

        Locate , 10
        Color 7: Print "VIEW ";
        Color 12: Print "R";
        Color 7: Print "OSTER IN SEQUENCE"

        Locate , 10
        Color 12: Print "E";
        Color 7: Print "XIT TO MAIN MENU"

        I$ = GetKeyPress$

        Select Case UCase$(I$)

            Case "P":
                Call ViewPlayer(careerName$)

            Case "R":
                Call ViewRosterSequence(careerName$)

        End Select

    Loop Until UCase$(I$) = "E" Or UCase$(I$) = "X" Or I$ = Chr$(27)

End Sub


'----------------------------------------
'          ViewPlayer Subroutine
'----------------------------------------
'This subroutine will display player stats
'from the team's career file
Sub ViewPlayer (careerName$)

    Shared careerPlayers$()

    Shared carPlyrStat!(), car3FGM!(), car3FGA!()

    Cls

    Locate 2, (80 - Len(careerName$)) / 2
    Color 12
    Print careerName$

    For I = 0 To 15:
        Locate I + 4
        Color 11: Print Using "##"; I;
        Print " ";
        Color 7: Print Left$(careerPlayers$(I), 12)
    Next

    For I = 16 To 31:
        Locate I - 12, 20
        Color 11: Print Using "##"; I;
        Print " ";
        Color 7: Print Left$(careerPlayers$(I), 12)
    Next

    For I = 32 To 46:
        Locate I - 28, 40
        Color 11: Print Using "##"; I;
        Print " ";
        Color 7: Print Left$(careerPlayers$(I), 12)
    Next

    For I = 47 To 61:
        Locate I - 43, 60
        Color 11: Print Using "##"; I;
        Print " ";
        Color 7: Print Left$(careerPlayers$(I), 12)
    Next

    Print
    Color 14

    Do
        Locate LAST_ROW - 2, 48
        Input "ENTER # OF PLAYER TO VIEW"; B9
    Loop Until B9 >= 0 And B9 <= 61

    Call OnePlayer_Screen (careerName$, B9, VV)

    Color 14
    Locate, 30
    Input "HIT ENTER TO CONTINUE"; DUMMY$

End Sub


'----------------------------------------
'     ViewRosterSequence Subroutine
'----------------------------------------
'This subroutine will display player stats
'from the team's career file, in sequence
Sub ViewRosterSequence (careerName$)

    Shared careerPlayers$()

    Do

        For I = 0 To 61

            'Check for valid player
            If careerPlayers$(I) <> "" And careerPlayers$(I) <> "XXX" Then

                Call OnePlayer_Screen (careerName$, I, VV)

                Do

                    Color 14
                    Locate 11+VV, 16
                    Print "HIT ENTER TO CONTINUE OR ESC TO GO TO MAIN MENU"
                    I$ = GetKeyPress$

                    If I$ = Chr$(27) Then Exit For

                    Cls
                    
                Loop Until I$ = Chr$(27) Or I$ = Chr$(13)

            End If 'Done checking for valid player

        Next I

        If I$ <> Chr$(27) Then

            Cls

            Color 14

            Do
                Locate 5, 20
                Print "VIEW AGAIN? (Y/N)"
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "N" Or UCase$(I$) = "Y" Or I$ = Chr$(27)

        End If

    Loop Until UCase$(I$) = "N" Or I$ = Chr$(27)

End Sub


'----------------------------------------
'      OnePlayer_Screen Subroutine
'----------------------------------------
'This subroutine will display career stats
'on the screen for a single player
Sub OnePlayer_Screen (careerName$, index, nbrYears)

    Shared careerPlayers$()

    Shared carPlyrStat!(), car3FGM!(), car3FGA!()

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

    nbrYears = 0

    Cls

    Color 11
    Locate 2, (80 - Len(careerName$)) / 2
    Print careerName$

    Color 12
    Locate 4, 1
    Print "PLAYER         YEAR  G  FGM  FGA  PCT  FTM  FTA  PCT 3FGM 3FGA  PCT  PTS  AVG."

    Color 15
    Print Left$(careerPlayers$(index), 13);

    For Z = 1 To MAX_CAREER_YEARS
        nbrYears = nbrYears + carPlyrStat!(Z, index, 14)
    Next Z

    For V = 1 To nbrYears

        Color 7

        'Year
        Print Tab(15); carPlyrStat!(V, index, 15);
        'Games
        Print Tab(21); Using "##"; carPlyrStat!(V, index, 0);
        'FGM
        Print Tab(24); Using "####"; carPlyrStat!(V, index, 2);
        'FGA
        Print Tab(29); Using "####"; carPlyrStat!(V, index, 1);

        'FG PCT
        If carPlyrStat!(V, index, 2) = 0 Then
            Print Tab(34); " --.-";
        End If
        If carPlyrStat!(V, index, 2) > 0 Then
            Print Tab(34); Using "###.#"; (carPlyrStat!(V, index, 2) / carPlyrStat!(V, index, 1)) * 100;
        End If

        'FTM
        Print Tab(40); Using "###"; carPlyrStat!(V, index, 4);
        'FTA
        Print Tab(45); Using "###"; carPlyrStat!(V, index, 3);

        'FT PCT
        If carPlyrStat!(V, index, 4) = 0 Then
            Print Tab(48); " --.-";
        End If
        If carPlyrStat!(V, index, 4) > 0 Then
            Print Tab(48); Using "###.#"; (carPlyrStat!(V, index, 4) / carPlyrStat!(V, index, 3)) * 100;
        End If

        '3FGM
        Print Tab(55); Using "###"; car3FGM!(V, I);
        '3FGA
        Print Tab(60); Using "###"; car3FGA!(V, I);

        '3FG PCT
        If car3FGM!(V, I) = 0 Then
            Print Tab(63); " --.-";
        End If
        If car3FGM!(V, I) > 0 Then
            Print Tab(63); Using "###.#"; (car3FGM!(V, I) / car3FGA!(V, I)) * 100;
        End If

        'PTS
        Print Tab(69); Using "####"; carPlyrStat!(V, index, 2) * 2 + carPlyrStat!(V, index, 4) + car3FGM!(V, I);

        'AVG
        If carPlyrStat!(V, index, 2) * 2 + carPlyrStat!(V, index, 4) + car3FGM!(V, I) = 0 Then
            Print Tab(74); " 0.0"
        End If
        If carPlyrStat!(V, index, 2) * 2 + carPlyrStat!(V, index, 4) + car3FGM!(V, I) > 0 Then
            Print Tab(74); Using "##.#"; (carPlyrStat!(V, index, 2) * 2 + carPlyrStat!(V, index, 4) + car3FGM!(V, I)) / carPlyrStat!(V, index, 0)
        End If

        TT1 = TT1 + carPlyrStat!(V, index, 0)
        TT2 = TT2 + carPlyrStat!(V, index, 2)
        TT3 = TT3 + carPlyrStat!(V, index, 1)
        TT4 = TT4 + carPlyrStat!(V, index, 4)
        TT5 = TT5 + carPlyrStat!(V, index, 3)
        TT6 = TT6 + (carPlyrStat!(V, index, 2) * 2 + carPlyrStat!(V, index, 4) + car3FGM!(V, I))
        TT7 = TT7 + car3FGM!(V, I)
        TT8 = TT8 + car3FGA!(V, I)

    Next V

    Print
    Color 3
    Print

    'Games
    Print Tab(18); Using "##"; nbrYears;

    'FGM
    Print Tab(23); Using "#####"; TT2;

    'FG PCT
    If TT2 = 0 Then
        Print Tab(33); " --.-";
    End If
    If TT2 > 0 Then
        Print Tab(33); Using "###.#"; (TT2 / TT3) * 100;
    End If

    'FTA
    Print Tab(43); Using "#####"; TT5;

    '3FGM
    Print Tab(53); Using "####"; TT7;

    '3FG PCT
    If TT7 = 0 Then
        Print Tab(63); " --.-";
    End If
    If TT7 > 0 Then
        Print Tab(63); Using "###.#"; (TT7 / TT8) * 100;
    End If

    'AVG PTS
    If TT6 = 0 Then
        Print Tab(74); " 0.0"
    End If
    If TT6 > 0 Then
        Print Tab(74); Using "##.#"; TT6 / TT1
    End If

    Print "TOTALS";

    'Games
    Print Tab(20); Using "####"; TT1;

    'FGA
    Print Tab(28); Using "#####"; TT3;

    'FTM
    Print Tab(38); Using "#####"; TT4;

    'FT PCT
    If TT4 = 0 Then
        Print Tab(48); " --.-";
    End If
    If TT4 > 0 Then
        Print Tab(48); Using "###.#"; (TT4 / TT5) * 100;
    End If

    '3FGA
    Print Tab(58); Using "####"; TT8;

    'PTS
    Print Tab(69); Using "####"; TT6

    Print
    Print
    Color 14
    Locate, 30
    Input "HIT ENTER TO CONTINUE"; DUMMY$

    Cls

    Locate 2, (80 - Len(careerName$)) / 2
    Color 11
    Print careerName$

    Color 12
    Locate 4, 1
    Print "PLAYER         YEAR OREB DREB  TRB  AVG  PF  AST BLK  ST  TO  HI  MIN  AVG."

    Color 15
    Print Left$(careerPlayers$(index), 13);

    For V = 1 To nbrYears

        Color 7

        'Year
        Print Tab(15); carPlyrStat!(V, index, 15);
        'Off Reb
        Print Tab(21); Using "####"; carPlyrStat!(V, index, 12);
        'Def Reb
        Print Tab(26); Using "####"; carPlyrStat!(V, index, 5) - carPlyrStat!(V, index, 12);
        'Total Reb
        Print Tab(31); Using "####"; carPlyrStat!(V, index, 5);

        'Average
        If carPlyrStat!(V, index, 5) = 0 Then
            Print Tab(36); " 0.0";
        End If
        If carPlyrStat!(V, index, 5) > 0 Then
            Print Tab(36); Using "##.#"; carPlyrStat!(V, index, 5) / carPlyrStat!(V, index, 0);
        End If

        'Personal Fouls
        Print Tab(41); Using "###"; carPlyrStat!(V, index, 6);
        'Assists
        Print Tab(46); Using "###"; carPlyrStat!(V, index, 8);
        'Blocks
        Print Tab(50); Using "###"; carPlyrStat!(V, index, 9);
        'Steals
        Print Tab(54); Using "###"; carPlyrStat!(V, index, 7);
        'Turnovers
        Print Tab(58); Using "###"; carPlyrStat!(V, index, 11);
        'Highest Points
        Print Tab(62); Using "###"; carPlyrStat!(V, index, 13);

        'Minutes
        If carPlyrStat!(V, index, 10) = 0 Then
            Print Tab(65); "   0";
        End If
        If carPlyrStat!(V, index, 10) > 0 Then
            Print Tab(66); Using "####"; Int((carPlyrStat!(V, index, 10) + 30) / 60);
        End If

        'Average Mins
        If carPlyrStat!(V, index, 10) = 0 Then
            Print Tab(73); " 0"
        End If
        If carPlyrStat!(V, index, 10) > 0 Then
            Print Tab(73); Using "##"; CInt(((carPlyrStat!(V, index, 10) / 60) / carPlyrStat!(V, index, 0)))
        End If

        TT9 = TT9 + carPlyrStat!(V, index, 5)
        TT10 = TT10 + carPlyrStat!(V, index, 6)
        TT11 = TT11 + carPlyrStat!(V, index, 8)
        TT12 = TT12 + carPlyrStat!(V, index, 9)
        TT13 = TT13 + carPlyrStat!(V, index, 7)
        TT14 = TT14 + carPlyrStat!(V, index, 10)
        TT16 = TT16 + carPlyrStat!(V, index, 11)
        TT17 = TT17 + carPlyrStat!(V, index, 12)

        If carPlyrStat!(V, index, 13) > TT15 Then
            TT15 = carPlyrStat!(V, index, 13)
        End If

    Next V

    Print
    Color 3
    Print

    Print Tab(18); Using "##"; nbrYears;
    Print Tab(26); Using "####"; TT9 - TT17;

    If TT9 = 0 Then
        Print Tab(36); " 0.0";
    End If
    If TT9 > 0 Then
        Print Tab(36); Using "##.#"; TT9 / TT1;
    End If

    Print Tab(45); Using "####"; TT11;
    Print Tab(54); Using "###"; TT13;
    Print Tab(62); Using "##"; TT15;

    If TT14 = 0 Then
        Print Tab(73); " 0"
    End If
    If TT14 > 0 Then
        Print Tab(73); Using "##"; Int((TT14 / 60) / TT1) + 0.5
    Else
        Print
    End If

    Print "TOTALS";
    Print Tab(21); Using "####"; TT17;
    Print Tab(30); Using "#####"; TT9;
    Print Tab(40); Using "####"; TT10;
    Print Tab(49); Using "####"; TT12;
    Print Tab(58); Using "###"; TT16;

    If TT14 = 0 Then
        Print Tab(65); "    0"
    End If
    If TT14 > 0 Then
        Print Tab(65); Using "#####"; Int((TT14 + 30) / 60)
    End If

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

End Sub
