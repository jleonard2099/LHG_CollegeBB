Sub CARVIEW ()

    _Title "Courtside College Basketball - View Career Files"

    Dim carviewA$(62)

    Shared ARS!(), W0S!(), W1S!()

    Do
        'Display list of files...
        Color 0, 0
        Cls

        Spec$ = diskPaths$(1) + "*.CAR"
        Count% = FCount%(Spec$)

        If Count% = 0 Then
            Locate 10, 8
            Call CSMsgBox("NO CAREER FILES WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, PASS)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, PASS)
        Else
            ReDim careerFiles$(0 To Count%)

            For X = 1 To Count%
                careerFiles$(X) = Space$(12)
            Next

            careerFiles$(0) = Spec$
            Call ReadFile(careerFiles$())

            Do
                Color , 1
                Cls

                Locate , 28
                Color 14
                Print "SELECT FILE TO VIEW": Color 15
                Selection% = 1
                START% = 1
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
                Print "-SELECTS FILE";

                _MouseShow "DEFAULT"
                MAMenu careerFiles$(), Selection%, START%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
                _MouseHide

                If ExitCode% <> 27 Then

                    Color 15, 1
                    targetFile$ = careerFiles$(Selection%)
                    Locate 2, 33: Print targetFile$

                    Do
                        Locate 3, 33: Print "ANY CHANGE (YN)";
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                    Color 15, 0

                End If

            Loop Until UCase$(I$) = "N" Or ExitCode% = 27

            If ExitCode% <> 27 Then

                targetFile$ = RTrim$(targetFile$)
                careerFile$ = Left$(targetFile$, Len(targetFile$) - 4)

                Color 15, 0
                Cls

                TMR% = 930
                
                Locate 12, 10: Print "LOADING CAREER FILE . . . . PLEASE WAIT"

                Open diskPaths$(1) + careerFile$ + ".CAR" For Input As #2

                Input #2, NB%, B$

                For N = 1 To 15

                    For I = 0 To 61

                        Input #2, carviewA$(I)

                        For J = 0 To 15
                            Input #2, ARS!(N, I, J)
                        Next J

                        Input #2, W0S!(N, I)
                        Input #2, W1S!(N, I)

                        Color 5: Locate 12, 52

                        TMR% = TMR% - 1
                        Print Using "####"; TMR%

                    Next I

                Next N

                Close #2

                Color 15, 0
                Cls

                Do
                    Locate 15, 10
                    Color 12: Print "V";: Color 7: Print "iew or ";
                    Color 12: Print "P";: Color 7: Print "rint"
                    A$ = GetKeyPress$
                Loop Until UCase$(A$) = "P" Or UCase$(A$) = "V"

                Select Case UCase$(A$)

                    Case "P":
                        Call CareerToPrinter(carviewA$(), careerFile$)

                    Case "V"
                        Call CareerToScreen(carviewA$(), careerFile$)

                End Select

            End If 'Done checking for ESC

        End If 'Done checking file count

        '...Until we press ESC
    Loop Until ExitCode% = 27 Or Count% = 0

End Sub

'----------------------------------------
'       CareerToPrinter subroutine
'----------------------------------------
'...explanation...
Sub CareerToPrinter (carviewA$(), careerFile$)

    Shared ARS!(), W0S!(), W1S!()

    PTN = 0

    LPrint Tab(59); careerFile$
    LPrint
    LPrint "PLAYER      YEAR   G  FGM   FGA  PCT  FTM   FTA  PCT   PTS  AVG.  3FGM 3FGA  PCT   RB   AVG   PF  AST  BLK   ST  HI   MIN  AVG."

    For I = 0 To 61

        If carviewA$(I) <> "" And carviewA$(I) <> "XXX" Then

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
            VV = ARS!(1, I, 14) + ARS!(2, I, 14) + ARS!(3, I, 14) + ARS!(4, I, 14) + ARS!(5, I, 14) + ARS!(6, I, 14) + ARS!(7, I, 14) + ARS!(8, I, 14) + ARS!(9, I, 14) + ARS!(10, I, 14) + ARS!(11, I, 14) + ARS!(12, I, 14) + ARS!(13, I, 14) + ARS!(14, I, 14) + ARS!(15, I, 14)

            If 55 - PTN < VV Then
                LPrint Chr$(12)
                LPrint Tab(59); careerFile$: LPrint
                LPrint "PLAYER      YEAR   G  FGM   FGA  PCT  FTM   FTA  PCT   PTS  AVG.  3FGM 3FGA  PCT   RB   AVG   PF  AST  BLK   ST  HI   MIN  AVG."
                PTN = 0
            End If

            LPrint Left$(carviewA$(I), 11);

            For V = 1 To VV
                PTN = PTN + 1
                LPrint Tab(12); ARS!(V, I, 15);
                LPrint Tab(19); Using "##"; ARS!(V, I, 0);
                LPrint Tab(23); Using "###"; ARS!(V, I, 2);
                LPrint Tab(28); Using "####"; ARS!(V, I, 1);

                If ARS!(V, I, 2) = 0 Then LPrint Tab(34); "--.-";
                If ARS!(V, I, 2) > 0 Then LPrint Tab(33); Using "###.#"; (ARS!(V, I, 2) / ARS!(V, I, 1)) * 100;

                LPrint Tab(39); Using "###"; ARS!(V, I, 4);
                LPrint Tab(45); Using "###"; ARS!(V, I, 3);

                If ARS!(V, I, 4) = 0 Then LPrint Tab(50); "--.-";
                If ARS!(V, I, 4) > 0 Then LPrint Tab(49); Using "###.#"; (ARS!(V, I, 4) / ARS!(V, I, 3)) * 100;

                LPrint Tab(55); Using "####"; ARS!(V, I, 2) * 2 + ARS!(V, I, 4) + W0S!(V, I);

                If ARS!(V, I, 2) * 2 + ARS!(V, I, 4) + W0S!(V, I) = 0 Then LPrint Tab(61); " 0.0";
                If ARS!(V, I, 2) * 2 + ARS!(V, I, 4) + W0S!(V, I) > 0 Then LPrint Tab(61); Using "##.#"; (ARS!(V, I, 2) * 2 + ARS!(V, I, 4) + W0S!(V, I)) / ARS!(V, I, 0);

                LPrint Tab(68); Using "###"; W0S!(V, I);
                LPrint Tab(73); Using "###"; W1S!(V, I);

                If W0S!(V, I) = 0 Then LPrint Tab(78); "--.-";
                If W0S!(V, I) > 0 Then LPrint Tab(77); Using "###.#"; (W0S!(V, I) / W1S!(V, I)) * 100;

                LPrint Tab(83); Using "###"; ARS!(V, I, 5);

                If ARS!(V, I, 5) = 0 Then LPrint Tab(88); " 0.0";
                If ARS!(V, I, 5) > 0 Then LPrint Tab(88); Using "##.#"; ARS!(V, I, 5) / ARS!(V, I, 0);

                LPrint Tab(94); Using "###"; ARS!(V, I, 6);
                LPrint Tab(99); Using "###"; ARS!(V, I, 8);
                LPrint Tab(104); Using "###"; ARS!(V, I, 9);
                LPrint Tab(109); Using "###"; ARS!(V, I, 7);
                LPrint Tab(113); Using "###"; ARS!(V, I, 13);

                If ARS!(V, I, 10) = 0 Then LPrint Tab(118); "   0";
                If ARS!(V, I, 10) > 0 Then LPrint Tab(118); Using "####"; Int((ARS!(V, I, 10) + 30) / 60);
                If ARS!(V, I, 10) = 0 Then LPrint Tab(125); " 0"
                If ARS!(V, I, 10) > 0 Then LPrint Tab(125); Using "##"; Int(((ARS!(V, I, 10) / 60) / ARS!(V, I, 0)) + .5)

            Next V

            VV = ARS!(1, I, 14) + ARS!(2, I, 14) + ARS!(3, I, 14) + ARS!(4, I, 14) + ARS!(5, I, 14) + ARS!(6, I, 14) + ARS!(7, I, 14) + ARS!(8, I, 14) + ARS!(9, I, 14) + ARS!(10, I, 14) + ARS!(11, I, 14) + ARS!(12, I, 14) + ARS!(13, I, 14) + ARS!(14, I, 14) + ARS!(15, I, 14)

            For V = 1 To VV
                TT1 = TT1 + ARS!(V, I, 0)
                TT2 = TT2 + ARS!(V, I, 2)
                TT3 = TT3 + ARS!(V, I, 1)
                TT4 = TT4 + ARS!(V, I, 4)
                TT5 = TT5 + ARS!(V, I, 3)
                TT6 = TT6 + (ARS!(V, I, 2) * 2 + ARS!(V, I, 4) + W0S!(V, I))
                TT7 = TT7 + W0S!(V, I)
                TT8 = TT8 + W1S!(V, I)
                TT9 = TT9 + ARS!(V, I, 5)
                TT10 = TT10 + ARS!(V, I, 6)
                TT11 = TT11 + ARS!(V, I, 8)
                TT12 = TT12 + ARS!(V, I, 9)
                TT13 = TT13 + ARS!(V, I, 7)
                TT14 = TT14 + ARS!(V, I, 10)
                TT16 = TT16 + ARS!(V, I, 11)
                TT17 = TT17 + ARS!(V, I, 12)
                If ARS!(V, I, 13) > TT15 Then TT15 = ARS!(V, I, 13)
            Next V

            LPrint "  TOTALS";
            LPrint Tab(15); Using "##"; VV;
            LPrint Tab(17); Using "####"; TT1;
            LPrint Tab(21); Using "#####"; TT2;
            LPrint Tab(27); Using "#####"; TT3;

            If TT2 = 0 Then LPrint Tab(34); "--.-";
            If TT2 > 0 Then LPrint Tab(34); Using "###.#"; (TT2 / TT3) * 100;

            LPrint Tab(37); Using "#####"; TT4;
            LPrint Tab(43); Using "#####"; TT5;

            If TT4 = 0 Then LPrint Tab(50); "--.-";
            If TT4 > 0 Then LPrint Tab(50); Using "###.#"; (TT4 / TT5) * 100;

            LPrint Tab(55); Using "####"; TT6;

            If TT6 = 0 Then LPrint Tab(61); " 0.0";
            If TT6 > 0 Then LPrint Tab(61); Using "##.#"; TT6 / TT1;

            LPrint Tab(67); Using "####"; TT7;
            LPrint Tab(72); Using "####"; TT8;

            If TT7 = 0 Then LPrint Tab(78); "--.-";
            If TT7 > 0 Then LPrint Tab(78); Using "###.#"; (TT7 / TT8) * 100;

            LPrint Tab(81); Using "#####"; TT9;

            If TT9 = 0 Then LPrint Tab(88); " 0.0";
            If TT9 > 0 Then LPrint Tab(88); Using "##.#"; TT9 / TT1;

            LPrint Tab(93); Using "####"; TT10;
            LPrint Tab(98); Using "####"; TT11;
            LPrint Tab(103); Using "####"; TT12;
            LPrint Tab(109); Using "###"; TT13;
            LPrint Tab(114); Using "##"; TT15;

            If TT14 = 0 Then LPrint Tab(117); "    0";
            If TT14 > 0 Then LPrint Tab(117); Using "#####"; Int((TT14 + 30) / 60);
            If TT14 = 0 Then LPrint Tab(125); " 0"
            If TT14 > 0 Then LPrint Tab(125); Using "##"; Int((TT14 / 60) / TT1) + .5

            LPrint

            PTN = PTN + 2

        End If

    Next I

    LPrint Chr$(12)

End Sub

'----------------------------------------
'       CareerToScreen subroutine
'----------------------------------------
'...explanation...
Sub CareerToScreen (carviewA$(), careerFile$)

    Do

        Color 15, 0
        Cls

        Color 7: Print "VIEW ";
        Color 12: Print "P";: Color 7: Print "LAYERS, ";
        Color 12: Print "R";: Color 7: Print "OSTER IN SEQUENCE, OR ";
        Color 12: Print "E";: Color 7: Print "XIT TO MAIN MENU"

        I$ = GetKeyPress$

        Select Case UCase$(I$)

            Case "P":
                Call ViewPlayer(carviewA$(), careerFile$)

            Case "R":
                Call ViewRosterSequence(carviewA$(), careerFile$)

        End Select

    Loop Until UCase$(I$) = "E" Or UCase$(I$) = "X" Or I$ = Chr$(27)

End Sub


'----------------------------------------
'       ViewPlayer Subroutine
'----------------------------------------
'...explanation...
Sub ViewPlayer (carviewA$(), careerFile$)

    Shared ARS!(), W0S!(), W1S!()

    Cls

    Color 12: Print careerFile$

    For I = 0 To 15:
        Locate I + 2: Color 3
        Print Using "##"; I;
        Print " ";
        Color 7: Print carviewA$(I)
    Next

    For I = 16 To 31:
        Locate I - 14, 20: Color 3
        Print Using "##"; I;
        Print " ";
        Color 7: Print carviewA$(I)
    Next

    For I = 32 To 46:
        Locate I - 30, 40: Color 3
        Print Using "##"; I;
        Print " ";
        Color 7: Print carviewA$(I)
    Next

    For I = 47 To 61:
        Locate I - 45, 60: Color 3
        Print Using "##"; I;
        Print " ";
        Color 7: Print carviewA$(I)
    Next

    Print
    Color 15
    Locate 22, 48: Input "ENTER # OF PLAYER TO VIEW"; B9

    Cls

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

    Color 11
    Locate 1: Print careerFile$
    Color 12
    Locate 2: Print "PLAYER         YEAR  G  FGM  FGA  PCT FTM  FTA  PCT 3FGM 3FGA  PCT  PTS  AVG."
    Color 15
    Locate 3: Print Left$(carviewA$(B9), 13);

    VV = ARS!(1, B9, 14) + ARS!(2, B9, 14) + ARS!(3, B9, 14) + ARS!(4, B9, 14) + ARS!(5, B9, 14) + ARS!(6, B9, 14) + ARS!(7, B9, 14) + ARS!(8, B9, 14) + ARS!(9, B9, 14) + ARS!(10, B9, 14) + ARS!(11, B9, 14) + ARS!(12, B9, 14) + ARS!(13, B9, 14) + ARS!(14, B9, 14) + ARS!(15, B9, 14)

    For V = 1 To VV
        Color 7

        'Year
        Print Tab(15); ARS!(V, B9, 15);
        'G
        Print Tab(21); Using "##"; ARS!(V, B9, 0);
        'FGM
        Print Tab(24); Using "####"; ARS!(V, B9, 2);
        'FGA
        Print Tab(29); Using "####"; ARS!(V, B9, 1);

        'FG PCT
        If ARS!(V, B9, 2) = 0 Then Print Tab(35); "--.-";
        If ARS!(V, B9, 2) > 0 Then Print Tab(34); Using "###.#"; (ARS!(V, B9, 2) / ARS!(V, B9, 1)) * 100;

        'FTM
        Print Tab(39); Using "###"; ARS!(V, B9, 4);
        'FTA
        Print Tab(44); Using "###"; ARS!(V, B9, 3);

        'FT PCT
        If ARS!(V, B9, 4) = 0 Then Print Tab(49); "--.-";
        If ARS!(V, B9, 4) > 0 Then Print Tab(48); Using "###.#"; (ARS!(V, B9, 4) / ARS!(V, B9, 3)) * 100;

        '3FGM
        Print Tab(54); Using "###"; W0S!(V, B9);
        '3FGA
        Print Tab(59); Using "###"; W1S!(V, B9);

        '3FG PCT
        If W0S!(V, B9) = 0 Then Print Tab(64); "--.-";
        If W0S!(V, B9) > 0 Then Print Tab(63); Using "###.#"; (W0S!(V, B9) / W1S!(V, B9)) * 100;

        'PTS
        Print Tab(68); Using "####"; ARS!(V, B9, 2) * 2 + ARS!(V, B9, 4) + W0S!(V, B9);

        'AVG
        If ARS!(V, B9, 2) * 2 + ARS!(V, B9, 4) + W0S!(V, B9) = 0 Then Print Tab(74); " 0.0"
        If ARS!(V, B9, 2) * 2 + ARS!(V, B9, 4) + W0S!(V, B9) > 0 Then Print Tab(74); Using "##.#"; (ARS!(V, B9, 2) * 2 + ARS!(V, B9, 4) + W0S!(V, B9)) / ARS!(V, B9, 0)

    Next V

    For V = 1 To VV
        TT1 = TT1 + ARS!(V, B9, 0)
        TT2 = TT2 + ARS!(V, B9, 2)
        TT3 = TT3 + ARS!(V, B9, 1)
        TT4 = TT4 + ARS!(V, B9, 4)
        TT5 = TT5 + ARS!(V, B9, 3)
        TT6 = TT6 + (ARS!(V, B9, 2) * 2 + ARS!(V, B9, 4) + W0S!(V, B9))
        TT7 = TT7 + W0S!(V, B9)
        TT8 = TT8 + W1S!(V, B9)
    Next V

    Print
    Color 3
    Print

    Print Tab(18); Using "##"; VV;
    Print Tab(23); Using "#####"; TT2;

    If TT2 = 0 Then Print Tab(35); "--.-";
    If TT2 > 0 Then Print Tab(35); Using "##.#"; (TT2 / TT3) * 100;

    Print Tab(42); Using "#####"; TT5;
    Print Tab(53); Using "####"; TT7;

    If TT7 = 0 Then Print Tab(64); "--.-";
    If TT7 > 0 Then Print Tab(64); Using "##.#"; (TT7 / TT8) * 100;
    If TT6 = 0 Then Print Tab(74); " 0.0"
    If TT6 > 0 Then Print Tab(74); Using "##.#"; TT6 / TT1

    Print "TOTALS";
    Print Tab(19); Using "####"; TT1;
    Print Tab(28); Using "#####"; TT3;
    Print Tab(37); Using "#####"; TT4;

    If TT4 = 0 Then Print Tab(49); "--.-";
    If TT4 > 0 Then Print Tab(49); Using "##.#"; (TT4 / TT5) * 100;

    Print Tab(58); Using "####"; TT8;
    Print Tab(68); Using "####"; TT6
    Print
    Print
    Print
    Color 14: Line Input "HIT ENTER TO CONTINUE"; DUMMY$

    Cls
    Color 11
    Locate 1: Print careerFile$
    Color 12: Print "PLAYER         YEAR OREB DREB  TRB  AVG  PF  AST BLK  ST  TO HI   MIN  AVG."
    Color 15
    Locate 3: Print Left$(carviewA$(B9), 13);

    For V = 1 To VV

        Color 7

        Print Tab(15); ARS!(V, B9, 15);
        Print Tab(21); Using "####"; ARS!(V, B9, 12);
        Print Tab(26); Using "####"; ARS!(V, B9, 5) - ARS!(V, B9, 12);
        Print Tab(31); Using "####"; ARS!(V, B9, 5);

        If ARS!(V, B9, 5) = 0 Then Print Tab(36); " 0.0";
        If ARS!(V, B9, 5) > 0 Then Print Tab(36); Using "##.#"; ARS!(V, B9, 5) / ARS!(V, B9, 0);

        Print Tab(41); Using "###"; ARS!(V, B9, 6);
        Print Tab(46); Using "###"; ARS!(V, B9, 8);
        Print Tab(50); Using "###"; ARS!(V, B9, 9);
        Print Tab(54); Using "###"; ARS!(V, B9, 7);
        Print Tab(58); Using "###"; ARS!(V, B9, 11);
        Print Tab(61); Using "###"; ARS!(V, B9, 13);

        If ARS!(V, B9, 10) = 0 Then Print Tab(65); "   0";
        If ARS!(V, B9, 10) > 0 Then Print Tab(66); Using "####"; Int((ARS!(V, B9, 10) + 30) / 60);
        If ARS!(V, B9, 10) = 0 Then Print Tab(73); " 0";
        If ARS!(V, B9, 10) > 0 Then Print Tab(73); Using "##"; Int(((ARS!(V, B9, 10) / 60) / ARS!(V, B9, 0)) + .5);

    Next V

    For V = 1 To VV
        TT9 = TT9 + ARS!(V, B9, 5)
        TT10 = TT10 + ARS!(V, B9, 6)
        TT11 = TT11 + ARS!(V, B9, 8)
        TT12 = TT12 + ARS!(V, B9, 9)
        TT13 = TT13 + ARS!(V, B9, 7)
        TT14 = TT14 + ARS!(V, B9, 10)
        TT16 = TT16 + ARS!(V, B9, 11)
        TT17 = TT17 + ARS!(V, B9, 12)
        If ARS!(V, B9, 13) > TT15 Then TT15 = ARS!(V, B9, 13)
    Next V

    Print
    Color 3
    Print
    Print Tab(18); Using "##"; VV;
    Print Tab(26); Using "####"; TT9 - TT17;

    If TT9 = 0 Then Print Tab(36); " 0.0";
    If TT9 > 0 Then Print Tab(36); Using "##.#"; TT9 / TT1;

    Print Tab(45); Using "####"; TT11;
    Print Tab(54); Using "###"; TT13;
    Print Tab(62); Using "##"; TT15;

    If TT14 = 0 Then Print Tab(73); "--.-"
    If TT14 > 0 Then Print Tab(73); Using "##"; Int((TT14 / 60) / TT1) + .5

    Print
    Print "TOTALS";
    Print Tab(21); Using "####"; TT17;
    Print Tab(30); Using "#####"; TT9;
    Print Tab(40); Using "####"; TT10;
    Print Tab(49); Using "####"; TT12;
    Print Tab(58); Using "###"; TT16;

    If TT14 = 0 Then Print Tab(65); "  --.-"
    If TT14 > 0 Then Print Tab(65); Using "#####"; Int((TT14 + 30) / 60)

    Print
    Print
    Color 14
    Input "HIT ENTER TO CONTINUE"; DUMMY$


End Sub


'----------------------------------------
'       ViewRosterSequence Subroutine
'----------------------------------------
'...explanation...
Sub ViewRosterSequence (carviewA$(), careerFile$)

    Shared ARS!(), W0S!(), W1S!()

    Do

        For I = 0 To 61

            If carviewA$(I) <> "" And carviewA$(I) <> "XXX" Then

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

                Cls

                Color 11
                Locate 1: Print careerFile$
                Color 12
                Locate 2: Print "PLAYER         YEAR  G FGM  FGA   PCT FTM FTA   PCT 3FGM 3FGA PCT  PTS  AVG"
                Color 15
                Locate 3: Print Left$(carviewA$(I), 13);
                VV = ARS!(1, I, 14) + ARS!(2, I, 14) + ARS!(3, I, 14) + ARS!(4, I, 14) + ARS!(5, I, 14) + ARS!(6, I, 14) + ARS!(7, I, 14) + ARS!(8, I, 14) + ARS!(9, I, 14) + ARS!(10, I, 14) + ARS!(11, I, 14) + ARS!(12, I, 14) + ARS!(13, I, 14) + ARS!(14, I, 14) + ARS!(15, I, 14)

                For V = 1 To VV

                    Color 7: Print Tab(15); ARS!(V, I, 15);

                    Print Tab(21); Using "##"; ARS!(V, I, 0);
                    Print Tab(24); Using "####"; ARS!(V, I, 2);
                    Print Tab(28); Using "####"; ARS!(V, I, 1);

                    If ARS!(V, I, 2) = 0 Then Print Tab(33); " --.-";
                    If ARS!(V, I, 2) > 0 Then Print Tab(33); Using "###.#"; (ARS!(V, I, 2) / ARS!(V, I, 1)) * 100;

                    Print Tab(39); Using "###"; ARS!(V, I, 4);
                    Print Tab(43); Using "###"; ARS!(V, I, 3);

                    If ARS!(V, I, 4) = 0 Then Print Tab(47); " --.-";
                    If ARS!(V, I, 4) > 0 Then Print Tab(47); Using "###.#"; (ARS!(V, I, 4) / ARS!(V, I, 3)) * 100;

                    Print Tab(53); Using "###"; W0S!(V, I);
                    Print Tab(57); Using "###"; W1S!(V, I);

                    If W0S!(V, I) = 0 Then Print Tab(61); " --.-";
                    If W0S!(V, I) > 0 Then Print Tab(61); Using "###.#"; (W0S!(V, I) / W1S!(V, I)) * 100;

                    Print Tab(67); Using "####"; ARS!(V, I, 2) * 2 + ARS!(V, I, 4) + W0S!(V, I);

                    If ARS!(V, I, 2) * 2 + ARS!(V, I, 4) + W0S!(V, I) = 0 Then Print Tab(72); " 0.0";
                    If ARS!(V, I, 2) * 2 + ARS!(V, I, 4) + W0S!(V, I) > 0 Then Print Tab(72); Using "##.#"; (ARS!(V, I, 2) * 2 + ARS!(V, I, 4) + W0S!(V, I)) / ARS!(V, I, 0);
                Next V

                VV = ARS!(1, I, 14) + ARS!(2, I, 14) + ARS!(3, I, 14) + ARS!(4, I, 14) + ARS!(5, I, 14) + ARS!(6, I, 14) + ARS!(7, I, 14) + ARS!(8, I, 14) + ARS!(9, I, 14) + ARS!(10, I, 14) + ARS!(11, I, 14) + ARS!(12, I, 14) + ARS!(13, I, 14) + ARS!(14, I, 14) + ARS!(15, I, 14)

                For V = 1 To VV
                    TT1 = TT1 + ARS!(V, I, 0)
                    TT2 = TT2 + ARS!(V, I, 2)
                    TT3 = TT3 + ARS!(V, I, 1)
                    TT4 = TT4 + ARS!(V, I, 4)
                    TT5 = TT5 + ARS!(V, I, 3)
                    TT6 = TT6 + (ARS!(V, I, 2) * 2 + ARS!(V, I, 4) + W0S!(V, I))
                    TT7 = TT7 + W0S!(V, I)
                    TT8 = TT8 + W1S!(V, I)
                Next V

                Print
                Color 3
                Print
                Print Tab(18); Using "##"; VV;
                Print Tab(22); Using "#####"; TT2;

                If TT2 = 0 Then Print Tab(33); " --.-";
                If TT2 > 0 Then Print Tab(33); Using "###.#"; (TT2 / TT3) * 100;

                Print Tab(41); Using "#####"; TT5;
                Print Tab(52); Using "####"; TT7;

                If TT7 = 0 Then Print Tab(61); " --.-";
                If TT7 > 0 Then Print Tab(61); Using "###.#"; (TT7 / TT8) * 100;
                If TT6 = 0 Then Print Tab(72); " 0.0"
                If TT6 > 0 Then Print Tab(72); Using "##.#"; TT6 / TT1

                Print
                Print "TOTALS";
                Print Tab(19); Using "####"; TT1;
                Print Tab(27); Using "#####"; TT3;
                Print Tab(37); Using "#####"; TT4;

                If TT4 = 0 Then Print Tab(47); " --.-";
                If TT4 > 0 Then Print Tab(47); Using "###.#"; (TT4 / TT5) * 100;

                Print Tab(56); Using "####"; TT8;
                Print Tab(67); Using "####"; TT6
                Print
                Print
                Print

                Color 14: Line Input "HIT ENTER TO CONTINUE"; DUMMY$

                Cls
                Color 11
                Locate 1: Print careerFile$
                Color 12
                Print "PLAYER         YEAR OREB DREB  TRB  AVG  PF  AST BLK  ST  TO HI   MIN  AVG."
                Color 15
                Locate 3: Print Left$(carviewA$(I), 13);

                For V = 1 To VV
                    Color 7: Print Tab(15); ARS!(V, I, 15);

                    Print Tab(21); Using "####"; ARS!(V, I, 12);
                    Print Tab(26); Using "####"; ARS!(V, I, 5) - ARS!(V, I, 12);
                    Print Tab(31); Using "####"; ARS!(V, I, 5);

                    If ARS!(V, I, 5) = 0 Then Print Tab(36); " 0.0";
                    If ARS!(V, I, 5) > 0 Then Print Tab(36); Using "##.#"; ARS!(V, I, 5) / ARS!(V, I, 0);

                    Print Tab(41); Using "###"; ARS!(V, I, 6);
                    Print Tab(46); Using "###"; ARS!(V, I, 8);
                    Print Tab(50); Using "###"; ARS!(V, I, 9);
                    Print Tab(54); Using "###"; ARS!(V, I, 7);
                    Print Tab(58); Using "###"; ARS!(V, I, 11);
                    Print Tab(61); Using "###"; ARS!(V, I, 13);

                    If ARS!(V, I, 10) = 0 Then Print Tab(65); "   0";
                    If ARS!(V, I, 10) > 0 Then Print Tab(66); Using "####"; Int((ARS!(V, I, 10) + 30) / 60);
                    If ARS!(V, I, 10) = 0 Then Print Tab(73); " 0";
                    If ARS!(V, I, 10) > 0 Then Print Tab(73); Using "##"; Int(((ARS!(V, I, 10) / 60) / ARS!(V, I, 0)) + .5);
                Next V

                VV = ARS!(1, I, 14) + ARS!(2, I, 14) + ARS!(3, I, 14) + ARS!(4, I, 14) + ARS!(5, I, 14) + ARS!(6, I, 14) + ARS!(7, I, 14) + ARS!(8, I, 14) + ARS!(9, I, 14) + ARS!(10, I, 14) + ARS!(11, I, 14) + ARS!(12, I, 14) + ARS!(13, I, 14) + ARS!(14, I, 14) + ARS!(15, I, 14)

                For V = 1 To VV
                    TT9 = TT9 + ARS!(V, I, 5)
                    TT10 = TT10 + ARS!(V, I, 6)
                    TT11 = TT11 + ARS!(V, I, 8)
                    TT12 = TT12 + ARS!(V, I, 9)
                    TT13 = TT13 + ARS!(V, I, 7)
                    TT14 = TT14 + ARS!(V, I, 10)
                    TT16 = TT16 + ARS!(V, I, 11)
                    TT17 = TT17 + ARS!(V, I, 12)
                    If ARS!(V, I, 13) > TT15 Then TT15 = ARS!(V, I, 13)
                Next V

                Print
                Color 3
                Print
                Print Tab(18); Using "##"; VV;
                Print Tab(26); Using "####"; TT9 - TT17;

                If TT9 = 0 Then Print Tab(36); " 0.0";
                If TT9 > 0 Then Print Tab(36); Using "##.#"; TT9 / TT1;

                Print Tab(45); Using "####"; TT11;
                Print Tab(54); Using "###"; TT13;
                Print Tab(62); Using "##"; TT15;

                If TT14 = 0 Then Print Tab(73); " 0"
                If TT14 > 0 Then Print Tab(73); Using "##"; Int((TT14 / 60) / TT1) + .5

                Print "TOTALS";
                Print Tab(21); Using "####"; TT17;
                Print Tab(30); Using "#####"; TT9;
                Print Tab(40); Using "####"; TT10;
                Print Tab(49); Using "####"; TT12;
                Print Tab(58); Using "###"; TT16;

                If TT14 = 0 Then Print Tab(65); "    0"
                If TT14 > 0 Then Print Tab(65); Using "#####"; Int((TT14 + 30) / 60)

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

                Print
                Print
                Print
                Color 14
                Print "HIT ENTER TO CONTINUE OR ESC TO GO TO MAIN MENU"
                I$ = GetKeyPress$

                If I$ = Chr$(27) Then Exit Sub

                Cls

            End If

        Next I

        Cls

        Color 14

        Do
            Locate 5, 20
            Print "VIEW AGAIN (Y/N)"
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "N" Or UCase$(I$) = "Y"

    Loop Until UCase$(I$) = "N"

End Sub

