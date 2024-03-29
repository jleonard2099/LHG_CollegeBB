'----------------------------------------
'          HD2HDCON Subroutine
'----------------------------------------
'This subroutine reads in stats from the
'selected team and then presents the user
'a menu of options for viewing Head-to-Head
'team comparisons.
Sub HD2HDCON ()

    _Title "Courtside College Basketball - Head-to-Head Stats"

    Shared teamScore(), oppScore()

    Shared AL!(), AW!(), HL!(), HW!()
    Shared R1!(), R2!(), hd2hdR3!(), R4!()
    Shared TAL!(), TAW!(), THL!(), THW!()
    Shared TR1!(), TR2!(), TR3!(), TR4!()

    Shared locIndicator$(), oppName$(), Z1$()

    Dim Z1_HD2HD(40), Z2_HD2HD$(40)

    Shared D1$, D2$

    teamNbr = 0
    Call ChooseTeam (YN$, NTMS, teamNbr, team$)
    statFile$ = nameStatFile$(team$)

    If Not _FileExists(diskPaths$(1) + statFile$ + "S") Then

        MsgText$ = "NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU"
        BoxWidth = 24
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

    Else

        Open diskPaths$(1) + statFile$ + "S" For Input As #1

        Input #1, Y

        For J = 1 To MAX_SCHED_STATS: 
            Input #1, teamScore(J)
        Next

        For J = 1 To MAX_SCHED_STATS: 
            Input #1, oppName$(J)
        Next

        For J = 1 To MAX_SCHED_STATS: 
            Input #1, oppScore(J)
        Next

        For J = 1 To MAX_SCHED_STATS: 
            Input #1, locIndicator$(J)
        Next

        Close #1

        conferenceFile$ = ChooseFile$ (diskPaths$(1) + "*.LIB", "CONFERENCE", ExitCode%)

        If ExitCode% <> 27 And ExitCode% <> 0 Then

            Cls

            Call MenuBox (8, 8, 61, 5, 15, 1)

            Do

                Locate 10
                Color 7, 1

                Locate , 30: Print "STATS TO ";
                Color 15: Print "S";
                Color 7: Print "CREEN"
                Locate , 30: Print "STATS TO ";
                Color 15: Print "P";
                Color 7: Print "RINTER"

                I$ = GetKeyPress$
                
                Color 14, 0
                
            Loop Until UCase$(I$) = "S" Or UCase$(I$) = "P"

            VPP = 0

            If UCase$(I$) = "P" Then
                X1 = 1
            Else
                VPP = 1
            End If

            Open diskPaths$(1) + conferenceFile$ + ".LIB" For Input As #2

            Input #2, X
            Input #2, conf$
            Input #2, D1$
            Input #2, D2$

            For X = 1 To 30
                Input #2, Z1$(X)
                Input #2, Z1_HD2HD(X)
                Input #2, Z2_HD2HD$(X)
            Next

            NTMS = 30

            Close #2

            Cls
            Color 15
            Locate 18, 18: Print "COMPUTING W/L/T RECORDS . . . PLEASE WAIT"

            Erase AL!, AW!, HL!, HW!
            Erase R1!, R2!, hd2hdR3!, R4!
            Erase TAL!, TAW!, THL!, THW!, TR1!, TR2!, TR3!, TR4!

            For K = 1 To 30

                If Z1$(K) <> "XXX" Then

                    For I = 1 To Y
                        If teamScore(I) > oppScore(I) And Left$(locIndicator$(I), 1) = "V" And oppName$(I) = Z1$(K) Then AW!(K) = AW!(K) + 1
                        If teamScore(I) > oppScore(I) And (Left$(locIndicator$(I), 1) = "H" Or Left$(locIndicator$(I), 1) = "N") And oppName$(I) = Z1$(K) Then HW!(K) = HW!(K) + 1
                        If teamScore(I) < oppScore(I) And Left$(locIndicator$(I), 1) = "V" And oppName$(I) = Z1$(K) Then AL!(K) = AL!(K) + 1
                        If teamScore(I) < oppScore(I) And (Left$(locIndicator$(I), 1) = "H" Or Left$(locIndicator$(I), 1) = "N") And oppName$(I) = Z1$(K) Then HL!(K) = HL!(K) + 1
                        If oppName$(I) = Z1$(K) And (Left$(locIndicator$(I), 1) = "H" Or Left$(locIndicator$(I), 1) = "N") Then R2!(K) = R2!(K) + oppScore(I)
                        If oppName$(I) = Z1$(K) And (Left$(locIndicator$(I), 1) = "H" Or Left$(locIndicator$(I), 1) = "N") Then R1!(K) = R1!(K) + teamScore(I)
                        If oppName$(I) = Z1$(K) And Left$(locIndicator$(I), 1) = "V" Then R4!(K) = R4!(K) + oppScore(I)
                        If oppName$(I) = Z1$(K) And Left$(locIndicator$(I), 1) = "V" Then hd2hdR3!(K) = hd2hdR3!(K) + teamScore(I)
                    Next I

                End If

            Next K

            If VPP <> 1 Then
                Call Hd2HdColl_Printer(team$)
            Else
                Call Hd2HdColl_Screen(team$)
            End If
        
        End If

    End If 'Done checking for stats file

End Sub


'----------------------------------------
'      Hd2HdColl_Printer Subroutine
'----------------------------------------
'This subroutine outputs the calculated 
'comparison values to the default printer
Sub Hd2HdColl_Printer (teamName$)

    Shared D1$, D2$

    Shared teamRatings%()

    Shared Z1$()

    Shared AL!(), AW!(), HL!(), HW!()
    Shared R1!(), R2!(), hd2hdR3!(), R4!()
    Shared TAL!(), TAW!(), THL!(), THW!()
    Shared TR1!(), TR2!(), TR3!(), TR4!()

    DV$(1) = D1$
    DV$(2) = D2$

    LPrint Tab(55); teamName$
    'LPrint Tab(35); teamName$
    LPrint
    LPrint Tab(22); "--------- HOME ----------"; Tab(54); "--------- AWAY ----------"; Tab(86); "---------- TOTALS ----------"
    'LPrint Tab(15); "--------- HOME ----------"; Tab(36); "--------- AWAY ----------"; Tab(58); "---------- TOTALS ----------"

    LPrint Tab(22); "W"; Tab(26); "L"; Tab(30); "PCT."; Tab(36); "PF/G"; Tab(43); "PA/G"; Tab(54); "W"; Tab(58); "L"; Tab(62); "PCT."; Tab(68); "PF/G"; Tab(75); "PA/G"; Tab(86); "W"; Tab(91); "L"; Tab(97); "PCT."; Tab(103); "PF/G"; Tab(110); "PA/G"
    'LPrint Tab(15); "W"; Tab(17); "L"; Tab(21); "PCT."; Tab(26); "PF/G"; Tab(32); "PA/G"; Tab(37); "W"; Tab(39); "L"; Tab(43); "PCT."; Tab(48); "PF/G"; Tab(54); "PA/G"; Tab(59); "W"; Tab(61); "L"; Tab(65); "PCT."; Tab(70); "PF/G"; Tab(76); "PA/G"

    Do

        RDD = RDD + 1

        DD = ((RDD - 1) * 10) + 1
        EE = RDD * 10

        If DV$(RDD) <> "XXX" And DV$(RDD) <> "" Then

            LPrint Tab(2); DV$(RDD)

            For K = DD To EE

                If Z1$(K) <> "XXX" And Z1$(K) <> teamName$ Then
                    LPrint Z1$(K);
                    LPrint Tab(22); Using "#"; HW!(K);
                    LPrint Tab(26); Using "#"; HL!(K);

                    If HW!(K) = HW!(K) + HL!(K) And HW!(K) <> 0 Then LPrint Tab(28); "1.000";
                    If HW!(K) = 0 Then LPrint Tab(29); ".000";
                    If HW!(K) > 0 And HW!(K) < HW!(K) + HL!(K) Then statPCT! = (HW!(K) / (HW!(K) + HL!(K))): LPrint Tab(29); Using ".###"; statPCT!;
                    If R1!(K) = 0 Then LPrint Tab(35); "  0.0";
                    If R1!(K) > 0 Then LPrint Tab(35); Using "###.#"; R1!(K) / (HW!(K) + HL!(K));
                    If R2!(K) = 0 Then LPrint Tab(42); "  0.0";
                    If R2!(K) > 0 Then LPrint Tab(42); Using "###.#"; R2!(K) / (HW!(K) + HL!(K));

                    LPrint Tab(54); Using "#"; AW!(K);
                    LPrint Tab(58); Using "#"; AL!(K);

                    If AW!(K) = AW!(K) + AL!(K) And AW!(K) <> 0 Then LPrint Tab(60); "1.000";
                    If AW!(K) = 0 Then LPrint Tab(61); ".000";
                    If AW!(K) > 0 And AW!(K) < AW!(K) + AL!(K) Then statPCT! = (AW!(K) / (AW!(K) + AL!(K))): LPrint Tab(61); Using ".###"; statPCT!;
                    If hd2hdR3!(K) = 0 Then LPrint Tab(67); "  0.0";
                    If hd2hdR3!(K) > 0 Then LPrint Tab(67); Using "###.#"; hd2hdR3!(K) / (AW!(K) + AL!(K));
                    If R4!(K) = 0 Then LPrint Tab(74); "  0.0";
                    If R4!(K) > 0 Then LPrint Tab(74); Using "###.#"; R4!(K) / (AW!(K) + AL!(K));

                    LPrint Tab(86); Using "##"; HW!(K) + AW!(K);
                    LPrint Tab(91); Using "##"; HL!(K) + AL!(K);

                    If HW!(K) + AW!(K) = HW!(K) + AW!(K) + HL!(K) + AL!(K) And HW!(K) + AW!(K) <> 0 Then LPrint Tab(95); "1.000";
                    If (HW!(K) + AW!(K)) = 0 Then LPrint Tab(96); ".000";
                    If (HW!(K) + AW!(K)) > 0 And (HW!(K) + AW!(K)) < (HW!(K) + AW!(K) + HL!(K) + AL!(K)) Then statPCT! = ((HW!(K) + AW!(K)) / (HW!(K) + AW!(K) + HL!(K) + AL!(K))): LPrint Tab(96); Using ".###"; statPCT!;
                    If R1!(K) + hd2hdR3!(K) = 0 Then LPrint Tab(102); "  0.0";
                    If R1!(K) + hd2hdR3!(K) > 0 Then LPrint Tab(102); Using "###.#"; (R1!(K) + hd2hdR3!(K)) / (HW!(K) + HL!(K) + AW!(K) + AL!(K));
                    If R2!(K) + R4!(K) = 0 Then LPrint Tab(109); "  0.0"
                    If R2!(K) + R4!(K) > 0 Then LPrint Tab(109); Using "###.#"; (R2!(K) + R4!(K)) / (HW!(K) + HL!(K) + AW!(K) + AL!(K))

                    THW!(RDD) = THW!(RDD) + HW!(K)
                    THL!(RDD) = THL!(RDD) + HL!(K)
                    TAW!(RDD) = TAW!(RDD) + AW!(K)
                    TAL!(RDD) = TAL!(RDD) + AL!(K)
                    TR1!(RDD) = TR1!(RDD) + R1!(K)
                    TR2!(RDD) = TR2!(RDD) + R2!(K)
                    TR3!(RDD) = TR3!(RDD) + hd2hdR3!(K)
                    TR4!(RDD) = TR4!(RDD) + R4!(K)
                End If

            Next K

            LPrint Tab(5); "TOTALS";
            LPrint Tab(21); Using "##"; THW!(RDD);
            LPrint Tab(25); Using "##"; THL!(RDD);

            If THW!(RDD) = THW!(RDD) + THL!(RDD) And THW!(RDD) <> 0 Then LPrint Tab(28); "1.000";
            If THW!(RDD) = 0 Then LPrint Tab(29); ".000";
            If THW!(RDD) > 0 And THW!(RDD) < THW!(RDD) + THL!(RDD) Then LPrint Tab(29); Using ".###"; (THW!(RDD) / (THW!(RDD) + THL!(RDD)));
            If TR1!(RDD) = 0 Then LPrint Tab(35); "  0.0";
            If TR1!(RDD) > 0 Then LPrint Tab(35); Using "###.#"; TR1!(RDD) / (THW!(RDD) + THL!(RDD));
            If TR2!(RDD) = 0 Then LPrint Tab(42); "  0.0";
            If TR2!(RDD) > 0 Then LPrint Tab(42); Using "###.#"; TR2!(RDD) / (THW!(RDD) + THL!(RDD));

            LPrint Tab(53); Using "##"; TAW!(RDD);
            LPrint Tab(57); Using "##"; TAL!(RDD);

            If TAW!(RDD) = TAW!(RDD) + TAL!(RDD) And TAW!(RDD) <> 0 Then LPrint Tab(60); "1.000";
            If TAW!(RDD) = 0 Then LPrint Tab(61); ".000";
            If TAW!(RDD) > 0 And TAW!(RDD) < TAW!(RDD) + TAL!(RDD) Then statPCT! = (TAW!(RDD) / (TAW!(RDD) + TAL!(RDD))): LPrint Tab(61); Using ".###"; statPCT!;
            If TR3!(RDD) = 0 Then LPrint Tab(67); "  0.0";
            If TR3!(RDD) > 0 Then LPrint Tab(67); Using "###.#"; TR3!(RDD) / (TAW!(RDD) + TAL!(RDD));
            If TR4!(RDD) = 0 Then LPrint Tab(74); "  0.0";
            If TR4!(RDD) > 0 Then LPrint Tab(74); Using "###.#"; TR4!(RDD) / (TAW!(RDD) + TAL!(RDD));

            LPrint Tab(84); Using "###"; THW!(RDD) + TAW!(RDD);
            LPrint Tab(89); Using "###"; THL!(RDD) + TAL!(RDD);

            If THW!(RDD) + TAW!(RDD) = THW!(RDD) + TAW!(RDD) + THL!(RDD) + TAL!(RDD) And THW!(RDD) + TAW!(RDD) <> 0 Then LPrint Tab(95); "1.000";
            If THW!(RDD) + TAW!(RDD) = 0 Then LPrint Tab(96); ".000";
            If THW!(RDD) + TAW!(RDD) > 0 And THW!(RDD) + TAW!(RDD) < THW!(RDD) + TAW!(RDD) + THL!(RDD) + TAL!(RDD) Then statPCT! = ((THW!(RDD) + TAW!(RDD)) / (THW!(RDD) + TAW!(RDD) + THL!(RDD) + TAL!(RDD))): LPrint Tab(96); Using ".###"; statPCT!;
            If TR1!(RDD) + TR3!(RDD) = 0 Then LPrint Tab(102); "  0.0";
            If TR1!(RDD) + TR3!(RDD) > 0 Then LPrint Tab(102); Using "###.#"; (TR1!(RDD) + TR3!(RDD)) / (THW!(RDD) + THL!(RDD) + TAW!(RDD) + TAL!(RDD));
            If TR2!(RDD) + TR4!(RDD) = 0 Then LPrint Tab(109); "  0.0"
            If TR2!(RDD) + TR4!(RDD) > 0 Then LPrint Tab(109); Using "###.#"; (TR2!(RDD) + TR4!(RDD)) / (THW!(RDD) + THL!(RDD) + TAW!(RDD) + TAL!(RDD))

            LPrint

        End If

    Loop Until RDD = 2

    LPrint
    LPrint
    LPrint Tab(22); "--------- HOME ----------"; Tab(54); "--------- AWAY ----------"; Tab(86); "---------- TOTALS ----------"
    LPrint Tab(22); "W"; Tab(26); "L"; Tab(30); "PCT."; Tab(36); "PF/G"; Tab(43); "PA/G"; Tab(54); "W"; Tab(58); "L"; Tab(62); "PCT."; Tab(68); "PF/G"; Tab(75); "PA/G"; Tab(86); "W"; Tab(91); "L"; Tab(97); "PCT."; Tab(103); "PF/G"; Tab(110); "PA/G"
    LPrint

    For W = 1 To 2

        If DV$(W) <> "XXX" And DV$(W) <> "" Then

            LPrint Tab(2); DV$(W);
            LPrint Tab(21); Using "##"; THW!(W);
            LPrint Tab(25); Using "##"; THL!(W);

            If THW!(W) = THW!(W) + THL!(W) And THW!(W) <> 0 Then LPrint Tab(28); "1.000";
            If THW!(W) = 0 Then LPrint Tab(29); ".000";
            If THW!(W) > 0 And THW!(W) < THW!(W) + THL!(W) Then statPCT! = (THW!(W) / (THW!(W) + THL!(W))): LPrint Tab(29); Using ".###"; statPCT!;
            If TR1!(W) = 0 Then LPrint Tab(35); "  0.0";
            If TR1!(W) > 0 Then LPrint Tab(35); Using "###.#"; TR1!(W) / (THW!(W) + THL!(W));
            If TR2!(W) = 0 Then LPrint Tab(42); "  0.0";
            If TR2!(W) > 0 Then LPrint Tab(42); Using "###.#"; TR2!(W) / (THW!(W) + THL!(W));

            LPrint Tab(53); Using "##"; TAW!(W);
            LPrint Tab(57); Using "##"; TAL!(W);

            If TAW!(W) = TAW!(W) + TAL!(W) And TAW!(W) <> 0 Then LPrint Tab(60); "1.000";
            If TAW!(W) = 0 Then LPrint Tab(61); ".000";
            If TAW!(W) > 0 And TAW!(W) < TAW!(W) + TAL!(W) Then statPCT! = (TAW!(W) / (TAW!(W) + TAL!(W))): LPrint Tab(61); Using ".###"; statPCT!;
            If TR3!(W) = 0 Then LPrint Tab(67); "  0.0";
            If TR3!(W) > 0 Then LPrint Tab(67); Using "###.#"; TR3!(W) / (TAW!(W) + TAL!(W));
            If TR4!(W) = 0 Then LPrint Tab(74); "  0.0";
            If TR4!(W) > 0 Then LPrint Tab(74); Using "###.#"; TR4!(W) / (TAW!(W) + TAL!(W));

            LPrint Tab(84); Using "###"; THW!(W) + TAW!(W);
            LPrint Tab(89); Using "###"; THL!(W) + TAL!(W);

            If THW!(W) + TAW!(W) = THW!(W) + TAW!(W) + THL!(W) + TAL!(W) And THW!(W) + TAW!(W) <> 0 Then LPrint Tab(95); "1.000";
            If (THW!(W) + TAW!(W)) = 0 Then LPrint Tab(96); ".000";
            If (THW!(W) + TAW!(W)) > 0 And THW!(W) + TAW!(W) < THW!(W) + TAW!(W) + THL!(W) + TAL!(W) Then statPCT! = ((THW!(W) + TAW!(W)) / (THW!(W) + TAW!(W) + THL!(W) + TAL!(W))): LPrint Tab(96); Using ".###"; statPCT!;
            If TR1!(W) + TR3!(W) = 0 Then LPrint Tab(102); "  0.0";
            If TR1!(W) + TR3!(W) > 0 Then LPrint Tab(102); Using "###.#"; (TR1!(W) + TR3!(W)) / (THW!(W) + THL!(W) + TAW!(W) + TAL!(W));
            If TR2!(W) + TR4!(W) = 0 Then LPrint Tab(109); "  0.0";
            If TR2!(W) + TR4!(W) > 0 Then LPrint Tab(109); Using "###.#"; (TR2!(W) + TR4!(W)) / (THW!(W) + THL!(W) + TAW!(W) + TAL!(W))

        End If

    Next W

    THWW = 0
    THLL = 0
    TAWW = 0
    TALL = 0

    For I = 1 To 4
        THWW = THWW + THW!(I)
        THLL = THLL + THL!(I)
        TAWW = TAWW + TAW!(I)
        TALL = TALL + TAL!(I)
    Next

    TR11 = 0
    TR22 = 0
    TR33 = 0
    TR44 = 0

    For I = 1 To 4
        TR11 = TR11 + TR1!(I)
        TR22 = TR22 + TR2!(I)
        TR33 = TR33 + TR3!(I)
        TR44 = TR44 + TR4!(I)
    Next

    LPrint Tab(5); "TOTALS";
    LPrint Tab(20); Using "###"; THWW;
    LPrint Tab(24); Using "###"; THLL;

    If THWW = THWW + THLL And THWW <> 0 Then LPrint Tab(28); "1.000";
    If THWW = 0 Then LPrint Tab(29); ".000";
    If THWW > 0 And THWW < THWW + THLL Then statPCT! = THWW / (THWW + THLL): LPrint Tab(29); Using ".###"; statPCT!;
    If TR11 = 0 Then LPrint Tab(35); "  0.0";
    If TR11 > 0 Then LPrint Tab(35); Using "###.#"; TR11 / (THWW + THLL);
    If TR22 = 0 Then LPrint Tab(42); "  0.0";
    If TR22 > 0 Then LPrint Tab(42); Using "###.#"; TR22 / (THWW + THLL);

    LPrint Tab(52); Using "###"; TAWW;
    LPrint Tab(56); Using "###"; TALL;

    If TAWW = TAWW + TALL And TAWW <> 0 Then LPrint Tab(60); "1.000";
    If TAWW = 0 Then LPrint Tab(61); ".000";
    If TAWW > 0 And TAWW < TAWW + TALL Then statPCT! = TAWW / (TAWW + TALL): LPrint Tab(61); Using ".###"; statPCT!;
    If TR33 = 0 Then LPrint Tab(67); "  0.0";
    If TR33 > 0 Then LPrint Tab(67); Using "###.#"; TR33 / (TAWW + TALL);
    If TR44 = 0 Then LPrint Tab(74); "  0.0";
    If TR44 > 0 Then LPrint Tab(74); Using "###.#"; TR44 / (TAWW + TALL);

    LPrint Tab(84); Using "###"; THWW + TAWW;
    LPrint Tab(89); Using "###"; THLL + TALL;

    If THWW + TAWW = THWW + TAWW + THLL + TALL And THWW + TAWW <> 0 Then LPrint Tab(95); "1.000";
    If THWW + TAWW = 0 Then LPrint Tab(96); ".000";
    If THWW + TAWW > 0 And THWW + TAWW < THWW + TAWW + THLL + TALL Then statPCT! = (THWW + TAWW) / (THWW + TAWW + THLL + TALL): LPrint Tab(96); Using ".###"; statPCT!;
    If TR11 + TR33 = 0 Then LPrint Tab(102); "  0.0";
    If TR11 + TR33 > 0 Then LPrint Tab(102); Using "###.#"; (TR11 + TR33) / (THWW + THLL + TAWW + TALL);
    If TR22 + TR44 = 0 Then LPrint Tab(109); "  0.0";
    If TR22 + TR44 > 0 Then LPrint Tab(109); Using "###.#"; (TR22 + TR44) / (THWW + THLL + TAWW + TALL)

    LPrint Chr$(12)

End Sub


'----------------------------------------
'      Hd2HdColl_Screen Subroutine
'----------------------------------------
'This subroutine outputs the calculated 
'comparison values to the end user display
Sub Hd2HdColl_Screen (teamName$)

    Dim DV$(1 To 4)

    Shared D1$, D2$

    Shared teamRatings%()

    Shared Z1$()

    Shared AL!(), AW!(), HL!(), HW!()
    Shared R1!(), R2!(), hd2hdR3!(), R4!()
    Shared TAL!(), TAW!(), THL!(), THW!()
    Shared TR1!(), TR2!(), TR3!(), TR4!()

    Color 15, 0

    DV$(1) = D1$
    DV$(2) = D2$

    RDD = 0

    Do

        Cls

        Locate 2, 35
        Color teamRatings%(0), teamRatings%(1): Print teamName$
        Color 3, 0

        'Table header
        Locate 4
        Print Tab(15); String$(7, 196); " HOME "; String$(8, 196); Tab(36); Chr$(194); String$(7, 196); " AWAY "; String$(8, 196); Tab(58); Chr$(194); String$(6, 196); " TOTALS "; String$(7, 196)
        Locate 5
        Print Tab(15); "W"; Tab(17); "L"; Tab(21); "PCT."; Tab(26); "PF/G"; Tab(32); "PA/G"; Tab(36); Chr$(179); Tab(37); "W"; Tab(39); "L"; Tab(43); "PCT."; Tab(48); "PF/G"; Tab(54); "PA/G"; Tab(58); Chr$(179); Tab(59); "W"; Tab(61); "L"; Tab(65); "PCT."; Tab(70); "PF/G"; Tab(76); "PA/G"

        RDD = RDD + 1

        DD = ((RDD - 1) * 15) + 1
        EE = RDD * 15

        If DV$(RDD) <> "XXX" And DV$(RDD) <> "" Then

            Color 11
            Print Left$(DV$(RDD), 15);

            Color 3: Print Tab(36); Chr$(179); Tab(58); Chr$(179)

            For K = DD To EE

                If Z1$(K) <> "XXX" And Z1$(K) <> teamName$ And Z1$(K) <> "*" Then

                    Color 15
                    Locate , 1: Print Left$(Z1$(K), 13);

                    Color 7
                    Locate , 15: Print Using "#"; HW!(K);
                    Locate , 17: Print Using "#"; HL!(K);

                    If HW!(K) = HW!(K) + HL!(K) And HW!(K) <> 0 Then Locate , 19: Print "1.000";
                    If HW!(K) = 0 Then Locate , 20: Print ".000";
                    If HW!(K) > 0 And HW!(K) < HW!(K) + HL!(K) Then statPCT! = (HW!(K) / (HW!(K) + HL!(K))): Locate , 20: Print Using ".###"; statPCT!;
                    If R1!(K) = 0 Then Locate , 25: Print "  0.0";
                    If R1!(K) > 0 Then Locate , 25: Print Using "###.#"; R1!(K) / (HW!(K) + HL!(K));
                    If R2!(K) = 0 Then Locate , 31: Print "  0.0";
                    If R2!(K) > 0 Then Locate , 31: Print Using "###.#"; R2!(K) / (HW!(K) + HL!(K));

                    Color 3: Print Tab(36); Chr$(179);

                    Color 7
                    Locate , 37: Print Using "#"; AW!(K);
                    Locate , 39: Print Using "#"; AL!(K);

                    If AW!(K) = AW!(K) + AL!(K) And AW!(K) <> 0 Then Locate , 41: Print "1.000";
                    If AW!(K) = 0 Then Locate , 42: Print ".000";
                    If AW!(K) > 0 And AW!(K) < AW!(K) + AL!(K) Then statPCT! = (AW!(K) / (AW!(K) + AL!(K))): Locate , 42: Print Using ".###"; statPCT!;
                    If hd2hdR3!(K) = 0 Then Locate , 47: Print "  0.0";
                    If hd2hdR3!(K) > 0 Then Locate , 47: Print Using "###.#"; hd2hdR3!(K) / (AW!(K) + AL!(K));
                    If R4!(K) = 0 Then Locate , 53: Print "  0.0";
                    If R4!(K) > 0 Then Locate , 53: Print Using "###.#"; R4!(K) / (AW!(K) + AL!(K));

                    Color 3: Print Tab(58); Chr$(179);
                    
                    Color 7
                    Locate , 59: Print Using "##"; HW!(K) + AW!(K);
                    Locate , 61: Print Using "##"; HL!(K) + AL!(K);

                    If (HW!(K) + AW!(K)) = (HW!(K) + AW!(K) + HL!(K) + AL!(K)) And (HW!(K) + AW!(K)) <> 0 Then Locate , 63: Print "1.000";
                    If (HW!(K) + AW!(K)) = 0 Then Locate , 64: Print ".000";
                    If (HW!(K) + AW!(K)) > 0 And (HW!(K) + AW!(K)) < (HW!(K) + AW!(K) + HL!(K) + AL!(K)) Then statPCT! = ((HW!(K) + AW!(K)) / (HW!(K) + AW!(K) + HL!(K) + AL!(K))): Locate , 64: Print Using ".###"; statPCT!;
                    If R1!(K) + hd2hdR3!(K) = 0 Then Locate , 69: Print "  0.0";
                    If R1!(K) + hd2hdR3!(K) > 0 Then Locate , 69: Print Using "###.#"; (R1!(K) + hd2hdR3!(K)) / (HW!(K) + HL!(K) + AW!(K) + AL!(K));
                    If R2!(K) + R4!(K) = 0 Then Locate , 75: Print "  0.0"
                    If R2!(K) + R4!(K) > 0 Then Locate , 75: Print Using "###.#"; (R2!(K) + R4!(K)) / (HW!(K) + HL!(K) + AW!(K) + AL!(K))

                    Color 3: Print Tab(36); Chr$(179); Tab(58); Chr$(179);

                    THW!(RDD) = THW!(RDD) + HW!(K)
                    THL!(RDD) = THL!(RDD) + HL!(K)
                    TAW!(RDD) = TAW!(RDD) + AW!(K)
                    TAL!(RDD) = TAL!(RDD) + AL!(K)
                    TR1!(RDD) = TR1!(RDD) + R1!(K)
                    TR2!(RDD) = TR2!(RDD) + R2!(K)
                    TR3!(RDD) = TR3!(RDD) + hd2hdR3!(K)
                    TR4!(RDD) = TR4!(RDD) + R4!(K)
                End If

            Next K

            'Division Team Totals
            Print

            Color 11
            Locate , 14: Print Using "##"; THW!(RDD);

            If THW!(RDD) = THW!(RDD) + THL!(RDD) And THW!(RDD) <> 0 Then Locate , 19: Print "1.000";
            If THW!(RDD) = 0 Then Locate , 20: Print ".000";
            If THW!(RDD) > 0 And THW!(RDD) < THW!(RDD) + THL!(RDD) Then statPCT! = (THW!(RDD) / (THW!(RDD) + THL!(RDD))): Locate , 20: Print Using ".###"; statPCT!;
            If TR2!(RDD) < 1 Then Locate , 32: Print " 0.0";
            If TR2!(RDD) > 0 Then Locate , 31: Print Using "###.#"; TR2!(RDD) / (THW!(RDD) + THL!(RDD));

            Locate , 38: Print Using "##"; TAL!(RDD);

            If TR3!(RDD) < 1 Then Locate , 48: Print " 0.0";
            If TR3!(RDD) > 0 Then Locate , 47: Print Using "###.#"; TR3!(RDD) / (TAW!(RDD) + TAL!(RDD));

            Locate , 57: Print Using "###"; THW!(RDD) + TAW!(RDD);

            Color 11

            If THW!(RDD) + TAW!(RDD) = THW!(RDD) + THL!(RDD) + TAW!(RDD) + TAL!(RDD) And THW!(RDD) + TAW!(RDD) <> 0 Then Locate , 63: Print "1.000";
            If THW!(RDD) + TAW!(RDD) = 0 Then Locate , 64: Print ".000";
            If THW!(RDD) + TAW!(RDD) > 0 And THW!(RDD) + TAW!(RDD) < THW!(RDD) + TAW!(RDD) + THL!(RDD) + TAL!(RDD) Then statPCT! = (THW!(RDD) + TAW!(RDD)) / (THW!(RDD) + TAW!(RDD) + THL!(RDD) + TAL!(RDD)): Locate , 64: Print Using ".###"; statPCT!;
            If TR2!(RDD) + TR4!(RDD) < 1 Then Locate , 76: Print " 0.0"
            If TR2!(RDD) + TR4!(RDD) > 0 Then Locate , 75: Print Using "###.#"; (TR2!(RDD) + TR4!(RDD)) / (THW!(RDD) + THL!(RDD) + TAW!(RDD) + TAL!(RDD))

            Locate , 5: Print "TOTALS";

            If TR1!(RDD) < 1 Then Locate , 26: Print " 0.0";
            If TR1!(RDD) > 0 Then Locate , 25: Print Using "###.#"; TR1!(RDD) / (THW!(RDD) + THL!(RDD));

            Color 11

            If TAW!(RDD) = TAW!(RDD) + TAL!(RDD) And TAW!(RDD) <> 0 Then Locate , 41: Print "1.000";
            If TAW!(RDD) = 0 Then Locate , 42: Print ".000";
            If TAW!(RDD) > 0 And TAW!(RDD) < TAW!(RDD) + TAL!(RDD) Then statPCT! = (TAW!(RDD) / (TAW!(RDD) + TAL!(RDD))): Locate , 42: Print Using ".###"; statPCT!;
            If TR4!(RDD) < 1 Then Locate , 54: Print " 0.0";
            If TR4!(RDD) > 0 Then Locate , 53: Print Using "###.#"; TR4!(RDD) / (TAW!(RDD) + TAL!(RDD));
            If TR1!(RDD) + TR3!(RDD) = 0 Then Locate , 70: Print " 0.0";
            If TR1!(RDD) + TR3!(RDD) > 0 Then Locate , 69: Print Using "###.#"; (TR1!(RDD) + TR3!(RDD)) / (THW!(RDD) + THL!(RDD) + TAW!(RDD) + TAL!(RDD));

            Locate , 16: Print Using "##"; THL!(RDD);
            Locate , 36: Print Using "##"; TAW!(RDD);
            Locate , 59: Print Using "###"; THL!(RDD) + TAL!(RDD)

            Color 14
            Locate 23, 5: Print "HIT ANY KEY TO CONTINUE"
            I$ = GetKeyPress$

        End If

    Loop Until RDD = DIVS_PER_CONFERENCE

    'Stats for Division Totals
    Cls

    Locate 2, 35
    Color teamRatings%(0), teamRatings%(1): Print teamName$
    Color 3, 0

    Locate 4
    Print Tab(9); String$(8, 196); " HOME "; String$(9, 196); Tab(32); Chr$(194); String$(8, 196); " AWAY "; String$(9, 196); Tab(56); Chr$(194); String$(7, 196); " TOTALS "; String$(8, 196)
    Locate 5
    Print Tab(10); "W"; Tab(13); "L"; Tab(17); "PCT."; Tab(22); "PF/G"; Tab(28); "PA/G"; Tab(32); Chr$(179); Tab(34); "W"; Tab(37); "L"; Tab(41); "PCT."; Tab(46); "PF/G"; Tab(52); "PA/G"; Tab(56); Chr$(179); Tab(58); "W"; Tab(61); "L"; Tab(65); "PCT."; Tab(70); "PF/G"; Tab(76); "PA/G"

    Color 3: Print Tab(32); Chr$(179); Tab(56); Chr$(179)

    For W = 1 To 2

        If DV$(W) <> "XXX" And DV$(W) <> "" Then

            Color 15
            'We have to limit this to 8 because we start stats on column 7
            Locate , 1: Print Left$(DV$(W), 8);

            Color 7
            Locate , 9: Print Using "##"; THW!(W);
            Locate , 12: Print Using "##"; THL!(W);

            If THW!(W) = THW!(W) + THL!(W) And THW!(W) <> 0 Then Locate , 15: Print "1.000";
            If THW!(W) = 0 Then Locate , 16: Print ".000";
            If THW!(W) > 0 And THW!(W) < THW!(W) + THL!(W) Then statPCT! = (THW!(W) / (THW!(W) + THL!(W))): Locate , 16: Print Using ".###"; statPCT!;
            If TR1!(W) = 0 Then Locate , 21: Print "  0.0";
            If TR1!(W) > 0 Then Locate , 21: Print Using "###.#"; TR1!(W) / (THW!(W) + THL!(W));
            If TR2!(W) = 0 Then Locate , 27: Print "  0.0";
            If TR2!(W) > 0 Then Locate , 27: Print Using "###.#"; TR2!(W) / (THW!(W) + THL!(W));

            Color 3: Print Tab(32); Chr$(179);

            Color 7
            Locate , 33: Print Using "##"; TAW!(W);
            Locate , 36: Print Using "##"; TAL!(W);

            If TAW!(W) = TAW!(W) + TAL!(W) And TAW!(W) <> 0 Then Locate , 39: Print "1.000";
            If TAW!(W) = 0 Then Locate , 40: Print ".000";
            If TAW!(W) > 0 And TAW!(W) < TAW!(W) + TAL!(W) Then statPCT! = (TAW!(W) / (TAW!(W) + TAL!(W))): Locate , 40: Print Using ".###"; statPCT!;
            If TR3!(W) = 0 Then Locate , 45: Print "  0.0";
            If TR3!(W) > 0 Then Locate , 45: Print Using "###.#"; TR3!(W) / (TAW!(W) + TAL!(W));
            If TR4!(W) = 0 Then Locate , 51: Print "  0.0";
            If TR4!(W) > 0 Then Locate , 51: Print Using "###.#"; TR4!(W) / (TAW!(W) + TAL!(W));

            Color 3: Print Tab(56); Chr$(179);

            Color 7
            Locate , 57: Print Using "##"; THW!(W) + TAW!(W);
            Locate , 60: Print Using "##"; THL!(W) + TAL!(W);

            If THW!(W) + TAW!(W) = THW!(W) + TAW!(W) + THL!(W) + TAL!(W) And THW!(W) + TAW!(W) <> 0 Then Locate , 63: Print "1.000";
            If (THW!(W) + TAW!(W)) = 0 Then Locate , 64: Print ".000";
            If (THW!(W) + TAW!(W)) > 0 And THW!(W) + TAW!(W) < THW!(W) + TAW!(W) + THL!(W) + TAL!(W) Then statPCT! = ((THW!(W) + TAW!(W)) / (THW!(W) + TAW!(W) + THL!(W) + TAL!(W))): Locate , 64: Print Using ".###"; statPCT!;

            If TR1!(W) + TR3!(W) = 0 Then Locate , 69: Print "  0.0";
            If TR1!(W) + TR3!(W) > 0 Then Locate , 69: Print Using "###.#"; (TR1!(W) + TR3!(W)) / (THW!(W) + THL!(W) + TAW!(W) + TAL!(W));

            If TR2!(W) + TR4!(W) = 0 Then Locate , 75: Print "  0.0"
            If TR2!(W) + TR4!(W) > 0 Then Locate , 75: Print Using "###.#"; (TR2!(W) + TR4!(W)) / (THW!(W) + THL!(W) + TAW!(W) + TAL!(W))

            Color 3: Print Tab(32); Chr$(179); Tab(56); Chr$(179);

        End If

    Next W

    THWW = 0
    THLL = 0
    TAWW = 0
    TALL = 0

    For I = 1 To 4
        THWW = THWW + THW!(I)
        THLL = THLL + THL!(I)
        TAWW = TAWW + TAW!(I)
        TALL = TALL + TAL!(I)
    Next

    TR11 = 0
    TR22 = 0
    TR33 = 0
    TR44 = 0

    For I = 1 To 4
        TR11 = TR11 + TR1!(I)
        TR22 = TR22 + TR2!(I)
        TR33 = TR33 + TR3!(I)
        TR44 = TR44 + TR4!(I)
    Next

    'Division Totals
    Color 11
    Print "TOTALS";

    Locate , 9: Print Using "##"; THWW;
    Locate , 12: Print Using "##"; THLL;

    If THWW = THWW + THLL And THWW <> 0 Then Locate , 15: Print "1.000";
    If THWW = 0 Then Locate , 16: Print ".000";
    If THWW > 0 And THWW < THWW + THLL Then Locate , 16: Print Using ".###"; THWW / (THWW + THLL);
    If TR11 = 0 Then Locate , 21: Print "  0.0";
    If TR11 > 0 Then Locate , 21: Print Using "###.#"; TR11 / (THWW + THLL);
    If TR22 = 0 Then Locate , 27: Print "  0.0";
    If TR22 > 0 Then Locate , 27: Print Using "###.#"; TR22 / (THWW + THLL);

    Color 3: Print Tab(32); Chr$(179);

    Color 11
    Locate , 33
    Print Using "##"; TAWW;
    Locate , 36
    Print Using "##"; TALL;

    If TAWW = TAWW + TALL And TAWW <> 0 Then Locate , 39: Print "1.000";
    If TAWW = 0 Then Locate , 40: Print ".000";
    If TAWW > 0 And TAWW < TAWW + TALL Then statPCT! = TAWW / (TAWW + TALL): Locate , 40: Print Using ".###"; statPCT!;
    If TR33 = 0 Then Locate , 45: Print "  0.0";
    If TR33 > 0 Then Locate , 45: Print Using "###.#"; TR33 / (TAWW + TALL);
    If TR44 = 0 Then Locate , 51: Print "  0.0";
    If TR44 > 0 Then Locate , 51: Print Using "###.#"; TR44 / (TAWW + TALL);

    Color 3: Print Tab(56); Chr$(179);

    Color 11
    Locate , 57
    Print Using "##"; THWW + TAWW;
    Locate , 60
    Print Using "##"; THLL + TALL;

    If THWW + TAWW = THWW + TAWW + THLL + TALL And THWW + TAWW <> 0 Then Locate , 63: Print "1.000";
    If THWW + TAWW = 0 Then Locate , 64: Print ".000";
    If THWW + TAWW > 0 And THWW + TAWW < THWW + TAWW + THLL + TALL Then statPCT! = (THWW + TAWW) / (THWW + TAWW + THLL + TALL): Locate , 64: Print Using ".###"; statPCT!;
    If TR11 + TR33 = 0 Then Locate , 69: Print "  0.0";
    If TR11 + TR33 > 0 Then Locate , 69: Print Using "###.#"; (TR11 + TR33) / (THWW + THLL + TAWW + TALL);
    If TR22 + TR44 = 0 Then Locate , 75: Print "  0.0"
    If TR22 + TR44 > 0 Then Locate , 75: Print Using "###.#"; (TR22 + TR44) / (THWW + THLL + TAWW + TALL)

    Color 14
    Locate 23, 5: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

End Sub
