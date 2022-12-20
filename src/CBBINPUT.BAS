Sub CBBINPUT ()

    _Title "Courtside College Basketball - Ratings Input"

    Dim E0(16), A(14, 19), DF(13)
    Dim QQ(3), RS(13, 13), XX(13), X(14, 6)
    Dim L%(9), T%(9)
    Dim N$(14), P$(14), X$(3)

    X$(0) = "COLLEGE BASKETBALL"
    X$(1) = "H S BASKETBALL"

    Cls

    Do

        Color 12: Print "SELECT TYPE OF TEAMS YOU ARE ENTERING"
        Print
        Color 14: Print "(0) ";: Color 15: Print "COLLEGE BASKETBALL"
        Print
        Color 14: Print "(1) ";: Color 15: Print "HIGH SCHOOL BASKETBALL"

        I$ = GetKeyPress$
        If I$ <> Chr$(27) Then
            H = Val(I$)
        End If

    Loop Until I$ = Chr$(27) Or H = 0 Or H = 1

    If I$ <> Chr$(27) Then
        'Show league data
        'H = 0, College BB
        'H = 1, High School BB

        Cls

        Print X$(H)
        If H = 0 Then
            L7 = 6: L3 = 20: L6 = 14
        Else
            'H = 1
            L7 = 5: L6 = 11: L3 = 16
        End If

        'C1 should be 0 when first getting here
        'C1 = 1 means we've entered all data and we're just
        'going to jump around from there on out
        300 Locate 3: Input "1. SEASON FGA/G "; L1: If C1 = 1 Then 307

        'Why is this only here and not at each step??
        'I'm removing it; you're only told later
        'that 999 is an exit from anything
        'If L1 = 999 Then 11
        301 Locate 4: Input "2. SEASON REBOUNDS/GAME "; L2: If C1 = 1 Then 307
        302 Locate 5: Input "3. SEASON FG % "; L4: If C1 = 1 Then 307
        303 Locate 6: Input "4. SEASON STEALS/GAME "; BA: If C1 = 1 Then 307
        304 Locate 7: Input "5. SEASON TURNOVERS/GAME "; BB: If C1 = 1 Then 307
        305 Locate 8: Input "6. SEASON 3 FGA/FGA % "; Q7: If C1 = 1 Then 307
        306 Locate 9: Input "7. SEASON 3 FG % "; Q8: If C1 = 1 Then 307
        T%(2) = 999

        'If we've already printed it to the screen
        'use a marker so that the loop later doesn't
        'have to print everything
        307 C1 = 1

        Do
            Locate 22: Input "# TO CHANGE (0 TO CONTINUE) "; B9

            Select Case B9
                Case 1: GoTo 300
                Case 2: GoTo 301
                Case 3: GoTo 302
                Case 4: GoTo 303
                Case 5: GoTo 304
                Case 6: GoTo 305
                Case 7: GoTo 306
            End Select

        Loop Until B9 = 0

        'If 0 was pressed, we continue

        C1 = 0

        Do
            Cls
			
			Print "YOU MUST HAVE THE FOLLOWING TEAM &"
			Print "INDIVIDUAL STATS: FGA,FG %,"
			Print "FTA,FT %,REBOUNDS"
			Print
            Print "IF YOU ARE INPUTTING A TEAM WITH ANY "
			Print "OF THE FOLLOWING STATS MISSING,"
			Print "SIMPLY CHECK 'N' AND THE PROGRAM WILL"
            Print "ALLOW YOU TO INPUT RATINGS DIRECTLY"
            Print
			Print "DO YOU HAVE THE FOLLOWING:"
            Print
			
			Input "ASSISTS (YN) "; I$: QQ(0) = 0
			If I$ = "N" Then QQ(0) = 1

            Input "PERSONAL FOULS (YN) "; I$: QQ(1) = 0
			If I$ = "N" Then QQ(1) = 1

            Input "STEALS & TURNOVERS (YN) "; I$: QQ(2) = 0
			If I$ = "N" Then QQ(2) = 1

            Input "BLOCKS (YN) "; I$: QQ(3) = 0
			If I$ = "N" Then QQ(3) = 1
            Print
			Input "ANY CHANGE (YN) "; I$

        Loop Until UCase$(I$) = "N"

        'No changes, let's move on
        Cls

        Do

            'C1 should be 0 our first time through
            308 
			Locate 1: Print X$(H)
            Locate 2: Input " 0. TEAM # (0-EXIT)"; T

            If T > 0 And T < 999 Then

                'If we've never entered data, skip ahead to that part
                If C1 = 0 Then
                    309 Locate 3: Input " 1. NAME "; A$: If C1 = 1 Then 326
                    310 Locate 4: Input " 2. STRENGTH OF SCHEDULE ADJ "; BC: If C1 = 1 Then 326
                    311 Locate 5: Input " 3. GAMES "; L5: If C1 = 1 Then 326
                    157 Locate 6: Input "17. TEXT COLOR (0-15) "; T%(0): If C1 = 1 Then 326
                    158 Locate 7: Input "18. BACKGROUND (0-7) "; T%(1): Locate 8, 1: Color T%(0), T%(1): Print A$: Color 15, 0: If C1 = 1 Then 326
                    159 Locate 2, 37: Input "19. NICKNAME "; A1$: If C1 = 1 Then 326
                    160 Locate 3, 37: Input "20. WINS "; T%(7): If C1 = 1 Then 326
                    161 Locate 4, 37: Input "21. LOSSES "; T%(8): If C1 = 1 Then 326
                    162 Locate 5, 37: Input "22. POWER RATING "; T%(9): If C1 = 1 Then 326
                    163 Locate 6, 37: Input "23. COACH "; C$: If C1 = 1 Then 326
                    165 Locate 7, 37: Input "24. ARENA "; S$: If C1 = 1 Then 326
                    166 Locate 8, 37: Input "25. AVG ATT "; avgAttendance%: If C1 = 1 Then 326
                    312 Locate 9, 10: Print A$;: Locate 9, 30: Print "OPPONENTS"
                    313 Locate 10, 1: Print "4. FGA ";: Locate 10, 15: Input T1
                    Locate 10, 30: Input T2: If C1 = 1 Then 326
                    314 Locate 11, 1: Print "5. FG % ";: Locate 11, 15: Input T6
                    Locate 11, 30: Input BE: If C1 = 1 Then 326
                    315 Locate 12, 1: Print "6. FTA";: Locate 12, 15: Input T7
                    Locate 12, 30: Input BF: If C1 = 1 Then 326
                    316 Locate 13, 1: Print "7. FT %";: Locate 13, 15: Input T8
                    Locate 13, 30: Input BG: If C1 = 1 Then 326
                    317 Locate 14, 1: Print "8. RB";: Locate 14, 15: Input T3
                    Locate 14, 30: Input T4: If C1 = 1 Then 326
                    318 Locate 15, 1: Print "9. AST";: Locate 15, 15: Input T0
                    Locate 15, 30: Input XX: If C1 = 1 Then 326
                    319 Locate 16, 1: Print "10. FOULS";: Locate 16, 15: Input L3
                    Locate 16, 30: Input BI: If C1 = 1 Then 326
                    320 Locate 17, 1: Print "11. TO";: Locate 17, 15: Input BJ
                    Locate 17, 30: Input BK: If C1 = 1 Then 326
                    321 Locate 18, 1: Print "12. STLS";: Locate 18, 15: Input T9
                    Locate 18, 30: Input BH: If C1 = 1 Then 326
                    322 Locate 19, 1: Print "13. 3 FGA ";: Locate 19, 15: Print "---";
                    Locate 19, 30: Input W0: If C1 = 1 Then 326
                    323 Locate 20, 1: Print "14. 3 FGA % ";: Locate 20, 15: Print "---";
                    Locate 20, 30: Input W1: If C1 = 1 Then 326
                    324 Locate 21, 1: Print "15. DISK ID ";: Input yearNumber$: If C1 = 1 Then 326
                    164 Locate 22: Input "16. HOMECOURT FACTOR (0-6)"; T%(6): If C1 = 1 Then 326
                    325 Locate 22, 30: Input "OPP 2FG% "; OE
                    If BI = 0 Then BI = 20 * L5
                    If L3 = 0 Then L3 = 20 * L5

                End If

                326 C1 = 1
                Locate 8, 1: Color T%(0), T%(1): Print A$
                Color 15, 0

                Locate 23: Input "# TO CHANGE (99 TO CONTINUE) "; B9
                'As earlier in the file this can be made into a case/select
                'The general idea is it jumps back up to the data entry
                '0   1   2    3    4    5    6    7    8    9    10   11   12   13   14   15   16   17   18   19   20   21   22   23   24   25   26
                On B9 + 1 GOTO 308, 309, 310, 311, 313, 314, 315, 316, 317, 318, 319, 320, 321, 322, 323, 324, 164, 157, 158, 159, 160, 161, 162, 163, 165, 166, 325

            End If

        Loop Until T = 0 Or B9 = 99

        'If T = 999 Then 200

        If B9 = 99 Then

            'We're prompted enter 99, but based on the logic above
            'we would continue if any number is entered higher
            'than what we expect

            '*** TM FGA/G
            T%(3) = Int(((T1 + T2) / (L5 * 2)) + .5)
            T%(4) = L1
            T%(5) = L5

            C1 = 0
            If H = 1 Then
                F3 = 32
            Else
                F3 = 40
            End If

            For I = 0 To 13

                Cls
                Print X$(H); " PLAYER- "; I + 1

                331 Locate 3: Input " 1. NAME (XXX TO QUIT) "; N$(I): If C1 = 1 Then 345

                If N$(I) <> "XXX" Then

                    349 Locate 4: Input "16. POSITION "; P$(I): If C1 = 1 Then 345
                    332 Locate 5: Input " 2. GAMES "; A(I, 7): X(I, 0) = A(I, 7): If C1 = 1 Then 345
                    333 Locate 6: Input " 3. MP "; A(I, 8): If C1 = 1 Then 345
                    334 Locate 7: Input " 4. FGM "; RS(I, 3): If C1 = 1 Then 345
                    335 Locate 8: Input " 5. FGA "; A(I, 9): X(I, 1) = A(I, 9): If C1 = 1 Then 345
                    337 Locate 9: Input " 7. FTM "; RS(I, 5): If C1 = 1 Then 345
                    336 Locate 10: Input " 6. FTA "; A(I, 16): If C1 = 1 Then 345
                    359 Locate 11: Input " 8. ORB "; A(I, 19): If C1 = 1 Then 345
                    338 Locate 12: Input " 9. TOT RB "; A(I, 10): If C1 = 1 Then 345
                    340 Locate 13: Input "10. PF "; A(I, 12): If C1 = 1 Then 345
                    339 Locate 14: Input "11. AST "; A(I, 11): If C1 = 1 Then 345
                    342 Locate 15: Input "12. BLK "; A(I, 14): If C1 = 1 Then 345
                    341 Locate 16: Input "13. STL "; A(I, 13): If C1 = 1 Then 345
                    344 Locate 17: Input "14. 3 FGM "; RS(I, 13): If C1 = 1 Then 345
                    343 Locate 18: Input "15. 3 FGA "; A(I, 17): If C1 = 1 Then 345
                    350 Locate 19: Input "17. DEFENSE (1-9) "; DF(I): If C1 = 1 Then 345

                    345 C1 = 1: Locate 22: Input "# TO CHANGE (0 TO CONTINUE) "; B9
                    'As earlier in the file this can be made into a case/select
                    'The general idea is it jumps back up to the data entry
                    On B9 GOTO 331, 332, 333, 334, 335, 336, 337, 359, 338, 340, 339, 342, 341, 344, 343, 349, 350

                    C1 = 0
                    RS(I, 0) = A(I, 7)
                    RS(I, 1) = A(I, 8)
                    RS(I, 2) = A(I, 9)

                    If RS(I, 2) = 0 Then
                        A(I, 0) = 0
                    Else
                        A(I, 0) = Int(RS(I, 3) / RS(I, 2) * 100 + .5)
                    End If

                    RS(I, 4) = A(I, 16)

                    If RS(I, 4) = 0 Then
                        A(I, 1) = 0
                    Else
                        A(I, 1) = Int(RS(I, 5) / RS(I, 4) * 100 + .5)
                    End If

                    RS(I, 6) = A(I, 19)
                    RS(I, 7) = A(I, 10)
                    RS(I, 8) = A(I, 12)
                    RS(I, 9) = A(I, 11)
                    RS(I, 10) = A(I, 14)
                    RS(I, 11) = A(I, 13)
                    RS(I, 12) = A(I, 17)
                    If RS(I, 12) = 0 Then
                        A(I, 18) = 0: X(I, 2) = 0
                    Else
                        A(I, 18) = Int(RS(I, 13) / RS(I, 12) * 100 + .5): X(I, 2) = A(I, 18)
                    End If
                Else
                    For I1 = I To 13: N$(I1) = "XXX": Next
                    Exit For
                End If 'Done checking for XXX

            Next I

            329 'GoSub 1500

            1500 X1 = Val(Mid$(A$, 1, 2))

            1501 If Not (X1 < 1 Or X1 > 72) Then
                X2 = .83
                If X1 > 52 And X1 < 58 Then X2 = .7
                T7 = Int((T7 * X2) + .5)
                BF = Int((BF * X2) + .5)
                For I = 0 To 13: A(I, 16) = Int((A(I, 16) * X2) + .5): Next I
            End If

            346 X9 = 0

            'GoSub 1400
            1400 X1 = T2 - ((BE * T2) / 100) + BF - ((BF * BG) / 100)

            1401 X2 = T1 - ((T1 * T6) / 100) + T7 - ((T7 * T8) / 100)

            1402 RD = (T3 - X1 + X2 - T4) / (L5 * 2): HS = T3 - X1 + X2 - T4

            1403 M7 = ((35 + RD) / L2)

            1404 If H = 2 Then M7 = ((44 + RD) / L2)

            347 For I = 0 To 13
                If N$(I) <> "XXX" Then
                    X9 = X9 + A(I, 10) + A(I, 9)
                End If
            100 Next I

            348 For I = 0 To 13
                If N$(I) <> "XXX" Then
                    If A(I, 8) <> 0 And H = 0 Then M = 40 / (A(I, 8) / A(I, 7))
                    If A(I, 8) <> 0 And H = 1 Then M = 32 / (A(I, 8) / A(I, 7))
                    If A(I, 8) = 0 Then M = (X9 / 5) / (A(I, 9) + A(I, 10)): MI = (A(I, 9) + A(I, 10)) / (X9 / 5)
                    If A(I, 8) = 0 And M < 1.1 Then M = 1.1
                    A(I, 2) = Int((((A(I, 10) / A(I, 7)) * M) * M7) + .5)
                    3500 X = Int(((((A(I, 10) / A(I, 7)) * M) * M7) * 10) + .5)
                    If A(I, 19) = 0 Then
                        X(I, 4) = Int((X * .35) + .5)
                    Else
                        X(I, 4) = Int((X * (A(I, 19) / A(I, 10))) + .5)
                    End If

                    3505 A(I, 2) = X - X(I, 4)

                    If QQ(0) = 0 Then A(I, 4) = Int(((A(I, 11) / A(I, 7)) * M) * (L6 / (T0 / L5)) + .5)
                    If QQ(2) = 0 Then A(I, 5) = Int(((A(I, 13) / A(I, 7)) * M) + .5)
                    If QQ(3) = 0 Then A(I, 6) = ((A(I, 14) / A(I, 7)) * M): A(I, 6) = Int(A(I, 6) + .5)
                End If

            Next I

            X = L1 + L2 + 20
            inputY! = (T1 + T2 + T3 + T4 + L3 + BI) / (2 * L5)

            S = Int(((2 * inputY!) - X) + .5)

            X1 = (T1 + T2) / (L5 * 2)
            X3 = (BC / 2) / X1
            X3 = Int((X3 * 100) + .5)
            BC = Int(X3 / 2)
            BY = Int(X3 / 2)
            X4 = Int(Rnd(1) * 2)
            If X3 - BC - BY > 0 And X4 = 0 Then BC = BC + 1
            If X3 - BC - BY > 0 And X4 = 1 Then BY = BY + 1
            U7 = Int((46 - L4) / 2)
            D = OE - L4 + BY - U7

            366 For I = 0 To 13
                If N$(I) <> "XXX" Then
                    A(I, 3) = (A(I, 9) + A(I, 10)) / X9
                    A(I, 3) = Int((100 * A(I, 3)) + .5)
                End If
            Next I

            V% = Int(((W0 / T2) + .005) * 100)
            V2% = V% - Q7
            If V2% < 0 Then V2% = 100 + V2%
            V1% = W1 - Q8
            If V1% < 0 Then V1% = 100 + V1%

            For I = 0 To 13
                If N$(I) <> "XXX" Then
                    If A(I, 9) <> 0 Then X(I, 3) = Int(A(I, 17) / A(I, 9) * 100 + .5)
                    If A(I, 8) <> 0 And H = 0 Then M = 40 / (A(I, 8) / A(I, 7))
                    If A(I, 8) <> 0 And H = 1 Then M = 32 / (A(I, 8) / A(I, 7))
                    If A(I, 8) = 0 Then M = (X9 / 5) / (A(I, 9) + A(I, 10))
                    If A(I, 8) = 0 And M < 1 Then M = 1
                    B3 = (A(I, 12) / A(I, 7)) * M: B3 = Int((B3 * 10) + .5)
                    X(I, 5) = B3
                    B3 = A(I, 9) * (A(I, 0) / 100): B4 = A(I, 17) * (A(I, 18) / 100)
                    If A(I, 9) - A(I, 17) <> 0 Then A(I, 0) = ((B3 - B4) / (A(I, 9) - A(I, 17))) + .005
                    A(I, 0) = Int((A(I, 0) * 100) + .5)
                End If
            Next I

            For I = 0 To 13
                If N$(I) <> "XXX" Then
                    A(I, 0) = A(I, 0) - BC + U7
                End If
            Next I

            L%(4) = 99

            For I = 0 To 13
                If N$(I) <> "XXX" Then
                    X! = ((A(I, 9) * A(I, 0)) / 100): MI = (A(I, 8) / A(I, 7)) / 40
                    If A(I, 8) = 0 Then MI = (A(I, 9) + A(I, 10)) / (X9 / 5)
                    X! = X! * .1: If MI > 1 And A(I, 8) = 0 Then MI = 1
                    X! = X! + ((A(I, 7) * 1.64) * MI)
                    X! = ((A(I, 16) - X!) / 2)
                    X! = Int(((X! / (A(I, 9) + X!)) * 100) + .5): If X! < 0 Then X! = 0
                    X(I, 6) = X!
                End If
            Next I

            R! = 1200 - (((T1 + T2) / (L5 * 2)) * 4)
            R! = R! / 6
            inputY! = BH / L5
            X! = (inputY! - 7) / .4
            X! = Int(X! + .5)

            If X! < 0 Then L%(0) = 100 + X!
            If X! > 0 Then L%(0) = X!

            inputY! = (BJ - BH) / L5
            X! = (inputY! / R!) - .01
            X! = Int((X! * 100) + .5)
            L%(1) = X!
            inputY! = (BK - T9) / L5
            X! = (inputY! / R!) - .01
            X! = Int((X! * 100) + .5)
            X! = X! - 4

            If X! < 0 Then L%(2) = 100 + X!
            If X! > 0 Then L%(2) = X!

            R1! = Int(((T2 * BE) / 100) + .5)
            X! = (R1! * .1) + (L5 * 8.2)
            X! = (BF - X!) / 2
            X! = (X! / (T2 + X!)) - .09
            X! = Int((X! * 100) + .5)

            If X! > 0 Then L%(3) = X!

            If X! < 0 Then L%(3) = X! + 100

            If QQ(0) = 1 Then
                Cls: Print "INPUT ASSIST RATINGS"
                Print "AA SHOULD = APPROX. 3 WHEN FINISHED"
                Print "FORWARDS,CENTERS ARE USUALLY 0,1,2,3 ; GUARDS ARE USUALLY 3,4,5,6"

                C1 = 0

                Print
                For I = 0 To 13
                    1702 Locate I + 6, 1: Print I; " "; N$(I);
                    Locate I + 6, 20: Input E0(I)
                    If C1 = 1 Then Exit For
                Next I

                C1 = 1
                AA = 0
                For I1 = 0 To 13
                    AA = AA + E0(I1) * Int(A(I1, 3))
                Next I1

                Locate 20, 1
                Print "AA = "; AA / 100
                Locate 21, 1: Print "ANY CHANGE (0-13) (N=NO CHANGE) "

                Input I$

                If UCase$(I$) = "Y" Then
                    I = Val(I$)
                    GoTo 1702
                End If
                
				For I = 0 To 13
                    A(I, 4) = A(I, 4) + E0(I)
                Next I
                C1 = 0

            End If

            If QQ(1) = 1 Then
                C1 = 0
                Cls
                Print "INPUT FOUL COMMITING RATINGS"
                Print "PF SHOULD = APPROX. 40 WHEN FINISHED"

                Print "AVG. RATING = 40 ; RANGE = 15 TO 99"
                Print

                For I = 0 To 13
                    1802 Locate I + 6, 1
                    Print I; " "; N$(I);
                    Locate I + 6, 20: Input E0(I)
                    If C1 = 1 Then Exit For
                Next I

                C1 = 1
                PF = 0
				TP = 0

                For I1 = 0 To 13
                    If N$(I1) <> "XXX" Then
                        TP = TP + 1
                        PF = PF + E0(I1)
                    End If
                Next I1

                Locate 20, 1
                Print "PF = "; PF / TP
                Locate 21, 1
				Print "ANY CHANGE (0-13) (N=NO CHANGE) "
                Input I$

                If UCase$(I$) <> "N" Then
                    I = Val(I$)
                Else
                    For I = 0 To 13
                        X(I, 5) = E0(I)
                    Next I
                    C1 = 0
                End If

            End If

            If QQ(2) = 1 Then
                HQ = T1 - T2
                HR = T7 - BF
                HW = HR / 350: If HW > 1 Then HW = 1
                HW = ((HW * 35) + 32) / 100: If HW < 0 Then HW = HW * -1
                HR = HR * HW
                HS = HS / 2: HX = Int((HR + HS - HQ) + .5): HX = Int(((HX / L5) * 10) + .5)

                Cls
                L%(4) = 99
                Print "INPUT STEALS & TURNOVERS RATINGS. THIS TEAMS ST SHOULD = APPROX. "; HX / 10
                Print "IND. STL RATINGS: AVG=1 RANGE = 0 TO 4"
                Print "TM OFF STL ADJ: AVG=0 RANGE=-10 TO 10": C1 = 0
                Print "TM TURNOVR RATING: AVG=4 RANGE = 1 TO 7"
                Print "TM DEF TRNOVR ADJ: AVG=0 RANGE -4 TO 4"

                1906 '
                For I = 0 To 13
                    Locate I + 6, 1: Print I; " "; N$(I);
                    Locate I + 6, 20: Input E0(I)
                    If C1 = 1 Then Exit For 'GoTo 1911
                Next I

                '--- should this still be here?
                1908 Locate 20, 1: Input "X-TM OFF STL ADJ   "; E0(14): If C1 = 1 Then GoTo 1911
                1909 Locate 21, 1: Input "Y-TM TURNOVER ADJ  "; E0(15): If C1 = 1 Then GoTo 1911
                1910 Locate 22, 1: Input "Z-TM DEF TURNOVR ADJ"; E0(16): If C1 = 1 Then GoTo 1911

                1911 C1 = 1

                ST = 0: For I = 0 To 13: ST = ST + E0(I) * Int(A(I, 3)): Next I
                ST = ST / 20: SY = 7 + E0(14) * .4
                SV = (R * E0(15)) / 100: SW = ((4 + E0(16)) * R) / 100
                ST = ST - SY - SV + SW: Locate 10, 26: Print "ST = "; ST;

                Locate 23, 1
				Print "ANY CHANGE (0-Z) (N = NO CHANGE) ";

                Input I$
                
				If UCase$(I$) = "N" Then
                    For I = 0 To 13
                        A(I, 5) = A(I, 5) + E0(I)
                    Next I
                    If E0(14) < 0 Then E0(14) = 100 + E0(14)

                    If E0(16) < 0 Then E0(16) = 100 + E0(16)
                    For I = 0 To 2
                        L%(I) = E0(I + 14)
                    Next
                    For I = 0 To 4
                        Print "L%("; I; ")="; L%(I)
                    Next
                    I$ = GetKeyPress$

                    C1 = 0
                    'Return; whatever we do should take us out of the larger If statement at 1652
                End If

                I = Val(I$)

                Select Case UCase$(I$)
                    Case "X": GoTo 1908
                    Case "Y": GoTo 1909
                    Case "Z": GoTo 1910
                End Select

                GoTo 1906

            End If

            If QQ(3) = 1 Then
            
			Cls
                Print "INPUT BLOCK RATINGS"
                Print "GUARDS ARE USUALLY 0,1 ; FORWARDS,CENTERS ARE USUALLY 0,1,2,3"
                Print "THE GREAT SHOT BLOCKERS ARE USUALLY 5,6,7"
                Print

                C1 = 0
                For I = 0 To 13

                    2102 '
					Locate I + 6, 1: Print I; " "; N$(I);
					Locate I + 6, 20: Input E0(I)

                    If C1 = 1 Then Exit For '2105

                Next I

                'The "RETURN" should take us out the If Statement, and then run the Locate/Print ??
                C1 = 1
                Locate 20, 1: Print "ANY CHANGE (0-13) (N=NO CHANGE) ";

                Input I$

                If UCase$(I$) <> "N" Then
					I = Val(I$)
					GoTo 2102
				Else
					For I = 0 To 13: A(I, 6) = A(I, 6) + E0(I): Next I
					C1 = 0
				End If

            End If

            'Call SaveTeam (YN$)
            Open diskPaths$(0) + "BASK." + yearNumber$ For Random As #1 Len = 655
            Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 2 As Q$(14), 2 As Q$(15), 2 As Q$(16), 2 As Q$(17), 2 As Q$(18), 2 As Q$(19), 602 As X$

            For I = 0 To 13
                Field #1, 53 + I * 43 As X$, 15 As Q$(I * 15 + 20), 2 As Q$(I * 15 + 21), 2 As Q$(I * 15 + 22), 2 As Q$(I * 15 + 23), 2 As Q$(I * 15 + 24), 2 As Q$(I * 15 + 25), 2 As Q$(I * 15 + 26), 2 As Q$(I * 15 + 27), 2 As Q$(I * 15 + 28), 2 As Q$(I * 15 + 29), 2 As Q$(I * 15 + 30), 2 As Q$(I * 15 + 31), 2 As Q$(I * 15 + 32), 2 As Q$(I * 15 + 33), 2 As Q$(I * 15 + 34), 655 - 53 - 43 - I * 43 As X$
            Next

            LSet Q$(0) = A$
            LSet Q$(1) = MKI$(S)
            LSet Q$(2) = MKI$(D)
            LSet Q$(3) = MKI$(V1%)
            LSet Q$(4) = MKI$(V2%)
            For I = 0 To 4: LSet Q$(I + 5) = MKI$(L%(I)): Next
            T%(2) = 999
            For I = 0 To 9: LSet Q$(I + 10) = MKI$(T%(I)): Next
            For I = 0 To 13
                LSet Q$(I * 15 + 20) = N$(I)
                For I1 = 0 To 6
                    LSet Q$(I * 15 + I1 + 21) = MKI$(A(I, I1))
                    LSet Q$(I * 15 + I1 + 28) = MKI$(X(I, I1))
                Next
            Next
            Put #1, T
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
                LSet Q$(I * 16 + 4) = MKI$(DF(I))
                LSet Q$(I * 16 + 5) = P$(I)
                For I1 = 0 To 13
                    LSet Q$(I * 16 + I1 + 6) = MKI$(RS(I, I1))
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
            Put #1, T: Close 1

            T9 = 0

            For I = 0 To 13
                For I1 = 0 To 19
                    A(I, I1) = 0
                Next I1
                For I1 = 0 To 6
                    X(I, 0) = 0
                Next I1
                For I1 = 0 To 13
                    RS(I, I1) = 0
                Next I1
                DF(I) = 0: P$(I) = ""
            Next I

            A1$ = "": S$ = "": avgAttendance% = 0: C$ = ""

            Erase N$

            'I don't understand the logic of
            'going to the beginning after you
            'save the team

        End If 'We must have T = 0 Condition here

    End If 'We ESC'd out of the first menu

End Sub
