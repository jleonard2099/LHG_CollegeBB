Sub SINPUT ()

    _Title "Courtside College Basketball - Stats Input"

    Shared statsW0!(), statsW1!()
    Shared statsZ!(), statsZ1!()

    Shared A$()
    Dim Z2(14, 14)

    Call GetTeam_SINPUT (T1$, YN$, D)

    statB$ = RTrim$(T1$)

    For XX = 4 To 11
        If Mid$(statB$, XX, 1) = " " Or Mid$(statB$, XX, 1) = "." Then Mid$(statB$, XX, 1) = "_"
    Next XX

    For XX = 12 To 14
        If Mid$(statB$, XX, 2) = "ST" Then Mid$(statB$, 9, 1) = "_": Mid$(statB$, 10, 2) = "ST"
    Next XX

    QQ = 8

    For XX = 11 To 4 Step -1
        If Mid$(statB$, XX, 1) = "_" Then
            QQ = XX - 4
        Else
            Exit For
        End If
    Next XX

    Z$ = Mid$(statB$, 4, QQ) + "." + Left$(statB$, 2)

    If Not _FileExists(diskPaths$(1) + Z$) Then

        Locate 10, 8
        Call CSMsgBox("NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, PASS)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, PASS)

    Else

        Open diskPaths$(1) + Z$ For Input As #1

        Input #1, A$

        For I = 0 To 13: 
            Input #1, A$(I)

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

        Close 1

        '---------------
        ' Team Stats
        '---------------
        Color 15, 0
        Cls

        X$ = "OFFENSIVE TEAM STATS"
        Call TITLE_STAT_HEADER(X$)

        Locate 3, 1
        Print "1. STAT FILE NAME "; Z$
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
                    Locate 3, 1: Input "1. STAT FILE NAME "; Z$

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
                Print "1. NAME "; A$(I)
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
                            Locate 3, 1: Input "1. NAME "; A$(I)

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
        Open diskPaths$(1) + Z$ For Output As #1

        Print #1, A$

        For I = 0 To 13:
            Print #1, A$(I)

            For J = 0 To 13: 
                Print #1, Z2(I, J)
            Next J

            Print #1, statsW0!(I)

            Print #1, statsW1!(I)

        Next I

        For L = 0 To 15: Print #1, statsZ!(L): Next L

        For L = 0 To 15: Print #1, statsZ1!(L): Next L

        Close 1

    End If

End Sub

'----------------------------------------
'     GetTeam_SINPUT Subroutine
'----------------------------------------
'...explanation...
Sub GetTeam_SINPUT (B$, YN$, D)

    Shared statsL%(), T%()

    Do
        Color 15, 0
        Cls
        JJ = 6
        SN = 1
        Locate 6, 8: Color 15, 4
        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";: For II = 1 To JJ: Locate 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";: Next
        Locate 7 + JJ, 8
        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

        Locate 7, 32
        Locate , 12: Print "ENTER DISK ID: ": Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Print
        Locate , 12: Print "ESC TO RETURN TO MAIN MENU"
        Color 15, 0

        I$ = GetKeyPress$

        If I$ <> Chr$(27) Then

            If I$ = Chr$(13) Then
                Call SelectTeam_SINPUT (B$, YN$, NTMS, D)
            Else
                Locate 7, 27: Print I$
                Locate 7, 28: Line Input NY$
                NY$ = I$ + NY$
                Color 15, 0

                If NY$ = "" And YN$ <> "" Then
                    Call SelectTeam_SINPUT (B$, YN$, NTMS, D)
                Else
                    YN$ = RTrim$(NY$)
                    Erase teamNames$

                    Open diskPaths$(0) + "BASK." + YN$ As #1 Len = 655

                    fileLength& = LOF(1)

                    Field #1, 15 As Q$(0), 640 As X$

                    For I1 = 1 to fileLength& / 655
                        Get #1, I1
                        teamNames$(I1) = Q$(0)
                    Next

                    Close 1

                    NTMS = fileLength& / 655

                    Call SelectTeam_SINPUT (B$, YN$, NTMS, D)
                End If

            End If

            If BO% = 1 Then
                BO% = 0
            Else
                Locate 2, 33
                Color T%(0), T%(1): Print B$

                Do
                    Color 15, 4
                    Locate 3, 33: Print "ANY CHANGE (YN)";
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"
            End If
        Else
            BO% = 1
        End If

    Loop Until UCase$(I$) = "N"

    Color 15, 0

End Sub

'----------------------------------------
'       SelectTeam_SINPUT Subroutine
'----------------------------------------
'...explanation...
Sub SelectTeam_SINPUT (T$, YN$, numberTeams, T1)

    Shared statsL%(), T%()

    NS% = 0

    Selection% = 1
    Count% = numberTeams
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

    If I$ <> Chr$(27) Then

        T1 = NS% * 80 + Y - 3 + (X - 1) * 20

        Open diskPaths$(0) + "BASK." + YN$ As #1 Len = 655

        FIELD #1,15 AS Q$(0),2 AS Q$(1),2 AS Q$(2),2 AS Q$(3),2 AS Q$(4),2 AS Q$(5),2 AS Q$(6),2 AS Q$(7),2 AS Q$(8),2 AS Q$(9),2 AS Q$(10),2 AS Q$(11),2 AS Q$(12),2 AS Q$(13),2 AS Q$(14),2 AS Q$(15),2 AS Q$(16),2 AS Q$(17),2 AS Q$(18),2 AS Q$(19),602 AS X$

        For I = 0 To 13
            FIELD #1,53+I*43 AS X$,15 AS Q$(I*15+20),2 AS Q$(I*15+21),2 AS Q$(I*15+22),2 AS Q$(I*15+23),2 AS Q$(I*15+24),2 AS Q$(I*15+25),2 AS Q$(I*15+26),2 AS Q$(I*15+27),2 AS Q$(I*15+28),2 AS Q$(I*15+29),2 AS Q$(I*15+30),2 AS Q$(I*15+31),2 AS Q$(I*15+32),2 AS Q$(I*15+33),2 AS Q$(I*15+34),655-53-43-I*43 AS X$
        Next

        Get #1, T1

        T$ = RTRim$(Q$(0))
        staminaRating% = CVI(Q$(1))
        defFGPctAdj% = CVI(Q$(2))
        V1% = CVI(Q$(3))
        V2% = CVI(Q$(4))

        For I = 0 To 4: statsL%(I) = CVI(Q$(I + 5)): Next

        For I = 0 To 9: T%(I) = CVI(Q$(I + 10)): Next

        Close 1
    Else
        BO% = 1
    End If 'Done checking ExitCode code

END SUB

