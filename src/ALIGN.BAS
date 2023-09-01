'----------------------------------------
'             ALIGN Subroutine
'----------------------------------------
'This routine presents a menu / interface
'that assists the user with aligning 
'player names in separate stat files. 
'(ie, Road Vs. Local) This should be done 
'before merging stats, since player name 
'entry may not be consistent.
Sub ALIGN ()

    _Title "Courtside College Basketball - Align Stats"

    Shared statPlyrNames$()
    Shared alignAR$()

    Shared AR!(), compareA!()
    Shared statsW0!(), statsW1!()
    Shared W0R!(), WR1!()
    Shared Z!(), Z1!()
    Shared ZR!(), ZR1!()

    Do

        Call GetMergePaths(pathCurrentStats$, pathRoadStats$, I$)

        If UCase$(I$) = "N" And I$ <> Chr$(27) Then

            Locate 15, 5: Print Space$(55)
            Locate 16, 5: Print Space$(55)
            Color 11

            Call ChooseTeam(YN$, NTMS, teamNbr, team$)
            
            statFile$ = nameStatFile$(team$)

            Color 12, 0
            Cls

            Locate 18, 5: Print "SEARCHING FOR FILES . . ."

            If _FileExists(pathRoadStats$ + statFile$) Then

                Open pathRoadStats$ + statFile$ For Input As #1

                Input #1, BR$

                For I = 0 To 13:

                    Input #1, alignAR$(I)

                    For J = 0 To 13: Input #1, AR!(I, J): Next

                    Input #1, W0R!(I)
                    Input #1, WR1!(I)

                Next

                For L = 0 To 15: Input #1, ZR!(L): Next

                For L = 0 To 15: Input #1, ZR1!(L): Next

                Close #1

                If _FileExists(pathCurrentStats$ + statFile$) Then

                    Open pathCurrentStats$ + statFile$ For Input As #2

                    Input #2, B$

                    For I = 0 To 13:

                        Input #2, statPlyrNames$(I)

                        For J = 0 To 13:
                            Input #2, compareA(I, J)
                        Next

                        Input #2, statsW0!(I)

                        Input #2, statsW1!(I)

                    Next

                    For L = 0 To 15: Input #2, Z!(L): Next

                    For L = 0 To 15: Input #2, Z1!(L): Next

                    Close #2

                    Call AlignStats(pathRoadStats$, statFile$)
                    DUMMY$ = "OK"

                Else

                    Cls

                    Color 7
                    Locate 2, 10
                    Print "NON-EXISTANT PATH OR INCORRECT FILE NAME!!!!"
                    Print
                    Locate , 10
                    Line Input "HIT ENTER TO RE-ENTER INFORMATION"; DUMMY$

                End If 'Done checking if current file exists

            Else

                Cls

                Color 7
                Locate 2, 10
                Print "NON-EXISTANT PATH OR INCORRECT FILE NAME!!!!"
                Print
                Locate , 10
                Line Input "HIT ENTER TO RE-ENTER INFORMATION"; DUMMY$

            End If 'Done checking if road file exists

        End If 'Done checking for ESC

    Loop Until DUMMY$ <> "" Or UCase$(I$) = "N" Or I$ = Chr$(27)

End Sub


'----------------------------------------
'         AlignStats Subroutine
'----------------------------------------
'This routine presents the user with a 
'summary of the statistics in each file.
'It then performs the alignment before 
'presenting the user a chance to review 
'the results.
Sub AlignStats (pathRoadStats$, statFile$)

    Shared statPlyrNames$()

    Shared alignAR$()
    Shared AR!()
    Shared W0R!(), WR1!()

    Shared AN$()
    Shared AN!()
    Shared W0N!(), WN1!()

    Cls

    Color 12
    Locate 1, 34: Print "STAT ROSTERS"

    Color 15
    Locate , 26: Print "CURRENT  ";
    Color 7: Print "             ROAD"

    For I = 0 To 13:

        Locate I + 4, 22

        Color 12: Print Using "##"; I;
        Print " ";
        Color 15: Print statPlyrNames$(I);
        Color 12: Print Tab(44); Using "##"; I;
        Print " ";
        Color 7: Print alignAR$(I)

    Next

    Color 11
    Locate 22: Line Input "HIT ENTER TO ALIGN"; DUMMY$
    Locate 22: Print "ALIGNING FILES . . .             "

    N = 0

    Do

        For J = 0 To 13

            If statPlyrNames$(N) = alignAR$(J) Then

                AN$(N) = alignAR$(J)

                For K = 0 To 13
                    AN!(N, K) = AR!(J, K)
                    W0N!(N) = W0R!(J)
                    WN1!(N) = WR1!(J)
                Next K

            End If

        Next J

        N = N + 1

        If N = 15 Then

            Locate 22: Print Space$(53)

            SEQ = 0

            For K = 0 To 13
                If AN$(K) <> statPlyrNames$(K) Then
                    SEQ = SEQ + 1
                End If
            Next

            If SEQ = 0 Then
                Call ReviewAndSaveAlignment(pathRoadStats$, statFile$)
            Else
                Color 14
                Locate 20
                Print "WARNING !! - - ROAD STAT FILE CONTAINS PLAYERS NOT IN EXISTING STATS.  YOU MUST"
                Print "CHANGE ROAD STAT FILE USING OPTION #6 ON GAME MENU BEFORE ATTEMPTING STAT MERGE."
                Color 13
                Locate 23, 30
                Line Input "HIT ENTER TO CONTINUE"; DUMMY$
            End If 'Done checking if players are in sequence

        End If 'Done checking N = 15

    Loop Until N = 15


End Sub


'----------------------------------------
'    ReviewAndSaveAlignment Subroutine
'----------------------------------------
'This routine displays the results of the 
'player name alignment, then gives the end
'user a chance to actually save the results 
Sub ReviewAndSaveAlignment (pathRoadStats$, statFile$)

    Shared statPlyrNames$()

    Shared ZR!(), ZR1!()

    Shared AN$()
    Shared AN!()
    Shared W0N!(), WN1!()

    Cls

    Color 12
    Locate 1, 32: Print "STAT ROSTERS"

    Color 15
    Locate , 26: Print "CURRENT  ";
    Color 7: Print "             ROAD"

    For I = 0 To 13:
        Locate I + 4, 22
        Color 12: Print Using "##"; I;
        Print " ";
        Color 15: Print statPlyrNames$(I);
        Color 12: Print Tab(44); Using "##"; I;
        Print " ";
        Color 7: Print AN$(I)
    Next

    Color 11
    Locate 22: Line Input "HIT ENTER TO ALIGN"; DUMMY$
    Locate 22: Print "ALIGNING FILES . . .             "

    Open pathRoadStats$ + statFile$ For Output As #1

    Write #1, BR$

    For I = 0 To 13:

        Write #1, AN$(I)

        For J = 0 To 13:
            Write #1, AN!(I, J)
        Next

        Write #1, W0N!(I)

        Write #1, WN1!(I)

    Next

    For L = 0 To 15: Write #1, ZR!(L): Next

    For L = 0 To 15: Write #1, ZR1!(L): Next

    Close #1

    Cls

    Locate 8, 20: Print "RE-ALIGNMENT COMPLETED"
    Color 12
    Locate 12, 20
    Line Input "HIT ENTER TO CONTINUE"; DUMMY$

    Cls

End Sub


