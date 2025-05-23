'----------------------------------------
'            ALIGN Subroutine
'----------------------------------------
'This routine presents a menu / interface
'that assists the user with aligning
'player names in separate stat files.
'(ie, Road Vs. Local) This should be done
'before merging stats, since player name
'entry may not be consistent.
Sub ALIGN ()

    _Title "Courtside College Basketball - Align Stats"

    Shared statPlyrNames$(), plyrName_Road$()

    Shared plyrStat_Road!()
    Shared stats3FGM!(), stats3FGA!()
    Shared statsOffTot!(), statsDefTot!()
    Shared threeFGM_Road!(), threeFGA_Road!()
    Shared statsOffTot_Road!(), statsDefTot_Road!()

    Do

        'Call GetMergePaths(pathCurrentStats$, pathRoadStats$, I$)
        pathCurrentStats$ = diskPaths$(1)
        pathRoadStats$ = _SelectFolderDialog$("Select folder with ROAD stats", diskPaths$(1))

        'If UCase$(I$) = "N" And I$ <> Chr$(27) Then
        If pathRoadStats$ <> "" Then

            pathRoadStats$ = pathRoadStats$ + "\"

            Locate 15, 5: Print Space$(55)
            Locate 16, 5: Print Space$(55)
            Color 11

            teamNbr = 0
            Call ChooseTeam(YN$, NTMS, teamNbr, team$)

            statFile$ = nameStatFile$(team$)

            Color 12, 0
            Cls

            Locate 18, 5: Print "SEARCHING FOR FILES . . ."

            If _FileExists(pathRoadStats$ + statFile$) Then

                Open pathRoadStats$ + statFile$ For Input As #1

                Input #1, teamNameRoad$

                For I = 0 To 13:

                    Input #1, plyrName_Road$(I)

                    For J = 0 To 13:
                        Input #1, plyrStat_Road!(I, J)
                    Next

                    Input #1, threeFGM_Road!(I)
                    Input #1, threeFGA_Road!(I)

                Next

                For L = 1 To 16:
                    Input #1, statsOffTot_Road!(L)
                Next

                For L = 1 To 16:
                    Input #1, statsDefTot_Road!(L)
                Next

                Close #1

                If _FileExists(pathCurrentStats$ + statFile$) Then

                    Call ReadTeamStats(teamNameStats$, statFile$)
                    Call AlignStats(teamNameStats$, pathRoadStats$, statFile$)
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
Sub AlignStats (teamNameStats$, pathRoadStats$, statFile$)

    Shared plyrName_Road$()
    Shared plyrName_Align$()
    Shared statPlyrNames$()

    Shared plyrStat_Road!()
    Shared threeFGM_Road!(), threeFGA_Road!()

    Shared plyrStat_Align!()
    Shared threeFGM_Align!(), threeFGA_Align!()

    Cls

    Color 12
    Locate 1, 34: Print "STAT ROSTERS"

    Color 15
    Locate 3, 26: Print "CURRENT";
    Color 7: Print Tab(48); "ROAD"

    For I = 0 To 13:

        Locate I + 5, 22

        Color 12: Print Using "##"; I;
        Print " ";
        Color 15: Print statPlyrNames$(I);
        Color 12: Print Tab(44); Using "##"; I;
        Print " ";
        Color 7: Print plyrName_Road$(I)

    Next

    Color 11
    Locate 24, 31: Line Input "HIT ENTER TO ALIGN"; DUMMY$

    N = 0

    Do

        For J = 0 To 13

            If statPlyrNames$(N) = plyrName_Road$(J) Then

                plyrName_Align$(N) = plyrName_Road$(J)

                For K = 0 To 13
                    plyrStat_Align!(N, K) = plyrStat_Road!(J, K)
                    threeFGM_Align!(N) = threeFGM_Road!(J)
                    threeFGA_Align!(N) = threeFGA_Road!(J)
                Next K

            End If

        Next J

        N = N + 1

        If N = 15 Then

            Locate 22: Print Space$(53)

            SEQ = 0

            For K = 0 To 13

                If plyrName_Align$(K) <> statPlyrNames$(K) Then
                    SEQ = SEQ + 1
                End If

            Next

            If SEQ = 0 Then
                Call ReviewAndSaveAlignment(teamNameStats$, pathRoadStats$, statFile$)
            Else
                Color 14
                Locate 20
                Print "WARNING !! ROAD STAT FILE CONTAINS PLAYERS NOT IN EXISTING STATS."
                Print "YOU MUST MODIFY ROAD STAT FILE BEFORE ATTEMPTING MERGE."
                Color 13
                Locate LAST_ROW - 2, 30
                Line Input "HIT ENTER TO CONTINUE"; DUMMY$
            End If 'Done checking if players are in sequence

        End If 'Done checking N = 15

    Loop Until N = 15


End Sub


'----------------------------------------
'   ReviewAndSaveAlignment Subroutine
'----------------------------------------
'This routine displays the results of the
'player name alignment, then gives the end
'user a chance to actually save the results
Sub ReviewAndSaveAlignment (teamNameStats$, pathRoadStats$, statFile$)

    Shared plyrName_Align$()
    Shared statPlyrNames$()

    Shared plyrStat_Align!()
    Shared threeFGM_Align!(), threeFGA_Align!()
    Shared statsOffTot_Road!(), statsDefTot_Road!()

    Cls

    Color 12
    Locate 1, 34: Print "STAT ROSTERS"

    Color 15
    Locate 3, 26: Print "CURRENT";
    Color 7: Print Tab(48); "ROAD"

    For I = 0 To 13:
        Color 12
        Locate I + 5, 22: Print Using "##"; I;
        Print " ";
        Color 15: Print statPlyrNames$(I);
        Color 12: Print Tab(44); Using "##"; I;
        Print " ";
        Color 7: Print plyrName_Align$(I)
    Next

    Color 11
    Locate 24, 26: Line Input "HIT ENTER TO SAVE ALIGNMENT"; DUMMY$

    Cls

    Locate 6, 30: Print "ALIGNING FILES . . ."

    Open pathRoadStats$ + statFile$ For Output As #1

    Write #1, teamNameStats$

    For I = 0 To 13:

        Write #1, plyrName_Align$(I)

        For J = 0 To 13:
            Write #1, plyrStat_Align!(I, J)
        Next

        Write #1, threeFGM_Align!(I)
        Write #1, threeFGA_Align!(I)

    Next

    For L = 1 To 16:
        Write #1, statsOffTot_Road!(L)
    Next

    For L = 1 To 16:
        Write #1, statsDefTot_Road!(L)
    Next

    Close #1

    Locate 8, 30: Print "RE-ALIGNMENT COMPLETED"
    Color 12
    Locate 10, 30
    Line Input "HIT ENTER TO CONTINUE"; DUMMY$

    Cls

End Sub
