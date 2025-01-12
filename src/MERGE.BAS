'----------------------------------------
'            MERGE Subroutine
'----------------------------------------
'This subroutine gives the user options
'for aligning / merging stat files
Sub MERGE ()

    _Title "Courtside College Basketball - Merge Stats"

    ReDim menuChoices$(1 To 3)

    Do

        Color , 0
        Cls

        Call TITLE_MERGE

        menuChoices$(1) = "MERGE STATISTICAL FILES"
        menuChoices$(2) = "ALIGN STATISTICAL FILES"
        menuChoices$(3) = "EXIT TO MAIN MENU      "
        SELECTION% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row%-1, Column%-1, Row%+Count%, Column%+menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case SELECTION%

                Case 1: 
                    Call MergeStats
                
                Case 2: 
                    Call ALIGN

            End Select

        End If

    Loop Until SELECTION% = 3 Or ExitCode% = 27

End Sub

'----------------------------------------
'          MergeStats Subroutine
'----------------------------------------
'This subroutine determines the parameters
'for the road and existing stat files that
'can be merged. It then reads in all stat
'files, checks for player sequence, and then
'calls a supporting routine to actually
'perform the merge
Sub MergeStats ()

    Shared gameAttendance&(), gameAtt_Road&()

    Shared powerRating_Road(), powerRating()

    Shared oppScore(), oppScore_Road()
    Shared teamScore(), teamScore_Road()

    Shared plyrStat_Merge!(), plyrRat_MERGE()
    Shared statsW0!(), statsW1!()
    Shared statsOffTot!(), statsDefTot!()
    Shared threeFGM_Merge!(), threeFGA_Merge!()
    Shared statsOffTot_Road!(), statsDefTot_Road!()

    Shared gameSite$(), gameSite_Road$()
    Shared locIndicator$(), locIndicator_Road$()
    Shared oppName$(), oppName_Road$()
    Shared statPlyrNames$(), plyrName_Road$()

    Do

        Call GetMergePaths(pathCurrentStats$, pathRoadStats$, I$)

        If UCase$(I$) = "N" And I$ <> Chr$(27) Then

            teamNbr = 0
            Call ChooseTeam(YN$, NTMS, teamNbr, team$)
            statFile$ = nameStatFile$(team$)

            Color 15, 0
            Cls

            Locate 10, 15: Print "ROAD STATS ARE ON DRIVE "; pathRoadStats$
            Locate , 15: Print "MASTER STATS ARE ON DRIVE "; pathCurrentStats$
            Locate , 15: Print "TEAM TO MERGE STATS FOR IS "; team$

            Do
                Locate 14, 15: Print "ANY CHANGE? (Y/N)"
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            If UCase$(I$) = "N" Then

                If _FileExists(pathRoadStats$ + statFile$) Then

                    Open pathRoadStats$ + statFile$ For Input As #1

                    Input #1, teamNameRoad$

                    For I = 0 To 13: 

                        Input #1, plyrName_Road$(I)

                        For J = 0 To 13: 
                            Input #1, plyrStat_Merge!(I, J)
                        Next
                        
                        Input #1, threeFGM_Merge!(I)
                        Input #1, threeFGA_Merge!(I)
                        
                    Next

                    For L = 0 To 15: 
                        Input #1, statsOffTot_Road!(L)
                    Next

                    For L = 0 To 15: 
                        Input #1, statsDefTot_Road!(L)
                    Next

                    Close #1

                    Open pathRoadStats$ + statFile$ + "S" For Input As #2

                    Input #2, nbrGamesRoad

                    For I = 1 To MAX_SCHED_STATS: 
                        Input #2, teamScore_Road(I)
                    Next I

                    For I = 1 To MAX_SCHED_STATS: 
                        Input #2, oppName_Road$(I)
                    Next I

                    For I = 1 To MAX_SCHED_STATS: 
                        Input #2, oppScore_Road(I)
                    Next I

                    For I = 1 To MAX_SCHED_STATS: 
                        Input #2, locIndicator_Road$(I)
                    Next I

                    For I = 1 To MAX_SCHED_STATS
                        For I1 = 0 To 1: 
                            Input #2, powerRating_Road(I, I1)
                        Next
                    Next

                    For I = 1 To MAX_SCHED_STATS: 
                        Input #2, gameAtt_Road&(I)
                    Next I

                    For I = 1 To MAX_SCHED_STATS: 
                        Input #2, gameSite_Road$(I)
                    Next I

                    Close #2

                    Locate 21, 5: Color 3

                    For J = 21 To 23
                        Locate J
                        Print Space$(64)
                    Next

                    If _FileExists(pathCurrentStats$ + statFile$) Then

                        Open pathCurrentStats$ + statFile$ For Input As #3

                        Input #3, teamNameStats$

                        For I = 0 To 13: 

                            Input #3, statPlyrNames$(I)

                            For J = 0 To 13: 
                                Input #3, plyrRat_MERGE(I, J)
                            Next

                            Input #3, statsW0!(I)
                            Input #3, statsW1!(I)

                        Next

                        For L = 0 To 15: 
                            Input #3, statsOffTot!(L)
                        Next

                        For L = 0 To 15: 
                            Input #3, statsDefTot!(L)
                        Next

                        Close #3

                        Open pathRoadStats$ + statFile$ + "S" For Input As #4

                        Input #4, nbrGamesCurr

                        For I = 1 To MAX_SCHED_STATS: 
                            Input #4, teamScore(I)
                        Next I

                        For I = 1 To MAX_SCHED_STATS: 
                            Input #4, oppName$(I)
                        Next I

                        For I = 1 To MAX_SCHED_STATS: 
                            Input #4, oppScore(I)
                        Next I

                        For I = 1 To MAX_SCHED_STATS: 
                            Input #4, locIndicator$(I)
                        Next I

                        For I = 1 To MAX_SCHED_STATS

                            For I1 = 0 To 1: 
                                Input #4, powerRating(I, I1)
                            Next

                        Next

                        For I = 1 To MAX_SCHED_STATS: 
                            Input #4, gameAttendance&(I)
                        Next I

                        For I = 1 To MAX_SCHED_STATS: 
                            Input #4, gameSite$(I)
                        Next I

                        Close #4

                        SEQ = 0

                        For K = 0 To 13
                            If UCase$(plyrName_Road$(K)) <> UCase$(statPlyrNames$(K)) Then
                                SEQ = SEQ + 1
                            End If
                        Next

                        If SEQ = 0 Then

                            Call DoTheMerge(pathCurrentStats$, statFile$, teamNameStats$, nbrGamesCurr, nbrGamesRoad)

                            For J = 13 To 23
                                Locate J
                                Print Space$(65)
                            Next

                        Else

                            Cls
                            Color 14
                            Locate 5, 27: Print "PLAYERS NOT IN SEQUENCE !!"
                            Color 15
                            Locate 7, 20: Print "STATS WILL BE DISTORTED UNLESS THE ROAD"
                            Locate , 19: Print "FILE LISTS THE PLAYERS IN THE SAME SEQUENCE"
                            Locate , 24: Print "AS YOUR CURRENT PLAYER STAT FILE."
                            Color 12
                            Locate 11, 24: Print "YOU MUST REALIGN ROAD STAT FILE OR"
                            Locate , 31: Print "ENTER STATS MANUALLY."
                            Color 13
                            Locate 15, 29: Line Input "HIT ENTER TO GO TO MENU"; DUMMY$

                        End If 'Done checking sequence

                    Else

                        Cls
                        Color 7
                        Locate 2, 10: Print "NON-EXISTANT PATH OR INCORRECT FILE NAME!!!!"
                        Print
                        Locate , 10: Line Input "HIT ENTER TO RE-ENTER INFORMATION"; DUMMY$

                        Do
                            Locate , 10
                            Print "TRY AGAIN? (Y/N) "
                            I$ = GetKeyPress$
                        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                        If UCase$(I$) = "Y" Then
                            Print
                            Print "HIT ENTER TO RE-ENTER INFORMATION"
                            I$ = GetKeyPress$
                        End If

                    End If 'Done checking if home file exists

                Else

                    Cls
                    Locate 2, 10
                    Color 7: Print "NON-EXISTANT PATH OR INCORRECT FILE NAME!!!!"
                    Print
                    Locate , 10: Line Input "HIT ENTER TO RE-ENTER INFORMATION"; DUMMY$

                    Do
                        Locate , 10
                        Print "TRY AGAIN? (Y/N) "
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                    If UCase$(I$) = "Y" Then
                        Print
                        Print "HIT ENTER TO RE-ENTER INFORMATION"
                        I$ = GetKeyPress$
                    End If

                End If 'Done checking if road file exists

            End If 'Done checking if ready to proceed

        End If 'Done checking for ESC

    Loop Until DUMMY$ <> "" Or UCase$(J$) = "N" Or UCase$(I$) = "N" Or I$ = Chr$(27)

End Sub


'----------------------------------------
'       DoTheMerge Subroutine
'----------------------------------------
'This subroutine literally adds the stat
'values together from each file and then
'overwrites the current file with the
'newly aggregated results.
Sub DoTheMerge (pathCurrentStats$, statFile$, teamNameStats$, nbrGamesCurr, nbrGamesRoad)

    Shared gameAttendance&(), gameAtt_Road&()

    Shared powerRating(), powerRating_Road()

    Shared oppScore(), oppScore_Road() 
    Shared teamScore(), teamScore_Road()

    Shared plyrStat_Merge!(), plyrRat_MERGE()

    Shared statsW0!(), statsW1!()
    Shared statsOffTot!(), statsDefTot!()
    Shared threeFGM_Merge!(), threeFGA_Merge!()
    Shared statsOffTot_Road!(), statsDefTot_Road!()

    Shared gameSite$(), gameSite_Road$()
    Shared locIndicator$(), locIndicator_Road$()
    Shared oppName$(), oppName_Road$()
    Shared statPlyrNames$()

    For K = 1 To nbrGamesRoad
        nbrGamesCurr = nbrGamesCurr + 1
        teamScore(nbrGamesCurr) = teamScore_Road(K)
        oppName$(nbrGamesCurr) = oppName_Road$(K)
        oppScore(nbrGamesCurr) = oppScore_Road(K)
        locIndicator$(nbrGamesCurr) = locIndicator_Road$(K)
        powerRating(nbrGamesCurr, 0) = powerRating_Road(nbrGamesCurr, 0)
        powerRating(nbrGamesCurr, 1) = powerRating_Road(nbrGamesCurr, 1)
        gameAttendance&(nbrGamesCurr) = gameAtt_Road&(nbrGamesCurr)
        gameSite$(nbrGamesCurr) = gameSite_Road$(nbrGamesCurr)
    Next

    Color 12
    Locate 18, 20: Print "MERGING STAT FILE AND SCHEDULE RESULTS . . . ."

    For I = 0 To 13

        For J = 0 To 12: 
            plyrRat_MERGE(I, J) = plyrRat_MERGE(I, J) + plyrStat_Merge!(I, J)
        Next J

        statsW0!(I) = statsW0!(I) + threeFGM_Merge!(I)
        statsW1!(I) = statsW1!(I) + threeFGA_Merge!(I)

        If plyrRat_MERGE(I, 13) < plyrStat_Merge!(I, 13) Then 
            plyrRat_MERGE(I, 13) = plyrStat_Merge!(I, 13) 
        Else 
            plyrRat_MERGE(I, 13) = plyrRat_MERGE(I, 13)
        End If

    Next I

    For L = 0 To 15:
        statsOffTot!(L) = statsOffTot!(L) + statsOffTot_Road!(L)
        statsDefTot!(L) = statsDefTot!(L) + statsDefTot_Road!(L)
    Next

    Open pathCurrentStats$ + statFile$ For Output As #1

    Write #1, teamNameStats$

    For I = 0 To 13: 

        Write #1, statPlyrNames$(I)

        For J = 0 To 13: 
            Write #1, plyrRat_MERGE(I, J)
        Next

        Write #1, statsW0!(I)
        Write #1, statsW1!(I)

    Next

    For L = 0 To 15: 
        Write #1, statsOffTot!(L)
    Next

    For L = 0 To 15: 
        Write #1, statsDefTot!(L)
    Next

    Close #1

    Open pathCurrentStats$ + statFile$ + "S" For Output As #2

    Write #2, nbrGamesCurr

    For I = 1 To MAX_SCHED_STATS: 
        Write #2, teamScore(I)
    Next I
    
    For I = 1 To MAX_SCHED_STATS: 
        Write #2, oppName$(I)
    Next I

    For I = 1 To MAX_SCHED_STATS: 
        Write #2, oppScore(I)
    Next I

    For I = 1 To MAX_SCHED_STATS: 
        Write #2, locIndicator$(I)
    Next I

    For I = 1 To MAX_SCHED_STATS
        For I1 = 0 To 1: 
            Write #2, powerRating(I, I1)
        Next
    Next

    For I = 1 To MAX_SCHED_STATS: 
        Write #2, gameAttendance&(I)
    Next I
    
    For I = 1 To MAX_SCHED_STATS: 
        Write #2, gameSite$(I)
    Next I

    Close #2

    Y = 0

    Color 12
    Locate 20, 20: Print "RESULTS SUCCESSFULLY MERGED!!"

End Sub


'----------------------------------------
'       GetMergePaths Subroutine
'----------------------------------------
'This subroutine assists the user with the
'entry of the separate paths to be used 
'for the merge / align process.
Sub GetMergePaths (pathCurrentStats$, pathRoadStats$, promptResponse$)

    Shared opSysType$

    Do
        Color 12, 0
        Cls

        Locate 2, 5: Print "ENTER COMPLETE PATH FOR TEAM STATS TO MERGE/ALIGN"
        Color 15
        Select Case opSysType$
            Case "[WINDOWS]":
                Locate 4, 5: Print "EXAMPLE - A:"
                Locate , 5: Print "          C:\HAFFNER"
                Locate , 5: Print "          C:\HAFFNER\1960"
                Locate , 5: Print "          D:\CBASKET"
            Case "[LINUX]":
                Locate 4, 5: Print "EXAMPLE - /mnt/floppy"
                Locate , 5: Print "           /usr/bin/haffner"
                Locate , 5: Print "           /usr/bin/haffner/1960"
                Locate , 5: Print "           /home/haffner/cbasket"
            Case "[MAC]", "[MACOSX]":
                Locate 4, 5: Print "EXAMPLE - /Volumes/floppy"
                Locate , 5: Print "           /Applications/haffner"
                Locate , 5: Print "           /Applications/haffner/1960"
                Locate , 5: Print "           /Users/haffner/cbasket"
        End Select

        For J = 10 To 13
            Locate J
            Print Space$(70)
        Next

        Color 10
        Locate 10, 5
        Line Input "ROAD STATS:     "; pathRoadStats$
        Locate , 5
        Locate , 5
        Line Input "CURRENT STATS:  "; pathCurrentStats$

        Color 14
        Locate 13, 5: Print "ANY CHANGE? (Y/N)"
        promptResponse$ = GetKeyPress$

    Loop Until UCase$(promptResponse$) = "N" Or promptResponse$ = Chr$(27)

    'Adjust path terminator for OS type
    Select Case opSysType$
        Case "[WINDOWS]":
            If Not (Right$(pathCurrentStats$, 1) = "\") Then
                pathCurrentStats$ = pathCurrentStats$ + "\"
            End If

            If Not (Right$(pathRoadStats$, 1) = "\") Then
                pathRoadStats$ = pathRoadStats$ + "\"
            End If

        Case "[MAC]", "[MACOSX]", "[LINUX]":
            If Not (Right$(pathCurrentStats$, 1) = "/") Then
                pathCurrentStats$ = pathCurrentStats$ + "/"
            End If

            If Not (Right$(pathRoadStats$, 1) = "/") Then
                pathRoadStats$ = pathRoadStats$ + "/"
            End If
    End Select

End Sub
