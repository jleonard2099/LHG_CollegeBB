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
        Selection% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        Column% = 28

        Call box0(7, 27, 11, 51, 2, 15, 1)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case Selection%

                Case 1: 
                    Call MergeStats
                
                Case 2: 
                    Call ALIGN

            End Select

        End If

    Loop Until Selection% = 3 Or ExitCode% = 27

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

    Dim ARP$(14)

    Shared Ycurr%, Yroad%

    Shared statPlyrNames$()
    Shared mergeH$(), HRP$()
    Shared mergeO$(), ORP$(), gameSite$(), SITERP$()

    Shared ARD(), CRD(), CRDRD()

    Shared APRD%()
    Shared mergeO%(), mergeT%()
    Shared mergeA(), mergeAP%()
    Shared ORD%(), TRD%()

    Shared statsW0!(), statsW1!()
    Shared statsZ!(), statsZ1!()
    Shared W0RD!(), W1RD!()
    Shared ZRD!(), ZRD1!()

    Do

        Call GetMergePaths(pathCurrentStats$, pathRoadStats$, I$)

        If UCase$(I$) = "N" And I$ <> Chr$(27) Then

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

                    Input #1, B$

                    For I = 0 To 13: 

                        Input #1, ARP$(I)

                        For J = 0 To 13: 
                            Input #1, ARD(I, J)
                        Next
                        
                        Input #1, W0RD!(I)
                        
                        Input #1, W1RD!(I)
                        
                    Next

                    For L = 0 To 15: Input #1, ZRD!(L): Next

                    For L = 0 To 15: Input #1, ZRD1!(L): Next

                    Close #1

                    Open pathRoadStats$ + statFile$ + "S" For Input As #2

                    Input #2, Yroad%

                    For I = 1 To 100: Input #2, ORD%(I): Next I

                    For I = 1 To 100: Input #2, ORP$(I): Next I

                    For I = 1 To 100: Input #2, TRD%(I): Next I

                    For I = 1 To 100: Input #2, HRP$(I): Next I

                    For I = 1 To 100
                        For I1 = 0 To 1: Input #2, APRD%(I, I1): Next
                    Next

                    For I = 1 To 100: Input #2, CRDRD(I): Next I

                    For I = 1 To 100: Input #2, SITERP$(I): Next I

                    Close #2

                    Locate 21, 5: Color 3

                    For J = 21 To 23
                        Locate J
                        Print Space$(64)
                    Next

                    If _FileExists(pathCurrentStats$ + statFile$) Then

                        Open pathCurrentStats$ + statFile$ For Input As #3

                        Input #3, B$

                        For I = 0 To 13: 

                            Input #3, statPlyrNames$(I)

                            For J = 0 To 13: 
                                Input #3, mergeA(I, J)
                            Next

                            Input #3, statsW0!(I)
                            Input #3, statsW1!(I)

                        Next

                        For L = 0 To 15: 
                            Input #3, statsZ!(L)
                        Next

                        For L = 0 To 15: 
                            Input #3, statsZ1!(L)
                        Next

                        Close #3

                        Open pathRoadStats$ + statFile$ + "S" For Input As #4

                        Input #4, Ycurr%

                        For I = 1 To 100: Input #4, mergeO%(I): Next I

                        For I = 1 To 100: Input #4, mergeO$(I): Next I

                        For I = 1 To 100: Input #4, mergeT%(I): Next I

                        For I = 1 To 100: Input #4, mergeH$(I): Next I

                        For I = 1 To 100

                            For I1 = 0 To 1: 
                                Input #4, mergeAP%(I, I1)
                            Next

                        Next

                        For I = 1 To 100: 
                            Input #4, CRD(I)
                        Next I

                        For I = 1 To 100: 
                            Input #4, gameSite$(I)
                        Next I

                        Close #4

                        SEQ = 0

                        For K = 0 To 13
                            If UCase$(ARP$(K)) <> UCase$(statPlyrNames$(K)) Then
                                SEQ = SEQ + 1
                            End If
                        Next

                        If SEQ = 0 Then

                            Call DoTheMerge(pathCurrentStats$, statFile$, B$)

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

    Loop Until DUMMY$ <> "" Or UCase$(I$) = "N"

End Sub


'----------------------------------------
'       DoTheMerge Subroutine
'----------------------------------------
'This subroutine literally adds the stat
'values together from each file and then
'overwrites the current file with the
'newly aggregated results.
Sub DoTheMerge (pathCurrentStats$, statFile$, B$)

    Shared Ycurr%, Yroad%

    Shared statPlyrNames$()
    Shared mergeH$(), HRP$()
    Shared mergeO$(), ORP$(), gameSite$(), SITERP$()

    Shared APRD%(), ARD(), CRD(), CRDRD()
    Shared mergeO%(), mergeT%()
    Shared mergeA(), mergeAP%()
    Shared ORD%(), TRD%()
    Shared W0RD!(), W1RD!()

    Shared statsW0!(), statsW1!()
    Shared statsZ!(), statsZ1!()
    Shared ZRD!(), ZRD1!()

    For K = 1 To Yroad%
        Ycurr% = Ycurr% + 1
        mergeO%(Ycurr%) = ORD%(K)
        mergeO$(Ycurr%) = ORP$(K)
        mergeT%(Ycurr%) = TRD%(K)
        mergeH$(Ycurr%) = HRP$(K)
        mergeAP%(Ycurr%, 0) = APRD%(Ycurr%, 0)
        mergeAP%(Ycurr%, 1) = APRD%(Ycurr%, 1)
        CRD(Ycurr%) = CRDRD(Ycurr%)
        gameSite$(Ycurr%) = SITERP$(Ycurr%)
    Next

    Locate 18, 20: Color 12: Print "MERGING STAT FILE AND SCHEDULE RESULTS . . . ."

    For I = 0 To 13

        For J = 0 To 12: 
            mergeA(I, J) = mergeA(I, J) + ARD(I, J)
        Next J

        statsW0!(I) = statsW0!(I) + W0RD!(I)
        statsW1!(I) = statsW1!(I) + W1RD!(I)

        If mergeA(I, 13) < ARD(I, 13) Then 
            mergeA(I, 13) = ARD(I, 13) 
        Else 
            mergeA(I, 13) = mergeA(I, 13)
        End If

    Next I

    For L = 0 To 15:
        statsZ!(L) = statsZ!(L) + ZRD!(L)
        statsZ1!(L) = statsZ1!(L) + ZRD1!(L)
    Next

    Open pathCurrentStats$ + statFile$ For Output As #1

    Write #1, B$

    For I = 0 To 13: 
        Write #1, statPlyrNames$(I)
        For J = 0 To 13: Write #1, mergeA(I, J): Next
        Write #1, statsW0!(I)
        Write #1, statsW1!(I)
    Next

    For L = 0 To 15: Write #1, statsZ!(L): Next

    For L = 0 To 15: Write #1, statsZ1!(L): Next

    Close #1

    Open pathCurrentStats$ + statFile$ + "S" For Output As #2

    Write #2, Ycurr%

    For I = 1 To 100: Write #2, mergeO%(I): Next I
    For I = 1 To 100: Write #2, mergeO$(I): Next I
    For I = 1 To 100: Write #2, mergeT%(I): Next I
    For I = 1 To 100: Write #2, mergeH$(I): Next I

    For I = 1 To 100
        For I1 = 0 To 1: Write #2, mergeAP%(I, I1): Next
    Next

    For I = 1 To 100: Write #2, CRD(I): Next I
    For I = 1 To 100: Write #2, gameSite$(I): Next I

    Close #2

    Y = 0

    Locate 20, 20: Color 12: Print "RESULTS SUCCESSFULLY MERGED!!"

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
