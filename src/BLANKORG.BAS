Sub BLANKORG ()

    _Title "Courtside College Basketball - Delete Org Stats"

    Dim H1$(40), Z4$(40)

    Do

        Cls

        'Display list of files...
		Spec$ = diskPaths$(1) + "*.CON"
		Count% = FCount%(Spec$)

        If Count% = 0 Then
            Locate 10, 8
            Call CSMsgBox("NO ORGANIZATION FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)
        Else
            ReDim organizationFiles$(0 To Count%)

            For X = 1 To Count%
                organizationFiles$(X) = Space$(12)
            Next
			
            organizationFiles$(0) = Spec$
            Call ReadFile(organizationFiles$())

            Do
                Color , 1
                Cls
                Locate , 28
                Color 14
                Print "SELECT ORGANIZATION TO ERASE"
                Color 15
                Selection% = 1
                Start% = 1
                FileFGColr% = 15: FileBGColr% = 1
                HiLiteFGColr% = 0: HiLiteBGColr% = 7
                Rows% = 20
                Columns% = 4
                Spacing% = 6
                Row% = 4
                Column% = 1

                Locate 25, 1
                Color 14
                Print "ENTER";
                Color 15
                Print "-SELECTS ORGANIZATION";
                Color 14
                Print "HIT ESCAPE KEY TO EXIT";
                Color 15

                _MouseShow "DEFAULT"
                MAMenu organizationFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
                _MouseHide

                If ExitCode% <> 27 Then
                    Color 15, 1
                    targetFile$ = organizationFiles$(Selection%)
                    Locate 2, 33: Print targetFile$

                    Do
                        Locate 3, 33: Print "ANY CHANGE (YN)";
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                    Color 15, 0

                    If UCase$(I$) = "N" Then
                        WE$ = Left$(targetFile$, Len(targetFile$) - 4)
                        leagueFile$ = Left$(WE$, 8)

                        Cls
                        Locate 10, 8
                        Call CSMsgBox("WARNING!!!::: THIS WILL ERASE ALL COMPILED STATS FOR THIS ORGANIZATION. IF YOU STILL WISH TO ERASE ALL STATS, HIT 'Y' FOR YES", 26, Pass)
                        I$ = GetKeyPress$
                        Call CSMsgBox("", 0, Pass)

                        If UCase$(I$) = "Y" Then

                            Open diskPaths$(1) + LH$ + ".CON" For Input As #2
                            Input #2, LG$
                            For X = 1 To MAX_CONFERENCES
                                Input #2, Z4$(X)
                            Next
                            Close 2

                            For X = 1 To MAX_CONFERENCES
                                If Z4$(X) <> "XXX" Then
                                    Open diskPaths$(1) + Z4$(X) + ".TMS" For Input As #2
                                    Input #2, LG$, D1$, D2$
                                    For XW = 1 To TEAMS_PER_CONFERENCE
                                        Input #2, H1$(XW)
                                    Next
                                    Close 2

                                    For XW = 1 To TEAMS_PER_CONFERENCE
                                        If H1$(XW) <> Chr$(42) Then
                                            Print "DELETING "; H1$(XW); " STATS"
                                            statFile$ = diskPaths$(1) + H1$(XW)
                                            If _FileExists(statFile$) Then Kill statFile$
                                            If _FileExists(statFile$ + "S") Then Kill statFile$ + "S"
                                        End If
                                    Next

                                End If 'done checking for XXX
                            Next

                        End If 'Done confirming to move forward with deletion

                    End If 'Done checking correct file

                End If 'Done checking for ESC

            Loop Until ExitCode% = 27 Or UCase$(I$) = "N"

        End If 'Done checking file count

        '...Until we press ESC
    Loop Until ExitCode% = 27 Or Count% = 0 Or UCase$(I2$) = "Y"

End Sub
