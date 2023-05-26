Sub BLANK ()

    _Title "Courtside College Basketball - Delete Conference Stats"

    Dim H1$(MAX_CONFERENCES)

    Do

        Cls

        'Display list of files...
		Spec$ = diskPaths$(1) + "*.LIB"
		Count% = FCount%(Spec$)

        If Count% = 0 Then
            Locate 10, 8
            Call CSMsgBox("NO CONFERENCE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)
        Else
            ReDim conferenceFiles$(0 To Count%)

            For X = 1 To Count%
                conferenceFiles$(X) = Space$(12)
            Next

            conferenceFiles$(0) = Spec$
            Call ReadFile(conferenceFiles$())

            Do
                'Select League
                Color , 1
                Cls
                Locate , 28
                Color 14
                Print "SELECT CONFERENCE TO ERASE"
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
                Print "-SELECTS CONFERENCE";
                Color 14
                Print "HIT ESCAPE KEY TO EXIT";
                Color 15

                _MouseShow "DEFAULT"
                MAMenu conferenceFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
                _MouseHide

                If ExitCode% <> 27 Then

                    Color 15, 1
                    targetFile$ = RTrim$(conferenceFiles$(Selection%))
                    Locate 2, 33: Print targetFile$

                    Do
                        Locate 3, 33: Print "ANY CHANGE (YN)";
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or I$ = Chr$(27)

                    Color 15, 0

                    If UCase$(I$) = "N" Then
                        WE$ = Left$(targetFile$, Len(targetFile$) - 4)
                        leagueFile$ = Left$(WE$, 8)

                        Color 7,0
                        Cls

                        Locate 10, 8
                        Call CSMsgBox("WARNING!!!::: THIS WILL ERASE ALL COMPILED STATS FOR THIS CONFERENCE. IF YOU STILL WISH TO ERASE ALL STATS, HIT 'Y' FOR YES", 26, Pass)
                        G$ = GetKeyPress$
                        Call CSMsgBox("", 0, Pass)

                        If UCase$(G$) = "Y" Then

                            Open diskPaths$(1) + leagueFile$ + ".TMS" For Input As #2
                            Input #2, LG$, D1$, D2$ ', D3$, D4$
                            For XW = 1 To TEAMS_PER_CONFERENCE
                                Input #2, H1$(XW)
                            Next
                            Close 2

                            For XW = 1 To TEAMS_PER_CONFERENCE
                                If H1$(XW) <> Chr$(42) Then
                                    statFileName$ = diskPaths$(1) + H1$(XW)
                                    Print "DELETING "; H1$(XW); " STATS"
                                    If _FileExists(statFileName$) Then Kill statFileName$
                                    If _FileExists(statFileName$ + "D") Then Kill statFileName$ + "D"
                                    If _FileExists(statFileName$ + "R") Then Kill statFileName$ + "R"
                                    If _FileExists(statFileName$ + "S") Then Kill statFileName$ + "S"
                                End If
                            Next

                        End If 'Done confirming to move forward with deletion

                    End If 'Done checking correct file

                End If 'Done checking for ESC

            Loop Until ExitCode% = 27 Or UCase$(I$) = "N"

        End If 'Done checking file count

        '...Until we press ESC
    Loop Until ExitCode% = 27 Or Count% = 0 Or UCase$(I$) = "Y"

End Sub
