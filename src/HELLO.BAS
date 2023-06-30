$Debug

Const GAME_VERSION$ = "4.32"
Const MAX_TEAMS = 1000
Const MAX_GAMES = 1500
Const MAX_FILES = 10240
'optimium = 1048576 (but slows code)
Const MAX_CONFERENCES = 40
Const TEAMS_PER_DIV = 15
Const DIVS_PER_CONFERENCE = 2
Const TEAMS_PER_CONFERENCE = 30
Const TEAM_SIZE_BYTES = 655
Const DATA_SIZE_BYTES = 566

'Const NUM_STATRECORDS = 162

DefInt A-Z

'INCLUDE: 'FileTeamSelection.bi'
'$INCLUDE: 'KeyInput.bi'
'$INCLUDE: 'QPProEqu.bi'
'$INCLUDE: 'PadRight.bi'
'$INCLUDE: 'Variables.bi'

' For Utils
'$INCLUDE: 'MainRoutines.bi'

'$INCLUDE: 'TITLE.bi'

' For Game
'$INCLUDE: 'GameRoutines.bi'


On Error GoTo Errhandler


'----------------------------------------
'          READ IN ALL DATA
'----------------------------------------
Data GAMES,MINUTES,FIELD GOALS MADE,FIELD GOAL ATTEMPS,F.G. PCT. (3.65 FGM/GAME),THREE POINTS MADE,THREE POINT ATTEMPTS,3FG (.30 3FGM/GAME),FREE THROWS MADE,FREE THROWS ATTEMPTED,F.T. PCT. (1.52 FTM/GAME)
Data POINTS,SCORING AVERAGE,HI-GAME,OFFENSIVE REBOUNDS,DEFENSIVE REBOUNDS,TOTAL REBOUNDS,AVERAGE REBOUNDS PER GAME,FOULS,ASSISTS,BLOCKED SHOTS,STEALS,TURNOVERS
Data POINTS/48 MINUTES,REBOUNDS/48 MINUTES,FOULS/48 MINUTES,ASSISTS/48 MINUTES,BLOCKS/48 MINUTES,STEALS/48 MINUTES,TURNOVERS/48 MINUTES,MINUTES/GAME,GO TO MAIN MENU
For I = 0 To 31: Read TP$(I): Next

Data FIELD GOAL ATTEMPTS,FIELD GOALS MADE,F.G. PCT.,THREE POINT ATTEMPTS,THREE POINTS MADE,THREE POINT PERCENTAGE,FREE THROWS ATTEMPTED,FREE THROWS MADE,FREE THROW PERCENTAGE,OFFENSIVE REBOUNDS
Data DEFENSIVE REBOUNDS,TOTAL REBOUNDS,REBOUNDS PER GAME,FOULS,ASSISTS,BLOCKS,STEALS,TURNOVERS,POINTS PER GAME
Data OPP. FIELD GOAL ATTEMPTS,OPP. FIELD GOALS MADE,OPP. FIELD GOAL PERCENTAGE,OPP. THREE POINTERS ATTEMPTED,OPP. THREE POINTERS MADE,OPP. THREE POINT PERCENTAGE,OPP. FREE THROWS ATTEMPTED,OPP. FREE THROWS MADE,OPP. FREE THROW PERCENTAGE
Data OPP. OFFENSIVE REBOUNDS,OPP. DEFENSIVE REBOUNDS,OPP. TOTAL REBOUNDS,OPP. REBOUNDS PER GAME,OPP. FOULS,OPP. ASSISTS,OPP. BLOCKS,OPP. STEALS,OPP. TURNOVERS,OPP. POINTS PER GAME,RETURN TO MAIN MENU
For I = 0 To 38: Read TP1$(I): Next

Data NAME,G,MIN,MIN/G,FGM,FGA,FG PCT,3FGM,3FGA,3FG PCT,FTM,FTA,FT PCT
Data NAME,PTS,PTG/G,HI,OFF REB,DEF REB,TOT REB,REB/G,PF,AST,BLK,STL,TO
For I = 0 To 25: Read CP$(I): Next

Data GAMES PLAYED,FIELD GOALS ATTEMPTED,FIELD GOALS MADE,FIELD GOAL %,3-FIELD GOALS ATTEMPTED,3-FIELD GOALS MADE,3-FIELD GOAL %
Data FREE THROWS ATTEMPTED,FREE THROWS MADE,FREE THROW %,MINUTES PLAYED,POINTS,SCORING AVERAGE,HI-GAME,OFFENSIVE REBOUNDS
Data DEFENSIVE REBOUNDS,TOTAL REBOUNDS,REBOUNDS PER GAME,PERSONAL FOULS,STEALS,ASSISTS,BLOCKS,TURNOVERS,MINUTES PER GAME,MVP VOTING
For I = 1 To 25: Read recconTB$(I): Next

Data FIELD GOALS ATTEMPTED,FIELD GOALS MADE,FIELD GOAL %,3-FIELD GOALS ATTEMPTED,3-FIELD GOALS MADE,3-FIELD GOAL %
Data FREE THROWS ATTEMPTED,FREE THROWS MADE,FREE THROW %,OFFENSIVE REBOUNDS,DEFENSIVE REBOUNDS,TOTAL REBOUNDS,REBOUNDS PER GAME
Data PERSONAL FOULS,STEALS,ASSISTS,BLOCKS,TURNOVERS,POINTS,POINTS PER GAME
For I = 1 To 20: Read TT$(I): Next


Data L,1,1,1,1,1,1,H,H,2,2,2,2,2,F
For I = 0 To 14: Read R$(I): Next

Data 0,1,1,2,2,2,-1,-1,-1,-1,-2,-3
For I = 0 To 1
    For I1 = 1 To 6
        Read HF%(I, I1)
    Next
Next

Data 1,4,2,5,15,29,22,36,7,9,7,12
For I = 0 To 2
    For I1 = 1 To 4
        Read DP(I, I1)
    Next
Next

Data -7,-10,-4,-10,70,100,20,100,-1,1,-2,2,15,22,8,29,5,7,3,9
For I = 0 To 4
    For I1 = 0 To 3
        Read D8(I, I1)
    Next
Next

Data "NORMAL"
Data "AGGRESSIVE"
Data "SAFE"
Data "QUICK OFFENSE (LAST 3 MIN)"
Data "DELAY OFFENSE (LAST 3 MIN)"
Data "MILK CLOCK TO 15 SEC (IF USING SHOT CLOCK)"
Data "STALL-ONLY REAL GOOD SHOT (MUST BE WITHIN 10 POINTS)"
Data "TOTAL STALL (LAST 3 MIN)"
Data "HOLD BALL VS PASSIVE ZONE (MUST BE AHEAD, WITH NO SHOT CLOCK)"
Data "SHOOT ONLY THREE POINTERS (LAST 3 MIN)"
For I = 0 To 9: Read O$(I): Next

Data "SOLID MAN TO MAN"
Data "PRESSURE MAN TO MAN"
Data "PASSIVE 2-3 ZONE"
Data "1-3-1 TRAPPING ZONE"
Data "FULL COURT MAN TO MAN PRESS/SOLID MAN TO MAN"
Data "FULL COURT MAN TO MAN (RUN & JUMP) PRESS/SOLID MAN TO MAN"
Data "FULL COURT MAN TO MAN (RUN & JUMP) PRESS/PRESSURE MAN TO MAN"
Data "PASSIVE 2-2-1 ZONE PRESS/2-3 ZONE"
Data "PASSIVE 2-2-1 ZONE PRESS/SOLID MAN TO MAN"
Data "PASSIVE 2-2-1 ZONE PRESS/PRESSURE MAN TO MAN"
Data "TRAPPING 1-2-1-1 ZONE PRESS/SOLID MAN TO MAN"
Data "TRAPPING 1-2-1-1 ZONE PRESS/PRESSURE MAN TO MAN"
Data "TRAPPING 1-2-1-1 ZONE PRESS/2-3 ZONE  "
Data "TRAPPING 1-2-1-1 ZONE PRESS/1-3-1 TRAPPING ZONE"
Data "FULL COURT M-M DENIAL & FOUL (LAST 3 MIN WHEN DOWN)"
For I = 0 To 14
    Read D$(I)
Next I

Data "NORMAL                            "
Data "AGGRESSIVE                        "
Data "SAFE                              "
Data "QUICK OFFENSE (LAST 3 MIN)        "
Data "DELAY OFFENSE (LAST 3 MIN)        "
Data "MILK CLOCK TO 15 SEC-IF USING S.C."
Data "STALL-GOOD SHOT-MUST BE WITHIN 10 "
Data "TOTAL STALL (LAST 3 MIN)          "
Data "HOLD BALL VS PASS.ZN-AHEAD-NO S.C."
Data "SHOOT ONLY THREES (LAST 3 MIN)    "
For I = 0 To 9
    Read OB$(I)
Next

Data "SOLID MAN TO MAN                  "
Data "PRESSURE MAN TO MAN               "
Data "PASSIVE 2-3 ZONE                  "
Data "1-3-1 TRAPPING ZONE               "
Data "FULL COURT M-M PRESS/SOLID M-M    "
Data "M-M (RUN & JUMP) PRESS/SOLID M-M  "
Data "M-M (RUN & JMP) PRESS/PRESSURE M-M"
Data "PASSIVE 2-2-1 ZONE PRESS/2-3 ZONE "
Data "PASSIVE 2-2-1 ZONE PRESS/SOLID M-M"
Data "PASSIVE 2-2-1 ZN PRSS/PRESSURE M-M"
Data "TRAP 1-2-1-1 ZONE PRESS/SOLID M-M "
Data "TRAP 1-2-1-1 ZN PRESS/PRESSURE M-M"
Data "TRAP 1-2-1-1 ZONE PRESS/2-3 ZONE  "
Data "TRAP 1-2-1-1 ZN PRSS/1-3-1 TRAP ZN"
Data "DENIAL & FOUL-LAST 3 MIN WHEN DOWN"
For I = 0 To 14
    Read DB$(I)
Next I


' Game options
modeAbbrev$(0) = "HH"
modeAbbrev$(1) = "CV"
modeAbbrev$(2) = "CH"
modeAbbrev$(3) = "CC"

MO$(0) = "HUMAN OPPONENT           "
MO$(1) = "COMPUTER IS VISITING TEAM"
MO$(2) = "COMPUTER IS HOME TEAM    "
MO$(3) = "COMPUTER VS COMPUTER     "

sclockOpt$(0) = "NO SHOT CLOCK       "
sclockOpt$(1) = "45 SECOND SHOT CLOCK"
sclockOpt$(2) = "35 SECOND SHOT CLOCK"

yesNo$(0) = "N"
yesNo$(1) = "Y"

foulShotOpt$(0) = "SHOOT 1+1"
foulShotOpt$(1) = "SHOOT 2  "

location$(0) = "@ TEAM 1            "
location$(1) = "HOME COURT ADVANTAGE"
location$(2) = "NEUTRAL SITE        "

coachOpt$(0) = "COACH AND SELECT SHOTS           "
coachOpt$(1) = "COACH ONLY AND LET COMPUTER SHOOT"

'----------------------------------------
'               MAIN CODE
'----------------------------------------
_Title "Courtside College Basketball - Menu"
$ExeIcon:'./lhg_ccb.ico'
_Icon
_AllowFullScreen

opSysType$ = Left$(_OS$, InStr(2, _OS$, "[") - 1)

If Not _FileExists("DEV.BAS") Then
    Open "DEV.BAS" For Output As #1
    For I = 0 To 1
        Select Case opSysType$
            Case "[WINDOWS]": Print #1, _CWD$ + "\"
            Case "[MAC]", "[MACOSX]", "[LINUX]": Print #1, _CWD$ + "/"
        End Select
    Next
    Close 1
End If

Open "DEV.BAS" For Input As #1
For I = 0 To 1
    Input #1, diskPaths$(I)
Next
Close 1

Selection% = 1

Do

    Call Main_Menu(Selection%)

Loop Until Selection% = 9

End

System


'----------------------------------------

Errhandler:
Open "errlog" For Append As #9
Print #9, "Error #"; Err; "on program file line"; _ErrorLine
If _FileExists("GAME82") Then
    Print #9, "Schedule game # "; Using "###"; AP%(0) + AP%(2)
    Print #9, B$(0) + " vs. " + B$(1)
End If
Print #9,
Close #9
Resume Next ' moves program to code following the error.

'----------------------------------------
'       Main_Menu Subroutine
'----------------------------------------
Sub Main_Menu (menuSelection%)

    ReDim menuChoices$(1 To 9)

    Do

        _Title "Courtside College Basketball - Menu"

        Cls

        Call TITLE

        menuChoices$(1) = "PLAY SINGLE GAME                "
        menuChoices$(2) = "TEAM MANAGEMENT / ORGANIZATION  "
        menuChoices$(3) = "REPLAY STATISTICS AND REPORTS   "
        menuChoices$(4) = "LEADERS / STANDINGS / RECORDS   "
        menuChoices$(5) = "SCHEDULER / AUTOPLAY / BOXSCORES"
        menuChoices$(6) = "TOURNAMENT PLAY                 "
        menuChoices$(7) = "HARD DRIVE INSTALLATION         "
        menuChoices$(8) = "CONFIGURE DRIVES FOR TEAMS,STATS"
        menuChoices$(9) = "EXIT TO DOS                     "

        Selection% = 1
        Count% = 9
        Start% = 1
        ExitCode% = 9
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 8
        Column% = 25

        Call box0(7, 24, 17, 57, 2, 15, 1)
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), menuSelection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then
            Select Case menuSelection%
                Case 1: Call COLHOOP
                Case 2: Call TeamMgt_Menu
                Case 3: Call Replay_Menu
                Case 4: Call Leaders_Menu
                Case 5: Call SCHEDULE
                Case 6: Call ADDTOURN
                Case 7: '
                Case 8: Call ConfigureDrives
                Case 9: Cls
            End Select
        End If

    Loop Until menuSelection% = 9 Or ExitCode% = 27

    If ExitCode% = 27 Then menuSelection% = 9

End Sub


'----------------------------------------
'       ConfigureDrives Subroutine
'----------------------------------------
'...explanation...
Sub ConfigureDrives ()

    Shared opSysType$

    Color 15, 1
    FG = 0
    BG = 7

    For I = 0 To 1
        diskPaths$(I) = Left$(diskPaths$(I), Len(diskPaths$(I)) - 1)
    Next

    Cls

    Locate , 16: Print "SPECIFY PATHS FOR YOUR TEAMS AND COMPILED STATS"
    Print
    Locate , 31: Print "PATHNAME EXAMPLES:"
    Select Case opSysType$
        Case "[WINDOWS]":
            Locate , 31: Print "A:"
            Locate , 31: Print "C:\COLBB\TEAMS"
        Case "[LINUX]":
            Locate , 31: Print "/mnt/floppy"
            Locate , 31: Print "/usr/bin/colbb/teams"
        Case "[MAC]", "[MACOSX]":
            Locate , 31: Print "/Volumes/floppy"
            Locate , 31: Print "/Applications/colbb/teams"
    End Select

    Print
    Locate 7, 1: QPrint "Path for Teams ", 14, 1, -1
    Locate , 16: QPrint PadRight$(diskPaths$(0), 61), FG, BG, -1
    Locate 9, 1: QPrint "Path for Stats ", 14, 1, -1
    Locate , 16: QPrint PadRight$(diskPaths$(1), 61), FG, BG, -1

    GetDiv1:
    Locate 7, 16 'location of beginning of field
    CapsOn = 0
    NumOnly = 0
    Max = 50
    Call TextIn(diskPaths$(0), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data
    diskPaths$(0) = _Trim$(diskPaths$(0))

    GetDiv2:
    Locate 9, 16 'location of beginning of field
    CapsOn = 0
    NumOnly = 0
    Max = 50
    Call TextIn(diskPaths$(1), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data
    diskPaths$(1) = _Trim$(diskPaths$(1))

    Done:
    Locate 23, 1
    Color 14, 1
    Print "Is this okay? (Yes/No) ";
    X$ = ""
    Call YesNoB(X$, XCode, FG, BG)
    Locate , 1
    ClearEOL 14, 1
    If XCode = 1 GoTo GetDiv2
    If X$ = "N" GoTo GetDiv1
    If X$ <> "Y" GoTo Done 'insist on either Y or N or up-arrow

    Open "DEV.BAS" For Output As #1
    For R = 0 To 1:
        Select Case opSysType$
            Case "[WINDOWS]": diskPaths$(R) = diskPaths$(R) + "\"
            Case "[MAC]", "[MACOSX]", "[LINUX]": diskPaths$(R) = diskPaths$(R) + "/"
        End Select
        Print #1, diskPaths$(R)
    Next R

    Close #1

End Sub


'----------------------------------------
'        TeamMgmt_Menu Subroutine
'----------------------------------------
'...explanation...
Sub TeamMgt_Menu ()

    Do
        Call TITLE
        ReDim menuChoices$(1 To 6)
        'Erase menuChoices$
        menuChoices$(1) = "INPUT A NEW TEAM             "
        menuChoices$(2) = "EDIT AN EXISTING TEAM        "
        menuChoices$(3) = "PRINT TEAM ROSTER / REPORT   "
        menuChoices$(4) = "VIEW TEAM ROSTER / REPORT    "
        menuChoices$(5) = "DRAFT TO NEW / EXISTING TEAM "
        menuChoices$(6) = "RETURN TO MAIN MENU          "
        Selection% = 1
        Count% = 6
        Start% = 1
        ExitCode% = 6
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 8
        Column% = 27
        Call box0(7, 26, 14, 57, 2, 15, 1)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0
        If ExitCode% <> 27 Then
            Select Case Selection%
                Case 1: Call CBBINPUT
                Case 2: Call CNGRAT(1)
                Case 3: Call CNGRAT(2)
                Case 4: Call CNGRAT(3)
                Case 5: Call DRAFT
                Case 6: Cls
            End Select
        Else
            Selection% = 6
        End If

    Loop Until Selection% = 6 Or ExitCode% = 27

End Sub


'----------------------------------------
'         Replay_Menu Subroutine
'----------------------------------------
'...explanation...
Sub Replay_Menu ()

    Do
        Cls

        Call TITLE

        ReDim menuChoices$(1 To 5)
        'Erase menuChoices$
        menuChoices$(1) = "VIEW / PRINT COMPILED STATISTICS "
        menuChoices$(2) = "ENCYCLOPEDIA/ TEAM REPLAY OPTIONS"
        menuChoices$(3) = "COMPARE REPLAY STATS TO ACTUAL   "
        menuChoices$(4) = "MERGE SEPARATE STATISTICAL FILES "
        menuChoices$(5) = "RETURN TO COURTSIDE MAIN MENU    "
        Selection% = 1
        Count% = 5
        Start% = 1
        ExitCode% = 5
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 8
        Column% = 25

        Call box0(7, 24, 13, 58, 2, 15, 1)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then
            Select Case Selection%
                Case 1: Call LOOKY
                Case 2: Call CAREER
                Case 3: Call COMPARE
                Case 4: Call MERGE
                Case 5: Cls
            End Select
        End If

    Loop Until Selection% = 5 Or ExitCode% = 27

End Sub


'----------------------------------------
'        Leaders_Menu Subroutine
'----------------------------------------
'...explanation...
Sub Leaders_Menu ()

    Do
        Cls

        Call TITLE
        ReDim menuChoices$(1 To 14)
        'Erase menuChoices$
        menuChoices$(1) = "CREATE/ EDIT A CONFERENCE"
        menuChoices$(2) = "UPDATE STATS/ STANDINGS  "
        menuChoices$(3) = "VIEW/ PRINT REPORTS      "
        menuChoices$(4) = "HEAD-TO-HEAD STATS       "
        menuChoices$(5) = "RECORD BOOKS             "
        menuChoices$(6) = "EXPANDED LEADERS         "
        menuChoices$(7) = "DELETE CONFERENCE STATS  "
        menuChoices$(8) = "CREATE/ EDIT ORGANIZATION"
        menuChoices$(9) = "UPDATE POLL/ STATS       "
        menuChoices$(10) = "VIEW/ PRINT REPORTS      "
        menuChoices$(11) = "RECORD BOOKS             "
        menuChoices$(12) = "DELETE ORGANIZATION STATS"
        menuChoices$(13) = "RETURN TO COURTSIDE MENU "
        menuChoices$(14) = "EXIT TO DOS              "
        Selection% = 1
        Count% = 14
        Start% = 1
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Rows% = 7
        Columns% = 2
        Spacing% = 4
        Row% = 9
        Column% = 12

        Call box0(7, 11, 16, 37, 2, 15, 1)
        Call box0(7, 40, 16, 66, 2, 15, 1)

        Color 15, 0
        Locate 8, 14: Print "COLLEGE CONFERENCE"
        Locate 8, 41: Print "NATIONAL ORGANIZATION"

        Locate 18
        Locate , 10
        Print "COLLEGE CONFERENCE IS USED TO DEFINE THE TEAMS IN A CONFERENCE"
        Print
        Locate , 10
        Print "NATIONAL ORGANIZATION ALLOWS YOU TO SELECT MULTIPLE CONFERENCES"
        Locate , 10
        Print "TO TRACK AND UPDATE TO GENERATE A NATIONAL POLL AND LEADERS."

        _MouseShow "DEFAULT"
        MAMenu menuChoices$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 Then

            Color , 0

            Select Case Selection%
                Case 1: Call CREATCON
                Case 2: Call COMPCON
                Case 3: Call SEECON
                Case 4: Call HD2HDCON
                Case 5: Call SeasonRecords
                Case 6: Call NEWLDR
                Case 7: Call BLANK
                Case 8: Call CREATNAT
                Case 9: Call COMPNAT
                Case 10: Call SEENAT
                Case 11: Call SeasonRecords
                Case 12: Call BLANKORG
                Case 13 Or 14: Cls
            End Select

        End If 'Done checking for ESC

    Loop Until Selection% >= 13 Or ExitCode% = 27

    If Selection% = 14 Then
        End
        System
    End If

End Sub


'INCLUDE: 'FileTeamSelection.bm'
'$INCLUDE: 'KeyInput.bm'
'$INCLUDE: 'QPProEqu.bm'
'$INCLUDE: 'PadRight.bm'

' For Utils
'$INCLUDE: 'TITLE.bm'
'$INCLUDE: 'ADDTOURN.BAS'
'$INCLUDE: 'ALIGN.BAS'
'$INCLUDE: 'BLANK.BAS'
'$INCLUDE: 'BLANKORG.BAS'
'$INCLUDE: 'BRACKET.BAS'
'INCLUDE: 'CAREER.BAS'




Sub CAREER ()

    _Title "Full Count Baseball - Career Files"

    ReDim menuChoices$(1 To 7)

    Do

        Color 15, 0
        Cls

        Call TITLE_CAREER

        menuChoices$(1) = "CREATE AND MERGE INITIAL CAREER STATS"
        menuChoices$(2) = "MERGE / UPDATE CAREER STATS          "
        menuChoices$(3) = "VIEW / PRINT CAREER STATS            "
        menuChoices$(4) = "EDIT CAREER STATS                    "
        menuChoices$(5) = "VIEW / PRINT CAREER RECORDS          "
        menuChoices$(6) = "UPDATE TEAM CAREER LEADERS           "
        menuChoices$(7) = "RETURN TO MAIN MENU                  "
        Selection% = 1
        Count% = 7
        Start% = 1
        ExitCode% = 7
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 8
        Column% = 21

        Call box0(7, 20, 15, 59, 2, 15, 1)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case Selection%

                Case 1:
                    Call CreateCareerFile

                Case 2:
                    Call MergeCareerFiles

                Case 3:
                    Call CARVIEW

                Case 4:
                    Call CAREDIT

                Case 5:
                    Call ViewPrintCareerRecords

                Case 6:
                    Call CARLDR

            End Select

        End If

    Loop Until Selection% = 7 Or ExitCode% = 27

End Sub


'----------------------------------------
'      CreateCareerFile Subroutine
'----------------------------------------
'...explanation...
Sub CreateCareerFile ()

    Cls
    Color 14

    Locate 5, 27: Print " - - - C A U T I O N - - -"

    Do

        Color 15
        Locate 8, 18: Print "THIS OPTION SHOULD ONLY BE USED WHEN CREATING"
        Locate , 18: Print "AND MERGING THE INITIAL STAT FILE.  IF YOU ARE"
        Locate , 18: Print "ADDING AN ADDITIONAL YEAR TO AN EXISTING FILE,"
        Locate , 18: Print "            USE OPTION #2."
        Print
        Print
        Print

        Color 12
        Locate , 25: Print "DO YOU WISH TO CONTINUE? (Y/N)"

        I$ = GetKeyPress$

    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or I$ = Chr$(27)

    If UCase$(I$) = "Y" Then
        Call Merge_FirstYear
    End If 'Done confirming user continue

End Sub

'----------------------------------------
'     MergeCareerFiles Subroutine
'----------------------------------------
'... explain here ...
Sub MergeCareerFiles ()

    Do
        Color 14, 0
        Cls

        Locate 5, 27
        Print "- - - C A U T I O N - - -"

        Color 15
        Locate 8, 1

        Print "THIS OPTION SHOULD ONLY BE USED WHEN ADDING YEARS TO AN EXISTING STAT"
        Print "FILE. IF YOU ARE CREATING A NEW CAREER FILE, USE THE CREATE OPTION."
        Print
        Print "IF YOU ARE ADDING AN ADDITIONAL CAREER YEAR, MAKE SURE THAT ANY UNUSED PLAYER"
        Print "SLOTS (XXX, END, ETC.) ARE AT THE END OF THE SEASON STAT LIST BEFORE YOU MERGE."
        Print
        Print "ALSO MAKE SURE THAT THE PLAYERS NAMES IN THE FILE YOU ARE ADDING, MATCH"
        Print "THE EXACT SPELLING OF THE NAME OF THE SAME PLAYER IN THE CAREER FILE FOR THE"
        Print "PLAYERS THAT HAD PLAYED IN PREVIOUS YEARS.  EXAMPLE - IF BILLY WILLIAMS IS"
        Print "LISTED AS 'WILLIAMS' IN THE CAREER FILE AND THE SEASON STATS HAD HIM AS"
        Print "'B.WILLIAMS', HE SHOULD BE CHANGED TO 'WILLIAMS' IN THE SEASON FILE BEFORE"
        Print "MERGING, OR YOU COULD CHANGE HIM TO 'B.WILLIAMS' IN THE CAREER FILE"
        Print
        Print

        Color 12
        Locate , 25: Print "DO YOU WISH TO CONTINUE? (Y/N)"

        I$ = GetKeyPress$

    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or I$ = Chr$(27)

    If UCase$(I$) = "Y" Then
        Call Merge_AddYear
    End If


End Sub

'----------------------------------------
'      ViewPrintCareerRecords Subroutine
'----------------------------------------
'...explanation...
Sub ViewPrintCareerRecords ()

    Shared BLL!(), PLL!()
    Shared BLYR%(), PLYR%()

    Shared BL$(), careerPL$()

    Do
        'Display list of files...
        Spec$ = diskPaths$(2) + "*.CLD"
        Count% = FCount%(Spec$)

        If Count% = 0 Then
            Locate 10, 8
            Call CSMsgBox("NO CAREER LEADER FILES WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)
            'GoTo 2
        Else

            ReDim careerLeaderFiles$(0 To Count%)

            For X = 1 To Count%
                careerLeaderFiles$(X) = Space$(12)
            Next

            careerLeaderFiles$(0) = Spec$
            Call ReadFile(careerLeaderFiles$())

            Do
                Color , 1
                Cls

                Locate , 28
                Color 14
                Print "SELECT FILE TO VIEW": Color 15
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
                Print "ENTER";: Color 15
                Print "-SELECTS FILE";

                _MouseShow "DEFAULT"
                MAMenu careerLeaderFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
                _MouseHide

                If ExitCode% <> 27 Then

                    Color 15, 1
                    targetFile$ = careerLeaderFiles$(Selection%)
                    Locate 2, 33: Print targetFile$

                    Do
                        Locate 3, 33: Print "ANY CHANGE (YN)";
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                    Color 15, 0

                End If

            Loop Until UCase$(I$) = "N" Or ExitCode% = 27

            If ExitCode% <> 27 Then

                Color 15, 0
                Cls

                targetFile$ = RTrim$(targetFile$)
                ZZ$ = Left$(targetFile$, Len(targetFile$) - 4)

                Open diskPaths$(2) + ZZ$ For Input As #1

                For I = 0 To 14: Input #1, BLYR%(I), BL$(I), BLL!(I): Next

                For I = 0 To 14: Input #1, PLYR%(I), careerPL$(I), PLL!(I): Next

                Close #1

                Do
                    Color 15, 0
                    Cls

                    Do
                        Locate 15, 10
                        Color 12: Print "V";: Color 7: Print "iew or ";
                        Color 12: Print "P";: Color 7: Print "rint"
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "P" Or UCase$(I$) = "V"

                    Select Case UCase$(I$)

                        Case "P":

                            Color 0, 15

                            LPrint "CATEGORY           RECORD           PLAYER              YEAR"
                            LPrint
                            LPrint "AT BATS";: LPrint Tab(23); Using "###"; BLL!(0);: LPrint Tab(37); BL$(0);: LPrint Tab(56); BLYR%(0)
                            LPrint "RUNS SCORED";: LPrint Tab(23); Using "###"; BLL!(1);: LPrint Tab(37); BL$(1);: LPrint Tab(56); BLYR%(1)
                            LPrint "HITS";: LPrint Tab(23); Using "###"; BLL!(2);: LPrint Tab(37); BL$(2);: LPrint Tab(56); BLYR%(2)
                            LPrint "DOUBLES";: LPrint Tab(23); Using "###"; BLL!(3);: LPrint Tab(37); BL$(3);: LPrint Tab(56); BLYR%(3)
                            LPrint "TRIPLES";: LPrint Tab(23); Using "###"; BLL!(4);: LPrint Tab(37); BL$(4);: LPrint Tab(56); BLYR%(4)
                            LPrint "HOME RUNS";: LPrint Tab(23); Using "###"; BLL!(5);: LPrint Tab(37); BL$(5);: LPrint Tab(56); BLYR%(5)
                            LPrint "RUNS BATTED IN";: LPrint Tab(23); Using "###"; BLL!(6);: LPrint Tab(37); BL$(6);: LPrint Tab(56); BLYR%(6)
                            LPrint "WALKS";: LPrint Tab(23); Using "###"; BLL!(7);: LPrint Tab(37); BL$(7);: LPrint Tab(56); BLYR%(7)
                            LPrint "STRIKEOUTS";: LPrint Tab(23); Using "###"; BLL!(8);: LPrint Tab(37); BL$(8);: LPrint Tab(56); BLYR%(8)
                            LPrint "STEALS";: LPrint Tab(23); Using "###"; BLL!(9);: LPrint Tab(37); BL$(9);: LPrint Tab(56); BLYR%(9)
                            LPrint "CAUGHT STEALING";: LPrint Tab(23); Using "###"; BLL!(10);: LPrint Tab(37); BL$(10);: LPrint Tab(56); BLYR%(10)
                            LPrint "ERRORS";: LPrint Tab(23); Using "###"; BLL!(11);: LPrint Tab(37); BL$(11);: LPrint Tab(56); BLYR%(11)

                            LPrint "BATTING AVERAGE";

                            If BLL!(12) > 0 Then
                                LPrint Tab(22); Using ".###"; BLL!(12);: LPrint Tab(37); BL$(12);: LPrint Tab(56); BLYR%(12)
                            Else
                                LPrint Tab(22); " ---"
                            End If

                            LPrint "SLUGGING PCT.";
                            If BLL!(13) > 0 Then
                                LPrint Tab(22); Using "#.###"; BLL!(13);: LPrint Tab(37); BL$(13);: LPrint Tab(56); BLYR%(13)
                            Else
                                LPrint Tab(22); " ---"
                            End If

                            LPrint "ON BASE PCT.";
                            If BLL!(14) > 0 Then
                                LPrint Tab(22); Using "#.###"; BLL!(14);: LPrint Tab(37); BL$(14);: LPrint Tab(56); BLYR%(14)
                            Else
                                LPrint Tab(22); " ---"
                            End If

                            LPrint "WINS";: LPrint Tab(23); Using "###"; PLL!(0);: LPrint Tab(37); careerPL$(0);: LPrint Tab(56); PLYR%(0)
                            LPrint "LOSSES";: LPrint Tab(23); Using "###"; PLL!(1);: LPrint Tab(37); careerPL$(1);: LPrint Tab(56); PLYR%(1)
                            LPrint "SAVES";: LPrint Tab(23); Using "###"; PLL!(2);: LPrint Tab(37); careerPL$(2);: LPrint Tab(56); PLYR%(2)
                            LPrint "INNINGS PITCHED";: LPrint Tab(23); Using "###"; PLL!(3);:

                            If PLL!(3) - (Int(PLL!(3) / 3) * 3) > 0 Then LPrint ".";: LPrint Using "#"; PLL!(3) - (Int(PLL!(3) / 3) * 3);:

                            LPrint Tab(37); careerPL$(3);: LPrint Tab(56); PLYR%(3)
                            LPrint "HITS ALLOWED";: LPrint Tab(23); Using "###"; PLL!(4);: LPrint Tab(37); careerPL$(4);: LPrint Tab(56); PLYR%(4)
                            LPrint "WALKS";: LPrint Tab(23); Using "###"; PLL!(5);: LPrint Tab(37); careerPL$(5);: LPrint Tab(56); PLYR%(5)
                            LPrint "STRIKOUTS";: LPrint Tab(23); Using "###"; PLL!(6);: LPrint Tab(37); careerPL$(6);: LPrint Tab(56); PLYR%(6)

                            LPrint "EARNED RUN AVERAGE";:
                            If PLL!(7) > 0 Then
                                LPrint Tab(22); Using "##.##"; PLL!(7);: LPrint Tab(37); careerPL$(7);: LPrint Tab(56); PLYR%(7)
                            Else
                                LPrint Tab(22); " ---"
                            End If

                            LPrint "GAMES";: LPrint Tab(23); Using "###"; PLL!(8);: LPrint Tab(37); careerPL$(8);: LPrint Tab(56); PLYR%(8)
                            LPrint "COMPLETE GAMES";: LPrint Tab(23); Using "###"; PLL!(9);: LPrint Tab(37); careerPL$(9);: LPrint Tab(56); PLYR%(9)
                            LPrint "RUNS ALLOWED";: LPrint Tab(23); Using "###"; PLL!(10);: LPrint Tab(37); careerPL$(10);: LPrint Tab(56); PLYR%(10)
                            LPrint "EARNED RUNS ALLOWED";: LPrint Tab(23); Using "###"; PLL!(11);: LPrint Tab(37); careerPL$(11);: LPrint Tab(56); PLYR%(11)
                            LPrint "SHUTOUTS";: LPrint Tab(23); Using "###"; PLL!(12);: LPrint Tab(37); careerPL$(12);: LPrint Tab(56); PLYR%(12)
                            LPrint "HOME RUNS ALLOWED";: LPrint Tab(23); Using "###"; PLL!(13);: LPrint Tab(37); careerPL$(13);: LPrint Tab(56); PLYR%(13)
                            LPrint "DOUBLE PLAYS";: LPrint Tab(23); Using "###"; PLL!(14);: LPrint Tab(37); careerPL$(14);: LPrint Tab(56); PLYR%(14)
                            LPrint Chr$(12)

                        Case "V":

                            Do
                                Cls
                                Color 0, 15

                                Color 12: Print "CATEGORY           RECORD           PLAYER              YEAR"
                                Print

                                Color 3: Print "AT BATS";: Color 7: Print Tab(23); Using "###"; BLL!(0);: Print Tab(37); BL$(0);: Print Tab(56); BLYR%(0)
                                Color 3: Print "RUNS SCORED";: Color 7: Print Tab(23); Using "###"; BLL!(1);: Print Tab(37); BL$(1);: Print Tab(56); BLYR%(1)
                                Color 3: Print "HITS";: Color 7: Print Tab(23); Using "###"; BLL!(2);: Print Tab(37); BL$(2);: Print Tab(56); BLYR%(2)
                                Color 3: Print "DOUBLES";: Color 7: Print Tab(23); Using "###"; BLL!(3);: Print Tab(37); BL$(3);: Print Tab(56); BLYR%(3)
                                Color 3: Print "TRIPLES";: Color 7: Print Tab(23); Using "###"; BLL!(4);: Print Tab(37); BL$(4);: Print Tab(56); BLYR%(4)
                                Color 3: Print "HOME RUNS";: Color 7: Print Tab(23); Using "###"; BLL!(5);: Print Tab(37); BL$(5);: Print Tab(56); BLYR%(5)
                                Color 3: Print "RUNS BATTED IN";: Color 7: Print Tab(23); Using "###"; BLL!(6);: Print Tab(37); BL$(6);: Print Tab(56); BLYR%(6)
                                Color 3: Print "WALKS";: Color 7: Print Tab(23); Using "###"; BLL!(7);: Print Tab(37); BL$(7);: Print Tab(56); BLYR%(7)
                                Color 3: Print "STRIKEOUTS";: Color 7: Print Tab(23); Using "###"; BLL!(8);: Print Tab(37); BL$(8);: Print Tab(56); BLYR%(8)
                                Color 3: Print "STEALS";: Color 7: Print Tab(23); Using "###"; BLL!(9);: Print Tab(37); BL$(9);: Print Tab(56); BLYR%(9)
                                Color 3: Print "CAUGHT STEALING";: Color 7: Print Tab(23); Using "###"; BLL!(10);: Print Tab(37); BL$(10);: Print Tab(56); BLYR%(10)
                                Color 3: Print "ERRORS";: Color 7: Print Tab(23); Using "###"; BLL!(11);: Print Tab(37); BL$(11);: Print Tab(56); BLYR%(11)
                                Color 3: Print "BATTING AVERAGE";: Color 7

                                If BLL!(12) > 0 Then
                                    Print Tab(22); Using ".###"; BLL!(12);: Print Tab(37); BL$(12);: Print Tab(56); BLYR%(12)
                                Else
                                    Print Tab(22); " ---"
                                End If

                                Color 3: Print "SLUGGING PCT.";: Color 7:
                                If BLL!(13) > 0 Then
                                    Print Tab(22); Using "#.###"; BLL!(13);: Print Tab(37); BL$(13);: Print Tab(56); BLYR%(13)
                                Else
                                    Print Tab(22); " ---"
                                End If

                                Color 3: Print "ON BASE PCT.";: Color 7:
                                If BLL!(13) > 0 Then
                                    Print Tab(22); Using "#.###"; BLL!(14);: Print Tab(37); BL$(14);: Print Tab(56); BLYR%(14)
                                Else
                                    Print Tab(22); " ---"
                                End If

                                Color 15
                                Locate 22: Print "HIT ANY KEY TO CONTINUE"
                                I$ = GetKeyPress$

                                Cls

                                Color 12: Print "CATEGORY           RECORD           PLAYER              YEAR"

                                Print

                                Color 3: Print "WINS";: Color 7: Print Tab(23); Using "###"; PLL!(0);: Print Tab(37); careerPL$(0);: Print Tab(56); PLYR%(0)
                                Color 3: Print "LOSSES";: Color 7: Print Tab(23); Using "###"; PLL!(1);: Print Tab(37); careerPL$(1);: Print Tab(56); PLYR%(1)
                                Color 3: Print "SAVES";: Color 7: Print Tab(23); Using "###"; PLL!(2);: Print Tab(37); careerPL$(2);: Print Tab(56); PLYR%(2)
                                Color 3: Print "INNINGS PITCHED";: Color 7: Print Tab(23); Using "###"; PLL!(3);:

                                If PLL!(3) - (Int(PLL!(3) / 3) * 3) > 0 Then Print ".";: Print Using "#"; PLL!(3) - (Int(PLL!(3) / 3) * 3);:

                                Print Tab(37); careerPL$(3);: Print Tab(56); PLYR%(3)
                                Color 3: Print "HITS ALLOWED";: Color 7: Print Tab(23); Using "###"; PLL!(4);: Print Tab(37); careerPL$(4);: Print Tab(56); PLYR%(4)
                                Color 3: Print "WALKS";: Color 7: Print Tab(23); Using "###"; PLL!(5);: Print Tab(37); careerPL$(5);: Print Tab(56); PLYR%(5)
                                Color 3: Print "STRIKOUTS";: Color 7: Print Tab(23); Using "###"; PLL!(6);: Print Tab(37); careerPL$(6);: Print Tab(56); PLYR%(6)

                                Color 3: Print "EARNED RUN AVERAGE";: Color 7
                                If PLL!(7) > 0 Then
                                    Print Tab(22); Using "##.##"; PLL!(7);: Print Tab(37); careerPL$(7);: Print Tab(56); PLYR%(7)
                                Else
                                    Print Tab(22); " ---"
                                End If

                                Color 3: Print "GAMES";: Color 7: Print Tab(23); Using "###"; PLL!(8);: Print Tab(37); careerPL$(8);: Print Tab(56); PLYR%(8)
                                Color 3: Print "COMPLETE GAMES";: Color 7: Print Tab(23); Using "###"; PLL!(9);: Print Tab(37); careerPL$(9);: Print Tab(56); PLYR%(9)
                                Color 3: Print "RUNS ALLOWED";: Color 7: Print Tab(23); Using "###"; PLL!(10);: Print Tab(37); careerPL$(10);: Print Tab(56); PLYR%(10)
                                Color 3: Print "EARNED RUNS ALLOWED";: Color 7: Print Tab(23); Using "###"; PLL!(11);: Print Tab(37); careerPL$(11);: Print Tab(56); PLYR%(11)
                                Color 3: Print "SHUTOUTS";: Color 7: Print Tab(23); Using "###"; PLL!(12);: Print Tab(37); careerPL$(12);: Print Tab(56); PLYR%(12)
                                Color 3: Print "HOME RUNS ALLOWED";: Color 7: Print Tab(23); Using "###"; PLL!(13);: Print Tab(37); careerPL$(13);: Print Tab(56); PLYR%(13)
                                Color 3: Print "DOUBLE PLAYS";: Color 7: Print Tab(23); Using "###"; PLL!(14);: Print Tab(37); careerPL$(14);: Print Tab(56); PLYR%(14)

                                Color 15

                                Do
                                    Locate 22: Print "VIEW AGAIN (Y/N)"
                                    I$ = GetKeyPress$
                                Loop Until UCase$(I$) = "N"

                            Loop Until UCase$(I$) = "N"
                            '
                    End Select

                Loop Until UCase$(I$) = "V" Or UCase$(I$) = "P" Or I$ = Chr$(27)

            End If 'Done checking ESC

        End If 'Done checking file count

        '...Until we press ESC
    Loop Until ExitCode% = 27 Or Count% = 0

End Sub

'----------------------------------------
'      Merge_FirstYear Subroutine
'----------------------------------------
'...explanation...
Sub Merge_FirstYear ()

    Shared careerB$(), careerP$()

    Shared careerB0%(), careerB1%(), BRS%()
    Shared careerP0%(), careerP1%(), PRS%()

    'Dim statT%(0 to 21), statT1%(0 to 22)
    Shared statT%(), statT1%()

    Dim TMR%

    TMR% = 4175
    NB = -1
    NP = -1

    Do

        Cls

        For J = 15 To 18:
            Locate J
            Print Space$(68)
        Next

        Color 11
        Locate 15, 5
        Input "ENTER NAME OF CAREER STAT FILE (MAX. - 8 CHARACTERS)"; careerFile$

        careerFile$ = Left$(careerFile$, 8) + ".CAR"

        'Call GetTeam(ti, A$)
        'B$ = A$

        'ti = team index of chosen team
        '0 means no team was chosen
        'If ti <> 0 Then

        For XX = 1 To Len(careerFile)
            If Mid$(careerFile$, XX, 1) = " " Then Mid$(careerFile$, XX, 1) = "_"
        Next XX

        Locate , 5: Input "ENTER NAME OF STAT FILE TO ADD (EX. - WASHINGT.60)"; statFile$

        If Len(statFile$) > 11 Then
            statFile$ = Left$(statFile$, 8) + Right$(statFile$, 3)
        End If

        Locate , 5: Input "ENTER YEAR TO ADD (EX. - 1960)"; statYear

        Do
            Locate 18, 5
            Print "ANY CHANGE (YN)"
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N"

    VV = 1

    Do
        Locate , 5
        Print "IS THIS THE FINAL UPDATE FOR THE CURRENT SEASON? (Y/N)"
        I$ = GetKeyPress$
        finalUpdate$ = UCase$(I$)
    Loop Until finalUpdate$ = "Y" Or finalUpdate$ = "N"

    If finalUpdate$ = "Y" Then finalupdate% = 1 Else finalupdate% = 0

    Locate 20, 5
    Color 15: Print "MERGING CAREER STATS AND UPDATING CAREER LEADER FILE . . . . PLEASE WAIT"

    If Not _FileExists(diskPaths$(1) + statFile$) Then
        Cls
        Color 7: Print "UNABLE TO LOCATE STAT FILE . . . PLEASE RE-ENTER"
        Print
        Line Input "HIT ENTER TO CONTINUE"; DUMMY$
    Else

        Open diskPaths$(1) + statFile$ For Input As #1

        Input #1, A$
        Input #1, W2%
        Input #1, L2%

        For I = 0 To 22:
            Input #1, careerB$(I)
            Input #1, careerB1%(I)
            Locate 21, 35: Color 9
            TMR% = TMR% - 1: Print Using "####"; TMR%
        Next I

        For I = 0 To 21:
            Input #1, careerP$(I)
            Input #1, careerP1%(I)
            Locate 21, 35: Color 9
            TMR% = TMR% - 1: Print Using "####"; TMR%
        Next I

        For I = 0 To 22:
            For I1 = 0 To 21:
                Input #1, careerB0%(I, I1)
                Locate 21, 35: Color 9
                TMR% = TMR% - 1: Print Using "####"; TMR%
            Next I1
        Next I

        For I = 0 To 21:
            For I1 = 0 To 41:
                Input #1, careerP0%(I, I1)
                Locate 21, 35: Color 9
                TMR% = TMR% - 1: Print Using "####"; TMR%
            Next I1
        Next I

        Input #1, D0%
        Input #1, D1%

        For I = 0 To 21:
            Input #1, statT%(I)
        Next I

        For I = 0 To 22:
            Input #1, statT1%(I)
        Next I

        Close #1

        For I = 0 To 99

            N = 1

            If careerB$(I) = "END" Or careerB$(I) = "XXX" Or careerB$(I) = "" Then
                careerB$(I) = ""
            Else
                NB = NB + 1
            End If

            For J = 0 To 15:
                BRS%(N, I, J) = careerB0%(I, J)
            Next J

            BRS%(N, I, 16) = 1
            BRS%(N, I, 17) = statYear

        Next I

        For I = 0 To 79

            If careerP$(I) = "END" Or careerP$(I) = "XXX" Or careerP$(I) = "" Then
                careerP$(I) = ""
            Else
                NP = NP + 1
            End If

            For J = 0 To 14
                PRS%(N, I, J) = careerP0%(I, J)
            Next J

            PRS%(N, I, 15) = 1
            PRS%(N, I, 16) = statYear

        Next I

        If finalupdate% = 1 Then Call UpdateCareerLeaders(careerFile$)

        Locate 20, 5: Color 15: Print "MERGING CAREER STATS AND UPDATING CAREER LEADER FILE . . . . PLEASE WAIT"

        Open diskPaths$(1) + careerFile$ For Output As #1

        Write #1, NB, NP

        For I = 0 To 99:
            Write #1, careerB$(I)
        Next

        For I = 0 To 79:
            Write #1, careerP$(I)
        Next

        For J = 1 To 15:
            For I = 0 To 99:
                For I1 = 0 To 17:
                    Write #1, BRS%(J, I, I1)
                Next I1
                Locate 21, 35: Color 9
                TMR% = TMR% - 1: Print Using "####"; TMR%
            Next I
        Next J

        For J = 1 To 15:
            For I = 0 To 79:
                For I1 = 0 To 16:
                    Write #1, PRS%(J, I, I1)
                Next I1
                Locate 21, 35: Color 9
                TMR% = TMR% - 1: Print Using "####"; TMR%
            Next I
        Next J

        Close #1

    End If 'Done checking for file

End Sub

'----------------------------------------
'      Merge_AddYear Subroutine
'----------------------------------------
'...explanation...
Sub Merge_AddYear ()

    Shared careerB$(), careerP$()

    Shared careerB0%(), careerB1%(), BRS%()
    Shared careerP0%(), careerP1%(), PRS%()

    'Dim statT%(0 to 21), statT1%(0 to 22)
    Shared statT%(), statT1%()

    Dim BR$(100), PR$(80)

    Dim TMR%, TMR1%

    TMR% = 4175
    TMR1% = 6875

    Do
        Color 12, 0
        Cls
        Locate 2, 5

        For J = 15 To 18:
            Locate J: Print Space$(69)
        Next

        Color 11
        Locate 15, 5
        Input "ENTER NAME OF CAREER STAT FILE (EX. - MINNESOT)"; careerFile$

        careerFile$ = Left$(careerFile$, 8) + ".CAR"
        For XX = 1 To Len(careerFile$)
            If Mid$(careerFile$, XX, 1) = " " Then Mid$(careerFile$, XX, 1) = "_"
        Next XX

        Locate , 5: Input "ENTER NAME OF FILE TO ADD (EX. - MINNESOT.61)"; statFile$
        If Len(statFile$) > 11 Then
            statFile$ = Left$(statFile$, 8) + Right$(statFile$, 3)
        End If

        Locate , 5: Input "ENTER YEAR TO ADD (EX. - 1961)"; statYear

        Do
            Locate 18, 5
            Print "ANY CHANGE (YN)"
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N"

    'Where is VV used??
    VV = 0

    Do
        Locate , 5
        Print "IS THIS THE FINAL UPDATE FOR THE CURRENT SEASON? (Y/N)"
        finalUpdate$ = UCase$(I$)
    Loop Until finalUpdate$ = "Y" Or finalUpdate$ = "N"

    If finalUpdate$ = "Y" Then finalupdate% = 1 Else finalupdate% = 0

    Color 15, 0
    Cls
    Locate 20, 5: Print "LOADING CAREER LEADER FILE . . . . PLEASE WAIT"

    If _FileExists(diskPaths$(1) + careerFile$) Then
        Open diskPaths$(1) + careerFile$ For Input As #1
        Input #1, NB, NP
        For I = 0 To 99: Input #1, careerB$(I): Next
        For I = 0 To 79: Input #1, careerP$(I): Next
        For J = 1 To 15: For I = 0 To 99: For I1 = O To 17: Input #1, BRS%(J, I, I1): Next I1: Locate 21, 35: TMR1% = TMR1% - 1: Color 9: Print Using "####"; TMR1%: Next I: Next J
        For J = 1 To 15: For I = 0 To 79: For I1 = 0 To 16: Input #1, PRS%(J, I, I1): Next I1: Locate 21, 35: TMR1% = TMR1% - 1: Color 9: Print Using "####"; TMR1%: Next I: Next J
        Close #1
    End If

    'Color 15, 0
    Locate 20, 5
    Print "LOADING STATS FILE . . . . PLEASE WAIT"
    If Not _FileExists(diskPaths$(1) + statFile$) Then
        Cls
        Color 7: Print "UNABLE TO LOCATE STAT FILE . . . PLEASE RE-ENTER"
        Print
        Line Input "HIT ENTER TO CONTINUE"; DUMMY$
    Else
        Open diskPaths$(1) + statFile$ For Input As #1
        Input #1, A$
        Input #1, W2%
        Input #1, L2%
        For I = 0 To 22: Input #1, BR$(I): Input #1, careerB1%(I): Locate 21, 35: TMR% = TMR% - 1: Color 9: Print Using "####"; TMR%: Next I
        For I = 0 To 21: Input #1, PR$(I): Input #1, careerP1%(I): Locate 21, 35: TMR% = TMR% - 1: Color 9: Print Using "####"; TMR%: Next I
        For I = 0 To 22: For I1 = 0 To 21: Input #1, careerB0%(I, I1): Locate 21, 35: TMR% = TMR% - 1: Color 9: Print Using "####"; TMR%: Next I1: Next I
        For I = 0 To 21: For I1 = 0 To 41: Input #1, careerP0%(I, I1): Locate 21, 35: TMR% = TMR% - 1: Color 9: Print Using "####"; TMR%: Next I1: Next I
        Input #1, D0%
        Input #1, D1%
        For I = 0 To 21: Input #1, statT%(I): Next I
        For I = 0 To 22: Input #1, statT1%(I): Next I
        Close #1

        For I = 0 To 22

            RR = 0
            CHK = 0
            doneLoop% = 0

            For J = 0 To 99

                If UCase$(BR$(I)) = UCase$(careerB$(J)) Then

                    For W = 1 To 15
                        If BRS%(W, J, 17) = statYear Then
                            CHK = CHK + 1
                        End If
                    Next W

                    If CHK > 0 Then
                        N = BRS%(1, J, 16) + BRS%(2, J, 16) + BRS%(3, J, 16) + BRS%(4, J, 16) + BRS%(5, J, 16) + BRS%(6, J, 16) + BRS%(7, J, 16) + BRS%(8, J, 16) + BRS%(9, J, 16) + BRS%(10, J, 16) + BRS%(11, J, 16) + BRS%(12, J, 16) + BRS%(13, J, 16) + BRS%(14, J, 16) + BRS%(15, J, 16)
                    Else
                        N = BRS%(1, J, 16) + BRS%(2, J, 16) + BRS%(3, J, 16) + BRS%(4, J, 16) + BRS%(5, J, 16) + BRS%(6, J, 16) + BRS%(7, J, 16) + BRS%(8, J, 16) + BRS%(9, J, 16) + BRS%(10, J, 16) + BRS%(11, J, 16) + BRS%(12, J, 16) + BRS%(13, J, 16) + BRS%(14, J, 16) + BRS%(15, J, 16) + 1
                    End If

                    For K = 0 To 15: BRS%(N, J, K) = careerB0%(I, K): Next K

                    BRS%(N, J, 16) = 1
                    BRS%(N, J, 17) = statYear
                Else
                    doneLoop% = 1
                End If

                If doneLoop% = 1 Then Exit For

            Next J

            If doneLoop% = 1 Then Exit For

        Next I

        RR = RR + 1

        If RR = 100 Then
            If BR$(I) <> "END" And BR$(I) <> "XXX" And BR$(I) <> "" Then
                NB = NB + 1
                careerB$(NB) = BR$(I)
                N = 1

                For K = 0 To 15:
                    BRS%(N, NB, K) = careerB0%(I, K)
                Next

                BRS%(N, NB, 16) = 1
                BRS%(N, NB, 17) = statYear

            End If

        End If

        For I = 0 To 21

            RRP = 0
            CHK = 0

            For J = 0 To 79

                If UCase$(PR$(I)) = UCase$(careerP$(J)) Then

                    For W = 1 To 15
                        If PRS%(W, J, 16) = statYear Then
                            CHK = CHK + 1
                        End If
                    Next W

                    If CHK > 0 Then
                        N = PRS%(1, J, 15) + PRS%(2, J, 15) + PRS%(3, J, 15) + PRS%(4, J, 15) + PRS%(5, J, 15) + PRS%(6, J, 15) + PRS%(7, J, 15) + PRS%(8, J, 15) + PRS%(9, J, 15) + PRS%(10, J, 15) + PRS%(11, J, 15) + PRS%(12, J, 15) + PRS%(13, J, 15) + PRS%(14, J, 15) + PRS%(15, J, 15)
                    Else
                        N = PRS%(1, J, 15) + PRS%(2, J, 15) + PRS%(3, J, 15) + PRS%(4, J, 15) + PRS%(5, J, 15) + PRS%(6, J, 15) + PRS%(7, J, 15) + PRS%(8, J, 15) + PRS%(9, J, 15) + PRS%(10, J, 15) + PRS%(11, J, 15) + PRS%(12, J, 15) + PRS%(13, J, 15) + PRS%(14, J, 15) + PRS%(15, J, 15) + 1
                    End If

                    For K = 0 To 14:
                        PRS%(N, J, K) = careerP0%(I, K)
                    Next K

                    PRS%(N, J, 15) = 1
                    PRS%(N, J, 16) = statYear

                    Exit For 'GoTo 1530

                Else

                    RRP = RRP + 1

                    If RRP = 80 Then

                        If PR$(I) <> "END" And PR$(I) <> "XXX" And PR$(I) <> "" Then
                            NP = NP + 1
                            careerP$(NP) = PR$(I)
                            N = 1

                            For K = 0 To 14:
                                PRS%(N, NP, K) = careerP0%(I, K)
                            Next

                            PRS%(N, NP, 15) = 1
                            PRS%(N, NP, 16) = statYear
                        End If

                    End If

                End If

            Next J

        Next I

        If finalupdate% = 1 Then
            Call UpdateCareerLeaders(careerFile$)
        End If

        Locate 20, 5: Color 15: Print "MERGING CAREER STATS AND UPDATING CAREER LEADER FILE . . . . PLEASE WAIT"

        Open diskPaths$(1) + careerFile$ For Output As #1

        Write #1, NB, NP

        For I = 0 To 99: Write #1, careerB$(I): Next

        For I = 0 To 79: Write #1, careerP$(I): Next

        For J = 1 To 15:
            For I = 0 To 99:
                For I1 = O To 17:
                    Write #1, BRS%(J, I, I1)
                Next I1
                Locate 21, 35: Color 9
                TMR1% = TMR1% - 1: Print Using "####"; TMR1%
            Next I
        Next J

        For J = 1 To 15:
            For I = 0 To 79:
                For I1 = 0 To 16:
                    Write #1, PRS%(J, I, I1)
                Next I1
                Locate 21, 35: Color 9
                TMR1% = TMR1% - 1: Print Using "####"; TMR1%
            Next I
        Next J

        Close #1

    End If 'Done checking if file exists

End Sub

'----------------------------------------
'     UpdateCareerLeaders Subroutine
'----------------------------------------
'...explanation...
Sub UpdateCareerLeaders (leaderFile$)

    Dim AVG!, SLG!, OBP!, EV!

    Shared BLL!(), PLL!()

    Shared BLYR%(), PLYR%()

    Shared careerB$(), careerP$()
    Shared BL$(), careerPL$()

    Shared BRS%(), PRS%()

    D = 0

    For J = 0 To 8
        D = D + 1
        X$ = Mid$(leaderFile$, D, 1)
        If X$ = "." Then finalupdate% = D - 1
    Next J

    If _FileExists(diskPaths$(2) + Left$(leaderFile$, finalupdate%) + ".CLD") Then

        Open diskPaths$(2) + Left$(leaderFile$, finalupdate%) + ".CLD" For Input As #1

        For I = 0 To 14:
            Input #1, BLYR%(I), BL$(I), BLL!(I)
        Next

        For I = 0 To 14:
            Input #1, PLYR%(I), careerPL$(I), PLL!(I)
        Next

        Close #1

    End If

    'Batters
    For I = 0 To 99

        If careerB$(I) <> "" And careerB$(I) <> "XXX" And careerB$(I) <> "END" Then

            For J = 1 To 15

                If BRS%(J, I, 17) <> 0 Then

                    If BRS%(J, I, 0) > BLL!(0) Then
                        BLL!(0) = BRS%(J, I, 0)
                        BL$(0) = careerB$(I)
                        BLYR%(0) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 2) > BLL!(1) Then
                        BLL!(1) = BRS%(J, I, 2)
                        BL$(1) = careerB$(I)
                        BLYR%(1) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 1) > BLL!(2) Then
                        BLL!(2) = BRS%(J, I, 1)
                        BL$(2) = careerB$(I)
                        BLYR%(2) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 4) > BLL!(3) Then
                        BLL!(3) = BRS%(J, I, 4)
                        BL$(3) = careerB$(I)
                        BLYR%(3) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 5) > BLL!(4) Then
                        BLL!(4) = BRS%(J, I, 5)
                        BL$(4) = careerB$(I)
                        BLYR%(4) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 6) > BLL!(5) Then
                        BLL!(5) = BRS%(J, I, 6)
                        BL$(5) = careerB$(I)
                        BLYR%(5) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 3) > BLL!(6) Then
                        BLL!(6) = BRS%(J, I, 3): BL$(6) = careerB$(I): BLYR%(6) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 7) > BLL!(7) Then
                        BLL!(7) = BRS%(J, I, 7)
                        BL$(7) = careerB$(I)
                        BLYR%(7) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 8) > BLL!(8) Then
                        BLL!(8) = BRS%(J, I, 8)
                        BL$(8) = careerB$(I)
                        BLYR%(8) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 9) > BLL!(9) Then
                        BLL!(9) = BRS%(J, I, 9)
                        BL$(9) = careerB$(I)
                        BLYR%(9) = BRS%(J, I, 17)
                    Else
                        If BRS%(J, I, 10) > BLL!(10) Then
                            BLL!(10) = BRS%(J, I, 10)
                            BL$(10) = careerB$(I)
                            BLYR%(10) = BRS%(J, I, 17)
                        End If
                    End If

                    If BRS%(J, I, 11) > BLL!(11) Then
                        BLL!(11) = BRS%(J, I, 11)
                        BL$(11) = careerB$(I)
                        BLYR%(11) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 0) + BRS%(J, I, 7) > 501 Then
                        AVG! = BRS%(J, I, 1) / BRS%(J, I, 0)

                        If AVG! > BLL!(12) Then
                            BLL!(12) = AVG!
                            BL$(12) = careerB$(I)
                            BLYR%(12) = BRS%(J, I, 17)
                        End If

                    End If

                    If BRS%(J, I, 0) + BRS%(J, I, 7) > 501 Then
                        SLG! = (BRS%(J, I, 1) + BRS%(J, I, 4) + BRS%(J, I, 5) * 2 + BRS%(J, I, 6) * 3) / BRS%(J, I, 0)
                        If SLG! > BLL!(13) Then
                            BLL!(13) = SLG!
                            BL$(13) = careerB$(I)
                            BLYR%(13) = BRS%(J, I, 17)
                        End If
                    End If

                    If BRS%(J, I, 0) + BRS%(J, I, 7) > 501 Then

                        OBP! = (BRS%(J, I, 1) + BRS%(J, I, 7)) / (BRS%(J, I, 7) + BRS%(J, I, 0))

                        test! = BLL!(14)
                        If OBP! > BLL!(14) Then
                            BLL!(14) = OBP!
                            BL$(14) = careerB$(I)
                            BLYR%(14) = BRS%(J, I, 17)
                        End If

                    End If

                End If 'Done checking for non-zero value

            Next J

        End If 'Done checking for terminating characters

    Next I

    'Pitchers
    For I = 0 To 79

        If careerP$(I) <> "" And careerP$(I) <> "XXX" And careerP$(I) <> "END" Then

            For J = 1 To 15

                If PRS%(J, I, 16) <> 0 Then

                    If PRS%(J, I, 6) > PLL!(0) Then
                        PLL!(0) = PRS%(J, I, 6)
                        careerPL$(0) = careerP$(I)
                        PLYR%(0) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 7) > PLL!(1) Then
                        PLL!(1) = PRS%(J, I, 7)
                        careerPL$(1) = careerP$(I)
                        PLYR%(1) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 12) > PLL!(2) Then
                        PLL!(2) = PRS%(J, I, 12)
                        careerPL$(2) = careerP$(I)
                        PLYR%(2) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 0) / 3 > PLL!(3) Then
                        PLL!(3) = PRS%(J, I, 0) / 3
                        careerPL$(3) = careerP$(I)
                        PLYR%(3) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 1) > PLL!(4) Then
                        PLL!(4) = PRS%(J, I, 1)
                        careerPL$(4) = careerP$(I)
                        PLYR%(4) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 4) > PLL!(5) Then
                        PLL!(5) = PRS%(J, I, 4)
                        careerPL$(5) = careerP$(I)
                        PLYR%(5) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 5) > PLL!(6) Then
                        PLL!(6) = PRS%(J, I, 5)
                        careerPL$(6) = careerP$(I)
                        PLYR%(6) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 0) / 3 > 162 Then
                        EV! = (PRS%(J, I, 3) * 9) / (PRS%(J, I, 0) / 3)
                        If PLL!(7) = 0 Then PLL!(7) = 1000
                        If EV! < PLL!(7) Then
                            PLL!(7) = EV!
                            careerPL$(7) = careerP$(I)
                            PLYR%(7) = PRS%(J, I, 16)
                        End If
                    End If

                    If PRS%(J, I, 8) > PLL!(8) Then
                        PLL!(8) = PRS%(J, I, 8)
                        careerPL$(8) = careerP$(I)
                        PLYR%(8) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 10) > PLL!(9) Then
                        PLL!(9) = PRS%(J, I, 10)
                        careerPL$(9) = careerP$(I)
                        PLYR%(9) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 2) > PLL!(10) Then
                        PLL!(10) = PRS%(J, I, 2)
                        careerPL$(10) = careerP$(I)
                        PLYR%(10) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 3) > PLL!(11) Then
                        PLL!(11) = PRS%(J, I, 3)
                        careerPL$(11) = careerP$(I)
                        PLYR%(11) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 11) > PLL!(12) Then
                        PLL!(12) = PRS%(J, I, 11)
                        careerPL$(12) = careerP$(I)
                        PLYR%(12) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 14) > PLL!(13) Then
                        PLL!(13) = PRS%(J, I, 14)
                        careerPL$(13) = careerP$(I)
                        PLYR%(13) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 13) > PLL!(14) Then
                        PLL!(14) = PRS%(J, I, 13)
                        careerPL$(14) = careerP$(I)
                        PLYR%(14) = PRS%(J, I, 16)
                    End If

                End If 'Done checking value of 0

            Next J

        End If 'Done checking for bogus values

    Next I

    D = 0

    For J = 0 To 8
        D = D + 1
        X$ = Mid$(leaderFile$, D, 1)
        If X$ = "." Then finalupdate% = D - 1
    Next J

    Open diskPaths$(2) + Left$(leaderFile$, finalupdate%) + ".CLD" For Output As #1

    For I = 0 To 14:
        Write #1, BLYR%(I), BL$(I), BLL!(I)
    Next

    For I = 0 To 14:
        Write #1, PLYR%(I), careerPL$(I), PLL!(I)
    Next

    Close #1

End Sub




'$INCLUDE: 'CARVIEW.BAS'
'$INCLUDE: 'CBBINPUT.BAS'
'$INCLUDE: 'CNGRAT.BAS'
'$INCLUDE: 'COMPARE.BAS'
'$INCLUDE: 'COMPCON.BAS'
'$INCLUDE: 'COMPNAT.BAS'
'$INCLUDE: 'CREATCON.BAS'
'$INCLUDE: 'CREATNAT.BAS'
'$INCLUDE: 'DRAFT.BAS'
'$INCLUDE: 'HD2HDCON.BAS'
'$INCLUDE: 'LOOKY.BAS'
'$INCLUDE: 'MERGE.BAS'
'$INCLUDE: 'NEWLDR.BAS'
'$INCLUDE: 'REC.BAS'
'$INCLUDE: 'RECORDS.BAS'
'$INCLUDE: 'SCHEDULE.BAS'
'$INCLUDE: 'SEECON.BAS'
'$INCLUDE: 'SEENAT.BAS'
'$INCLUDE: 'SINPUT.BAS'
'$INCLUDE: 'TOURNEY.BAS'

' For Game
'$INCLUDE: 'COLHOOP.BAS'
'$INCLUDE: 'GameRoutines.bm'

