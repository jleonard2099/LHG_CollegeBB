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
'$INCLUDE: 'CAREER.BAS'
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

