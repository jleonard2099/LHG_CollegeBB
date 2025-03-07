$Debug
$Color:32

Const GAME_VERSION$ = "5.1"
Const MAX_SCHEDULE_GAMES = 6000
Const MAX_TOURNAMENT_GAMES = 200
Const MAX_TEAMS = 1000
Const MAX_FILES = 10240
'optimium = 1048576 (but slows code)
Const TEAMS_PER_DIV = 15
Const DIVS_PER_CONFERENCE = 2
Const TEAMS_PER_CONFERENCE = 30
Const MAX_CAREER_YEARS = 15
Const MAX_CONFERENCES = 40

Const MAX_SCHED_STATS = 100
Const SCHEDULE_SIZE_BYTES = 52

DefInt A-Z

' General Sub
'$Include: 'KeyInput.bi'
'$Include: 'QPProEqu.bi'
'$Include: 'PadRight.bi'
'$Include: 'QBWRITE.bi'

' For Utils
'$Include: 'BoxRoutines.bi'
'$Include: 'FileTeamSelection.bi'
'$Include: 'Variables.bi'
'$Include: 'MainRoutines.bi'
'$Include: 'TITLE.bi'

' For Game
'$Include: 'GameRoutines.bi'


On Error GoTo Errhandler


'----------------------------------------
'          READ IN ALL DATA
'----------------------------------------

'Defaults for Event Settings
Data 2,200,100,110,55,40,15,9,6,5,5,0,0
For X = 1 To 13:
    Read eventSettings(X)
Next


Data NAME,G,MIN,MIN/G,FGM,FGA,FG PCT,3FGM,3FGA,3FG PCT,FTM,FTA,FT PCT
Data NAME,PTS,PTG/G,HI,OFF REB,DEF REB,TOT REB,REB/G,PF,AST,BLK,STL,TO
For I = 0 To 25:
    Read statCategoryAbbr$(I)
Next

Data GAMES,MINUTES,FIELD GOALS MADE,FIELD GOAL ATTEMPS,F.G. PCT. (3.65 FGM/GAME),THREE POINTS MADE,THREE POINT ATTEMPTS,3FG (.30 3FGM/GAME),FREE THROWS MADE,FREE THROWS ATTEMPTED,F.T. PCT. (1.52 FTM/GAME)
Data POINTS,SCORING AVERAGE,HI-GAME,OFFENSIVE REBOUNDS,DEFENSIVE REBOUNDS,TOTAL REBOUNDS,AVERAGE REBOUNDS PER GAME,FOULS,ASSISTS,BLOCKED SHOTS,STEALS,TURNOVERS
Data POINTS/48 MINUTES,REBOUNDS/48 MINUTES,FOULS/48 MINUTES,ASSISTS/48 MINUTES,BLOCKS/48 MINUTES,STEALS/48 MINUTES,TURNOVERS/48 MINUTES,MINUTES/GAME,GO TO MAIN MENU
For I = 0 To 31:
    Read expIndCategory$(I)
Next

Data FIELD GOAL ATTEMPTS,FIELD GOALS MADE,F.G. PCT.,THREE POINT ATTEMPTS,THREE POINTS MADE,THREE POINT PERCENTAGE,FREE THROWS ATTEMPTED,FREE THROWS MADE,FREE THROW PERCENTAGE,OFFENSIVE REBOUNDS
Data DEFENSIVE REBOUNDS,TOTAL REBOUNDS,REBOUNDS PER GAME,FOULS,ASSISTS,BLOCKS,STEALS,TURNOVERS,POINTS PER GAME
Data OPP. FIELD GOAL ATTEMPTS,OPP. FIELD GOALS MADE,OPP. FIELD GOAL PERCENTAGE,OPP. THREE POINTERS ATTEMPTED,OPP. THREE POINTERS MADE,OPP. THREE POINT PERCENTAGE,OPP. FREE THROWS ATTEMPTED,OPP. FREE THROWS MADE,OPP. FREE THROW PERCENTAGE
Data OPP. OFFENSIVE REBOUNDS,OPP. DEFENSIVE REBOUNDS,OPP. TOTAL REBOUNDS,OPP. REBOUNDS PER GAME,OPP. FOULS,OPP. ASSISTS,OPP. BLOCKS,OPP. STEALS,OPP. TURNOVERS,OPP. POINTS PER GAME,RETURN TO MAIN MENU
For I = 0 To 38:
    Read expTeamCategory$(I)
Next

Data GAMES PLAYED,FIELD GOALS ATTEMPTED,FIELD GOALS MADE,FIELD GOAL %,3-FIELD GOALS ATTEMPTED,3-FIELD GOALS MADE,3-FIELD GOAL %
Data FREE THROWS ATTEMPTED,FREE THROWS MADE,FREE THROW %,MINUTES PLAYED,POINTS,SCORING AVERAGE,HI-GAME,OFFENSIVE REBOUNDS
Data DEFENSIVE REBOUNDS,TOTAL REBOUNDS,REBOUNDS PER GAME,PERSONAL FOULS,STEALS,ASSISTS,BLOCKS,TURNOVERS,MINUTES PER GAME,MVP VOTING
For I = 1 To 25:
    Read indRecordDesc$(I)
Next

Data FIELD GOALS ATTEMPTED,FIELD GOALS MADE,FIELD GOAL %,3-FIELD GOALS ATTEMPTED,3-FIELD GOALS MADE,3-FIELD GOAL %
Data FREE THROWS ATTEMPTED,FREE THROWS MADE,FREE THROW %,OFFENSIVE REBOUNDS,DEFENSIVE REBOUNDS,TOTAL REBOUNDS,REBOUNDS PER GAME
Data PERSONAL FOULS,STEALS,ASSISTS,BLOCKS,TURNOVERS,POINTS,POINTS PER GAME
For I = 1 To 20:
    Read teamRecordDesc$(I)
Next

Data L,1,1,1,1,1,1,H,H,2,2,2,2,2,F
For I = 0 To 14:
    Read tickerPeriod$(I)
Next

Data 0,1,1,2,2,2,-1,-1,-1,-1,-2,-3
For I = 0 To 1
    For I1 = 1 To 6
        Read homeCtFactor(I, I1)
    Next
Next

'1st index
'0 = Turnover chance
'1 = Steal chance
'2 = Foul chance
'2nd index represents Def Category
'Data 1,4,2,5,15,29,22,36,7,9,7,12
Data 2,5,1,4,15,29,22,36,7,9,3,12
For I = 0 To 2
    For I1 = 1 To 4
        Read defAdjVsPass(I, I1)
    Next
Next

'1st index
'0 = Shot chance
'1 = Assist chance
'2 = Turnover chance
'3 = Steal chance
'4 = Foul chance
'2nd index represents Def Coverage
'Data -7,-10,-4,-10,70,100,20,100,-1,1,-2,2,15,22,8,29,5,7,3,9
Data -7,-10,-4,-10,70,100,20,100,-1,1,-2,2,15,22,8,29,5,9,3,7
For I = 0 To 4
    For I1 = 0 To 3
        Read defAdjVsShot(I, I1)
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
For I = 0 To 9:
    Read offStyles$(I)
Next

Data "SOLID MAN-TO-MAN"
Data "PRESSURE MAN-TO-MAN"
Data "2-3 ZONE (PASSIVE)"
Data "1-3-1 ZONE (TRAPPING)"
Data "FULL COURT PRESS / SOLID MAN-TO-MAN"
Data "FULL COURT PRESS (RUN & JUMP) / SOLID MAN-TO-MAN"
Data "FULL COURT PRESS (RUN & JUMP) / PRESSURE MAN-TO-MAN"
Data "2-2-1 ZONE PRESS (PASSIVE) / 2-3 ZONE"
Data "2-2-1 ZONE PRESS (PASSIVE) / SOLID MAN-TO-MAN"
Data "2-2-1 ZONE PRESS (PASSIVE) / PRESSURE MAN-TO-MAN"
Data "DIAMOND ZONE (TRAPPING) / SOLID MAN-TO-MAN"
Data "DIAMOND ZONE (TRAPPING) / PRESSURE MAN-TO-MAN"
Data "DIAMOND ZONE (TRAPPING) / 2-3 ZONE (PASSIVE)"
Data "DIAMOND ZONE (TRAPPING) / 1-3-1 ZONE (TRAPPING)"
Data "FULL COURT MAN-TO-MAN DENIAL (LAST 3 MIN WHEN DOWN)"
Data "3-2 ZONE (PASSIVE)"
Data "2-2-1 ZONE PRESS (TRAPPING) / 3-2 ZONE (PASSIVE)"
Data "DIAMOND ZONE (TRAPPING) / 3-2 ZONE (PASSIVE)"
For I = 0 To 17
    Read defStyles$(I)
Next I

Data "NORMAL                            "
Data "AGGRESSIVE                        "
Data "SAFE                              "
Data "QUICK OFFENSE (LAST 3 MIN)        "
Data "DELAY OFFENSE (LAST 3 MIN)        "
Data "MILK CLOCK TO 15 SEC (USING S.C.) "
Data "STALL F GOOD SHOT (WITHIN 10 PTS) "
Data "TOTAL STALL (LAST 3 MIN)          "
Data "HOLD VS PASS ZN (AHEAD, NO S.C.)  "
Data "SHOOT ONLY THREES (LAST 3 MIN)    "
For I = 0 To 9
    Read offStyles_brief$(I)
Next

Data "SOLID MAN-TO-MAN                  "
Data "PRESSURE MAN-TO-MAN               "
Data "2-3 ZONE (PSV)                    "
Data "1-3-1 ZONE (TRAP)                 "
Data "FULL COURT / SOLID M-M            "
Data "FULL COURT (R&J) / SOLID M-M      "
Data "FULL COURT (R&J) / PRESSURE M-M   "
Data "2-2-1 ZN PRS (PSV) / 2-3 ZN (PSV) "
Data "2-2-1 ZN PRS (PSV) / SOLID M-M    "
Data "2-2-1 ZN PRS (PSV) / PRESSURE M-M "
Data "DIAMOND ZONE (TRAP) / SOLID M-M   "
Data "DIAMOND ZONE (TRAP) / PRESSURE M-M"
Data "DIAMOND ZONE (TRAP) / 2-3 ZN (PSV)"
Data "DIAMOND ZN / 1-3-1 ZN (TRAP)      "
Data "FULL COURT DENIAL (LAST 3 MIN)    "
Data "3-2 ZONE (PSV)                    "
Data "2-2-1 ZN PRS (PSV) / 3-2 ZN (PSV) "
Data "DIAMOND ZN (TRAP) / 3-2 ZN (PSV)  "
For I = 0 To 17
    Read defStyles_brief$(I)
Next I


' Game options

coachMode$(0) = "COACH AND SELECT SHOTS           "
coachMode$(1) = "COACH ONLY AND LET COMPUTER SHOOT"

foulShotOpt$(0) = "SHOOT 1+1"
foulShotOpt$(1) = "SHOOT 2  "

location$(0) = "@ VISITING TEAM     "
location$(1) = "HOME COURT ADVANTAGE"
location$(2) = "NEUTRAL SITE        "

modeAbbrev$(0) = "HH"
modeAbbrev$(1) = "CV"
modeAbbrev$(2) = "CH"
modeAbbrev$(3) = "CC"

playMode$(0) = "HUMAN OPPONENT           "
playMode$(1) = "COMPUTER IS VISITING TEAM"
playMode$(2) = "COMPUTER IS HOME TEAM    "
playMode$(3) = "COMPUTER VS COMPUTER     "

sClockOpt$(0) = "NO SHOT CLOCK       "
sClockOpt$(1) = "45 SECOND SHOT CLOCK"
sClockOpt$(2) = "35 SECOND SHOT CLOCK"
sClockOpt$(3) = "30 SECOND SHOT CLOCK"

yesNoText$(0) = "NO "
yesNoText$(1) = "YES"

yesNo$(0) = "N"
yesNo$(1) = "Y"


'----------------------------------------
'               MAIN CODE
'----------------------------------------
_Title "Courtside College Basketball - Menu"
$ExeIcon:'./lhg_ccb.ico'
_Icon
_AllowFullScreen

opSysType$ = Left$(_OS$, InStr(2, _OS$, "[") - 1)

'Default new folders to current path
For I = 0 To 3

    Select Case opSysType$
        Case "[WINDOWS]":
            diskPaths$(I) = _CWD$ + "\"
        Case "[MAC]", "[MACOSX]", "[LINUX]":
            diskPaths$(I) = _CWD$ + "/"
    End Select

Next

'Read-in existing DEV.BAS
If _FileExists("DEV.BAS") Then

    Open "DEV.BAS" For Input As #1

    'DEV.BAS will not have the "Schedule" / "Leader" folders configured
    For I = 0 To 1
        Input #1, diskPaths$(I)
    Next

    Close #1

    Kill "DEV.BAS"

End If

'Write FOLDERS.CFG
If Not _FileExists("FOLDERS.CFG") Then

    Open "FOLDERS.CFG" For Output As #2

    For I = 0 To 3
        Print #2, diskPaths$(I)
    Next

    Close #2

End If

'Read in path values
Open "FOLDERS.CFG" For Input As #3

For I = 0 To 3
    Input #3, diskPaths$(I)
Next

Close #3

'Run main menu
SELECTION% = 1

'Screen 12

Do

    Call Main_Menu(SELECTION%)

Loop Until SELECTION% = 8

System


'----------------------------------------

Errhandler:

Open "errlog" For Append As #10
Print #10, "Error #"; Err; "on program file line"; _ErrorLine
If _FileExists("GAME82") Then
    Print #10, "Schedule game # "; Using "###"; schedGame(0) + schedGame(2)
    Print #10, gameTeams$(0) + " vs. " + gameTeams$(1)
End If
Print #10, rebIdx
Close #10
Resume Next ' moves program to code following the error.

'----------------------------------------
'       Main_Menu Subroutine
'----------------------------------------
Sub Main_Menu (menuSelection)

    ReDim menuChoices$(1 To 8)

    Do

        _Title "Courtside College Basketball - Menu"

        Cls

        Call TITLE

        menuChoices$(1) = "PLAY SINGLE GAME       "
        menuChoices$(2) = "TEAM MANAGEMENT        "
        menuChoices$(3) = "REPLAY ASSESSMENT      "
        menuChoices$(4) = "LEADERS AND RECORDS    "
        menuChoices$(5) = "SCHEDULES AND BOXSCORES"
        menuChoices$(6) = "TOURNAMENT PLAY        "
        'menuChoices$(7) = "HARD DRIVE INSTALLATION   "
        menuChoices$(7) = "CONFIGURE GAME FOLDERS "
        menuChoices$(8) = "EXIT PROGRAM           "

        SELECTION% = 1
        Count% = 8
        Start% = 1
        ExitCode% = 8
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), menuSelection, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then
            Select Case menuSelection

                Case 1:
                    Call COLHOOP

                Case 2:
                    Call TeamMgt_Menu

                Case 3:
                    Call Replay_Menu

                Case 4:
                    Call Leaders_Menu

                Case 5:
                    Call SCHEDULE

                Case 6:
                    Call ADDTOURN

                Case 7:
                    Call ConfigureDrives

            End Select
        End If

    Loop Until menuSelection = 8 Or ExitCode% = 27

    If ExitCode% = 27 Then menuSelection = 8

End Sub


'----------------------------------------
'        TeamMgmt_Menu Subroutine
'----------------------------------------
'This subroutine provodes the user with
'several options for managing a team and
'it's roster.
Sub TeamMgt_Menu ()

    Shared opSysType$

    Do

        Call TITLE

        ReDim menuChoices$(1 To 7)

        menuChoices$(1) = "INPUT A NEW TEAM          "
        menuChoices$(2) = "EDIT AN EXISTING TEAM     "
        menuChoices$(3) = "PRINT TEAM ROSTER / REPORT"
        menuChoices$(4) = "VIEW TEAM ROSTER / REPORT "
        menuChoices$(5) = "DRAFT PLAYERS             "
        menuChoices$(6) = "CONVERT DATA FILE FROM 4.0"
        menuChoices$(7) = "RETURN TO MAIN MENU       "
        SELECTION% = 1
        Count% = 7
        Start% = 1
        ExitCode% = 7
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case SELECTION%

                Case 1:
                    Call InputTeam

                Case 2:
                    Call CNGTM(1)

                Case 3:
                    Call CNGTM(2)

                Case 4:
                    Call CNGTM(3)

                Case 5:
                    Call DRAFT

                Case 6:
                    targetFile$ = _OpenFileDialog$("Select team file", diskPaths$(0), "COLBBTMS.*", "Team files")

                    Call ConvertTeam4to5(targetFile$, 0)

            End Select

        Else

            SELECTION% = 7

        End If

    Loop Until SELECTION% = 7 Or ExitCode% = 27

End Sub


'----------------------------------------
'         Replay_Menu Subroutine
'----------------------------------------
'This subroutine provodes the user with
'several options for managing replay
'data, from statistics to reports and
'team careers
Sub Replay_Menu ()

    Do

        Cls

        Call TITLE

        ReDim menuChoices$(1 To 6)

        menuChoices$(1) = "VIEW / PRINT COMPILED STATS     "
        menuChoices$(2) = "TEAM CAREER OPTIONS             "
        menuChoices$(3) = "COMPARE REPLAY STATS TO ACTUAL  "
        menuChoices$(4) = "MODIFY REPLAY STATS             "
        menuChoices$(5) = "MERGE SEPARATE STATISTICAL FILES"
        menuChoices$(6) = "RETURN TO MAIN MENU             "
        SELECTION% = 1
        Count% = 6
        Start% = 1
        ExitCode% = 6
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case SELECTION%

                Case 1:
                    Call LOOKY

                Case 2:
                    Call CAREER

                Case 3:
                    Call COMPARE

                Case 4:
                    Call EditStats

                Case 5
                    Call MERGE

                Case 6:
                    '

            End Select

        End If

    Loop Until SELECTION% = 6 Or ExitCode% = 27

End Sub


'----------------------------------------
'        Leaders_Menu Subroutine
'----------------------------------------
'This subroutine provodes the user with
'several options for managing team
'leaders, stats and standings
Sub Leaders_Menu ()

    Do
        Cls

        Call TITLE

        ReDim menuChoices$(1 To 14)

        menuChoices$(1) = "MANAGE CONFERENCES       "
        menuChoices$(2) = "UPDATE STATS / STANDINGS "
        menuChoices$(3) = "ACCESS CONFERENCE REPORTS"
        menuChoices$(4) = "HEAD-TO-HEAD RESULTS     "
        menuChoices$(5) = "CONFERENCE RECORDS       "
        menuChoices$(6) = "EXPANDED LEADERS         "
        menuChoices$(7) = "DELETE CONFERENCE STATS  "

        menuChoices$(8) = "MANAGE ORGANIZATIONS     "
        menuChoices$(9) = "UPDATE POLL / STATS      "
        menuChoices$(10) = "ACCESS NAT'L. REPORTS    "
        menuChoices$(11) = "NATIONAL RECORDS         "
        menuChoices$(12) = "DELETE NATIONAL STATS    "
        menuChoices$(13) = "RETURN TO MAIN MENU      "
        menuChoices$(14) = "EXIT PROGRAM             "
        SELECTION% = 1
        Count% = 14
        Start% = 1
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Rows% = 7
        Columns% = 2
        Spacing% = 3
        Row% = 9
        Column% = 12

        Call box0(7, 11, 16, 37, 2, 15, 1)
        Call box0(7, 39, 16, 67, 2, 15, 1)

        Color 15, 0
        Locate 8, 14: Print "COLLEGE CONFERENCE"
        Locate 8, 44: Print "NATIONAL ORGANIZATION"

        Locate 18
        Locate , 10
        Print "COLLEGE CONFERENCE IS USED TO DEFINE THE TEAMS IN A CONFERENCE"
        Print
        Locate , 10
        Print "NATIONAL ORGANIZATION ALLOWS YOU TO SELECT MULTIPLE CONFERENCES"
        Locate , 10
        Print "TO TRACK AND UPDATE TO GENERATE A NATIONAL POLL AND LEADERS."

        _MouseShow "DEFAULT"
        MAMenu menuChoices$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Rows%, Columns%, Spacing%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 Then

            Color , 0

            Select Case SELECTION%
                Case 1: Call CREATCON
                Case 2: Call CompileCollege
                Case 3: Call SEECON
                Case 4: Call HD2HDCON
                Case 5: Call Records_Menu
                Case 6: Call NEWLDR
                Case 7: Call BLANKCON
                Case 8: Call CREATENATL
                Case 9: Call CompileNatlOrg
                Case 10: Call SEENATL
                Case 11: Call Records_Menu
                Case 12: Call BLANKORG

            End Select

        End If 'Done checking for ESC

    Loop Until SELECTION% >= 13 Or ExitCode% = 27

    If SELECTION% = 14 Then
        System
    End If

End Sub


' General purpose
'$Include: 'KeyInput.bm'
'$Include: 'QPProEqu.bm'
'$Include: 'PadRight.bm'
'$Include: 'QBWRITE.bm'

' For Utils
'$Include: 'BoxRoutines.bm'
'$Include: 'ConfigureDrives.bm'
'$Include: 'FileTeamSelection.bm'
'$Include: 'CONVERT.BAS'
'$Include: 'TITLE.bm'
'$Include: 'ADDTOURN.BAS'
'$Include: 'ALIGN.BAS'
'$Include: 'BLANKCON.BAS'
'$Include: 'BLANKORG.BAS'
'$Include: 'BRACKET.BAS'
'$Include: 'CAREER.BAS'
'$Include: 'CAREDIT.BAS'
'$Include: 'CARLDR.BAS'
'$Include: 'CARVIEW.BAS'
'$Include: 'CNGTM.BAS'
'$Include: 'COMPARE.BAS'
'$Include: 'COMPCON.BAS'
'$Include: 'COMPNATL.BAS'
'$Include: 'CREATCON.BAS'
'$Include: 'CREATNATL.BAS'
'$Include: 'DRAFT.BAS'
'$Include: 'HD2HDCON.BAS'
'$Include: 'LOOKY.BAS'
'$Include: 'MERGE.BAS'
'$Include: 'NEWLDR.BAS'
'$Include: 'NEWLDRST.BAS'
'$Include: 'REC.BAS'
'$Include: 'RECORDS.BAS'
'$Include: 'SCHEDULE.BAS'
'$Include: 'SEECON.BAS'
'$Include: 'SEENATL.BAS'
'$Include: 'SINPUT.BAS'
'$Include: 'TINPUT.BAS'
'$Include: 'TOURNEY.BAS'

' For Game
'Include: 'COLHOOP.BAS'


'----------------------------------------
'           COLHOOP Subroutine
'----------------------------------------
'This subroutine serves as the main "source"
'for the action in the game. It steps the
'user through choosing game parameters and
'teams, as well as determining players to
'disable, etc.
Sub COLHOOP ()

    _Title "Courtside College Basketball v" + GAME_VERSION$

    Shared actualAttendance&
    Shared avgAttendance&()

    Shared scheduleFile$, tourneyFile$

    Shared tickerStart
    Shared FY%()

    Shared coachMode$(), foulShotOpt$()
    Shared location$()
    Shared playMode$()
    Shared sClockOpt$(), yesNoText$()

    Color 7, 0
    Cls

    Randomize Timer

    Call InitVar

    '----------------------------------------
    '         DETERMINE GAME OPTIONS
    '----------------------------------------

    If Not _FileExists("DEFAULT") Then

        Open "DEFAULT" For Output As #3

        'Player option
        Print #3, 3

        'Three point option
        Print #3, 1

        'Shot clock value
        Print #3, 3

        'Location
        Print #3, 1

        'Fouls to DQ
        Print #3, 5

        Print #3, 1.2

        'Coach / FT Options
        For I = 7 To 10:
            Print #3, 1
        Next

        'PBP Option
        Print #3, 0

        Close #3

    End If

    Open "DEFAULT" For Input As #4
    Input #4, playerOpt
    Input #4, threePtOpt
    Input #4, sClockVal
    Input #4, gameLoc
    Input #4, foulsToDQ
    Input #4, pbpDelay!
    Input #4, coachOpt
    Input #4, ft10thFoul
    Input #4, ft3PtFoul
    Input #4, ft6FloorFouls
    Input #4, pbpOpt
    Close #4

    If _FileExists("TICKER") Then

        Open "TICKER" For Input As #5

        For I = 1 To 32
            For I1 = 0 To 1
                For I2 = 1 To 14
                    Input #5, SX%(I, I1, I2)
                Next
            Next
        Next

        For I = 1 To 32
            For I1 = 0 To 1
                Input #5, SX$(I, I1)
            Next
        Next

        For I = 1 To 32:
            Input #5, ST%(I)
        Next

        Close #5

        tickerGames = 1

    Else

        tickerGames = 0

    End If

    'Load game in-progress, if there is one
    If _FileExists("GAME82") Then

        Open "GAME82" For Input As #6

        Input #6, scheduleFile$

        For I = 0 To 2:
            Input #6, schedGame(I)
        Next

        Close #6

        currGame = schedGame(0) + schedGame(2)

        'Open "pbplog" + Str$(currGame) For Append As #11
        Open "pbplog" For Append As #11
        
        If Not _FileExists(diskPaths$(3) + scheduleFile$) Then

            'Notify user that game won't execute
            MsgText$ = "CAN'T FIND OR LOAD TARGET SCHEDULE.  PRESS ANY KEY TO RETURN TO MENU"
            BoxWidth = 18
            BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
            Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
            Call CSMsgBox(MsgText$, BoxWidth, PASS)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, PASS)

            earlyExit = 1

        Else

            Open diskPaths$(3) + scheduleFile$ For Random As #7 Len = SCHEDULE_SIZE_BYTES

            For X = 0 To 18
                Field #7, X * 2 As X$, 2 As Q$(X + 1), 48 - 2 - 2 * X As X$
            Next

            Field #7, 38 As X$, 2 As Q$(22), 2 As Q$(23), 5 As Q$(20), 5 As Q$(21)

            Get #7, schedGame(0) + schedGame(2)

            For X = 0 To 18:
                schedOptions(X) = CVI(Q$(X + 1))
            Next

            For X = 0 To 1:
                diskIDs$(X) = Q$(20 + X)
            Next

            VS% = CVI(Q$(22))
            HS% = CVI(Q$(23))

            Close #7

            autoPlay = 1

        End If

    Else

        Open "pbplog" For Append As #11

    End If

    If _FileExists("TOURNEY") Then

        Open "TOURNEY" For Input As #8

        Input #8, tourneyFile$

        For I = 0 To 2:
            Input #8, schedGame(I)
        Next

        Close #8

        'Other areas of code, like creating box files,
        'will use scheduleFile$. We would check for
        'tourneyPlay = 1 to distinguish but tourneyPlay = 0 after most
        'of the initial AUTOSETUP process
        scheduleFile$ = tourneyFile$

        'If start game = end game,
        'we're all done!
        If schedGame(2) = schedGame(1) Then
            Kill "TOURNEY"
            earlyExit = 1
        Else

            Open diskPaths$(1) + tourneyFile$ As #9 Len = 94

            For X = 0 To 14
                Field #9, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
            Next

            Field #9, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
            Field #9, 64 As X$, 5 As Q$(20), 5 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
            Field #9, 88 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)

            Get #9, schedGame(0) + schedGame(2)

            VS% = CVI(Q$(16))
            HS% = CVI(Q$(17))
            schedVisTm$ = Q$(18)
            schedHomeTm$ = Q$(19)

            For X = 0 To 14:
                schedOptions(X) = CVI(Q$(X + 1))
            Next

            For X = 0 To 3:
                diskIDs$(X) = Q$(20 + X)
            Next

            For X = 1 To 3:
                schedOptions(X + 14) = CVI(Q$(X + 23))
            Next

            Close #9

            autoPlay = 1
            tourneyPlay = 1

        End If

    End If

    If autoPlay = 1 Then

        Call AUTOSETUP(goodTeam)

        If goodTeam = 0 Then

            'Notify user that game won't execute
            MsgText$ = "ATTEMPTED TO LOAD AN INVALID TEAM. PRESS ANY KEY TO RETURN TO MENU"
            BoxWidth = 18
            BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
            Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
            Call CSMsgBox(MsgText$, BoxWidth, PASS)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, PASS)

            earlyExit = 1

        End If

    End If

    'AUTOSETUP will reset tourneyPlay = 0 in many circumstances
    If autoPlay <> 1 And tourneyPlay <> 1 Then

        ' USER SELECTION OF OPTIONS
        'gameLoc = 1 is neutral site
        If gameLoc = 0 Then gameLoc = 1

        Do

            Color 15, 0
            Cls

            Color 2
            Print Tab(29); "SELECT OPENING OPTIONS"

            Locate 2, 2: Color 14
            Print "(1) ";: Color 15: Print playMode$(playerOpt)
            Locate 3, 2: Color 14
            Print "(2) ";
            Color 15: Print "VISITING TEAM: ";
            Color teamRat_GAME(0, 0), teamRat_GAME(0, 1): Print gameTeams$(0)
            Color , 0
            Locate 4, 2: Color 14, 0
            Print "(3) ";
            Color 15: Print "HOME TEAM: ";
            Color teamRat_GAME(1, 0), teamRat_GAME(1, 1): Print gameTeams$(1)
            Locate 5, 2: Color 14, 0
            Print "(4) ";
            Color 15: Print "GAME LOCATION: "; location$(gameLoc)
            Locate 6, 2: Color 14
            Print "(5) ";
            Color 15: Print "SHOT CLOCK: "; sClockOpt$(sClockVal)
            Locate 7, 2: Color 14
            Print "(6) ";
            Color 15: Print "3-POINT SHOT: "; yesNoText$(threePtOpt)
            Locate 8, 2: Color 14
            Print "(7) ";
            Color 15: Print "FOULS TO DISQUALIFY: "; foulsToDQ
            Locate 9, 2: Color 14
            Print "(8) ";
            Color 15: Print "FREE THROWS ON 10TH FOUL: "; foulShotOpt$(ft10thFoul)
            Locate 10, 2: Color 14
            Print "(9) ";
            Color 15: Print "START GAME"
            Locate 11, 2: Color 14
            Print "(0) ";
            Color 15: Print "SAVE SETTINGS"
            Locate 12, 2: Color 14
            Print "(A) ";
            Color 15: Print Using "PLAY-BY-PLAY DELAY (SECS): ##.#"; pbpDelay!
            Locate 13, 2: Color 14
            Print "(B) ";
            Color 15: Print "HUMAN COACHES: "; coachMode$(coachOpt);
            Locate 1, 1
            Locate 14, 2: Color 14
            Print "(C) ";
            Color 15: Print "3 FT WHEN FOULED ON 3-PT ATT: "; yesNoText$(ft3PtFoul)
            Locate 15, 2: Color 14
            Print "(D) ";
            Color 15: Print "1 FT ON 1ST 6 FLOOR FOULS : "; yesNoText$(ft6FloorFouls)
            Locate 16, 2: Color 14
            Print "(E) ";
            Color 15: Print "PLAY-BY-PLAY STYLE: "; pbpStyle$(pbpOpt)

            'Locate 19, 2: Print "NOTE::"
            'Print "        1 FT ON 1ST 6 FLOOR FOULS THROUGH 1972-73 SEASON"
            'Print "        45 SEC SHOT CLOCK ADOPTED 1984-85, BUT NOT FOR POST-SEASON"
            'Print "        3 PT SHOT ADOPTED IN 1986-87"
            'Print "        2 FT INSTEAD OF 1+1 AFTER 10TH FOUL ADOPTED 1990-91"
            'Print "        3 FT WHEN FOULED ON A 3 PT ATT ADOPTED 1990-91"
            'Print "        35 SEC SHOT CLOCK IN 1993-94; 30 SEC SHOT CLOCK IN 2015-16";

            Locate 18, 2: Color 14
            Print "NOTE:: "
            Color 15

            Locate , 3: Print "* 1 FT ON 1ST 6 FLOOR FOULS THROUGH 1972-73 SEASON"
            'Locate , 3: Print "* 45 SEC SHOT CLOCK ADOPTED 1984-85, BUT NOT IN TOURNAMENTS"
            Locate , 3: Print "* NO SHOT CLOCK UNTIL 1984-1985; 45 SEC CLOCK ADOPTED BUT NOT FOR TOURNAMENT"
            Locate , 3: Print "* 35 SEC SHOT CLOCK IN 1993-94; 30 SEC SHOT CLOCK IN 2015-16"
            Locate , 3: Print "* 3 PT SHOT ADOPTED IN 1986-87"
            Locate , 3: Print "* BEFORE 1990-91, 10TH FOUL ALLOWED 1+1 FT; NOW IT IS 2 FT"
            Locate , 3: Print "* STARTING 1990-91, 3 FT REWARD!ED WHEN FOULED ON A 3 PT ATT";

            Do
                Color 7

                I$ = GetKeyPress$

                Call LetterToNumber(I$, setting)

            Loop Until setting >= 0 And setting <= 14 Or I$ = Chr$(27) Or UCase$(I$) = "Q"

            If I$ <> Chr$(27) And UCase$(I$) <> "Q" Then

                Select Case setting

                    Case 0:
                        Open "DEFAULT" For Output As #10
                        Print #10, playerOpt
                        Print #10, threePtOpt
                        Print #10, sClockVal
                        Print #10, gameLoc
                        Print #10, foulsToDQ
                        Print #10, pbpDelay!
                        Print #10, coachOpt
                        Print #10, ft10thFoul
                        Print #10, ft3PtFoul
                        Print #10, ft6FloorFouls
                        Print #10, pbpOpt
                        Close #10

                    Case 1:
                        playerOpt = playerOpt + 1
                        If playerOpt > 3 Then playerOpt = 0

                    Case 2:
                        Call ChooseGameTeam(diskIDs$(0), numberTeams, tmIdx, gameTeams$(0), 0)
                        If tmIdx <> 0 Then
                            Call TEAMSETUP(0)
                            Call DISABLEPLAYERS(0)
                        End If

                    Case 3:
                        Call ChooseGameTeam(diskIDs$(1), numberTeams, tmIdx, gameTeams$(1), 1)
                        If tmIdx <> 0 Then
                            Call TEAMSETUP(1)
                            Call DISABLEPLAYERS(1)
                        End If

                    Case 4:
                        gameLoc = gameLoc + 1
                        If gameLoc > 2 Then gameLoc = 1

                    Case 5:
                        sClockVal = sClockVal + 1
                        If sClockVal > 3 Then sClockVal = 0

                    Case 6:
                        threePtOpt = 1 - threePtOpt

                    Case 7:
                        foulsToDQ = foulsToDQ + 1
                        If foulsToDQ > 6 Then foulsToDQ = 5

                    Case 8:
                        ft10thFoul = 1 - ft10thFoul

                    Case 9:
                        '

                    Case 10:
                        Do
                            Locate 12, 32
                            Input pbpDelay!
                        Loop Until pbpDelay! >= 0

                    Case 11:
                        coachOpt = 1 - coachOpt

                    Case 12:
                        ft3PtFoul = 1 - ft3PtFoul

                    Case 13:
                        ft6FloorFouls = 1 - ft6FloorFouls

                    Case 14:
                        pbpOpt = 1 - pbpOpt

                End Select

            End If

        Loop Until (setting = 9 And gameTeams$(0) <> "" And gameTeams$(1) <> "") Or I$ = Chr$(27)

        If I$ <> Chr$(27) And UCase$(I$) <> "Q" Then

            'TICKER PROMPT
            Cls
            Color 3

            Call TEAMBOX(7, 1)

            Locate 7, 11

            Print "IF YOU ARE USING THE 10 MINUTE TICKER OPTION FOR THIS GAME,"
            Locate , 11: Print "ENTER STARTING TIME FOR GAME IN THIS FORMAT"
            Locate , 11: Print "(EXAMPLES: 1:00P, 11:30A). OTHERWISE, JUST HIT ENTER"
            Print
            Locate , 11: Input "START TIME: "; startTime$

            L = Len(startTime$)

            If L < 4 Then
                startTime$ = "1:00P"
            Else
                If L = 5 Then
                    HR = Val(Left$(startTime$, 1))
                    MIN = Val(Mid$(startTime$, 3, 2))
                End If
                If L = 6 Then
                    HR = Val(Left$(startTime$, 2))
                    MIN = Val(Mid$(startTime$, 4, 2))
                End If
            End If

            tickerMeridian$ = Right$(startTime$, 1)

            If UCase$(tickerMeridian$) = "P" And Left$(startTime$, 2) <> "12" Then HR = HR + 12

            TM = HR * 60 + MIN
            tickerStart = Int(TM / 30) - 18

        Else

            earlyExit = 1

        End If

    End If

    'If we've run into issues so far...
    If earlyExit = 1 Then Exit Sub

    '----------------------------------------
    '    AUTO-DETERMINE OTHER OPTIONS
    '----------------------------------------

    ' Determine computer team
    Select Case playerOpt

        'Case 0:
        'playerMode = 0
        'compTeam = 0

        Case 1:
            playerMode = 1
            compTeam = 0

        Case 2:
            playerMode = 1
            compTeam = 1

        Case 3:
            playerMode = 2
            'compTeam = 0

    End Select

    'playerMode represents the following values:
    '   0 = Human
    '   1 = Computer Team
    '   2 = PC vs. PC

    For I = 0 To 1:
        'If disk ID has alpha-char, or is only 3 characters,
        'then assume the year is the first 2 chars
        If CheckString%(diskIDs$(I)) = 1 Or Len(diskIDs$(I)) <= 3 Then
            teamYears(I) = GetTeamYear%(Left$(gameTeams$(I), 2))
        Else
            'If we have 4 or more characters,
            'then Val will return the first 4 digits
            teamYears(I) = Val(RTrim$(diskIDs$(I)))
        End If

    Next I

    Call ChangeContributions
    Call LOADSTATS

    If teamRat_GAME(1, 0) = teamRat_GAME(0, 0) And teamRat_GAME(1, 1) = teamRat_GAME(0, 1) Then
        Call ColorChange
    End If

    'Determine attendance
    If gameLoc = 2 Or avgAttendance&(1) = 0 Then
        'Neutral Site
        'Random # 2,000 - 14,000
        '-- Yes, some teams struggle to get 1,500
        '-- but it's rare or an outlier
        '-- Original game ranged 9,000 to 11,000
        actualAttendance& = Int(Rnd(1) * 12000) + 2000
    Else
        'Average attendance +/ 12%
        'College attendance ranges 70% to 95% of max capacity
        margin = Int(avgAttendance&(1) * .12)
        actualAttendance& = avgAttendance&(1) + Int(Rnd(1) * margin * 2) - margin

        'Average attendance +/- 1,000
        'actualAttendance& = avgAttendance&(1) + Int(Rnd(1) * 2000) - 1000
    End If

    For I = 0 To 1
        For I1 = 0 To 13
            playerTime(I, I1) = 1200
        Next
    Next

    Select Case sClockVal
        Case 1: shotClock = 45
        Case 2: shotClock = 35
        Case 3: shotClock = 30
    End Select

    gameAvgTmStamina = (teamStamina(0) + teamStamina(1)) / 2

    For P9 = 0 To 1

        Call SETSTAMINA

        If playerMode = 1 And P9 = compTeam Or playerMode = 2 Then
            Call ComputerCoach
        Else
            Call PlayStyle
            Call PlayItSafe
        End If

        If threePtOpt = 1 Then

            'Adjust foul draw rating based on 3-pt contributions??
            For I = 0 To 13

                M8 = 100 - plyrOff_GAME!(P9, I, 19)

                If M8 <> 0 Then
                    M9 = 100 / M8
                End If

                plyrOff_GAME!(P9, I, 16) = CInt((plyrOff_GAME!(P9, I, 16) * M9))

            Next I

        End If

    Next P9

    Call CheckGameClock
    Call STEALADJ

    If autoPlay <> 1 And playerMode <> 2 Then
        whistle& = _SndOpen("whistle.mp3")
        swish& = _SndOpen("swish.mp3")
        backboard& = _SndOpen("backboard.mp3")
    End If

    'Figure out which team to favor
    'for trends happening over decades.
    If gameLoc = 2 Then
        impactTeam = 0
    Else
        impactTeam = gameLoc
    End If

    250 '
    'Jump Ball !!!
    Select Case currHalf

        Case 1
            jumpBall = Int(Rnd(1) * 2)
            P = jumpBall
            D = 1 - P
        
        Case Is >= 2
            'NCAA used jump balls each period
            'before 1981.
            If teamYears(impactTeam) < 1981 Then
                jumpBall = Int(Rnd(1) * 2)
                P = jumpBall
                D = 1 - P
            End If

    End Select

    Call ChangePossession

    Call SCOREBOARD

    Call LASTSHOT
    Call CheckGameClock
    If gameClock! = 0 Then 170

    Call ResetShotClock

    '----------------------------------------
    '                Tip Off!
    '----------------------------------------

    If (currHalf = 1 Or currHalf = 3) Or teamYears(impactTeam) < 1981 Then
        possArrow = D
        pbpString$ = gameTeams$(P) + " GETS THE JUMP"
    Else
        'currHalf = 2 Or currHalf >= 4
        P = possArrow
        D = 1 - P
        possArrow = 1 - possArrow
        pbpString$ = gameTeams$(P) + " INBOUNDS THE BALL"
    End If

    If (autoPlay = 0 Or pbpDelay! > 0) Then
        Call SELECTPBP(P, 1)
    End If

    GoTo 300

    '----- NOODLE -----

    170 '
    Erase defStrat, defCoverage, defCategory

    gameClock! = 0
    Call QUARTERTOTAL
    currHalf = currHalf + 1
    gameClock! = 1200

    offStatus = 0
    XS = 1

    If currHalf = 2 Then

        pbpString$ = "END OF HALF"

        Select Case pbpOpt

            Case 0:
                Color 7, 0
                Call WINDEX
                Print pbpString$

            Case 1:
                If pbpDelay! > 0 Then
                    Call WINDEX
                    Print pbpString$
                End If

        End Select

        pbpString$ = "HALFTIME!"

        Select Case pbpOpt

            Case 0:
                Color 7, 0
                Call WINDEX
                Print pbpString$

            Case 1:
                Call WINDEX
                Print pbpString$
                Call DELAY

        End Select

        halfTime = 1
        Call BOXOPTIONS(0)
        gameOver = 0
        Call TICKER
        halfTime = 0

        Call FOULHARD

    End If

    If currHalf > 3 Then

        pbpString$ = "END OF OVERTIME" + Str$(currHalf - 3)

        Select Case pbpOpt

            Case 0:
                Color 7, 0
                Print pbpString$

            Case 1:
                If pbpDelay! > 0 Then
                    Call WINDEX
                    Print pbpString$
                End If

        End Select

    End If

    If currHalf < 3 And playerMode <> 0 Then

        If playerMode = 1 Then
            P9 = compTeam
            Call ComputerCoach
        End If

        If playerMode = 2 Then
            P9 = 0
            Call ComputerCoach
            P9 = 1
            Call ComputerCoach
        End If

    End If

    If currHalf < 3 Then

        Erase teamFouls

        Call CheckGameClock
        Call SCOREBOARD

        ballCarrier = -1
        Call DEADBALLOPTIONS

        gameClock! = 1200
        Call UPDATESCREEN

        Erase tmFatigue

        For I = 0 To 1
            For I1 = 0 To 13:
                playerTime(I, I1) = 1200
            Next
        Next

        GoTo 250

    End If

    'Considered doing an ELSE here
    'Would have to be careful with
    'all involved conditionals to
    'make sure we reach end game
    Call CheckGameClock
    Call SCOREBOARD

    If currHalf = 3 Then

        pbpString$ = "END OF REGULATION"

        Select Case pbpOpt

            Case 0:
                Color 7, 0
                Call WINDEX
                Print pbpString$

            Case 1:
                If pbpDelay! > 0 Then
                    Call WINDEX
                    Print pbpString$
                End If

        End Select

    End If

    Locate 2, 31
    Print "END OF REGULATION"

    'If scores are tied, it's Overtime!
    If score(0, 0) = score(1, 0) Then

        pbpString$ = "OVERTIME !!!"

        Select Case pbpOpt
            Case 0:
                Color 7, 0
                Call WINDEX
                Print pbpString$
            Case 1:
                Call WINDEX
                Print pbpString$
                Call DELAY
        End Select

        timeouts(0) = timeouts(0) + 1
        timeouts(1) = timeouts(1) + 1

        gameClock! = 300
        Call UPDATESCREEN

        Erase tmFatigue

        For I = 0 To 1
            For I1 = 0 To 13:
                playerTime(I, I1) = 300
            Next
        Next

        ballCarrier = -1
        Call DEADBALLOPTIONS

        Call SCOREBOARD

        GoTo 250

    End If

    Call GetKeyWhenNoDelay
    Call EndOfGame

    Exit Sub

    '----------------------------------------
    '           END OF GAME!!!
    '----------------------------------------

    '================================================================================
    ' Below are all the different GoTo branches for the gameplay
    ' These have not yet been worked out into a different flow
    ' yet for the game's logic.
    '================================================================================

    TIMEEXP:
    timeElapsed! = gameClock!
    Call AdjustPlayTime
    gameClock! = 0
    Call CheckGameShotClock

    If autoPlay <> 1 And playerMode <> 2 Then
        Call HORN
    End If

    GoTo 170

    '----- NOODLE -----

    300 '
    ' ### LAST SECONDS OF GAME ###

    'Last 5 secs, no timeout, offense is ahead
    If gameClock! <= 5 And timeouts(D) <= 0 And score(P, 0) > score(D, 0) Then

        timeElapsed! = gameClock!
        Call AdjustPlayTime
        gameClock! = 0
        Call CheckGameShotClock

        Call WINDEX
        Print gameTeams$(P); " RUNS THE CLOCK OUT!"
        Call DELAY
        Call DELAY

        If autoPlay <> 1 And playerMode <> 2 Then
            Call HORN
        End If

        GoTo 170

    End If

    scoreDiffOp = score(D, 0) - score(P, 0)

    'Last 5 seconds, down by 1 score!!!
    If gameClock! <= 5 And (threePtOpt = 1 And scoreDiffOp <= 3 Or threePtOpt = 0 And scoreDiffOp <= 2) And scoreDiffOp >= 0 And currHalf >= 2 Then

        If playerMode = 2 Or playerMode = 1 And P = compTeam Then

            ' Computer handle last 5 seconds
            scoreDiffTm = score(P, 0) - score(D, 0)

            If gameClock! >= 3 And gameClock! <= 5 Then

                If threePtOpt = 0 Then
                    If scoreDiffTm < 3 Then
                        I1% = 5
                    End If
                Else
                    'threePtOpt = 1
                    If scoreDiffTm = 3 Then
                        I1% = 4
                    End If
                    If scoreDiffTm = 2 Then
                        If d100% <= 50 Then
                            I1% = 5
                        Else
                            I1% = 4
                        End If
                    End If

                End If

                If gameClock! < 3 Then
                    If threePtOpt = 0 Then
                        If scoreDiffTm < 3 Then
                            I1% = 0
                        End If
                    Else
                        'threePtOpt = 1
                        If scoreDiffTm = 3 Then
                            I1% = 1
                        End If
                        If scoreDiffTm = 2 Then
                            If d100% <= 50 Then
                                I1% = 0
                            Else
                                I1% = 1
                            End If
                        End If
                    End If
                End If

            End If

        Else

            Call LASTFIVEMENU

            280 '
            Do
                I$ = GetKeyPress$
                I1% = Val(I$)
            Loop Until I1% >= 0 And I1% <= 5 And Not (I1% = 3 And timeouts(P9) <= 0) And ((I1% = 1 Or I1% = 4) And threePtOpt = 1)

            Color 15, 0
            Call SCOREBOARD

        End If

        ballCarrier = Int(Rnd(1) * 5)

        Call Rolld100(RN0)

        'Values of I1%
        '0 | 1 = Full Court Pass [Shoot 2 | Shoot 3]
        '2 = Shoot from Back Court
        '3 = TO from Half Court
        '4 | 5 = Work the Floor [Shoot 2 | Shoot 3]

        If I1% = 0 And RN0 <= 50 Or I1% = 1 And RN0 <= 33 Then

            pbpString$ = "LONG PASS DOWN COURT"
            Call PBP(P)
            Call DELAY
            pbpString$ = "IT'S INTERCEPTED...GAME OVER!!"
            Call PBP(P)
            GoTo TIMEEXP

        End If

        If I1% = 2 Then

            rndFtLine = Int(Rnd(1) * 45) + 45

            pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " HAS THE BALL"
            Call PBP(P)

            pbpString$ = "PUTS IT UP FROM" + Str$(rndFtLine) + " FEET"
            Call PBP(P)
            Call DELAY

            If RN0 = 61 Then

                pbpString$ = "GOOD!!"
                Call PBP(P)

                'Reward at least 2 points (depends on if 3-pt shot is supported)
                score(P, currHalf) = score(P, currHalf) + 2
                score(P, 0) = score(P, 0) + 2

                Call CreditAssists

                plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 8) = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 8) + 1
                plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 7) = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 7) + 1

                If threePtOpt = 1 Then

                    'Add the "3rd" point
                    score(P, currHalf) = score(P, currHalf) + 1
                    score(P, 0) = score(P, 0) + 1

                    'Increase 3FGM + 3FGA
                    threeFG(P, lineupIdx(P, ballCarrier), 1) = threeFG(P, lineupIdx(P, ballCarrier), 1) + 1
                    threeFG(P, lineupIdx(P, ballCarrier), 0) = threeFG(P, lineupIdx(P, ballCarrier), 0) + 1

                End If

                Call UPDATESCREEN

            Else

                pbpString$ = "SHOT MISSES...GAME OVER"
                Call PBP(P)

                If autoPlay <> 1 Or playerMode <> 2 And backboard& Then
                    _SndPlay backboard&
                End If

                plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 7) = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 7) + 1

                If threePtOpt = 1 Then
                    threeFG(P, lineupIdx(P, ballCarrier), 1) = threeFG(P, lineupIdx(P, ballCarrier), 1) + 1
                End If

            End If

            GoTo TIMEEXP

        End If

        If I1% >= 3 Then

            Select Case RN0

                Case Is <= 8
                    pbpString$ = "BALL LOST O.B.  GAME OVER"
                    Call PBP(P)
                    GoTo TIMEEXP

                Case 9 To 18
                    pbpString$ = "PASS INTERCEPTED"
                    Call PBP(P)
                    GoTo TIMEEXP

            End Select

        End If

        If I1% = 3 Then

            If gameClock! < 1.5 Then

                pbpString$ = "INBOUND PASS TO " + players$(P, lineupIdx(P, ballCarrier))
                Call PBP(P)

                pbpString$ = "TIME EXPIRES"
                Call PBP(P)

                GoTo TIMEEXP

            Else

                Call AdjustClockTime((Rnd(1) + 1.5))
                Call AdjustPlayTime
                'Call AdjustShotClock

                pbpString$ = gameTeams$(P) + " GETS THE BALL TO HALF COURT"
                Call PBP(P)

                pbpString$ = "THEY QUICKLY CALL FOR A TIMEOUT"
                Call PBP(P)

                timeouts(P) = timeouts(P) - 1

                Call DEADBALLOPTIONS

                Call SCOREBOARD

                Call HalfCourt(I2%)

            End If

        End If

        If I1% >= 4 And gameClock! < 3 Then

            pbpString$ = "INBOUND PASS TO " + players$(P, lineupIdx(P, ballCarrier))
            Call PBP(P)

            pbpString$ = "TIME EXPIRES"
            Call PBP(P)

            GoTo TIMEEXP

        End If

        If I1% = 1 Or I1% = 4 Or (I1% = 3 And I2% = 1) Then
            shootFor3 = 1
        Else
            shootFor3 = 0
        End If

        If shootFor3 = 1 And threePtOpt = 1 Then

            ' Chance of shot based on clock time!!!
            Select Case gameClock!
                Case Is <= .5
                    XX% = 2
                Case .5 To 1.8
                    XX% = 12
                Case Is > 1.8
                    XX% = 20
            End Select

            plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 7) = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 7) + 1

            If d100% > XX% Then

                pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " MISSES"
                Call PBP(P)

                If autoPlay <> 1 Or playerMode <> 2 And backboard& And d100% <= 10 Then
                    _SndPlay backboard&
                End If

                threeFG(P, lineupIdx(P, ballCarrier), 1) = threeFG(P, lineupIdx(P, ballCarrier), 1) + 1

            Else

                pbpString$ = "BASKET BY " + players$(P, lineupIdx(P, ballCarrier)) + "!!"
                Call PBP(P)

                If autoPlay <> 1 Or playerMode <> 2 And swish& Then
                    _SndPlay swish&
                End If

                plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 8) = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 8) + 1

                score(P, currHalf) = score(P, currHalf) + 3
                score(P, 0) = score(P, 0) + 3

                'Increase 3FGM + 3FGA
                threeFG(P, lineupIdx(P, ballCarrier), 1) = threeFG(P, lineupIdx(P, ballCarrier), 1) + 1
                threeFG(P, lineupIdx(P, ballCarrier), 0) = threeFG(P, lineupIdx(P, ballCarrier), 0) + 1

                Call CreditAssists

                Call UPDATESCREEN

            End If

        Else

            'Go for 2!!!
            Select Case gameClock!
                Case Is <= .5
                    XX% = 3
                Case .5 To 1.8
                    XX% = 16
                Case Is > 1.8
                    XX% = 29
            End Select

            plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 7) = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 7) + 1

            If d100% > XX% Then

                pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " MISSES"
                Call PBP(P)

                If autoPlay <> 1 Or playerMode <> 2 And backboard& And d100% <= 10 Then
                    _SndPlay backboard&
                End If

            Else

                pbpString$ = "BASKET BY " + players$(P, lineupIdx(P, ballCarrier)) + "!!"
                Call PBP(P)

                If autoPlay <> 1 Or playerMode <> 2 And swish& Then
                    _SndPlay swish&
                End If

                plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 8) = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 8) + 1

                score(P, currHalf) = score(P, currHalf) + 2
                score(P, 0) = score(P, 0) + 2

                Call CreditAssists

                Call UPDATESCREEN

            End If

        End If

        GoTo TIMEEXP

    End If

    'If we can work the above into a better structure,
    'all branches end the same so we can shortcut this:

    'GoTo TIMEXP

    'Else

    ' ### NORMAL GAME PLAY ###
    LS = 0
    XS = 0

    Select Case sClockVal
        Case 1: shotClock = 45
        Case 2: shotClock = 35
        Case 3: shotClock = 30
    End Select

    LS = 0

    If gameClock! < 6 And LT <> 4 Then
        LS = 4
    End If

    A6 = 0
    assistShotBoost = 0
    D6 = 0
    passes = 1
    playChoice = 0
    shotChance! = 0

    'X will never be 90!!!
    If offStatus <> 90 Then
        offStatus = 0
    End If

    'Pressure Man-to-Man | Trapping Zone AND Single Strategy ONLY
    If (defCoverage(D) = 1 Or defCoverage(D) = 3) And defCategory(D) = 0 Then
        X0% = 50
        X1% = -1
        Call PressFatigueDef
    End If

    'Passive Zone AND Single Strategy ONLY
    If defCoverage(D) = 2 And defCategory(D) = 0 Then
        X0% = 100
        X1% = 1
        Call PressFatigueDef
        Call PressFatigueOff
    End If

    
    'Pressure Man-to-Man AND Single Strategy ONLY
    If defCoverage(D) = 1 And defCategory(D) = 0 Then
        X0% = 25
        X1% = -1
        Call PressFatigueOff
    End If

    GoTo 400

    'End If

    '----- NOODLE -----

    350 '
    A6 = 1
    D6 = 0
    passes = passes + 1

    playChoice = 0
    assistShotBoost = 0
    shotChance! = 0

    offStatus = 0

    'Multi-Strategy Def And 2nd Pass ???
    If defCategory(D) > 0 And passes = 2 Then

        'Pressure Man-to-Man Or Passive Zone
        If (defCoverage(D) = 1 Or defCoverage(D) = 3) Then
            X0% = 50
            X1% = -1
            Call PressFatigueDef
        End If

        'Passive Zone
        If defCoverage(D) = 2 Then
            X0% = 100
            X1% = 1
            Call PressFatigueDef
            Call PressFatigueOff
        End If

        'Pressure Man-to-Man
        If defCoverage(D) = 1 Then
            X0% = 25
            X1% = -1
            Call PressFatigueOff
        End If

    End If

    400 '
    D6 = 0
    P3 = 0
    assistShotBoost = 0

    ballCarrier = Int(Rnd(1) * 5)
    Call BALLDISTRIBUTE

    ' *** BRANCH TO AREA BASED ON DEFENSE ***

    'Multi-Strategy Defense and First Pass
    If defCategory(D) > 0 And passes = 1 Then

        ' Defense is a "combined" strategy

        'It's odd that Case 1, 2 and 4 have identical outcome.
        'That is the code as it was written.
        'It is left as is in case there is a perceived need to adjust.
        'My guess is they were previously different in the past.

        Select Case defCategory(D)

            Case 1:
                'Full Court Press
                X0% = 50
                X1% = -1

            Case 2, 4:
                'Run&Jump, Diamond
                X0% = 50
                X1% = -1

            Case 3:
                '2-2-1
                X0% = 25
                X1% = -1

        End Select

        Call PressFatigueDef

        'Check for Denial / Foul
        If currHalf >= 2 And gameClock! <= 150 And defCategory(D) = 5 Then

            If d100% <= 10 Then

                '10% chance of a break from the inbound
                Call AdjustClockTime(4)
                Call AdjustPlayTime
                'Call AdjustShotClock
                Call WHOLAY

                If passes = 1 Then
                    pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " GETS THE INBOUND PASS"
                Else
                    pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " HAS THE BALL"
                End If

                Call PBP(P)
                pbpString$ = teamGender$(P) + "'S STREAKING DOWN THE FLOOR!"
                Call PBP(P)

                LAYUP:
                fastBreak = 1
                shotChance! = 98
                playChoice = 1

                GoTo 520
                
            End If

            Call Rolld100(RN0)
            Call Rolld100(RN1)

            foulOnPlay = 99
            C1 = 2
            P9 = D

            pbpString$ = "DESIGNATED FOULER (0-4)"
            Call SELECTPBP(D, 1)

            skipInbound% = 0

            If playerMode = 1 And D = compTeam Or playerMode = 2 Then
                desigFouler = Int(Rnd(1) * 5)
            Else
                Do
                    I$ = GetKeyPress$
                    desigFouler = Val(I$)
                Loop Until desigFouler >= 0 And desigFouler <= 4

                If passes > 1 Then skipInbound% = 1

            End If

            If skipInbound% = 0 Then

                Select Case RN1

                    Case 1 To 4

                        pbpString$ = gameTeams$(P) + " CAN'T INBOUND THE BALL"
                        Call SELECTPBP(P, 1)

                        If timeouts(P) > 0 Then

                            pbpString$ = gameTeams$(P) + " CALLS A TIME OUT"
                            Call SELECTPBP(P, 0)

                            passes = 0
                            Call TimeoutCalled(P)

                            If A6 = 0 Then
                                Call PRESSGUARD
                                GoTo 300
                            Else
                                GoTo 350
                            End If

                        Else

                            pbpString$ = "5 SECOND VIOLATION...TURNOVER"
                            shotViolation = 1
                            GoTo 517

                        End If

                    Case 5 To 7
                        pbpString$ = gameTeams$(P) + " CAN'T INBOUND THE BALL"
                        Call SELECTPBP(P, 1)

                        pbpString$ = "5 SECOND VIOLATION...TURNOVER"
                        shotViolation = 1
                        GoTo 517

                End Select


                ' *** CHECK FOR TURNOVER ***

                'NOTE - defAdjVsPass(0, 4) is a value of 5
                'We are not relying on the current DEFENSE???
                CY = 7 + defAdjVsPass(0, 4)

                If RN1 <= CY Then

                    Select Case RN0

                        Case 1 To 80:
                            Call AdjustClockTime(-4)
                            Call AdjustPlayTime
                            'Call AdjustShotClock

                        Case 81 To 90:
                            Call AdjustClockTime(3)
                            Call AdjustPlayTime
                            'Call AdjustShotClock

                    End Select

                    Call LASTSHOT

                    GoTo 517

                End If

                ' *** CHECK FOR STEAL ***

                'Off Steal Rating + Steal Chance
                'We are referencing a fixed index of defAdjVsPass
                'We are not relying on the current DEFENSE
                'So technically fixed values could be put in here
                'However, if research would show them needing changed...
                CY = CY + teamAdj(P, 4) + defAdjVsPass(1, 1)

                If RN1 <= CY Then

                    Select Case RN0

                        Case 1 To 80:
                            Call AdjustClockTime(-4)
                            Call AdjustPlayTime
                            'Call AdjustShotClock

                        Case 81 To 90:
                            Call AdjustClockTime(3)
                            Call AdjustPlayTime
                            'Call AdjustShotClock

                    End Select

                    'Check for steal
                    GoTo 435

                Else
                    432 '
                    pbpString$ = "PASS TO " + players$(P, lineupIdx(P, ballCarrier))
                    Call SELECTPBP(P, 1)

                End If

            End If

            If d100% <= 5 Then
                intentional = 1
            End If

            Call Rolld100(RN1)

            Select Case RN1

                Case 1 To 80:
                    Call AdjustClockTime(Int(Rnd(1) * 3) + 1)

                Case 81 To 90:
                    Call AdjustClockTime(Int(Rnd(1) * 3) + 8)

                Case 91 To 100:
                    Call AdjustClockTime(Int(Rnd(1) * 3) + 5)

            End Select

            Call AdjustPlayTime
            'Call AdjustShotClock

            '80% chance of foul by designated person
            If d100% <= 80 Then
                foulPlayer = desigFouler
                Call FoulCalled(0)
            Else
                Call FoulCalled(1)
            End If

            GoTo 9065

        End If

        If defCategory(D) = 1 Then

            'Full Court Press
            X0% = 25
            X1% = -1
            tmFatigue(P, lineupIdx(P, guardIdx)) = tmFatigue(P, lineupIdx(P, guardIdx)) - 1
            Call PressFatigueOff

        Else

            If defCategory(D) > 1 Then
                'Multi-Strategy Defense
                X0% = 25
                X1% = -1
                Call PressFatigueOff
            End If

            Select Case defCategory(D)

                Case 1, 3
                    Call AdjustClockTime(2)

                Case 2
                    If d100% <= 75 Then
                        Call AdjustClockTime(2)
                    Else
                        Call AdjustClockTime(-2)
                    End If

                Case 4
                    If d100% <= 50 Then
                        Call AdjustClockTime(2)
                    Else
                        Call AdjustClockTime(-2)
                    End If

            End Select

            Call AdjustPlayTime
            'Call AdjustShotClock

            Call Rolld100(RN0)

            If RN0 <= defAdjVsPass(2, defCategory(D)) Then
                'Foul
                Call LASTSHOT
                Call ResetShotClock
                Call UPDATESCREEN

                offStatus = 88
                Call FoulCalled(1)

                LT = 4

                GoTo 9065
            End If

            ' *** CHECK FOR OFF FOUL ***
            If RN0 <= defAdjVsPass(2, defCategory(D)) + 1 Then
                'Offensive Foul
                offStatus = 0
                GoTo 580
            End If

            ' *** CHECK FOR TURNOVER ***

            'Chance of Turnover
            '   Def Foul Chance + Off T/O Rating + Def TO Adj + Def TO Chance - Pressure Rating
            '   5% = fudge factor to get Turnovers where they should be overall
            CY = defAdjVsPass(2, defCategory(D)) + teamAdj(P, 5) + teamAdj(D, 6) + defAdjVsPass(0, defCategory(D)) - pressureRating(D, 0) + 1

            If RN0 <= CY Then

                Call LASTSHOT

                'Turnover
                GoTo 517

            End If

            ' *** CHECK FOR STEAL ***

            'Chance of Steal
            '           Off Steal Rating + Def Steal Chance - Pressure Rating
            CY = CY + teamAdj(P, 4) + defAdjVsPass(1, defCategory(D)) - pressureRating(D, 1)
            pctContrib(D) = pctContrib(D) + 1

            If pctContrib(D) > 3 Then
                pressureRating(D, 0) = 0
                pressureRating(D, 1) = 0
            End If

            If RN0 <= CY Then
                'Steal
                GoTo 435
            End If

        End If

    Else

        Call Rolld100(RN0)

        'If NOT an Offensive Stall OR Defense is not 2-3 Zone
        If (offStrat(P) <> 8 And offStrat(P) <> 7) Or defCoverage(D) <> 2 Then

            ' *** CHECK FOR PERSONAL FOUL ***
            foulChance = defAdjVsShot(4, defCoverage(D))

            'Around 2005, the NCAA  saw a shift towards fewer jump shots.
            'This naturally leads to increased personal fouls.
            'Here, we arbitrarily increase the chance of a foul during
            'these years, but only against Man-to-Man defense.

            'The alternative would be to differentiate between
            'jump shots and layups (which I think can be done)
            If (teamYears(D) >= 2005) And (defCoverage(D) < 2) Then
                foulChance = foulChance + 4
            End If

            If RN0 <= foulChance Then
                Call LASTSHOT
                Call ResetShotClock
                Call UPDATESCREEN

                offStatus = 88
                Call FoulCalled(1)

                LT = 4

                GoTo 9065
            End If

            ' *** CHECK FOR OFF FOUL ***

            If RN0 <= foulChance + 1 Then
                'Offensive Foul
                offStatus = 0
                GoTo 580
            End If

            ' *** CHECK FOR TURNOVER ***

            If offStrat(P) = 4 Or offStrat(P) = 7 Then
                'Delay / Stall
                'OY% is -(defAdjVsShot/2)
                'So, either +-1 or +-2
                OY% = CInt((defAdjVsShot(2, defCoverage(D)) / 2)) * -1
            Else
                OY% = 0
            End If

            'Chance of Turnover
            '   Def Foul Chance + Off TO Rating + Def TO Adj + Def TO Chance + OY%
            '--> 95+% of turnovers get calculated from here
            CY = defAdjVsShot(4, defCoverage(D)) + teamAdj(P, 5) + teamAdj(D, 6) + defAdjVsShot(2, defCoverage(D)) + OY% + 3

            If RN0 <= CY Then

                Call LASTSHOT

                GoTo 517

            End If


            ' *** CHECK FOR STEAL ***

            'Chance of Steal (based on Turnover)
            '         Off Steal Rating + Def Steal Chance
            CY = CY + teamAdj(P, 4) + defAdjVsShot(3, defCoverage(D))

            Select Case offStrat(P)

                Case 0, 3, 5, 6, 8, 9
                    'Normal, Quick, Milk Clock, Stall, Hold, Shoot Three
                    OY% = 0

                Case 1
                    'Aggressive
                    OY% = 7

                Case 2
                    'Safe
                    OY% = -7

                Case 4, 7
                    'Delay, Total Stall
                    OY% = CInt((defAdjVsShot(3, defCoverage(D)) / 2)) * -1

            End Select

            If RN0 <= CY + OY% Then
                'Check for steal
                GoTo 435
            End If

        End If

    End If

    'Is this when we actually pass the ball ???


    '-----

    465 '
    'Adjust likelihood of taking a shot based on defense
    GS% = ShotPacing%

    'Pressure Man-to-Man | 2-3 Zone
    If defCoverage(D) = 1 Or defCoverage(D) = 2 Then
        GS% = GS% + 7
    End If

    'Trapping Zone
    If defCoverage(D) = 3 Then
        GS% = GS% - 7
    End If

    'Quick Off
    If offStrat(P) = 3 Then
        GS% = GS% + 21
    End If

    'Delay Off
    If offStrat(P) = 4 Then
        GS% = GS% - 21
    End If

    'Total Stall | Hold Ball | Milk Clock and 15+ Secs on Shot Clock
    'Shot Clock + Difficult Shot | No Shot Clock + Difficult Shot OR 25% of the time
    'If offStrat(P) = 7 Or offStrat(P) = 8 Or (sClockVal >= 1 And offStrat(P) = 5 And shotClock > 15) Or (d100% > GS%) Then
    If offStrat(P) = 7 Or offStrat(P) = 8 Or (sClockVal >= 1 And offStrat(P) = 5 And shotClock > 15) Or (sClockVal >= 1 And d100% > GS%) Or ((sClockVal = 0 And d100% > GS% And d100% <= 80)) Then

        'No Shot / Difficult Shot
        GoTo 437

    Else

        If d100% <= defAdjVsShot(1, defCoverage(D)) Then

            assistShotBoost = 0

            If Not (currHalf = 2 And gameClock! <= 120 Or currHalf > 2) Then

                If currHalf <= 2 Then
                    If playerFatigue%(P, lineupIdx(P, ballCarrier)) <= 0 Then
                        'Fatigued - No Shot / Difficult Shot
                        GoTo 437
                    End If
                End If

            End If

            Call SHOTPERCENT

            rndCarrier = Int(Rnd(1) * 5)
            assistShotBoost = Int(Rnd(1) * 28) + 1
            assistChance = Int(Rnd(1) * 9) + 1

            ' *** CHECK ASSIST SHOT ***
            If tmFatigue(P, lineupIdx(P, ballCarrier)) > -5 Then

                'Player is not fatigued - check assist rating
                If assistChance <= plyrOff_GAME!(P, lineupIdx(P, rndCarrier), 4) Then

                    'increase shot chance for assist
                    shotChance! = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 0) + dFGPA(D) + assistShotBoost + ft3PtFoul(P) + ft3PtFoul(D)
                    ft3PtFoul(P) = 0
                    ft3PtFoul(D) = 0
                    shotType = 2

                    offStatus = 0

                    If d100% <= 50 Then
                        shotType = 3
                    End If

                    GoTo 520

                End If

            End If

        End If


        ' *** CHECK AVERAGE SHOT ***
        'Delay Offense
        If offStrat(P) = 4 Then

            'No Shot / Difficult Shot
            GoTo 437

        Else

            If currHalf <= 2 Then

                If Not (currHalf = 2 And gameClock! <= 120 Or currHalf > 2) Then
                    If playerFatigue%(P, lineupIdx(P, ballCarrier)) <= 0 Then
                        'No Shot / Difficult Shot
                        GoTo 437
                    End If
                End If

            End If

        End If

        playChoice = 1
        Call SHOTPERCENT

        offStatus = 0

        shotType = Int(Rnd(1) * 3) + 5

        If J8% = 1 Then
            shotType = 4
        End If

        GoTo 520

    End If


    '----- NOODLE -----

    ' ***** CHECK FOR STEALS *****
    435 '
    'Random ball carrier (0-4)
    rndCarrier = Int(Rnd(1) * 5)

    'Compare random # 1-4 against that player's steal rating
    '(basically, average steals / game)
    stealRating = plyrOff_GAME!(D, rndCarrier, 5)
    RNX = Int(Rnd(1) * 4) + 1

    If RNX <= (stealRating) Then

        'Successful steal!
        plyrOff_GAME!(D, lineupIdx(D, rndCarrier), 18) = plyrOff_GAME!(D, lineupIdx(D, rndCarrier), 18) + 1
        plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 23) = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 23) + 1

        If defCategory(D) <= 0 Or passes <> 1 Then
            Call LASTSHOT
        End If

        Call ResetShotClock

        If (autoPlay = 0 Or pbpDelay! > 0) Then

            pbpString$ = "BALL STOLEN BY " + players$(D, lineupIdx(D, rndCarrier)) + "!"

            If pbpOpt = 1 Then
                Call WINDEX
                Call PBP(D)
                Call DELAY
            Else

                Call Rolld100(RN0)

                Select Case RN0
                    Case 1 To 3
                        pbpString$ = players$(D, lineupIdx(D, rndCarrier)) + " QUICKLY REACHES IN!"
                        Call PBP(D)
                    Case 4 To 6
                        pbpString$ = players$(D, lineupIdx(D, rndCarrier)) + " SNEAKS IN FROM BEHIND!"
                        Call PBP(D)
                    Case 7 To 10
                        pbpString$ = players$(D, lineupIdx(D, rndCarrier)) + " GETS A HAND ON THE BALL!"
                        Call PBP(D)
                        'No case for 11 to 100
                End Select

                RN1 = Int(Rnd(1) * 20) + 1

                Select Case RN1

                    Case 1
                        pbpString$ = "PASS INTERCEPTED BY " + players$(D, lineupIdx(D, rndCarrier)) + "!"
                    Case 2
                        pbpString$ = "PASS IS PICKED OFF BY " + players$(D, lineupIdx(D, rndCarrier)) + "!"
                    Case 3
                        pbpString$ = "BALL IS STOLEN BY " + players$(D, lineupIdx(D, rndCarrier)) + "!"
                    Case 4
                        pbpString$ = players$(D, lineupIdx(D, rndCarrier)) + " WITH THE STEAL!"
                    Case 5
                        pbpString$ = players$(D, lineupIdx(D, rndCarrier)) + " TAKES IT AWAY!"
                    Case 6
                        pbpString$ = players$(D, lineupIdx(D, rndCarrier)) + " STEALS IT!"
                    Case 7
                        pbpString$ = players$(D, lineupIdx(D, rndCarrier)) + " RIPS IT LOOSE!"
                    Case 8
                        pbpString$ = players$(D, lineupIdx(D, rndCarrier)) + " TAKES THE BALL AWAY!"
                    Case 9
                        pbpString$ = players$(D, lineupIdx(D, rndCarrier)) + " STEALS THE BALL!"
                    Case 10
                        pbpString$ = players$(D, lineupIdx(D, rndCarrier)) + " PICKS IT OFF!"
                    Case 11
                        pbpString$ = players$(D, lineupIdx(D, rndCarrier)) + " SWIPES THE BALL!"
                    Case 12
                        pbpString$ = players$(D, lineupIdx(D, rndCarrier)) + " WITH THE STOLEN BALL!"
                    Case 13
                        pbpString$ = players$(D, lineupIdx(D, rndCarrier)) + " STRIPS IT AWAY!"
                    Case 14
                        pbpString$ = players$(D, lineupIdx(D, rndCarrier)) + " WITH THE STEAL!"
                    Case 15
                        pbpString$ = players$(D, lineupIdx(D, rndCarrier)) + " SWIPES AND STEALS IT!"
                    Case 16
                        pbpString$ = players$(D, lineupIdx(D, rndCarrier)) + " SNATCHES IT AWAY!"
                    Case 17
                        pbpString$ = players$(D, lineupIdx(D, rndCarrier)) + " QUICKLY REACHES IN!"
                    Case 18
                        pbpString$ = players$(D, lineupIdx(D, rndCarrier)) + " TAKES IT THE OTHER WAY!"
                    Case 19
                        pbpString$ = players$(D, lineupIdx(D, rndCarrier)) + " POKES IT LOOSE!"
                    Case 20
                        pbpString$ = players$(D, lineupIdx(D, rndCarrier)) + " STRIPS IT CLEAN!"

                End Select

                Call PBP(D)

            End If

        End If

        plyrOff_GAME!(D, lineupIdx(D, rndCarrier), 13) = plyrOff_GAME!(D, lineupIdx(D, rndCarrier), 13) + 1
        turnovers(P) = turnovers(P) + 1

        Call UPDATESCREEN

        Call ChangePossession
        P9 = D

        If gameClock! <= 0 Then 170

        fastBreak = 1

        Call WHOLAY

        Call Rolld100(RN1)

        If passes > 1 Then

            Select Case defCoverage(P9)

                Case 2
                    'Passive Zone
                    If RN1 <= 5 Then GoTo LAYUP

                Case 0, 1, 3
                    'Everything Else
                    If RN1 <= 15 Then GoTo LAYUP

            End Select

        Else

            'Shooting, Multi-Strategy Defense
            If defCategory(P) > 0 Then

                If RN1 <= 33 Or (defCategory(P) <> 3 And RN1 <= 40) Then
                    GoTo LAYUP
                End If

            End If

        End If

        GoTo 300

    Else

        'No steal - layup or take a shot!
        Call Rolld100(RN0)

        Select Case defCategory(D)

            Case 1
                If passes = 1 Then
                    If RN0 <= plyrOff_GAME!(P, lineupIdx(P, guardNbr), 4) Then GoTo LAYUP
                    If RN0 <= plyrOff_GAME!(P, lineupIdx(P, guardNbr), 4) * 2 Then GoTo GDSHOT
                End If

            Case 2
                If passes = 1 Then
                    If RN0 <= plyrOff_GAME!(P, lineupIdx(P, guardNbr), 4) * 2 Then GoTo LAYUP
                    If RN0 <= plyrOff_GAME!(P, lineupIdx(P, guardNbr), 4) * 4 Then GoTo GDSHOT
                End If

            Case 3
                If passes = 1 Then
                    If RN0 <= 7 Then GoTo LAYUP
                    If RN0 <= 14 Then GoTo GDSHOT
                End If

            Case 4
                If passes = 1 Then
                    If RN0 <= 15 Then GoTo LAYUP
                    If RN0 <= 30 Then GoTo GDSHOT
                End If

            Case 5
                GoTo 432

        End Select

    End If


    ' ***BAD SHOT ***

    437 '
    J8% = 0

    playChoice = 0
    shotType = 1

    threePtShotChance = Int(Rnd(1) * 7) + 3

    RN0 = Int(Rnd(1) * 3) + 1

    'Adjust shot % based on player's FG%
    If plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 0) > 60 Then

        Select Case RN0
            Case 1
                shotChance! = 24
            Case 2
                shotChance! = 30
            Case 3
                shotChance! = 36
        End Select

    Else

        Select Case RN0
            Case 1
                shotChance! = CInt(plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 0) * .4)
            Case 2
                shotChance! = CInt(plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 0) * .5)
            Case 3
                shotChance! = CInt(plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 0) * .6)
        End Select

    End If

    If Int(Rnd(1) * 150) + 1 = 40 Then

        '0.66% chance of TIEUP
        GoTo TIEUP

    Else

        offStatus = 0

        If shotType = 2 And d100% <= 50 Then
            shotType = 3
            playChoice = 1
        End If

        GoTo 520

    End If

    '----- NOODLE -----

    517 '
    Call ResetShotClock

    RN0 = Int(Rnd(1) * 25) + 1

    If RN0 = 13 Then

        GoTo TIEUP

    Else

        'Increase player and team TO's
        plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 23) = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 23) + 1
        turnovers(P) = turnovers(P) + 1

        If shotViolation = 0 Then
            pbpType = 1
            Call MAKEPBP(pbpType)
        End If

        shotViolation = 0

        pbpString$ = "TURNOVER BY " + gameTeams$(P)
        Call SELECTPBP(P, 1)

        pbpString$ = "BALL OUT OF BOUNDS TO " + gameTeams$(D)
        Call SELECTPBP(D, 0)
        Call DELAY

        Call UPDATESCREEN

        Locate 10, 1

        If gameClock! <= 0 Then 170

        Call ChangePossession

        Call DEADBALLOPTIONS

        For I = 0 To 1
            If defCategory(I) = 0 Then
                pressureRating(I, 0) = 1
                pressureRating(I, 1) = 7
                pctContrib(I) = 0
            End If
        Next

        GoTo 300

    End If

    '----- NOODLE -----

    GDSHOT:
    baseShotChance = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 0)

    If tmFatigue(P, lineupIdx(P, ballCarrier)) <= -5 Then
        baseShotChance = baseShotChance - 10
    End If

    shotChance! = baseShotChance + Int(Rnd(20)) + 11 + ft3PtFoul(D) + ft3PtFoul(P)
    ft3PtFoul(P) = 0
    ft3PtFoul(D) = 0

    offStatus = 0

    shotType = Int(Rnd(1) * 3) + 5

    520 '
    If J8% = 1 Then
        shotType = 4
    End If

    Call LASTSHOT

    If gameClock! <= 0 Then 170

    If shotType = 4 And threePtShotChance <= 0 Then
        threePtShotChance = 3
    End If

    If (autoPlay = 0 Or pbpDelay! > 0) Then

        If shotType = 10 Then

            GDSHOTDES:
            If (autoPlay = 0 Or pbpDelay! > 0) Then

                Call Rolld100(RN0)

                Select Case RN0

                    Case Is <= 25
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " BREAKS IN TO FRONT COURT"

                    Case 25 To 50
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " GETS THE PASS IN FRONT COURT"

                    Case 51 To 75
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " BREAKS THE PRESS"

                    Case 76 To 100
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " DEFEATS THE PRESS"

                End Select

                Call PBP(P)

                Call Rolld100(RN1)

                Select Case RN1

                    Case Is <= 33
                        pbpString$ = "HAS AN OPEN JUMPER AT THE FT LINE"

                    Case 34 To 66
                        pbpString$ = teamGender$(P) + "'S OPEN ON THE BASELINE"

                    Case 66 To 100
                        pbpString$ = teamGender$(P) + "'S OPEN FROM THE WING"

                End Select

                Call SHOTSTRING
                Call PBP(P)

            End If

        Else

            If shotChance! > 70 And fastBreak = 1 And passes = 1 Then

                pbpType = 6
                shotType = 8
                Call MAKEPBP(pbpType)
                Call PBP(P)

            Else

                If players$(P, lineupIdx(P, ballCarrier)) <> prevBall$ Then
                    If prevBall$ = "" Then
                        pbpString$ = "PASS TO " + players$(P, lineupIdx(P, ballCarrier))
                        Call PBP(P)
                    Else
                        pbpType = 10
                        Call MAKEPBP(pbpType)
                        Call PBP(P)
                    End If
                Else
                    pbpType = 9
                    Call MAKEPBP(pbpType)
                    Call PBP(P)
                End If

                If shotClock <= 0 And sClockVal > 0 Then

                    pbpString$ = "SHOT CLOCK HAS EXPIRED!"
                    shotViolation = 1

                    Call LASTSHOT

                    GoTo 517

                End If

                prevBall$ = players$(P, lineupIdx(P, ballCarrier))

                Select Case shotType

                    Case 1:

                        If (autoPlay = 0 Or pbpDelay! > 0) Then

                            RN0 = Int(Rnd(1) * 12) + 1

                            Select Case RN0

                                Case 1
                                    pbpString$ = teamGender$(P) + "'S HEAVILY GUARDED"
                                Case 2
                                    pbpString$ = teamGender$(P) + "'S GUARDED"
                                Case 3
                                    pbpString$ = teamGender$(P) + "'S GUARDED BY " + players$(D, lineupIdx(D, ballCarrier))

                                Case 4, 5
                                    pbpString$ = teamGender$(P) + " HAS A DEFENDER ON "
                                    If teamGender$(P) = "HE" Then
                                        pbpString$ = pbpString$ + "HIM"
                                    Else
                                        pbpString$ = pbpString$ + "HER"
                                    End If

                                Case 6
                                    pbpString$ = teamGender$(P) + "'S COVERED"
                                Case 7
                                    pbpString$ = teamGender$(P) + " CAN'T GET AROUND THE DEFENDER"
                                Case 8
                                    pbpString$ = teamGender$(P) + "'S PRESSURED"
                                Case 9
                                    pbpString$ = teamGender$(P) + " HAS NOWHERE TO GO"
                                Case 10
                                    pbpString$ = teamGender$(P) + " HAS A HAND IN "
                                    If teamGender$(P) = "HE" Then
                                        pbpString$ = pbpString$ + "HIS FACE"
                                    Else
                                        pbpString$ = pbpString$ + "HER FACE"
                                    End If

                                Case 11
                                    pbpString$ = teamGender$(P) + "'S WATCHED BY " + players$(D, lineupIdx(D, ballCarrier))
                                Case 12
                                    pbpString$ = teamGender$(P) + "'S WORKING ON " + players$(D, lineupIdx(D, ballCarrier))
                            End Select

                        End If

                        Call PBP(P)

                    Case 2:
                        pbpString$ = teamGender$(P) + " WORKS FREE INSIDE"
                        Call PBP(P)
                        pbpString$ = teamGender$(P) + " HAS AN OPEN SHOT"
                        Call SHOTSTRING
                        Call PBP(P)

                        playChoice = 1

                    Case 3:

                        If (autoPlay = 0 Or pbpDelay! > 0) Then

                            playChoice = 1

                            Call Rolld100(RN0)

                            'Center
                            If ballCarrier = 4 Then

                                Select Case RN0
                                    Case Is <= 25
                                        pbpString$ = teamGender$(P) + " MAKES A MOVE DOWN LOW"
                                        Call PBP(P)

                                    Case 26 To 50
                                        If teamGender$(P) = "HE" Then
                                            pbpString$ = teamGender$(P) + " POSITIONS HIMSELF DOWN LOW"
                                        Else
                                            pbpString$ = teamGender$(P) + " POSITIONS HERSELF DOWN LOW"
                                        End If

                                        Call PBP(P)

                                    Case 51 To 75
                                        pbpString$ = teamGender$(P) + " POSTS UP UNDERNEATH"
                                        Call PBP(P)

                                    Case 76 To 100
                                        pbpString$ = teamGender$(P) + " BACKS IN AGAINST " + players$(D, lineupIdx(D, 4))

                                End Select

                                Call PBP(P)

                                pbpString$ = teamGender$(P) + " HAS A GOOD SHOT INSIDE"
                                Call SHOTSTRING
                                Call PBP(P)

                            End If

                            'Forwards
                            If (ballCarrier = 2 Or ballCarrier = 3) Then

                                Select Case RN0

                                    Case Is <= 17
                                        pbpString$ = teamGender$(P) + " DRIVES THE BASELINE"

                                    Case 18 To 34
                                        pbpString$ = teamGender$(P) + " PENETRATES THE LANE"

                                    Case 35 To 50
                                        pbpString$ = teamGender$(P) + " MOVES INSIDE"

                                    Case 51 To 67
                                        pbpString$ = teamGender$(P) + " DRIVES THE LANE"

                                    Case 68 To 84
                                        pbpString$ = teamGender$(P) + " MOVES INTO THE PAINT"

                                    Case 85 To 100
                                        pbpString$ = teamGender$(P) + " DRIVES UNDERNEATH"

                                End Select

                                Call PBP(P)

                                pbpString$ = teamGender$(P) + "'S GOT A GOOD SHOT UNDERNEATH"
                                Call SHOTSTRING
                                Call PBP(P)

                            End If

                            'Guards
                            If (ballCarrier = 0 Or ballCarrier = 1) Then

                                Select Case RN0
                                    Case Is <= 25
                                        pbpString$ = teamGender$(P) + " DRIVES THE LANE":

                                    Case 26 To 50
                                        pbpString$ = teamGender$(P) + " DRIVES THROUGH THE KEY"
                                        Call PBP(P)
                                        pbpString$ = teamGender$(P) + "'S INTO THE LANE"

                                    Case 51 To 75
                                        pbpString$ = teamGender$(P) + " DRIVES TO THE BASKET"

                                    Case 76 To 100
                                        pbpString$ = teamGender$(P) + " DRIVES THROUGH AN OPENING"

                                End Select

                                Call PBP(P)

                                pbpString$ = teamGender$(P) + " HAS A GOOD SHOT CLOSE IN"
                                Call SHOTSTRING
                                Call PBP(P)

                            End If

                        End If

                    Case 4:
                        pbpString$ = teamGender$(P) + "'S OPEN FROM 3-POINT RANGE!"
                        pbpString$ = pbpString$ + Str$(threePtShotChance) + "*"
                        Call PBP(P)

                        playChoice = 4

                    Case 5 To 7
                        pbpString$ = teamGender$(P) + "'S OPEN FROM MEDIUM RANGE"
                        Call SHOTSTRING
                        Call PBP(P)

                        playChoice = 1

                End Select

            End If

        End If

    End If

    529 '
    If shotChance! <= 0 Then shotChance! = 1
    If shotChance! >= 100 Then shotChance! = 99

    If (autoPlay = 0 Or pbpDelay! > 0) Then

        If pbpOpt = 1 Then

            Call WINDEX
            passString$ = "PASS TO " + players$(P, lineupIdx(P, ballCarrier)) + " OF "
            Print passString$;
            Color teamRat_GAME(P, 0), teamRat_GAME(P, 1)
            If Len(passString$) + Len(gameTeams$(P)) > 38 Then
                Print Left$(gameTeams$(P), 38 - Len(passString$))
            Else
                Print gameTeams$(P)
            End If
            Color 15, 0

            Locate 10
            If shotType = 4 Then
                Print "3 PT SHOT %"; threePtShotChance;
                Print "*";
            Else
                Print "SHOT %"; shotChance!;
                If shotType >= 2 And shotType <= 8 Then
                    Print "*";
                End If
            End If

            Call DELAY

        Else

            Locate 8, 41
            If shotType = 4 Then
                Print Using "SHOT % ##"; threePtShotChance;
                Print "*";
            Else
                Print Using "SHOT % ##"; shotChance!;
                If shotType >= 2 And shotType <= 8 Then
                    Print "*";
                End If
            End If

        End If

    End If

    Print


    ' *** Determine value for playChoice ***
    'playChoice = play type
    '0 = Pass
    '1 = Shoot (2FGA)
    '2 = Timeout
    '3 = Foul
    '4 = Shoot (3FGA)

    'Computer Opponent or "Let Computer Shoot"
    scoreDiff = score(P, 0) - score(D, 0)
    If playerMode <> 0 Or coachOpt = 1 Then

        ' NOT (H2 | Overtime, 4 mins left, Offense Ahead, Computer is Visitor / On Offense)
        If Not (currHalf >= 2 And gameClock! <= 240 And scoreDiff > 0 And playerMode = 1 And P = compTeam) Then

            'PC vs PC, Computer on Defense
            If playerMode = 2 Or playerMode = 1 And D = compTeam Then

                'Human Opponent | PC is Visitor AND Coach Shots
                If (playerMode = 1 Or playerMode = 0) And coachOpt <> 1 Then
                    'Get user input
                    GoTo 530
                End If

            End If

        End If

        'Determine when to shoot
        If gameClock! <= 1 Or (sClockVal >= 1 And shotClock <= 7) Then
            playChoice = 1
        Else
            If gameClock! <= 8 And scoreDiff <= -3 And currHalf >= 2 Then
                playChoice = 1
            Else
                If gameClock! <= 20 And scoreDiff <= -5 And currHalf >= 2 Then

                    playChoice = 1

                Else

                    baseShotChance = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 0)

                    If tmFatigue(P, lineupIdx(P, ballCarrier)) <= -5 Then
                        baseShotChance = baseShotChance - 10
                    End If

                    'Determine when PC will take TO
                    If playChoice <> 1 Then

                        'Human Opponent, Computer Shoots | Computer is Visitor AND on Defense, Computer Shoots | Computer is Visitor AND On Offense | Computer is Home Team
                        If (playerMode = 0 And coachOpt = 1 Or playerMode = 1 And D = compTeam And coachOpt = 1 Or playerMode = 1 And P = compTeam Or playerMode = 2) Then

                            If gameClock! <= 660 And gameClock! >= 540 And currHalf = 1 And timeouts(P) >= 5 Then
                                playChoice = 2
                                computerTimeout = 1
                            Else
                                If gameClock! <= 240 And gameClock! >= 120 And currHalf = 1 And timeouts(P) >= 4 Then
                                    playChoice = 2
                                    computerTimeout = 1
                                Else
                                    If gameClock! <= 840 And gameClock! >= 720 And currHalf = 2 And timeouts(P) >= 3 Then
                                        playChoice = 2
                                        computerTimeout = 1
                                    Else
                                        If gameClock! <= 300 And gameClock! >= 180 And currHalf = 2 And timeouts(P) >= 2 Then
                                            playChoice = 2
                                            computerTimeout = 1
                                        End If
                                    End If
                                End If

                            End If

                        End If

                    End If

                End If

            End If

        End If

    Else

        'Respond to manual input from user for shot options
        530 '
        Do

            I$ = GetKeyPress$

            Select Case UCase$(I$)

                Case "B"
                    Call BOXOPTIONS(0)
                    Call SCOREBOARD

                Case "D"
                    Call ZONECHANGE
                    Call SCOREBOARD

                Case "O"
                    Call OPTIONS

                Case "T"
                    Call TICKER
                    Call SCOREBOARD

                Case "Q"
                    Exit Sub

            End Select

            playChoice = Val(I$)

        Loop Until UCase$(I$) = "Q" Or UCase$(I$) = "B" Or (playChoice >= 0 And playChoice <= 4)

        If playChoice = 4 Then
            forcedThree = 1
        End If

    End If

    'Loop Until playerMode <> 0 Or coachOpt = 1 Or (UCase$(I$) <> "T" And UCase$(I$) <> "D" And UCase$(I$) <> "O")

    '----------------------------------------------------
    '--- THIS IS CRITCAL FOR PBP MOVING AUTOMATICALLY ---
    '----------------------------------------------------
    ' essentially it allows the user to press a key
    ' during game play to activate certain functions
    ' or special menus, etc.
    532 '
    I$ = InKey$

    Select Case UCase$(I$)

        Case "B"
            Call BOXOPTIONS(0)
            Call SCOREBOARD

        Case "D"
            Call ZONECHANGE
            Call SCOREBOARD
            GoTo 529

        Case "O"
            Call OPTIONS
            GoTo 529

        Case "T"
            Call TICKER
            Call SCOREBOARD
            GoTo 529

        Case "Q"
            Exit Sub

    End Select

    'Foul was chosen | Computer on offense and last 4 minutes of game
    If Val(I$) = 3 And (playerMode = 0 Or playerMode = 1 And P = compTeam) And currHalf >= 4 And gameClock! < 240 Then
        playChoice = 3
    End If

    'Timeout
    If Val(I$) = 2 And (playerMode = 0 Or playerMode = 1 And D = compTeam) And timeouts(P) > 0 Then
        playChoice = 2
    End If

    P3 = playChoice

    If playerMode < 2 Then
        If (playChoice = 2 And timeouts(P) = 0) Or (playChoice = 3 And currHalf = 1) Or (playChoice = 3 And currHalf = 2 And gameClock! > 240) Or (playChoice = 3 And playerMode = 1 And D = compTeam) Then
            'Take shot with no timeouts | Take timeout with more than 4 mins left | Take timeout before final half | Take timeout with PC on Defense
            GoTo 530
        End If
    End If

    ' *** Decide if 3FGA ***
    If J8% = 1 Then
        If (playerMode = 1 And P = compTeam) Or (playerMode = 2) Then
            playChoice = 4
        End If
    End If

    scoreDiff = score(D, 0) - score(P, 0)

    'Try for 3 near end of game when down by 3
    If currHalf >= 2 And gameClock! < 30 And scoreDiff >= 3 And ((playerMode = 1 And P = compTeam) Or playerMode = 2) Then
        If threePtOpt = 1 Then
            playChoice = 4
        End If
    End If

    'Shoot and in 3 pt range
    If (playChoice = 1 And shotType = 4) Then
        If threePtOpt = 1 Then
            playChoice = 4
        End If
    End If

    ' *** Done determining value for playChoice ***
    playType = playChoice + 1

    Select Case playType

        Case 1
            ' *** PASS ***
            GoTo 350

        Case 2
            537 '
            ' *** SHOOT !!! ***
            pbpType = 3
            Call MAKEPBP(pbpType)
            Call PBP(P)

            shotRoll = 0
            Call Rolld100(foulRoll)

            plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 7) = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 7) + 1

            scoreDiff = score(D, 0) - score(P, 0)

            If Not (currHalf > 1 And gameClock! <= 30 And scoreDiff = 2 Or currHalf > 1 And gameClock! <= 30 And scoreDiff = 0 Or currHalf > 1 And gameClock! <= 30 And scoreDiff = -2) Then

                'Player attempt to draw FOUL
                '-- Compare roll to Foul Draw Rating + Def Foul Adj
                If foulRoll <= plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 16) + teamAdj(D, 7) Then

                    'FOUL!!!
                    plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 7) = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 7) - 1
                    Call ResetShotClock

                    Call AdjustClockTime(4)
                    Call AdjustPlayTime
                    'Call AdjustShotClock

                    Call UPDATESCREEN

                    Call FoulCalled(1)

                    Call FreeThrow_Two(missedLast)

                    LT = 0

                    If gameClock! <= 0 Then

                        timeElapsed! = gameClock!
                        Call AdjustPlayTime

                        If autoPlay <> 1 And playerMode <> 2 Then
                            Call HORN
                        End If

                        GoTo 170

                    End If

                    If missedLast = 1 Then
                        'Missed last FT - REBOUND
                        GoTo 650
                    Else

                        598 '
                        Call ChangePossession

                        pbpString$ = gameTeams$(P) + " BALL O.B. AFTER SCORE"
                        Call SELECTPBP(P, 1)
                        Call DELAY

                        Call STOPPAGE
                        Call PRESSGUARD

                        ballCarrier = 6
                        offStatus = 0

                        GoTo 300

                    End If

                End If

            End If

            Call Rolld100(shotRoll)

            'So, foul chance is up to 10% of the shot chance???
            foulChance! = shotChance! * ((rebFoulChance / 100) + (.03 * threePtOpt))

            If Not (currHalf > 1 And gameClock! < 30 And scoreDiff = 2 Or currHalf > 1 And gameClock! < 30 And scoreDiff = 0 Or currHalf > 1 And gameClock! < 30 And scoreDiff = -2) Then

                If shotRoll <= foulChance! Then

                    Call ResetShotClock

                    Call AdjustClockTime(4)
                    Call AdjustPlayTime
                    'Call AdjustShotClock

                    Call UPDATESCREEN

                    Call CreditAssists

                    LT = 0

                    If willDunk = 1 Then

                        If pbpOpt = 1 Then
                            Call WINDEX
                        Else
                            'Women dunk on 0.15% of FGAs
                            '(as opposed to 2.31% for men)
                            If teamGender$(P) = "SHE" And d100% > 5 Then
                                pbpType = 3
                            Else
                                pbpType = 2
                                'Failed dunk
                            End If

                            Call MAKEPBP(pbpType)
                        End If

                        pbpString$ = "STUFF BY " + players$(P, lineupIdx(P, ballCarrier)) + " !!!"
                        Call PBP(D)
                        Call DELAY

                        willDunk = 0

                    Else

                        If shotType = 8 And pbpOpt = 0 Then
                            pbpType = 7
                            Call MAKEPBP(pbpType)
                            shotType = 0
                            Call PBP(P)
                        Else
                            pbpString$ = "BASKET GOOD BY " + players$(P, lineupIdx(P, ballCarrier))
                            Call SELECTPBP(P, 1)
                        End If

                    End If

                    617 '
                    pbpString$ = "... AND " + teamGender$(P) + "'S FOULED !!!"
                    Call SELECTPBP(P, 0)
                    Call DELAY

                    'Update score and FGA
                    score(P, currHalf) = score(P, currHalf) + 2
                    score(P, 0) = score(P, 0) + 2
                    plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 8) = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 8) + 1

                    Call UPDATESCREEN

                    Call FoulCalled(1)

                    619 '
                    If (autoPlay = 0 Or pbpDelay! > 0) Then

                        If pbpOpt = 1 Then
                            Call WINDEX
                            Print gameTeams$(P)
                        End If

                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " WILL SHOOT 1"
                        Call SELECTPBP(P, 0)

                    End If

                    Call FreeThrow_One(missedLast)

                    If gameClock! <= 0 Then

                        timeElapsed! = gameClock!
                        Call AdjustPlayTime

                        If autoPlay <> 1 And playerMode <> 2 Then
                            Call HORN
                        End If

                        GoTo 170

                    End If

                    offStatus = 99

                    If missedLast = 1 Then

                        GoTo 670

                    Else

                        Call ChangePossession

                        pbpString$ = gameTeams$(P) + " BALL O.B. AFTER SCORE"
                        Call SELECTPBP(P, 1)
                        Call DELAY

                        Call STOPPAGE
                        Call PRESSGUARD

                        ballCarrier = 6
                        offStatus = 0

                        GoTo 300

                    End If

                End If

            End If

            If shotRoll <= shotChance! Then

                'Shot Good
                LT = 0

                If willDunk = 1 Then

                    If pbpOpt = 1 Then
                        Call WINDEX
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " DUNKS IT IN!!!"
                    Else
                        'Women dunk on 0.15% of FGAs
                        '(as opposed to 2.31% for men)
                        '-- "1%" factor = 20
                        If teamGender$(P) = "SHE" And d100% > 5 Then
                            pbpType = 3
                        Else
                            pbpType = 2
                        End If

                        Call MAKEPBP(pbpType)

                    End If

                    Call PBP(P)
                    Call DELAY
                    willDunk = 0

                Else

                    If pbpOpt = 0 And shotType = 8 Then

                        pbpType = 7
                        Call MAKEPBP(pbpType)
                        Call PBP(P)
                        shotType = 0

                    Else

                        pbpString$ = "BASKET BY " + players$(P, lineupIdx(P, ballCarrier)) + " !!!"

                        If pbpOpt = 1 Then

                            If pbpDelay! > 0 Then
                                Call WINDEX
                                Call PBP(P)
                                Call DELAY
                            End If

                        Else

                            If d100% > 50 Then

                                RN1 = Int(Rnd(1) * 22) + 1

                                Select Case RN1
                                    Case 1
                                        pbpString$ = "NOTHING BUT THE BOTTOM OF THE NET!"
                                    Case 2
                                        pbpString$ = "COUNT IT!"
                                    Case 3
                                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " TICKLES THE TWINE!"
                                    Case 4
                                        pbpString$ = "NO RIM, NO BOARD!, JUST CORD!"
                                    Case 5
                                        pbpString$ = "GOOD!"
                                    Case 6
                                        pbpString$ = "YES!"
                                    Case 7
                                        pbpString$ = "COUNT IT!"
                                    Case 8
                                        pbpString$ = "IT'S GOOD!"
                                    Case 9
                                        pbpString$ = "BINGO!"
                                    Case 10
                                        pbpString$ = "IT FALLS!"
                                    Case 11
                                        pbpString$ = teamGender$(P) + " DRAINS IT!"
                                    Case 12
                                        pbpString$ = "SWISH!"
                                    Case 13
                                        pbpString$ = "ALL NET!"
                                    Case 14
                                        pbpString$ = "IT'S THERE!"
                                    Case 15
                                        pbpString$ = teamGender$(P) + " BURIES IT!"
                                    Case 16
                                        pbpString$ = "IT GOES IN!"
                                    Case 17
                                        pbpString$ = "RING IT UP!"
                                    Case 18
                                        pbpString$ = "IT GOES DOWN!"
                                    Case 19
                                        pbpString$ = "BULLSEYE!"
                                    Case 20
                                        pbpString$ = teamGender$(P) + " SCORES!"
                                    Case 21
                                        pbpString$ = teamGender$(P) + " HAS IT!"
                                    Case 22
                                        pbpString$ = teamGender$(P) + " CONNECTS!"
                                End Select

                            End If

                            Call PBP(P)

                        End If

                    End If

                End If

                560 '
                score(P, currHalf) = score(P, currHalf) + 2
                score(P, 0) = score(P, 0) + 2

                Call CreditAssists

                Call UPDATESCREEN

                plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 8) = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 8) + 1

                562 '
                '-- Adjust time: shot
                Call ResetShotClock

                Call AdjustClockTime(4)
                Call AdjustPlayTime
                'Call AdjustShotClock

                If gameClock! <= 0 Then

                    timeElapsed! = gameClock!
                    Call AdjustPlayTime

                    If autoPlay <> 1 And playerMode <> 2 Then
                        Call HORN
                    End If

                    GoTo 170

                End If

                Call UPDATESCREEN

                Call ChangePossession

                pbpString$ = gameTeams$(P) + " BALL O.B. AFTER SCORE"
                Call SELECTPBP(P, 1)
                Call DELAY

                Call STOPPAGE
                Call PRESSGUARD

                GoTo 300

            Else

                'Shot Missed
                LT = 0

                ' *** CHECK FOR BLOCK ***
                blockChance = Int(Rnd(1) * 5)
                rndPlayer = Int(Rnd(1) * 5)
                blockRating = plyrOff_GAME!(D, lineupIdx(D, rndPlayer), 6)

                'Check player's blocks / game
                If blockChance > blockRating Then

                    If pbpOpt = 1 Then
                        If pbpDelay! > 0 Then
                            pbpString$ = "SHOT MISSED BY " + players$(P, lineupIdx(P, ballCarrier))
                        End If
                    Else
                        pbpType = 8
                        Call MAKEPBP(pbpType)
                    End If

                    Call SELECTPBP(P, 1)

                    If autoPlay <> 1 Or playerMode <> 2 And backboard& And d100% < 10 Then
                        _SndPlay backboard&
                    End If

                    Call ResetShotClock
                    Call AdjustClockTime(4)
                    Call AdjustPlayTime
                    'Call AdjustShotClock

                    If gameClock! <= 0 Then
                        timeElapsed! = gameClock!
                        Call AdjustPlayTime
                        GoTo 170
                    End If

                    Call UPDATESCREEN

                    LT = 0

                Else

                    'Players with 0 rating, need a slight chance
                    'Players with 1 rating will have too frequent a chance
                    If (blockRating = 0 And d100% <= 10) Or (blockRating = 1 And d100% <= 70) Or blockRating > 1 Then

                        If (autoPlay = 0 Or pbpDelay! > 0) Then

                            If pbpOpt = 1 Then
                                Call WINDEX
                                Print "SHOT BLOCKED BY "; players$(D, lineupIdx(D, rndPlayer))
                                Call DELAY
                            Else

                                Call Rolld100(RN0)

                                Select Case RN0
                                    Case 1 To 10
                                        pbpString$ = players$(D, lineupIdx(D, rndPlayer)) + " STUFFS THE SHOT!!"
                                    Case 11 To 15
                                        pbpString$ = "DENIED BY " + players$(D, lineupIdx(D, rndPlayer)) + "!!"
                                    Case 16 To 20
                                        pbpString$ = "ERASED BY " + players$(D, lineupIdx(D, rndPlayer)) + "!!"
                                    Case 21 To 25
                                        pbpString$ = "EXPUNGED BY " + players$(D, lineupIdx(D, rndPlayer)) + "!!"
                                    Case 26 To 30
                                        pbpString$ = "REBUKED BY " + players$(D, lineupIdx(D, rndPlayer)) + "!!"
                                    Case 31 To 35
                                        pbpString$ = "SHOT BLOCKED CLEANLY BY " + players$(D, lineupIdx(D, rndPlayer)) + "!!"
                                    Case 36 To 70
                                        pbpString$ = "SHOT BLOCKED BY " + players$(D, lineupIdx(D, rndPlayer)) + "!!"
                                    Case 71 To 90
                                        pbpString$ = "SHOT REJECTED BY " + players$(D, lineupIdx(D, rndPlayer)) + "!!"
                                    Case 91 To 100
                                        pbpString$ = "SHOT SWATTED OUT BY " + players$(D, lineupIdx(D, rndPlayer)) + "!!"
                                End Select

                                Call PBP(D)

                            End If

                        End If

                        offStatus = 98

                        'Increase blocks
                        plyrOff_GAME!(D, lineupIdx(D, rndPlayer), 15) = plyrOff_GAME!(D, lineupIdx(D, rndPlayer), 15) + 1

                        Call ResetShotClock
                        Call AdjustClockTime(4)
                        Call AdjustPlayTime
                        'Call AdjustShotClock

                        If gameClock! <= 0 Then
                            timeElapsed! = gameClock!
                            Call AdjustPlayTime
                            was3PtShot = 0
                            GoTo 170
                        End If

                        Call UPDATESCREEN

                    End If

                End If

                GoTo 670
                
            End If

            '--- done with "routines" for taking a shot

        Case 3
            ' *** TIMEOUT ***
            Call TimeoutCalled(P)

            If A6 = 0 Then
                Call PRESSGUARD
                GoTo 300
            Else
                GoTo 350
            End If


        Case 4
            ' *** FOUL ***
            foulOnPlay = 99

            Call LASTSHOT
            If gameClock! <= 0 Then 170

            C1 = 2
            P9 = D

            pbpString$ = "DESIGNATED FOULER (0-4)"
            Call SELECTPBP(P, 1)

            If playerMode = 1 And D = compTeam Or playerMode = 2 Then
                desigFouler = Int(Rnd(1) * 5)
            Else
                Do
                    I$ = GetKeyPress$
                    desigFouler = Val(I$)
                Loop Until desigFouler >= 0 And desigFouler <= 4
            End If

            foulPlayer = desigFouler
            Call FoulCalled(0)

            GoTo 9065

        Case 5
            ' *** 3FG ATTEMPT ***
            J8% = 0

            If threePtOpt = 0 Then
                '3-point not supported
                'Take regular shot
                P3 = 1
                playChoice = 1
                GoTo 537

            Else

                'Increase 3FGA
                threeFG(P, lineupIdx(P, ballCarrier), 1) = threeFG(P, lineupIdx(P, ballCarrier), 1) + 1
                plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 7) = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 7) + 1

                pbpType = 3 'Shot
                Call MAKEPBP(pbpType)
                Call PBP(P)

                'Check for foul
                Call Rolld100(shotRoll)
                Call Rolld100(foulChance)

                Call pbplog(6221)

                '2% chance of being fouled on 3-pointer
                If foulChance <= 2 And ft3PtFoul = 1 Then

                    If shotRoll > threePtShotChance Then

                        'FOULED on Missed Shot

                        'Remove the attempt from the record
                        plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 7) = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 7) - 1
                        threeFG(P, lineupIdx(P, ballCarrier), 1) = threeFG(P, lineupIdx(P, ballCarrier), 1) - 1

                        Call ResetShotClock
                        Call AdjustClockTime(4)
                        Call AdjustPlayTime
                        'Call AdjustShotClock

                        Call UPDATESCREEN

                        Call FoulCalled(1)

                        If (autoPlay = 0 Or pbpDelay! > 0) Then

                            If pbpOpt = 1 Then
                                Call WINDEX
                                Print gameTeams$(P)
                                End

                                pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " WILL SHOOT 3"
                                Call SELECTPBP(P, 0)

                            End If

                        End If

                        Call FreeThrow_Three(missedLast)

                        LT = 0

                        If gameClock! <= 0 Then
                            timeElapsed! = gameClock!
                            Call AdjustPlayTime
                            GoTo 170
                        End If

                        If missedLast = 1 Then
                            'Missed shot
                            GoTo 650
                        Else

                            Call ChangePossession

                            pbpString$ = gameTeams$(P) + " BALL O.B. AFTER SCORE"
                            Call SELECTPBP(P, 1)
                            Call DELAY

                            Call STOPPAGE
                            Call PRESSGUARD

                            ballCarrier = 6
                            offStatus = 0

                            GoTo 300

                        End If

                    Else

                        'FOULED on Good Shot

                        Call AdjustClockTime(4)
                        Call AdjustPlayTime
                        'Call AdjustShotClock

                        Call UPDATESCREEN

                        Call CreditAssists

                        pbpString$ = "3 PT BASKET GOOD BY " + players$(P, lineupIdx(P, ballCarrier))
                        Call SELECTPBP(P, 1)

                        Call CreditAssists

                        pbpString$ = "AND " + teamGender$(P) + " WAS FOULED !!!"
                        Call SELECTPBP(P, 0)

                        score(P, currHalf) = score(P, currHalf) + 3
                        score(P, 0) = score(P, 0) + 3

                        plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 8) = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 8) + 1
                        threeFG(P, lineupIdx(P, ballCarrier), 0) = threeFG(P, lineupIdx(P, ballCarrier), 0) + 1

                        Call UPDATESCREEN

                        Call FoulCalled(1)

                        GoTo 619

                    End If 'Done checking foul shot

                Else

                    'Not Fouled

                    If shotRoll > threePtShotChance Then

                        'Missed Shot

                        If pbpOpt = 1 Then
                            If pbpDelay! > 0 Then
                                pbpString$ = "3 PT FG MISSED BY " + players$(P, lineupIdx(P, ballCarrier))
                            End If
                        Else
                            was3PtShot = 1
                            pbpType = 5
                            Call MAKEPBP(pbpType)
                        End If

                        Call SELECTPBP(P, 0)

                        If autoPlay <> 1 Or playerMode <> 2 And backboard& And d100% < 10 Then
                            _SndPlay backboard&
                        End If

                        Call ResetShotClock
                        Call AdjustClockTime(4)
                        Call AdjustPlayTime
                        'Call AdjustShotClock

                        If gameClock! <= 0 Then
                            timeElapsed! = gameClock!
                            Call AdjustPlayTime
                            was3PtShot = 0
                            GoTo 170
                        End If

                        Call UPDATESCREEN

                        GoTo 670

                    Else

                        'Good Shot

                        If pbpOpt = 1 Then
                            If pbpDelay! > 0 Then
                                pbpString$ = "3 PT FG GOOD BY " + players$(P, lineupIdx(P, ballCarrier)) + " !!!"
                            End If
                        Else
                            pbpType = 4
                            Call MAKEPBP(pbpType)
                        End If

                        Call SELECTPBP(P, 0)

                        If autoPlay <> 1 Or playerMode <> 2 And swish& Then
                            _SndPlay swish&
                        End If

                        369 '
                        If XX% = 1 Then
                            'Foot was on the line - it's only a 2FGA
                            XX% = 0
                            score(P, currHalf) = score(P, currHalf) + 2
                            score(P, 0) = score(P, 0) + 2
                            threeFG(P, lineupIdx(P, ballCarrier), 1) = threeFG(P, lineupIdx(P, ballCarrier), 1) - 1
                        Else
                            'Reward 3 pts
                            score(P, currHalf) = score(P, currHalf) + 3
                            score(P, 0) = score(P, 0) + 3
                            threeFG(P, lineupIdx(P, ballCarrier), 0) = threeFG(P, lineupIdx(P, ballCarrier), 0) + 1
                        End If

                        'Increase FGM
                        plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 8) = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 8) + 1

                        Call CreditAssists

                        Call UPDATESCREEN

                        GoTo 562

                    End If 'Done processing normal 3-pointer

                End If 'Done processing foul on shot

            End If 'Done confirming 3-pt option

    End Select

    '----- NOODLE -----

    ' BONUS!!!
    ' 1 + 1 Free Throw

    570 '
    Call FreeThrow_OneAndOne(missedLast)

    If gameClock! <= 0 Then 170

    If missedLast = 1 Then

        GoTo 650

    Else

        Call ChangePossession

        pbpString$ = gameTeams$(P) + " BALL O.B. AFTER SCORE"
        Call SELECTPBP(D, 1)
        Call DELAY

        Call STOPPAGE
        Call PRESSGUARD

        offStatus = 0

        GoTo 300

    End If


    '----- NOODLE -----

    ' *** TEAM FOULS ***

    580 '
    Call LASTSHOT

    Call ResetShotClock

    'FoulCalled always targets the DEF
    'Switch teams before we handle
    Call ChangePossession

    Call FoulCalled(1)

    ballCarrier = foulPlayer

    'Check if in BONUS
    If teamFouls(D) > 6 Then

        GoTo 570

    Else

        If gameClock! <= 0 Then 170

        pbpString$ = "OFFENSIVE FOUL"
        Call SELECTPBP(P, 1)

        pbpString$ = gameTeams$(P) + " BALL O.B. AFTER FOUL"
        Call SELECTPBP(P, 1)

        Locate 8, 41: Print Space$(12)

        ballCarrier = 6
        Call DELAY

        turnovers(D) = turnovers(D) + 1

        Call UPDATESCREEN

        Call DEADBALLOPTIONS

        For I = 0 To 1
            If defCategory(I) = 0 Then
                pressureRating(I, 0) = 1
                pressureRating(I, 1) = 7
                pctContrib(I) = 0
            End If
        Next

        Call PRESSGUARD

        GoTo 300

    End If


    '----- NOODLE -----

    ' ***** CHECK FOR REBOUNDS *****

    650 '
    A6 = 1
    playChoice = 0
    assistShotBoost = 0
    D6 = 0
    shotRoll = 0

    'So we reset the shot clcok as soon
    'as we know it's a REBOUND
    Call ResetShotClock

    Call Rolld100(RN0)

    scoreDiff = score(P, 0) - score(D, 0)

    'Roughly 65% of rebounds are DEFENSIVE
    'Roughly 35% of rebounds are OFFENSIVE
    'Roughly 10 - 20% of ALL rebounds are fouled

    If (currHalf > 1 And gameClock! < 30 And score(D, 0) - score(P, 0) = 2 And RN0 <= 12) Or (currHalf > 1 And gameClock! < 30 And scoreDiff = 0 And RN0 <= 12) Then

        Call ReboundOffense
        GoTo 760

    Else

        Call Rolld100(RN1)

        Select Case RN1

            Case 1 To 16
                foulOnReb = 1
                GoTo 707

            Case 17 To 24
                Call ReboundOffense
                GoTo 760

            Case 25 To 100
                Call ReboundDefense
                GoTo 800

        End Select

    End If


    '----- NOODLE -----

    670 '
    A6 = 1
    playChoice = 0
    assistShotBoost = 0
    D6 = 0

    Call ResetShotClock

    '-- Start Loop
    700 '
    shotRoll = 0
    assistShotBoost = 0
    D6 = 0

    Call Rolld100(RN0)

    scoreDiff = score(D, 0) - score(P, 0)
    If currHalf > 1 And gameClock! < 30 And (scoreDiff = 2 Or scoreDiff = 0 Or scoreDiff = -2) And RN0 <= 12 Then
        GoTo 700
    End If
    '--- loop

    If was3PtShot = 1 And RN0 <= 19 Then
        'why would we do this?
        'either it was a 3 pointer or not?
        was3PtShot = 0
    End If

    Select Case RN0

        Case 1

            If offStatus = 98 Then
            
                GoTo 700

            Else

                turnovers(P) = turnovers(P) + 1

                pbpString$ = "OFFENSIVE GOAL TENDING"
                Call SELECTPBP(P, 1)

                pbpString$ = "BALL O.B. TO " + gameTeams$(D)
                Call SELECTPBP(P, 1)

                Call DELAY

                Call ChangePossession

                Call DEADBALLOPTIONS

                For I = 0 To 1
                    If defCategory(I) = 0 Then
                        pressureRating(I, 0) = 1
                        pressureRating(I, 1) = 7
                        pctContrib(I) = 0
                    End If
                Next

                GoTo 300

            End If

        Case 2 To rebFoulChance
            foulOnReb = 1
            'GoTo 707

        Case rebFoulChance + 1 To 17

            If d100% > 35 Then

                totRebounds(D) = totRebounds(D) + 1

                Call UPDATESCREEN

                pbpString$ = "RB KNOCKED O.B. TO " + gameTeams$(D)
                Call SELECTPBP(D, 1)
                Call DELAY

                Call DEADBALLOPTIONS

                For I = 0 To 1
                    If defCategory(I) = 0 Then
                        pressureRating(I, 0) = 1
                        pressureRating(I, 1) = 7
                        pctContrib(I) = 0
                    End If
                Next

                Call ChangePossession

                GoTo 300

            Else

                752 '
                totRebounds(P) = totRebounds(P) + 1

                Call UPDATESCREEN

                pbpString$ = "RB KNOCKED O.B. TO " + gameTeams$(P)
                Call SELECTPBP(P, 1)
                Call DELAY

                Call DEADBALLOPTIONS

                For I = 0 To 1
                    If defCategory(I) = 0 Then
                        pressureRating(I, 0) = 1
                        pressureRating(I, 1) = 7
                        pctContrib(I) = 0
                    End If
                Next

                LT = 4

                GoTo 520

            End If

        Case 18 To 19

            If offStatus = 99 Then
                
                GoTo 700
                
            Else

                Call ReboundOffense

                plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 7) = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 7) + 1
                plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 8) = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 8) + 1
                plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 11) = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 11) + 1
                totRebounds(P) = totRebounds(P) + 1

                score(P, currHalf) = score(P, currHalf) + 2
                score(P, 0) = score(P, 0) + 2

                Call UPDATESCREEN

                If pbpOpt = 1 Then
                    If pbpDelay! > 0 Then
                        Call WINDEX
                        Print "TIPPED IN BY "; players$(P, lineupIdx(P, ballCarrier))
                        Print "   BASKET GOOD !!!"
                        Call DELAY
                    End If
                Else
                    pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " TIPS IT IN!!"
                    Call PBP(P)
                End If

                Call ChangePossession

                pbpString$ = gameTeams$(P) + " BALL O.B. AFTER SCORE"
                Call SELECTPBP(P, 1)
                Call DELAY

                Call STOPPAGE
                Call PRESSGUARD

                GoTo 300

            End If

        Case 20

            If offStatus = 99 Then

                GoTo 700

            Else

                pbpString$ = "DEFENSIVE GOAL TENDING"
                Call SELECTPBP(P, 1)

                If was3PtShot = 1 Then

                    was3PtShot = 0

                    If XX% = 1 Then
                        XX% = 0
                        'Foot was on the line - it's only a 2FGA
                        score(P, currHalf) = score(P, currHalf) + 2
                        score(P, 0) = score(P, 0) + 2
                        threeFG(P, lineupIdx(P, ballCarrier), 1) = threeFG(P, lineupIdx(P, ballCarrier), 1) - 1
                    Else
                        'Reward 3 pts
                        score(P, currHalf) = score(P, currHalf) + 3
                        score(P, 0) = score(P, 0) + 3
                        threeFG(P, lineupIdx(P, ballCarrier), 0) = threeFG(P, lineupIdx(P, ballCarrier), 0) + 1
                    End If

                    plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 8) = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 8) + 1

                    Call CreditAssists

                    Call UPDATESCREEN

                    GoTo 562

                Else

                    GoTo 560

                End If
                
            End If
            
        Case 21 To 100
            707 '
            'Determine rebound contributions
            '-- RB is a total of "rebound ratings" for both teams
            '-- rebRatings is an array to track "rebound ratings" for each player
            was3PtShot = 0
            totalRB = 0

            Erase rebRatings

            'Team 1
            For I = 0 To 4

                plyrRebRat = plyrOff_GAME!(P, lineupIdx(P, I), 21)

                If Not (currHalf > 2 Or currHalf = 2 And gameClock! <= 120) Then
                    If playerFatigue%(P, lineupIdx(P, I)) <= 0 Then
                        plyrRebRat = CInt(plyrRebRat * .5)
                    End If
                End If

                If tmFatigue(P, lineupIdx(P, I)) <= -5 Then
                    plyrRebRat = CInt(plyrRebRat * .9)
                End If

                rebRatings(I) = rebRatings(I) + plyrRebRat
                totalRB = totalRB + plyrRebRat

            Next

            ORB = totalRB

            'Team 2
            For I = 0 To 4

                plyrRebRat = plyrOff_GAME!(1 - P, lineupIdx(1 - P, I), 2)

                If Not (currHalf > 2 Or currHalf = 2 And gameClock! <= 120) Then
                    If playerFatigue%(1 - P, lineupIdx(1 - P, I)) <= 0 Then
                        plyrRebRat = CInt(plyrRebRat * .5)
                    End If
                End If

                If tmFatigue(1 - P, lineupIdx(1 - P, I)) <= -5 Then
                    plyrRebRat = CInt(plyrRebRat * .9)
                End If

                rebRatings(I + 5) = rebRatings(I + 5) + plyrRebRat
                totalRB = totalRB + plyrRebRat

            Next

            DRB = totalRB - ORB

            'XQ = Rebound Chance, Based on Defense
            Select Case defCoverage(D)

                Case 0
                    'Solid Man-to-Man
                    '6% of total rebounds
                    XQ = CInt(totalRB * .06)

                Case 1
                    'Pressure Man-to-Man
                    '12% of total rebounds
                    XQ = CInt(totalRB * .12)

                Case 2
                    'Passive Zone
                    '10% of total rebounds
                    XQ = CInt(totalRB * .1)

                Case 3
                    'Trapping Zone
                    '14% of total rebounds
                    XQ = CInt(totalRB * .14)

            End Select

            If crashBoards(P) = 1 Then
                '17% of total rebounds
                XQ = CInt(totalRB * .17)
            End If

            rbChance = Rnd(1) * totalRB

            If rbChance <= ORB + XQ Then

                rndOReb = Rnd(1) * ORB

                For ballCarrier = 0 To 4
                    If rndOReb <= rebRatings(ballCarrier) Then GoTo 760
                    rebRatings(ballCarrier + 1) = rebRatings(ballCarrier + 1) + rebRatings(ballCarrier)
                Next

                ballCarrier = Int(Rnd(1) * 5)

                GoTo 760

            Else

                rndDReb = Rnd(1) * DRB
                skipRandom% = 0

                For ballCarrier = 5 To 9

                    If rndDReb <= rebRatings(ballCarrier) Then
                        ballCarrier = ballCarrier - 5
                        skipRandom% = 1
                        Exit For
                    Else
                        rebRatings(ballCarrier + 1) = rebRatings(ballCarrier + 1) + rebRatings(ballCarrier)
                    End If

                Next

                If skipRandom% = 0 Then
                    ballCarrier = Int(Rnd(1) * 5)
                End If

            End If

    End Select


    ' *** DEFENSIVE REBOUND ***
    800 '
    totRebounds(D) = totRebounds(D) + 1
    plyrOff_GAME!(D, lineupIdx(D, ballCarrier), 11) = plyrOff_GAME!(D, lineupIdx(D, ballCarrier), 11) + 1

    Call UPDATESCREEN

    If (autoPlay = 0 Or pbpDelay! > 0) Then

        pbpString$ = players$(D, lineupIdx(D, ballCarrier)) + " HAS THE DEF RB"
        Call SELECTPBP(D, 1)
        
        If pbpOpt = 1 Then
            pbpString$ = "FOR " + gameTeams$(D)
            Call PBP(D)
            Call DELAY
        End If

    End If

    If foulOnReb = 1 Then

        If (autoPlay = 0 Or pbpDelay! > 0) Then

            If pbpOpt = 1 Then
                pbpString$ = gameTeams$(D) + " AND IS FOULED !!!"
                Call PBP(D)
                Call DELAY
            Else
                pbpString$ = teamGender$(D) + "'S FOULED !!!"
                Call PBP(D)
            End If

        End If

        'FoulCalled always targets the DEF
        'Switch teams before we handle
        Call ChangePossession

        Call FoulCalled(1)

        LT = 4
        foulOnReb = 0

        GoTo 9065

    Else

        'New defender
        prevBall$ = players$(D, lineupIdx(D, ballCarrier))
        C1 = 2

        Call ChangePossession
        P9 = D

        fastBreak = 1

        'New offense
        prevBall$ = players$(P, lineupIdx(P, ballCarrier))

        RN0 = Int(Rnd(1) * 130) + 1

        If RN0 = 15 Then
            fastBreak = 0
            GoTo RBTIEUP
        End If

        If crashBoards(D) = 1 And d100% <= 50 Then

            Call AdjustClockTime(4)
            Call AdjustPlayTime
            'Call AdjustShotClock

            Call WHOLAY
            GoTo LAYUP

        Else

            GoTo 300

        End If

    End If

    '----- NOODLE -----

    ' *** OFFENSIVE REBOUND ***

    760 '
    plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 11) = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 11) + 1
    plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 24) = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 24) + 1
    totRebounds(P) = totRebounds(P) + 1

    Call UPDATESCREEN

    If (autoPlay = 0 Or pbpDelay! > 0) Then

        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " HAS THE OFF RB"
        Call SELECTPBP(P, 1)

        If pbpOpt = 1 Then
            Print "FOR " + gameTeams$(P)
            Call DELAY
        End If

    End If

    If foulOnReb = 1 Then

        pbpString$ = teamGender$(P) + "'S FOULED !!!"
        Call SELECTPBP(P, 1)

        Call UPDATESCREEN

        Call FoulCalled(1)

        LT = 4
        foulOnReb = 0

        GoTo 9065

    Else
 
        If gameClock! <= 0 Then

            GoTo 170

        Else

            RN0 = Int(Rnd(1) * 130) + 1
            prevBall$ = players$(P, lineupIdx(P, ballCarrier))

            If RN0 = 32 Then

                '0.75% chance
                GoTo RBTIEUP

            Else

                rebShotRoll = Int(Rnd(1) * 10) + 1
                Call SHOTPERCENT

                If Not (currHalf > 2 Or currHalf = 2 And gameClock! <= 120) Then
                    If playerFatigue%(P, lineupIdx(P, ballCarrier)) <= 0 Then
                        shotChance! = CInt(shotChance! * .6)
                    End If
                End If

                If rebShotRoll <= 7 Then
                    shotType = 9
                    playChoice = 1
                    GoTo 537
                Else
                    GoTo 520
                End If

            End If

        End If
        
    End If


    '----- NOODLE -----

    TIEUP:
    If pbpOpt <> 1 Then

        Call Rolld100(RN0)

        Select Case RN0
            Case 1 To 20
                pbpString$ = "BALL IS SLAPPED AWAY"
            Case 21 To 40
                pbpString$ = "BALL IS LOOSE ON THE FLOOR"
            Case 41 To 60
                pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " IS TRAPPED"
            Case 61 To 80
                pbpString$ = "TRIES TO WORK LOOSE"
            Case 81 To 100
                pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " IS PINNED ON THE LINE"
        End Select

        Call PBP(P)

    End If

    RBTIEUP:
    Call Rolld100(RN0)
    P9 = Int(Rnd(1) * 5)

    If pbpOpt = 1 And pbpDelay! > 0 Then
        Call WINDEX
        Print "JUMP BALL!"
        Call DELAY
    Else
        Select Case RN0
            Case 1 To 25
                pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " IS TIED UP BY " + players$(D, lineupIdx(D, P9))
            Case 26 To 50
                pbpString$ = players$(D, lineupIdx(D, P9)) + " TIES UP " + players$(P, lineupIdx(P, ballCarrier))
            Case 51 To 75
                pbpString$ = "HELD BALL!"
            Case 76 To 100
                pbpString$ = "JUMP BALL!"
        End Select

        Call PBP(P)

    End If

    If teamYears(impactTeam) >= 1981 Then
        pbpString$ = "POSSESSION ARROW TO " + gameTeams$(possArrow)
        Call SELECTPBP(P, 0)
    End If

    Call DELAY

    If possArrow = D Then

        turnovers(P) = turnovers(P) + 1

        Call DEADBALLOPTIONS

        For I = 0 To 1
            If defCategory(I) = 0 Then
                pressureRating(I, 0) = 1
                pressureRating(I, 1) = 7
                pctContrib(I) = 0
            End If
        Next

        Call ChangePossession

        possArrow = 1 - possArrow

        GoTo 300

    Else

        possArrow = 1 - possArrow

        Call DEADBALLOPTIONS

        For I = 0 To 1
            If defCategory(I) = 0 Then
                pressureRating(I, 0) = 1
                pressureRating(I, 1) = 7
                pctContrib(I) = 0
            End If
        Next

        LT = 4

        GoTo 350

    End If


    '----- NOODLE -----


    ' *** PERSONAL FOULS ***
    9065 '

    If intentional = 1 Or (ft10thFoul = 1 And teamFouls(D) >= 10) Then

        'BONUS +
        If (autoPlay = 0 Or pbpDelay! > 0) Then

            If pbpOpt = 1 Then
                Call WINDEX
                Print gameTeams$(P)
            End If

            pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " WILL SHOOT 2"
            Call SELECTPBP(P, 0)

        End If

        Call FreeThrow_Two(missedLast)

        If gameClock! <= 0 Then 170

        Call CLEARPBPBOX

        If missedLast And intentional = 0 Then

            GoTo 650

        Else

            If intentional = 1 Then

                passes = 1
                intentional = 0

                pbpString$ = gameTeams$(P) + " BALL AFTER INT. FOUL"
                Call SELECTPBP(P, 1)
                Call DELAY

                Call STOPPAGE
                Call PRESSGUARD

                offStatus = 0

            Else

                Call ChangePossession

                pbpString$ = gameTeams$(P) + " BALL O.B. AFTER SCORE"
                Call SELECTPBP(P, 1)
                Call DELAY

                Call STOPPAGE
                Call PRESSGUARD

                offStatus = 0

            End If

            GoTo 300

        End If

    Else

        'Check if BONUS
        If teamFouls(D) > 6 Then

            GoTo 570

        Else
        
            'Not in bonus
            If gameClock! <= 0 Then 170

            Call DEADBALLOPTIONS

            For I = 0 To 1
                If defCategory(I) = 0 Then
                    pressureRating(I, 0) = 1
                    pressureRating(I, 1) = 7
                    pctContrib(I) = 0
                End If
            Next

            If ft6FloorFouls = 0 Then

                pbpString$ = gameTeams$(P) + " BALL O.B. AFTER FOUL"
                Call SELECTPBP(P, 1)

                GoTo 350

            Else

                'FT after 6 floor fouls
                Call UPDATESCREEN

                'One free throw
                Call FreeThrow_One(missedLast)

                LT = 0

                If gameClock! <= 0 Then
                    timeElapsed! = gameClock!
                    Call AdjustPlayTime
                    GoTo 170
                End If

                If missedLast = 1 Then

                    GoTo 650

                Else

                    Call ChangePossession

                    pbpString$ = gameTeams$(P) + " BALL O.B. AFTER SCORE"
                    Call SELECTPBP(P, 1)
                    Call DELAY

                    Call STOPPAGE
                    Call PRESSGUARD

                    ballCarrier = 6
                    offStatus = 0

                    GoTo 300

                End If

            End If

        End If

    End If

End Sub



'Include: 'GameRoutines.bm'

'----------------------------------------
'            INFO Subroutine
'----------------------------------------
'This subroutine displays a simple header
'and footer for the game options menu.
Sub INFO ()

    Cls

    Color , 1
    Locate 1, 1
    Print Tab(80); " ";

    Locate , 33
    Color 14
    Print "GAME OPTIONS MENU"

    Locate 24, 1
    Print Tab(80); " ";
    Locate , 1
    Print "    LANCE HAFFNER 1986-2024"; Tab(70); Left$(Time$, 5);
    Locate 3, 1
    Color , 0

End Sub


'----------------------------------------
'           InitVar Subroutine
'----------------------------------------
' Initializes variables for the game
' Does not process any that will be read
' in from file or assigned elsewhere
Sub InitVar ()

    Shared actualAttendance&

    Shared scheduleFile$, tourneyFile$

    Shared avgAttendance&(), gameAttendance&()

    Shared d3FG_Adj(), dthreeFGA_Adj()
    Shared eventSettings(), powerRating()
    Shared oppScore(), rosterStatus()
    Shared teamScore(), tourneySettings()

    Shared FY%()

    Shared game3FGM!(), game3FGA!()
    Shared statsOffTot!(), statsDefTot!(), gameStatPlyrVal!()
    Shared indRecords!(), teamRecords!()

    Shared alpha$()
    Shared gameSite$(), locIndicator$(), oppName$()
    Shared indRecDesc$(), teamRecDesc$()
    Shared teamIndicator$(), teamYrTourn$()

    For I = 0 To 1
        For I1 = 0 To 4
            lineupIdx(I, I1) = -1
        Next I1
    Next I

    teamIndicator$(0) = "VISITING TEAM"
    teamIndicator$(1) = "HOME TEAM"

    alpha$(0) = "A "
    alpha$(1) = "B "
    alpha$(2) = "C "
    alpha$(3) = "D "
    alpha$(4) = "E "

    pbpStyle$(0) = "VERBOSE        "
    pbpStyle$(1) = "HAFFNER CLASSIC"

    posnName$(0) = "G"
    posnName$(1) = "G"
    posnName$(2) = "F"
    posnName$(3) = "F"
    posnName$(4) = "C"

    scSettings(0) = 0
    scSettings(1) = 45
    scSettings(2) = 35
    scSettings(3) = 30

    gameClock! = 1200

    ' # of timeouts
    timeouts(1) = 5
    timeouts(0) = 5

    possArrow = -1

    teamRat_GAME(0, 0) = 15
    teamRat_GAME(0, 1) = 1

    teamRat_GAME(1, 0) = 15
    teamRat_GAME(1, 1) = 1

    autoPlay = 0
    ballCarrier = 0
    C1 = 0
    compTeam = 0
    currHalf = 1
    D = 0
    endAllGames = 0
    gameOver = 0
    fastBreak = 0
    forcedThree = 0
    G9 = 0
    gameAvgTmStamina = 0
    halfTime = 0
    foulOnPlay = 0
    clockSecs = 0
    MJ = 0
    nbrLines = 1
    P = 0
    P9 = 0
    passes = 0
    pbpType = 0
    rebFoulChance = 0
    S2 = 0
    shotClock = 0
    shotType = 0
    statSpecial = 0
    threePtShotChance = 0
    timeElapsed! = 0
    tickerGames = 0
    tourneyPlay = 0
    willDunk = 0

    guardNbr = 0
    lineupChanged = 0
    X0% = 0
    X1% = 0
    XX% = 0

    shotChance! = 0

    A1$ = ""
    B1$ = ""
    C1$ = ""
    D1$ = ""
    E1$ = ""
    F1$ = ""
    G1$ = ""
    H1$ = ""

    pbpString$ = ""
    prevBall$ = ""
    scheduleFile$ = ""
    tourneyFile$ = ""
    schedVisTm$ = ""
    schedHomeTm$ = ""

    'These are read in from teams
    'Erase dFGPA, d3FG_Adj, dthreeFGA_Adj
    Erase leagRat_GAME, plyrOff_GAME!, plyrRat_GAME
    'Erase plyrDef_GAME, plyrStat_GAME, teamStats_GAME
    'Erase rosterStatus, teamStamina
    'Erase gameArena$, gameCoaches$, gameMascots$
    'Erase players$, positions_GAME$, posnName$

    'Erase tourneySettings
    'Erase seedSlotName$

    Erase crashBoards
    Erase defStrat, defCategory, defCoverage, eventSettings
    Erase ft3PtFoul, lineupIdx, offStrat, oppScore
    Erase P4, P5
    Erase pbpFG, pbpBG
    Erase plyrStaminaGame, plyrRebIdx, plyrRebVal
    Erase playerTime, powerRating, pressureRating
    Erase QQ, QR, rebRatings, rosterIdx
    Erase schedOptions
    Erase schedGame, score
    Erase statTotals
    Erase statsGame3FGM, statsGame3FGA
    Erase teamAdj, teamIndex
    Erase teamRecWins, teamRecLoss
    Erase teamFouls, teamScore
    Erase threeFG, timePlayed, tmFatigue
    Erase totRebounds, tourneyOptions, turnovers

    Erase FY%, G9%
    Erase ST%, SX%

    Erase diskIDs$, gameSite$, gameTeams$
    Erase indRecDesc$, locIndicator$, oppName$
    Erase Q$, SX$
    Erase teamNames$, teamRecDesc$, teamYrTourn$

    Erase game3FGM!, game3FGA!
    Erase statsOffTot!, statsDefTot!, gameStatPlyrVal!
    Erase indRecords!, teamRecords!

    actualAttendance& = 0
    Erase avgAttendance&, gameAttendance&

End Sub


'----------------------------------------
'          AUTOSETUP Subroutine
'----------------------------------------
'This subroutine auto-assigns game param-
'eters from a schedule file's data.
Sub AUTOSETUP (validTeam)

    Shared tickerStart

    Shared tourneyFile$

    Shared tourneySettings()

    gameLoc = schedOptions(2)
    threePtOpt = schedOptions(3)
    pbpDelay! = schedOptions(4) / 10
    foulsToDQ = schedOptions(5)
    ft10thFoul = schedOptions(6)
    sClockVal = schedOptions(7)
    playerOpt = schedOptions(8)
    coachOpt = schedOptions(9)
    ft3PtFoul = schedOptions(10)
    tickerStart = schedOptions(12)
    ft6FloorFouls = schedOptions(14)

    validTeam = 0

    For tgtTeam = 0 To 1

        tgtId = schedOptions(tgtTeam)
        tgtYear$ = RTrim$(diskIDs$(tgtTeam))

        teamGender$(tgtTeam) = GetTeamGender$(tgtYear$)

        Call ReadGameTeam(tgtYear$, gameTeams$(tgtTeam), tgtId, tgtTeam)
        Call ReadGameExtraData(tgtYear$, tgtId, tgtTeam, validTeam)

        schedOptions(tgtTeam) = tgtId

        If tgtId = 0 Then
            'In case chosen team ends up with 0 value
            validTeam = 0
        End If

        If validTeam = 1 Then
            Call TEAMSETUP(tgtTeam)
        End If

    Next tgtTeam

    If validTeam = 1 And tourneyPlay = 1 Then

        'Read in tournament settings
        tournFileNoExt$ = Mid$(tourneyFile$, 1, Len(tourneyFile$) - 4)
        Open diskPaths$(1) + tournFileNoExt$ + ".INF" For Input As #1

        Input #1, X$

        Input #1, X$ ' NUMBER OF REGIONS

        For I = 1 To 16
            Input #1, X$ ' NUMBER OF TEAMS IN REGION
            Input #1, X$ ' NAME OF REGION
        Next

        For I = 1 To 16
            For I1 = 1 To 16
                For I2 = 1 To 4
                    Input #1, seedSlotName$(I, I1, I2) ' SEED SLOT NAME
                    Input #1, tourneySettings(I, I1, I2) ' REGION, SEED NUMBER, MODE PLAY/TM#
                Next
            Next
        Next

        Close #1

        tourneyPlay = 0

        'Check if there are more games to play
        For I = 1 To 16

            For I1 = 1 To 16

                If schedOptions(0) = tourneySettings(I, I1, 0) And diskIDs$(0) = seedSlotName$(I, I1, 1) And tourneySettings(I, I1, 1) = 0 Then
                    tourneyPlay = 1
                    exitAll% = 1
                    Exit For
                End If

                If schedOptions(1) = tourneySettings(I, I1, 0) And diskIDs$(1) = seedSlotName$(I, I1, 1) And tourneySettings(I, I1, 1) = 0 Then
                    tourneyPlay = 1
                    exitAll% = 1
                    Exit For
                End If
            Next

            If exitAll% = 1 Then Exit For

        Next

    End If 'Done checking for tournament

End Sub


'----------------------------------------
'       LetterToNumber Subroutine
'----------------------------------------
'This subroutine converts an alphabetic
'character to a numeric equivalent,
'where A = 10, B = 11, etc.
'This is useful for long menus that have
'more than 10 options (0 - 9) and a single
'key is desired for a choice.
Sub LetterToNumber (promptVal$, J)

    J = Val(promptVal$)

    Select Case UCase$(promptVal$)

        Case "A": J = 10
        Case "B": J = 11
        Case "C": J = 12
        Case "D": J = 13
        Case "E": J = 14
        Case "F": J = 15
        Case "G": J = 16
        Case "H": J = 17

    End Select

End Sub


'----------------------------------------
'          TEAMSETUP Subroutine
'----------------------------------------
'This subroutine initializes team data,
'sets up player data and configures any
'league adjustments
Sub TEAMSETUP (teamIdx)

    Shared d3FG_Adj(), dthreeFGA_Adj()

    ' *** This is where we "adjust" values that were in the 90's

    'Off Steal Rating
    teamAdj(teamIdx, 4) = leagRat_GAME(teamIdx, 0)
    If teamAdj(teamIdx, 4) > 20 Then teamAdj(teamIdx, 4) = teamAdj(teamIdx, 4) - 100

    'Def TO Adj
    teamAdj(teamIdx, 6) = leagRat_GAME(teamIdx, 2)
    If teamAdj(teamIdx, 6) > 20 Then teamAdj(teamIdx, 6) = teamAdj(teamIdx, 6) - 100

    'Def Foul Adj
    teamAdj(teamIdx, 7) = leagRat_GAME(teamIdx, 3)
    If teamAdj(teamIdx, 7) > 20 Then teamAdj(teamIdx, 7) = teamAdj(teamIdx, 7) - 100

    'Off T/O Rating
    teamAdj(teamIdx, 5) = leagRat_GAME(teamIdx, 1)

    'Default to 4 if not provided
    If teamAdj(teamIdx, 5) = 0 Then teamAdj(teamIdx, 5) = 4

    '"99" Value
    teamAdj(teamIdx, 8) = leagRat_GAME(teamIdx, 4)
    If teamAdj(teamIdx, 8) <> 99 Then
        teamAdj(teamIdx, 4) = 0
        teamAdj(teamIdx, 5) = 4
        teamAdj(teamIdx, 6) = 0
        teamAdj(teamIdx, 7) = 0
    End If

    'Ratings based on player contributions
    For I = 0 To 13

        'Foul Draw Rating
        plyrOff_GAME!(teamIdx, I, 16) = plyrRat_GAME(teamIdx, I, 6)
        If plyrOff_GAME!(teamIdx, I, 16) = 0 Then
            plyrOff_GAME!(teamIdx, I, 16) = 6
        End If

        'Foul Commit Rating
        plyrOff_GAME!(teamIdx, I, 17) = plyrRat_GAME(teamIdx, I, 5)
        If plyrOff_GAME!(teamIdx, I, 17) = 0 Then
            plyrOff_GAME!(teamIdx, I, 17) = 40
        End If

        '3FGM / 3FGA
        plyrOff_GAME!(teamIdx, I, 20) = plyrRat_GAME(teamIdx, I, 2)

        '3FGA / FGA
        plyrOff_GAME!(teamIdx, I, 19) = plyrRat_GAME(teamIdx, I, 3)

        'Based on ORB / TRB
        plyrOff_GAME!(teamIdx, I, 21) = plyrRat_GAME(teamIdx, I, 4)

        'Based on TRB / Game
        plyrOff_GAME!(teamIdx, I, 2) = plyrOff_GAME!(teamIdx, I, 2)

    Next I

    teamAdj(teamIdx, 2) = d3FG_Adj(teamIdx)
    If teamAdj(teamIdx, 2) > 20 Then
        teamAdj(teamIdx, 2) = teamAdj(teamIdx, 2) - 100
    End If

    teamAdj(teamIdx, 3) = dthreeFGA_Adj(teamIdx)
    If teamAdj(teamIdx, 3) > 20 Then
        teamAdj(teamIdx, 3) = teamAdj(teamIdx, 3) - 100
    End If

End Sub


'----------------------------------------
'        TimeoutCalled Subroutine
'----------------------------------------
'This subroutine handles the PBP and
'variable values for when a team takes
'a timeout. This includes resetting the
'pressure and fatigue ratings.
Sub TimeoutCalled (chargeTeam)

    prevBall$ = ""

    If timeouts(chargeTeam) > 0 Then

        timeouts(chargeTeam) = timeouts(chargeTeam) - 1
        Call SCOREBOARD

        'Offense
        Call Rolld100(RN0)
        If RN0 <= 8 Then
            ft3PtFoul(chargeTeam) = 10
        End If
        If RN0 > 8 And RN0 <= 16 Then
            ft3PtFoul(chargeTeam) = 5
        End If

        'Defense
        Call Rolld100(RN1)
        If RN1 <= 8 Then
            ft3PtFoul(1 - chargeTeam) = -10
        End If
        If RN1 > 8 And RN1 <= 16 Then
            ft3PtFoul(1 - chargeTeam) = -5
        End If

        'Restore time on clock
        Call AdjustClockTime(-clockSecs)
        Call AdjustPlayTime

        If sClockVal >= 1 Then
            shotClock = shotClock + clockSecs
        End If

        Erase pressureRating

        pbpString$ = "TIME OUT " + gameTeams$(chargeTeam)
        Call SELECTPBP(chargeTeam, 1)
        Call DELAY

        Call DEADBALLOPTIONS

        For I = 0 To 1
            For I1 = 0 To 13
                tmFatigue(I, I1) = tmFatigue(I, I1) + 1
                If tmFatigue(I, I1) > 0 Then
                    tmFatigue(I, I1) = 0
                End If
            Next
        Next

    End If

End Sub


'----------------------------------------
'           TEAMBOX Subroutine
'----------------------------------------
'This subroutine draws an ASCII box using
'team colors. The box starts at row 6 and
'is approx. 60 char's wide. When done, the
'cursor is set at the first printable row
'and column inside the box.
Sub TEAMBOX (boxHeight, teamIdx)

    Locate 6, 8
    Color teamRat_GAME(teamIdx, 0), teamRat_GAME(teamIdx, 1)

    'Print box top
    Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";

    'Print box sides
    For II = 1 To boxHeight
        Locate 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
    Next

    'Print box bottom
    Locate 7 + boxHeight, 8
    Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";

    Locate 7, 11

End Sub


'----------------------------------------
'         FRAMEOPTION Subroutine
'----------------------------------------
'This subroutine draws an ASCII box using
'team colors. The box starts at row 8 and
'is approx. 60 char's wide. When done, the
'cursor is set at the first printable row
'and column inside the box.
Sub FRAMEOPTION (boxHeight, teamIdx)

    PCopy 0, 1
    Screen , , 1, 1

    Locate 8, 8
    Color teamRat_GAME(teamIdx, 0), teamRat_GAME(teamIdx, 1)

    'Print box top
    Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

    'Print box sides
    For II = 1 To boxHeight
        Locate 8 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
    Next

    'Print box bottom
    Locate 9 + boxHeight, 8
    Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

    Locate 9, 12

End Sub


'----------------------------------------
'            d100 Subroutine
'----------------------------------------
'This subroutine assigns a random number
'between 1 and 100 to the variable that
'it is called with
Sub Rolld100 (randNumVar)

    randNumVar = Int(Rnd(1) * 100) + 1

End Sub


'----------------------------------------
'            d100 Subroutine
'----------------------------------------
'This subroutine assigns a random number
'between 1 and 100 to the variable that
'it is called with
Function d100% ()

    d100% = Int(Rnd(1) * 100) + 1

End Function


'----------------------------------------
'       DISABLEPLAYERS Subroutine
'----------------------------------------
'This subroutine gives the user an inter-
'face for disabling team players. It will
'then take care of updating the associated
'arrays for the available roster.
Sub DISABLEPLAYERS (teamIdx)

    Do

        Color 15, 0
        Cls

        Call TEAMBOX(1, teamIdx)

        U$ = "DISABLE PLAYERS FOR " + gameTeams$(teamIdx) + "? (Y/N)"
        Locate , (80 - Len(U$)) / 2
        Print U$
        Color 15, 0

        Print
        Print
        Print "NOTE:: THE COMPUTER WILL AUTOMATICALLY DISABLE PLAYERS BASED ON"
        Print "       THE RATIO OF GAMES PLAYED FOR EACH PLAYER TO THE TOTAL"
        Print "       NUMBER OF GAMES PLAYED BY THE TEAM. YOU MAY WANT TO CHECK"
        Print "       TO ACTIVATE ANY COMPUTER DEACTIVATED PLAYERS"
        I$ = GetKeyPress$

    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    If UCase$(I$) = "Y" Then

        DX% = 0

        Cls
        Print "TEAM ROSTER FOR ";
        Color teamRat_GAME(teamIdx, 0), teamRat_GAME(teamIdx, 1): Print gameTeams$(teamIdx)
        Color 15, 0
        Print "# NAME            POS   GP"

        For I = 0 To 13

            If I <= 9 Then Print Using "#"; I;
            If I = 10 Then Print "A";
            If I = 11 Then Print "B";
            If I = 12 Then Print "C";
            If I = 13 Then Print "D";
            If I = 14 Then Print "E";

            Print " ";

            'Automatically disable "non-existent" players
            If players$(teamIdx, I) = "XXX" Then
                rosterStatus(teamIdx, I) = -1
                Color teamRat_GAME(teamIdx, 0), teamRat_GAME(teamIdx, 1)
            End If

            'Color players who have been disabled
            If rosterStatus(teamIdx, I) = 1 Or rosterStatus(teamIdx, I) = -1 Then
                Color teamRat_GAME(teamIdx, 0), teamRat_GAME(teamIdx, 1)
            End If

            'Print player name
            Print players$(teamIdx, I);
            Color 15, 0

            'Print player position, GP
            Print Tab(20); positions_GAME$(teamIdx, I); Tab(22); plyrRat_GAME(teamIdx, I, 0)

        Next

        Do
            Locate 19, 1
            Print "ENTER PLAYER # TO DISABLE/ENABLE (Z=FINISHED)"
            Print "YOU MUST HAVE AT LEAST 8 ACTIVE PLAYERS"

            I$ = GetKeyPress$

            If UCase$(I$) <> "Z" Then

                I1 = Val(I$)
                Call LetterToNumber(I$, I1)

                'Player is active
                If rosterStatus(teamIdx, I1) = 0 And DX% < 4 Then
                    rosterStatus(teamIdx, I1) = -1
                    DX% = DX% + 1
                    Locate 3 + I1, 3
                    Color teamRat_GAME(teamIdx, 0), teamRat_GAME(teamIdx, 1): Print players$(teamIdx, I1)
                    Color 15, 0
                Else
                    If rosterStatus(teamIdx, I1) = -1 Then
                        rosterStatus(teamIdx, I1) = 0
                        DX% = DX% - 1
                        Locate 3 + I1, 2
                        Color 15, 0
                        Print " "; players$(teamIdx, I1)
                    End If
                End If
            End If

        Loop Until UCase$(I$) = "Z"

    End If

End Sub


'----------------------------------------
'            DELAY Subroutine
'----------------------------------------
'This subroutine simply serves as a place-
'holder for the command to initiate the
'pbp delay.
Sub DELAY ()

    _Delay pbpDelay!

End Sub


'----------------------------------------
'             PBP Subroutine
'----------------------------------------
'This is the classic Haffner Play-by-play
'subroutine. Given the target text, it will
'place it in the correct spot in the pbp
'text window. No text is placed if the
'delay is 0.
Sub PBP (teamIdx)

    M$ = InKey$

    If UCase$(M$) = "O" Then
        Call OPTIONS
    End If

    If UCase$(M$) = "Q" Then
        Cls
        End
    End If

    'Positive delay OR Verbose PBP
    If (autoPlay = 0 Or pbpDelay! > 0) And pbpOpt = 0 Then

        pbpString$ = PadRight$(pbpString$, 38)
        'Print #11, pbpString$

        Locate 8, 1

        Select Case nbrLines

            Case 1
                A1$ = pbpString$

            Case 2
                B1$ = pbpString$

            Case 3
                C1$ = pbpString$

            Case 4
                D1$ = pbpString$

            Case 5
                E1$ = pbpString$

            Case 6
                F1$ = pbpString$

            Case 7
                G1$ = pbpString$

            Case 8
                H1$ = pbpString$

            Case Is > 8
                A1$ = B1$
                B1$ = C1$
                C1$ = D1$
                D1$ = E1$
                E1$ = F1$
                F1$ = G1$
                G1$ = H1$
                H1$ = pbpString$

                For I = 1 To 7
                    pbpFG(I) = pbpFG(I + 1)
                    pbpBG(I) = pbpBG(I + 1)
                Next

        End Select

        If nbrLines < 8 Then
            pbpFG(nbrLines) = teamRat_GAME(teamIdx, 0)
            pbpBG(nbrLines) = teamRat_GAME(teamIdx, 1)
        Else
            pbpFG(8) = teamRat_GAME(teamIdx, 0)
            pbpBG(8) = teamRat_GAME(teamIdx, 1)
        End If

        nbrLines = nbrLines + 1

        Color pbpFG(1), pbpBG(1): Print Left$(A1$, 38)
        Color pbpFG(2), pbpBG(2): Print Left$(B1$, 38)
        Color pbpFG(3), pbpBG(3): Print Left$(C1$, 38)
        Color pbpFG(4), pbpBG(4): Print Left$(D1$, 38)
        Color pbpFG(5), pbpBG(5): Print Left$(E1$, 38)
        Color pbpFG(6), pbpBG(6): Print Left$(F1$, 38)
        Color pbpFG(7), pbpBG(7): Print Left$(G1$, 38)
        Color pbpFG(8), pbpBG(8): Print Left$(H1$, 38)

        Color 15, 0
        Call DELAY

    End If

End Sub


'----------------------------------------
'          SELECTPBP Subroutine
'----------------------------------------
'This subroutine essentially handles the
'prep for PBP depending on the style that
'was chosen for the game (classic / verbose)
Sub SELECTPBP (teamIdx, clean)

    Select Case pbpOpt

        Case 0:
            Call PBP(teamIdx)

        Case 1:
            If pbpDelay! > 0 Then
                If clean = 1 Then Call WINDEX
                Print pbpString$
                Call DELAY
            End If

    End Select

End Sub


'----------------------------------------
'           MAKEPBP Subroutine
'----------------------------------------
'This subroutine helps build strings for
'the Play-by-Play text, depending on the
'context given (assigned as pbpType)
Sub MAKEPBP (pbpType)

    '--> only set this if we need to track where code is going
    'If autoPlay = 1 Then
    If (autoPlay = 0 Or pbpDelay! > 0) Then

        Call Rolld100(RN0)

        Select Case pbpType

            Case 1
                'TURNOVER
                pbpString$ = players$(P, lineupIdx(P, ballCarrier))

                If RN0 > 35 And RN0 <= 68 And defCategory(D) > 1 And passes = 1 Then

                    pbpString$ = gameTeams$(P) + " CALLED FOR 10 SEC VIOLATION"

                Else

                    RN1 = Int(Rnd(1) * 14) + 1

                    Select Case RN1

                        Case 1
                            pbpString$ = pbpString$ + " TURNS THE BALL OVER"
                        Case 2
                            pbpString$ = pbpString$ + " THROWS IT AWAY"
                        Case 3
                            pbpString$ = pbpString$ + " IS WHISTLED FOR TRAVELING"
                        Case 4
                            pbpString$ = pbpString$ + " TURNS IT OVER"
                        Case 5
                            pbpString$ = pbpString$ + " LOSES CONTROL OF THE BALL"
                        Case 6
                            pbpString$ = pbpString$ + " LOSES IT OUT OF BOUNDS"
                        Case 7
                            pbpString$ = pbpString$ + " LOSES THE HANDLE"
                        Case 8
                            pbpString$ = pbpString$ + " LOSES IT"
                        Case 9
                            pbpString$ = pbpString$ + " IS CALLED FOR STEPS"
                        Case 10
                            pbpString$ = pbpString$ + " TRAVELS"
                        Case 11
                            pbpString$ = pbpString$ + " WALKS WITH IT"
                        Case 12
                            pbpString$ = pbpString$ + " LETS IT GET AWAY"
                        Case 13
                            pbpString$ = pbpString$ + " DOUBLE DRIBBLES"
                        Case 14
                            pbpString$ = pbpString$ + " LOSES CONTROL"

                    End Select

                End If

            Case 2
                'DUNK
                RN1 = Int(Rnd(1) * 39) + 1

                Select Case RN1

                    Case 1
                        pbpString$ = "DIPSY-DOO DUNK-A-ROO!!!"
                    Case 2
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " MAKES IT HAMMER TIME!!!"
                    Case 3
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " SLAMS IT IN!!!"
                    Case 4
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " WITH THE 2-HANDED SLAM!!!"
                    Case 5
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " SLAMS IT HOME!!!"
                    Case 6
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " JAMS IT HOME!!!"
                    Case 7
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " THROWS IT DOWN!!!"
                    Case 8
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " THUNDERSLAM!!!"
                    Case 9
                        pbpString$ = "RIM ROCKING JAM!!!"
                    Case 10
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " WITH THE MONSTER SLAM!!"
                    Case 11
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + "POWER SLAMS IT!!!"
                    Case 12
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " WITH THE 2-HANDED STUFshotChance!!!"
                    Case 13
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " BACKWARD!S JAM!!!"
                    Case 14
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " WITH THE 1-HANDED SLAM!!!"
                    Case 15
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " WITH THE REVERSE SLAM!!!"
                    Case 16
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " WITH THE REVERSE JAM!!!"
                    Case 17
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " WITH THE WINDMILL JAM!!!"
                    Case 18
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " WITH THE WINDMILL SLAM!!!"
                    Case 19
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " WITH THE TOMAHAWK SLAM!!!"
                    Case 20
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " WITH THE 360 SLAM!!!"
                    Case 21
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " WITH THE SPINNING SLAM!!!"
                    Case 22
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " WITH THE LEFT HANDED DUNK!!!"
                    Case 23
                        pbpString$ = "DOUBLE CLUTCH JAM!!!"
                    Case 24
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " GETS THE FLUSH!!!"
                    Case 25
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " PUNCHES IT HOME!!!"
                    Case 26
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + "...DOUBLE PUMP SLAM!!!"
                    Case 27
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + "...IN YOUR FACE SLAM!!!"
                    Case 28
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " IS UP"
                        Call PBP(P)
                        pbpString$ = teamGender$(P) + " SPINS"
                        Call PBP(P)
                        pbpString$ = "BEHIND THE BACK SLAM!!!"
                    Case 29
                        pbpString$ = teamGender$(P) + " SLAMS IT HOME!!"
                    Case 30
                        pbpString$ = teamGender$(P) + " LAYS IT UP AND IN!!"
                    Case 31
                        pbpString$ = teamGender$(P) + " BANGS IT IN!!"
                    Case 32
                        pbpString$ = teamGender$(P) + " RAMS IT IN!!"
                    Case 33
                        pbpString$ = teamGender$(P) + " SLAMS IT IN!!!"
                    Case 34
                        pbpString$ = "DIPSY-DOO DUNK-A-ROO!!!"
                    Case 35
                        pbpString$ = teamGender$(P) + " MAKES IT HAMMER TIME!!!"
                    Case 36
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " WITH THE 2-HANDED SLAM!!!"
                    Case 37
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " IS UP"
                        Call PBP(P)
                        pbpString$ = teamGender$(P) + " SPINS"
                        Call PBP(P)
                        pbpString$ = "BEHIND THE BACK SLAM!!!"
                    Case 38
                        pbpString$ = teamGender$(P) + " JAMS IT HOME!!!"
                    Case 39
                        pbpString$ = teamGender$(P) + " HAMMERS IT HOME!"

                End Select

            Case 3
                'SHOT
                pbpString$ = players$(P, lineupIdx(P, ballCarrier))

                Select Case shotType

                    Case 1
                        'BAD
                        Select Case RN0

                            Case 1 To 40
                                pbpString$ = pbpString$ + " TAKES A TOUGH SHOT"
                            Case 40 To 45
                                pbpString$ = pbpString$ + " SHOOTS OFF BALANCE"
                            Case 46 To 50
                                pbpString$ = pbpString$ + " FORCES ONE UP"
                            Case 51 To 60
                                pbpString$ = pbpString$ + " SHOOTS UNDER PRESSURE"
                            Case 61 To 65
                                pbpString$ = pbpString$ + " WITH A BAD SHOT"
                            Case 66 To 70
                                pbpString$ = pbpString$ + " SHOOTS IN HEAVY TRAFFIC"
                            Case 71 To 75
                                pbpString$ = pbpString$ + " THROWS UP A PRAYER"
                            Case 76 To 80
                                pbpString$ = pbpString$ + " TAKES A BAD SHOT"
                            Case 81 To 85
                                pbpString$ = pbpString$ + " FORCES THE INSIDE SHOT"
                            Case 86 To 100
                                pbpString$ = pbpString$ + " TURNS AND SHOOTS"

                        End Select

                    Case 2
                        'INSIDE W/ASSIST
                        checkForDunk = 0

                        Select Case RN0

                            Case 1 To 10
                                pbpString$ = teamGender$(P) + " CUTS TO THE BASKET"
                                Call PBP(P)
                                pbpString$ = "TAKES IT UP"
                                checkForDunk = 1

                            Case 11 To 20
                                pbpString$ = teamGender$(P) + " TAKES IT TO THE HOLE"
                                checkForDunk = 1

                            Case 21 To 30
                                pbpString$ = teamGender$(P) + " TAKES IT TO THE HOOP"
                                checkForDunk = 1

                            Case 31 To 40
                                pbpString$ = teamGender$(P) + " LEANS IN AND SHOOTS"

                            Case 41 To 50
                                pbpString$ = teamGender$(P) + " PUTS IT UP"

                            Case 51 To 60
                                pbpString$ = teamGender$(P) + " TAKES IT TO THE BOARD!"
                                checkForDunk = 1

                            Case 61 To 70
                                pbpString$ = teamGender$(P) + " PUTS IT OFF THE GLASS"

                            Case 71 To 80
                                If ballCarrier = 4 Then
                                    'Center
                                    pbpString$ = teamGender$(P) + " SHOOTS THE HOOK"
                                Else
                                    pbpString$ = teamGender$(P) + " GOES IN FOR THE LAYUP"
                                End If

                            Case 81 To 90
                                pbpString$ = teamGender$(P) + " TAKES IT UP STRONG"
                                checkForDunk = 1

                            Case 91 To 95
                                pbpString$ = teamGender$(P) + " POWERS IT UP"
                                checkForDunk = 1

                            Case 96 To 100
                                pbpString$ = teamGender$(P) + " LAYS IT UP"

                        End Select

                        If checkForDunk = 1 Then

                            'Frequency of dunk based on position

                            'Guards rarely dunk; this is the only place they do
                            'In women's basketball they dunk even less often
                            '-- "1%" factor = 15
                            If (ballCarrier = 0 Or ballCarrier = 1) Then
                                '-- was 18
                                If (teamGender$(P) = "SHE" And d100% <= 22) Or (teamGender$(P) = "HE" And d100% <= 45) Then
                                    willDunk = 1
                                End If
                            End If

                            'Forwards are very likely
                            '-- "1%" factor = 10
                            If (ballCarrier = 2 Or ballCarrier = 3) And d100% <= 30 Then
                                willDunk = 1
                            End If

                            'Centers are most likely to dunk, but rarely reach here
                            '-- "1%" factor = 5
                            If ballCarrier = 4 Then
                                '-- was 18
                                If (teamGender$(P) = "SHE" And d100% <= 22) Or (teamGender$(P) = "HE" And d100% <= 15) Then
                                    'Centers dunk slightly more often in women's basketball
                                    willDunk = 1
                                End If
                            End If

                        End If

                    Case 3
                        'PENETRATION
                        checkForDunk = 0

                        Select Case ballCarrier

                            Case 0, 1

                                Select Case RN0

                                    Case 1 To 25
                                        pbpString$ = teamGender$(P) + " SPINS FREE"
                                        Call PBP(P)
                                        pbpString$ = "SHOOTS"

                                    Case 26 To 50
                                        pbpString$ = teamGender$(P) + " TAKES IT UP UNDER THE BASKET"
                                        checkForDunk = 1

                                    Case 51 To 52
                                        pbpString$ = teamGender$(P) + " MAKES A NICE SPIN MOVE"
                                        Call PBP(P)
                                        pbpString$ = "AND SOARS TO THE HOOP"
                                        checkForDunk = 1

                                    Case 53 To 54
                                        pbpString$ = teamGender$(P) + " BLOWS PAST TWO DEFENDERS"

                                    Case 55 To 56
                                        pbpString$ = teamGender$(P) + " DRIVES THE BASELINE"
                                        Call PBP(P)
                                        pbpString$ = "SPINS RIGHT AND GOES UP"
                                        checkForDunk = 1

                                    Case 57
                                        pbpString$ = teamGender$(P) + " SLICES IN THE HOLE"
                                        Call PBP(P)
                                        pbpString$ = "UNDERHAND SCOOP SHOT"

                                    Case 58
                                        pbpString$ = "STRONG CUT ALONG THE BASELINE"
                                        Call PBP(P)
                                        pbpString$ = "DOUBLE PUMP REVERSE LAYUP"

                                    Case 59
                                        pbpString$ = teamGender$(P) + " PENETRATES"
                                        Call PBP(P)
                                        pbpString$ = "GORGEOUS FINGER ROLL!"

                                    Case 60 To 75
                                        pbpString$ = teamGender$(P) + " TAKES THE JUMPER"

                                    Case 76 To 90
                                        pbpString$ = teamGender$(P) + " STOPS"
                                        Call PBP(P)
                                        pbpString$ = "SHOOTS"

                                    Case 91
                                        pbpString$ = teamGender$(P) + " LOOKS TO PASS"
                                        Call PBP(P)
                                        pbpString$ = "NO ONE IS OPEN"
                                        Call PBP(P)
                                        pbpString$ = teamGender$(P) + " PUTS IT UP"

                                    Case 92 To 100
                                        pbpString$ = teamGender$(P) + " TAKES THE SHOT"

                                End Select

                                '-- "1%" factor = 2
                                If checkForDunk = 1 And d100% <= 6 Then
                                    willDunk = 1
                                End If

                            Case 2, 3

                                checkForDunk = 0

                                Select Case RN0

                                    Case 1 To 16
                                        pbpString$ = teamGender$(P) + " TAKES IT UP"
                                        checkForDunk = 1

                                    Case 17 To 32
                                        pbpString$ = teamGender$(P) + " SPINS FREE"
                                        Call PBP(P)
                                        pbpString$ = "SHOOTS"
                                        checkForDunk = 1

                                    Case 33 To 48
                                        pbpString$ = teamGender$(P) + " GETS LOOSE IN THE PAINT"
                                        Call PBP(P)
                                        pbpString$ = "TAKES IT UP"
                                        checkForDunk = 1

                                    Case 49 To 64
                                        pbpString$ = teamGender$(P) + " PUTS IT UP"

                                    Case 65 To 67
                                        pbpString$ = teamGender$(P) + " CURLS OFF A PICK IN THE PAINT"
                                        Call PBP(P)
                                        pbpString$ = "TAKES IT UP"
                                        checkForDunk = 1

                                    Case 68, 69
                                        pbpString$ = teamGender$(P) + " MAKES A NICE SPIN MOVE"
                                        Call PBP(P)
                                        pbpString$ = "AND SOARS TO THE HOOP"
                                        checkForDunk = 1

                                    Case 70, 71
                                        pbpString$ = teamGender$(P) + " DRIVES THE BASELINE"
                                        Call PBP(P)
                                        pbpString$ = "SPINS RIGHT AND GOES UP"
                                        checkForDunk = 1

                                    Case 72
                                        pbpString$ = teamGender$(P) + " SLICES IN THE HOLE"
                                        Call PBP(P)
                                        pbpString$ = "UNDERHAND SCOOP SHOT"

                                    Case 73
                                        pbpString$ = "STRONG CUT ALONG THE BASELINE"
                                        Call PBP(P)
                                        pbpString$ = "DOUBLE PUMP REVERSE LAYUP"

                                    Case 74
                                        pbpString$ = teamGender$(P) + " PENETRATES"
                                        Call PBP(P)
                                        pbpString$ = "GORGEOUS FINGER ROLL!"

                                    Case 75 To 80
                                        pbpString$ = teamGender$(P) + " SHOOTS"

                                    Case 81 To 100
                                        pbpString$ = teamGender$(P) + " TAKES THE SHOT"

                                End Select

                                '-- "1%" factor = 5
                                If checkForDunk = 1 And d100% <= 15 Then
                                    willDunk = 1
                                End If

                            Case 4

                                Select Case RN0

                                    Case 1 To 15
                                        pbpString$ = teamGender$(P) + " PUTS IT OFF THE GLASS"

                                    Case 16 To 25
                                        pbpString$ = teamGender$(P) + " PIVOTS TO THE BASKET"

                                    Case 26 To 40
                                        pbpString$ = teamGender$(P) + " POWERS IT UP"

                                    Case 41 To 58
                                        pbpString$ = teamGender$(P) + " TAKES IT TO THE HOOP"
                                        checkForDunk = 1

                                    Case 59 To 70
                                        pbpString$ = teamGender$(P) + " TAKES THE SHOT"

                                    Case 71, 72
                                        pbpString$ = teamGender$(P) + " CURLS OFF A PICK IN THE PAINT"
                                        Call PBP(P)
                                        pbpString$ = "TAKES IT UP"
                                        checkForDunk = 1

                                    Case 73, 74
                                        pbpString$ = teamGender$(P) + " MAKES A NICE SPIN MOVE"
                                        Call PBP(P)
                                        pbpString$ = "AND SOARS TO THE HOOP"
                                        checkForDunk = 1

                                    Case 75 To 100
                                        pbpString$ = teamGender$(P) + " TAKES IT UP STRONG"
                                        checkForDunk = 1

                                End Select

                                '-- "1%" factor = 10
                                If checkForDunk = 1 And d100% <= 30 Then
                                    willDunk = 1
                                End If

                        End Select

                    Case 4
                        'THREE!
                        If forcedThree = 1 Then

                            If RN0 <= 50 Then
                                pbpString$ = pbpString$ + " THROWS UP A PRAYER"
                            Else
                                pbpString$ = pbpString$ + " LAUNCHES A THREE"
                            End If

                            forcedThree = 0

                        Else

                            Select Case RN0

                                Case 1 To 10
                                    pbpString$ = pbpString$ + " TRIES FOR THREE!"
                                Case 11 To 20
                                    pbpString$ = pbpString$ + " SHOOTS FROM 3-POINT RANGE!"
                                Case 21 To 30
                                    pbpString$ = pbpString$ + " HOISTS A THREE!"
                                Case 31 To 40
                                    pbpString$ = pbpString$ + " LOBS A THREE FROM THE CORNER!"
                                Case 41 To 50
                                    pbpString$ = pbpString$ + " FIRES FROM THE 3-POINT LINE!"
                                Case 51 To 60
                                    pbpString$ = pbpString$ + " SHOOTS FOR THREE!"
                                Case 61 To 70
                                    pbpString$ = pbpString$ + " SHOOTS THE 3-POINTER!"
                                Case 71 To 80
                                    pbpString$ = pbpString$ + " FOR 3...!"
                                Case 81 To 90
                                    pbpString$ = pbpString$ + " PUTS UP THE TREY!"
                                Case 91 To 100
                                    pbpString$ = pbpString$ + " TAKES THE 3-POINTER!"

                            End Select

                        End If

                    Case 5
                        'SHORT JUMP
                        willDunk = 0

                        RN1 = Int(Rnd(1) * 59) + 1

                        Select Case RN1

                            Case 1
                                pbpString$ = pbpString$ + " WITH A TWISTING JUMPER"
                            Case 2
                                pbpString$ = pbpString$ + " SHOOTS THE J"
                            Case 3
                                If ballCarrier = 4 Then
                                    'Center
                                    pbpString$ = pbpString$ + " RELEASES THE HOOK"
                                Else
                                    pbpString$ = pbpString$ + " TAKES THE SHOT"
                                End If
                            Case 4
                                pbpString$ = pbpString$ + " WITH A SHORT JUMPER"
                            Case 5
                                pbpString$ = pbpString$ + " WITH THE 10 FOOTER"
                            Case 6
                                If ballCarrier = 4 Then
                                    'Center
                                    pbpString$ = pbpString$ + " WITH THE RUNNING HOOK"
                                Else
                                    pbpString$ = pbpString$ + " WITH THE RUNNER"
                                End If

                            Case 7
                                If ballCarrier = 4 Then
                                    'Center
                                    pbpString$ = pbpString$ + " WITH A SWEEPING HOOK"
                                Else
                                    pbpString$ = pbpString$ + " WORKS FREE AND SHOOTS"
                                End If

                            Case 8
                                If ballCarrier = 4 Then
                                    'Center
                                    pbpString$ = pbpString$ + " WITH A BABY HOOK SHOT"
                                Else
                                    pbpString$ = pbpString$ + " TAKES THE SHOT"
                                End If

                            Case 9
                                pbpString$ = pbpString$ + " LEANS IN AND SHOOTS"
                            Case 10
                                pbpString$ = pbpString$ + " SHOOTS"
                            Case 11
                                pbpString$ = pbpString$ + " SHOOTS FALLING AWAY"
                            Case 12
                                pbpString$ = pbpString$ + " LEANS IN FOR THE SHOT"
                            Case 13
                                pbpString$ = pbpString$ + " TAKES THE LEFT HANDER"
                            Case 14
                                pbpString$ = pbpString$ + " HARD! TO THE HOLE..."
                            Case 15
                                pbpString$ = pbpString$ + " FLIPS IT UP"
                            Case 16
                                pbpString$ = pbpString$ + " FORCES THE INSIDE SHOT"
                            Case 17
                                pbpString$ = pbpString$ + " CUTS TO THE BASKET"
                            Case 18
                                pbpString$ = pbpString$ + " TAKES IT IN THE PAINT"
                            Case 19
                                pbpString$ = pbpString$ + " FLOATS IT OVER "
                                If teamGender$(P) = "HE" Then
                                    pbpString$ = pbpString$ + "HIS MAN"
                                Else
                                    pbpString$ = pbpString$ + "HER DEFENDER"
                                End If

                            Case 20
                                pbpString$ = pbpString$ + " TAKES "
                                If teamGender$(P) = "HE" Then
                                    pbpString$ = pbpString$ + "HIS MAN INSIDE"
                                Else
                                    pbpString$ = pbpString$ + "HER DEFENDER INSIDE"
                                End If

                            Case 21
                                pbpString$ = pbpString$ + " WHEELS IN THE LANE"
                            Case 22
                                pbpString$ = pbpString$ + " CUTS INSIDE FOR THE SHOT"
                            Case 23
                                pbpString$ = pbpString$ + " PENETRATES AND SHOOTS"
                            Case 24
                                pbpString$ = pbpString$ + " SLICES INSIDE"
                            Case 25
                                pbpString$ = pbpString$ + " LOOKS FOR TWO"
                            Case 26
                                pbpString$ = pbpString$ + " DRIVES INTO THE PAINT"
                            Case 27
                                pbpString$ = pbpString$ + " HAS THE BASELINE JUMPER"
                            Case 28
                                pbpString$ = pbpString$ + " GLIDES TO THE HOLE"
                            Case 29
                                pbpString$ = pbpString$ + " GETS INSIDE FOR THE SHOT"
                            Case 30
                                pbpString$ = pbpString$ + " SPINS TOWARD! THE HOOP"
                            Case 31
                                pbpString$ = pbpString$ + " FROM SHORT RANGE"
                            Case 32
                                pbpString$ = pbpString$ + " GOES BASELINE"
                            Case 33
                                pbpString$ = pbpString$ + " PENETRATES AND SHOOTS"
                            Case 34
                                pbpString$ = pbpString$ + " FROM 8 FEET"
                            Case 35
                                pbpString$ = pbpString$ + " FROM 6 FEET"
                            Case 36
                                pbpString$ = pbpString$ + " FROM 5 FEET"
                            Case 37
                                pbpString$ = pbpString$ + " DRIVES TO THE HOLE"
                            Case 38
                                pbpString$ = pbpString$ + " GLIDES TO THE HOLE"
                            Case 39
                                pbpString$ = pbpString$ + " GETS INSIDE FOR THE SHOT"
                            Case 40
                                pbpString$ = pbpString$ + " GOES TO THE HOLE"
                            Case 41
                                pbpString$ = pbpString$ + " CUTS INSIDE"
                            Case 42
                                pbpString$ = pbpString$ + " SPINS INSIDE"
                            Case 43
                                pbpString$ = pbpString$ + " BREAKS INSIDE"
                            Case 44
                                pbpString$ = pbpString$ + " SNEAKS INSIDE"
                            Case 45
                                pbpString$ = pbpString$ + " SCOOTS INSIDE"
                            Case 46
                                pbpString$ = pbpString$ + " WORKS IT INSIDE"
                            Case 47
                                pbpString$ = pbpString$ + " KNIFES INSIDE"
                            Case 48
                                pbpString$ = pbpString$ + " SCISSORS THROUGH"
                            Case 49
                                pbpString$ = pbpString$ + " BANGS INSIDE"
                            Case 50
                                pbpString$ = pbpString$ + " SLICES INSIDE"
                            Case 51
                                pbpString$ = pbpString$ + " PUMPS AND SHOOTS"
                            Case 52
                                pbpString$ = pbpString$ + " DOUBLE PUMPS AND SHOOTS"
                            Case 53
                                pbpString$ = pbpString$ + "...ONE HANDED FADE AWAY"
                            Case 54
                                pbpString$ = pbpString$ + "...LEFT HANDED FADE AWAY"
                            Case 55
                                pbpString$ = pbpString$ + "...HIGH ARCHING SHOT"
                            Case 56
                                pbpString$ = pbpString$ + " WITH A RUNNING LEFT HANDER"
                            Case 57
                                pbpString$ = pbpString$ + " WITH A RUNNING SCOOP SHOT"
                            Case 58
                                pbpString$ = pbpString$ + " WITH THE SCOOP SHOT"
                            Case 59
                                pbpString$ = pbpString$ + " WITH THE FINGER ROLL"

                        End Select

                    Case 6
                        willDunk = 0

                        RN1 = Int(Rnd(1) * 59) + 1

                        Select Case RN1

                            Case 1
                                pbpString$ = pbpString$ + " POPS FROM OUTSIDE"
                            Case 2
                                pbpString$ = pbpString$ + " DECIDES TO SHOOT IT"
                            Case 3
                                pbpString$ = pbpString$ + " FIRES THE FADE-AWAY"
                            Case 4
                                pbpString$ = pbpString$ + " LETS IT FLY"
                            Case 5
                                pbpString$ = pbpString$ + " LAUNCHES ONE"
                            Case 6
                                pbpString$ = pbpString$ + " WITH THE OUTSIDE SHOT"
                            Case 7
                                pbpString$ = pbpString$ + " OUT OF THE CORNER"
                            Case 8
                                pbpString$ = pbpString$ + " PUTS IT UP FROM 18"
                            Case 9
                                pbpString$ = pbpString$ + " SPOTS UP OUTSIDE"
                            Case 10
                                pbpString$ = pbpString$ + " RELEASES THE JUMPER"
                            Case 11
                                pbpString$ = pbpString$ + " FROM THE TOP OF THE KEY"
                            Case 12
                                pbpString$ = pbpString$ + " FIRES FROM THE LEFT WING"
                            Case 13
                                pbpString$ = pbpString$ + " TRIES THE 12 FOOTER"
                            Case 14
                                pbpString$ = pbpString$ + " PULLS UP AND POPS"
                            Case 15
                                pbpString$ = pbpString$ + " FIRES FROM 15"
                            Case 16
                                pbpString$ = pbpString$ + " FROM THE PERIMETER"
                            Case 17
                                pbpString$ = pbpString$ + " TURNS AND SHOOTS"
                            Case 18
                                pbpString$ = pbpString$ + " SHOOTS OFF THE SCREEN"
                            Case 19
                                pbpString$ = pbpString$ + " FAKES AND SHOOTS"
                            Case 20
                                pbpString$ = pbpString$ + " FIRES AWAY"
                            Case 21
                                pbpString$ = pbpString$ + " POPS FROM THE BASELINE"
                            Case 22
                                pbpString$ = pbpString$ + " DRIVES THE LANE"
                            Case 23
                                pbpString$ = pbpString$ + " TAKES THE SHOT"
                            Case 24
                                pbpString$ = pbpString$ + " WORKS FREE AND FIRES"
                            Case 25
                                pbpString$ = pbpString$ + " SHOOTS OVER "

                                If teamGender$(P) = "HE" Then
                                    pbpString$ = pbpString$ + "HIS MAN"
                                Else
                                    pbpString$ = pbpString$ + "HER DEFENDER"
                                End If

                            Case 26
                                pbpString$ = pbpString$ + " SHOOTS FALLING AWAY"
                            Case 27
                                pbpString$ = pbpString$ + " FAKES AND SHOOTS"
                            Case 28
                                pbpString$ = pbpString$ + " FIRES AWAY"
                            Case 29
                                pbpString$ = pbpString$ + " POPS FROM THE BASELINE"
                            Case 30
                                pbpString$ = pbpString$ + " DRIVES THE LANE"
                            Case 31
                                pbpString$ = pbpString$ + " TAKES THE SHOT"
                            Case 32
                                pbpString$ = pbpString$ + " WORKS FREE AND FIRES"
                            Case 33
                                pbpString$ = pbpString$ + " SHOOTS FROM 15"
                            Case 34
                                pbpString$ = pbpString$ + " SHOOTS"
                            Case 35
                                pbpString$ = pbpString$ + " TAKES THE JUMPER"
                            Case 36
                                pbpString$ = pbpString$ + " FROM THE RIGHT CORNER"
                            Case 37
                                pbpString$ = pbpString$ + " FROM THE LEFT CORNER"
                            Case 38
                                pbpString$ = pbpString$ + " FROM THE KEY"
                            Case 39
                                pbpString$ = pbpString$ + " FIRES FROM OUTSIDE"
                            Case 40
                                pbpString$ = pbpString$ + " PUTS IT UP FROM 16"
                            Case 41
                                pbpString$ = pbpString$ + " FROM THE PERIMETER"
                            Case 42
                                pbpString$ = pbpString$ + " FAKES AND SHOOTS"
                            Case 43
                                pbpString$ = pbpString$ + " FROM AROUND 16 FEET"
                            Case 44
                                pbpString$ = pbpString$ + " PUTS IT UP"
                            Case 45
                                pbpString$ = pbpString$ + " WITH A RUNNING ONE-HANDER"
                            Case 46
                                pbpString$ = pbpString$ + " PULLS UP"
                            Case 47
                                pbpString$ = pbpString$ + " KISSES THE GLASS"
                            Case 48
                                pbpString$ = pbpString$ + " SPINS AND SHOOTS"
                            Case 49
                                pbpString$ = pbpString$ + " PUTS IT UP FROM 16"
                            Case 50
                                pbpString$ = pbpString$ + " WITH A LONG JUMPER"
                            Case 51
                                pbpString$ = pbpString$ + " PUMPS AND SHOOTS"
                            Case 52
                                pbpString$ = pbpString$ + " DOUBLE PUMPS AND SHOOTS"
                            Case 53
                                pbpString$ = pbpString$ + "...ONE HANDED FADE AWAY"
                            Case 54
                                pbpString$ = pbpString$ + "...LEFT HANDED FADE AWAY"
                            Case 55
                                pbpString$ = pbpString$ + "...HIGH ARCHING SHOT"
                            Case 56
                                pbpString$ = pbpString$ + " WITH A RUNNING LEFT HANDER"
                            Case 57
                                pbpString$ = pbpString$ + " WITH A RUNNING SCOOP SHOT"
                            Case 58
                                pbpString$ = pbpString$ + " WITH THE SCOOP SHOT"
                            Case 59
                                pbpString$ = pbpString$ + " WITH THE FINGER ROLL"

                        End Select

                    Case 7
                        willDunk = 0

                        Select Case RN0

                            Case 1 To 10
                                pbpString$ = pbpString$ + " FIRES FROM THE PERIMETER"
                            Case 11 To 20
                                pbpString$ = pbpString$ + " SHOOTS FROM 17"
                            Case 21 To 30
                                pbpString$ = pbpString$ + " PUTS IT UP FROM 17"
                            Case 31 To 40
                                pbpString$ = pbpString$ + " PUTS IT UP"
                            Case 41 To 45
                                pbpString$ = pbpString$ + " FROM INSIDE THE THE KEY"
                            Case 46 To 50
                                pbpString$ = pbpString$ + " FIRES FROM THE CORNER"
                            Case 51 To 60
                                pbpString$ = pbpString$ + " SHOOTS FROM OUTSIDE"
                            Case 61 To 70
                                pbpString$ = pbpString$ + " PUMPS AND SHOOTS"
                            Case 71 To 80
                                pbpString$ = pbpString$ + " FIRES THE FADE-AWAY"
                            Case 81 To 90
                                If threePtOpt = 1 Then
                                    pbpString$ = pbpString$ + " FROM INSIDE THE 3-POINT LINE"
                                End If
                            Case 91
                                If threePtOpt = 1 Then
                                    pbpString$ = pbpString$ + " FOR THREE!"
                                    Call PBP(P)

                                    pbpString$ = "NO!"
                                    Call PBP(P)

                                    If teamGender$(P) = "HE" Then
                                        pbpString$ = "HIS"
                                    Else
                                        pbpString$ = "HER"
                                    End If
                                    pbpString$ = pbpString$ + " FOOT IS ON THE 3-POINT LINE"

                                End If
                            Case 92 To 100
                                pbpString$ = pbpString$ + " SHOOTS FROM THE WING"

                        End Select

                    Case 8

                        Select Case RN0

                            Case 1 To 20
                                pbpString$ = teamGender$(P) + " SAILS TO THE HOOP"
                            Case 21 To 23
                                pbpString$ = teamGender$(P) + " TAKES IT TO THE BUCKET"
                            Case 24 To 26
                                pbpString$ = teamGender$(P) + " TAKES IT TO THE HOLE"
                            Case 27 To 29
                                pbpString$ = teamGender$(P) + " SLASHES TO THE HOLE"
                            Case 30 To 32
                                pbpString$ = teamGender$(P) + " SLASHES TO THE BUCKET"
                            Case 33 To 35
                                pbpString$ = teamGender$(P) + " SOARS TO THE BUCKET"
                            Case 36 To 38
                                pbpString$ = teamGender$(P) + " SOARS TO THE HOLE"
                            Case 39 To 40
                                pbpString$ = teamGender$(P) + " GLIDES TO THE HOLE"
                            Case 41 To 60
                                pbpString$ = teamGender$(P) + " GOES IN STRONG"
                            Case 61 To 80
                                pbpString$ = teamGender$(P) + " TAKES IT IN"
                            Case 81 To 90
                                pbpString$ = teamGender$(P) + " TAKES A DEFENDER WITH "
                                If teamGender$(P) = "HE" Then
                                    pbpString$ = pbpString$ + "HIM"
                                Else
                                    pbpString$ = pbpString$ + "HER"
                                End If

                            Case 91 To 100
                                pbpString$ = teamGender$(P) + " GOES UP WITH IT"

                        End Select

                    Case 9
                        'BACK AFTER REB
                        pbpString$ = teamGender$(P) + " "

                        RN1 = Int(Rnd(1) * 11) + 1

                        Select Case RN1

                            Case 1
                                pbpString$ = pbpString$ + "TAKES IT BACK UP"
                            Case 2
                                pbpString$ = pbpString$ + "GOES BACK UP WITH IT"
                            Case 3
                                If teamGender$(P) = "HE" Then
                                    pbpString$ = "POWERS HIS"
                                Else
                                    pbpString$ = "POWERS HER"
                                End If
                                pbpString$ = pbpString$ + " WAY UP WITH IT"

                            Case 4
                                pbpString$ = pbpString$ + "GOES STRONG TO THE BASKET"
                            Case 5
                                pbpString$ = pbpString$ + "MUSCLES IT UP"
                            Case 6
                                pbpString$ = pbpString$ + "SHOOTS"
                            Case 7
                                pbpString$ = pbpString$ + "TAKES IT TO THE GOAL"
                            Case 8
                                pbpString$ = pbpString$ + "PUTS IT UP AGAIN"
                            Case 9
                                pbpString$ = pbpString$ + "SHOOTS IT AGAIN"
                            Case 10
                                If teamGender$(P) = "HE" Then
                                    pbpString$ = "CLAWS HIS WAY UP"
                                Else
                                    pbpString$ = "CLAWS HER WAY UP"
                                End If

                            Case 11
                                pbpString$ = pbpString$ + "SHOOTS IN TRAFFIC"

                        End Select

                        'Forward
                        '-- "1%" factor = 10
                        If (ballCarrier = 2 Or ballCarrier = 3) And d100% <= 30 Then
                            willDunk = 1
                        End If

                        'Center
                        '-- "1%" factor = 8
                        If ballCarrier = 4 Then
                            'Centers dunk slightly more often in women's basketball
                            If (teamGender$(P) = "SHE" And d100% <= 36) Or (teamGender$(P) = "HE" And d100% <= 24) Then
                                willDunk = 1
                            End If
                        End If

                    Case 10
                        willDunk = 0

                        Call Rolld100(RN1)

                        Select Case RN1

                            Case 1 To 20
                                pbpString$ = pbpString$ + " SHOOTS FROM 15"
                            Case 21 To 40
                                pbpString$ = pbpString$ + " SHOOTS"
                            Case 41 To 60
                                pbpString$ = pbpString$ + " TAKES THE JUMPER"
                            Case 61 To 80
                                pbpString$ = pbpString$ + " PUTS IT UP FROM 16"
                            Case 81 To 100
                                pbpString$ = pbpString$ + " FAKES AND SHOOTS"

                        End Select

                End Select

            Case 4
                'THREE GOOD
                Select Case RN0

                    Case 1 To 6
                        pbpString$ = teamGender$(P) + " DRAINS THE THREE!"
                        Call PBP(P)
                    Case 7 To 10
                        pbpString$ = teamGender$(P) + " FILLS UP THE BASKET!!!"
                    Case 11 To 20
                        pbpString$ = teamGender$(P) + " HITS THE TREY!!!"
                    Case 21 To 30
                        pbpString$ = "IT'S GOOOODD!!"
                    Case 31 To 40
                        pbpString$ = teamGender$(P) + " GETS IT!!!"
                    Case 41 To 45
                        pbpString$ = "NOTHING BUT THE BOTTOM OF THE NET"
                    Case 46 To 50
                        pbpString$ = "COUNT IT!!"
                    Case 51 To 55
                        pbpString$ = "WAY DOWNTOWN BANG!!!"
                    Case 56 To 60
                        pbpString$ = teamGender$(P) + " RINGS IT UP!!!"
                    Case 61 To 80
                        pbpString$ = teamGender$(P) + " DRAINS IT!!!"
                    Case 81 To 90
                        pbpString$ = teamGender$(P) + " BURIES IT!!!"
                    Case 91 To 100
                        pbpString$ = "IT'S ALL NET!!!"

                End Select

                If d100% <= 1 Then

                    pbpString$ = "NO!"
                    Call PBP(P)

                    If teamGender$(P) = "HE" Then
                        pbpString$ = "HIS"
                    Else
                        pbpString$ = "HER"
                    End If
                    pbpString$ = pbpString$ + " FOOT IS ON THE 3-POINT LINE"

                    XX% = 1

                End If

            Case 5
                'THREE BAD
                Select Case RN0

                    Case 1 To 20
                        pbpString$ = teamGender$(P) + " GETS NONE"
                    Case 21 To 25
                        pbpString$ = "IT NEVER HAD A CHANCE"
                    Case 26 To 35
                        pbpString$ = "AIRRRR BALLLL!!"
                    Case 36 To 40
                        pbpString$ = teamGender$(P) + " MISFIRES"
                    Case 41 To 60
                        pbpString$ = "IT'S OFF THE RIM...NO GOOD!"
                    Case 61 To 80
                        pbpString$ = "IT'S SHORT"
                    Case 81 To 100
                        pbpString$ = teamGender$(P) + " MISSES"

                End Select

            Case 6
                'FAST BREAK
                pbpString$ = players$(P, lineupIdx(P, ballCarrier))

                RN1 = Int(Rnd(1) * 38) + 1

                Select Case RN1

                    Case 1
                        pbpString$ = pbpString$ + " HAS IT ON THE FAST BREAK!"
                    Case 2
                        pbpString$ = pbpString$ + " IS IN TRANSITION!"
                    Case 3
                        pbpString$ = pbpString$ + " STARTS THE BREAK!"
                    Case 4
                        pbpString$ = pbpString$ + " IS DOWN COURT IN A HURRY!"
                    Case 5
                        pbpString$ = pbpString$ + " IS ON THE RUN!"
                    Case 6
                        pbpString$ = pbpString$ + " GOES COAST TO COAST!"
                    Case 7
                        pbpString$ = pbpString$ + " ON THE BREAK!"
                    Case 8
                        pbpString$ = pbpString$ + " ON THE RUN!"
                    Case 9
                        pbpString$ = pbpString$ + " ON THE FAST BREAK!"
                    Case 10
                        pbpString$ = pbpString$ + " HAS THE FAST BREAK!"
                    Case 11
                        pbpString$ = pbpString$ + " BREAKS INTO THE FORECOURT!"
                    Case 12
                        pbpString$ = pbpString$ + " FLIES UPCOURT!"
                    Case 13
                        pbpString$ = pbpString$ + " STARTS THE FASTBREAK!"
                    Case 14
                        pbpString$ = pbpString$ + " ON THE MOVE!"
                    Case 15
                        pbpString$ = pbpString$ + " IN TRANSITION!"
                    Case 16
                        pbpString$ = pbpString$ + " DOWN COURT IN A HURRY!"
                    Case 17
                        pbpString$ = pbpString$ + " CHARGES ACROSS THE LINE!"
                    Case 18
                        pbpString$ = pbpString$ + " BREAKS QUICKLY!"
                    Case 19
                        pbpString$ = pbpString$ + " BREAKS DOWN COURT!"
                    Case 20
                        pbpString$ = pbpString$ + " STARTS THE RUNNING GAME!"
                    Case 21
                        pbpString$ = pbpString$ + " FLIES DOWN THE FLOOR!"
                    Case 22
                        pbpString$ = pbpString$ + " GOES RIGHT TO WORK!"
                    Case 23
                        pbpString$ = pbpString$ + " BACK THE OTHER WAY!"
                    Case 24
                        pbpString$ = pbpString$ + " QUICKLY UP THE FLOOR!"
                    Case 25
                        pbpString$ = pbpString$ + " PUSHES IT UP COURT!"
                    Case 26
                        pbpString$ = pbpString$ + " WASTES NO TIME!"
                    Case 27
                        pbpString$ = pbpString$ + " UPS THE TEMPO!"
                    Case 28
                        pbpString$ = pbpString$ + " HUSTLES DOWN COURT!"
                    Case 29
                        pbpString$ = pbpString$ + " RUSHES UP THE FLOOR!"
                    Case 30
                        pbpString$ = pbpString$ + " ON THE MOVE!"
                    Case 31
                        pbpString$ = pbpString$ + " STREAKS UP THE COURT!"
                    Case 32
                        pbpString$ = pbpString$ + " FLIES DOWN THE COURT!"
                    Case 33
                        pbpString$ = pbpString$ + " JETS DOWN COURT!"
                    Case 34
                        pbpString$ = pbpString$ + " QUICKLY GETS IT UP COURT!"
                    Case 35
                        pbpString$ = pbpString$ + " TAKES IT COAST TO COAST!"
                    Case 36
                        pbpString$ = pbpString$ + " IS RUNNING AND GUNNING!"
                    Case 37
                        pbpString$ = pbpString$ + " FLIES OVER THE TIME LINE!"
                    Case 38
                        pbpString$ = pbpString$ + " RUSHES IT UP THE FLOOR!"

                End Select

                Call SHOTSTRING

            Case 7
                'FAST BREAK SCORE
                fastBreak = 0

                RN1 = Int(Rnd(1) * 39) + 1

                Select Case RN1

                    Case 1
                        pbpString$ = "DIPSY-DOO DUNK-A-ROO!!!"
                    Case 2
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " MAKES IT HAMMER TIME!!!"
                    Case 3
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " SLAMS IT IN!!!"
                    Case 4
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " WITH THE 2-HANDED SLAM!!!"
                    Case 5
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " SLAMS IT HOME!!!"
                    Case 6
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " JAMS IT HOME!!!"
                    Case 7
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " THROWS IT DOWN!!!"
                    Case 8
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " THUNDERSLAM!!!"
                    Case 9
                        pbpString$ = "RIM ROCKING JAM!!!"
                    Case 10
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " WITH THE MONSTER SLAM!!"
                    Case 11
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + "POWER SLAMS IT!!!"
                    Case 12
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " WITH THE 2-HANDED STUFshotChance!!!"
                    Case 13
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " BACKWARD!S JAM!!!"
                    Case 14
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " WITH THE 1-HANDED SLAM!!!"
                    Case 15
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " WITH THE REVERSE SLAM!!!"
                    Case 16
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " WITH THE REVERSE JAM!!!"
                    Case 17
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " WITH THE WINDMILL JAM!!!"
                    Case 18
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " WITH THE WINDMILL SLAM!!!"
                    Case 19
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " WITH THE TOMAHAWK SLAM!!!"
                    Case 20
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " WITH THE 360 SLAM!!!"
                    Case 21
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " WITH THE SPINNING SLAM!!!"
                    Case 22
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " WITH THE LEFT HANDED DUNK!!!"
                    Case 23
                        pbpString$ = "DOUBLE CLUTCH JAM!!!"
                    Case 24
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " GETS THE FLUSH!!!"
                    Case 25
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " PUNCHES IT HOME!!!"
                    Case 26
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + "...DOUBLE PUMP SLAM!!!"
                    Case 27
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + "...IN YOUR FACE SLAM!!!"
                    Case 28
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " IS UP"
                        Call PBP(P)
                        pbpString$ = teamGender$(P) + " SPINS"
                        Call PBP(P)
                        pbpString$ = "BEHIND THE BACK SLAM!!!"
                    Case 29
                        pbpString$ = teamGender$(P) + " SLAMS IT HOME!!"
                    Case 30
                        pbpString$ = teamGender$(P) + " LAYS IT UP AND IN!!"
                    Case 31
                        pbpString$ = teamGender$(P) + " BANGS IT IN!!"
                    Case 32
                        pbpString$ = teamGender$(P) + " RAMS IT IN!!"
                    Case 33
                        pbpString$ = teamGender$(P) + " SLAMS IT IN!!!"
                    Case 34
                        pbpString$ = "DIPSY-DOO DUNK-A-ROO!!!"
                    Case 35
                        pbpString$ = teamGender$(P) + " MAKES IT HAMMER TIME!!!"
                    Case 36
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " WITH THE 2-HANDED SLAM!!!"
                    Case 37
                        pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " IS UP"
                        Call PBP(P)
                        pbpString$ = teamGender$(P) + " SPINS"
                        Call PBP(P)
                        pbpString$ = "BEHIND THE BACK SLAM!!!"
                    Case 38
                        pbpString$ = teamGender$(P) + " JAMS IT HOME!!!"
                    Case 39
                        pbpString$ = teamGender$(P) + " HAMMERS IT HOME!"

                End Select

            Case 8
                'MISS
                If shotType = 8 Then

                    shotType = 0
                    fastBreak = 0

                    Select Case RN0

                        Case 1 To 20
                            pbpString$ = teamGender$(P) + "MISSES THE EASY BASKET!"

                        Case 21 To 40
                            pbpString$ = teamGender$(P) + "JAMS IT TOO HARD!!!"

                        Case 41 To 60
                            pbpString$ = teamGender$(P) + "SLAMS IT OFF THE IRON...NO GOOD!"

                        Case 61 To 80
                            pbpString$ = teamGender$(P) + "CAN'T GET IT DOWN!"

                        Case 81 To 100
                            pbpString$ = teamGender$(P) + "BLOWS THE EASY BASKET!!"

                    End Select

                Else

                    RN1 = Int(Rnd(1) * 50) + 1

                    Select Case RN1

                        Case 1
                            pbpString$ = "IT RATTLES IN AND OUT...NO GOOD"
                        Case 2
                            pbpString$ = "OFF THE IRON"
                        Case 3
                            pbpString$ = "IT SPINS IN AND OUT"
                        Case 4
                            pbpString$ = "IT'S OFF THE GLASS"
                        Case 5
                            pbpString$ = "IT'S NO GOOD"
                        Case 6
                            pbpString$ = "IT'S OFF THE MARK"
                        Case 7
                            pbpString$ = "IT'S A BRICK"
                        Case 8
                            pbpString$ = "OFF THE RIM"
                        Case 9
                            pbpString$ = "NOT EVEN CLOSE!"
                        Case 10
                            pbpString$ = "NO GOOD"
                        Case 11
                            pbpString$ = teamGender$(P) + " MISSES"
                        Case 12
                            pbpString$ = "DOESN'T GET THE ROLL"
                        Case 13
                            pbpString$ = "OFF THE GLASS"
                        Case 14
                            pbpString$ = "FALLS SHORT"
                        Case 15
                            pbpString$ = "NO GOOD"
                        Case 16
                            pbpString$ = "NO"
                        Case 17
                            pbpString$ = teamGender$(P) + " MISSES"
                        Case 18
                            pbpString$ = "IN AND OUT"
                        Case 19
                            pbpString$ = "OFF THE RIM"
                        Case 20
                            pbpString$ = "OFF THE IRON"
                        Case 21
                            pbpString$ = "IT'S NOT THERE"
                        Case 22
                            pbpString$ = "OFF TARGET"
                        Case 23
                            pbpString$ = "IT RATTLES OUT"
                        Case 24
                            pbpString$ = "IT DOESN'T FALL"
                        Case 25
                            pbpString$ = "IT BOUNCES OFF"
                        Case 26
                            pbpString$ = "IT ROLLS OFF"
                        Case 27
                            pbpString$ = "SHORT"
                        Case 28
                            pbpString$ = "IT DOESN'T GO DOWN"
                        Case 29
                            pbpString$ = "IT'S NO GOOD"
                        Case 30
                            pbpString$ = "NO GOOD"
                        Case 31
                            pbpString$ = teamGender$(P) + " MISSES"
                        Case 32
                            pbpString$ = "IT'S A BRICK"
                        Case 33
                            pbpString$ = teamGender$(P) + " FAILS TO CONNECT"
                        Case 34
                            pbpString$ = "OFF THE SIDE OF THE RIM"
                        Case 35
                            pbpString$ = "IT COMES UP SHORT"
                        Case 36
                            pbpString$ = "IT ROLLS OUT"
                        Case 37
                            pbpString$ = "AROUND AND OUT"
                        Case 38
                            pbpString$ = "OFF THE BACK OF THE IRON"
                        Case 39
                            pbpString$ = "ON THE FRONT RIM...ROLLS OFF"
                        Case 40
                            pbpString$ = "IN THE AIR...NO GOOD"
                        Case 41
                            pbpString$ = "TOO SHORT"
                        Case 42
                            pbpString$ = "NO GOOD"
                        Case 43
                            pbpString$ = "IT COMES UP SHORT"
                        Case 44
                            pbpString$ = "IT ROLLS OUT"
                        Case 45
                            pbpString$ = "IN AND OUT"
                        Case 46
                            pbpString$ = "OFF THE FRONT OF THE IRON"
                        Case 47
                            pbpString$ = "ON THE RIM...ROLLS OFF"
                        Case 48
                            pbpString$ = "OFF THE FRONT OF THE RIM"
                        Case 49
                            pbpString$ = "TOO SHORT"
                        Case 50
                            pbpString$ = "TOO STRONG"

                    End Select

                End If

            Case 9
                'NO PASS
                pbpString$ = players$(P, lineupIdx(P, ballCarrier)) + " "

                RN1 = Int(Rnd(1) * 56) + 1

                Select Case RN1

                    Case 1
                        pbpString$ = pbpString$ + "LOOKS INSIDE"
                    Case 2
                        pbpString$ = pbpString$ + "TRIES TO WORK FREE"
                    Case 3
                        pbpString$ = pbpString$ + "SETS UP THE PLAY"
                    Case 4
                        pbpString$ = pbpString$ + "LOOKS FOR AN OPENING"
                    Case 5
                        pbpString$ = pbpString$ + "DRIBBLES AROUND "

                        If teamGender$(P) = "HE" Then
                            pbpString$ = "HIS MAN"
                        Else
                            pbpString$ = "HER DEFENDER"
                        End If

                    Case 6
                        pbpString$ = pbpString$ + "SIGNALS THE PLAY"
                    Case 7
                        pbpString$ = pbpString$ + "IS IN CONTROL"
                    Case 8
                        pbpString$ = pbpString$ + "IS IN POSSESSION"
                    Case 9
                        pbpString$ = pbpString$ + "TRIES TO GET OPEN"
                    Case 10
                        pbpString$ = pbpString$ + "YO-YO'S OUT TOP"
                    Case 11
                        pbpString$ = pbpString$ + "GOES TO WORK"
                    Case 12
                        pbpString$ = pbpString$ + "WORKS OFF THE PICK"
                    Case 13
                        pbpString$ = pbpString$ + "ON THE DRIBBLE"
                    Case 14
                        pbpString$ = pbpString$ + "CUTS TO THE LEFT"
                    Case 15
                        pbpString$ = pbpString$ + "CUTS TO THE RIGHT"
                    Case 16
                        pbpString$ = pbpString$ + "WHEELS AND DEALS"
                    Case 17
                        pbpString$ = pbpString$ + "SETS UP ON TOP"
                    Case 18
                        pbpString$ = pbpString$ + "SLIDES RIGHT"
                    Case 19
                        pbpString$ = pbpString$ + "SLIDES LEFT"
                    Case 20
                        pbpString$ = pbpString$ + "GOES TO WORK"
                    Case 21
                        pbpString$ = pbpString$ + "MOVES IT LEFT-SIDE"
                    Case 22
                        pbpString$ = pbpString$ + "MOVES IT RIGHT-SIDE"
                    Case 23
                        pbpString$ = pbpString$ + "REVERSES THE DRIBBLE"
                    Case 24
                        pbpString$ = pbpString$ + "TRIES TO GET OPEN"
                    Case 25
                        pbpString$ = pbpString$ + "DRIBBLES TO THE MIDDLE"
                    Case 26
                        pbpString$ = pbpString$ + "FAKES THE PASS"
                    Case 27
                        pbpString$ = pbpString$ + "SETS UP ON TOP"
                    Case 28
                        pbpString$ = pbpString$ + "SLIDES RIGHT"
                    Case 29
                        pbpString$ = pbpString$ + "SLIDES LEFT"
                    Case 30
                        pbpString$ = pbpString$ + "DRIBBLES IT BEHIND THE BACK"
                    Case 31
                        pbpString$ = pbpString$ + "FAKES LEFT...GOES RIGHT"
                    Case 32
                        pbpString$ = pbpString$ + "FAKES RIGHT...GOES LEFT"
                    Case 33
                        pbpString$ = pbpString$ + "SPINS HARD! TO THE LEFT"
                    Case 34
                        pbpString$ = pbpString$ + "SPINS HARD! TO THE RIGHT"
                    Case 35
                        pbpString$ = pbpString$ + "LEANS LEFT...GOES RIGHT"
                    Case 36
                        pbpString$ = pbpString$ + "LEANS RIGHT...GOES LEFT"
                    Case 37
                        pbpString$ = pbpString$ + "LEANS LEFT...CUTS RIGHT"
                    Case 38
                        pbpString$ = pbpString$ + "LEANS RIGHT...CUTS LEFT"
                    Case 39
                        pbpString$ = pbpString$ + "DOUBLE REVERSES THE DRIBBLE"
                    Case 40
                        pbpString$ = pbpString$ + "BACKS IN"
                    Case 41
                        pbpString$ = pbpString$ + "LEANS FORWARD!"
                    Case 42
                        pbpString$ = pbpString$ + "CROSSES OVER...SPINS"
                    Case 43
                        pbpString$ = pbpString$ + "CUTS LEFT"
                    Case 44
                        pbpString$ = pbpString$ + "CUTS RIGHT"
                    Case 45
                        pbpString$ = pbpString$ + "WITH THE HESITATION DRIBBLE"
                        Call PBP(P)
                        pbpString$ = teamGender$(P) + " SPINS LEFT"
                    Case 46
                        pbpString$ = pbpString$ + "WITH THE HESITATION DRIBBLE"
                        Call PBP(P)
                        pbpString$ = teamGender$(P) + " SPINS RIGHT"
                    Case 47
                        pbpString$ = pbpString$ + "WITH THE HESITATION DRIBBLE"
                        Call PBP(P)
                        pbpString$ = teamGender$(P) + " CUTS LEFT"
                    Case 48
                        pbpString$ = pbpString$ + "WITH THE HESITATION DRIBBLE"
                        Call PBP(P)
                        pbpString$ = teamGender$(P) + " CUTS RIGHT"
                    Case 49
                        pbpString$ = pbpString$ + "STUTTER STEPS"
                        Call PBP(P)
                        pbpString$ = teamGender$(P) + " MOVES LEFT"
                    Case 50
                        pbpString$ = pbpString$ + "MAINTAINS THE DRIBBLE"
                    Case 51
                        pbpString$ = pbpString$ + "FAKE SPINS LEFT...GOES RIGHT"
                    Case 52
                        pbpString$ = pbpString$ + "FAKE SPINS RIGHT...GOES LEFT"
                    Case 53
                        pbpString$ = pbpString$ + "PALMS THE BALL"
                        Call PBP(P)
                        pbpString$ = teamGender$(P) + " TAKES IT AND SPINS"
                    Case 54
                        pbpString$ = pbpString$ + "SHAKES AND BAKES"
                    Case 55
                        pbpString$ = pbpString$ + "DRIBBLES WITH THE LEFT HAND"
                    Case 56
                        pbpString$ = pbpString$ + "DRIBBLES WITH THE RIGHT HAND"

                End Select

            Case 10
                'PASS
                'prevBall$=PREV HANDLER
                V$ = players$(P, lineupIdx(P, ballCarrier))

                RN1 = Int(Rnd(1) * 60) + 1

                Select Case RN1

                    Case 1
                        pbpString$ = prevBall$ + " TO " + V$
                    Case 2
                        pbpString$ = prevBall$ + " KICKS IT OUT"
                        Call PBP(P)
                        pbpString$ = V$ + " TAKES THE PASS"
                    Case 3
                        pbpString$ = prevBall$ + " FEEDS " + V$
                    Case 4
                        pbpString$ = prevBall$ + " PASSES TO " + V$
                    Case 4
                        pbpString$ = prevBall$ + " DISHES TO " + V$
                    Case 5
                        pbpString$ = prevBall$ + " GIVES IT TO " + V$
                    Case 6
                        pbpString$ = prevBall$ + " WITH A NICE GIVE TO " + V$
                    Case 7
                        pbpString$ = prevBall$ + " LOOKS"
                        Call PBP(P)
                        pbpString$ = teamGender$(P) + " GETS IT TO " + V$
                    Case 8
                        pbpString$ = prevBall$ + " SENDS IT TO " + V$
                    Case 9
                        pbpString$ = prevBall$ + " TO " + V$
                    Case 10
                        pbpString$ = prevBall$ + " GETS IT TO " + V$
                    Case 11
                        pbpString$ = prevBall$ + " OVER TO " + V$
                    Case 12
                        pbpString$ = prevBall$ + " ZIPS IT TO " + V$
                    Case 13
                        pbpString$ = prevBall$ + " FINDS " + V$
                    Case 14
                        pbpString$ = prevBall$ + " LOOKS FOR " + V$
                    Case 14
                        pbpString$ = prevBall$ + " THROWS IT TO " + V$
                    Case 15
                        pbpString$ = prevBall$ + " GOES TO " + V$
                    Case 16
                        pbpString$ = prevBall$ + " SETS UP " + V$
                    Case 17
                        pbpString$ = prevBall$ + " OUT TO " + V$
                    Case 18
                        pbpString$ = prevBall$ + " FLIPS IT TO " + V$
                    Case 19
                        pbpString$ = "BOUNCE PASS TO " + V$
                    Case 20
                        pbpString$ = prevBall$ + " LEAVES IT FOR " + V$
                    Case 21
                        pbpString$ = prevBall$ + " LOOPS IT TO " + V$
                    Case 22
                        pbpString$ = prevBall$ + " SHOVELS IT OVER TO " + V$
                    Case 23
                        pbpString$ = prevBall$ + " GIVES IT BACK TO " + V$
                    Case 24
                        pbpString$ = prevBall$ + " FEEDS IT TO " + V$
                    Case 25
                        pbpString$ = prevBall$ + " GIVES IT OFF TO " + V$
                    Case 26
                        pbpString$ = "PASSES TO " + V$
                    Case 27
                        pbpString$ = "DISHES TO " + V$
                    Case 28
                        pbpString$ = "GIVES TO " + V$
                    Case 29
                        pbpString$ = "OVER TO " + V$
                    Case 30
                        pbpString$ = "SENDS IT BACK TO " + V$
                    Case 31
                        pbpString$ = "NO-LOOK PASS TO " + V$
                    Case 32
                        pbpString$ = "NO-LOOK BOUNCE PASS TO " + V$
                    Case 33
                        pbpString$ = "BEHIND THE BACK TO " + V$
                    Case 34
                        pbpString$ = prevBall$ + " SWINGS IT TO " + V$
                    Case 35
                        pbpString$ = prevBall$ + " FIRES A PASS TO " + V$
                    Case 36
                        pbpString$ = prevBall$ + " SNAPS A PASS TO " + V$
                    Case 37
                        pbpString$ = prevBall$ + " SWINGS A PASS TO " + V$
                    Case 38
                        pbpString$ = "TWO-HANDED PASS TO " + V$
                    Case 39
                        pbpString$ = "WRAP AROUND PASS TO " + V$
                    Case 40
                        pbpString$ = "OVER THE SHOULDER PASS TO " + V$
                    Case 41
                        pbpString$ = "BACKHAND PASS TO " + V$
                    Case 42
                        pbpString$ = "BACKHAND DISH TO " + V$
                    Case 43
                        pbpString$ = "PASS CONNECTED TO " + V$
                    Case 44
                        pbpString$ = prevBall$ + " WIRES IT TO " + V$
                    Case 45
                        pbpString$ = prevBall$ + " WIRES A PASS TO " + V$
                    Case 46
                        pbpString$ = prevBall$ + " SNAPS A PASS TO " + V$
                    Case 47
                        pbpString$ = prevBall$ + " FAKES..PASSES TO " + V$
                    Case 48
                        pbpString$ = prevBall$ + " SHUFFLES IT TO " + V$
                    Case 49
                        pbpString$ = prevBall$ + " SLINGS IT TO " + V$
                    Case 50
                        pbpString$ = prevBall$ + " FLIPS IT TO " + V$
                    Case 51
                        pbpString$ = prevBall$ + " WHIPS IT OVER TO " + V$
                    Case 52
                        pbpString$ = prevBall$ + " SLINGS A BOUNCE PASS TO " + V$
                    Case 53
                        pbpString$ = prevBall$ + " SHOVELS IT TO " + V$
                    Case 54
                        pbpString$ = prevBall$ + " PUMP FAKES A SHOT"
                        Call PBP(P)
                        pbpString$ = teamGender$(P) + " SWINGS IT TO " + V$
                    Case 55
                        pbpString$ = prevBall$ + " PUMP FAKES A SHOT"
                        Call PBP(P)
                        pbpString$ = teamGender$(P) + " BOUNCES IT TO " + V$
                    Case 56
                        pbpString$ = prevBall$ + " WITH A FOOT FAKE"
                        Call PBP(P)
                        pbpString$ = teamGender$(P) + " SPINS AND DUMPS IT TO " + V$
                    Case 57
                        pbpString$ = prevBall$ + " CUTS ACROSS THE LANE"
                        Call PBP(P)
                        pbpString$ = teamGender$(P) + " DISHES INSIDE TO " + V$
                    Case 58
                        pbpString$ = prevBall$ + " DRIVES THE BASELINE"
                        Call PBP(P)
                        pbpString$ = "DISHES UNDERNEATH TO " + V$
                    Case 59
                        pbpString$ = prevBall$ + " FAKES RIGHT...CUTS LEFT"
                        Call PBP(P)
                        pbpString$ = "SHUFFLES IT TO " + V$
                    Case 60
                        pbpString$ = prevBall$ + " HEAD FAKES"
                        Call PBP(P)
                        pbpString$ = teamGender$(P) + " PUSHES IT TO " + V$

                End Select

            Case Else
                'Not a value from 1-10 ???
                pbpString$ = players$(P, lineupIdx(P, ballCarrier))

        End Select

    End If

End Sub


'----------------------------------------
'           WINDEX Subroutine
'----------------------------------------
'This subroutine cleans the "window" used
'for the play-by-play text.
Sub WINDEX ()

    For I = 8 To 15
        Locate I, 1
        Call BLANKLINE
    Next I

    Locate 8, 1

End Sub


'----------------------------------------
'       BLANKLINE Subroutine
'----------------------------------------
'This subroutine literally prints blank
'spaces to clear a line of the PBP box.
Sub BLANKLINE ()

    Print Space$(38)

End Sub


'----------------------------------------
'         CLEARPBPBOX Subroutine
'----------------------------------------
'This subroutine is similar to windex,
'but only cleans the Play-by-Play area
'if there is a PBP delay. It also places
'the cursor further down when done.
Sub CLEARPBPBOX ()

    For I = 8 To 15:
        Locate I, 1
        Call BLANKLINE
    Next I

    Locate 10, 1

End Sub


'----------------------------------------
'          CLEARPBP Subroutine
'----------------------------------------
'This subroutine clears all the strings
'used for each line of PBP display
Sub CLEARPBP ()

    If pbpDelay! > 0 And pbpOpt <> 1 Then
        nbrLines = 1
        A1$ = ""
        B1$ = ""
        C1$ = ""
        D1$ = ""
        E1$ = ""
        F1$ = ""
        G1$ = ""
        H1$ = ""
    End If

End Sub


'----------------------------------------
'           TICKER Subroutine
'----------------------------------------
'This subroutine displays the score of all
'current games being tracked by the ticker.
'For each game it will find the score that
'matches the current game time / period.
Sub TICKER ()

    Shared tickerPeriod$()

    If autoPlay <> 1 Or playerMode <> 2 Then

        Color 14, 0
        Cls

        Call Rolld100(RN0)

        If halfTime = 1 Then

            If G9 = 0 Then
                If RN0 <= 50 Then
                    Locate , 23: Print "PRUDENTIAL AT THE HALF SCOREBOARD"
                Else
                    Locate , 28: Print "RED MAN HALFTIME REPORT"
                End If
            Else
                If RN0 <= 50 Then
                    Locate , 29: Print "TOYOTA HALFTIME REPORT"
                Else
                    Locate , 29: Print "HONDA HALFTIME REPORT"
                End If
            End If

        Else

            If gameOver = 1 Then

                If G9 = 0 Then
                    Locate , 27: Print "CHEVROLET FINAL SCOREBOARD"
                Else
                    Select Case RN0
                        Case 1 To 33: Locate , 27: Print "USA TODAY FINAL SCOREBOARD"
                        Case 34 To 67: Locate , 24: Print "FEDERAL EXPRESS FINAL SCOREBOARD"
                        Case 67 To 100: Locate , 23: Print "ESPN SPORTSCENTER FINAL SCOREBOARD"
                    End Select
                End If
            Else
                If halfTime = 0 Then Locate , 30: Print "LANCE HAFFNER GAMES SCOREBOARD"
            End If

        End If

        Color 15

        If tickerGames = 0 Then

            Locate 10, 28: Print "NO OTHER GAMES SCHEDULED"

        Else

            If currHalf = 1 Then

                Select Case gameClock!
                    Case 960 To 1200
                        timeInterval = 1

                    Case 720 To 959
                        timeInterval = 2

                    Case 480 To 719
                        timeInterval = 3

                    Case 240 To 479
                        timeInterval = 4

                    Case 120 To 238
                        timeInterval = 5

                    Case Is < 120
                        timeInterval = 6
                End Select

            End If

            If halfTime = 1 Then
                timeInterval = 8
            End If

            If currHalf = 2 Then

                Select Case gameClock!
                    Case 960 To 1200
                        timeInterval = 9

                    Case 720 To 959
                        timeInterval = 10

                    Case 480 To 719
                        timeInterval = 11

                    Case 240 To 479
                        timeInterval = 12

                    Case 120 To 238
                        timeInterval = 13

                    Case Is < 120
                        timeInterval = 14
                End Select

            End If

            If currHalf = 3 Then

                Select Case gameClock!
                    Case 960 To 1200
                        timeInterval = 14

                    Case 720 To 959
                        timeInterval = 14

                    Case 480 To 719
                        timeInterval = 15

                    Case 240 To 479
                        timeInterval = 16

                    Case Is < 240
                        timeInterval = 17
                End Select

            End If

            If Left$(SX$(1, 0), 3) <> "XXX" Then

                Locate 2, 1
                X = 1
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(1, 0), 13);
                Locate , 15: Print Using "### "; SX%(1, 0, tickInterval);
                Print tickerPeriod$(tickInterval)

                Print Left$(SX$(1, 1), 13);
                Locate , 15: Print Using "### #"; SX%(1, 1, tickInterval)

            End If

            If Left$(SX$(2, 0), 3) <> "XXX" Then

                Locate 5, 1
                X = 2
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(2, 0), 13);
                Locate , 15: Print Using "### "; SX%(2, 0, tickInterval);

                Print tickerPeriod$(tickInterval)

                Print Left$(SX$(2, 1), 13);
                Locate , 15: Print Using "### #"; SX%(2, 1, tickInterval)

            End If

            If Left$(SX$(3, 0), 3) <> "XXX" Then

                Locate 8, 1
                X = 3
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(3, 0), 13);
                Locate , 15: Print Using "### "; SX%(3, 0, tickInterval);

                Print tickerPeriod$(tickInterval)

                Print Left$(SX$(3, 1), 13);
                Locate , 15: Print Using "### #"; SX%(3, 1, tickInterval)

            End If

            If Left$(SX$(4, 0), 3) <> "XXX" Then

                Locate 11, 1
                X = 4
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(4, 0), 13);
                Locate , 15: Print Using "### "; SX%(4, 0, tickInterval);

                Print tickerPeriod$(tickInterval)

                Print Left$(SX$(4, 1), 13);
                Locate , 15: Print Using "### #"; SX%(4, 1, tickInterval)

            End If

            If Left$(SX$(5, 0), 3) <> "XXX" Then

                Locate 14, 1
                X = 5
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(5, 0), 13);
                Locate , 15: Print Using "### "; SX%(5, 0, tickInterval);

                Print tickerPeriod$(tickInterval)

                Print Left$(SX$(5, 1), 13);
                Locate , 15: Print Using "### #"; SX%(5, 1, tickInterval)

            End If

            If Left$(SX$(6, 0), 3) <> "XXX" Then

                Locate 17, 1
                X = 6
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(6, 0), 13);
                Locate , 15: Print Using "### "; SX%(6, 0, tickInterval);
                Print tickerPeriod$(tickInterval)

                Print Left$(SX$(6, 1), 13);
                Locate , 15: Print Using "### #"; SX%(6, 1, tickInterval)

            End If

            If Left$(SX$(7, 0), 3) <> "XXX" Then

                Locate 20, 1
                X = 7
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(7, 0), 13);
                Locate , 15: Print Using "### "; SX%(7, 0, tickInterval);

                Print tickerPeriod$(tickInterval)

                Print Left$(SX$(7, 1), 13);
                Locate , 15: Print Using "### #"; SX%(7, 1, tickInterval)

            End If

            If Left$(SX$(8, 0), 3) <> "XXX" Then

                Locate 23, 1
                X = 8
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(8, 0), 13);
                Locate , 15: Print Using "### "; SX%(8, 0, tickInterval);

                Print tickerPeriod$(tickInterval)

                Print Left$(SX$(8, 1), 13);
                Locate , 15: Print Using "### #"; SX%(8, 1, tickInterval);

            End If

            If Left$(SX$(9, 0), 3) <> "XXX" Then

                Locate 2, 21
                X = 9
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(9, 0), 13);
                Locate , 35: Print Using "### "; SX%(9, 0, tickInterval);

                Print tickerPeriod$(tickInterval)
                Locate , 21

                Print Left$(SX$(9, 1), 13);
                Locate , 35: Print Using "### #"; SX%(9, 1, tickInterval)

            End If

            If Left$(SX$(10, 0), 3) <> "XXX" Then

                Locate 5, 21
                X = 10
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(10, 0), 13);
                Locate , 35: Print Using "### "; SX%(10, 0, tickInterval);

                Print tickerPeriod$(tickInterval)
                Locate , 21

                Print Left$(SX$(10, 1), 13);
                Locate , 35: Print Using "### #"; SX%(10, 1, tickInterval)

            End If

            If Left$(SX$(11, 0), 3) <> "XXX" Then

                Locate 8, 21
                X = 11
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(11, 0), 13);
                Locate , 35: Print Using "### "; SX%(11, 0, tickInterval);

                Print tickerPeriod$(tickInterval)
                Locate , 21

                Print Left$(SX$(11, 1), 13);
                Locate , 35: Print Using "### #"; SX%(11, 1, tickInterval)

            End If

            If Left$(SX$(12, 0), 3) <> "XXX" Then

                Locate 11, 21
                X = 12
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(12, 0), 13);
                Locate , 35: Print Using "### "; SX%(12, 0, tickInterval);

                Print tickerPeriod$(tickInterval)
                Locate , 21

                Print Left$(SX$(12, 1), 13);
                Locate , 35: Print Using "### #"; SX%(12, 1, tickInterval)

            End If

            If Left$(SX$(13, 0), 3) <> "XXX" Then

                Locate 14, 21
                X = 13
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(13, 0), 13);
                Locate , 35: Print Using "### "; SX%(13, 0, tickInterval);

                Print tickerPeriod$(tickInterval)
                Locate , 21

                Print Left$(SX$(13, 1), 13);
                Locate , 35: Print Using "### #"; SX%(13, 1, tickInterval)

            End If

            If Left$(SX$(14, 0), 3) <> "XXX" Then

                Locate 17, 21
                X = 14
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(14, 0), 13);
                Locate , 35: Print Using "### "; SX%(14, 0, tickInterval);

                Print tickerPeriod$(tickInterval)
                Locate , 21

                Print Left$(SX$(14, 1), 13);
                Locate , 35: Print Using "### #"; SX%(14, 1, tickInterval)

            End If

            If Left$(SX$(15, 0), 3) <> "XXX" Then

                Locate 20, 21
                X = 15
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(15, 0), 13);
                Locate , 35: Print Using "### "; SX%(15, 0, tickInterval);

                Print tickerPeriod$(tickInterval)
                Locate , 21

                Print Left$(SX$(15, 1), 13);
                Locate , 35: Print Using "### #"; SX%(15, 1, tickInterval)

            End If

            If Left$(SX$(16, 0), 3) <> "XXX" Then

                Locate 23, 21
                X = 16
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(16, 0), 13);
                Locate , 35: Print Using "### "; SX%(16, 0, tickInterval);

                Print tickerPeriod$(tickInterval)
                Locate , 21

                Print Left$(SX$(16, 1), 13);
                Locate , 35: Print Using "### #"; SX%(16, 1, tickInterval);

            End If

            If Left$(SX$(17, 0), 3) <> "XXX" Then

                Locate 2, 41
                X = 17
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(17, 0), 13);
                Locate , 55: Print Using "### "; SX%(17, 0, tickInterval);

                Print tickerPeriod$(tickInterval)
                Locate , 41

                Print Left$(SX$(17, 1), 13);
                Locate , 55: Print Using "### #"; SX%(17, 1, tickInterval)

            End If

            If Left$(SX$(18, 0), 3) <> "XXX" Then

                Locate 5, 41
                X = 18
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(18, 0), 13);
                Locate , 55: Print Using "### "; SX%(18, 0, tickInterval);

                Print tickerPeriod$(tickInterval)
                Locate , 41

                Print Left$(SX$(18, 1), 13);
                Locate , 55: Print Using "### #"; SX%(18, 1, tickInterval)

            End If

            If Left$(SX$(19, 0), 3) <> "XXX" Then

                Locate 8, 41
                X = 19
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(19, 0), 13);
                Locate , 55: Print Using "### "; SX%(19, 0, tickInterval);

                Print tickerPeriod$(tickInterval)
                Locate , 41

                Print Left$(SX$(19, 1), 13);
                Locate , 55: Print Using "### #"; SX%(19, 1, tickInterval)

            End If

            If Left$(SX$(20, 0), 3) <> "XXX" Then

                Locate 11, 41
                X = 20
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(20, 0), 13);
                Locate , 55: Print Using "### "; SX%(20, 0, tickInterval);

                Print tickerPeriod$(tickInterval)
                Locate , 41

                Print Left$(SX$(20, 1), 13);
                Locate , 55: Print Using "### #"; SX%(20, 1, tickInterval)

            End If

            If Left$(SX$(21, 0), 3) <> "XXX" Then

                Locate 14, 41
                X = 21
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(21, 0), 13);
                Locate , 55: Print Using "### "; SX%(21, 0, tickInterval);

                Print tickerPeriod$(tickInterval)
                Locate , 41

                Print Left$(SX$(21, 1), 13);
                Locate , 55: Print Using "### #"; SX%(21, 1, tickInterval)

            End If

            If Left$(SX$(22, 0), 3) <> "XXX" Then

                Locate 17, 41
                X = 22
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(22, 0), 13);
                Locate , 55: Print Using "### "; SX%(22, 0, tickInterval);

                Print tickerPeriod$(tickInterval)
                Locate , 41

                Print Left$(SX$(22, 1), 13);
                Locate , 55: Print Using "### #"; SX%(22, 1, tickInterval)

            End If

            If Left$(SX$(23, 0), 3) <> "XXX" Then

                Locate 20, 41
                X = 23
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(23, 0), 13);
                Locate , 55: Print Using "### "; SX%(23, 0, tickInterval);

                Print tickerPeriod$(tickInterval)
                Locate , 41

                Print Left$(SX$(23, 1), 13);
                Locate , 55: Print Using "### #"; SX%(23, 1, tickInterval)

            End If

            If Left$(SX$(24, 0), 3) <> "XXX" Then

                Locate 23, 41
                X = 24
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(24, 0), 13);
                Locate , 55: Print Using "### "; SX%(24, 0, tickInterval);

                Print tickerPeriod$(tickInterval)
                Locate , 41

                Print Left$(SX$(24, 1), 13);
                Locate , 55: Print Using "### #"; SX%(24, 1, tickInterval);

            End If

            If Left$(SX$(25, 0), 3) <> "XXX" Then

                Locate 2, 61
                X = 25
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(25, 0), 13);
                Locate , 75: Print Using "### "; SX%(25, 0, tickInterval);

                Print tickerPeriod$(tickInterval)
                Locate , 61

                Print Left$(SX$(25, 1), 13);
                Locate , 75: Print Using "### #"; SX%(25, 1, tickInterval)

            End If

            If Left$(SX$(26, 0), 3) <> "XXX" Then

                Locate 5, 61
                X = 26
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(26, 0), 13);
                Locate , 75: Print Using "### "; SX%(26, 0, tickInterval);

                Print tickerPeriod$(tickInterval)
                Locate , 61

                Print Left$(SX$(26, 1), 13);
                Locate , 75: Print Using "### #"; SX%(26, 1, tickInterval)

            End If

            If Left$(SX$(27, 0), 3) <> "XXX" Then

                Locate 8, 61
                X = 27
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(27, 0), 13);
                Locate , 75: Print Using "### "; SX%(27, 0, tickInterval);

                Print tickerPeriod$(tickInterval)
                Locate , 61

                Print Left$(SX$(27, 1), 13);
                Locate , 75: Print Using "### #"; SX%(27, 1, tickInterval)

            End If

            If Left$(SX$(28, 0), 3) <> "XXX" Then

                Locate 11, 61
                X = 28
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(28, 0), 13);
                Locate , 75: Print Using "### "; SX%(28, 0, tickInterval);

                Print tickerPeriod$(tickInterval)
                Locate , 61

                Print Left$(SX$(28, 1), 13);
                Locate , 75: Print Using "### #"; SX%(28, 1, tickInterval)

            End If

            If Left$(SX$(29, 0), 3) <> "XXX" Then

                Locate 14, 61
                X = 29
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(29, 0), 13);
                Locate , 75: Print Using "### "; SX%(29, 0, tickInterval);

                Print tickerPeriod$(tickInterval)
                Locate , 61

                Print Left$(SX$(29, 1), 13);
                Locate , 75: Print Using "### #"; SX%(29, 1, tickInterval)

            End If

            If Left$(SX$(30, 0), 3) <> "XXX" Then

                Locate 17, 61
                X = 30
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(30, 0), 13);
                Locate , 75: Print Using "### "; SX%(30, 0, tickInterval);

                Print tickerPeriod$(tickInterval)
                Locate , 61

                Print Left$(SX$(30, 1), 13);
                Locate , 75: Print Using "### #"; SX%(30, 1, tickInterval)

            End If

            If Left$(SX$(31, 0), 3) <> "XXX" Then

                Locate 20, 61
                X = 31
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(31, 0), 13);
                Locate , 75: Print Using "### "; SX%(31, 0, tickInterval);

                Print tickerPeriod$(tickInterval)
                Locate , 61

                Print Left$(SX$(31, 1), 13);
                Locate , 75: Print Using "### #"; SX%(31, 1, tickInterval)

            End If

            If Left$(SX$(32, 0), 3) <> "XXX" Then

                Locate 23, 61
                X = 32
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(32, 0), 13);
                Locate , 75: Print Using "### "; SX%(32, 0, tickInterval);

                Print tickerPeriod$(tickInterval)
                Locate , 61

                Print Left$(SX$(32, 1), 13);
                Locate , 75: Print Using "### #"; SX%(32, 1, tickInterval);

            End If

        End If

        I$ = GetKeyPress$

    End If

End Sub


'----------------------------------------
'          ShotPacing Function
'----------------------------------------
' Use several factors for both team's FGA
' to determine the pacing of the game
Function ShotPacing%

    Shared FY%()

    Dim nbrPoss(0 To 1)

    'JY = Average FGA/G Adjustment For Both Teams
    '   FY%(0) = 2 * (Visitor Tm Average FGA/G - League FGA/G)
    '   FY%(1) = 2 * (Home Tm Average FGA/G - League FGA/G)
    JY = CInt(((FY%(1) + FY%(0)) / 2))

    'https://kenpom.com/blog/the-possession/
    yFactor! = .44
    For X = 0 To 1
        'Average team possessions / game
        nbrPoss(X) = ((teamStats_GAME(X, 1) - teamStats_GAME(X, 9)) + teamStats_GAME(X, 15) + (yFactor! * teamStats_GAME(X, 5))) / teamStats_GAME(X, 0)
    Next X

    'https://sports.icalculator.com/basketball-pace-factor-calculator.html
    paceFactor! = 40 * ((nbrPoss(0) + nbrPoss(1)) / 2)

    If paceFactor! = 0 Then

        'This will most often come from teams before 1997
        'Without team actual stats being entered, there
        'is no way to determine a value above
        Select Case sClockVal

            Case 0
                'No Shot Clock
                'Average 50% chance of taking shot
                '75 = ~50 FGA
                '85 = ~54 FGA
                '90 = ~55 FGA
                'If d100% <= 90 Then
                paceValue = 39 + CInt((JY - 40) * .8)
                'End If

            Case 1
                '45-Sec Shot Clock
                'Average 50% chance of taking shot
                paceValue = 39 + CInt((JY - 48) * .8)

            Case 2
                '35-Sec Shot Clock
                'Average 50% chance of taking shot
                paceValue = 39 + CInt((JY - 58) * .8)

            Case 3
                '30-Sec Shot Clock
                'Average 50% chance of taking shot
                paceValue = 39 + CInt((JY - 60) * .8)

        End Select

    Else

        'Using modern pacing formulas will always yield
        'the right number of possessions, regardless
        'of the shot clock settings
        leagFGAG = teamRat_GAME(gameIdx, 4)
        paceValue = Int((paceFactor! + 0.5) / (100 - teamRat_GAME(gameIdx, 4)))

    End If

    ShotPacing% = paceValue

End Function


'----------------------------------------
'          TickerTime Function
'----------------------------------------
'This subroutine determines which of the
'pre-determined time intervals the ticker
'display should be using.
Function TickerTime% (timeInterval, X)

    Shared tickerStart

    tickerIdx = timeInterval + (tickerStart - ST%(X)) * 3

    If tickerIdx < 0 Then tickerIdx = 0
    If tickerIdx > 14 Then tickerIdx = 14
    If gameOver = 1 Then tickerIdx = 14

    TickerTime% = tickerIdx

End Function


'----------------------------------------
'        TICKERUPDATE Subroutine
'----------------------------------------
'This subroutine calculates the updated
'scores for the ticker games.
Sub TICKERUPDATE ()

    If autoPlay = 1 And schedGame(2) <= 31 Then

        If currHalf = 1 Then

            Select Case gameClock!

                Case 721 To 960
                    If SX%(schedGame(2) + 1, 0, 1) = -1 Then
                        SX%(schedGame(2) + 1, 0, 1) = score(0, 0)
                        SX%(schedGame(2) + 1, 1, 1) = score(1, 0)
                    End If

                Case 481 To 720
                    If SX%(schedGame(2) + 1, 0, 2) = -1 Then
                        SX%(schedGame(2) + 1, 0, 2) = score(0, 0)
                        SX%(schedGame(2) + 1, 1, 2) = score(1, 0)
                    End If

                Case 241 To 480
                    If SX%(schedGame(2) + 1, 0, 3) = -1 Then
                        SX%(schedGame(2) + 1, 0, 3) = score(0, 1)
                        SX%(schedGame(2) + 1, 1, 3) = score(1, 1)
                    End If

                Case 121 To 240
                    If SX%(schedGame(2) + 1, 0, 4) = -1 Then
                        SX%(schedGame(2) + 1, 0, 4) = score(0, 0)
                        SX%(schedGame(2) + 1, 1, 4) = score(1, 0)
                    End If

                Case 0 To 120
                    If SX%(schedGame(2) + 1, 0, 5) = -1 Then
                        SX%(schedGame(2) + 1, 0, 5) = score(0, 0)
                        SX%(schedGame(2) + 1, 1, 5) = score(1, 0)
                    End If

            End Select

        Else

            If currHalf > 1 And SX%(schedGame(2) + 1, 0, 6) = -1 Then

                For I2 = 6 To 8:
                    SX%(schedGame(2) + 1, 0, I2) = score(0, 2) + score(0, 1)
                    SX%(schedGame(2) + 1, 1, I2) = score(1, 2) + score(1, 1)
                Next

            Else

                If currHalf = 2 Then

                    Select Case gameClock!

                        Case 721 To 960
                            If SX%(schedGame(2) + 1, 0, 9) = -1 Then
                                SX%(schedGame(2) + 1, 0, 9) = score(0, 0)
                                SX%(schedGame(2) + 1, 1, 9) = score(1, 0)
                            End If

                        Case 481 To 720
                            If SX%(schedGame(2) + 1, 0, 10) = -1 Then
                                SX%(schedGame(2) + 1, 0, 10) = score(0, 0)
                                SX%(schedGame(2) + 1, 1, 10) = score(1, 0)
                            End If

                        Case 241 To 480
                            If SX%(schedGame(2) + 1, 0, 11) = -1 Then
                                SX%(schedGame(2) + 1, 0, 11) = score(0, 3) + score(0, 2) + score(0, 1)
                                SX%(schedGame(2) + 1, 1, 11) = score(1, 3) + score(1, 2) + score(1, 1)
                            End If

                        Case 121 To 240
                            If SX%(schedGame(2) + 1, 0, 12) = -1 Then
                                SX%(schedGame(2) + 1, 0, 12) = score(0, 0)
                                SX%(schedGame(2) + 1, 1, 12) = score(1, 0)
                            End If

                        Case 0 To 120
                            If SX%(schedGame(2) + 1, 0, 13) = -1 Then
                                SX%(schedGame(2) + 1, 0, 13) = score(0, 0)
                                SX%(schedGame(2) + 1, 1, 13) = score(1, 0)
                            End If

                    End Select

                Else

                    'Overtime
                    SX%(schedGame(2) + 1, 0, 14) = score(0, 0)
                    SX%(schedGame(2) + 1, 1, 14) = score(1, 0)

                End If

            End If

        End If

    End If

End Sub


'----------------------------------------
'       TickerSchedule Subroutine
'----------------------------------------
'This subroutine reads in the scores from
'the ticker and uses them to update the
'corresponding schedule file.
Sub TickerSchedule

    Shared scheduleFile$

    If schedGame(2) <= 31 Then

        For X = 0 To 1
            SX$(schedGame(2) + 1, X) = gameTeams$(X)
        Next

        Open "TICKER" For Output As #1

        For I = 1 To 32
            For I1 = 0 To 1
                For I2 = 1 To 14
                    Print #1, SX%(I, I1, I2)
                Next
            Next
        Next

        For I = 1 To 32
            For I1 = 0 To 1
                Print #1, SX$(I, I1)
            Next
        Next

        For I = 1 To 32
            Print #1, ST%(I)
        Next

        Close #1

    End If

    Open diskPaths$(3) + scheduleFile$ For Random As #1 Len = SCHEDULE_SIZE_BYTES

    For X = 0 To 18
        Field #1, X * 2 As X$, 2 As Q$(X + 1), SCHEDULE_SIZE_BYTES - 2 - 2 * X As X$
    Next

    Field #1, 38 As X$, 2 As Q$(22), 2 As Q$(23), 5 As Q$(20), 5 As Q$(21)

    schedOptions(13) = 1

    For X = 0 To 18:
        LSet Q$(X + 1) = MKI$(schedOptions(X))
    Next

    For X = 0 To 1
        LSet Q$(20 + X) = diskIDs$(X)
    Next

    LSet Q$(22) = MKI$(score(0, 0))
    LSet Q$(23) = MKI$(score(1, 0))

    Put #1, schedGame(0) + schedGame(2)

    Close #1

    schedGame(2) = schedGame(2) + 1
    autoPlay = 1

End Sub


'----------------------------------------
'          HalfCourt Subroutine
'----------------------------------------
'This subroutine gives the user options
'for handling a strategy at half court
'when time is low on the clock.
'I2% = user choice for shot
Sub HalfCourt (shotChoice)

    Call FRAMEOPTION(8, P)
    Locate 10, 15

    Print gameTeams$(P); " HALF COURT OPTION"

    Locate 12, 14
    Locate , 14: Print Using "TIME REMAINING #.#"; T!
    Locate , 14: Print "0 - SHOOT A 2"
    Locate , 14: Print "1 - SHOOT A 3"

    Do
        I$ = GetKeyPress$
        shotChoice = Val(I$)
    Loop Until shotChoice >= 0 And shotChoice <= 1

    If shotChoice = 1 And threePtOpt = 0 Then
        shotChoice = 0
    End If

    Color 15, 0

    Call SCOREBOARD

End Sub


'----------------------------------------
'           WHOLAY Subroutine
'----------------------------------------
'This subroutine assignes a player ID to
'the person performing a layup
Sub WHOLAY ()

    Call Rolld100(RN0)

    Select Case RN0

        Case 1 To 38:
            ballCarrier = 0

        Case 39 To 75:
            ballCarrier = 1

        Case 76 To 83:
            ballCarrier = 2

        Case 84 To 91:
            ballCarrier = 3

        Case 92 To 100:
            ballCarrier = 4

    End Select

End Sub


'----------------------------------------
'      ChangePossession Subroutine
'----------------------------------------
'This subroutine handles swapping of
'variables related to which team is
'in possession of the ball
Sub ChangePossession

    prevBall$ = ""
    P = 1 - P
    D = 1 - D
    C1 = 2
    P9 = P

End Sub


'----------------------------------------
'         FoulCalled Subroutine
'----------------------------------------
'This subroutione handles calculations and
'Play-by-Play for when a player is fouled
'This will always process as the DEF team
'Finally, it handles the aftermath of a
'foul / posession change.
Sub FoulCalled (randomFouler)

    Shared rosterStatus()

    If autoPlay <> 1 Or playerMode <> 2 And whistle& Then
        _SndPlay whistle&
    End If

    'Determine who fouled
    If randomFouler = 1 Then

        foulPlayer = 0
        'Random # from 1 to FC rating
        M9 = Int(Rnd(1) * teamAdj(D, 0)) + 1

        'Determine fouler based on % contribution
        For M8 = 0 To 4:

            foulPlayer = foulPlayer + plyrOff_GAME!(D, lineupIdx(D, M8), 17)

            If M9 <= foulPlayer Then Exit For

        Next M8

        If M8 < 0 Or M8 > 4 Then
            M8 = Int(Rnd(1) * 5)
        End If

        foulPlayer = M8

    End If

    'Increase player / team fouls
    plyrOff_GAME!(D, lineupIdx(D, foulPlayer), 12) = plyrOff_GAME!(D, lineupIdx(D, foulPlayer), 12) + 1
    teamFouls(D) = teamFouls(D) + 1

    Call UPDATESCREEN

    If playerFatigue%(D, foulPlayer) = 0 Then
        plyrOff_GAME!(D, lineupIdx(D, foulPlayer), 3) = plyrOff_GAME!(D, lineupIdx(D, foulPlayer), 3) + 1
    End If

    If plyrOff_GAME!(D, lineupIdx(D, foulPlayer), 12) < foulsToDQ Then

        If pbpOpt = 1 Then
            If pbpDelay! > 0 Then
                Call WINDEX
                Print "FOUL ON "; players$(D, lineupIdx(D, foulPlayer))
                Call DELAY
                Print teamGender$(D) + " HAS"; plyrOff_GAME!(D, lineupIdx(D, foulPlayer), 12)
                Call DELAY
            End If
        Else
            pbpString$ = "FOUL NUMBER" + Str$(plyrOff_GAME!(D, lineupIdx(D, foulPlayer), 12)) + " CALLED ON " + players$(D, lineupIdx(D, foulPlayer))
            Call PBP(D)
        End If

        Call DELAY

    Else

        M9 = 0

        For I3 = 0 To 13
            If Not (I3 = lineupIdx(D, foulPlayer) Or players$(D, I3) = "XXX" Or rosterStatus(D, I3) <> 0) Then
                M9 = M9 + 1
            End If
        Next

        For I3 = 0 To 13

            If Not (I3 = lineupIdx(D, foulPlayer) Or players$(D, I3) = "XXX" Or rosterStatus(D, I3) <> 0) Then
                If plyrOff_GAME!(D, I3, 0) <= plyrOff_GAME!(D, lineupIdx(D, foulPlayer), 0) Then
                    plyrOff_GAME!(D, I3, 3) = plyrOff_GAME!(D, I3, 3) + CInt((plyrOff_GAME!(D, lineupIdx(D, foulPlayer), 3) / M9))
                End If
            End If

        Next I3

        'Player fouled
        If pbpOpt = 1 Then
            If pbpDelay! > 0 Then
                Call WINDEX
                Print "FOUL ON "; players$(D, lineupIdx(D, foulPlayer))
                Call DELAY
            End If
        Else
            pbpString$ = "FOUL CALLED ON " + players$(D, lineupIdx(D, foulPlayer))
            Call PBP(D)
        End If

        If intentional = 1 Then

            pbpString$ = "FOUL IS RULED INTENTIONAL!"
            Call SELECTPBP(D, 0)
            Call DELAY

        End If

        pbpString$ = "AND " + teamGender$(D) + " HAS JUST FOULED OUT !!!"
        Call SELECTPBP(D, 1)
        Call DELAY

        'Disable player from roster
        rosterStatus(D, lineupIdx(D, foulPlayer)) = -1

        If teamFouls(D) > 6 Then
            offStatus = 50
        End If

        If playerMode = 0 Or playerMode = 1 And D <> compTeam Then

            teamIdx = D
            C1 = 1
            Call PlayStyle
            Call PlayItSafe

        End If

        Call DEADBALLOPTIONS

        For I = 0 To 1
            If defCategory(I) = 0 Then
                pressureRating(I, 0) = 1
                pressureRating(I, 1) = 7
                pctContrib(I) = 0
            End If
        Next

        Call SCOREBOARD

        Call CheckGameClock

        offStatus = 0

    End If

End Sub


'----------------------------------------
'         CreditAssists Subroutine
'----------------------------------------
'This subroutine appears to be crediting
'assists, per its similarity to the code
'from Pro BB to do the same
Sub CreditAssists ()

    randPlayer = Int(Rnd(1) * 5)

    assistRating = plyrOff_GAME!(P, lineupIdx(P, randPlayer), 4)
    'assistChance = Int(Rnd(1) * 3) + 1
    Select Case assistRating

        Case 0
            assistChance = Int(Rnd(1) * 4) + 1

        Case 1 To 2
            assistChance = Int(Rnd(1) * 3) + 1

        Case 3 To 4
            assistChance = Int(Rnd(1) * 2) + 1

        Case Is >= 5
            assistChance = Int(Rnd(1) * (assistRating * 0.8)) + 1 ' Slightly higher weight

    End Select

    If ((assistChance <= assistRating) Or (assistRating > 4 And assistChance < assistRating - 3)) And (players$(P, lineupIdx(P, ballCarrier)) <> players$(P, lineupIdx(P, randPlayer))) Then

        Do

            totalAssisst = 0

            For M8 = 0 To 4
                totalAssisst = totalAssisst + plyrOff_GAME!(P, lineupIdx(P, M8), 4)
            Next

            RN0 = Int(Rnd(1) * totalAssisst) + 1

            totalAssisst = 0

            For M8 = 0 To 4
                totalAssisst = totalAssisst + plyrOff_GAME!(P, lineupIdx(P, M8), 4)
                If RN0 <= totalAssisst Then
                    Exit For
                End If
            Next

            tgtPlayer = M8

        Loop Until players$(P, lineupIdx(P, ballCarrier)) <> players$(P, lineupIdx(P, tgtPlayer))

        plyrOff_GAME!(P, lineupIdx(P, tgtPlayer), 14) = plyrOff_GAME!(P, lineupIdx(P, tgtPlayer), 14) + 1

    End If

End Sub


'----------------------------------------
'            HORN Subroutine
'----------------------------------------
'This subroutine plays a sound on the PC
'speaker that is close to the buzzer / horn
'commonly associated with basketball.

'A replacement for the near future would be
'to load and play a sound file
Sub HORN ()

    If _FileExists("buzzer.mp3") Then
        _SndPlayFile ("buzzer.mp3")
    Else
        For PP = 1 To 70
            Sound 50, .05
            Sound 100, .05
            Sound 317, .3
        Next PP
    End If

End Sub


'----------------------------------------
'         SHOTSTRING Subroutine
'----------------------------------------
'This subroutine simply builds a string
'which represents the % chance of making
'the current shot (rounding up)
Sub SHOTSTRING ()

    pbpString$ = pbpString$ + Str$(Int(shotChance!)) + "*"

End Sub


'----------------------------------------
'         REBOUNDSORT Subroutine
'----------------------------------------
'This subroutine clearly does something
'related to sorting players by rebound?
Sub REBOUNDSORT ()

    L = 3

    8827 '
    S6 = 0
    S7 = 0

    8828 '
    If plyrRebVal(S6) > plyrRebVal(S6 + 1) Then

        Swap plyrRebIdx(S6), plyrRebIdx(S6 + 1)
        Swap plyrRebVal(S6), plyrRebVal(S6 + 1)
        S7 = S6

    End If

    S6 = S6 + 1

    If S6 <= L Then
        GoTo 8828
    End If

    If S7 <> 0 Then
        L = S7 - 1
        GoTo 8827
    End If

End Sub


'----------------------------------------
'        ComputerCoach Subroutine
'----------------------------------------
'This subroutine essentially handles the
'AI for the computer coach. After an
'evaluation of substitutions, it then calls
'another routine to decide on strategies
Sub ComputerCoach ()

    'Assign and sort stamina for all players
    For I = 0 To 13

        rosterIdx(I) = I

        plyrStaminaGame(I) = playerFatigue%(P9, I)

        If currHalf = 2 And gameClock! <= 120 Or currHalf > 2 Then
            'Reset fatigue for final 2 mins or Overtime
            plyrStaminaGame(I) = plyrOff_GAME!(P9, I, 3)
        End If

        If plyrOff_GAME!(P9, I, 12) >= foulsToDQ Then
            '(plyrOff_GAME!(P9, rosterIdx(I), 12) >= foulsToDQ) Or (currHalf = 1 And plyrOff_GAME!(P9, rosterIdx(I), 12) >= foulsToDQ - 2) Or (plyrOff_GAME!(P9, rosterIdx(I), 12) >= foulsToDQ - 1 And currHalf = 2 And gameClock! > 720)
            'Fouled out player should not be used
            plyrStaminaGame(I) = 0
        End If

    Next I

    Call STAMINASORT

    'Assign and sort rebounds for all players
    rebIdx = 14

    skipLoop% = 0

    For I1 = 0 To 4

        8794 '
        rebIdx = rebIdx - 1

        If rebIdx >= 0 Then

            '8794 will just go to the next "For" entry
            'Could we use "Continue"???
            If goodPlayer%(P9, rosterIdx(rebIdx)) = 0 Then
                '(rosterStatus(P9, rosterIdx(rebIdx)) <> 0 Or players$(P9, rosterIdx(rebIdx)) = "XXX" Or (plyrOff_GAME!(P9, rosterIdx(rebIdx), 12) >= foulsToDQ) Or (currHalf = 1 And plyrOff_GAME!(P9, rosterIdx(rebIdx), 12) >= foulsToDQ - 2) Or (plyrOff_GAME!(P9, rosterIdx(rebIdx), 12) >= foulsToDQ - 1 And currHalf = 2 And gameClock! > 720))
                GoTo 8794
            End If

            'Skip players with less than 1/2 their contribution before halftime
            If currHalf = 1 And plyrStaminaGame(rebIdx) / plyrOff_GAME!(P9, rosterIdx(rebIdx), 3) < .5 Then
                GoTo 8794
            End If

            'Skip players who haven't contributed 1/4 after 10 minutes in 1st half
            If currHalf = 1 And gameClock! >= 600 And plyrStaminaGame(rebIdx) / plyrOff_GAME!(P9, rosterIdx(rebIdx), 3) < .75 Then
                GoTo 8794
            End If

            'Skip players who haven't contributed 3/4 after 10 minutes in 2nd half
            If currHalf = 2 And gameClock! >= 600 And plyrStaminaGame(rebIdx) / plyrOff_GAME!(P9, rosterIdx(rebIdx), 3) < .25 Then
                GoTo 8794
            End If

            'Skip players who haven't contributed enough in last 5-10 mins of game
            If currHalf = 2 And gameClock! >= 300 And gameClock! < 600 And plyrStaminaGame(rebIdx) / plyrOff_GAME!(P9, rosterIdx(rebIdx), 3) < .13 Then
                GoTo 8794
            End If

            'Skip players who haven't contributed enough in last 5 minutes of game
            If currHalf = 2 And gameClock! < 300 And ((plyrStaminaGame(rebIdx) / plyrOff_GAME!(P9, rosterIdx(rebIdx), 3)) < (gameClock! / 2400)) Then
                GoTo 8794
            End If

            'Skip players who are fatigued in last 2 mins / overtime
            If ((currHalf = 2 And gameClock! > 120) Or currHalf < 2) And plyrStaminaGame(rebIdx) <= 0 Then
                GoTo 8794
            End If

            'At this point the player hasn't been eliminated
            'Let's assign them to our roster
            plyrRebIdx(I1) = rosterIdx(rebIdx)
            plyrRebVal(I1) = plyrOff_GAME!(P9, rosterIdx(rebIdx), 2)

        Else

            'Try to fill with any remaining positions
            '(skipping only "bad" players - aka fatigued / "XXX"  etc.
            rebIdx = 14
            'rebIdx = 13

            For I1 = 0 To 4


                'Do While goodPlayer%(P9, rosterIdx(rebIdx)) = 0
                'rebIdx = rebIdx - 1
                'Loop

                'plyrRebIdx(I1) = rosterIdx(rebIdx)
                'plyrRebVal(I1) = plyrOff_GAME!(P9, rosterIdx(rebIdx), 2)

                8705 '
                rebIdx = rebIdx - 1

                If rebIdx >= 0 Then

                    If goodPlayer%(P9, rosterIdx(rebIdx)) = 0 Then
                        '(rosterStatus(P9, rosterIdx(rebIdx)) <> 0 Or players$(P9, rosterIdx(rebIdx)) = "XXX" Or (plyrOff_GAME!(P9, rosterIdx(rebIdx), 12) >= foulsToDQ) Or (plyrOff_GAME!(P9, rosterIdx(rebIdx), 12) >= foulsToDQ - 1 And (currHalf = 1 Or currHalf = 2 And gameClock! > 720))
                        GoTo 8705
                    Else
                        plyrRebIdx(I1) = rosterIdx(rebIdx)
                        plyrRebVal(I1) = plyrOff_GAME!(P9, rosterIdx(rebIdx), 2)
                    End If

                Else

                    'Try to fill with any remaining positions
                    '(skipping only disabled players / "XXX"
                    rebIdx = 13

                    For I1 = 0 To 4

                        Do Until (rosterStatus(P9, rosterIdx(rebIdx)) = 0 And players$(P9, rosterIdx(rebIdx)) <> "XXX") Or rebIdx = 0
                            rebIdx = rebIdx - 1
                        Loop

                        plyrRebIdx(I1) = rosterIdx(rebIdx)
                        plyrRebVal(I1) = plyrOff_GAME!(P9, rosterIdx(rebIdx), 2)

                    Next I1

                    skipLoop% = 1

                End If

                If skipLoop% = 1 Then Exit For

            Next I1

            If skipLoop% = 1 Then Exit For

        End If

    Next I1

    Call REBOUNDSORT

    'Pick top 5 players for lineup after sorting
    For I = 0 To 4
        lineupIdx(P9, I) = plyrRebIdx(I)
    Next I

    Call ComputerStrategy

End Sub


'----------------------------------------
'        TOURNEY_Game Subroutine
'----------------------------------------
'This subroutine checks the file for the
'tournament and, as needed, calls a routine
'to update it with the game scores
Sub TOURNEY_Game ()

    Dim fileLength&
    Dim loseTeam$, winTeam$

    Shared tourneyFile$

    Shared teamYrTourn$()

    'CHECK FOR FILLING IN TOURNEY SCHEDULE
    Open diskPaths$(1) + tourneyFile$ As #1 Len = 94
    fileLength& = LOF(1)
    Close #1

    numGames = fileLength& / 90

    For I = 1 To numGames

        Open diskPaths$(1) + tourneyFile$ As #1 Len = 94

        For X = 0 To 14
            Field #1, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
        Next

        Field #1, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
        Field #1, 64 As X$, 5 As Q$(20), 5 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
        Field #1, 88 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)
        Get #1, I

        VS1% = CVI(Q$(16))
        HS1% = CVI(Q$(17))

        loseTeam$ = Q$(18)
        winTeam$ = Q$(19)

        For X = 0 To 14:
            tourneyOptions(X) = CVI(Q$(X + 1))
        Next

        For X = 0 To 3:
            teamYrTourn$(X) = Q$(20 + X)
        Next

        For X = 1 To 3:
            tourneyOptions(X + 14) = CVI(Q$(23 + X))
        Next

        Close #1

        If tourneyOptions(15) = schedOptions(15) And schedOptions(15) <> 17 Then

            If score(0, 0) > score(1, 0) And tourneyOptions(0) - 900 = schedOptions(17) Then
                tourneyOptions(0) = schedOptions(0)
                teamYrTourn$(0) = diskIDs$(0)
                loseTeam$ = schedVisTm$
                Call UpdateTourneyFile(I, loseTeam$, winTeam$)
            End If

            If score(1, 0) > score(0, 0) And tourneyOptions(0) - 900 = schedOptions(17) Then
                tourneyOptions(0) = schedOptions(1)
                teamYrTourn$(0) = diskIDs$(1)
                loseTeam$ = schedHomeTm$
                Call UpdateTourneyFile(I, loseTeam$, winTeam$)
            End If

            If score(0, 0) > score(1, 0) And tourneyOptions(1) - 900 = schedOptions(17) Then
                tourneyOptions(1) = schedOptions(0)
                teamYrTourn$(1) = diskIDs$(0)
                winTeam$ = schedVisTm$
                Call UpdateTourneyFile(I, loseTeam$, winTeam$)
            End If

            If score(1, 0) > score(0, 0) And tourneyOptions(1) - 900 = schedOptions(17) Then
                tourneyOptions(1) = schedOptions(1)
                teamYrTourn$(1) = diskIDs$(1)
                winTeam$ = schedHomeTm$
                Call UpdateTourneyFile(I, loseTeam$, winTeam$)
            End If

            If score(0, 0) < score(1, 0) And tourneyOptions(0) - 800 = schedOptions(17) Then
                tourneyOptions(0) = schedOptions(0)
                teamYrTourn$(0) = diskIDs$(0)
                loseTeam$ = schedVisTm$
                Call UpdateTourneyFile(I, loseTeam$, winTeam$)
            End If

            If score(1, 0) < score(0, 0) And tourneyOptions(0) - 800 = schedOptions(17) Then
                tourneyOptions(0) = schedOptions(1)
                teamYrTourn$(0) = diskIDs$(1)
                loseTeam$ = schedHomeTm$
                Call UpdateTourneyFile(I, loseTeam$, winTeam$)
            End If

            If score(0, 0) < score(1, 0) And tourneyOptions(1) - 800 = schedOptions(17) Then
                tourneyOptions(1) = schedOptions(0)
                teamYrTourn$(1) = diskIDs$(0)
                winTeam$ = schedVisTm$
                Call UpdateTourneyFile(I, loseTeam$, winTeam$)
            End If

            If score(1, 0) < score(0, 0) And tourneyOptions(1) - 800 = schedOptions(17) Then
                tourneyOptions(1) = schedOptions(1)
                teamYrTourn$(1) = diskIDs$(1)
                winTeam$ = schedHomeTm$
                Call UpdateTourneyFile(I, loseTeam$, winTeam$)
            End If

        End If

        If schedOptions(15) <> 17 Then

            If score(0, 0) > score(1, 0) And schedOptions(16) = 89 And tourneyOptions(15) = 17 And tourneyOptions(0) - 900 = schedOptions(15) Then
                tourneyOptions(0) = schedOptions(0)
                teamYrTourn$(0) = diskIDs$(0)
                loseTeam$ = schedVisTm$
                Call UpdateTourneyFile(I, loseTeam$, winTeam$)
            End If

            If score(1, 0) > score(0, 0) And schedOptions(16) = 89 And tourneyOptions(15) = 17 And tourneyOptions(0) - 900 = schedOptions(15) Then
                tourneyOptions(0) = schedOptions(1)
                teamYrTourn$(0) = diskIDs$(1)
                loseTeam$ = schedHomeTm$
                Call UpdateTourneyFile(I, loseTeam$, winTeam$)
            End If

            If score(0, 0) > score(1, 0) And schedOptions(16) = 89 And tourneyOptions(15) = 17 And tourneyOptions(1) - 900 = schedOptions(15) Then
                tourneyOptions(1) = schedOptions(0)
                teamYrTourn$(1) = diskIDs$(0)
                winTeam$ = schedVisTm$
                Call UpdateTourneyFile(I, loseTeam$, winTeam$)
            End If

            If score(1, 0) > score(0, 0) And schedOptions(16) = 89 And tourneyOptions(15) = 17 And tourneyOptions(1) - 900 = schedOptions(15) Then
                tourneyOptions(1) = schedOptions(1)
                teamYrTourn$(1) = diskIDs$(1)
                winTeam$ = schedHomeTm$
                Call UpdateTourneyFile(I, loseTeam$, winTeam$)
            End If

        End If

        If score(0, 0) > score(1, 0) And schedOptions(15) = 17 And tourneyOptions(0) - 1100 = schedOptions(17) Then
            tourneyOptions(0) = schedOptions(0)
            teamYrTourn$(0) = diskIDs$(0)
            loseTeam$ = schedVisTm$
            Call UpdateTourneyFile(I, loseTeam$, winTeam$)
        End If

        If score(1, 0) > score(0, 0) And schedOptions(15) = 17 And tourneyOptions(0) - 1100 = schedOptions(17) Then
            tourneyOptions(0) = schedOptions(1)
            teamYrTourn$(0) = diskIDs$(1)
            loseTeam$ = schedHomeTm$
            Call UpdateTourneyFile(I, loseTeam$, winTeam$)
        End If

        If score(0, 0) > score(1, 0) And schedOptions(15) = 17 And tourneyOptions(1) - 1100 = schedOptions(17) Then
            tourneyOptions(1) = schedOptions(0)
            teamYrTourn$(1) = diskIDs$(0)
            winTeam$ = schedVisTm$
            Call UpdateTourneyFile(I, loseTeam$, winTeam$)
        End If

        If score(1, 0) > score(0, 0) And schedOptions(15) = 17 And tourneyOptions(1) - 1100 = schedOptions(17) Then
            tourneyOptions(1) = schedOptions(1)
            teamYrTourn$(1) = diskIDs$(1)
            winTeam$ = schedHomeTm$
            Call UpdateTourneyFile(I, loseTeam$, winTeam$)
        End If


        If score(0, 0) < score(1, 0) And schedOptions(15) = 17 And tourneyOptions(0) - 1000 = schedOptions(17) Then
            tourneyOptions(0) = schedOptions(0)
            teamYrTourn$(0) = diskIDs$(0)
            loseTeam$ = schedVisTm$
            Call UpdateTourneyFile(I, loseTeam$, winTeam$)
        End If

        If score(1, 0) < score(0, 0) And schedOptions(15) = 17 And tourneyOptions(0) - 1000 = schedOptions(17) Then
            tourneyOptions(0) = schedOptions(1)
            teamYrTourn$(0) = diskIDs$(1)
            loseTeam$ = schedHomeTm$
            Call UpdateTourneyFile(I, loseTeam$, winTeam$)
        End If

        If score(0, 0) < score(1, 0) And schedOptions(15) = 17 And tourneyOptions(1) - 1000 = schedOptions(17) Then
            tourneyOptions(1) = schedOptions(0)
            teamYrTourn$(1) = diskIDs$(0)
            winTeam$ = schedVisTm$
            Call UpdateTourneyFile(I, loseTeam$, winTeam$)
        End If

        If score(1, 0) < score(0, 0) And schedOptions(15) = 17 And tourneyOptions(1) - 1000 = schedOptions(17) Then
            tourneyOptions(1) = schedOptions(1)
            teamYrTourn$(1) = diskIDs$(1)
            winTeam$ = schedHomeTm$
            Call UpdateTourneyFile(I, loseTeam$, winTeam$)
        End If

        If score(0, 0) < score(1, 0) And schedOptions(15) = 17 And tourneyOptions(0) - 900 = schedOptions(17) Then
            tourneyOptions(0) = schedOptions(0)
            teamYrTourn$(0) = diskIDs$(0)
            loseTeam$ = schedVisTm$
            Call UpdateTourneyFile(I, loseTeam$, winTeam$)
        End If

        If score(1, 0) < score(0, 0) And schedOptions(15) = 17 And tourneyOptions(0) - 900 = schedOptions(17) Then
            tourneyOptions(0) = schedOptions(1)
            teamYrTourn$(0) = diskIDs$(1)
            loseTeam$ = schedHomeTm$
            Call UpdateTourneyFile(I, loseTeam$, winTeam$)
        End If

        If score(0, 0) < score(1, 0) And schedOptions(15) = 17 And tourneyOptions(1) - 900 = schedOptions(17) Then
            tourneyOptions(1) = schedOptions(0)
            teamYrTourn$(1) = diskIDs$(0)
            winTeam$ = schedVisTm$
            Call UpdateTourneyFile(I, loseTeam$, winTeam$)
        End If

        If score(1, 0) < score(0, 0) And schedOptions(15) = 17 And tourneyOptions(1) - 900 = schedOptions(17) Then
            tourneyOptions(1) = schedOptions(1)
            teamYrTourn$(1) = diskIDs$(1)
            winTeam$ = schedHomeTm$
            Call UpdateTourneyFile(I, loseTeam$, winTeam$)
        End If

    Next

End Sub


'----------------------------------------
'      UpdateTourneyFile Subroutine
'----------------------------------------
'This subroutine actually changes any data
'associated with the tournament file.
Sub UpdateTourneyFile (gameIdx, loseTeam$, winTeam$)

    Shared tourneyFile$

    Shared teamYrTourn$()

    Open diskPaths$(1) + tourneyFile$ As #1 Len = 94

    For X = 0 To 14
        Field #1, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
    Next

    Field #1, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
    Field #1, 64 As X$, 5 As Q$(20), 5 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
    Field #1, 88 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)

    For X = 0 To 14:
        LSet Q$(1 + X) = MKI$(tourneyOptions(X))
    Next

    For X = 0 To 3:
        LSet Q$(20 + X) = teamYrTourn$(X)
    Next

    For X = 1 To 3:
        LSet Q$(23 + X) = MKI$(tourneyOptions(X + 14))
    Next

    LSet Q$(16) = MKI$(score(0, 0))
    LSet Q$(17) = MKI$(score(1, 0))

    LSet Q$(18) = loseTeam$
    LSet Q$(19) = winTeam$

    Put #1, gameIdx

    Close #1

End Sub

'----------------------------------------
'         UPDATESCREEN Subroutine
'----------------------------------------
'This subroutine refreshes all the values
'for the main screen, including the
'scoreboard, team options and rosters.
'The frame will have already been drawn
'by the SCOREBOARD routine.
Sub UPDATESCREEN ()

    Call TICKERUPDATE

    'Team 1
    Locate 2, 1
    Locate , 9
    Color teamRat_GAME(0, 0), teamRat_GAME(0, 1): Print gameTeams$(0);

    'Game Clock - Minutes
    Color 15, 0
    Locate 1, 37
    Print Using "##"; Int(gameClock! / 60);
    Print ":";

    'Game Clock - Seconds
    '(Let's just round it to 2 places first)
    gcSeconds! = gameClock! - Int(gameClock! / 60) * 60
    gcSeconds! = Fix((gcSeconds! * 100 + Sgn(gcSeconds!) * .5)) / 100

    If gameClock! <= 5 Then
        'print 1/10th of a sec for last 5 seconds
        Print "0";
        Print Using "#.#"; gcSeconds!;
    Else

        If gcSeconds! < 10 Then
            Print "0";
            Print Using "#"; gcSeconds!;
        Else
            Print Using "##"; gcSeconds!;
        End If

    End If

    'Team 2
    Locate 2, 56
    Color teamRat_GAME(1, 0), teamRat_GAME(1, 1): Print gameTeams$(1)

    'Score Team 1
    Color 14, 0
    Locate 4, 13: Print Using "###"; score(0, 0);

    'Poss Arrow
    Locate 2, 33: Print " ";
    If possArrow = 0 And teamYears(impactTeam) >= 1981 Then Locate , 33: Print Chr$(17);

    'Period Designation
    Locate 2, 35
    If currHalf > 2 Then
        Print Using "OVERTIME #"; currHalf - 2;
        Locate , 46: Print " ";
        If possArrow = 1 And teamYears(impactTeam) >= 1981 Then Locate , 46: Print Chr$(16)
    Else
        Print Using "  HALF #"; currHalf;
        Locate , 44: Print " ";
        If possArrow = 1 And teamYears(impactTeam) >= 1981 Then Locate , 44: Print Chr$(16);
    End If

    'Score Team 2
    Color 14, 0
    Locate 4, 60: Print Using "###"; score(1, 0)

    'Turnovers
    Color 7
    Locate 3, 35
    Print Using "##  "; turnovers(0);
    Color 12: Print "TO  ";
    Color 7
    Print Using "##"; turnovers(1)

    'Rebounds
    Locate , 35: Print Using "##  "; totRebounds(0);
    Color 12: Print "RB  ";
    Color 7: Print Using "##"; totRebounds(1)

    'Fouls
    Locate , 35: Print Using "##  "; teamFouls(0);
    Color 12: Print "TF  ";
    Color 7: Print Using "##"; teamFouls(1);

    'Bonus
    If teamFouls(0) > 5 Then
        Color 0, 2
        Locate , 46: Print "BONUS";
        Color 7, 0
    End If

    If teamFouls(1) > 5 Then
        Color 0, 2
        Locate , 29: Print "BONUS"
        Color 7, 0
    End If

    'Timeouts
    Locate 6, 9: Print "TIMEOUTS:"; timeouts(0);
    Locate , 56: Print "TIMEOUTS:"; timeouts(1)

    Color 14, 0

    'Shot Clock
    'If no shot clock | < X secs left with X sec shot clock | Overtime with 5 Mins Left (and a shot clock)
    If sClockVal = 0 Or (gameClock! < scSettings(sClockVal) And shotClock >= gameClock!) Or (sClockVal >= 1 And currHalf > 2 And gameClock! = 300) Then
        Locate 6, 37: Print " :  "
    Else
        Locate 6, 37: Print Using " :##"; shotClock
    End If

    'Team strategies / decisions
    If (autoPlay = 0 Or pbpDelay! > 0) Then

        'Team Play Styles
        Locate 9, 41
        Color teamRat_GAME(D, 0), teamRat_GAME(D, 1)
        Print "DEF-"; defStyles_brief$(defStrat(D))
        Locate , 41
        Color teamRat_GAME(P, 0), teamRat_GAME(P, 1)
        Print "OFF-"; offStyles_brief$(offStrat(P))

        Color 15, 0
        Locate 11, 41
        Color 14: Print "(1) ";
        Color 15: Print "SHOOT        ";
        Color 14: Print "ENTER";
        Color 15: Print "-PASS"
        Locate 12, 41
        Color 14: Print "(2)  ";
        Color 15: Print gameTeams$(P);
        Print " CALL TIMEOUT     "
        Locate 13, 41
        Color 14: Print "(T)  ";
        Color 15: Print "10 MIN TICKER"
        Locate 14, 41
        Color 14: Print "(D)  ";
        Color 15: Print "CHANGE DEFENSE FROM 2-3 ZONE"
        Locate 15, 41
        Color 14: Print "(B)  ";
        Color 15: Print "BOXSCORE"

        Color 15, 0

        Locate 19, 1
        Call PrintActiveLineup(0)
        Locate 19, 40
        Call PrintActiveLineup(1)

    End If

End Sub


'----------------------------------------
'         SCOREBOARD Subroutine
'----------------------------------------
'This subroutine draws up the main screen
'for the game interface, including a frame,
'scoreboard, team options and rosters
Sub SCOREBOARD ()

    Cls

    Call TICKERUPDATE

    'Frames
    Color 9, 0

    Locate 3, 11: Print Chr$(218); String$(5, 196); Chr$(191)
    Locate , 11: Print Chr$(179);: Locate , 17: Print Chr$(179)
    Locate , 11: Print Chr$(192); String$(5, 196); Chr$(217)

    Locate 3, 58: Print Chr$(218); String$(5, 196); Chr$(191)
    Locate , 58: Print Chr$(179);: Locate , 64: Print Chr$(179)
    Locate , 58: Print Chr$(192); String$(5, 196); Chr$(217)

    'Team 1
    Locate 2, 1
    Locate , 9
    Color teamRat_GAME(0, 0), teamRat_GAME(0, 1): Print gameTeams$(0);

    'Game Clock - Minutes
    Color 15, 0
    Locate 1, 37
    Print Using "##"; Int(gameClock! / 60);
    Print ":";

    'Game Clock - Seconds
    '(Let's just round it to 2 places first)
    gcSeconds! = gameClock! - Int(gameClock! / 60) * 60
    gcSeconds! = Fix((gcSeconds! * 100 + Sgn(gcSeconds!) * .5)) / 100

    If gcSeconds! < 10 Then
        Print "0";
        Print Using "#"; gcSeconds!;
    Else
        Print Using "##"; gcSeconds!;
    End If

    'Team 2
    Locate 2, 56
    Color teamRat_GAME(1, 0), teamRat_GAME(1, 1): Print gameTeams$(1)

    'Score Team 1
    Color 14, 0
    Locate 4, 13: Print Using "###"; score(0, 0);

    'Period Designation
    Locate 2, 35
    If currHalf > 2 Then
        Print Using "OVERTIME #"; currHalf - 2
    Else
        Print Using "  HALF #"; currHalf;
    End If

    'Score Team 2
    Color 14, 0
    Locate 4, 60: Print Using "###"; score(1, 0)

    'Turnovers
    Color 7
    Locate 3, 35
    Print Using "##  "; turnovers(0);
    Color 12: Print "TO  ";
    Color 7
    Print Using "##"; turnovers(1)

    'Rebounds
    Locate , 35: Print Using "##  "; totRebounds(0);
    Color 12: Print "RB  ";
    Color 7: Print Using "##"; totRebounds(1)

    'Fouls
    Locate , 35: Print Using "##  "; teamFouls(0);
    Color 12: Print "TF  ";
    Color 7: Print Using "##"; teamFouls(1);

    'Bonus
    If teamFouls(0) > 5 Then
        Color 0, 2
        Locate , 46: Print "BONUS";
        Color 7, 0
    End If

    If teamFouls(1) > 5 Then
        Color 0, 2
        Locate , 29: Print "BONUS"
        Color 7, 0
    End If

    'Timeouts
    Locate 6, 9: Print "TIMEOUTS:"; timeouts(0);
    Locate , 56: Print "TIMEOUTS:"; timeouts(1)

    'Team strategies / decisions
    If (autoPlay = 0 Or pbpDelay! > 0) Then

        Locate 9, 41
        Color teamRat_GAME(D, 0), teamRat_GAME(D, 1)
        Print "DEF-"; defStyles_brief$(defStrat(D))
        Locate , 41
        Color teamRat_GAME(P, 0), teamRat_GAME(P, 1)
        Print "OFF-"; offStyles_brief$(offStrat(P))

        Color 15, 0

        Locate 11, 41
        Color 14: Print "(1) ";
        Color 15: Print "SHOOT        ";
        Color 14: Print "ENTER";
        Color 15: Print "-PASS"
        Locate 12, 41
        Color 14: Print "(2)  ";
        Color 15: Print gameTeams$(P);
        Print " CALL TIMEOUT     "
        Locate 13, 41
        Color 14: Print "(T)  ";
        Color 15: Print "10 MIN TICKER"
        Locate 14, 41
        Color 14: Print "(D)  ";
        Color 15: Print "CHANGE DEFENSE FROM 2-3 ZONE"
        Locate 15, 41
        Color 14: Print "(B)  ";
        Color 15: Print "BOXSCORE"

        Color 9, 0
        Locate 7, 1
        Print String$(38, 205); Chr$(203); String$(38, 205)

        Locate 8, 1
        For I = 1 To 8
            Locate , 39: Print Chr$(186)
        Next

        'Team 1 Overview
        Locate 17, 1
        Color teamRat_GAME(0, 0), teamRat_GAME(0, 1): Print gameTeams$(0);

        'Win/Loss record from current stats files
        Print Using " [##"; teamRecWins(0);: Print "-";: Print Using "##]"; teamRecLoss(0)

        Color 14, 0
        Print "# NAME         FG FT RB CX EF PA PT F"

        Call PrintActiveLineup(0)

        'Team 2 Overview
        Locate 17, 40
        Color teamRat_GAME(1, 0), teamRat_GAME(1, 1): Print gameTeams$(1);

        'Win/Loss record from current stats files
        Print Using " [##"; teamRecWins(1);: Print "-";: Print Using "##]"; teamRecLoss(1)

        Color 14, 0
        Locate , 40: Print "# NAME          FG FT RB CX EF PA PT F"
        Call PrintActiveLineup(1)

        Color 9, 0
        For I = 1 To 9:
            Locate 14 + I, 39: Print Chr$(186)
        Next

        'Color 9, 0
        Locate 16, 1: Print String$(79, 205)
        Locate 16, 39: Print Chr$(206)
        Color 7, 0

    End If

End Sub


'----------------------------------------
'      PrintActiveLineup Subroutine
'----------------------------------------
'This subroutine displays the summary of
'the current lineup at the bottom of the
'game screen
Sub PrintActiveLineup (teamIdx)

    'Team Roster
    For I1 = 0 To 4

        If teamIdx = 1 Then Locate , 40

        Color 2: Print Using "# "; I1;
        Color 15

        'Player Name
        Print Left$(players$(teamIdx, lineupIdx(teamIdx, I1)), 12);

        If teamIdx = 0 Then
            Locate , 15
        Else
            Locate , 55
        End If

        'If player has taken more than 7% of shots as 3FG
        If plyrOff_GAME!(teamIdx, lineupIdx(teamIdx, I1), 19) >= 7 Then
            Print "*";
        End If

        If teamIdx = 0 Then
            Locate , 16
        Else
            Locate , 56
        End If

        'Field Goals
        Print Using "##"; plyrOff_GAME!(teamIdx, lineupIdx(teamIdx, I1), 0);

        'Free Throw %
        ftPct = plyrOff_GAME!(teamIdx, lineupIdx(teamIdx, I1), 1)
        If ftPct >= 100 Then ftPct = 99
        Print Using "### "; ftPct;

        'Rebounds
        If P = teamIdx Then
            Print Using "##"; plyrOff_GAME!(teamIdx, lineupIdx(teamIdx, I1), 21) * 2 / 10;
        Else
            Print Using "##"; plyrOff_GAME!(teamIdx, lineupIdx(teamIdx, I1), 2) * 2 / 10;
        End If

        'Stamina
        J = 0
        J = playerFatigue%(teamIdx, I1)
        If J <= 0 Then J = 0
        If J <= 0 And (currHalf = 1 Or currHalf = 2 And gameClock! > 120) Then Color 12
        Print Using "### "; J;

        Color 15

        'Effective Fatigue
        If tmFatigue(teamIdx, lineupIdx(teamIdx, I1)) * -1 >= 5 And (currHalf = 1 Or currHalf = 2 And gameClock! > 120) Then Color 12
        Print Using "## "; tmFatigue(teamIdx, lineupIdx(teamIdx, I1)) * -1;

        Color 15

        'Pass Rating
        Print Using "##"; plyrOff_GAME!(teamIdx, lineupIdx(teamIdx, I1), 4);

        'Total Points
        Print Using "### "; plyrOff_GAME!(teamIdx, lineupIdx(teamIdx, I1), 8) * 2 + plyrOff_GAME!(teamIdx, lineupIdx(teamIdx, I1), 10) + threeFG(teamIdx, lineupIdx(teamIdx, I1), 0);

        'Fouls
        If plyrOff_GAME!(teamIdx, lineupIdx(teamIdx, I1), 12) >= (foulsToDQ - 1) Then Color 12
        Print Using "#"; plyrOff_GAME!(teamIdx, lineupIdx(teamIdx, I1), 12)

        Color 15

    Next I1

End Sub


'----------------------------------------
'         ColorChange Subroutine
'----------------------------------------
'This routine checks to see if both teams
'have conflicting primary colors.
'If so, it will substitute the secondary
'color with white/grey and reverse them.
'In essence, it's the digital equivalent
'of assigning an alternate jersey color.
Sub ColorChange ()

    If teamRat_GAME(1, 0) = 15 And teamRat_GAME(1, 1) = 4 Then
        teamRat_GAME(1, 0) = 4
        teamRat_GAME(1, 1) = 7
    End If

    If teamRat_GAME(1, 0) = 15 And teamRat_GAME(1, 1) = 1 Then
        teamRat_GAME(1, 0) = 1
        teamRat_GAME(1, 1) = 7
    End If

    If teamRat_GAME(1, 0) = 0 And teamRat_GAME(1, 1) = 4 Then
        teamRat_GAME(1, 0) = 12
        teamRat_GAME(1, 1) = 0
    End If

    If teamRat_GAME(1, 0) = 1 And teamRat_GAME(1, 1) = 7 Then
        teamRat_GAME(1, 0) = 15
        teamRat_GAME(1, 1) = 1
    End If

    If teamRat_GAME(1, 0) = 4 And teamRat_GAME(1, 1) = 7 Then
        teamRat_GAME(1, 0) = 15
        teamRat_GAME(1, 1) = 4
    End If

End Sub


'----------------------------------------
'       LOADSTATS Subroutine
'----------------------------------------
'This subroutine loads team schedule stats
'data into arrays for each team from their
'corresponding stat file.
Sub LOADSTATS ()

    Shared powerRating(), teamScore(), oppScore()

    Shared gameAttendance&()

    Shared gameSite$(), locIndicator$(), oppName$()

    For I = 0 To 1

        Color 15, 0
        Cls

        statFile$ = nameStatFile$(gameTeams$(I))

        If _FileExists(diskPaths$(1) + statFile$ + "S") Then

            Call ReadScheduleStats(statFile$, numberGames)

            For I1 = 1 To numberGames
                If teamScore(I1) > oppScore(I1) Then
                    teamRecWins(I) = teamRecWins(I) + 1
                End If
                If teamScore(I1) < oppScore(I1) Then
                    teamRecLoss(I) = teamRecLoss(I) + 1
                End If
            Next

        End If

    Next

End Sub


'----------------------------------------
'         ReboundOffense Subroutine
'----------------------------------------
'This subroutine clearly does something
'related to offensive rebounds
Sub ReboundOffense ()

    RB = 0

    For I = 0 To 4:

        rebRatings(I) = 0
        XZ = plyrOff_GAME!(P, lineupIdx(P, I), 21)

        If Not (currHalf > 2 Or currHalf = 2 And gameClock! <= 120) Then
            If playerFatigue%(P, lineupIdx(P, I)) <= 0 Then
                XZ = CInt(XZ * .5)
            End If
        End If

        If tmFatigue(P, lineupIdx(P, I)) <= -5 Then
            XZ = CInt(XZ * .9)
        End If

        rebRatings(I) = rebRatings(I) + XZ
        RB = RB + XZ

    Next

    rndReb = Rnd(1) * RB

    skipRandom% = 0

    For ballCarrier = 0 To 4

        If rndReb <= rebRatings(ballCarrier) Then
            skipRandom% = 1
            Exit For
        End If

        rebRatings(ballCarrier + 1) = rebRatings(ballCarrier + 1) + rebRatings(ballCarrier)

    Next

    If skipRandom% = 0 Then
        ballCarrier = Int(Rnd(1) * 5)
    End If

End Sub


'----------------------------------------
'         ReboundDefense Subroutine
'----------------------------------------
'This subroutine determines which player
'gets the rebound, based on the team's
'distribution of total rebounds
Sub ReboundDefense ()

    RB = 0

    For I = 0 To 4

        rebRatings(I) = 0
        XZ = plyrOff_GAME!(1 - P, lineupIdx(1 - P, I), 2)

        If Not (currHalf > 2 Or currHalf = 2 And gameClock! <= 120) Then
            If playerFatigue%(1 - P, lineupIdx(1 - P, I)) <= 0 Then
                XZ = CInt(XZ * .5)
            End If
        End If

        If tmFatigue(1 - P, lineupIdx(1 - P, I)) <= -5 Then
            XZ = CInt(XZ * .9)
        End If

        rebRatings(I) = rebRatings(I) + XZ
        RB = RB + XZ

    Next

    rndReb = Rnd(1) * RB

    skipRandom% = 0

    For ballCarrier = 0 To 4

        If rndReb <= rebRatings(ballCarrier) Then
            skipRandom% = 1
            Exit For
        End If

        rebRatings(ballCarrier + 1) = rebRatings(ballCarrier + 1) + rebRatings(ballCarrier)

    Next

    If skipRandom% = 0 Then
        ballCarrier = Int(Rnd(1) * 5)
    End If

End Sub


'----------------------------------------
'     ChangeContributions Subroutine
'----------------------------------------
'This subroutine lets the user adjust the
'contribution ratings for players
Sub ChangeContributions ()

    If autoPlay <> 1 Or playerMode <> 2 Then

        Do

            Color 15, 0
            Cls

            Print "EDIT CONTRIBUTION PERCENTAGES OF PLAYERS FROM:"
            Print
            Color 14
            Print "(0) - ";: Color teamRat_GAME(0, 0), teamRat_GAME(0, 1)
            Print gameTeams$(0): Color , 0
            Print
            Color 14
            Print "(1) - ";: Color teamRat_GAME(1, 0), teamRat_GAME(1, 1)
            Print gameTeams$(1): Color , 0
            Print
            Color 14
            Print "(2) - ";: Color 3: Print "PROCEED TO GAME"
            Color 15, 0
            Print
            Print "NOTE:: THE CONTRIBUTION RATING CONSISTS OF THE PLAYER'S"
            Print "       FREQUENCY TO SHOOT, REBOUND, AND COMMIT FOULS."

            Do
                I$ = GetKeyPress$
                teamChoice = Val(I$)
            Loop Until teamChoice >= 0 And teamChoice <= 2

            If teamChoice <> 2 Then

                Cls

                TX = 0

                For I1 = 0 To 13:
                    TX = TX + plyrOff_GAME!(teamChoice, I1, 3)
                Next

                For I1 = 0 To 13:
                    plyrOff_GAME!(teamChoice, I1, 3) = plyrOff_GAME!(teamChoice, I1, 3) / TX * 100
                Next

                Do

                    Locate 2, 1
                    Print "CONTRIBUTION RATINGS FOR ";
                    Color teamRat_GAME(teamChoice, 0), teamRat_GAME(teamChoice, 1): Print gameTeams$(teamChoice)
                    Color 15, 0
                    Print
                    Print " #  PLAYER             CX"
                    TY% = 0

                    For I1 = 0 To 13:

                        TY% = TY% + plyrOff_GAME!(teamChoice, I1, 3)
                        If rosterStatus(teamChoice, I1) = -1 Then Color teamRat_GAME(teamChoice, 0), teamRat_GAME(teamChoice, 1)

                        Print Using "## "; I1 + 1;
                        Print players$(teamChoice, I1);
                        Locate , 24
                        Print Using "##"; plyrOff_GAME!(teamChoice, I1, 3)
                        Color 15, 0

                    Next

                    Locate , 23: Print Using "###"; TY%

                    Input "SELECT PLAYER TO CHANGE (0 TO CONTINUE)"; I1
                    I1 = I1 - 1

                    If I1 <= 13 And I1 >= 0 Then

                        Locate 22, 1
                        Print "ENTER NEW CX RATING FOR "; players$(teamChoice, I1); " ";
                        Input plyrOff_GAME!(teamChoice, I1, 3)
                        Locate 22, 1: Print Space$(52)

                    Else

                        If TY% <> 100 Then
                            Locate 22, 1: Print "TOTAL MUST ADD UP TO 100%! PRESS ANY KEY"
                            I$ = GetKeyPress$
                            Locate 22, 1: Print Space$(52)
                        End If

                    End If

                Loop Until I1 < 0

                For I1 = 0 To 13:
                    plyrOff_GAME!(teamChoice, I1, 3) = plyrOff_GAME!(teamChoice, I1, 3) / 100 * TX
                Next

            End If

        Loop Until teamChoice = 2

    End If

    offStatus = 0

End Sub


'----------------------------------------
'         ZONECHANGE Subroutine
'----------------------------------------
'This subroutine lets the user configure
'their zone strategy, except for a 2-3 / 3-2.
Sub ZONECHANGE ()

    If (playerMode = 1 And compTeam = P9 Or playerMode = 0) And (defStrat(D) = 2 Or defStrat(D) = 7 Or defStrat(D) = 12 Or defStrat(D) >= 15) And (offStrat(P) <= 8 And offStrat(P) >= 4) Then

        Call FRAMEOPTION(8, P)
        Locate 10, 15

        Print gameTeams$(D); " SELECT STRATEGY:"
        Locate 12, 14
        Locate , 14: Print "0 - SOLID MAN-TO-MAN"
        Locate , 14: Print "1 - PRESSURE MAN-TO-MAN"
        Locate , 14: Print "2 - 1-3-1 ZONE (TRAPPING)"

        Do
            I$ = GetKeyPress$
            I1 = Val(I$)
        Loop Until I1 >= 0 And I1 <= 2

        If I1 = 2 Then I1 = 3

        Select Case defStrat(D)

            Case Is <= 3, 15
                'If currently a single defense, simply
                'make the straight up change
                defStrat(D) = I1

            Case 7
                'If a 2-2-1, shift to the appropriate zone
                If I1 = 2 Then
                    '(I1 + 8) will go past 2-2-1 options
                    defStrat(D) = 3
                Else
                    defStrat(D) = I1 + 8
                End If

            Case 12
                If I1 = 2 Then
                    '(I1 + 10) will go past Diamond options
                    defStrat(D) = 13
                Else
                    defStrat(D) = I1 + 10
                End If

        End Select

        P9 = D
        
        Call CategorizeDefense(P9)

        Color 15, 0

    End If

    Color 7, 0

End Sub


'----------------------------------------
'         DEFENSEMENU Subroutine
'----------------------------------------
'This subroutine gives the user OPTIONS
'for the various defensive play styles.
'Once chosen, it assigns the appropriate
'value to the array for tracking.
Sub DEFENSEMENU ()

    Do

        Cls

        wrongChoice% = 0

        Print ; Tab(33); "DEFENSIVE STYLE"
        Print

        For I = 0 To 17
            Locate , 5: Print Using "# "; I;
            If I = 10 Then Locate , 5: Print "A ";
            If I = 11 Then Locate , 5: Print "B ";
            If I = 12 Then Locate , 5: Print "C ";
            If I = 13 Then Locate , 5: Print "D ";
            If I = 14 Then Locate , 5: Print "E ";
            If I = 15 Then Locate , 5: Print "F ";
            If I = 16 Then Locate , 5: Print "G ";
            If I = 17 Then Locate , 5: Print "H ";
            Print defStyles$(I)
        Next

        Print
        Locate 22, 5: Print "SELECT DEFENSIVE STYLE FOR "; gameTeams$(P9)

        Do
            I$ = GetKeyPress$
            I1 = Val(I$)
            Call LetterToNumber(I$, I1)
        Loop Until I1 >= 0 And I1 <= 17

        defStrat(P9) = I1

        Call CategorizeDefense(P9)

        If defStrat(P9) = 14 And (currHalf < 2 Or currHalf >= 2 And gameClock! > 180) Then
            Beep
            Locate 23, 1: Print "CAN ONLY USE LAST 3 MIN OF 2ND HALF OR OT"
            I$ = GetKeyPress$
            wrongChoice% = 1
        End If

    Loop Until wrongChoice% = 0

    Color 15, 0

End Sub


'----------------------------------------
'         SETSTAMINA Subroutine
'----------------------------------------
'This subroutine calculates and assigns
'stamina ratings to players
Sub SETSTAMINA ()

    SQ% = 0

    For I = 0 To 13
        If teamRat_GAME(P9, 5) = 0 And plyrRat_GAME(P9, I, 0) = 0 And rosterStatus(P9, I) = 0 Then
            SQ% = SQ% + plyrOff_GAME!(P9, I, 3)
        End If
        If teamRat_GAME(P9, 5) <> 0 And plyrRat_GAME(P9, I, 0) <> 0 And rosterStatus(P9, I) = 0 Then
            SQ% = SQ% + plyrOff_GAME!(P9, I, 3) * teamRat_GAME(P9, 5) / plyrRat_GAME(P9, I, 0)
            plyrOff_GAME!(P9, I, 3) = plyrOff_GAME!(P9, I, 3) * teamRat_GAME(P9, 5) / plyrRat_GAME(P9, I, 0)
        End If
    Next

    'So SQ% is a sum of plyrOff_GAME!(P9, X, 3) values...
    For I = 0 To 13
        If teamRat_GAME(P9, 5) = 0 And plyrRat_GAME(P9, I, 0) = 0 And rosterStatus(P9, I) = 0 Then
            plyrOff_GAME!(P9, I, 3) = CInt(plyrOff_GAME!(P9, I, 3) / SQ% * gameAvgTmStamina)
        End If
        If teamRat_GAME(P9, 5) <> 0 And plyrRat_GAME(P9, I, 0) <> 0 And rosterStatus(P9, I) = 0 Then
            plyrOff_GAME!(P9, I, 3) = CInt(plyrOff_GAME!(P9, I, 3) / SQ% * gameAvgTmStamina)
        End If
        If plyrOff_GAME!(P9, I, 3) < 1 Then
            plyrOff_GAME!(P9, I, 3) = 1
        End If
    Next I

    SQ% = 0

    For I = 0 To 13
        If rosterStatus(P9, I) = 0 Then SQ% = SQ% + plyrOff_GAME!(P9, I, 3)
    Next

    For I = 0 To 13
        If gameAvgTmStamina > 120 And teamAdj(P9, 8) <> 99 Then
            plyrOff_GAME!(P9, I, 16) = plyrOff_GAME!(P9, I, 16) * (120 / gameAvgTmStamina)
            plyrOff_GAME!(P9, I, 16) = CInt(plyrOff_GAME!(P9, I, 16))
        End If
    Next I

    rebFoulChance = 8

    If gameAvgTmStamina > 114 Then
        rebFoulChance = CInt((114 / gameAvgTmStamina) * rebFoulChance)
    End If

End Sub


'----------------------------------------
'        COMPUTERSUBS Subroutine
'----------------------------------------
'This subroutine uses some basic logic to
'automatically handle player substitution
Sub COMPUTERSUBS ()

    If currHalf >= 2 And gameClock! <= 60 And threePtOpt = 1 And (score(P9, 0) - score(1 - P9, 0) = -3 Or score(P9, 0) - score(1 - P9, 0) = -6) Then
        offStrat(P9) = 9
    End If

    fatiguePlayers = 0
    disabledPlayers = 0
    tiredPlayers = 0
    foulPlayers = 0
    skipG9% = 0

    'offStatus = 50 means team is in BONUS
    If Not (offStatus = 50 And (playerMode = 1 And P9 = compTeam) Or (playerMode = 2 And P9 = P)) Then

        For I1 = 0 To 13

            M6 = playerFatigue%(P9, I1)

            If currHalf < 2 And M6 <= 0 Or currHalf = 2 And gameClock! > 120 And M6 <= 0 Then
                fatiguePlayers = fatiguePlayers + 1
            End If

            'Disabled player or inactive spot
            'Based on roster name, disable status or # of fouls so far
            If goodPlayer%(P9, rosterIdx(I)) = 0 Then
                '(rosterStatus(P9, I1) <> 0 Or players$(P9, I1) = "XXX" Or plyrOff_GAME!(P9, I1, 12) >= foulsToDQ)
                disabledPlayers = disabledPlayers + 1
            End If

            If I1 <= 4 Then

                M6 = playerFatigue%(P9, I1)

                If currHalf = 1 And M6 / plyrOff_GAME!(P9, lineupIdx(P9, I1), 3) < .5 Then
                    tiredPlayers = tiredPlayers + 1
                End If

                If currHalf = 1 And gameClock! >= 600 And M6 / plyrOff_GAME!(P9, lineupIdx(P9, I1), 3) < .75 Then
                    tiredPlayers = tiredPlayers + 1
                End If

                If currHalf = 2 And gameClock! >= 600 And M6 / plyrOff_GAME!(P9, lineupIdx(P9, I1), 3) < .25 Then
                    tiredPlayers = tiredPlayers + 1
                End If

                If currHalf = 2 And gameClock! >= 300 And gameClock! < 600 And M6 / plyrOff_GAME!(P9, lineupIdx(P9, I1), 3) < .13 Then
                    tiredPlayers = tiredPlayers + 1
                End If

                If currHalf = 2 And gameClock! < 300 And M6 / plyrOff_GAME!(P9, lineupIdx(P9, I1), 3) < gameClock! / 2400 Then
                    tiredPlayers = tiredPlayers + 1
                End If

                If M6 <= 0 And currHalf < 2 Or M6 <= 0 And currHalf = 2 And gameClock! > 120 Then
                    tiredPlayers = tiredPlayers + 1
                End If

                If goodPlayer%(P9, lineupIdx(P9, I1)) = 0 Then
                    '((plyrOff_GAME!(P9, lineupIdx(P9, I1), 12) >= foulsToDQ) Or (plyrOff_GAME!(P9, lineupIdx(P9, I1), 12) >= foulsToDQ - 2 And currHalf = 1) Or (plyrOff_GAME!(P9, lineupIdx(P9, I1), 12) >= foulsToDQ - 1 And currHalf = 2 And gameClock! > 720)) Then
                    foulPlayers = foulPlayers + 1
                End If

            End If

        Next

        'Some notes about these conditions:
        '-- XS = 0 during regular play
        '-- XS = 1 when the quarter is over
        If (tiredPlayers >= 1 And 5 - tiredPlayers < 14 - fatiguePlayers) Or (foulPlayers >= 1 And disabledPlayers < 13) Or (currHalf = 2 And gameClock! < 120 And XS = 0 And G9%(P9) = 0) Or computerTimeout = 1 Then
            computerTimeout = 0
            Call ComputerCoach
            skipG9% = 1
        End If

    End If

    If skipG9% = 0 Then
        If currHalf = 2 And gameClock! <= 120 Then
            G9%(P9) = 1
        End If
    End If

End Sub


'----------------------------------------
'           FATIGUE Subroutine
'----------------------------------------
'This subroutine picks the appropriate
'defense for the computer team.
Sub FATIGUE ()

    'COMPUTER DEFENSE FOR FATIGUE
    FD% = 0
    FQ% = 0

    For I = 0 To 4

        FD% = FD% + tmFatigue(P9, lineupIdx(P9, I))

        If tmFatigue(P9, lineupIdx(P9, I)) <= -4 Then
            FQ% = 1
        End If

    Next

    'FQ% = 1 means 1 or more player's fatigue is -4 or lower

    If FD% < -15 Or FQ% = 1 Then
        'If the team's total fatigue is really low,
        'or a single player is depleted...

        'Switch to a 2-3 or 3-2
        If (d100% <= 80) Then
            defStrat(P9) = 2
        Else
            defStrat(P9) = 15
        End If

        Call CategorizeDefense(P9)

    End If

End Sub


'----------------------------------------
'          PlayStyle Subroutine
'----------------------------------------
'This subroutine walks the user through
'the selection of play styles for their
'team on both offense and defense.
Sub PlayStyle ()

    Call LINEUPS

    'Select the lineup
    Color 14, 0
    Locate 19, 1: Print "STARTING LINEUP"

    If C1 = 1 Then

        For M = 0 To 4

            Locate M + 20, 1
            Color 2, 0
            Print Using "# "; M;
            Color 15, 0
            Print "ENTER "; posnName$(M); " # ";

            Z = lineupIdx(P9, M)

            nonPlayers = 0

            For IP% = 0 To Z
                If rosterStatus(P9, IP%) <> 0 Or players$(P9, IP%) = "" Or players$(P9, IP%) = "XXX" Then
                    nonPlayers = nonPlayers + 1
                End If
            Next

            Print players$(P9, Z);
            Locate Z + 3 - nonPlayers, 3
            Color teamRat_GAME(P9, 0), teamRat_GAME(P9, 1)
            Print Left$(players$(P9, Z), 12)
            Color 15, 0

        Next

    Else

        For I = 0 To 4

            Do

                Locate I + 20, 1
                Color 2, 0
                Print Using "# "; I;
                Color 15
                Print "ENTER "; posnName$(I); " # ";

                I$ = GetKeyPress$
                Call LetterToNumber(I$, posn)

                If posn >= 0 And posn <= 13 And plyrOff_GAME!(P9, posn, 12) < foulsToDQ And players$(P9, posn) <> "XXX" And rosterStatus(P9, posn) = 0 Then
                    For I2 = 0 To 4
                        If lineupIdx(P9, I2) = posn Then Exit For
                    Next I2
                End If

            Loop Until posn >= 0 And posn <= 13

            lineupIdx(P9, I) = posn
            nonPlayers = 0

            For IP% = 0 To lineupIdx(P9, I)
                If rosterStatus(P9, IP%) <> 0 Or players$(P9, IP%) = "" Or players$(P9, IP%) = "XXX" Then
                    nonPlayers = nonPlayers + 1
                End If
            Next

            Print players$(P9, posn);
            Locate posn + 3 - nonPlayers, 3: Color teamRat_GAME(P9, 0), teamRat_GAME(P9, 1)
            Print Left$(players$(P9, posn), 12)
            Color 15, 0
        Next

    End If

    Do

        Do
            Locate 25, 1
            Color 12, 0
            Print "# TO CHANGE (0-4) 6=NONE";
            Locate 1, 1
            I$ = GetKeyPress$
            K = Val(I$)

        Loop Until K >= 0 And K <= 4 Or K = 6

        If K = 6 Then

            skipMenu% = 0

            nonPlayers = 0

            For I1 = 0 To 13
                If goodPlayer%(P9, I1) = 0 Then
                    'rosterStatus(P9, I1) <> 0 Or players$(P9, I1) = "XXX" Or plyrOff_GAME!(P9, I1, 12) >= foulsToDQ Then
                    nonPlayers = nonPlayers + 1
                End If
            Next

            If 14 - nonPlayers >= 5 Then

                For I1 = 0 To 4

                    If plyrOff_GAME!(P9, lineupIdx(P9, I1), 12) >= foulsToDQ Then
                        Locate 20, 29
                        Color 14, 0: Print players$(P9, lineupIdx(P9, I1)); " HAS FOULED OUT"
                        skipMenu% = 1
                        Exit For

                    End If

                Next

            End If

            If skipMenu% = 0 Then Call CHANGESTRATEGY

        Else

            If offStatus = 50 And K = ballCarrier And P9 = P Then

                Color 14, 0
                Locate 20, 29: Print players$(P9, lineupIdx(P9, K)); " IS AT THE FT LINE"

            Else

                Locate 25, 1: Print Space$(25);

                J = lineupIdx(P9, K)
                Z = J
                playerTime(P9, J) = gameClock!

                nonPlayers = 0

                For IP% = 0 To Z
                    If rosterStatus(P9, IP%) <> 0 Or players$(P9, IP%) = "" Or players$(P9, IP%) = "XXX" Then
                        nonPlayers = nonPlayers + 1
                    End If
                Next

                Locate J + 3 - nonPlayers, 3
                Color 15, 0: Print Left$(players$(P9, J), 12)
                Locate K + 20, 1: Print Space$(26);

                lineupIdx(P9, K) = -1

                Locate , 1
                Color 2: Print Using "# "; K;
                Color 15
                Print "ENTER "; posnName$(K); " # ";

                Do
                    I$ = GetKeyPress$
                    Call LetterToNumber(I$, posn)

                    If posn >= 0 And posn <= 13 And plyrOff_GAME!(P9, posn, 12) < foulsToDQ And players$(P9, posn) <> "XXX" And rosterStatus(P9, posn) = 0 Then
                        For I2 = 0 To 4
                            If lineupIdx(P9, I2) = posn Then Exit For
                        Next I2

                    End If

                Loop Until posn >= 0 And posn <= 13

                lineupIdx(P9, K) = posn
                Z = posn

                'FATIGUE
                If playerTime(P9, posn) - gameClock! > 60 And playerTime(P9, posn) < 120 Then
                    tmFatigue(P9, posn) = tmFatigue(P9, posn) + 1
                End If

                If playerTime(P9, posn) - gameClock! >= 120 Then
                    tmFatigue(P9, posn) = tmFatigue(P9, posn) + 4
                End If

                If tmFatigue(P9, posn) > 0 Then
                    tmFatigue(P9, posn) = 0
                End If

                nonPlayers = 0

                For IP% = 0 To Z
                    If rosterStatus(P9, IP%) <> 0 Or players$(P9, IP%) = "" Or players$(P9, IP%) = "XXX" Then
                        nonPlayers = nonPlayers + 1
                    End If
                Next

                Print players$(P9, posn);
                Locate posn + 3 - nonPlayers, 3
                Color teamRat_GAME(P9, 0), teamRat_GAME(P9, 1): Print Left$(players$(P9, posn), 12)
                Color 15, 0

            End If

        End If

    Loop Until K = 6



End Sub


'----------------------------------------
'           LINEUPS Subroutine
'----------------------------------------
'This subroutine displays the starting
'lineup for each team and gives the user
'an interface for changing the lineup
Sub LINEUPS ()

    Shared alpha$()

    Cls
    Color teamRat_GAME(P9, 0), teamRat_GAME(P9, 1): Print gameTeams$(P9);

    Color 15, 0
    Print " TEAM ROSTER    COACH: "; gameCoaches$(P9);

    Print "  RECORD: ";
    Print Using "##"; teamRat_GAME(P9, 7);
    Print "-";
    Print Using "##"; teamRat_GAME(P9, 8)

    Color 14
    Print "# NAME         POS FG  FT 3FG %3A ORB DRB RB PA  S  B CX EF  PT  F  FC  PTS/G"

    Locate 3, 1

    For R1 = 0 To 13

        If rosterStatus(P9, R1) = 0 And players$(P9, R1) <> "XXX" And players$(P9, R1) <> "" Then

            Color 2
            If R1 <= 9 Then
                Print Using "# "; R1;
            Else
                Print alpha$(R1 - 10);
            End If

            Color 15
            Print Left$(players$(P9, R1), 12);
            Locate , 16: Print positions_GAME$(P9, R1);
            Locate , 20

            L = 0
            L = playerFatigue%(P9, R1)
            If L <= 0 Then L = 0

            'FG, FT, 3FG, %3A, ORB, DRB, TRB
            Print Using "## ###  ##  ##  ##  ## ## "; plyrOff_GAME!(P9, R1, 0), plyrOff_GAME!(P9, R1, 1), plyrOff_GAME!(P9, R1, 20), plyrOff_GAME!(P9, R1, 19), plyrOff_GAME!(P9, R1, 21) * 2 / 10, plyrOff_GAME!(P9, R1, 2) * 2 / 10, (plyrOff_GAME!(P9, R1, 2) + plyrOff_GAME!(P9, R1, 21)) / 5;
            'Pass Rating, Steals, Blocks
            Print Using "## ## ##"; plyrOff_GAME!(P9, R1, 4), plyrOff_GAME!(P9, R1, 5), plyrOff_GAME!(P9, R1, 6);

            'Contribution
            If L <= 0 And (currHalf = 1 Or currHalf = 2 And gameClock! > 120) Then Color 12
            Print Using "### "; L;

            Color 15

            'Effective Fatigue
            If tmFatigue(P9, R1) * -1 >= 5 And (currHalf = 1 Or currHalf = 2 And gameClock! > 120) Then Color 12
            Print Using "##"; tmFatigue(P9, R1) * -1;

            Color 15

            'Points
            Locate , 61: Print Using "###  "; plyrOff_GAME!(P9, R1, 8) * 2 + plyrOff_GAME!(P9, R1, 10) + threeFG(P9, R1, 0);

            'Fouls
            If plyrOff_GAME!(P9, R1, 12) >= 4 Then Color 12
            Print Using "# "; plyrOff_GAME!(P9, R1, 12);

            Color 15

            'Fouls Committed
            Print Using "###   "; plyrOff_GAME!(P9, R1, 17);

            'Points / Game
            If plyrStat_GAME(P9, R1, 0) = 0 Then
                Print "--.-"
            Else
                Print Using "##.#"; (plyrStat_GAME(P9, R1, 3) * 2 + plyrStat_GAME(P9, R1, 5) + plyrStat_GAME(P9, R1, 13)) / plyrStat_GAME(P9, R1, 0)
            End If

        End If

    Next R1

    Locate 18, 1: Color 9, 0
    Print String$(27, 205); Chr$(203); String$(51, 205)

    For R1 = 1 To 7:
        Locate R1 + 18, 28
        Print Chr$(186);
    Next


End Sub


'----------------------------------------
'          PlayStyle Subroutine
'----------------------------------------
'This subroutine walks the user through
'the selection of play styles for their
'team on both offense and defense.
Sub CHANGESTRATEGY ()

    109 '
    Color teamRat_GAME(P9, 0), teamRat_GAME(P9, 1)
    Cls
    Print ; Tab(33); "OFFENSIVE STYLE"
    Print

    For I = 0 To 9
        Locate , 5: Print Using "# "; I;: Print offStyles$(I)
    Next

    Print
    Locate 14, 5: Print "SELECT OFFENSIVE STYLE FOR "; gameTeams$(P9)

    Do
        I$ = GetKeyPress$
        I1 = Val(I$)
    Loop Until I1 >= 0 And I1 <= 9

    If (I1 = 3 Or I1 = 4 Or I1 = 7 Or I1 = 9) And (currHalf < 2 Or currHalf >= 2 And gameClock! > 180) Then
        Beep
        Locate 18, 1: Print "CAN ONLY USE LAST 3 MIN OF 2ND HALF OR OT"
        I$ = GetKeyPress$
        GoTo 109
    End If

    If I1 = 8 And defCoverage(1 - P9) <> 2 Then
        Beep
        Locate 18, 1: Print "DEFENSE NOT IN PASSIVE ZONE"
        I$ = GetKeyPress$
        GoTo 109
    End If

    If I1 = 4 And score(P9, 0) - score(1 - P9, 0) > -11 Then
        Beep
        Locate 18, 1: Print "ILLEGAL SELECTION"
        I$ = GetKeyPress$
        GoTo 109
    End If

    If I1 = 9 And threePtOpt = 0 Then
        Beep
        Locate 18, 1: Print "NOT USING 3 POINT SHOT"
        I$ = GetKeyPress$
        GoTo 109
    End If

    If I1 = 5 And sClockVal = 0 Then
        Beep
        Locate 18, 1: Print "NOT USING SHOT CLOCK"
        I$ = GetKeyPress$
        GoTo 109
    End If

    If I1 = 7 And score(P9, 0) - score(1 - P9, 0) <= 0 Then
        Beep
        Locate 18, 1: Print "ILLEGAL SELECTION"
        I$ = GetKeyPress$
        GoTo 109
    End If

    'How to loop around this with so many conditions?
    'Possibly build a bunch of nested If/Then statements?
    offStrat(P9) = I1
    crashBoards(P9) = 0

    If Not (currHalf < 2 Or currHalf >= 2 And gameClock! > 120) Then
        
        Print "CRASH OFFENSIVE BOARDS (LAST 2 MIN)? (Y/N)";
        
        Do
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"
        
        If UCase$(I$) = "Y" Then crashBoards(P9) = 1
        
    End If

    Call DEFENSEMENU

End Sub


'----------------------------------------
'         PlayItSafe Subroutine
'----------------------------------------
'This subroutine lets the user review all
'players who are taking it safe on defense
'and determine if they should play normal.
Sub PlayItSafe ()

    Cls

    Color 15

    A = 0
    S2 = 1
    S3 = 1
    SW = 0

    For ZZ = 0 To 4

        If plyrOff_GAME!(P9, lineupIdx(P9, ZZ), 22) = 50 Then

            A = A + 1

            If SW = 0 Then
                Print
                Print "THE FOLLOWING PLAYER(S) ARE PLAYING SAFE BUT CAN NOW PLAY NORMAL IF YOU WISH"
                SW = 1
            End If

            If SW = 1 And S2 < 6 Then

                Locate S2 + 3, 1
                Print S2; "-"; players$(P9, lineupIdx(P9, ZZ))
                P5(A) = S2
                P4(A) = ZZ
                S2 = S2 + 1
                S3 = S3 + 1

            End If

        End If

    Next ZZ

    skipChangeToSafe% = 0

    If SW = 1 Then

        A = 1
        Print
        Print "Z - CONTINUE"

        Do
            I$ = GetKeyPress$
            If UCase$(I$) = "Z" And MJ = 0 Then Exit Do
            If UCase$(I$) = "Z" And MJ = 25 Then skipChangeToSafe% = 1: Exit Do
            playChoice = Val(I$)

            A = playChoice

            If playChoice = P5(playChoice) Then

                plyrOff_GAME!(P9, lineupIdx(P9, P4(A)), 17) = plyrOff_GAME!(P9, lineupIdx(P9, P4(A)), 17) * 3
                plyrOff_GAME!(P9, lineupIdx(P9, P4(A)), 22) = 0

                Locate P5(A) + 3, 20
                Print players$(P9, lineupIdx(P9, P4(A))); " WILL NOW PLAY NORMAL DEFENSE!"
                Call STEALADJ

                dFGPA(P9) = dFGPA(P9) - 1
                A = A + 1
                MJ = 25

            End If

        Loop Until playChoice >= 1 And playChoice <= (S2 + 1) And playChoice <> P5(playChoice)

    End If

    If skipChangeToSafe% = 0 Then

        A = 0
        S2 = 1

        For ZZ = 0 To 4

            If (currHalf <= 1 And plyrOff_GAME!(P9, lineupIdx(P9, ZZ), 12) >= foulsToDQ - 3 And plyrOff_GAME!(P9, lineupIdx(P9, ZZ), 22) = 0) Or (plyrOff_GAME!(P9, lineupIdx(P9, ZZ), 12) = foulsToDQ - 1 And plyrOff_GAME!(P9, lineupIdx(P9, ZZ), 22) = 0) Then

                A = A + 1

                If S2 = 1 Then
                    Cls
                    Print
                    Print "THE FOLLOWING PLAYER(S) CAN ME MADE TO PLAY SAFE IF YOU WISH"
                    Print
                End If

            End If

            If A = 0 And ZZ > 3 Then
                GoTo 1230
            End If

            If S2 = A And S2 < 6 Then
                Print S2; "- "; players$(P9, lineupIdx(P9, ZZ))
                P5(A) = S2
                P4(A) = ZZ
                S2 = S2 + 1
                S3 = S3 + 1
            End If

        Next ZZ

        A = 1
        Print
        Print " Z - CONTINUE"

        1220 '
        Do
            I$ = GetKeyPress$
            playChoice = Val(I$)
        Loop Until playChoice >= 1 And playChoice <= (S2 + 1) Or UCase$(I$) = "Z"

        If UCase$(I$) <> "Z" Then

            A = playChoice

            If playChoice = P5(playChoice) Then

                plyrOff_GAME!(P9, lineupIdx(P9, P4(A)), 17) = Int((plyrOff_GAME!(P9, lineupIdx(P9, P4(A)), 17) + 1) / 3)
                plyrOff_GAME!(P9, lineupIdx(P9, P4(A)), 22) = 50

                Locate P5(A) + 3, 20
                Print players$(P9, lineupIdx(P9, P4(A))); " WILL NOW PLAY SAFE!"
                Call STEALADJ

                dFGPA(P9) = dFGPA(P9) + 1
                A = A + 1
                S3 = S3 + 1

                GoTo 1220

            End If

        End If

    End If

    1230 '
    MJ = 0

    'Call UPDATESCREEN
    Call SCOREBOARD

End Sub


'----------------------------------------
'          FOULHARD Subroutine
'----------------------------------------
' ...explanation...
Sub FOULHARD ()

    For I = 0 To 1

        For I1 = 0 To 13

            If plyrOff_GAME!(I, I1, 22) = 50 Then
                plyrOff_GAME!(I, I1, 22) = 0
                plyrOff_GAME!(I, I1, 17) = plyrOff_GAME!(I, I1, 17) * 3
                dFGPA(I) = dFGPA(I) - 1
                MJ = 50
            End If

        Next I1

        If MJ = 50 Then

            MJ = 0
            teamAdj(I, 0) = 0
            For I1 = 0 To 4:
                teamAdj(I, 0) = teamAdj(I, 0) + plyrOff_GAME!(I, lineupIdx(I, I1), 17)
            Next I1

        End If

    Next I

End Sub


'----------------------------------------
'          STEALADJ Subroutine
'----------------------------------------
'Uses data from all player ratings to help
'calculate several team-based adjustments
'for frequency of steals, etc.
Sub STEALADJ ()

    For I = 0 To 1

        teamAdj(I, 0) = 0
        teamAdj(I, 1) = 0

        For I1 = 0 To 4
            teamAdj(I, 0) = teamAdj(I, 0) + plyrOff_GAME!(I, lineupIdx(I, I1), 17)
            teamAdj(I, 1) = teamAdj(I, 1) + plyrOff_GAME!(I, lineupIdx(I, I1), 18)
        Next I1

    Next I

End Sub


'----------------------------------------
'         SHOTPERCENT Subroutine
'----------------------------------------
'This subroutine calculates the chance of
'a shot being made based on the active
'defense / offense / players.
Sub SHOTPERCENT ()

    baseChance = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 0)

    If tmFatigue(P, lineupIdx(P, ballCarrier)) <= -5 Then
        baseChance = baseChance - 10
    End If

    shotChance! = baseChance + dFGPA(D) + defAdjVsShot(0, defCoverage(D))

    'Improve chance for homefield advantage
    If gameLoc = P Then
        shotChance! = shotChance! + homeCtFactor(0, teamRat_GAME(P, 6))
    Else
        If gameLoc <> 2 Then
            shotChance! = shotChance! + homeCtFactor(1, teamRat_GAME(P, 6))
        End If
    End If

    ' ** Chance for 3-Point Shot **
    Call Rolld100(threePtShotChance)
    J8% = 0

    'Adjust 3-point chance based on defense
    If teamYears(D) < 2000 Then
        threePtDefAdj = 0
    Else
        'Three pointers were significantly more frequent
        'after the year 2000. Defenses trended towards
        'allowing more b/c their likelihood makes them risky.
        'This is reflected here by increasing the likelihood
        'the shot will be taken.
        threePtDefAdj = 0 '10
    End If

    Select Case defStrat(D)

        Case 1, 6, 9, 11
            'Pressure Man-Man
            threePtDefAdj = 5

        Case 2, 7, 12
            '2-3 Zone
            threePtDefAdj = 13

        Case 15 To 17
            '3-2 Zone is more effective
            'than most defenses, but not
            'quite on par with Man-to-Man
            threePtDefAdj = 7

    End Select

    skipCalcs% = 0

    ' ** Check for three-point shot **
    '% of player shots that are 3 FG + def threeFGA_Adj + adjustment due to Def
    If threePtShotChance > plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 19) + teamAdj(D, 3) + threePtDefAdj Or plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 19) = 0 Then

        'Strategy = Shoot only 3's
        If offStrat(P) = 9 Then
            'Shoot 3's no matter what!
            threePtShotChance = Int(Rnd(1) * 5) + 11
            J8% = 1
        Else
            threePtShotChance = Int(Rnd(1) * 5) + 3
            J8% = 0
        End If

    Else

        'Good shot - based on FG make %
        FX% = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 20)

        If tmFatigue(P, lineupIdx(P, ballCarrier)) <= -5 Then
            'Fatigue impacts chance for shot
            FX% = CInt(FX% * .67)
        End If

        'If more than 2 minutes in half and already contributed share
        If (currHalf < 3 And gameClock! > 120) And (playerFatigue%(P, lineupIdx(P, ballCarrier)) <= 0) Then
            'Bad shot b/c of fatigue
            threePtShotChance = Int(Rnd(1) * 5) + 3
            J8% = 0
        Else
            threePtShotChance = FX% + teamAdj(D, 2)
            J8% = 1
        End If

    End If

End Sub


'----------------------------------------
'           THREEFT Subroutine
'----------------------------------------
'This subroutine walks the user through
'taking 3 free throw attempts.
Sub THREEFT (ftNumber)

    'Increase FTA
    plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 10) = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 10) + 1

    Call Rolld100(RN0)

    Select Case ftNumber

        Case 1
            If RN0 <= 50 Then
                pbpString$ = "FREE THROW IS GOOD"
            Else
                pbpString$ = teamGender$(P) + " HITS THE FIRST FREE THROW"
            End If

        Case 2
            If RN0 <= 50 Then
                pbpString$ = "SECOND FREE THROW IS GOOD"
            Else
                pbpString$ = teamGender$(P) + " HITS THE SECOND ONE"
            End If

        Case 3
            If RN0 <= 50 Then
                pbpString$ = "THIRD FREE THROW IS GOOD"
            Else
                pbpString$ = teamGender$(P) + " HITS THE LAST ONE"
            End If

    End Select

    Call PBP(P)

    score(P, currHalf) = score(P, currHalf) + 1
    score(P, 0) = score(P, 0) + 1

    Call UPDATESCREEN

End Sub


'----------------------------------------
'           MADEFT Subroutine
'----------------------------------------
'This subroutine handles score, stats and
'Play-by-Play for a successful free throw
Sub MADEFT (ftNumber)

    'Increase FTM
    plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 10) = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 10) + 1

    If (pbpDelay! > 0) Then
        Locate ftNumber + 9, 1
        Call DELAY
        Print ftNumber; " GOOD"
    End If

    Call DELAY

    score(P, currHalf) = score(P, currHalf) + 1
    score(P, 0) = score(P, 0) + 1

    Call UPDATESCREEN

End Sub


'----------------------------------------
'           MISSFT Subroutine
'----------------------------------------
'This subroutine handles Play-by-Play
'for a missed free throw
Sub MISSFT (ftNumber)

    If (pbpDelay! > 0) Then
        Call DELAY
        Locate ftNumber + 9, 1
        Print ftNumber; " MISSED"
        Call DELAY
    End If

End Sub


'----------------------------------------
'            BADFT Subroutine
'----------------------------------------
'This subroutine handles Play-by-Play for
'missed FT's when there are 3 attempts.
Sub BADFT (ftNumber)

    Select Case ftNumber
        Case 1
            pbpString$ = "FIRST FREE THROW IS MISSED"
        Case 2
            pbpString$ = "SECOND FREE THROW IS MISSED"
        Case 3
            pbpString$ = "THIRD FREE THROW IS MISSED"
    End Select

    Call PBP(P)

End Sub


'----------------------------------------
'       PressFatigueDef Subroutine
'----------------------------------------
'This subroutine calculates fatigue of the
'defense from a press
Sub PressFatigueDef ()

    For I = 0 To 4

        Call Rolld100(RN0)

        If RN0 <= X0% Then
            tmFatigue(D, lineupIdx(D, I)) = tmFatigue(D, lineupIdx(D, I)) + X1%
        Else
            tmFatigue(D, lineupIdx(D, I)) = tmFatigue(D, lineupIdx(D, I)) + X1% + 1
        End If

        If tmFatigue(D, lineupIdx(D, I)) > 0 Then tmFatigue(D, lineupIdx(D, I)) = 0
        If tmFatigue(D, lineupIdx(D, I)) < -5 Then tmFatigue(D, lineupIdx(D, I)) = -5

    Next

End Sub


'----------------------------------------
'       PressFatigueOff Subroutine
'----------------------------------------
'This subroutine calculates fatigue of the
'offense from a press
Sub PressFatigueOff ()

    For I = 0 To 4

        Call Rolld100(RN0)

        If RN0 <= X0% Then
            tmFatigue(P, lineupIdx(P, I)) = tmFatigue(P, lineupIdx(P, I)) + X1%
        Else
            tmFatigue(P, lineupIdx(P, I)) = tmFatigue(P, lineupIdx(P, I)) + X1% + 1
        End If

        If tmFatigue(P, lineupIdx(P, I)) > 0 Then tmFatigue(P, lineupIdx(P, I)) = 0
        If tmFatigue(P, lineupIdx(P, I)) < -5 Then tmFatigue(P, lineupIdx(P, I)) = -5

    Next

End Sub


'----------------------------------------
'         WHOGESTBALL Subroutine
'----------------------------------------
'Determine ball carrier for the computer
'control when a pass is happening
Sub WHOGETSBALL ()

    CNT% = 0

    While (CNT% < 4)

        1041 '
        CNT% = CNT% + 1

        If CNT% <> 4 Then

            M9 = 0
            ballCarrier = Int(Rnd(1) * 5)

            If d100% > 25 Then

                For I = 0 To 4
                    M8 = CInt(plyrRat_GAME(P, lineupIdx(P, I), 1) / plyrRat_GAME(P, lineupIdx(P, I), 0)) - plyrOff_GAME!(P, lineupIdx(P, I), 7)
                    If M8 <= 0 Then M8 = 0
                    If M8 >= M9 Then
                        ballCarrier = I
                        M9 = M8
                    End If
                Next I

            End If

            M8 = playerFatigue%(P, lineupIdx(P, ballCarrier))

            If plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 4) >= 5 And M8 / plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 3) <= 0.4 And currHalf = 1 Then
                GoTo 1041
            End If

            If plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 4) >= 5 And M8 / plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 3) <= 0.2 And currHalf = 2 And gameClock! > 120 Then
                GoTo 1041
            End If

            If plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 3) < 10 And plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 4) >= 5 And M8 / plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 3) <= 0.75 And currHalf = 1 Then
                GoTo 1041
            End If

            If plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 3) < 10 And plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 4) >= 5 And M8 / plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 3) <= 0.5 And currHalf = 2 And gameClock! > 120 Then
                GoTo 1041
            End If

        End If

    Wend

End Sub


'----------------------------------------
'       BALLDISTRIBUTE Subroutine
'----------------------------------------
'Determine how the ball gets distributed
'when passing. Depending on the team
'stamina, sometimes the user will be
'presented with a choice and sometimes
'they won't. The computer of course
'has a routine for this separately.
Sub BALLDISTRIBUTE ()

    randomCarrier = 1
    
    Call Rolld100(RN0)

    If (playerMode = 0 And coachOpt = 1) Or (playerMode = 1 And D = compTeam And coachOpt = 1) Or (playerMode = 1 And P = compTeam) Or playerMode = 2 Then
        'Computer in charge - auto determine

        Call WHOGETSBALL

    Else

        'Select / Case statement
        If Not (defCoverage(D) = 2 Or defCoverage(D) = 3) Then

            If gameAvgTmStamina <= 90 And RN0 <= 15 Then randomCarrier = 0
            If gameAvgTmStamina <= 100 And RN0 <= 20 Then randomCarrier = 0
            If gameAvgTmStamina <= 110 And RN0 <= 25 Then randomCarrier = 0
            If gameAvgTmStamina <= 120 And RN0 <= 30 Then randomCarrier = 0
            If gameAvgTmStamina <= 130 And RN0 <= 38 Then randomCarrier = 0
            If gameAvgTmStamina <= 140 And RN0 <= 53 Then randomCarrier = 0
            If gameAvgTmStamina <= 150 And RN0 <= 60 Then randomCarrier = 0
            If gameAvgTmStamina > 150 And RN0 <= 67 Then randomCarrier = 0

        Else

            If gameAvgTmStamina <= 90 And RN0 <= 8 Then randomCarrier = 0
            If gameAvgTmStamina <= 100 And RN0 <= 10 Then randomCarrier = 0
            If gameAvgTmStamina <= 110 And RN0 <= 13 Then randomCarrier = 0
            If gameAvgTmStamina <= 120 And RN0 <= 15 Then randomCarrier = 0
            If gameAvgTmStamina <= 130 And RN0 <= 18 Then randomCarrier = 0
            If gameAvgTmStamina <= 140 And RN0 <= 53 Then randomCarrier = 0
            If gameAvgTmStamina <= 150 And RN0 <= 60 Then randomCarrier = 0
            If gameAvgTmStamina > 150 And RN0 <= 67 Then randomCarrier = 0

        End If

        If randomCarrier = 0 Then
            Call GetPassChoice
        Else
            ballCarrier = Int(Rnd(1) * 5)
        End If

    End If

End Sub


'----------------------------------------
'        GetPassChoice Subroutine
'----------------------------------------
'This routine prompts the user for a
'player to pass to.
Sub GetPassChoice ()

    Do

        If autoPlay <> 1 Then

            If pbpOpt = 1 Then
                If (playerMode <> 2 And pbpDelay! > 0) Then
                    Call WINDEX
                    Print "PASS TO CHOICE (0-4)"
                    Call PBP(P)
                End If
            Else
                pbpString$ = "PASS TO CHOICE (0-4)"
                Call PBP(P)
            End If

            Do

                I$ = GetKeyPress$

                Select Case UCase$(I$)

                    Case "T"
                        Call TICKER
                        Call SCOREBOARD

                    Case "Q"
                        Cls
                        End

                    Case "O"
                        Call OPTIONS

                    Case Else
                        ballCarrier = Val(I$)

                End Select

            Loop Until ballCarrier >= 0 And ballCarrier <= 4

        End If

    Loop Until ballCarrier >= 0 And ballCarrier <= 4

End Sub

'----------------------------------------
'       DEADBALLOPTIONS Subroutine
'----------------------------------------
'This subroutine handles any deadball
'situations. If not computer controlled,
'the user is given a chance to handle
'any changes to lineup, strategy, players.
Sub DEADBALLOPTIONS ()

    Do

        prevBall$ = ""
        Call CLEARPBP

        'Substitutions for computer teams
        If playerMode = 2 Then
            P9 = 0
            Call COMPUTERSUBS
            Call FATIGUE
            P9 = 1
            Call COMPUTERSUBS
            Call FATIGUE
        End If

        If playerMode = 1 Then
            P9 = compTeam
            Call COMPUTERSUBS
            Call FATIGUE
        End If

        Call CLEARPBPBOX

        'Present user options
        If playerMode = 2 Then

            S2 = 0
            P9 = P
            C1 = 2
            Call CheckGameClock
            Call STEALADJ

        Else

            Locate 9, 1
            Color 14, 0
            Print "<ENTER>";
            Color 15, 0
            Print "-CONTINUE"
            Print "1-SUB "; gameTeams$(P)
            Print "2-SUB "; gameTeams$(D)
            Print "3-CHANGE STRATEGY "; gameTeams$(P)
            Print "4-CHANGE STRATEGY "; gameTeams$(D)
            Print "5-TIME OUT "; gameTeams$(P)
            Print "6-TIME OUT "; gameTeams$(D)

            221 '
            I$ = GetKeyPress$
            P3 = Val(I$)

            If P3 = 0 Or (playerMode = 1 And (P3 = 1 Or P3 = 3 Or P3 = 5 And timeouts(P) <= 0) And P = compTeam) Or (playerMode = 1 And (P3 = 2 Or P3 = 4 Or P3 = 6 And timeouts(D) <= 0) And D = compTeam) Then

                'This is essentially for automated input,
                'when ENTER is pressed (to continue),
                'or if the team has no timeouts
                S2 = 0
                P9 = P
                C1 = 2
                Call CheckGameClock
                Call STEALADJ

                If lineupChanged = 1 Then
                    lineupChanged = 0
                    Call CheckGameClock
                    Call SCOREBOARD
                End If

                If pbpDelay! > 0 Then
                    Call WINDEX
                End If

            Else

                Select Case P3

                    Case 1
                        'Substitution - Offense
                        S2 = P
                        C1 = 1
                        P9 = S2
                        Call PlayStyle
                        Call PlayItSafe
                        P9 = P
                        lineupChanged = 1

                    Case 2
                        'Substitution - Defense
                        S2 = D
                        C1 = 1
                        P9 = S2
                        Call PlayStyle
                        Call PlayItSafe
                        P9 = P
                        lineupChanged = 1

                    Case 3
                        'Strategy - Offense
                        P9 = P
                        Call CHANGESTRATEGY
                        Call SCOREBOARD

                    Case 4
                        'Strategy - Defense
                        P9 = D
                        Call CHANGESTRATEGY
                        Call SCOREBOARD

                    Case 5
                        'Timeouts - Offense
                        If timeouts(P) <= 0 Then
                            GoTo 221
                        Else
                            timeouts(P) = timeouts(P) - 1
                            Call RESETFATIGUE
                            Call SCOREBOARD
                        End If

                    Case 6
                        'Timeouts - Defense
                        If timeouts(D) <= 0 Then
                            GoTo 221
                        Else
                            timeouts(D) = timeouts(D) - 1
                            Call RESETFATIGUE
                            Call SCOREBOARD
                        End If

                End Select

            End If 'Done checking input

        End If 'Done checking computer control

    Loop Until P3 = 0 Or playerMode = 2

End Sub


'----------------------------------------
'        RESETFATIGUE Subroutine
'----------------------------------------
'This subroutine resets player fatigue
'levels for a fresh start.
Sub RESETFATIGUE ()

    For I = 0 To 1

        For I1 = 0 To 1
            pressureRating(I, I1) = 0
        Next

    Next

    For I = 0 To 1

        For I1 = 0 To 13
            tmFatigue(I, I1) = tmFatigue(I, I1) + 1
            If tmFatigue(I, I1) > 0 Then tmFatigue(I, I1) = 0
        Next

    Next

End Sub


'------------------------------
'    STOPPAGE Subroutine
'------------------------------
'This routine handles all the
'things that can happen when
'the clock is stopped, including
'substitutions, strategy and
'time-outs.
Sub STOPPAGE ()

    810 '
    Call CLEARPBP

    If playerMode <> 0 Then

        'Computer team substitutions
        If playerMode = 2 Then
            P9 = 0
            Call COMPUTERSUBS
            Call FATIGUE
            P9 = 1
            Call COMPUTERSUBS
            Call FATIGUE
        End If

        If playerMode = 1 Then
            P9 = compTeam
            Call COMPUTERSUBS
            Call FATIGUE
        End If

    End If

    If playerMode = 2 Then

        S2 = 0
        P9 = P
        C1 = 2
        Call CheckGameClock

    Else

        If pbpOpt = 1 Then
            If pbpDelay! > 0 Then
                Locate 11, 1
            End If
        Else
            Call CLEARPBPBOX
        End If

        Color 14, 0
        Print "<ENTER>";
        Color 15, 0
        Print "-CONTINUE"
        Print "1-TIME OUT "; gameTeams$(P)
        Print "2-TIME OUT "; gameTeams$(D)
        Print "3-CHANGE STRATEGY "; gameTeams$(P)
        Print "4-CHANGE STRATEGY "; gameTeams$(D)

        813 '
        I$ = GetKeyPress$
        P3 = Val(I$)

        If P3 < 1 Or P3 > 4 Then

            S2 = 0
            P9 = P
            C1 = 2
            Call CheckGameClock

        Else

            'Can't do timeout?
            If P3 = 1 And timeouts(P) <= 0 Or P3 = 2 And timeouts(D) <= 0 Or (P3 = 1 Or P3 = 3) And P = compTeam And playerMode = 1 Or (P3 = 2 Or P3 = 4) And D = compTeam And playerMode = 1 Then
                GoTo 813
            End If

            'Strategy - Offense
            If P3 = 3 Then
                P9 = P
                Call CHANGESTRATEGY
                Call SCOREBOARD
                GoTo 810
            End If

            'Strategy - Defense
            If P3 = 4 Then
                P9 = D
                Call CHANGESTRATEGY
                Call SCOREBOARD
                GoTo 810
            End If

            For I = 0 To 1
                For I1 = 0 To 1
                    pressureRating(I, I1) = 0
                Next
            Next

            'Timeout - Offense
            If P3 = 1 Then

                timeouts(P) = timeouts(P) - 1
                Color 14, 0
                Call SCOREBOARD

                Call DEADBALLOPTIONS

            End If

            'Timeout - Defense
            If P3 = 2 Then

                timeouts(D) = timeouts(D) - 1
                Color 14, 0
                Call SCOREBOARD

                Call DEADBALLOPTIONS

            End If

        End If

        Call CLEARPBPBOX

    End If

End Sub


'----------------------------------------
'         PRESSGUARD Subroutine
'----------------------------------------
'This subroutine handles PBP and stats
'for when a team is pressing
Sub PRESSGUARD ()

    Call WINDEX

    If (autoPlay = 0 Or pbpDelay! > 0) Then

        If defCategory(D) > 0 Then
            pbpString$ = gameTeams$(D) + " IS PRESSING"
            Call SELECTPBP(D, 0)
        End If

    End If

    If defCategory(D) = 1 Or defCategory(D) = 2 Then
        'Full Court Press only

        If playerMode = 2 Or playerMode = 1 And P = compTeam Then
            ' COMPUTER GUARD FOR PRESS
            guardNbr = 1
            If plyrOff_GAME!(P, lineupIdx(P, 0), 4) >= plyrOff_GAME!(P, lineupIdx(P, 1), 4) And tmFatigue(P, lineupIdx(P, 0)) <= 3 Then
                guardNbr = 0
            End If

        Else
            Call FRAMEOPTION(8, P)
            Locate 10, 15

            Print gameTeams$(P); " SELECT GUARD TO HANDLE BALL:"

            Locate 12, 14
            Locate , 14: Print "#   NAME"; Tab(35); "PA"; Tab(38); "FA"
            Locate , 14: Print "0 - "; players$(P, lineupIdx(P, 0)); Tab(35); plyrOff_GAME!(P, lineupIdx(P, 0), 4); Tab(38); Abs(tmFatigue(P, lineupIdx(P, 0)))
            Locate , 14: Print "1 - "; players$(P, lineupIdx(P, 1)); Tab(35); plyrOff_GAME!(P, lineupIdx(P, 1), 4); Tab(38); Abs(tmFatigue(P, lineupIdx(P, 1)))

            Do
                I$ = GetKeyPress$
                I1 = Val(I$)
            Loop Until I1 >= 0 And I1 <= 1

            guardNbr = I1
            Color 15, 0
            Call SCOREBOARD

        End If

    End If

End Sub


'----------------------------------------
'        QUARTERTOTAL Subroutine
'----------------------------------------
'This subroutine calculates quarter totals
'for all stat categories right before the
'period end
Sub QUARTERTOTAL ()

    For I = 0 To 1

        Erase statTotals

        For I1 = 0 To 13
            statTotals(0) = statTotals(0) + plyrOff_GAME!(I, I1, 8) 'Minutes played
            statTotals(1) = statTotals(1) + plyrOff_GAME!(I, I1, 7) 'Games
            statTotals(2) = statTotals(2) + plyrOff_GAME!(I, I1, 10) 'Reb
            statTotals(3) = statTotals(3) + plyrOff_GAME!(I, I1, 9) 'FGA
            statTotals(4) = statTotals(4) + plyrOff_GAME!(I, I1, 11) 'Ast
            statTotals(5) = statTotals(5) + (plyrOff_GAME!(I, I1, 8) * 2) + plyrOff_GAME!(I, I1, 10) + threeFG(I, I1, 0)
            statTotals(13) = statTotals(13) + threeFG(I, I1, 0)
            statTotals(14) = statTotals(14) + threeFG(I, I1, 1)
            statTotals(6) = statTotals(6) + plyrOff_GAME!(I, I1, 12)
            statTotals(7) = statTotals(7) + plyrOff_GAME!(I, I1, 14)
            statTotals(8) = statTotals(8) + plyrOff_GAME!(I, I1, 15)
            statTotals(9) = statTotals(9) + plyrOff_GAME!(I, I1, 13)
            statTotals(11) = statTotals(11) + plyrOff_GAME!(I, I1, 24)
            statTotals(12) = statTotals(12) + plyrOff_GAME!(I, I1, 23)
        Next

        If currHalf = 1 Or currHalf = 2 Then

            For I2 = 0 To 14
                QR(I, currHalf, I2) = statTotals(I2) - QR(I, currHalf - 1, I2)
            Next

            For I1 = 0 To 13

                For I2 = 7 To 15
                    QQ(I, currHalf, I1, I2 - 6) = plyrOff_GAME!(I, I1, I2) - QQ(I, currHalf - 1, I1, I2 - 6)
                Next

                QQ(I, currHalf, I1, 10) = threeFG(I, I1, 0) - QQ(I, currHalf - 1, I1, 10)
                QQ(I, currHalf, I1, 11) = threeFG(I, I1, 1) - QQ(I, currHalf - 1, I1, 11)
                QQ(I, currHalf, I1, 12) = plyrOff_GAME!(I, I1, 24) - QQ(I, currHalf - 1, I1, 12)
                QQ(I, currHalf, I1, 13) = plyrOff_GAME!(I, I1, 8) * 2 + plyrOff_GAME!(I, I1, 10) + threeFG(I, I1, 0) - QQ(I, currHalf - 1, I1, 13)
                QQ(I, currHalf, I1, 14) = plyrOff_GAME!(I, I1, 23) - QQ(I, currHalf - 1, I1, 14)

            Next

        End If

        If currHalf = 3 Then

            For I2 = 0 To 14
                QR(I, currHalf, I2) = statTotals(I2) - QR(I, currHalf - 1, I2) - QR(I, currHalf - 2, I2)
            Next

            For I1 = 0 To 13

                For I2 = 7 To 15
                    QQ(I, currHalf, I1, I2 - 6) = plyrOff_GAME!(I, I1, I2) - QQ(I, currHalf - 1, I1, I2 - 6) - QQ(I, currHalf - 2, I1, I2 - 6)
                Next

                QQ(I, currHalf, I1, 10) = threeFG(I, I1, 0) - QQ(I, currHalf - 1, I1, 10) - QQ(I, currHalf - 2, I1, 10)
                QQ(I, currHalf, I1, 11) = threeFG(I, I1, 1) - QQ(I, currHalf - 1, I1, 11) - QQ(I, currHalf - 2, I1, 11)
                QQ(I, currHalf, I1, 12) = plyrOff_GAME!(I, I1, 24) - QQ(I, currHalf - 1, I1, 12) - QQ(I, currHalf - 2, I1, 12)
                QQ(I, currHalf, I1, 13) = plyrOff_GAME!(I, I1, 8) * 2 + plyrOff_GAME!(I, I1, 10) + threeFG(I, I1, 0) - QQ(I, currHalf - 1, I1, 13) - QQ(I, currHalf - 2, I1, 13)
                QQ(I, currHalf, I1, 14) = plyrOff_GAME!(I, I1, 23) - QQ(I, currHalf - 1, I1, 14) - QQ(I, currHalf - 2, I1, 14)

            Next

        End If

        If currHalf = 4 Then

            For I2 = 0 To 14
                QR(I, currHalf, I2) = statTotals(I2) - QR(I, currHalf - 1, I2) - QR(I, currHalf - 2, I2) - QR(I, currHalf - 3, I2)
            Next

            For I1 = 0 To 13

                For I2 = 7 To 15
                    QQ(I, currHalf, I1, I2 - 6) = plyrOff_GAME!(I, I1, I2) - QQ(I, currHalf - 1, I1, I2 - 6) - QQ(I, currHalf - 2, I1, I2 - 6) - QQ(I, currHalf - 3, I1, I2 - 6)
                Next

                QQ(I, currHalf, I1, 10) = threeFG(I, I1, 0) - QQ(I, currHalf - 1, I1, 10) - QQ(I, currHalf - 2, I1, 10) - QQ(I, currHalf - 3, I1, 10)
                QQ(I, currHalf, I1, 11) = threeFG(I, I1, 1) - QQ(I, currHalf - 1, I1, 11) - QQ(I, currHalf - 2, I1, 11) - QQ(I, currHalf - 3, I1, 11)
                QQ(I, currHalf, I1, 12) = plyrOff_GAME!(I, I1, 24) - QQ(I, currHalf - 1, I1, 12) - QQ(I, currHalf - 2, I1, 12) - QQ(I, currHalf - 3, I1, 12)
                QQ(I, currHalf, I1, 13) = plyrOff_GAME!(I, I1, 8) * 2 + plyrOff_GAME!(I, I1, 10) + threeFG(I, I1, 0) - QQ(I, currHalf - 1, I1, 13) - QQ(I, currHalf - 2, I1, 13) - QQ(I, currHalf - 3, I1, 13)
                QQ(I, currHalf, I1, 14) = plyrOff_GAME!(I, I1, 23) - QQ(I, currHalf - 1, I1, 14) - QQ(I, currHalf - 2, I1, 14) - QQ(I, currHalf - 3, I1, 14)

            Next

        End If

        If currHalf = 5 Then

            For I2 = 0 To 14
                QR(I, currHalf, I2) = statTotals(I2) - QR(I, currHalf - 1, I2) - QR(I, currHalf - 2, I2) - QR(I, currHalf - 3, I2) - QR(I, currHalf - 4, I2)
            Next

            For I1 = 0 To 13

                For I2 = 7 To 15
                    QQ(I, currHalf, I1, I2 - 6) = plyrOff_GAME!(I, I1, I2) - QQ(I, currHalf - 1, I1, I2 - 6) - QQ(I, currHalf - 2, I1, I2 - 6) - QQ(I, currHalf - 3, I1, I2 - 6) - QQ(I, currHalf - 4, I1, I2 - 6)
                Next

                QQ(I, currHalf, I1, 10) = threeFG(I, I1, 0) - QQ(I, currHalf - 1, I1, 10) - QQ(I, currHalf - 2, I1, 10) - QQ(I, currHalf - 3, I1, 10) - QQ(I, currHalf - 4, I1, 10)
                QQ(I, currHalf, I1, 11) = threeFG(I, I1, 1) - QQ(I, currHalf - 1, I1, 11) - QQ(I, currHalf - 2, I1, 11) - QQ(I, currHalf - 3, I1, 11) - QQ(I, currHalf - 4, I1, 11)
                QQ(I, currHalf, I1, 12) = plyrOff_GAME!(I, I1, 24) - QQ(I, currHalf - 1, I1, 12) - QQ(I, currHalf - 2, I1, 12) - QQ(I, currHalf - 3, I1, 12) - QQ(I, currHalf - 4, I1, 12)
                QQ(I, currHalf, I1, 13) = plyrOff_GAME!(I, I1, 8) * 2 + plyrOff_GAME!(I, I1, 10) + threeFG(I, I1, 0) - QQ(I, currHalf - 1, I1, 13) - QQ(I, currHalf - 2, I1, 13) - QQ(I, currHalf - 3, I1, 13) - QQ(I, currHalf - 4, I1, 13)
                QQ(I, currHalf, I1, 14) = plyrOff_GAME!(I, I1, 23) - QQ(I, currHalf - 1, I1, 14) - QQ(I, currHalf - 2, I1, 14) - QQ(I, currHalf - 3, I1, 14) - QQ(I, currHalf - 4, I1, 14)

            Next

        End If

        If currHalf = 6 Then

            For I2 = 0 To 14
                QR(I, currHalf, I2) = statTotals(I2) - QR(I, currHalf - 1, I2) - QR(I, currHalf - 2, I2) - QR(I, currHalf - 3, I2) - QR(I, currHalf - 4, I2) - QR(I, currHalf - 5, I2)
            Next

            For I1 = 0 To 13

                For I2 = 7 To 15
                    QQ(I, currHalf, I1, I2 - 6) = plyrOff_GAME!(I, I1, I2) - QQ(I, currHalf - 1, I1, I2 - 6) - QQ(I, currHalf - 2, I1, I2 - 6) - QQ(I, currHalf - 3, I1, I2 - 6) - QQ(I, currHalf - 4, I1, I2 - 6) - QQ(I, currHalf - 5, I1, I2 - 6)
                Next

                QQ(I, currHalf, I1, 10) = threeFG(I, I1, 0) - QQ(I, currHalf - 1, I1, 10) - QQ(I, currHalf - 2, I1, 10) - QQ(I, currHalf - 3, I1, 10) - QQ(I, currHalf - 4, I1, 10) - QQ(I, currHalf - 5, I1, 10)
                QQ(I, currHalf, I1, 11) = threeFG(I, I1, 1) - QQ(I, currHalf - 1, I1, 11) - QQ(I, currHalf - 2, I1, 11) - QQ(I, currHalf - 3, I1, 11) - QQ(I, currHalf - 4, I1, 11) - QQ(I, currHalf - 5, I1, 11)
                QQ(I, currHalf, I1, 12) = plyrOff_GAME!(I, I1, 24) - QQ(I, currHalf - 1, I1, 12) - QQ(I, currHalf - 2, I1, 12) - QQ(I, currHalf - 3, I1, 12) - QQ(I, currHalf - 4, I1, 12) - QQ(I, currHalf - 5, I1, 12)
                QQ(I, currHalf, I1, 13) = plyrOff_GAME!(I, I1, 8) * 2 + plyrOff_GAME!(I, I1, 10) + threeFG(I, I1, 0) - QQ(I, currHalf - 1, I1, 13) - QQ(I, currHalf - 2, I1, 13) - QQ(I, currHalf - 3, I1, 13) - QQ(I, currHalf - 4, I1, 13) - QQ(I, currHalf - 5, I1, 13)
                QQ(I, currHalf, I1, 14) = plyrOff_GAME!(I, I1, 23) - QQ(I, currHalf - 1, I1, 14) - QQ(I, currHalf - 2, I1, 14) - QQ(I, currHalf - 3, I1, 14) - QQ(I, currHalf - 4, I1, 14) - QQ(I, currHalf - 5, I1, 14)

            Next

        End If

        If currHalf = 7 Then

            For I2 = 0 To 14
                QR(I, currHalf, I2) = statTotals(I2) - QR(I, currHalf - 1, I2) - QR(I, currHalf - 2, I2) - QR(I, currHalf - 3, I2) - QR(I, currHalf - 4, I2) - QR(I, currHalf - 5, I2) - QR(I, currHalf - 6, I2)
            Next

            For I1 = 0 To 13

                For I2 = 7 To 15
                    QQ(I, currHalf, I1, I2 - 6) = plyrOff_GAME!(I, I1, I2) - QQ(I, currHalf - 1, I1, I2 - 6) - QQ(I, currHalf - 2, I1, I2 - 6) - QQ(I, currHalf - 3, I1, I2 - 6) - QQ(I, currHalf - 4, I1, I2 - 6) - QQ(I, currHalf - 5, I1, I2 - 6) - QQ(I, currHalf - 6, I1, I2 - 6)
                Next

                QQ(I, currHalf, I1, 10) = threeFG(I, I1, 0) - QQ(I, currHalf - 1, I1, 10) - QQ(I, currHalf - 2, I1, 10) - QQ(I, currHalf - 3, I1, 10) - QQ(I, currHalf - 4, I1, 10) - QQ(I, currHalf - 5, I1, 10) - QQ(I, currHalf - 6, I1, 10)
                QQ(I, currHalf, I1, 11) = threeFG(I, I1, 1) - QQ(I, currHalf - 1, I1, 11) - QQ(I, currHalf - 2, I1, 11) - QQ(I, currHalf - 3, I1, 11) - QQ(I, currHalf - 4, I1, 11) - QQ(I, currHalf - 5, I1, 11) - QQ(I, currHalf - 6, I1, 11)
                QQ(I, currHalf, I1, 12) = plyrOff_GAME!(I, I1, 24) - QQ(I, currHalf - 1, I1, 12) - QQ(I, currHalf - 2, I1, 12) - QQ(I, currHalf - 3, I1, 12) - QQ(I, currHalf - 4, I1, 12) - QQ(I, currHalf - 5, I1, 12) - QQ(I, currHalf - 6, I1, 12)
                QQ(I, currHalf, I1, 13) = plyrOff_GAME!(I, I1, 8) * 2 + plyrOff_GAME!(I, I1, 10) + threeFG(I, I1, 0) - QQ(I, currHalf - 1, I1, 13) - QQ(I, currHalf - 2, I1, 13) - QQ(I, currHalf - 3, I1, 13) - QQ(I, currHalf - 4, I1, 13) - QQ(I, currHalf - 5, I1, 13) - QQ(I, currHalf - 6, I1, 13)
                QQ(I, currHalf, I1, 14) = plyrOff_GAME!(I, I1, 23) - QQ(I, currHalf - 1, I1, 14) - QQ(I, currHalf - 2, I1, 14) - QQ(I, currHalf - 3, I1, 14) - QQ(I, currHalf - 4, I1, 14) - QQ(I, currHalf - 5, I1, 14) - QQ(I, currHalf - 6, I1, 14)

            Next

        End If

    Next

    Erase statTotals

End Sub


'----------------------------------------
'       ComputerStrategy Subroutine
'----------------------------------------
'This subroutine guides the computer AI
'in how to handle it's strategy.
Sub ComputerStrategy ()

    'P9 is the current team being assigned strategy
    opposeTeam = 1 - P9

    ' **************************
    ' *** Defensive Strategy ***
    ' **************************
    'Default to Solid Man-to-Man
    defStrat(P9) = 0

    'Where is our score compared to opponent?
    scoreDiffTm = score(P9, 0) - score(opposeTeam, 0)

    If scoreDiffTm < 0 Then
        'Current team is behind is behind

        'Where is opponent's score compared to us?
        scoreDiffOpp = score(opposeTeam, 0) - score(P9, 0)

        Call Rolld100(RN0)

        Select Case currHalf

            Case 1
                '1st half strategy
                Select Case RN0

                    Case 1 To 43
                        defStrat(P9) = 0

                    Case 44 To 67
                        defStrat(P9) = 1

                    Case 68 To 92
                        defStrat(P9) = 2

                    Case 93 To 94
                        defStrat(P9) = 15

                    Case 95 To 100
                        defStrat(P9) = 3

                End Select

                Call Rolld100(RN1)
                Select Case RN1

                    Case 1 To 3
                        defStrat(P9) = 10

                    Case 4 To 5
                        defStrat(P9) = 9

                    Case 6
                        defStrat(P9) = 16

                    Case 7 To 9
                        defStrat(P9) = 7

                    Case 10 To 12
                        defStrat(P9) = 8

                    Case 13 To 14
                        defStrat(P9) = 4

                    Case 15
                        defStrat(P9) = 15

                End Select


            Case 2
                '2nd Half strategy

                Select Case gameClock!

                    Case Is >= 600

                        Select Case scoreDiffOpp

                            Case Is < 10
                                'Opponent behind or within 10

                                Select Case RN0

                                    Case 1 To 52
                                        defStrat(P9) = 0

                                    Case 53 To 75
                                        defStrat(P9) = 1

                                    Case 76 To 84
                                        defStrat(P9) = 2

                                    Case 85
                                        defStrat(P9) = 15

                                    Case 86 To 93
                                        defStrat(P9) = 5

                                    Case 94 To 97
                                        defStrat(P9) = 3

                                    Case 98 To 100
                                        defStrat(P9) = 12

                                End Select

                            Case 10 To 20
                                'Opponent ahead by 10-20

                                Select Case RN0

                                    Case 1 To 57
                                        defStrat(P9) = 0

                                    Case 58 To 86
                                        defStrat(P9) = 4

                                    Case 87 To 94
                                        defStrat(P9) = 1

                                    Case 95 To 99
                                        defStrat(P9) = 10

                                    Case 100
                                        defStrat(P9) = 11

                                End Select

                            Case Is > 20
                                'Opponent ahead by 21+

                                Select Case RN0

                                    Case 1 To 42
                                        defStrat(P9) = 0

                                    Case 43 To 80
                                        defStrat(P9) = 1

                                    Case 81 To 92
                                        defStrat(P9) = 6

                                    Case 93 To 97
                                        defStrat(P9) = 10

                                    Case 98 To 100
                                        defStrat(P9) = 11

                                End Select

                        End Select

                    Case 120 To 599

                        Select Case scoreDiffOpp
                            'Opponent ahead by 20+

                            Case Is <= 10
                                'Opponent behind or within 10

                                Select Case RN0

                                    Case 1 To 52
                                        defStrat(P9) = 0

                                    Case 53 To 71
                                        defStrat(P9) = 2

                                    Case 72
                                        defStrat(P9) = 15

                                    Case 73 To 78
                                        defStrat(P9) = 1

                                    Case 79 To 100
                                        defStrat(P9) = 10

                                End Select


                            Case 11 To 19
                                'Opponent ahead by 11-19

                                Select Case RN0

                                    Case 1 To 52
                                        defStrat(P9) = 0

                                    Case 53 To 80
                                        defStrat(P9) = 1

                                    Case 81 To 90
                                        defStrat(P9) = 5

                                    Case 91 To 100
                                        defStrat(P9) = 12

                                End Select

                            Case Is > 20

                                Select Case RN0

                                    Case 1 To 52
                                        defStrat(P9) = 0

                                    Case 53 To 74
                                        defStrat(P9) = 1

                                    Case 75 To 84
                                        defStrat(P9) = 5

                                    Case 85 To 94
                                        defStrat(P9) = 10

                                    Case 95 To 100
                                        defStrat(P9) = 13

                                End Select

                        End Select

                    Case 0 To 119

                        Select Case scoreDiffOpp

                            Case Is <= 4
                                'Opponent behind or within 4

                                Select Case RN0

                                    Case 1 To 15
                                        defStrat(P9) = 5

                                    Case 16 To 41
                                        defStrat(P9) = 6

                                    Case 42 To 63
                                        defStrat(P9) = 11

                                    Case 64 To 84
                                        defStrat(P9) = 13

                                    Case 85 To 100
                                        defStrat(P9) = 1

                                End Select

                            Case 5 To 10
                                'Opponent ahead by 5-10
                                defStrat(P9) = 14

                            Case Is >= 11
                                'Opponent ahead by 11+

                                Select Case RN0

                                    Case 1 To 47
                                        defStrat(P9) = 11

                                    Case 48 To 93
                                        defStrat(P9) = 13

                                    Case 94 To 100
                                        defStrat(P9) = 1

                                End Select

                        End Select

                End Select


            Case Is > 2
                'Overtime strategy

                Select Case RN0

                    Case 0 To 32
                        defStrat(P9) = 0

                    Case 33 To 36
                        defStrat(P9) = 1

                    Case 37 To 50
                        'Nothing here?

                    Case 51 To 96
                        defStrat(P9) = 2

                    Case 97 To 100
                        defStrat(P9) = 15

                End Select

        End Select

    Else
        'Team is ahead

        Call Rolld100(RN0)

        Select Case RN0

            Case 1 To 46
                defStrat(P9) = 0

            Case 47 To 74
                defStrat(P9) = 1

            Case 75 To 98
                defStrat(P9) = 2

            Case 99 To 100
                defStrat(P9) = 15

        End Select

        Call Rolld100(RN0)

        Select Case RN0

            Case 1 To 5
                defStrat(P9) = 4

            Case 6 To 9
                defStrat(P9) = 8

            Case 10
                defStrat(P9) = 7

            Case 11
                defStrat(P9) = 16

        End Select

    End If


    Call CategorizeDefense(P9)


    ' **************************
    ' *** Offensive Strategy ***
    ' **************************
    Call Rolld100(RN0)

    If currHalf = 1 Then

        Select Case RN0

            Case 1 To 75
                offStrat(P9) = 0

            Case 76 To 94
                offStrat(P9) = 1

            Case 95 To 100
                offStrat(P9) = 2

        End Select

    Else
        'currHalf >= 2

        If scoreDiffTm < 0 Then
            'We are behind

            If gameClock! <= 180 Then

                If RN0 <= 30 Then
                    'Quick
                    offStrat(P9) = 3
                Else
                    'Delay
                    offStrat(P9) = 4
                End If

            Else

                If RN0 <= 70 Then
                    offStrat(P9) = 0
                Else
                    offStrat(P9) = 1
                End If

            End If

        Else

            'We are ahead
            If RN0 <= 85 Then
                offStrat(P9) = 0
            Else
                offStrat(P9) = 2
            End If

        End If

        If gameClock! <= 60 And threePtOpt = 1 And scoreDiffTm <= -3 Then
            'We are behind by 3 or more
            offStrat(P9) = 9
        End If

    End If

End Sub


'----------------------------------------
'      CategorizeDefense Subroutine
'----------------------------------------
'This subroutine assigns a coverage type
'and category to each defensive style.
'These are used to generalize adjustments
'to play outcomes based on the defense.
Sub CategorizeDefense (teamIdx)

    Select Case defStrat(teamIdx)

        Case 0 To 3, 15
            'Single Strategy
            defCategory(teamIdx) = 0

        Case 4
            'Full Court Press
            defCategory(teamIdx) = 1

        Case 5, 6
            'Run & Jump
            defCategory(teamIdx) = 2

        Case 7 To 9, 16
            'Passive 2-2-1 Zone
            defCategory(teamIdx) = 3

        Case 10 To 13, 17
            'Diamond Trap Zone
            defCategory(teamIdx) = 4

        Case 14
            'Fulll Court Man-to-Man Denial / Foul
            defCategory(teamIdx) = 5

    End Select

    Select Case defStrat(teamIdx)

        Case 0, 4, 5, 8, 10
            'Solid Man-Man
            defCoverage(teamIdx) = 0

        Case 1, 6, 9, 11
            'Pressure Man-Man
            defCoverage(teamIdx) = 1

        Case 2, 7, 12, 15, 16
            'Passive Zone
            defCoverage(teamIdx) = 2

        Case 3, 13, 17
            'Trapping Zone
            defCoverage(teamIdx) = 3

    End Select

End Sub


'----------------------------------------
'         STAMINASORT Subroutine
'----------------------------------------
'This subroutine PRESUMABLY sorts players
'by their current stamina.
Sub STAMINASORT ()

    L = 12

    8802 '
    S6 = 0
    S7 = 0

    8803 '
    H1 = 0
    H2 = 0

    If plyrStaminaGame(S6) > plyrStaminaGame(S6 + 1) Then

        Swap rosterIdx(S6), rosterIdx(S6 + 1)
        Swap plyrStaminaGame(S6), plyrStaminaGame(S6 + 1)
        S7 = S6

    End If

    S6 = S6 + 1

    If S6 <= L Then
        GoTo 8803
    End If

    If S7 <> 0 Then
        L = S7 - 1
        GoTo 8802
    End If

End Sub


'----------------------------------------
'        LASTFIVEMENU Subroutine
'----------------------------------------
'This subroutine simply prints a menu of
'all the user options for the last 5 sec.
'of the game
Sub LASTFIVEMENU ()

    Call FRAMEOPTION(12, P)
    Locate 10, 15

    Print gameTeams$(P); " LAST 5 SEC STRATEGY:"

    Locate 12, 14
    Locate , 14: Print Using "TIME REMAINING #.#"; gameClock!
    Locate , 14: Print "                                     TIME REQD"
    Locate , 14: Print "0 - FULL COURT PASS (SHOOT A 2)      0.1 - 3.0"
    Locate , 14: Print "1 - FULL COURT PASS (SHOOT A 3)      0.1 - 3.0"
    Locate , 14: Print "2 - SHOOT FROM BACKCOURT             0.1 - 0.5"
    Locate , 14: Print "3 - GET TO HALF COURT, CALL TIME OUT 1.5 - 2.5"
    Locate , 14: Print "4 - WORK DOWN THE FLOOR FOR A 3      3.0 - 4.0"
    Locate , 14: Print "5 - WORK DOWN THE FLOOR FOR A 2      3.0 - 5.0"

End Sub


'----------------------------------------
'          LASTSHOT Subroutine
'----------------------------------------
'This subroutine handles calculations for
'the clock and playtime with the last shot.
Sub LASTSHOT ()

    clockSecs = 0

    If gameClock! <= 2 Or foulOnPlay = 99 Then

        clockSecs = 1

    Else

        Select Case gameClock!

            Case 2 To 4
                clockSecs = 2

            Case 5 To 16
                clockSecs = Int(Rnd(1) * 2) + 3

            Case Else
                clockSecs = Int(Rnd(1) * 3) + 5

        End Select

    End If

    Call AdjustClockTime(clockSecs)
    Call AdjustPlayTime

    foulOnPlay = 0

    If sClockVal >= 1 Then
        shotClock = shotClock - clockSecs
    End If

    Call CheckGameShotClock

End Sub


'----------------------------------------
'       CheckGameClock Subroutine
'----------------------------------------
'This subroutine checks the game clock
'and resets it if expired
Sub CheckGameClock ()

    If gameClock! <= 0 Then

        timeElapsed! = gameClock!
        Call AdjustPlayTime
        gameClock! = 0
        
        If autoPlay <> 1 And playerMode <> 2 Then
            Call HORN
        End If

    End If

End Sub


'----------------------------------------
'     CheckGameShotClock Subroutine
'----------------------------------------
'This subroutine checks if either the game
'clock and shot clock have expired, and
'zeros them out if so. UPDATESCREEN is
'called to update the clock on the board.
Sub CheckGameShotClock ()

    If gameClock! <= .001 Then
        timeElapsed! = gameClock!
        Call AdjustPlayTime
        gameClock! = 0
    End If

    If shotClock <= 0 Then
        shotClock = shotClock - shotClock
        gameClock! = gameClock! - shotClock
        timeElapsed! = shotClock
        Call AdjustPlayTime
    End If

    Call UPDATESCREEN

End Sub


'----------------------------------------
'       AdjustClockTime Subroutine
'----------------------------------------
'This subroutine updates the clock for
'the play, based on the time specified
Sub AdjustClockTime (timeVal!)

    If sClockVal = 0 Then
        'Increase time taken for no shot clock
        timeElapsed! = timeVal! '+ 1
    Else
        timeElapsed! = timeVal!
    End If

    gameClock! = gameClock! - timeElapsed!

End Sub


'----------------------------------------
'       AdjustPlayTime Subroutine
'----------------------------------------
'This subroutine increases the play time
'for each active player
Sub AdjustPlayTime ()

    ' *** INCREMENT MINUTES ***
    For I3 = 0 To 1

        For I4 = 0 To 4
            timePlayed(I3, lineupIdx(I3, I4)) = timePlayed(I3, lineupIdx(I3, I4)) + timeElapsed!
        Next

    Next

End Sub


'----------------------------------------
'       ResetShotClock Subroutine
'----------------------------------------
'This subroutine resets the shot clock.
'It uses defined setting value in order
'to determine how much time to add.
'A possible rewrite of the function
'would see it simply accept the amount
'of time as a parameter instead, but
'that would be harder to control for any
'invalid values that might get passed.
Sub ResetShotClock ()

    If sClockVal > 0 Then

        Select Case sClockVal
            Case 1: shotClock = 45
            Case 2: shotClock = 35
            Case 3: shotClock = 30
        End Select

        LS = 4

        If gameClock! < 46 And shotClock >= gameClock! Or currHalf > 2 And gameClock! = 300 Then
            Locate 5, 5: Print "     "
        End If

        If gameClock! < 6 And LT <> 4 Then
            shotChance! = 5
        End If

    End If

End Sub


'----------------------------------------
'       AdjustShotClock Subroutine
'----------------------------------------
'This subroutine substracts time from
'the shot clock based on the time that
'has elapsed during game play.
Sub AdjustShotClock ()

    If sClockVal >= 1 Then
        shotClock = shotClock - timeElapsed!
    End If

    'Shot clock expired
    If shotClock < 0 Then

        'shotClock = 0?
        shotClock = shotClock - shotClock

        'gameClock = no change?
        gameClock! = gameClock! - shotClock

        'timeElapsed = 0
        timeElapsed! = shotClock

        Call AdjustPlayTime

    End If

End Sub


'----------------------------------------
'        FreeThrow_One Subroutine
'----------------------------------------
'After handling the deadball situation,
'this routine walks through play-by-play
'for 1 free throw. It also calculates
'the stat adjustments and score afterward.
Sub FreeThrow_One (missedLastFT)

    offStatus = 50

    Call DEADBALLOPTIONS

    For I = 0 To 1
        If defCategory(I) = 0 Then
            pressureRating(I, 0) = 1
            pressureRating(I, 1) = 7
            pctContrib(I) = 0
        End If
    Next

    offStatus = 0

    If (autoPlay = 0 Or pbpDelay! > 0) Then

        If pbpOpt = 1 Then
            If pbpDelay! > 0 Then
                Call WINDEX
                Print gameTeams$(P)
            End If
        End If

        pbpString$ = "1 FT: " + players$(P, lineupIdx(P, ballCarrier)) + "   " + Str$(plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 1)) + " %"
        Call SELECTPBP(P, 0)

    End If

    ftShotChance = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 1)

    If tmFatigue(P, lineupIdx(P, ballCarrier)) <= -5 Then
        ftShotChance = CInt(ftShotChance * .9)
    End If

    Call Rolld100(shotRoll)

    If shotRoll <= ftShotChance Then

        If pbpOpt = 1 Then
            Call MADEFT(1)
        Else

            'Increase FTM
            plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 10) = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 10) + 1

            If (autoPlay = 0 Or pbpDelay! > 0) Then
                pbpString$ = "FREE THROW IS GOOD"
                Call PBP(P)
            End If

            score(P, currHalf) = score(P, currHalf) + 1
            score(P, 0) = score(P, 0) + 1

            Call UPDATESCREEN

        End If

        If autoPlay <> 1 Or playerMode <> 2 And swish& And d100% < 10 Then
            _SndPlay swish&
        End If

    Else

        If pbpOpt = 1 Then
            Call MISSFT(1)
        Else
            If (autoPlay = 0 Or pbpDelay! > 0) Then
                pbpString$ = "FREE THROW IS MISSED"
                Call PBP(P)
            End If
        End If

        If autoPlay <> 1 Or playerMode <> 2 And backboard& And d100% < 10 Then
            _SndPlay backboard&
        End If

    End If

    'Increase FT ATT
    plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 9) = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 9) + 1

    If shotRoll > ftShotChance Then
        missedLastFT = 1
    Else
        missedLastFT = 0
    End If

End Sub


'----------------------------------------
'    FreeThrow_OneAndOne Subroutine
'----------------------------------------
'After handling the deadball situation,
'this routine walks through play-by-play
'for 1+1 free throw. It also calculates
'the stat adjustments and score afterward.
Sub FreeThrow_OneAndOne (missedLastFT)

    offStatus = 50

    Call DEADBALLOPTIONS

    For I = 0 To 1
        If defCategory(I) = 0 Then
            pressureRating(I, 0) = 1
            pressureRating(I, 1) = 7
            pctContrib(I) = 0
        End If
    Next

    offStatus = 0

    If (autoPlay = 0 Or pbpDelay! > 0) Then

        If pbpOpt = 0 Then
            pbpString$ = "1+1 FT: " + players$(P, lineupIdx(P, ballCarrier)) + Str$(plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 1)) + " %"
            Call PBP(P)
        Else
            Call WINDEX
            Print gameTeams$(P)
            Print "1+1 FT: "; players$(P, lineupIdx(P, ballCarrier)); "   "; plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 1); " %"
        End If

    End If

    '2 FT Attempts
    For freethrow = 1 To 2

        'Increase FT ATT
        plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 9) = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 9) + 1

        Call Rolld100(shotRoll)

        'FT Success Chance
        ftShotChance = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 1)
        If tmFatigue(P, lineupIdx(P, ballCarrier)) <= -5 Then
            ftShotChance = CInt(ftShotChance * .9)
        End If

        If shotRoll <= ftShotChance Then

            If pbpOpt = 0 Then
                Call THREEFT(freethrow)
            Else
                Call MADEFT(freethrow)
            End If

            If autoPlay <> 1 Or playerMode <> 2 And swish& And d100% < 10 Then
                _SndPlay swish&
            End If

        Else

            If pbpOpt = 1 Then
                Call MISSFT(freethrow)
            Else
                Call BADFT(freethrow)
            End If

            If autoPlay <> 1 Or playerMode <> 2 And backboard& And d100% < 10 Then
                _SndPlay backboard&
            End If

        End If

        If freethrow = 1 And shotRoll > plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 1) Then
            Exit For
        End If

    Next freethrow

    If shotRoll > ftShotChance Then
        missedLastFT = 1
    Else
        missedLastFT = 0
    End If

End Sub


'----------------------------------------
'        FreeThrow_Two Subroutine
'----------------------------------------
'After handling the deadball situation,
'this routine walks through play-by-play
'for 2 free throws. It also calculates
'the stat adjustments and score afterward.
Sub FreeThrow_Two (missedLastFT)

    offStatus = 50

    Call DEADBALLOPTIONS

    For I = 0 To 1
        If defCategory(I) = 0 Then
            pressureRating(I, 0) = 1
            pressureRating(I, 1) = 7
            pctContrib(I) = 0
        End If
    Next

    offStatus = 0

    If (autoPlay = 0 Or pbpDelay! > 0) Then

        If pbpOpt = 1 Then
            Call WINDEX
            Print gameTeams$(P)
        End If

        pbpString$ = "2 FT: " + players$(P, lineupIdx(P, ballCarrier)) + Str$(plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 1)) + " %"
        Call SELECTPBP(P, 0)

    End If

    'Two FT Attempts
    For freethrow = 1 To 2:

        'Increase FT ATT
        plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 9) = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 9) + 1

        Call Rolld100(shotRoll)

        'FT Success Chance
        ftShotChance = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 1)

        If tmFatigue(P, lineupIdx(P, ballCarrier)) <= -5 Then
            ftShotChance = CInt(ftShotChance * .9)
        End If

        If shotRoll <= ftShotChance Then

            If pbpOpt = 1 Then
                Call MADEFT(freethrow)
            Else
                Call THREEFT(freethrow)
            End If

            If autoPlay <> 1 Or playerMode <> 2 And swish& And d100% < 10 Then
                _SndPlay swish&
            End If

        Else

            If pbpOpt = 1 Then
                Call MISSFT(freethrow)
            Else
                Call BADFT(freethrow)
            End If

            If autoPlay <> 1 Or playerMode <> 2 And backboard& And d100% < 10 Then
                _SndPlay backboard&
            End If

        End If

    Next freethrow

    If shotRoll > ftShotChance Then
        missedLastFT = 1
    Else
        missedLastFT = 0
    End If


End Sub


'----------------------------------------
'       FreeThrow_Three Subroutine
'----------------------------------------
'After handling the deadball situation,
'this routine walks through play-by-play
'for 3 free throws. It also calculates
'the stat adjustments and score afterward.
Sub FreeThrow_Three (missedLastFT)

    offStatus = 50

    Call DEADBALLOPTIONS

    For I = 0 To 1
        If defCategory(I) = 0 Then
            pressureRating(I, 0) = 1
            pressureRating(I, 1) = 7
            pctContrib(I) = 0
        End If
    Next

    offStatus = 0

    If (autoPlay = 0 Or pbpDelay! > 0) Then

        If pbpOpt = 1 Then
            Call WINDEX
            Print gameTeams$(P)
        End If

        pbpString$ = "3 FT: " + players$(P, lineupIdx(P, ballCarrier)) + Str$(plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 1)) + " %"
        Call SELECTPBP(P, 0)

    End If

    'Increase FT ATT
    plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 9) = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 9) + 3

    '3 FT Attempts
    For freethrow = 1 To 3:

        Call Rolld100(shotRoll)

        'FT Success Chance
        ftShotChance = plyrOff_GAME!(P, lineupIdx(P, ballCarrier), 1)
        If tmFatigue(P, lineupIdx(P, ballCarrier)) <= -5 Then
            ftShotChance = Int(ftShotChance * .9)
        End If

        If shotRoll <= ftShotChance Then

            If pbpOpt = 1 Then
                Call MADEFT(freethrow)
            Else
                Call THREEFT(freethrow)
            End If

            If autoPlay <> 1 Or playerMode <> 2 And swish& And d100% < 10 Then
                _SndPlay swish&
            End If

        Else

            If pbpOpt = 1 Then
                Call MISSFT(freethrow)
            Else
                Call BADFT(freethrow)
            End If

            If autoPlay <> 1 Or playerMode <> 2 And backboard& And d100% < 10 Then
                _SndPlay backboard&
            End If

        End If

    Next freethrow

    If shotRoll > ftShotChance Then
        missedLastFT = 1
    Else
        missedLastFT = 0
    End If
    
End Sub


'----------------------------------------
'           OPTIONS Subroutine
'----------------------------------------
'This subroutine gives you a small menu
'for changing some of the game options
'(such as pbp delay, coaching mode) in
'the middle of game play.
Sub OPTIONS ()

    Shared coachMode$(), playMode$()

    Screen , , 0, 0

    Call INFO

    Do

        Color , 0
        Locate 3, 1
        Color 14: Print "(1) ";
        Color 15: Print playMode$(playerOpt)
        Color 14: Print "(2) ";
        Color 15: Print Using "PLAY-BY-PLAY DELAY: #.# "; pbpDelay!
        Color 14: Print "(3) ";
        Color 15: Print "HUMAN COACHES: "; coachMode$(coachOpt)

        Print
        Color 14: Print "<ENTER> ";
        Color 15: Print "RETURN TO GAME PLAY"

        Do
            Color 7, 0
            'Locate 24, 1: Print "HIT ANY KEY TO CONTINUE";
            I$ = GetKeyPress$
            I = Val(I$)
        Loop Until I >= 0 And I <= 3

        Select Case I

            Case 1
                playerOpt = playerOpt + 1
                If playerOpt > 3 Then playerOpt = 0

            Case 2
                Locate 4, 23: Input pbpDelay!

            Case 3
                coachOpt = 1 - coachOpt

        End Select

        ' Determine computer team
        Select Case playerOpt

            Case 1:
                playerMode = 1
                compTeam = 0

            Case 2:
                playerMode = 1
                compTeam = 1

            Case 3:
                playerMode = 2
                'compTeam = 0

        End Select

    Loop Until I$ = Chr$(13)

    Screen , , 0, 0
    Call SCOREBOARD

End Sub


'----------------------------------------
'           EVENTS Subroutine
'----------------------------------------
'This subroutine works with another to
'look for any events that meet the limits
'for "special". If found, it will create
'a corresponding "special" box score
Sub EVENTS (specialCount, specialStat)

    Shared eventSettings()

    If _FileExists("EVENTS.BOX") Then

        Open "EVENTS.BOX" For Input As #1

        For X = 1 To 13
            Input #1, eventSettings(X)
        Next

        Close #1

        checkSpecial% = 0

        'Team / game conditions for special box score
        If currHalf - 3 > eventSettings(1) Or score(1, 0) + score(0, 0) > eventSettings(2) Or totRebounds(0) + totRebounds(1) > eventSettings(3) Or score(1, 0) > eventSettings(4) Or score(0, 0) > eventSettings(4) Or totRebounds(1) > eventSettings(5) Or totRebounds(0) > eventSettings(5) Then

            checkSpecial% = 1

        Else

            For I = 0 To 1

                For I1 = 0 To 13

                    'Player conditions for special box score
                    'FGM * 2 + FTM + 3FM = Points / Game
                    If plyrOff_GAME!(I, I1, 8) * 2 + plyrOff_GAME!(I, I1, 10) + threeFG(I, I1, 0) > eventSettings(6) Or plyrOff_GAME!(I, I1, 11) > eventSettings(7) Or plyrOff_GAME!(I, I1, 14) > eventSettings(8) Or plyrOff_GAME!(I, I1, 15) > eventSettings(9) Or plyrOff_GAME!(I, I1, 13) > eventSettings(10) Or threeFG(I, I1, 0) > eventSettings(11) Then
                        checkSpecial% = 1
                    End If

                    If checkSpecial% = 1 Then Exit For

                Next

                If checkSpecial% = 1 Then Exit For

            Next 'done checking if special event already found

        End If

    End If 'Done checking EVENTS.BOX file

    If checkSpecial% = 1 Then
        Call FoundSpecial(specialCount, specialStat)
    End If

End Sub


'----------------------------------------
'        FoundSpecial Subroutine
'----------------------------------------
'This subroutine simply counts how many
'special boxscore files already exist, so
'that it an assign a unique number to the
'boxscore about to be created.
Sub FoundSpecial (specialCount, specialStat)

    'Get count of SPC files to determine
    'number to assign next one
    'Spec$ = diskPaths$(1) + "*.SPC"
    Spec$ = diskPaths$(1) + "*.BSS"
    specialCount = FileCount%(Spec$)
    specialCount = specialCount + 1
    specialStat = 1

End Sub


'----------------------------------------
'          POSTGAME Subroutine
'----------------------------------------
'This subroutine processes everything that
'is necessary after a game is complete.
'It checks for records / special events,
'handles a prompt for stats, and saving
'of stats.
Sub POSTGAME ()

    Call CheckRecords
    Call EVENTS(specialCount, specialStat)
    gameOver = 1

    Do

        Color 15, 0
        Cls

        Color 14: Print "(0) ";
        Color 15: Print "STATS TO SCREEN"
        Print

        Color 14: Print "(1) ";
        Color 15: Print "STATS TO PRINTER"
        Print

        Color 14: Print "(2) ";
        Color 15: Print "STATS TO TEXT FILE"

        If autoPlay = 1 And playerMode = 2 Then
            response = 2
        Else
            Do
                I$ = GetKeyPress$
                response = Val(I$)
            Loop Until response >= 0 And response <= 2
        End If

        Call BOXOPTIONS(response)
        Call TICKER

        'Reset option for next call
        If response <> 0 Then response = 0

        'Prompt to display stats again
        Color 15, 0
        Cls

        If Not (autoPlay = 1 And playerMode = 2) Then

            Do
                'Using Home Team's color for the box
                Call TEAMBOX(1, 1)
                U$ = "DISPLAY STATS AGAIN? (Y/N)"
                Locate 7, (80 - Len(U$)) / 2
                Print U$
                Color 15, 0
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        End If

    Loop Until UCase$(I$) = "N" Or (autoPlay = 1 And playerMode = 2)

    'If we had special events
    If specialStat = 1 Then

        boxName$ = "BOXSCORE" + Mid$(Str$(specialCount), 2, Len(Str$(specialCount))) + ".BSS"
        'boxName$ = "BOX" + Mid$(Str$(specialCount), 2, Len(Str$(specialCount))) + ".SPC"

        'Append special events
        Call BOX2SPC(boxName$)

        Call BOX2FILE(2, boxName$, specialStat)

    End If

    Call SAVESTATS

    gameOver = 0
    specialStat = 0

End Sub


'----------------------------------------
'         BOXOPTIONS Subroutine
'----------------------------------------
'This subroutine takes appropriate action
'for the output of the box file, which
'depends on the choice given to it.

' PARAMETER outputChoice
'    represents menu choice
'       0 = Display on screen
'       1 = Send to printer
'       2 = Save to file

'The option to send to printer will first
'create a temporary file, then loop THROUGH
'it to print each line.
Sub BOXOPTIONS (outputChoice)

    Shared scheduleFile$, tourneyFile$

    Select Case outputChoice

        Case 0:
            Call BOX2SCREEN

        Case 1, 2:
            If outputChoice = 1 Then
                'Printer = temporary file
                boxName$ = "TEMPBOX"
            Else
                'File = prompt | auto-create
                If autoPlay <> 1 Then
                    Cls
                    Print "ENTER FILENAME FOR BOXSCORE (NO SPACES OR PERIODS)";
                    Input boxName$
                    boxName$ = boxName$ + ".BSS"
                    'boxName$ = boxName$ + ".SPC"
                Else
                    'ScheduleName_GameXXX.BS
                    boxName$ = Mid$(scheduleFile$, 1, Len(scheduleFile$) - 4) + "_GAME" + Mid$(Str$(schedGame(0) + schedGame(2)), 2, Len(Str$(schedGame(0) + schedGame(2)))) + ".BS"
                    'boxName$ = "BOX" + Mid$(Str$(schedGame(0) + schedGame(2)), 2, Len(Str$(schedGame(0) + schedGame(2))))
                End If

            End If

            'Includes a special option to send to printer when done
            Call BOX2FILE(outputChoice, boxName$, 0)

    End Select

End Sub


'----------------------------------------
'         BOX2SCREEN Subroutine
'----------------------------------------
'This subroutine formats the boxscore
'for output to the end user display
Sub BOX2SCREEN ()

    Shared actualAttendance&

    Cls

    Color 14
    If gameLoc = 2 Then
        Print "SITE: ";
        Color 15
        Print "NEUTRAL"
    Else
        Print "SITE: ";
        Color 15
        Print gameArena$(gameLoc)
    End If

    Color 14
    Print "ATTENDANCE: ";
    Color 15
    Print Using "###,###"; actualAttendance&
    Print

    Call ScoreSummary

    If Not (autoPlay = 1 And playerMode = 2) Then
        Locate 24, 1: Print "HIT ANY KEY TO CONTINUE";
        I$ = GetKeyPress
    End If

    Call STATS2SCREEN

End Sub


'----------------------------------------
'        ScoreSummary Subroutine
'----------------------------------------
'This subroutine prints a score by period
'for each team.
Sub ScoreSummary ()

    Locate 4, 17
    Color 14

    If halfTime = 1 Then
        Print "H1 "; Tab(25); "GAME"
    Else
        Print "H1 H2 ";

        If score(0, 1) + score(0, 2) = score(1, 1) + score(1, 2) Then

            Locate , 23
            Print "O1 ";

            If score(0, 1) + score(0, 2) + score(0, 3) = score(1, 1) + score(1, 2) + score(1, 3) Then
                Locate , 26
                Print "O2 ";

                If score(0, 1) + score(0, 2) + score(0, 3) + score(0, 4) = score(1, 1) + score(1, 2) + score(1, 3) + score(1, 4) Then
                    Locate , 29
                    Print "O3 ";

                    If score(0, 1) + score(0, 2) + score(0, 3) + score(0, 4) + score(0, 5) = score(1, 1) + score(1, 2) + score(1, 3) + score(1, 4) + score(1, 5) Then
                        Locate , 32
                        Print "O4 ";

                        If score(0, 1) + score(0, 2) + score(0, 3) + score(0, 4) + score(0, 5) + score(0, 6) = score(1, 1) + score(1, 2) + score(1, 3) + score(1, 4) + score(1, 5) + score(1, 6) Then
                            Locate , 35
                            Print "O5";
                            Locate , 40
                            Print "GAME"
                        Else
                            Locate , 37
                            Print "GAME"
                        End If
                    Else
                        Locate , 34
                        Print "GAME"
                    End If
                Else
                    Locate , 31
                    Print "GAME"
                End If
            Else
                Locate , 28
                Print "GAME"
            End If
        Else
            Locate , 25
            Print "GAME"
        End If

    End If

    Color 15, 0

    For I = 0 To 1:

        Locate 5 + I
        Color teamRat_GAME(I, 0), teamRat_GAME(I, 1)
        Print gameTeams$(I);

        Color 15, 0

        If halfTime = 1 Then
            Print Tab(16); Str$(score(I, 1)); Tab(22); " - "; score(I, 0)
        Else
            Print Tab(16); Str$(score(I, 1)); Tab(19); Str$(score(I, 2));

            If score(0, 1) + score(0, 2) = score(1, 1) + score(1, 2) Then
                Locate , 22: Print Str$(score(I, 3));

                If score(0, 1) + score(0, 2) + score(0, 3) = score(1, 1) + score(1, 2) + score(1, 3) Then
                    Locate , 25: Print Str$(score(I, 4));

                    If score(0, 1) + score(0, 2) + score(0, 3) + score(0, 4) = score(1, 1) + score(1, 2) + score(1, 3) + score(1, 4) Then
                        Locate , 28: Print Str$(score(I, 5));

                        If score(0, 1) + score(0, 2) + score(0, 3) + score(0, 4) + score(0, 5) = score(1, 1) + score(1, 2) + score(1, 3) + score(1, 4) + score(1, 5) Then
                            Locate , 31: Print Str$(score(I, 6));

                            If score(0, 1) + score(0, 2) + score(0, 3) + score(0, 4) + score(0, 5) + score(0, 6) = score(1, 1) + score(1, 2) + score(1, 3) + score(1, 4) + score(1, 5) + score(1, 6) Then
                                Locate , 35: Print Str$(score(I, 7)); " -  "; Str$(score(I, 0))
                            Else
                                Locate , 34: Print " - "; Str$(score(I, 0))
                            End If

                        Else
                            Locate , 31: Print " - "; Str$(score(I, 0))
                        End If

                    Else
                        Locate , 28: Print " - "; Str$(score(I, 0))
                    End If

                Else
                    Locate , 25: Print " - "; score(I, 0)
                End If

            Else
                Locate , 22: Print " - "; score(I, 0)
            End If

        End If

    Next I

End Sub


'----------------------------------------
'          BOX2FILE Subroutine
'----------------------------------------
'This subroutine formats the boxscore for
'an output file.

' PARAMETER printChoice
'    choice to print file vs. saving it

'The option to send to printer will first
'create a temporary file, then loop THROUGH
'it to print each line.
Sub BOX2FILE (printChoice, boxName$, hasSpecial)

    Shared actualAttendance&

    If hasSpecial = 1 Then
        Open (diskPaths$(1) + boxName$) For Append As #1
    Else
        Open (diskPaths$(1) + boxName$) For Output As #1
    End If

    If gameLoc = 2 Then
        Print #1, "SITE: NEUTRAL"
    Else
        Print #1, "SITE: "; gameArena$(gameLoc)
    End If

    Print #1, Using "ATTENDANCE: ###,###"; actualAttendance&
    Print #1,

    Print #1, ; Tab(17); "H1 H2 ";

    If score(0, 1) + score(0, 2) = score(1, 1) + score(1, 2) Then
        Print #1, ; Tab(23); "O1 ";

        If score(0, 1) + score(0, 2) + score(0, 3) = score(1, 1) + score(1, 2) + score(1, 3) Then
            Print #1, ; Tab(26); "O2 ";

            If score(0, 1) + score(0, 2) + score(0, 3) + score(0, 4) = score(1, 1) + score(1, 2) + score(1, 3) + score(1, 4) Then
                Print #1, ; Tab(29); "O3 ";

                If score(0, 1) + score(0, 2) + score(0, 3) + score(0, 4) + score(0, 5) = score(1, 1) + score(1, 2) + score(1, 3) + score(1, 4) + score(1, 5) Then
                    Print #1, ; Tab(32); "O4 ";

                    If score(0, 1) + score(0, 2) + score(0, 3) + score(0, 4) + score(0, 5) + score(0, 6) = score(1, 1) + score(1, 2) + score(1, 3) + score(1, 4) + score(1, 5) + score(1, 6) Then
                        Print #1, ; Tab(35); "O5";
                        Print #1, ; Tab(40); "GAME"
                    Else
                        Print #1, ; Tab(37); "GAME"
                    End If
                Else
                    Print #1, ; Tab(34); "GAME"
                End If
            Else
                Print #1, ; Tab(31); "GAME"
            End If
        Else
            Print #1, ; Tab(28); "GAME"
        End If
    Else
        Print #1, ; Tab(25); "GAME"
    End If

    For I = 0 To 1

        Print #1, gameTeams$(I); Tab(16); Str$(score(I, 1)); Tab(19); Str$(score(I, 2));

        If score(0, 1) + score(0, 2) = score(1, 1) + score(1, 2) Then

            Print #1, ; Tab(22); Str$(score(I, 3));

            If score(0, 1) + score(0, 2) + score(0, 3) = score(1, 1) + score(1, 2) + score(1, 3) Then
                Print #1, ; Tab(25); Str$(score(I, 4));

                If score(0, 1) + score(0, 2) + score(0, 3) + score(0, 4) = score(1, 1) + score(1, 2) + score(1, 3) + score(1, 4) Then
                    Print #1, ; Tab(28); Str$(score(I, 5));

                    If score(0, 1) + score(0, 2) + score(0, 3) + score(0, 4) + score(0, 5) = score(1, 1) + score(1, 2) + score(1, 3) + score(1, 4) + score(1, 5) Then
                        Print #1, ; Tab(31); Str$(score(I, 6));

                        If score(0, 1) + score(0, 2) + score(0, 3) + score(0, 4) + score(0, 5) + score(0, 6) = score(1, 1) + score(1, 2) + score(1, 3) + score(1, 4) + score(1, 5) + score(1, 6) Then
                            Print #1, ; Tab(35); Str$(score(I, 7)); " -  "; Str$(score(I, 0))
                        Else
                            Print #1, ; Tab(34); " - "; Str$(score(I, 0))
                        End If
                    Else
                        Print #1, ; Tab(31); " - "; Str$(score(I, 0))
                    End If
                Else
                    Print #1, ; Tab(28); " - "; Str$(score(I, 0))
                End If
            Else
                Print #1, ; Tab(25); " - "; score(I, 0)
            End If
        Else
            Print #1, ; Tab(22); " - "; score(I, 0)
        End If

    Next I

    For I = 0 To 1:

        Print #1,
        Print #1, gameTeams$(I)
        Print #1, "NAME"; Tab(14); " MIN   FG- A 3FG- A  FT- A ORB REB PTS PF  AST BLK STL TO"

        Erase statTotals

        For I1 = 0 To 13

            If players$(I, I1) <> "XXX" Then

                'Player Name
                Print #1, players$(I, I1); Tab(14);

                'Player Time
                If timePlayed(I, I1) = 0 Then
                    Print #1, Tab(15); "DNP";
                Else
                    Print #1, Using "##"; Int(timePlayed(I, I1) / 60);: Print #1, ":";

                    If timePlayed(I, I1) - Int(timePlayed(I, I1) / 60) * 60 > 9 Then
                        Print #1, Using "##"; timePlayed(I, I1) - Int(timePlayed(I, I1) / 60) * 60;
                    Else
                        Print #1, "0";: Print #1, Using "#"; timePlayed(I, I1) - Int(timePlayed(I, I1) / 60) * 60;
                    End If
                End If

                Print #1, ; Tab(21);

                'FGM - FGA
                Print #1, Using "##"; plyrOff_GAME!(I, I1, 8);
                Print #1, "-";
                Print #1, Using "##"; plyrOff_GAME!(I, I1, 7);

                '3FGM - 3FGA
                Print #1, Using "  ##"; threeFG(I, I1, 0);
                Print #1, "-";
                Print #1, Using "##"; threeFG(I, I1, 1);

                'FTM - FTA
                Print #1, Using "  ##"; plyrOff_GAME!(I, I1, 10);
                Print #1, "-";
                Print #1, Using "##"; plyrOff_GAME!(I, I1, 9);

                'ORB / RB / Points / Fouls / Assists / Blocks / Steals / Turnovers
                Print #1, Using "  ##  ## ###  #  ##  ##  ##  ##  ##  ##"; plyrOff_GAME!(I, I1, 24), plyrOff_GAME!(I, I1, 11), plyrOff_GAME!(I, I1, 8) * 2 + plyrOff_GAME!(I, I1, 10) + threeFG(I, I1, 0), plyrOff_GAME!(I, I1, 12), plyrOff_GAME!(I, I1, 14), plyrOff_GAME!(I, I1, 15), plyrOff_GAME!(I, I1, 13), plyrOff_GAME!(I, I1, 23)

                statTotals(0) = statTotals(0) + plyrOff_GAME!(I, I1, 8)
                statTotals(1) = statTotals(1) + plyrOff_GAME!(I, I1, 7)
                statTotals(2) = statTotals(2) + plyrOff_GAME!(I, I1, 10)
                statTotals(3) = statTotals(3) + plyrOff_GAME!(I, I1, 9)
                statTotals(4) = statTotals(4) + plyrOff_GAME!(I, I1, 11)
                statTotals(5) = statTotals(5) + (plyrOff_GAME!(I, I1, 8) * 2) + plyrOff_GAME!(I, I1, 10) + threeFG(I, I1, 0)
                statTotals(13) = statTotals(13) + threeFG(I, I1, 0)
                statTotals(14) = statTotals(14) + threeFG(I, I1, 1)
                statTotals(6) = statTotals(6) + plyrOff_GAME!(I, I1, 12)
                statTotals(7) = statTotals(7) + plyrOff_GAME!(I, I1, 14)
                statTotals(8) = statTotals(8) + plyrOff_GAME!(I, I1, 15)
                statTotals(9) = statTotals(9) + plyrOff_GAME!(I, I1, 13)
                statTotals(11) = statTotals(11) + plyrOff_GAME!(I, I1, 24)
                statTotals(12) = statTotals(12) + plyrOff_GAME!(I, I1, 23)
                statTotals(10) = statTotals(10) + timePlayed(I, I1)

            End If

        Next I1

        If statTotals(3) = 0 Then statTotals(3) = 1

        Print #1,
        Print #1, "TOTALS"; Tab(14); Int(statTotals(10) / 60); Tab(21);

        'FGM - FGA
        Print #1, Using "##"; statTotals(0);
        Print #1, "-";
        Print #1, Using "###"; statTotals(1);

        '3FGM - 3FGA
        Print #1, ; Tab(28);
        Print #1, Using "##"; statTotals(13);
        Print #1, "-";
        Print #1, Using "##"; statTotals(14);

        'FTM - FTA
        Print #1, ; Tab(35);
        Print #1, Using "##"; statTotals(2);
        Print #1, "-";
        Print #1, Using "##"; statTotals(3);

        Print #1, ; Tab(42);
        'ORB / RB / Points / Fouls / Assists / Blocks / Steals / Turnovers
        Print #1, Using "##  ## ### ##  ##  ##  ##  ##"; statTotals(11), statTotals(4), statTotals(5), statTotals(6), statTotals(7), statTotals(8), statTotals(9), statTotals(12)

        'FG %
        Print #1, ; Tab(22);
        If statTotals(1) > 0 Then Print #1, ; CInt(((statTotals(0) / statTotals(1)) * 100)); "%";

        '3FG %
        Print #1, ; Tab(28);
        If statTotals(14) > 0 Then Print #1, ; CInt(((statTotals(13) / statTotals(14)) * 100)); "%";

        'FT %
        Print #1, ; Tab(35);
        If statTotals(3) > 0 Then Print #1, ; CInt(((statTotals(2) / statTotals(3)) * 100)); "%"

        Print #1,
        Print #1, "TOTAL TURNOVERS -"; turnovers(I)
        Print #1, "TOTAL REBOUNDS  -"; totRebounds(I)

    Next I

    Print #1,

    '-- The code below was used to print
    '-- disabled players in the boxscore
    '-- output so that they could be easily identified.
    'Print #1, "BENCH WARMERS"
    '--Print #1, "DISABLED BUT PLAYED"

    'For I = 0 To 1

    '    For I1 = 0 To 13

    '        'If players$(I, I1) <> "XXX" And rosterStatus(I, I1) = -1 Then
    '--If players$(I, I1) <> "XXX" And rosterStatus(I, I1) = -1 And timePlayed(I, I1) > 0 Then
    '            Print #1, players$(I, I1); " ["; gameTeams$(I); "]"
    '        End If

    '    Next I1

    'Next I

    If printChoice = 1 Then Print #1, Chr$(12)

    Close #1

    If printChoice = 1 Then

        Open diskPaths$(1) + boxName$ For Input As #2

        Do
            Line Input #2, X$
            LPrint X$
        Loop While Left$(X$, 1) <> Chr$(12)

        Close #2

        Kill "TEMPBOX"

    End If

End Sub


'----------------------------------------
'           BOX2SPC Subroutine
'----------------------------------------
'This subroutine adds any special event
'information to a boxscore file
Sub BOX2SPC (boxName$)

    Shared eventSettings()

    Open diskPaths$(1) + boxName$ For Output As #1

    Print #1,

    'Output any special events
    If currHalf - 3 > eventSettings(1) Then Print #1, Using "MORE THAN ## OVERTIMES"; eventSettings(1)
    If score(1, 0) + score(0, 0) > eventSettings(2) Then Print #1, Using "MORE THAN ### COMBINED POINTS"; eventSettings(2)
    If totRebounds(0) + totRebounds(1) > eventSettings(3) Then Print #1, Using "MORE THAN ### COMBINED REBOUNDS"; eventSettings(3)
    If score(0, 0) > eventSettings(4) Then Print #1, gameTeams$(0);: Print #1, Using " SCORED MORE THAN ### POINTS"; eventSettings(4)
    If score(1, 0) > eventSettings(4) Then Print #1, gameTeams$(1);: Print #1, Using " SCORED MORE THAN ### POINTS"; eventSettings(4)
    If totRebounds(0) > eventSettings(5) Then Print #1, gameTeams$(0);: Print #1, Using " HAD MORE THAN ### REBOUNDS"; eventSettings(5)
    If totRebounds(1) > eventSettings(5) Then Print #1, gameTeams$(1);: Print #1, Using " HAD MORE THAN ### REBOUNDS"; eventSettings(5)

    For I = 0 To 1
        For I1 = 0 To 13
            If plyrOff_GAME!(I, I1, 8) * 2 + plyrOff_GAME!(I, I1, 10) + threeFG(I, I1, 0) > eventSettings(6) Then Print #1, players$(I, I1);: Print #1, Using " SCORED MORE THAN ### POINTS"; eventSettings(6)
            If plyrOff_GAME!(I, I1, 11) > eventSettings(7) Then Print #1, players$(I, I1);: Print #1, Using " HAD MORE THAN ### REBOUNDS"; eventSettings(7)
            If plyrOff_GAME!(I, I1, 14) > eventSettings(8) Then Print #1, players$(I, I1);: Print #1, Using " HAD MORE THAN ### ASSISTS"; eventSettings(8)
            If plyrOff_GAME!(I, I1, 15) > eventSettings(9) Then Print #1, players$(I, I1);: Print #1, Using " BLOCKED MORE THAN ### SHOTS"; eventSettings(9)
            If plyrOff_GAME!(I, I1, 13) > eventSettings(10) Then Print #1, players$(I, I1);: Print #1, Using " HAD MORE THAN ### STEALS"; eventSettings(10)
            If threeFG(I, I1, 0) > eventSettings(11) Then Print #1, players$(I, I1);: Print #1, Using " MADE MORE THAN ### 3-PT FIELD GOALS"; eventSettings(11)
        Next
    Next

    Print #1,

    Close #1

End Sub


'----------------------------------------
'        STATS2SCREEN Subroutine
'----------------------------------------
'This subroutine formats the game stats
'for output to the end user display
Sub STATS2SCREEN

    For I = 0 To 1:

        Cls

        Color teamRat_GAME(I, 0), teamRat_GAME(I, 1)
        Print gameTeams$(I)

        Color 14, 0
        Print "NAME"; Tab(14); " MIN   FG- A 3FG- A  FT- A ORB REB PTS PF  AST BLK STL TO"

        Erase statTotals

        For I1 = 0 To 13

            If players$(I, I1) <> "XXX" And timePlayed(I, I1) <> 0 Then

                Color 15

                'Name
                Print players$(I, I1); Tab(14);

                'Minutes
                If timePlayed(I, I1) <> 0 Then
                    Print Using "##"; Int(timePlayed(I, I1) / 60);
                    Print ":";
                    If timePlayed(I, I1) - Int(timePlayed(I, I1) / 60) * 60 > 9 Then
                        Print Using "##"; timePlayed(I, I1) - Int(timePlayed(I, I1) / 60) * 60;
                    Else
                        Print "0";
                        Print Using "#"; timePlayed(I, I1) - Int(timePlayed(I, I1) / 60) * 60;
                    End If
                End If

                Locate , 21

                'FG
                Print Using "##"; plyrOff_GAME!(I, I1, 8);
                Print "-";
                Print Using "##"; plyrOff_GAME!(I, I1, 7);

                '3FG
                Print Using "  ##"; threeFG(I, I1, 0);
                Print "-";
                Print Using "##"; threeFG(I, I1, 1);

                'FT
                Print Using "  ##"; plyrOff_GAME!(I, I1, 10);
                Print "-";
                Print Using "##"; plyrOff_GAME!(I, I1, 9);

                'ORB / RB / PT / F / A /  B / S / TO
                Print Using "  ##  ## ###  #  ##  ##  ##  ##  ##  ##"; plyrOff_GAME!(I, I1, 24), plyrOff_GAME!(I, I1, 11), plyrOff_GAME!(I, I1, 8) * 2 + plyrOff_GAME!(I, I1, 10) + threeFG(I, I1, 0), plyrOff_GAME!(I, I1, 12), plyrOff_GAME!(I, I1, 14), plyrOff_GAME!(I, I1, 15), plyrOff_GAME!(I, I1, 13), plyrOff_GAME!(I, I1, 23);

                If timePlayed(I, I1) = 0 Then Locate , 15: Print "DNP" Else Print

                statTotals(0) = statTotals(0) + plyrOff_GAME!(I, I1, 8)
                statTotals(1) = statTotals(1) + plyrOff_GAME!(I, I1, 7)
                statTotals(2) = statTotals(2) + plyrOff_GAME!(I, I1, 10)
                statTotals(3) = statTotals(3) + plyrOff_GAME!(I, I1, 9)
                statTotals(4) = statTotals(4) + plyrOff_GAME!(I, I1, 11)
                statTotals(5) = statTotals(5) + (plyrOff_GAME!(I, I1, 8) * 2) + plyrOff_GAME!(I, I1, 10) + threeFG(I, I1, 0)

                statTotals(13) = statTotals(13) + threeFG(I, I1, 0)
                statTotals(14) = statTotals(14) + threeFG(I, I1, 1)

                statTotals(6) = statTotals(6) + plyrOff_GAME!(I, I1, 12)
                statTotals(7) = statTotals(7) + plyrOff_GAME!(I, I1, 14)
                statTotals(8) = statTotals(8) + plyrOff_GAME!(I, I1, 15)
                statTotals(9) = statTotals(9) + plyrOff_GAME!(I, I1, 13)
                statTotals(11) = statTotals(11) + plyrOff_GAME!(I, I1, 24)
                statTotals(12) = statTotals(12) + plyrOff_GAME!(I, I1, 23)
                statTotals(10) = statTotals(10) + timePlayed(I, I1)

            End If

        Next I1

        If statTotals(3) = 0 Then statTotals(3) = 1

        Print
        Color 14
        Print "TOTALS";

        Color 15
        'Minutes
        Print Tab(14); Int(statTotals(10) / 60);

        'FGM / FGA
        Locate , 21: Print Using "##"; statTotals(0);
        Print "-";
        Print Using "###"; statTotals(1);

        '3FGM / 3FGA
        Locate , 28: Print Using "##"; statTotals(13);
        Print "-";
        Print Using "##"; statTotals(14);

        'FTM / FTA
        Locate , 35: Print Using "##"; statTotals(2);
        Print "-";
        Print Using "##"; statTotals(3);

        'ORB / RB / PT / F / A / B / S / TO
        Locate , 42: Print Using "##  ## ### ##  ##  ##  ##  ##"; statTotals(11), statTotals(4), statTotals(5), statTotals(6), statTotals(7), statTotals(8), statTotals(9), statTotals(12)

        'FG %
        Print Tab(22);
        If statTotals(1) > 0 Then Print CInt(((statTotals(0) / statTotals(1)) * 100)); "%";

        '3FG %
        Print Tab(28);
        If statTotals(14) > 0 Then Print CInt(((statTotals(13) / statTotals(14)) * 100)); "%";

        'FT %
        Print Tab(35);
        If statTotals(3) > 0 Then Print CInt(((statTotals(2) / statTotals(3)) * 100)); "%"

        Print
        Color 14: Print "TOTAL TURNOVERS -";
        Color 15: Print turnovers(I)

        Color 14: Print "TOTAL REBOUNDS  -";
        Color 15: Print totRebounds(I)

        If Not (autoPlay = 1 And playerMode = 2) Then
            Locate 24, 1: Print "HIT ANY KEY TO CONTINUE";
            I$ = GetKeyPress$
        End If

    Next I

End Sub


'----------------------------------------
'          SAVESTATS Subroutine
'----------------------------------------
'This subroutine prompts the user in ORDER
'to determine which teams to save satts For
'(autoplay will always save both teams).
'It also updates the GAME82 progress file,
'since at this point only can the game be
'considered fully complete.
Sub SAVESTATS ()

    Shared game3FGM!(), game3FGA!(), statsOffTot!()

    Shared scheduleFile$, tourneyFile$

    Color 15, 0
    Cls

    If autoPlay <> 1 Then

        Call TEAMBOX(1, 1)

        Do
            U$ = "SAVE STATS? (Y/N)"
            Locate 7, (80 - Len(U$)) / 2
            Print U$
            Color 15, 0
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    End If

    ' Update schedule, ticker, etc.
    If UCase$(I$) = "Y" Or autoPlay = 1 Then

        If _FileExists("GAME82") Then

            Call TickerSchedule

            If schedGame(1) < schedGame(0) + schedGame(2) Then
                Kill "GAME82"
                endAllGames = 1
            Else

                Open "GAME82" For Output As #1

                Print #1, RTrim$(scheduleFile$)

                For I = 0 To 2
                    Print #1, schedGame(I)
                Next

                Close #1

            End If

        Else

            If _FileExists("TOURNEY") Then

                Open "TOURNEY" For Input As #2

                Input #2, tourneyFile$

                For I = 0 To 2
                    Input #2, schedGame(I)
                Next

                Close #2

                Open diskPaths$(1) + tourneyFile$ As #1 Len = 94

                For X = 0 To 14
                    Field #1, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
                Next

                Field #1, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
                Field #1, 64 As X$, 5 As Q$(20), 5 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
                Field #1, 88 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)

                schedOptions(13) = 1

                For X = 0 To 14:
                    LSet Q$(X + 1) = MKI$(schedOptions(X))
                Next

                For X = 0 To 3:
                    LSet Q$(20 + X) = diskIDs$(X)
                Next

                For X = 1 To 3:
                    LSet Q$(X + 23) = MKI$(schedOptions(X + 14))
                Next

                LSet Q$(16) = MKI$(score(0, 0))
                LSet Q$(17) = MKI$(score(1, 0))
                LSet Q$(18) = schedVisTm$
                LSet Q$(19) = schedHomeTm$

                Put #1, schedGame(0) + schedGame(2)

                Close #1

                schedGame(2) = schedGame(2) + 1
                autoPlay = 1

                Open "TOURNEY" For Output As #2

                Print #2, tourneyFile$

                For I = 0 To 2
                    Print #2, schedGame(I)
                Next

                Close #2

                Call TOURNEY_Game

                If schedGame(1) < schedGame(0) + schedGame(2) Then
                    Kill "TOURNEY"
                    endAllGames = 1
                End If

            End If

        End If

        Cls

        For PX = 0 To 1
            For J = 0 To 13
                plyrOff_GAME!(PX, J, 16) = timePlayed(PX, J)
                plyrOff_GAME!(PX, J, 17) = plyrOff_GAME!(PX, J, 23)
                plyrOff_GAME!(PX, J, 18) = plyrOff_GAME!(PX, J, 24)
            Next
        Next

        For PX = 0 To 1

            For I = 0 To 13

                For J = 1 To 12
                    plyrOff_GAME!(PX, I, J) = plyrOff_GAME!(PX, I, J + 6)
                Next

                game3FGM!(PX, I) = threeFG(PX, I, 0)
                game3FGA!(PX, I) = threeFG(PX, I, 1)

            Next

        Next

        'Start with 1st team
        For idx = 0 To 1

            skipCompile% = 0

            Color 15, 0
            Cls

            Call TEAMBOX(1, idx)

            U$ = "COMPILE STATS FOR " + gameTeams$(idx) + "? (Y/N)"
            Locate , (80 - Len(U$)) / 2
            Print U$
            Color 15, 0

            If autoPlay <> 1 Then

                Do
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                If UCase$(I$) = "N" Then skipCompile% = 1

            End If

            If skipCompile% <> 1 Then

                Call COMPILESTATFILES(idx)

            End If

            statsOffTot!(12) = 0

        Next idx

    End If

End Sub


'----------------------------------------
'      COMPILESTATFILES Subroutine
'----------------------------------------
'This subroutine updates the various stat
'files for the corresponding team based on
'the stats generated for a completed game.
Sub COMPILESTATFILES (teamIdx)

    Shared actualAttendance&

    Shared gameAttendance&()

    Shared game3FGM!(), game3FGA!()
    Shared statsOffTot!(), statsDefTot!(), gameStatPlyrVal!()

    Shared powerRating(), teamScore(), oppScore()

    Shared gameSite$(), locIndicator$(), oppName$()

    statFile$ = nameStatFile$(gameTeams$(teamIdx))

    If _FileExists(diskPaths$(1) + statFile$) Then

        Open diskPaths$(1) + statFile$ For Input As #1

        Input #1, gameTeams$(teamIdx)

        For I = 0 To 13:

            Input #1, players$(teamIdx, I)

            For J = 0 To 13:
                Input #1, gameStatPlyrVal!(I, J)
            Next J

            Input #1, statsGame3FGM(teamIdx, I)
            Input #1, statsGame3FGA(teamIdx, I)

        Next I

        For L = 0 To 15:
            Input #1, statsOffTot!(L)
        Next L

        For L = 0 To 15:
            Input #1, statsDefTot!(L)
        Next L

        Close #1

    End If

    TG! = 0

    For I = 0 To 13

        If plyrOff_GAME!(teamIdx, I, 2) * 2 + plyrOff_GAME!(teamIdx, I, 4) + game3FGM!(teamIdx, I) > gameStatPlyrVal!(I, 13) Then
            gameStatPlyrVal!(I, 13) = plyrOff_GAME!(teamIdx, I, 2) * 2 + plyrOff_GAME!(teamIdx, I, 4) + game3FGM!(teamIdx, I)
        End If

        If plyrOff_GAME!(teamIdx, I, 10) > 0 Then gameStatPlyrVal!(I, 0) = gameStatPlyrVal!(I, 0) + 1

        For J = 1 To 12

            gameStatPlyrVal!(I, J) = gameStatPlyrVal!(I, J) + plyrOff_GAME!(teamIdx, I, J)

            If J <> 5 And J <> 10 And J <> 11 Then
                statsOffTot!(J) = statsOffTot!(J) + plyrOff_GAME!(teamIdx, I, J)
                statsDefTot!(J) = statsDefTot!(J) + plyrOff_GAME!(1 - teamIdx, I, J)
            End If

        Next J

        statsOffTot!(0) = statsOffTot!(0) + plyrOff_GAME!(teamIdx, I, 10)
        TG! = TG! + plyrOff_GAME!(teamIdx, I, 10)

    Next I

    For I = 0 To 1

        'Total Steals
        statTotals(I) = 0

        For I1 = 0 To 13
            statTotals(I) = statTotals(I) + plyrOff_GAME!(I, I1, 5)
        Next

    Next

    'Total Steals
    statsOffTot!(15) = statsOffTot!(15) + totRebounds(teamIdx) - statTotals(teamIdx)
    statsDefTot!(15) = statsDefTot!(15) + totRebounds(1 - teamIdx) - statTotals(1 - teamIdx)

    'Total Rebounds
    statsOffTot!(5) = statsOffTot!(5) + totRebounds(teamIdx)
    statsDefTot!(5) = statsDefTot!(5) + totRebounds(1 - teamIdx)

    'Total Turnovers
    statsOffTot!(11) = statsOffTot!(11) + turnovers(teamIdx)
    statsDefTot!(11) = statsDefTot!(11) + turnovers(1 - teamIdx)

    For J = 0 To 13:
        statsOffTot!(13) = statsOffTot!(13) + game3FGM!(teamIdx, J)
        statsOffTot!(14) = statsOffTot!(14) + game3FGA!(teamIdx, J)
        statsDefTot!(13) = statsDefTot!(13) + game3FGM!(1 - teamIdx, J)
        statsDefTot!(14) = statsDefTot!(14) + game3FGA!(1 - teamIdx, J)
        statsGame3FGM(teamIdx, J) = statsGame3FGM(teamIdx, J) + game3FGM!(teamIdx, J)
        statsGame3FGA(teamIdx, J) = statsGame3FGA(teamIdx, J) + game3FGA!(teamIdx, J)
    Next J

    statsOffTot!(10) = statsOffTot!(10) + score(teamIdx, 0)
    statsDefTot!(10) = statsDefTot!(10) + score(1 - teamIdx, 0)

    Call TEAMBOX(1, teamIdx)
    pbpString$ = "SAVING STATS FOR " + gameTeams$(teamIdx)
    Locate , (80 - Len(pbpString$)) / 2
    Print pbpString$

    Color 15, 0

    Open diskPaths$(1) + statFile$ For Output As #2

    Print #2, gameTeams$(teamIdx)

    For I = 0 To 13:

        Print #2, players$(teamIdx, I)

        For J = 0 To 13:
            Print #2, gameStatPlyrVal!(I, J)
        Next J

        Print #2, statsGame3FGM(teamIdx, I)
        Print #2, statsGame3FGA(teamIdx, I)

    Next I

    For L = 0 To 15:
        Print #2, statsOffTot!(L)
    Next L

    For L = 0 To 15:
        Print #2, statsDefTot!(L)
    Next L

    Close #2

    Print

    numberGames = 0

    If _FileExists(diskPaths$(1) + statFile$ + "S") Then
        Call ReadScheduleStats(statFile$, numberGames)
    End If

    If numberGames < MAX_SCHED_STATS Then
        numberGames = numberGames + 1
    End If

    Select Case gameLoc

        Case 2
            Select Case TG!
                Case Is < 12100
                    locIndicator$(numberGames) = "N"
                Case 12100 To 13599
                    locIndicator$(numberGames) = "N1"
                Case 13600 To 15099
                    locIndicator$(numberGames) = "N2"
                Case 15100 To 16599
                    locIndicator$(numberGames) = "N3"
                Case 16600 To 18099
                    locIndicator$(numberGames) = "N4"
            End Select

        Case teamIdx
            Select Case TG!
                Case Is < 12100
                    locIndicator$(numberGames) = "H"
                Case 12100 To 13599
                    locIndicator$(numberGames) = "H1"
                Case 13600 To 15099
                    locIndicator$(numberGames) = "H2"
                Case 15100 To 16599
                    locIndicator$(numberGames) = "H3"
                Case 16600 To 18099
                    locIndicator$(numberGames) = "H4"
            End Select
        

        Case (1 - teamIdx)
            Select Case TG!
                Case Is < 12100
                    locIndicator$(numberGames) = "V"
                Case 12100 To 13599
                    locIndicator$(numberGames) = "V1"
                Case 13600 To 15099
                    locIndicator$(numberGames) = "V2"
                Case 15100 To 16599
                    locIndicator$(numberGames) = "V3"
                Case 16600 To 18099
                    locIndicator$(numberGames) = "V4"
            End Select

    End Select

    teamScore(numberGames) = score(teamIdx, 0)
    oppName$(numberGames) = gameTeams$(1 - teamIdx)
    oppScore(numberGames) = score(1 - teamIdx, 0)
    gameAttendance&(numberGames) = actualAttendance&

    If teamRat_GAME(0, 9) = 0 Then
        teamRat_GAME(0, 9) = 78
    End If
    If teamRat_GAME(1, 9) = 0 Then
        teamRat_GAME(1, 9) = 78
    End If

    powerRating(numberGames, 0) = teamRat_GAME(teamIdx, 9)
    powerRating(numberGames, 1) = teamRat_GAME(1 - teamIdx, 9)

    If gameArena$(1) = "" Then
        gameArena$(1) = "MEMORIAL COLISEUM"
    End If

    If gameLoc = 2 Then
        gameSite$(numberGames) = "NEUTRAL SITE"
    Else
        gameSite$(numberGames) = gameArena$(1)
    End If

    Call SaveScheduleStats(statFile$, numberGames)

    'Reset arrays for next iteration
    Erase powerRating, oppScore, teamScore
    Erase gameAttendance&
    Erase statsOffTot!, statsDefTot!, gameStatPlyrVal!
    Erase gameSite$, locIndicator$, oppName$

End Sub


'----------------------------------------
'            pbplog Routine
'----------------------------------------
'This subroutine is designed to help me
'print information about where the game
'is at during the code. The general idea
'was originally to be called everywhere
'there is PBP. This helps me debug where
'issues are happening in a game.
Sub pbplog (lineNumber)

    'Print #11, "Calling from "; lineNumber
    'Print #11, currGame; ","; ballCarrier; ","; lineNumber
    'Print #11, test
    'Print #11, lineNumber
    Print #11, shotRoll; " | "; threePtShotChance
    'Print #11,

End Sub


'----------------------------------------
'       ChooseGameTeam Subroutine
'----------------------------------------
'This subroutine provides interface for
'choosing a team for the game
Sub ChooseGameTeam (yearNumber$, numberTeams, chosenIdx, teamName$, gameIdx)

    Shared previousYear$

    Shared teamIndicator$()

    Do

        chosenIdx = 0

        Color 15, 0
        Cls

        Call MenuBox(6, 8, 61, 7, 15, 4)

        Locate 7, 32

        Locate , 12: Print teamIndicator$(gameIdx)
        Locate , 12: Print "ENTER DISK ID: "
        Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Print
        Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
        Locate 8,

        I$ = GetKeyPress$

        If I$ <> Chr$(27) Then

            If I$ <> Chr$(13) Then

                Locate 8, 27: Print I$;

                NY$ = ""

                Do Until NY$ = Chr$(13)

                    NY$ = GetKeyPress$

                    If NY$ = Chr$(8) And Len(I$) >= 1 Then
                        col% = Pos(0)
                        I$ = Left$(I$, (Len(I$) - 1))
                        Locate , col% - 1
                        Print " ";
                        Locate , col% - 1
                    Else
                        I$ = I$ + NY$
                        Print NY$;
                    End If

                Loop

                I$ = Left$(I$, (Len(I$) - 1))

                If Not (I$ = "" And yearNumber$ <> "") Then

                    yearNumber$ = RTrim$(I$)
                    Erase teamNames$
                    numberTeams = 0

                    Cls

                    If Not _FileExists(diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$) Then

                        MsgText$ = "NO TEAMS WERE FOUND MATCHING THAT ID... PRESS ANY KEY TO SELECT ANOTHER ID"
                        BoxWidth = 20
                        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                        Call CSMsgBox(MsgText$, BoxWidth, PASS)
                        I$ = GetKeyPress$
                        Call CSMsgBox("", 0, PASS)

                        Exit Sub

                    Else

                        Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$ For Random As #1 Len = TEAM_SIZE_BYTES

                        fileLength& = LOF(1)

                        Field #1, 15 As Q$(0)

                        numberTeams = fileLength& / TEAM_SIZE_BYTES

                        For teamIdx = 1 To numberTeams
                            Get #1, teamIdx
                            'numberTeams = numberTeams + 1
                            teamNames$(teamIdx) = Q$(0)
                            teamIndex(teamIdx) = teamIdx
                        Next teamIdx

                        Close #1

                    End If 'Done checking if file exists

                End If 'Done checking for blank year number

                previousYear$ = yearNumber$

            Else

                yearNumber$ = previousYear$

            End If 'Done checking for ENTER key

            'Locate 2, 33
            'Color 31: Print "LOADING..."

            Do

                Call SelectGameTeam(numberTeams, yearNumber$, chosenIdx, teamName$, ExitCode%, gameIdx)
                teamGender$(gameIdx) = GetTeamGender$(yearNumber$)

                If ExitCode% <> 27 And numberTeams <> 0 And chosenIdx > 0 Then

                    Do

                        YCoord = Int((80 - Len(teamName$)) / 2)
                        Locate 2, YCoord
                        Color teamRat_GAME(gameIdx, 0), teamRat_GAME(gameIdx, 1): Print teamName$

                        Color 15, 4
                        Locate 3, 32: Print "ANY CHANGE? (Y/N)";

                        J$ = GetKeyPress$

                    Loop Until J$ = Chr$(27) Or UCase$(J$) = "Y" Or UCase$(J$) = "N"

                End If

                'chosenIdx = -1 means there was invalid data
            Loop Until ExitCode% = 27 Or J$ = Chr$(27) Or UCase$(J$) = "N" Or chosenIdx = -1

        End If 'Done checking for ESC

    Loop Until UCase$(J$) = "N" Or I$ = Chr$(27) 'Or J$ = Chr$(27)

    'If I$ = Chr$(27) Or J$ = Chr$(27) Then chosenIdx = 0

End Sub


'----------------------------------------
'       SelectGameTeam Subroutine
'----------------------------------------
'This subroutine provides an interface for
'selecting a team from the list loaded by
'the disk ID.
Sub SelectGameTeam (numberTeams, yearNumber$, selectedIdx, teamName$, ExitCode%, gameIdx)

    ExitCode% = -1
    validData = 0

    If numberTeams > 0 And CheckString%(teamNames$(numberTeams)) = 0 Then

        SELECTION% = 1
        Count% = numberTeams
        Start% = 1
        FileFGColor = 15: FileBGColor = 4
        HiLiteFGColor = 0: HiLiteBGColor = 7
        Rows = 20
        Columns% = 4
        Spacing% = 6
        Row% = 4
        Column% = 1

        Color , 4
        Cls
        Color 14
        Locate , 33: Print "TEAM SELECTION"
        Locate 25, 1
        Color 14: Print "ESC";
        Color 15: Print "-SELECT NEW ID  PGDN  PGUP  ";
        Color 14: Print "ENTER";
        Color 15: Print "-SELECTS TEAM";

        _MouseShow "DEFAULT"
        MAMenu teamNames$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor, HiLiteBGColor, Rows%, Columns%, Spacing%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 Then

            selectedIdx = teamIndex(SELECTION%)

            'Locate 2, 33
            'Color 31: Print "LOADING..."

            Call ReadGameTeam(yearNumber$, teamName$, selectedIdx, gameIdx)
            Call ReadGameExtraData(yearNumber$, selectedIdx, gameIdx, validData)

        End If

    End If

    If validData = 0 And ExitCode% <> 27 Then

        selectedIdx = -1
        Locate 9,
        Call CSMsgBox("THERE WAS AN ISSUE READING TEAM DATA. YOU MAY HAVE CHOSEN A TEAM FROM 4.0... PRESS ANY KEY AND TRY ANOTHER DISK ID", 20, PASS)
        K$ = GetKeyPress$
        Call CSMsgBox("", 0, PASS)

    End If

End Sub


'----------------------------------------
'        ReadGameTeam Subroutine
'----------------------------------------
'This subroutine reads in the data for the
'selected tema into each of the data arrays
'needed by the game.
Sub ReadGameTeam (teamYear$, targetTeam$, teamIdx, gameIdx)

    Shared FY%()

    Shared d3FG_Adj(), dthreeFGA_Adj()

    Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + teamYear$ For Random As #1 Len = TEAM_SIZE_BYTES

    Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 2 As Q$(14), 2 As Q$(15), 2 As Q$(16), 2 As Q$(17), 2 As Q$(18), 2 As Q$(19), 602 As X$

    For I = 0 To 13
        Field #1, 53 + I * 43 As X$, 15 As Q$(I * 15 + 20), 2 As Q$(I * 15 + 21), 2 As Q$(I * 15 + 22), 2 As Q$(I * 15 + 23), 2 As Q$(I * 15 + 24), 2 As Q$(I * 15 + 25), 2 As Q$(I * 15 + 26), 2 As Q$(I * 15 + 27), 2 As Q$(I * 15 + 28), 2 As Q$(I * 15 + 29), 2 As Q$(I * 15 + 30), 2 As Q$(I * 15 + 31), 2 As Q$(I * 15 + 32), 2 As Q$(I * 15 + 33), 2 As Q$(I * 15 + 34), TEAM_SIZE_BYTES - 53 - 43 - I * 43 As X$
    Next

    Get #1, teamIdx

    targetTeam$ = RTrim$(Q$(0))
    teamStamina(gameIdx) = CVI(Q$(1))
    dFGPA(gameIdx) = CVI(Q$(2))
    d3FG_Adj(gameIdx) = CVI(Q$(3))
    dthreeFGA_Adj(gameIdx) = CVI(Q$(4))

    For I3 = 0 To 4:
        leagRat_GAME(gameIdx, I3) = CVI(Q$(I3 + 5))
    Next

    For I3 = 0 To 9:
        teamRat_GAME(gameIdx, I3) = CVI(Q$(I3 + 10))
    Next

    For I3 = 0 To 13

        players$(gameIdx, I3) = RTrim$(Q$(I3 * 15 + 20))

        For I4 = 0 To 6
            plyrOff_GAME!(gameIdx, I3, I4) = CVI(Q$(I3 * 15 + I4 + 21))
            plyrRat_GAME(gameIdx, I3, I4) = CVI(Q$(I3 * 15 + I4 + 28))
        Next

    Next

    Close #1

    'FY% = 2 * (Average FGA/G for this team (Off + Def) - League FGA/G)
    FY%(gameIdx) = 2 * teamRat_GAME(gameIdx, 3) - teamRat_GAME(gameIdx, 4)

    CNT% = 0
    CNTX% = 0

    Do
        If teamRat_GAME(gameIdx, 5) <> 0 Then

            For I = 0 To 13

                If rosterStatus(gameIdx, I) = -1 Then rosterStatus(gameIdx, I) = 0

                Call Rolld100(RN0)

                If UCase$(players$(gameIdx, I)) = "XXX" Then
                    CNTX% = CNTX% + 1
                End If

                If UCase$(players$(gameIdx, I)) = "XXX" Or (rosterStatus(gameIdx, I) = 0 And RN0 > CInt(plyrRat_GAME(gameIdx, I, 0) / teamRat_GAME(gameIdx, 5) * 100)) Then
                    rosterStatus(gameIdx, I) = -1
                End If

            Next

            For I = 0 To 13:
                If rosterStatus(gameIdx, I) = 0 Then CNT% = CNT% + 1
            Next

        End If

    Loop Until (CNT% < 10 And CNTX% < 4) Or (CNTX% >= 5 And CNT% < 8)

End Sub


'----------------------------------------
'     ReadGameExtraData Subroutine
'----------------------------------------
'Reads in extra data for the selected team,
' given it's index in the team file.
'This extra file usually includes info.
' related to stadiums, coaches, etc.
'This routine is highly customized TO
' the specific game for which it has
' been included for.
Sub ReadGameExtraData (teamYear$, teamIdx, gameIdx, dataOK)

    Shared avgAttendance&()

    If Not _FileExists(diskPaths$(0) + DATA_FILE_NAME$ + "." + teamYear$) Then

        MsgText$ = "WARNING!!! NO DATA FILE WAS FOUND FOR THIS TEAM. PRESS ANY KEY TO CONTINUE"
        BoxWidth = 20
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, PASS)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, PASS)

    Else

        Open diskPaths$(0) + DATA_FILE_NAME$ + "." + teamYear$ As #2 Len = DATA_SIZE_BYTES

        Field #2, 15 As Q$(0), 40 As Q$(1), 15 As Q$(2), 4 As Q$(3)

        For I = 0 To 13
            Field #2, 74 + I * 32 As X$, 2 As Q$(I * 16 + 4), 2 As Q$(I * 16 + 5), 2 As Q$(I * 16 + 6), 2 As Q$(I * 16 + 7), 2 As Q$(I * 16 + 8), 2 As Q$(I * 16 + 9), 2 As Q$(I * 16 + 10), 2 As Q$(I * 16 + 11), 2 As Q$(I * 16 + 12), 2 As Q$(I * 16 + 13), 2 As Q$(I * 16 + 14), 2 As Q$(I * 16 + 15), 2 As Q$(I * 16 + 16), 2 As Q$(I * 16 + 17), 2 As Q$(I * 16 + 18), 2 As Q$(I * 16 + 19)
        Next

        Field #2, 522 As X$, 2 As Q$(228), 2 As Q$(229), 2 As Q$(230), 2 As Q$(231), 2 As Q$(232), 2 As Q$(233), 2 As Q$(234), 2 As Q$(235), 2 As Q$(236), 2 As Q$(237), 2 As Q$(238), 2 As Q$(239), 2 As Q$(240), 2 As Q$(241), 2 As Q$(242), 2 As Q$(243), 2 As Q$(244), 2 As Q$(245), 2 As Q$(246), 2 As Q$(247), 2 As Q$(248), 2 As Q$(249), 2 As Q$(250), 2 As Q$(251), 2 As Q$(252), 2 As Q$(253)

        Get #2, teamIdx

        gameCoaches$(gameIdx) = RTrim$(Q$(0))
        gameArena$(gameIdx) = RTrim$(Q$(1))
        gameMascots$(gameIdx) = RTrim$(Q$(2))
        avgAttendance&(gameIdx) = CVL(Q$(3))

        For I = 0 To 13

            plyrDef_GAME(gameIdx, I) = CVI(Q$(I * 16 + 4))
            positions_GAME$(gameIdx, I) = Q$(I * 16 + 5)

            For I1 = 0 To 13
                plyrStat_GAME(gameIdx, I, I1) = CVI(Q$(I * 16 + I1 + 6))
            Next

        Next

        For I = 0 To 20
            teamStats_GAME(gameIdx, I) = CVI(Q$(228 + I))
        Next

        If avgAttendance&(gameIdx) > 100000 Or ((CheckString%(gameArena$(gameIdx)) = 1) And gameArena$(gameIdx) <> "") Then
            'If attendance is abnormally high, or arena is not a string,
            'we ARE clearly not reading accurate data

            dataOK = 0

        Else

            For I = 21 To 24
                teamStats_GAME(gameIdx, I) = CVI(Q$(228 + I))
            Next

            CK = CVI(Q$(253))

            dataOK = 1

        End If

        Close #2

    End If

End Sub


'----------------------------------------
'      GetKeyWhenNoDelay Subroutine
'----------------------------------------
'Allows user to silently press a key
'(no prompt) when the game is not autoplay
Sub GetKeyWhenNoDelay ()

    'If we're not in autoplay
    If Not (autoPlay = 1 And playerMode = 2) Then
        I$ = GetKeyPress$
    End If

End Sub


'----------------------------------------
'          EndOfGame Subroutine
'----------------------------------------
'This subroutine serves as the final portion
'of a post game routine. At this point,
'stats have been saved and we are simply
'looking to determine whether a new game
'should be started.
Sub EndOfGame ()

    Close #11
    Call POSTGAME

    Cls

    ' Prompt to play another game
    If autoPlay <> 1 Then

        'Using Home Team's color for the box
        Call TEAMBOX(1, 1)

        pbpString$ = "PLAY ANOTHER GAME? (Y/N)"
        Locate 7, (80 - Len(pbpString$)) / 2
        Print pbpString$

        Color 15, 0

        Do
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    End If

    If UCase$(I$) = "Y" Or (autoPlay = 1 And endAllGames <> 1) Then
        Cls
        Print "PLEASE WAIT ..."
        Call COLHOOP
    End If

End Sub


'----------------------------------------
'        CheckRecords Subroutine
'----------------------------------------
'This subroutine checks all stats to see
'if any records were broken. Any that have
'been reached will be assigned to arrays
'for later updating the files.
Sub CheckRecords ()

    Shared indRecDesc$(), teamRecDesc$()
    Shared indRecords!(), teamRecords!()

    Dim V8(1, 14)
    Dim VERSUS$(1)

    VERSUS$(0) = "AT"
    VERSUS$(1) = "VS."

    If _FileExists(diskPaths$(1) + "RECORDS.GAM") Then

        Open diskPaths$(1) + "RECORDS.GAM" For Input As #1

        For I = 0 To 48
            For I1 = 0 To 2
                Input #1, indRecords!(I, I1)
            Next
            For I1 = 0 To 4
                Input #1, indRecDesc$(I, I1)
            Next
        Next

        For I = 0 To 125
            For I1 = 0 To 2
                Input #1, teamRecords!(I, I1)
            Next
            For I1 = 0 To 3
                Input #1, teamRecDesc$(I, I1)
            Next
        Next

        Close #1

    Else

        teamRecords!(2, 0) = 999
        teamRecords!(4, 0) = 999
        teamRecords!(7, 0) = 999
        teamRecords!(9, 0) = 999
        teamRecords!(11, 0) = 999
        teamRecords!(13, 0) = 999
        teamRecords!(15, 0) = 999
        teamRecords!(17, 0) = 999
        teamRecords!(19, 0) = 999
        teamRecords!(21, 0) = 999
        teamRecords!(23, 0) = 999
        teamRecords!(25, 0) = 999
        teamRecords!(27, 0) = 999
        teamRecords!(29, 0) = 999
        teamRecords!(31, 0) = 999
        teamRecords!(33, 0) = 999
        teamRecords!(35, 0) = 999
        teamRecords!(37, 0) = 999
        teamRecords!(43, 0) = 999
        teamRecords!(45, 0) = 999
        teamRecords!(61, 0) = 999
        teamRecords!(63, 0) = 999
        teamRecords!(65, 0) = 999
        teamRecords!(67, 0) = 999
        teamRecords!(73, 0) = 999
        teamRecords!(75, 0) = 999
        teamRecords!(81, 0) = 999
        teamRecords!(83, 0) = 999
        teamRecords!(87, 0) = 999
        teamRecords!(89, 0) = 999
        teamRecords!(92, 0) = 999
        teamRecords!(94, 0) = 999
        teamRecords!(97, 0) = 999
        teamRecords!(99, 0) = 999
        teamRecords!(105, 0) = 999
        teamRecords!(107, 0) = 999
        teamRecords!(113, 0) = 999
        teamRecords!(115, 0) = 999
        teamRecords!(120, 0) = 999
        teamRecords!(123, 0) = 999
        teamRecords!(125, 0) = 999

    End If

    'CHECK THE INDIVIDUAL RECORDS

    For I = 0 To 1

        For I1 = 0 To 13

            ' SCORING RECORDS FOR POINTS IN A GAME, HALF, QUARTER AND OVERTIME PERIOD
            If plyrOff_GAME!(I, I1, 8) * 2 + plyrOff_GAME!(I, I1, 10) + threeFG(I, I1, 0) > indRecords!(0, 0) Then
                indRecords!(0, 0) = plyrOff_GAME!(I, I1, 8) * 2 + plyrOff_GAME!(I, I1, 10) + threeFG(I, I1, 0) ' POINTS
                indRecords!(0, 1) = 1 ' TIMES DONE
                indRecords!(0, 2) = 0 ' GAME
                indRecDesc$(0, 0) = players$(I, I1) ' NAME
                indRecDesc$(0, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(0, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(0, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(0, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 13) > indRecords!(1, 0) Then
                indRecords!(1, 0) = QQ(I, 1, I1, 13) ' POINTS
                indRecords!(1, 1) = 1 ' TIMES DONE
                indRecords!(1, 2) = 1 ' HALF
                indRecDesc$(1, 0) = players$(I, I1) ' NAME
                indRecDesc$(1, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(1, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(1, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(1, 4) = Date$ ' DATE
            End If

            If QQ(I, 2, I1, 13) > indRecords!(1, 0) Then
                indRecords!(1, 0) = QQ(I, 2, I1, 13) ' POINTS
                indRecords!(1, 1) = 1 ' TIMES DONE
                indRecords!(1, 2) = 2 ' HALF
                indRecDesc$(1, 0) = players$(I, I1) ' NAME
                indRecDesc$(1, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(1, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(1, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(1, 4) = Date$ ' DATE
            End If

            For I2 = 3 To 8
                If QQ(I, I2, I1, 13) > indRecords!(3, 0) Then
                    indRecords!(3, 0) = QQ(I, I2, I1, 13) ' POINTS
                    indRecords!(3, 1) = 1 ' TIMES DONE
                    indRecords!(3, 2) = I2 ' QTR
                    indRecDesc$(3, 0) = players$(I, I1) ' NAME
                    indRecDesc$(3, 1) = gameTeams$(I) ' TEAM
                    indRecDesc$(3, 2) = VERSUS$(I) ' AT/VS
                    indRecDesc$(3, 3) = gameTeams$(1 - I) ' OPPONENT
                    indRecDesc$(3, 4) = Date$ ' DATE
                End If
            Next I2

            ' FIELD GOAL PERCENTAGE RECORDS FOR HIGH %, MOST W/NO MISSES, MOST W/NONE MADE
            If plyrOff_GAME!(I, I1, 8) >= 15 And plyrOff_GAME!(I, I1, 7) <> 0 Then
                If plyrOff_GAME!(I, I1, 8) / plyrOff_GAME!(I, I1, 7) > indRecords!(4, 0) Then
                    indRecords!(4, 0) = plyrOff_GAME!(I, I1, 8) / plyrOff_GAME!(I, I1, 7) ' PCTG
                    indRecords!(4, 1) = plyrOff_GAME!(I, I1, 8) ' FGM
                    indRecords!(4, 2) = plyrOff_GAME!(I, I1, 7) ' FGA
                    indRecDesc$(4, 0) = players$(I, I1) ' NAME
                    indRecDesc$(4, 1) = gameTeams$(I) ' TEAM
                    indRecDesc$(4, 2) = VERSUS$(I) ' AT/VS
                    indRecDesc$(4, 3) = gameTeams$(1 - I) ' OPPONENT
                    indRecDesc$(4, 4) = Date$ ' DATE
                End If
            End If

            If plyrOff_GAME!(I, I1, 8) = plyrOff_GAME!(I, I1, 7) And plyrOff_GAME!(I, I1, 7) > indRecords!(5, 0) Then
                indRecords!(5, 0) = plyrOff_GAME!(I, I1, 7) ' MOST FGM
                indRecDesc$(5, 0) = players$(I, I1) ' NAME
                indRecDesc$(5, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(5, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(5, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(5, 4) = Date$ ' DATE
            End If

            If plyrOff_GAME!(I, I1, 8) = 0 And plyrOff_GAME!(I, I1, 7) > indRecords!(6, 0) Then
                indRecords!(6, 0) = plyrOff_GAME!(I, I1, 7) ' 0 FGM /FGA
                indRecDesc$(6, 0) = players$(I, I1) ' NAME
                indRecDesc$(6, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(6, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(6, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(6, 4) = Date$ ' DATE
            End If

            ' FIELD GOALS MADE IN A GAME, HALF, QUARTER AND OVERTIME PERIOD
            If plyrOff_GAME!(I, I1, 8) > indRecords!(7, 0) Then
                indRecords!(7, 0) = plyrOff_GAME!(I, I1, 8) ' FGM
                indRecords!(7, 1) = 1 ' TIMES DONE
                indRecords!(7, 2) = 0 ' GAME
                indRecDesc$(7, 0) = players$(I, I1) ' NAME
                indRecDesc$(7, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(7, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(7, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(7, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 2) > indRecords!(8, 0) Then
                indRecords!(8, 0) = QQ(I, 1, I1, 2) ' FGM
                indRecords!(8, 1) = 1 ' TIMES DONE
                indRecords!(8, 2) = 1 ' HALF
                indRecDesc$(8, 0) = players$(I, I1) ' NAME
                indRecDesc$(8, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(8, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(8, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(8, 4) = Date$ ' DATE
            End If

            If QQ(I, 2, I1, 2) > indRecords!(8, 0) Then
                indRecords!(8, 0) = QQ(I, 2, I1, 2) ' FGM
                indRecords!(8, 1) = 1 ' TIMES DONE
                indRecords!(8, 2) = 2 ' HALF
                indRecDesc$(8, 0) = players$(I, I1) ' NAME
                indRecDesc$(8, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(8, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(8, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(8, 4) = Date$ ' DATE
            End If

            For I2 = 3 To 8
                If QQ(I, I2, I1, 2) > indRecords!(10, 0) Then
                    indRecords!(10, 0) = QQ(I, I2, I1, 2) ' FGM
                    indRecords!(10, 1) = 1 ' TIMES DONE
                    indRecords!(10, 2) = I2 ' QTR
                    indRecDesc$(10, 0) = players$(I, I1) ' NAME
                    indRecDesc$(10, 1) = gameTeams$(I) ' TEAM
                    indRecDesc$(10, 2) = VERSUS$(I) ' AT/VS
                    indRecDesc$(10, 3) = gameTeams$(1 - I) ' OPPONENT
                    indRecDesc$(10, 4) = Date$ ' DATE
                End If
            Next I2

            ' FIELD GOAL ATTEMPTS MADE IN A GAME, HALF, QUARTER AND OVERTIME PERIOD
            If plyrOff_GAME!(I, I1, 7) > indRecords!(11, 0) Then
                indRecords!(11, 0) = plyrOff_GAME!(I, I1, 7) ' FGA
                indRecords!(11, 1) = 1 ' TIMES DONE
                indRecords!(11, 2) = 0 ' GAME
                indRecDesc$(11, 0) = players$(I, I1) ' NAME
                indRecDesc$(11, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(11, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(11, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(11, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 1) > indRecords!(12, 0) Then
                indRecords!(12, 0) = QQ(I, 1, I1, 1) ' FGA
                indRecords!(12, 1) = 1 ' TIMES DONE
                indRecords!(12, 2) = 1 ' HALF
                indRecDesc$(12, 0) = players$(I, I1) ' NAME
                indRecDesc$(12, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(12, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(12, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(12, 4) = Date$ ' DATE
            End If

            If QQ(I, 2, I1, 1) > indRecords!(12, 0) Then
                indRecords!(12, 0) = QQ(I, 2, I1, 1) ' FGA
                indRecords!(12, 1) = 1 ' TIMES DONE
                indRecords!(12, 2) = 2 ' HALF
                indRecDesc$(12, 0) = players$(I, I1) ' NAME
                indRecDesc$(12, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(12, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(12, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(12, 4) = Date$ ' DATE
            End If

            For I2 = 3 To 8
                If QQ(I, I2, I1, 1) > indRecords!(14, 0) Then
                    indRecords!(14, 0) = QQ(I, I2, I1, 1) ' FGA
                    indRecords!(14, 1) = 1 ' TIMES DONE
                    indRecords!(14, 2) = I2 ' QTR
                    indRecDesc$(14, 0) = players$(I, I1) ' NAME
                    indRecDesc$(14, 1) = gameTeams$(I) ' TEAM
                    indRecDesc$(14, 2) = VERSUS$(I) ' AT/VS
                    indRecDesc$(14, 3) = gameTeams$(1 - I) ' OPPONENT
                    indRecDesc$(14, 4) = Date$ ' DATE
                End If
            Next I2

            ' THREE-POINT FIELD GOALS MOST MADE NO MISSES, MOST ATT NONE MADE
            If threeFG(I, I1, 0) = threeFG(I, I1, 1) And threeFG(I, I1, 0) > indRecords!(15, 0) Then
                indRecords!(15, 0) = threeFG(I, I1, 0) ' MOST 3-FGM
                indRecDesc$(15, 0) = players$(I, I1) ' NAME
                indRecDesc$(15, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(15, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(15, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(15, 4) = Date$ ' DATE
            End If

            If threeFG(I, I1, 0) = 0 And threeFG(I, I1, 1) > indRecords!(16, 0) Then
                indRecords!(16, 0) = threeFG(I, I1, 1) ' 0 3FGM /3FGA
                indRecDesc$(16, 0) = players$(I, I1) ' NAME
                indRecDesc$(16, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(16, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(16, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(16, 4) = Date$ ' DATE
            End If

            ' 3-FIELD GOALS MADE IN A GAME, HALF, QUARTER AND OVERTIME PERIOD
            If threeFG(I, I1, 0) > indRecords!(17, 0) Then
                indRecords!(17, 0) = threeFG(I, I1, 0) ' 3FGM
                indRecords!(17, 1) = 1 ' TIMES DONE
                indRecords!(17, 2) = 0 ' GAME
                indRecDesc$(17, 0) = players$(I, I1) ' NAME
                indRecDesc$(17, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(17, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(17, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(17, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 10) > indRecords!(18, 0) Then
                indRecords!(18, 0) = QQ(I, 1, I1, 10) ' 3FGM
                indRecords!(18, 1) = 1 ' TIMES DONE
                indRecords!(18, 2) = 1 ' HALF
                indRecDesc$(18, 0) = players$(I, I1) ' NAME
                indRecDesc$(18, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(18, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(18, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(18, 4) = Date$ ' DATE
            End If

            If QQ(I, 2, I1, 10) > indRecords!(18, 0) Then
                indRecords!(18, 0) = QQ(I, 2, I1, 10) ' 3FGM
                indRecords!(18, 1) = 1 ' TIMES DONE
                indRecords!(18, 2) = 2 ' HALF
                indRecDesc$(18, 0) = players$(I, I1) ' NAME
                indRecDesc$(18, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(18, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(18, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(18, 4) = Date$ ' DATE
            End If

            For I2 = 3 To 8
                If QQ(I, I2, I1, 10) > indRecords!(20, 0) Then
                    indRecords!(20, 0) = QQ(I, I2, I1, 10) ' 3FGM
                    indRecords!(20, 1) = 1 ' TIMES DONE
                    indRecords!(20, 2) = I2 ' QTR
                    indRecDesc$(20, 0) = players$(I, I1) ' NAME
                    indRecDesc$(20, 1) = gameTeams$(I) ' TEAM
                    indRecDesc$(20, 2) = VERSUS$(I) ' AT/VS
                    indRecDesc$(20, 3) = gameTeams$(1 - I) ' OPPONENT
                    indRecDesc$(20, 4) = Date$ ' DATE
                End If
            Next I2

            ' 3-FIELD GOALS ATT IN A GAME, HALF
            If threeFG(I, I1, 1) > indRecords!(21, 0) Then
                indRecords!(21, 0) = threeFG(I, I1, 1) ' 3FGA
                indRecords!(21, 1) = 1 ' TIMES DONE
                indRecords!(21, 2) = 0 ' GAME
                indRecDesc$(21, 0) = players$(I, I1) ' NAME
                indRecDesc$(21, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(21, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(21, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(21, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 11) > indRecords!(22, 0) Then
                indRecords!(22, 0) = QQ(I, 1, I1, 11) ' 3FGA
                indRecords!(22, 1) = 1 ' TIMES DONE
                indRecords!(22, 2) = 1 ' HALF
                indRecDesc$(22, 0) = players$(I, I1) ' NAME
                indRecDesc$(22, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(22, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(22, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(22, 4) = Date$ ' DATE
            End If

            If QQ(I, 2, I1, 11) > indRecords!(22, 0) Then
                indRecords!(22, 0) = QQ(I, 2, I1, 10) ' 3FGA
                indRecords!(22, 1) = 1 ' TIMES DONE
                indRecords!(22, 2) = 2 ' HALF
                indRecDesc$(22, 0) = players$(I, I1) ' NAME
                indRecDesc$(22, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(22, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(22, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(22, 4) = Date$ ' DATE
            End If

            ' FREE THROWS -  MOST MADE NO MISSES, MOST ATT NONE MADE
            If plyrOff_GAME!(I, I1, 9) = plyrOff_GAME!(I, I1, 10) And plyrOff_GAME!(I, I1, 9) > indRecords!(23, 0) Then
                indRecords!(23, 0) = plyrOff_GAME!(I, I1, 9) ' MOST FTM
                indRecDesc$(23, 0) = players$(I, I1) ' NAME
                indRecDesc$(23, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(23, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(23, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(23, 4) = Date$ ' DATE
            End If

            If plyrOff_GAME!(I, I1, 10) = 0 And plyrOff_GAME!(I, I1, 9) > indRecords!(24, 0) Then
                indRecords!(24, 0) = plyrOff_GAME!(I, I1, 10) ' 0 FTM /FTA
                indRecDesc$(24, 0) = players$(I, I1) ' NAME
                indRecDesc$(24, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(24, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(24, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(24, 4) = Date$ ' DATE
            End If

            ' MADE FREE THROWS IN A GAME, HALF, QUARTER
            If plyrOff_GAME!(I, I1, 10) > indRecords!(25, 0) Then
                indRecords!(25, 0) = plyrOff_GAME!(I, I1, 10) ' FTM
                indRecords!(25, 1) = 1 ' TIMES DONE
                indRecords!(25, 2) = 0 ' GAME
                indRecDesc$(25, 0) = players$(I, I1) ' NAME
                indRecDesc$(25, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(25, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(25, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(25, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 4) > indRecords!(26, 0) Then
                indRecords!(26, 0) = QQ(I, 1, I1, 4) ' FTM
                indRecords!(26, 1) = 1 ' TIMES DONE
                indRecords!(26, 2) = 1 ' HALF
                indRecDesc$(26, 0) = players$(I, I1) ' NAME
                indRecDesc$(26, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(26, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(26, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(26, 4) = Date$ ' DATE
            End If

            If QQ(I, 2, I1, 4) > indRecords!(26, 0) Then
                indRecords!(26, 0) = QQ(I, 2, I1, 4) ' FTM
                indRecords!(26, 1) = 1 ' TIMES DONE
                indRecords!(26, 2) = 2 ' HALF
                indRecDesc$(26, 0) = players$(I, I1) ' NAME
                indRecDesc$(26, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(26, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(26, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(26, 4) = Date$ ' DATE
            End If

            ' FREE THROWS ATTEMPTS IN A GAME, HALF, QUARTER
            If plyrOff_GAME!(I, I1, 9) > indRecords!(28, 0) Then
                indRecords!(28, 0) = plyrOff_GAME!(I, I1, 9) ' FTA
                indRecords!(28, 1) = 1 ' TIMES DONE
                indRecords!(28, 2) = 0 ' GAME
                indRecDesc$(28, 0) = players$(I, I1) ' NAME
                indRecDesc$(28, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(28, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(28, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(28, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 3) > indRecords!(29, 0) Then
                indRecords!(29, 0) = QQ(I, 1, I1, 3) ' FTA
                indRecords!(29, 1) = 1 ' TIMES DONE
                indRecords!(29, 2) = 1 ' HALF
                indRecDesc$(29, 0) = players$(I, I1) ' NAME
                indRecDesc$(29, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(29, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(29, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(29, 4) = Date$ ' DATE
            End If

            If QQ(I, 2, I1, 3) > indRecords!(29, 0) Then
                indRecords!(29, 0) = QQ(I, 2, I1, 3) ' FTM
                indRecords!(29, 1) = 1 ' TIMES DONE
                indRecords!(29, 2) = 2 ' HALF
                indRecDesc$(29, 0) = players$(I, I1) ' NAME
                indRecDesc$(29, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(29, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(29, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(29, 4) = Date$ ' DATE
            End If

            ' TOTAL REBOUNDS IN A GAME, HALF, QUARTER
            If plyrOff_GAME!(I, I1, 11) > indRecords!(31, 0) Then
                indRecords!(31, 0) = plyrOff_GAME!(I, I1, 11) ' FTA
                indRecords!(31, 1) = 1 ' TIMES DONE
                indRecords!(31, 2) = 0 ' GAME
                indRecDesc$(31, 0) = players$(I, I1) ' NAME
                indRecDesc$(31, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(31, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(31, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(31, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 5) > indRecords!(32, 0) Then
                indRecords!(32, 0) = QQ(I, 1, I1, 5) ' RB
                indRecords!(32, 1) = 1 ' TIMES DONE
                indRecords!(32, 2) = 1 ' HALF
                indRecDesc$(32, 0) = players$(I, I1) ' NAME
                indRecDesc$(32, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(32, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(32, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(32, 4) = Date$ ' DATE
            End If

            If QQ(I, 2, I1, 5) > indRecords!(32, 0) Then
                indRecords!(32, 0) = QQ(I, 2, I1, 5) ' RB
                indRecords!(32, 1) = 1 ' TIMES DONE
                indRecords!(32, 2) = 2 ' HALF
                indRecDesc$(32, 0) = players$(I, I1) ' NAME
                indRecDesc$(32, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(32, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(32, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(32, 4) = Date$ ' DATE
            End If

            ' TOTAL OFFENSIVE REBOUNDS IN A GAME, HALF, QUARTER
            If plyrOff_GAME!(I, I1, 24) > indRecords!(34, 0) Then
                indRecords!(34, 0) = plyrOff_GAME!(I, I1, 24) ' FTA
                indRecords!(34, 1) = 1 ' TIMES DONE
                indRecords!(34, 2) = 0 ' GAME
                indRecDesc$(34, 0) = players$(I, I1) ' NAME
                indRecDesc$(34, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(34, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(34, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(34, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 12) > indRecords!(35, 0) Then
                indRecords!(35, 0) = QQ(I, 1, I1, 12) ' RB
                indRecords!(35, 1) = 1 ' TIMES DONE
                indRecords!(35, 2) = 1 ' HALF
                indRecDesc$(35, 0) = players$(I, I1) ' NAME
                indRecDesc$(35, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(35, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(35, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(35, 4) = Date$ ' DATE
            End If

            If QQ(I, 2, I1, 12) > indRecords!(35, 0) Then
                indRecords!(35, 0) = QQ(I, 2, I1, 12) ' RB
                indRecords!(35, 1) = 1 ' TIMES DONE
                indRecords!(35, 2) = 2 ' HALF
                indRecDesc$(35, 0) = players$(I, I1) ' NAME
                indRecDesc$(35, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(35, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(35, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(35, 4) = Date$ ' DATE
            End If

            ' TOTAL DEFENSIVE REBOUNDS IN A GAME, HALF, QUARTER
            If plyrOff_GAME!(I, I1, 11) - plyrOff_GAME!(I, I1, 24) > indRecords!(37, 0) Then
                indRecords!(37, 0) = plyrOff_GAME!(I, I1, 11) - plyrOff_GAME!(I, I1, 24) ' FTA
                indRecords!(37, 1) = 1 ' TIMES DONE
                indRecords!(37, 2) = 0 ' GAME
                indRecDesc$(37, 0) = players$(I, I1) ' NAME
                indRecDesc$(37, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(37, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(37, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(37, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 5) - QQ(I, 1, I1, 12) > indRecords!(38, 0) Then
                indRecords!(38, 0) = QQ(I, 1, I1, 5) - QQ(I, 1, I1, 12) ' RB
                indRecords!(38, 1) = 1 ' TIMES DONE
                indRecords!(38, 2) = 1 ' HALF
                indRecDesc$(38, 0) = players$(I, I1) ' NAME
                indRecDesc$(38, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(38, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(38, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(38, 4) = Date$ ' DATE
            End If

            If QQ(I, 2, I1, 5) - QQ(I, 2, I1, 12) > indRecords!(38, 0) Then
                indRecords!(38, 0) = QQ(I, 2, I1, 5) - QQ(I, 2, I1, 12) ' RB
                indRecords!(38, 1) = 1 ' TIMES DONE
                indRecords!(38, 2) = 2 ' HALF
                indRecDesc$(38, 0) = players$(I, I1) ' NAME
                indRecDesc$(38, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(38, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(38, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(38, 4) = Date$ ' DATE
            End If

            ' ASSISTS IN A GAME, HALF, QUARTER
            If plyrOff_GAME!(I, I1, 14) > indRecords!(40, 0) Then
                indRecords!(40, 0) = plyrOff_GAME!(I, I1, 14) ' FTA
                indRecords!(40, 1) = 1 ' TIMES DONE
                indRecords!(40, 2) = 0 ' GAME
                indRecDesc$(40, 0) = players$(I, I1) ' NAME
                indRecDesc$(40, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(40, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(40, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(40, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 8) > indRecords!(41, 0) Then
                indRecords!(41, 0) = QQ(I, 1, I1, 8) ' FTA
                indRecords!(41, 1) = 1 ' TIMES DONE
                indRecords!(41, 2) = 1 ' HALF
                indRecDesc$(41, 0) = players$(I, I1) ' NAME
                indRecDesc$(41, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(41, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(41, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(41, 4) = Date$ ' DATE
            End If

            If QQ(I, 2, I1, 8) > indRecords!(41, 0) Then
                indRecords!(41, 0) = QQ(I, 2, I1, 8) ' FTM
                indRecords!(41, 1) = 1 ' TIMES DONE
                indRecords!(41, 2) = 2 ' HALF
                indRecDesc$(41, 0) = players$(I, I1) ' NAME
                indRecDesc$(41, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(41, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(41, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(41, 4) = Date$ ' DATE
            End If

            ' STEALS IN A GAME, HALF, QUARTER
            If plyrOff_GAME!(I, I1, 13) > indRecords!(43, 0) Then
                indRecords!(43, 0) = plyrOff_GAME!(I, I1, 13) ' FTA
                indRecords!(43, 1) = 1 ' TIMES DONE
                indRecords!(43, 2) = 0 ' GAME
                indRecDesc$(43, 0) = players$(I, I1) ' NAME
                indRecDesc$(43, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(43, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(43, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(43, 4) = Date$ ' DATE
            End If
            If QQ(I, 1, I1, 7) > indRecords!(44, 0) Then
                indRecords!(44, 0) = QQ(I, 1, I1, 7) ' FTA
                indRecords!(44, 1) = 1 ' TIMES DONE
                indRecords!(44, 2) = 1 ' HALF
                indRecDesc$(44, 0) = players$(I, I1) ' NAME
                indRecDesc$(44, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(44, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(44, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(44, 4) = Date$ ' DATE
            End If
            If QQ(I, 2, I1, 7) > indRecords!(44, 0) Then
                indRecords!(44, 0) = QQ(I, 2, I1, 7) ' FTM
                indRecords!(44, 1) = 1 ' TIMES DONE
                indRecords!(44, 2) = 2 ' HALF
                indRecDesc$(44, 0) = players$(I, I1) ' NAME
                indRecDesc$(44, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(44, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(44, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(44, 4) = Date$ ' DATE
            End If

            ' BLOCKS IN A GAME, HALF, QUARTER
            If plyrOff_GAME!(I, I1, 15) > indRecords!(46, 0) Then
                indRecords!(46, 0) = plyrOff_GAME!(I, I1, 15) ' FTA
                indRecords!(46, 1) = 1 ' TIMES DONE
                indRecords!(46, 2) = 0 ' GAME
                indRecDesc$(46, 0) = players$(I, I1) ' NAME
                indRecDesc$(46, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(46, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(46, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(46, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 9) > indRecords!(47, 0) Then
                indRecords!(47, 0) = QQ(I, 1, I1, 9) ' FTA
                indRecords!(47, 1) = 1 ' TIMES DONE
                indRecords!(47, 2) = 1 ' HALF
                indRecDesc$(47, 0) = players$(I, I1) ' NAME
                indRecDesc$(47, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(47, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(47, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(47, 4) = Date$ ' DATE
            End If

            If QQ(I, 2, I1, 9) > indRecords!(47, 0) Then
                indRecords!(47, 0) = QQ(I, 2, I1, 9) ' FTM
                indRecords!(47, 1) = 1 ' TIMES DONE
                indRecords!(47, 2) = 2 ' HALF
                indRecDesc$(47, 0) = players$(I, I1) ' NAME
                indRecDesc$(47, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(47, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(47, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(47, 4) = Date$ ' DATE
            End If

        Next
    Next

    ' MOST OVERTIME PERIODS
    If currHalf - 3 > teamRecords!(0, 0) Then
        teamRecords!(0, 0) = currHalf - 3
        teamRecords!(0, 1) = score(0, 0)
        teamRecords!(0, 2) = score(1, 0)
        teamRecDesc$(0, 0) = gameTeams$(0)
        teamRecDesc$(0, 1) = gameTeams$(1)
        teamRecDesc$(0, 2) = Date$
    End If

    For I = 0 To 1

        If score(I, 0) > teamRecords!(1, 0) Then
            teamRecords!(1, 0) = score(I, 0)
            teamRecords!(1, 1) = 1
            teamRecords!(1, 2) = 0
            teamRecDesc$(1, 0) = gameTeams$(I)
            teamRecDesc$(1, 1) = VERSUS$(I)
            teamRecDesc$(1, 2) = gameTeams$(1 - I)
            teamRecDesc$(1, 3) = Date$
        End If

        If score(I, 0) < teamRecords!(2, 0) Then
            teamRecords!(2, 0) = score(I, 0)
            teamRecords!(2, 1) = 1
            teamRecords!(2, 2) = 0
            teamRecDesc$(2, 0) = gameTeams$(I)
            teamRecDesc$(2, 1) = VERSUS$(I)
            teamRecDesc$(2, 2) = gameTeams$(1 - I)
            teamRecDesc$(2, 3) = Date$
        End If

        If score(I, 0) + score(1 - I, 0) > teamRecords!(3, 0) Then
            teamRecords!(3, 0) = score(I, 0) + score(1 - I, 0)
            teamRecords!(3, 1) = score(I, 0)
            teamRecords!(3, 2) = score(1 - I, 0)
            teamRecDesc$(3, 0) = gameTeams$(I)
            teamRecDesc$(3, 1) = VERSUS$(I)
            teamRecDesc$(3, 2) = gameTeams$(1 - I)
            teamRecDesc$(3, 3) = Date$
        End If

        If score(I, 0) + score(1 - I, 0) < teamRecords!(4, 0) Then
            teamRecords!(4, 0) = score(I, 0) + score(1 - I, 0)
            teamRecords!(4, 1) = score(I, 0)
            teamRecords!(4, 2) = score(1 - I, 0)
            teamRecDesc$(4, 0) = gameTeams$(I)
            teamRecDesc$(4, 1) = VERSUS$(I)
            teamRecDesc$(4, 2) = gameTeams$(1 - I)
            teamRecDesc$(4, 3) = Date$
        End If

        If score(I, 1) > teamRecords!(6, 0) Then
            teamRecords!(6, 0) = score(I, 1)
            teamRecords!(6, 1) = 1
            teamRecords!(6, 2) = 0
            teamRecDesc$(6, 0) = gameTeams$(I)
            teamRecDesc$(6, 1) = VERSUS$(I)
            teamRecDesc$(6, 2) = gameTeams$(1 - I)
            teamRecDesc$(6, 3) = Date$
        End If

        If score(I, 1) < teamRecords!(7, 0) Then
            teamRecords!(7, 0) = score(I, 1)
            teamRecords!(7, 1) = 1
            teamRecords!(7, 2) = 0
            teamRecDesc$(7, 0) = gameTeams$(I)
            teamRecDesc$(7, 1) = VERSUS$(I)
            teamRecDesc$(7, 2) = gameTeams$(1 - I)
            teamRecDesc$(7, 3) = Date$
        End If

        If score(I, 1) + score(1 - I, 1) > teamRecords!(8, 0) Then
            teamRecords!(8, 0) = score(I, 1) + score(1 - I, 1)
            teamRecords!(8, 1) = score(I, 1)
            teamRecords!(8, 2) = score(1 - I, 1)
            teamRecDesc$(8, 0) = gameTeams$(I)
            teamRecDesc$(8, 1) = VERSUS$(I)
            teamRecDesc$(8, 2) = gameTeams$(1 - I)
            teamRecDesc$(8, 3) = Date$
        End If

        If score(I, 1) + score(1 - I, 1) < teamRecords!(9, 0) Then
            teamRecords!(9, 0) = score(I, 1) + score(1 - I, 1)
            teamRecords!(9, 1) = score(I, 1)
            teamRecords!(9, 2) = score(1 - I, 1)
            teamRecDesc$(9, 0) = gameTeams$(I)
            teamRecDesc$(9, 1) = VERSUS$(I)
            teamRecDesc$(9, 2) = gameTeams$(1 - I)
            teamRecDesc$(9, 3) = Date$
        End If

        If score(I, 2) > teamRecords!(10, 0) Then
            teamRecords!(10, 0) = score(I, 2)
            teamRecords!(10, 1) = 1
            teamRecords!(10, 2) = 0
            teamRecDesc$(10, 0) = gameTeams$(I)
            teamRecDesc$(10, 1) = VERSUS$(I)
            teamRecDesc$(10, 2) = gameTeams$(1 - I)
            teamRecDesc$(10, 3) = Date$
        End If

        If score(I, 2) < teamRecords!(11, 0) Then
            teamRecords!(11, 0) = score(I, 2)
            teamRecords!(11, 1) = 1
            teamRecords!(11, 2) = 0
            teamRecDesc$(11, 0) = gameTeams$(I)
            teamRecDesc$(11, 1) = VERSUS$(I)
            teamRecDesc$(11, 2) = gameTeams$(1 - I)
            teamRecDesc$(11, 3) = Date$
        End If

        If score(I, 2) + score(1 - I, 2) > teamRecords!(12, 0) Then
            teamRecords!(12, 0) = score(I, 2) + score(1 - I, 2)
            teamRecords!(12, 1) = score(I, 2)
            teamRecords!(12, 2) = score(1 - I, 2)
            teamRecDesc$(12, 0) = gameTeams$(I)
            teamRecDesc$(12, 1) = VERSUS$(I)
            teamRecDesc$(12, 2) = gameTeams$(1 - I)
            teamRecDesc$(12, 3) = Date$
        End If

        If score(I, 2) + score(1 - I, 2) < teamRecords!(13, 0) Then
            teamRecords!(13, 0) = score(I, 2) + score(1 - I, 2)
            teamRecords!(13, 1) = score(I, 2)
            teamRecords!(13, 2) = score(1 - I, 2)
            teamRecDesc$(13, 0) = gameTeams$(I)
            teamRecDesc$(13, 1) = VERSUS$(I)
            teamRecDesc$(13, 2) = gameTeams$(1 - I)
            teamRecDesc$(13, 3) = Date$
        End If

        For I2 = 3 To 8
            If score(I, I2) > teamRecords!(14, 0) Then
                teamRecords!(14, 0) = score(I, I2)
                teamRecords!(14, 1) = I2
                teamRecords!(14, 2) = 0
                teamRecDesc$(14, 0) = gameTeams$(I)
                teamRecDesc$(14, 1) = VERSUS$(I)
                teamRecDesc$(14, 2) = gameTeams$(1 - I)
                teamRecDesc$(14, 3) = Date$
            End If

            If score(I, I2) < teamRecords!(15, 0) Then
                teamRecords!(15, 0) = score(I, I2)
                teamRecords!(15, 1) = I2
                teamRecords!(15, 2) = 0
                teamRecDesc$(15, 0) = gameTeams$(I)
                teamRecDesc$(15, 1) = VERSUS$(I)
                teamRecDesc$(15, 2) = gameTeams$(1 - I)
                teamRecDesc$(15, 3) = Date$
            End If

            If score(I, I2) + score(1 - I, I2) > teamRecords!(16, 0) Then
                teamRecords!(16, 0) = score(I, I2) + score(1 - I, I2)
                teamRecords!(16, 1) = score(I, I2)
                teamRecords!(16, 2) = score(1 - I, I2)
                teamRecDesc$(16, 0) = gameTeams$(I)
                teamRecDesc$(16, 1) = VERSUS$(I)
                teamRecDesc$(16, 2) = gameTeams$(1 - I)
                teamRecDesc$(16, 3) = Date$
            End If

            If score(I, I2) + score(1 - I, I2) < teamRecords!(17, 0) Then
                teamRecords!(17, 0) = score(I, I2) + score(1 - I, I2)
                teamRecords!(17, 1) = score(I, I2)
                teamRecords!(17, 2) = score(1 - I, I2)
                teamRecDesc$(17, 0) = gameTeams$(I)
                teamRecDesc$(17, 1) = VERSUS$(I)
                teamRecDesc$(17, 2) = gameTeams$(1 - I)
                teamRecDesc$(17, 3) = Date$
            End If
        Next I2

        ' TEAM FIELD GOAL PERCENTAGES
        For I1 = 0 To 14:
            V8(I, I1) = 0
            V8(1 - I, I1) = 0
        Next

        For I2 = 0 To 1
            For I1 = 0 To 13
                V8(I2, 0) = V8(I2, 0) + plyrOff_GAME!(I2, I1, 8)
                V8(I2, 1) = V8(I2, 1) + plyrOff_GAME!(I2, I1, 7)
                V8(I2, 2) = V8(I2, 2) + plyrOff_GAME!(I2, I1, 10)
                V8(I2, 3) = V8(I2, 3) + plyrOff_GAME!(I2, I1, 9)
                V8(I2, 4) = V8(I2, 4) + plyrOff_GAME!(I2, I1, 11)
                V8(I2, 5) = V8(I2, 5) + (plyrOff_GAME!(I2, I1, 8) * 2) + plyrOff_GAME!(I2, I1, 10) + threeFG(I2, I1, 0)
                V8(I2, 13) = V8(I2, 13) + threeFG(I2, I1, 0)
                V8(I2, 14) = V8(I2, 14) + threeFG(I2, I1, 1)
                V8(I2, 6) = V8(I2, 6) + plyrOff_GAME!(I2, I1, 12)
                V8(I2, 7) = V8(I2, 7) + plyrOff_GAME!(I2, I1, 14)
                V8(I2, 8) = V8(I2, 8) + plyrOff_GAME!(I2, I1, 15)
                V8(I2, 9) = V8(I2, 9) + plyrOff_GAME!(I2, I1, 13)
                V8(I2, 11) = V8(I2, 11) + plyrOff_GAME!(I2, I1, 24)
                V8(I2, 12) = V8(I2, 12) + plyrOff_GAME!(I2, I1, 23)
                V8(I2, 10) = V8(I2, 10) + timePlayed(I2, I1)
            Next
        Next

        If V8(I, 1) <> 0 Then

            If V8(I, 0) / V8(I, 1) > teamRecords!(30, 0) Then
                teamRecords!(30, 0) = V8(I, 0) / V8(I, 1)
                teamRecords!(30, 1) = V8(I, 0)
                teamRecords!(30, 2) = V8(I, 1)
                teamRecDesc$(30, 0) = gameTeams$(I)
                teamRecDesc$(30, 1) = VERSUS$(I)
                teamRecDesc$(30, 2) = gameTeams$(1 - I)
                teamRecDesc$(30, 3) = Date$
            End If

            If V8(I, 0) / V8(I, 1) < teamRecords!(31, 0) Then
                teamRecords!(31, 0) = V8(I, 0) / V8(I, 1)
                teamRecords!(31, 1) = V8(I, 0)
                teamRecords!(31, 2) = V8(I, 1)
                teamRecDesc$(31, 0) = gameTeams$(I)
                teamRecDesc$(31, 1) = VERSUS$(I)
                teamRecDesc$(31, 2) = gameTeams$(1 - I)
                teamRecDesc$(31, 3) = Date$
            End If

        End If

        If V8(I, 1) + V8(1 - I, 1) <> 0 Then

            If (V8(I, 0) + V8(1 - I, 0)) / (V8(I, 1) + V8(1 - I, 1)) > teamRecords!(32, 0) Then
                teamRecords!(32, 0) = (V8(I, 0) + V8(1 - I, 0)) / (V8(I, 1) + V8(1 - I, 1))
                teamRecords!(32, 1) = V8(I, 0) / V8(I, 1)
                teamRecords!(32, 2) = V8(1 - I, 0) / (V8(1 - I, 1) + 0.01)
                teamRecDesc$(32, 0) = gameTeams$(I)
                teamRecDesc$(32, 1) = VERSUS$(I)
                teamRecDesc$(32, 2) = gameTeams$(1 - I)
                teamRecDesc$(32, 3) = Date$
            End If

            If (V8(I, 0) + V8(1 - I, 0)) / (V8(I, 1) + V8(1 - I, 1)) < teamRecords!(33, 0) Then
                teamRecords!(33, 0) = (V8(I, 0) + V8(1 - I, 0)) / (V8(I, 1) + V8(1 - I, 1))
                teamRecords!(33, 1) = V8(I, 0) / V8(I, 1)
                teamRecords!(33, 2) = V8(1 - I, 0) / (V8(1 - I, 1) + 0.01)
                teamRecDesc$(33, 0) = gameTeams$(I)
                teamRecDesc$(33, 1) = VERSUS$(I)
                teamRecDesc$(33, 2) = gameTeams$(1 - I)
                teamRecDesc$(33, 3) = Date$
            End If

        End If

        ' TEAM FIELD GOALS MADE
        If V8(I, 0) > teamRecords!(34, 0) Then
            teamRecords!(34, 0) = V8(I, 0)
            teamRecords!(34, 1) = 0
            teamRecords!(34, 2) = 0
            teamRecDesc$(34, 0) = gameTeams$(I)
            teamRecDesc$(34, 1) = VERSUS$(I)
            teamRecDesc$(34, 2) = gameTeams$(1 - I)
            teamRecDesc$(34, 3) = Date$
        End If

        If V8(I, 0) < teamRecords!(35, 0) Then
            teamRecords!(35, 0) = V8(I, 0)
            teamRecords!(35, 1) = 0
            teamRecords!(35, 2) = 0
            teamRecDesc$(35, 0) = gameTeams$(I)
            teamRecDesc$(35, 1) = VERSUS$(I)
            teamRecDesc$(35, 2) = gameTeams$(1 - I)
            teamRecDesc$(35, 3) = Date$
        End If

        If V8(I, 0) + V8(1 - I, 0) > teamRecords!(36, 0) Then
            teamRecords!(36, 0) = V8(I, 0) + V8(1 - I, 0)
            teamRecords!(36, 1) = V8(I, 0)
            teamRecords!(36, 2) = V8(1 - I, 0)
            teamRecDesc$(36, 0) = gameTeams$(I)
            teamRecDesc$(36, 1) = VERSUS$(I)
            teamRecDesc$(36, 2) = gameTeams$(1 - I)
            teamRecDesc$(36, 3) = Date$
        End If

        If V8(I, 0) + V8(1 - I, 0) < teamRecords!(37, 0) Then
            teamRecords!(37, 0) = V8(I, 0) + V8(1 - I, 0)
            teamRecords!(37, 1) = V8(I, 0)
            teamRecords!(37, 2) = V8(1 - I, 0)
            teamRecDesc$(37, 0) = gameTeams$(I)
            teamRecDesc$(37, 1) = VERSUS$(I)
            teamRecDesc$(37, 2) = gameTeams$(1 - I)
            teamRecDesc$(37, 3) = Date$
        End If

        If QR(I, 1, 0) > teamRecords!(38, 0) Then
            teamRecords!(38, 0) = QR(I, 1, 0)
            teamRecords!(38, 1) = 1
            teamRecords!(38, 2) = 0
            teamRecDesc$(38, 0) = gameTeams$(I)
            teamRecDesc$(38, 1) = VERSUS$(I)
            teamRecDesc$(38, 2) = gameTeams$(1 - I)
            teamRecDesc$(38, 3) = Date$
        End If

        If QR(I, 2, 0) > teamRecords!(38, 0) Then
            teamRecords!(38, 0) = QR(I, 2, 0)
            teamRecords!(38, 1) = 2
            teamRecords!(38, 2) = 0
            teamRecDesc$(38, 0) = gameTeams$(I)
            teamRecDesc$(38, 1) = VERSUS$(I)
            teamRecDesc$(38, 2) = gameTeams$(1 - I)
            teamRecDesc$(38, 3) = Date$
        End If

        If QR(I, 1, 0) + QR(1 - I, 1, 0) > teamRecords!(39, 0) Then
            teamRecords!(39, 0) = QR(I, 1, 0) + QR(1 - I, 1, 0)
            teamRecords!(39, 1) = 1
            teamRecords!(39, 2) = 0
            teamRecDesc$(39, 0) = gameTeams$(I)
            teamRecDesc$(39, 1) = VERSUS$(I)
            teamRecDesc$(39, 2) = gameTeams$(1 - I)
            teamRecDesc$(39, 3) = Date$
        End If

        If QR(I, 2, 0) + QR(1 - I, 2, 0) > teamRecords!(39, 0) Then
            teamRecords!(39, 0) = QR(I, 2, 0) + QR(1 - I, 2, 0)
            teamRecords!(39, 1) = 2
            teamRecords!(39, 2) = 0
            teamRecDesc$(39, 0) = gameTeams$(I)
            teamRecDesc$(39, 1) = VERSUS$(I)
            teamRecDesc$(39, 2) = gameTeams$(1 - I)
            teamRecDesc$(39, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS
        If V8(I, 1) > teamRecords!(42, 0) Then
            teamRecords!(42, 0) = V8(I, 1)
            teamRecords!(42, 1) = 0
            teamRecords!(42, 2) = 0
            teamRecDesc$(42, 0) = gameTeams$(I)
            teamRecDesc$(42, 1) = VERSUS$(I)
            teamRecDesc$(42, 2) = gameTeams$(1 - I)
            teamRecDesc$(42, 3) = Date$
        End If

        If V8(I, 1) < teamRecords!(43, 0) Then
            teamRecords!(43, 0) = V8(I, 1)
            teamRecords!(43, 1) = 0
            teamRecords!(43, 2) = 0
            teamRecDesc$(43, 0) = gameTeams$(I)
            teamRecDesc$(43, 1) = VERSUS$(I)
            teamRecDesc$(43, 2) = gameTeams$(1 - I)
            teamRecDesc$(43, 3) = Date$
        End If

        If V8(I, 1) + V8(1 - I, 1) > teamRecords!(44, 0) Then
            teamRecords!(44, 0) = V8(I, 1) + V8(1 - I, 1)
            teamRecords!(44, 1) = V8(I, 1)
            teamRecords!(44, 2) = V8(1 - I, 1)
            teamRecDesc$(44, 0) = gameTeams$(I)
            teamRecDesc$(44, 1) = VERSUS$(I)
            teamRecDesc$(44, 2) = gameTeams$(1 - I)
            teamRecDesc$(44, 3) = Date$
        End If

        If V8(I, 1) + V8(1 - I, 1) < teamRecords!(45, 0) Then
            teamRecords!(45, 0) = V8(I, 1) + V8(1 - I, 1)
            teamRecords!(45, 1) = V8(I, 1)
            teamRecords!(45, 2) = V8(1 - I, 1)
            teamRecDesc$(45, 0) = gameTeams$(I)
            teamRecDesc$(45, 1) = VERSUS$(I)
            teamRecDesc$(45, 2) = gameTeams$(1 - I)
            teamRecDesc$(45, 3) = Date$
        End If

        If QR(I, 1, 1) > teamRecords!(46, 0) Then
            teamRecords!(46, 0) = QR(I, 1, 1)
            teamRecords!(46, 1) = 1
            teamRecords!(46, 2) = 0
            teamRecDesc$(46, 0) = gameTeams$(I)
            teamRecDesc$(46, 1) = VERSUS$(I)
            teamRecDesc$(46, 2) = gameTeams$(1 - I)
            teamRecDesc$(46, 3) = Date$
        End If

        If QR(I, 2, 1) > teamRecords!(46, 0) Then
            teamRecords!(46, 0) = QR(I, 2, 1)
            teamRecords!(46, 1) = 2
            teamRecords!(46, 2) = 0
            teamRecDesc$(46, 0) = gameTeams$(I)
            teamRecDesc$(46, 1) = VERSUS$(I)
            teamRecDesc$(46, 2) = gameTeams$(1 - I)
            teamRecDesc$(46, 3) = Date$
        End If

        If QR(I, 1, 1) + QR(1 - I, 1, 1) > teamRecords!(47, 0) Then
            teamRecords!(47, 0) = QR(I, 1, 1) + QR(1 - I, 1, 1)
            teamRecords!(47, 1) = 1
            teamRecords!(47, 2) = QR(I, 1, 1)
            teamRecDesc$(47, 0) = gameTeams$(I)
            teamRecDesc$(47, 1) = VERSUS$(I)
            teamRecDesc$(47, 2) = gameTeams$(1 - I)
            teamRecDesc$(47, 3) = Date$
        End If

        If QR(I, 2, 1) + QR(1 - I, 2, 1) > teamRecords!(47, 0) Then
            teamRecords!(47, 0) = QR(I, 2, 1) + QR(1 - I, 2, 1)
            teamRecords!(47, 1) = 2
            teamRecords!(47, 2) = QR(I, 2, 1)
            teamRecDesc$(47, 0) = gameTeams$(I)
            teamRecDesc$(47, 1) = VERSUS$(I)
            teamRecDesc$(47, 2) = gameTeams$(1 - I)
            teamRecDesc$(47, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS
        If V8(I, 13) = V8(I, 14) And V8(I, 14) > teamRecords!(50, 0) Then
            teamRecords!(50, 0) = V8(I, 13)
            teamRecords!(50, 1) = 0
            teamRecords!(50, 2) = 0
            teamRecDesc$(50, 0) = gameTeams$(I)
            teamRecDesc$(50, 1) = VERSUS$(I)
            teamRecDesc$(50, 2) = gameTeams$(1 - I)
            teamRecDesc$(50, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS MOST MADE
        If V8(I, 13) = V8(I, 14) And V8(1 - I, 13) = V8(1 - I, 14) And V8(I, 14) + V8(1 - I, 14) > teamRecords!(51, 0) Then
            teamRecords!(51, 0) = V8(I, 13)
            teamRecords!(51, 1) = V8(1 - I, 13)
            teamRecords!(51, 2) = 0
            teamRecDesc$(51, 0) = gameTeams$(I)
            teamRecDesc$(51, 1) = VERSUS$(I)
            teamRecDesc$(51, 2) = gameTeams$(1 - I)
            teamRecDesc$(51, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS NONE MADE
        If V8(I, 13) = 0 And V8(I, 14) > teamRecords!(52, 0) Then
            teamRecords!(52, 0) = V8(I, 14)
            teamRecords!(52, 1) = 0
            teamRecords!(52, 2) = 0
            teamRecDesc$(52, 0) = gameTeams$(I)
            teamRecDesc$(52, 1) = VERSUS$(I)
            teamRecDesc$(52, 2) = gameTeams$(1 - I)
            teamRecDesc$(52, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS NONE MADE
        If V8(I, 13) > teamRecords!(53, 0) Then
            teamRecords!(53, 0) = V8(I, 13)
            teamRecords!(53, 1) = 0
            teamRecords!(53, 2) = 0
            teamRecDesc$(53, 0) = gameTeams$(I)
            teamRecDesc$(53, 1) = VERSUS$(I)
            teamRecDesc$(53, 2) = gameTeams$(1 - I)
            teamRecDesc$(53, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS NONE MADE
        If V8(I, 13) + V8(1 - I, 13) > teamRecords!(54, 0) Then
            teamRecords!(54, 0) = V8(I, 13) + V8(1 - I, 13)
            teamRecords!(54, 1) = V8(I, 13)
            teamRecords!(54, 2) = V8(1 - I, 13)
            teamRecDesc$(54, 0) = gameTeams$(I)
            teamRecDesc$(54, 1) = VERSUS$(I)
            teamRecDesc$(54, 2) = gameTeams$(1 - I)
            teamRecDesc$(54, 3) = Date$
        End If

        If QR(I, 1, 13) > teamRecords!(55, 0) Then
            teamRecords!(55, 0) = QR(I, 1, 13)
            teamRecords!(55, 1) = 1
            teamRecords!(55, 2) = 0
            teamRecDesc$(55, 0) = gameTeams$(I)
            teamRecDesc$(55, 1) = VERSUS$(I)
            teamRecDesc$(55, 2) = gameTeams$(1 - I)
            teamRecDesc$(55, 3) = Date$
        End If

        If QR(I, 2, 13) > teamRecords!(55, 0) Then
            teamRecords!(55, 0) = QR(I, 2, 13)
            teamRecords!(55, 1) = 2
            teamRecords!(55, 2) = 0
            teamRecDesc$(55, 0) = gameTeams$(I)
            teamRecDesc$(55, 1) = VERSUS$(I)
            teamRecDesc$(55, 2) = gameTeams$(1 - I)
            teamRecDesc$(55, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS NONE MADE
        If V8(I, 14) > teamRecords!(57, 0) Then
            teamRecords!(57, 0) = V8(I, 14)
            teamRecords!(57, 1) = 0
            teamRecords!(57, 2) = 0
            teamRecDesc$(57, 0) = gameTeams$(I)
            teamRecDesc$(57, 1) = VERSUS$(I)
            teamRecDesc$(57, 2) = gameTeams$(1 - I)
            teamRecDesc$(57, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS NONE MADE
        If V8(I, 14) + V8(1 - I, 14) > teamRecords!(58, 0) Then
            teamRecords!(58, 0) = V8(I, 14) + V8(1 - I, 14)
            teamRecords!(58, 1) = V8(I, 14)
            teamRecords!(58, 2) = V8(1 - I, 14)
            teamRecDesc$(58, 0) = gameTeams$(I)
            teamRecDesc$(58, 1) = VERSUS$(I)
            teamRecDesc$(58, 2) = gameTeams$(1 - I)
            teamRecDesc$(58, 3) = Date$
        End If

        If QR(I, 1, 14) > teamRecords!(59, 0) Then
            teamRecords!(59, 0) = QR(I, 1, 14)
            teamRecords!(59, 1) = 1
            teamRecords!(59, 2) = 0
            teamRecDesc$(59, 0) = gameTeams$(I)
            teamRecDesc$(59, 1) = VERSUS$(I)
            teamRecDesc$(59, 2) = gameTeams$(1 - I)
            teamRecDesc$(59, 3) = Date$
        End If

        If QR(I, 2, 14) > teamRecords!(59, 0) Then
            teamRecords!(59, 0) = QR(I, 2, 14)
            teamRecords!(59, 1) = 2
            teamRecords!(59, 2) = 0
            teamRecDesc$(59, 0) = gameTeams$(I)
            teamRecDesc$(59, 1) = VERSUS$(I)
            teamRecDesc$(59, 2) = gameTeams$(1 - I)
            teamRecDesc$(59, 3) = Date$
        End If

        ' FREE THROW ATTEMPTS NO MISS
        If V8(I, 2) = V8(I, 3) And V8(I, 2) > teamRecords!(60, 0) Then
            teamRecords!(60, 0) = V8(I, 2)
            teamRecords!(60, 1) = 0
            teamRecords!(60, 2) = 0
            teamRecDesc$(60, 0) = gameTeams$(I)
            teamRecDesc$(60, 1) = VERSUS$(I)
            teamRecDesc$(60, 2) = gameTeams$(1 - I)
            teamRecDesc$(60, 3) = Date$
        End If

        ' FREE THROW ATTEMPTS LOW %
        If V8(I, 3) <> 0 Then

            If V8(I, 2) / V8(I, 3) < teamRecords!(61, 0) Then
                teamRecords!(61, 0) = V8(I, 2) / V8(I, 3)
                teamRecords!(61, 1) = V8(I, 2)
                teamRecords!(61, 2) = V8(I, 3)
                teamRecDesc$(61, 0) = gameTeams$(I)
                teamRecDesc$(61, 1) = VERSUS$(I)
                teamRecDesc$(61, 2) = gameTeams$(1 - I)
                teamRecDesc$(61, 3) = Date$
            End If

        End If

        ' FREE THROW ATTEMPTS COMBINED HIGH %
        If V8(I, 3) + V8(1 - I, 3) <> 0 Then

            If (V8(I, 2) + V8(1 - I, 2)) / (V8(I, 3) + V8(1 - I, 3)) > teamRecords!(62, 0) Then
                teamRecords!(62, 0) = (V8(I, 2) + V8(1 - I, 2)) / (V8(I, 3) + V8(1 - I, 3))
                teamRecords!(62, 1) = V8(I, 2) / V8(I, 3)
                teamRecords!(62, 2) = V8(1 - I, 2) / (V8(1 - I, 3) + 0.01)
                teamRecDesc$(62, 0) = gameTeams$(I)
                teamRecDesc$(62, 1) = VERSUS$(I)
                teamRecDesc$(62, 2) = gameTeams$(1 - I)
                teamRecDesc$(62, 3) = Date$
            End If

            ' FREE THROW ATTEMPTS COMBINED LOW %
            If (V8(I, 2) + V8(1 - I, 2)) / (V8(I, 3) + V8(1 - I, 3)) < teamRecords!(63, 0) Then
                teamRecords!(63, 0) = (V8(I, 2) + V8(1 - I, 2)) / (V8(I, 3) + V8(1 - I, 3))
                teamRecords!(63, 1) = V8(I, 2) / V8(I, 3)
                teamRecords!(63, 2) = V8(1 - I, 2) / (V8(1 - I, 3) + 0.01)
                teamRecDesc$(63, 0) = gameTeams$(I)
                teamRecDesc$(63, 1) = VERSUS$(I)
                teamRecDesc$(63, 2) = gameTeams$(1 - I)
                teamRecDesc$(63, 3) = Date$
            End If
        End If

        ' TEAM FREE THROWS MADE
        104 If V8(I, 2) > teamRecords!(64, 0) Then
            teamRecords!(64, 0) = V8(I, 2)
            teamRecords!(64, 1) = 0
            teamRecords!(64, 2) = 0
            teamRecDesc$(64, 0) = gameTeams$(I)
            teamRecDesc$(64, 1) = VERSUS$(I)
            teamRecDesc$(64, 2) = gameTeams$(1 - I)
            teamRecDesc$(64, 3) = Date$
        End If

        If V8(I, 2) < teamRecords!(65, 0) Then
            teamRecords!(65, 0) = V8(I, 2)
            teamRecords!(65, 1) = 0
            teamRecords!(65, 2) = 0
            teamRecDesc$(65, 0) = gameTeams$(I)
            teamRecDesc$(65, 1) = VERSUS$(I)
            teamRecDesc$(65, 2) = gameTeams$(1 - I)
            teamRecDesc$(65, 3) = Date$
        End If

        If V8(I, 2) + V8(1 - I, 2) > teamRecords!(66, 0) Then
            teamRecords!(66, 0) = V8(I, 2) + V8(1 - I, 2)
            teamRecords!(66, 1) = V8(I, 2)
            teamRecords!(66, 2) = V8(1 - I, 2)
            teamRecDesc$(66, 0) = gameTeams$(I)
            teamRecDesc$(66, 1) = VERSUS$(I)
            teamRecDesc$(66, 2) = gameTeams$(1 - I)
            teamRecDesc$(66, 3) = Date$
        End If

        If V8(I, 2) + V8(1 - I, 2) < teamRecords!(67, 0) Then
            teamRecords!(67, 0) = V8(I, 2) + V8(1 - I, 2)
            teamRecords!(67, 1) = V8(I, 2)
            teamRecords!(67, 2) = V8(1 - I, 2)
            teamRecDesc$(67, 0) = gameTeams$(I)
            teamRecDesc$(67, 1) = VERSUS$(I)
            teamRecDesc$(67, 2) = gameTeams$(1 - I)
            teamRecDesc$(67, 3) = Date$
        End If

        If QR(I, 1, 2) > teamRecords!(68, 0) Then
            teamRecords!(68, 0) = QR(I, 1, 2)
            teamRecords!(68, 1) = 1
            teamRecords!(68, 2) = 0
            teamRecDesc$(68, 0) = gameTeams$(I)
            teamRecDesc$(68, 1) = VERSUS$(I)
            teamRecDesc$(68, 2) = gameTeams$(1 - I)
            teamRecDesc$(68, 3) = Date$
        End If

        If QR(I, 2, 2) > teamRecords!(68, 0) Then
            teamRecords!(68, 0) = QR(I, 2, 2)
            teamRecords!(68, 1) = 2
            teamRecords!(68, 2) = 0
            teamRecDesc$(68, 0) = gameTeams$(I)
            teamRecDesc$(68, 1) = VERSUS$(I)
            teamRecDesc$(68, 2) = gameTeams$(1 - I)
            teamRecDesc$(68, 3) = Date$
        End If

        ' TEAM FREE THROWS ATTEMPTED
        If V8(I, 3) > teamRecords!(72, 0) Then
            teamRecords!(72, 0) = V8(I, 3)
            teamRecords!(72, 1) = 0
            teamRecords!(72, 2) = 0
            teamRecDesc$(72, 0) = gameTeams$(I)
            teamRecDesc$(72, 1) = VERSUS$(I)
            teamRecDesc$(72, 2) = gameTeams$(1 - I)
            teamRecDesc$(72, 3) = Date$
        End If

        If V8(I, 3) < teamRecords!(73, 0) Then
            teamRecords!(73, 0) = V8(I, 3)
            teamRecords!(73, 1) = 0
            teamRecords!(73, 2) = 0
            teamRecDesc$(73, 0) = gameTeams$(I)
            teamRecDesc$(73, 1) = VERSUS$(I)
            teamRecDesc$(73, 2) = gameTeams$(1 - I)
            teamRecDesc$(73, 3) = Date$
        End If

        If V8(I, 3) + V8(1 - I, 3) > teamRecords!(74, 0) Then
            teamRecords!(74, 0) = V8(I, 3) + V8(1 - I, 3)
            teamRecords!(74, 1) = V8(I, 3)
            teamRecords!(74, 2) = V8(1 - I, 3)
            teamRecDesc$(74, 0) = gameTeams$(I)
            teamRecDesc$(74, 1) = VERSUS$(I)
            teamRecDesc$(74, 2) = gameTeams$(1 - I)
            teamRecDesc$(74, 3) = Date$
        End If

        If V8(I, 3) + V8(1 - I, 3) < teamRecords!(75, 0) Then
            teamRecords!(75, 0) = V8(I, 3) + V8(1 - I, 3)
            teamRecords!(75, 1) = V8(I, 3)
            teamRecords!(75, 2) = V8(1 - I, 3)
            teamRecDesc$(75, 0) = gameTeams$(I)
            teamRecDesc$(75, 1) = VERSUS$(I)
            teamRecDesc$(75, 2) = gameTeams$(1 - I)
            teamRecDesc$(75, 3) = Date$
        End If

        If QR(I, 1, 3) > teamRecords!(76, 0) Then
            teamRecords!(76, 0) = QR(I, 1, 3)
            teamRecords!(76, 1) = 1
            teamRecords!(76, 2) = 0
            teamRecDesc$(76, 0) = gameTeams$(I)
            teamRecDesc$(76, 1) = VERSUS$(I)
            teamRecDesc$(76, 2) = gameTeams$(1 - I)
            teamRecDesc$(76, 3) = Date$
        End If

        If QR(I, 2, 3) > teamRecords!(76, 0) Then
            teamRecords!(76, 0) = QR(I, 2, 3)
            teamRecords!(76, 1) = 2
            teamRecords!(76, 2) = 0
            teamRecDesc$(76, 0) = gameTeams$(I)
            teamRecDesc$(76, 1) = VERSUS$(I)
            teamRecDesc$(76, 2) = gameTeams$(1 - I)
            teamRecDesc$(76, 3) = Date$
        End If

        If QR(I, 1, 3) + QR(1 - I, 1, 3) > teamRecords!(77, 0) Then
            teamRecords!(77, 0) = QR(I, 1, 3) + QR(1 - I, 1, 3)
            teamRecords!(77, 1) = 1
            teamRecords!(77, 2) = QR(I, 1, 3)
            teamRecDesc$(77, 0) = gameTeams$(I)
            teamRecDesc$(77, 1) = VERSUS$(I)
            teamRecDesc$(77, 2) = gameTeams$(1 - I)
            teamRecDesc$(77, 3) = Date$
        End If

        If QR(I, 2, 3) + QR(1 - I, 2, 3) > teamRecords!(77, 0) Then
            teamRecords!(77, 0) = QR(I, 2, 3) + QR(1 - I, 2, 3)
            teamRecords!(77, 1) = 2
            teamRecords!(77, 2) = QR(I, 2, 3)
            teamRecDesc$(77, 0) = gameTeams$(I)
            teamRecDesc$(77, 1) = VERSUS$(I)
            teamRecDesc$(77, 2) = gameTeams$(1 - I)
            teamRecDesc$(77, 3) = Date$
        End If

        If totRebounds(I) > teamRecords!(80, 0) Then
            teamRecords!(80, 0) = totRebounds(I)
            teamRecords!(80, 1) = 0
            teamRecords!(80, 2) = 0
            teamRecDesc$(80, 0) = gameTeams$(I)
            teamRecDesc$(80, 1) = VERSUS$(I)
            teamRecDesc$(80, 2) = gameTeams$(1 - I)
            teamRecDesc$(80, 3) = Date$
        End If

        If totRebounds(I) < teamRecords!(81, 0) Then
            teamRecords!(81, 0) = totRebounds(I)
            teamRecords!(81, 1) = 0
            teamRecords!(81, 2) = 0
            teamRecDesc$(81, 0) = gameTeams$(I)
            teamRecDesc$(81, 1) = VERSUS$(I)
            teamRecDesc$(81, 2) = gameTeams$(1 - I)
            teamRecDesc$(81, 3) = Date$
        End If

        If totRebounds(I) + totRebounds(1 - I) > teamRecords!(82, 0) Then
            teamRecords!(82, 0) = totRebounds(I) + totRebounds(1 - I)
            teamRecords!(82, 1) = totRebounds(I)
            teamRecords!(82, 2) = totRebounds(1 - I)
            teamRecDesc$(82, 0) = gameTeams$(I)
            teamRecDesc$(82, 1) = VERSUS$(I)
            teamRecDesc$(82, 2) = gameTeams$(1 - I)
            teamRecDesc$(82, 3) = Date$
        End If

        If totRebounds(I) + totRebounds(1 - I) < teamRecords!(83, 0) Then
            teamRecords!(83, 0) = totRebounds(I) + totRebounds(1 - I)
            teamRecords!(83, 1) = totRebounds(I)
            teamRecords!(83, 2) = totRebounds(1 - I)
            teamRecDesc$(83, 0) = gameTeams$(I)
            teamRecDesc$(83, 1) = VERSUS$(I)
            teamRecDesc$(83, 2) = gameTeams$(1 - I)
            teamRecDesc$(83, 3) = Date$
        End If

        If QR(I, 1, 4) > teamRecords!(84, 0) Then
            teamRecords!(84, 0) = QR(I, 1, 4)
            teamRecords!(84, 1) = 1
            teamRecords!(84, 2) = 0
            teamRecDesc$(84, 0) = gameTeams$(I)
            teamRecDesc$(84, 1) = VERSUS$(I)
            teamRecDesc$(84, 2) = gameTeams$(1 - I)
            teamRecDesc$(84, 3) = Date$
        End If

        If QR(I, 2, 4) > teamRecords!(84, 0) Then
            teamRecords!(84, 0) = QR(I, 2, 4)
            teamRecords!(84, 1) = 2
            teamRecords!(84, 2) = 0
            teamRecDesc$(84, 0) = gameTeams$(I)
            teamRecDesc$(84, 1) = VERSUS$(I)
            teamRecDesc$(84, 2) = gameTeams$(1 - I)
            teamRecDesc$(84, 3) = Date$
        End If

        If V8(I, 11) > teamRecords!(86, 0) Then
            teamRecords!(86, 0) = V8(I, 11)
            teamRecords!(86, 1) = 0
            teamRecords!(86, 2) = 0
            teamRecDesc$(86, 0) = gameTeams$(I)
            teamRecDesc$(86, 1) = VERSUS$(I)
            teamRecDesc$(86, 2) = gameTeams$(1 - I)
            teamRecDesc$(86, 3) = Date$
        End If

        If V8(I, 11) < teamRecords!(87, 0) Then
            teamRecords!(87, 0) = V8(I, 11)
            teamRecords!(87, 1) = 0
            teamRecords!(87, 2) = 0
            teamRecDesc$(87, 0) = gameTeams$(I)
            teamRecDesc$(87, 1) = VERSUS$(I)
            teamRecDesc$(87, 2) = gameTeams$(1 - I)
            teamRecDesc$(87, 3) = Date$
        End If

        If V8(I, 11) + V8(1 - I, 11) > teamRecords!(88, 0) Then
            teamRecords!(88, 0) = V8(I, 11) + V8(1 - I, 11)
            teamRecords!(88, 1) = V8(I, 11)
            teamRecords!(88, 2) = V8(1 - I, 11)
            teamRecDesc$(88, 0) = gameTeams$(I)
            teamRecDesc$(88, 1) = VERSUS$(I)
            teamRecDesc$(88, 2) = gameTeams$(1 - I)
            teamRecDesc$(88, 3) = Date$
        End If

        If V8(I, 11) + V8(1 - I, 11) < teamRecords!(89, 0) Then
            teamRecords!(89, 0) = V8(I, 11) + V8(1 - I, 11)
            teamRecords!(89, 1) = V8(I, 11)
            teamRecords!(89, 2) = V8(1 - I, 11)
            teamRecDesc$(89, 0) = gameTeams$(I)
            teamRecDesc$(89, 1) = VERSUS$(I)
            teamRecDesc$(89, 2) = gameTeams$(1 - I)
            teamRecDesc$(89, 3) = Date$
        End If

        If QR(I, 1, 11) > teamRecords!(90, 0) Then
            teamRecords!(90, 0) = QR(I, 1, 11)
            teamRecords!(90, 1) = 1
            teamRecords!(90, 2) = 0
            teamRecDesc$(90, 0) = gameTeams$(I)
            teamRecDesc$(90, 1) = VERSUS$(I)
            teamRecDesc$(90, 2) = gameTeams$(1 - I)
            teamRecDesc$(90, 3) = Date$
        End If

        If QR(I, 2, 11) > teamRecords!(90, 0) Then
            teamRecords!(90, 0) = QR(I, 2, 11)
            teamRecords!(90, 1) = 2
            teamRecords!(90, 2) = 0
            teamRecDesc$(90, 0) = gameTeams$(I)
            teamRecDesc$(90, 1) = VERSUS$(I)
            teamRecDesc$(90, 2) = gameTeams$(1 - I)
            teamRecDesc$(90, 3) = Date$
        End If

        ' ASSISTS
        If V8(I, 7) > teamRecords!(96, 0) Then
            teamRecords!(96, 0) = V8(I, 7)
            teamRecords!(96, 1) = 0
            teamRecords!(96, 2) = 0
            teamRecDesc$(96, 0) = gameTeams$(I)
            teamRecDesc$(96, 1) = VERSUS$(I)
            teamRecDesc$(96, 2) = gameTeams$(1 - I)
            teamRecDesc$(96, 3) = Date$
        End If

        If V8(I, 7) < teamRecords!(97, 0) Then
            teamRecords!(97, 0) = V8(I, 7)
            teamRecords!(97, 1) = 0
            teamRecords!(97, 2) = 0
            teamRecDesc$(97, 0) = gameTeams$(I)
            teamRecDesc$(97, 1) = VERSUS$(I)
            teamRecDesc$(97, 2) = gameTeams$(1 - I)
            teamRecDesc$(97, 3) = Date$
        End If

        If V8(I, 7) + V8(1 - I, 7) > teamRecords!(98, 0) Then
            teamRecords!(98, 0) = V8(I, 7) + V8(1 - I, 7)
            teamRecords!(98, 1) = V8(I, 7)
            teamRecords!(98, 2) = V8(1 - I, 7)
            teamRecDesc$(98, 0) = gameTeams$(I)
            teamRecDesc$(98, 1) = VERSUS$(I)
            teamRecDesc$(98, 2) = gameTeams$(1 - I)
            teamRecDesc$(98, 3) = Date$
        End If

        If V8(I, 7) + V8(1 - I, 7) < teamRecords!(99, 0) Then
            teamRecords!(99, 0) = V8(I, 7) + V8(1 - I, 7)
            teamRecords!(99, 1) = V8(I, 7)
            teamRecords!(99, 2) = V8(1 - I, 7)
            teamRecDesc$(99, 0) = gameTeams$(I)
            teamRecDesc$(99, 1) = VERSUS$(I)
            teamRecDesc$(99, 2) = gameTeams$(1 - I)
            teamRecDesc$(99, 3) = Date$
        End If

        If QR(I, 1, 7) > teamRecords!(100, 0) Then
            teamRecords!(100, 0) = QR(I, 1, 7)
            teamRecords!(100, 1) = 1
            teamRecords!(100, 2) = 0
            teamRecDesc$(100, 0) = gameTeams$(I)
            teamRecDesc$(100, 1) = VERSUS$(I)
            teamRecDesc$(100, 2) = gameTeams$(1 - I)
            teamRecDesc$(100, 3) = Date$
        End If

        If QR(I, 2, 7) > teamRecords!(100, 0) Then
            teamRecords!(100, 0) = QR(I, 2, 7)
            teamRecords!(100, 1) = 2
            teamRecords!(100, 2) = 0
            teamRecDesc$(100, 0) = gameTeams$(I)
            teamRecDesc$(100, 1) = VERSUS$(I)
            teamRecDesc$(100, 2) = gameTeams$(1 - I)
            teamRecDesc$(100, 3) = Date$
        End If

        If QR(I, 2, 7) + QR(1 - I, 2, 7) > teamRecords!(101, 0) Then
            teamRecords!(101, 0) = QR(I, 2, 7) + QR(1 - I, 2, 7)
            teamRecords!(101, 1) = 2
            teamRecords!(101, 2) = 0
            teamRecDesc$(101, 0) = gameTeams$(I)
            teamRecDesc$(101, 1) = VERSUS$(I)
            teamRecDesc$(101, 2) = gameTeams$(1 - I)
            teamRecDesc$(101, 3) = Date$
        End If

        ' FOULS
        If V8(I, 6) > teamRecords!(104, 0) Then
            teamRecords!(104, 0) = V8(I, 6)
            teamRecords!(104, 1) = 0
            teamRecords!(104, 2) = 0
            teamRecDesc$(104, 0) = gameTeams$(I)
            teamRecDesc$(104, 1) = VERSUS$(I)
            teamRecDesc$(104, 2) = gameTeams$(1 - I)
            teamRecDesc$(104, 3) = Date$
        End If

        If V8(I, 6) < teamRecords!(105, 0) Then
            teamRecords!(105, 0) = V8(I, 6)
            teamRecords!(105, 1) = 0
            teamRecords!(105, 2) = 0
            teamRecDesc$(105, 0) = gameTeams$(I)
            teamRecDesc$(105, 1) = VERSUS$(I)
            teamRecDesc$(105, 2) = gameTeams$(1 - I)
            teamRecDesc$(105, 3) = Date$
        End If

        If V8(I, 6) + V8(1 - I, 6) > teamRecords!(106, 0) Then
            teamRecords!(106, 0) = V8(I, 6) + V8(1 - I, 6)
            teamRecords!(106, 1) = V8(I, 6)
            teamRecords!(106, 2) = V8(1 - I, 6)
            teamRecDesc$(106, 0) = gameTeams$(I)
            teamRecDesc$(106, 1) = VERSUS$(I)
            teamRecDesc$(106, 2) = gameTeams$(1 - I)
            teamRecDesc$(106, 3) = Date$
        End If

        If V8(I, 6) + V8(1 - I, 6) < teamRecords!(107, 0) Then
            teamRecords!(107, 0) = V8(I, 6) + V8(1 - I, 6)
            teamRecords!(107, 1) = V8(I, 6)
            teamRecords!(107, 2) = V8(1 - I, 6)
            teamRecDesc$(107, 0) = gameTeams$(I)
            teamRecDesc$(107, 1) = VERSUS$(I)
            teamRecDesc$(107, 2) = gameTeams$(1 - I)
            teamRecDesc$(107, 3) = Date$
        End If

        If QR(I, 1, 6) > teamRecords!(108, 0) Then
            teamRecords!(108, 0) = QR(I, 1, 6)
            teamRecords!(108, 1) = 1
            teamRecords!(108, 2) = 0
            teamRecDesc$(108, 0) = gameTeams$(I)
            teamRecDesc$(108, 1) = VERSUS$(I)
            teamRecDesc$(108, 2) = gameTeams$(1 - I)
            teamRecDesc$(108, 3) = Date$
        End If

        If QR(I, 2, 6) > teamRecords!(108, 0) Then
            teamRecords!(108, 0) = QR(I, 2, 6)
            teamRecords!(108, 1) = 2
            teamRecords!(108, 2) = 0
            teamRecDesc$(108, 0) = gameTeams$(I)
            teamRecDesc$(108, 1) = VERSUS$(I)
            teamRecDesc$(108, 2) = gameTeams$(1 - I)
            teamRecDesc$(108, 3) = Date$
        End If

        If QR(I, 1, 6) + QR(1 - I, 1, 6) > teamRecords!(109, 0) Then
            teamRecords!(109, 0) = QR(I, 1, 6) + QR(1 - I, 1, 6)
            teamRecords!(109, 1) = 1
            teamRecords!(109, 2) = QR(I, 1, 6)
            teamRecDesc$(109, 0) = gameTeams$(I)
            teamRecDesc$(109, 1) = VERSUS$(I)
            teamRecDesc$(109, 2) = gameTeams$(1 - I)
            teamRecDesc$(109, 3) = Date$
        End If

        If QR(I, 2, 6) + QR(1 - I, 2, 6) > teamRecords!(109, 0) Then
            teamRecords!(109, 0) = QR(I, 2, 6) + QR(1 - I, 2, 6)
            teamRecords!(109, 1) = 2
            teamRecords!(109, 2) = QR(I, 2, 6)
            teamRecDesc$(109, 0) = gameTeams$(I)
            teamRecDesc$(109, 1) = VERSUS$(I)
            teamRecDesc$(109, 2) = gameTeams$(1 - I)
            teamRecDesc$(109, 3) = Date$
        End If

        ' STEALS
        If V8(I, 9) > teamRecords!(112, 0) Then
            teamRecords!(112, 0) = V8(I, 9)
            teamRecords!(112, 1) = 0
            teamRecords!(112, 2) = 0
            teamRecDesc$(112, 0) = gameTeams$(I)
            teamRecDesc$(112, 1) = VERSUS$(I)
            teamRecDesc$(112, 2) = gameTeams$(1 - I)
            teamRecDesc$(112, 3) = Date$
        End If

        If V8(I, 9) + V8(1 - I, 9) > teamRecords!(114, 0) Then
            teamRecords!(114, 0) = V8(I, 9) + V8(1 - I, 9)
            teamRecords!(114, 1) = V8(I, 9)
            teamRecords!(114, 2) = V8(1 - I, 9)
            teamRecDesc$(114, 0) = gameTeams$(I)
            teamRecDesc$(114, 1) = VERSUS$(I)
            teamRecDesc$(114, 2) = gameTeams$(1 - I)
            teamRecDesc$(114, 3) = Date$
        End If

        If QR(I, 1, 9) > teamRecords!(116, 0) Then
            teamRecords!(116, 0) = QR(I, 1, 9)
            teamRecords!(116, 1) = 1
            teamRecords!(116, 2) = 0
            teamRecDesc$(116, 0) = gameTeams$(I)
            teamRecDesc$(116, 1) = VERSUS$(I)
            teamRecDesc$(116, 2) = gameTeams$(1 - I)
            teamRecDesc$(116, 3) = Date$
        End If

        If QR(I, 2, 9) > teamRecords!(116, 0) Then
            teamRecords!(116, 0) = QR(I, 2, 9)
            teamRecords!(116, 1) = 2
            teamRecords!(116, 2) = 0
            teamRecDesc$(116, 0) = gameTeams$(I)
            teamRecDesc$(116, 1) = VERSUS$(I)
            teamRecDesc$(116, 2) = gameTeams$(1 - I)
            teamRecDesc$(116, 3) = Date$
        End If

        ' BLOCKS
        If V8(I, 8) > teamRecords!(118, 0) Then
            teamRecords!(118, 0) = V8(I, 8)
            teamRecords!(118, 1) = 0
            teamRecords!(118, 2) = 0
            teamRecDesc$(118, 0) = gameTeams$(I)
            teamRecDesc$(118, 1) = VERSUS$(I)
            teamRecDesc$(118, 2) = gameTeams$(1 - I)
            teamRecDesc$(118, 3) = Date$
        End If

        If V8(I, 8) + V8(1 - I, 8) > teamRecords!(119, 0) Then
            teamRecords!(119, 0) = V8(I, 8) + V8(1 - I, 8)
            teamRecords!(119, 1) = V8(I, 8)
            teamRecords!(119, 2) = V8(1 - I, 8)
            teamRecDesc$(119, 0) = gameTeams$(I)
            teamRecDesc$(119, 1) = VERSUS$(I)
            teamRecDesc$(119, 2) = gameTeams$(1 - I)
            teamRecDesc$(119, 3) = Date$
        End If

        If QR(I, 1, 8) > teamRecords!(121, 0) Then
            teamRecords!(121, 0) = QR(I, 1, 8)
            teamRecords!(121, 1) = 1
            teamRecords!(121, 2) = 0
            teamRecDesc$(121, 0) = gameTeams$(I)
            teamRecDesc$(121, 1) = VERSUS$(I)
            teamRecDesc$(121, 2) = gameTeams$(1 - I)
            teamRecDesc$(121, 3) = Date$
        End If

        If QR(I, 2, 8) > teamRecords!(121, 0) Then
            teamRecords!(121, 0) = QR(I, 2, 8)
            teamRecords!(121, 1) = 1
            teamRecords!(121, 2) = 0
            teamRecDesc$(121, 0) = gameTeams$(I)
            teamRecDesc$(121, 1) = VERSUS$(I)
            teamRecDesc$(121, 2) = gameTeams$(1 - I)
            teamRecDesc$(121, 3) = Date$
        End If

        ' TURNOVERS
        If V8(I, 12) > teamRecords!(122, 0) Then
            teamRecords!(122, 0) = V8(I, 12)
            teamRecords!(122, 1) = 0
            teamRecords!(122, 2) = 0
            teamRecDesc$(122, 0) = gameTeams$(I)
            teamRecDesc$(122, 1) = VERSUS$(I)
            teamRecDesc$(122, 2) = gameTeams$(1 - I)
            teamRecDesc$(122, 3) = Date$
        End If

        If V8(I, 12) < teamRecords!(123, 0) Then
            teamRecords!(123, 0) = V8(I, 12)
            teamRecords!(123, 1) = 0
            teamRecords!(123, 2) = 0
            teamRecDesc$(123, 0) = gameTeams$(I)
            teamRecDesc$(123, 1) = VERSUS$(I)
            teamRecDesc$(123, 2) = gameTeams$(1 - I)
            teamRecDesc$(123, 3) = Date$
        End If

        If V8(I, 12) + V8(1 - I, 12) > teamRecords!(124, 0) Then
            teamRecords!(124, 0) = V8(I, 12) + V8(1 - I, 12)
            teamRecords!(124, 1) = V8(I, 12)
            teamRecords!(124, 2) = V8(1 - I, 12)
            teamRecDesc$(124, 0) = gameTeams$(I)
            teamRecDesc$(124, 1) = VERSUS$(I)
            teamRecDesc$(124, 2) = gameTeams$(1 - I)
            teamRecDesc$(124, 3) = Date$
        End If

        If V8(I, 12) + V8(1 - I, 12) < teamRecords!(125, 0) Then
            teamRecords!(125, 0) = V8(I, 12) + V8(1 - I, 12)
            teamRecords!(125, 1) = V8(I, 12)
            teamRecords!(125, 2) = V8(1 - I, 12)
            teamRecDesc$(125, 0) = gameTeams$(I)
            teamRecDesc$(125, 1) = VERSUS$(I)
            teamRecDesc$(125, 2) = gameTeams$(1 - I)
            teamRecDesc$(125, 3) = Date$
        End If

    Next

    Open diskPaths$(1) + "RECORDS.GAM" For Output As #2

    For I = 0 To 48

        For I1 = 0 To 2
            Write #2, indRecords!(I, I1)
        Next

        For I1 = 0 To 4
            Write #2, indRecDesc$(I, I1)
        Next

    Next

    For I = 0 To 125

        For I1 = 0 To 2
            Write #2, teamRecords!(I, I1)
        Next

        For I1 = 0 To 3
            Write #2, teamRecDesc$(I, I1)
        Next

    Next

    Close #2

End Sub


'----------------------------------------
'          GetTeamYear Function
'----------------------------------------
'This subroutine determines the 4-digit
'team year from the team loaded by the
'file. It uses the 2 digit year to make
'assumptions. (00 to 24 are year 2000.)
'The first NCAA  tournament was held in
'1939, so it's safe to guess teams from
'00 to 38 are year 2000.
Function GetTeamYear% (yearFromName$)

    teamYear% = Val(RTrim$(yearFromName$))

    'For 2-digit years
    If teamYear% >= 0 And teamYear% < 39 Then
        GetTeamYear% = 2000 + teamYear%
    Else
        GetTeamYear% = 1900 + teamYear%
    End If

End Function


'----------------------------------------
'          goodPlayer% Function
'----------------------------------------
'This function returns whether the player
'for the specified team / roster index
'is valid for the game. This is based on
'several factors such as roster status,
'player name, or how much the player
'has fouled
Function goodPlayer% (teamIdx, playerIdx)

    playerFouls = plyrOff_GAME!(teamIdx, playerIdx, 12)

    If (rosterStatus(teamIdx, playerIdx) <> 0 Or players$(teamIdx, playerIdx) = "XXX" Or (playerFouls >= foulsToDQ) Or (currHalf = 1 And playerFouls >= foulsToDQ - 2) Or (playerFouls >= foulsToDQ - 1 And currHalf = 2 And gameClock! > 720)) Then

        goodPlayer% = 0

    Else

        goodPlayer% = 1

    End If

End Function


'----------------------------------------
'         playerFatigue% Function
'----------------------------------------
'This functions calculates the player's
'current fatigue level based on their
'existing contribution to the game
Function playerFatigue% (team, handler)

    'CX = (FGA + Off Reb + Def RB + Fouls) / (Game Minutes)
    playerCX! = plyrOff_GAME!(team, handler, 3)

    If sClockVal = 0 Then playerCX! = playerCX! * 1.2

    'Fatigue = CX - Curr FGA - Curr Rebounds - Curr Fouls
    playerFatigue% = playerCX! - plyrOff_GAME!(team, handler, 7) - plyrOff_GAME!(team, handler, 11) - plyrOff_GAME!(team, handler, 12)

End Function


'----------------------------------------
'        GetTeamGender$ Function
'----------------------------------------
'This function attempts to determine if
'the team has been loaded from a disk of
'women's teams only". It is only able to
'do this by checking if the last letter
'(ie, 2024W) or first letter (ie, WNBA)
' is a "W", as has been common to do in
'the modding community.
'This obviously would not be useful for
'leagues that would choose a W arbitrarily
'(ie, for a "World" league, etc.)
Function GetTeamGender$ (diskID$)

    If Right$(diskID$, 1) = "W" Or Left$(diskID$, 1) = "W" Then
        GetTeamGender$ = "SHE"
    Else
        GetTeamGender$ = "HE"
    End If

End Function

