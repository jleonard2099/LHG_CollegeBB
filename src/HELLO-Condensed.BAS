$Debug
$Color:32

Const GAME_VERSION$ = "5.00"
Const MAX_SCHEDULE_GAMES = 6000
Const MAX_TEAMS = 1000
Const MAX_FILES = 10240
'optimium = 1048576 (but slows code)
Const MAX_CONFERENCES = 40
Const TEAMS_PER_DIV = 15
Const DIVS_PER_CONFERENCE = 2
Const TEAMS_PER_CONFERENCE = 30

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

Data 1,4,2,5,15,29,22,36,7,9,7,12
For I = 0 To 2
    For I1 = 1 To 4
        Read defAdjVsPass(I, I1)
    Next
Next

Data -7,-10,-4,-10,70,100,20,100,-1,1,-2,2,15,22,8,29,5,7,3,9
For I = 0 To 4
    'For I = 0 To 8
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

Open "FOLDERS.CFG" For Input As #3

For I = 0 To 3
    Input #3, diskPaths$(I)
Next

Close #3

Selection% = 1

'Screen 12

Do

    Call Main_Menu(Selection%)

Loop Until Selection% = 8

System


'----------------------------------------

Errhandler:
Open "errlog" For Append As #10
Print #10, "Error #"; Err; "on program file line"; _ErrorLine
If _FileExists("GAME82") Then
    Print #10, "Schedule game # "; Using "###"; schedGame(0) + schedGame(2)
    Print #10, gameTeams$(0) + " vs. " + gameTeams$(1)
    Print #10, defStrat(D)
End If
Print #10, findFile$
Print #10,
Close #10
Resume Next ' moves program to code following the error.

'----------------------------------------
'       Main_Menu Subroutine
'----------------------------------------
Sub Main_Menu (menuSelection%)

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

        Selection% = 1
        Count% = 8
        Start% = 1
        ExitCode% = 8
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor%, FileBGColor%)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), menuSelection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then
            Select Case menuSelection%

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

    Loop Until menuSelection% = 8 Or ExitCode% = 27

    If ExitCode% = 27 Then menuSelection% = 8

End Sub


'----------------------------------------
'        TeamMgmt_Menu Subroutine
'----------------------------------------
'...explanation...
Sub TeamMgt_Menu ()

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
        Selection% = 1
        Count% = 7
        Start% = 1
        ExitCode% = 7
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor%, FileBGColor%)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case Selection%

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
                    Call ConvertTeamData

            End Select

        Else

            Selection% = 7

        End If

    Loop Until Selection% = 7 Or ExitCode% = 27

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

        ReDim menuChoices$(1 To 5)

        menuChoices$(1) = "VIEW / PRINT COMPILED STATS     "
        menuChoices$(2) = "TEAM CAREER OPTIONS             "
        menuChoices$(3) = "COMPARE REPLAY STATS TO ACTUAL  "
        'menuChoices$(4) = "MODIFY REPLAY STATS             "
        menuChoices$(4) = "MERGE SEPARATE STATISTICAL FILES"
        menuChoices$(5) = "RETURN TO MAIN MENU             "
        Selection% = 1
        Count% = 5
        Start% = 1
        ExitCode% = 5
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor%, FileBGColor%)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case Selection%

                Case 1:
                    Call LOOKY

                Case 2:
                    Call CAREER

                Case 3:
                    Call COMPARE

                Case 4:
                    Call MERGE

                Case 5:
                    '
                    
            End Select

        End If

    Loop Until Selection% = 5 Or ExitCode% = 27

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

        menuChoices$(1) = "MANAGE CONFERENCES        "
        menuChoices$(2) = "UPDATE STATS / STANDINGS  "
        menuChoices$(3) = "ACCESS CONFERENCE REPORTS "
        menuChoices$(4) = "HEAD-TO-HEAD RESULTS      "
        menuChoices$(5) = "CONFERENCE RECORDS        "
        menuChoices$(6) = "EXPANDED LEADERS          "
        menuChoices$(7) = "DELETE CONFERENCE STATS   "
        menuChoices$(8) = "MANAGE ORGANIZATIONS      "
        menuChoices$(9) = "UPDATE POLL / STATS       "
        menuChoices$(10) = "ACCESS NAT'L. REPORTS     "
        menuChoices$(11) = "NATIONAL RECORDS          "
        menuChoices$(12) = "DELETE NATIONAL STATS     "
        menuChoices$(13) = "RETURN TO MAIN MENU       "
        menuChoices$(14) = "EXIT PROGRAM              "
        Selection% = 1
        Count% = 14
        Start% = 1
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Rows% = 7
        Columns% = 2
        Spacing% = 3
        Row% = 9
        Column% = 12

        Call box0(7, 11, 16, 38, 2, 15, 1)
        Call box0(7, 40, 16, 67, 2, 15, 1)

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
        MAMenu menuChoices$(), Selection%, Start%, Count%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Rows%, Columns%, Spacing%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 Then

            Color , 0

            Select Case Selection%
                Case 1: Call CREATCON
                Case 2: Call CompileCollege
                Case 3: Call SEECON
                Case 4: Call HD2HDCON
                Case 5: Call SeasonRecords
                Case 6: Call NEWLDR
                Case 7: Call BLANKCON
                Case 8: Call CREATENATL
                Case 9: Call CompileNatlOrg
                Case 10: Call SEENATL
                Case 11: Call SeasonRecords
                Case 12: Call BLANKORG

            End Select

        End If 'Done checking for ESC

    Loop Until Selection% >= 13 Or ExitCode% = 27

    If Selection% = 14 Then
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
'$Include: 'TITLE.bm'
'$Include: 'ADDTOURN.BAS'
'$Include: 'ALIGN.BAS'
'$Include: 'BLANKCON.BAS'
'$Include: 'BLANKORG.BAS'
'$Include: 'BRACKET.BAS'
'$Include: 'CAREER.BAS'
'$Include: 'CARVIEW.BAS'
'$Include: 'CNGTM.BAS'
'$Include: 'COMPARE.BAS'
'$Include: 'COMPCON.BAS'
'$Include: 'COMPNATL.BAS'
'$Include: 'Convert.BAS'
'$Include: 'CREATCON.BAS'
'$Include: 'CREATNATL.BAS'
'$Include: 'DRAFT.BAS'
'$Include: 'HD2HDCON.BAS'
'$Include: 'LOOKY.BAS'
'$Include: 'MERGE.BAS'
'$Include: 'NEWLDR.BAS'
'$Include: 'REC.BAS'
'$Include: 'RECORDS.BAS'
'$Include: 'SCHEDULE.BAS'
'$Include: 'SEECON.BAS'
'$Include: 'SEENATL.BAS'
'$Include: 'SINPUT.BAS'
'$Include: 'TINPUT.BAS'
'$Include: 'TOURNEY.BAS'

' For Game
'$Include: 'COLHOOP.BAS'
'$Include: 'GameRoutines.bm'
