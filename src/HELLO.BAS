Const GAME_VERSION$ = "4.25"
Const MAX_CONFERENCES = 40
Const TEAMS_PER_CONFERENCE = 30
Const TEAMS_PER_DIV = 15
Const DIVS_PER_CONFERENCE = 2
Const MAX_TEAMS = 500
Const MAX_GAMES = 1500
Const MAX_FILES = 1024 '1048576

'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'
'$INCLUDE: 'PadRight.BI'
'$INCLUDE: 'MainRoutines.BI'
'$INCLUDE: 'TITLE.BI'

DefInt A-Z

'Dim menuChoices$(1 To 9)

'----------------------------------------
' Used across more than one source file
'----------------------------------------
Dim A1$, C$, S$
Dim ATT, CK, DA, JX, LS, SP, TC, WN
Dim BO%, V1%, V2%

Dim Shared diskPaths$(3), Q$(0 To 500)
Dim Shared teamNames$(MAX_TEAMS)
Dim Shared teamIndex%(MAX_TEAMS)

Dim Shared DirCount%

'File size - does this really need shared?
Dim Shared Y As Long

Dim A$(0 To 15)
Dim L%(0 To 4), T1%(20)
Dim A(0 To 14, 0 To 13), DF(13), RS(13, 13), X(13, 13)
Dim W0(15), W1(15)
Dim Z!(15), Z1!(15)

Dim homeTeam$(MAX_GAMES), visitingTeam$(MAX_GAMES)
Dim homeScore%(MAX_GAMES), visitingScore%(MAX_GAMES)
Dim T%(0 To 9)


'----------------------------------------
'Shared across ADDTOURN.BAS,
'   BRACKET.BAS routines
'----------------------------------------
Dim R

Dim N%(17, 17, 3) 'REGION, SEED NUMBER, TEAM#/MODE OF PLAY (0,1)
Dim NN%(17) 'Number of Teams Per Region (up to 16 Regions)

Dim slotName$(17, 17, 3) 'Team Names for Each Seed Slot
Dim regionNames$(1 To 16) 'Region Names
Dim yearNumber$(MAX_TEAMS, 3)

Dim tourneyDefaults%(14) 'TOURNAMENT DEFAULTS
Dim consolationSetting%(1) '0-CONSOLATION IN REGIONS, 1-CONSOLATION IN CHAMPIONSHIP

'----------------------------------------
'Shared across CARVIEW.BAS routines
'----------------------------------------
Dim AR$(62)
Dim ARS(15, 62, 15)
Dim W0S(15, 62), W1S(15, 62)
Dim careerT%(0 To 34)

'----------------------------------------
'Shared across CNGRAT.BAS routines
'----------------------------------------
'14 positions, 0 to 13 array index
Dim cngratA$(0 To 13), P$(0 To 13)

'----------------------------------------
'Shared across COMPARE.BAS routines
'----------------------------------------
Dim compareA(15, 14)
Dim A1(13, 6), X1(13, 6)
Dim HP%(100), compareL%(6), OP%(100), compareT%(34)
Dim B$(14), HP$(100), OP$(100)

'----------------------------------------
'Shared across COMPNAT.BAS routines
'----------------------------------------
Dim compnatH$(100), compnatN$(0 To 14)
Dim LD(1 To 250, 0 To 2)
Dim NDL!(1 To 40, 1 To 20), NLD(1 To 250, 1 To 2), NOL!(1 To 40, 1 To 20)
Dim S(0 To 14, 0 To 26)

Dim DT!(21), OT!(21)

Dim NDL$(40), NZ0$(1 To 250), NZ1$(30), NZ3$(1 To 250)
Dim Z0$(1 To 250), Z3$(1 To 250)

'----------------------------------------
'Shared across DRAFT.BAS routines
'----------------------------------------
Dim draftA$(0 To 1, 0 To 13), draftP$(1, 13), draftT$(1), draftYN$(1)
Dim A1$(1), C$(1), S$(1)

Dim draftA(1, 13, 6), ATT(1), CK(1)
Dim DA(1), draftDF(1, 13)
Dim draftRS(1, 13, 13), SP(1), draftX(1, 13, 6)

Dim draftL%(1, 4), draftT%(1, 9), draftT1%(1, 20), TR%(1, 13, 12)
Dim V1%(1), V2%(1)


'----------------------------------------
'Shared across EXPANCON.BAS routines
'----------------------------------------
Dim JB, PP, TS, TMR, TTS

Dim AL$(600), TP$(31), TP1$(39)
Dim expanTT$(40, 15), TT1$(40, 15)
Dim TMA$(600)

Dim TMM$(600), TPP$(600)

'These should all be single
Dim statsAL!(1 To 600, 0 To 14), GM1!(1 To 40), GMA!(600)
Dim TT!(40, 15), TT1!(40, 15)
ReDim TYP!(600)
Dim W0L!(600), W1L!(600)


Data GAMES,MINUTES,FIELD GOALS MADE,FIELD GOAL ATTEMPS,F.G. PCT. (3.65 FGM/GAME),THREE POINTS MADE,THREE POINT ATTEMPTS,3FG (.30 3FGM/GAME),FREE THROWS MADE,FREE THROWS ATTEMPTED,F.T. PCT. (1.52 FTM/GAME)
Data POINTS,SCORING AVERAGE,HI-GAME,OFFENSIVE REBOUNDS,DEFENSIVE REBOUNDS,TOTAL REBOUNDS,AVERAGE REBOUNDS PER GAME,FOULS,ASSISTS,BLOCKED SHOTS,STEALS,TURNOVERS
Data POINTS/48 MINUTES,REBOUNDS/48 MINUTES,FOULS/48 MINUTES,ASSISTS/48 MINUTES,BLOCKS/48 MINUTES,STEALS/48 MINUTES,TURNOVERS/48 MINUTES,MINUTES/GAME,GO TO MAIN MENU
For I = 0 To 31: Read TP$(I): Next

Data FIELD GOAL ATTEMPTS,FIELD GOALS MADE,F.G. PCT.,THREE POINT ATTEMPTS,THREE POINTS MADE,THREE POINT PERCENTAGE,FREE THROWS ATTEMPTED,FREE THROWS MADE,FREE THROW PERCENTAGE,OFFENSIVE REBOUNDS
Data DEFENSIVE REBOUNDS,TOTAL REBOUNDS,REBOUNDS PER GAME,FOULS,ASSISTS,BLOCKS,STEALS,TURNOVERS,POINTS PER GAME
Data OPP. FIELD GOAL ATTEMPTS,OPP. FIELD GOALS MADE,OPP. FIELD GOAL PERCENTAGE,OPP. THREE POINTERS ATTEMPTED,OPP. THREE POINTERS MADE,OPP. THREE POINT PERCENTAGE,OPP. FREE THROWS ATTEMPTED,OPP. FREE THROWS MADE,OPP. FREE THROW PERCENTAGE
Data OPP. OFFENSIVE REBOUNDS,OPP. DEFENSIVE REBOUNDS,OPP. TOTAL REBOUNDS,OPP. REBOUNDS PER GAME,OPP. FOULS,OPP. ASSISTS,OPP. BLOCKS,OPP. STEALS,OPP. TURNOVERS,OPP. POINTS PER GAME,RETURN TO MAIN MENU
For I = 0 To 38: Read TP1$(I): Next

'----------------------------------------
'Shared across HD2HD.BAS routines
'----------------------------------------
Dim AL(50), AW(50), HL(50), HW(50)
Dim R1(50), R2(50), R3(50), R4(50)
Dim TAW(40), THW(40), THL(40), TAL(40)
Dim TR1(40), TR2(40), TR3(40), TR4(40)

'----------------------------------------
'Shared across LOOKY/MERGE.BAS routines
'----------------------------------------
Dim HL%, HW%, NL%, NW%, VL%, VW%
Dim L%, W%

Dim mergeT%(100)
Dim O%(100), P%(100)
Dim CP$(25), O$(100)

Dim lookyA(0 To 14, 24)

Data NAME,G,MIN,MIN/G,FGM,FGA,FG PCT,3FGM,3FGA,3FG PCT,FTM,FTA,FT PCT
Data NAME,PTS,PTG/G,HI,OFF REB,DEF REB,TOT REB,REB/G,PF,AST,BLK,STL,TO
For I = 0 To 25: Read CP$(I): Next

'----------------------------------------
'Shared across RECORDS.BAS routines
'----------------------------------------
Dim REC(50, 2), TREC(125, 2)
Dim recordsA$(5), RC$(50, 4), TRC$(125, 3)

'----------------------------------------
'Shared across RECCON.BAS routines
'----------------------------------------
Dim BRC(32), TRC!(21), TRC1!(21)
Dim BRC$(32, 1), recconTRC$(21), recconTRC1$(21)

Dim TT$(20), TB$(25)

Data GAMES PLAYED,FIELD GOALS ATTEMPTED,FIELD GOALS MADE,FIELD GOAL %,3-FIELD GOALS ATTEMPTED,3-FIELD GOALS MADE,3-FIELD GOAL %
Data FREE THROWS ATTEMPTED,FREE THROWS MADE,FREE THROW %,MINUTES PLAYED,POINTS,SCORING AVERAGE,HI-GAME,OFFENSIVE REBOUNDS
Data DEFENSIVE REBOUNDS,TOTAL REBOUNDS,REBOUNDS PER GAME,PERSONAL FOULS,STEALS,ASSISTS,BLOCKS,TURNOVERS,MINUTES PER GAME,MVP VOTING
For I = 1 To 25: Read TB$(I): Next

Data FIELD GOALS ATTEMPTED,FIELD GOALS MADE,FIELD GOAL %,3-FIELD GOALS ATTEMPTED,3-FIELD GOALS MADE,3-FIELD GOAL %
Data FREE THROWS ATTEMPTED,FREE THROWS MADE,FREE THROW %,OFFENSIVE REBOUNDS,DEFENSIVE REBOUNDS,TOTAL REBOUNDS,REBOUNDS PER GAME
Data PERSONAL FOULS,STEALS,ASSISTS,BLOCKS,TURNOVERS,POINTS,POINTS PER GAME

For I = 1 To 20: Read TT$(I): Next

'----------------------------------------
'Shared across SCHEDULE.BAS routines
'----------------------------------------
Dim BS%, NS%
Dim N$
Dim NG%(MAX_GAMES, 18) 'number of Games

Dim scheduleAP%(1), scheduleT%(9), scheduleZ1%(1 To 30)
Dim scheduleH$(1 To 20), scheduleV$(1 To 20)
Dim scheduleYN$(MAX_GAMES, 1)
Dim Z1$(1 To 30), Z2$(1 To 30)

'----------------------------------------
'Shared across SEECON/SEENAT.BAS routines
'----------------------------------------
Dim DL$(60), OL$(60), PR$(1200)
Dim DL!(60, 20), OL!(60, 20) ', seeconP(60)
Dim O1(30), O2(30), O3(30), O4(30), O5(30), O6(30)
Dim seenatZ!(260), seenatZ1!(260)
Dim seenatZ1$(260), seenatZ2$(260), seenatZ3$(260)
Dim PT#(1200, 5)
Dim T$(30)


'----------------------------------------
'               MAIN CODE
'----------------------------------------
_Title "Courtside College Basketball - Menu"
$ExeIcon:'./lhg_ccb.ico'
_Icon
_AllowFullScreen

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
'       Main_Menu Subroutine
'----------------------------------------
Sub Main_Menu (menuSelection%)

    ReDim menuChoices$(1 To 9)

    Do

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
                Case 1: Run "COLHOOP.EXE"
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

    505 Color 15, 1
    FG = 0
    BG = 7
    For I = 0 To 1
        diskPaths$(I) = Left$(diskPaths$(I), Len(diskPaths$(I)) - 1)
    Next
    'Colr = OneColor%(FG, BG) 'pack FG and BG into a single byte
    Cls: Locate , 16: Print "SPECIFY PATHS FOR YOUR TEAMS AND COMPILED STATS": Print

    510 Locate , 31: Print "PATHNAME EXAMPLES:": Print

    515 Locate , 31: Print "A:"

    520 Locate , 31: Print "C:\COLBB\TEAMS"
    Locate 7, 1: QPrint "Path for Teams ", 14, 1, -1
    Locate , 16: QPrint PadRight$(diskPaths$(0), 30), FG, BG, -1
    Locate 9, 1: QPrint "Path for Stats ", 14, 1, -1
    Locate , 16: QPrint PadRight$(diskPaths$(1), 30), FG, BG, -1

    GetDiv1:
    Locate 7, 16 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 30 'specify field parameters
    Call TextIn(diskPaths$(0), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data
    diskPaths$(0) = _Trim$(diskPaths$(0))
    GetDiv2:
    Locate 9, 16 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 30 'specify field parameters
    Call TextIn(diskPaths$(1), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data
    diskPaths$(1) = _Trim$(diskPaths$(1))

    Done:
    Locate 23, 1
    Print "Is this okay? (Yes/No) ";
    X$ = ""
    Call YesNoB(X$, XCode, FG, BG)
    Locate , 1
    ClearEOL 14, 1
    If XCode = 1 GoTo GetDiv2
    If X$ = "N" GoTo GetDiv1
    If X$ <> "Y" GoTo Done 'insist on either Y or N or up-arrow

    Open "DEV.BAS" For Output As #1
    For R = 0 To 2: diskPaths$(R) = diskPaths$(R) + "\": Next R
    Print #1, diskPaths$(0)
    Print #1, diskPaths$(1)
    Close #1

End Sub


'----------------------------------------
'       TeamMgmt_Menu Subroutine
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
'       ReadStats Subroutine
'----------------------------------------
'...explanation...
Sub Replay_Menu

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
'       Leaders_Menu Subroutine
'----------------------------------------
'...explanation...
Sub Leaders_Menu

    Do
        Cls

        800 Call TITLE
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
        Locate 17
        Locate , 10: Print "COLLEGE CONFERENCE IS USED TO DEFINE THE TEAMS IN A CONFERENCE"
        Print
        Locate , 10: Print "NATIONAL ORGANIZATION ALLOWS YOU TO SELECT MULTIPLE CONFERENCES"
        Locate , 10: Print "TO TRACK AND UPDATE TO GENERATE A NATIONAL POLL AND LEADERS."

        _MouseShow "DEFAULT"
        MAMenu menuChoices$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
        _MouseHide

        If ExitCode% = 27 Then Exit Sub

        Color , 0

        Select Case Selection%
            Case 1: Call CREATCON
            Case 2: Call COMPCON
            Case 3: Call SEECON
            Case 4: Call HD2HDCON
            Case 5: Call RECCON
            Case 6: Call EXPANCON
            Case 7: Call BLANK
            Case 8: Call CREATNAT
            Case 9: Call COMPNAT
            Case 10: Call SEENAT
            Case 11: Call RECCON
            Case 12: Call BLANKORG
            Case 13 Or 14: Cls
        End Select

    Loop Until Selection% >= 13 Or ExitCode% = 27

    If Selection% = 14 Then
        End
        System
    End If

End Sub


'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
'$INCLUDE: 'PadRight.BM'
'$INCLUDE: 'TITLE.BM'
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
'$INCLUDE: 'EXPANCON.BAS'
'$INCLUDE: 'HD2HDCON.BAS'
'$INCLUDE: 'LOOKY.BAS'
'$INCLUDE: 'MERGE.BAS'
'$INCLUDE: 'RECCON.BAS'
'$INCLUDE: 'RECORDS.BAS'
'$INCLUDE: 'SCHEDULE.BAS'
'$INCLUDE: 'SEECON.BAS'
'$INCLUDE: 'SEENAT.BAS'
'$INCLUDE: 'SINPUT.BAS'
'$INCLUDE: 'TOURNEY.BAS'

