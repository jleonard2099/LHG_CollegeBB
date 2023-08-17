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

'Const NUM_STATRECORDS = 162
Const SCHEDULE_SIZE_BYTES = 48

DefInt A-Z

'$INCLUDE: 'FileTeamSelection.bi'
'$INCLUDE: 'KeyInput.bi'
'$INCLUDE: 'QPProEqu.bi'
'$INCLUDE: 'PadRight.bi'
'INCLUDE: 'Variables.bi'



'----------------------------------------
' Used in more than one source file
'----------------------------------------
Dim opSysType$

Dim Shared diskPaths$(0 To 3), Q$(0 To 500)
Dim Shared teamNames$(MAX_TEAMS)
Dim Shared teamIndex%(MAX_TEAMS)

' *** Reading Stat Data ***
' -------------------------
'-- what else can fit here?
Dim statsW0!(15), statsW1!(15)
Dim statsZ!(15), statsZ1!(15)

' *** Schedule Data ***
' -------------------------
Dim homeTeam$(MAX_GAMES), visitingTeam$(MAX_GAMES)
Dim homeScores(MAX_GAMES), visitorScores(MAX_GAMES)

' *** Game Options ***
' -------------------------
Dim Shared DT$, TM$

Dim coachMode$(0 To 1), foulShotOpt$(0 To 2)
Dim location$(0 To 2), MO$(0 To 3), modeAbbrev$(0 To 3)
Dim sclockOpt$(0 To 2), threeFtOpt$(0 To 1), threePtOpt$(0 To 1)
Dim yesNo$(0 To 1)

' *** Miscellaneous Use ***
' -------------------------
Dim finalUpdate%
Dim replayLosses%, replayWins%

'----------------------------------------
' Used in ALIGN / MERGE routines
'----------------------------------------
Dim Ycurr%, Yroad%

Dim alignAR$(15), AN$(15)
Dim mergeH$(100), HRP$(100)
Dim mergeO$(100), ORP$(100), gameSite$(100), SITERP$(100)

Dim APRD%(100, 1), ARD(14, 14), CRD(100), CRDRD(100)
Dim mergeA(14, 14), mergeAP%(100, 1)
Dim mergeO%(100), mergeT%(100)
Dim ORD%(100), TRD%(100)

Dim AN!(15, 14), AR!(15, 14)
Dim W0N!(15), WN1!(15)
Dim W0R!(15), WR1!(15)
Dim W0RD!(15), W1RD!(15)
Dim ZR!(15), ZR1!(15)
Dim ZRD!(15), ZRD1!(15)


'----------------------------------------
' Used in CAREER / LEADER routines
'----------------------------------------
Dim AR$(62)
Dim ARS!(15, 62, 15)
Dim W0S!(15, 62), W1S!(15, 62)

Dim JB, tStats, ttStats

Dim AL$(600), expanTT$(40, 15)
Dim TMA$(600), TP$(31), TP1$(39), TT1$(40, 15)

Dim TMM$(600), TPP$(600)

'These should all be single
Dim GM!(1 To 40), GM1!(1 To 40), GMA!(600)
Dim statsAL!(1 To 600, 0 To 14)
Dim TT!(40, 15), TT1!(40, 15)
Dim TYP!(600)
Dim W0L!(600), W1L!(600)


'----------------------------------------
' Used in COMPARE routines
'----------------------------------------
Dim HP%(100), compareL%(6), OP%(100)

Dim compareA!(15, 14)
Dim A1!(13, 6), X1!(13, 6)

Dim compareB$(14), H1$(40), HP$(100), OP$(100)


'----------------------------------------
' Used in COMPILE routines
'----------------------------------------
Dim confWins, confLosses
Dim fullWins, fullLosses

Dim DT!(21), OT!(21)
Dim LD!(1 To 250, 0 To 2)
Dim NDL!(1 To 40, 1 To 20), NLD!(1 To 250, 1 To 2), NOL!(1 To 40, 1 To 20)
Dim compS!(0 To 14, 0 To 26)


Dim compH$(100), compN$(0 To 14)
Dim CP$(25), compO$(100)
Dim NDL$(40), NZ0$(1 To 250), NZ1$(30), NZ3$(1 To 250)
Dim PT$(1 To 1200), Z0$(1 To 250), Z3$(1 To 250)


'----------------------------------------
' Used in CREATE routines
'----------------------------------------
Dim creatZ1$(MAX_CONFERENCES)


'----------------------------------------
' Used in DRAFT routines
'----------------------------------------
Dim CK!(1)

Dim dFGPctAdj_DRAFT!(1)

Dim lgRat_DRAFT(1, 4), miscRat_DRAFT(1, 13, 12)
Dim teamRat_DRAFT(1, 9), teamStats_DRAFT(1, 20)

Dim plyrDef_DRAFT!(1, 13), plyrOff_DRAFT!(1, 13, 6)
Dim plyrRat_DRAFT!(1, 13, 6), plyrStat_DRAFT!(1, 13, 13)
Dim stamina_DRAFT!(1)

Dim att_DRAFT&(1)

Dim coaches_DRAFT$(1), mascots_DRAFT$(1), stadiums_DRAFT$(1)
Dim plyrNames_DRAFT$(0 To 1, 0 To 13), posn_DRAFT$(1, 13)
Dim teamYears$(1), tmName_DRAFT$(1)


'----------------------------------------
'   Used in Head-To-Head routines
'----------------------------------------
Dim AL!(50), AW!(50), HL!(50), HW!(50)
Dim R1!(50), R2!(50), hd2hdR3!(50), R4!(50)
Dim TAW!(40), THW!(40), THL!(40), TAL!(40)
Dim TR1!(40), TR2!(40), TR3!(40), TR4!(40)


'----------------------------------------
' Used in RECORDS routines
'----------------------------------------
'---> these should be Single
Dim REC!(50, 2), TREC!(125, 2)

Dim recordsA$(5), RC$(50, 4), TRC$(125, 3)

Dim BRC!(32), TRC!(1 To 21), TRC1!(1 To 21)
Dim BRC$(32, 1), recconTRC$(1 To 21), recconTRC1$(1 To 21)

Dim TT$(20), recconTB$(25)


'----------------------------------------
' Used in STAT / INPUT routines
'----------------------------------------
Dim HL%, HW%, NL%, NW%, VL%, VW%
Dim L%, W%

Dim lookyA!(0 To 14, 24)


'----------------------------------------
' Used in SEExxx routines
'----------------------------------------
Dim DL$(60), OL$(60), NOL$(1 To 60), PR$(1200)
Dim DL!(60, 20), OL!(60, 20) ', seeconP(60)
Dim O1!(30), O2!(30), O3!(30), O4!(30), O5!(30), O6!(30)
Dim seenatZ!(260), seenatZ1!(260)
Dim seenatZ1$(260), seenatZ2$(260), seenatZ3$(260)
Dim PT#(1200, 5)
Dim T$(30)


'----------------------------------------
' Used in SCHEDULE routines
'----------------------------------------
Dim BS%, NS%
Dim N$

Dim scheduleNG%(MAX_GAMES, 18) 'number of Games

Dim scheduleAP%(1), scheduleZ1%(1 To 30)

Dim scheduleH$(1 To 20), scheduleV$(1 To 20)
Dim scheduleYN$(MAX_GAMES, 1)

Dim Z1$(1 To 30), Z2$(1 To 30)


'----------------------------------------
' Used in ADDTOURN / BRACKET routines
'----------------------------------------
Dim TC!

'           REGION, SEED NUMBER, TEAM#/MODE OF PLAY (0,1)
Dim tourneyN%(17, 17, 3)

'Number of Teams Per Region (up to 16 Regions)
Dim NN%(17)

'Team Names for Each Seed Slot
Dim slotName$(17, 17, 0 To 3)

'Region Names
Dim regionNames$(1 To 17)

Dim yearNumber$(MAX_TEAMS, 3)

'TOURNAMENT DEFAULTS
Dim tourneyDefaults%(14)

'0-CONSOLATION IN REGIONS, 1-CONSOLATION IN CHAMPIONSHIP
Dim consolationSetting%(1)


'----------------------------------------
' Used in Game Routines
'----------------------------------------
Dim scheduleFile$

Dim Shared AP, B, C1, DY, CT, D, endGame, endAllGames, FO, F3, G9
Dim Shared H, halfTime, I, J, IN, M5, MJ, N, nbrLines, NTMS
Dim Shared P, P9, Q, S2, S9, sClockVal, shotClock
Dim Shared tickerIdx, TMT, TOU, U5, XM, XS

Dim Shared BO%, BU%, coachOpt, DK%, F3S%, FB%, FT%, J8%, LC%, N7%, NF%
Dim Shared PA%, PASS%, PB%, PT%, PZ%, ST%, TB%, TF%, X3%, X0%, X1%, XX%

Dim Shared F!, R0!

Dim Shared gameClock!, pbpDelay!, timeElapsed!

Dim Shared actualAttendance&

Dim Shared A1$, B1$, C1$, D1$, E1$, F1$, G1$, H1$, J$, PB$
Dim Shared tourneyFile$, U$, VT$, VT1$, W$, W1$, YN$

'-- FA%() I believe is related to tracking player fatigue
Dim Shared AP%(2), APT%(100, 1), B%(1, 13), CZ%(1), DT%(1), E%(13), FA%(1, 13), FY%(2)
Dim Shared G9%(1), HF%(1, 6), HT%(100), L%(1, 4), N%(16, 16, 0 To 4), NG%(18), NG1%(18)
Dim Shared O%(100), OF%(1), PC%(1), PR%(1, 1), ST%(32), SX%(32, 1, 14)
Dim Shared T2%(1, 20), TF%(1), TM%(1, 13), TP%(1), TR%(1, 9), def3FG_Adj(1)
Dim Shared VG%(8), VH%(8), W%(1, 13, 1)

Dim Shared B1(0 To 1, 0 To 4)
Dim Shared C(1, 13, 6), D1(1), D2(1), D8(4, 3), DP(2, 4)
Dim Shared DF(1, 13)
Dim Shared F1(13), G4(13), G5(13), P2(1), P4(14), P5(14), P7(1)
Dim Shared QQ(1, 8, 13, 14), QR(1, 7, 14), RB(9), RS(1, 13, 13)
Dim Shared S1(1), S3(13), T0(1), T1(1), V9(14)
Dim Shared W2(1, 13), W3(1, 13), Z5(1), Z6(1), timePlayed(1, 13)

Dim Shared F5%(0 To 1, 0 To 8)

Dim Shared A!(0 To 1, 0 To 13, 0 To 24)
Dim Shared R3!(1), S!(1, 9), W0!(1, 13), W1!(1, 13)

'CRD stores attendance for stat files
'  I think this is because files are
'  operated on linearly, so 100 records
'  is the most supported?
Dim Shared avgAttendance&(1), CRD&(100)

Dim Shared AA$(1, 13), B$(0 To 1), D$(15), DB$(14)
Dim Shared gameMascot$(0 To 1), gamearenaName$(0 To 1), gameCoach$(0 To 1)
Dim Shared H$(100), HO$(100), LC$(2), N$(16, 16, 0 To 4), O$(9), OB$(9)
Dim Shared PB$(1), PS$(4), R$(14), SITE$(100), SX$(32, 2)
Dim Shared PO$(1, 13)
Dim Shared X$(3), Y$(1), YN$(5), YN1$(3)

Dim Shared Z!(15), Z1!(15), Z2!(13, 13)



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

coachMode$(0) = "COACH AND SELECT SHOTS           "
coachMode$(1) = "COACH ONLY AND LET COMPUTER SHOOT"

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
    Color 14, 1
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
                Case 1: Call TINPUT
                Case 2: Call CNGTM(1)
                Case 3: Call CNGTM(2)
                Case 4: Call CNGTM(3)
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
                Case 1: Call CREATCOL
                Case 2: Call CompileCollege
                Case 3: Call SEECON
                Case 4: Call HD2HDCON
                Case 5: Call SeasonRecords
                Case 6: Call NEWLDR
                Case 7: Call BLANK
                Case 8: Call CREATNAT
                Case 9: Call CompileNatlOrg
                Case 10: Call SEENATL
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


'$INCLUDE: 'FileTeamSelection.bm'
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
'$INCLUDE: 'CNGTM.BAS'
'$INCLUDE: 'COMPARE.BAS'
'$INCLUDE: 'COMPCOLL.BAS'
'$INCLUDE: 'COMPNATL.BAS'
'$INCLUDE: 'CREATCOL.BAS'
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
'$INCLUDE: 'SEENATL.BAS'
'$INCLUDE: 'SINPUT.BAS'
'$INCLUDE: 'TINPUT.BAS'
'$INCLUDE: 'TOURNEY.BAS'

' For Game
'INCLUDE: 'COLHOOP.BAS'



'----------------------------------------
'           COLHOOP Subroutine
'----------------------------------------
'This subroutine serves as the main "source"
'for the action in the game. It steps the
'user through choosing game parameters and
'teams, as well as determining players to
'disable, etc.
Sub COLHOOP ()

    _Title "Courtside College Basketball v" + GAME_VERSION$ + " - HOOPS!"

    'Open "pbplog" For Output As #7

    Shared scheduleFile$

    Shared tickerStart

    Shared coachMode$(), foulShotOpt$(), location$()
    Shared MO$(), sclockOpt$(), yesNo$()

    Color 7, 0
    Cls

    Randomize Timer

    Call InitVar

    '----------------------------------------
    '         DETERMINE GAME OPTIONS
    '----------------------------------------

    If Not _FileExists("DEV.BAS") Then
        Open "DEV.BAS" For Output As #1
        For I = 0 To 1
            Print #1, _CWD$ + "\"
        Next
        Close 1
    End If

    Open "DEV.BAS" For Input As #1
    For I = 0 To 1
        Input #1, diskPaths$(I)
    Next
    Close 1

    If Not _FileExists("DEFAULT") Then
        Open "DEFAULT" For Output As #1
        For I = 1 To 11:
            Print #1, 0
        Next
        Close 1
    End If

    Open "DEFAULT" For Input As #1
    Input #1, U5
    Input #1, X3%
    Input #1, sClockVal
    Input #1, N
    Input #1, NF%
    Input #1, pbpDelay!
    Input #1, coachOpt
    Input #1, TB%
    Input #1, TF%
    Input #1, N7%
    Input #1, PB%
    Close 1

    If _FileExists("TICKER") Then

        Open "TICKER" For Input As #2

        For I = 1 To 32
            For I1 = 0 To 1
                For I2 = 1 To 14
                    Input #2, SX%(I, I1, I2)
                Next
            Next
        Next

        For I = 1 To 32
            For I1 = 0 To 1
                Input #2, SX$(I, I1)
            Next
        Next

        For I = 1 To 32:
            Input #2, ST%(I)
        Next

        Close 2

        TMT = 1
    Else
        TMT = 0
    End If

    'Load game in-progress, if there is one
    If _FileExists("GAME82") Then

        Open "GAME82" For Input As #2

        Input #2, scheduleFile$

        For I = 0 To 2:
            Input #2, AP%(I)
        Next

        Close 2

        Open diskPaths$(0) + scheduleFile$ For Random As #1 Len = 48

        For X = 0 To 18
            Field #1, X * 2 As X$, 2 As Q$(X + 1), 48 - 2 - 2 * X As X$
        Next

        Field #1, 38 As X$, 2 As Q$(22), 2 As Q$(23), 3 As Q$(20), 3 As Q$(21)

        Get #1, AP%(0) + AP%(2)

        For X = 0 To 18:
            NG%(X) = CVI(Q$(X + 1))
        Next

        For X = 0 To 1:
            YN$(X) = Q$(20 + X)
        Next

        VS% = CVI(Q$(22))
        HS% = CVI(Q$(23))

        Close 1

        AP = 1

    End If

    If _FileExists("TOURNEY") Then

        Open "TOURNEY" For Input As #2

        Input #2, tourneyFile$

        For I = 0 To 2:
            Input #2, AP%(I)
        Next

        Close 2

        'If start game = end game,
        'we're all done!
        If AP%(2) = AP%(1) Then
            Kill "TOURNEY"
            'System
            'End
            Exit Sub

        Else

            Open diskPaths$(1) + tourneyFile$ + ".TOU" As #1 Len = 90

            For X = 0 To 14
                Field #1, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
            Next

            Field #1, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
            Field #1, 64 As X$, 3 As Q$(20), 3 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
            Field #1, 84 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)

            Get #1, AP%(0) + AP%(2)

            VS% = CVI(Q$(16))
            HS% = CVI(Q$(17))
            VT$ = Q$(18)
            W$ = Q$(19)

            For X = 0 To 14:
                NG%(X) = CVI(Q$(X + 1))
            Next

            For X = 0 To 3:
                YN$(X) = Q$(20 + X)
            Next

            For X = 1 To 3:
                NG%(X + 14) = CVI(Q$(X + 23))
            Next

            Close 1

            AP = 1
            TOU = 1

        End If

    End If

    If AP = 1 Then Call AUTOSETUP

    'AUTOSETUP will reset TOU = 0
    'in many circumstances
    If AP <> 1 And TOU <> 1 Then

        ' USER SELECTION OF OPTIONS
        'N = 1 is neutral site
        N = 1

        Do
            Color 15, 0
            Cls

            Color 2
            Print ; Tab(29); "SELECT OPENING OPTIONS"
            Print
            Locate 2, 2: Color 14
            Print "(1) ";: Color 15: Print MO$(U5)
            Locate 3, 2: Color 14
            Print "(2) ";: Color 15: Print "VISITING TEAM: ";: Color TR%(0, 0), TR%(0, 1): Print B$(0)
            Color , 0
            Locate 4, 2: Color 14
            Print "(3) ";: Color 15: Print "HOME TEAM: ";: Color TR%(1, 0), TR%(1, 1): Print B$(1)
            Color , 0
            Locate 5, 2: Color 14
            Print "(4) ";: Color 15: Print "3-POINT SHOT: "; yesNo$(X3%)
            Locate 6, 2: Color 14
            Print "(5) ";: Color 15: Print sclockOpt$(sClockVal)
            Locate 7, 2: Color 14
            Print "(6) ";: Color 15: Print "LOCATION: "; location$(N)
            Locate 8, 2: Color 14
            Print "(7) ";: Color 15: Print Using "DISPLAY PAUSE IN SECONDS: ##.#"; pbpDelay!
            Locate 9, 2: Color 14
            Print "(8) ";: Color 15: Print "START GAME"
            Locate 10, 2: Color 14
            Print "(9) ";: Color 15: Print "FOULS TO DISQUALIFY: "; NF%
            Locate 11, 2: Color 14
            Print "(0) ";: Color 15: Print "SAVE SETTINGS"
            Locate 12, 2: Color 14
            Print "(A) ";: Color 15: Print "FREE THROWS ON 10TH FOUL: "; foulShotOpt$(TB%)
            Locate 13, 2: Color 14
            Print "(B) ";: Color 15: Print "HUMAN COACHES WILL: "; coachMode$(coachOpt);
            Locate 1, 1
            Locate 14, 2: Color 14
            Print "(C) ";: Color 15: Print "3 FT WHEN FOULED ON 3-PT ATT: "; yesNo$(TF%)
            Locate 15, 2: Color 14
            Print "(D) ";: Color 15: Print "1 FT ON 1ST 6 FLOOR FOULS : "; yesNo$(N7%)
            Locate 16, 2: Color 14
            Print "(E) ";: Color 15: Print "PLAY BY PLAY: "; PB$(PB%)

            Locate 17, 2: Print "NOTE:: 1 FT ON 1ST 6 FLOOR FOULS THROUGH 1972-73 SEASON"
            Locate , 2: Print "       45 SEC SHOT CLOCK WAS ADOPTED IN 1984-85, BUT NOT USED IN"
            Locate , 2: Print "       84-85 POST SEASON TOURNAMENTS"
            Locate , 2: Print "       3 PT SHOT WAS ADOPTED IN 1986-87"
            Locate , 2: Print "       2 FT INSTEAD OF 1+1 AFTER 10TH FOUL ADOPTED IN 1990-91"
            Locate , 2: Print "       3 FT WHEN FOULED ON A 3 PT ATT ADOPTED IN 1990-91"
            Locate , 2: Print "       35 SEC SHOT CLOCK WAS ADOPTED IN 1993-94"

            'Start opening options with:
            'Locate 3, 2:
            'Options wil go up to Locate 17, 2 before the below:

            'Locate 19, 2: Print "NOTE::"
            'Print "        1 FT ON 1ST 6 FLOOR FOULS THROUGH 1972-73 SEASON"
            'Print "        45 SEC SHOT CLOCK ADOPTED 1984-85, BUT NOT FOR POST-SEASON"
            'Print "        3 PT SHOT ADOPTED IN 1986-87"
            'Print "        2 FT INSTEAD OF 1+1 AFTER 10TH FOUL ADOPTED 1990-91"
            'Print "        3 FT WHEN FOULED ON A 3 PT ATT ADOPTED 1990-91"
            'Print "        35 SEC SHOT CLOCK IN 1993-94; 30 SEC SHOT CLOCK IN 2015-16";

            Do
                Color 7

                I$ = GetKeyPress$

                If I$ <> Chr$(27) Then
                    iVal = Val(I$)
                    If UCase$(I$) = "A" Then iVal = 11
                    If UCase$(I$) = "B" Then iVal = 12
                    If UCase$(I$) = "C" Then iVal = 13
                    If UCase$(I$) = "D" Then iVal = 14
                    If UCase$(I$) = "E" Then iVal = 15
                    If iVal = 0 Then iVal = 10
                End If

            Loop Until iVal >= 0 And iVal <= 15 Or I$ = Chr$(27) Or UCase$(I$) = "Q"

            If I$ <> Chr$(27) And UCase$(I$) <> "Q" Then

                Select Case iVal

                    Case 1:
                        U5 = U5 + 1
                        If U5 > 3 Then U5 = 0

                    Case 2:
                        P9 = 0
                        Call ChooseGameTeam(YN$, numberTeams, tmIdx, B$(P9), P9)
                        If tmIdx <> 0 Then
                            Color 15, 0
                            Call TEAMSETUP
                            Call DISABLEPLAYERS
                        End If

                    Case 3:
                        P9 = 1
                        Call ChooseGameTeam(YN$, numberTeams, tmIdx, B$(P9), P9)
                        If tmIdx <> 0 Then
                            Color 15, 0
                            Call TEAMSETUP
                            Call DISABLEPLAYERS
                        End If

                    Case 4:
                        X3% = 1 - X3%

                    Case 5:
                        sClockVal = sClockVal + 1
                        '-- for 30 second shot clock option
                        '-- If sClockVal > 3 Then sClockVal = 0
                        If sClockVal > 2 Then sClockVal = 0

                    Case 6:
                        N = N + 1
                        If N > 2 Then N = 1

                    Case 7:
                        Do
                            Locate 8, 32
                            Input pbpDelay!
                        Loop Until pbpDelay! >= 0

                    Case 8:
                        If B$(0) <> "" And B$(1) <> "" Then Exit Do

                    Case 9:
                        NF% = NF% + 1
                        If NF% > 6 Then NF% = 5

                    Case 10:
                        Open "DEFAULT" For Output As #1
                        Print #1, U5
                        Print #1, X3%
                        Print #1, sClockVal
                        Print #1, N
                        Print #1, NF%
                        Print #1, pbpDelay!
                        Print #1, coachOpt
                        Print #1, TB%
                        Print #1, TF%
                        Print #1, N7%
                        Print #1, PB%
                        Close 1

                    Case 11:
                        TB% = 1 - TB%

                    Case 12:
                        coachOpt = 1 - coachOpt

                    Case 13:
                        TF% = 1 - TF%

                    Case 14:
                        N7% = 1 - N7%

                    Case 15:
                        PB% = 1 - PB%

                End Select

            End If

        Loop Until (iVal = 8 And B$(0) <> "" And B$(1) <> "") Or I$ = Chr$(27)

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
                If L = 5 Then HR = Val(Left$(startTime$, 1)): MIN = Val(Mid$(startTime$, 3, 2))
                If L = 6 Then HR = Val(Left$(startTime$, 2)): MIN = Val(Mid$(startTime$, 4, 2))
            End If

            tickerMeridian$ = Right$(startTime$, 1)

            If UCase$(tickerMeridian$) = "P" And Left$(startTime$, 2) <> "12" Then HR = HR + 12

            TM = HR * 60 + MIN
            tickerStart = Int(TM / 30) - 18

            Call SETUPRATINGS

        Else

            'End
            'System
            Exit Sub

        End If

    End If

    FY%(2) = Int(((FY%(1) + FY%(0)) / 2) + .5)

    For I = 0 To 1
        For I1 = 0 To 13
            TM%(I, I1) = 1200
        Next
    Next

    'Determine attendance
    ' N = 2 is a non-neutral site
    If N = 2 Or avgAttendance&(1) = 0 Then
        actualAttendance& = Int(Rnd(1) * 2000) + 9000
    Else
        actualAttendance& = Int(Rnd(1) * 2000) + avgAttendance&(1) - 1000
    End If

    '----------------------------------------
    '    AUTO-DETERMINE OTHER OPTIONS
    '----------------------------------------

    ' Determine computer team
    Select Case U5

        Case 1:
            CT = 0
        Case 2:
            U5 = 1
            CT = 1
        Case 3:
            U5 = 2

    End Select

    Call LOADSTATS

    If AP = 1 Then N = NG%(2)

    Select Case sClockVal
        Case 1: shotClock = 45
        Case 2: shotClock = 35
        Case 3: shotClock = 30
    End Select

    S9 = (S1(0) + S1(1)) / 2

    For P9 = 0 To 1

        Call SETSTAMINA

        If U5 = 1 And P9 = CT Or U5 = 2 Then
            Call COMCOACH
        Else
            Call LINEUPS
            Call FOULSAFE
        End If

        If X3% <> 0 Then

            For I = 0 To 13

                M8 = 100 - A!(P9, I, 19)

                If M8 <> 0 Then
                    M9 = 100 / M8
                End If

                A!(P9, I, 16) = Int((A!(P9, I, 16) * M9) + .5)

            Next I

        End If

    Next P9

    Call CheckGameClock
    Call SCOREBOARD
    Call STEALADJ

    250 '
    P = Int(Rnd(1) * 2) '0 or 1

    If P = 0 Then
        D = 1
    Else
        D = 0
    End If

    PB$ = ""

    If P = 1 Then
        P = 0
        D = 1
        P9 = 0
        C1 = 2
    Else
        P = 1
        D = 0
        P9 = 1
        C1 = 2
    End If

    Call LASTSHOT
    Call CheckGameClock
    If gameClock! = 0 Then 170

    Call ResetShotClock

    If pbpDelay! <> 0 Then

        If (H = 1 Or H = 3) And PB% = 0 Then U$ = B$(P) + " GETS THE JUMP": PA% = D: Call PBP
        If (H = 1 Or H = 3) And PB% = 1 Then Call WINDEX: Print B$(P) + " GETS THE JUMP": PA% = D: Call DELAY

        If (H <> 1 And H <> 3 Or H > 3) And PB% = 0 Then P = PA%: D = 1 - P: PA% = 1 - PA%: U$ = B$(P) + " INBOUNDS THE BALL": Call PBP
        If (H <> 1 And H <> 3 Or H > 3) And PB% = 1 Then P = PA%: D = 1 - P: PA% = 1 - PA%: Call WINDEX: Print B$(P) + " INBOUNDS THE BALL": Call DELAY

    Else

        If (H = 1 Or H = 3) Then
            PA% = D
        Else
            P = PA%
            D = 1 - P
            PA% = 1 - PA%
        End If

    End If

    GoTo 300

    '----- NOODLE -----

    170 For I = 0 To 1
        DT%(I) = 0
        D2(I) = 0
        P7(I) = 0
    Next

    gameClock! = 0
    Call QUARTERTOTAL
    H = H + 1
    gameClock! = 1200
    XS = 1
    X = 0

    If H = 2 Then
        U$ = "END OF HALF"
        Select Case PB%
            Case 0:
                Color 7, 0
                Call WINDEX
                Print U$
            Case 1:
                If pbpDelay! > 0 Then Call WINDEX: Print U$
        End Select
    End If

    If H > 3 Then
        U$ = "END OF OVERTIME" + Str$(H - 3)
        Select Case PB%
            Case 0:
                Color 7, 0
                Print U$
            Case 1:
                If pbpDelay! > 0 Then Call WINDEX: Print U$
        End Select
    End If

    If H = 2 Then

        U$ = "HALFTIME!"

        Select Case PB%
            Case 0:
                Color 7, 0
                Call WINDEX
                Print U$
            Case 1:
                Call WINDEX
                Print U$
                Call DELAY
        End Select

        halfTime = 1
        Call BOXOPTIONS(0)
        Call TICKER
        halfTime = 0

        Call FOULHARD

    End If

    If H < 3 And U5 <> 0 Then
        If U5 = 1 Then P9 = CT: Call COMCOACH
        If U5 = 2 Then P9 = 0: Call COMCOACH: P9 = 1: Call COMCOACH
    End If

    If H < 3 Then
        P2(0) = 0
        P2(1) = 0

        Call CheckGameClock
        Call SCOREBOARD

        Print
        B = -1

        Call DEADBALLOPTIONS

        gameClock! = 1200

        Call UPDATESCREEN

        For I = 0 To 1
            For I1 = 0 To 13:
                TM%(I, I1) = 1200
                FA%(I, I1) = 0
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

    If H = 3 Then
        U$ = "END OF REGULATION"
        Select Case PB%
            Case 0:
                Color 7, 0
                Call WINDEX
                Print U$
            Case 1:
                If pbpDelay! > 0 Then Call WINDEX: Print U$
        End Select
    End If

    Locate 2, 31
    Print "END OF REGULATION"

    'If scores are tied, it's Overtime!
    If S!(0, 0) = S!(1, 0) Then

        U$ = "OVERTIME !!!"

        Select Case PB%
            Case 0:
                Color 7, 0
                Call WINDEX
                Print U$
            Case 1:
                Call WINDEX
                Print U$
                Call DELAY
        End Select

        T1(0) = T1(0) + 1
        T1(1) = T1(1) + 1
        gameClock! = 300

        Call UPDATESCREEN

        For I = 0 To 1
            For I1 = 0 To 13:
                TM%(I, I1) = 300
                FA%(I, I1) = 0
            Next
        Next

        B = -1

        Call DEADBALLOPTIONS
        Call SCOREBOARD

        GoTo 250

    End If

    If Not (AP = 1 And U5 = 2) Then
        I$ = GetKeyPress
    End If

    '--Start "EndOfGame" routine
    Call POSTGAME

    ' Prompt to play another game
    Cls

    If AP <> 1 Then

        'Using Home Team's color for the box
        Call TEAMBOX(1, 1)

        Do
            U$ = "PLAY ANOTHER GAME (YN)"
            Locate 7, (80 - Len(U$)) / 2
            Print U$

            Color 15, 0
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    End If

    If UCase$(I$) = "Y" Or (AP = 1 And endAllGames <> 1) Then
        Cls
        Print "PLEASE WAIT ..."
        Call COLHOOP
    End If

    '--End "EndOfGame" routine

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

    GoTo 170

    '----- NOODLE -----

    300 '

    If gameClock! <= 5 And T1(D) <= 0 And S!(P, 0) > S!(D, 0) Then
        timeElapsed! = gameClock!
        Call AdjustPlayTime
        gameClock! = 0
        Call CheckGameShotClock

        Call WINDEX
        Print B$(P); " RUNS THE CLOCK OUT!"
        Call DELAY
        Call DELAY

        GoTo 170

    End If

    '--- RESUME HERE
    If gameClock! <= 5 And (X3% = 1 And S!(D, 0) - S!(P, 0) <= 3 Or X3% = 0 And S!(D, 0) - S!(P, 0) <= 2) And S!(D, 0) - S!(P, 0) >= 0 And H >= 2 Then

        'Last 5 seconds!!!
        If U5 = 2 Or U5 = 1 And P = CT Then
            ' Computer handle last 5 seconds
            Call d100(RN)
            R0! = S!(P, 0) - S!(D, 0)
            If gameClock! >= 3 And gameClock! <= 5 And X3% = 0 And R0! < 3 Then I1% = 5
            If gameClock! >= 3 And gameClock! <= 5 And X3% = 1 And R0! = 3 Then I1% = 4
            If gameClock! >= 3 And gameClock! <= 5 And X3% = 1 And R0! = 2 And RN <= 50 Then I1% = 5
            If gameClock! >= 3 And gameClock! <= 5 And X3% = 1 And R0! = 2 And RN > 50 Then I1% = 4
            If gameClock! < 3 And X3% = 0 And R0! < 3 Then I1% = 0
            If gameClock! < 3 And X3% = 1 And R0! = 3 Then I1% = 1
            If gameClock! < 3 And X3% = 1 And R0! = 2 And RN <= 50 Then I1% = 0
            If gameClock! < 3 And X3% = 1 And R0! = 2 And RN > 50 Then I1% = 1

        Else

            Call LASTFIVEMENU

            280 I$ = GetKeyPress$
            I1% = Val(I$): If I1% < 0 Or I1% > 5 Then 280
            If I1% = 3 And T1(P9) <= 0 Then 280
            If (I1% = 1 Or I1% = 4) And X3% = 0 Then 280

            Color 15, 0
            Call SCOREBOARD

        End If

        B = Int(Rnd(1) * 5)

        Call d100(RN)

        If I1% = 0 And RN <= 50 Or I1% = 1 And RN <= 33 Then
            U$ = "LONG PASS DOWN COURT"
            Call PBP
            Call DELAY
            U$ = "IT'S INTERCEPTED...GAME OVER!!"
            Call PBP
            GoTo TIMEEXP
        End If

        If I1% = 2 Then
            I3% = Int(Rnd(1) * 45) + 45

            U$ = AA$(P, B1(P, B)) + " HAS THE BALL"
            Call PBP

            U$ = "HE PUTS IT UP FROM" + Str$(I3%) + " FEET"
            Call PBP
            Call DELAY

            If RN = 61 Then

                U$ = "GOOD!!"
                Call PBP

                S!(P, H) = S!(P, H) + 2
                S!(P, 0) = S!(P, 0) + 2

                Call GoodBasket

                A!(P, B1(P, B), 8) = A!(P, B1(P, B), 8) + 1
                A!(P, B1(P, B), 7) = A!(P, B1(P, B), 7) + 1

                If X3% <> 0 Then
                    S!(P, H) = S!(P, H) + 1: S!(P, 0) = S!(P, 0) + 1
                    W%(P, B1(P, B), 1) = W%(P, B1(P, B), 1) + 1
                    W%(P, B1(P, B), 0) = W%(P, B1(P, B), 0) + 1
                End If

                Call UPDATESCREEN

                GoTo TIMEEXP

            End If

        End If

        If I1% = 2 Then
            U$ = "SHOT MISSES...GAME OVER"
            Call PBP

            A!(P, B1(P, B), 7) = A!(P, B1(P, B), 7) + 1
            If X3% <> 0 Then
                W%(P, B1(P, B), 1) = W%(P, B1(P, B), 1) + 1
            End If

            GoTo TIMEEXP
        End If

        If I1% >= 3 And RN <= 8 Then
            U$ = "BALL LOST O.B.  GAME OVER"
            Call PBP
            GoTo TIMEEXP
        End If

        If I1% >= 3 And RN > 8 And RN <= 18 Then
            U$ = "PASS INTERCEPTED"
            Call PBP
            GoTo TIMEEXP
        End If

        If I1% = 3 And gameClock! < 1.5 Or I1% >= 4 And gameClock! < 3 Then
            U$ = "INBOUND PASS TO " + AA$(P, B1(P, B))
            Call PBP

            U$ = "TIME EXPIRES"
            Call PBP

            GoTo TIMEEXP
        End If

        If I1% = 3 Then

            timeElapsed! = (Rnd(1) + 1.5)
            gameClock! = gameClock! - timeElapsed!

            Call AdjustPlayTime
            Call AdjustShotClock

            U$ = B$(P) + " GETS THE BALL TO HALF COURT"
            Call PBP

            U$ = "THEY QUICKLY CALL FOR A TIMEOUT"
            Call PBP

            T1(P) = T1(P) - 1

            Call DEADBALLOPTIONS
            Call SCOREBOARD
            Call HalfCourt

        End If

        If I1% = 0 Or I1% = 4 Or (I1% = 3 And I2% = 1) Then

            ' TAKE3
            Call d100(RN)
            If gameClock! <= .5 Then XX% = 2
            If gameClock! > .5 And gameClock! <= 1.8 Then XX% = 12
            If gameClock! > 1.8 Then XX% = 20

            If RN > XX% Then
                U$ = AA$(P, B1(P, B)) + " MISSES"
                Call PBP
                A!(P, B1(P, B), 7) = A!(P, B1(P, B), 7) + 1
                W%(P, B1(P, B), 1) = W%(P, B1(P, B), 1) + 1
            Else
                U$ = "BASKET BY " + AA$(P, B1(P, B)) + "!!"
                Call PBP
                A!(P, B1(P, B), 7) = A!(P, B1(P, B), 7) + 1
                A!(P, B1(P, B), 8) = A!(P, B1(P, B), 8) + 1
                S!(P, H) = S!(P, H) + 3
                S!(P, 0) = S!(P, 0) + 3
                W%(P, B1(P, B), 1) = W%(P, B1(P, B), 1) + 1
                W%(P, B1(P, B), 0) = W%(P, B1(P, B), 0) + 1
                Call GoodBasket
                Call UPDATESCREEN
            End If

            GoTo TIMEEXP

        End If

        Call d100(RN)
        If gameClock! <= .5 Then XX% = 3
        If gameClock! > .5 And gameClock! <= 1.8 Then XX% = 16
        If gameClock! > 1.8 Then XX% = 29

        If RN > XX% Then
            U$ = AA$(P, B1(P, B)) + " MISSES"
            Call PBP
            A!(P, B1(P, B), 7) = A!(P, B1(P, B), 7) + 1
        Else
            U$ = "BASKET BY " + AA$(P, B1(P, B)) + "!!"
            Call PBP
            A!(P, B1(P, B), 7) = A!(P, B1(P, B), 7) + 1
            A!(P, B1(P, B), 8) = A!(P, B1(P, B), 8) + 1
            S!(P, H) = S!(P, H) + 2
            S!(P, 0) = S!(P, 0) + 2
            Call GoodBasket
            Call UPDATESCREEN
        End If

        GoTo TIMEEXP

    End If

    'If we can work the above into a better structure,
    'all branches end the same so we can shortcut this:

    'GoTo TIMEXP

    'Else

    FO = 0
    LS = 0
    XS = 0

    Select Case sClockVal
        Case 1: shotClock = 45
        Case 2: shotClock = 35
        Case 3: shotClock = 30
    End Select

    LS = 0

    If gameClock! < 6 And LT <> 4 Then LS = 4

    P6 = 0
    D6 = 0
    A4 = 0
    A6 = 0
    PASS% = 1
    F! = 0

    If X <> 90 Then X = 0
    If (D2(D) = 1 Or D2(D) = 3) And P7(D) = 0 Then X0% = 50: X1% = -1: Call PRDFAT
    If D2(D) = 2 And P7(D) = 0 Then X0% = 100: X1% = 1: Call PRDFAT: Call PRPFAT
    If D2(D) = 1 And P7(D) = 0 Then X0% = 25: X1% = -1: Call PRPFAT

    GoTo 400

    'End If

    '----- NOODLE -----

    350 '
    PASS% = PASS% + 1
    P6 = 0
    A4 = 0
    D6 = 0
    A6 = 1
    F! = 0
    X = 0

    If (D2(D) = 1 Or D2(D) = 3) And PASS% = 2 And P7(D) > 0 Then X0% = 50: X1% = -1: Call PRDFAT
    If D2(D) = 2 And P7(D) > 0 And PASS% = 2 Then X0% = 100: X1% = 1: Call PRDFAT: Call PRPFAT
    If D2(D) = 1 And P7(D) > 0 And PASS% = 2 Then X0% = 25: X1% = -1: Call PRPFAT

    GoTo 400

    '----- NOODLE -----

    400 '
    A4 = 0
    D6 = 0
    P3 = 0
    B = Int(Rnd(1) * 5)
    Call BALLDISTRIBUTE

    If P7(D) > 0 And PASS% = 1 Then

        ' *** FULL COURT PRESS ***

        'Yes I see it odd that Case 1, 2 and 4
        'all have identical outcome.
        'That is the code as it was written.
        'I wonder if 2,4 should be a value
        'between 25 and 50.

        Select Case P7(D)
            Case 1:
                X0% = 50: X1% = -1
            Case 2, 4:
                X0% = 50: X1% = -1
            Case 3:
                X0% = 25: X1% = -1
        End Select

        Call PRDFAT

        If H >= 2 And gameClock! <= 180 And P7(D) = 5 Then

            'DENIAL
            Call d100(RN0)
            If RN0 <= 10 Then

                gameClock! = gameClock! - 4
                timeElapsed! = 4
                Call AdjustPlayTime
                Call AdjustShotClock
                Call WHOLAY

                If PASS% <> 1 Then U$ = AA$(P, B1(P, B)) + " HAS THE BALL"
                If PASS% = 1 Then U$ = AA$(P, B1(P, B)) + " GETS THE INBOUND PASS"
                Call PBP
                U$ = "HE IS STREAKING DOWN THE FLOOR!"
                Call PBP

                LAYUP:
                FB% = 1
                F! = 98
                P6 = 1

                GoTo 520

            End If

            Call d100(RN)
            Call d100(R0!)

            IN = 99
            C1 = 2
            P9 = D

            U$ = "DESIGNATED FOULER (0-4)"
            Call SELECTPBP

            skipInbound% = 0

            If U5 = 1 And D = CT Or U5 = 2 Then
                DF% = Int(Rnd(1) * 5)
            Else
                Do
                    I$ = GetKeyPress$
                    DF% = Val(I$)
                Loop Until DF% >= 0 And DF% <= 4

                If PASS% > 1 Then skipInbound% = 1

            End If

            If skipInbound% = 0 Then

                If RN <= 4 And T1(P) > 0 Then
                    U$ = B$(P) + " CAN'T INBOUND THE BALL": Call SELECTPBP: U$ = B$(P) + " CALLS A TIME OUT"
                    If PB% = 0 Then Call PBP
                    If PB% = 1 And pbpDelay! <> 0 Then Print U$: Call DELAY
                    PASS% = 0
                    GoTo 640
                End If

                If RN <= 4 And T1(P) = 0 Or RN > 4 And RN <= 7 Then
                    U$ = B$(P) + " CAN'T INBOUND THE BALL"
                    Call SELECTPBP
                    U$ = "5 SECOND VIOLATION...TURNOVER"
                    SV% = 1
                    GoTo 517
                End If

                CY = 7 + DP(0, 4)
                If RN <= CY Then
                    Select Case R0!
                        Case 1 To 80:
                            gameClock! = gameClock! + 4
                            timeElapsed! = -4
                            Call AdjustPlayTime
                            Call AdjustShotClock

                        Case 81 To 90:
                            gameClock! = gameClock! - 3
                            timeElapsed! = 3
                            Call AdjustPlayTime
                            Call AdjustShotClock
                    End Select

                    Call LASTSHOT
                    GoTo 517
                End If

                CY = CY + F5%(P, 4) + DP(1, 4)
                If RN <= CY Then

                    Select Case R0!
                        Case 1 To 80:
                            gameClock! = gameClock! + 4
                            timeElapsed! = -4
                            Call AdjustPlayTime
                            Call AdjustShotClock

                        Case 81 To 90:
                            gameClock! = gameClock! - 3
                            timeElapsed! = 3
                            Call AdjustPlayTime
                            Call AdjustShotClock
                    End Select

                    GoTo 435

                End If

                432 '
                U$ = "PASS TO " + AA$(P, B1(P, B))
                Call SELECTPBP
            End If

            Call d100(RN0)
            If RN0 <= 5 Then IN% = 1

            Call d100(RN)

            Select Case RN

                Case 1 To 80:
                    timeElapsed! = Int(Rnd(1) * 3) + 1

                Case 81 To 90:
                    timeElapsed! = Int(Rnd(1) * 3) + 8

                Case 91 To 100:
                    timeElapsed! = Int(Rnd(1) * 3) + 5

            End Select

            gameClock! = gameClock! - timeElapsed!
            Call AdjustPlayTime
            Call AdjustShotClock

            Call d100(RN)
            If RN <= 80 Then
                W5 = DF%
                Call FoulCalled(0)
            Else
                Call FoulCalled(1)
            End If

            GoTo 9065

        End If

        If P7(D) = 1 Then X0% = 25: X1% = -1: FA%(P, B1(P, BU%)) = FA%(P, B1(P, BU%)) - 1: Call PRPFAT
        If P7(D) > 1 Then X0% = 25: X1% = -1: Call PRPFAT

        Call d100(R0!)
        If P7(D) = 1 Or P7(D) = 3 Then gameClock! = gameClock! - 2: timeElapsed! = 2
        If P7(D) = 2 And R0! <= 75 Then gameClock! = gameClock! - 2: timeElapsed! = 2
        If P7(D) = 2 And R0! > 75 Then gameClock! = gameClock! + 2: timeElapsed! = -2
        If P7(D) = 4 And R0! <= 50 Then gameClock! = gameClock! - 2: timeElapsed! = 2
        If P7(D) = 4 And R0! > 50 Then gameClock! = gameClock! + 2: timeElapsed! = -2

        Call AdjustPlayTime
        Call AdjustShotClock

        Call d100(R0!)

        If R0! <= DP(2, P7(D)) Then
            Call LASTSHOT
            Call ResetShotClock
            Call UPDATESCREEN
            X = 88

            Call FoulCalled(1)
            LT = 4

            GoTo 9065
        End If

        'We have some near identical behavior below,
        'and these are some of the only places these
        'code branches "GoTo". But I don't think it
        'can be condensed at all.
        If R0! <= DP(2, P7(D)) + 1 Then X = 0: GoTo 580

        CY = DP(2, P7(D)) + 1 + F5%(P, 5) + F5%(D, 6) + DP(0, P7(D)) - PR%(D, 0)
        If R0! <= CY Then Call LASTSHOT: GoTo 517

        CY = CY + F5%(P, 4) + DP(1, P7(D)) - PR%(D, 1)
        PC%(D) = PC%(D) + 1
        If PC%(D) > 3 Then PR%(D, 0) = 0: PR%(D, 1) = 0

        If R0! <= CY Then 435

    Else

        Call d100(R0!)

        If (OF%(P) <> 8 And OF%(P) <> 7) Or D2(D) <> 2 Then

            If R0! <= D8(4, D2(D)) Then
                Call LASTSHOT
                Call ResetShotClock
                Call UPDATESCREEN
                X = 88

                Call FoulCalled(1)
                LT = 4

                GoTo 9065
            End If

            If R0! <= D8(4, D2(D)) + 1 Then X = 0: GoTo 580

            OY% = 0
            If OF%(P) = 4 Or OF%(P) = 7 Then OY% = Int((D8(2, D2(D)) / 2) + .5) * -1

            CY = D8(4, D2(D)) + 1 + F5%(P, 5) + F5%(D, 6) + D8(2, D2(D)) + OY% + 1
            If R0! <= CY Then Call LASTSHOT: GoTo 517

            CY = CY + F5%(P, 4) + D8(3, D2(D))
            OY% = 0
            If OF%(P) = 4 Or OF%(P) = 7 Then OY% = Int((D8(3, D2(D)) / 2) + .5) * -1
            If OF%(P) = 1 Then OY% = 7
            If OF%(P) = 2 Then OY% = -7

            If R0! <= CY + OY% Then 435

        End If

    End If

    '-----

    465 Call d100(R0!)

    '--- What to do for 30 second shot clock?
    '   What is GS%?
    '   What is this doing?
    'If sClockVal = 3 Then GS% = 39 + Int(((FY%(2) - 58) * .8) + .5)
    If sClockVal = 2 Then GS% = 39 + Int(((FY%(2) - 58) * .8) + .5)

    If sClockVal = 1 Then GS% = 39 + Int(((FY%(2) - 52) * .8) + .5)

    If sClockVal = 0 Then GS% = 39 + Int(((FY%(2) - 52) * .8) + .5)
    'The "no shot clock" was previously - 40 (not - 52)

    If D2(D) = 1 Or D2(D) = 2 Then GS% = GS% + 7
    If D2(D) = 3 Then GS% = GS% - 7
    If OF%(P) = 3 Then GS% = GS% + 21
    If OF%(P) = 4 Then GS% = GS% - 21

    If OF%(P) = 7 Or OF%(P) = 8 Then 437
    If OF%(P) = 5 And shotClock > 15 And sClockVal >= 1 Then 437

    If R0! > GS% Then
        GoTo 437
    Else

        R0! = Int(Rnd(1) * 100)
        If R0! <= D8(1, D2(D)) Then

            A4 = 0
            If Not (H = 2 And gameClock! <= 120 Or H > 2) Then
                If H <= 2 Then
                    If A!(P, B1(P, B), 3) - A!(P, B1(P, B), 7) - A!(P, B1(P, B), 11) - A!(P, B1(P, B), 12) <= 0 Then 437
                End If
            End If

            Call SHOTPERCENT

            A4 = Int(Rnd(1) * 28) + 1
            R0! = Int(Rnd(1) * 9) + 1
            I2 = Int(Rnd(1) * 5)

            If FA%(P, B1(P, B)) > -5 Then

                If R0! <= A!(P, B1(P, I2), 4) Then

                    F! = A!(P, B1(P, B), 0) + D1(D) + A4 + TF%(P) + TF%(D)
                    TF%(P) = 0
                    TF%(D) = 0
                    PZ% = 2
                    P6 = 1
                    X = 0

                    Call d100(RN0)

                    If PZ% = 2 And RN0 <= 50 Then
                        PZ% = 3
                        P6 = 1
                    End If

                    GoTo 520

                End If

            End If

        End If

    End If

    If OF%(P) = 4 Then
        GoTo 437
    Else

        If H <= 2 Then

            If Not (H = 2 And gameClock! <= 120 Or H > 2) Then
                If A!(P, B1(P, B), 3) - A!(P, B1(P, B), 7) - A!(P, B1(P, B), 11) - A!(P, B1(P, B), 12) <= 0 Then 437
            End If

        End If

        P6 = 1
        Call SHOTPERCENT

        PZ% = Int(Rnd(1) * 3) + 5

        X = 0

        Call d100(RN0)
        If PZ% = 2 And RN0 <= 50 Then
            PZ% = 3
            P6 = 1
        End If

        GoTo 520

    End If


    '----- NOODLE -----

    435 '
    P9 = Int(Rnd(1) * 5)

    If Int(Rnd(1) * 5) + 1 <= A!(D, B1(D, P9), 5) Then

        A!(D, B1(D, P9), 18) = A!(D, B1(D, P9), 18) + 1

        If P7(D) <= 0 Or PASS% <> 1 Then
            Call LASTSHOT
        End If

        Call ResetShotClock
        A!(P, B1(P, B), 23) = A!(P, B1(P, B), 23) + 1

        If pbpDelay! <> 0 Then

            U$ = "BALL STOLEN BY " + AA$(D, B1(D, P9)) + "!"

            If PB% = 1 Then
                Call WINDEX
                Print U$
                Call DELAY
            Else

                Call d100(RN)
                Select Case RN
                    Case 1 To 3
                        U$ = AA$(D, B1(D, P9)) + " QUICKLY REACHES IN!": Call PBP
                    Case 4 To 6
                        U$ = AA$(D, B1(D, P9)) + " SNEAKS IN FROM BEHIND!": Call PBP
                    Case 7 To 10
                        U$ = AA$(D, B1(D, P9)) + " GETS A HAND ON THE BALL!": Call PBP
                        'No case for 11 to 100
                End Select

                RN = Int(Rnd(1) * 20) + 1
                Select Case RN

                    Case 1
                        U$ = "PASS INTERCEPTED BY " + AA$(D, B1(D, P9)) + "!"
                    Case 2
                        U$ = "PASS IS PICKED OFF BY " + AA$(D, B1(D, P9)) + "!"
                    Case 3
                        U$ = "BALL IS STOLEN BY " + AA$(D, B1(D, P9)) + "!"
                    Case 4
                        U$ = AA$(D, B1(D, P9)) + " WITH THE STEAL!"
                    Case 5
                        U$ = AA$(D, B1(D, P9)) + " TAKES IT AWAY!"
                    Case 6
                        U$ = AA$(D, B1(D, P9)) + " STEALS IT!"
                    Case 7
                        U$ = AA$(D, B1(D, P9)) + " RIPS IT LOOSE!"
                    Case 8
                        U$ = AA$(D, B1(D, P9)) + " TAKES THE BALL AWAY!"
                    Case 9
                        U$ = AA$(D, B1(D, P9)) + " STEALS THE BALL!"
                    Case 10
                        U$ = AA$(D, B1(D, P9)) + " PICKS IT OFF!"
                    Case 11
                        U$ = AA$(D, B1(D, P9)) + " SWIPES THE BALL!"
                    Case 12
                        U$ = AA$(D, B1(D, P9)) + " WITH THE STOLEN BALL!"
                    Case 13
                        U$ = AA$(D, B1(D, P9)) + " STRIPS IT AWAY!"
                    Case 14
                        U$ = AA$(D, B1(D, P9)) + " WITH THE STEAL!"
                    Case 15
                        U$ = AA$(D, B1(D, P9)) + " SWIPES AND STEALS IT!"
                    Case 16
                        U$ = AA$(D, B1(D, P9)) + " SNATCHES IT AWAY!"
                    Case 17
                        U$ = AA$(D, B1(D, P9)) + " QUICKLY REACHES IN!"
                    Case 18
                        U$ = AA$(D, B1(D, P9)) + " TAKES IT THE OTHER WAY!"
                    Case 19
                        U$ = AA$(D, B1(D, P9)) + " POKES IT LOOSE!"
                    Case 20
                        U$ = AA$(D, B1(D, P9)) + " STRIPS IT CLEAN!"

                End Select

                Call PBP

            End If

        End If

        A!(D, B1(D, P9), 13) = A!(D, B1(D, P9), 13) + 1
        T0(P) = T0(P) + 1

        Call UPDATESCREEN

        C1 = 2
        P9 = D

        If gameClock! <= 0 Then 170

        PB$ = ""
        If P = 1 Then
            P = 0
            D = 1
            P9 = 0
            C1 = 2
        Else
            'P = 0
            P = 1
            D = 0
            P9 = 1
            C1 = 2
        End If

        FB% = 1

        Call d100(RN)
        If RN <= 38 Then B = 0
        If RN > 38 And RN <= 75 Then B = 1
        If RN > 75 And RN <= 83 Then B = 2
        If RN > 83 And RN <= 91 Then B = 3
        If RN > 91 Then B = 4

        Call d100(RN)
        If PASS% > 1 And (D2(P9) = 0 Or D2(P9) = 1 Or D2(P9) = 3) And RN <= 15 Then GoTo LAYUP
        If PASS% > 1 And D2(P9) = 2 And RN <= 5 Then GoTo LAYUP
        If PASS% = 1 And P7(P) > 0 And RN <= 33 Then GoTo LAYUP
        If PASS% = 1 And P7(P) > 0 And P7(P) <> 3 And RN <= 40 Then GoTo LAYUP

        GoTo 300

    End If

    Call d100(R0!)

    If P7(D) = 1 And PASS% = 1 And R0! <= A!(P, B1(P, BU%), 4) Then GoTo LAYUP
    If P7(D) = 2 And PASS% = 1 And R0! <= A!(P, B1(P, BU%), 4) * 2 Then GoTo LAYUP
    If P7(D) = 3 And PASS% = 1 And R0! <= 7 Then GoTo LAYUP
    If P7(D) = 4 And PASS% = 1 And R0! <= 15 Then GoTo LAYUP

    If P7(D) = 1 And PASS% = 1 And R0! <= A!(P, B1(P, BU%), 4) * 2 Then GoTo GDSHOT
    If P7(D) = 2 And PASS% = 1 And R0! <= A!(P, B1(P, BU%), 4) * 4 Then GoTo GDSHOT
    If P7(D) = 3 And PASS% = 1 And R0! <= 14 Then GoTo GDSHOT
    If P7(D) = 4 And PASS% = 1 And R0! <= 30 Then GoTo GDSHOT
    If P7(D) = 5 Then 432

    ' --- I think this is by itself and the above are separate??? ---

    437 '

    P6 = 0
    R0! = Int(Rnd(1) * 3) + 1
    J8% = 0
    F3S% = Int(Rnd(1) * 7) + 3
    PZ% = 1

    If A!(P, B1(P, B), 0) > 60 And R0! = 1 Then F! = 24
    If A!(P, B1(P, B), 0) > 60 And R0! = 2 Then F! = 30
    If A!(P, B1(P, B), 0) > 60 And R0! = 3 Then F! = 36
    If A!(P, B1(P, B), 0) <= 60 And R0! = 1 Then F! = Int(A!(P, B1(P, B), 0) * .4 + .5)
    If A!(P, B1(P, B), 0) <= 60 And R0! = 2 Then F! = Int(A!(P, B1(P, B), 0) * .5 + .5)
    If A!(P, B1(P, B), 0) <= 60 And R0! = 3 Then F! = Int(A!(P, B1(P, B), 0) * .6 + .5)

    If Int(Rnd(1) * 150) + 1 = 40 Then
        GoTo TIEUP
    Else
        X = 0
        Call d100(RN0)
        If PZ% = 2 And RN0 <= 50 Then PZ% = 3: P6 = 1
        GoTo 520
    End If

    '----- NOODLE -----

    517 Call ResetShotClock

    R0! = Int(Rnd(1) * 25) + 1

    If R0! = 13 Then

        GoTo TIEUP

    Else

        A!(P, B1(P, B), 23) = A!(P, B1(P, B), 23) + 1
        T0(P) = T0(P) + 1

        If SV% <> 1 Then PT% = 1: Call MAKEPBP

        SV% = 0

        If PB% = 1 And pbpDelay! <> 0 Then Call WINDEX: Print "TURNOVER BY "; B$(P): Call DELAY
        If PB% = 0 Then Call PBP

        U$ = "BALL OUT OF BOUNDS TO " + B$(D)
        If PB% = 0 Then Call PBP
        If PB% = 1 And pbpDelay! <> 0 Then Print U$: Call DELAY

        Call DELAY
        Call UPDATESCREEN

        Locate 10, 1

        If gameClock! <= 0 Then 170

        PB$ = ""
        If P = 1 Then
            P = 0
            D = 1
            P9 = 0
            C1 = 2
        Else
            'P = 0
            P = 1
            D = 0
            P9 = 1
            C1 = 2
        End If

        Call DEADBALLOPTIONS

        For I = 0 To 1
            If P7(I) = 0 Then PR%(I, 0) = 1: PR%(I, 1) = 7: PC%(I) = 0
        Next

        GoTo 300

    End If

    '----- NOODLE -----

    GDSHOT:
    FX% = A!(P, B1(P, B), 0)

    If FA%(P, B1(P, B)) <= -5 Then FX% = FX% - 10

    F! = FX% + Int(Rnd(20)) + 11 + TF%(D) + TF%(P)
    TF%(P) = 0
    TF%(D) = 0
    PZ% = 10

    PZ% = Int(Rnd(1) * 3) + 5
    X = 0

    Call d100(RN0)
    If PZ% = 2 And RN0 <= 50 Then PZ% = 3: P6 = 1

    520 '
    If J8% = 1 Then PZ% = 4

    Call LASTSHOT: If gameClock! <= 0 Then 170

    If PZ% = 4 And F3S% <= 0 Then F3S% = 3

    If pbpDelay! <> 0 Then

        If PZ% = 10 Then

            GDSHOTDES:
            If pbpDelay! <> 0 Then

                Call d100(RN)
                If RN <= 25 Then U$ = AA$(P, B1(P, B)) + " BREAKS IN TO FRONT COURT"
                If RN > 25 And RN <= 50 Then U$ = AA$(P, B1(P, B)) + " GETS THE PASS IN FRONT COURT"
                If RN > 50 And RN <= 75 Then U$ = AA$(P, B1(P, B)) + " BREAKS THE PRESS"
                If RN > 75 Then U$ = AA$(P, B1(P, B)) + " DEFEATS THE PRESS"
                Call PBP

                Call d100(RN)
                If RN <= 33 Then U$ = "HE HAS AN OPEN JUMPER AT THE FT LINE"
                If RN > 33 And RN <= 66 Then U$ = "HE'S OPEN ON THE BASELINE"
                If RN > 66 Then U$ = "HE'S OPEN FROM THE WING"
                Call SHOTSTRING
                Call PBP

            End If

        Else

            If F! > 70 And FB% = 1 And PASS% = 1 Then
                PT% = 6
                PZ% = 8
                Call MAKEPBP
                Call PBP

            Else

                If AA$(P, B1(P, B)) <> PB$ And PB$ = "" Then U$ = "PASS TO " + AA$(P, B1(P, B)): Call PBP
                If AA$(P, B1(P, B)) <> PB$ And PB$ <> "" Then PT% = 10: Call MAKEPBP: Call PBP
                If AA$(P, B1(P, B)) = PB$ Then PT% = 9: Call MAKEPBP: Call PBP

                If shotClock <= 0 And sClockVal > 0 Then
                    U$ = "SHOT CLOCK HAS EXPIRED!"
                    SV% = 1
                    Call LASTSHOT
                    GoTo 517
                End If

                PB$ = AA$(P, B1(P, B))
                YY$ = "HE"

                Select Case PZ%
                    Case 1:
                        If pbpDelay! <> 0 Then

                            RN = Int(Rnd(1) * 12) + 1
                            Select Case RN
                                Case 1
                                    U$ = YY$ + "'S HEAVILY GUARDED"
                                Case 2
                                    U$ = YY$ + "'S GUARDED"
                                Case 3
                                    U$ = YY$ + "'S GUARDED BY " + AA$(D, B1(D, B))
                                Case 4
                                    U$ = YY$ + " HAS A MAN ON HIM"
                                Case 5
                                    U$ = YY$ + " HAS A DEFENDER ON HIM"
                                Case 6
                                    U$ = YY$ + "'S COVERED"
                                Case 7
                                    U$ = YY$ + " CAN'T GET AROUND THE DEFENDER"
                                Case 8
                                    U$ = YY$ + "'S PRESSURED"
                                Case 9
                                    U$ = YY$ + " HAS NOWHERE TO GO"
                                Case 10
                                    U$ = YY$ + " HAS A HAND IN HIS FACE"
                                Case 11
                                    U$ = YY$ + "'S WATCHED BY " + AA$(D, B1(D, B))
                                Case 12
                                    U$ = YY$ + "'S WORKING ON " + AA$(D, B1(D, B))
                            End Select

                        End If

                        Call PBP

                    Case 2:
                        U$ = YY$ + " WORKS FREE INSIDE"
                        Call PBP
                        U$ = "HE HAS AN OPEN SHOT"
                        Call SHOTSTRING
                        Call PBP

                        P6 = 1

                    Case 3:
                        If pbpDelay! <> 0 Then

                            P6 = 1

                            Call d100(RN)
                            If B = 4 And RN <= 25 Then U$ = YY$ + " MAKES A MOVE DOWN LOW": Call PBP: GoTo 602
                            If B = 4 And RN <= 50 Then U$ = YY$ + " POSITIONS HIMSELF DOWN LOW": Call PBP: GoTo 602
                            If B = 4 And RN <= 75 Then U$ = YY$ + " POSTS UP UNDERNEATH": Call PBP: GoTo 602
                            If B = 4 Then U$ = YY$ + " BACKS IN AGAINST " + AA$(D, B1(D, 4)): Call PBP

                            602 '
                            If B = 4 Then U$ = "HE HAS A GOOD SHOT INSIDE": Call SHOTSTRING: Call PBP: GoTo 529
                            If (B = 3 Or B = 2) And RN <= 17 Then U$ = YY$ + " DRIVES THE BASELINE": Call PBP: GoTo 603
                            If (B = 3 Or B = 2) And RN <= 34 Then U$ = YY$ + " PENETRATES THE LANE": Call PBP: GoTo 603
                            If (B = 3 Or B = 2) And RN <= 50 Then U$ = YY$ + " MOVES INSIDE": Call PBP: GoTo 603
                            If (B = 3 Or B = 2) And RN <= 67 Then U$ = YY$ + " DRIVES THE LANE": Call PBP: GoTo 603
                            If (B = 3 Or B = 2) And RN <= 84 Then U$ = YY$ + " MOVES INTO THE PAINT": Call PBP: GoTo 603
                            If (B = 3 Or B = 2) Then U$ = YY$ + " DRIVES UNDERNEATH": Call PBP

                            603 If (B = 3 Or B = 2) Then U$ = "HE'S GOT A GOOD SHOT UNDERNEATH": Call SHOTSTRING: Call PBP: GoTo 529
                            If (B = 0 Or B = 1) And RN <= 25 Then U$ = YY$ + " DRIVES THE LANE": Call PBP: GoTo 604
                            If (B = 0 Or B = 1) And RN <= 50 Then U$ = YY$ + " DRIVES THROUGH THE KEY": Call PBP: U$ = "HE'S INTO THE LANE": Call PBP: GoTo 604
                            If (B = 0 Or B = 1) And RN <= 75 Then U$ = YY$ + " DRIVES TO THE BASKET": Call PBP: GoTo 604
                            U$ = YY$ + " DRIVES THROUGH AN OPENING": Call PBP

                            604 U$ = "HE HAS A GOOD SHOT CLOSE IN": Call SHOTSTRING: Call PBP
                        End If

                    Case 4:
                        U$ = "HE'S OPEN FROM 3-POINT RANGE!"
                        U$ = U$ + Str$(F3S%) + "*"
                        Call PBP

                        P6 = 1

                    Case 5:
                        U$ = "HE'S OPEN FROM MEDIUM RANGE"
                        Call SHOTSTRING
                        Call PBP

                        P6 = 1

                    Case 6:
                        U$ = "HE'S OPEN FROM MEDIUM RANGE"
                        Call SHOTSTRING
                        Call PBP

                        P6 = 1

                    Case 7:
                        U$ = "HE'S OPEN FROM MEDIUM RANGE"
                        Call SHOTSTRING
                        Call PBP

                        P6 = 1

                End Select

            End If

        End If

    End If

    529 '
    If F! <= 0 Then F! = 1
    If F! >= 100 Then F! = 99

    skipShotPct% = 0

    If pbpDelay! <> 0 Then

        If PB% = 1 Then Call WINDEX: Print "PASS TO "; AA$(P, B1(P, B)); " OF "; B$(P)

        If PB% = 1 And PZ% <> 4 Then Locate 10: Print "SHOT % "; F!;

        If PB% = 1 And PZ% = 4 Then
            Locate 10
            Print "3 PT SHOT % "; F3S%;: Print " *";
            skipShotPct% = 1
        Else
            If PB% = 1 And PZ% >= 2 And PZ% <= 8 Then Print "*";
        End If

    End If

    If skipShotPct% = 0 Then
        If pbpDelay! <> 0 And PB% <> 1 Then

            If PZ% <> 4 Then Locate 8, 41: Print Using "SHOT % ###   "; F!;

            If PZ% = 4 Then
                Locate 8, 41
                Print Using "SHOT % ###"; F3S%;: Print " *";
            Else
                If PZ% >= 2 And PZ% <= 8 Then Locate , 52: Print "*";
            End If
        End If

    End If

    Print
    If PB% = 1 And pbpDelay! <> 0 Then Call DELAY

    If U5 <> 0 Or coachOpt = 1 Then

        If Not (H >= 2 And gameClock! <= 240 And S!(P, 0) - S!(D, 0) > 0 And U5 = 1 And P = CT) Then

            If U5 = 2 Or U5 = 1 And D = CT Then
                If (U5 = 1 Or U5 = 0) And coachOpt <> 1 Then
                    GoTo 530
                End If
            End If

        End If

        If gameClock! <= 1 Or sClockVal >= 1 And shotClock <= 7 Then
            P6 = 1
        Else
            If gameClock! <= 8 And S!(P, 0) - S!(D, 0) <= -3 And H >= 2 Then
                P6 = 1
            Else
                If gameClock! <= 20 And S!(P, 0) - S!(D, 0) <= -5 And H >= 2 Then
                    P6 = 1
                Else

                    FX% = A!(P, B1(P, B), 0)

                    If FA%(P, B1(P, B)) <= -5 Then FX% = FX% - 10

                    If P6 <> 1 Then

                        If (U5 = 0 And coachOpt = 1 Or U5 = 1 And D = CT And coachOpt = 1 Or U5 = 1 And P = CT Or U5 = 2) Then

                            If gameClock! <= 660 And gameClock! >= 540 And H = 1 And T1(P) >= 5 Then
                                P6 = 2: CTO% = 1
                            Else

                                If gameClock! <= 240 And gameClock! >= 120 And H = 1 And T1(P) >= 4 Then

                                    P6 = 2: CTO% = 1

                                Else

                                    If gameClock! <= 840 And gameClock! >= 720 And H = 2 And T1(P) >= 3 Then
                                        P6 = 2: CTO% = 1
                                    Else

                                        If gameClock! <= 300 And gameClock! >= 180 And H = 2 And T1(P) >= 2 Then
                                            P6 = 2: CTO% = 1
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

        530 '
        I$ = GetKeyPress$

        If UCase$(I$) = "T" Then
            Call TICKER
            Call SCOREBOARD
            GoTo 529
        End If

        If UCase$(I$) = "D" Then
            Call ZONECHANGE
            Call SCOREBOARD
            GoTo 529
        End If

        P6 = Val(I$)

        If P6 < 1 Or P6 > 4 Then P6 = 0
        If P6 = 4 Then FT% = 1

    End If

    If Val(I$) = 3 And (U5 = 0 Or U5 = 1 And P = CT) And H >= 4 And gameClock! < 240 Then P6 = 3
    If Val(I$) = 2 And (U5 = 0 Or U5 = 1 And D = CT) And T1(P) > 0 Then P6 = 2
    P3 = P6

    If P6 = 2 And T1(P) = 0 Or P6 = 3 And H = 1 Or P6 = 3 And U5 = 1 And D = CT Or P6 = 3 And H = 2 And gameClock! > 240 Then 530

    If U5 = 1 And P = CT And J8% = 1 Or U5 = 2 And J8% = 1 Then P6 = 4
    If U5 = 1 And P = CT And H >= 2 And gameClock! < 30 And S!(D, 0) - S!(P, 0) = 3 Or U5 = 2 And H >= 2 And gameClock! < 30 And S!(D, 0) - S!(P, 0) = 3 Then P6 = 4
    If (P6 = 1 Or P6 = 4) And PZ% = 4 Then P6 = 4

    caseVar% = P6 + 1

    Select Case caseVar%

        'On caseVar% GOTO 350, 537, 640,

        Case 1
            GoTo 350

        Case 2
            'WARNING - this is a long "routine"

            537
            ' *** SHOOT !!! ***
            PT% = 3
            Call MAKEPBP
            Call PBP

            S = 0
            SS = 0

            Call d100(R0!)

            A!(P, B1(P, B), 7) = A!(P, B1(P, B), 7) + 1
            GG = S!(D, 0) - S!(P, 0)

            If Not (H > 1 And gameClock! <= 30 And GG = 2 Or H > 1 And gameClock! <= 30 And GG = 0 Or H > 1 And gameClock! <= 30 And GG = -2) Then

                If R0! <= A!(P, B1(P, B), 16) + F5%(D, 7) Then
                    A!(P, B1(P, B), 7) = A!(P, B1(P, B), 7) - 1
                    Call ResetShotClock
                    LT = 0

                    gameClock! = gameClock! - 4
                    timeElapsed! = 4
                    Call AdjustPlayTime
                    Call AdjustShotClock
                    Call UPDATESCREEN

                    Call FoulCalled(1)

                    If PB% = 0 Then
                        U$ = "2 FT: " + AA$(P, B1(P, B)) + Str$(A!(P, B1(P, B), 1)) + " %"
                        Call PBP
                        Call DELAY
                    Else
                        Call WINDEX
                        Print B$(P)
                        Print "2 FT: "; AA$(P, B1(P, B)); "   "; A!(P, B1(P, B), 1); " %"
                    End If

                    X = 50

                    Call DEADBALLOPTIONS

                    For I = 0 To 1
                        If P7(I) = 0 Then PR%(I, 0) = 1: PR%(I, 1) = 7: PC%(I) = 0
                    Next

                    X = 0

                    If pbpDelay! <> 0 Then

                        If PB% = 0 Then
                            U$ = "2 FT: " + AA$(P, B1(P, B)) + Str$(A!(P, B1(P, B), 1)) + " %"
                            Call PBP
                        Else
                            Call WINDEX
                            Print B$(P)
                            Print "2 FT: "; AA$(P, B1(P, B)); "   "; A!(P, B1(P, B), 1); " %"
                        End If

                    End If

                    'Two FT Attempts
                    For F3 = 1 To 2:

                        Call d100(S)

                        'FT Success Chance
                        FX% = A!(P, B1(P, B), 1)

                        If FA%(P, B1(P, B)) <= -5 Then FX% = Int(FX% * .9 + .5)

                        If S <= FX% Then

                            If PB% = 0 Then
                                Call THREEFT
                            Else
                                'PB% = 1
                                Call MADEFT
                            End If
                        Else
                            'S > FX% And
                            If PB% = 0 Then
                                Call BADFT
                            Else
                                'PB% = 1
                                Call MISSFT

                            End If
                        End If

                    Next F3

                    'Increase FT ATT
                    A!(P, B1(P, B), 9) = A!(P, B1(P, B), 9) + 2

                    LT = 0
                    If gameClock! <= 0 Then
                        timeElapsed! = gameClock!
                        Call AdjustPlayTime
                        GoTo 170
                    End If

                    If S > FX% Then
                        GoTo 650
                    Else
                        598 '
                        PB$ = ""
                        If P = 1 Then
                            P = 0
                            D = 1
                            P9 = 0
                            C1 = 2
                        Else
                            'P = 0
                            P = 1
                            D = 0
                            P9 = 1
                            C1 = 2
                        End If

                        U$ = B$(P) + " BALL O.B. AFTER SCORE"
                        Call SELECTPBP

                        Locate 8, 41: Print Space$(12)

                        Call DELAY
                        Call STOPPAGE
                        B = 6
                        X = 0
                        Call PRESSGUARD

                        GoTo 300

                    End If

                End If

            End If

            Call d100(S)
            R0! = F! * ((DY / 100) + (.03 * X3%))

            If Not (H > 1 And gameClock! < 30 And GG = 2 Or H > 1 And gameClock! < 30 And GG = 0 Or H > 1 And gameClock! < 30 And GG = -2) Then

                If S <= R0! Then
                    Call ResetShotClock
                    LT = 0

                    gameClock! = gameClock! - 4
                    timeElapsed! = 4
                    Call AdjustPlayTime
                    Call AdjustShotClock
                    Call UPDATESCREEN
                    Call GoodBasket

                    If DK% = 1 And PB% = 0 Then PT% = 2: Call MAKEPBP: U$ = "STUFF BY " + AA$(P, B1(P, B)): Call PBP: DK% = 0: GoTo 617
                    If DK% = 1 And PB% = 1 Then Call WINDEX: Print "STUFF BY " + AA$(P, B1(P, B)): Call DELAY: DK% = 0: GoTo 617
                    If PZ% = 8 Then PT% = 7: Call MAKEPBP: PZ% = 0: Call PBP: GoTo 617

                    F3 = 1
                    If PB% = 0 Then
                        U$ = "BASKET GOOD BY " + AA$(P, B1(P, B))
                        Call PBP
                    Else
                        'PB% = 1
                        If pbpDelay! <> 0 Then Call WINDEX: Print "BASKET GOOD BY " + AA$(P, B1(P, B)): Call DELAY
                    End If

                    617 U$ = "... AND HE'S FOULED !!!"

                    If PB% = 0 Then Call PBP
                    If PB% = 1 And pbpDelay! <> 0 Then Print U$: Call DELAY

                    Call DELAY

                    F3 = 1
                    S!(P, H) = S!(P, H) + 2
                    S!(P, 0) = S!(P, 0) + 2
                    A!(P, B1(P, B), 8) = A!(P, B1(P, B), 8) + 1
                    Call UPDATESCREEN
                    Call FoulCalled(1)

                    619 '
                    If PB% = 1 And pbpDelay! <> 0 Then Call WINDEX: Print B$(P): Print "1 FT: "; AA$(P, B1(P, B)); "   "; A!(P, B1(P, B), 1); " %"
                    If PB% = 0 Then U$ = AA$(P, B1(P, B)) + " WILL SHOOT 1": Call PBP

                    X = 50
                    Call DEADBALLOPTIONS

                    For I = 0 To 1
                        If P7(I) = 0 Then PR%(I, 0) = 1: PR%(I, 1) = 7: PC%(I) = 0
                    Next

                    X = 0

                    Call d100(S)

                    If PB% = 1 And pbpDelay! <> 0 Then
                        Call WINDEX
                        Print B$(P)
                        Print "1 FT: "; AA$(P, B1(P, B)); "   "; A!(P, B1(P, B), 1); " %"
                    End If

                    FX% = A!(P, B1(P, B), 1)
                    If FA%(P, B1(P, B)) <= -5 Then FX% = Int(FX% * .9 + .5)

                    If S <= FX% And PB% = 0 Then
                        'Increase FTM
                        A!(P, B1(P, B), 10) = A!(P, B1(P, B), 10) + 1

                        If pbpDelay! <> 0 Then U$ = "FREE THROW IS GOOD": Call PBP

                        S!(P, H) = S!(P, H) + 1
                        S!(P, 0) = S!(P, 0) + 1

                        Call UPDATESCREEN

                    End If

                    If S <= FX% Then
                        If PB% = 1 Then Call MADEFT
                    Else
                        If PB% = 0 Then If pbpDelay! <> 0 Then U$ = "FREE THROW IS MISSED": Call PBP
                        If PB% = 1 Then Call MISSFT
                    End If

                    'Increase FT ATT
                    A!(P, B1(P, B), 9) = A!(P, B1(P, B), 9) + 1

                    If gameClock! <= 0 Then
                        timeElapsed! = gameClock!
                        Call AdjustPlayTime
                        GoTo 170
                    End If

                    X = 99

                    If S > FX% Then 670

                    PB$ = ""
                    If P = 1 Then
                        P = 0
                        D = 1
                        P9 = 0
                        C1 = 2
                    Else
                        'P = 0
                        P = 1
                        D = 0
                        P9 = 1
                        C1 = 2
                    End If

                    U$ = B$(P) + " BALL O.B. AFTER SCORE"
                    Call SELECTPBP
                    Locate 8, 41: Print Space$(12)
                    Call DELAY
                    Call STOPPAGE
                    B = 6
                    X = 0
                    Call PRESSGUARD

                    GoTo 300

                End If

            End If

            If S <= F! Then

                LT = 0

                If DK% = 1 And PB% = 0 Then
                    PT% = 2
                    Call MAKEPBP
                    U$ = "STUFF BY "
                    DK% = 0
                Else
                    If DK% = 1 And PB% = 1 Then
                        U$ = "STUFF BY "
                        DK% = 0
                    Else

                        If PB% = 0 And PZ% = 8 Then
                            PT% = 7
                            Call MAKEPBP
                            Call PBP
                            PZ% = 0

                        Else
                            U$ = "BASKET BY "

                            U$ = U$ + AA$(P, B1(P, B)) + " !!!"

                            If PB% = 1 And pbpDelay! <> 0 Then
                                Call WINDEX: Print U$: Call DELAY
                            Else

                                Call d100(RN)

                                If RN > 50 Then

                                    RN = Int(Rnd(1) * 22) + 1
                                    Select Case RN
                                        Case 1
                                            U$ = "NOTHING BUT THE BOTTOM OF THE NET!"
                                        Case 2
                                            U$ = "COUNT IT!"
                                        Case 3
                                            U$ = AA$(P, B1(P, B)) + " TICKLES THE TWINE!"
                                        Case 4
                                            U$ = "NO RIM, NO BOARD, JUST CORD!"
                                        Case 5
                                            U$ = "GOOD!"
                                        Case 6
                                            U$ = "YES!"
                                        Case 7
                                            U$ = "COUNT IT!"
                                        Case 8
                                            U$ = "IT'S GOOD!"
                                        Case 9
                                            U$ = "BINGO!"
                                        Case 10
                                            U$ = "IT FALLS!"
                                        Case 11
                                            U$ = "HE DRAINS IT!"
                                        Case 12
                                            U$ = "SWISH!"
                                        Case 13
                                            U$ = "ALL NET!"
                                        Case 14
                                            U$ = "IT'S THERE!"
                                        Case 15
                                            U$ = "HE BURIES IT!"
                                        Case 16
                                            U$ = "IT GOES IN!"
                                        Case 17
                                            U$ = "RING IT UP!"
                                        Case 18
                                            U$ = "IT GOES DOWN!"
                                        Case 19
                                            U$ = "BULLSEYE!"
                                        Case 20
                                            U$ = "HE SCORES!"
                                        Case 21
                                            U$ = "HE HAS IT!"
                                        Case 22
                                            U$ = "HE CONNECTS!"
                                    End Select

                                End If

                                If PB% = 0 Then Call PBP

                            End If

                        End If

                    End If

                End If

                560 S!(P, H) = S!(P, H) + 2
                S!(P, 0) = S!(P, 0) + 2

                Call GoodBasket

                Call UPDATESCREEN

                A!(P, B1(P, B), 8) = A!(P, B1(P, B), 8) + 1

                562 U$ = B$(D) + " BALL O.B. AFTER SCORE"
                If PB% = 1 And pbpDelay! <> 0 Then Locate 9, 1: Print U$
                If PB% = 0 Then Locate 8, 41: Print Space$(12): Call PBP

                Call DELAY
                Call ResetShotClock

                gameClock! = gameClock! - 4
                timeElapsed! = 4

                Call AdjustPlayTime
                Call AdjustShotClock

                If gameClock! <= 0 Then
                    timeElapsed! = gameClock!
                    Call AdjustPlayTime
                    GoTo 170
                End If

                Call UPDATESCREEN

                PB$ = ""
                If P = 1 Then
                    P = 0
                    D = 1
                    P9 = 0
                    C1 = 2
                Else
                    'P = 0
                    P = 1
                    D = 0
                    P9 = 1
                    C1 = 2
                End If

                Call STOPPAGE
                Call PRESSGUARD

                GoTo 300

            Else
                Call d100(R0!)
                LT = 0

                I2 = Int(Rnd(1) * 5) + 1
                I3 = Int(Rnd(1) * 5)

                If I2 > A!(D, B1(D, I3), 6) Then

                    If PB% = 1 And pbpDelay! <> 0 Then Call WINDEX: Print "SHOT MISSED BY "; AA$(P, B1(P, B)): Call DELAY
                    If PB% = 0 Then PT% = 8: Call MAKEPBP: Call PBP

                    Call ResetShotClock
                    gameClock! = gameClock! - 4
                    timeElapsed! = 4

                    Call AdjustPlayTime
                    Call AdjustShotClock

                    If gameClock! <= 0 Then
                        timeElapsed! = gameClock!
                        Call AdjustPlayTime
                        GoTo 170
                    End If

                    Call UPDATESCREEN

                    LT = 0

                    GoTo 670

                End If

                If pbpDelay! <> 0 Then

                    If PB% = 1 Then
                        Call WINDEX
                        Print "SHOT BLOCKED BY "; AA$(D, B1(D, I3))
                        Call DELAY
                    Else

                        Call d100(RN)
                        Select Case RN
                            Case 1 To 10
                                U$ = AA$(D, B1(D, I3)) + " STUFFS THE SHOT!!"
                            Case 11 To 15
                                U$ = "DENIED BY " + AA$(D, B1(D, I3)) + "!!"
                            Case 16 To 20
                                U$ = "ERASED BY " + AA$(D, B1(D, I3)) + "!!"
                            Case 21 To 25
                                U$ = "EXPUNGED BY " + AA$(D, B1(D, I3)) + "!!"
                            Case 26 To 30
                                U$ = "REBUKED BY " + AA$(D, B1(D, I3)) + "!!"
                            Case 31 To 35
                                U$ = "SHOT BLOCKED CLEANLY BY " + AA$(D, B1(D, I3)) + "!!"
                            Case 36 To 70
                                U$ = "SHOT BLOCKED BY " + AA$(D, B1(D, I3)) + "!!"
                            Case 71 To 90
                                U$ = "SHOT REJECTED BY " + AA$(D, B1(D, I3)) + "!!"
                            Case 91 To 100
                                U$ = "SHOT SWATTED OUT BY " + AA$(D, B1(D, I3)) + "!!"
                        End Select

                        Call PBP

                    End If

                End If

                A!(D, B1(D, I3), 15) = A!(D, B1(D, I3), 15) + 1
                X = 98

                Call ResetShotClock

                gameClock! = gameClock! - 4
                timeElapsed! = 4
                Call AdjustPlayTime

                Call AdjustShotClock

                If gameClock! <= 0 Then
                    timeElapsed! = gameClock!
                    Call AdjustPlayTime
                    TP% = 0
                    GoTo 170
                End If

                Call UPDATESCREEN

                GoTo 670

            End If

            '--- done with "routines" for taking a shot

        Case 3
            GoTo 640

        Case 4
            IN = 99
            Call LASTSHOT

            If gameClock! <= 0 Then 170

            C1 = 2: P9 = D
            U$ = "DESIGNATED FOULER (0-4)"

            Call SELECTPBP

            If U5 = 1 And D = CT Or U5 = 2 Then
                W5 = Int(Rnd(1) * 5)
            Else
                Do
                    I$ = GetKeyPress$
                    W5 = Val(I$)
                Loop Until W5 >= 0 And W5 <= 4
            End If

            Call FoulCalled(0)

            GoTo 9065

        Case 5
            J8% = 0
            If X3% = 0 Then
                P3 = 1
                P6 = 1
                GoTo 537
            Else
                Call d100(S)
                W%(P, B1(P, B), 1) = W%(P, B1(P, B), 1) + 1
                A!(P, B1(P, B), 7) = A!(P, B1(P, B), 7) + 1
                PT% = 3

                Call MAKEPBP
                Call PBP

                I2 = Int(Rnd(1) * 50) + 1

                If I2 = 43 And TF% = 1 And S > F3S% Then

                    A!(P, B1(P, B), 7) = A!(P, B1(P, B), 7) - 1
                    W%(P, B1(P, B), 1) = W%(P, B1(P, B), 1) - 1

                    Call ResetShotClock
                    gameClock! = gameClock! - 4
                    timeElapsed! = 4
                    Call AdjustPlayTime
                    Call AdjustShotClock

                    Call UPDATESCREEN

                    Call FoulCalled(1)

                    If pbpDelay! <> 0 Then
                        If PB% = 0 Then
                            U$ = "3 FT: " + AA$(P, B1(P, B)) + Str$(A!(P, B1(P, B), 1)) + " %"
                            Call PBP
                            Call DELAY
                        Else
                            Call WINDEX
                            Print B$(P)
                            Print "3 FT: "; AA$(P, B1(P, B)); "   "; A!(P, B1(P, B), 1); " %"
                        End If
                    End If

                    X = 50
                    Call DEADBALLOPTIONS

                    For I = 0 To 1
                        If P7(I) = 0 Then PR%(I, 0) = 1: PR%(I, 1) = 7: PC%(I) = 0
                    Next

                    X = 0

                    If pbpDelay! <> 0 Then
                        If PB% = 0 Then
                            U$ = "3 FT: " + AA$(P, B1(P, B)) + Str$(A!(P, B1(P, B), 1)) + " %"
                            Call PBP
                        Else
                            Call WINDEX
                            Print B$(P)
                            Print "3 FT: "; AA$(P, B1(P, B)); "   "; A!(P, B1(P, B), 1); " %"
                        End If

                    End If

                    'Increase FT ATT
                    A!(P, B1(P, B), 9) = A!(P, B1(P, B), 9) + 3

                    '3 FT Attempts
                    For F3 = 1 To 3:

                        Call d100(S)

                        'FT Success Chance
                        FX% = A!(P, B1(P, B), 1)
                        If FA%(P, B1(P, B)) <= -5 Then FX% = Int(FX% * .9 + .5)

                        If S <= FX% Then
                            If PB% = 0 Then
                                Call THREEFT
                            Else
                                Call MADEFT
                            End If
                        Else
                            'S > FX%
                            If PB% = 0 Then
                                Call BADFT
                            Else
                                Call MISSFT
                            End If
                        End If

                    Next F3

                    LT = 0

                    If gameClock! <= 0 Then
                        timeElapsed! = gameClock!
                        Call AdjustPlayTime
                        GoTo 170
                    End If

                    'We already have this exact logic
                    'worked out above !!!
                    If S > FX% Then
                        GoTo 650
                    Else
                        GoTo 598
                    End If

                End If

                If I2 = 43 And TF% = 1 And S <= F3S% Then

                    gameClock! = gameClock! - 4
                    timeElapsed! = 4
                    Call AdjustPlayTime
                    Call AdjustShotClock
                    Call UPDATESCREEN
                    Call GoodBasket
                    U$ = "3 PT BASKET GOOD BY " + AA$(P, B1(P, B))
                    Call SELECTPBP

                    Call GoodBasket

                    U$ = "AND HE WAS FOULED !!!"
                    If PB% = 0 Then Call PBP
                    If PB% = 1 And pbpDelay! <> 0 Then Print U$: Call DELAY

                    F3 = 1
                    S!(P, H) = S!(P, H) + 3
                    S!(P, 0) = S!(P, 0) + 3
                    A!(P, B1(P, B), 8) = A!(P, B1(P, B), 8) + 1
                    W%(P, B1(P, B), 0) = W%(P, B1(P, B), 0) + 1
                    Call UPDATESCREEN

                    Call FoulCalled(1)

                    GoTo 619

                End If

                If S > F3S% Then

                    If PB% = 1 And pbpDelay! <> 0 Then Call WINDEX: Print "3 PT FG MISSED BY "; AA$(P, B1(P, B)): Call DELAY
                    If PB% = 0 Then PT% = 5: TP% = 1: Call MAKEPBP: Call PBP

                    Call ResetShotClock

                    gameClock! = gameClock! - 4
                    timeElapsed! = 4
                    Call AdjustPlayTime

                    Call AdjustShotClock

                    If gameClock! <= 0 Then
                        timeElapsed! = gameClock!
                        Call AdjustPlayTime
                        TP% = 0
                        GoTo 170
                    End If

                    Call UPDATESCREEN

                    GoTo 670

                Else

                    If PB% = 1 And pbpDelay! <> 0 Then Call WINDEX: Print "3 PT FG GOOD BY "; AA$(P, B1(P, B)); " !!!": Call DELAY
                    If PB% = 0 Then PT% = 4: Call MAKEPBP: Call PBP

                    369 If XX% = 1 Then
                        XX% = 0: S!(P, H) = S!(P, H) + 2: S!(P, 0) = S!(P, 0) + 2: W%(P, B1(P, B), 1) = W%(P, B1(P, B), 1) - 1
                    Else
                        S!(P, H) = S!(P, H) + 3: S!(P, 0) = S!(P, 0) + 3: W%(P, B1(P, B), 0) = W%(P, B1(P, B), 0) + 1
                    End If

                    A!(P, B1(P, B), 8) = A!(P, B1(P, B), 8) + 1
                    Call GoodBasket
                    Call UPDATESCREEN

                    GoTo 562

                End If

            End If

    End Select

    '----- NOODLE -----

    570 '
    If pbpDelay! <> 0 Then
        If PB% = 0 Then
            U$ = "1+1 FT: " + AA$(P, B1(P, B)) + Str$(A!(P, B1(P, B), 1)) + " %"
            Call PBP
        Else
            Call WINDEX
            Print B$(P)
            Print "1+1 FT: "; AA$(P, B1(P, B)); " "; A!(P, B1(P, B), 1); " %"
        End If

    End If

    X = 50
    Call DEADBALLOPTIONS

    For I = 0 To 1
        If P7(I) = 0 Then PR%(I, 0) = 1: PR%(I, 1) = 7: PC%(I) = 0
    Next

    X = 0

    If pbpDelay! <> 0 Then

        If PB% = 0 Then
            U$ = "1+1 FT: " + AA$(P, B1(P, B)) + Str$(A!(P, B1(P, B), 1)) + " %"
            Call PBP
        Else
            Call WINDEX
            Print B$(P)
            Print "1+1 FT: "; AA$(P, B1(P, B)); "   "; A!(P, B1(P, B), 1); " %"
        End If

    End If

    '2 FT Attempts
    For F3 = 1 To 2

        'Increase FT ATT
        A!(P, B1(P, B), 9) = A!(P, B1(P, B), 9) + 1

        Call d100(S)

        'FT Success Chance
        FX% = A!(P, B1(P, B), 1)
        If FA%(P, B1(P, B)) <= -5 Then FX% = Int(FX% * .9 + .5)

        If S <= FX% Then

            If PB% = 0 Then
                Call THREEFT
            Else
                'PB% = 1 Then
                Call MADEFT
            End If
        Else
            'S > FX% And
            If PB% = 0 Then
                Call BADFT
            Else
                'PB% = 1
                Call MISSFT
            End If

        End If

        If F3 = 1 And S > A!(P, B1(P, B), 1) Then Exit For

    Next F3

    575 If gameClock! <= 0 Then 170

    If S > FX% Then 650

    PB$ = ""
    If P = 1 Then
        P = 0
        D = 1
        P9 = 0
        C1 = 2
    Else
        'P = 0
        P = 1
        D = 0
        P9 = 1
        C1 = 2
    End If

    X = 0
    U$ = B$(P) + " BALL O.B. AFTER SCORE"
    Call SELECTPBP
    Locate 8, 41: Print Space$(12)
    Call DELAY
    Call STOPPAGE
    Call PRESSGUARD

    GoTo 300

    '----- NOODLE -----

    580 '
    Call LASTSHOT
    Call ResetShotClock

    PB$ = ""
    If P = 1 Then
        P = 0
        D = 1
        P9 = 0
        C1 = 2
    Else
        'P = 0
        P = 1
        D = 0
        P9 = 1
        C1 = 2
    End If

    Call FoulCalled(1)

    B = W5

    If P2(D) > 6 Then 570

    If gameClock! <= 0 Then 170
    U$ = "OFFENSIVE FOUL"
    Call SELECTPBP

    U$ = B$(P) + " BALL O.B. AFTER FOUL"
    If PB% = 0 Then Call PBP
    If PB% = 1 And pbpDelay! <> 0 Then Print U$: Call DELAY

    Locate 8, 41: Print Space$(12): B = 6
    Call DELAY
    T0(D) = T0(D) + 1
    Call UPDATESCREEN
    Call DEADBALLOPTIONS

    For I = 0 To 1
        If P7(I) = 0 Then PR%(I, 0) = 1: PR%(I, 1) = 7: PC%(I) = 0
    Next

    Call PRESSGUARD

    GoTo 300

    '----- NOODLE -----

    640 '
    PB$ = ""

    If T1(P) > 0 Then
        T1(P) = T1(P) - 1
        Call SCOREBOARD

        RN = Int(Rnd(1) * 100)
        If RN <= 8 Then TF%(P) = 10
        If RN > 8 And RN <= 16 Then TF%(P) = 5

        RN = Int(Rnd(1) * 100)
        If RN <= 8 Then
            TF%(D) = -10
        Else
            If RN <= 16 Then TF%(D) = -5
        End If

        gameClock! = gameClock! + M5
        timeElapsed! = -M5
        Call AdjustPlayTime
        If sClockVal >= 1 Then shotClock = shotClock + M5
        '
        For I = 0 To 1
            For I1 = 0 To 1
                PR%(I, I1) = 0
            Next
        Next

        U$ = "TIME OUT " + B$(P)
        Call SELECTPBP
        Call DELAY
        Call DEADBALLOPTIONS

        For I = 0 To 1
            For I1 = 0 To 13
                FA%(I, I1) = FA%(I, I1) + 1
                If FA%(I, I1) > 0 Then FA%(I, I1) = 0
            Next
        Next

    End If

    If A6 = 0 Then Call PRESSGUARD: GoTo 300

    GoTo 350

    '----- NOODLE -----

    650 A6 = 1
    P6 = 0
    A4 = 0
    D6 = 0
    Call ResetShotClock
    S = 0

    Call d100(R0!)

    If H > 1 And gameClock! < 30 And S!(D, 0) - S!(P, 0) = 2 And R0! <= 12 Or H > 1 And gameClock! < 30 And S!(P, 0) - S!(D, 0) = 0 And R0! <= 12 Then
        Call OFFREBOUND
        GoTo 760
    End If

    Call d100(R0!)

    Select Case R0!
        Case 1 To 11
            FF% = 1
            GoTo 707
        Case 12 To 19
            Call OFFREBOUND
            GoTo 760
        Case 20 To 100
            Call DEFREBOUND
            GoTo 800
    End Select

    '----- NOODLE -----

    670 A6 = 1
    P6 = 0
    A4 = 0
    D6 = 0
    Call ResetShotClock

    '--- loop
    700 S = 0
    A4 = 0
    D6 = 0

    Call d100(R0!)
    GG = S!(D, 0) - S!(P, 0)

    If H > 1 And gameClock! < 30 And (GG = 2 Or GG = 0 Or GG = -2) And R0! <= 12 Then 700
    '--- loop

    If TP% = 1 And R0! <= 19 Then TP% = 0

    Select Case R0!
        Case 0 To 1

            725 If X = 98 Then 700
            T0(P) = T0(P) + 1
            U$ = "OFFENSIVE GOAL TENDING"
            Call SELECTPBP

            U$ = "BALL O.B. TO " + B$(D)
            If PB% = 0 Then Call PBP
            If PB% = 1 And pbpDelay! <> 0 Then Print U$: Call DELAY

            Call DELAY

            PB$ = ""
            If P = 1 Then
                P = 0
                D = 1
                P9 = 0
                C1 = 2
            Else
                'P = 0
                P = 1
                D = 0
                P9 = 1
                C1 = 2
            End If

            Call DEADBALLOPTIONS
            For I = 0 To 1
                If P7(I) = 0 Then PR%(I, 0) = 1: PR%(I, 1) = 7: PC%(I) = 0
            Next

            GoTo 300

        Case 2 To DY
            FF% = 1

        Case DY + 1 To 17

            Call d100(R0!)

            If R0! > 35 Then

                R3!(D) = R3!(D) + 1: Call UPDATESCREEN
                U$ = "RB KNOCKED O.B. TO " + B$(D)
                Call SELECTPBP
                Call DELAY
                Call DEADBALLOPTIONS

                For I = 0 To 1
                    If P7(I) = 0 Then PR%(I, 0) = 1: PR%(I, 1) = 7: PC%(I) = 0
                Next

                PB$ = ""
                If P = 1 Then
                    P = 0
                    D = 1
                    P9 = 0
                    C1 = 2
                Else
                    'P = 0
                    P = 1
                    D = 0
                    P9 = 1
                    C1 = 2
                End If

                C1 = 2
                P9 = P

                GoTo 300

            Else

                752 R3!(P) = R3!(P) + 1: Call UPDATESCREEN
                U$ = "RB KNOCKED O.B. TO " + B$(P)
                Call SELECTPBP
                Call DELAY
                Call DEADBALLOPTIONS

                For I = 0 To 1
                    If P7(I) = 0 Then PR%(I, 0) = 1: PR%(I, 1) = 7: PC%(I) = 0
                Next

                LT = 4

                GoTo 520

            End If

        Case 18 To 19

            If X = 99 Then 700

            Call OFFREBOUND

            A!(P, B1(P, B), 7) = A!(P, B1(P, B), 7) + 1
            A!(P, B1(P, B), 8) = A!(P, B1(P, B), 8) + 1
            A!(P, B1(P, B), 11) = A!(P, B1(P, B), 11) + 1
            R3!(P) = R3!(P) + 1: S!(P, H) = S!(P, H) + 2
            S!(P, 0) = S!(P, 0) + 2
            Call UPDATESCREEN

            If PB% = 1 And pbpDelay! <> 0 Then Call WINDEX: Print "TIPPED IN BY "; AA$(P, B1(P, B)): Print "   BASKET GOOD !!!": Call DELAY
            If PB% = 0 Then U$ = AA$(P, B1(P, B)) + " TIPS IT IN!!": Call PBP

            PB$ = ""
            If P = 1 Then
                P = 0
                D = 1
                P9 = 0
                C1 = 2
            Else
                'P = 0
                P = 1
                D = 0
                P9 = 1
                C1 = 2
            End If

            U$ = B$(P) + " BALL O.B. AFTER SCORE"
            Locate 8, 41: Print Space$(12)

            If PB% = 0 Then Call PBP
            If PB% = 1 And pbpDelay! <> 0 Then Print U$: Call DELAY

            Call DELAY
            Call STOPPAGE
            Call PRESSGUARD

            GoTo 300

        Case 20
            If X = 99 Then 700

            U$ = "DEFENSIVE GOAL TENDING"
            Call SELECTPBP

            If TP% = 1 Then
                TP% = 0

                If XX% = 1 Then
                    XX% = 0
                    S!(P, H) = S!(P, H) + 2
                    S!(P, 0) = S!(P, 0) + 2
                    W%(P, B1(P, B), 1) = W%(P, B1(P, B), 1) - 1
                Else
                    S!(P, H) = S!(P, H) + 3
                    S!(P, 0) = S!(P, 0) + 3
                    W%(P, B1(P, B), 0) = W%(P, B1(P, B), 0) + 1
                End If

                A!(P, B1(P, B), 8) = A!(P, B1(P, B), 8) + 1
                Call GoodBasket
                Call UPDATESCREEN

                GoTo 562

            Else

                GoTo 560

            End If

    End Select

    'Essentially this is Case 21 to 100
    '   It's not written as such because
    '   case 2 to DY will end up here too
    707 '
    TP% = 0
    RB = 0

    For I = 0 To 9: RB(I) = 0: Next

    For I = 0 To 4
        XZ = A!(P, B1(P, I), 21)

        If Not (H > 2 Or H = 2 And gameClock! <= 120) Then
            If A!(P, B1(P, I), 3) - A!(P, B1(P, I), 7) - A!(P, B1(P, I), 11) - A!(P, B1(P, I), 12) <= 0 Then XZ = Int(XZ * .5 + .5)
        End If

        If FA%(P, B1(P, I)) <= -5 Then XZ = Int(XZ * .9 + .5)

        RB(I) = RB(I) + XZ: RB = RB + XZ
    Next

    ORB = RB

    For I = 0 To 4
        XZ = A!(1 - P, B1(1 - P, I), 2)
        If H > 2 Or H = 2 And gameClock! <= 120 Then 778
        If A!(1 - P, B1(1 - P, I), 3) - A!(1 - P, B1(1 - P, I), 7) - A!(1 - P, B1(1 - P, I), 11) - A!(1 - P, B1(1 - P, I), 12) <= 0 Then XZ = Int(XZ * .5 + .5)
        778 If FA%(1 - P, B1(1 - P, I)) <= -5 Then XZ = Int(XZ * .9 + .5)
        RB(I + 5) = RB(I + 5) + XZ: RB = RB + XZ
    Next

    DRB = RB - ORB

    If D2(D) = 0 Then XQ = Int((RB * .06) + .5)
    If D2(D) = 1 Then XQ = Int((RB * .12) + .5)
    If D2(D) = 2 Then XQ = Int((RB * .1) + .5)
    If D2(D) = 3 Then XQ = Int((RB * .14) + .5)
    If CZ%(P) = 1 Then XQ = Int((RB * .17) + .5)

    RN = Rnd(1) * RB
    If RN <= ORB + XQ Then

        RN = Rnd(1) * ORB
        For B = 0 To 4
            If RN <= RB(B) Then 760
            RB(B + 1) = RB(B + 1) + RB(B)
        Next

        B = Int(Rnd(1) * 5)

        GoTo 760

    End If

    RN = Rnd(1) * DRB
    skipRandom% = 0

    For B = 5 To 9

        If RN <= RB(B) Then
            B = B - 5
            skipRandom% = 1
            Exit For
        Else
            RB(B + 1) = RB(B + 1) + RB(B)
        End If

    Next

    If skipRandom% = 0 Then
        B = Int(Rnd(1) * 5)
    End If

    800 '
    If FF% = 1 Then

        FF% = 0

        If pbpDelay! <> 0 Then
            If PB% = 1 Then
                Call WINDEX
                Print AA$(D, B1(D, B)); " HAS THE DEF RB FOR"
                Print B$(D); " AND IS FOULED !!!"
                Call DELAY
            Else
                U$ = AA$(D, B1(D, B)) + " HAS THE DEF RB"
                Call PBP
                U$ = "HE'S FOULED !!!"
                Call PBP
            End If
        End If

        R3!(D) = R3!(D) + 1
        A!(D, B1(D, B), 11) = A!(D, B1(D, B), 11) + 1

        Call UPDATESCREEN

        PB$ = ""
        If P = 1 Then
            P = 0
            D = 1
            P9 = 0
            C1 = 2
        Else
            'P = 0
            P = 1
            D = 0
            P9 = 1
            C1 = 2
        End If

        Call FoulCalled(1)
        LT = 4

        GoTo 9065

    End If

    A!(D, B1(D, B), 11) = A!(D, B1(D, B), 11) + 1
    R3!(D) = R3!(D) + 1
    Call UPDATESCREEN
    U$ = AA$(D, B1(D, B)) + " HAS THE DEF RB"

    If PB% = 0 And pbpDelay! <> 0 Then Call PBP
    If PB% = 1 And pbpDelay! <> 0 Then Call WINDEX: Print U$: Print "FOR " + B$(D): Call DELAY

    PB$ = AA$(D, B1(D, B))
    C1 = 2
    P9 = D

    PB$ = ""
    If P = 1 Then
        P = 0
        D = 1
        P9 = 0
        C1 = 2
    Else
        'P = 0
        P = 1
        D = 0
        P9 = 1
        C1 = 2
    End If

    FB% = 1
    PB$ = AA$(P, B1(P, B))

    R0! = Int(Rnd(1) * 130) + 1

    If R0! = 15 Then FB% = 0: GoTo RBTIEUP

    Call d100(RN0)
    If CZ%(D) = 1 And RN0 <= 50 Then

        gameClock! = gameClock! - 4
        timeElapsed! = 4
        Call AdjustPlayTime
        Call AdjustShotClock
        Call WHOLAY

        GoTo LAYUP

    Else

        GoTo 300

    End If

    '----- NOODLE -----

    760 '
    If FF% = 1 Then

        FF% = 0

        U$ = AA$(P, B1(P, B)) + " HAS THE OFF RB"
        If PB% = 0 Then Call PBP
        If PB% = 1 And pbpDelay! <> 0 Then Call WINDEX: Print U$: Print "FOR "; B$(P): Call DELAY

        U$ = "HE'S FOULED !!!"
        If PB% = 0 Then Call PBP
        If PB% = 1 And pbpDelay! <> 0 Then Print U$: Call DELAY

        A!(P, B1(P, B), 11) = A!(P, B1(P, B), 11) + 1
        A!(P, B1(P, B), 24) = A!(P, B1(P, B), 24) + 1
        R3!(P) = R3!(P) + 1

        Call UPDATESCREEN
        Call FoulCalled(1)
        LT = 4

        GoTo 9065

    End If

    A!(P, B1(P, B), 24) = A!(P, B1(P, B), 24) + 1
    A!(P, B1(P, B), 11) = A!(P, B1(P, B), 11) + 1
    R3!(P) = R3!(P) + 1
    Call UPDATESCREEN

    U$ = AA$(P, B1(P, B)) + " HAS THE OFF RB"
    If PB% = 0 And pbpDelay! <> 0 Then Call PBP
    If PB% = 1 And pbpDelay! <> 0 Then Call WINDEX: Print U$: Print "FOR " + B$(P): Call DELAY

    If gameClock! <= 0 Then

        GoTo 170

    Else

        R0! = Int(Rnd(1) * 130) + 1
        PB$ = AA$(P, B1(P, B))

        If R0! = 32 Then
            GoTo RBTIEUP
        Else
            R0! = Int(Rnd(1) * 10) + 1
            Call SHOTPERCENT

            If Not (H > 2 Or H = 2 And gameClock! <= 120) Then
                If A!(P, B1(P, B), 3) - A!(P, B1(P, B), 7) - A!(P, B1(P, B), 11) - A!(P, B1(P, B), 12) <= 0 Then F! = Int(F! * .6 + .5)
            End If

            If R0! <= 8 Then
                PZ% = 9
                P6 = 1
                GoTo 537
            Else
                GoTo 520
            End If

        End If

    End If

    '----- NOODLE -----

    TIEUP:
    If PB% <> 1 Then

        Call d100(R0!)

        Select Case R0!
            Case 1 To 20
                U$ = "BALL IS SLAPPED AWAY"
            Case 21 To 40
                U$ = "BALL IS LOOSE ON THE FLOOR"
            Case 41 To 60
                U$ = AA$(P, B1(P, B)) + " IS TRAPPED"
            Case 61 To 80
                U$ = "TRIES TO WORK LOOSE"
            Case 81 To 100
                U$ = AA$(P, B1(P, B)) + " IS PINNED ON THE LINE"
        End Select

        Call PBP

    End If

    RBTIEUP:
    Call d100(R0!)
    P9 = Int(Rnd(1) * 5)

    If PB% = 1 And pbpDelay! <> 0 Then
        Call WINDEX
        Print "JUMP BALL!"
        Call DELAY
    Else
        Select Case R0!
            Case 1 To 25
                U$ = AA$(P, B1(P, B)) + " IS TIED UP BY " + AA$(D, B1(D, P9))
            Case 26 To 50
                U$ = AA$(D, B1(D, P9)) + " TIES UP " + AA$(P, B1(P, B))
            Case 51 To 75
                U$ = "HELD BALL!"
            Case 76 To 100
                U$ = "JUMP BALL!"
        End Select

        Call PBP

    End If

    U$ = "POSSESSION ARROW TO " + B$(PA%)
    If PB% = 0 Then Call PBP
    If PB% = 1 And pbpDelay! <> 0 Then Print U$: Call DELAY

    Call DELAY

    If PA% = D Then
        T0(P) = T0(P) + 1
        Call DEADBALLOPTIONS

        For I = 0 To 1
            If P7(I) = 0 Then PR%(I, 0) = 1: PR%(I, 1) = 7: PC%(I) = 0
        Next

        PB$ = ""
        If P = 1 Then
            P = 0
            D = 1
            P9 = 0
            C1 = 2
        Else
            'P = 0
            P = 1
            D = 0
            P9 = 1
            C1 = 2
        End If

        C1 = 2
        P9 = P
        PA% = 1 - PA%

        GoTo 300

    Else

        PA% = 1 - PA%
        Call DEADBALLOPTIONS
        For I = 0 To 1
            If P7(I) = 0 Then PR%(I, 0) = 1: PR%(I, 1) = 7: PC%(I) = 0
        Next
        LT = 4

        GoTo 350

    End If

    '----- NOODLE -----

    9065

    If IN% = 1 Or (TB% = 1 And P2(D) >= 10) Then

        If pbpDelay! <> 0 Then
            If PB% = 0 Then
                U$ = "2 FT: " + AA$(P, B1(P, B)) + Str$(A!(P, B1(P, B), 1)) + " %"
                Call PBP
                Call DELAY
            Else
                Call WINDEX
                Print B$(P)
                Print "2 FT: "; AA$(P, B1(P, B)); "   "; A!(P, B1(P, B), 1); " %"
            End If
        End If

        X = 50
        Call DEADBALLOPTIONS

        For I = 0 To 1
            If P7(I) = 0 Then PR%(I, 0) = 1: PR%(I, 1) = 7: PC%(I) = 0
        Next

        X = 0

        If pbpDelay! <> 0 Then
            If PB% = 0 Then
                U$ = "2 FT: " + AA$(P, B1(P, B)) + Str$(A!(P, B1(P, B), 1)) + " %"
                Call PBP
            Else
                Call WINDEX
                Print B$(P)
                Print "2 FT: "; AA$(P, B1(P, B)); "   "; A!(P, B1(P, B), 1); " %"
            End If
        End If

        '2 FT Attempts
        For F3 = 1 To 2:

            'Increase FT ATT
            A!(P, B1(P, B), 9) = A!(P, B1(P, B), 9) + 1

            Call d100(S)

            'FT Success Chance
            FX% = A!(P, B1(P, B), 1)
            If FA%(P, B1(P, B)) <= -5 Then FX% = Int(FX% * .9 + .5)

            If S <= FX% Then

                If PB% = 0 Then
                    Call THREEFT
                Else
                    'PB% = 1
                    Call MADEFT
                End If
            Else
                'S > FX% And
                If PB% = 0 Then
                    Call BADFT
                Else
                    'PB% = 1
                    Call MISSFT
                End If

            End If

        Next F3

        If gameClock! <= 0 Then 170

        Call CLEARPBPBOX

        If S > FX% And IN% <> 1 Then 650

        If IN% = 1 Then
            PASS% = 1
            IN% = 0
            X = 0
            U$ = B$(P) + " BALL AFTER INT. FOUL"
            Call SELECTPBP
            Locate 8, 41: Print Space$(12)
            Call DELAY
            Call STOPPAGE
            Call PRESSGUARD
        Else

            PB$ = ""

            If P = 1 Then
                P = 0
                D = 1
                P9 = 0
                C1 = 2
            Else
                'P = 0
                P = 1
                D = 0
                P9 = 1
                C1 = 2
            End If

            X = 0

            U$ = B$(P) + " BALL O.B. AFTER SCORE"
            Locate 8, 41: Print Space$(12)

            Call SELECTPBP
            Call DELAY
            Call STOPPAGE
            Call PRESSGUARD
        End If

        GoTo 300

    Else

        If P2(D) > 6 Then 570

        If gameClock! <= 0 Then 170

        Call DEADBALLOPTIONS

        For I = 0 To 1
            If P7(I) = 0 Then PR%(I, 0) = 1: PR%(I, 1) = 7: PC%(I) = 0
        Next

        If N7% = 0 And PB% = 0 Or N7% = 0 And pbpDelay! = 0 Then
            Call WINDEX
            U$ = B$(P) + " BALL O.B. AFTER FOUL": Call PBP: Locate 8, 41: Print Space$(12)
            GoTo 350
        End If

        If N7% = 0 And PB% = 1 Then
            Call WINDEX
            Print B$(P) + " BALL O.B. AFTER FOUL": Call DELAY: Locate 8, 41: Print Space$(12)
            GoTo 350
        End If

        Call UPDATESCREEN

        U$ = "1 FT: " + AA$(P, B1(P, B)) + Str$(A!(P, B1(P, B), 1)) + " %"
        Call PBP

        X = 50

        Call DEADBALLOPTIONS

        For I = 0 To 1
            If P7(I) = 0 Then PR%(I, 0) = 1: PR%(I, 1) = 7: PC%(I) = 0
        Next

        X = 0

        U$ = "1 FT: " + AA$(P, B1(P, B)) + Str$(A!(P, B1(P, B), 1)) + " %"

        Call PBP

        F3 = 1

        'Increase FT ATT
        A!(P, B1(P, B), 9) = A!(P, B1(P, B), 9) + 1


        FX% = A!(P, B1(P, B), 1)
        If FA%(P, B1(P, B)) <= -5 Then FX% = Int(FX% * .9 + .5)

        Call d100(S)

        If S <= FX% Then
            If PB% = 0 Then
                Call THREEFT
            Else
                'PB% = 1
                Call MADEFT
            End If
        Else
            'S > FX%
            If PB% = 0 Then
                Call BADFT
            Else
                'PB% = 1
                Call MISSFT
            End If
        End If

        LT = 0

        If gameClock! <= 0 Then
            timeElapsed! = gameClock!
            Call AdjustPlayTime
            GoTo 170
        End If

        If S > FX% Then 650

        PB$ = ""
        If P = 1 Then
            P = 0
            D = 1
            P9 = 0
            C1 = 2
        Else
            'P = 0
            P = 1
            D = 0
            P9 = 1
            C1 = 2
        End If

        U$ = B$(P) + " BALL O.B. AFTER SCORE"
        Locate 8, 41: Print Space$(12)

        Call PBP
        Call DELAY
        Call STOPPAGE

        B = 6
        X = 0

        Call PRESSGUARD

        GoTo 300

    End If

End Sub




'INCLUDE: 'GameRoutines.bm'



'----------------------------------------
'            INFO Subroutine
'----------------------------------------
'This subroutine displays a simple header
'and footer for the game options menu.
Sub INFO ()

    Cls

    Color , 1
    Locate 1, 1
    Print Tab(80);

    Locate , 33: Color 14
    Print "GAME OPTIONS MENU"

    Locate 24, 1
    Print Tab(80);
    Locate , 1
    Print "(C) LANCE HAFFNER 1986-2000"; Tab(70); Left$(Time$, 5);
    Locate 3, 1: Color , 0

End Sub


'----------------------------------------
'           InitVar Subroutine
'----------------------------------------
'This suhroutine initializes variables for
'the game. It does not process any that will
' be read in from file or assigned elsewhere
Sub InitVar ()

    Shared scheduleFile$

    Shared d3FG_Adj(), d3FGA_Adj()

    For I = 0 To 1
        For I1 = 0 To 4
            B1(I, I1) = -1
        Next I1
    Next I

    gameClock! = 1200

    nbrLines = 1

    H = 1

    ' # of timeouts
    T1(1) = 5
    T1(0) = 5

    X$(0) = "A "
    X$(1) = "B "
    X$(2) = "C "
    X$(3) = "D "

    PA% = -1

    TR%(0, 0) = 15
    TR%(0, 1) = 1
    TR%(1, 0) = 15
    TR%(1, 1) = 1

    PB$(0) = "VERBOSE        "
    PB$(1) = "HAFFNER CLASSIC"

    AP = 0
    B = 0
    C1 = 0
    DY = 0
    CT = 0
    D = 0
    endGame = 0
    endAllGames = 0
    FO = 0
    F3 = 0
    G9 = 0
    halfTime = 0
    I = 0
    J = 0
    IN = 0
    M5 = 0
    MJ = 0
    NTMS = 0
    P = 0
    P9 = 0
    Q = 0
    shotClock = 0
    statSpecial = 0
    S2 = 0
    S9 = 0
    timeElapsed! = 0
    TMT = 0
    TOU = 0
    XM = 0
    XS = 0

    BO% = 0
    BU% = 0
    DK% = 0
    F3S% = 0
    FB% = 0
    FT% = 0
    J8% = 0
    LC% = 0
    PASS% = 0
    PT% = 0
    PZ% = 0
    X0% = 0
    X1% = 0
    XX% = 0

    skipUserOpts% = 0

    F! = 0
    R0! = 0

    A1$ = ""
    B1$ = ""
    C1$ = ""
    D1$ = ""
    E1$ = ""
    F1$ = ""
    G1$ = ""
    H1$ = ""
    J$ = ""
    PB$ = ""
    scheduleFile$ = ""
    tourneyFile$ = ""
    U$ = ""
    VT$ = ""
    VT1$ = ""
    W$ = ""
    W1$ = ""
    YN$ = ""

    Erase B1
    Erase C, D1, D2, DF, d3FG_Adj, d3FGA_Adj
    Erase F1, G4, G5, P2, P4, P5, P7
    Erase QQ, QR, RB, RS
    Erase S1, S3, T0, V9
    Erase W2, W3, Z5, Z6, timePlayed

    Erase AP%, APT%, B%, CZ%, DT%, E%, FA%, F5%, FY%
    Erase G9%, HT%, L%, N%, NG%, NG1%
    Erase O%, OF%, PR%, ST%, SX%
    Erase T2%, TF%, TM%, TP%, teamIndex%
    Erase VG%, VH%, W%

    Erase AA$, B$
    Erase gameMascot$, gamearenaName$, gameCoach$
    Erase H$, HO$, N$
    Erase PO$, PS$, Q$, SITE$, SX$
    Erase teamNames$, Y$, YN$, YN1$

    Erase A!, R3!, S!, W0!, W1!
    Erase Z!, Z1!, Z2!

    actualAttendance& = 0
    Erase avgAttendance&, CRD&

End Sub


'----------------------------------------
'          AUTOSETUP Subroutine
'----------------------------------------
'This subroutine auto-assigns game param-
'eters from a schedule file's data.
Sub AUTOSETUP ()

    Shared tickerStart

    N = NG%(2)
    X3% = NG%(3)
    pbpDelay! = NG%(4) / 10
    NF% = NG%(5)
    TB% = NG%(6)
    sClockVal = NG%(7)
    U5 = NG%(8)
    coachOpt = NG%(9)
    TF% = NG%(10)
    tickerStart = NG%(12)
    N7% = NG%(14)

    For P9 = 0 To 1

        I1 = NG%(P9)
        YN$ = YN$(P9)

        Call ReadGameTeam(YN$, B$(P9), I1, P9)

        If I1 <> 0 Then
            Call TEAMSETUP
            'Else
            '   -- throw an error message
        End If

    Next P9

    If TOU = 1 Then

        tourneyFile$ = RTrim$(tourneyFile$)
        YN$(0) = RTrim$(YN$(0))
        YN$(1) = RTrim$(YN$(1))

        Open diskPaths$(1) + tourneyFile$ + ".INF" For Input As #2

        Input #2, X$

        Input #2, X$ ' NUMBER OF REGIONS

        For I = 1 To 16
            Input #2, X$ ' NUMBER OF TEAMS IN REGION
            Input #2, X$ ' NAME OF REGION
        Next

        For I = 1 To 16
            For I1 = 1 To 16
                For I2 = 1 To 4
                    Input #2, N$(I, I1, I2) ' SEED SLOT NAME
                    Input #2, N%(I, I1, I2) ' REGION, SEED NUMBER, MODE PLAY/TM#
                Next
            Next
        Next

        Close 2

        TOU = 0

        For I = 1 To 16

            For I1 = 1 To 16
                'PRINT LEN(YN$(0)),LEN(YN$(1)),LEN( N$(I,I1,1) )
                If NG%(0) = N%(I, I1, 0) And YN$(0) = N$(I, I1, 1) And N%(I, I1, 1) = 0 Then
                    TOU = 1
                    exitAll% = 1
                    Exit For
                End If

                If NG%(1) = N%(I, I1, 0) And YN$(1) = N$(I, I1, 1) And N%(I, I1, 1) = 0 Then
                    TOU = 1
                    exitAll% = 1
                    Exit For
                End If
            Next

            If exitAll% = 1 Then Exit For

        Next

    End If

End Sub


'------------------------------
'    LetterToNumber Subroutine
'------------------------------
' ...explanation...
Sub LetterToNumber (promptVal$, J)

    J = Val(promptVal$)
    If UCase$(promptVal$) = "A" Then J = 10
    If UCase$(promptVal$) = "B" Then J = 11
    If UCase$(promptVal$) = "C" Then J = 12
    If UCase$(promptVal$) = "D" Then J = 13
    If UCase$(promptVal$) = "E" Then J = 14

End Sub


'----------------------------------------
'          TEAMSETUP Subroutine
'----------------------------------------
'This subroutine initializes team data,
'sets up player data and configures any
'league adjustments
Sub TEAMSETUP ()

    Shared d3FG_Adj(), d3FGA_Adj()

    F5%(P9, 4) = L%(P9, 0)
    If F5%(P9, 4) > 20 Then F5%(P9, 4) = F5%(P9, 4) - 100

    F5%(P9, 5) = L%(P9, 1)
    If F5%(P9, 5) = 0 Then F5%(P9, 5) = 4

    F5%(P9, 6) = L%(P9, 2)
    If F5%(P9, 6) > 20 Then F5%(P9, 6) = F5%(P9, 6) - 100

    F5%(P9, 7) = L%(P9, 3)
    If F5%(P9, 7) > 20 Then F5%(P9, 7) = F5%(P9, 7) - 100

    F5%(P9, 8) = L%(P9, 4)
    If F5%(P9, 8) <> 99 Then F5%(P9, 4) = 0: F5%(P9, 5) = 4: F5%(P9, 6) = 0: F5%(P9, 7) = 0

    For I = 0 To 13

        A!(P9, I, 16) = C(P9, I, 6)
        If A!(P9, I, 16) = 0 Then A!(P9, I, 16) = 6

        A!(P9, I, 17) = C(P9, I, 5)
        If A!(P9, I, 17) = 0 Then A!(P9, I, 17) = 40

        A!(P9, I, 19) = C(P9, I, 3)
        A!(P9, I, 20) = C(P9, I, 2)
        A!(P9, I, 21) = C(P9, I, 4)
        A!(P9, I, 2) = A!(P9, I, 2)

    Next I

    F5%(P9, 2) = d3FG_Adj(P9)
    If F5%(P9, 2) > 20 Then F5%(P9, 2) = F5%(P9, 2) - 100

    F5%(P9, 3) = d3FGA_Adj(P9)
    If F5%(P9, 3) > 20 Then F5%(P9, 3) = F5%(P9, 3) - 100

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
    Color TR%(teamIdx, 0), TR%(teamIdx, 1)

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

    Locate 8, 8
    Color TR%(teamIdx, 0), TR%(teamIdx, 1)

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
Sub d100 (randNumVar)

    randNumVar = Int(Rnd(1) * 100) + 1

End Sub


'----------------------------------------
'       DISABLEPLAYERS Subroutine
'----------------------------------------
'This subroutine gives the user an inter-
'face for disabling team players. It will
'then take care of updating the associated
'arrays for the available roster.
Sub DISABLEPLAYERS ()

    Do
        Cls

        Call TEAMBOX(1, P9)

        U$ = "DISABLE PLAYERS FOR " + B$(P9) + "? (Y/N)"
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
        Color TR%(P9, 0), TR%(P9, 1): Print B$(P9)
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
            If AA$(P9, I) = "XXX" Then B%(P9, I) = -1: Color TR%(P9, 0), TR%(P9, 1)

            'Color players who have been disabled
            If B%(P9, I) = 1 Or B%(P9, I) = -1 Then Color TR%(P9, 0), TR%(P9, 1)

            'Print player name
            Print AA$(P9, I);
            Color 15, 0

            'Print player position, GP
            Print Tab(20); PO$(P9, I); Tab(22); C(P9, I, 0)

        Next

        Do
            Locate 19, 1
            Print "ENTER PLAYER # TO DISABLE/ENABLE (Z=FINISHED)"
            Print "YOU MUST HAVE AT LEAST 8 ACTIVE PLAYERS"

            I$ = GetKeyPress$

            If UCase$(I$) <> "Z" Then

                I1 = Val(I$)
                Call LetterToNumber(I$, I1)

                If B%(P9, I1) = 0 And DX% < 4 Then
                    B%(P9, I1) = -1
                    DX% = DX% + 1
                    Locate 3 + I1, 3
                    Color TR%(P9, 0), TR%(P9, 1): Print AA$(P9, I1)
                    Color 15, 0
                Else
                    If B%(P9, I1) = -1 Then
                        B%(P9, I1) = 0
                        DX% = DX% - 1
                        Locate 3 + I1, 2
                        Color 15, 0
                        Print " "; AA$(P9, I1)
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
Sub PBP ()

    i$ = InKey$

    If UCase$(i$) = "Q" Then End

    If pbpDelay! <> 0 And PB% <> 1 Then

        U$ = PadRight$(U$, 38)

        Locate 8, 1
        If nbrLines = 1 Then A1$ = U$: VG%(1) = TR%(P, 0): VH%(1) = TR%(P, 1)
        If nbrLines = 2 Then B1$ = U$: VG%(2) = TR%(P, 0): VH%(2) = TR%(P, 1)
        If nbrLines = 3 Then C1$ = U$: VG%(3) = TR%(P, 0): VH%(3) = TR%(P, 1)
        If nbrLines = 4 Then D1$ = U$: VG%(4) = TR%(P, 0): VH%(4) = TR%(P, 1)
        If nbrLines = 5 Then E1$ = U$: VG%(5) = TR%(P, 0): VH%(5) = TR%(P, 1)
        If nbrLines = 6 Then F1$ = U$: VG%(6) = TR%(P, 0): VH%(6) = TR%(P, 1)
        If nbrLines = 7 Then G1$ = U$: VG%(7) = TR%(P, 0): VH%(7) = TR%(P, 1)
        If nbrLines = 8 Then H1$ = U$: VG%(8) = TR%(P, 0): VH%(8) = TR%(P, 1)

        If nbrLines > 8 Then

            A1$ = B1$
            B1$ = C1$
            C1$ = D1$
            D1$ = E1$
            E1$ = F1$
            F1$ = G1$
            G1$ = H1$
            H1$ = U$

            For I = 1 To 7
                VG%(I) = VG%(I + 1)
                VH%(I) = VH%(I + 1)
            Next

            VG%(8) = TR%(P, 0)
            VH%(8) = TR%(P, 1)

        End If

        nbrLines = nbrLines + 1

        Color VG%(1), VH%(1): Print Left$(A1$, 38)
        Color VG%(2), VH%(2): Print Left$(B1$, 38)
        Color VG%(3), VH%(3): Print Left$(C1$, 38)
        Color VG%(4), VH%(4): Print Left$(D1$, 38)
        Color VG%(5), VH%(5): Print Left$(E1$, 38)
        Color VG%(6), VH%(6): Print Left$(F1$, 38)
        Color VG%(7), VH%(7): Print Left$(G1$, 38)
        Color VG%(8), VH%(8): Print Left$(H1$, 38)

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
Sub SELECTPBP ()

    Select Case PB%

        Case 0:
            Call PBP

        Case 1:
            If pbpDelay! <> 0 Then
                Call WINDEX
                Print U$
                Call DELAY
            End If

    End Select

End Sub


'----------------------------------------
'           MAKEPBP Subroutine
'----------------------------------------
'This subroutine helps build strings for
'the Play-by-Play text, depending on the
'context given (assigned as PT%)
Sub MAKEPBP ()

    If pbpDelay! <> 0 Then

        RN = Int(Rnd(1) * 100) + 1

        Select Case PT%
            Case 1
                'TURNOVER
                U$ = AA$(P, B1(P, B))

                If RN > 35 And RN <= 68 And P7(D) > 1 And PASS% = 1 Then
                    U$ = B$(P) + " CALLED FOR 10 SEC VIOLATION"
                Else

                    RN = Int(Rnd(1) * 14) + 1
                    Select Case RN
                        Case 1
                            U$ = U$ + " TURNS THE BALL OVER"
                        Case 2
                            U$ = U$ + " THROWS IT AWAY"
                        Case 3
                            U$ = U$ + " IS WHISTLED FOR TRAVELING"
                        Case 4
                            U$ = U$ + " TURNS IT OVER"
                        Case 5
                            U$ = U$ + " LOSES CONTROL OF THE BALL"
                        Case 6
                            U$ = U$ + " LOSES IT OUT OF BOUNDS"
                        Case 7
                            U$ = U$ + " LOSES THE HANDLE"
                        Case 8
                            U$ = U$ + " LOSES IT"
                        Case 9
                            U$ = U$ + " IS CALLED FOR STEPS"
                        Case 10
                            U$ = U$ + " TRAVELS"
                        Case 11
                            U$ = U$ + " WALKS WITH IT"
                        Case 12
                            U$ = U$ + " LETS IT GET AWAY"
                        Case 13
                            U$ = U$ + " DOUBLE DRIBBLES"
                        Case 14
                            U$ = U$ + " LOSES CONTROL"
                    End Select

                End If

            Case 2
                'DUNK
                RN = Int(Rnd(1) * 39) + 1
                Select Case RN
                    Case 1
                        U$ = "DIPSY-DOO DUNK-A-ROO!!!"
                    Case 2
                        U$ = AA$(P, B1(P, B)) + " MAKES IT HAMMER TIME!!!"
                    Case 3
                        U$ = AA$(P, B1(P, B)) + " SLAMS IT IN!!!"
                    Case 4
                        U$ = AA$(P, B1(P, B)) + " WITH THE 2-HANDED SLAM!!!"
                    Case 5
                        U$ = AA$(P, B1(P, B)) + " SLAMS IT HOME!!!"
                    Case 6
                        U$ = AA$(P, B1(P, B)) + " JAMS IT HOME!!!"
                    Case 7
                        U$ = AA$(P, B1(P, B)) + " THROWS IT DOWN!!!"
                    Case 8
                        U$ = AA$(P, B1(P, B)) + " THUNDERSLAM!!!"
                    Case 9
                        U$ = "RIM ROCKING JAM!!!"
                    Case 10
                        U$ = AA$(P, B1(P, B)) + " WITH THE MONSTER SLAM!!"
                    Case 11
                        U$ = AA$(P, B1(P, B)) + "POWER SLAMS IT!!!"
                    Case 12
                        U$ = AA$(P, B1(P, B)) + " WITH THE 2-HANDED STUFF!!!"
                    Case 13
                        U$ = AA$(P, B1(P, B)) + " BACKWARDS JAM!!!"
                    Case 14
                        U$ = AA$(P, B1(P, B)) + " WITH THE 1-HANDED SLAM!!!"
                    Case 15
                        U$ = AA$(P, B1(P, B)) + " WITH THE REVERSE SLAM!!!"
                    Case 16
                        U$ = AA$(P, B1(P, B)) + " WITH THE REVERSE JAM!!!"
                    Case 17
                        U$ = AA$(P, B1(P, B)) + " WITH THE WINDMILL JAM!!!"
                    Case 18
                        U$ = AA$(P, B1(P, B)) + " WITH THE WINDMILL SLAM!!!"
                    Case 19
                        U$ = AA$(P, B1(P, B)) + " WITH THE TOMAHAWK SLAM!!!"
                    Case 20
                        U$ = AA$(P, B1(P, B)) + " WITH THE 360 SLAM!!!"
                    Case 21
                        U$ = AA$(P, B1(P, B)) + " WITH THE SPINNING SLAM!!!"
                    Case 22
                        U$ = AA$(P, B1(P, B)) + " WITH THE LEFT HANDED DUNK!!!"
                    Case 23
                        U$ = "DOUBLE CLUTCH JAM!!!"
                    Case 24
                        U$ = AA$(P, B1(P, B)) + " GETS THE FLUSH!!!"
                    Case 25
                        U$ = AA$(P, B1(P, B)) + " PUNCHES IT HOME!!!"
                    Case 26
                        U$ = AA$(P, B1(P, B)) + "...DOUBLE PUMP SLAM!!!"
                    Case 27
                        U$ = AA$(P, B1(P, B)) + "...IN YOUR FACE SLAM!!!"
                    Case 28
                        U$ = AA$(P, B1(P, B)) + " IS UP": Call PBP: U$ = "HE SPINS": Call PBP: U$ = "BEHIND THE BACK SLAM!!!"
                    Case 29
                        U$ = "HE SLAMS IT HOME!!"
                    Case 30
                        U$ = "HE LAYS IT UP AND IN!!"
                    Case 31
                        U$ = "HE BANGS IT IN!!"
                    Case 32
                        U$ = "HE RAMS IT IN!!"
                    Case 33
                        U$ = "HE SLAMS IT IN!!!"
                    Case 34
                        U$ = "DIPSY-DOO DUNK-A-ROO!!!"
                    Case 35
                        U$ = "HE MAKES IT HAMMER TIME!!!"
                    Case 36
                        U$ = AA$(P, B1(P, B)) + " WITH THE 2-HANDED SLAM!!!"
                    Case 37
                        U$ = AA$(P, B1(P, B)) + " IS UP": Call PBP: U$ = "HE SPINS": Call PBP: U$ = "BEHIND THE BACK SLAM!!!"
                    Case 38
                        U$ = "HE JAMS IT HOME!!!"
                    Case 39
                        U$ = "HE HAMMERS IT HOME!"
                End Select

            Case 3
                U$ = AA$(P, B1(P, B))

                Select Case PZ%
                    Case 1
                        'BAD
                        Select Case RN
                            Case 1 To 40
                                U$ = U$ + " TAKES A TOUGH SHOT"
                            Case 40 To 45
                                U$ = U$ + " SHOOTS OFF BALANCE"
                            Case 46 To 50
                                U$ = U$ + " FORCES ONE UP"
                            Case 51 To 60
                                U$ = U$ + " SHOOTS UNDER PRESSURE"
                            Case 61 To 65
                                U$ = U$ + " WITH A BAD SHOT"
                            Case 66 To 70
                                U$ = U$ + " SHOOTS IN HEAVY TRAFFIC"
                            Case 71 To 75
                                U$ = U$ + " THROWS UP A PRAYER"
                            Case 76 To 80
                                U$ = U$ + " TAKES A BAD SHOT"
                            Case 81 To 85
                                U$ = U$ + " FORCES THE INSIDE SHOT"
                            Case 86 To 100
                                U$ = U$ + " TURNS AND SHOOTS"
                        End Select

                    Case 2
                        'INSIDE W/ASSIST
                        Select Case RN
                            Case 1 To 10
                                U$ = "HE CUTS TO THE BASKET": Call PBP: U$ = "TAKES IT UP"

                                If (B = 0 Or B = 1) And Int(Rnd(1) * 100) + 1 <= 1 Then DK% = 1
                                If (B = 2 Or B = 3) And Int(Rnd(1) * 100) + 1 <= 5 Then DK% = 1
                                If B = 4 And Int(Rnd(1) * 100) + 1 <= 7 Then DK% = 1

                            Case 11 To 20
                                U$ = "HE TAKES IT TO THE HOLE"
                                If (B = 0 Or B = 1) And Int(Rnd(1) * 100) + 1 <= 1 Then DK% = 1
                                If (B = 2 Or B = 3) And Int(Rnd(1) * 100) + 1 <= 5 Then DK% = 1
                                If B = 4 And Int(Rnd(1) * 100) + 1 <= 7 Then DK% = 1

                            Case 21 To 30
                                U$ = "HE TAKES IT TO THE HOOP"
                                If (B = 0 Or B = 1) And Int(Rnd(1) * 100) + 1 <= 1 Then DK% = 1
                                If (B = 2 Or B = 3) And Int(Rnd(1) * 100) + 1 <= 5 Then DK% = 1
                                If B = 4 And Int(Rnd(1) * 100) + 1 <= 7 Then DK% = 1

                            Case 31 To 40
                                U$ = "HE LEANS IN AND SHOOTS"

                            Case 41 To 50
                                U$ = "HE PUTS IT UP"

                            Case 51 To 60
                                U$ = "HE TAKES IT TO THE BOARD"
                                If (B = 0 Or B = 1) And Int(Rnd(1) * 100) + 1 <= 1 Then DK% = 1
                                If (B = 2 Or B = 3) And Int(Rnd(1) * 100) + 1 <= 5 Then DK% = 1
                                If B = 4 And Int(Rnd(1) * 100) + 1 <= 7 Then DK% = 1

                            Case 61 To 70
                                U$ = "HE PUTS IT OFF THE GLASS"

                            Case 71 To 80
                                If B = 4 Then
                                    U$ = "HE SHOOTS THE HOOK"
                                Else
                                    'B <> 4
                                    U$ = "HE GOES IN FOR THE LAYUP"
                                End If

                            Case 81 To 90
                                U$ = "HE TAKES IT UP STRONG"
                                If (B = 0 Or B = 1) And Int(Rnd(1) * 100) + 1 <= 1 Then DK% = 1
                                If (B = 2 Or B = 3) And Int(Rnd(1) * 100) + 1 <= 5 Then DK% = 1
                                If B = 4 And Int(Rnd(1) * 100) + 1 <= 7 Then DK% = 1

                            Case 91 To 95
                                U$ = "HE POWERS IT UP"
                                If (B = 0 Or B = 1) And Int(Rnd(1) * 100) + 1 <= 1 Then DK% = 1
                                If (B = 2 Or B = 3) And Int(Rnd(1) * 100) + 1 <= 5 Then DK% = 1
                                If B = 4 And Int(Rnd(1) * 100) + 1 <= 7 Then DK% = 1

                            Case 96 To 100
                                U$ = "HE LAYS IT UP"

                        End Select

                    Case 3
                        'PENETRATE
                        Select Case B
                            Case 0, 1
                                Select Case RN
                                    Case 1 To 25
                                        U$ = "HE SPINS FREE"
                                        Call PBP
                                        U$ = "SHOOTS"
                                    Case 26 To 50
                                        U$ = "HE TAKES IT UP UNDER THE BASKET"
                                        If Int(Rnd(1) * 100) + 1 <= 2 Then DK% = 1
                                    Case 51 To 52
                                        U$ = "HE MAKES A NICE SPIN MOVE"
                                        Call PBP
                                        U$ = "AND SOARS TO THE HOOP"
                                        If Int(Rnd(1) * 100) + 1 <= 2 Then DK% = 1
                                    Case 53 To 54
                                        U$ = "HE BLOWS PAST TWO DEFENDERS"
                                    Case 55 To 56
                                        U$ = "HE DRIVES THE BASELINE"
                                        Call PBP
                                        U$ = "SPINS RIGHT AND GOES UP"
                                        If Int(Rnd(1) * 100) + 1 <= 2 Then DK% = 1
                                    Case 57
                                        U$ = "HE SLICES IN THE HOLE"
                                        Call PBP
                                        U$ = "UNDERHAND SCOOP SHOT"
                                    Case 58
                                        U$ = "STRONG CUT ALONG THE BASELINE"
                                        Call PBP
                                        U$ = "DOUBLE PUMP REVERSE LAYUP"
                                    Case 59
                                        U$ = "HE PENETRATES"
                                        Call PBP
                                        U$ = "GORGEOUS FINGER ROLL!"
                                    Case 60 To 75
                                        U$ = "HE TAKES THE JUMPER"
                                    Case 76 To 90
                                        U$ = "HE STOPS"
                                        Call PBP
                                        U$ = "SHOOTS"
                                    Case 91
                                        U$ = "HE LOOKS TO PASS"
                                        Call PBP
                                        U$ = "NO ONE IS OPEN"
                                        Call PBP
                                        U$ = "HE PUTS IT UP"
                                    Case 92 To 100
                                        U$ = "HE TAKES THE SHOT"
                                End Select

                            Case 2, 3
                                Select Case RN
                                    Case 1 To 16
                                        U$ = "HE TAKES IT UP"
                                        If Int(Rnd(1) * 100) + 1 <= 7 Then DK% = 1
                                    Case 17 To 32
                                        U$ = "HE SPINS FREE"
                                        Call PBP
                                        U$ = "SHOOTS"
                                        If Int(Rnd(1) * 100) + 1 <= 7 Then DK% = 1
                                    Case 33 To 48
                                        U$ = "HE GETS LOOSE IN THE PAINT"
                                        Call PBP
                                        U$ = "TAKES IT UP"
                                        If Int(Rnd(1) * 100) + 1 <= 7 Then DK% = 1
                                    Case 49 To 64
                                        U$ = "HE PUTS IT UP"
                                    Case 65 To 67
                                        U$ = "HE CURLS OFF A PICK IN THE PAINT"
                                        Call PBP
                                        U$ = "TAKES IT UP"
                                        If Int(Rnd(1) * 100) + 1 <= 7 Then DK% = 1
                                    Case 68, 69
                                        U$ = "HE MAKES A NICE SPIN MOVE"
                                        Call PBP
                                        U$ = "AND SOARS TO THE HOOP"
                                        If Int(Rnd(1) * 100) + 1 <= 7 Then DK% = 1
                                    Case 70, 71
                                        U$ = "HE DRIVES THE BASELINE"
                                        Call PBP
                                        U$ = "SPINS RIGHT AND GOES UP"
                                        If Int(Rnd(1) * 100) + 1 <= 7 Then DK% = 1
                                    Case 72
                                        U$ = "HE SLICES IN THE HOLE"
                                        Call PBP
                                        U$ = "UNDERHAND SCOOP SHOT"
                                    Case 73
                                        U$ = "STRONG CUT ALONG THE BASELINE"
                                        Call PBP
                                        U$ = "DOUBLE PUMP REVERSE LAYUP"
                                    Case 74
                                        U$ = "HE PENETRATES"
                                        Call PBP
                                        U$ = "GORGEOUS FINGER ROLL!"
                                    Case 75 To 80
                                        U$ = "HE SHOOTS"
                                    Case 81 To 100
                                        U$ = "HE TAKES THE SHOT"
                                End Select

                            Case 4

                                Select Case RN
                                    Case 1 To 15
                                        U$ = "HE PUTS IT OFF THE GLASS"
                                    Case 16 To 25
                                        U$ = "HE PIVOTS TO THE BASKET"
                                    Case 26 To 40
                                        U$ = "HE POWERS IT UP"
                                    Case 41 To 58
                                        U$ = "HE TAKES IT TO THE HOOP"
                                        If Int(Rnd(1) * 100) + 1 <= 15 Then DK% = 1
                                    Case 59 To 70
                                        U$ = "HE TAKES THE SHOT"
                                    Case 71, 72
                                        U$ = "HE CURLS OFF A PICK IN THE PAINT"
                                        Call PBP
                                        U$ = "TAKES IT UP"
                                        If Int(Rnd(1) * 100) + 1 <= 15 Then DK% = 1
                                    Case 73, 74
                                        U$ = "HE MAKES A NICE SPIN MOVE"
                                        Call PBP
                                        U$ = "AND SOARS TO THE HOOP"
                                        If Int(Rnd(1) * 100) + 1 <= 15 Then DK% = 1
                                    Case 75 To 100
                                        U$ = "HE TAKES IT UP STRONG"
                                        If Int(Rnd(1) * 100) + 1 <= 15 Then DK% = 1
                                End Select

                        End Select

                    Case 4
                        'THREE!
                        If FT% = 1 And RN <= 50 Then U$ = U$ + " THROWS UP A PRAYER"
                        If FT% = 1 And RN > 50 Then U$ = U$ + " LAUNCHES A THREE"

                        If FT% = 1 Then
                            FT% = 0
                        Else
                            Select Case RN
                                Case 1 To 10
                                    U$ = U$ + " TRIES FOR THREE!"
                                Case 11 To 20
                                    U$ = U$ + " SHOOTS FROM 3-POINT RANGE!"
                                Case 21 To 30
                                    U$ = U$ + " HOISTS A THREE!"
                                Case 31 To 40
                                    U$ = U$ + " BOMBS A THREE FROM THE CORNER!"
                                Case 41 To 50
                                    U$ = U$ + " FIRES FROM THE 3-POINT LINE!"
                                Case 51 To 60
                                    U$ = U$ + " SHOOTS FOR THREE!"
                                Case 61 To 70
                                    U$ = U$ + " SHOOTS THE 3-POINTER!"
                                Case 71 To 80
                                    U$ = U$ + " FOR 3...!"
                                Case 81 To 90
                                    U$ = U$ + " PUTS UP THE TREY!"
                                Case 91 To 100
                                    U$ = U$ + " TAKES THE 3-POINTER!"
                            End Select

                        End If

                    Case 5
                        'SHORT JUMP
                        DK% = 0

                        RN = Int(Rnd(1) * 59) + 1
                        Select Case RN
                            Case 1
                                U$ = U$ + " WITH A TWISTING JUMPER"
                            Case 2
                                U$ = U$ + " SHOOTS THE J"
                            Case 3
                                If B = 4 Then
                                    U$ = U$ + " RELEASES THE HOOK"
                                Else
                                    'B <> 4
                                    U$ = U$ + " TAKES THE SHOT"
                                End If
                            Case 4
                                U$ = U$ + " WITH A SHORT JUMPER"
                            Case 5
                                U$ = U$ + " WITH THE 10 FOOTER"
                            Case 6
                                If B = 4 Then
                                    U$ = U$ + " WITH THE RUNNING HOOK"
                                Else
                                    'B <> 4
                                    U$ = U$ + " WITH THE RUNNER"
                                End If

                            Case 7
                                If B = 4 Then
                                    U$ = U$ + " WITH A SWEEPING HOOK"
                                Else
                                    'B <> 4
                                    U$ = U$ + " WORKS FREE AND SHOOTS"
                                End If

                            Case 8
                                If B = 4 Then
                                    U$ = U$ + " WITH A BABY HOOK SHOT"
                                Else
                                    'B <> 4
                                    U$ = U$ + " TAKES THE SHOT"
                                End If

                            Case 9
                                U$ = U$ + " LEANS IN AND SHOOTS"
                            Case 10
                                U$ = U$ + " SHOOTS"
                            Case 11
                                U$ = U$ + " SHOOTS FALLING AWAY"
                            Case 12
                                U$ = U$ + " LEANS IN FOR THE SHOT"
                            Case 13
                                U$ = U$ + " TAKES THE LEFT HANDER"
                            Case 14
                                U$ = U$ + " HARD TO THE HOLE..."
                            Case 15
                                U$ = U$ + " FLIPS IT UP"
                            Case 16
                                U$ = U$ + " FORCES THE INSIDE SHOT"
                            Case 17
                                U$ = U$ + " CUTS TO THE BASKET"
                            Case 18
                                U$ = U$ + " TAKES IT IN THE PAINT"
                            Case 19
                                U$ = U$ + " FLOATS IT OVER HIS MAN"
                            Case 20
                                U$ = U$ + " TAKES HIS MAN INSIDE"
                            Case 21
                                U$ = U$ + " WHEELS IN THE LANE"
                            Case 22
                                U$ = U$ + " CUTS INSIDE FOR THE SHOT"
                            Case 23
                                U$ = U$ + " PENETRATES AND SHOOTS"
                            Case 24
                                U$ = U$ + " SLICES INSIDE"
                            Case 25
                                U$ = U$ + " LOOKS FOR TWO"
                            Case 26
                                U$ = U$ + " DRIVES INTO THE PAINT"
                            Case 27
                                U$ = U$ + " HAS THE BASELINE JUMPER"
                            Case 28
                                U$ = U$ + " GLIDES TO THE HOLE"
                            Case 29
                                U$ = U$ + " GETS INSIDE FOR THE SHOT"
                            Case 30
                                U$ = U$ + " SPINS TOWARD THE HOOP"
                            Case 31
                                U$ = U$ + " FROM SHORT RANGE"
                            Case 32
                                U$ = U$ + " GOES BASELINE"
                            Case 33
                                U$ = U$ + " PENETRATES AND SHOOTS"
                            Case 34
                                U$ = U$ + " FROM 8 FEET"
                            Case 35
                                U$ = U$ + " FROM 6 FEET"
                            Case 36
                                U$ = U$ + " FROM 5 FEET"
                            Case 37
                                U$ = U$ + " DRIVES TO THE HOLE"
                            Case 38
                                U$ = U$ + " GLIDES TO THE HOLE"
                            Case 39
                                U$ = U$ + " GETS INSIDE FOR THE SHOT"
                            Case 40
                                U$ = U$ + " GOES TO THE HOLE"
                            Case 41
                                U$ = U$ + " CUTS INSIDE"
                            Case 42
                                U$ = U$ + " SPINS INSIDE"
                            Case 43
                                U$ = U$ + " BREAKS INSIDE"
                            Case 44
                                U$ = U$ + " SNEAKS INSIDE"
                            Case 45
                                U$ = U$ + " SCOOTS INSIDE"
                            Case 46
                                U$ = U$ + " WORKS IT INSIDE"
                            Case 47
                                U$ = U$ + " KNIFES INSIDE"
                            Case 48
                                U$ = U$ + " SCISSORS THROUGH"
                            Case 49
                                U$ = U$ + " BANGS INSIDE"
                            Case 50
                                U$ = U$ + " SLICES INSIDE"
                            Case 51
                                U$ = U$ + " PUMPS AND SHOOTS"
                            Case 52
                                U$ = U$ + " DOUBLE PUMPS AND SHOOTS"
                            Case 53
                                U$ = U$ + "...ONE HANDED FADE AWAY"
                            Case 54
                                U$ = U$ + "...LEFT HANDED FADE AWAY"
                            Case 55
                                U$ = U$ + "...HIGH ARCHING SHOT"
                            Case 56
                                U$ = U$ + " WITH A RUNNING LEFT HANDER"
                            Case 57
                                U$ = U$ + " WITH A RUNNING SCOOP SHOT"
                            Case 58
                                U$ = U$ + " WITH THE SCOOP SHOT"
                            Case 59
                                U$ = U$ + " WITH THE FINGER ROLL"
                        End Select

                    Case 6
                        DK% = 0

                        RN = Int(Rnd(1) * 59) + 1

                        Select Case RN
                            Case 1
                                U$ = U$ + " POPS FROM OUTSIDE"
                            Case 2
                                U$ = U$ + " DECIDES TO SHOOT IT"
                            Case 3
                                U$ = U$ + " FIRES THE FADE-AWAY"
                            Case 4
                                U$ = U$ + " LETS IT FLY"
                            Case 5
                                U$ = U$ + " LAUNCHES ONE"
                            Case 6
                                U$ = U$ + " WITH THE OUTSIDE SHOT"
                            Case 7
                                U$ = U$ + " OUT OF THE CORNER"
                            Case 8
                                U$ = U$ + " PUTS IT UP FROM 18"
                            Case 9
                                U$ = U$ + " SPOTS UP OUTSIDE"
                            Case 10
                                U$ = U$ + " RELEASES THE JUMPER"
                            Case 11
                                U$ = U$ + " FROM THE TOP OF THE KEY"
                            Case 12
                                U$ = U$ + " FIRES FROM THE LEFT WING"
                            Case 13
                                U$ = U$ + " TRIES THE 12 FOOTER"
                            Case 14
                                U$ = U$ + " PULLS UP AND POPS"
                            Case 15
                                U$ = U$ + " FIRES FROM 15"
                            Case 16
                                U$ = U$ + " FROM THE PERIMETER"
                            Case 17
                                U$ = U$ + " TURNS AND SHOOTS"
                            Case 18
                                U$ = U$ + " SHOOTS OFF THE SCREEN"
                            Case 19
                                U$ = U$ + " FAKES AND SHOOTS"
                            Case 20
                                U$ = U$ + " FIRES AWAY"
                            Case 21
                                U$ = U$ + " POPS FROM THE BASELINE"
                            Case 22
                                U$ = U$ + " DRIVES THE LANE"
                            Case 23
                                U$ = U$ + " TAKES THE SHOT"
                            Case 24
                                U$ = U$ + " WORKS FREE AND FIRES"
                            Case 25
                                U$ = U$ + " SHOOTS OVER HIS MAN"
                            Case 26
                                U$ = U$ + " SHOOTS FALLING AWAY"
                            Case 27
                                U$ = U$ + " FAKES AND SHOOTS"
                            Case 28
                                U$ = U$ + " FIRES AWAY"
                            Case 29
                                U$ = U$ + " POPS FROM THE BASELINE"
                            Case 30
                                U$ = U$ + " DRIVES THE LANE"
                            Case 31
                                U$ = U$ + " TAKES THE SHOT"
                            Case 32
                                U$ = U$ + " WORKS FREE AND FIRES"
                            Case 33
                                U$ = U$ + " SHOOTS FROM 15"
                            Case 34
                                U$ = U$ + " SHOOTS"
                            Case 35
                                U$ = U$ + " TAKES THE JUMPER"
                            Case 36
                                U$ = U$ + " FROM THE RIGHT CORNER"
                            Case 37
                                U$ = U$ + " FROM THE LEFT CORNER"
                            Case 38
                                U$ = U$ + " FROM THE KEY"
                            Case 39
                                U$ = U$ + " FIRES FROM OUTSIDE"
                            Case 40
                                U$ = U$ + " PUTS IT UP FROM 16"
                            Case 41
                                U$ = U$ + " FROM THE PERIMETER"
                            Case 42
                                U$ = U$ + " FAKES AND SHOOTS"
                            Case 43
                                U$ = U$ + " FROM AROUND 16 FEET"
                            Case 44
                                U$ = U$ + " PUTS IT UP"
                            Case 45
                                U$ = U$ + " WITH A RUNNING ONE-HANDER"
                            Case 46
                                U$ = U$ + " PULLS UP"
                            Case 47
                                U$ = U$ + " KISSES THE GLASS"
                            Case 48
                                U$ = U$ + " SPINS AND SHOOTS"
                            Case 49
                                U$ = U$ + " PUTS IT UP FROM 16"
                            Case 50
                                U$ = U$ + " WITH A LONG JUMPER"
                            Case 51
                                U$ = U$ + " PUMPS AND SHOOTS"
                            Case 52
                                U$ = U$ + " DOUBLE PUMPS AND SHOOTS"
                            Case 53
                                U$ = U$ + "...ONE HANDED FADE AWAY"
                            Case 54
                                U$ = U$ + "...LEFT HANDED FADE AWAY"
                            Case 55
                                U$ = U$ + "...HIGH ARCHING SHOT"
                            Case 56
                                U$ = U$ + " WITH A RUNNING LEFT HANDER"
                            Case 57
                                U$ = U$ + " WITH A RUNNING SCOOP SHOT"
                            Case 58
                                U$ = U$ + " WITH THE SCOOP SHOT"
                            Case 59
                                U$ = U$ + " WITH THE FINGER ROLL"
                        End Select

                    Case 7
                        DK% = 0
                        Select Case RN
                            Case 1 To 10
                                U$ = U$ + " FIRES FROM THE PERIMETER"
                            Case 11 To 20
                                U$ = U$ + " SHOOTS FROM 17"
                            Case 21 To 30
                                U$ = U$ + " PUTS IT UP FROM 17"
                            Case 31 To 40
                                U$ = U$ + " PUTS IT UP"
                            Case 41 To 45
                                U$ = U$ + " FROM INSIDE THE THE KEY"
                            Case 46 To 50
                                U$ = U$ + " FIRES FROM THE CORNER"
                            Case 51 To 60
                                U$ = U$ + " SHOOTS FROM OUTSIDE"
                            Case 61 To 70
                                U$ = U$ + " PUMPS AND SHOOTS"
                            Case 71 To 80
                                U$ = U$ + " FIRES THE FADE-AWAY"
                            Case 81 To 90
                                If X3% = 1 Then U$ = U$ + " FROM INSIDE THE 3-POINT LINE"
                            Case 91
                                If X3% = 1 Then U$ = U$ + " FOR THREE!": Call PBP: U$ = "NO!": Call PBP: U$ = "HIS FOOT IS ON THE 3-POINT LINE"
                            Case 92 To 100
                                U$ = U$ + " SHOOTS FROM THE WING"
                        End Select

                    Case 8
                        Select Case RN
                            Case 1 To 20
                                U$ = "HE SAILS TO THE HOOP"
                            Case 21 To 23
                                U$ = "HE TAKES IT TO THE BUCKET"
                            Case 24 To 26
                                U$ = "HE TAKES IT TO THE HOLE"
                            Case 27 To 29
                                U$ = "HE SLASHES TO THE HOLE"
                            Case 30 To 32
                                U$ = "HE SLASHES TO THE BUCKET"
                            Case 33 To 35
                                U$ = "HE SOARS TO THE BUCKET"
                            Case 36 To 38
                                U$ = "HE SOARS TO THE HOLE"
                            Case 39 To 40
                                U$ = "HE GLIDES TO THE HOLE"
                            Case 41 To 60
                                U$ = "HE GOES IN STRONG"
                            Case 61 To 80
                                U$ = "HE TAKES IT IN"
                            Case 81 To 90
                                U$ = "HE TAKES A DEFENDER WITH HIM"
                            Case 91 To 100
                                U$ = "HE GOES UP WITH IT"
                        End Select

                    Case 9
                        'BACK AFTER REB
                        U$ = "HE "

                        RN = Int(Rnd(1) * 11) + 1
                        Select Case RN
                            Case 1
                                U$ = U$ + "TAKES IT BACK UP"
                            Case 2
                                U$ = U$ + "GOES BACK UP WITH IT"
                            Case 3
                                U$ = U$ + "POWERS HIS WAY UP WITH IT"
                            Case 4
                                U$ = U$ + "GOES STRONG TO THE BASKET"
                            Case 5
                                U$ = U$ + "MUSCLES IT UP"
                            Case 6
                                U$ = U$ + "SHOOTS"
                            Case 7
                                U$ = U$ + "TAKES IT TO THE GOAL"
                            Case 8
                                U$ = U$ + "PUTS IT UP AGAIN"
                            Case 9
                                U$ = U$ + "SHOOTS IT AGAIN"
                            Case 10
                                U$ = U$ + "CLAWS HIS WAY UP"
                            Case 11
                                U$ = U$ + "SHOOTS IN TRAFFIC"
                        End Select

                        If B = 2 Or B = 3 And Int(Rnd(1) * 100) + 1 <= 7 Then DK% = 1
                        If B = 4 And Int(Rnd(1) * 100) + 1 <= 15 Then DK% = 1

                    Case 10
                        DK% = 0

                        RN = Int(Rnd(1) * 100) + 1
                        Select Case RN
                            Case 1 To 20
                                U$ = U$ + " SHOOTS FROM 15"
                            Case 21 To 40
                                U$ = U$ + " SHOOTS"
                            Case 41 To 60
                                U$ = U$ + " TAKES THE JUMPER"
                            Case 61 To 80
                                U$ = U$ + " PUTS IT UP FROM 16"
                            Case 81 To 100
                                U$ = U$ + " FAKES AND SHOOTS"
                        End Select

                End Select

            Case 4
                'THREE GOOD
                U$ = "HE "

                Select Case RN
                    Case 1 To 6
                        U$ = U$ + " DRAINS THE THREE!"
                        Call PBP
                        U$ = "NO!"
                        Call PBP
                        U$ = "HIS FOOT IS ON THE 3-POINT LINE"
                        XX% = 1
                    Case 7 To 10
                        U$ = U$ + "FILLS UP THE BASKET!!!"
                    Case 11 To 20
                        U$ = U$ + "HITS THE TREY!!!"
                    Case 21 To 30
                        U$ = "IT'S GOOOODD!!"
                    Case 31 To 40
                        U$ = U$ + "GETS IT!!!"
                    Case 41 To 45
                        U$ = "NOTHING BUT THE BOTTOM OF THE NET"
                    Case 46 To 50
                        U$ = "COUNT IT!!"
                    Case 51 To 55
                        U$ = "WAY DOWNTOWN BANG!!!"
                    Case 56 To 60
                        U$ = U$ + "RINGS IT UP!!!"
                    Case 61 To 80
                        U$ = U$ + "DRAINS IT!!!"
                    Case 81 To 90
                        U$ = "HE BURIES IT!!!"
                    Case 91 To 100
                        U$ = "IT'S ALL NET!!!"
                End Select

            Case 5
                'THREE BAD
                U$ = "HE "

                Select Case RN
                    Case 1 To 20
                        U$ = U$ + "GETS NONE"
                    Case 21 To 25
                        U$ = "IT NEVER HAD A CHANCE"
                    Case 26 To 35
                        U$ = "AIRRRR BALLLL!!"
                    Case 36 To 40
                        U$ = U$ + "MISFIRES"
                    Case 41 To 60
                        U$ = "IT'S OFF THE RIM...NO GOOD!"
                    Case 61 To 80
                        U$ = "IT'S SHORT"
                    Case 81 To 100
                        U$ = U$ + "MISSES"
                End Select

            Case 6
                'FAST BREAK
                U$ = AA$(P, B1(P, B))

                RN = Int(Rnd(1) * 38) + 1
                Select Case RN
                    Case 1
                        U$ = U$ + " HAS IT ON THE FAST BREAK!"
                    Case 2
                        U$ = U$ + " IS IN TRANSITION!"
                    Case 3
                        U$ = U$ + " STARTS THE BREAK!"
                    Case 4
                        U$ = U$ + " IS DOWN COURT IN A HURRY!"
                    Case 5
                        U$ = U$ + " IS ON THE RUN!"
                    Case 6
                        U$ = U$ + " GOES COAST TO COAST!"
                    Case 7
                        U$ = U$ + " ON THE BREAK!"
                    Case 8
                        U$ = U$ + " ON THE RUN!"
                    Case 9
                        U$ = U$ + " ON THE FAST BREAK!"
                    Case 10
                        U$ = U$ + " HAS THE FAST BREAK!"
                    Case 11
                        U$ = U$ + " BREAKS INTO THE FORECOURT!"
                    Case 12
                        U$ = U$ + " FLIES UPCOURT!"
                    Case 13
                        U$ = U$ + " STARTS THE FASTBREAK!"
                    Case 14
                        U$ = U$ + " ON THE MOVE!"
                    Case 15
                        U$ = U$ + " IN TRANSITION!"
                    Case 16
                        U$ = U$ + " DOWN COURT IN A HURRY!"
                    Case 17
                        U$ = U$ + " CHARGES ACROSS THE LINE!"
                    Case 18
                        U$ = U$ + " BREAKS QUICKLY!"
                    Case 19
                        U$ = U$ + " BREAKS DOWN COURT!"
                    Case 20
                        U$ = U$ + " STARTS THE RUNNING GAME!"
                    Case 21
                        U$ = U$ + " FLIES DOWN THE FLOOR!"
                    Case 22
                        U$ = U$ + " GOES RIGHT TO WORK!"
                    Case 23
                        U$ = U$ + " BACK THE OTHER WAY!"
                    Case 24
                        U$ = U$ + " QUICKLY UP THE FLOOR!"
                    Case 25
                        U$ = U$ + " PUSHES IT UP COURT!"
                    Case 26
                        U$ = U$ + " WASTES NO TIME!"
                    Case 27
                        U$ = U$ + " UPS THE TEMPO!"
                    Case 28
                        U$ = U$ + " HUSTLES DOWN COURT!"
                    Case 29
                        U$ = U$ + " RUSHES UP THE FLOOR!"
                    Case 30
                        U$ = U$ + " ON THE MOVE!"
                    Case 31
                        U$ = U$ + " STREAKS UP THE COURT!"
                    Case 32
                        U$ = U$ + " FLIES DOWN THE COURT!"
                    Case 33
                        U$ = U$ + " JETS DOWN COURT!"
                    Case 34
                        U$ = U$ + " QUICKLY GETS IT UP COURT!"
                    Case 35
                        U$ = U$ + " TAKES IT COAST TO COAST!"
                    Case 36
                        U$ = U$ + " IS RUNNING AND GUNNING!"
                    Case 37
                        U$ = U$ + " FLIES OVER THE TIME LINE!"
                    Case 38
                        U$ = U$ + " RUSHES IT UP THE FLOOR!"
                End Select

                Call SHOTSTRING

            Case 7
                'FAST BREAK SCORE
                FB% = 0

                RN = Int(Rnd(1) * 39) + 1
                Select Case RN
                    Case 1
                        U$ = "DIPSY-DOO DUNK-A-ROO!!!"
                    Case 2
                        U$ = AA$(P, B1(P, B)) + " MAKES IT HAMMER TIME!!!"
                    Case 3
                        U$ = AA$(P, B1(P, B)) + " SLAMS IT IN!!!"
                    Case 4
                        U$ = AA$(P, B1(P, B)) + " WITH THE 2-HANDED SLAM!!!"
                    Case 5
                        U$ = AA$(P, B1(P, B)) + " SLAMS IT HOME!!!"
                    Case 6
                        U$ = AA$(P, B1(P, B)) + " JAMS IT HOME!!!"
                    Case 7
                        U$ = AA$(P, B1(P, B)) + " THROWS IT DOWN!!!"
                    Case 8
                        U$ = AA$(P, B1(P, B)) + " THUNDERSLAM!!!"
                    Case 9
                        U$ = "RIM ROCKING JAM!!!"
                    Case 10
                        U$ = AA$(P, B1(P, B)) + " WITH THE MONSTER SLAM!!"
                    Case 11
                        U$ = AA$(P, B1(P, B)) + "POWER SLAMS IT!!!"
                    Case 12
                        U$ = AA$(P, B1(P, B)) + " WITH THE 2-HANDED STUFF!!!"
                    Case 13
                        U$ = AA$(P, B1(P, B)) + " BACKWARDS JAM!!!"
                    Case 14
                        U$ = AA$(P, B1(P, B)) + " WITH THE 1-HANDED SLAM!!!"
                    Case 15
                        U$ = AA$(P, B1(P, B)) + " WITH THE REVERSE SLAM!!!"
                    Case 16
                        U$ = AA$(P, B1(P, B)) + " WITH THE REVERSE JAM!!!"
                    Case 17
                        U$ = AA$(P, B1(P, B)) + " WITH THE WINDMILL JAM!!!"
                    Case 18
                        U$ = AA$(P, B1(P, B)) + " WITH THE WINDMILL SLAM!!!"
                    Case 19
                        U$ = AA$(P, B1(P, B)) + " WITH THE TOMAHAWK SLAM!!!"
                    Case 20
                        U$ = AA$(P, B1(P, B)) + " WITH THE 360 SLAM!!!"
                    Case 21
                        U$ = AA$(P, B1(P, B)) + " WITH THE SPINNING SLAM!!!"
                    Case 22
                        U$ = AA$(P, B1(P, B)) + " WITH THE LEFT HANDED DUNK!!!"
                    Case 23
                        U$ = "DOUBLE CLUTCH JAM!!!"
                    Case 24
                        U$ = AA$(P, B1(P, B)) + " GETS THE FLUSH!!!"
                    Case 25
                        U$ = AA$(P, B1(P, B)) + " PUNCHES IT HOME!!!"
                    Case 26
                        U$ = AA$(P, B1(P, B)) + "...DOUBLE PUMP SLAM!!!"
                    Case 27
                        U$ = AA$(P, B1(P, B)) + "...IN YOUR FACE SLAM!!!"
                    Case 28
                        U$ = AA$(P, B1(P, B)) + " IS UP": Call PBP: U$ = "HE SPINS": Call PBP: U$ = "BEHIND THE BACK SLAM!!!"
                    Case 29
                        U$ = "HE SLAMS IT HOME!!"
                    Case 30
                        U$ = "HE LAYS IT UP AND IN!!"
                    Case 31
                        U$ = "HE BANGS IT IN!!"
                    Case 32
                        U$ = "HE RAMS IT IN!!"
                    Case 33
                        U$ = "HE SLAMS IT IN!!!"
                    Case 34
                        U$ = "DIPSY-DOO DUNK-A-ROO!!!"
                    Case 35
                        U$ = "HE MAKES IT HAMMER TIME!!!"
                    Case 36
                        U$ = AA$(P, B1(P, B)) + " WITH THE 2-HANDED SLAM!!!"
                    Case 37
                        U$ = AA$(P, B1(P, B)) + " IS UP": Call PBP: U$ = "HE SPINS": Call PBP: U$ = "BEHIND THE BACK SLAM!!!"
                    Case 38
                        U$ = "HE JAMS IT HOME!!!"
                    Case 39
                        U$ = "HE HAMMERS IT HOME!"
                End Select

            Case 8
                'MISS
                If PZ% = 8 Then

                    PZ% = 0
                    FB% = 0
                    1475 U$ = "HE "

                    Select Case RN

                        Case 1 To 20
                            U$ = U$ + "MISSES THE EASY BASKET!"
                        Case 21 To 40
                            U$ = U$ + "JAMS IT TOO HARD!!"
                        Case 41 To 60
                            U$ = U$ + "SLAMS IT OFF THE IRON...NO GOOD!"
                        Case 61 To 80
                            U$ = U$ + "CAN'T GET IT DOWN!"
                        Case 81 To 100
                            U$ = U$ + "BLOWS THE EASY BASKET!!"
                    End Select

                Else
                    RN = Int(Rnd(1) * 50) + 1
                    Select Case RN
                        Case 1
                            U$ = "IT RATTLES IN AND OUT...NO GOOD"
                        Case 2
                            U$ = "OFF THE IRON"
                        Case 3
                            U$ = "IT SPINS IN AND OUT"
                        Case 4
                            U$ = "IT'S OFF THE GLASS"
                        Case 5
                            U$ = "IT'S NO GOOD"
                        Case 6
                            U$ = "IT'S OFF THE MARK"
                        Case 7
                            U$ = "IT'S A BRICK"
                        Case 8
                            U$ = "OFF THE RIM"
                        Case 9
                            U$ = "NOT EVEN CLOSE!"
                        Case 10
                            U$ = "NO GOOD"
                        Case 11
                            U$ = "HE MISSES"
                        Case 12
                            U$ = "DOESN'T GET THE ROLL"
                        Case 13
                            U$ = "OFF THE GLASS"
                        Case 14
                            U$ = "FALLS SHORT"
                        Case 15
                            U$ = "NO GOOD"
                        Case 16
                            U$ = "NO"
                        Case 17
                            U$ = "HE MISSES"
                        Case 18
                            U$ = "IN AND OUT"
                        Case 19
                            U$ = "OFF THE RIM"
                        Case 20
                            U$ = "OFF THE IRON"
                        Case 21
                            U$ = "IT'S NOT THERE"
                        Case 22
                            U$ = "OFF TARGET"
                        Case 23
                            U$ = "IT RATTLES OUT"
                        Case 24
                            U$ = "IT DOESN'T FALL"
                        Case 25
                            U$ = "IT BOUNCES OFF"
                        Case 26
                            U$ = "IT ROLLS OFF"
                        Case 27
                            U$ = "SHORT"
                        Case 28
                            U$ = "IT DOESN'T GO DOWN"
                        Case 29
                            U$ = "IT'S NO GOOD"
                        Case 30
                            U$ = "NO GOOD"
                        Case 31
                            U$ = "HE MISSES"
                        Case 32
                            U$ = "IT'S A BRICK"
                        Case 33
                            U$ = "HE FAILS TO CONNECT"
                        Case 34
                            U$ = "OFF THE SIDE OF THE RIM"
                        Case 35
                            U$ = "IT COMES UP SHORT"
                        Case 36
                            U$ = "IT ROLLS OUT"
                        Case 37
                            U$ = "AROUND AND OUT"
                        Case 38
                            U$ = "OFF THE BACK OF THE IRON"
                        Case 39
                            U$ = "ON THE FRONT RIM...ROLLS OFF"
                        Case 40
                            U$ = "IN THE AIR...NO GOOD"
                        Case 41
                            U$ = "TOO SHORT"
                        Case 42
                            U$ = "NO GOOD"
                        Case 43
                            U$ = "IT COMES UP SHORT"
                        Case 44
                            U$ = "IT ROLLS OUT"
                        Case 45
                            U$ = "IN AND OUT"
                        Case 46
                            U$ = "OFF THE FRONT OF THE IRON"
                        Case 47
                            U$ = "ON THE RIM...ROLLS OFF"
                        Case 48
                            U$ = "OFF THE FRONT OF THE RIM"
                        Case 49
                            U$ = "TOO SHORT"
                        Case 50
                            U$ = "TOO STRONG"
                    End Select

                End If

            Case 9
                'NOPASS
                U$ = AA$(P, B1(P, B)) + " "

                RN = Int(Rnd(1) * 56) + 1
                Select Case RN
                    Case 1
                        U$ = U$ + "LOOKS INSIDE"
                    Case 2
                        U$ = U$ + "TRIES TO WORK FREE"
                    Case 3
                        U$ = U$ + "SETS UP THE PLAY"
                    Case 4
                        U$ = U$ + "LOOKS FOR AN OPENING"
                    Case 5
                        U$ = U$ + "DRIBBLES AROUND HIS MAN"
                    Case 6
                        U$ = U$ + "SIGNALS THE PLAY"
                    Case 7
                        U$ = U$ + "IS IN CONTROL"
                    Case 8
                        U$ = U$ + "IS IN POSSESSION"
                    Case 9
                        U$ = U$ + "TRIES TO GET OPEN"
                    Case 10
                        U$ = U$ + "YO-YO'S OUT TOP"
                    Case 11
                        U$ = U$ + "GOES TO WORK"
                    Case 12
                        U$ = U$ + "WORKS OFF THE PICK"
                    Case 13
                        U$ = U$ + "ON THE DRIBBLE"
                    Case 14
                        U$ = U$ + "CUTS TO THE LEFT"
                    Case 15
                        U$ = U$ + "CUTS TO THE RIGHT"
                    Case 16
                        U$ = U$ + "WHEELS AND DEALS"
                    Case 17
                        U$ = U$ + "SETS UP ON TOP"
                    Case 18
                        U$ = U$ + "SLIDES RIGHT"
                    Case 19
                        U$ = U$ + "SLIDES LEFT"
                    Case 20
                        U$ = U$ + "GOES TO WORK"
                    Case 21
                        U$ = U$ + "MOVES IT LEFT-SIDE"
                    Case 22
                        U$ = U$ + "MOVES IT RIGHT-SIDE"
                    Case 23
                        U$ = U$ + "REVERSES THE DRIBBLE"
                    Case 24
                        U$ = U$ + "TRIES TO GET OPEN"
                    Case 25
                        U$ = U$ + "DRIBBLES TO THE MIDDLE"
                    Case 26
                        U$ = U$ + "FAKES THE PASS"
                    Case 27
                        U$ = U$ + "SETS UP ON TOP"
                    Case 28
                        U$ = U$ + "SLIDES RIGHT"
                    Case 29
                        U$ = U$ + "SLIDES LEFT"
                    Case 30
                        U$ = U$ + "DRIBBLES IT BEHIND THE BACK"
                    Case 31
                        U$ = U$ + "FAKES LEFT...GOES RIGHT"
                    Case 32
                        U$ = U$ + "FAKES RIGHT...GOES LEFT"
                    Case 33
                        U$ = U$ + "SPINS HARD TO THE LEFT"
                    Case 34
                        U$ = U$ + "SPINS HARD TO THE RIGHT"
                    Case 35
                        U$ = U$ + "LEANS LEFT...GOES RIGHT"
                    Case 36
                        U$ = U$ + "LEANS RIGHT...GOES LEFT"
                    Case 37
                        U$ = U$ + "LEANS LEFT...CUTS RIGHT"
                    Case 38
                        U$ = U$ + "LEANS RIGHT...CUTS LEFT"
                    Case 39
                        U$ = U$ + "DOUBLE REVERSES HIS DRIBBLE"
                    Case 40
                        U$ = U$ + "BACKS IN"
                    Case 41
                        U$ = U$ + "LEANS FORWARD"
                    Case 42
                        U$ = U$ + "CROSSES OVER...SPINS"
                    Case 43
                        U$ = U$ + "CUTS LEFT"
                    Case 44
                        U$ = U$ + "CUTS RIGHT"
                    Case 45
                        U$ = U$ + "WITH THE HESITATION DRIBBLE": Call PBP: U$ = "HE SPINS LEFT"
                    Case 46
                        U$ = U$ + "WITH THE HESITATION DRIBBLE": Call PBP: U$ = "HE SPINS RIGHT"
                    Case 47
                        U$ = U$ + "WITH THE HESITATION DRIBBLE": Call PBP: U$ = "HE CUTS LEFT"
                    Case 48
                        U$ = U$ + "WITH THE HESITATION DRIBBLE": Call PBP: U$ = "HE CUTS RIGHT"
                    Case 49
                        U$ = U$ + "STUTTER STEPS": Call PBP: U$ = "HE MOVES LEFT"
                    Case 50
                        U$ = U$ + "MAINTAINS HIS DRIBBLE"
                    Case 51
                        U$ = U$ + "FAKE SPINS LEFT...GOES RIGHT"
                    Case 52
                        U$ = U$ + "FAKE SPINS RIGHT...GOES LEFT"
                    Case 53
                        U$ = U$ + "PALMS THE BALL": Call PBP: U$ = "HE TAKES IT AND SPINS"
                    Case 54
                        U$ = U$ + "SHAKES AND BAKES"
                    Case 55
                        U$ = U$ + "DRIBBLES WITH THE LEFT HAND"
                    Case 56
                        U$ = U$ + "DRIBBLES WITH THE RIGHT HAND"
                End Select

            Case 10
                'PASS PB$=PREV HANDLER
                V$ = AA$(P, B1(P, B))

                RN = Int(Rnd(1) * 60) + 1
                Select Case RN
                    Case 1
                        U$ = PB$ + " TO " + V$
                    Case 2
                        U$ = PB$ + " KICKS IT OUT": Call PBP: U$ = V$ + " TAKES THE PASS"
                    Case 3
                        U$ = PB$ + " FEEDS " + V$
                    Case 4
                        U$ = PB$ + " PASSES TO " + V$
                    Case 4
                        U$ = PB$ + " DISHES TO " + V$
                    Case 5
                        U$ = PB$ + " GIVES IT TO " + V$
                    Case 6
                        U$ = PB$ + " WITH A NICE GIVE TO " + V$
                    Case 7
                        U$ = PB$ + " LOOKS": Call PBP: U$ = "HE GETS IT TO " + V$
                    Case 8
                        U$ = PB$ + " SENDS IT TO " + V$
                    Case 9
                        U$ = PB$ + " TO " + V$
                    Case 10
                        U$ = PB$ + " GETS IT TO " + V$
                    Case 11
                        U$ = PB$ + " OVER TO " + V$
                    Case 12
                        U$ = PB$ + " ZIPS IT TO " + V$
                    Case 13
                        U$ = PB$ + " FINDS " + V$
                    Case 14
                        U$ = PB$ + " LOOKS FOR " + V$
                    Case 14
                        U$ = PB$ + " THROWS IT TO " + V$
                    Case 15
                        U$ = PB$ + " GOES TO " + V$
                    Case 16
                        U$ = PB$ + " SETS UP " + V$
                    Case 17
                        U$ = PB$ + " OUT TO " + V$
                    Case 18
                        U$ = PB$ + " FLIPS IT TO " + V$
                    Case 19
                        U$ = "BOUNCE PASS TO " + V$
                    Case 20
                        U$ = PB$ + " LEAVES IT FOR " + V$
                    Case 21
                        U$ = PB$ + " LOOPS IT TO " + V$
                    Case 22
                        U$ = PB$ + " SHOVELS IT OVER TO " + V$
                    Case 23
                        U$ = PB$ + " GIVES IT BACK TO " + V$
                    Case 24
                        U$ = PB$ + " FEEDS IT TO " + V$
                    Case 25
                        U$ = PB$ + " GIVES IT OFF TO " + V$
                    Case 26
                        U$ = "PASSES TO " + V$
                    Case 27
                        U$ = "DISHES TO " + V$
                    Case 28
                        U$ = "GIVES TO " + V$
                    Case 29
                        U$ = "OVER TO " + V$
                    Case 30
                        U$ = "SENDS IT BACK TO " + V$
                    Case 31
                        U$ = "NO-LOOK PASS TO " + V$
                    Case 32
                        U$ = "NO-LOOK BOUNCE PASS TO " + V$
                    Case 33
                        U$ = "BEHIND THE BACK TO " + V$
                    Case 34
                        U$ = PB$ + " SWINGS IT TO " + V$
                    Case 35
                        U$ = PB$ + " FIRES A PASS TO " + V$
                    Case 36
                        U$ = PB$ + " SNAPS A PASS TO " + V$
                    Case 37
                        U$ = PB$ + " SWINGS A PASS TO " + V$
                    Case 38
                        U$ = "TWO-HANDED PASS TO " + V$
                    Case 39
                        U$ = "WRAP AROUND PASS TO " + V$
                    Case 40
                        U$ = "OVER THE SHOULDER PASS TO " + V$
                    Case 41
                        U$ = "BACKHAND PASS TO " + V$
                    Case 42
                        U$ = "BACKHAND DISH TO " + V$
                    Case 43
                        U$ = "PASS CONNECTED TO " + V$
                    Case 44
                        U$ = PB$ + " WIRES IT TO " + V$
                    Case 45
                        U$ = PB$ + " WIRES A PASS TO " + V$
                    Case 46
                        U$ = PB$ + " SNAPS A PASS TO " + V$
                    Case 47
                        U$ = PB$ + " FAKES..PASSES TO " + V$
                    Case 48
                        U$ = PB$ + " SHUFFLES IT TO " + V$
                    Case 49
                        U$ = PB$ + " SLINGS IT TO " + V$
                    Case 50
                        U$ = PB$ + " FLIPS IT TO " + V$
                    Case 51
                        U$ = PB$ + " WHIPS IT OVER TO " + V$
                    Case 52
                        U$ = PB$ + " SLINGS A BOUNCE PASS TO " + V$
                    Case 53
                        U$ = PB$ + " SHOVELS IT TO " + V$
                    Case 54
                        U$ = PB$ + " PUMP FAKES A SHOT"
                        Call PBP
                        U$ = "HE SWINGS IT TO " + V$
                    Case 55
                        U$ = PB$ + " PUMP FAKES A SHOT"
                        Call PBP
                        U$ = "HE BOUNCES IT TO " + V$
                    Case 56
                        U$ = PB$ + " WITH A FOOT FAKE"
                        Call PBP
                        U$ = "HE SPINS AND DUMPS IT TO " + V$
                    Case 57
                        U$ = PB$ + " CUTS ACROSS THE LANE"
                        Call PBP
                        U$ = "HE DISHES INSIDE TO " + V$
                    Case 58
                        U$ = PB$ + " DRIVES THE BASELINE"
                        Call PBP
                        U$ = "DISHES UNDERNEATH TO " + V$
                    Case 59
                        U$ = PB$ + " FAKES RIGHT...CUTS LEFT"
                        Call PBP
                        U$ = "SHUFFLES IT TO " + V$
                    Case 60
                        U$ = PB$ + " HEAD FAKES"
                        Call PBP
                        U$ = "HE PUSHES IT TO " + V$
                End Select

            Case Else
                U$ = AA$(P, B1(P, B))

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

    If pbpDelay! <> 0 Then

        For I = 8 To 15:
            Locate I, 1
            Call BLANKLINE
        Next I

    End If

    Locate 10

End Sub


'----------------------------------------
'          CLEARPBP Subroutine
'----------------------------------------
'This subroutine clears all the strings
'used for each line of PBP display
Sub CLEARPBP ()

    If pbpDelay! <> 0 And PB% <> 1 Then
        nbrLines = 1
        A1$ = ""
        B1$ = ""
        C1$ = ""
        D1$ = ""
        E1$ = ""
        F1$ = ""
        G1$ = ""
        H1$ = ""
        J$ = ""
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

    If AP <> 1 Or U5 <> 2 Then

        Color 14, 0
        Cls

        RN = Int(Rnd(1) * 100) + 1

        If halfTime = 1 Then

            If G9 = 0 Then
                If RN <= 50 Then
                    Locate , 23: Print "PRUDENTIAL AT THE HALF SCOREBOARD"
                Else
                    Locate , 28: Print "RED MAN HALFTIME REPORT"
                End If
            Else
                If RN <= 50 Then
                    Locate , 29: Print "TOYOTA HALFTIME REPORT"
                Else
                    Locate , 29: Print "HONDA HALFTIME REPORT"
                End If
            End If

        Else

            If endGame = 1 Then

                If G9 = 0 Then
                    Locate , 27: Print "CHEVROLET FINAL SCOREBOARD"
                Else
                    Select Case RN
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

        If TMT = 0 Then
            Locate 10, 28: Print "NO OTHER GAMES SCHEDULED"
        Else
            If H = 1 And gameClock! <= 1200 And gameClock! >= 960 Then timeInterval = 1
            If H = 1 And gameClock! < 960 And gameClock! >= 720 Then timeInterval = 2
            If H = 1 And gameClock! < 720 And gameClock! >= 480 Then timeInterval = 3
            If H = 1 And gameClock! < 480 And gameClock! >= 240 Then timeInterval = 4
            If H = 1 And gameClock! < 240 And gameClock! >= 120 Then timeInterval = 5
            If H = 1 And gameClock! < 120 Then timeInterval = 6

            If halfTime = 1 Then timeInterval = 8

            If H = 2 And gameClock! < 1200 And gameClock! >= 960 Then timeInterval = 9
            If H = 2 And gameClock! < 960 And gameClock! >= 720 Then timeInterval = 10
            If H = 2 And gameClock! < 720 And gameClock! >= 480 Then timeInterval = 11
            If H = 2 And gameClock! < 480 And gameClock! >= 240 Then timeInterval = 12
            If H = 2 And gameClock! < 240 And gameClock! >= 120 Then timeInterval = 13
            If H = 2 And gameClock! < 120 Then timeInterval = 14

            '-- Could we not got higher than 16?
            '-- In fact why do we strip things down to 14 anyway?
            If H = 3 And gameClock! <= 1200 And gameClock! >= 960 Then timeInterval = 14
            If H = 3 And gameClock! < 960 And gameClock! >= 720 Then timeInterval = 14
            If H = 3 And gameClock! < 720 And gameClock! >= 480 Then timeInterval = 15
            If H = 3 And gameClock! < 480 And gameClock! >= 240 Then timeInterval = 16
            If H = 3 And gameClock! < 240 Then timeInterval = 17


            If Left$(SX$(1, 0), 3) <> "XXX" Then

                Locate 2, 1
                X = 1
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(1, 0), 13);
                Locate , 15: Print Using "### "; SX%(1, 0, tickInterval);
                Print R$(tickInterval)

                Print Left$(SX$(1, 1), 13);
                Locate , 15: Print Using "### #"; SX%(1, 1, tickInterval)
            End If

            If Left$(SX$(2, 0), 3) <> "XXX" Then
                Locate 5, 1
                X = 2
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(2, 0), 13);
                Locate , 15: Print Using "### "; SX%(2, 0, tickInterval);

                Print R$(tickInterval)

                Print Left$(SX$(2, 1), 13);
                Locate , 15: Print Using "### #"; SX%(2, 1, tickInterval)
            End If

            If Left$(SX$(3, 0), 3) <> "XXX" Then
                Locate 8, 1
                X = 3
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(3, 0), 13);
                Locate , 15: Print Using "### "; SX%(3, 0, tickInterval);

                Print R$(tickInterval)

                Print Left$(SX$(3, 1), 13);
                Locate , 15: Print Using "### #"; SX%(3, 1, tickInterval)
            End If

            If Left$(SX$(4, 0), 3) <> "XXX" Then
                Locate 11, 1
                X = 4
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(4, 0), 13);
                Locate , 15: Print Using "### "; SX%(4, 0, tickInterval);

                Print R$(tickInterval)

                Print Left$(SX$(4, 1), 13);
                Locate , 15: Print Using "### #"; SX%(4, 1, tickInterval)
            End If

            If Left$(SX$(5, 0), 3) <> "XXX" Then
                Locate 14, 1
                X = 5
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(5, 0), 13);
                Locate , 15: Print Using "### "; SX%(5, 0, tickInterval);

                Print R$(tickInterval)

                Print Left$(SX$(5, 1), 13);
                Locate , 15: Print Using "### #"; SX%(5, 1, tickInterval)
            End If

            If Left$(SX$(6, 0), 3) <> "XXX" Then
                Locate 17, 1
                X = 6
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(6, 0), 13);
                Locate , 15: Print Using "### "; SX%(6, 0, tickInterval);
                Print R$(tickInterval)

                Print Left$(SX$(6, 1), 13);
                Locate , 15: Print Using "### #"; SX%(6, 1, tickInterval)
            End If

            If Left$(SX$(7, 0), 3) <> "XXX" Then
                Locate 20, 1
                X = 7
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(7, 0), 13);
                Locate , 15: Print Using "### "; SX%(7, 0, tickInterval);

                Print R$(tickInterval)

                Print Left$(SX$(7, 1), 13);
                Locate , 15: Print Using "### #"; SX%(7, 1, tickInterval)
            End If

            If Left$(SX$(8, 0), 3) <> "XXX" Then
                Locate 23, 1
                X = 8
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(8, 0), 13);
                Locate , 15: Print Using "### "; SX%(8, 0, tickInterval);

                Print R$(tickInterval)

                Print Left$(SX$(8, 1), 13);
                Locate , 15: Print Using "### #"; SX%(8, 1, tickInterval);
            End If

            If Left$(SX$(9, 0), 3) <> "XXX" Then
                Locate 2, 21
                X = 9
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(9, 0), 13);
                Locate , 35: Print Using "### "; SX%(9, 0, tickInterval);

                Print R$(tickInterval)
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

                Print R$(tickInterval)
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

                Print R$(tickInterval)
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

                Print R$(tickInterval)
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

                Print R$(tickInterval)
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

                Print R$(tickInterval)
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

                Print R$(tickInterval)
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

                Print R$(tickInterval)
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

                Print R$(tickInterval)
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

                Print R$(tickInterval)
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

                Print R$(tickInterval)
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

                Print R$(tickInterval)
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

                Print R$(tickInterval)
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

                Print R$(tickInterval)
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

                Print R$(tickInterval)
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

                Print R$(tickInterval)
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

                Print R$(tickInterval)
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

                Print R$(tickInterval)
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

                Print R$(tickInterval)
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

                Print R$(tickInterval)
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

                Print R$(tickInterval)
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

                Print R$(tickInterval)
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

                Print R$(tickInterval)
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

                Print R$(tickInterval)
                Locate , 61

                Print Left$(SX$(32, 1), 13);
                Locate , 75: Print Using "### #"; SX%(32, 1, tickInterval);
            End If

        End If

        I$ = GetKeyPress$

    End If

End Sub


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
    If endGame = 1 Then tickerIdx = 14

    TickerTime% = tickerIdx

End Function


'----------------------------------------
'        TICKERUPDATE Subroutine
'----------------------------------------
'This subroutine calculates the updated
'scores for the ticker games.
Sub TICKERUPDATE ()

    If AP = 1 And AP%(2) <= 31 Then

        If H = 1 Then

            Select Case gameClock!

                Case 721 To 960
                    If SX%(AP%(2) + 1, 0, 1) = -1 Then SX%(AP%(2) + 1, 0, 1) = S!(0, 0): SX%(AP%(2) + 1, 1, 1) = S!(1, 0)

                Case 481 To 720
                    If SX%(AP%(2) + 1, 0, 2) = -1 Then SX%(AP%(2) + 1, 0, 2) = S!(0, 0): SX%(AP%(2) + 1, 1, 2) = S!(1, 0)

                Case 241 To 480
                    If SX%(AP%(2) + 1, 0, 3) = -1 Then SX%(AP%(2) + 1, 0, 3) = S!(0, 1): SX%(AP%(2) + 1, 1, 3) = S!(1, 1)

                Case 121 To 240
                    If SX%(AP%(2) + 1, 0, 4) = -1 Then SX%(AP%(2) + 1, 0, 4) = S!(0, 0): SX%(AP%(2) + 1, 1, 4) = S!(1, 0)

                Case 0 To 120
                    If SX%(AP%(2) + 1, 0, 5) = -1 Then SX%(AP%(2) + 1, 0, 5) = S!(0, 0): SX%(AP%(2) + 1, 1, 5) = S!(1, 0)

            End Select

        Else

            If H > 1 And SX%(AP%(2) + 1, 0, 6) = -1 Then

                For I2 = 6 To 8:
                    SX%(AP%(2) + 1, 0, I2) = S!(0, 2) + S!(0, 1)
                    SX%(AP%(2) + 1, 1, I2) = S!(1, 2) + S!(1, 1)
                Next

            Else

                If H = 2 Then

                    Select Case gameClock!

                        Case 721 To 960
                            If SX%(AP%(2) + 1, 0, 9) = -1 Then SX%(AP%(2) + 1, 0, 9) = S!(0, 0): SX%(AP%(2) + 1, 1, 9) = S!(1, 0)

                        Case 481 To 720
                            If SX%(AP%(2) + 1, 0, 10) = -1 Then SX%(AP%(2) + 1, 0, 10) = S!(0, 0): SX%(AP%(2) + 1, 1, 10) = S!(1, 0)

                        Case 241 To 480
                            If SX%(AP%(2) + 1, 0, 11) = -1 Then SX%(AP%(2) + 1, 0, 11) = S!(0, 3) + S!(0, 2) + S!(0, 1): SX%(AP%(2) + 1, 1, 11) = S!(1, 3) + S!(1, 2) + S!(1, 1)

                        Case 121 To 240
                            If SX%(AP%(2) + 1, 0, 12) = -1 Then SX%(AP%(2) + 1, 0, 12) = S!(0, 0): SX%(AP%(2) + 1, 1, 12) = S!(1, 0)

                        Case 0 To 120
                            If SX%(AP%(2) + 1, 0, 13) = -1 Then SX%(AP%(2) + 1, 0, 13) = S!(0, 0): SX%(AP%(2) + 1, 1, 13) = S!(1, 0)

                    End Select

                Else

                    'Overtime
                    SX%(AP%(2) + 1, 0, 14) = S!(0, 0)
                    SX%(AP%(2) + 1, 1, 14) = S!(1, 0)

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

    If AP%(2) <= 31 Then

        For X = 0 To 1
            SX$(AP%(2) + 1, X) = B$(X)
        Next

        Open "TICKER" For Output As #2

        For I = 1 To 32
            For I1 = 0 To 1
                For I2 = 1 To 14
                    Print #2, SX%(I, I1, I2)
                Next
            Next
        Next

        For I = 1 To 32
            For I1 = 0 To 1
                Print #2, SX$(I, I1)
            Next
        Next

        For I = 1 To 32
            Print #2, ST%(I)
        Next

        Close 2

    End If

    Open diskPaths$(0) + scheduleFile$ For Random As #1 Len = 48

    For X = 0 To 18
        Field #1, X * 2 As X$, 2 As Q$(X + 1), 48 - 2 - 2 * X As X$
    Next

    Field #1, 38 As X$, 2 As Q$(22), 2 As Q$(23), 3 As Q$(20), 3 As Q$(21)

    NG%(13) = 1

    For X = 0 To 18:
        LSet Q$(X + 1) = MKI$(NG%(X))
    Next

    For X = 0 To 1
        LSet Q$(20 + X) = YN$(X)
    Next

    LSet Q$(22) = MKI$(S!(0, 0))
    LSet Q$(23) = MKI$(S!(1, 0))

    Put #1, AP%(0) + AP%(2)

    Close 1

    AP%(2) = AP%(2) + 1
    AP = 1

End Sub


'----------------------------------------
'          HalfCourt Subroutine
'----------------------------------------
'This subroutine gives the user options
'for handling a strategy at half court
'when time is low on the clock.
'I2% = user choice for shot
Sub HalfCourt ()

    Call FRAMEOPTION(8, P)
    Locate 10, 15

    Print B$(P); " HALF COURT OPTION"

    Locate 12, 14
    Locate , 14: Print Using "TIME REMAINING #.#"; T!
    Locate , 14: Print "0 - SHOOT A 2"
    Locate , 14: Print "1 - SHOOT A 3"

    Do
        I$ = GetKeyPress$
        I2% = Val(I$)
    Loop Until I2% >= 0 And I2% <= 1

    If I2% = 1 And X3% = 0 Then I2% = 0

    Color 15, 0

    Call SCOREBOARD

End Sub


'----------------------------------------
'           WHOLAY Subroutine
'----------------------------------------
'This subroutine assignes a player ID to
'the person performing a layup
Sub WHOLAY ()

    RN = Int(Rnd(1) * 100) + 1

    Select Case RN
        Case 1 To 38: B = 0
        Case 39 To 75: B = 1
        Case 76 To 83: B = 2
        Case 84 To 91: B = 3
        Case 92 To 100: B = 4
    End Select

End Sub


'----------------------------------------
'         FoulCalled Subroutine
'----------------------------------------
'This subroutione handles calculations and
'Play-by-Play fo when a player is fouled
Sub FoulCalled (randomCalcs%)

    If randomCalcs% = 1 Then

        W5 = 0
        M9 = Int(Rnd(1) * F5%(D, 0)) + 1

        For M8 = 0 To 4:

            W5 = W5 + A!(D, B1(D, M8), 17)

            If M9 <= W5 Then Exit For

        Next M8

        If M8 < 0 Or M8 > 4 Then M8 = Int(Rnd(1) * 5)

        W5 = M8

    End If

    A!(D, B1(D, W5), 12) = A!(D, B1(D, W5), 12) + 1
    P2(D) = P2(D) + 1
    Call UPDATESCREEN

    If A!(D, B1(D, W5), 3) - A!(D, B1(D, W5), 7) - A!(D, B1(D, W5), 11) - A!(D, B1(D, W5), 12) = 0 Then A!(D, B1(D, W5), 3) = A!(D, B1(D, W5), 3) + 1

    If A!(D, B1(D, W5), 12) < NF% Then

        If PB% = 1 And pbpDelay! <> 0 Then
            Call WINDEX
            Print "FOUL ON "; AA$(D, B1(D, W5))
            Call DELAY
            Print "HE HAS "; A!(D, B1(D, W5), 12)
            Call DELAY
        End If

        If PB% = 0 Then U$ = "FOUL NUMBER" + Str$(A!(D, B1(D, W5), 12)) + " CALLED ON " + AA$(D, B1(D, W5)): Call PBP

        Call DELAY

    Else
        '   A!(D, B1(D, W5), 12) >= NF%

        M9 = 0
        For I3 = 0 To 13
            If Not (I3 = B1(D, W5) Or AA$(D, I3) = "XXX" Or B%(D, I3) <> 0) Then
                M9 = M9 + 1
            End If
        Next

        For I3 = 0 To 13
            If Not (I3 = B1(D, W5) Or AA$(D, I3) = "XXX" Or B%(D, I3) <> 0) Then
                If A!(D, I3, 0) <= A!(D, B1(D, W5), 0) Then A!(D, I3, 3) = A!(D, I3, 3) + Int((A!(D, B1(D, W5), 3) / M9) + .5)
            End If
        Next I3

        'Player fouled
        635 If PB% = 1 And pbpDelay! <> 0 Then
            Call WINDEX
            Print "FOUL ON "; AA$(D, B1(D, W5))
            Call DELAY
        End If

        If PB% = 0 Then U$ = "FOUL CALLED ON " + AA$(D, B1(D, W5)): Call PBP
        If IN% = 1 Then
            U$ = "FOUL IS RULED INTENTIONAL!"
            If PB% = 0 Then Call PBP
            If PB% = 1 And pbpDelay! <> 0 Then Print U$: Call DELAY
            Call DELAY
        End If

        U$ = "AND HE HAS JUST FOULED OUT !!!"
        If PB% = 0 Then Call PBP
        If PB% = 1 And pbpDelay! <> 0 Then Print U$: Call DELAY

        Call DELAY
        If P2(D) > 6 Then X = 50
        If U5 = 0 Or U5 = 1 And D <> CT Then
            P9 = D
            C1 = 1
            Call LINEUPS
            Call FOULSAFE
            Call DEADBALLOPTIONS
            For I = 0 To 1
                If P7(I) = 0 Then PR%(I, 0) = 1: PR%(I, 1) = 7: PC%(I) = 0
            Next
            Call SCOREBOARD
        End If

        Call CheckGameClock
        X = 0

    End If

End Sub


'----------------------------------------
'         GoodBasket Subroutine
'----------------------------------------
'This subroutine appears perform calcu-
'lations on W5 depending on the value of
'some random numbers.
'I don't know what it's doing exactly,
'but it only gets called after a good basket.
'It doesn't look like it's adjusting the
'score though
Sub GoodBasket ()

    R0! = Int(Rnd(1) * 3) + 1
    I2 = Int(Rnd(1) * 5)

    If (R0! <= A!(P, B1(P, I2), 4) And AA$(P, B1(P, B)) <> AA$(P, B1(P, I2))) Or (A!(P, B1(P, I2), 4) > 3 And R0! < A!(P, B1(P, I2), 4) - 3 And AA$(P, B1(P, B)) <> AA$(P, B1(P, I2))) Then

        Do
            W5 = 0

            For M8 = 0 To 4
                W5 = W5 + A!(P, B1(P, M8), 4)
            Next

            R0! = Int(Rnd(1) * W5) + 1
            W5 = 0

            For M8 = 0 To 4
                W5 = W5 + A!(P, B1(P, M8), 4)
                If R0! <= W5 Then Exit For
            Next

            I2 = M8
        Loop Until AA$(P, B1(P, B)) <> AA$(P, B1(P, I2))

        A!(P, B1(P, I2), 14) = A!(P, B1(P, I2), 14) + 1

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

    For PP = 1 To 70
        Sound 50, .05
        Sound 100, .05
        Sound 317, .3
    Next PP

End Sub


'----------------------------------------
'         SHOTSTRING Subroutine
'----------------------------------------
'This subroutine simply builds a string
'which represents the % chance of making
'the current shot (rounding up)
Sub SHOTSTRING ()
    U$ = U$ + Str$(Int(F! + .5)) + "*"
End Sub


'----------------------------------------
'         REBOUNDSORT Subroutine
'----------------------------------------
'This subroutine clearly does something
'related to sorting players by rebound?
Sub REBOUNDSORT ()

    M = 4
    L = 3

    8827 '
    S6 = 0
    S7 = 0

    8828 '
    H1 = 0
    H2 = 0

    If G5(S6) > G5(S6 + 1) Then
        H1 = G5(S6)
        H2 = G4(S6)
        G5(S6) = G5(S6 + 1)
        G4(S6) = G4(S6 + 1)
        G5(S6 + 1) = H1
        G4(S6 + 1) = H2
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
'          COMCOACH Subroutine
'----------------------------------------
'This subroutine essentially handles the
'AI for the computer coach. It relies on
'other routines to help deal with some of
'the complexities.
Sub COMCOACH ()

    'COMPUTER COACH FOR SUBSTITUTIONS AND AI

    'Call pbplog(3590)

    For I = 0 To 13
        F1(I) = I
        S3(I) = A!(P9, I, 3) - A!(P9, I, 7) - A!(P9, I, 11) - A!(P9, I, 12)
        If H > 2 Then S3(I) = A!(P9, I, 3)
        If H = 2 And gameClock! <= 120 Then S3(I) = A!(P9, I, 3)
        If A!(P9, I, 12) >= NF% Then S3(I) = 0
    Next I

    Call STAMINASORT

    skipLoop% = 0

    I = 14

    For I1 = 0 To 4

        8794 '
        I = I - 1

        If I < 0 Then

            I = 14

            For I1 = 0 To 4

                8705 '
                I = I - 1

                If I < 0 Then

                    I = 14

                    For I1 = 0 To 4

                        Do
                            I = I - 1
                        Loop Until Not (B%(P9, F1(I)) <> 0 Or AA$(P9, F1(I)) = "XXX")

                        'Call pbplog(3628)
                        G4(I1) = F1(I)
                        G5(I1) = A!(P9, F1(I), 2)
                        'Call pbplog(3632)

                    Next I1

                    skipLoop% = 1

                Else

                    If Not (B%(P9, F1(I)) <> 0 Or AA$(P9, F1(I)) = "XXX" Or A!(P9, F1(I), 12) >= NF% Or A!(P9, F1(I), 12) >= NF% - 1 And (H = 1 Or H = 2 And gameClock! > 720)) Then
                        'Call pbplog(3641)
                        G4(I1) = F1(I)
                        G5(I1) = A!(P9, F1(I), 2)
                        'Call pbplog(3644)
                    Else
                        GoTo 8705
                    End If

                End If

                If skipLoop% = 1 Then Exit For

            Next I1

            If skipLoop% = 1 Then Exit For

        Else

            If B%(P9, F1(I)) <> 0 Or AA$(P9, F1(I)) = "XXX" Or A!(P9, F1(I), 12) >= NF% Or A!(P9, F1(I), 12) >= NF% - 2 And H = 1 Or A!(P9, F1(I), 12) >= NF% - 1 And H = 2 And gameClock! > 720 Then 8794

            If H = 1 And S3(I) / A!(P9, F1(I), 3) < .5 Then 8794
            If H = 1 And gameClock! >= 600 And S3(I) / A!(P9, F1(I), 3) < .75 Then 8794
            If H = 2 And gameClock! >= 600 And S3(I) / A!(P9, F1(I), 3) < .25 Then 8794
            If H = 2 And gameClock! >= 300 And gameClock! < 600 And S3(I) / A!(P9, F1(I), 3) < .13 Then 8794
            If H = 2 And gameClock! < 300 And S3(I) / A!(P9, F1(I), 3) < gameClock! / 2400 Then 8794
            If S3(I) <= 0 And H < 2 Or S3(I) <= 0 And H = 2 And gameClock! > 120 Then 8794

            'Call pbplog(3668)
            G4(I1) = F1(I)
            G5(I1) = A!(P9, F1(I), 2)
            'Call pbplog(3671)

        End If

    Next I1

    Call REBOUNDSORT

    'Call pbplog(3683)
    For I = 0 To 4
        B1(P9, I) = G4(I)
    Next I
    'Call pbplog(3687)

    Call COMPUTERCOACH

End Sub


'----------------------------------------
'        TOURNEY_Game Subroutine
'----------------------------------------
'This subroutine checks the file for the
'tournament and, as needed, calls a routine
'to update it with the game scores
Sub TOURNEY_Game ()

    Dim fileLength&

    'CHECK FOR FILLING IN TOURNEY SCHEDULE
    Open diskPaths$(1) + tourneyFile$ + ".TOU" As #1 Len = 90
    fileLength& = LOF(1)
    Close 1

    NTMS = fileLength& / 90

    For I = 1 To NTMS

        Open diskPaths$(1) + tourneyFile$ + ".TOU" As #1 Len = 90

        For X = 0 To 14
            Field #1, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
        Next

        Field #1, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
        Field #1, 64 As X$, 3 As Q$(20), 3 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
        Field #1, 84 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)
        Get #1, I

        VS1% = CVI(Q$(16))
        HS1% = CVI(Q$(17))
        VT1$ = Q$(18)
        W1$ = Q$(19)

        For X = 0 To 14: NG1%(X) = CVI(Q$(X + 1)): Next
        For X = 0 To 3: YN1$(X) = Q$(20 + X): Next
        For X = 1 To 3: NG1%(X + 14) = CVI(Q$(X + 23)): Next

        Close 1

        If NG1%(15) = NG%(15) And NG%(15) <> 17 Then

            If S!(0, 0) > S!(1, 0) And NG1%(0) - 900 = NG%(17) Then NG1%(0) = NG%(0): YN1$(0) = YN$(0): VT1$ = VT$: Call UpdateTourneyFile
            If S!(1, 0) > S!(0, 0) And NG1%(0) - 900 = NG%(17) Then NG1%(0) = NG%(1): YN1$(0) = YN$(1): VT1$ = W$: Call UpdateTourneyFile
            If S!(0, 0) > S!(1, 0) And NG1%(1) - 900 = NG%(17) Then NG1%(1) = NG%(0): YN1$(1) = YN$(0): W1$ = VT$: Call UpdateTourneyFile
            If S!(1, 0) > S!(0, 0) And NG1%(1) - 900 = NG%(17) Then NG1%(1) = NG%(1): YN1$(1) = YN$(1): W1$ = W$: Call UpdateTourneyFile
            If S!(0, 0) < S!(1, 0) And NG1%(0) - 800 = NG%(17) Then NG1%(0) = NG%(0): YN1$(0) = YN$(0): VT1$ = VT$: Call UpdateTourneyFile
            If S!(1, 0) < S!(0, 0) And NG1%(0) - 800 = NG%(17) Then NG1%(0) = NG%(1): YN1$(0) = YN$(1): VT1$ = W$: Call UpdateTourneyFile
            If S!(0, 0) < S!(1, 0) And NG1%(1) - 800 = NG%(17) Then NG1%(1) = NG%(0): YN1$(1) = YN$(0): W1$ = VT$: Call UpdateTourneyFile
            If S!(1, 0) < S!(0, 0) And NG1%(1) - 800 = NG%(17) Then NG1%(1) = NG%(1): YN1$(1) = YN$(1): W1$ = W$: Call UpdateTourneyFile

        End If

        If NG%(15) <> 17 Then
            If S!(0, 0) > S!(1, 0) And NG%(16) = 89 And NG1%(15) = 17 And NG1%(0) - 900 = NG%(15) Then NG1%(0) = NG%(0): YN1$(0) = YN$(0): VT1$ = VT$: Call UpdateTourneyFile
            If S!(1, 0) > S!(0, 0) And NG%(16) = 89 And NG1%(15) = 17 And NG1%(0) - 900 = NG%(15) Then NG1%(0) = NG%(1): YN1$(0) = YN$(1): VT1$ = W$: Call UpdateTourneyFile
            If S!(0, 0) > S!(1, 0) And NG%(16) = 89 And NG1%(15) = 17 And NG1%(1) - 900 = NG%(15) Then NG1%(1) = NG%(0): YN1$(1) = YN$(0): W1$ = VT$: Call UpdateTourneyFile
            If S!(1, 0) > S!(0, 0) And NG%(16) = 89 And NG1%(15) = 17 And NG1%(1) - 900 = NG%(15) Then NG1%(1) = NG%(1): YN1$(1) = YN$(1): W1$ = W$: Call UpdateTourneyFile
        End If

        If S!(0, 0) > S!(1, 0) And NG%(15) = 17 And NG1%(0) - 1100 = NG%(17) Then NG1%(0) = NG%(0): YN1$(0) = YN$(0): VT1$ = VT$: Call UpdateTourneyFile
        If S!(1, 0) > S!(0, 0) And NG%(15) = 17 And NG1%(0) - 1100 = NG%(17) Then NG1%(0) = NG%(1): YN1$(0) = YN$(1): VT1$ = W$: Call UpdateTourneyFile
        If S!(0, 0) > S!(1, 0) And NG%(15) = 17 And NG1%(1) - 1100 = NG%(17) Then NG1%(1) = NG%(0): YN1$(1) = YN$(0): W1$ = VT$: Call UpdateTourneyFile
        If S!(1, 0) > S!(0, 0) And NG%(15) = 17 And NG1%(1) - 1100 = NG%(17) Then NG1%(1) = NG%(1): YN1$(1) = YN$(1): W1$ = W$: Call UpdateTourneyFile
        If S!(0, 0) < S!(1, 0) And NG%(15) = 17 And NG1%(0) - 1000 = NG%(17) Then NG1%(0) = NG%(0): YN1$(0) = YN$(0): VT1$ = VT$: Call UpdateTourneyFile
        If S!(1, 0) < S!(0, 0) And NG%(15) = 17 And NG1%(0) - 1000 = NG%(17) Then NG1%(0) = NG%(1): YN1$(0) = YN$(1): VT1$ = W$: Call UpdateTourneyFile
        If S!(0, 0) < S!(1, 0) And NG%(15) = 17 And NG1%(1) - 1000 = NG%(17) Then NG1%(1) = NG%(0): YN1$(1) = YN$(0): W1$ = VT$: Call UpdateTourneyFile
        If S!(1, 0) < S!(0, 0) And NG%(15) = 17 And NG1%(1) - 1000 = NG%(17) Then NG1%(1) = NG%(1): YN1$(1) = YN$(1): W1$ = W$: Call UpdateTourneyFile
        If S!(0, 0) < S!(1, 0) And NG%(15) = 17 And NG1%(0) - 900 = NG%(17) Then NG1%(0) = NG%(0): YN1$(0) = YN$(0): VT1$ = VT$: Call UpdateTourneyFile
        If S!(1, 0) < S!(0, 0) And NG%(15) = 17 And NG1%(0) - 900 = NG%(17) Then NG1%(0) = NG%(1): YN1$(0) = YN$(1): VT1$ = W$: Call UpdateTourneyFile
        If S!(0, 0) < S!(1, 0) And NG%(15) = 17 And NG1%(1) - 900 = NG%(17) Then NG1%(1) = NG%(0): YN1$(1) = YN$(0): W1$ = VT$: Call UpdateTourneyFile
        If S!(1, 0) < S!(0, 0) And NG%(15) = 17 And NG1%(1) - 900 = NG%(17) Then NG1%(1) = NG%(1): YN1$(1) = YN$(1): W1$ = W$: Call UpdateTourneyFile
    Next

End Sub


'----------------------------------------
'      UpdateTourneyFile Subroutine
'----------------------------------------
'This subroutine actually changes any data
'associated with the tournament file.
Sub UpdateTourneyFile ()

    Open diskPaths$(1) + tourneyFile$ + ".TOU" As #1 Len = 90

    For X = 0 To 14
        Field #1, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
    Next

    Field #1, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
    Field #1, 64 As X$, 3 As Q$(20), 3 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
    Field #1, 84 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)

    For X = 0 To 14:
        LSet Q$(X + 1) = MKI$(NG1%(X))
    Next

    For X = 0 To 3:
        LSet Q$(20 + X) = YN1$(X)
    Next

    For X = 1 To 3:
        LSet Q$(X + 23) = MKI$(NG1%(X + 14))
    Next

    LSet Q$(16) = MKI$(S!(0, 0))
    LSet Q$(17) = MKI$(S!(1, 0))
    LSet Q$(18) = VT1$
    LSet Q$(19) = W1$

    Put #1, I

    Close 1

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
    Color TR%(0, 0), TR%(0, 1): Print B$(0);

    'Game Clock - Minutes
    Locate 1, 37
    Color 15, 0: Print Using "##"; Int(gameClock! / 60);: Print ":";

    'Game Clock - Seconds
    If gameClock! <= 5 Then
        'print 1/10th of a sec for last 5 seconds
        Print "0";: Print Using "#.#"; gameClock! - Int(gameClock! / 60) * 60;
    Else
        If gameClock! - Int(gameClock! / 60) * 60 < 10 Then
            Print "0";: Print Using "#"; gameClock! - Int(gameClock! / 60) * 60;
        Else
            Print Using "##"; gameClock! - Int(gameClock! / 60) * 60;
        End If

    End If

    'Team 2
    Locate 2, 56
    Color TR%(1, 0), TR%(1, 1): Print B$(1)
    Color 14, 0

    'Score Team 1
    Locate 4, 13: Print Using "###"; S!(0, 0);
    Locate 2, 33: Print " ";
    If PA% = 0 Then Locate , 33: Print Chr$(17);

    'Period Designation
    Locate 2, 35: Print Using "  HALF #"; H;: Locate , 44: Print " ";
    If PA% = 1 Then Locate , 44: Print Chr$(16);

    'Overtime
    If H > 2 Then
        Locate , 35: Print Using "OVERTIME #"; H - 2;
        Locate , 46: Print " ";
        If PA% = 1 Then Locate , 46: Print Chr$(16)
    End If

    'Score Team 2
    Color 14, 0
    Locate 4, 60: Print Using "###"; S!(1, 0)

    'TO / RB / TF
    Color 7
    Locate 3, 35: Print Using "##  "; T0(0);
    Color 12: Print "TO  ";: Color 7: Print Using "##"; T0(1)
    Locate , 35: Print Using "##  "; R3!(0);
    Color 12: Print "RB  ";: Color 7: Print Using "##"; R3!(1)
    Locate , 35: Print Using "##  "; P2(0);
    Color 12: Print "TF  ";: Color 7: Print Using "##"; P2(1);

    'Bonus
    If P2(0) > 5 Then
        Color 0, 2
        Locate , 46: Print "BONUS";
        Color 7, 0
    End If

    If P2(1) > 5 Then
        Color 0, 2
        Locate , 29: Print "BONUS"
        Color 7, 0
    End If

    'Timeouts
    Locate 6, 9: Print "TIMEOUTS:"; T1(0);
    Locate , 56: Print "TIMEOUTS:"; T1(1)

    Color 14, 0

    'Shot Clock
    Locate 6, 37: Print Using " :##"; shotClock

    'If no shot clock, or shot clock expired
    If sClockVal = 0 Or gameClock! < 45 And shotClock >= gameClock! And sClockVal = 1 Or gameClock! < 35 And shotClock >= gameClock! And sClockVal = 2 Or (sClockVal = 1 Or sClockVal = 2) And H > 2 And gameClock! = 300 Then
        Locate 6, 37: Print " :  "
    End If

    'Team strategies / decisions
    If pbpDelay! <> 0 Then

        'Team Play Styles
        Locate 9, 41
        Color TR%(D, 0), TR%(D, 1): Print "DEF-"; DB$(DT%(D))
        Locate , 41
        Color TR%(P, 0), TR%(P, 1): Print "OFF-"; OB$(OF%(P))

        Color 15, 0
        Locate 11, 41
        Color 14: Print "ENTER ";: Color 15: Print "PASS"
        Locate , 41
        Color 14: Print "(1)  ";: Color 15: Print "SHOOT"
        Locate , 41
        Color 14: Print "(2)  ";: Color 15: Print B$(P); " CALL TIMEOUT     "
        Locate , 41
        Color 14: Print "(T)  ";: Color 15: Print "10 MIN TICKER"
        Locate , 41
        Color 14: Print "(D)  ";: Color 15: Print "CHANGE DEFENSE FROM 2-3 ZONE"
        Color 15, 0

        Locate 19, 1

        'Roster for team 1
        For I1 = 0 To 4:

            Color 2, 0
            Print Using "# "; I1;

            Color 15
            Print Left$(AA$(0, B1(0, I1)), 12); Tab(15);

            If A!(0, B1(0, I1), 19) >= 7 Then Print "*";
            Print ; Tab(16);
            J = (A!(0, B1(0, I1), 3)) - A!(0, B1(0, I1), 7) - A!(0, B1(0, I1), 11) - A!(0, B1(0, I1), 12): If J <= 0 Then J = 0

            Print Using "## ## "; A!(0, B1(0, I1), 0), A!(0, B1(0, I1), 1);
            If P = 0 Then Print Using "##"; A!(0, B1(0, I1), 21) * 2 / 10; Else Print Using "##"; A!(0, B1(0, I1), 2) * 2 / 10;
            If J <= 0 And (H = 1 Or H = 2 And gameClock! > 120) Then Color 12

            Print Using "### "; J;

            Color 15
            If FA%(0, B1(0, I1)) * -1 >= 5 And (H = 1 Or H = 2 And gameClock! > 120) Then Color 12
            Print Using "## "; FA%(0, B1(0, I1)) * -1;

            Color 15
            Print Using "##"; A!(0, B1(0, I1), 4);
            Print Using "###  "; A!(0, B1(0, I1), 8) * 2 + A!(0, B1(0, I1), 10) + W%(0, B1(0, I1), 0);
            If A!(0, B1(0, I1), 12) >= 4 Then Color 12

            Print Using "#"; A!(0, B1(0, I1), 12)
            Color 15

        Next I1

        Locate 19, 40

        'Roster for team 2
        For I1 = 0 To 4:

            Color 2, 0
            Locate , 40: Print Using "# "; I1;
            Color 15: Print Left$(AA$(1, B1(1, I1)), 12); Tab(55);

            If A!(1, B1(1, I1), 19) >= 7 Then Print "*";

            Print ; Tab(56);

            J = (A!(1, B1(1, I1), 3)) - A!(1, B1(1, I1), 7) - A!(1, B1(1, I1), 11) - A!(1, B1(1, I1), 12)
            If J <= 0 Then J = 0

            Print Using "## ## "; A!(1, B1(1, I1), 0), A!(1, B1(1, I1), 1);

            If P = 1 Then Print Using "##"; A!(1, B1(1, I1), 21) * 2 / 10; Else Print Using "##"; A!(1, B1(1, I1), 2) * 2 / 10;

            If J <= 0 And (H = 1 Or H = 2 And gameClock! > 120) Then Color 12
            Print Using "### "; J;

            Color 15
            If FA%(1, B1(1, I1)) * -1 >= 5 And (H = 1 Or H = 2 And gameClock! > 120) Then Color 12
            Print Using "## "; FA%(1, B1(1, I1)) * -1;

            Color 15
            Print Using "##"; A!(1, B1(1, I1), 4);
            Print Using "###  "; A!(1, B1(1, I1), 8) * 2 + A!(1, B1(1, I1), 10) + W%(1, B1(1, I1), 0);

            If A!(1, B1(1, I1), 12) >= 4 Then Color 12
            Print Using "#"; A!(1, B1(1, I1), 12)

            Color 15

        Next I1

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
    Color TR%(0, 0), TR%(0, 1): Print B$(0);

    'Game Clock - Minutes
    Color 15, 0
    Locate 1, 37: Print Using "##"; Int(gameClock! / 60);: Print ":";

    'Game Clock - Seconds
    If gameClock! - Int(gameClock! / 60) * 60 < 10 Then
        Print "0";: Print Using "#"; gameClock! - Int(gameClock! / 60) * 60;
    Else
        Print Using "##"; gameClock! - Int(gameClock! / 60) * 60;
    End If

    'Team 2
    Locate 2, 56
    Color TR%(1, 0), TR%(1, 1): Print B$(1)

    'Score Team 1
    Color 14, 0
    Locate 4, 13: Print Using "###"; S!(0, 0);

    'Period Designation
    Locate 2, 35
    If H > 2 Then
        Print Using "OVERTIME #"; H - 2
    Else
        Print Using "  HALF #"; H;
    End If

    'Score Team 2
    Color 14, 0
    Locate 4, 60: Print Using "###"; S!(1, 0)

    'TO / RB / TF
    Color 7
    Locate 3, 35: Print Using "##  "; T0(0);
    Color 12: Print "TO  ";
    Color 7: Print Using "##"; T0(1)

    Locate , 35: Print Using "##  "; R3!(0);
    Color 12: Print "RB  ";
    Color 7: Print Using "##"; R3!(1)

    Locate , 35: Print Using "##  "; P2(0);
    Color 12: Print "TF  ";
    Color 7: Print Using "##"; P2(1);

    'Bonus
    If P2(0) > 5 Then
        Color 0, 2
        Locate , 46: Print "BONUS";
        Color 7, 0
    End If

    If P2(1) > 5 Then
        Color 0, 2
        Locate , 29: Print "BONUS"
        Color 7, 0
    End If

    'Timeouts
    Locate 6, 9: Print "TIMEOUTS:"; T1(0);
    Locate , 56: Print "TIMEOUTS:"; T1(1)

    'Team strategies / decisions
    Locate 9, 41
    Color TR%(D, 0), TR%(D, 1): Print "DEF-"; DB$(DT%(D))
    Locate , 41
    Color TR%(P, 0), TR%(P, 1): Print "OFF-"; OB$(OF%(P))

    Color 15, 0
    Locate 11, 41: Color 14: Print "ENTER";: Color 14: Print "-PASS"
    Locate , 41: Color 14: Print "(1)  ";: Color 15: Print "SHOOT"
    Locate , 41: Color 14: Print "(2)  ";: Color 15: Print B$(P); " CALL TIMEOUT     "
    Locate , 41: Color 14: Print "(T)  ";: Color 15: Print "10 MIN TICKER"
    Locate , 41: Color 14: Print "(D)  ";: Color 15: Print "CHANGE DEFENSE FROM 2-3 ZONE"

    Color 9, 0
    Locate 7, 1
    Print String$(38, 205); Chr$(203); String$(38, 205)

    Locate 8, 1
    For I = 1 To 8
        Locate , 39
        Print Chr$(186)
    Next

    'Team 1 Overview
    Locate 17, 1
    Color TR%(0, 0), TR%(0, 1): Print B$(0);

    'Win/Loss record from current stats files
    Print Using " [##"; Z5(0);: Print "-";: Print Using "##]"; Z6(0)
    Color 14, 0: Print "# NAME         FG FT RB CX EF PA PT  F"

    'Team 1 Roster
    For I1 = 0 To 4

        Color 2: Print Using "# "; I1;
        Color 3
        Color 15: Print Left$(AA$(0, B1(0, I1)), 12);: Locate , 15

        If A!(0, B1(0, I1), 19) >= 7 Then Print "*";

        Locate , 16

        J = (A!(0, B1(0, I1), 3)) - A!(0, B1(0, I1), 7) - A!(0, B1(0, I1), 11) - A!(0, B1(0, I1), 12): If J <= 0 Then J = 0

        Print Using "##"; A!(0, B1(0, I1), 0);
        Print Using "### "; A!(0, B1(0, I1), 1);

        If P = 0 Then Print Using "##"; A!(0, B1(0, I1), 21) * 2 / 10; Else Print Using "##"; A!(0, B1(0, I1), 2) * 2 / 10;
        If J <= 0 And (H = 1 Or H = 2 And gameClock! > 120) Then Color 12

        Print Using "### "; J;
        Color 15

        If FA%(0, B1(0, I1)) * -1 >= 5 And (H = 1 Or H = 2 And gameClock! > 120) Then Color 12

        Print Using "## "; FA%(0, B1(0, I1)) * -1;
        Color 15
        Print Using "##"; A!(0, B1(0, I1), 4);
        Print Using "###  "; A!(0, B1(0, I1), 8) * 2 + A!(0, B1(0, I1), 10) + W%(0, B1(0, I1), 0);

        If A!(0, B1(0, I1), 12) >= 4 Then Color 12

        Print Using "#"; A!(0, B1(0, I1), 12)
        Color 15

    Next I1

    'Team 2 Overview
    Locate 17, 40
    Color TR%(1, 0), TR%(1, 1): Print B$(1);

    'Win/Loss record from current stats files
    Print Using " [##"; Z5(1);: Print "-";: Print Using "##]"; Z6(1)

    Color 14, 0
    Locate , 40: Print "# NAME          FG FT RB CX EF PA PT  F"

    'Team 2 Roster
    For I1 = 0 To 4:

        Locate , 40
        Color 2: Print Using "# "; I1;
        Color 15: Print Left$(AA$(1, B1(1, I1)), 12);: Locate , 55

        If A!(1, B1(1, I1), 19) >= 7 Then Print "*";

        Locate , 56

        J = (A!(1, B1(1, I1), 3)) - A!(1, B1(1, I1), 7) - A!(1, B1(1, I1), 11) - A!(1, B1(1, I1), 12): If J <= 0 Then J = 0

        Print Using "##"; A!(1, B1(1, I1), 0);
        Print Using "### "; A!(1, B1(1, I1), 1);

        If P = 1 Then Print Using "##"; A!(1, B1(1, I1), 21) * 2 / 10; Else Print Using "##"; A!(1, B1(1, I1), 2) * 2 / 10;
        If J <= 0 And (H = 1 Or H = 2 And gameClock! > 120) Then Color 12

        Print Using "### "; J;
        Color 15

        If FA%(1, B1(1, I1)) * -1 >= 5 And (H = 1 Or H = 2 And gameClock! > 120) Then Color 12

        Print Using "## "; FA%(1, B1(1, I1)) * -1;
        Color 15
        Print Using "##"; A!(1, B1(1, I1), 4);
        Print Using "###  "; A!(1, B1(1, I1), 8) * 2 + A!(1, B1(1, I1), 10) + W%(1, B1(1, I1), 0);

        If A!(1, B1(1, I1), 12) >= 4 Then Color 12

        Print Using "#"; A!(1, B1(1, I1), 12)
        Color 15

    Next I1

    Color 9, 0
    For I = 1 To 9:
        Locate 14 + I, 39: Print Chr$(186)
    Next

    'Color 9, 0
    Locate 16, 1: Print String$(79, 205)
    Locate 16, 39: Print Chr$(206)
    Color 7, 0

End Sub

'------------------------------
'    COLCNG Subroutine
'------------------------------
' ...explanation...
Sub COLCNG ()

    If TR%(1, 0) = 15 And TR%(1, 1) = 4 Then TR%(1, 0) = 4: TR%(1, 1) = 7
    If TR%(1, 0) = 15 And TR%(1, 1) = 1 Then TR%(1, 0) = 1: TR%(1, 1) = 7
    If TR%(1, 0) = 0 And TR%(1, 1) = 4 Then TR%(1, 0) = 12: TR%(1, 1) = 0
    If TR%(1, 0) = 1 And TR%(1, 1) = 7 Then TR%(1, 0) = 15: TR%(1, 1) = 1
    If TR%(1, 0) = 4 And TR%(1, 1) = 7 Then TR%(1, 0) = 15: TR%(1, 1) = 4

End Sub


'----------------------------------------
'       LOADSTATS Subroutine
'----------------------------------------
'This subroutine loads team schedule stats
'data into arrays for each team from their
'corresponding stat file.
Sub LOADSTATS ()

    If TR%(1, 0) = TR%(0, 0) And TR%(1, 1) = TR%(0, 1) Then Call COLCNG

    For I = 0 To 1

        Color 15, 0
        Cls

        statFile$ = nameStatFile$(B$(I))

        If _FileExists(diskPaths$(1) + statFile$ + "S") Then

            Open diskPaths$(1) + statFile$ + "S" For Input As #2

            Input #2, YY%

            For I1 = 1 To 100: Input #2, O%(I1): Next
            For I1 = 1 To 100: Input #2, HO$(I1): Next
            For I1 = 1 To 100: Input #2, HT%(I1): Next
            For I1 = 1 To 100: Input #2, HO$(I1): Next

            For I1 = 1 To 100
                For I2 = 0 To 1
                    Input #2, APT%(I1, I2)
                Next
            Next

            For I1 = 1 To 100: Input #2, CRD&(I1): Next I1
            For I1 = 1 To 100: Input #2, SITE$(I1): Next I1

            Close 2

            For I1 = 1 To YY%
                If O%(I1) > HT%(I1) Then Z5(I) = Z5(I) + 1
                If O%(I1) < HT%(I1) Then Z6(I) = Z6(I) + 1
            Next

        End If

    Next

End Sub


'----------------------------------------
'         OFFREBOUND Subroutine
'----------------------------------------
'This subroutine clearly does something
'related to offensive rebounds
Sub OFFREBOUND ()
    RB = 0

    For I = 0 To 4: RB(I) = 0
        XZ = A!(P, B1(P, I), 21)

        If Not (H > 2 Or H = 2 And gameClock! <= 120) Then
            If A!(P, B1(P, I), 3) - A!(P, B1(P, I), 7) - A!(P, B1(P, I), 11) - A!(P, B1(P, I), 12) <= 0 Then XZ = Int(XZ * .5 + .5)
        End If

        If FA%(P, B1(P, I)) <= -5 Then XZ = Int(XZ * .9 + .5)
        RB(I) = RB(I) + XZ: RB = RB + XZ
    Next

    RN = Rnd(1) * RB

    skipRandom% = 0

    For B = 0 To 4
        If RN <= RB(B) Then skipRandom% = 1: Exit For
        RB(B + 1) = RB(B + 1) + RB(B)
    Next

    If skipRandom% = 0 Then B = Int(Rnd(1) * 5)

End Sub


'----------------------------------------
'         DEFREBOUND Subroutine
'----------------------------------------
'This subroutine clearly does something
'related to defensive rebounds
Sub DEFREBOUND ()
    RB = 0

    For I = 0 To 4
        RB(I) = 0
        XZ = A!(1 - P, B1(1 - P, I), 2)

        If Not (H > 2 Or H = 2 And gameClock! <= 120) Then
            If A!(1 - P, B1(1 - P, I), 3) - A!(1 - P, B1(1 - P, I), 7) - A!(1 - P, B1(1 - P, I), 11) - A!(1 - P, B1(1 - P, I), 12) <= 0 Then XZ = Int(XZ * .5 + .5)
        End If

        If FA%(1 - P, B1(1 - P, I)) <= -5 Then XZ = Int(XZ * .9 + .5)
        RB(I) = RB(I) + XZ: RB = RB + XZ
    Next

    RN = Rnd(1) * RB

    skipRandom% = 0

    For B = 0 To 4
        If RN <= RB(B) Then skipRandom% = 1: Exit For
        RB(B + 1) = RB(B + 1) + RB(B)
    Next

    If skipRandom% = 0 Then B = Int(Rnd(1) * 5)

End Sub


'----------------------------------------
'        SETUPRATINGS Subroutine
'----------------------------------------
'This subroutine lets the user adjust the
'contribution ratings for players
Sub SETUPRATINGS ()

    9300 '

    If AP <> 1 Or U5 <> 2 Then

        Color 15, 0: Cls
        Print "EDIT CONTRIBUTION PERCENTAGES OF PLAYERS FROM:"
        Print
        Color 14
        Print "(0) - ";: Color TR%(0, 0), TR%(0, 1)
        Print B$(0): Color , 0
        Print
        Color 14
        Print "(1) - ";: Color TR%(1, 0), TR%(1, 1)
        Print B$(1): Color , 0
        Print
        Color 14
        Print "(2) - ";: Color 3: Print "PROCEED TO GAME"
        Color 15, 0
        Print
        Print "NOTE:: THE CONTRIBUTION RATING CONSISTS OF THE PLAYER'S"
        Print "       FREQUENCY TO SHOOT, REBOUND, AND COMMIT FOULS."

        Do
            I$ = GetKeyPress$
            I = Val(I$)
        Loop Until I >= 0 And I <= 2

        If I <> 2 Then

            Cls
            TX = 0
            For I1 = 0 To 13: TX = TX + A!(I, I1, 3): Next
            For I1 = 0 To 13: A!(I, I1, 3) = A!(I, I1, 3) / TX * 100: Next

            9305 '
            Locate 2, 1: Print "CONTRIBUTION RATINGS FOR ";
            Color TR%(I, 0), TR%(I, 1): Print B$(I)
            Color 15, 0
            Print
            Print " #  PLAYER             CX"
            TY% = 0

            For I1 = 0 To 13:
                TY% = TY% + A!(I, I1, 3)
                If B%(I, I1) = -1 Then Color TR%(I, 0), TR%(I, 1)
                Print Using "## "; I1 + 1;
                Print AA$(I, I1);
                Locate , 24
                Print Using "##"; A!(I, I1, 3)
                Color 15, 0
            Next

            Locate , 23: Print Using "###"; TY%

            Input "SELECT PLAYER TO CHANGE (0 TO CONTINUE)"; I1
            I1 = I1 - 1

            If I1 > 13 Or I1 < 0 Then

                If TY% <> 100 Then
                    Locate 22, 1: Print "TOTAL MUST ADD UP TO 100%! PRESS ANY KEY"
                    I$ = GetKeyPress$
                    Locate 22, 1: Print Space$(52)
                    GoTo 9305
                End If

                For I1 = 0 To 13: A!(I, I1, 3) = A!(I, I1, 3) / 100 * TX: Next

                GoTo 9300

            End If

            Locate 22, 1: Print "ENTER NEW CX RATING FOR "; AA$(I, I1); " ";
            Input A!(I, I1, 3)

            Locate 22, 1: Print Space$(52)
            GoTo 9305

        End If

    End If

    X = 0

End Sub


'----------------------------------------
'         ZONECHANGE Subroutine
'----------------------------------------
'This subroutine lets the user configure
'their zone strategy.
Sub ZONECHANGE ()

    If Not (U5 = 1 And CT = D Or U5 = 2) Or DT%(D) = 2 Or DT%(D) = 7 Or DT%(D) = 12 Or (OF%(P) <= 8 And OF%(P) >= 4) Then

        Call FRAMEOPTION(8, P)
        Locate 10, 15

        Print B$(D); " SELECT STRATEGY:"
        Locate 12, 14
        Locate , 14: Print "0 - SOLID MAN TO MAN"
        Locate , 14: Print "1 - PRESSURE MAN TO MAN"
        Locate , 14: Print "2 - 1-3-1 TRAPPING ZONE"

        Do
            I$ = GetKeyPress$
            I1 = Val(I$)
        Loop Until I1 >= 0 And I1 <= 2

        If I1 = 2 Then I1 = 3
        If DT%(D) <= 3 Then DT%(D) = I1
        If DT%(D) = 7 Then DT%(D) = I + 8
        If DT%(D) = 10 Then DT%(D) = 3
        If DT%(D) = 12 Then DT%(D) = I + 10
        If DT%(D) = 12 Then DT%(D) = 13

        P9 = D

        If DT%(P9) <= 3 Then P7(P9) = 0
        If DT%(P9) = 4 Then P7(P9) = 1
        If DT%(P9) = 5 Or DT%(P9) = 6 Then P7(P9) = 2
        If DT%(P9) > 6 And DT%(P9) <= 10 Then P7(P9) = 3
        If DT%(P9) > 9 And DT%(P9) < 14 Then P7(P9) = 4
        If DT%(P9) = 14 Then P7(P9) = 5
        If DT%(P9) = 0 Or DT%(P9) = 4 Or DT%(P9) = 5 Or DT%(P9) = 8 Or DT%(P9) = 10 Then D2(P9) = 0
        If DT%(P9) = 1 Or DT%(P9) = 6 Or DT%(P9) = 9 Or DT%(P9) = 11 Then D2(P9) = 1
        If DT%(P9) = 2 Or DT%(P9) = 7 Or DT%(P9) = 12 Then D2(P9) = 2
        If DT%(P9) = 3 Or DT%(P9) = 13 Then D2(P9) = 3
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

        For I = 0 To 14
            Locate , 5: Print Using "# "; I;
            If I = 10 Then Locate , 5: Print "A ";
            If I = 11 Then Locate , 5: Print "B ";
            If I = 12 Then Locate , 5: Print "C ";
            If I = 13 Then Locate , 5: Print "D ";
            If I = 14 Then Locate , 5: Print "E ";
            Print D$(I)
        Next

        Print
        Locate 19, 5: Print "SELECT DEFENSIVE STYLE FOR "; B$(P9)

        Do
            I$ = GetKeyPress$
            I1 = Val(I$)
            Call LetterToNumber(I$, I1)
        Loop Until I1 >= 0 And I1 <= 14

        DT%(P9) = I1

        Select Case DT%(P9)

            Case 0 To 3
                P7(P9) = 0

            Case 4
                P7(P9) = 1

            Case 5, 6
                P7(P9) = 2

            Case 7 To 9
                P7(P9) = 3

            Case 10 To 13
                P7(P9) = 4

            Case 14
                If (H < 2 Or H >= 2 And gameClock! > 180) Then
                    Beep
                    Locate 23, 1: Print "CAN ONLY USE LAST 3 MIN OF 2ND HALF OR OT"
                    I$ = GetKeyPress$
                    wrongChoice% = 1
                Else
                    P7(P9) = 5
                End If

        End Select

    Loop Until wrongChoice% = 0

    Select Case DT%(P9)

        Case 0, 4, 5, 8, 10
            D2(P9) = 0

        Case 1, 6, 9, 11
            D2(P9) = 1

        Case 2, 7, 12
            D2(P9) = 2

        Case 3, 13
            D2(P9) = 3

    End Select

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
        If TR%(P9, 5) = 0 And C(P9, I, 0) = 0 And B%(P9, I) = 0 Then SQ% = SQ% + A!(P9, I, 3)
        If TR%(P9, 5) <> 0 And C(P9, I, 0) <> 0 And B%(P9, I) = 0 Then SQ% = SQ% + A!(P9, I, 3) * TR%(P9, 5) / C(P9, I, 0): A!(P9, I, 3) = A!(P9, I, 3) * TR%(P9, 5) / C(P9, I, 0)
    Next

    'So SQ% is a sum of A!(P9, X, 3) values...
    For I = 0 To 13
        If TR%(P9, 5) = 0 And C(P9, I, 0) = 0 And B%(P9, I) = 0 Then A!(P9, I, 3) = Int(A!(P9, I, 3) / SQ% * S9 + .5)
        If TR%(P9, 5) <> 0 And C(P9, I, 0) <> 0 And B%(P9, I) = 0 Then A!(P9, I, 3) = Int(A!(P9, I, 3) / SQ% * S9 + .5)
        If A!(P9, I, 3) < 1 Then A!(P9, I, 3) = 1
    Next I

    SQ% = 0

    For I = 0 To 13
        If B%(P9, I) = 0 Then SQ% = SQ% + A!(P9, I, 3)
    Next

    For I = 0 To 13
        If S9 > 120 And F5%(P9, 8) <> 99 Then A!(P9, I, 16) = A!(P9, I, 16) * (120 / S9): A!(P9, I, 16) = Int(A!(P9, I, 16) + .5)
    Next I

    DY = 7

    If S9 > 114 Then DY = (114 / S9) * DY: DY = Int(DY + .5)

End Sub


'----------------------------------------
'        COMPUTERSUBS Subroutine
'----------------------------------------
'This subroutine uses some basic logic to
'automatically handle player substitution
Sub COMPUTERSUBS ()

    If H >= 2 And gameClock! <= 60 And X3% = 1 And (S!(P9, 0) - S!(1 - P9, 0) = -3 Or S!(P9, 0) - S!(1 - P9, 0) = -6) Then
        OF%(P9) = 9
    End If

    M1 = 0
    M2 = 0
    M3 = 0
    M4 = 0
    skipG9% = 0

    If Not (U5 = 1 And P9 = CT And X = 50 Or U5 = 2 And P9 = P And X = 50) Then

        For I1 = 0 To 13

            M6 = A!(P9, I1, 3) - A!(P9, I1, 7) - A!(P9, I1, 11) - A!(P9, I1, 12)

            If H < 2 And M6 <= 0 Or H = 2 And gameClock! > 120 And M6 <= 0 Then M1 = M1 + 1

            'Disabled player or inactive spot
            If A!(P9, I1, 12) >= NF% Or AA$(P9, I1) = "XXX" Or B%(P9, I1) <> 0 Then M2 = M2 + 1

            If I1 <= 4 Then

                M6 = A!(P9, B1(P9, I1), 3) - A!(P9, B1(P9, I1), 7) - A!(P9, B1(P9, I1), 11) - A!(P9, B1(P9, I1), 12)

                If H = 1 And M6 / A!(P9, B1(P9, I1), 3) < .5 Then M3 = M3 + 1
                If H = 1 And gameClock! >= 600 And M6 / A!(P9, B1(P9, I1), 3) < .75 Then M3 = M3 + 1
                If H = 2 And gameClock! >= 600 And M6 / A!(P9, B1(P9, I1), 3) < .25 Then M3 = M3 + 1
                If H = 2 And gameClock! >= 300 And gameClock! < 600 And M6 / A!(P9, B1(P9, I1), 3) < .13 Then M3 = M3 + 1
                If H = 2 And gameClock! < 300 And M6 / A!(P9, B1(P9, I1), 3) < gameClock! / 2400 Then M3 = M3 + 1
                If M6 <= 0 And H < 2 Or M6 <= 0 And H = 2 And gameClock! > 120 Then M3 = M3 + 1
                If A!(P9, B1(P9, I1), 12) >= NF% Or A!(P9, B1(P9, I1), 12) >= NF% - 2 And H = 1 Or H = 2 And gameClock! > 720 And A!(P9, B1(P9, I1), 12) >= NF% - 1 Then M4 = M4 + 1

            End If

        Next

        If M3 >= 1 And 5 - M3 < 14 - M1 Or H = 2 And gameClock! < 120 And XS = 0 And G9%(P9) = 0 Or M4 >= 1 And M2 < 13 Or CTO% = 1 Then
            CTO% = 0
            XM = 1
            ''Call pbplog(4588)
            Call COMCOACH
            ''Call pbplog(4590)
            skipG9% = 1
        End If

    End If

    If skipG9% = 0 Then
        If H = 2 And gameClock! <= 120 Then G9%(P9) = 1
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
        FD% = FD% + FA%(P9, B1(P9, I))
        If FA%(P9, B1(P9, I)) <= -4 Then FQ% = 1
    Next

    If FD% < -15 Or FQ% = 1 Then D2(P9) = 2: P7(P9) = 0: DT%(P9) = 2

End Sub


'----------------------------------------
'           LINEUPS Subroutine
'----------------------------------------
'This subroutine displays the starting
'lineup for each team and gives the user
'an interface for changing the lineup
Sub LINEUPS ()

    Cls
    Color TR%(P9, 0), TR%(P9, 1): Print B$(P9);
    Color 15, 0: Print " TEAM ROSTER    COACH: "; gameCoach$(P9);
    Print "  RECORD: ";
    Print Using "##"; TR%(P9, 7);
    Print "-";
    Print Using "##"; TR%(P9, 8)

    Color 14: Print "# NAME         POS FG  FT 3FG %3A ORB DRB RB PA  S  B CX EF  PT  F  FC  PTS/G"
    Locate 3, 1

    For R1 = 0 To 13

        If B%(P9, R1) = 0 And AA$(P9, R1) <> "XXX" And AA$(P9, R1) <> "" Then

            Color 2
            If R1 <= 9 Then
                Print Using "# "; R1;
            Else
                Print X$(R1 - 10);
            End If

            Color 15
            Print Left$(AA$(P9, R1), 12);
            Locate , 16: Print PO$(P9, R1);
            Locate , 20

            L = 0
            L = A!(P9, R1, 3) - A!(P9, R1, 7) - A!(P9, R1, 11) - A!(P9, R1, 12)

            If L <= 0 Then L = 0

            Print Using "## ###  ##  ##  ##  ## ## "; A!(P9, R1, 0), A!(P9, R1, 1), A!(P9, R1, 20), A!(P9, R1, 19), A!(P9, R1, 21) * 2 / 10, A!(P9, R1, 2) * 2 / 10, (A!(P9, R1, 2) + A!(P9, R1, 21)) / 5;
            Print Using "## ## ##"; A!(P9, R1, 4), A!(P9, R1, 5), A!(P9, R1, 6);

            If L <= 0 And (H = 1 Or H = 2 And gameClock! > 120) Then Color 12
            Print Using "### "; L;

            Color 15
            If FA%(P9, R1) * -1 >= 5 And (H = 1 Or H = 2 And gameClock! > 120) Then Color 12
            Print Using "##"; FA%(P9, R1) * -1;

            Color 15
            Locate , 61: Print Using "###  "; A!(P9, R1, 8) * 2 + A!(P9, R1, 10) + W%(P9, R1, 0);

            If A!(P9, R1, 12) >= 4 Then Color 12
            Print Using "# "; A!(P9, R1, 12);

            Color 15
            Print Using "###   "; A!(P9, R1, 17);

            If RS(P9, R1, 0) = 0 Then
                Print "--.-"
            Else
                Print Using "##.#"; (RS(P9, R1, 3) * 2 + RS(P9, R1, 5) + RS(P9, R1, 13)) / RS(P9, R1, 0)
            End If

        End If

    Next R1

    Locate 18, 1: Color 9, 0
    Print String$(27, 205); Chr$(203); String$(51, 205)

    For R1 = 1 To 7:
        Locate R1 + 18, 28
        Print Chr$(186);
    Next

    Color 14, 0
    Locate 19, 1: Print "STARTING LINEUP"

    If C1 = 1 Then

        For M = 0 To 4

            Locate M + 20, 1
            Color 2, 0
            Print Using "# "; M;
            Color 15, 0
            Print "ENTER "; PS$(M); " # ";

            Z = B1(P9, M)
            CV% = 0

            For IP% = 0 To Z
                If B%(P9, IP%) <> 0 Or AA$(P9, IP%) = "" Or AA$(P9, IP%) = "XXX" Then CV% = CV% + 1
            Next

            Print AA$(P9, Z);
            Locate Z + 3 - CV%, 3
            Color TR%(P9, 0), TR%(P9, 1)
            Print Left$(AA$(P9, Z), 12)
            Color 15, 0

        Next

    Else

        For I = 0 To 4

            Do

                Locate I + 20, 1
                Color 2, 0
                Print Using "# "; I;
                Color 15
                Print "ENTER "; PS$(I); " # ";

                I$ = GetKeyPress$
                Call LetterToNumber(I$, J)

                If J >= 0 And J <= 13 And A!(P9, J, 12) < NF% And AA$(P9, J) <> "XXX" And B%(P9, J) = 0 Then
                    For I2 = 0 To 4
                        If B1(P9, I2) = J Then Exit For
                    Next I2
                End If

            Loop Until J >= 0 And J <= 13

            B1(P9, I) = J
            Z = J
            CV% = 0

            For IP% = 0 To Z
                If B%(P9, IP%) <> 0 Or AA$(P9, IP%) = "" Or AA$(P9, IP%) = "XXX" Then CV% = CV% + 1
            Next

            Print AA$(P9, J);
            Locate J + 3 - CV%, 3: Color TR%(P9, 0), TR%(P9, 1)
            Print Left$(AA$(P9, J), 12)
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

        Loop Until K >= 0 And K <= 6

        If K = 6 Then

            skipMenu% = 0

            CV% = 0

            For I1 = 0 To 13
                If B%(P9, I1) <> 0 Or AA$(P9, I1) = "XXX" Or A!(P9, I1, 12) >= NF% Then CV% = CV% + 1
            Next

            If 14 - CV% >= 5 Then

                For I1 = 0 To 4

                    If A!(P9, B1(P9, I1), 12) >= NF% Then
                        Locate 20, 29
                        Color 14, 0: Print AA$(P9, B1(P9, I1)); " HAS FOULED OUT"
                        skipMenu% = 1
                        Exit For

                    End If

                Next

            End If

            If skipMenu% = 0 Then Call PlayStyle

        Else

            If X = 50 And K = B And P9 = P Then

                Color 14, 0
                Locate 20, 29: Print AA$(P9, B1(P9, K)); " IS AT THE FT LINE"

            Else

                Locate 25, 1: Print Space$(25);

                J = B1(P9, K)
                Z = J
                TM%(P9, J) = gameClock!
                CV% = 0

                For IP% = 0 To Z
                    If B%(P9, IP%) <> 0 Or AA$(P9, IP%) = "" Or AA$(P9, IP%) = "XXX" Then
                        CV% = CV% + 1
                    End If
                Next

                Locate J + 3 - CV%, 3
                Color 15, 0: Print Left$(AA$(P9, J), 12)
                Locate K + 20, 1: Print Space$(26);

                B1(P9, K) = -1

                Locate , 1: Color 2: Print Using "# "; K;
                Color 15: Print "ENTER "; PS$(K); " # ";


                Do
                    I$ = GetKeyPress$
                    Call LetterToNumber(I$, J)

                    If J >= 0 And J <= 13 And A!(P9, J, 12) < NF% And AA$(P9, J) <> "XXX" And B%(P9, J) = 0 Then
                        For I2 = 0 To 4
                            If B1(P9, I2) = J Then Exit For
                        Next I2

                    End If

                Loop Until J >= 0 And J <= 13

                B1(P9, K) = J
                Z = J

                'FATIGUE
                If TM%(P9, J) - gameClock! > 60 And TM%(P9, J) < 120 Then FA%(P9, J) = FA%(P9, J) + 1
                If TM%(P9, J) - gameClock! >= 120 Then FA%(P9, J) = FA%(P9, J) + 4
                If FA%(P9, J) > 0 Then FA%(P9, J) = 0

                CV% = 0
                For IP% = 0 To Z
                    If B%(P9, IP%) <> 0 Or AA$(P9, IP%) = "" Or AA$(P9, IP%) = "XXX" Then
                        CV% = CV% + 1
                    End If
                Next

                Print AA$(P9, J);
                Locate J + 3 - CV%, 3
                Color TR%(P9, 0), TR%(P9, 1): Print Left$(AA$(P9, J), 12)
                Color 15, 0

            End If

        End If

    Loop Until K = 6

End Sub


'----------------------------------------
'          PlayStyle Subroutine
'----------------------------------------
'This subroutine walks the user through
'the selection of play styles for their
'team on both offense and defense.
Sub PlayStyle ()

    109 '
    Color TR%(P9, 0), TR%(P9, 1)
    Cls
    Print ; Tab(33); "OFFENSIVE STYLE"
    Print

    For I = 0 To 9
        Locate , 5: Print Using "# "; I;: Print O$(I)
    Next

    Print
    Locate 14, 5: Print "SELECT OFFENSIVE STYLE FOR "; B$(P9)

    Do
        I$ = GetKeyPress$
        I1 = Val(I$)
    Loop Until I1 >= 0 And I1 <= 9

    If (I1 = 3 Or I1 = 4 Or I1 = 7 Or I1 = 9) And (H < 2 Or H >= 2 And gameClock! > 180) Then Beep: Locate 18, 1: Print "CAN ONLY USE LAST 3 MIN OF 2ND HALF OR OT": I$ = GetKeyPress$: GoTo 109
    If I1 = 8 And D2(1 - P9) <> 2 Then Beep: Locate 18, 1: Print "DEFENSE NOT IN PASSIVE ZONE": I$ = GetKeyPress$: GoTo 109
    If I1 = 4 And S!(P9, 0) - S!(1 - P9, 0) > -11 Then Beep: Locate 18, 1: Print "ILLEGAL SELECTION": I$ = GetKeyPress$: GoTo 109
    If I1 = 9 And X3% = 0 Then Beep: Locate 18, 1: Print "NOT USING 3 POINT SHOT": I$ = GetKeyPress$: GoTo 109
    If I1 = 5 And sClockVal = 0 Then Beep: Locate 18, 1: Print "NOT USING SHOT CLOCK": I$ = GetKeyPress$: GoTo 109
    If I1 = 7 And S!(P9, 0) - S!(1 - P9, 0) <= 0 Then Beep: Locate 18, 1: Print "ILLEGAL SELECTION": I$ = GetKeyPress$: GoTo 109

    'How to loop around this with so many conditions?
    'Possibly build a bunch of nested If/Then statements?
    OF%(P9) = I1
    CZ%(P9) = 0

    If Not (H < 2 Or H >= 2 And gameClock! > 120) Then
        Print "CRASH OFFENSIVE BOARDS (LAST 2 MIN) (YN)";
        I$ = GetKeyPress$
        If UCase$(I$) = "Y" Then CZ%(P9) = 1
    End If

    Call DEFENSEMENU

End Sub


'----------------------------------------
'          FOULSAFE Subroutine
'----------------------------------------
' ...explanation...
Sub FOULSAFE ()

    Cls

    Color 15

    A = 0
    S2 = 1
    S3 = 1
    SW = 0

    For ZZ = 0 To 4
        If A!(P9, B1(P9, ZZ), 22) = 50 Then A = A + 1: If SW = 0 Then Print: Print "THE FOLLOWING PLAYER(S) ARE PLAYING SAFE BUT CAN NOW PLAY NORMAL IF YOU WISH": SW = 1
        If A!(P9, B1(P9, ZZ), 22) = 50 And SW = 1 And S2 < 6 Then Locate S2 + 3, 1: Print S2; "-"; AA$(P9, B1(P9, ZZ)): P5(A) = S2: P4(A) = ZZ: S2 = S2 + 1: S3 = S3 + 1
    Next ZZ

    skipChangeToSafe% = 0

    If SW <> 0 Then

        A = 1
        Print
        Print "Z - CONTINUE"

        Do
            I$ = GetKeyPress$
            If UCase$(I$) = "Z" And MJ = 0 Then Exit Do
            If UCase$(I$) = "Z" And MJ = 25 Then skipChangeToSafe% = 1: Exit Do
            P6 = Val(I$)

            A = P6

            If P6 = P5(P6) Then

                A!(P9, B1(P9, P4(A)), 17) = A!(P9, B1(P9, P4(A)), 17) * 3
                A!(P9, B1(P9, P4(A)), 22) = 0

                Locate P5(A) + 3, 20
                Print AA$(P9, B1(P9, P4(A))); " WILL NOW PLAY NORMAL DEFENSE!"
                Call STEALADJ

                D1(P9) = D1(P9) - 1
                A = A + 1
                MJ = 25

            End If

        Loop Until P6 >= 1 And P6 <= (S2 + 1) And P6 <> P5(P6)

    End If

    If skipChangeToSafe% = 0 Then

        A = 0
        S2 = 1

        For ZZ = 0 To 4
            If H <= 1 And A!(P9, B1(P9, ZZ), 12) >= NF% - 3 And A!(P9, B1(P9, ZZ), 22) = 0 Or A!(P9, B1(P9, ZZ), 12) = NF% - 1 And A!(P9, B1(P9, ZZ), 22) = 0 Then
                A = A + 1
                If S2 = 1 Then Cls: Print: Print "THE FOLLOWING PLAYER(S) CAN ME MADE TO PLAY SAFE IF YOU WISH": Print
            End If

            If A = 0 And ZZ > 3 Then 1230

            If S2 = A And S2 < 6 Then Print S2; "- "; AA$(P9, B1(P9, ZZ)): P5(A) = S2: P4(A) = ZZ: S2 = S2 + 1: S3 = S3 + 1

        Next ZZ

        A = 1
        Print
        Print "Z - CONTINUE"

        1220 '
        Do
            I$ = GetKeyPress$
            P6 = Val(I$)
        Loop Until P6 >= 1 And P6 <= (S2 + 1) Or UCase$(I$) = "Z"

        If UCase$(I$) <> "Z" Then

            A = P6

            If P6 = P5(P6) Then

                A!(P9, B1(P9, P4(A)), 17) = Int((A!(P9, B1(P9, P4(A)), 17) + 1) / 3)
                A!(P9, B1(P9, P4(A)), 22) = 50

                Locate P5(A) + 3, 20: Print AA$(P9, B1(P9, P4(A))); " WILL NOW PLAY SAFE!"
                Call STEALADJ

                D1(P9) = D1(P9) + 1
                A = A + 1
                S3 = S3 + 1

                GoTo 1220

            End If

        End If

    End If

    1230 '
    MJ = 0

    FO = 99

    If H = 1 And gameClock! = 1200 Then FO = 0

    'Call UPDATESCREEN
    Call SCOREBOARD

End Sub


'----------------------------------------
'          FOULSAFE Subroutine
'----------------------------------------
' ...explanation...
Sub FOULHARD ()

    For I = 0 To 1

        For I1 = 0 To 13
            If A!(I, I1, 22) = 50 Then A!(I, I1, 22) = 0: A!(I, I1, 17) = A!(I, I1, 17) * 3: D1(I) = D1(I) - 1: MJ = 50
        Next I1

        If MJ = 50 Then
            MJ = 0
            F5%(I, 0) = 0
            For I1 = 0 To 4: F5%(I, 0) = F5%(I, 0) + A!(I, B1(I, I1), 17): Next I1
        End If

    Next I

End Sub


'----------------------------------------
'          STEALADJ Subroutine
'----------------------------------------
' ...explanation...
Sub STEALADJ ()

    For I = 0 To 1
        F5%(I, 0) = 0
        F5%(I, 1) = 0
        For I1 = 0 To 4
            F5%(I, 0) = F5%(I, 0) + A!(I, B1(I, I1), 17)
            F5%(I, 1) = F5%(I, 1) + A!(I, B1(I, I1), 18)
        Next I1
    Next I

End Sub


'----------------------------------------
'         SHOTPERCENT Subroutine
'----------------------------------------
'This subroutine calculates the chance
'of a shot being made based on the active
'defense / offense / players.
Sub SHOTPERCENT ()

    FX% = A!(P, B1(P, B), 0) + TP%(D) + TP%(P)
    TP%(D) = 0
    TP%(P) = 0

    If FA%(P, B1(P, B)) <= -5 Then FX% = FX% - 10

    F! = FX% + D1(D) + D8(0, D2(D)): J8% = 0

    If N = P Then
        F! = F! + HF%(0, TR%(P, 6))
    Else
        If N <> 2 Then F! = F! + HF%(1, TR%(P, 6))
    End If

    F3S% = Int(Rnd(1) * 100) + 1
    TPA% = 0

    If DT%(D) = 2 Or DT%(D) = 7 Or DT%(D) = 12 Then TPA% = 3
    If DT%(D) = 1 Or DT%(D) = 6 Or DT%(D) = 9 Or DT%(D) = 11 Then TPA% = -5

    skipCalcs% = 0

    If OF%(P) = 9 And (F3S% > A!(P, B1(P, B), 19) + F5%(D, 3) + TPA% Or A!(P, B1(P, B), 19) = 0) Then
        F3S% = Int(Rnd(1) * 5) + 11
        J8% = 1
    Else
        If OF%(P) <> 9 Then

            If F3S% > A!(P, B1(P, B), 19) + F5%(D, 3) + TPA% Or A!(P, B1(P, B), 19) = 0 Then
                F3S% = Int(Rnd(1) * 5) + 3
                skipCalcs% = 1
            End If

        End If

        If skipCalcs% = 0 Then
            FX% = A!(P, B1(P, B), 20)

            If FA%(P, B1(P, B)) <= -5 Then FX% = Int(FX% * .67 + .5)

            F3S% = FX% + F5%(D, 2): J8% = 1

            If A!(P, B1(P, B), 3) - A!(P, B1(P, B), 7) - A!(P, B1(P, B), 11) - A!(P, B1(P, B), 12) <= 0 And (H = 1 Or H = 2 And gameClock! > 120) Then F3S% = Int(Rnd(1) * 5) + 3: J8% = 0
        End If

    End If

End Sub


'----------------------------------------
'           THREEFT Subroutine
'----------------------------------------
'This subroutine walks the user through
'taking 3 free throw attempts.
Sub THREEFT ()

    A!(P, B1(P, B), 10) = A!(P, B1(P, B), 10) + 1

    RN = Int(Rnd(1) * 100) + 1

    Select Case F3

        Case 1
            If RN <= 50 Then
                U$ = "FREE THROW IS GOOD"
            Else
                U$ = "HE HITS THE FIRST FREE THROW"
            End If

        Case 2
            If RN <= 50 Then
                U$ = "SECOND FREE THROW IS GOOD"
            Else
                U$ = "HE HITS THE SECOND ONE"
            End If

        Case 3
            If RN <= 50 Then
                U$ = "THIRD FREE THROW IS GOOD"
            Else
                U$ = "HE HITS THE LAST ONE"
            End If

    End Select

    Call PBP

    S!(P, H) = S!(P, H) + 1
    S!(P, 0) = S!(P, 0) + 1

    Call UPDATESCREEN

End Sub


'----------------------------------------
'           MADEFT Subroutine
'----------------------------------------
'This subroutine handles score, stats and
'Play-by-Play for a successful free throw
Sub MADEFT ()

    A!(P, B1(P, B), 10) = A!(P, B1(P, B), 10) + 1

    If pbpDelay! <> 0 Then
        Locate F3 + 9, 1
        Call DELAY
        Print F3; " GOOD"
    End If

    S!(P, H) = S!(P, H) + 1
    S!(P, 0) = S!(P, 0) + 1

    Call UPDATESCREEN
    Call DELAY

End Sub


'----------------------------------------
'           MISSFT Subroutine
'----------------------------------------
'This subroutine handles Play-by-Play
'for a missed free throw
Sub MISSFT ()

    If pbpDelay! <> 0 Then
        Call DELAY
        Locate F3 + 9, 1
        Print F3; " MISSED"
        Call DELAY
    End If

End Sub


'----------------------------------------
'            BADFT Subroutine
'----------------------------------------
'This subroutine handles Play-by-Play for
'missed FT's when there are 3 attempts.
Sub BADFT ()

    If F3 = 1 Then U$ = "FIRST FREE THROW IS MISSED"
    If F3 = 2 Then U$ = "SECOND FREE THROW IS MISSED"
    If F3 = 3 Then U$ = "THIRD FREE THROW IS MISSED"
    Call PBP

End Sub


'----------------------------------------
'           PRDFAT Subroutine
'----------------------------------------
'This subroutine calculates fatigue of the
'defense due pressure on the defense.
Sub PRDFAT ()

    For I = 0 To 4

        R0! = Int(Rnd(1) * 100) + 1

        If R0! <= X0% Then
            FA%(D, B1(D, I)) = FA%(D, B1(D, I)) + X1%
        Else
            FA%(D, B1(D, I)) = FA%(D, B1(D, I)) + X1% + 1
        End If

        If FA%(D, B1(D, I)) > 0 Then FA%(D, B1(D, I)) = 0
        If FA%(D, B1(D, I)) < -5 Then FA%(D, B1(D, I)) = -5

    Next

End Sub


'----------------------------------------
'           PRDFAT Subroutine
'----------------------------------------
'This subroutine calculates fatigue of the
'defense due pressure on the defense.
Sub PRPFAT ()

    For I = 0 To 4

        R0! = Int(Rnd(1) * 100) + 1

        If R0! <= X0% Then
            FA%(P, B1(P, I)) = FA%(P, B1(P, I)) + X1%
        Else
            FA%(P, B1(P, I)) = FA%(P, B1(P, I)) + X1% + 1
        End If

        If FA%(P, B1(P, I)) > 0 Then FA%(P, B1(P, I)) = 0
        If FA%(P, B1(P, I)) < -5 Then FA%(P, B1(P, I)) = -5

    Next

End Sub


'----------------------------------------
'         WHOGESTBALL Subroutine
'----------------------------------------
'...explanation...
Sub WHOGETSBALL ()

    CNT% = 0

    While (CNT% < 4)

        1041 '
        CNT% = CNT% + 1

        If CNT% <> 4 Then

            M9 = 0
            B = Int(Rnd(1) * 5)
            X = Int(Rnd(1) * 100) + 1

            If X > 25 Then

                For I = 0 To 4
                    M8 = Int(C(P, B1(P, I), 1) / C(P, B1(P, I), 0) + .5) - A!(P, B1(P, I), 7)
                    If M8 <= 0 Then M8 = 0
                    If M8 >= M9 Then B = I: M9 = M8
                Next I

            End If

            M8 = A!(P, B1(P, B), 3) - A!(P, B1(P, B), 7) - A!(P, B1(P, B), 11) - A!(P, B1(P, B), 12)

            If A!(P, B1(P, B), 4) >= 5 And M8 / A!(P, B1(P, B), 3) <= 0.4 And H = 1 Then 1041
            If A!(P, B1(P, B), 4) >= 5 And M8 / A!(P, B1(P, B), 3) <= 0.2 And H = 2 And gameClock! > 120 Then 1041
            If A!(P, B1(P, B), 3) < 10 And A!(P, B1(P, B), 4) >= 5 And M8 / A!(P, B1(P, B), 3) <= 0.75 And H = 1 Then 1041
            If A!(P, B1(P, B), 3) < 10 And A!(P, B1(P, B), 4) >= 5 And M8 / A!(P, B1(P, B), 3) <= 0.5 And H = 2 And gameClock! > 120 Then 1041
            GoTo 1041

        End If

    Wend

End Sub


'----------------------------------------
'       BALLDISTRIBUTE Subroutine
'----------------------------------------
' ...explanation...
Sub BALLDISTRIBUTE ()

    R0! = Int(Rnd(1) * 100) + 1

    If U5 = 0 And coachOpt = 1 Or U5 = 1 And D = CT And coachOpt = 1 Or U5 = 1 And P = CT Or U5 = 2 Then
        Call WHOGETSBALL
    Else
        'Select / Case statement
        If D2(D) = 2 Or D2(D) = 3 Then 415
        If S9 <= 90 And R0! <= 15 Then 424
        If S9 <= 100 And R0! <= 20 Then 424
        If S9 <= 110 And R0! <= 25 Then 424
        If S9 <= 120 And R0! <= 30 Then 424
        If S9 <= 130 And R0! <= 38 Then 424
        If S9 <= 140 And R0! <= 53 Then 424
        If S9 <= 150 And R0! <= 60 Then 424
        If S9 > 150 And R0! <= 67 Then 424
        GoTo 423

        '----------

        'Select / Case statement
        415 '
        If S9 <= 90 And R0! <= 8 Then 424
        If S9 <= 100 And R0! <= 10 Then 424
        If S9 <= 110 And R0! <= 13 Then 424
        If S9 <= 120 And R0! <= 15 Then 424
        If S9 <= 130 And R0! <= 18 Then 424
        If S9 <= 140 And R0! <= 53 Then 424
        If S9 <= 150 And R0! <= 60 Then 424
        If S9 > 150 And R0! <= 67 Then 424

        423 '
        B = Int(Rnd(1) * 5)
        GoTo 427

        '----------

        Do
            424 '
            If pbpDelay! <> 0 And PB% = 0 Then
                U$ = "PASS TO CHOICE (0-4)"
                Call PBP
            End If

            If pbpDelay! > 0 And PB% = 1 Then
                'IF AP=1 AND U5=2 OR pbpDelay!=0 THEN 425
                For I = 8 To 15
                    Locate I, 1
                    Call BLANKLINE
                Next I
                Locate 8, 1
                Print "PASS TO CHOICE (0-4)"
                Call DELAY
            End If

            I$ = GetKeyPress$
            If UCase$(I$) = "T" Then
                Call TICKER
                Call SCOREBOARD
            Else
                B = Val(I$)
            End If

        Loop Until B >= 0 And B <= 4

    End If

    427 '

End Sub


'----------------------------------------
'       DEADBALLOPTIONS Subroutine
'----------------------------------------
'This subroutine handles any deadball
'situations. If not computer controlled,
'the user is given a chance to handle
'any changes to lineup, strategy, players.
Sub DEADBALLOPTIONS ()

    220 '
    PB$ = ""
    Call CLEARPBP

    If U5 = 2 Then P9 = 0: Call COMPUTERSUBS: Call FATIGUE: P9 = 1: Call COMPUTERSUBS: Call FATIGUE
    If U5 = 1 Then P9 = CT: Call COMPUTERSUBS: Call FATIGUE

    FO = 0
    XM = 0

    Call CLEARPBPBOX

    If U5 = 2 Then
        S2 = 0
        P9 = P
        C1 = 2
        Call CheckGameClock
        Call STEALADJ
    Else

        Locate 11, 1
        Color 14, 0
        Print "<ENTER>-CONTINUE"
        Print "1-SUB "; B$(P)
        Print "2-SUB "; B$(D)
        Print "3-CHANGE STRATEGY "; B$(P)
        Print "4-CHANGE STRATEGY "; B$(D)
        Print "5-TIME OUT "; B$(P)
        Print "6-TIME OUT "; B$(D)

        221 '
        I$ = GetKeyPress$
        P3 = Val(I$)

        If P3 < 1 Or P3 > 6 Or U5 = 1 And (P3 = 1 Or P3 = 3 Or P3 = 5 And T1(P) <= 0) And P = CT Or U5 = 1 And (P3 = 2 Or P3 = 4 Or P3 = 6 And T1(D) <= 0) And D = CT Then

            'This is essentially the case for automated input,
            'or anything outside 1-6 (ie, press ENTER)
            S2 = 0
            P9 = P
            C1 = 2
            Call CheckGameClock
            Call STEALADJ

            If LC% = 1 Then
                LC% = 0
                Call CheckGameClock
                Call SCOREBOARD
            End If

            If pbpDelay! <> 0 Then
                For I = 8 To 15: Locate I, 1: Call BLANKLINE: Next I
            End If

        Else

            Select Case P3

                Case 1
                    S2 = P

                Case 2
                    S2 = D

                Case 3
                    P9 = P
                    Call PlayStyle
                    Call SCOREBOARD
                    GoTo 220

                Case 4
                    P9 = D
                    Call PlayStyle
                    Call SCOREBOARD
                    GoTo 220

                Case 5
                    If T1(P) <= 0 Or P3 = 6 And T1(D) <= 0 Then
                        GoTo 221
                    Else
                        T1(P) = T1(P) - 1
                        Call RESETFATIGUE
                        Call SCOREBOARD
                        GoTo 220
                    End If

                Case 6
                    T1(D) = T1(D) - 1
                    Call RESETFATIGUE
                    Call SCOREBOARD
                    GoTo 220

            End Select

            C1 = 1
            P9 = S2
            Call LINEUPS
            Call FOULSAFE
            P9 = P
            LC% = 1

            GoTo 220

        End If 'Done checking input

    End If 'Done checking computer control

End Sub


'----------------------------------------
'        RESETFATIGUE Subroutine
'----------------------------------------
'This subroutine resets player fatigue
'levels for a fresh start.
Sub RESETFATIGUE ()

    For I = 0 To 1

        For I1 = 0 To 1
            PR%(I, I1) = 0
        Next

    Next

    For I = 0 To 1

        For I1 = 0 To 13
            FA%(I, I1) = FA%(I, I1) + 1
            If FA%(I, I1) > 0 Then FA%(I, I1) = 0
        Next

    Next

End Sub


'------------------------------
'    STOPPAGE Subroutine
'------------------------------
' ...explanation...
Sub STOPPAGE ()

    810 '
    Call CLEARPBP

    If U5 <> 0 Then
        If U5 = 2 Then P9 = 0: Call COMPUTERSUBS: Call FATIGUE: P9 = 1: Call COMPUTERSUBS: Call FATIGUE
        If U5 = 1 Then P9 = CT: Call COMPUTERSUBS: Call FATIGUE
        XM = 0
    End If

    If U5 = 2 Then
        S2 = 0
        P9 = P
        C1 = 2
        Call CheckGameClock
    Else

        If PB% = 0 Then Call CLEARPBPBOX
        If PB% = 1 And pbpDelay! <> 0 Then Locate 11, 1
        Color 14, 0
        Print "<ENTER>-CONTINUE"
        Print "1-TIME OUT "; B$(P)
        Print "2-TIME OUT "; B$(D)
        Print "3-CHANGE STRATEGY "; B$(P)
        Print "4-CHANGE STRATEGY "; B$(D)

        813 '
        I$ = GetKeyPress$
        P3 = Val(I$)

        If P3 < 1 Or P3 > 4 Then
            S2 = 0
            P9 = P
            C1 = 2
            Call CheckGameClock
        Else
            If P3 = 1 And T1(P) <= 0 Or P3 = 2 And T1(D) <= 0 Or (P3 = 1 Or P3 = 3) And P = CT And U5 = 1 Or (P3 = 2 Or P3 = 4) And D = CT And U5 = 1 Then 813
            If P3 = 3 Then P9 = P: Call PlayStyle: Call SCOREBOARD: GoTo 810
            If P3 = 4 Then P9 = D: Call PlayStyle: Call SCOREBOARD: GoTo 810

            For I = 0 To 1
                For I1 = 0 To 1
                    PR%(I, I1) = 0
                Next
            Next

            If P3 = 1 Then T1(P) = T1(P) - 1: Color 14, 0: Call SCOREBOARD: Call DEADBALLOPTIONS
            If P3 = 2 Then T1(D) = T1(D) - 1: Color 14, 0: Call SCOREBOARD: Call DEADBALLOPTIONS

        End If

        Call CLEARPBPBOX

    End If

End Sub


'----------------------------------------
'         PRESSGUARD Subroutine
'----------------------------------------
'This subroutine handles PBP and stats
'for when a Guard is pressing
Sub PRESSGUARD ()

    Call WINDEX

    If pbpDelay! <> 0 Then
        If P7(D) > 0 Then U$ = B$(D) + " IS PRESSING": Call SELECTPBP
    End If

    If P7(D) <> 0 And P7(D) <= 2 Then

        If U5 = 2 Or U5 = 1 And P = CT Then
            ' COMPUTER GUARD FOR PRESS
            BU% = 1
            If A!(P, B1(P, 0), 4) >= A!(P, B1(P, 1), 4) And FA%(P, B1(P, 0)) <= 3 Then BU% = 0

        Else
            Call FRAMEOPTION(8, P)
            Locate 10, 15

            Print B$(P); " SELECT GUARD TO HANDLE BALL:"

            Locate 12, 14
            Locate , 14: Print "#   NAME"; Tab(35); "PA"; Tab(38); "FA"
            Locate , 14: Print "0 - "; AA$(P, B1(P, 0)); Tab(35); A!(P, B1(P, 0), 4); Tab(38); Abs(FA%(P, B1(P, 0)))
            Locate , 14: Print "1 - "; AA$(P, B1(P, 1)); Tab(35); A!(P, B1(P, 1), 4); Tab(38); Abs(FA%(P, B1(P, 1)))

            Do
                I$ = GetKeyPress$
                I1 = Val(I$)
            Loop Until I1 >= 0 And I1 <= 1

            BU% = I1
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

        For I2 = 0 To 14
            V9(I2) = 0
        Next

        For I1 = 0 To 13
            V9(0) = V9(0) + A!(I, I1, 8)
            V9(1) = V9(1) + A!(I, I1, 7)
            V9(2) = V9(2) + A!(I, I1, 10)
            V9(3) = V9(3) + A!(I, I1, 9)
            V9(4) = V9(4) + A!(I, I1, 11)
            V9(5) = V9(5) + (A!(I, I1, 8) * 2) + A!(I, I1, 10) + W%(I, I1, 0)
            V9(13) = V9(13) + W%(I, I1, 0)
            V9(14) = V9(14) + W%(I, I1, 1)
            V9(6) = V9(6) + A!(I, I1, 12)
            V9(7) = V9(7) + A!(I, I1, 14)
            V9(8) = V9(8) + A!(I, I1, 15)
            V9(9) = V9(9) + A!(I, I1, 13)
            V9(11) = V9(11) + A!(I, I1, 24)
            V9(12) = V9(12) + A!(I, I1, 23)
        Next

        If H = 1 Or H = 2 Then

            For I2 = 0 To 14
                QR(I, H, I2) = V9(I2) - QR(I, H - 1, I2)
            Next

            For I1 = 0 To 13

                For I2 = 7 To 15
                    QQ(I, H, I1, I2 - 6) = A!(I, I1, I2) - QQ(I, H - 1, I1, I2 - 6)
                Next

                QQ(I, H, I1, 10) = W%(I, I1, 0) - QQ(I, H - 1, I1, 10)
                QQ(I, H, I1, 11) = W%(I, I1, 1) - QQ(I, H - 1, I1, 11)
                QQ(I, H, I1, 12) = A!(I, I1, 24) - QQ(I, H - 1, I1, 12)
                QQ(I, H, I1, 13) = A!(I, I1, 8) * 2 + A!(I, I1, 10) + W%(I, I1, 0) - QQ(I, H - 1, I1, 13)
                QQ(I, H, I1, 14) = A!(I, I1, 23) - QQ(I, H - 1, I1, 14)

            Next

        End If

        If H = 3 Then

            For I2 = 0 To 14
                QR(I, H, I2) = V9(I2) - QR(I, H - 1, I2) - QR(I, H - 2, I2)
            Next

            For I1 = 0 To 13

                For I2 = 7 To 15
                    QQ(I, H, I1, I2 - 6) = A!(I, I1, I2) - QQ(I, H - 1, I1, I2 - 6) - QQ(I, H - 2, I1, I2 - 6)
                Next

                QQ(I, H, I1, 10) = W%(I, I1, 0) - QQ(I, H - 1, I1, 10) - QQ(I, H - 2, I1, 10)
                QQ(I, H, I1, 11) = W%(I, I1, 1) - QQ(I, H - 1, I1, 11) - QQ(I, H - 2, I1, 11)
                QQ(I, H, I1, 12) = A!(I, I1, 24) - QQ(I, H - 1, I1, 12) - QQ(I, H - 2, I1, 12)
                QQ(I, H, I1, 13) = A!(I, I1, 8) * 2 + A!(I, I1, 10) + W%(I, I1, 0) - QQ(I, H - 1, I1, 13) - QQ(I, H - 2, I1, 13)
                QQ(I, H, I1, 14) = A!(I, I1, 23) - QQ(I, H - 1, I1, 14) - QQ(I, H - 2, I1, 14)

            Next

        End If

        If H = 4 Then

            For I2 = 0 To 14
                QR(I, H, I2) = V9(I2) - QR(I, H - 1, I2) - QR(I, H - 2, I2) - QR(I, H - 3, I2)
            Next

            For I1 = 0 To 13

                For I2 = 7 To 15
                    QQ(I, H, I1, I2 - 6) = A!(I, I1, I2) - QQ(I, H - 1, I1, I2 - 6) - QQ(I, H - 2, I1, I2 - 6) - QQ(I, H - 3, I1, I2 - 6)
                Next

                QQ(I, H, I1, 10) = W%(I, I1, 0) - QQ(I, H - 1, I1, 10) - QQ(I, H - 2, I1, 10) - QQ(I, H - 3, I1, 10)
                QQ(I, H, I1, 11) = W%(I, I1, 1) - QQ(I, H - 1, I1, 11) - QQ(I, H - 2, I1, 11) - QQ(I, H - 3, I1, 11)
                QQ(I, H, I1, 12) = A!(I, I1, 24) - QQ(I, H - 1, I1, 12) - QQ(I, H - 2, I1, 12) - QQ(I, H - 3, I1, 12)
                QQ(I, H, I1, 13) = A!(I, I1, 8) * 2 + A!(I, I1, 10) + W%(I, I1, 0) - QQ(I, H - 1, I1, 13) - QQ(I, H - 2, I1, 13) - QQ(I, H - 3, I1, 13)
                QQ(I, H, I1, 14) = A!(I, I1, 23) - QQ(I, H - 1, I1, 14) - QQ(I, H - 2, I1, 14) - QQ(I, H - 3, I1, 14)

            Next

        End If

        If H = 5 Then

            For I2 = 0 To 14
                QR(I, H, I2) = V9(I2) - QR(I, H - 1, I2) - QR(I, H - 2, I2) - QR(I, H - 3, I2) - QR(I, H - 4, I2)
            Next

            For I1 = 0 To 13

                For I2 = 7 To 15
                    QQ(I, H, I1, I2 - 6) = A!(I, I1, I2) - QQ(I, H - 1, I1, I2 - 6) - QQ(I, H - 2, I1, I2 - 6) - QQ(I, H - 3, I1, I2 - 6) - QQ(I, H - 4, I1, I2 - 6)
                Next

                QQ(I, H, I1, 10) = W%(I, I1, 0) - QQ(I, H - 1, I1, 10) - QQ(I, H - 2, I1, 10) - QQ(I, H - 3, I1, 10) - QQ(I, H - 4, I1, 10)
                QQ(I, H, I1, 11) = W%(I, I1, 1) - QQ(I, H - 1, I1, 11) - QQ(I, H - 2, I1, 11) - QQ(I, H - 3, I1, 11) - QQ(I, H - 4, I1, 11)
                QQ(I, H, I1, 12) = A!(I, I1, 24) - QQ(I, H - 1, I1, 12) - QQ(I, H - 2, I1, 12) - QQ(I, H - 3, I1, 12) - QQ(I, H - 4, I1, 12)
                QQ(I, H, I1, 13) = A!(I, I1, 8) * 2 + A!(I, I1, 10) + W%(I, I1, 0) - QQ(I, H - 1, I1, 13) - QQ(I, H - 2, I1, 13) - QQ(I, H - 3, I1, 13) - QQ(I, H - 4, I1, 13)
                QQ(I, H, I1, 14) = A!(I, I1, 23) - QQ(I, H - 1, I1, 14) - QQ(I, H - 2, I1, 14) - QQ(I, H - 3, I1, 14) - QQ(I, H - 4, I1, 14)

            Next

        End If

        If H = 6 Then

            For I2 = 0 To 14
                QR(I, H, I2) = V9(I2) - QR(I, H - 1, I2) - QR(I, H - 2, I2) - QR(I, H - 3, I2) - QR(I, H - 4, I2) - QR(I, H - 5, I2)
            Next

            For I1 = 0 To 13

                For I2 = 7 To 15
                    QQ(I, H, I1, I2 - 6) = A!(I, I1, I2) - QQ(I, H - 1, I1, I2 - 6) - QQ(I, H - 2, I1, I2 - 6) - QQ(I, H - 3, I1, I2 - 6) - QQ(I, H - 4, I1, I2 - 6) - QQ(I, H - 5, I1, I2 - 6)
                Next

                QQ(I, H, I1, 10) = W%(I, I1, 0) - QQ(I, H - 1, I1, 10) - QQ(I, H - 2, I1, 10) - QQ(I, H - 3, I1, 10) - QQ(I, H - 4, I1, 10) - QQ(I, H - 5, I1, 10)
                QQ(I, H, I1, 11) = W%(I, I1, 1) - QQ(I, H - 1, I1, 11) - QQ(I, H - 2, I1, 11) - QQ(I, H - 3, I1, 11) - QQ(I, H - 4, I1, 11) - QQ(I, H - 5, I1, 11)
                QQ(I, H, I1, 12) = A!(I, I1, 24) - QQ(I, H - 1, I1, 12) - QQ(I, H - 2, I1, 12) - QQ(I, H - 3, I1, 12) - QQ(I, H - 4, I1, 12) - QQ(I, H - 5, I1, 12)
                QQ(I, H, I1, 13) = A!(I, I1, 8) * 2 + A!(I, I1, 10) + W%(I, I1, 0) - QQ(I, H - 1, I1, 13) - QQ(I, H - 2, I1, 13) - QQ(I, H - 3, I1, 13) - QQ(I, H - 4, I1, 13) - QQ(I, H - 5, I1, 13)
                QQ(I, H, I1, 14) = A!(I, I1, 23) - QQ(I, H - 1, I1, 14) - QQ(I, H - 2, I1, 14) - QQ(I, H - 3, I1, 14) - QQ(I, H - 4, I1, 14) - QQ(I, H - 5, I1, 14)

            Next

        End If

        If H = 7 Then

            For I2 = 0 To 14
                QR(I, H, I2) = V9(I2) - QR(I, H - 1, I2) - QR(I, H - 2, I2) - QR(I, H - 3, I2) - QR(I, H - 4, I2) - QR(I, H - 5, I2) - QR(I, H - 6, I2)
            Next

            For I1 = 0 To 13

                For I2 = 7 To 15
                    QQ(I, H, I1, I2 - 6) = A!(I, I1, I2) - QQ(I, H - 1, I1, I2 - 6) - QQ(I, H - 2, I1, I2 - 6) - QQ(I, H - 3, I1, I2 - 6) - QQ(I, H - 4, I1, I2 - 6) - QQ(I, H - 5, I1, I2 - 6) - QQ(I, H - 6, I1, I2 - 6)
                Next

                QQ(I, H, I1, 10) = W%(I, I1, 0) - QQ(I, H - 1, I1, 10) - QQ(I, H - 2, I1, 10) - QQ(I, H - 3, I1, 10) - QQ(I, H - 4, I1, 10) - QQ(I, H - 5, I1, 10) - QQ(I, H - 6, I1, 10)
                QQ(I, H, I1, 11) = W%(I, I1, 1) - QQ(I, H - 1, I1, 11) - QQ(I, H - 2, I1, 11) - QQ(I, H - 3, I1, 11) - QQ(I, H - 4, I1, 11) - QQ(I, H - 5, I1, 11) - QQ(I, H - 6, I1, 11)
                QQ(I, H, I1, 12) = A!(I, I1, 24) - QQ(I, H - 1, I1, 12) - QQ(I, H - 2, I1, 12) - QQ(I, H - 3, I1, 12) - QQ(I, H - 4, I1, 12) - QQ(I, H - 5, I1, 12) - QQ(I, H - 6, I1, 12)
                QQ(I, H, I1, 13) = A!(I, I1, 8) * 2 + A!(I, I1, 10) + W%(I, I1, 0) - QQ(I, H - 1, I1, 13) - QQ(I, H - 2, I1, 13) - QQ(I, H - 3, I1, 13) - QQ(I, H - 4, I1, 13) - QQ(I, H - 5, I1, 13) - QQ(I, H - 6, I1, 13)
                QQ(I, H, I1, 14) = A!(I, I1, 23) - QQ(I, H - 1, I1, 14) - QQ(I, H - 2, I1, 14) - QQ(I, H - 3, I1, 14) - QQ(I, H - 4, I1, 14) - QQ(I, H - 5, I1, 14) - QQ(I, H - 6, I1, 14)

            Next

        End If

    Next

    For I = 0 To 1
        V9(I) = 0
    Next

End Sub


'----------------------------------------
'        COMPUTERCOACH Subroutine
'----------------------------------------
'This subroutine guides the computer AI
'in how to handle it's strategy.
Sub COMPUTERCOACH ()

    If P9 = 0 Then I6 = 1
    If P9 = 1 Then I6 = 0

    If S!(P9, 0) - S!(I6, 0) < 0 Then

        I5 = S!(I6, 0) - S!(P9, 0)

        R0! = Int(Rnd(1) * 100) + 1
        If H = 1 And R0! > 37 Then D2(P9) = 0: P7(P9) = 0: DT%(P9) = 0
        If H = 1 And R0! > 10 And R0! <= 36 Then D2(P9) = 2: P7(P9) = 0: DT%(P9) = 2
        If H = 1 And R0! > 5 And R0! <= 10 Then D2(P9) = 3: P7(P9) = 0: DT%(P9) = 3
        If H = 1 And R0! <= 5 Then D2(P9) = 1: P7(P9) = 0: DT%(P9) = 1

        R0! = Int(Rnd(1) * 100) + 1
        If H = 1 And R0! <= 15 Then D2(P9) = 0: P7(P9) = 1: DT%(P9) = 4
        If H = 1 And R0! > 9 And R0! <= 12 Then D2(P9) = 0: P7(P9) = 3: DT%(P9) = 8
        If H = 1 And R0! > 6 And R0! <= 9 Then D2(P9) = 2: P7(P9) = 3: DT%(P9) = 7
        If H = 1 And R0! > 3 And R0! <= 6 Then D2(P9) = 1: P7(P9) = 3: DT%(P9) = 9
        If H = 1 And R0! <= 3 Then D2(P9) = 0: P7(P9) = 4: DT%(P9) = 10

        R0! = Int(Rnd(1) * 100) + 1
        If H = 2 And gameClock! >= 600 And I5 > 20 And R0! <= 60 Then D2(P9) = 0: P7(P9) = 0: DT%(P9) = 0
        If H = 2 And gameClock! >= 600 And I5 > 20 And R0! > 60 And R0! <= 80 Then D2(P9) = 1: P7(P9) = 0: DT%(P9) = 1
        If H = 2 And gameClock! >= 600 And I5 > 20 And R0! > 80 And R0! <= 90 Then D2(P9) = 1: P7(P9) = 2: DT%(P9) = 6
        If H = 2 And gameClock! >= 600 And I5 > 20 And R0! > 90 And R0! <= 95 Then D2(P9) = 0: P7(P9) = 4: DT%(P9) = 10
        If H = 2 And gameClock! >= 600 And I5 > 20 And R0! > 95 Then D2(P9) = 1: P7(P9) = 4: DT%(P9) = 11
        If H = 2 And gameClock! >= 600 And I5 >= 10 And I5 < 20 And R0! <= 75 Then D2(P9) = 0: P7(P9) = 0: DT%(P9) = 0
        If H = 2 And gameClock! >= 600 And I5 >= 10 And I5 < 20 And R0! > 75 And R0! <= 83 Then D2(P9) = 0: P7(P9) = 1: DT%(P9) = 4
        If H = 2 And gameClock! >= 600 And I5 >= 10 And I5 < 20 And R0! > 83 And R0! <= 91 Then D2(P9) = 1: P7(P9) = 0: DT%(P9) = 1
        If H = 2 And gameClock! >= 600 And I5 >= 10 And I5 < 20 And R0! > 91 And R0! <= 96 Then D2(P9) = 0: P7(P9) = 4: DT%(P9) = 10
        If H = 2 And gameClock! >= 600 And I5 >= 10 And I5 < 20 And R0! > 96 Then D2(P9) = 1: P7(P9) = 4: DT%(P9) = 11
        If H = 2 And gameClock! >= 600 And I5 < 10 And R0! <= 70 Then D2(P9) = 0: P7(P9) = 0: DT%(P9) = 0
        If H = 2 And gameClock! >= 600 And I5 < 10 And R0! > 70 And R0! <= 75 Then D2(P9) = 1: P7(P9) = 0: DT%(P9) = 1
        If H = 2 And gameClock! >= 600 And I5 < 10 And R0! > 76 And R0! <= 85 Then D2(P9) = 2: P7(P9) = 0: DT%(P9) = 2
        If H = 2 And gameClock! >= 600 And I5 < 10 And R0! > 85 And R0! <= 93 Then D2(P9) = 0: P7(P9) = 2: DT%(P9) = 5
        If H = 2 And gameClock! >= 600 And I5 < 10 And R0! > 93 And R0! <= 97 Then D2(P9) = 3: P7(P9) = 0: DT%(P9) = 3
        If H = 2 And gameClock! >= 600 And I5 < 10 And R0! > 97 Then D2(P9) = 2: P7(P9) = 4: DT%(P9) = 12
        If H = 2 And gameClock! >= 120 And gameClock! < 600 And I5 > 20 And R0! <= 70 Then D2(P9) = 0: P7(P9) = 0: DT%(P9) = 0
        If H = 2 And gameClock! >= 120 And gameClock! < 600 And I5 > 20 And R0! > 70 And R0! <= 80 Then D2(P9) = 0: P7(P9) = 2: DT%(P9) = 5
        If H = 2 And gameClock! >= 120 And gameClock! < 600 And I5 > 20 And R0! > 80 And R0! <= 90 Then D2(P9) = 0: P7(P9) = 4: DT%(P9) = 10
        If H = 2 And gameClock! >= 120 And gameClock! < 600 And I5 > 20 And R0! > 90 Then D2(P9) = 3: P7(P9) = 4: DT%(P9) = 13
        If H = 2 And gameClock! >= 120 And gameClock! < 600 And I5 > 10 And I5 < 20 And R0! <= 70 Then D2(P9) = 0: P7(P9) = 0: DT%(P9) = 0
        If H = 2 And gameClock! >= 120 And gameClock! < 600 And I5 > 10 And I5 < 20 And R0! > 70 And R0! <= 80 Then D2(P9) = 0: P7(P9) = 2: DT%(P9) = 5
        If H = 2 And gameClock! >= 120 And gameClock! < 600 And I5 > 10 And I5 < 20 And R0! > 80 And R0! <= 90 Then D2(P9) = 1: P7(P9) = 4: DT%(P9) = 12
        If H = 2 And gameClock! >= 120 And gameClock! < 600 And I5 > 10 And I5 < 20 And R0! > 90 Then D2(P9) = 1: P7(P9) = 0: DT%(P9) = 1
        If H = 2 And gameClock! >= 120 And gameClock! < 600 And I5 <= 10 And R0! <= 70 Then D2(P9) = 0: P7(P9) = 0: DT%(P9) = 0
        If H = 2 And gameClock! >= 120 And gameClock! < 600 And I5 <= 10 And R0! > 70 And R0! <= 90 Then D2(P9) = 2: P7(P9) = 0: DT%(P9) = 2
        If H = 2 And gameClock! >= 120 And gameClock! < 600 And I5 <= 10 And R0! > 90 And R0! <= 96 Then D2(P9) = 1: P7(P9) = 0: DT%(P9) = 1
        If H = 2 And gameClock! >= 120 And gameClock! < 600 And I5 <= 10 And R0! > 96 Then D2(P9) = 0: P7(P9) = 4: DT%(P9) = 10
        If H = 2 And gameClock! < 120 And I5 > 10 And R0! <= 50 Then D2(P9) = 1: P7(P9) = 4: DT%(P9) = 11
        If H = 2 And gameClock! < 120 And I5 > 10 And R0! > 50 Then D2(P9) = 3: P7(P9) = 4: DT%(P9) = 13
        If H = 2 And gameClock! < 120 And I5 <= 10 And I5 > 4 Then D2(P9) = 3: P7(P9) = 5: DT%(P9) = 14
        If H = 2 And gameClock! < 120 And I5 <= 4 And R0! <= 25 Then D2(P9) = 0: P7(P9) = 2: DT%(P9) = 5
        If H = 2 And gameClock! < 120 And I5 <= 4 And R0! > 25 And R0! <= 50 Then D2(P9) = 1: P7(P9) = 2: DT%(P9) = 6
        If H = 2 And gameClock! < 120 And I5 <= 4 And R0! > 50 And R0! <= 75 Then D2(P9) = 1: P7(P9) = 4: DT%(P9) = 11
        If H = 2 And gameClock! < 120 And I5 <= 4 And R0! > 75 Then D2(P9) = 3: P7(P9) = 4: DT%(P9) = 13
        If H > 2 And R0! <= 50 Then D2(P9) = 0: P7(P9) = 0: DT%(P9) = 0
        If H > 2 And R0! > 50 Then D2(P9) = 2: P7(P9) = 0: DT%(P9) = 2

    Else

        R0! = Int(Rnd(1) * 100) + 1
        DT%(P9) = 0
        D2(P9) = 0
        P7(P9) = 0

        Select Case R0!
            Case 1 To 10
                D2(P9) = 1
                DT%(P9) = 1
                P7(P9) = 0
            Case 11 To 36
                D2(P9) = 2
                DT%(P9) = 2
                P7(P9) = 0
            Case 37 To 100
                DT%(P9) = 0
                D2(P9) = 0
                P7(P9) = 0
        End Select

        R0! = Int(Rnd(1) * 100) + 1

        Select Case R0!
            Case 1 To 5
                P7(P9) = 1
                DT%(P9) = 4
                D2(P9) = 0
            Case 6 To 9
                P7(P9) = 3
                DT%(P9) = 8
                D2(P9) = 0
            Case 10
                P7(P9) = 3
                DT%(P9) = 7
                D2(P9) = 2
                'Nothing for 11 to 100
        End Select

    End If

    R0! = Int(Rnd(1) * 100) + 1

    If H = 1 Then

        Select Case R0!
            Case 1 To 80
                OF%(P9) = 0
            Case 81 To 90
                OF%(P9) = 1
            Case 91 To 100
                OF%(P9) = 2
        End Select

    End If

    If H >= 2 And S!(P9, 0) - S!(I6, 0) < 0 Then

        If R0! <= 80 Then
            OF%(P9) = 0
        Else
            'R0 > 80
            OF%(P9) = 1
        End If

    End If

    If H >= 2 And S!(P9, 0) - S!(I6, 0) >= 0 Then

        If R0! <= 80 Then
            OF%(P9) = 0
        Else
            'R0! > 80
            OF%(P9) = 2
        End If

    End If

    If H >= 2 And gameClock! <= 180 And S!(P9, 0) - S!(I6, 0) < 0 Then

        If R0! <= 30 Then
            OF%(P9) = 3
        Else
            'R0! <= 30
            OF%(P9) = 4
        End If

    End If

    If H >= 2 And gameClock! <= 60 And X3% = 1 And (S!(P9, 0) - S!(1 - P9, 0) = -3 Or S!(P9, 0) - S!(1 - P9, 0) = -6) Then OF%(P9) = 9

End Sub


'----------------------------------------
'         STAMINASORT Subroutine
'----------------------------------------
'This subroutine PRESUMABLY sorts players
'by their current stamina.
Sub STAMINASORT ()
    M = 13
    L = 12

    8802 '
    S6 = 0
    S7 = 0

    8803 '
    H1 = 0
    H2 = 0

    If S3(S6) > S3(S6 + 1) Then
        H1 = S3(S6)
        H2 = F1(S6)
        S3(S6) = S3(S6 + 1)
        F1(S6) = F1(S6 + 1)
        S3(S6 + 1) = H1
        F1(S6 + 1) = H2
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

    Print B$(P); " LAST 5 SEC STRATEGY:"

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

    M5 = 0

    If gameClock! <= 2 Or IN = 99 Then
        M5 = 1
    Else
        If gameClock! > 2 And gameClock! <= 4 Then M5 = 2
        If gameClock! > 4 And gameClock! <= 16 Then M5 = Int(Rnd(1) * 2) + 3
        If gameClock! > 16 Then M5 = Int(Rnd(1) * 3) + 5
    End If

    timeElapsed! = M5

    Call AdjustPlayTime

    gameClock! = gameClock! - M5
    IN = 0

    If sClockVal >= 1 Then shotClock = shotClock - M5

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
'       AdjustPlayTime Subroutine
'----------------------------------------
'This subroutine increases the play time
'for each active player
Sub AdjustPlayTime ()

    ' *** INCREMENT MINUTES ***
    For I3 = 0 To 1

        For I4 = 0 To 4
            timePlayed(I3, B1(I3, I4)) = timePlayed(I3, B1(I3, I4)) + timeElapsed!
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

        If gameClock! < 46 And shotClock >= gameClock! Or H > 2 And gameClock! = 300 Then Locate 5, 5: Print "     "

        If gameClock! < 6 And LS = 4 And LT <> 4 Then F! = 5

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
'           EVENTS Subroutine
'----------------------------------------
'This subroutine works with another to
'look for any events that meet the limits
'for "special". If found, it will create
'a corresponding "special" box score
Sub EVENTS (specialCount, specialStat)

    If _FileExists("EVENTS.BOX") Then

        Open "EVENTS.BOX" For Input As #1
        For X = 1 To 13
            Input #1, E%(X)
        Next
        Close 1

        checkSpecial% = 0

        'Team / game conditions for special box score
        If H - 3 > E%(1) Or S!(1, 0) + S!(0, 0) > E%(2) Or R3!(0) + R3!(1) > E%(3) Or S!(1, 0) > E%(4) Or S!(0, 0) > E%(4) Or R3!(1) > E%(5) Or R3!(0) > E%(5) Then

            checkSpecial% = 1

        Else

            For I = 0 To 1

                For I1 = 0 To 13

                    'Player conditions for special box score
                    'FGM * 2 + FTM + 3FM = Points / Game
                    If A!(I, I1, 8) * 2 + A!(I, I1, 10) + W%(I, I1, 0) > E%(6) Or A!(I, I1, 11) > E%(7) Or A!(I, I1, 14) > E%(8) Or A!(I, I1, 15) > E%(9) Or A!(I, I1, 13) > E%(10) Or W%(I, I1, 0) > E%(11) Then
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
    Spec$ = diskPaths$(1) + "*.SPC"
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
    endGame = 1

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

        If AP = 1 And U5 = 2 Then
            X = 2
        Else
            Do
                I$ = GetKeyPress$
                X = Val(I$)
            Loop Until X >= 0 And X <= 2
        End If

        Call BOXOPTIONS(X)
        Call TICKER

        'Reset option for next call
        If X <> 0 Then X = 0

        'Prompt to display stats again
        Color 15, 0
        Cls

        If Not (AP = 1 And U5 = 2) Then

            Do
                'Using Home Team's color for the box
                Call TEAMBOX(1, 1)
                U$ = "DISPLAY STATS AGAIN (YN)"
                Locate 7, (80 - Len(U$)) / 2
                Print U$
                Color 15, 0
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        End If

    Loop Until UCase$(I$) = "N" Or (AP = 1 And U5 = 2)

    'If we had special events
    If specialStat = 1 Then

        NM$ = "BOX" + Mid$(Str$(specialCount), 2, Len(Str$(specialCount))) + ".SPC"

        'Append special events
        Call BOX2SPC(NM$)

        Call BOX2FILE(2, NM$, specialStat)

    End If

    Call SAVESTATS

    endGame = 0
    specialStat = 0
    'Close #7

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

    Shared scheduleFile$

    Select Case outputChoice

        Case 0:
            Call BOX2SCREEN

        Case 1, 2:
            If outputChoice = 1 Then
                'Printer = temporary file
                boxFile$ = "TEMPBOX"
            Else
                'File = prompt | auto-create
                If AP <> 1 Then
                    Cls
                    Print "ENTER 8-CHARACTER FILENAME FOR BOXSCORE (NO SPACES OR PERIODS)";: Input boxFile$
                    boxFile$ = boxFile$ + ".SPC"
                Else
                    ' NM$ = "BOXSCORE." + MID$(STR$(AP%(0) + AP%(2)), 2, LEN(STR$(AP%(0) + AP%(2))))
                    boxFile$ = "BOX" + Mid$(Str$(AP%(0) + AP%(2)), 2, Len(Str$(AP%(0) + AP%(2))))
                End If

            End If

            Call BOX2FILE(outputChoice, boxFile$, specialStat)

    End Select

End Sub


'----------------------------------------
'         BOX2SCREEN Subroutine
'----------------------------------------
'This subroutine formats the boxscore
'for output to the end user display
Sub BOX2SCREEN ()

    Cls

    Color 14
    If N = 2 Then
        Print "SITE: ";
        Color 15
        Print "NEUTRAL"
    Else
        Print "SITE: ";
        Color 15
        Print gamearenaName$(N)
    End If

    Color 14
    Print "ATTENDANCE: ";
    Color 15
    Print Using "###,###"; actualAttendance&
    Print

    Call ScoreSummary

    If Not (AP = 1 And U5 = 2) Then
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

        If S!(0, 1) + S!(0, 2) = S!(1, 1) + S!(1, 2) Then

            Locate , 23
            Print "O1 ";

            If S!(0, 1) + S!(0, 2) + S!(0, 3) = S!(1, 1) + S!(1, 2) + S!(1, 3) Then
                Locate , 26
                Print "O2 ";

                If S!(0, 1) + S!(0, 2) + S!(0, 3) + S!(0, 4) = S!(1, 1) + S!(1, 2) + S!(1, 3) + S!(1, 4) Then
                    Locate , 29
                    Print "O3 ";

                    If S!(0, 1) + S!(0, 2) + S!(0, 3) + S!(0, 4) + S!(0, 5) = S!(1, 1) + S!(1, 2) + S!(1, 3) + S!(1, 4) + S!(1, 5) Then
                        Locate , 32
                        Print "O4 ";

                        If S!(0, 1) + S!(0, 2) + S!(0, 3) + S!(0, 4) + S!(0, 5) + S!(0, 6) = S!(1, 1) + S!(1, 2) + S!(1, 3) + S!(1, 4) + S!(1, 5) + S!(1, 6) Then
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
        Color TR%(I, 0), TR%(I, 1)
        Print B$(I);

        Color 15, 0

        If halfTime = 1 Then
            Print Tab(16); Str$(S!(I, 1)); Tab(22); " - "; S!(I, 0)
        Else
            Print Tab(16); Str$(S!(I, 1)); Tab(19); Str$(S!(I, 2));

            If S!(0, 1) + S!(0, 2) = S!(1, 1) + S!(1, 2) Then
                Locate , 22: Print Str$(S!(I, 3));

                If S!(0, 1) + S!(0, 2) + S!(0, 3) = S!(1, 1) + S!(1, 2) + S!(1, 3) Then
                    Locate , 25: Print Str$(S!(I, 4));

                    If S!(0, 1) + S!(0, 2) + S!(0, 3) + S!(0, 4) = S!(1, 1) + S!(1, 2) + S!(1, 3) + S!(1, 4) Then
                        Locate , 28: Print Str$(S!(I, 5));

                        If S!(0, 1) + S!(0, 2) + S!(0, 3) + S!(0, 4) + S!(0, 5) = S!(1, 1) + S!(1, 2) + S!(1, 3) + S!(1, 4) + S!(1, 5) Then
                            Locate , 31: Print Str$(S!(I, 6));

                            If S!(0, 1) + S!(0, 2) + S!(0, 3) + S!(0, 4) + S!(0, 5) + S!(0, 6) = S!(1, 1) + S!(1, 2) + S!(1, 3) + S!(1, 4) + S!(1, 5) + S!(1, 6) Then
                                Locate , 35: Print Str$(S!(I, 7)); " -  "; Str$(S!(I, 0))
                            Else
                                Locate , 34: Print " - "; Str$(S!(I, 0))
                            End If

                        Else
                            Locate , 31: Print " - "; Str$(S!(I, 0))
                        End If

                    Else
                        Locate , 28: Print " - "; Str$(S!(I, 0))
                    End If

                Else
                    Locate , 25: Print " - "; S!(I, 0)
                End If

            Else
                Locate , 22: Print " - "; S!(I, 0)
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
Sub BOX2FILE (printChoice, NM$, isSpecial)

    If isSpecial = 1 Then
        'special scores come after the events
        Open diskPaths$(1) + NM$ For Append As #2
    Else
        Open diskPaths$(1) + NM$ For Output As #2
    End If

    If N = 2 Then
        Print #2, "SITE: NEUTRAL"
    Else
        Print #2, "SITE: "; gamearenaName$(N)
    End If

    Print #2, Using "ATTENDANCE: ###,###"; actualAttendance&
    Print #2,

    Print #2, ; Tab(17); "H1 H2 ";

    If S!(0, 1) + S!(0, 2) = S!(1, 1) + S!(1, 2) Then
        Print #2, ; Tab(23); "O1 ";

        If S!(0, 1) + S!(0, 2) + S!(0, 3) = S!(1, 1) + S!(1, 2) + S!(1, 3) Then
            Print #2, ; Tab(26); "O2 ";

            If S!(0, 1) + S!(0, 2) + S!(0, 3) + S!(0, 4) = S!(1, 1) + S!(1, 2) + S!(1, 3) + S!(1, 4) Then
                Print #2, ; Tab(29); "O3 ";

                If S!(0, 1) + S!(0, 2) + S!(0, 3) + S!(0, 4) + S!(0, 5) = S!(1, 1) + S!(1, 2) + S!(1, 3) + S!(1, 4) + S!(1, 5) Then
                    Print #2, ; Tab(32); "O4 ";

                    If S!(0, 1) + S!(0, 2) + S!(0, 3) + S!(0, 4) + S!(0, 5) + S!(0, 6) = S!(1, 1) + S!(1, 2) + S!(1, 3) + S!(1, 4) + S!(1, 5) + S!(1, 6) Then
                        Print #2, ; Tab(35); "O5";
                        Print #2, ; Tab(40); "GAME"
                    Else
                        Print #2, ; Tab(37); "GAME"
                    End If
                Else
                    Print #2, ; Tab(34); "GAME"
                End If
            Else
                Print #2, ; Tab(31); "GAME"
            End If
        Else
            Print #2, ; Tab(28); "GAME"
        End If
    Else
        Print #2, ; Tab(25); "GAME"
    End If

    For I = 0 To 1

        Print #2, B$(I); Tab(16); Str$(S!(I, 1)); Tab(19); Str$(S!(I, 2));

        If S!(0, 1) + S!(0, 2) = S!(1, 1) + S!(1, 2) Then

            Print #2, ; Tab(22); Str$(S!(I, 3));

            If S!(0, 1) + S!(0, 2) + S!(0, 3) = S!(1, 1) + S!(1, 2) + S!(1, 3) Then
                Print #2, ; Tab(25); Str$(S!(I, 4));

                If S!(0, 1) + S!(0, 2) + S!(0, 3) + S!(0, 4) = S!(1, 1) + S!(1, 2) + S!(1, 3) + S!(1, 4) Then
                    Print #2, ; Tab(28); Str$(S!(I, 5));

                    If S!(0, 1) + S!(0, 2) + S!(0, 3) + S!(0, 4) + S!(0, 5) = S!(1, 1) + S!(1, 2) + S!(1, 3) + S!(1, 4) + S!(1, 5) Then
                        Print #2, ; Tab(31); Str$(S!(I, 6));

                        If S!(0, 1) + S!(0, 2) + S!(0, 3) + S!(0, 4) + S!(0, 5) + S!(0, 6) = S!(1, 1) + S!(1, 2) + S!(1, 3) + S!(1, 4) + S!(1, 5) + S!(1, 6) Then
                            Print #2, ; Tab(35); Str$(S!(I, 7)); " -  "; Str$(S!(I, 0))
                        Else
                            Print #2, ; Tab(34); " - "; Str$(S!(I, 0))
                        End If
                    Else
                        Print #2, ; Tab(31); " - "; Str$(S!(I, 0))
                    End If
                Else
                    Print #2, ; Tab(28); " - "; Str$(S!(I, 0))
                End If
            Else
                Print #2, ; Tab(25); " - "; S!(I, 0)
            End If
        Else
            Print #2, ; Tab(22); " - "; S!(I, 0)
        End If

    Next I


    For I = 0 To 1:

        Print #2,
        Print #2, B$(I)
        Print #2, "NAME"; Tab(14); " MIN   FG- A 3FG- A  FT- A ORB  RB  PT  F   A   B   S  TO"

        For I1 = 0 To 13

            If AA$(I, I1) <> "XXX" Then

                'Player Name
                Print #2, AA$(I, I1); Tab(14);

                'Player Time
                If timePlayed(I, I1) = 0 Then
                    Print #2, ; Tab(15); "DNP";
                Else
                    Print #2, Using "##"; Int(timePlayed(I, I1) / 60);: Print #2, ":";

                    If timePlayed(I, I1) - Int(timePlayed(I, I1) / 60) * 60 > 9 Then
                        Print #2, Using "##"; timePlayed(I, I1) - Int(timePlayed(I, I1) / 60) * 60;
                    Else
                        Print #2, "0";: Print #2, Using "#"; timePlayed(I, I1) - Int(timePlayed(I, I1) / 60) * 60;
                    End If
                End If

                Print #2, ; Tab(21);

                'FGM - FGA
                Print #2, Using "##"; A!(I, I1, 8);
                Print #2, "-";
                Print #2, Using "##"; A!(I, I1, 7);

                '3FGM - 3FGA
                Print #2, Using "  ##"; W%(I, I1, 0);
                Print #2, "-";
                Print #2, Using "##"; W%(I, I1, 1);

                'FTM - FTA
                Print #2, Using "  ##"; A!(I, I1, 10);
                Print #2, "-";
                Print #2, Using "##"; A!(I, I1, 9);

                'Rebounds / PT / F / A /  B / S / TO
                Print #2, Using "  ##  ## ###  #  ##  ##  ##  ##  ##  ##"; A!(I, I1, 24), A!(I, I1, 11), A!(I, I1, 8) * 2 + A!(I, I1, 10) + W%(I, I1, 0), A!(I, I1, 12), A!(I, I1, 14), A!(I, I1, 15), A!(I, I1, 13), A!(I, I1, 23)

                V9(0) = V9(0) + A!(I, I1, 8)
                V9(1) = V9(1) + A!(I, I1, 7)
                V9(2) = V9(2) + A!(I, I1, 10)
                V9(3) = V9(3) + A!(I, I1, 9)
                V9(4) = V9(4) + A!(I, I1, 11)
                V9(5) = V9(5) + (A!(I, I1, 8) * 2) + A!(I, I1, 10) + W%(I, I1, 0)
                V9(13) = V9(13) + W%(I, I1, 0)
                V9(14) = V9(14) + W%(I, I1, 1)
                V9(6) = V9(6) + A!(I, I1, 12)
                V9(7) = V9(7) + A!(I, I1, 14)
                V9(8) = V9(8) + A!(I, I1, 15)
                V9(9) = V9(9) + A!(I, I1, 13)
                V9(11) = V9(11) + A!(I, I1, 24)
                V9(12) = V9(12) + A!(I, I1, 23)
                V9(10) = V9(10) + timePlayed(I, I1)

            End If

        Next I1

        If V9(3) = 0 Then V9(3) = 1

        Print #2,
        Print #2, "TOTALS"; Tab(14); Int(V9(10) / 60); Tab(21);

        Print #2, Using "##"; V9(0);
        Print #2, "-";
        Print #2, Using "###"; V9(1);

        Print #2, ; Tab(28);
        Print #2, Using "##"; V9(13);
        Print #2, "-";
        Print #2, Using "##"; V9(14);

        Print #2, ; Tab(35);
        Print #2, Using "##"; V9(2);
        Print #2, "-";
        Print #2, Using "##"; V9(3);

        Print #2, ; Tab(42);
        Print #2, Using "##  ## ### ##  ##  ##  ##  ##"; V9(11), V9(4), V9(5), V9(6), V9(7), V9(8), V9(9), V9(12)

        Print #2, ; Tab(22); Int(((V9(0) / V9(1)) * 100) + .5); "%"; Tab(35); Int(((V9(2) / V9(3)) * 100) + .5); "%"

        Print #2,
        Print #2, "TOTAL TURNOVERS -"; T0(I)
        Print #2, "TOTAL REBOUNDS  -"; R3!(I)

        Erase V9

    Next I

    Print #2,

    '-- The code below was used to print
    '-- disabled players in the boxscore
    '-- output so that they could be easily identified.
    'Print #2, "BENCH WARMERS"
    '--Print #2, "DISABLED BUT PLAYED"

    'For I = 0 To 1

    '    For I1 = 0 To 13

    '        'If AA$(I, I1) <> "XXX" And B%(I, I1) = -1 Then
    '--If AA$(I, I1) <> "XXX" And B%(I, I1) = -1 And timePlayed(I, I1) > 0 Then
    '            Print #2, AA$(I, I1); " ["; B$(I); "]"
    '        End If

    '    Next I1

    'Next I

    If printChoice = 1 Then Print #2, Chr$(12)

    Close #2

    If printChoice = 1 Then

        Open diskPaths$(1) + NM$ For Input As #2

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
Sub BOX2SPC (NM$)

    Open diskPaths$(1) + NM$ For Append As #2

    Print #2,

    'Output any special events
    If H - 3 > E%(1) Then Print #2, Using "MORE THAN ## OVERTIMES"; E%(1)
    If S!(1, 0) + S!(0, 0) > E%(2) Then Print #2, Using "MORE THAN ### COMBINED POINTS"; E%(2)
    If R3!(0) + R3!(1) > E%(3) Then Print #2, Using "MORE THAN ### COMBINED REBOUNDS"; E%(3)
    If S!(0, 0) > E%(4) Then Print #2, B$(0);: Print #2, Using " SCORED MORE THAN ### POINTS"; E%(4)
    If S!(1, 0) > E%(4) Then Print #2, B$(1);: Print #2, Using " SCORED MORE THAN ### POINTS"; E%(4)
    If R3!(0) > E%(5) Then Print #2, B$(0);: Print #2, Using " HAD MORE THAN ### REBOUNDS"; E%(5)
    If R3!(1) > E%(5) Then Print #2, B$(1);: Print #2, Using " HAD MORE THAN ### REBOUNDS"; E%(5)

    For I = 0 To 1
        For I1 = 0 To 13
            If A!(I, I1, 8) * 2 + A!(I, I1, 10) + W%(I, I1, 0) > E%(6) Then Print #2, AA$(I, I1);: Print #2, Using " SCORED MORE THAN ### POINTS"; E%(6)
            If A!(I, I1, 11) > E%(7) Then Print #2, AA$(I, I1);: Print #2, Using " HAD MORE THAN ### REBOUNDS"; E%(7)
            If A!(I, I1, 14) > E%(8) Then Print #2, AA$(I, I1);: Print #2, Using " HAD MORE THAN ### ASSISTS"; E%(8)
            If A!(I, I1, 15) > E%(9) Then Print #2, AA$(I, I1);: Print #2, Using " BLOCKED MORE THAN ### SHOTS"; E%(9)
            If A!(I, I1, 13) > E%(10) Then Print #2, AA$(I, I1);: Print #2, Using " HAD MORE THAN ### STEALS"; E%(10)
            If W%(I, I1, 0) > E%(11) Then Print #2, AA$(I, I1);: Print #2, Using " MADE MORE THAN ### 3-PT FIELD GOALS"; E%(11)
        Next
    Next

    Print #2,

    Close #2

End Sub


'----------------------------------------
'        STATS2SCREEN Subroutine
'----------------------------------------
'This subroutine formats the game stats
'for output to the end user display
Sub STATS2SCREEN

    For I = 0 To 1:

        Cls

        Color TR%(I, 0), TR%(I, 1)
        Print B$(I)

        Color 14, 0
        Print "NAME"; Tab(14); " MIN   FG- A 3FG- A  FT- A ORB  RB  PT  F   A   B   S  TO"

        For I1 = 0 To 13

            If AA$(I, I1) <> "XXX" And timePlayed(I, I1) <> 0 Then

                Color 15

                'Name
                Print AA$(I, I1); Tab(14);

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
                Print Using "##"; A!(I, I1, 8);
                Print "-";
                Print Using "##"; A!(I, I1, 7);

                '3FG
                Print Using "  ##"; W%(I, I1, 0);
                Print "-";
                Print Using "##"; W%(I, I1, 1);

                'FT
                Print Using "  ##"; A!(I, I1, 10);
                Print "-";
                Print Using "##"; A!(I, I1, 9);

                'Rebounds / PT / F / A /  B / S / TO
                Print Using "  ##  ## ###  #  ##  ##  ##  ##  ##  ##"; A!(I, I1, 24), A!(I, I1, 11), A!(I, I1, 8) * 2 + A!(I, I1, 10) + W%(I, I1, 0), A!(I, I1, 12), A!(I, I1, 14), A!(I, I1, 15), A!(I, I1, 13), A!(I, I1, 23);

                If timePlayed(I, I1) = 0 Then Locate , 15: Print "DNP" Else Print

                V9(0) = V9(0) + A!(I, I1, 8)
                V9(1) = V9(1) + A!(I, I1, 7)
                V9(2) = V9(2) + A!(I, I1, 10)
                V9(3) = V9(3) + A!(I, I1, 9)
                V9(4) = V9(4) + A!(I, I1, 11)
                V9(5) = V9(5) + (A!(I, I1, 8) * 2) + A!(I, I1, 10) + W%(I, I1, 0)
                V9(13) = V9(13) + W%(I, I1, 0)
                V9(14) = V9(14) + W%(I, I1, 1)
                V9(6) = V9(6) + A!(I, I1, 12)
                V9(7) = V9(7) + A!(I, I1, 14)
                V9(8) = V9(8) + A!(I, I1, 15)
                V9(9) = V9(9) + A!(I, I1, 13)
                V9(11) = V9(11) + A!(I, I1, 24)
                V9(12) = V9(12) + A!(I, I1, 23)
                V9(10) = V9(10) + timePlayed(I, I1)

            End If

        Next I1

        If V9(3) = 0 Then V9(3) = 1

        Print
        Color 14
        Print "TOTALS";

        Color 15
        Print Tab(14); Int(V9(10) / 60);

        Locate , 21: Print Using "##"; V9(0);
        Print "-";
        Print Using "###"; V9(1);

        Locate , 28: Print Using "##"; V9(13);
        Print "-";
        Print Using "##"; V9(14);

        Locate , 35: Print Using "##"; V9(2);
        Print "-";
        Print Using "##"; V9(3);

        Locate , 42: Print Using "##  ## ### ##  ##  ##  ##  ##"; V9(11), V9(4), V9(5), V9(6), V9(7), V9(8), V9(9), V9(12)

        Print Tab(22);
        If V9(1) <> 0 Then Print Int(((V9(0) / V9(1)) * 100) + .5); "%";

        Print Tab(35);
        If V9(3) <> 0 Then Print Int(((V9(2) / V9(3)) * 100) + .5); "%"

        Print
        Color 14: Print "TOTAL TURNOVERS -";
        Color 15: Print T0(I)

        Color 14: Print "TOTAL REBOUNDS  -";
        Color 15: Print R3!(I)

        For I2 = 0 To 14: V9(I2) = 0: Next I2

        If Not (AP = 1 And U5 = 2) Then
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

    Shared scheduleFile$

    Color 15, 0
    Cls

    If AP <> 1 Then

        Call TEAMBOX(1, 1)

        Do
            U$ = "SAVE STATS (YN)"
            Locate 7, (80 - Len(U$)) / 2
            Print U$
            Color 15, 0
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    End If

    ' Update schedule, ticker, etc.
    If UCase$(I$) = "Y" Or AP = 1 Then

        If _FileExists("GAME82") Then

            Call TickerSchedule

            If AP%(1) < AP%(0) + AP%(2) Then
                Kill "GAME82"
                endAllGames = 1
            Else

                Open "GAME82" For Output As #2

                Print #2, RTrim$(scheduleFile$)

                For I = 0 To 2
                    Print #2, AP%(I)
                Next

                Close 2

            End If

        Else

            If _FileExists("TOURNEY") Then

                Open "TOURNEY" For Input As #2

                Input #2, tourneyFile$

                For I = 0 To 2
                    Input #2, AP%(I)
                Next

                Close 2

                Open diskPaths$(1) + tourneyFile$ + ".TOU" As #1 Len = 90
                For X = 0 To 14
                    Field #1, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
                Next
                Field #1, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
                Field #1, 64 As X$, 3 As Q$(20), 3 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
                Field #1, 84 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)

                NG%(13) = 1

                For X = 0 To 14: LSet Q$(X + 1) = MKI$(NG%(X)): Next
                For X = 0 To 3: LSet Q$(20 + X) = YN$(X): Next
                For X = 1 To 3: LSet Q$(X + 23) = MKI$(NG%(X + 14)): Next

                LSet Q$(16) = MKI$(S!(0, 0)): LSet Q$(17) = MKI$(S!(1, 0))
                LSet Q$(18) = VT$: LSet Q$(19) = W$

                Put #1, AP%(0) + AP%(2)

                Close 1

                AP%(2) = AP%(2) + 1
                AP = 1

                Open "TOURNEY" For Output As #2

                Print #2, tourneyFile$

                For I = 0 To 2
                    Print #2, AP%(I)
                Next

                Close 2

                Call TOURNEY_Game

                If AP%(1) < AP%(0) + AP%(2) Then
                    Kill "TOURNEY"
                    endAllGames = 1
                End If

            End If

        End If

        Cls

        H% = N

        For PX = 0 To 1
            For J = 0 To 13
                A!(PX, J, 16) = timePlayed(PX, J)
                A!(PX, J, 17) = A!(PX, J, 23)
                A!(PX, J, 18) = A!(PX, J, 24)
            Next
        Next

        For PX = 0 To 1

            For I = 0 To 13

                For J = 1 To 12
                    A!(PX, I, J) = A!(PX, I, J + 6)
                Next

                W0!(PX, I) = W%(PX, I, 0)
                W1!(PX, I) = W%(PX, I, 1)

            Next

        Next

        'Start with 1st team
        For idx = 0 To 1

            skipCompile% = 0

            Color 15, 0
            Cls

            Call TEAMBOX(1, idx)

            U$ = "COMPILE STATS FOR " + B$(idx) + " (YN)"
            Locate , (80 - Len(U$)) / 2
            Print U$
            Color 15, 0

            If AP <> 1 Then
                Do
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                If UCase$(I$) = "N" Then skipCompile% = 1

            End If

            If skipCompile% <> 1 Then

                Call COMPILESTATFILES(idx)

            End If

            Z!(12) = 0

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

    statFile$ = nameStatFile$(B$(teamIdx))

    If _FileExists(diskPaths$(1) + statFile$) Then

        Open diskPaths$(1) + statFile$ For Input As #2

        Input #2, B$(teamIdx)

        For I = 0 To 13:

            Input #2, AA$(teamIdx, I)

            For J = 0 To 13:
                Input #2, Z2!(I, J)
            Next J

            Input #2, W2(teamIdx, I)
            Input #2, W3(teamIdx, I)

        Next I

        For L = 0 To 15: Input #2, Z!(L): Next L

        For L = 0 To 15: Input #2, Z1!(L): Next L

        Close #2

    End If

    TG = 0

    For I = 0 To 13

        If A!(teamIdx, I, 2) * 2 + A!(teamIdx, I, 4) + W0!(teamIdx, I) > Z2!(I, 13) Then
            Z2!(I, 13) = A!(teamIdx, I, 2) * 2 + A!(teamIdx, I, 4) + W0!(teamIdx, I)
        End If

        If A!(teamIdx, I, 10) > 0 Then Z2!(I, 0) = Z2!(I, 0) + 1

        For J = 1 To 12

            Z2!(I, J) = Z2!(I, J) + A!(teamIdx, I, J)

            If J <> 5 And J <> 10 And J <> 11 Then
                Z!(J) = Z!(J) + A!(teamIdx, I, J)
                Z1!(J) = Z1!(J) + A!(1 - teamIdx, I, J)
            End If

        Next J

        Z!(0) = Z!(0) + A!(teamIdx, I, 10)
        TG = TG + A!(teamIdx, I, 10)

    Next I

    For I = 0 To 1

        V9(I) = 0

        For I1 = 0 To 13
            V9(I) = V9(I) + A!(I, I1, 5)
        Next

    Next

    Z!(15) = Z!(15) + R3!(teamIdx) - V9(teamIdx)
    Z1!(15) = Z1!(15) + R3!(1 - teamIdx) - V9(1 - teamIdx)
    Z!(5) = Z!(5) + R3!(teamIdx)
    Z1!(5) = Z1!(5) + R3!(1 - teamIdx)
    Z!(11) = Z!(11) + T0(teamIdx)
    Z1!(11) = Z1!(11) + T0(1 - teamIdx)

    For J = 0 To 13:
        Z!(13) = Z!(13) + W0!(teamIdx, J)
        Z!(14) = Z!(14) + W1!(teamIdx, J)
        Z1!(13) = Z1!(13) + W0!(1 - teamIdx, J)
        Z1!(14) = Z1!(14) + W1!(1 - teamIdx, J)
        W2(teamIdx, J) = W2(teamIdx, J) + W0!(teamIdx, J)
        W3(teamIdx, J) = W3(teamIdx, J) + W1!(teamIdx, J)
    Next J

    Z!(10) = Z!(10) + S!(teamIdx, 0)
    Z1!(10) = Z1!(10) + S!(1 - teamIdx, 0)

    Call TEAMBOX(1, teamIdx)
    U$ = "SAVING STATS FOR " + B$(teamIdx)
    Locate , (80 - Len(U$)) / 2
    Print U$
    Color 15, 0

    Open diskPaths$(1) + statFile$ For Output As #1

    Print #1, B$(teamIdx)

    For I = 0 To 13:

        Print #1, AA$(teamIdx, I)

        For J = 0 To 13:
            Print #1, Z2!(I, J)
        Next J

        Print #1, W2(teamIdx, I):
        Print #1, W3(teamIdx, I)

    Next I

    For L = 0 To 15:
        Print #1, Z!(L)
    Next L

    For L = 0 To 15:
        Print #1, Z1!(L)
    Next L

    Close #1

    Print

    YY% = 0

    If _FileExists(diskPaths$(1) + statFile$ + "S") Then

        Open diskPaths$(1) + statFile$ + "S" For Input As #1

        Input #1, YY%

        For I = 1 To 100: Input #1, O%(I): Next I
        For I = 1 To 100: Input #1, HO$(I): Next I
        For I = 1 To 100: Input #1, HT%(I): Next I
        For I = 1 To 100: Input #1, H$(I): Next I

        For I = 1 To 100
            For I1 = 0 To 1
                Input #1, APT%(I, I1)
            Next
        Next

        For I = 1 To 100: Input #1, CRD&(I): Next I
        For I = 1 To 100: Input #1, SITE$(I): Next I

        Close #1

    End If

    If YY% < 100 Then YY% = YY% + 1
    If H% = 2 And TG < 12100 Then H$(YY%) = "N": GoTo 6540
    If H% <> teamIdx And TG < 12100 Then H$(YY%) = "V": GoTo 6540
    If H% = teamIdx And TG < 12100 Then H$(YY%) = "H": GoTo 6540
    If H% = teamIdx And TG < 13600 Then H$(YY%) = "H1": GoTo 6540
    If H% = teamIdx And TG < 15100 Then H$(YY%) = "H2": GoTo 6540
    If H% = teamIdx And TG < 16600 Then H$(YY%) = "H3": GoTo 6540
    If H% = teamIdx And TG < 18100 Then H$(YY%) = "H4": GoTo 6540
    If H% = 2 And TG < 13600 Then H$(YY%) = "N1": GoTo 6540
    If H% = 2 And TG < 15100 Then H$(YY%) = "N2": GoTo 6540
    If H% = 2 And TG < 16600 Then H$(YY%) = "N3": GoTo 6540
    If H% = 2 And TG < 18100 Then H$(YY%) = "N4": GoTo 6540
    If H% <> teamIdx And TG < 13600 Then H$(YY%) = "V1": GoTo 6540
    If H% <> teamIdx And TG < 15100 Then H$(YY%) = "V2": GoTo 6540
    If H% <> teamIdx And TG < 16600 Then H$(YY%) = "V3": GoTo 6540
    If H% <> teamIdx And TG < 18100 Then H$(YY%) = "V4": GoTo 6540

    6540 '
    O%(YY%) = S!(teamIdx, 0)
    HO$(YY%) = B$(1 - teamIdx)
    HT%(YY%) = S!(1 - teamIdx, 0)
    CRD&(YY%) = actualAttendance&

    If TR%(0, 9) = 0 Then TR%(0, 9) = 78
    If TR%(1, 9) = 0 Then TR%(1, 9) = 78

    APT%(YY%, 0) = TR%(teamIdx, 9)
    APT%(YY%, 1) = TR%(1 - teamIdx, 9)

    If gamearenaName$(1) = "" Then gamearenaName$(1) = "MEMORIAL COLISEUM"

    If N = 2 Then
        SITE$(YY%) = "NEUTRAL SITE"
    Else
        SITE$(YY%) = gamearenaName$(1)
    End If

    Open diskPaths$(1) + statFile$ + "S" For Output As #1

    Print #1, YY%

    For I = 1 To 100: Print #1, O%(I): Next I
    For I = 1 To 100: Print #1, HO$(I): Next I
    For I = 1 To 100: Print #1, HT%(I): Next I
    For I = 1 To 100: Print #1, H$(I): Next I

    For I = 1 To 100
        For I1 = 0 To 1
            Print #1, APT%(I, I1)
        Next
    Next

    For I = 1 To 100: Print #1, CRD&(I): Next I
    For I = 1 To 100: Print #1, SITE$(I): Next I

    Close #1

    For I = 1 To 100
        O%(I) = 0
        HT%(I) = 0
        HO$(I) = ""
        H$(I) = ""
        CRD&(I) = 0
        SITE$(I) = ""

        For I1 = 0 To 1
            APT%(I, I1) = 0
        Next

    Next I

    For J = 0 To 13
        For K = 0 To 13
            Z2!(J, K) = 0
        Next K
    Next J

    For L = 0 To 15
        Z!(L) = 0
        Z1!(L) = 0
    Next L

End Sub


'----------------------------------------
'            pbplog Function
'----------------------------------------
'This subroutine is designed to help me
'print information about where the game
'is at during the code. The general idea
'was originally to be called everywhere
'there is PBP. This helps me debug where
'issues are happening in a game.
Sub pbplog (lineNumber)

    Print #7, "Called from line "; lineNumber

    For I3 = 0 To 1
        Print #7, B$(I3)
        For I4 = 0 To 4
            Print #7, "["; B1(I3, I4); "] "; AA$(I3, B1(I3, I4)) + " (STATUS = "; B%(I3, B1(I3, I4)); ")"
            Print
        Next I4
    Next I3

    Print #7,

End Sub


'----------------------------------------
'       ChooseGameTeam Subroutine
'----------------------------------------
'This subroutine provides interface for
'choosing a team for the game
Sub ChooseGameTeam (yearNumber$, numberTeams, chosenIdx, teamName$, P9)

    Shared previousYear$

    Do

        Color 15, 0
        Cls

        JJ = 7

        Locate 6, 8
        Color 15, 4

        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

        For II = 1 To JJ
            Locate 6 + II, 8
            Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
        Next

        Locate 7 + JJ, 8
        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

        Locate 7, 32

        Locate , 12: Print "ENTER DISK ID: "
        Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Print
        Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"

        I$ = GetKeyPress$

        If I$ <> Chr$(27) Then

            If I$ <> Chr$(13) Then

                Locate 7, 27: Print I$
                Locate 7, 28: Line Input NY$
                NY$ = I$ + NY$
                Color 15, 0

                If Not (NY$ = "" And yearNumber$ <> "") Then

                    yearNumber$ = RTrim$(NY$)
                    Erase teamNames$
                    numberTeams = 0

                    Cls

                    If Not _FileExists(diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$) Then

                        Locate 10, 8

                        Call CSMsgBox("NO TEAMS WERE FOUND MATCHING THAT ID... PRESS ANY KEY TO SELECT ANOTHER ID", 20, PASS)
                        I$ = GetKeyPress$
                        Call CSMsgBox("", 0, PASS)

                    Else

                        Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$ For Random As #1 Len = TEAM_SIZE_BYTES

                        fileLength& = LOF(1)

                        Field #1, 15 As Q$(0)

                        numberTeams = fileLength& / TEAM_SIZE_BYTES

                        For teamIdx = 1 To numberTeams
                            Get #1, teamIdx
                            'numberTeams = numberTeams + 1
                            teamNames$(teamIdx) = Q$(0)
                            teamIndex%(teamIdx) = teamIdx
                        Next teamIdx

                        Close 1

                    End If 'Done checking if file exists

                End If 'Done checking for blank year number

                previousYear$ = yearNumber$

            Else

                yearNumber$ = previousYear$

            End If 'Done checking for ENTER key

            'Locate 2, 33
            'Color 31: Print "LOADING..."

            Do

                Call SelectGameTeam(numberTeams, yearNumber$, chosenIdx, teamName$, ExitCode%, P9)

                If ExitCode% <> 27 Then

                    Do
                        Locate 2, 33
                        Color TR%(P9, 0), TR%(P9, 1): Print teamName$

                        Color 15, 4
                        Locate 3, 33: Print "ANY CHANGE? (YN)";

                        J$ = GetKeyPress$

                    Loop Until J$ = Chr$(27) Or UCase$(J$) = "Y" Or UCase$(J$) = "N"

                End If

            Loop Until ExitCode% = 27 Or J$ = Chr$(27) Or UCase$(J$) = "N"

        End If 'Done checking for ESC

    Loop Until UCase$(J$) = "N" Or I$ = Chr$(27) 'Or J$ = Chr$(27)

    If I$ = Chr$(27) Or J$ = Chr$(27) Then chosenIdx = 0

End Sub


'----------------------------------------
'       SelectGameTeam Subroutine
'----------------------------------------
'This subroutine provides an interface for
'selecting a team from the list loaded by
'the disk ID.
Sub SelectGameTeam (numberTeams, yearNumber$, selectedIdx, teamName$, ExitCode%, P9)

    If numberTeams > 0 Then

        SELECTION% = 1
        Count% = numberTeams
        Start% = 1
        FileFGColr% = 15: FileBGColr% = 4
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Rows% = 20
        Columns% = 4
        Spacing% = 6
        Row% = 4
        Column% = 1

        Color , 4
        Cls
        Color 14
        Locate , 33: Print "TEAM SELECTION"
        Locate 25, 1
        Color 14: Print "ESC";: Color 15: Print "-SELECT NEW ID  PGDN  PGUP  ";
        Color 14: Print "ENTER";: Color 15: Print "-SELECTS TEAM";

        _MouseShow "DEFAULT"
        MAMenu teamNames$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 Then

            selectedIdx = teamIndex%(SELECTION%)

            'Locate 2, 33
            'Color 31: Print "LOADING..."

            Call ReadGameTeam(yearNumber$, teamName$, selectedIdx, P9)

        End If

    End If

End Sub


'----------------------------------------
'        ReadGameTeam Subroutine
'----------------------------------------
'This subroutine reads in the data for the
'selected tema into each of the data arrays
'needed by the game.
Sub ReadGameTeam (YN$, targetTeam$, I1, P9)

    Shared d3FG_Adj(), d3FGA_Adj()

    teamYear$ = RTrim$(YN$)

    Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + teamYear$ For Random As #1 Len = TEAM_SIZE_BYTES

    Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 2 As Q$(14), 2 As Q$(15), 2 As Q$(16), 2 As Q$(17), 2 As Q$(18), 2 As Q$(19), 602 As X$

    For I = 0 To 13
        Field #1, 53 + I * 43 As X$, 15 As Q$(I * 15 + 20), 2 As Q$(I * 15 + 21), 2 As Q$(I * 15 + 22), 2 As Q$(I * 15 + 23), 2 As Q$(I * 15 + 24), 2 As Q$(I * 15 + 25), 2 As Q$(I * 15 + 26), 2 As Q$(I * 15 + 27), 2 As Q$(I * 15 + 28), 2 As Q$(I * 15 + 29), 2 As Q$(I * 15 + 30), 2 As Q$(I * 15 + 31), 2 As Q$(I * 15 + 32), 2 As Q$(I * 15 + 33), 2 As Q$(I * 15 + 34), TEAM_SIZE_BYTES - 53 - 43 - I * 43 As X$
    Next

    Get #1, I1

    targetTeam$ = RTrim$(Q$(0))
    S1(P9) = CVI(Q$(1))
    D1(P9) = CVI(Q$(2))
    d3FG_Adj(P9) = CVI(Q$(3))
    d3FGA_Adj(P9) = CVI(Q$(4))

    For I3 = 0 To 4:
        L%(P9, I3) = CVI(Q$(I3 + 5))
    Next

    For I3 = 0 To 9:
        TR%(P9, I3) = CVI(Q$(I3 + 10))
    Next

    For I3 = 0 To 13
        AA$(P9, I3) = RTrim$(Q$(I3 * 15 + 20))

        For I4 = 0 To 6
            A!(P9, I3, I4) = CVI(Q$(I3 * 15 + I4 + 21))
            C(P9, I3, I4) = CVI(Q$(I3 * 15 + I4 + 28))
        Next
    Next

    Close 1

    FY%(P9) = 2 * TR%(P9, 3) - TR%(P9, 4)

    CNT% = 0
    CNTX% = 0

    Do
        If TR%(P9, 5) <> 0 Then

            For I = 0 To 13
                If B%(P9, I) = -1 Then B%(P9, I) = 0
                RN = Int(Rnd(1) * 100) + 1
                If UCase$(AA$(P9, I)) = "XXX" Then CNTX% = CNTX% + 1
                If UCase$(AA$(P9, I)) = "XXX" Or (B%(P9, I) = 0 And RN > Int(C(P9, I, 0) / TR%(P9, 5) * 100 + .5)) Then B%(P9, I) = -1
            Next

            For I = 0 To 13:
                If B%(P9, I) = 0 Then CNT% = CNT% + 1
            Next

        End If

    Loop Until (CNT% < 10 And CNTX% < 4) Or (CNTX% >= 5 And CNT% < 8)

    Open diskPaths$(0) + DATA_FILE_NAME$ + "." + teamYear$ As #1 Len = DATA_SIZE_BYTES

    Field #1, 15 As Q$(0), 40 As Q$(1), 15 As Q$(2), 4 As Q$(3)

    For I = 0 To 13
        Field #1, 74 + I * 32 As X$, 2 As Q$(I * 16 + 4), 2 As Q$(I * 16 + 5), 2 As Q$(I * 16 + 6), 2 As Q$(I * 16 + 7), 2 As Q$(I * 16 + 8), 2 As Q$(I * 16 + 9), 2 As Q$(I * 16 + 10), 2 As Q$(I * 16 + 11), 2 As Q$(I * 16 + 12), 2 As Q$(I * 16 + 13), 2 As Q$(I * 16 + 14), 2 As Q$(I * 16 + 15), 2 As Q$(I * 16 + 16), 2 As Q$(I * 16 + 17), 2 As Q$(I * 16 + 18), 2 As Q$(I * 16 + 19)
        Field #1, 522 As X$, 2 As Q$(228), 2 As Q$(229), 2 As Q$(230), 2 As Q$(231), 2 As Q$(232), 2 As Q$(233), 2 As Q$(234), 2 As Q$(235), 2 As Q$(236), 2 As Q$(237), 2 As Q$(238), 2 As Q$(239), 2 As Q$(240), 2 As Q$(241), 2 As Q$(242), 2 As Q$(243), 2 As Q$(244), 2 As Q$(245), 2 As Q$(246), 2 As Q$(247), 2 As Q$(248), 2 As Q$(249)
    Next

    Get #1, I1

    gameCoach$(P9) = Q$(0)
    gamearenaName$(P9) = Q$(1)
    gameMascot$(P9) = Q$(2)
    avgAttendance&(P9) = CVL(Q$(3))

    For I = 0 To 13

        DF(P9, I) = CVI(Q$(I * 16 + 4))
        PO$(P9, I) = Q$(I * 16 + 5)

        For I1 = 0 To 13
            RS(P9, I, I1) = CVI(Q$(I * 16 + I1 + 6))
        Next

    Next

    For I = 0 To 20
        T2%(P9, I) = CVI(Q$(228 + I))
    Next

    CK = CVI(Q$(249))

    Close 1

    If CK <> 999 Then

        gameCoach$(P9) = ""
        gamearenaName$(P9) = ""
        gameMascot$(P9) = ""
        avgAttendance&(P9) = 0

        For I = 0 To 13:
            DF(P9, I) = 5
            PO$(P9, I) = ""

            For I1 = 0 To 13
                RS(P9, I, I1) = 0
            Next
        Next

        For I = 0 To 20:
            T2%(P9, I) = 0
        Next

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

    Dim RC$(50, 4), TRC$(125, 3)
    Dim REC!(50, 2), TREC!(125, 2)
    Dim V8(1, 14)
    Dim VERSUS$(1)

    VERSUS$(0) = "AT"
    VERSUS$(1) = "VS."

    If Not _FileExists(diskPaths$(1) + "RECORDS.GAM") Then
        TREC!(2, 0) = 999
        TREC!(4, 0) = 999
        TREC!(7, 0) = 999
        TREC!(9, 0) = 999
        TREC!(11, 0) = 999
        TREC!(13, 0) = 999
        TREC!(15, 0) = 999
        TREC!(17, 0) = 999
        TREC!(19, 0) = 999
        TREC!(21, 0) = 999
        TREC!(23, 0) = 999
        TREC!(25, 0) = 999
        TREC!(27, 0) = 999
        TREC!(29, 0) = 999
        TREC!(31, 0) = 999
        TREC!(33, 0) = 999
        TREC!(35, 0) = 999
        TREC!(37, 0) = 999
        TREC!(43, 0) = 999
        TREC!(45, 0) = 999
        TREC!(61, 0) = 999
        TREC!(63, 0) = 999
        TREC!(65, 0) = 999
        TREC!(67, 0) = 999
        TREC!(73, 0) = 999
        TREC!(75, 0) = 999
        TREC!(81, 0) = 999
        TREC!(83, 0) = 999
        TREC!(87, 0) = 999
        TREC!(89, 0) = 999
        TREC!(92, 0) = 999
        TREC!(94, 0) = 999
        TREC!(97, 0) = 999
        TREC!(99, 0) = 999
        TREC!(105, 0) = 999
        TREC!(107, 0) = 999
        TREC!(113, 0) = 999
        TREC!(115, 0) = 999
        TREC!(120, 0) = 999
        TREC!(123, 0) = 999
        TREC!(125, 0) = 999
    Else

        Open diskPaths$(1) + "RECORDS.GAM" For Input As #1

        For I = 0 To 48
            For I1 = 0 To 2
                Input #1, REC!(I, I1)
            Next
            For I1 = 0 To 4
                Input #1, RC$(I, I1)
            Next
        Next

        For I = 0 To 125
            For I1 = 0 To 2
                Input #1, TREC!(I, I1)
            Next
            For I1 = 0 To 3
                Input #1, TRC$(I, I1)
            Next
        Next

        Close 1

    End If

    'CHECK THE INDIVIDUAL RECORDS

    For I = 0 To 1

        For I1 = 0 To 13

            ' SCORING RECORDS FOR POINTS IN A GAME, HALF, QUARTER AND OVERTIME PERIOD
            If A!(I, I1, 8) * 2 + A!(I, I1, 10) + W%(I, I1, 0) > REC!(0, 0) Then
                REC!(0, 0) = A!(I, I1, 8) * 2 + A!(I, I1, 10) + W%(I, I1, 0) ' POINTS
                REC!(0, 1) = 1 ' TIMES DONE
                REC!(0, 2) = 0 ' GAME
                RC$(0, 0) = AA$(I, I1) ' NAME
                RC$(0, 1) = B$(I) ' TEAM
                RC$(0, 2) = VERSUS$(I) ' AT/VS
                RC$(0, 3) = B$(1 - I) ' OPPONENT
                RC$(0, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 13) > REC!(1, 0) Then
                REC!(1, 0) = QQ(I, 1, I1, 13) ' POINTS
                REC!(1, 1) = 1 ' TIMES DONE
                REC!(1, 2) = 1 ' HALF
                RC$(1, 0) = AA$(I, I1) ' NAME
                RC$(1, 1) = B$(I) ' TEAM
                RC$(1, 2) = VERSUS$(I) ' AT/VS
                RC$(1, 3) = B$(1 - I) ' OPPONENT
                RC$(1, 4) = Date$ ' DATE
            End If

            If QQ(I, 2, I1, 13) > REC!(1, 0) Then
                REC!(1, 0) = QQ(I, 2, I1, 13) ' POINTS
                REC!(1, 1) = 1 ' TIMES DONE
                REC!(1, 2) = 2 ' HALF
                RC$(1, 0) = AA$(I, I1) ' NAME
                RC$(1, 1) = B$(I) ' TEAM
                RC$(1, 2) = VERSUS$(I) ' AT/VS
                RC$(1, 3) = B$(1 - I) ' OPPONENT
                RC$(1, 4) = Date$ ' DATE
            End If

            For I2 = 3 To 8
                If QQ(I, I2, I1, 13) > REC!(3, 0) Then
                    REC!(3, 0) = QQ(I, I2, I1, 13) ' POINTS
                    REC!(3, 1) = 1 ' TIMES DONE
                    REC!(3, 2) = I2 ' QTR
                    RC$(3, 0) = AA$(I, I1) ' NAME
                    RC$(3, 1) = B$(I) ' TEAM
                    RC$(3, 2) = VERSUS$(I) ' AT/VS
                    RC$(3, 3) = B$(1 - I) ' OPPONENT
                    RC$(3, 4) = Date$ ' DATE
                End If
            Next I2

            ' FIELD GOAL PERCENTAGE RECORDS FOR HIGH %, MOST W/NO MISSES, MOST W/NONE MADE
            If A!(I, I1, 8) >= 15 And A!(I, I1, 7) <> 0 Then
                If A!(I, I1, 8) / A!(I, I1, 7) > REC!(4, 0) Then
                    REC!(4, 0) = A!(I, I1, 8) / A!(I, I1, 7) ' PCTG
                    REC!(4, 1) = A!(I, I1, 8) ' FGM
                    REC!(4, 2) = A!(I, I1, 7) ' FGA
                    RC$(4, 0) = AA$(I, I1) ' NAME
                    RC$(4, 1) = B$(I) ' TEAM
                    RC$(4, 2) = VERSUS$(I) ' AT/VS
                    RC$(4, 3) = B$(1 - I) ' OPPONENT
                    RC$(4, 4) = Date$ ' DATE
                End If
            End If

            If A!(I, I1, 8) = A!(I, I1, 7) And A!(I, I1, 7) > REC!(5, 0) Then
                REC!(5, 0) = A!(I, I1, 7) ' MOST FGM
                RC$(5, 0) = AA$(I, I1) ' NAME
                RC$(5, 1) = B$(I) ' TEAM
                RC$(5, 2) = VERSUS$(I) ' AT/VS
                RC$(5, 3) = B$(1 - I) ' OPPONENT
                RC$(5, 4) = Date$ ' DATE
            End If

            If A!(I, I1, 8) = 0 And A!(I, I1, 7) > REC!(6, 0) Then
                REC!(6, 0) = A!(I, I1, 7) ' 0 FGM /FGA
                RC$(6, 0) = AA$(I, I1) ' NAME
                RC$(6, 1) = B$(I) ' TEAM
                RC$(6, 2) = VERSUS$(I) ' AT/VS
                RC$(6, 3) = B$(1 - I) ' OPPONENT
                RC$(6, 4) = Date$ ' DATE
            End If

            ' FIELD GOALS MADE IN A GAME, HALF, QUARTER AND OVERTIME PERIOD
            If A!(I, I1, 8) > REC!(7, 0) Then
                REC!(7, 0) = A!(I, I1, 8) ' FGM
                REC!(7, 1) = 1 ' TIMES DONE
                REC!(7, 2) = 0 ' GAME
                RC$(7, 0) = AA$(I, I1) ' NAME
                RC$(7, 1) = B$(I) ' TEAM
                RC$(7, 2) = VERSUS$(I) ' AT/VS
                RC$(7, 3) = B$(1 - I) ' OPPONENT
                RC$(7, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 2) > REC!(8, 0) Then
                REC!(8, 0) = QQ(I, 1, I1, 2) ' FGM
                REC!(8, 1) = 1 ' TIMES DONE
                REC!(8, 2) = 1 ' HALF
                RC$(8, 0) = AA$(I, I1) ' NAME
                RC$(8, 1) = B$(I) ' TEAM
                RC$(8, 2) = VERSUS$(I) ' AT/VS
                RC$(8, 3) = B$(1 - I) ' OPPONENT
                RC$(8, 4) = Date$ ' DATE
            End If

            If QQ(I, 2, I1, 2) > REC!(8, 0) Then
                REC!(8, 0) = QQ(I, 2, I1, 2) ' FGM
                REC!(8, 1) = 1 ' TIMES DONE
                REC!(8, 2) = 2 ' HALF
                RC$(8, 0) = AA$(I, I1) ' NAME
                RC$(8, 1) = B$(I) ' TEAM
                RC$(8, 2) = VERSUS$(I) ' AT/VS
                RC$(8, 3) = B$(1 - I) ' OPPONENT
                RC$(8, 4) = Date$ ' DATE
            End If

            For I2 = 3 To 8
                If QQ(I, I2, I1, 2) > REC!(10, 0) Then
                    REC!(10, 0) = QQ(I, I2, I1, 2) ' FGM
                    REC!(10, 1) = 1 ' TIMES DONE
                    REC!(10, 2) = I2 ' QTR
                    RC$(10, 0) = AA$(I, I1) ' NAME
                    RC$(10, 1) = B$(I) ' TEAM
                    RC$(10, 2) = VERSUS$(I) ' AT/VS
                    RC$(10, 3) = B$(1 - I) ' OPPONENT
                    RC$(10, 4) = Date$ ' DATE
                End If
            Next I2

            ' FIELD GOAL ATTEMPTS MADE IN A GAME, HALF, QUARTER AND OVERTIME PERIOD
            If A!(I, I1, 7) > REC!(11, 0) Then
                REC!(11, 0) = A!(I, I1, 7) ' FGA
                REC!(11, 1) = 1 ' TIMES DONE
                REC!(11, 2) = 0 ' GAME
                RC$(11, 0) = AA$(I, I1) ' NAME
                RC$(11, 1) = B$(I) ' TEAM
                RC$(11, 2) = VERSUS$(I) ' AT/VS
                RC$(11, 3) = B$(1 - I) ' OPPONENT
                RC$(11, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 1) > REC!(12, 0) Then
                REC!(12, 0) = QQ(I, 1, I1, 1) ' FGA
                REC!(12, 1) = 1 ' TIMES DONE
                REC!(12, 2) = 1 ' HALF
                RC$(12, 0) = AA$(I, I1) ' NAME
                RC$(12, 1) = B$(I) ' TEAM
                RC$(12, 2) = VERSUS$(I) ' AT/VS
                RC$(12, 3) = B$(1 - I) ' OPPONENT
                RC$(12, 4) = Date$ ' DATE
            End If

            If QQ(I, 2, I1, 1) > REC!(12, 0) Then
                REC!(12, 0) = QQ(I, 2, I1, 1) ' FGA
                REC!(12, 1) = 1 ' TIMES DONE
                REC!(12, 2) = 2 ' HALF
                RC$(12, 0) = AA$(I, I1) ' NAME
                RC$(12, 1) = B$(I) ' TEAM
                RC$(12, 2) = VERSUS$(I) ' AT/VS
                RC$(12, 3) = B$(1 - I) ' OPPONENT
                RC$(12, 4) = Date$ ' DATE
            End If

            For I2 = 3 To 8
                If QQ(I, I2, I1, 1) > REC!(14, 0) Then
                    REC!(14, 0) = QQ(I, I2, I1, 1) ' FGA
                    REC!(14, 1) = 1 ' TIMES DONE
                    REC!(14, 2) = I2 ' QTR
                    RC$(14, 0) = AA$(I, I1) ' NAME
                    RC$(14, 1) = B$(I) ' TEAM
                    RC$(14, 2) = VERSUS$(I) ' AT/VS
                    RC$(14, 3) = B$(1 - I) ' OPPONENT
                    RC$(14, 4) = Date$ ' DATE
                End If
            Next I2

            ' THREE-POINT FIELD GOALS MOST MADE NO MISSES, MOST ATT NONE MADE
            If W%(I, I1, 0) = W%(I, I1, 1) And W%(I, I1, 0) > REC!(15, 0) Then
                REC!(15, 0) = W%(I, I1, 0) ' MOST 3-FGM
                RC$(15, 0) = AA$(I, I1) ' NAME
                RC$(15, 1) = B$(I) ' TEAM
                RC$(15, 2) = VERSUS$(I) ' AT/VS
                RC$(15, 3) = B$(1 - I) ' OPPONENT
                RC$(15, 4) = Date$ ' DATE
            End If

            If W%(I, I1, 0) = 0 And W%(I, I1, 1) > REC!(16, 0) Then
                REC!(16, 0) = W%(I, I1, 1) ' 0 3FGM /3FGA
                RC$(16, 0) = AA$(I, I1) ' NAME
                RC$(16, 1) = B$(I) ' TEAM
                RC$(16, 2) = VERSUS$(I) ' AT/VS
                RC$(16, 3) = B$(1 - I) ' OPPONENT
                RC$(16, 4) = Date$ ' DATE
            End If

            ' 3-FIELD GOALS MADE IN A GAME, HALF, QUARTER AND OVERTIME PERIOD
            If W%(I, I1, 0) > REC!(17, 0) Then
                REC!(17, 0) = W%(I, I1, 0) ' 3FGM
                REC!(17, 1) = 1 ' TIMES DONE
                REC!(17, 2) = 0 ' GAME
                RC$(17, 0) = AA$(I, I1) ' NAME
                RC$(17, 1) = B$(I) ' TEAM
                RC$(17, 2) = VERSUS$(I) ' AT/VS
                RC$(17, 3) = B$(1 - I) ' OPPONENT
                RC$(17, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 10) > REC!(18, 0) Then
                REC!(18, 0) = QQ(I, 1, I1, 10) ' 3FGM
                REC!(18, 1) = 1 ' TIMES DONE
                REC!(18, 2) = 1 ' HALF
                RC$(18, 0) = AA$(I, I1) ' NAME
                RC$(18, 1) = B$(I) ' TEAM
                RC$(18, 2) = VERSUS$(I) ' AT/VS
                RC$(18, 3) = B$(1 - I) ' OPPONENT
                RC$(18, 4) = Date$ ' DATE
            End If

            If QQ(I, 2, I1, 10) > REC!(18, 0) Then
                REC!(18, 0) = QQ(I, 2, I1, 10) ' 3FGM
                REC!(18, 1) = 1 ' TIMES DONE
                REC!(18, 2) = 2 ' HALF
                RC$(18, 0) = AA$(I, I1) ' NAME
                RC$(18, 1) = B$(I) ' TEAM
                RC$(18, 2) = VERSUS$(I) ' AT/VS
                RC$(18, 3) = B$(1 - I) ' OPPONENT
                RC$(18, 4) = Date$ ' DATE
            End If

            For I2 = 3 To 8
                If QQ(I, I2, I1, 10) > REC!(20, 0) Then
                    REC!(20, 0) = QQ(I, I2, I1, 10) ' 3FGM
                    REC!(20, 1) = 1 ' TIMES DONE
                    REC!(20, 2) = I2 ' QTR
                    RC$(20, 0) = AA$(I, I1) ' NAME
                    RC$(20, 1) = B$(I) ' TEAM
                    RC$(20, 2) = VERSUS$(I) ' AT/VS
                    RC$(20, 3) = B$(1 - I) ' OPPONENT
                    RC$(20, 4) = Date$ ' DATE
                End If
            Next I2

            ' 3-FIELD GOALS ATT IN A GAME, HALF
            If W%(I, I1, 1) > REC!(21, 0) Then
                REC!(21, 0) = W%(I, I1, 1) ' 3FGA
                REC!(21, 1) = 1 ' TIMES DONE
                REC!(21, 2) = 0 ' GAME
                RC$(21, 0) = AA$(I, I1) ' NAME
                RC$(21, 1) = B$(I) ' TEAM
                RC$(21, 2) = VERSUS$(I) ' AT/VS
                RC$(21, 3) = B$(1 - I) ' OPPONENT
                RC$(21, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 11) > REC!(22, 0) Then
                REC!(22, 0) = QQ(I, 1, I1, 11) ' 3FGA
                REC!(22, 1) = 1 ' TIMES DONE
                REC!(22, 2) = 1 ' HALF
                RC$(22, 0) = AA$(I, I1) ' NAME
                RC$(22, 1) = B$(I) ' TEAM
                RC$(22, 2) = VERSUS$(I) ' AT/VS
                RC$(22, 3) = B$(1 - I) ' OPPONENT
                RC$(22, 4) = Date$ ' DATE
            End If

            If QQ(I, 2, I1, 11) > REC!(22, 0) Then
                REC!(22, 0) = QQ(I, 2, I1, 10) ' 3FGA
                REC!(22, 1) = 1 ' TIMES DONE
                REC!(22, 2) = 2 ' HALF
                RC$(22, 0) = AA$(I, I1) ' NAME
                RC$(22, 1) = B$(I) ' TEAM
                RC$(22, 2) = VERSUS$(I) ' AT/VS
                RC$(22, 3) = B$(1 - I) ' OPPONENT
                RC$(22, 4) = Date$ ' DATE
            End If

            ' FREE THROWS -  MOST MADE NO MISSES, MOST ATT NONE MADE
            If A!(I, I1, 9) = A!(I, I1, 10) And A!(I, I1, 9) > REC!(23, 0) Then
                REC!(23, 0) = A!(I, I1, 9) ' MOST FTM
                RC$(23, 0) = AA$(I, I1) ' NAME
                RC$(23, 1) = B$(I) ' TEAM
                RC$(23, 2) = VERSUS$(I) ' AT/VS
                RC$(23, 3) = B$(1 - I) ' OPPONENT
                RC$(23, 4) = Date$ ' DATE
            End If

            If A!(I, I1, 10) = 0 And A!(I, I1, 9) > REC!(24, 0) Then
                REC!(24, 0) = A!(I, I1, 10) ' 0 FTM /FTA
                RC$(24, 0) = AA$(I, I1) ' NAME
                RC$(24, 1) = B$(I) ' TEAM
                RC$(24, 2) = VERSUS$(I) ' AT/VS
                RC$(24, 3) = B$(1 - I) ' OPPONENT
                RC$(24, 4) = Date$ ' DATE
            End If

            ' MADE FREE THROWS IN A GAME, HALF, QUARTER
            If A!(I, I1, 10) > REC!(25, 0) Then
                REC!(25, 0) = A!(I, I1, 10) ' FTM
                REC!(25, 1) = 1 ' TIMES DONE
                REC!(25, 2) = 0 ' GAME
                RC$(25, 0) = AA$(I, I1) ' NAME
                RC$(25, 1) = B$(I) ' TEAM
                RC$(25, 2) = VERSUS$(I) ' AT/VS
                RC$(25, 3) = B$(1 - I) ' OPPONENT
                RC$(25, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 4) > REC!(26, 0) Then
                REC!(26, 0) = QQ(I, 1, I1, 4) ' FTM
                REC!(26, 1) = 1 ' TIMES DONE
                REC!(26, 2) = 1 ' HALF
                RC$(26, 0) = AA$(I, I1) ' NAME
                RC$(26, 1) = B$(I) ' TEAM
                RC$(26, 2) = VERSUS$(I) ' AT/VS
                RC$(26, 3) = B$(1 - I) ' OPPONENT
                RC$(26, 4) = Date$ ' DATE
            End If

            If QQ(I, 2, I1, 4) > REC!(26, 0) Then
                REC!(26, 0) = QQ(I, 2, I1, 4) ' FTM
                REC!(26, 1) = 1 ' TIMES DONE
                REC!(26, 2) = 2 ' HALF
                RC$(26, 0) = AA$(I, I1) ' NAME
                RC$(26, 1) = B$(I) ' TEAM
                RC$(26, 2) = VERSUS$(I) ' AT/VS
                RC$(26, 3) = B$(1 - I) ' OPPONENT
                RC$(26, 4) = Date$ ' DATE
            End If

            ' FREE THROWS ATTEMPTS IN A GAME, HALF, QUARTER
            If A!(I, I1, 9) > REC!(28, 0) Then
                REC!(28, 0) = A!(I, I1, 9) ' FTA
                REC!(28, 1) = 1 ' TIMES DONE
                REC!(28, 2) = 0 ' GAME
                RC$(28, 0) = AA$(I, I1) ' NAME
                RC$(28, 1) = B$(I) ' TEAM
                RC$(28, 2) = VERSUS$(I) ' AT/VS
                RC$(28, 3) = B$(1 - I) ' OPPONENT
                RC$(28, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 3) > REC!(29, 0) Then
                REC!(29, 0) = QQ(I, 1, I1, 3) ' FTA
                REC!(29, 1) = 1 ' TIMES DONE
                REC!(29, 2) = 1 ' HALF
                RC$(29, 0) = AA$(I, I1) ' NAME
                RC$(29, 1) = B$(I) ' TEAM
                RC$(29, 2) = VERSUS$(I) ' AT/VS
                RC$(29, 3) = B$(1 - I) ' OPPONENT
                RC$(29, 4) = Date$ ' DATE
            End If

            If QQ(I, 2, I1, 3) > REC!(29, 0) Then
                REC!(29, 0) = QQ(I, 2, I1, 3) ' FTM
                REC!(29, 1) = 1 ' TIMES DONE
                REC!(29, 2) = 2 ' HALF
                RC$(29, 0) = AA$(I, I1) ' NAME
                RC$(29, 1) = B$(I) ' TEAM
                RC$(29, 2) = VERSUS$(I) ' AT/VS
                RC$(29, 3) = B$(1 - I) ' OPPONENT
                RC$(29, 4) = Date$ ' DATE
            End If

            ' TOTAL REBOUNDS IN A GAME, HALF, QUARTER
            If A!(I, I1, 11) > REC!(31, 0) Then
                REC!(31, 0) = A!(I, I1, 11) ' FTA
                REC!(31, 1) = 1 ' TIMES DONE
                REC!(31, 2) = 0 ' GAME
                RC$(31, 0) = AA$(I, I1) ' NAME
                RC$(31, 1) = B$(I) ' TEAM
                RC$(31, 2) = VERSUS$(I) ' AT/VS
                RC$(31, 3) = B$(1 - I) ' OPPONENT
                RC$(31, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 5) > REC!(32, 0) Then
                REC!(32, 0) = QQ(I, 1, I1, 5) ' RB
                REC!(32, 1) = 1 ' TIMES DONE
                REC!(32, 2) = 1 ' HALF
                RC$(32, 0) = AA$(I, I1) ' NAME
                RC$(32, 1) = B$(I) ' TEAM
                RC$(32, 2) = VERSUS$(I) ' AT/VS
                RC$(32, 3) = B$(1 - I) ' OPPONENT
                RC$(32, 4) = Date$ ' DATE
            End If

            If QQ(I, 2, I1, 5) > REC!(32, 0) Then
                REC!(32, 0) = QQ(I, 2, I1, 5) ' RB
                REC!(32, 1) = 1 ' TIMES DONE
                REC!(32, 2) = 2 ' HALF
                RC$(32, 0) = AA$(I, I1) ' NAME
                RC$(32, 1) = B$(I) ' TEAM
                RC$(32, 2) = VERSUS$(I) ' AT/VS
                RC$(32, 3) = B$(1 - I) ' OPPONENT
                RC$(32, 4) = Date$ ' DATE
            End If

            ' TOTAL OFFENSIVE REBOUNDS IN A GAME, HALF, QUARTER
            If A!(I, I1, 24) > REC!(34, 0) Then
                REC!(34, 0) = A!(I, I1, 24) ' FTA
                REC!(34, 1) = 1 ' TIMES DONE
                REC!(34, 2) = 0 ' GAME
                RC$(34, 0) = AA$(I, I1) ' NAME
                RC$(34, 1) = B$(I) ' TEAM
                RC$(34, 2) = VERSUS$(I) ' AT/VS
                RC$(34, 3) = B$(1 - I) ' OPPONENT
                RC$(34, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 12) > REC!(35, 0) Then
                REC!(35, 0) = QQ(I, 1, I1, 12) ' RB
                REC!(35, 1) = 1 ' TIMES DONE
                REC!(35, 2) = 1 ' HALF
                RC$(35, 0) = AA$(I, I1) ' NAME
                RC$(35, 1) = B$(I) ' TEAM
                RC$(35, 2) = VERSUS$(I) ' AT/VS
                RC$(35, 3) = B$(1 - I) ' OPPONENT
                RC$(35, 4) = Date$ ' DATE
            End If

            If QQ(I, 2, I1, 12) > REC!(35, 0) Then
                REC!(35, 0) = QQ(I, 2, I1, 12) ' RB
                REC!(35, 1) = 1 ' TIMES DONE
                REC!(35, 2) = 2 ' HALF
                RC$(35, 0) = AA$(I, I1) ' NAME
                RC$(35, 1) = B$(I) ' TEAM
                RC$(35, 2) = VERSUS$(I) ' AT/VS
                RC$(35, 3) = B$(1 - I) ' OPPONENT
                RC$(35, 4) = Date$ ' DATE
            End If

            ' TOTAL DEFENSIVE REBOUNDS IN A GAME, HALF, QUARTER
            If A!(I, I1, 11) - A!(I, I1, 24) > REC!(37, 0) Then
                REC!(37, 0) = A!(I, I1, 11) - A!(I, I1, 24) ' FTA
                REC!(37, 1) = 1 ' TIMES DONE
                REC!(37, 2) = 0 ' GAME
                RC$(37, 0) = AA$(I, I1) ' NAME
                RC$(37, 1) = B$(I) ' TEAM
                RC$(37, 2) = VERSUS$(I) ' AT/VS
                RC$(37, 3) = B$(1 - I) ' OPPONENT
                RC$(37, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 5) - QQ(I, 1, I1, 12) > REC!(38, 0) Then
                REC!(38, 0) = QQ(I, 1, I1, 5) - QQ(I, 1, I1, 12) ' RB
                REC!(38, 1) = 1 ' TIMES DONE
                REC!(38, 2) = 1 ' HALF
                RC$(38, 0) = AA$(I, I1) ' NAME
                RC$(38, 1) = B$(I) ' TEAM
                RC$(38, 2) = VERSUS$(I) ' AT/VS
                RC$(38, 3) = B$(1 - I) ' OPPONENT
                RC$(38, 4) = Date$ ' DATE
            End If

            If QQ(I, 2, I1, 5) - QQ(I, 2, I1, 12) > REC!(38, 0) Then
                REC!(38, 0) = QQ(I, 2, I1, 5) - QQ(I, 2, I1, 12) ' RB
                REC!(38, 1) = 1 ' TIMES DONE
                REC!(38, 2) = 2 ' HALF
                RC$(38, 0) = AA$(I, I1) ' NAME
                RC$(38, 1) = B$(I) ' TEAM
                RC$(38, 2) = VERSUS$(I) ' AT/VS
                RC$(38, 3) = B$(1 - I) ' OPPONENT
                RC$(38, 4) = Date$ ' DATE
            End If

            ' ASSISTS IN A GAME, HALF, QUARTER
            If A!(I, I1, 14) > REC!(40, 0) Then
                REC!(40, 0) = A!(I, I1, 14) ' FTA
                REC!(40, 1) = 1 ' TIMES DONE
                REC!(40, 2) = 0 ' GAME
                RC$(40, 0) = AA$(I, I1) ' NAME
                RC$(40, 1) = B$(I) ' TEAM
                RC$(40, 2) = VERSUS$(I) ' AT/VS
                RC$(40, 3) = B$(1 - I) ' OPPONENT
                RC$(40, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 8) > REC!(41, 0) Then
                REC!(41, 0) = QQ(I, 1, I1, 8) ' FTA
                REC!(41, 1) = 1 ' TIMES DONE
                REC!(41, 2) = 1 ' HALF
                RC$(41, 0) = AA$(I, I1) ' NAME
                RC$(41, 1) = B$(I) ' TEAM
                RC$(41, 2) = VERSUS$(I) ' AT/VS
                RC$(41, 3) = B$(1 - I) ' OPPONENT
                RC$(41, 4) = Date$ ' DATE
            End If

            If QQ(I, 2, I1, 8) > REC!(41, 0) Then
                REC!(41, 0) = QQ(I, 2, I1, 8) ' FTM
                REC!(41, 1) = 1 ' TIMES DONE
                REC!(41, 2) = 2 ' HALF
                RC$(41, 0) = AA$(I, I1) ' NAME
                RC$(41, 1) = B$(I) ' TEAM
                RC$(41, 2) = VERSUS$(I) ' AT/VS
                RC$(41, 3) = B$(1 - I) ' OPPONENT
                RC$(41, 4) = Date$ ' DATE
            End If

            ' STEALS IN A GAME, HALF, QUARTER
            If A!(I, I1, 13) > REC!(43, 0) Then
                REC!(43, 0) = A!(I, I1, 13) ' FTA
                REC!(43, 1) = 1 ' TIMES DONE
                REC!(43, 2) = 0 ' GAME
                RC$(43, 0) = AA$(I, I1) ' NAME
                RC$(43, 1) = B$(I) ' TEAM
                RC$(43, 2) = VERSUS$(I) ' AT/VS
                RC$(43, 3) = B$(1 - I) ' OPPONENT
                RC$(43, 4) = Date$ ' DATE
            End If
            If QQ(I, 1, I1, 7) > REC!(44, 0) Then
                REC!(44, 0) = QQ(I, 1, I1, 7) ' FTA
                REC!(44, 1) = 1 ' TIMES DONE
                REC!(44, 2) = 1 ' HALF
                RC$(44, 0) = AA$(I, I1) ' NAME
                RC$(44, 1) = B$(I) ' TEAM
                RC$(44, 2) = VERSUS$(I) ' AT/VS
                RC$(44, 3) = B$(1 - I) ' OPPONENT
                RC$(44, 4) = Date$ ' DATE
            End If
            If QQ(I, 2, I1, 7) > REC!(44, 0) Then
                REC!(44, 0) = QQ(I, 2, I1, 7) ' FTM
                REC!(44, 1) = 1 ' TIMES DONE
                REC!(44, 2) = 2 ' HALF
                RC$(44, 0) = AA$(I, I1) ' NAME
                RC$(44, 1) = B$(I) ' TEAM
                RC$(44, 2) = VERSUS$(I) ' AT/VS
                RC$(44, 3) = B$(1 - I) ' OPPONENT
                RC$(44, 4) = Date$ ' DATE
            End If

            ' BLOCKS IN A GAME, HALF, QUARTER
            If A!(I, I1, 15) > REC!(46, 0) Then
                REC!(46, 0) = A!(I, I1, 15) ' FTA
                REC!(46, 1) = 1 ' TIMES DONE
                REC!(46, 2) = 0 ' GAME
                RC$(46, 0) = AA$(I, I1) ' NAME
                RC$(46, 1) = B$(I) ' TEAM
                RC$(46, 2) = VERSUS$(I) ' AT/VS
                RC$(46, 3) = B$(1 - I) ' OPPONENT
                RC$(46, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 9) > REC!(47, 0) Then
                REC!(47, 0) = QQ(I, 1, I1, 9) ' FTA
                REC!(47, 1) = 1 ' TIMES DONE
                REC!(47, 2) = 1 ' HALF
                RC$(47, 0) = AA$(I, I1) ' NAME
                RC$(47, 1) = B$(I) ' TEAM
                RC$(47, 2) = VERSUS$(I) ' AT/VS
                RC$(47, 3) = B$(1 - I) ' OPPONENT
                RC$(47, 4) = Date$ ' DATE
            End If

            If QQ(I, 2, I1, 9) > REC!(47, 0) Then
                REC!(47, 0) = QQ(I, 2, I1, 9) ' FTM
                REC!(47, 1) = 1 ' TIMES DONE
                REC!(47, 2) = 2 ' HALF
                RC$(47, 0) = AA$(I, I1) ' NAME
                RC$(47, 1) = B$(I) ' TEAM
                RC$(47, 2) = VERSUS$(I) ' AT/VS
                RC$(47, 3) = B$(1 - I) ' OPPONENT
                RC$(47, 4) = Date$ ' DATE
            End If

        Next
    Next

    ' MOST OVERTIME PERIODS
    If H - 3 > TREC!(0, 0) Then
        TREC!(0, 0) = H - 3
        TREC!(0, 1) = S!(0, 0)
        TREC!(0, 2) = S!(1, 0)
        TRC$(0, 0) = B$(0)
        TRC$(0, 1) = B$(1)
        TRC$(0, 2) = Date$
    End If

    For I = 0 To 1

        If S!(I, 0) > TREC!(1, 0) Then
            TREC!(1, 0) = S!(I, 0)
            TREC!(1, 1) = 1
            TREC!(1, 2) = 0
            TRC$(1, 0) = B$(I)
            TRC$(1, 1) = VERSUS$(I)
            TRC$(1, 2) = B$(1 - I)
            TRC$(1, 3) = Date$
        End If

        If S!(I, 0) < TREC!(2, 0) Then
            TREC!(2, 0) = S!(I, 0)
            TREC!(2, 1) = 1
            TREC!(2, 2) = 0
            TRC$(2, 0) = B$(I)
            TRC$(2, 1) = VERSUS$(I)
            TRC$(2, 2) = B$(1 - I)
            TRC$(2, 3) = Date$
        End If

        If S!(I, 0) + S!(1 - I, 0) > TREC!(3, 0) Then
            TREC!(3, 0) = S!(I, 0) + S!(1 - I, 0)
            TREC!(3, 1) = S!(I, 0)
            TREC!(3, 2) = S!(1 - I, 0)
            TRC$(3, 0) = B$(I)
            TRC$(3, 1) = VERSUS$(I)
            TRC$(3, 2) = B$(1 - I)
            TRC$(3, 3) = Date$
        End If

        If S!(I, 0) + S!(1 - I, 0) < TREC!(4, 0) Then
            TREC!(4, 0) = S!(I, 0) + S!(1 - I, 0)
            TREC!(4, 1) = S!(I, 0)
            TREC!(4, 2) = S!(1 - I, 0)
            TRC$(4, 0) = B$(I)
            TRC$(4, 1) = VERSUS$(I)
            TRC$(4, 2) = B$(1 - I)
            TRC$(4, 3) = Date$
        End If

        If S!(I, 1) > TREC!(6, 0) Then
            TREC!(6, 0) = S!(I, 1)
            TREC!(6, 1) = 1
            TREC!(6, 2) = 0
            TRC$(6, 0) = B$(I)
            TRC$(6, 1) = VERSUS$(I)
            TRC$(6, 2) = B$(1 - I)
            TRC$(6, 3) = Date$
        End If

        If S!(I, 1) < TREC!(7, 0) Then
            TREC!(7, 0) = S!(I, 1)
            TREC!(7, 1) = 1
            TREC!(7, 2) = 0
            TRC$(7, 0) = B$(I)
            TRC$(7, 1) = VERSUS$(I)
            TRC$(7, 2) = B$(1 - I)
            TRC$(7, 3) = Date$
        End If

        If S!(I, 1) + S!(1 - I, 1) > TREC!(8, 0) Then
            TREC!(8, 0) = S!(I, 1) + S!(1 - I, 1)
            TREC!(8, 1) = S!(I, 1)
            TREC!(8, 2) = S!(1 - I, 1)
            TRC$(8, 0) = B$(I)
            TRC$(8, 1) = VERSUS$(I)
            TRC$(8, 2) = B$(1 - I)
            TRC$(8, 3) = Date$
        End If

        If S!(I, 1) + S!(1 - I, 1) < TREC!(9, 0) Then
            TREC!(9, 0) = S!(I, 1) + S!(1 - I, 1)
            TREC!(9, 1) = S!(I, 1)
            TREC!(9, 2) = S!(1 - I, 1)
            TRC$(9, 0) = B$(I)
            TRC$(9, 1) = VERSUS$(I)
            TRC$(9, 2) = B$(1 - I)
            TRC$(9, 3) = Date$
        End If

        If S!(I, 2) > TREC!(10, 0) Then
            TREC!(10, 0) = S!(I, 2)
            TREC!(10, 1) = 1
            TREC!(10, 2) = 0
            TRC$(10, 0) = B$(I)
            TRC$(10, 1) = VERSUS$(I)
            TRC$(10, 2) = B$(1 - I)
            TRC$(10, 3) = Date$
        End If

        If S!(I, 2) < TREC!(11, 0) Then
            TREC!(11, 0) = S!(I, 2)
            TREC!(11, 1) = 1
            TREC!(11, 2) = 0
            TRC$(11, 0) = B$(I)
            TRC$(11, 1) = VERSUS$(I)
            TRC$(11, 2) = B$(1 - I)
            TRC$(11, 3) = Date$
        End If

        If S!(I, 2) + S!(1 - I, 2) > TREC!(12, 0) Then
            TREC!(12, 0) = S!(I, 2) + S!(1 - I, 2)
            TREC!(12, 1) = S!(I, 2)
            TREC!(12, 2) = S!(1 - I, 2)
            TRC$(12, 0) = B$(I)
            TRC$(12, 1) = VERSUS$(I)
            TRC$(12, 2) = B$(1 - I)
            TRC$(12, 3) = Date$
        End If

        If S!(I, 2) + S!(1 - I, 2) < TREC!(13, 0) Then
            TREC!(13, 0) = S!(I, 2) + S!(1 - I, 2)
            TREC!(13, 1) = S!(I, 2)
            TREC!(13, 2) = S!(1 - I, 2)
            TRC$(13, 0) = B$(I)
            TRC$(13, 1) = VERSUS$(I)
            TRC$(13, 2) = B$(1 - I)
            TRC$(13, 3) = Date$
        End If

        For I2 = 3 To 8
            If S!(I, I2) > TREC!(14, 0) Then
                TREC!(14, 0) = S!(I, I2)
                TREC!(14, 1) = I2
                TREC!(14, 2) = 0
                TRC$(14, 0) = B$(I)
                TRC$(14, 1) = VERSUS$(I)
                TRC$(14, 2) = B$(1 - I)
                TRC$(14, 3) = Date$
            End If

            If S!(I, I2) < TREC!(15, 0) Then
                TREC!(15, 0) = S!(I, I2)
                TREC!(15, 1) = I2
                TREC!(15, 2) = 0
                TRC$(15, 0) = B$(I)
                TRC$(15, 1) = VERSUS$(I)
                TRC$(15, 2) = B$(1 - I)
                TRC$(15, 3) = Date$
            End If

            If S!(I, I2) + S!(1 - I, I2) > TREC!(16, 0) Then
                TREC!(16, 0) = S!(I, I2) + S!(1 - I, I2)
                TREC!(16, 1) = S!(I, I2)
                TREC!(16, 2) = S!(1 - I, I2)
                TRC$(16, 0) = B$(I)
                TRC$(16, 1) = VERSUS$(I)
                TRC$(16, 2) = B$(1 - I)
                TRC$(16, 3) = Date$
            End If

            If S!(I, I2) + S!(1 - I, I2) < TREC!(17, 0) Then
                TREC!(17, 0) = S!(I, I2) + S!(1 - I, I2)
                TREC!(17, 1) = S!(I, I2)
                TREC!(17, 2) = S!(1 - I, I2)
                TRC$(17, 0) = B$(I)
                TRC$(17, 1) = VERSUS$(I)
                TRC$(17, 2) = B$(1 - I)
                TRC$(17, 3) = Date$
            End If
        Next I2

        ' TEAM FIELD GOAL PERCENTAGES
        For I1 = 0 To 14: V8(I, I1) = 0: V8(1 - I, I1) = 0: Next
        For I2 = 0 To 1
            For I1 = 0 To 13
                V8(I2, 0) = V8(I2, 0) + A!(I2, I1, 8)
                V8(I2, 1) = V8(I2, 1) + A!(I2, I1, 7)
                V8(I2, 2) = V8(I2, 2) + A!(I2, I1, 10)
                V8(I2, 3) = V8(I2, 3) + A!(I2, I1, 9)
                V8(I2, 4) = V8(I2, 4) + A!(I2, I1, 11)
                V8(I2, 5) = V8(I2, 5) + (A!(I2, I1, 8) * 2) + A!(I2, I1, 10) + W%(I2, I1, 0)
                V8(I2, 13) = V8(I2, 13) + W%(I2, I1, 0)
                V8(I2, 14) = V8(I2, 14) + W%(I2, I1, 1)
                V8(I2, 6) = V8(I2, 6) + A!(I2, I1, 12)
                V8(I2, 7) = V8(I2, 7) + A!(I2, I1, 14)
                V8(I2, 8) = V8(I2, 8) + A!(I2, I1, 15)
                V8(I2, 9) = V8(I2, 9) + A!(I2, I1, 13)
                V8(I2, 11) = V8(I2, 11) + A!(I2, I1, 24)
                V8(I2, 12) = V8(I2, 12) + A!(I2, I1, 23)
                V8(I2, 10) = V8(I2, 10) + timePlayed(I2, I1)
            Next
        Next

        If V8(I, 1) <> 0 Then

            If V8(I, 0) / V8(I, 1) > TREC!(30, 0) Then
                TREC!(30, 0) = V8(I, 0) / V8(I, 1)
                TREC!(30, 1) = V8(I, 0)
                TREC!(30, 2) = V8(I, 1)
                TRC$(30, 0) = B$(I)
                TRC$(30, 1) = VERSUS$(I)
                TRC$(30, 2) = B$(1 - I)
                TRC$(30, 3) = Date$
            End If

            If V8(I, 0) / V8(I, 1) < TREC!(31, 0) Then
                TREC!(31, 0) = V8(I, 0) / V8(I, 1)
                TREC!(31, 1) = V8(I, 0)
                TREC!(31, 2) = V8(I, 1)
                TRC$(31, 0) = B$(I)
                TRC$(31, 1) = VERSUS$(I)
                TRC$(31, 2) = B$(1 - I)
                TRC$(31, 3) = Date$
            End If

        End If

        If V8(I, 1) + V8(1 - I, 1) <> 0 Then

            If (V8(I, 0) + V8(1 - I, 0)) / (V8(I, 1) + V8(1 - I, 1)) > TREC!(32, 0) Then
                TREC!(32, 0) = (V8(I, 0) + V8(1 - I, 0)) / (V8(I, 1) + V8(1 - I, 1))
                TREC!(32, 1) = V8(I, 0) / V8(I, 1)
                TREC!(32, 2) = V8(1 - I, 0) / (V8(1 - I, 1) + 0.01)
                TRC$(32, 0) = B$(I)
                TRC$(32, 1) = VERSUS$(I)
                TRC$(32, 2) = B$(1 - I)
                TRC$(32, 3) = Date$
            End If

            If (V8(I, 0) + V8(1 - I, 0)) / (V8(I, 1) + V8(1 - I, 1)) < TREC!(33, 0) Then
                TREC!(33, 0) = (V8(I, 0) + V8(1 - I, 0)) / (V8(I, 1) + V8(1 - I, 1))
                TREC!(33, 1) = V8(I, 0) / V8(I, 1)
                TREC!(33, 2) = V8(1 - I, 0) / (V8(1 - I, 1) + 0.01)
                TRC$(33, 0) = B$(I)
                TRC$(33, 1) = VERSUS$(I)
                TRC$(33, 2) = B$(1 - I)
                TRC$(33, 3) = Date$
            End If

        End If

        ' TEAM FIELD GOALS MADE
        If V8(I, 0) > TREC!(34, 0) Then
            TREC!(34, 0) = V8(I, 0)
            TREC!(34, 1) = 0
            TREC!(34, 2) = 0
            TRC$(34, 0) = B$(I)
            TRC$(34, 1) = VERSUS$(I)
            TRC$(34, 2) = B$(1 - I)
            TRC$(34, 3) = Date$
        End If

        If V8(I, 0) < TREC!(35, 0) Then
            TREC!(35, 0) = V8(I, 0)
            TREC!(35, 1) = 0
            TREC!(35, 2) = 0
            TRC$(35, 0) = B$(I)
            TRC$(35, 1) = VERSUS$(I)
            TRC$(35, 2) = B$(1 - I)
            TRC$(35, 3) = Date$
        End If

        If V8(I, 0) + V8(1 - I, 0) > TREC!(36, 0) Then
            TREC!(36, 0) = V8(I, 0) + V8(1 - I, 0)
            TREC!(36, 1) = V8(I, 0)
            TREC!(36, 2) = V8(1 - I, 0)
            TRC$(36, 0) = B$(I)
            TRC$(36, 1) = VERSUS$(I)
            TRC$(36, 2) = B$(1 - I)
            TRC$(36, 3) = Date$
        End If

        If V8(I, 0) + V8(1 - I, 0) < TREC!(37, 0) Then
            TREC!(37, 0) = V8(I, 0) + V8(1 - I, 0)
            TREC!(37, 1) = V8(I, 0)
            TREC!(37, 2) = V8(1 - I, 0)
            TRC$(37, 0) = B$(I)
            TRC$(37, 1) = VERSUS$(I)
            TRC$(37, 2) = B$(1 - I)
            TRC$(37, 3) = Date$
        End If

        If QR(I, 1, 0) > TREC!(38, 0) Then
            TREC!(38, 0) = QR(I, 1, 0)
            TREC!(38, 1) = 1
            TREC!(38, 2) = 0
            TRC$(38, 0) = B$(I)
            TRC$(38, 1) = VERSUS$(I)
            TRC$(38, 2) = B$(1 - I)
            TRC$(38, 3) = Date$
        End If

        If QR(I, 2, 0) > TREC!(38, 0) Then
            TREC!(38, 0) = QR(I, 2, 0)
            TREC!(38, 1) = 2
            TREC!(38, 2) = 0
            TRC$(38, 0) = B$(I)
            TRC$(38, 1) = VERSUS$(I)
            TRC$(38, 2) = B$(1 - I)
            TRC$(38, 3) = Date$
        End If

        If QR(I, 1, 0) + QR(1 - I, 1, 0) > TREC!(39, 0) Then
            TREC!(39, 0) = QR(I, 1, 0) + QR(1 - I, 1, 0)
            TREC!(39, 1) = 1
            TREC!(39, 2) = 0
            TRC$(39, 0) = B$(I)
            TRC$(39, 1) = VERSUS$(I)
            TRC$(39, 2) = B$(1 - I)
            TRC$(39, 3) = Date$
        End If

        If QR(I, 2, 0) + QR(1 - I, 2, 0) > TREC!(39, 0) Then
            TREC!(39, 0) = QR(I, 2, 0) + QR(1 - I, 2, 0)
            TREC!(39, 1) = 2
            TREC!(39, 2) = 0
            TRC$(39, 0) = B$(I)
            TRC$(39, 1) = VERSUS$(I)
            TRC$(39, 2) = B$(1 - I)
            TRC$(39, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS
        If V8(I, 1) > TREC!(42, 0) Then
            TREC!(42, 0) = V8(I, 1)
            TREC!(42, 1) = 0
            TREC!(42, 2) = 0
            TRC$(42, 0) = B$(I)
            TRC$(42, 1) = VERSUS$(I)
            TRC$(42, 2) = B$(1 - I)
            TRC$(42, 3) = Date$
        End If

        If V8(I, 1) < TREC!(43, 0) Then
            TREC!(43, 0) = V8(I, 1)
            TREC!(43, 1) = 0
            TREC!(43, 2) = 0
            TRC$(43, 0) = B$(I)
            TRC$(43, 1) = VERSUS$(I)
            TRC$(43, 2) = B$(1 - I)
            TRC$(43, 3) = Date$
        End If

        If V8(I, 1) + V8(1 - I, 1) > TREC!(44, 0) Then
            TREC!(44, 0) = V8(I, 1) + V8(1 - I, 1)
            TREC!(44, 1) = V8(I, 1)
            TREC!(44, 2) = V8(1 - I, 1)
            TRC$(44, 0) = B$(I)
            TRC$(44, 1) = VERSUS$(I)
            TRC$(44, 2) = B$(1 - I)
            TRC$(44, 3) = Date$
        End If

        If V8(I, 1) + V8(1 - I, 1) < TREC!(45, 0) Then
            TREC!(45, 0) = V8(I, 1) + V8(1 - I, 1)
            TREC!(45, 1) = V8(I, 1)
            TREC!(45, 2) = V8(1 - I, 1)
            TRC$(45, 0) = B$(I)
            TRC$(45, 1) = VERSUS$(I)
            TRC$(45, 2) = B$(1 - I)
            TRC$(45, 3) = Date$
        End If

        If QR(I, 1, 1) > TREC!(46, 0) Then
            TREC!(46, 0) = QR(I, 1, 1)
            TREC!(46, 1) = 1
            TREC!(46, 2) = 0
            TRC$(46, 0) = B$(I)
            TRC$(46, 1) = VERSUS$(I)
            TRC$(46, 2) = B$(1 - I)
            TRC$(46, 3) = Date$
        End If

        If QR(I, 2, 1) > TREC!(46, 0) Then
            TREC!(46, 0) = QR(I, 2, 1)
            TREC!(46, 1) = 2
            TREC!(46, 2) = 0
            TRC$(46, 0) = B$(I)
            TRC$(46, 1) = VERSUS$(I)
            TRC$(46, 2) = B$(1 - I)
            TRC$(46, 3) = Date$
        End If

        If QR(I, 1, 1) + QR(1 - I, 1, 1) > TREC!(47, 0) Then
            TREC!(47, 0) = QR(I, 1, 1) + QR(1 - I, 1, 1)
            TREC!(47, 1) = 1
            TREC!(47, 2) = QR(I, 1, 1)
            TRC$(47, 0) = B$(I)
            TRC$(47, 1) = VERSUS$(I)
            TRC$(47, 2) = B$(1 - I)
            TRC$(47, 3) = Date$
        End If

        If QR(I, 2, 1) + QR(1 - I, 2, 1) > TREC!(47, 0) Then
            TREC!(47, 0) = QR(I, 2, 1) + QR(1 - I, 2, 1)
            TREC!(47, 1) = 2
            TREC!(47, 2) = QR(I, 2, 1)
            TRC$(47, 0) = B$(I)
            TRC$(47, 1) = VERSUS$(I)
            TRC$(47, 2) = B$(1 - I)
            TRC$(47, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS
        If V8(I, 13) = V8(I, 14) And V8(I, 14) > TREC!(50, 0) Then
            TREC!(50, 0) = V8(I, 13)
            TREC!(50, 1) = 0
            TREC!(50, 2) = 0
            TRC$(50, 0) = B$(I)
            TRC$(50, 1) = VERSUS$(I)
            TRC$(50, 2) = B$(1 - I)
            TRC$(50, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS MOST MADE
        If V8(I, 13) = V8(I, 14) And V8(1 - I, 13) = V8(1 - I, 14) And V8(I, 14) + V8(1 - I, 14) > TREC!(51, 0) Then
            TREC!(51, 0) = V8(I, 13)
            TREC!(51, 1) = V8(1 - I, 13)
            TREC!(51, 2) = 0
            TRC$(51, 0) = B$(I)
            TRC$(51, 1) = VERSUS$(I)
            TRC$(51, 2) = B$(1 - I)
            TRC$(51, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS NONE MADE
        If V8(I, 13) = 0 And V8(I, 14) > TREC!(52, 0) Then
            TREC!(52, 0) = V8(I, 14)
            TREC!(52, 1) = 0
            TREC!(52, 2) = 0
            TRC$(52, 0) = B$(I)
            TRC$(52, 1) = VERSUS$(I)
            TRC$(52, 2) = B$(1 - I)
            TRC$(52, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS NONE MADE
        If V8(I, 13) > TREC!(53, 0) Then
            TREC!(53, 0) = V8(I, 13)
            TREC!(53, 1) = 0
            TREC!(53, 2) = 0
            TRC$(53, 0) = B$(I)
            TRC$(53, 1) = VERSUS$(I)
            TRC$(53, 2) = B$(1 - I)
            TRC$(53, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS NONE MADE
        If V8(I, 13) + V8(1 - I, 13) > TREC!(54, 0) Then
            TREC!(54, 0) = V8(I, 13) + V8(1 - I, 13)
            TREC!(54, 1) = V8(I, 13)
            TREC!(54, 2) = V8(1 - I, 13)
            TRC$(54, 0) = B$(I)
            TRC$(54, 1) = VERSUS$(I)
            TRC$(54, 2) = B$(1 - I)
            TRC$(54, 3) = Date$
        End If

        If QR(I, 1, 13) > TREC!(55, 0) Then
            TREC!(55, 0) = QR(I, 1, 13)
            TREC!(55, 1) = 1
            TREC!(55, 2) = 0
            TRC$(55, 0) = B$(I)
            TRC$(55, 1) = VERSUS$(I)
            TRC$(55, 2) = B$(1 - I)
            TRC$(55, 3) = Date$
        End If

        If QR(I, 2, 13) > TREC!(55, 0) Then
            TREC!(55, 0) = QR(I, 2, 13)
            TREC!(55, 1) = 2
            TREC!(55, 2) = 0
            TRC$(55, 0) = B$(I)
            TRC$(55, 1) = VERSUS$(I)
            TRC$(55, 2) = B$(1 - I)
            TRC$(55, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS NONE MADE
        If V8(I, 14) > TREC!(57, 0) Then
            TREC!(57, 0) = V8(I, 14)
            TREC!(57, 1) = 0
            TREC!(57, 2) = 0
            TRC$(57, 0) = B$(I)
            TRC$(57, 1) = VERSUS$(I)
            TRC$(57, 2) = B$(1 - I)
            TRC$(57, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS NONE MADE
        If V8(I, 14) + V8(1 - I, 14) > TREC!(58, 0) Then
            TREC!(58, 0) = V8(I, 14) + V8(1 - I, 14)
            TREC!(58, 1) = V8(I, 14)
            TREC!(58, 2) = V8(1 - I, 14)
            TRC$(58, 0) = B$(I)
            TRC$(58, 1) = VERSUS$(I)
            TRC$(58, 2) = B$(1 - I)
            TRC$(58, 3) = Date$
        End If

        If QR(I, 1, 14) > TREC!(59, 0) Then
            TREC!(59, 0) = QR(I, 1, 14)
            TREC!(59, 1) = 1
            TREC!(59, 2) = 0
            TRC$(59, 0) = B$(I)
            TRC$(59, 1) = VERSUS$(I)
            TRC$(59, 2) = B$(1 - I)
            TRC$(59, 3) = Date$
        End If

        If QR(I, 2, 14) > TREC!(59, 0) Then
            TREC!(59, 0) = QR(I, 2, 14)
            TREC!(59, 1) = 2
            TREC!(59, 2) = 0
            TRC$(59, 0) = B$(I)
            TRC$(59, 1) = VERSUS$(I)
            TRC$(59, 2) = B$(1 - I)
            TRC$(59, 3) = Date$
        End If

        ' FREE THROW ATTEMPTS NO MISS
        If V8(I, 2) = V8(I, 3) And V8(I, 2) > TREC!(60, 0) Then
            TREC!(60, 0) = V8(I, 2)
            TREC!(60, 1) = 0
            TREC!(60, 2) = 0
            TRC$(60, 0) = B$(I)
            TRC$(60, 1) = VERSUS$(I)
            TRC$(60, 2) = B$(1 - I)
            TRC$(60, 3) = Date$
        End If

        ' FREE THROW ATTEMPTS LOW %
        If V8(I, 3) <> 0 Then

            If V8(I, 2) / V8(I, 3) < TREC!(61, 0) Then
                TREC!(61, 0) = V8(I, 2) / V8(I, 3)
                TREC!(61, 1) = V8(I, 2)
                TREC!(61, 2) = V8(I, 3)
                TRC$(61, 0) = B$(I)
                TRC$(61, 1) = VERSUS$(I)
                TRC$(61, 2) = B$(1 - I)
                TRC$(61, 3) = Date$
            End If

        End If

        ' FREE THROW ATTEMPTS COMBINED HIGH %
        If V8(I, 3) + V8(1 - I, 3) <> 0 Then

            If (V8(I, 2) + V8(1 - I, 2)) / (V8(I, 3) + V8(1 - I, 3)) > TREC!(62, 0) Then
                TREC!(62, 0) = (V8(I, 2) + V8(1 - I, 2)) / (V8(I, 3) + V8(1 - I, 3))
                TREC!(62, 1) = V8(I, 2) / V8(I, 3)
                TREC!(62, 2) = V8(1 - I, 2) / (V8(1 - I, 3) + 0.01)
                TRC$(62, 0) = B$(I)
                TRC$(62, 1) = VERSUS$(I)
                TRC$(62, 2) = B$(1 - I)
                TRC$(62, 3) = Date$
            End If

            ' FREE THROW ATTEMPTS COMBINED LOW %
            If (V8(I, 2) + V8(1 - I, 2)) / (V8(I, 3) + V8(1 - I, 3)) < TREC!(63, 0) Then
                TREC!(63, 0) = (V8(I, 2) + V8(1 - I, 2)) / (V8(I, 3) + V8(1 - I, 3))
                TREC!(63, 1) = V8(I, 2) / V8(I, 3)
                TREC!(63, 2) = V8(1 - I, 2) / (V8(1 - I, 3) + 0.01)
                TRC$(63, 0) = B$(I)
                TRC$(63, 1) = VERSUS$(I)
                TRC$(63, 2) = B$(1 - I)
                TRC$(63, 3) = Date$
            End If
        End If

        ' TEAM FREE THROWS MADE
        104 If V8(I, 2) > TREC!(64, 0) Then
            TREC!(64, 0) = V8(I, 2)
            TREC!(64, 1) = 0
            TREC!(64, 2) = 0
            TRC$(64, 0) = B$(I)
            TRC$(64, 1) = VERSUS$(I)
            TRC$(64, 2) = B$(1 - I)
            TRC$(64, 3) = Date$
        End If

        If V8(I, 2) < TREC!(65, 0) Then
            TREC!(65, 0) = V8(I, 2)
            TREC!(65, 1) = 0
            TREC!(65, 2) = 0
            TRC$(65, 0) = B$(I)
            TRC$(65, 1) = VERSUS$(I)
            TRC$(65, 2) = B$(1 - I)
            TRC$(65, 3) = Date$
        End If

        If V8(I, 2) + V8(1 - I, 2) > TREC!(66, 0) Then
            TREC!(66, 0) = V8(I, 2) + V8(1 - I, 2)
            TREC!(66, 1) = V8(I, 2)
            TREC!(66, 2) = V8(1 - I, 2)
            TRC$(66, 0) = B$(I)
            TRC$(66, 1) = VERSUS$(I)
            TRC$(66, 2) = B$(1 - I)
            TRC$(66, 3) = Date$
        End If

        If V8(I, 2) + V8(1 - I, 2) < TREC!(67, 0) Then
            TREC!(67, 0) = V8(I, 2) + V8(1 - I, 2)
            TREC!(67, 1) = V8(I, 2)
            TREC!(67, 2) = V8(1 - I, 2)
            TRC$(67, 0) = B$(I)
            TRC$(67, 1) = VERSUS$(I)
            TRC$(67, 2) = B$(1 - I)
            TRC$(67, 3) = Date$
        End If

        If QR(I, 1, 2) > TREC!(68, 0) Then
            TREC!(68, 0) = QR(I, 1, 2)
            TREC!(68, 1) = 1
            TREC!(68, 2) = 0
            TRC$(68, 0) = B$(I)
            TRC$(68, 1) = VERSUS$(I)
            TRC$(68, 2) = B$(1 - I)
            TRC$(68, 3) = Date$
        End If

        If QR(I, 2, 2) > TREC!(68, 0) Then
            TREC!(68, 0) = QR(I, 2, 2)
            TREC!(68, 1) = 2
            TREC!(68, 2) = 0
            TRC$(68, 0) = B$(I)
            TRC$(68, 1) = VERSUS$(I)
            TRC$(68, 2) = B$(1 - I)
            TRC$(68, 3) = Date$
        End If

        ' TEAM FREE THROWS ATTEMPTED
        If V8(I, 3) > TREC!(72, 0) Then
            TREC!(72, 0) = V8(I, 3)
            TREC!(72, 1) = 0
            TREC!(72, 2) = 0
            TRC$(72, 0) = B$(I)
            TRC$(72, 1) = VERSUS$(I)
            TRC$(72, 2) = B$(1 - I)
            TRC$(72, 3) = Date$
        End If

        If V8(I, 3) < TREC!(73, 0) Then
            TREC!(73, 0) = V8(I, 3)
            TREC!(73, 1) = 0
            TREC!(73, 2) = 0
            TRC$(73, 0) = B$(I)
            TRC$(73, 1) = VERSUS$(I)
            TRC$(73, 2) = B$(1 - I)
            TRC$(73, 3) = Date$
        End If

        If V8(I, 3) + V8(1 - I, 3) > TREC!(74, 0) Then
            TREC!(74, 0) = V8(I, 3) + V8(1 - I, 3)
            TREC!(74, 1) = V8(I, 3)
            TREC!(74, 2) = V8(1 - I, 3)
            TRC$(74, 0) = B$(I)
            TRC$(74, 1) = VERSUS$(I)
            TRC$(74, 2) = B$(1 - I)
            TRC$(74, 3) = Date$
        End If

        If V8(I, 3) + V8(1 - I, 3) < TREC!(75, 0) Then
            TREC!(75, 0) = V8(I, 3) + V8(1 - I, 3)
            TREC!(75, 1) = V8(I, 3)
            TREC!(75, 2) = V8(1 - I, 3)
            TRC$(75, 0) = B$(I)
            TRC$(75, 1) = VERSUS$(I)
            TRC$(75, 2) = B$(1 - I)
            TRC$(75, 3) = Date$
        End If

        If QR(I, 1, 3) > TREC!(76, 0) Then
            TREC!(76, 0) = QR(I, 1, 3)
            TREC!(76, 1) = 1
            TREC!(76, 2) = 0
            TRC$(76, 0) = B$(I)
            TRC$(76, 1) = VERSUS$(I)
            TRC$(76, 2) = B$(1 - I)
            TRC$(76, 3) = Date$
        End If

        If QR(I, 2, 3) > TREC!(76, 0) Then
            TREC!(76, 0) = QR(I, 2, 3)
            TREC!(76, 1) = 2
            TREC!(76, 2) = 0
            TRC$(76, 0) = B$(I)
            TRC$(76, 1) = VERSUS$(I)
            TRC$(76, 2) = B$(1 - I)
            TRC$(76, 3) = Date$
        End If

        If QR(I, 1, 3) + QR(1 - I, 1, 3) > TREC!(77, 0) Then
            TREC!(77, 0) = QR(I, 1, 3) + QR(1 - I, 1, 3)
            TREC!(77, 1) = 1
            TREC!(77, 2) = QR(I, 1, 3)
            TRC$(77, 0) = B$(I)
            TRC$(77, 1) = VERSUS$(I)
            TRC$(77, 2) = B$(1 - I)
            TRC$(77, 3) = Date$
        End If

        If QR(I, 2, 3) + QR(1 - I, 2, 3) > TREC!(77, 0) Then
            TREC!(77, 0) = QR(I, 2, 3) + QR(1 - I, 2, 3)
            TREC!(77, 1) = 2
            TREC!(77, 2) = QR(I, 2, 3)
            TRC$(77, 0) = B$(I)
            TRC$(77, 1) = VERSUS$(I)
            TRC$(77, 2) = B$(1 - I)
            TRC$(77, 3) = Date$
        End If

        If R3!(I) > TREC!(80, 0) Then
            TREC!(80, 0) = R3!(I)
            TREC!(80, 1) = 0
            TREC!(80, 2) = 0
            TRC$(80, 0) = B$(I)
            TRC$(80, 1) = VERSUS$(I)
            TRC$(80, 2) = B$(1 - I)
            TRC$(80, 3) = Date$
        End If

        If R3!(I) < TREC!(81, 0) Then
            TREC!(81, 0) = R3!(I)
            TREC!(81, 1) = 0
            TREC!(81, 2) = 0
            TRC$(81, 0) = B$(I)
            TRC$(81, 1) = VERSUS$(I)
            TRC$(81, 2) = B$(1 - I)
            TRC$(81, 3) = Date$
        End If

        If R3!(I) + R3!(1 - I) > TREC!(82, 0) Then
            TREC!(82, 0) = R3!(I) + R3!(1 - I)
            TREC!(82, 1) = R3!(I)
            TREC!(82, 2) = R3!(1 - I)
            TRC$(82, 0) = B$(I)
            TRC$(82, 1) = VERSUS$(I)
            TRC$(82, 2) = B$(1 - I)
            TRC$(82, 3) = Date$
        End If

        If R3!(I) + R3!(1 - I) < TREC!(83, 0) Then
            TREC!(83, 0) = R3!(I) + R3!(1 - I)
            TREC!(83, 1) = R3!(I)
            TREC!(83, 2) = R3!(1 - I)
            TRC$(83, 0) = B$(I)
            TRC$(83, 1) = VERSUS$(I)
            TRC$(83, 2) = B$(1 - I)
            TRC$(83, 3) = Date$
        End If

        If QR(I, 1, 4) > TREC!(84, 0) Then
            TREC!(84, 0) = QR(I, 1, 4)
            TREC!(84, 1) = 1
            TREC!(84, 2) = 0
            TRC$(84, 0) = B$(I)
            TRC$(84, 1) = VERSUS$(I)
            TRC$(84, 2) = B$(1 - I)
            TRC$(84, 3) = Date$
        End If

        If QR(I, 2, 4) > TREC!(84, 0) Then
            TREC!(84, 0) = QR(I, 2, 4)
            TREC!(84, 1) = 2
            TREC!(84, 2) = 0
            TRC$(84, 0) = B$(I)
            TRC$(84, 1) = VERSUS$(I)
            TRC$(84, 2) = B$(1 - I)
            TRC$(84, 3) = Date$
        End If

        If V8(I, 11) > TREC!(86, 0) Then
            TREC!(86, 0) = V8(I, 11)
            TREC!(86, 1) = 0
            TREC!(86, 2) = 0
            TRC$(86, 0) = B$(I)
            TRC$(86, 1) = VERSUS$(I)
            TRC$(86, 2) = B$(1 - I)
            TRC$(86, 3) = Date$
        End If

        If V8(I, 11) < TREC!(87, 0) Then
            TREC!(87, 0) = V8(I, 11)
            TREC!(87, 1) = 0
            TREC!(87, 2) = 0
            TRC$(87, 0) = B$(I)
            TRC$(87, 1) = VERSUS$(I)
            TRC$(87, 2) = B$(1 - I)
            TRC$(87, 3) = Date$
        End If

        If V8(I, 11) + V8(1 - I, 11) > TREC!(88, 0) Then
            TREC!(88, 0) = V8(I, 11) + V8(1 - I, 11)
            TREC!(88, 1) = V8(I, 11)
            TREC!(88, 2) = V8(1 - I, 11)
            TRC$(88, 0) = B$(I)
            TRC$(88, 1) = VERSUS$(I)
            TRC$(88, 2) = B$(1 - I)
            TRC$(88, 3) = Date$
        End If

        If V8(I, 11) + V8(1 - I, 11) < TREC!(89, 0) Then
            TREC!(89, 0) = V8(I, 11) + V8(1 - I, 11)
            TREC!(89, 1) = V8(I, 11)
            TREC!(89, 2) = V8(1 - I, 11)
            TRC$(89, 0) = B$(I)
            TRC$(89, 1) = VERSUS$(I)
            TRC$(89, 2) = B$(1 - I)
            TRC$(89, 3) = Date$
        End If

        If QR(I, 1, 11) > TREC!(90, 0) Then
            TREC!(90, 0) = QR(I, 1, 11)
            TREC!(90, 1) = 1
            TREC!(90, 2) = 0
            TRC$(90, 0) = B$(I)
            TRC$(90, 1) = VERSUS$(I)
            TRC$(90, 2) = B$(1 - I)
            TRC$(90, 3) = Date$
        End If

        If QR(I, 2, 11) > TREC!(90, 0) Then
            TREC!(90, 0) = QR(I, 2, 11)
            TREC!(90, 1) = 2
            TREC!(90, 2) = 0
            TRC$(90, 0) = B$(I)
            TRC$(90, 1) = VERSUS$(I)
            TRC$(90, 2) = B$(1 - I)
            TRC$(90, 3) = Date$
        End If

        ' ASSISTS
        If V8(I, 7) > TREC!(96, 0) Then
            TREC!(96, 0) = V8(I, 7)
            TREC!(96, 1) = 0
            TREC!(96, 2) = 0
            TRC$(96, 0) = B$(I)
            TRC$(96, 1) = VERSUS$(I)
            TRC$(96, 2) = B$(1 - I)
            TRC$(96, 3) = Date$
        End If

        If V8(I, 7) < TREC!(97, 0) Then
            TREC!(97, 0) = V8(I, 7)
            TREC!(97, 1) = 0
            TREC!(97, 2) = 0
            TRC$(97, 0) = B$(I)
            TRC$(97, 1) = VERSUS$(I)
            TRC$(97, 2) = B$(1 - I)
            TRC$(97, 3) = Date$
        1 End If

        If V8(I, 7) + V8(1 - I, 7) > TREC!(98, 0) Then
            TREC!(98, 0) = V8(I, 7) + V8(1 - I, 7)
            TREC!(98, 1) = V8(I, 7)
            TREC!(98, 2) = V8(1 - I, 7)
            TRC$(98, 0) = B$(I)
            TRC$(98, 1) = VERSUS$(I)
            TRC$(98, 2) = B$(1 - I)
            TRC$(98, 3) = Date$
        2 End If

        If V8(I, 7) + V8(1 - I, 7) < TREC!(99, 0) Then
            TREC!(99, 0) = V8(I, 7) + V8(1 - I, 7)
            TREC!(99, 1) = V8(I, 7)
            TREC!(99, 2) = V8(1 - I, 7)
            TRC$(99, 0) = B$(I)
            TRC$(99, 1) = VERSUS$(I)
            TRC$(99, 2) = B$(1 - I)
            TRC$(99, 3) = Date$
        3 End If

        If QR(I, 1, 7) > TREC!(100, 0) Then
            TREC!(100, 0) = QR(I, 1, 7)
            TREC!(100, 1) = 1
            TREC!(100, 2) = 0
            TRC$(100, 0) = B$(I)
            TRC$(100, 1) = VERSUS$(I)
            TRC$(100, 2) = B$(1 - I)
            TRC$(100, 3) = Date$
        4 End If

        If QR(I, 2, 7) > TREC!(100, 0) Then
            TREC!(100, 0) = QR(I, 2, 7)
            TREC!(100, 1) = 2
            TREC!(100, 2) = 0
            TRC$(100, 0) = B$(I)
            TRC$(100, 1) = VERSUS$(I)
            TRC$(100, 2) = B$(1 - I)
            TRC$(100, 3) = Date$
        End If

        If QR(I, 2, 7) + QR(1 - I, 2, 7) > TREC!(101, 0) Then
            TREC!(101, 0) = QR(I, 2, 7) + QR(1 - I, 2, 7)
            TREC!(101, 1) = 2
            TREC!(101, 2) = 0
            TRC$(101, 0) = B$(I)
            TRC$(101, 1) = VERSUS$(I)
            TRC$(101, 2) = B$(1 - I)
            TRC$(101, 3) = Date$
        5 End If

        ' FOULS
        If V8(I, 6) > TREC!(104, 0) Then
            TREC!(104, 0) = V8(I, 6)
            TREC!(104, 1) = 0
            TREC!(104, 2) = 0
            TRC$(104, 0) = B$(I)
            TRC$(104, 1) = VERSUS$(I)
            TRC$(104, 2) = B$(1 - I)
            TRC$(104, 3) = Date$
        10 End If

        If V8(I, 6) < TREC!(105, 0) Then
            TREC!(105, 0) = V8(I, 6)
            TREC!(105, 1) = 0
            TREC!(105, 2) = 0
            TRC$(105, 0) = B$(I)
            TRC$(105, 1) = VERSUS$(I)
            TRC$(105, 2) = B$(1 - I)
            TRC$(105, 3) = Date$
        11 End If

        If V8(I, 6) + V8(1 - I, 6) > TREC!(106, 0) Then
            TREC!(106, 0) = V8(I, 6) + V8(1 - I, 6)
            TREC!(106, 1) = V8(I, 6)
            TREC!(106, 2) = V8(1 - I, 6)
            TRC$(106, 0) = B$(I)
            TRC$(106, 1) = VERSUS$(I)
            TRC$(106, 2) = B$(1 - I)
            TRC$(106, 3) = Date$
        12 End If

        If V8(I, 6) + V8(1 - I, 6) < TREC!(107, 0) Then
            TREC!(107, 0) = V8(I, 6) + V8(1 - I, 6)
            TREC!(107, 1) = V8(I, 6)
            TREC!(107, 2) = V8(1 - I, 6)
            TRC$(107, 0) = B$(I)
            TRC$(107, 1) = VERSUS$(I)
            TRC$(107, 2) = B$(1 - I)
            TRC$(107, 3) = Date$
        13 End If

        If QR(I, 1, 6) > TREC!(108, 0) Then
            TREC!(108, 0) = QR(I, 1, 6)
            TREC!(108, 1) = 1
            TREC!(108, 2) = 0
            TRC$(108, 0) = B$(I)
            TRC$(108, 1) = VERSUS$(I)
            TRC$(108, 2) = B$(1 - I)
            TRC$(108, 3) = Date$
        14 End If

        If QR(I, 2, 6) > TREC!(108, 0) Then
            TREC!(108, 0) = QR(I, 2, 6)
            TREC!(108, 1) = 2
            TREC!(108, 2) = 0
            TRC$(108, 0) = B$(I)
            TRC$(108, 1) = VERSUS$(I)
            TRC$(108, 2) = B$(1 - I)
            TRC$(108, 3) = Date$
        15 End If

        If QR(I, 1, 6) + QR(1 - I, 1, 6) > TREC!(109, 0) Then
            TREC!(109, 0) = QR(I, 1, 6) + QR(1 - I, 1, 6)
            TREC!(109, 1) = 1
            TREC!(109, 2) = QR(I, 1, 6)
            TRC$(109, 0) = B$(I)
            TRC$(109, 1) = VERSUS$(I)
            TRC$(109, 2) = B$(1 - I)
            TRC$(109, 3) = Date$
        16 End If

        If QR(I, 2, 6) + QR(1 - I, 2, 6) > TREC!(109, 0) Then
            TREC!(109, 0) = QR(I, 2, 6) + QR(1 - I, 2, 6)
            TREC!(109, 1) = 2
            TREC!(109, 2) = QR(I, 2, 6)
            TRC$(109, 0) = B$(I)
            TRC$(109, 1) = VERSUS$(I)
            TRC$(109, 2) = B$(1 - I)
            TRC$(109, 3) = Date$
        17 End If

        ' STEALS
        If V8(I, 9) > TREC!(112, 0) Then
            TREC!(112, 0) = V8(I, 9)
            TREC!(112, 1) = 0
            TREC!(112, 2) = 0
            TRC$(112, 0) = B$(I)
            TRC$(112, 1) = VERSUS$(I)
            TRC$(112, 2) = B$(1 - I)
            TRC$(112, 3) = Date$
        20 End If

        If V8(I, 9) + V8(1 - I, 9) > TREC!(114, 0) Then
            TREC!(114, 0) = V8(I, 9) + V8(1 - I, 9)
            TREC!(114, 1) = V8(I, 9)
            TREC!(114, 2) = V8(1 - I, 9)
            TRC$(114, 0) = B$(I)
            TRC$(114, 1) = VERSUS$(I)
            TRC$(114, 2) = B$(1 - I)
            TRC$(114, 3) = Date$
        21 End If

        If QR(I, 1, 9) > TREC!(116, 0) Then
            TREC!(116, 0) = QR(I, 1, 9)
            TREC!(116, 1) = 1
            TREC!(116, 2) = 0
            TRC$(116, 0) = B$(I)
            TRC$(116, 1) = VERSUS$(I)
            TRC$(116, 2) = B$(1 - I)
            TRC$(116, 3) = Date$
        End If

        If QR(I, 2, 9) > TREC!(116, 0) Then
            TREC!(116, 0) = QR(I, 2, 9)
            TREC!(116, 1) = 2
            TREC!(116, 2) = 0
            TRC$(116, 0) = B$(I)
            TRC$(116, 1) = VERSUS$(I)
            TRC$(116, 2) = B$(1 - I)
            TRC$(116, 3) = Date$
        23 End If

        ' BLOCKS
        If V8(I, 8) > TREC!(118, 0) Then
            TREC!(118, 0) = V8(I, 8)
            TREC!(118, 1) = 0
            TREC!(118, 2) = 0
            TRC$(118, 0) = B$(I)
            TRC$(118, 1) = VERSUS$(I)
            TRC$(118, 2) = B$(1 - I)
            TRC$(118, 3) = Date$
        25 End If

        If V8(I, 8) + V8(1 - I, 8) > TREC!(119, 0) Then
            TREC!(119, 0) = V8(I, 8) + V8(1 - I, 8)
            TREC!(119, 1) = V8(I, 8)
            TREC!(119, 2) = V8(1 - I, 8)
            TRC$(119, 0) = B$(I)
            TRC$(119, 1) = VERSUS$(I)
            TRC$(119, 2) = B$(1 - I)
            TRC$(119, 3) = Date$
        26 End If

        If QR(I, 1, 8) > TREC!(121, 0) Then
            TREC!(121, 0) = QR(I, 1, 8)
            TREC!(121, 1) = 1
            TREC!(121, 2) = 0
            TRC$(121, 0) = B$(I)
            TRC$(121, 1) = VERSUS$(I)
            TRC$(121, 2) = B$(1 - I)
            TRC$(121, 3) = Date$
        22 End If

        If QR(I, 2, 8) > TREC!(121, 0) Then
            TREC!(121, 0) = QR(I, 2, 8)
            TREC!(121, 1) = 1
            TREC!(121, 2) = 0
            TRC$(121, 0) = B$(I)
            TRC$(121, 1) = VERSUS$(I)
            TRC$(121, 2) = B$(1 - I)
            TRC$(121, 3) = Date$
        End If

        ' TURNOVERS
        If V8(I, 12) > TREC!(122, 0) Then
            TREC!(122, 0) = V8(I, 12)
            TREC!(122, 1) = 0
            TREC!(122, 2) = 0
            TRC$(122, 0) = B$(I)
            TRC$(122, 1) = VERSUS$(I)
            TRC$(122, 2) = B$(1 - I)
            TRC$(122, 3) = Date$
        29 End If

        If V8(I, 12) < TREC!(123, 0) Then
            TREC!(123, 0) = V8(I, 12)
            TREC!(123, 1) = 0
            TREC!(123, 2) = 0
            TRC$(123, 0) = B$(I)
            TRC$(123, 1) = VERSUS$(I)
            TRC$(123, 2) = B$(1 - I)
            TRC$(123, 3) = Date$
        30 End If

        If V8(I, 12) + V8(1 - I, 12) > TREC!(124, 0) Then
            TREC!(124, 0) = V8(I, 12) + V8(1 - I, 12)
            TREC!(124, 1) = V8(I, 12)
            TREC!(124, 2) = V8(1 - I, 12)
            TRC$(124, 0) = B$(I)
            TRC$(124, 1) = VERSUS$(I)
            TRC$(124, 2) = B$(1 - I)
            TRC$(124, 3) = Date$
        31 End If

        If V8(I, 12) + V8(1 - I, 12) < TREC!(125, 0) Then
            TREC!(125, 0) = V8(I, 12) + V8(1 - I, 12)
            TREC!(125, 1) = V8(I, 12)
            TREC!(125, 2) = V8(1 - I, 12)
            TRC$(125, 0) = B$(I)
            TRC$(125, 1) = VERSUS$(I)
            TRC$(125, 2) = B$(1 - I)
            TRC$(125, 3) = Date$
        32 End If

    Next

    Open diskPaths$(1) + "RECORDS.GAM" For Output As #1

    For I = 0 To 48
        For I1 = 0 To 2
            Write #1, REC!(I, I1)
        Next
        For I1 = 0 To 4
            Write #1, RC$(I, I1)
        Next
    Next
    For I = 0 To 125
        For I1 = 0 To 2
            Write #1, TREC!(I, I1)
        Next
        For I1 = 0 To 3
            Write #1, TRC$(I, I1)
        Next
    Next

    Close 1

End Sub

