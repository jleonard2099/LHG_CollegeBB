$Debug

Const GAME_VERSION$ = "4.43"
Const MAX_SCHEDULE_GAMES = 6000
Const MAX_TEAMS = 1000
Const MAX_FILES = 10240
'optimium = 1048576 (but slows code)
Const MAX_CONFERENCES = 40
Const TEAMS_PER_DIV = 15
Const DIVS_PER_CONFERENCE = 2
Const TEAMS_PER_CONFERENCE = 30

Const MAX_SCHED_STATS = 100
Const SCHEDULE_SIZE_BYTES = 48

DefInt A-Z

' General purpose
'$INCLUDE: 'KeyInput.bi'
'$INCLUDE: 'QPProEqu.bi'
'$INCLUDE: 'PadRight.bi'
'$INCLUDE: 'QBWRITE.bi'

' For Utils
'$INCLUDE: 'BoxRoutines.bi'
'$INCLUDE: 'FileTeamSelection.bi'
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
Dim statsA!(15, 14)
Dim statsW0!(15), statsW1!(15)
Dim statsZ!(15), statsZ1!(15)
Dim statsZ2!(13, 13)

Dim gameAttendance&(MAX_SCHED_STATS), oppScore(MAX_SCHED_STATS)
Dim powerRating(MAX_SCHED_STATS, 1), teamScore(MAX_SCHED_STATS)

Dim gameSite$(MAX_SCHED_STATS), locIndicator$(MAX_SCHED_STATS), oppName$(MAX_SCHED_STATS)

'-- For Road Data
Dim APRD%(MAX_SCHED_STATS, 1)
Dim ORD%(MAX_SCHED_STATS), TRD%(MAX_SCHED_STATS)

Dim gameAttRoad&(MAX_SCHED_STATS)

Dim HRP$(MAX_SCHED_STATS), ORP$(MAX_SCHED_STATS), SITERP$(MAX_SCHED_STATS)


' *** Schedule Data ***
' -------------------------
Dim homeTeam$(MAX_SCHEDULE_GAMES), visitingTeam$(MAX_SCHEDULE_GAMES)
Dim homeScores(MAX_SCHEDULE_GAMES), visitorScores(MAX_SCHEDULE_GAMES)

' *** Game Options ***
' -------------------------
Dim Shared DT$, TM$

Dim coachMode$(0 To 1), foulShotOpt$(0 To 2)
Dim location$(0 To 2), modeAbbrev$(0 To 3)
Dim playMode$(0 To 3), sclockOpt$(0 To 3)
Dim teamIndicator$(1)
Dim yesNo$(0 To 1)

' *** Miscellaneous Use ***
' -------------------------
'Dim doPostLeaders
Dim replayLosses%, replayWins%

'----------------------------------------
' Used in ALIGN / MERGE routines
'----------------------------------------
Dim nbrGamesCurr, nbrGamesRoad

Dim alignAR$(15), AN$(15)

Dim ARD(14, 14), mergeA%(14, 14)

Dim AN!(15, 14), AR!(15, 14)
Dim W0N!(15), WN1!(15)
Dim W0R!(15), WR1!(15)
Dim W0RD!(15), W1RD!(15)
Dim ZR!(15), ZR1!(15)
Dim ZRD!(15), ZRD1!(15)

'----------------------------------------
' Used in CAREER / LEADER routines
'----------------------------------------
Dim ARS!(15, 62, 15)
Dim W0S!(15, 62), W1S!(15, 62)

Dim AL$(600), expanTT$(40, 15)
Dim TMA$(600), TP$(31), TP1$(39), TT1$(40, 15)

'-- does the "R" refer to "Replay"?
'-- for instance, A$ is player Names; AR$ is player names in the replay stats?
Dim careerAR$(62)
Dim TMM$(600), TPP$(600)

Dim GM!(1 To 40), GM1!(1 To 40), GMA!(600)
Dim leadersAL!(1 To 600, 0 To 14)
Dim TT!(40, 15), TT1!(40, 15)
Dim TYP!(600)
Dim W0L!(600), W1L!(600)


'----------------------------------------
' Used in COMPARE routines
'----------------------------------------
Dim H1$(40)


'----------------------------------------
' Used in COMPILE routines
'----------------------------------------
Dim confWins, confLosses
Dim fullWins, fullLosses

Dim defTotals!(21), offTotals!(21)
Dim leaderVals!(1 To 250, 0 To 2), natLeaderVals!(1 To 250, 1 To 2)
Dim natDefLeaders!(1 To 40, 1 To 20), natOffLeaders!(1 To 40, 1 To 20)
Dim compS!(0 To 14, 0 To 26)

Dim categories$(25)
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
Dim BRC!(32), TRC!(1 To 21), TRC1!(1 To 21)
Dim indRec!(50, 2), teamRec!(0 To 125, 0 To 2)

Dim BRC$(32, 1), recconTRC$(1 To 21), recconTRC1$(1 To 21)
Dim indRecDesc$(50, 4), teamRecDesc$(0 To 125, 0 To 3)

Dim recconTB$(25), recconTT$(20)


'----------------------------------------
' Used in STAT / INPUT routines
'----------------------------------------
Dim HL%, HW%, NL%, NW%, VL%, VW%
Dim losses, wins

Dim values!(0 To 14, 24)


'----------------------------------------
' Used in SEExxx routines
'----------------------------------------
Dim DL$(60), OL$(60), NOL$(1 To 60), PR$(1200)
Dim seeT$(30)

Dim defLeaders!(60, 20), offLeaders!(60, 20)
Dim O1!(30), O2!(30), O3!(30), O4!(30), O5!(30), O6!(30)

Dim seeZ!(260), seeZ1!(260)
Dim seeZ1$(260), seeZ2$(260), seeZ3$(260)

Dim PT#(1200, 5)


'----------------------------------------
' Used in SCHEDULE routines
'----------------------------------------
Dim BS%, NS%

Dim scheduleAP%(1), scheduleZ1%(1 To 30)
Dim scheduleNG%(MAX_SCHEDULE_GAMES, 18) 'number of Games

Dim scheduleH$(1 To 20), scheduleV$(1 To 20)
Dim scheduleYN$(MAX_SCHEDULE_GAMES, 1)

Dim Z1$(1 To 30), Z2$(1 To 30)


'----------------------------------------
' Used in TINPUT routine
'----------------------------------------
Dim nbrGmsPlayed, oppAsst, opp3FGA, opp3FGAPct, oppFGPct
Dim oppFouls, oppFTA, oppFTPct, oppReb, oppSteal, oppTO
Dim tmFouls, tmFGA, oppFGA, tmFGPct, tmFTA
Dim tmFTPct, tmReb, tmAsst, tmSteal, tmTO

'----------------------------------------
' Used in ADDTOURN / BRACKET routines
'----------------------------------------
Dim TC!

'           REGION, SEED NUMBER, TEAM#/MODE OF PLAY (0,1)
Dim tourneyN%(1 To 17, 1 To 17, 0 To 3)

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
Dim scheduleFile$, tourneyFile$
Dim actualAttendance&

Dim alpha$(3), tickerPeriod$(14), teamYrTourn$(0 To 3)

Dim tourneySettings(1 To 16, 1 To 16, 0 To 4)

Dim avgAttendance&(1)

Dim Shared autoPlay, ballCarrier, C1, currHalf
Dim Shared DY, compTeam, D, endGame, endAllGames
'Dim Shared fouler, desigFouler
Dim Shared FO, F3, G9, gameLoc, halfTime
Dim Shared JY, IN, M5, MJ, nbrLines
Dim Shared P, P9, playerMode, playerOpt
Dim Shared S2, S9, sClockVal, shotClock
Dim Shared tickerIdx, TMT, TOU, XM, XS

Dim Shared BO%, BU%, coachOpt, DK%
Dim Shared ft10thFoul, ft3PtFoul, ft6FloorFouls, foulsToDQ
Dim Shared F3S%, FB%, FT%, J8%, LC%
Dim Shared PA%, PASS%, pbpOpt, PT%, PZ%, ST%
Dim Shared threePtOpt, X0%, X1%, XX%

Dim Shared F!

Dim Shared gameClock!, pbpDelay!, timeElapsed!

Dim Shared A1$, B1$, C1$, D1$, E1$, F1$, G1$, H1$
Dim Shared prevBall$, pbpString$, schedVisTm$, schedHomeTm$, YN$

Dim Shared CZ%(1), F5%(0 To 1, 0 To 8), FY%(0 To 1)
Dim Shared G9%(1), HF%(1, 6), NG%(18), NG1%(18)
Dim Shared offStrat(1), PC%(1), PR%(1, 1)
Dim Shared rosterStatus(1, 13), ST%(32), SX%(32, 1, 14)
Dim Shared teamStats_GAME(1, 20), TM%(1, 13), TP%(1)
Dim Shared pbpFG(8), pbpBG(8)

Dim Shared B1(0 To 1, 0 To 4)
Dim Shared dFGPA(1), def3FG_Adj(1), defStrat(1), defType(1)
Dim Shared D8(4, 3), DP(2, 4), playerDef_GAME(1, 13)
Dim Shared eventSettings(13), ft3PtFoul(1), F1(13)
Dim Shared G4(13), G5(13), leagRat_GAME(1, 4)
Dim Shared plyrRat_GAME(1, 13, 6), plyrStamina(13)
Dim Shared P2(1), P4(14), P5(14), P7(1)
Dim Shared QQ(1, 8, 13, 14), QR(1, 7, 14), RB(9), playerStat_GAME(1, 13, 13)
Dim Shared schedGame(2), score(1, 9), scSettings(0 To 3), statTotals(14)
Dim Shared teamRat_GAME(1, 9), teamStamina(1), threeFG(1, 13, 1)
Dim Shared timeouts(1), timePlayed(1, 13), tmFatigue(1, 13), turnovers(1)
Dim Shared W2(1, 13), W3(1, 13), Z5(1), Z6(1)

Dim Shared gameR3!(1)
Dim Shared plyrOff_GAME!(0 To 1, 0 To 13, 0 To 24)
Dim gameW0!(1, 13), gameW1!(1, 13)

Dim Shared defStyles$(15), defStyles_brief$(14), diskIDs$(5)
Dim Shared gameArena$(0 To 1), gameCoaches$(0 To 1), gameMascots$(0 To 1), gameTeams$(0 To 1)
Dim Shared offStyles$(9), offStyles_brief$(9)
Dim Shared pbpType$(1), players$(1, 13)
Dim Shared positions_GAME$(1, 13), posnName$(0 To 4)
Dim Shared seedSlotName$(1 To 16, 1 To 16, 0 To 4), SX$(32, 2)


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
For I = 0 To 25: Read categories$(I): Next

Data GAMES PLAYED,FIELD GOALS ATTEMPTED,FIELD GOALS MADE,FIELD GOAL %,3-FIELD GOALS ATTEMPTED,3-FIELD GOALS MADE,3-FIELD GOAL %
Data FREE THROWS ATTEMPTED,FREE THROWS MADE,FREE THROW %,MINUTES PLAYED,POINTS,SCORING AVERAGE,HI-GAME,OFFENSIVE REBOUNDS
Data DEFENSIVE REBOUNDS,TOTAL REBOUNDS,REBOUNDS PER GAME,PERSONAL FOULS,STEALS,ASSISTS,BLOCKS,TURNOVERS,MINUTES PER GAME,MVP VOTING
For I = 1 To 25: Read recconTB$(I): Next

Data FIELD GOALS ATTEMPTED,FIELD GOALS MADE,FIELD GOAL %,3-FIELD GOALS ATTEMPTED,3-FIELD GOALS MADE,3-FIELD GOAL %
Data FREE THROWS ATTEMPTED,FREE THROWS MADE,FREE THROW %,OFFENSIVE REBOUNDS,DEFENSIVE REBOUNDS,TOTAL REBOUNDS,REBOUNDS PER GAME
Data PERSONAL FOULS,STEALS,ASSISTS,BLOCKS,TURNOVERS,POINTS,POINTS PER GAME
For I = 1 To 20: Read recconTT$(I): Next


Data L,1,1,1,1,1,1,H,H,2,2,2,2,2,F
For I = 0 To 14: Read tickerPeriod$(I): Next

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
For I = 0 To 9:
    Read offStyles$(I)
Next

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
    Read defStyles$(I)
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
    Read offStyles_brief$(I)
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

sclockOpt$(0) = "NO SHOT CLOCK       "
sclockOpt$(1) = "45 SECOND SHOT CLOCK"
sclockOpt$(2) = "35 SECOND SHOT CLOCK"
sclockOpt$(3) = "30 SECOND SHOT CLOCK"

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

If Not _FileExists("DEV.BAS") Then

    Open "DEV.BAS" For Output As #1

    For I = 0 To 1
        Select Case opSysType$
            Case "[WINDOWS]": Print #1, _CWD$ + "\"
            Case "[MAC]", "[MACOSX]", "[LINUX]": Print #1, _CWD$ + "/"
        End Select

    Next

    Close #1

End If

Open "DEV.BAS" For Input As #2

For I = 0 To 1
    Input #2, diskPaths$(I)

Next

Close #2

Selection% = 1

Do

    Call Main_Menu(Selection%)

Loop Until Selection% = 8

End

System


'----------------------------------------

Errhandler:
Open "errlog" For Append As #10
Print #10, "Error #"; Err; "on program file line"; _ErrorLine
If _FileExists("GAME82") Then
    Print #10, "Schedule game # "; Using "###"; schedGame(0) + schedGame(2)
    Print #10, gameTeams$(0) + " vs. " + gameTeams$(1)
End If
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

        menuChoices$(1) = "PLAY SINGLE GAME                "
        menuChoices$(2) = "TEAM MANAGEMENT / ORGANIZATION  "
        menuChoices$(3) = "REPLAY STATISTICS AND REPORTS   "
        menuChoices$(4) = "LEADERS / STANDINGS / RECORDS   "
        menuChoices$(5) = "SCHEDULER / AUTOPLAY / BOXSCORES"
        menuChoices$(6) = "TOURNAMENT PLAY                 "
        'menuChoices$(7) = "HARD DRIVE INSTALLATION         "
        menuChoices$(7) = "CONFIGURE DRIVES FOR TEAMS,STATS"
        menuChoices$(8) = "EXIT TO DOS                     "

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

        menuChoices$(1) = "INPUT A NEW TEAM            "
        menuChoices$(2) = "EDIT AN EXISTING TEAM       "
        menuChoices$(3) = "PRINT TEAM ROSTER / REPORT  "
        menuChoices$(4) = "VIEW TEAM ROSTER / REPORT   "
        menuChoices$(5) = "DRAFT TO NEW / EXISTING TEAM"
        menuChoices$(6) = "RETURN TO MAIN MENU         "
        Selection% = 1
        Count% = 6
        Start% = 1
        ExitCode% = 6
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
                Case 1: Call TINPUT
                Case 2: Call CNGTM(1)
                Case 3: Call CNGTM(2)
                Case 4: Call CNGTM(3)
                Case 5: Call DRAFT
            End Select

        Else
            Selection% = 6
        End If

    Loop Until Selection% = 6 Or ExitCode% = 27

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

        menuChoices$(1) = "VIEW / PRINT COMPILED STATS       "
        menuChoices$(2) = "ENCYCLOPEDIA / TEAM REPLAY OPTIONS"
        menuChoices$(3) = "COMPARE REPLAY STATS TO ACTUAL    "
        'menuChoices$(4) = "MODIFY REPLAY STATS               "
        menuChoices$(4) = "MERGE SEPARATE STATISTICAL FILES  "
        menuChoices$(5) = "RETURN TO MAIN MENU               "
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

        menuChoices$(1) = "CREATE / EDIT A CONFERENCE"
        menuChoices$(2) = "UPDATE STATS / STANDINGS  "
        menuChoices$(3) = "VIEW / PRINT REPORTS      "
        menuChoices$(4) = "HEAD-TO-HEAD STATS        "
        menuChoices$(5) = "RECORD BOOKS              "
        menuChoices$(6) = "EXPANDED LEADERS          "
        menuChoices$(7) = "DELETE CONFERENCE STATS   "
        menuChoices$(8) = "CREATE / EDIT ORGANIZATION"
        menuChoices$(9) = "UPDATE POLL / STATS       "
        menuChoices$(10) = "VIEW / PRINT REPORTS      "
        menuChoices$(11) = "RECORD BOOKS              "
        menuChoices$(12) = "DELETE ORGANIZATION STATS "
        menuChoices$(13) = "RETURN TO COURTSIDE MENU  "
        menuChoices$(14) = "EXIT TO DOS               "
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


' General purpose
'$INCLUDE: 'KeyInput.bm'
'$INCLUDE: 'QPProEqu.bm'
'$INCLUDE: 'PadRight.bm'
'$INCLUDE: 'QBWRITE.bm'

' For Utils
'$INCLUDE: 'BoxRoutines.bm'
'$INCLUDE: 'FileTeamSelection.bm'
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
'$INCLUDE: 'COLHOOP.BAS'
'$INCLUDE: 'GameRoutines.bm'

