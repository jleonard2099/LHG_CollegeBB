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

Dim gameSite$(NUM_STATRECORDS), SITERP$(NUM_STATRECORDS)
Dim HO$(NUM_STATRECORDS)
Dim HRP$(NUM_STATRECORDS), ORP$(NUM_STATRECORDS)
Dim statH$(NUM_STATRECORDS), statO$(NUM_STATRECORDS)

Dim statO%(NUM_STATRECORDS)


' *** Schedule Data ***
' -------------------------
Dim homeTeam$(MAX_GAMES), visitingTeam$(MAX_GAMES)
Dim homeScores(MAX_GAMES), visitorScores(MAX_GAMES)

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
Dim Ycurr%, Yroad%

Dim alignAR$(15), AN$(15)

Dim APRD%(NUM_STATRECORDS, 1), ARD(14, 14)
Dim CRD(NUM_STATRECORDS), CRDRD(NUM_STATRECORDS)
Dim mergeA%(14, 14), mergeAP%(NUM_STATRECORDS, 1)
Dim mergeO%(NUM_STATRECORDS), mergeT%(NUM_STATRECORDS)
Dim ORD%(NUM_STATRECORDS), TRD%(NUM_STATRECORDS)

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
Dim HP%(NUM_STATRECORDS), OP%(NUM_STATRECORDS)

Dim X1!(13, 6)

Dim H1$(40), HP$(NUM_STATRECORDS), OP$(NUM_STATRECORDS)


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
Dim scheduleNG%(MAX_GAMES, 18) 'number of Games

Dim scheduleH$(1 To 20), scheduleV$(1 To 20)
Dim scheduleYN$(MAX_GAMES, 1)

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

'CRD stores attendance for stat files
'  I think this is because files are operated on
'   linearly, so 100 records is the most supported?
Dim avgAttendance&(1), CRD&(NUM_STATRECORDS)

Dim Shared autoPlay, ballCarrier, C1, currHalf, DY, compTeam, D, endGame, endAllGames, FO, F3
Dim Shared G9, gameLoc, halfTime, JY, IN, M5, MJ, nbrLines
Dim Shared P, P9, playerMode, playerOpt, S2, S9, sClockVal, shotClock
Dim Shared tickerIdx, TMT, TOU, XM, XS

Dim Shared BO%, BU%, coachOpt, DK%, F3S%, FB%, FT%, J8%, LC%, ft6FloorFouls, foulsToDQ
Dim Shared PA%, PASS%, pbpOpt, PT%, PZ%, ST%, ft10thFoul, ft3PtFoul, threePtOpt, X0%, X1%, XX%

Dim Shared F!

Dim Shared gameClock!, pbpDelay!, timeElapsed!

Dim Shared A1$, B1$, C1$, D1$, E1$, F1$, G1$, H1$, prevBall$
Dim Shared pbpString$, schedVisTm$, schedHomeTm$, YN$

Dim Shared APT%(NUM_STATRECORDS, 1)
Dim Shared HT%(NUM_STATRECORDS)

Dim Shared CZ%(1), F5%(0 To 1, 0 To 8), FY%(0 To 1)
Dim Shared G9%(1), HF%(1, 6), NG%(18), NG1%(18)
Dim Shared offStrat(1), PC%(1), PR%(1, 1)
Dim Shared rosterStatus(1, 13), ST%(32), SX%(32, 1, 14)
Dim Shared T2%(1, 20), TM%(1, 13), TP%(1)
Dim Shared VG%(8), VH%(8), YR%(1)

Dim Shared B1(0 To 1, 0 To 4)
Dim Shared dFGPA(1), def3FG_Adj(1), defStrat(1), defType(1), D8(4, 3), DP(2, 4), DF(1, 13)
Dim Shared eventSettings(13), ft3PtFoul(1), F1(13), G4(13), G5(13)
Dim Shared leagRat_GAME(1, 4), plyrRat_GAME(1, 13, 6), plyrStamina(13)
Dim Shared P2(1), P4(14), P5(14), P7(1)
Dim Shared QQ(1, 8, 13, 14), QR(1, 7, 14), RB(9), RS(1, 13, 13)
Dim Shared schedGame(2), score(1, 9), scSettings(0 To 3), statTotals(14)
Dim Shared teamRat_GAME(1, 9), teamStamina(1), threeFG(1, 13, 1)
Dim Shared timeouts(1), timePlayed(1, 13), tmFatigue(1, 13), turnovers(1)
Dim Shared W2(1, 13), W3(1, 13), Z5(1), Z6(1)

Dim Shared gameR3!(1)
Dim Shared plyrOff_GAME!(0 To 1, 0 To 13, 0 To 24)
Dim gameW0!(1, 13), gameW1!(1, 13)

Dim Shared defStyles$(15), defStyles_brief$(14)
Dim Shared gameArena$(0 To 1), gameCoaches$(0 To 1), gameMascots$(0 To 1), gameTeams$(0 To 1)
Dim Shared N$(1 To 16, 1 To 16, 0 To 4), offStyles$(9), offStyles_brief$(9)
Dim Shared pbpType$(1), players$(1, 13), PO$(1, 13), PS$(4)
Dim Shared SITE$(NUM_STATRECORDS), SX$(32, 2)
Dim Shared YN$(5)
