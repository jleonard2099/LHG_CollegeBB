'----------------------------------------
' Used in more than one source file
'----------------------------------------
Dim Shared findFile$

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
Dim gameAtt_Road&(MAX_SCHED_STATS)
Dim oppScore_Road(MAX_SCHED_STATS), powerRating_Road(MAX_SCHED_STATS, 1)
Dim teamScore_Road(MAX_SCHED_STATS)

Dim gameAttRoad&(MAX_SCHED_STATS)

Dim gameSite_Road$(MAX_SCHED_STATS), locIndicator_Road$(MAX_SCHED_STATS), oppName_Road$(MAX_SCHED_STATS)


' *** Schedule Data ***
' -------------------------
Dim homeTeam$(MAX_SCHEDULE_GAMES), visitingTeam$(MAX_SCHEDULE_GAMES)
Dim homeScores(MAX_SCHEDULE_GAMES), visitorScores(MAX_SCHEDULE_GAMES)

' *** Game Options ***
' -------------------------
Dim Shared DT$, TM$

Dim coachMode$(0 To 1), foulShotOpt$(0 To 2)
Dim location$(0 To 2), modeAbbrev$(0 To 3)
Dim playMode$(0 To 3), sClockOpt$(0 To 3)
Dim teamIndicator$(1)
Dim yesNo$(0 To 1)

' *** Miscellaneous Use ***
' -------------------------
'Dim doPostLeaders
Dim replayLosses, replayWins

'----------------------------------------
' Used in ALIGN / MERGE routines
'----------------------------------------
Dim nbrGamesCurr, nbrGamesRoad

Dim roadPlyrNames$(15), AN$(15)

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
Dim BLYR(21), BL(21)
Dim W0S!(15, 62), W1S!(15, 62)


Dim AL$(600), plyLeaderTeams$(MAX_CONFERENCES, 15)
Dim BL$(21)
Dim TMA$(600), expIndCategory$(31), expTeamCategory$(39), tmLeaderTeams$(MAX_CONFERENCES, 15)

'-- does the "R" refer to "Replay"?
'-- for instance, A$ is player Names; AR$ is player names in the replay stats?
Dim careerAR$(62)
Dim TMM$(600), TPP$(600)

Dim plyrLeaderYears!(MAX_CONFERENCES), tmLeaderYears!(MAX_CONFERENCES), GMA!(600)
Dim plyrStatLeaders!(0 To 600, 0 To 14)
Dim TT!(MAX_CONFERENCES, 15), TT1!(MAX_CONFERENCES, 15)
Dim TYP!(600)
Dim W0L!(600), W1L!(600)


'----------------------------------------
' Used in COMPARE routines
'----------------------------------------
Dim statFiles$(TEAMS_PER_CONFERENCE)

'----------------------------------------
' Used in COMPILE routines
'----------------------------------------
Dim confWins, confLosses
Dim fullWins, fullLosses

Dim defTotals!(21), offTotals!(21)
Dim leaderVals!(1 To 250, 0 To 2), natLeaderVals!(1 To 250, 1 To 2)
Dim natDefLeaders!(MAX_CONFERENCES, 1 To 20), natOffLeaders!(MAX_CONFERENCES, 1 To 20)
Dim compS!(0 To 14, 0 To 26)

Dim statCategoryAbbr$(25)
Dim natDefLdrNames$(40), natZ1$(1 To 250), NZ1$(30), natZ2$(1 To 250)
Dim PT$(1 To 1200), Z0$(1 To 250), Z3$(1 To 250)


'----------------------------------------
' Used in CREATE routines
'----------------------------------------
Dim memberConf$(MAX_CONFERENCES)


'----------------------------------------
' Used in DRAFT routines
'----------------------------------------
Dim CK!(1)

Dim dFGPctAdj_DRAFT!(1)

Dim lgRat_DRAFT(1, 4), avgRat_DRAFT(1, 13, 12)
Dim teamRat_DRAFT(1, 9)

Dim plyrDef_DRAFT!(1, 13), plyrOff_DRAFT!(1, 13, 6)
Dim plyrRat_DRAFT!(1, 13, 6), plyrStat_DRAFT!(1, 13, 13)
Dim stamina_DRAFT!(1), teamStats_DRAFT!(1, 24)

Dim att_DRAFT&(1)

Dim coaches_DRAFT$(1), mascots_DRAFT$(1), stadiums_DRAFT$(1)
Dim plyrNames_DRAFT$(0 To 1, 0 To 13), posn_DRAFT$(1, 13)
Dim teamYears$(1), tmName_DRAFT$(1)


'----------------------------------------
'   Used in Head-To-Head routines
'----------------------------------------
Dim awayLosses!(50), awayWins!(50)
Dim homeLosses!(50), homeWins!(50)
Dim homeScoreTeam!(50), homeScoreOpp!(50)
Dim awayScoreTeam!(50), awayScoreOpp!(50)
Dim totAwayWin!(MAX_CONFERENCES), totHomeWins!(MAX_CONFERENCES)
Dim totHomeLosses!(MAX_CONFERENCES), totAwayLosses!(MAX_CONFERENCES)
Dim totHomeScoreTeam!(MAX_CONFERENCES), totHomeScoreOpp!(MAX_CONFERENCES)
Dim totAwayScoreTeam!(MAX_CONFERENCES), totAwayScoreOpp!(MAX_CONFERENCES)

Dim div1Name$, div2Name$


'----------------------------------------
' Used in RECORDS routines
'----------------------------------------
Dim plyrRecords!(32), offRecords!(1 To 21), defRecords!(1 To 21)
Dim plyrRecDesc$(32, 1), offRecDesc$(1 To 21), defRecDesc$(1 To 21)

Dim indRecords!(50, 2), teamRecords!(125, 2)
Dim indRecordDesc$(25), teamRecordDesc$(20)
Dim indRecDesc$(50, 4), teamRecDesc$(125, 3)


'----------------------------------------
' Used in STAT / INPUT routines
'----------------------------------------
Dim homeLosses, homeWins, neutralLosses, neutralWins
Dim roadLosses, roadWins, totalLosses, totalWins

Dim values!(0 To 14, 24)


'----------------------------------------
' Used in SEExxx routines
'----------------------------------------
Dim defLeaderNames$(60), offLeaderNames$(60)
Dim natOffLdrNames$(1 To 60), PR$(1200)
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

Dim scheduleAP%(1)
Dim scheduleNG%(1000, 18) 'number of Games

Dim scheduleH$(1 To 20), scheduleV$(1 To 20)
Dim scheduleYN$(1000, 1)

Dim memberIdx(TEAMS_PER_CONFERENCE)
Dim memberName$(TEAMS_PER_CONFERENCE)
Dim memberYear$(TEAMS_PER_CONFERENCE)


'----------------------------------------
' Used in InputTeam routine
'----------------------------------------
Dim nbrGmsPlayed!, opp3FGA!, opp3FGAPct!, oppAsst!, oppBlocks!
Dim oppFGPct!, oppFouls!, oppFGA!, oppFTA!, oppFTPct!
Dim oppReb!, oppSteal!, oppTO!
Dim tm3FGA!, tm3FGAPct!, tmAsst!, tmBlocks!, tmFouls!
Dim tmFGA!, tmFGPct!, tmFTA!, tmFTPct!
Dim tmReb!, tmSteal!, tmTO!


'----------------------------------------
' Used in ADDTOURN / BRACKET routines
'----------------------------------------
Dim TC!

'           REGION, SEED NUMBER, TEAM#/MODE OF PLAY (0,1)
Dim tourneyN%(1 To 17, 1 To 17, 0 To 3)

'Number of Teams Per Region (up to 16 Regions)
Dim nbrRegionalTeams(17)

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
Dim Shared whistle&, swish&, backboard&

Dim FY%(0 To 1)

Dim alpha$(4), tickerPeriod$(14), teamYrTourn$(0 To 3)

Dim eventSettings(13)
Dim tourneySettings(1 To 16, 1 To 16, 0 To 4)

Dim avgAttendance&(1)

'-- I doubt these are needed
Dim Shared Q, I, J, RN0

Dim Shared autoPlay, ballCarrier, C1, coachOpt, currHalf
Dim Shared compTeam, currGame, D, endGame, endAllGames
'Dim Shared desigFouler, desigdesigFouler
Dim Shared fastBreak, FO, ftNumber
Dim Shared G9, gameAvgTmStamina, gameLoc, guardIdx
Dim Shared halfTime
Dim Shared ft3PtFoul, ft6FloorFouls, ft10thFoul, foulsToDQ
Dim Shared JY, IN, M5, MJ, nbrLines
Dim Shared offStatus
Dim Shared P, P9, playerMode, playerOpt, pbpOpt, pbpType
Dim Shared rebFoulChance, S2, sClockVal, shotClock, shotType
Dim Shared threePtOpt, threePtShotChance
Dim Shared tickerIdx, TMT, TOU, willDunk, XM, XS

Dim Shared GS%, FT%, J8%, LC%
Dim Shared PA%, PASS, PT%, ST%
Dim Shared X0%, X1%, XX%

Dim Shared gameClock!, pbpDelay!
Dim Shared shotChance!, timeElapsed!

Dim Shared A1$, B1$, C1$, D1$, E1$, F1$, G1$, H1$
Dim Shared prevBall$, pbpString$, schedHomeTm$, schedVisTm$, YN$

Dim Shared CZ%(1), G9%(1), NG%(18), NG1%(18)
Dim Shared pressureRating(1, 1), ST%(32), SX%(32, 1, 14)
Dim Shared TM%(1, 13), TP%(1)

Dim Shared defAdjVsPass(2, 4), defAdjVsShot(4, 3)
Dim Shared dFGPA(1), def3FG_Adj(1), defStrat(1), defCategory(1), defCoverage(1)
Dim Shared ft3PtFoul(1), F1(13)
Dim Shared G4(13), G5(13), homeCtFactor(1, 6)
Dim Shared leagRat_GAME(1, 4), lineupIdx(0 To 1, 0 To 4), offStrat(1)
Dim Shared P4(14), P5(14)
Dim Shared pbpFG(8), pbpBG(8), pctContrib(1)
Dim Shared playerDef_GAME(0 To 1, 0 To 13), plyrOff_GAME!(0 To 1, 0 To 13, 0 To 24)
Dim Shared plyrRat_GAME(0 To 1, 0 To 13, 0 To 6), playerStat_GAME(0 To 1, 0 To 13, 0 To 15)
Dim Shared plyrStamina(13)
Dim Shared rosterStatus(1, 13), QQ(1, 8, 13, 14), QR(1, 7, 14), rebRatings(9)
Dim Shared schedGame(2), score(0 To 1, 0 To 10), scSettings(0 To 3), statTotals(14)
Dim Shared teamAdj(0 To 1, 0 To 8), teamRat_GAME(1, 9), teamRecWins(1), teamRecLoss(1)
Dim Shared teamStats_GAME(0 To 1, 0 To 24), teamFouls(1), teamStamina(1)
Dim Shared threeFG(1, 13, 1), timeouts(1), timePlayed(1, 13)
Dim Shared tmFatigue(1, 13), totRebounds(1), turnovers(1)
Dim Shared W2(1, 13), W3(1, 13)

'Dim Shared gameRatings!(0 To 1, 0 To 13, 0 To 25)
Dim gameW0!(1, 13), gameW1!(1, 13)

Dim Shared defStyles$(0 To 17), defStyles_brief$(0 To 17), diskIDs$(5)
Dim Shared gameArena$(0 To 1), gameCoaches$(0 To 1), gameMascots$(0 To 1), gameTeams$(0 To 1)
Dim Shared offStyles$(9), offStyles_brief$(9)
Dim Shared pbpStyle$(1), players$(1, 13)
Dim Shared positions_GAME$(0 To 1, 0 To 13), posnName$(0 To 4)
Dim Shared seedSlotName$(1 To 16, 1 To 16, 0 To 4), SX$(32, 2)
Dim Shared teamGender$(0 To 1)


