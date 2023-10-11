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
Dim location$(0 To 2), modeAbbrev$(0 To 3)
Dim playMode$(0 To 3), sclockOpt$(0 To 2)
Dim teamIndicator$(1)
Dim threeFtOpt$(0 To 1), threePtOpt$(0 To 1), yesNo$(0 To 1)

' *** Miscellaneous Use ***
' -------------------------
Dim finalUpdate%
Dim replayLosses%, replayWins%

'----------------------------------------
' Used in ALIGN / MERGE routines
'----------------------------------------
Dim Ycurr%, Yroad%

Dim alignAR$(15), AN$(15)
Dim mergeH$(NUM_STATRECORDS), HRP$(NUM_STATRECORDS)
Dim mergeO$(NUM_STATRECORDS), ORP$(NUM_STATRECORDS), gameSite$(NUM_STATRECORDS), SITERP$(NUM_STATRECORDS)

Dim APRD%(NUM_STATRECORDS, 1), ARD(14, 14), CRD(NUM_STATRECORDS), CRDRD(NUM_STATRECORDS)
Dim mergeA(14, 14), mergeAP%(NUM_STATRECORDS, 1)
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
Dim HP%(NUM_STATRECORDS), compareleagRat_GAME(6), OP%(NUM_STATRECORDS)

Dim compareA!(15, 14)
Dim A1!(13, 6), X1!(13, 6)

Dim compareB$(14), H1$(40), HP$(NUM_STATRECORDS), OP$(NUM_STATRECORDS)


'----------------------------------------
' Used in COMPILE routines
'----------------------------------------
Dim confWins, confLosses
Dim fullWins, fullLosses

Dim DT!(21), OT!(21)
Dim LD!(1 To 250, 0 To 2)
Dim NDL!(1 To 40, 1 To 20), NLD!(1 To 250, 1 To 2), NOL!(1 To 40, 1 To 20)
Dim compS!(0 To 14, 0 To 26)


Dim compH$(NUM_STATRECORDS), compN$(0 To 14)
Dim CP$(25), compO$(NUM_STATRECORDS)
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
Dim R1!(50), R2!(50), R3_HD2HD!(50), R4!(50)
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
Dim actualAttendance&

'CRD stores attendance for stat files
'  I think this is because files are operated on
'   linearly, so 100 records is the most supported?
Dim avgAttendance&(1), CRD&(NUM_STATRECORDS)

Dim Shared autoPlay, B, C1, DY, compTeam, D, endGame, endAllGames, FO, F3
Dim Shared G9, gameLoc, H, halfTime, I, J, IN, M5, MJ, nbrLines, NTMS
Dim Shared P, P9, playerMode, playerOpt, Q, S2, S9, sClockVal, shotClock
Dim Shared tickerIdx, TMT, TOU, XM, XS

Dim Shared BO%, BU%, coachOpt, DK%, F3S%, FB%, FT%, J8%, LC%, ft6FloorFouls, foulsToDQ
Dim Shared PA%, PASS%, pbpOpt, PT%, PZ%, ST%, ft10thFoul, ft3PtFoul, threePtOpt, X0%, X1%, XX%

Dim Shared F!

Dim Shared gameClock!, pbpDelay!, timeElapsed!

Dim Shared A1$, B1$, C1$, D1$, E1$, F1$, G1$, H1$, J$, prevBall$
Dim Shared tourneyFile$, U$, VT$, VT1$, W$, W1$, YN$

'-- FA%() I believe is related to tracking player fatigue
Dim Shared APT%(NUM_STATRECORDS, 1), B%(1, 13), CZ%(1), DT%(1), eventSettings(13), FY%(2)
Dim Shared G9%(1), HF%(1, 6), HT%(NUM_STATRECORDS), N%(16, 16, 0 To 4), NG%(18), NG1%(18)
Dim Shared O%(NUM_STATRECORDS), OF%(1), PC%(1), PR%(1, 1)
Dim Shared ST%(32), SX%(32, 1, 14)
Dim Shared T2%(1, 20), TM%(1, 13), TP%(1)
Dim Shared VG%(8), VH%(8), threeFG(1, 13, 1)

Dim Shared B1(0 To 1, 0 To 4)
Dim Shared dFGPA(1), def3FG_Adj(1), D2(1), D8(4, 3), DP(2, 4), DF(1, 13)
Dim Shared ft3PtFoul(1), F1(13), G4(13), G5(13)
Dim Shared leagRat_GAME(1, 4), plyrRat_GAME(1, 13, 6), plyrStamina(13)
Dim Shared P2(1), P4(14), P5(14), P7(1)
Dim Shared QQ(1, 8, 13, 14), QR(1, 7, 14), RB(9), RS(1, 13, 13)
Dim Shared schedGame(2), score(1, 9), statTotals(14)
Dim Shared teamRat_GAME(1, 9), teamStamina(1), timeouts(1), timePlayed(1, 13), tmFatigue(1, 13), turnovers(1)
Dim Shared W2(1, 13), W3(1, 13), Z5(1), Z6(1)

Dim Shared F5%(0 To 1, 0 To 8)

Dim Shared plyrOff_GAME!(0 To 1, 0 To 13, 0 To 24)
Dim Shared R3!(1), W0!(1, 13), W1!(1, 13)

Dim Shared players$(1, 13), D$(15), DB$(14)
Dim Shared gameArena$(0 To 1), gameCoaches$(0 To 1), gameMascots$(0 To 1), gameTeams$(0 To 1)
Dim Shared H$(NUM_STATRECORDS), HO$(NUM_STATRECORDS), LC$(2), N$(16, 16, 0 To 4), O$(9), OB$(9)
Dim Shared pbpType$(1), PS$(4), R$(14), SITE$(NUM_STATRECORDS), SX$(32, 2)
Dim Shared PO$(1, 13)
Dim Shared X$(3), Y$(1), YN$(5), YN1$(3)

Dim Shared Z!(15), Z1!(15), Z2!(13, 13)
