DefInt A-Z

Declare Sub BoxTicker ()
Declare Sub ResetShotClock ()
Declare Sub ShotClock ()
Declare Sub HalfCourt ()
Declare Sub WHOLAY ()
Declare Sub FoulCalled (randomCalcs%)
Declare Sub GoodBasket ()
Declare Sub DISABLEPLAYER ()
Declare Sub DRAWBOX ()
Declare Sub GETATEAM (I1, YN$)
Declare Sub HORN ()
Declare Sub MAKEPBP ()
Declare Sub PBP ()
Declare Sub SHOTSTRING ()
Declare Sub TEAMSETUP ()
Declare Sub TICKER ()
Declare Sub TickerTime
Declare Sub BOX2FILE ()
Declare Sub BOX2PRINTER ()
Declare Sub BOX2SPC (NM$)
Declare Sub STAMINASORT ()
Declare Sub REBOUNDSORT ()
Declare Sub COMCOACH ()
Declare Sub TOURNEY ()
Declare Sub UpdateTourneyFile (tourneyFile$)
Declare Sub STATCONVERT (Q, QQ)
Declare Sub TICKERCHECK ()
Declare Sub MAINSCREEN ()
Declare Sub SCOREBOARD ()
Declare Sub COLCNG ()
Declare Sub LOADSTATS ()
Declare Sub OFFREBOUND ()
Declare Sub DEFREBOUND ()
Declare Sub EDITCX ()
Declare Sub DELAY ()
Declare Sub CLEARPBP ()
Declare Sub ZONECHANGE ()
Declare Sub DEFENSEMENU ()
Declare Sub SETSTAMINA ()
Declare Sub FRAME ()
Declare Sub COMPUTERSUB ()
Declare Sub FATIGUE ()
Declare Sub AUTOSETUP ()
Declare Sub LINEUPS ()
Declare Sub OFFENSE ()
Declare Sub FOULSAFE ()
Declare Sub FOULHARD ()
Declare Sub STEALADJ ()
Declare Sub ALPHANUM (I$, J)
Declare Sub BOXSCREEN ()
Declare Sub SHOTPERCENT ()
Declare Sub THREEFT ()
Declare Sub MADEFT ()
Declare Sub MISSFT ()
Declare Sub BADFT ()
Declare Sub PRELOAD ()
Declare Sub TEAMSELECT ()
Declare Sub SELECTID ()
Declare Sub PRDFAT ()
Declare Sub PRPFAT ()
Declare Sub WHOGETSBALL ()
Declare Sub BALLDISTRIBUTE ()
Declare Sub PlayingTime ()
Declare Sub CLOCKOVER ()
Declare Sub BLANKLINE ()
Declare Sub CLEARPBPBOX ()
Declare Sub DEADBALLOPTIONS ()
Declare Sub LASTFIVEMENU ()
Declare Sub RESETFATIGUE ()
Declare Sub WINDEX ()
Declare Sub BOXTOP ()
Declare Sub STOPPAGE ()
Declare Sub LASTSHOT ()
Declare Sub ZeroCheck ()
Declare Sub SELECTPBP ()
Declare Sub PRESSGUARD ()
Declare Sub SAVESTATS ()
Declare Sub EVENTS (COUNT, SPECIAL)
Declare Sub QUARTERTOTAL ()
Declare Sub AICOMCOACH ()
Declare Sub RECORDSCHECK ()

Dim Shared AP, B, C1, C7, DY, CT, D, FO, F3, G9, H, I, IN, J, JJ
Dim Shared M5, MJ, N, NTMS, P, P9, Q, S2, S9, TMT, TOU, U5, XM, XS

Dim Shared B1(1, 4), C(1, 13, 6), D1(1), D2(1), D8(4, 3), DF(1, 13), DP(2, 4)
Dim Shared F1(13), G4(13), G5(13), P2(1), P4(14), P5(14), P7(1)
Dim Shared QQ(1, 8, 13, 14), QR(1, 7, 14), R3!(1), RB(9), RS(1, 13, 13)
Dim Shared S!(1, 9), S1(1), S3(13), T0(1), T1(1), V9(14)
Dim Shared W0!(1, 13), W1!(1, 13), W2(1, 13), W3(1, 13), Z5(1), Z6(1), ZZ(1, 13)

Dim Shared BO%, BU%, CM%, DK%, EG%, F3S%, FB%, FT%, HT%, J8%, LC%, N7%, NF%, NL%
Dim Shared PA%, PASS%, PB%, PT%, PZ%, ST%, TB%, TF%, TS%, X3%, X0%, X1%, XX%

Dim Shared AP%(2), APT%(100, 1), B%(1, 13, 18), CZ%(1), DT%(1), E%(13), F5%(1, 8), FA%(1, 13), FY%(2)
Dim Shared G9%(1), HF%(1, 6), HT%(100), L%(1, 4), N%(16, 16, 3), NG%(18), NG1%(18)
Dim Shared O%(100), OF%(1), PR%(1, 1), ST%(32), SX%(32, 1, 14)
Dim Shared T2%(1, 20), TF%(1), TM%(1, 13), TP%(1), TR%(1, 9), teamIndex%(500), V1%(1), V2%(1)
Dim Shared vg%(8), vh%(8), W%(1, 13, 1)

Dim Shared A1$, B1$, C1$, D1$, E1$, F1$, FF$, G1$, H1$, J$, PB$, U$, VT$, VT1$, W$, W1$, YN$

Dim Shared C7$(2) 'C7$(3) would allow for the 30-second shot clock option
Dim Shared A1$(1), AA$(1, 13), B$(1), C$(1), CM$(1), D$(15), DB$(14)
Dim Shared H$(100), HO$(100), LC$(2), N$(16, 16, 3), O$(9), OB$(9), diskPaths$(1)
Dim Shared PB$(1), PO$(1, 13), PS$(4), Q$(250), R$(14), S$(1), SITE$(100), SX$(32, 2)
Dim Shared TB$(1), TS$(1), U5$(3), teamNames$(500), X$(3), Y$(1), YN$(5), YN1$(3)

Dim Shared A!(1, 13, 24)
Dim Shared Z!(15), Z1!(15), Z2!(13, 13)
Dim Shared F!, pbpDelay!, R0!, T!, TE!

'CRD stores attendance for stat files
'I think this is because the files are
'operated on linearly, so 100 records
'is the most supported?
Dim Shared avgAttendance&(1), CRD&(100)

'Y3 = Actual Attendance
Dim Shared fileLength&, actualAttendance&
