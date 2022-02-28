DEFINT A-Z
DECLARE SUB CHAMPIONSHIP ()
DECLARE SUB QPrint (X$, Colr, Page)
DECLARE FUNCTION Peek1% (Segment, Address)
DECLARE SUB SETTINGS ()
DECLARE SUB FCopy (Source$, Dest$, Buffer$, ErrCode%)
DECLARE SUB WRITEGAME ()
DIM NN%(16): REM NUMBER OF TEAMS IN A REGION, UP TO 16 REGIONS
DIM N$(16, 16, 3): REM TEAM NAMES FOR EACH SEED SLOT
DIM R$(16): REM REGIONAL NAMES
DIM D$(3)
DIM OP%(14): REM TOURNAMENT DEFAULTS
DIM Q$(300), VV$(500), V%(500), NG%(300, 17), H$(300), V$(300), HS%(300), VS%(300)
DIM YN$(300, 3)
DIM N%(16, 16, 3): REM: REGION, SEED NUMBER, TEAM#/MODE OF PLAY (0,1)
DIM C%(1): REM 0-CONSOLATION IN REGIONS, 1-CONSOLATION IN CHAMPIONSHIP
COMMON SHARED R, TC, NG%(), OP%(), V$(), H$(), YN$(), F$, HS%(), VS%(), Q$(), C%(), D$()

SUB CHAMPIONSHIP ()
 ON R GOTO 5105, 5100, 5200, 5300, 5400, 5500, 5600, 5700, 5800, 5900, 6000, 6100, 6200, 6300, 6400, 6500

5105 GOTO 5050

5100 REM:2 REGIONS
    TC = TC + 1
    REGION = 17
    GAME = 1
    ROUND = 99
    CALL SETTINGS
    NG%(TC, 0) = 901
    NG%(TC, 1) = 902
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 1 WINNER"
    H$(TC) = "REGION 2 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

      GOTO 5050

5200 REM:3 REGIONS

    TC = TC + 1
    REGION = 17
    GAME = 1
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 903
    NG%(TC, 1) = 902
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 3 WINNER"
    H$(TC) = "REGION 2 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 2
    ROUND = 99
    CALL SETTINGS
    NG%(TC, 0) = 1101
    NG%(TC, 1) = 901
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 1 WINNER"
    H$(TC) = "REGION 1 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
    GOTO 5050

5300 REM:4 REGIONS

    TC = TC + 1
    REGION = 17
    GAME = 1
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 901
    NG%(TC, 1) = 904
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 1 WINNER"
    H$(TC) = "REGION 4 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 2
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 902
    NG%(TC, 1) = 903
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 2 WINNER"
    H$(TC) = "REGION 3 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    IF C%(1) = 0 THEN 5305
    TC = TC + 1
    REGION = 17
    GAME = 3
    ROUND = 98
    CALL SETTINGS
    NG%(TC, 0) = 1001
    NG%(TC, 1) = 1002
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 1 LOSER"
    H$(TC) = "GAME 2 LOSER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 4
    ROUND = 99
    CALL SETTINGS
    NG%(TC, 0) = 1101
    NG%(TC, 1) = 1102
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 1 WINNER"
    H$(TC) = "GAME 2 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
    GOTO 5050

5305 TC = TC + 1
    REGION = 17
    GAME = 3
    ROUND = 99
    CALL SETTINGS
    NG%(TC, 0) = 1101
    NG%(TC, 1) = 1102
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 1 WINNER"
    H$(TC) = "GAME 2 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
    GOTO 5050


5400 REM:5 REGIONS

    TC = TC + 1
    REGION = 17
    GAME = 1
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 905
    NG%(TC, 1) = 904
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 5 WINNER"
    H$(TC) = "REGION 4 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 2
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 1101
    NG%(TC, 1) = 901
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 1 WINNER"
    H$(TC) = "REGION 1 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 3
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 902
    NG%(TC, 1) = 903
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 2 WINNER"
    H$(TC) = "REGION 3 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    IF C%(1) = 0 THEN 5405
    TC = TC + 1
    REGION = 17
    GAME = 4
    ROUND = 98
    CALL SETTINGS
    NG%(TC, 0) = 1003
    NG%(TC, 1) = 1002
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 3 LOSER"
    H$(TC) = "GAME 2 LOSER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 5
    ROUND = 99
    CALL SETTINGS
    NG%(TC, 0) = 1103
    NG%(TC, 1) = 1102
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 3 WINNER"
    H$(TC) = "GAME 2 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
    GOTO 5050

5405 TC = TC + 1
    REGION = 17
    GAME = 4
    ROUND = 99
    CALL SETTINGS
    NG%(TC, 0) = 1103
    NG%(TC, 1) = 1102
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 3 WINNER"
    H$(TC) = "GAME 2 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
    GOTO 5050

5500 REM:6 REGIONS

    TC = TC + 1
    REGION = 17
    GAME = 1
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 905
    NG%(TC, 1) = 904
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 5 WINNER"
    H$(TC) = "REGION 4 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 2
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 903
    NG%(TC, 1) = 906
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 3 WINNER"
    H$(TC) = "REGION 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 3
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 1101
    NG%(TC, 1) = 901
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 1 WINNER"
    H$(TC) = "REGION 1 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 4
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 1102
    NG%(TC, 1) = 902
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 2 WINNER"
    H$(TC) = "REGION 2 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    IF C%(1) = 0 THEN 5505
    TC = TC + 1
    REGION = 17
    GAME = 5
    ROUND = 98
    CALL SETTINGS
    NG%(TC, 0) = 1003
    NG%(TC, 1) = 1004
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 3 LOSER"
    H$(TC) = "GAME 4 LOSER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 6
    ROUND = 99
    CALL SETTINGS
    NG%(TC, 0) = 1103
    NG%(TC, 1) = 1104
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 3 WINNER"
    H$(TC) = "GAME 4 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
    GOTO 5050

5505 TC = TC + 1
    REGION = 17
    GAME = 5
    ROUND = 99
    CALL SETTINGS
    NG%(TC, 0) = 1103
    NG%(TC, 1) = 1104
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 3 WINNER"
    H$(TC) = "GAME 4 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
    GOTO 5050

5600 REM:7 REGIONS

    TC = TC + 1
    REGION = 17
    GAME = 1
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 903
    NG%(TC, 1) = 906
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 3 WINNER"
    H$(TC) = "REGION 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 2
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 902
    NG%(TC, 1) = 907
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 2 WINNER"
    H$(TC) = "REGION 7 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 3
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 904
    NG%(TC, 1) = 905
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 4 WINNER"
    H$(TC) = "REGION 5 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 4
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 1102
    NG%(TC, 1) = 1101
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 2 WINNER"
    H$(TC) = "GAME 1 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 5
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 1103
    NG%(TC, 1) = 901
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 3 WINNER"
    H$(TC) = "REGION 1 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    IF C%(1) = 0 THEN 5605
    TC = TC + 1
    REGION = 17
    GAME = 6
    ROUND = 98
    CALL SETTINGS
    NG%(TC, 0) = 1005
    NG%(TC, 1) = 1004
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 5 LOSER"
    H$(TC) = "GAME 4 LOSER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 7
    ROUND = 99
    CALL SETTINGS
    NG%(TC, 0) = 1105
    NG%(TC, 1) = 1104
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 5 WINNER"
    H$(TC) = "GAME 4 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
    GOTO 5050

5605 TC = TC + 1
    REGION = 17
    GAME = 6
    ROUND = 99
    CALL SETTINGS
    NG%(TC, 0) = 1105
    NG%(TC, 1) = 1104
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 5 WINNER"
    H$(TC) = "GAME 4 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
    GOTO 5050

5700 REM:8 REGIONS

    TC = TC + 1
    REGION = 17
    GAME = 1
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 902
    NG%(TC, 1) = 907
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 2 WINNER"
    H$(TC) = "REGION 7 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 2
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 903
    NG%(TC, 1) = 906
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 3 WINNER"
    H$(TC) = "REGION 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 3
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 904
    NG%(TC, 1) = 905
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 4 WINNER"
    H$(TC) = "REGION 5 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 4
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 901
    NG%(TC, 1) = 908
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 1 WINNER"
    H$(TC) = "REGION 8 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 5
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 1101
    NG%(TC, 1) = 1102
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 1 WINNER"
    H$(TC) = "GAME 2 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 6
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 1103
    NG%(TC, 1) = 1104
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 3 WINNER"
    H$(TC) = "GAME 4 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    IF C%(1) = 0 THEN 5705
    TC = TC + 1
    REGION = 17
    GAME = 7
    ROUND = 98
    CALL SETTINGS
    NG%(TC, 0) = 1005
    NG%(TC, 1) = 1006
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 5 LOSER"
    H$(TC) = "GAME 6 LOSER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 8
    ROUND = 99
    CALL SETTINGS
    NG%(TC, 0) = 1105
    NG%(TC, 1) = 1106
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 5 WINNER"
    H$(TC) = "GAME 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
    GOTO 5050

5705 TC = TC + 1
    REGION = 17
    GAME = 7
    ROUND = 99
    CALL SETTINGS
    NG%(TC, 0) = 1105
    NG%(TC, 1) = 1106
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 5 WINNER"
    H$(TC) = "GAME 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
    GOTO 5050

5800 REM:9 REGIONS

    TC = TC + 1
    REGION = 17
    GAME = 1
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 909
    NG%(TC, 1) = 908
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 9 WINNER"
    H$(TC) = "REGION 8 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 2
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 902
    NG%(TC, 1) = 907
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 2 WINNER"
    H$(TC) = "REGION 7 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 3
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 903
    NG%(TC, 1) = 906
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 3 WINNER"
    H$(TC) = "REGION 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 4
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 904
    NG%(TC, 1) = 905
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 4 WINNER"
    H$(TC) = "REGION 5 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 5
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 901
    NG%(TC, 1) = 1101
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 1 WINNER"
    H$(TC) = "GAME 1 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 6
    ROUND = 3
    CALL SETTINGS
    NG%(TC, 0) = 1102
    NG%(TC, 1) = 1103
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 2 WINNER"
    H$(TC) = "GAME 3 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 7
    ROUND = 3
    CALL SETTINGS
    NG%(TC, 0) = 1104
    NG%(TC, 1) = 1105
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 4 WINNER"
    H$(TC) = "GAME 5 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    IF C%(1) = 0 THEN 5805
    TC = TC + 1
    REGION = 17
    GAME = 8
    ROUND = 98
    CALL SETTINGS
    NG%(TC, 0) = 1007
    NG%(TC, 1) = 1006
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 7 LOSER"
    H$(TC) = "GAME 6 LOSER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 9
    ROUND = 99
    CALL SETTINGS
    NG%(TC, 0) = 1107
    NG%(TC, 1) = 1106
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 7 WINNER"
    H$(TC) = "GAME 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
    GOTO 5050

5805 TC = TC + 1
    REGION = 17
    GAME = 8
    ROUND = 99
    CALL SETTINGS
    NG%(TC, 0) = 1107
    NG%(TC, 1) = 1106
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 7 WINNER"
    H$(TC) = "GAME 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
    GOTO 5050

5900 REM:10 REGIONS

    TC = TC + 1
    REGION = 17
    GAME = 1
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 910
    NG%(TC, 1) = 907
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 10 WINNER"
    H$(TC) = "REGION 7 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 2
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 908
    NG%(TC, 1) = 909
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 8 WINNER"
    H$(TC) = "REGION 9 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 3
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 1101
    NG%(TC, 1) = 902
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 1 WINNER"
    H$(TC) = "REGION 2 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 4
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 903
    NG%(TC, 1) = 906
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 3 WINNER"
    H$(TC) = "REGION 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 5
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 904
    NG%(TC, 1) = 905
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 4 WINNER"
    H$(TC) = "REGION 5 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 6
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 901
    NG%(TC, 1) = 1102
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 1 WINNER"
    H$(TC) = "GAME 2 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 7
    ROUND = 3
    CALL SETTINGS
    NG%(TC, 0) = 1103
    NG%(TC, 1) = 1104
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 3 WINNER"
    H$(TC) = "GAME 4 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 8
    ROUND = 3
    CALL SETTINGS
    NG%(TC, 0) = 1105
    NG%(TC, 1) = 1106
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 5 WINNER"
    H$(TC) = "GAME 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    IF C%(1) = 0 THEN 5905
    TC = TC + 1
    REGION = 17
    GAME = 9
    ROUND = 98
    CALL SETTINGS
    NG%(TC, 0) = 1007
    NG%(TC, 1) = 1008
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 7 LOSER"
    H$(TC) = "GAME 8 LOSER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 10
    ROUND = 99
    CALL SETTINGS
    NG%(TC, 0) = 1107
    NG%(TC, 1) = 1108
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 7 WINNER"
    H$(TC) = "GAME 8 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
    GOTO 5050

5905 TC = TC + 1
    REGION = 17
    GAME = 9
    ROUND = 99
    CALL SETTINGS
    NG%(TC, 0) = 1107
    NG%(TC, 1) = 1108
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 7 WINNER"
    H$(TC) = "GAME 8 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
    GOTO 5050

6000 REM:11 REGIONS

    TC = TC + 1
    REGION = 17
    GAME = 1
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 910
    NG%(TC, 1) = 907
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 10 WINNER"
    H$(TC) = "REGION 7 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 2
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 911
    NG%(TC, 1) = 906
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 11 WINNER"
    H$(TC) = "REGION 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 3
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 909
    NG%(TC, 1) = 908
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 9 WINNER"
    H$(TC) = "REGION 8 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 4
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 1101
    NG%(TC, 1) = 902
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 1 WINNER"
    H$(TC) = "REGION 2 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 5
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 903
    NG%(TC, 1) = 1102
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 3 WINNER"
    H$(TC) = "GAME 2 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 6
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 904
    NG%(TC, 1) = 905
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 4 WINNER"
    H$(TC) = "REGION 5 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 7
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 901
    NG%(TC, 1) = 1103
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 1 WINNER"
    H$(TC) = "GAME 3 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 8
    ROUND = 3
    CALL SETTINGS
    NG%(TC, 0) = 1104
    NG%(TC, 1) = 1105
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 4 WINNER"
    H$(TC) = "GAME 5 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 9
    ROUND = 3
    CALL SETTINGS
    NG%(TC, 0) = 1106
    NG%(TC, 1) = 1107
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 6 WINNER"
    H$(TC) = "GAME 7 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    IF C%(1) = 0 THEN 6005
    TC = TC + 1
    REGION = 17
    GAME = 10
    ROUND = 98
    CALL SETTINGS
    NG%(TC, 0) = 1009
    NG%(TC, 1) = 1008
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 9 LOSER"
    H$(TC) = "GAME 8 LOSER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 11
    ROUND = 99
    CALL SETTINGS
    NG%(TC, 0) = 1109
    NG%(TC, 1) = 1108
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 9 WINNER"
    H$(TC) = "GAME 8 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
    GOTO 5050

6005 TC = TC + 1
    REGION = 17
    GAME = 10
    ROUND = 99
    CALL SETTINGS
    NG%(TC, 0) = 1109
    NG%(TC, 1) = 1108
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 7 WINNER"
    H$(TC) = "GAME 8 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
    GOTO 5050

6100 REM:12 REGIONS

    TC = TC + 1
    REGION = 17
    GAME = 1
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 910
    NG%(TC, 1) = 907
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 10 WINNER"
    H$(TC) = "REGION 7 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 2
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 911
    NG%(TC, 1) = 906
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 11 WINNER"
    H$(TC) = "REGION 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 3
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 912
    NG%(TC, 1) = 905
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 12 WINNER"
    H$(TC) = "REGION 5 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 4
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 909
    NG%(TC, 1) = 908
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 9 WINNER"
    H$(TC) = "REGION 8 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 5
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 902
    NG%(TC, 1) = 1101
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 2 WINNER"
    H$(TC) = "GAME 1 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 6
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 1102
    NG%(TC, 1) = 903
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 2 WINNER"
    H$(TC) = "REGION 3 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 7
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 904
    NG%(TC, 1) = 1103
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 4 WINNER"
    H$(TC) = "GAME 3 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 8
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 1104
    NG%(TC, 1) = 901
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 4 WINNER"
    H$(TC) = "REGION 1 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 9
    ROUND = 3
    CALL SETTINGS
    NG%(TC, 0) = 1106
    NG%(TC, 1) = 1105
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 6 WINNER"
    H$(TC) = "GAME 5 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 10
    ROUND = 3
    CALL SETTINGS
    NG%(TC, 0) = 1107
    NG%(TC, 1) = 1108
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 7 WINNER"
    H$(TC) = "GAME 8 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    IF C%(1) = 0 THEN 6105
    TC = TC + 1
    REGION = 17
    GAME = 11
    ROUND = 98
    CALL SETTINGS
    NG%(TC, 0) = 1009
    NG%(TC, 1) = 1010
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 9 LOSER"
    H$(TC) = "GAME 10 LOSER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 12
    ROUND = 99
    CALL SETTINGS
    NG%(TC, 0) = 1109
    NG%(TC, 1) = 1110
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 9 WINNER"
    H$(TC) = "GAME 10 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
    GOTO 5050

6105 TC = TC + 1
    REGION = 17
    GAME = 11
    ROUND = 99
    CALL SETTINGS
    NG%(TC, 0) = 1109
    NG%(TC, 1) = 1110
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 9 WINNER"
    H$(TC) = "GAME 10 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
    GOTO 5050

6200 REM:13 REGIONS

    TC = TC + 1
    REGION = 17
    GAME = 1
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 913
    NG%(TC, 1) = 904
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 13 WINNER"
    H$(TC) = "REGION 4 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 2
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 912
    NG%(TC, 1) = 905
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 12 WINNER"
    H$(TC) = "REGION 5 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 3
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 909
    NG%(TC, 1) = 908
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 9 WINNER"
    H$(TC) = "REGION 8 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 4
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 910
    NG%(TC, 1) = 907
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 10 WINNER"
    H$(TC) = "REGION 7 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 5
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 906
    NG%(TC, 1) = 911
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 6 WINNER"
    H$(TC) = "REGION 11 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 6
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 1101
    NG%(TC, 1) = 1102
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 1 WINNER"
    H$(TC) = "GAME 2 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 7
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 901
    NG%(TC, 1) = 1103
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 1 WINNER"
    H$(TC) = "GAME 3 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 8
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 1104
    NG%(TC, 1) = 902
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 4 WINNER"
    H$(TC) = "REGION 2 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 9
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 903
    NG%(TC, 1) = 1105
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 3 WINNER"
    H$(TC) = "GAME 5 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 10
    ROUND = 3
    CALL SETTINGS
    NG%(TC, 0) = 1107
    NG%(TC, 1) = 1106
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 7 WINNER"
    H$(TC) = "GAME 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 11
    ROUND = 3
    CALL SETTINGS
    NG%(TC, 0) = 1103
    NG%(TC, 1) = 1109
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 3 WINNER"
    H$(TC) = "GAME 9 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    IF C%(1) = 0 THEN 6205
    TC = TC + 1
    REGION = 17
    GAME = 12
    ROUND = 98
    CALL SETTINGS
    NG%(TC, 0) = 1011
    NG%(TC, 1) = 1010
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 11 LOSER"
    H$(TC) = "GAME 10 LOSER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 13
    ROUND = 99
    CALL SETTINGS
    NG%(TC, 0) = 1111
    NG%(TC, 1) = 1110
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 11 WINNER"
    H$(TC) = "GAME 10 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
    GOTO 5050

6205 TC = TC + 1
    REGION = 17
    GAME = 12
    ROUND = 99
    CALL SETTINGS
    NG%(TC, 0) = 1111
    NG%(TC, 1) = 1110
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 11 WINNER"
    H$(TC) = "GAME 10 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
    GOTO 5050

6300 REM:14 REGIONS

    TC = TC + 1
    REGION = 17
    GAME = 1
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 914
    NG%(TC, 1) = 903
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 14 WINNER"
    H$(TC) = "REGION 3 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 2
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 913
    NG%(TC, 1) = 904
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 13 WINNER"
    H$(TC) = "REGION 4 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 3
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 912
    NG%(TC, 1) = 905
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 12 WINNER"
    H$(TC) = "REGION 5 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 4
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 911
    NG%(TC, 1) = 906
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 11 WINNER"
    H$(TC) = "REGION 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 5
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 910
    NG%(TC, 1) = 907
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 10 WINNER"
    H$(TC) = "REGION 7 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 6
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 909
    NG%(TC, 1) = 909
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 9 WINNER"
    H$(TC) = "REGION 8 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 7
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 1104
    NG%(TC, 1) = 1101
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 4 WINNER"
    H$(TC) = "GAME 1 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 8
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 1105
    NG%(TC, 1) = 902
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 5 WINNER"
    H$(TC) = "REGION 2 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 9
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 1102
    NG%(TC, 1) = 1103
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 2 WINNER"
    H$(TC) = "GAME 3 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 10
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 1106
    NG%(TC, 1) = 901
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 6 WINNER"
    H$(TC) = "REGION 1 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 11
    ROUND = 3
    CALL SETTINGS
    NG%(TC, 0) = 1107
    NG%(TC, 1) = 1108
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 7 WINNER"
    H$(TC) = "GAME 8 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 12
    ROUND = 3
    CALL SETTINGS
    NG%(TC, 0) = 1109
    NG%(TC, 1) = 1110
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 9 WINNER"
    H$(TC) = "GAME 10 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    IF C%(1) = 0 THEN 6305
    TC = TC + 1
    REGION = 17
    GAME = 13
    ROUND = 98
    CALL SETTINGS
    NG%(TC, 0) = 1012
    NG%(TC, 1) = 1011
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 12 LOSER"
    H$(TC) = "GAME 11 LOSER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 14
    ROUND = 99
    CALL SETTINGS
    NG%(TC, 0) = 1112
    NG%(TC, 1) = 1111
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 12 WINNER"
    H$(TC) = "GAME 11 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
    GOTO 5050

6305 TC = TC + 1
    REGION = 17
    GAME = 13
    ROUND = 99
    CALL SETTINGS
    NG%(TC, 0) = 1111
    NG%(TC, 1) = 1112
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 11 WINNER"
    H$(TC) = "GAME 12 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
    GOTO 5050

6400 REM:15 REGIONS

    TC = TC + 1
    REGION = 17
    GAME = 1
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 915
    NG%(TC, 1) = 902
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 15 WINNER"
    H$(TC) = "REGION 2 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 2
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 914
    NG%(TC, 1) = 903
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 14 WINNER"
    H$(TC) = "REGION 3 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 3
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 913
    NG%(TC, 1) = 904
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 13 WINNER"
    H$(TC) = "REGION 4 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 4
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 912
    NG%(TC, 1) = 905
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 12 WINNER"
    H$(TC) = "REGION 5 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 5
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 911
    NG%(TC, 1) = 906
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 11 WINNER"
    H$(TC) = "REGION 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 6
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 910
    NG%(TC, 1) = 907
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 10 WINNER"
    H$(TC) = "REGION 7 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 7
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 909
    NG%(TC, 1) = 908
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 9 WINNER"
    H$(TC) = "REGION 8 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 8
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 1101
    NG%(TC, 1) = 1106
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 1 WINNER"
    H$(TC) = "GAME 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 9
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 1102
    NG%(TC, 1) = 1105
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 2 WINNER"
    H$(TC) = "GAME 5 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 10
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 1103
    NG%(TC, 1) = 1104
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 3 WINNER"
    H$(TC) = "GAME 4 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 11
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 1107
    NG%(TC, 1) = 901
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 7 WINNER"
    H$(TC) = "REGION 1 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 12
    ROUND = 3
    CALL SETTINGS
    NG%(TC, 0) = 1109
    NG%(TC, 1) = 1108
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 9 WINNER"
    H$(TC) = "GAME 8 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 13
    ROUND = 3
    CALL SETTINGS
    NG%(TC, 0) = 1110
    NG%(TC, 1) = 1111
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 10 WINNER"
    H$(TC) = "GAME 11 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    IF C%(1) = 0 THEN 6405
    TC = TC + 1
    REGION = 17
    GAME = 14
    ROUND = 98
    CALL SETTINGS
    NG%(TC, 0) = 1012
    NG%(TC, 1) = 1013
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 12 LOSER"
    H$(TC) = "GAME 13 LOSER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 15
    ROUND = 99
    CALL SETTINGS
    NG%(TC, 0) = 1112
    NG%(TC, 1) = 1113
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 12 WINNER"
    H$(TC) = "GAME 13 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
    GOTO 5050

6405 TC = TC + 1
    REGION = 17
    GAME = 14
    ROUND = 99
    CALL SETTINGS
    NG%(TC, 0) = 1112
    NG%(TC, 1) = 1113
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 12 WINNER"
    H$(TC) = "GAME 13 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
    GOTO 5050
6500 REM:16 REGIONS

    TC = TC + 1
    REGION = 17
    GAME = 1
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 916
    NG%(TC, 1) = 901
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 16 WINNER"
    H$(TC) = "REGION 1 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 2
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 915
    NG%(TC, 1) = 902
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 15 WINNER"
    H$(TC) = "REGION 2 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 3
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 914
    NG%(TC, 1) = 903
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 14 WINNER"
    H$(TC) = "REGION 3 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 4
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 913
    NG%(TC, 1) = 904
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 13 WINNER"
    H$(TC) = "REGION 4 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 5
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 912
    NG%(TC, 1) = 905
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 12 WINNER"
    H$(TC) = "REGION 5 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 6
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 911
    NG%(TC, 1) = 906
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 11 WINNER"
    H$(TC) = "REGION 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 7
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 910
    NG%(TC, 1) = 907
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 10 WINNER"
    H$(TC) = "REGION 7 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 8
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = 909
    NG%(TC, 1) = 908
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 9 WINNER"
    H$(TC) = "REGION 8 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 9
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 1101
    NG%(TC, 1) = 1108
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 1 WINNER"
    H$(TC) = "GAME 8 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 10
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 1104
    NG%(TC, 1) = 1105
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 4 WINNER"
    H$(TC) = "GAME 5 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 11
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 1103
    NG%(TC, 1) = 1106
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 3 WINNER"
    H$(TC) = "GAME 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 12
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 1102
    NG%(TC, 1) = 1107
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 2 WINNER"
    H$(TC) = "GAME 7 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 13
    ROUND = 3
    CALL SETTINGS
    NG%(TC, 0) = 1109
    NG%(TC, 1) = 1110
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 9 WINNER"
    H$(TC) = "GAME 10 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 14
    ROUND = 3
    CALL SETTINGS
    NG%(TC, 0) = 1111
    NG%(TC, 1) = 1112
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 11 WINNER"
    H$(TC) = "GAME 12 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    IF C%(1) = 0 THEN 6505
    TC = TC + 1
    REGION = 17
    GAME = 15
    ROUND = 98
    CALL SETTINGS
    NG%(TC, 0) = 1014
    NG%(TC, 1) = 1013
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 14 LOSER"
    H$(TC) = "GAME 13 LOSER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME

    TC = TC + 1
    REGION = 17
    GAME = 16
    ROUND = 99
    CALL SETTINGS
    NG%(TC, 0) = 1114
    NG%(TC, 1) = 1113
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 14 WINNER"
    H$(TC) = "GAME 13 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
    GOTO 5050

6505 TC = TC + 1
    REGION = 17
    GAME = 15
    ROUND = 99
    CALL SETTINGS
    NG%(TC, 0) = 1114
    NG%(TC, 1) = 1113
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 14 WINNER"
    H$(TC) = "GAME 13 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME


5050 END SUB

SUB SETTINGS ()
     FOR I = 0 TO 14
     NG%(TC, I) = OP%(I)
     NEXT
     NG%(TC, 8) = 3
     NG%(TC, 2) = 2
END SUB

SUB SORTGAMES ()

    FOR RD = 1 TO 89
    IF RD > 5 AND RD < 88 OR RD > 89 THEN 999
    LOCATE 22,25:PRINT "SCHEDULING GAMES FOR BRACKET"NR
    FOR NR = 1 TO 16
    IF NR > R THEN 998
    FOR AP% = 1 TO TC
    
    GOSUB 996

    IF NG%(0, 16) <> RD OR NG%(0, 15) <> NR THEN 997

    MATCH = MATCH + 1

    GOSUB 995

997 NEXT AP%
998 NEXT NR
999 NEXT RD
    NR = 17
    FOR RD = 1 TO 99
    IF RD > 5 AND RD < 88 OR RD > 89 AND RD < 98 THEN 993
    FOR AP% = 1 TO TC
    GOSUB 996

    IF NG%(0, 16) <> RD OR NG%(0, 15) <> NR THEN 991

    MATCH = MATCH + 1

    GOSUB 995

991 NEXT AP%
993 NEXT RD

    GOTO 994

996 OPEN D$(0) + F$ + ".TMP" FOR RANDOM AS #1 LEN = 90
    FOR X = 0 TO 14
    FIELD #1, X * 2 AS X$, 2 AS Q$(X + 1), 90 - 2 - 2 * X AS X$
    NEXT
    FIELD #1, 30 AS X$, 2 AS Q$(16), 2 AS Q$(17), 15 AS Q$(18), 15 AS Q$(19), 26 AS X$
    FIELD #1, 64 AS X$, 3 AS Q$(20), 3 AS Q$(21), 6 AS Q$(22), 8 AS Q$(23), 6 AS X$
    FIELD #1, 84 AS X$, 2 AS Q$(24), 2 AS Q$(25), 2 AS Q$(26)
    GET #1, AP%

    FOR X = 0 TO 14: NG%(0, X) = CVI(Q$(X + 1)): NEXT
    FOR X = 0 TO 3: YN$(0, X) = Q$(20 + X): NEXT
    VS%(0) = CVI(Q$(16)): HS%(0) = CVI(Q$(17))
    V$(0) = Q$(18): H$(0) = Q$(19)
    FOR X = 1 TO 3: NG%(0, X + 14) = CVI(Q$(X + 23)): NEXT
    CLOSE 1
    RETURN

995 OPEN D$(0) + F$ + ".TOU" FOR RANDOM AS #1 LEN = 90
    FOR X = 0 TO 14
    FIELD #1, X * 2 AS X$, 2 AS Q$(X + 1), 90 - 2 - 2 * X AS X$
    NEXT
    FIELD #1, 30 AS X$, 2 AS Q$(16), 2 AS Q$(17), 15 AS Q$(18), 15 AS Q$(19), 26 AS X$
    FIELD #1, 64 AS X$, 3 AS Q$(20), 3 AS Q$(21), 6 AS Q$(22), 8 AS Q$(23), 6 AS X$
    FIELD #1, 84 AS X$, 2 AS Q$(24), 2 AS Q$(25), 2 AS Q$(26)

    FOR X = 0 TO 14: LSET Q$(X + 1) = MKI$(NG%(0, X)): NEXT
    FOR X = 0 TO 3: LSET Q$(20 + X) = YN$(0, X): NEXT
    LSET Q$(16) = MKI$(VS%(0)): LSET Q$(17) = MKI$(HS%(0))
    LSET Q$(18) = V$(0): LSET Q$(19) = H$(0)
    FOR X = 24 TO 26: LSET Q$(X) = MKI$(NG%(0, X - 9)): NEXT
    PUT #1, MATCH
    CLOSE 1
    RETURN
994 KILL D$(0) + F$ + ".TMP"
    SHELL "COPY "+D$(0)+F$+".TOU "+D$(0)+F$+".TCX >NUL:"
    'CALL FCOPY (F$+".TOU", F$+".TCX", BUFFER$, ERRCODE%)
    END SUB

STATIC SUB TextIn (T$, Max, NumOnly, CapsOn, ExitCode, Colr)

TInitialize:

   Clr = 0                                      'determine monitor type
   IF Peek1%(0, &H463) <> &HB4 THEN Clr = 1

   X$ = T$                                      'work on a copy of the string
'  X$ = QPTrim$(T$)                             'use this if the incoming
                                                '  string is fixed-length
TC:

   ExitCode = 0: Insrt = 0: Curpo = 1           'initialize flags

   Length = LEN(X$)
   IF Length > Max THEN EXIT SUB                'already to big to edit

   X$ = X$ + SPACE$(Max - Length)               'pad with trailing spaces
   QPrint X$, Colr, -1
   LOCATE , , 1                                 'turn on the cursor

   GOSUB TInsertOff                     'set cursor size according to display



TGetKey:


   'disallow insert if cursor past end
   IF Curpo > Length AND Insrt <> 0 THEN GOSUB TInsertOff

   IF Curpo > Max GOTO TEnter           'field is filled, handle as Enter key

   A$ = INKEY$
   IF A$ = "" GOTO TGetKey
   IF LEN(A$) = 1 GOTO TRegularKey

   A$ = RIGHT$(A$, 1)                   'it was an extended key, get the code

   ON INSTR(CHR$(15) + ".GHKMOPRSstu" + CHR$(19), A$) GOTO TShiftTab, TClear, THome, TUp, TLeft, TRight, TEndKey, TDown, TIns, TDel, TCtrlLeft, TCtrlRight, TCtrlEnd, TRestore

   GOTO TGetKey                         'none of the above, get again



TShiftTab:

   ExitCode = 1                         'user wants to go back a field
   GOTO TEnter                          'handle as if it were the Enter key



TClear:

   X$ = ""                              'Alt-C, erase the current string
   GOSUB TInsertOff                     'clear insert mode and restore cursor
   LOCATE , POS(0) - (Curpo - 1)
   GOTO TC                              'and start all over again



THome:

   LOCATE , POS(0) - (Curpo - 1)        'put cursor at beginning of line
   Curpo = 1                            'show cursor as being on 1st character
   GOTO TGetKey



TUp:

   ExitCode = 1                         'user wants to go back a field
   GOTO TEnter                          'handle as if it were the Enter key



TLeft:

   IF Curpo = 1 GOTO TShiftTab          'cursor is on the first character,
                                        'handle as if it were a Shift-Tab
   Curpo = Curpo - 1                    'update Curpo
   LOCATE , POS(0) - 1                  'back up the cursor
   GOTO TGetKey



TRight:

   Curpo = Curpo + 1                    'update Curpo
   LOCATE , POS(0) + 1                  'advance the cursor on the screen
   GOTO TGetKey



TEndKey:

   LOCATE , POS(0) + (Length - Curpo) + 1 'put cursor at the end of the line
   Curpo = Length + 1                   'update Curpo
   GOTO TGetKey



TDown:

   GOTO TEnter



TIns:

   IF Insrt THEN                        'insert is already on, turn it off
      GOSUB TInsertOff
      GOTO TGetKey
   END IF

   IF Curpo > Length GOTO TGetKey       'ignore Ins if cursor is past the end
   IF Length = Max GOTO TGetKey         'also ignore if field is full

   Insrt = 1                            'set the insert flag
   IF Clr THEN                          'set cursor size according to display
      LOCATE , , , 0, 7
   ELSE
      LOCATE , , , 0, 13
   END IF

   GOTO TGetKey



TDel:

   IF Curpo > Length GOTO TGetKey       'ignore Del if cursor is past end

   'slide all characters left one position, add a trailing space and re-print
   MID$(X$, Curpo) = MID$(X$, Curpo + 1) + " "
   QPrint MID$(X$, Curpo), Colr, -1

   Length = Length - 1                  'show string as one character shorter
   GOTO TGetKey



TCtrlLeft:

   IF Curpo = 1 GOTO TGetKey            'at the beginning, ignore

   A = Curpo                            'save cursor position

   'we're within a word, find beginning
   IF MID$(X$, Curpo - 1, 1) <> " " GOTO TSeekLeft2

TSeekLeft1:

   IF Curpo = 1 GOTO TCtrlLeftExit      'at the beginning, give up

   IF MID$(X$, Curpo - 1, 1) = " " THEN
      Curpo = Curpo - 1
      GOTO TSeekLeft1                   'seek previous non-blank character
   END IF

TSeekLeft2:

   IF Curpo = 1 GOTO TCtrlLeftExit      'at the beginning, give up
   IF MID$(X$, Curpo - 1, 1) <> " " THEN
      Curpo = Curpo - 1
      GOTO TSeekLeft2                   'seek character preceeded by a blank
   END IF

TCtrlLeftExit:

   LOCATE , POS(0) - (A - Curpo)        'position the cursor
   GOTO TGetKey



TCtrlRight:

   A = Curpo                            'save cursor position

TSeekRight1:

   IF A > Length GOTO TGetKey           'at the end, give up

   IF MID$(X$, A, 1) <> " " THEN
      A = A + 1                         'consider next character
      GOTO TSeekRight1                  'seek next blank space
   END IF

TSeekRight2:

   IF A > Length GOTO TGetKey           'at the end, give up

   IF MID$(X$, A, 1) = " " THEN
      A = A + 1                         'consider next character
      GOTO TSeekRight2                  'seek next non-blank character
   END IF

   LOCATE , POS(0) + (A - Curpo)        'position the cursor

   Curpo = A                            'show cursor as being on the next word
   GOTO TGetKey                         'get another keypress




TCtrlEnd:

   IF Curpo > Length GOTO TGetKey       'cursor is past the end, ignore

   QPrint SPACE$(Length - Curpo + 1), Colr, -1  'blank from cursor to the end
   MID$(X$, Curpo) = SPACE$(Length - Curpo + 1) 'clear that part of the string
   Length = Curpo - 1                   'show the length being at the cursor
   GOTO TGetKey                         'get another keypress



TRestore:

   LOCATE , POS(0) - (Curpo - 1)        'locate cursor at beginning of line,
   GOTO TInitialize                     '  and start all over again



TRegularKey:

   IF A$ < " " THEN                     'a control key
      ON INSTR(CHR$(8) + CHR$(9) + CHR$(13) + CHR$(27), A$) GOTO TBackspace, TTabKey, TEnter, TEscape
      GOTO TGetKey                      'none of the above
   END IF

   IF CapsOn THEN                       'convert to upper case if requested
      IF A$ >= "a" AND A$ <= "z" THEN A$ = CHR$(ASC(A$) AND 95)
   END IF

   IF NumOnly THEN                      'disallow non-numeric if requested
      IF A$ < "0" OR A$ > "9" THEN
         PLAY "L16O3EC"
         GOTO TGetKey
      END IF
   END IF

   QPrint A$, Colr, -1                  'print character
   LOCATE , POS(0) + 1
   Curpo = Curpo + 1                    'show cursor being ahead

   IF Insrt GOTO THandleInsert

   MID$(X$, Curpo - 1, 1) = A$          'assign the character

   'cursor is past end, increase length
   IF Curpo > Length + 1 THEN Length = Curpo - 1

   'field complete, handle as Enter key
   IF Length = Max AND Curpo > Length GOTO TEnter

   GOTO TGetKey



THandleInsert:

   Length = Length + 1                  'show string being 1 character longer
   Temp$ = MID$(X$, Curpo - 1)          'using Temp$ avoids a BASIC 7 bug
   MID$(X$, Curpo) = Temp$              'move characters one position ahead

   MID$(X$, Curpo - 1, 1) = A$          'assign the current character
   QPrint MID$(X$, Curpo, Length - Curpo + 1), Colr, -1 're-print X$

   IF Length = Max GOTO TEnter          'field complete, handle as Enter key
   GOTO TGetKey



TBackspace:

   IF Curpo = 1 GOTO TGetKey            'can't back up any more, ignore
   Curpo = Curpo - 1                    'show cursor being 1 character before
   LOCATE , POS(0) - 1                  'back up the cursor
   GOTO TDel                            'handle as if it were the Delete key



TTabKey:                                'reserved for your Tab routine if you
                                        '  want to handle it differently
TEnter:

   GOSUB TInsertOff                     'clear insert, restore cursor size
   X$ = LEFT$(X$, Length)               'retain only the current length

   T$ = X$                              'assign the string
   LOCATE , , 0                         'turn off the cursor
   EXIT SUB



TEscape:

   ExitCode = 2                         'show that the user pressed Escape
   GOTO TEnter                          'handle as if it were the Enter Key



TInsertOff:

   'clear Insert mode and restore cursor, depending on monitor type

   Insrt = 0
   IF Clr THEN
      LOCATE , , , 6, 7
   ELSE
      LOCATE , , , 12, 13
   END IF
   RETURN

END SUB

SUB WRITEGAME ()

    REM:WRITE OUT A GAME
    OPEN D$(0) + F$ + ".TMP" FOR RANDOM AS #1 LEN = 90
    FOR X = 0 TO 14
    FIELD #1, X * 2 AS X$, 2 AS Q$(X + 1), 90 - 2 - 2 * X AS X$
    NEXT
    FIELD #1, 30 AS X$, 2 AS Q$(16), 2 AS Q$(17), 15 AS Q$(18), 15 AS Q$(19), 26 AS X$
    FIELD #1, 64 AS X$, 3 AS Q$(20), 3 AS Q$(21), 6 AS Q$(22), 8 AS Q$(23), 6 AS X$
    FIELD #1, 84 AS X$, 2 AS Q$(24), 2 AS Q$(25), 2 AS Q$(26)

    FOR X = 0 TO 14: LSET Q$(X + 1) = MKI$(NG%(TC, X)): NEXT
    FOR X = 0 TO 3: LSET Q$(20 + X) = YN$(TC, X): NEXT
    LSET Q$(16) = MKI$(VS%(TC)): LSET Q$(17) = MKI$(HS%(TC))
    LSET Q$(18) = V$(TC): LSET Q$(19) = H$(TC)
    FOR X = 24 TO 26: LSET Q$(X) = MKI$(NG%(TC, X - 9)): NEXT
    PUT #1, TC
    CLOSE 1


    OPEN D$(0) + F$ + ".TCX" FOR RANDOM AS #1 LEN = 90
    FOR X = 0 TO 14
    FIELD #1, X * 2 AS X$, 2 AS Q$(X + 1), 90 - 2 - 2 * X AS X$
    NEXT
    FIELD #1, 30 AS X$, 2 AS Q$(16), 2 AS Q$(17), 15 AS Q$(18), 15 AS Q$(19), 26 AS X$
    FIELD #1, 64 AS X$, 3 AS Q$(20), 3 AS Q$(21), 6 AS Q$(22), 8 AS Q$(23), 6 AS X$
    FIELD #1, 84 AS X$, 2 AS Q$(24), 2 AS Q$(25), 2 AS Q$(26)

    FOR X = 0 TO 14: LSET Q$(X + 1) = MKI$(NG%(TC, X)): NEXT
    FOR X = 0 TO 3: LSET Q$(20 + X) = YN$(TC, X): NEXT
    LSET Q$(16) = MKI$(VS%(TC)): LSET Q$(17) = MKI$(HS%(TC))
    LSET Q$(18) = V$(TC): LSET Q$(19) = H$(TC)
    FOR X = 24 TO 26: LSET Q$(X) = MKI$(NG%(TC, X - 9)): NEXT
    PUT #1, TC
    CLOSE 1

END SUB

STATIC SUB YesNoB (YN$, ExitCode, Colr)

   ExitCode = 0                                 'initialize flag

   X$ = YN$
   IF X$ = "" THEN X$ = " "
   QPrint X$, Colr, -1
   LOCATE , , 1



YGetKey:

   X$ = INKEY$
   IF X$ = "" GOTO YGetKey

   IF UCASE$(X$) = "Y" OR UCASE$(X$) = "N" GOTO YDone

   IF X$ = CHR$(9) OR X$ = CHR$(13) GOTO YD2    'Tab or Enter
   IF X$ = CHR$(27) THEN                        'Escape
      YN$ = X$
      ExitCode = 2
      GOTO YD2
   END IF

   IF LEN(X$) = 2 THEN                          'extended key
      X$ = RIGHT$(X$, 1)
      IF X$ = "P" OR X$ = "M" THEN GOTO YD2     'Down arrow, Right arrow
      IF X$ = "H" OR X$ = "K" OR X$ = CHR$(15) THEN
         ExitCode = 1                           'Up, Left, Shift-Tab
         GOTO YD2
      END IF
   END IF

   GOTO YGetKey



YDone:

   IF X$ > "Y" THEN X$ = CHR$(ASC(X$) - 32)     'capitalize if necessary
   YN$ = X$
   QPrint YN$, Colr, -1

YD2:

   LOCATE , , 0

END SUB

