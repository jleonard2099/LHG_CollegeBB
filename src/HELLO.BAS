DEFINT A-Z
DECLARE SUB GETKEY ()
DECLARE SUB TITLE ()
DECLARE SUB Comline (N, A$(), Max)
   
   DECLARE SUB MMenuVert (BYVAL Address%, Choice%, Start%, SCANCODE%, NormClr%, HiLite%, NumRows%, Row%, Column%)
   DECLARE SUB box0 (ULRow%, ULCol%, LRRow%, LRCol%, Char%, Colr%)
   DECLARE FUNCTION GetDrive% ()
   DECLARE SUB TextIn (T$, Max, NumOnly, CapsOn, ExitCode, Colr)
   DECLARE SUB YesNoB (YN$, ExitCode, Colr)
   DECLARE SUB ClearEOL (Colr)
   DECLARE SUB MAMenu (BYVAL Address%, SELECTION%, Start%, Count%, ExitCode%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%)
   DECLARE FUNCTION FUsing$ (Number$, Mask$)
   DECLARE FUNCTION OneColor% (FG, BG)
   DECLARE FUNCTION Peek1% (Segment, Address)
   DECLARE SUB QPrint (X$, Colr, Page)
   DECLARE FUNCTION QPTRIM$ (X$)
   DECLARE SUB CapNum ()
   DIM D$(3), item$(18)
   DIM A$(1 TO 15)
   DRIVE = GetDrive%
   DEF FnExt$ (X$, Length) = X$ + SPACE$(Length - LEN(X$))  'for display only
   OPEN "DEV.BAS" FOR INPUT AS #1
   FOR I = 0 TO 1
   INPUT #1, D$(I)
   NEXT
   CLOSE 1
   
 CALL Comline(N, A$(), 10)     ' Get what was typed on the command line
 PRINT "Number of arguments = "; N ' Print out each part of the command line
 PRINT "Arguments are: "
 FOR I = 1 TO N: PRINT A$(I): NEXT I
 
' The following is a sample command line and output for a stand-alone
' executable file (assumes program name is ARG.EXE):
'
' arg one  two   three    four     five      six
 
' Use SUB procedure to get command line and split into arguments.
' Parameters:  NumArgs : Number of command-line arguments found
'              Args$() : Array in which to return arguments
'              MaxArgs : Maximum number of arguments array can return
   IF A$(1) = "3" THEN A$(1) = "": GOTO 800
   IF A$(1) = "2" THEN A$(1) = "": GOTO 700
   IF A$(1) = "4" THEN A$(1) = "": GOTO 750
1  CALL TITLE
   item$(1) = "PLAY SINGLE GAME                "
   item$(2) = "TEAM MANAGEMENT / ORGANIZATION  "
   item$(3) = "REPLAY STATISTICS AND REPORTS   "
   item$(4) = "LEADERS / STANDINGS / RECORDS   "
   item$(5) = "SCHEDULER / AUTOPLAY / BOXSCORES"
   item$(6) = "TOURNAMENT PLAY                 "
   item$(7) = "HARD DRIVE INSTALLATION         "
   item$(8) = "CONFIGURE DRIVES FOR TEAMS,STATS"
   item$(9) = "EXIT TO DOS                     "
     SELECTION% = 1
     Count% = 9
     Start% = 1
     SCANCODE% = 9
     FileColr% = 31
     HiLiteColr% = 112
     Spacing% = 1
     Row% = 8
     Column% = 25
     CALL box0(7, 24, 17, 57, 2, 31)
     CALL ShowCursor
     MMenuVert VARPTR(item$(1)), SELECTION%, Start%, SCANCODE%, FileColr%, HiLiteColr%, Count%, Row%, Column%
     CALL HideCursor
     COLOR , 0
     IF SCANCODE% = 27 THEN END

    IF SELECTION% = 1 THEN RUN "COLHOOP.EXE"
    IF SELECTION% = 2 THEN 700
    IF SELECTION% = 3 THEN 750
    IF SELECTION% = 4 THEN 800
    IF SELECTION% = 5 THEN RUN "SCHEDULE.EXE"
    IF SELECTION% = 6 THEN RUN "ADDTOURN.EXE"
    IF SELECTION% = 7 THEN RUN "INSTALL.EXE"
    IF SELECTION% = 8 THEN 505
    IF SELECTION% = 9 THEN CLS : END


505 COLOR 15, 1
      FG = 0
      BG = 7
      FOR I = 0 TO 1
      D$(I) = LEFT$(D$(I), LEN(D$(I)) - 1)
      NEXT
   Colr = OneColor%(FG, BG)             'pack FG and BG into a single byte
    CLS : LOCATE , 16: PRINT "SPECIFY PATHS FOR YOUR TEAMS AND COMPILED STATS": PRINT
510 LOCATE , 31: PRINT "PATHNAME EXAMPLES:": PRINT
515 LOCATE , 31: PRINT "A:"
520 LOCATE , 31: PRINT "C:\COLBB\TEAMS"

    LOCATE 7, 1: QPrint "Path for Teams ", 30, -1
    LOCATE , 16: QPrint FnExt$(D$(0), 30), Colr, -1
    LOCATE 9, 1: QPrint "Path for Stats ", 30, -1
    LOCATE , 16: QPrint FnExt$(D$(1), 30), Colr, -1

GetDiv1:

   LOCATE 7, 16                         'location of beginning of field
   CapsOn = 0: NumOnly = 0: Max = 30     'specify field parameters
   CALL TextIn(D$(0), Max, NumOnly, CapsOn, XCode, Colr)        'get input
   IF XCode = 1 GOTO GetDiv1:           'can't back up any more, stay here
   IF XCode = 2 GOTO Done               'all done entering data
   D$(0) = QPTRIM$(D$(0))
GetDiv2:

   LOCATE 9, 16                         'location of beginning of field
   CapsOn = 0: NumOnly = 0: Max = 30    'specify field parameters
   CALL TextIn(D$(1), Max, NumOnly, CapsOn, XCode, Colr)        'get input
   IF XCode = 1 GOTO GetDiv1:           'can't back up any more, stay here
   IF XCode = 2 GOTO Done               'all done entering data
   D$(1) = QPTRIM$(D$(1))


Done:

   LOCATE 23, 1
   PRINT "Is this okay? (Yes/No) ";

   X$ = ""
   CALL YesNoB(X$, XCode, Colr)
   LOCATE , 1
   ClearEOL 30
   IF XCode = 1 GOTO GetDiv2
   IF X$ = "N" GOTO GetDiv1
   IF X$ <> "Y" GOTO Done               'insist on either Y or N or up-arrow

580 OPEN "DEV.BAS" FOR OUTPUT AS #1
585 FOR R = 0 TO 2: D$(R) = D$(R) + "\": NEXT R
590 PRINT #1, D$(0)
595 PRINT #1, D$(1)
600 CLOSE #1

    GOTO 1

700 CALL TITLE
   item$(1) = "INPUT A NEW TEAM              "
   item$(2) = "EDIT AN EXISTING TEAM         "
   item$(3) = "PRINT TEAM ROSTER / REPORT    "
   item$(4) = "VIEW TEAM ROSTER / REPORT     "
   item$(5) = "DRAFT TO NEW / EXISTING TEAM  "
   item$(6) = "RETURN TO MAIN MENU           "
     SELECTION% = 1
     Count% = 6
     Start% = 1
     SCANCODE% = 6
     FileColr% = 31
     HiLiteColr% = 112
     Spacing% = 1
     Row% = 8
     Column% = 27
     CALL box0(7, 26, 14, 57, 2, 31)
     CALL ShowCursor
     MMenuVert VARPTR(item$(1)), SELECTION%, Start%, SCANCODE%, FileColr%, HiLiteColr%, Count%, Row%, Column%
     CALL HideCursor
     COLOR , 0
    IF SCANCODE% = 27 THEN 1
    IF SELECTION% = 1 THEN RUN "CBBINPUT.EXE"
    IF SELECTION% = 2 THEN RUN "CNGRAT.EXE 1"
    IF SELECTION% = 3 THEN RUN "CNGRAT.EXE 2"
    IF SELECTION% = 4 THEN RUN "CNGRAT.EXE 3"
    IF SELECTION% = 5 THEN RUN "DRAFT.EXE"
    IF SELECTION% = 6 THEN 1

750 CALL TITLE
   item$(1) = "VIEW / PRINT COMPILED STATISTICS "
   item$(2) = "ENCYCLOPEDIA/ TEAM REPLAY OPTIONS"
   item$(3) = "COMPARE REPLAY STATS TO ACTUAL   "
   item$(4) = "MERGE SEPARATE STATISTICAL FILES "
   item$(5) = "RETURN TO COURTSIDE MAIN MENU    "
     SELECTION% = 1
     Count% = 5
     Start% = 1
     SCANCODE% = 5
     FileColr% = 31
     HiLiteColr% = 112
     Spacing% = 1
     Row% = 8
     Column% = 25
     CALL box0(7, 24, 13, 58, 2, 31)
     CALL ShowCursor
     MMenuVert VARPTR(item$(1)), SELECTION%, Start%, SCANCODE%, FileColr%, HiLiteColr%, Count%, Row%, Column%
     CALL HideCursor
     COLOR , 0
     IF SCANCODE% = 27 THEN 1
    IF SELECTION% = 1 THEN RUN "LOOKY.EXE"
    IF SELECTION% = 2 THEN RUN "CAREER.EXE"
    IF SELECTION% = 3 THEN RUN "COMPARE.EXE"
    IF SELECTION% = 4 THEN RUN "MERGE.EXE"
    IF SELECTION% = 5 THEN 1

800 CALL TITLE
   item$(1) = "CREATE/ EDIT A CONFERENCE"
   item$(2) = "UPDATE STATS/ STANDINGS  "
   item$(3) = "VIEW/ PRINT REPORTS      "
   item$(4) = "HEAD-TO-HEAD STATS       "
   item$(5) = "RECORD BOOKS             "
   item$(6) = "EXPANDED LEADERS         "
   item$(7) = "DELETE CONFERENCE STATS  "
   item$(8) = "CREATE/ EDIT ORGANIZATION"
   item$(9) = "UPDATE POLL/ STATS       "
   item$(10) = "VIEW/ PRINT REPORTS      "
   item$(11) = "RECORD BOOKS             "
   item$(12) = "DELETE ORGANIZATION STATS"
   item$(13) = "RETURN TO COURTSIDE MENU "
   item$(14) = "EXIT TO DOS              "
     SELECTION% = 1
     Count% = 12
     Start% = 1
     FileColr% = 31
     HiLiteColr% = 112
     Rows% = 6
     Columns% = 2
     Spacing% = 4
     Row% = 9
     Column% = 12
     CALL box0(7, 11, 15, 37, 2, 31)
     CALL box0(7, 40, 15, 66, 2, 31)
     COLOR 15, 0
     LOCATE 8, 14: PRINT "COLLEGE CONFERENCE"
     LOCATE 8, 41: PRINT "NATIONAL ORGANIZATION"
     LOCATE 17
     LOCATE , 10: PRINT "COLLEGE CONFERENCE IS USED TO DEFINE THE TEAMS IN A CONFERENCE"
     PRINT
     LOCATE , 10: PRINT "NATIONAL ORGANIZATION ALLOWS YOU TO SELECT MULTIPLE CONFERENCES"
     LOCATE , 10: PRINT "TO TRACK AND UPDATE TO GENERATE A NATIONAL POLL AND LEADERS."
     CALL ShowCursor
20   MAMenu VARPTR(item$(1)), SELECTION%, Start%, Count%, ExitCode%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%
     CALL HideCursor
     IF ExitCode% = 27 THEN 1
     IF ExitCode% = -71 THEN Start% = 1: ExitCode% = 3: SELECTION% = 1: GOTO 20
     IF ExitCode% = -79 THEN Start% = 15: ExitCode% = 3: SELECTION% = 15: GOTO 20
     IF ExitCode% = -73 THEN Start% = Start% - 80: IF Start% < 1 THEN Start% = 1
     IF ExitCode% = -81 THEN Start% = Start% + 80: IF Start% > 15 THEN Start% = 15
     IF ExitCode% = -81 OR ExitCode% = -73 THEN ExitCode% = 3: SELECTION% = Start%: GOTO 20
     COLOR , 0
    IF SELECTION% = 1 THEN RUN "CREATCON.EXE"
    IF SELECTION% = 2 THEN RUN "COMPCON.EXE"
    IF SELECTION% = 3 THEN RUN "SEECON.EXE"
    IF SELECTION% = 4 THEN RUN "HD2HDCON.EXE"
    IF SELECTION% = 5 THEN RUN "RECCON.EXE"
    IF SELECTION% = 6 THEN RUN "EXPANCON.EXE"
    IF SELECTION% = 7 THEN RUN "BLANK.EXE"
    IF SELECTION% = 8 THEN RUN "CREATNAT.EXE"
    IF SELECTION% = 9 THEN RUN "COMPNAT.EXE"
    IF SELECTION% = 10 THEN RUN "SEENAT.EXE"
    IF SELECTION% = 11 THEN RUN "RECCON.EXE"
    IF SELECTION% = 12 THEN RUN "BLANKORG.EXE"
    IF SELECTION% = 13 THEN 1
    IF SELECTION% = 14 THEN CLS : END

STATIC SUB CapNum ()

   CapStatus = Peek1%(0, &H417) AND &H40        'get BIOS status of Caps Lock
   NumStatus = Peek1%(0, &H417) AND &H20        'get BIOS status of Num Lock
   H = POS(0): V = CSRLIN                       'save cursor position

   IF Cap = CapStatus GOTO QNumCheck            'no change since last check

   Cap = CapStatus                              'save Caps status
   LOCATE 25, 69                                'position to print status msg

   IF CapStatus THEN
      QPrint " CAP ", 112, -1                   'white on black
   ELSE
      QPrint SPACE$(5), 7, -1                   'print or erase message
   END IF



QNumCheck:

   IF Num = NumStatus GOTO QCapNumExit          'no change since last check

   Num = NumStatus                              'save Num status
   LOCATE 25, 75                                'position to print status msg

   IF NumStatus THEN                            'print or erase message
      QPrint " NUM ", 112, -1
   ELSE
      QPrint SPACE$(5), 7, -1
   END IF



QCapNumExit:

   LOCATE V, H                                  'restore cursor position

END SUB

 STATIC SUB Comline (NumArgs, Args$(), MaxArgs)
 CONST TRUE = -1, FALSE = 0
 
     NumArgs = 0: In = FALSE
     ' Get the command line using the COMMAND$ function
     Cl$ = COMMAND$
     L = LEN(Cl$)
     ' Go through the command line a character at a time
     FOR I = 1 TO L
          C$ = MID$(Cl$, I, 1)
          ' Test for character being a blank or a tab
          IF (C$ <> " " AND C$ <> CHR$(9)) THEN
          ' Neither blank nor tab; test if you're already inside
          ' an argument
               IF NOT In THEN
               ' You've found the start of a new argument
                    ' Test for too many arguments
                      IF NumArgs = MaxArgs THEN EXIT FOR
                      NumArgs = NumArgs + 1
                      In = TRUE
               END IF
               ' Add the character to the current argument
               Args$(NumArgs) = Args$(NumArgs) + C$
          ELSE
          ' Found a blank or a tab.
               ' Set "Not in an argument" flag to FALSE
               In = FALSE
          END IF
     NEXT I
 
 END SUB

SUB GETKEY ()
LOCATE 23, 1: PRINT "PRESS ANY KEY TO CONTINUE"
21 I$ = INKEY$: IF I$ = "" THEN 21
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

   'CapNum                               'display Cap/Num if active

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

SUB TITLE ()
   COLOR , 0: KEY OFF: CLS
   COLOR , 1: LOCATE 1, 1: PRINT TAB(80); : LOCATE , 24: COLOR 12
   PRINT "COURTSIDE COLLEGE BASKETBALL V4.1"
   LOCATE 24, 1: PRINT TAB(80); : LOCATE , 1

   PRINT "(C) LANCE HAFFNER/SHANNON LYNN 1987-1994"; TAB(70); LEFT$(TIME$, 5);
   LOCATE 2, 1: COLOR 2, 0
   COLOR 15
   PRINT
   PRINT "            PROGRAMMED AND DEVELOPED BY LANCE HAFFNER AND SHANNON LYNN"
   PRINT "          TECHNICAL SUPPORT, CALL (800) 477-7032 EMAIL AT ecciLR@AOL.COM"
   COLOR 10
END SUB

'----- entry parameters
'
'YN$ = string to be input or edited (use the name of your choice)
'Colr is the combined foreground and background colors that will be used
'
'----- exit parameters
'
'YN$  = final edited amount
'ExitCode indicates how editing was terminated -
'   0 = Enter, Tab, Down-Arrow, Right-Arrow past end, or field filled
'   1 = Shift-Tab, Up-Arrow, or Left-Arrow past beginning
'   2 = Escape key pressed
'
'----- local variables
'
'X$ is a string version of the number while it is being input or edited
'
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

