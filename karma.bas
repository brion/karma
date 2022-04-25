'use: qbas karma /l
DECLARE SUB mouse (m0%, m1%, m2%, m3%)
DECLARE SUB mShow ()
DECLARE SUB mHide ()
DEFINT A-Z
TYPE RegType
  AX AS INTEGER
  BX AS INTEGER
  CX AS INTEGER
  DX AS INTEGER
  BP AS INTEGER
  SI AS INTEGER
  DI AS INTEGER
  FLAGS AS INTEGER
  DS AS INTEGER
  ES AS INTEGER
END TYPE
DEF fnmouse (z)
  m0 = 3: m1 = 0: m2 = 0: m3 = 0: ni = 0
  mouse m0, m1, m2, m3
  IF z = 0 THEN ni = m1
  IF z = 1 OR z = 3 THEN ni = m2 / 2
  IF z = 2 OR z = 4 THEN ni = m3
  fnmouse = ni
END DEF
mouse m0, m1, m2, m3

'Copyright 1987
'COMPUTE! Publications, Inc.
'All Rights Reserved.

DEFINT A-Z':defsng r,g,b
DIM sides(52), xcord(52, 5), ycord(52, 5), numadj(52), neighbors(52, 3)
DIM owner(52), renter(52), update(52), start(20), xfind(52), yfind(52)
DIM r(15), g(15), b(15), hue(100), ToDo(54)
gamenum = 1

RANDOMIZE TIMER

SCREEN 13: VIEW (0, 0)-(311, 186)

newgame:

COLOR 1: CLS : score(1) = 10: score(2) = 10
COLOR 1: LOCATE 8, 13: PRINT "     KARMA     "
COLOR 1: PRINT : PRINT "  Copyright 1987 Compute! Publ., Inc."
PRINT "          All Rights Reserved"
PRINT : PRINT : COLOR 1
PRINT "              Choose game.            ": PRINT
PRINT "              1. Capture All"
PRINT "              2. Four Corners"
PRINT "              3. Two Pies"
PRINT "              4. 2500 Points"
PRINT "              5. Quit"

GetAKey:
mShow
a$ = INKEY$: IF a$ = "" THEN GOTO GetAKey
IF a$ < "1" OR a$ > "5" THEN GOTO GetAKey
gamenum = VAL(a$): LOCATE 21, 19: PRINT " "; a$; " ": IF gamenum < 5 THEN PRINT "  Please wait."
IF gamenum = 5 THEN GOSUB QuittingTime

RESTORE findpoints
FOR i = 0 TO 52
 READ x, y: xfind(i) = x * 10: yfind(i) = y * 10
NEXT i

RESTORE Karma
FOR i = 0 TO 52
 READ sides(i)
 FOR ii = 0 TO sides(i) - 2
  READ xc, yc
  xcord(i, ii) = xc * 12
  ycord(i, ii) = yc * 12
 NEXT ii
 READ numadj(i)
 FOR ii = 0 TO numadj(i) - 1
  READ neighbors(i, ii)
 NEXT ii
NEXT i

RESTORE thecoLors
FOR i = 0 TO 15
 READ r, g, b: r(i) = INT(r / 2): g(i) = INT(g / 2): b(i) = INT(b / 2)
 PALETTE i, 65536 * b(i) + 256 * g(i) + r(i)
NEXT i

thecoLors:

DATA 50,40,30
DATA 16,16,16
DATA 0,0,0
DATA 0,5,40
DATA 25,5,30
DATA 50,5,20
DATA 75,5,10
DATA 100,5,0
DATA 100,55,0
DATA 30, 30, 30
DATA 0,0,0
DATA 70,70,70
DATA 0,0,0
DATA 0,0,0
DATA 0,0,0
DATA 4,0,0

'DATA .5,.4,.3
'DATA .16,.16,.16
'DATA 0,0,0
'DATA 0,.05,.4
'DATA .25,.05,.3
'DATA .5,.05,.2
'DATA .75,.05,.1
'DATA 1,.05,0
'DATA 1,.55,0
'DATA .3,.3,.3
'DATA 0,0,0
'DATA .7,.7,.7
'DATA 0,0,0
'DATA 0,0,0
'DATA 0,0,0
'DATA .4,0,0


mHide
COLOR 1: FOR i = 0 TO 24: PRINT : NEXT i
LOCATE 1, 8: COLOR 10: PRINT " K a r m a "
COLOR 11: LOCATE 1, 25

IF gamenum = 1 THEN PRINT " Capture All "
IF gamenum = 2 THEN PRINT " Four Corners "
IF gamenum = 3 THEN PRINT " Two Pies "
IF gamenum = 4 THEN PRINT " 2500 Points "

RESTORE start
FOR i = 0 TO 19
 READ start(i)
NEXT i

start:
DATA 4,13,5,14,11,15,23,33,24,34,25,35,18,8,30,20,37,36,39,38

FOR i = 0 TO 52
 owner(i) = 0: renter(i) = 0: update(i) = 0
NEXT i

FOR i = 0 TO 19
 owner(start(i)) = (i AND 1) + 1: renter(start(i)) = 1
NEXT i

FOR i = 0 TO 52
 GOSUB DoOne
NEXT i

player = 2: play$(1) = "black": play$(2) = "white"

game:

player = 3 - player
LOCATE 7, 25: COLOR p + player: PRINT " "; play$(player); "'s turn "



LoopIt:

WHILE fnmouse(0) = 0: mShow: WEND: mHide
x = fnmouse(1): y = fnmouse(2): hue = POINT(x, y)
IF hue < 3 OR hue > 8 THEN GOTO LoopIt
PALETTE 14, rgb(hue)
PAINT (x, y), 14, 2

which = -1
FOR i = 0 TO 52
 IF POINT(xfind(i), yfind(i)) = 14 THEN which = i
NEXT i
IF which < 0 THEN STOP

IF owner(which) <> player THEN PAINT (x, y), hue, 2: GOTO LoopIt

SOUND 130, 1: SOUND 135, 0E->76

'FOR reaL = 0 TO 1 STEP .02
' h = hue: rr = reaL
' PALETTE 13, 65536 * INT(b(h) - ((.1 * rr) * 5)) + 256 * 3 + INT(r(h) + ((.25 * rr) * 12))
'NEXT reaL

MaxToDo = 0

again:

renter(which) = renter(which) + 1
IF renter(which) + 1 > numadj(which) THEN
 FOR i = 0 TO numadj(which) - 1
  MaxToDo = MaxToDo + 1: t = neighbors(which, i): ToDo(MaxToDo) = t
  PAINT (xfind(t), yfind(t)), POINT(xfind(t), yfind(t)) + 1, 2
  NEXT i: SOUND 200 + which * 16, 1: SOUND 200 + which * 8, 1
 renter(which) = renter(which) - numadj(which)
END IF

i = which
IF owner(i) = 3 - player THEN score(3 - player) = score(3 - player) - 1
IF owner(i) <> player THEN score(player) = score(player) + 1
owner(i) = player: GOSUB DoOne: SOUND 200 + 6 * which, 0E->76
IF score(1) = 0 OR score(2) = 0 THEN GOTO gameover
IF MaxToDo <> 0 THEN which = ToDo(MaxToDo): MaxToDo = MaxToDo - 1: GOTO again

WHILE fnmouse(0) = 0: mShow: WEND: mHide

IF gamenum = 3 THEN
 win1 = 0: win2 = 0
 FOR j = 0 TO 3
  garbage = owner(j * 8 + 4)
  FOR k = 1 TO 7
   garbage = owner(j * 8 + 4 + k) AND garbage
  NEXT k
  IF garbage = 1 THEN win1 = win1 + 1
  IF garbage = 2 THEN win2 = win2 + 1
 NEXT j
 IF win1 >= 2 OR win2 >= 2 THEN GOTO gameover
END IF
FOR j = 1 TO 2
 FOR i = 0 TO 52
  IF owner(i) = j THEN points(j) = points(j) + numadj(i)
 NEXT i
NEXT j
LOCATE 23, 25: COLOR 10: PRINT points(1)
LOCATE 23, 32: COLOR 11: PRINT points(2)
IF gamenum = 4 AND (points(1) > 2499 OR POINT(2) > 2499) THEN GOTO gameover

GOTO game

SCREEN 0: WIDTH 80

'GOTO doit

END

DoOne:

si2 = sides(i) - 2: COLOR 7 - (numadj(i) - renter(i))
PSET (xcord(i, si2) + 12, ycord(i, si2) + 12), 13
FOR ii = 0 TO si2
 LINE -(xcord(i, ii) + 12, ycord(i, ii) + 12), 13
NEXT ii
PAINT (xfind(i) - 1, yfind(i) - 1), , 13
COLOR 2
PSET (xcord(i, si2) + 12, ycord(i, si2) + 12)
FOR ii = 0 TO si2
 LINE -(xcord(i, ii) + 12, ycord(i, ii) + 12)
NEXT ii

DoOne2:

si2 = sides(i) - 2: COLOR owner(i) + 9
PSET (xcord(i, si2) / 2 + 202, ycord(i, si2) / 2 + 90), 13
FOR ii = 0 TO si2
 LINE -(xcord(i, ii) / 2 + 202, ycord(i, ii) / 2 + 90), 13
NEXT ii
PAINT (xfind(i) / 2 + (202 - 7), yfind(i) / 2 + (90 - 7)), , 13
COLOR 1
PSET (xcord(i, si2) / 2 + 202, ycord(i, si2) / 2 + 90)
FOR ii = 0 TO si2
 LINE -(xcord(i, ii) / 2 + 202, ycord(i, ii) / 2 + 90)
NEXT ii
RETURN

gameover:
FOR i = 0 TO 52
 GOSUB DoOne2
NEXT i
FOR i = 0 TO 40
 FOR j = 0 TO 3
  SOUND 140 + (RND * i * 10), 2
 NEXT j
NEXT i
FOR i = 40 TO 0 STEP -1
 FOR j = 0 TO 3
  SOUND 140 + (RND * i * 10), 2
 NEXT j
NEXT i
LOCATE 12, 5: PRINT "Click Mouse To Continue"
WHILE fnmouse(0) = 0: mShow: WEND
RUN

QuittingTime:
SCREEN 0: WIDTH 80, 25: END
RETURN

Karma:

DATA 5,1,1,4,1,3,3,1,4
DATA 2,43,44
DATA 5,10,1,13,1,13,4,11,3
DATA 2,45,46
DATA 5,11,11,13,10,13,13,10,13
DATA 2,40,47
DATA 5,1,10,3,11,4,13,1,13
DATA 2,41,42
DATA 4,6,2,8,2,7,4
DATA 3,5,11,39
DATA 4,8,2,9,3,7,4
DATA 3,4,6,45
DATA 4,9,3,9,5,7,4
DATA 3,5,7,50
DATA 4,9,5,8,6,7,4
DATA 3,6,8,19
DATA 4,8,6,6,6,7,4
DATA 3,7,9,52
DATA 4,6,6,5,5,7,4
DATA 3,8,10,29
DATA 4,5,5,5,3,7,4
DATA 3,9,11,49
DATA 4,5,3,6,2,7,4
DATA 3,4,10,44
DATA 4,9,5,11,5,10,7
DATA 3,13,19,50
DATA 4,11,5,12,6,10,7
DATA 3,12,14,46
DATA 4,12,6,12,8,10,7
DATA 3,13,15,36
DATA 4,12,8,11,9,10,7
DATA 3,14,16,47
DATA 4,11,9,9,9,10,7
DATA 3,15,17,51
DATA 4,9,9,8,8,10,7
DATA 3,16,18,21
DATA 4,8,8,8,6,10,7
DATA 3,17,19,52
DATA 4,8,6,9,5,10,7
DATA 3,7,12,18
DATA 4,6,8,8,8,7,10
DATA 3,21,27,52
DATA 4,8,8,9,9,7,10
DATA 3,17,20,22
DATA 4,9,9,9,11,7,10
DATA 3,21,23,51
DATA 4,9,11,8,12,7,10
DATA 3,22,24,40
DATA 4,8,12,6,12,7,10
DATA 3,23,25,37
DATA 4,6,12,5,11,7,10
DATA 3,24,26,41
DATA 4,5,11,5,9,7,10
DATA 3,25,27,48
DATA 4,5,9,6,8,7,10
DATA 3,20,26,31
DATA 4,3,5,5,5,4,7
DATA 3,29,35,49
DATA 4,5,5,6,6,4,7
DATA 3,9,28,30
DATA 4,6,6,6,8,4,7
DATA 3,29,31,52
DATA 4,6,8,5,9,4,7
DATA 3,27,30,32
DATA 4,5,9,3,9,4,7
DATA 3,31,33,48
DATA 4,3,9,2,8,4,7
DATA 3,32,34,42
DATA 4,2,8,2,6,4,7
DATA 3,33,35,38
DATA 4,2,6,3,5,4,7
DATA 3,28,34,43
DATA 5,12,6,14,6,14,8,12,8
DATA 4,14,38,46,47
DATA 5,6,12,8,12,8,14,6,14
DATA 4,24,39,40,41
DATA 5,0,6,2,6,2,8,0,8
DATA 4,34,36,42,43
DATA 5,6,0,8,0,8,2,6,2
DATA 4,4,37,44,45
DATA 6,9,11,11,11,10,13,8,14,8,12
DATA 4,2,23,37,51
DATA 6,3,11,5,11,6,12,6,14,4,13
DATA 4,3,25,37,48
DATA 6,0,8,2,8,3,9,3,11,1,10
DATA 4,3,33,38,48
DATA 6,1,4,3,3,3,5,2,6,0,6
DATA 4,0,35,38,49
DATA 6,4,1,6,0,6,2,5,3,3,3
DATA 4,0,11,39,49
DATA 6,8,0,10,1,11,3,9,3,8,2
DATA 4,1,5,39,50
DATA 6,11,3,13,4,14,6,12,6,11,5
DATA 4,1,13,36,50
DATA 6,12,8,14,8,13,10,11,11,11,9
DATA 4,2,15,36,51
DATA 5,3,9,5,9,5,11,3,11
DATA 4,26,32,41,42
DATA 5,3,3,5,3,5,5,3,5
DATA 4,10,28,43,44
DATA 5,9,3,11,3,11,5,9,5
DATA 4,6,12,45,46
DATA 5,9,9,11,9,11,11,9,11
DATA 4,16,22,40,47
DATA 5,6,6,6,8,8,8,8,6
DATA 4,8,18,20,30

findpoints:

DATA 4,4,16,5,16,15,4,15
DATA 10,4,11,5,11,6,11,7,10,8,9,7,8,6,9,5
DATA 13,8,14,9,15,10,15,11,13,11,12,11,12,10,12,8
DATA 9.5,11.5,11,12,11,13,11,15,10,15,9,14,8,13,8,12
DATA 6,8,7,9,8,10,8,11,6,11,5,11,5,10,5,8
DATA 17,10,10,17,3,10,10,3
DATA 13,16,7,16,4,13,4,7,7,3,12,3,16,7,16,12
DATA 6,13,6,6,13,6,13,13
DATA 10, 10

SUB mHide
DIM InRegs AS RegType
       InRegs.AX = 2
       CALL interruptx(51, InRegs, InRegs)
END SUB

SUB mouse (m0%, m1%, m2%, m3%)
DIM InRegs AS RegType, OutRegs AS RegType
       InRegs.AX = m0%: InRegs.BX = m1%: InRegs.CX = m2%: InRegs.DX = m3%
       CALL interruptx(51, InRegs, OutRegs)
       m0% = OutRegs.AX: m1% = OutRegs.BX: m2% = OutRegs.CX: m3% = OutRegs.DX
END SUB

SUB mShow
DIM InRegs AS RegType
       InRegs.AX = 1
       CALL interruptx(51, InRegs, InRegs)
END SUB

