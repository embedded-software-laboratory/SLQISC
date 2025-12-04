SECTION main[@0]
main:
  CALL render
  INC cX
  MOV $0 #40
  SUB $0 cX
  JLEQ $0 rowDone
  JMP main
rowDone:
  MOV cX #0
  INC cY
  MOV $0 #30
  SUB $0 cY
  JLEQ $0 colDone
  JMP main
colDone:
  MOV cY #0
  JMP main

SECTION funcs
isqrt:
  MOV $1 lookisqrt
  ADD $1 $0
  LDI $0 $1
  RET

square:
  JLEQ $2 isneg
  MOV $3 $2
  MUL $2 $3
  RET
isneg:
  NEG $2
  MOV $3 $2
  MUL $2 $3
  RET

render:
  MOV $0 cX
  MOV $1 cY
  SUB $0 #20
  SUB $1 #15
  MOV $2 $0
  CALL square
  MOV $0 $2
  MOV $2 $1
  CALL square
  ADD $0 $2
  SUB $0 #100
  JLEQ $0 inside
  OUT black
  RET
inside:
  NEG $0
  CALL isqrt
  MUL $0 #2
  ADD $0 cX
  ADD $0 cY
  SUB $0 #51
  JLEQ $0 shade
  JMP light
shade:
  ADD $0 #16
  MOV $1 dgray
  MOV $2 lgray
  CALL dither
  RET
light:
  ADD $0 #4
  MOV $1 lgray
  MOV $2 white
  CALL dither
  RET

dither:
  MOV $3 cX
  MOV $4 cY
  MOD $3 #4
  MOD $4 #4
  MUL $4 #4
  ADD $4 $3
  ADD $4 lookdither
  LDI $3 $4
  SUB $0 $3
  JLEQ $0 ddark
  OUT $2
  RET
ddark:
  OUT $1
  RET

SECTION params
cX: 0
cY: 0

SECTION colors
black: 0
dgray: 1365
lgray: 2730
white: 4095

SECTION lookup
lookisqrt: isqdata
isqdata:
0 1 1 1 2 2 2 2 2 3
3 3 3 3 3 3 4 4 4 4
4 4 4 4 4 5 5 5 5 5
5 5 5 5 5 5 6 6 6 6
6 6 6 6 6 6 6 6 6 7
7 7 7 7 7 7 7 7 7 7
7 7 7 7 8 8 8 8 8 8
8 8 8 8 8 8 8 8 8 8
8 9 9 9 9 9 9 9 9 9
9 9 9 9 9 9 9 9 9 9
10
lookdither: ditherdata
ditherdata:
 0  8  2 10
12  4 14  6
 3 11  1  9
15  7 13  5

SECTION sreg[@65533]
  65503 1
