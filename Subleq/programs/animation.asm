SECTION main[@0]
loop:
  MOV $0 start
  ADD $0 fullOffset
  ADD $0 offset
  LDI $1 $0
  OUT $1
  INC offset
  MOV $2 offset
  SUB $2 #1199
  JLEQ $2 loop
  MOV offset #0
  ADD fullOffset #1200
  MOD fullOffset #8400
  JMP loop

SECTION vars
offset: 0
fullOffset: 0
start: logo

SECTION data
logo:
  IMG "images/tree0.png"
  IMG "images/tree1.png"
  IMG "images/tree2.png"
  IMG "images/tree3.png"
  IMG "images/tree4.png"
  IMG "images/tree5.png"
  IMG "images/tree6.png"

SECTION sreg[@65533]
  65503 1
