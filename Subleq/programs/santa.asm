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
  CALL delay
  MOD fullOffset #13200
  JMP loop

delay:
  MOV $0 #0
dloop:
  INC $0
  MOV $1 #-8000
  ADD $1 $0
  JLEQ $1 dloop
  RET

SECTION vars
offset: 0
fullOffset: 0
start: logo

SECTION data
logo:
  IMG "images/santa1.png"
  IMG "images/santa2.png"
  IMG "images/santa3.png"
  IMG "images/santa4.png"
  IMG "images/santa5.png"
  IMG "images/santa6.png"
  IMG "images/santa7.png"
  IMG "images/santa8.png"
  IMG "images/santa9.png"
  IMG "images/santa9.png"
  IMG "images/santa9.png"

SECTION sreg[@65533]
  65503 1
