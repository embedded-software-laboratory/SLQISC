SECTION main[@0]
loop:
  MOV $0 start
  ADD $0 offset
  LDI $1 $0
  OUT $1
  INC offset
  MOD offset #1200
  JMP loop

SECTION vars
offset: 0
start: logo

SECTION data
logo:
  IMG "images/tree.png"
