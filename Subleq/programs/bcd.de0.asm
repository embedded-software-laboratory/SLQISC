SECTION main[@0]
entry:
  MOV $0 vx
  DIV $0 #1000
  MOV $1 vx
  MOV $4 $0
  MUL $4 #1000
  SUB $1 $4
  DIV $1 #100
  MOV $2 vx
  MOV $4 $0
  MUL $4 #10
  ADD $4 $1
  MUL $4 #100
  SUB $2 $4
  DIV $2 #10
  MOV $3 vx
  MOV $4 $0
  MUL $4 #10
  ADD $4 $1
  MUL $4 #10
  ADD $4 $2
  MUL $4 #10
  SUB $3 $4
  MUL $0 #16
  ADD $0 $1
  MUL $0 #16
  ADD $0 $2
  MUL $0 #16
  ADD $0 $3
  SUB $O $0
sl:
  JMP sl

SECTION vars
  vx: 2187