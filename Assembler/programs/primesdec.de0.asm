SECTION main[@0]
entry:
  MOV vy vd
  MUL vy vd
  MOV vz vx
  INC vz
  vy vz prime
  MOV vy vx
  MOD vy vd
  SUB vz vz
  SUB vz vy
  SUB vy vy
  vz vy nonprime
  INC vd
  JMP entry
prime:
  CALL bcdout
  MOV $0 #-32760
delay:
  INC $0
  JLEQ $0 delay
nonprime:
  INC vx
  MOV vd #2
  JMP entry

SECTION funcs
bcdout:
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
  RET

SECTION SP[@16381]
  16128

SECTION vars
  vx: 2
  vd: 2
  vy: 0
  vz: 0