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
  OUT @ 
  OUT @ 
  OUT @ 
  OUT @ 
  OUT @ 
  OUT @ 
  OUT @ 
  OUT @ 
  OUT @ 
  OUT @ 
  OUT @ 
  PRNT vx
  MOV $0 #-32760
delay:
  INC $0
  JLEQ $0 delay
nonprime:
  INC vx
  MOV vd #2
  JMP entry

SECTION vars
  vx: 2
  vd: 2
  vy: 0
  vz: 0