SECTION main[@0]
entry:
  MOV vy vd
  MUL vy vd
  MOV vz vx
  INC vz
  vy vz prime
  MOV vy vx
  MOD vy vd
  vz vz (?+1)
  vy vz (?+1)
  vy vy (?+1)
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
nonprime:
  INC vx
  MOV vd #2
  JMP entry

SECTION vars
  vx: 2
  vd: 2
  vy: 0
  vz: 0