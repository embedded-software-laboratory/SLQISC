SECTION main[@0]
  IN inVal
  MOV result #0
  MOV $0 #14
  MOV $1 pow2
  ADD $1 $0
  LDI $2 $1
loop:
  SUB inVal $2
  JLEQ inVal bel
  DEC $1
  LDI $2 $1
  ADD result $2
  JMP loop
bel:
  ADD inVal $2
  DEC $1
  LDI $2 $1
  JMP loop
exit:
  PRNT result
trap:
  JMP trap

SECTION tables
  pow2: 1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384

SECTION vars
  inVal: 0
  result: 0