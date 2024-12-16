SECTION main[@0]
loop:
  OUT x
  INC x
  MOV $0 #-32760
delay:
  INC $0
  JLEQ $0 delay
  JMP loop

SECTION vars
  x:0