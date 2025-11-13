SECTION main[@0]
  MOV $2 loop
  JMP loop

SECTION data
hw:
  STR "HELLO WORLD!    "
  STR "                "
  STR "I LOVE TI!      "
  STR "                "

SECTION loop
loop:
  OUT hw
  MOV $0 #-32760
delay:
  INC $0
  JLEQ $0 delay
  INC loop
  JMP loop