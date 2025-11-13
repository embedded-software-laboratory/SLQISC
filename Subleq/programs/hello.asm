SECTION main[@0]
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
  SUB loop #-1
  JMP loop