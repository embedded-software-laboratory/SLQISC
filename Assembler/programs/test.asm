SECTION main[@0]
  JMP loop

SECTION data
data:
  STR "HELLO WORLD!"

SECTION loop
loop:
  OUT data
  SUB loop #-1
  JMP loop