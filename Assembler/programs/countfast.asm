SECTION main[@0]
loop:
  OUT x
  ADD x #1
  JMP loop

SECTION vars
  x:0