SECTION main[@0]
  MOV $I #1
loop:
  MOV $0 max
  SUB $0 min
  JLEQ $0 liar
  MOV $0 curGuess
  DIV $0 #1000
  MOV $1 curGuess
  MOV $4 $0
  MUL $4 #1000
  SUB $1 $4
  DIV $1 #100
  MOV $2 curGuess
  MOV $4 $0
  MUL $4 #10
  ADD $4 $1
  MUL $4 #100
  SUB $2 $4
  DIV $2 #10
  MOV $3 curGuess
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
  OUT $0
  IN $0
  JLEQ $0 low
  DEC $0
  JLEQ $0 high
correct:
  OUT #24737
  JMP correct
liar:
  OUT #57005
  JMP liar
high:
  MOV min curGuess
  MOV $0 min
  ADD $0 max
  DIV $0 #2
  MOV curGuess $0
  JMP loop
low:
  MOV max curGuess
  MOV $0 min
  ADD $0 max
  DIV $0 #2
  MOV curGuess $0
  JMP loop

SECTION vars
  curGuess: 5000
  min: 0
  max: 10000