SECTION main[@0]
  MOV $I #1
loop:
  MOV $0 max
  SUB $0 min
  JLEQ $0 liar
  OUT @I
  OUT @ 
  OUT @G
  OUT @U
  OUT @E
  OUT @S
  OUT @S
  OUT @ 
  PRNT curGuess
  OUT @ 
  OUT @ 
  OUT @ 
  IN $0
  JLEQ $0 low
  DEC $0
  JLEQ $0 high
correct:
  OUT @I
  OUT @ 
  OUT @W
  OUT @O
  OUT @N
  OUT @!
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
  JMP trap
liar:
  OUT @Y
  OUT @O
  OUT @U
  OUT @ 
  OUT @L
  OUT @I
  OUT @E
  OUT @D
  OUT @!
  OUT @ 
  OUT @ 
  OUT @ 
  OUT @ 
  OUT @ 
  OUT @ 
  OUT @ 
  JMP trap
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
trap:
  TRP

SECTION vars
  curGuess: 5000
  min: 0
  max: 10000
