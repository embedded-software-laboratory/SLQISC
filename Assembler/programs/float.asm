SECTION main[@0]
  MOV $SP #65280
  MOV $0 ctest
  CALL fprint
  OUT @E
sl:
  JMP sl

SECTION funcs
print:
  PRNT $0
  RET

itof:
  RET

fsub:
  RET

fprint:
  MOV $1 $0
  CALL trunc
  PRNT $0
  OUT @i
  CALL itof
  CALL fsub
  RET

trunc:
  MOV $1 $0
  CALL exp
  SUB $0 #10
  MOV $2 $0
  MOV $0 $1
  CALL mant
  ADD $0 #1024
  JLEQ $2 shr
shl:
  MUL $0 #2
  DEC $2
  JLEQ $2 exit
  JMP shl
shr:
  NEG $2
shrloop:
  JLEQ $2 exit
  DIV $0 #2
  DEC $2
  JMP shrloop
exit:
  RET

sign:
  INC $0
  JLEQ $0 signneg
  SUB $0 $0
  RET
signneg:
  MOV $0 #1
  RET

exp:
  INC $0
  JLEQ $0 expneg
  DEC $0
  JMP div
expneg:
  SUB $0 #32769
div:
  DIV $0 #1024
  JLEQ $0 denorm
  SUB $0 #15
  RET
denorm:
  MOV $0 #-14
  RET

mant:
  INC $0
  JLEQ $0 mantneg
  DEC $0
  JMP mod
mantneg:
  SUB $0 #32769
mod:
  MOD $0 #1024
  RET

SECTION consts
  ctest: ((15*1024)+825)