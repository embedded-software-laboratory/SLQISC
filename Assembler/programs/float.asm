SECTION main[@0]
  MOV $SP #65280
  MOV $0 ctest
  CALL sign
  PRNT $0
  MOV $0 ctest
  CALL exp
  PRNT $0
  MOV $0 ctest
  CALL mant
  PRNT $0

SECTION funcs
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