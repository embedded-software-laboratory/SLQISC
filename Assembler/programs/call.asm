SECTION main[@0]
  MOV $SP #65280
  CALL callee
  PRNT $0

SECTION callee
  callee:
  MOV $0 #42
  RET