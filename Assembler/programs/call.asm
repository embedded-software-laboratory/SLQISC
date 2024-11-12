SECTION main[@0]
  CALL callee
  INC $0

SECTION callee
  callee:
  MOV $0 #42
  RET