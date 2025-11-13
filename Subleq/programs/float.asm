SECTION main[@0]
  MOV $SP #65280
//  MOV $0 ctest
  MOV $0 #25
  CALL itof
  CALL fprint
  OUT @E
sl:
  JMP sl

SECTION funcs
print:
  PRNT $0
  RET

itof:
  SUB $2 $2
itofloop:
  MOV $3 #1024
  SLQ $0 $3 shifted
  DEC $2
  MUL $0 #2
  JMP itofloop
shifted:
  ADD $2 #25
  MOV $4 #1024
  MUL $4 $2
  SUB $0 #1024
  ADD $0 $4
  RET

fsub:
  MOV fsubtmp $0
  CALL exp
  MOV fsubexpa $0
  MOV $0 fsubtmp
  CALL mant
  MOV fsubmanta $0
  MOV $0 $1
  CALL exp
  MOV fsubexpb $0
  MOV $0 $1
  CALL mant
  MOV fsubmantb $0
  SUB fsubexpb fsubexpa
  ADD fsubmantb #1024
  ADD fsubmanta #1024
fsubloop:
  JLEQ fsubexpb mbshifted
  MUL fsubmantb #2
  JMP fsubloop
mbshifted:
  SUB fsubmantb fsubmanta
  RET
fsubexpa: 0
fsubmanta: 0
fsubexpb: 0
fsubmantb: 0
fsubtmp: 0


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