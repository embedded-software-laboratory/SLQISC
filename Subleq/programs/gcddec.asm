SECTION main[@0]
loop:
  IN $0
  PRNT $0
  IN $1
  PRNT $1
  OUT @ 
  CALL gcd
  PRNT $0
  JMP loop

gcd:
  JLEQ $1 done
  MOD $0 $1
  MOV $2 $0
  MOV $0 $1
  MOV $1 $2
  JMP gcd
done:
  RET

SECTION sreg[@65533]
  65503 1