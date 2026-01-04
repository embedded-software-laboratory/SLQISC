SECTION main[@0]
main:
  CALL render
  INC cX
  MOV $0 #40
  SUB $0 cX
  JLEQ $0 rowDone
  JMP main
rowDone:
  MOV cX #0
  INC cY
  MOV $0 #30
  SUB $0 cY
  JLEQ $0 colDone
  JMP main
colDone:
  MOV cY #0
  CALL move
  JMP main

SECTION trig
trig_k: 0
trig_t: 0
trig_x: 0
trig_y: 0
trig_p: 0
trig_i: 0
cordic:
  MOV trig_k #19898
  MOV trig_t #0
  MOV trig_x #32767
  MOV trig_y #0
  MOV trig_p #32767
  MOV trig_i #0
  MOV $1 $0
cloop:
  MOV $2 $0
  SUB $2 trig_t
  JLEQ $2 sneg
spos:
  MOV $3 lookatan
  ADD $3 trig_i
  LDI $2 $3
  ADD trig_t $2
  MOV $2 trig_x
  MOV $0 trig_y
  CALL shr
  SUB trig_x $0
  MOV $0 trig_x
  CALL shr
  ADD trig_y $0
sneg:

shr:
  MOV $1 #0
  SUB $0 #16383
  JLEQ $0 shr_n0
  ADD $1 #8192
  JMP shr_1
shr_n0:
  ADD $0 #16383
shr_1:
  SUB $0 #8191
  JLEQ $0 shr_n1
  ADD $1 #4096
  JMP shr_2
shr_n1:
  ADD $0 #8191
shr_2:
  SUB $0 #4095
  JLEQ $0 shr_n2
  ADD $1 #2048
  JMP shr_3
shr_n2:
  ADD $0 #4095
shr_3:
  SUB $0 #2047
  JLEQ $0 shr_n3
  ADD $1 #1024
  JMP shr_4
shr_n3:
  ADD $0 #2047
shr_4:
  SUB $0 #1023
  JLEQ $0 shr_n4
  ADD $1 #512
  JMP shr_5
shr_n4:
  ADD $0 #1023
shr_5:
  SUB $0 #511
  JLEQ $0 shr_n5
  ADD $1 #256
  JMP shr_6
shr_n5:
  ADD $0 #511
shr_6:
  SUB $0 #255
  JLEQ $0 shr_n6
  ADD $1 #128
  JMP shr_7
shr_n6:
  ADD $0 #255
shr_7:
  SUB $0 #127
  JLEQ $0 shr_n7
  ADD $1 #64
  JMP shr_8
shr_n7:
  ADD $0 #127
shr_8:
  SUB $0 #63
  JLEQ $0 shr_n8
  ADD $1 #32
  JMP shr_9
shr_n8:
  ADD $0 #63
shr_9:
  SUB $0 #31
  JLEQ $0 shr_n9
  ADD $1 #16
  JMP shr_10
shr_n9:
  ADD $0 #31
shr_10:
  SUB $0 #15
  JLEQ $0 shr_n10
  ADD $1 #8
  JMP shr_11
shr_n10:
  ADD $0 #15
shr_11:
  SUB $0 #7
  JLEQ $0 shr_n11
  ADD $1 #4
  JMP shr_12
shr_n11:
  ADD $0 #7
shr_12:
  SUB $0 #3
  JLEQ $0 shr_n12
  ADD $1 #2
  JMP shr_13
shr_n12:
  ADD $0 #3
shr_13:
  SUB $0 #1
  JLEQ $0 shr_n13
  ADD $1 #1
shr_n13:
  MOV $0 $1
  RET

SECTION lookup
lookatan:atandata
atandata:
25735 15192 8027 4074 2045 1023 511 255 127 63 31 15 7 3 1
lookcork: corkdata
corkdata:
23170 20724 20105 19950 19911 19901 19899 19898 19898 19898 19898 19898 19898 19898 19898

SECTION sreg[@65533]
  65503 1
