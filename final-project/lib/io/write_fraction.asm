 
/* io/write_integer.asm
 * Print a decimal representation of a fraction argument to stdout
 * 
 */


 /*
 WRITE_FRACTION:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R1);
  MOV(R0, FPARG(0));
  CMP(R0, IMM(0));
  JUMP_EQ(L_WI_01);
  JUMP_LT(L_WI_N1);
  PUSH(R0);
  CALL(L_WI_LOOP1);
  POP(R1);
  JUMP(L_WI_EX1);

 L_WI_LOOP1:
  
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  CMP(R0, IMM(0));
  JUMP_EQ(L_WI_LOOP_END1);
  REM(R0, IMM(10));
  PUSH(R0);
  MOV(R0, FPARG(0));
  DIV(R0, IMM(10));
  PUSH(R0);
  CALL(L_WI_LOOP1);
  POP(R0);
  POP(R0);
  ADD(R0, IMM('0'));
  PUSH(R0);
  CALL(PUTCHAR);
  POP(R0);

 L_WI_LOOP_END1:
  
  POP(FP);
  RETURN;

 L_WI_N1:

  PUSH(IMM('-'));
  CALL(PUTCHAR);
  POP(R1);
  MOV(R0, FPARG(0));
  MOV(R1, IMM(0));
  SUB(R1, R0);
  PUSH(R1);
  CALL(WRITE_FRACTION);
  POP(R1);
  JUMP(L_WI_EX1);
 
 L_WI_01:

  PUSH(IMM('0'));
  CALL(PUTCHAR);
  POP(R0);
  JUMP(L_WI_EX1);
 
 L_WI_EX1:

  POP(R1);
  POP(FP);
  RETURN;
  */