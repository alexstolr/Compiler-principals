#include <stdio.h>
#include <stdlib.h>
#include <string.h> /* TODO DO I NEED THIS? */
#include "cisc.h"

#define SIZE_OF_SYMBOL_TABLE 0
#define SIZE_OF_CONSTABLE 15
#define SIZE_OF_TABLES 67  // size of const table + var free table
#define FREE_ADDRESS 1067  // size of const table + var free table
#define DO_SHOW 1

//#define SCMENV (FPARG(0))
//#define SCMNARGS (FPARG(1))
//#define SCMARG(n) (FPARG(n+2)) //n'th argument

int main() 
{
    START_MACHINE;
    PUSH(IMM(0)); //GLOBAL ENV
    PUSH(IMM(0));
    PUSH(FP);
  JUMP(CONTINUE);
  #include "char.lib"
  #include "io.lib"
  #include "math.lib"
  #include "string.lib"
  #include "system.lib"
  #include "scheme.lib"/*  (* UNCOMMENT *) more_code */
  #include "debug_macros.h"   

CONTINUE:;
  long mem_init[SIZE_OF_CONSTABLE]={T_VOID,T_NIL,T_BOOL,1,T_BOOL,0,T_INTEGER,1,T_INTEGER,0,T_INTEGER,2,T_LINK,-1,1001};
  memcpy(&IND(1000),mem_init,SIZE_OF_CONSTABLE*sizeof(long));
  PUSH(IMM(FREE_ADDRESS)); // TODO ORIAN - I CHANGED FROM SIZE_OF_TABLES
  MOV(R8,IMM(1012)) // 0
  CALL(MALLOC);
  DROP(1);/*
 	MOV(ADDR(1000), IMM(T_VOID));
	MOV(ADDR(1001), IMM(T_NIL));
	MOV(ADDR(1002), IMM(T_BOOL));
	MOV(ADDR(1003), IMM(1));
	MOV(ADDR(1004), IMM(T_BOOL));
	MOV(ADDR(1005), IMM(0));
 	MOV(ADDR(1006), T_INTEGER);
	MOV(ADDR(1007), 1);
	MOV(ADDR(1008), T_INTEGER);
	MOV(ADDR(1009), 0);
	MOV(ADDR(1010), T_INTEGER);
	MOV(ADDR(1011), 2);
*/
	MOV(IND(1015), T_UNDEFINED);
	MOV(IND(1016), T_UNDEFINED);
	MOV(IND(1017), T_UNDEFINED);
	MOV(IND(1018), T_UNDEFINED);
	MOV(IND(1019), T_UNDEFINED);
	MOV(IND(1020), T_UNDEFINED);
	MOV(IND(1021), T_UNDEFINED);
	MOV(IND(1022), T_UNDEFINED);
	MOV(IND(1023), T_UNDEFINED);
	MOV(IND(1024), T_UNDEFINED);
	MOV(IND(1025), T_UNDEFINED);
	MOV(IND(1026), T_UNDEFINED);
	MOV(IND(1027), T_UNDEFINED);
	MOV(IND(1028), T_UNDEFINED);
	MOV(IND(1029), T_UNDEFINED);
	MOV(IND(1030), T_UNDEFINED);
	MOV(IND(1031), T_UNDEFINED);
	MOV(IND(1032), T_UNDEFINED);
	MOV(IND(1033), T_UNDEFINED);
	MOV(IND(1034), T_UNDEFINED);
	MOV(IND(1035), T_UNDEFINED);
	MOV(IND(1036), T_UNDEFINED);
	MOV(IND(1037), T_UNDEFINED);
	MOV(IND(1038), T_UNDEFINED);
	MOV(IND(1039), T_UNDEFINED);
	MOV(IND(1040), T_UNDEFINED);
	MOV(IND(1041), T_UNDEFINED);
	MOV(IND(1042), T_UNDEFINED);
	MOV(IND(1043), T_UNDEFINED);
	MOV(IND(1044), T_UNDEFINED);
	MOV(IND(1045), T_UNDEFINED);
	MOV(IND(1046), T_UNDEFINED);
	MOV(IND(1047), T_UNDEFINED);
	MOV(IND(1048), T_UNDEFINED);
	MOV(IND(1049), T_UNDEFINED);
	MOV(IND(1050), T_UNDEFINED);
	MOV(IND(1051), T_UNDEFINED);
	MOV(IND(1052), T_UNDEFINED);
	MOV(IND(1053), T_UNDEFINED);
	MOV(IND(1054), T_UNDEFINED);
	MOV(IND(1055), T_UNDEFINED);
	MOV(IND(1056), T_UNDEFINED);
	MOV(IND(1057), T_UNDEFINED);
	MOV(IND(1058), T_UNDEFINED);
	MOV(IND(1059), T_UNDEFINED);
	MOV(IND(1060), T_UNDEFINED);
	MOV(IND(1061), T_UNDEFINED);
	MOV(IND(1062), T_UNDEFINED);
	MOV(IND(1063), T_UNDEFINED);
	MOV(IND(1064), T_UNDEFINED);
	MOV(IND(1065), T_UNDEFINED);
	MOV(IND(1066), T_UNDEFINED);

	/* ++++++++++++ apply ++++++++++++ */

	JUMP(L_cont_43);

L_prim_apply:
	PUSH(FP);
	MOV(FP, SP);
	PUSH(R1);
	PUSH(R2);
	PUSH(R3);
	MOV(R0,FPARG(2));
	MOV(R1,FPARG(3));
	MOV(R2,IMM(0));
	MOV(R3,R2);
L_loop_code_56:
	CMP(R1,1001);
	JUMP_EQ(L_loop_code_55);
	PUSH(INDD(R1,1));
	MOV(R1,INDD(R1,2));
	ADD(R3, 1);
	JUMP(L_loop_code_56);
L_loop_code_55:
	CMP(R2, R3);
	JUMP_GE(L_loop_end_55);
	SUB(R2,1);
	SUB(R3,2);
	MOV(R1,STARG(R2));
	MOV(STARG(R2),STARG(R3));
	MOV(STARG(R3),R1);
	ADD(R2,IMM(2));
	ADD(R3,IMM(1));
	JUMP(L_loop_code_55);
L_loop_end_55:
	MOV(R1,SP);
	SUB(R1,IMM(3));
	SUB(R1,FP);
	PUSH(R1);
	PUSH(IMM(0));
	CALLA(INDD(R0,2));
	MOV(R3,IMM(2));
	ADD(R3,STARG(0));
	DROP(R3);
L_loop_end_56:
	POP(R3);
	POP(R2);
	POP(R1);
	POP(FP);
	RETURN;

L_cont_43:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_apply))); // address of Label

	MOV(IND(1051),R0);
	/* ------------ apply ------------ */
	/* ++++++++++++ eq? ++++++++++++ */

	JUMP(L_cont_42);

L_prim_rts_isEqual:
	PUSH(FP);
	MOV(FP, SP);

MOV(R1,FPARG(2));
MOV(R2,FPARG(3));
CMP(IND(R1),IMM(T_FRACTION));
JUMP_EQ(L_prim_rts_isEqual_compare_fraction);
CMP(IND(R1),IMM(T_BOOL));
JUMP_EQ(L_prim_rts_isEqual_compare_field);
CMP(IND(R1),IMM(T_NIL));
JUMP_EQ(L_prim_rts_isEqual_compare_field);
CMP(IND(R1),IMM(T_VOID));
JUMP_EQ(L_prim_rts_isEqual_compare_field);
CMP(IND(R1),IMM(T_INTEGER));
JUMP_EQ(L_prim_rts_isEqual_compare_field);
CMP(IND(R1),IMM(T_CHAR));
JUMP_EQ(L_prim_rts_isEqual_compare_field);
CMP(IND(R1),IMM(T_SYMBOL));
JUMP_EQ(L_prim_rts_isEqual_compare_field);
CMP(R1,R2);
JUMP_NE(L_prim_rts_isEqual_false);
MOV(R0,1002);
JUMP(L_prim_rts_isEqual_exit);

L_prim_rts_isEqual_compare_fraction:
CMP(INDD(R1,1),INDD(R2,1));
JUMP_NE(L_prim_rts_isEqual_false);
CMP(INDD(R1,2),INDD(R2,2));
JUMP_NE(L_prim_rts_isEqual_false);
MOV(R0,1002);
JUMP(L_prim_rts_isEqual_exit);

L_prim_rts_isEqual_compare_field:
CMP(INDD(R1,1),INDD(R2,1));
JUMP_NE(L_prim_rts_isEqual_false);
MOV(R0,1002);
JUMP(L_prim_rts_isEqual_exit);

L_prim_rts_isEqual_false:
MOV(R0,1004);

L_prim_rts_isEqual_exit:
  	POP(FP);
	RETURN;

L_cont_42:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_rts_isEqual))); // address of Label

	MOV(IND(1024),R0);
	/* ------------ eq? ------------ */
	/* ++++++++++++ symbol->string ++++++++++++ */

	JUMP(L_cont_41);

L_prim_rts_symbol_to_string:
	PUSH(FP);
	MOV(FP, SP);
    MOV(R0,INDD(FPARG(2),1));
  	POP(FP);
	RETURN;

L_cont_41:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_rts_symbol_to_string))); // address of Label

	MOV(IND(1043),R0);
	/* ------------ symbol->string ------------ */
	/* ++++++++++++ string->symbol ++++++++++++ */

	JUMP(L_cont_40);

L_prim_rts_string_to_symbol:
	PUSH(FP);
	MOV(FP, SP);
   MOV(R0,IMM(-1));
    CMP(INDD(R8,IMM(1)),R0); // check if linked list is empty
    JUMP_EQ(L_list_is_empty);
    // not empty -> run on all links and search for string
    MOV(R0,INDD(FPARG(2),1)); // length of input string.
L_search_in_links_loop:
    MOV(R1,INDD(R8,IMM(1))); // pointer to symbol in current link.
    MOV(R1,INDD(R1,IMM(1)));
    CMP(R0,INDD(R1,IMM(1))); // compare length of input string to lentgh of string in link.
    JUMP_EQ(L_sts_compare_chars); // if length is equal, compare each char.
    ADD(R8,IMM(3));
    CMP(INDD(R8,IMM(-1)),1001); // last link
    JUMP_NE(L_search_in_links_loop);
    JUMP(L_list_is_empty);

L_sts_compare_chars:
  MOV(R2,FPARG(2)); // R2 = POINTER TO INPUT T_STRING
  ADD(R2,IMM(2));
  ADD(R1,IMM(2));
  MOV(R3,IMM(0)); // i = 0

L_sts_compare_chars_loop_start:
  CMP(R3,R0);
  JUMP_GE(L_sts_compare_chars_loop_end);
  CMP(R1,R2);
  JUMP_NE(L_sts_compare_chars_loop_break);
  ADD(R3,IMM(1));
  ADD(R1,IMM(1));
  ADD(R2,IMM(1));
  JUMP(L_sts_compare_chars_loop_start);

//found string (all compared chars are equal)
L_sts_compare_chars_loop_end:
  MOV(R0,INDD(R8,1));
  JUMP(L_exit_string_to_symbol);
// not found - some chars arent equal
L_sts_compare_chars_loop_break:
    ADD(R8,IMM(3));
    JUMP(L_search_in_links_loop);;

  // creates a new symbol - FINISHED
L_list_is_empty:
    PUSH(2);
    CALL(MALLOC);
    DROP(1);
    MOV(IND(R0),T_SYMBOL);
    MOV(INDD(R0,1),FPARG(2)); // pointer to t_string

L_exit_string_to_symbol:	POP(FP);
	RETURN;

L_cont_40:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_rts_string_to_symbol))); // address of Label

	MOV(IND(1040),R0);
	/* ------------ string->symbol ------------ */
	/* ++++++++++++ vector-set! ++++++++++++ */

	JUMP(L_cont_39);

L_prim_rts_vector_set_bang:
	PUSH(FP);
	MOV(FP, SP);
   // check if index is larger then vector length or smaller then 0
    MOV(R0,IMM(0)); // R0 = 0
    MOV(R1,INDD(FPARG(3),1)); // index of char to be changed
    MOV(R2,INDD(FPARG(2),1)); // LENGTH OF STRING
    CMP(R1,R0);
    JUMP_LT(L_exit_vector_set_bang);
    CMP(R1,R2);
    JUMP_GE(L_exit_vector_set_bang);
    MOV(R0,FPARG(2)); // pointer to vector
    ADD(R1,IMM(2));
    MOV(R2,FPARG(4)); //  char to be put instead of previous char
    MOV(INDD(R0,R1),R2);
    MOV(R0,1000);
    POP(FP);
    RETURN;

L_exit_vector_set_bang:
MOV(R0,1000);
	POP(FP);
	RETURN;

L_cont_39:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_rts_vector_set_bang))); // address of Label

	MOV(IND(1047),R0);
	/* ------------ vector-set! ------------ */
	/* ++++++++++++ string-set! ++++++++++++ */

	JUMP(L_cont_38);

L_prim_rts_string_set_bang:
	PUSH(FP);
	MOV(FP, SP);
   // check if index is larger then string length or smaller then 0
    MOV(R0,IMM(0)); // R0 = 0
    MOV(R1,INDD(FPARG(3),1)); // index of char to be changed
    MOV(R2,INDD(FPARG(2),1)); // LENGTH OF STRING
    CMP(R1,R0);
    JUMP_LT(L_exit_string_set_bang);
    CMP(R1,R2);
    JUMP_GE(L_exit_string_set_bang);
    MOV(R0,FPARG(2)); // pointer to string
    ADD(R1,IMM(2));
    MOV(R2,INDD(FPARG(4),1)); //  char to be put instead of previous char
    MOV(INDD(R0,R1),R2);
    MOV(R0,1000);
    POP(FP);
    RETURN;

L_exit_string_set_bang:
MOV(R0,1000);
	POP(FP);
	RETURN;

L_cont_38:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_rts_string_set_bang))); // address of Label

	MOV(IND(1039),R0);
	/* ------------ string-set! ------------ */
	/* ++++++++++++ vector-ref ++++++++++++ */

	JUMP(L_cont_37);

L_prim_vector_ref:
	PUSH(FP);
	MOV(FP, SP);
    MOV(R0,INDD(FPARG(3),1)); // r0  = char at n. 
    MOV(R1,INDD(FPARG(2),1)); // r1 = length of vector
    CMP(R0,R1); // if (char at n) >= length of vector then exit.
    JUMP_GE(L_vector_ref_exit);
    CMP(R0,IMM(0)); // if (char at n) < 0 then exit.
    JUMP_LT(L_vector_ref_exit);
    CMP(R0,IMM(0)); // if (char at n) == 0 then exit.
    JUMP_EQ(L_vector_ref_goto_zero);
    MOV(R1,IMM(0));

L_vector_ref_start_loop:
    CMP(R1,R0);
    JUMP_GE(L_vector_ref_exit_loop);
    PUSH(INDD(FPARG(2),R1+2));
    INCR(R1);
    JUMP(L_vector_ref_start_loop);

L_vector_ref_exit_loop:
    MOV(R0,INDD(FPARG(2),R1+2));
    DROP(R1);
    POP(FP);
    RETURN;

L_vector_ref_goto_zero:
    MOV(R0,INDD(FPARG(2),2));
    POP(FP);
    RETURN;

L_vector_ref_exit:
    //enteres here only in case n < 0. which will never happend.
	POP(FP);
	RETURN;

L_cont_37:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_vector_ref))); // address of Label

	MOV(IND(1046),R0);
	/* ------------ vector-ref ------------ */
	/* ++++++++++++ string-ref ++++++++++++ */

	JUMP(L_cont_36);

L_prim_string_ref:
	PUSH(FP);
	MOV(FP, SP);
    MOV(R0,INDD(FPARG(3),1)); // r0  = char at n. 
    MOV(R1,INDD(FPARG(2),1)); // r1 = length of string
    CMP(R0,R1); // if (char at n) >= length of string then exit.
    JUMP_GE(L_string_ref_exit);
    CMP(R0,IMM(0)); // if (char at n) < 0 then exit.
    JUMP_LT(L_string_ref_exit);
    CMP(R0,IMM(0)); // if (char at n) == 0 then exit.
    JUMP_EQ(L_string_ref_goto_zero);
    MOV(R1,IMM(0));

L_string_ref_start_loop:
    CMP(R1,R0);
    JUMP_GE(L_string_ref_exit_loop);
    PUSH(INDD(FPARG(2),R1+2));
    INCR(R1);
    JUMP(L_string_ref_start_loop);

L_string_ref_exit_loop:
    PUSH(INDD(FPARG(2),R1+2));
    INCR(R1);
    CALL(MAKE_SOB_CHAR);
    DROP(R1);
    POP(FP);
    RETURN;

L_string_ref_goto_zero:
    PUSH(INDD(FPARG(2),2));
    //PUSH(1);
    CALL(MAKE_SOB_CHAR);
    DROP(1);
    POP(FP);
    RETURN;

L_string_ref_exit:
    //TODO  not sure if need to do MOV(R0,IMM(1));
    PUSH(IMM(0));
    CALL(MAKE_SOB_CHAR);
    DROP(1);
	POP(FP);
	RETURN;

L_cont_36:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_string_ref))); // address of Label

	MOV(IND(1038),R0);
	/* ------------ string-ref ------------ */
	/* ++++++++++++ vector ++++++++++++ */

	JUMP(L_cont_35);

L_prim_vector:
	PUSH(FP);
	MOV(FP, SP);
    MOV(R0,FPARG(1)); // r0  = num of args. 
    MOV(R1,IMM(0));// r1 = 0 (start) . incrementing r1 in loop.
L_vector_start_loop:    
    CMP(R1,R0); // while (r1 < n) do:
    JUMP_GE(L_vector_exit_loop);
    MOV(R2,R1);
    ADD(R2,IMM(2));
    PUSH(FPARG(R2));
    INCR(R1);
    JUMP(L_vector_start_loop);

L_vector_exit_loop:
    PUSH(R0);
    ADD(R1,IMM(1));
    CALL(MAKE_SOB_VECTOR);
    DROP(R1);
	POP(FP);
	RETURN;

L_cont_35:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_vector))); // address of Label

	MOV(IND(1044),R0);
	/* ------------ vector ------------ */
	/* ++++++++++++ > ++++++++++++ */

	JUMP(L_cont_34);

L_prim_greater_then:
	PUSH(FP);
	MOV(FP, SP);
	MOV(R0, 1); 
CMP(IND(FPARG(2)), T_FRACTION);
JUMP_NE(L_prim_greater_then_not_fraction);
	MOV(R1, INDD(FPARG(2), 1));
	MOV(R2, INDD(FPARG(2), 2));
	JUMP(L_loop_code_54);
L_prim_greater_then_not_fraction:
	MOV(R1, INDD(FPARG(2), 1));
	MOV(R2, 1);
L_loop_code_54:
	CMP(R0, FPARG(1));
	JUMP_GE(L_loop_end_54);
		MOV(R3, R0);
		ADD(R3, 2);
	CMP(IND(FPARG(R3)), T_FRACTION);
	JUMP_NE(L_prim_greater_then_loop_not_fraction_start);
		MOV(R4, INDD(FPARG(R3), 1));
		MUL(R4, R2);
		MOV(R5, INDD(FPARG(R3), 2));
		MUL(R5, R1);
		CMP(R5, R4);
		JUMP_LE(L_prim_greater_then_false);
		MOV(R1, INDD(FPARG(R3), 1));
		MOV(R2, INDD(FPARG(R3), 2));
		JUMP(L_prim_greater_then_loop_not_fraction_end);
L_prim_greater_then_loop_not_fraction_start:
		MOV(R4, INDD(FPARG(R3),1));
		MUL(R4, R2);
		CMP(R1, R4);
		JUMP_LE(L_prim_greater_then_false);
		MOV(R1, R4);
L_prim_greater_then_loop_not_fraction_end:
		ADD(R0, 1);
		JUMP(L_loop_code_54);
L_loop_end_54:
	MOV(R0, 1002);
	JUMP(L_prim_greater_then_end);
L_prim_greater_then_false:
	MOV(R0, 1004);
L_prim_greater_then_end:	POP(FP);
	RETURN;

L_cont_34:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_greater_then))); // address of Label

	MOV(IND(1052),R0);
	/* ------------ > ------------ */
	/* ++++++++++++ < ++++++++++++ */

	JUMP(L_cont_33);

L_prim_less_then:
	PUSH(FP);
	MOV(FP, SP);
	MOV(R0, 1); 
	CMP(IND(FPARG(2)), T_FRACTION);
	JUMP_NE(L_prim_less_then_not_fraction);
	MOV(R1, INDD(FPARG(2), 1));
	MOV(R2, INDD(FPARG(2), 2));
	JUMP(L_loop_code_53);
L_prim_less_then_not_fraction:
	MOV(R1, INDD(FPARG(2), 1));
	MOV(R2, 1);
L_loop_code_53:
	CMP(R0, FPARG(1));
	JUMP_GE(L_loop_end_53);
		MOV(R3, R0);
		ADD(R3, 2);
	CMP(IND(FPARG(R3)), T_FRACTION);
	JUMP_NE(L_prim_less_then_loop_not_fraction_start);
		MOV(R4, INDD(FPARG(R3), 1));
		MUL(R4, R2);
		MOV(R5, INDD(FPARG(R3), 2));
		MUL(R5, R1);
		CMP(R5, R4);
		JUMP_GE(L_prim_less_then_false);
		MOV(R1, INDD(FPARG(R3), 1));
		MOV(R2, INDD(FPARG(R3), 2));
		JUMP(L_prim_less_then_loop_not_fraction_end);
L_prim_less_then_loop_not_fraction_start:
		MOV(R4, INDD(FPARG(R3),1));
		MUL(R4, R2);
		CMP(R1, R4);
		JUMP_GE(L_prim_less_then_false);
		MOV(R1, R4);
L_prim_less_then_loop_not_fraction_end:
		ADD(R0, 1);
		JUMP(L_loop_code_53);
L_loop_end_53:
	MOV(R0, 1002);
	JUMP(L_prim_less_then_end);
L_prim_less_then_false:
	MOV(R0, 1004);
L_prim_less_then_end:	POP(FP);
	RETURN;

L_cont_33:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_less_then))); // address of Label

	MOV(IND(1015),R0);
	/* ------------ < ------------ */
	/* ++++++++++++ = ++++++++++++ */

	JUMP(L_cont_32);

L_prim_equal:
	PUSH(FP);
	MOV(FP, SP);
	MOV(R0, 1); 
CMP(IND(FPARG(2)), T_FRACTION);
JUMP_NE(L_prim_equal_not_fraction);
	MOV(R1, INDD(FPARG(2), 1));
	MOV(R2, INDD(FPARG(2), 2));
	JUMP(L_loop_code_52);
L_prim_equal_not_fraction:
	MOV(R1, INDD(FPARG(2), 1));
	MOV(R2, 1);
L_loop_code_52:
	CMP(R0, FPARG(1));
	JUMP_GE(L_loop_end_52);
		MOV(R3, R0);
		ADD(R3, 2);
	CMP(IND(FPARG(R3)), T_FRACTION);
	JUMP_NE(L_prim_equal_loop_not_fraction_start);
		CMP(R1, INDD(FPARG(R3), 1));
		JUMP_NE(L_prim_equal_false);
		CMP(R2, INDD(FPARG(R3), 2));
		JUMP_NE(L_prim_equal_false);
		JUMP(L_prim_equal_loop_not_fraction_end);
L_prim_equal_loop_not_fraction_start:
		CMP(R1, INDD(FPARG(R3), 1));
		JUMP_NE(L_prim_equal_false);
		CMP(R2, 1);
		JUMP_NE(L_prim_equal_false);
L_prim_equal_loop_not_fraction_end:
		ADD(R0, 1);
		JUMP(L_loop_code_52);
L_loop_end_52:
	MOV(R0, 1002);
	JUMP(L_prim_equal_end);
L_prim_equal_false:
	MOV(R0, 1004);
L_prim_equal_end:	POP(FP);
	RETURN;

L_cont_32:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_equal))); // address of Label

	MOV(IND(1062),R0);
	/* ------------ = ------------ */
	/* ++++++++++++ * ++++++++++++ */

	JUMP(L_cont_31);

L_prim_mul:
	PUSH(FP);
	MOV(FP, SP);
	MOV(R0, 0); 
	MOV(R1, 1);
	MOV(R2, 1);
L_loop_code_51:
	CMP(R0, FPARG(1));
	JUMP_GE(L_loop_end_51);
		MOV(R3, R0);
		ADD(R3, 2);
		CMP(IND(FPARG(R3)), T_FRACTION);
		JUMP_NE(nL_prim_mul_loop_not_fraction_start);
		MUL(R1, INDD(FPARG(R3), 1));
		MUL(R2, INDD(FPARG(R3), 2));
		JUMP(nL_prim_mul_loop_not_fraction_end);
nL_prim_mul_loop_not_fraction_start:
		MUL(R1, INDD(FPARG(R3), 1));
nL_prim_mul_loop_not_fraction_end:
		ADD(R0, 1);
		JUMP(L_loop_code_51);
L_loop_end_51:
	PUSH(R2);
	PUSH(R1);
	CALL(MAKE_SOB_FRACTION);
	DROP(2);
	POP(FP);
	RETURN;

L_cont_31:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_mul))); // address of Label

	MOV(IND(1018), R0);
	/* ------------ * ------------ */
	/* ++++++++++++ - ++++++++++++ */

	JUMP(L_cont_30);

L_prim_minus:
	PUSH(FP);
	MOV(FP, SP);
	MOV(R0, 1);
	CMP(FPARG(1), 1);
	JUMP_NE(L_prim_minus_lot_params);
	CMP(IND(FPARG(2)), T_FRACTION);
	JUMP_NE(L_prim_minus_one_param_not_fraction);
	MOV(R1, INDD(FPARG(2), 1));
	MUL(R1, -1);
	MOV(R2, INDD(FPARG(2), 2));
JUMP(L_loop_end_50);
L_prim_minus_one_param_not_fraction:
	MOV(R1, INDD(FPARG(2), 1));
	MUL(R1, -1);
	MOV(R2, 1);
JUMP(L_loop_end_50);
L_prim_minus_lot_params:
	CMP(IND(FPARG(2)), T_FRACTION);
	JUMP_NE(L_prim_minus_not_fraction);
	MOV(R1, INDD(FPARG(2), 1));
	MOV(R2, INDD(FPARG(2), 2));
	JUMP(L_loop_code_50);
L_prim_minus_not_fraction:
	MOV(R1, INDD(FPARG(2), 1));
	MOV(R2, 1);
L_loop_code_50:
	CMP(R0, FPARG(1));
	JUMP_GE(L_loop_end_50);
		MOV(R3, R0);
		ADD(R3, 2);
		CMP(IND(FPARG(R3)), T_FRACTION);
		JUMP_NE(L_prim_minus_loop_not_fraction_start);
		MUL(R1, INDD(FPARG(R3), 2));
		MUL(R2, INDD(FPARG(R3), 1));
		SUB(R1, R2);
		DIV(R2, INDD(FPARG(R3), 1));
		MUL(R2, INDD(FPARG(R3), 2));
		JUMP(L_prim_minus_loop_not_fraction_end);
L_prim_minus_loop_not_fraction_start:
		MOV(R4,R2);
		MUL(R4, INDD(FPARG(R3), 1));
		SUB(R1, R4);
L_prim_minus_loop_not_fraction_end:
		ADD(R0, 1);
		JUMP(L_loop_code_50);
L_loop_end_50:
	PUSH(R2);
	PUSH(R1);
	CALL(MAKE_SOB_FRACTION);
	DROP(2);
	POP(FP);
	RETURN;

L_cont_30:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_minus))); // address of Label

	MOV(IND(1019), R0);
	/* ------------ - ------------ */
	/* ++++++++++++ + ++++++++++++ */

	JUMP(L_cont_29);

L_prim_plus:
	PUSH(FP);
	MOV(FP, SP);
	CMP(IMM(0), FPARG(1));
	JUMP_NE(L_prim_plus_with_params);
	MOV(R1, 1);
	MOV(R2, 1);
	JUMP(L_loop_end_49);
L_prim_plus_with_params:
	MOV(R0, 1);
	CMP(IND(FPARG(2)), T_FRACTION);
	JUMP_NE(L_prim_plus_not_fraction);
	MOV(R1, INDD(FPARG(2), 1));
	MOV(R2, INDD(FPARG(2), 2));
	JUMP(L_loop_code_49);
L_prim_plus_not_fraction:
	MOV(R1, INDD(FPARG(2), 1));
	MOV(R2, 1);
L_loop_code_49:
	CMP(R0, FPARG(1));
	JUMP_GE(L_loop_end_49);
		MOV(R3, R0);
		ADD(R3, 2);
		CMP(IND(FPARG(R3)), T_FRACTION);
		JUMP_NE(L_prim_plus_loop_not_fraction_start);
		MUL(R1, INDD(FPARG(R3), 2));
		MUL(R2, INDD(FPARG(R3), 1));
		ADD(R1, R2);
		DIV(R2, INDD(FPARG(R3), 1));
		MUL(R2, INDD(FPARG(R3), 2));
		JUMP(L_prim_plus_loop_not_fraction_end);
L_prim_plus_loop_not_fraction_start:
		MOV(R4,R2);
		MUL(R4, INDD(FPARG(R3), 1));
		ADD(R1, R4);
L_prim_plus_loop_not_fraction_end:
		ADD(R0, 1);
		JUMP(L_loop_code_49);
L_loop_end_49:
	PUSH(R2);
	PUSH(R1);
	CALL(MAKE_SOB_FRACTION);
	DROP(2);
	POP(FP);
	RETURN;

L_cont_29:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_plus))); // address of Label

	MOV(IND(1016),R0);
	/* ------------ + ------------ */
	/* ++++++++++++ make-vector ++++++++++++ */

	JUMP(L_cont_28);

L_prim_make_vector:
	PUSH(FP);
	MOV(FP, SP);
    PUSH(IMM(0));
    CALL(MAKE_SOB_INTEGER);
    DROP(1);
    MOV(R2,INDD(FPARG(2),1)); // RO = n
    CMP(R2,IMM(0)); // if n == 0 creates empty vector #()
    JUMP_LE(L_make_vector_num_neg);
    MOV(R1,IMM(0)); // int i = 0
L_make_vector_loop:
    CMP(R1,R2);
    JUMP_GE(L_end_make_vector_loop);
    CMP(FPARG(1),IMM(1)); // if got only one function argument --> push zeroes.
    JUMP_EQ(L_make_vector_of_zeroes)
    PUSH(FPARG(3));
    INCR(R1);
    JUMP(L_make_vector_loop);


L_make_vector_of_zeroes:
    PUSH(R0);
    INCR(R1);
    JUMP(L_make_vector_loop);

L_end_make_vector_loop:
    PUSH(R2);
    MOV(R1,R2);
    ADD(R1,IMM(1));
    CALL(MAKE_SOB_VECTOR);
    DROP(R1);
    POP(FP);
    RETURN;


L_make_vector_num_neg:
    MOV(R0,IMM(1));
    PUSH(IMM(0));
    CALL(MAKE_SOB_VECTOR);
    DROP(1);
	POP(FP);
	RETURN;

L_cont_28:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_make_vector))); // address of Label

	MOV(IND(1028),R0);
	/* ------------ make-vector ------------ */
	/* ++++++++++++ make-string ++++++++++++ */

	JUMP(L_cont_27);

L_prim_make_string:
	PUSH(FP);
	MOV(FP, SP);
    MOV(R0,INDD(FPARG(2),1)); // RO = n
    CMP(R0,IMM(0));
    JUMP_LE(L_make_string_num_neg);
    MOV(R1,IMM(0)); // int i = 0
L_make_string_loop:
    CMP(R1,R0);
    JUMP_GE(L_end_make_string_loop);
    PUSH(INDD(FPARG(3),1));
    INCR(R1);
    JUMP(L_make_string_loop);

L_end_make_string_loop:
    PUSH(R0);
    ADD(R1,IMM(1));
    CALL(MAKE_SOB_STRING);
    DROP(R1);
    POP(FP);
    RETURN;

L_make_string_num_neg:
    MOV(R0,IMM(1));
    PUSH(IMM(0));
    CALL(MAKE_SOB_STRING);
    DROP(1);
	POP(FP);
	RETURN;

L_cont_27:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_make_string))); // address of Label

	MOV(IND(1027),R0);
	/* ------------ make-string ------------ */
	/* ++++++++++++ remainder ++++++++++++ */

	JUMP(L_cont_26);

L_prim_remainder:
	PUSH(FP);
	MOV(FP, SP);
	MOV(R0,INDD(FPARG(2),1));
	REM(R0,INDD(FPARG(3),1));
	PUSH(R0);
	CALL(MAKE_SOB_INTEGER);
	DROP(1);
	POP(FP);
	RETURN;

L_cont_26:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_remainder))); // address of Label

	MOV(IND(1034),R0);
	/* ------------ remainder ------------ */
	/* ++++++++++++ numerator ++++++++++++ */

	JUMP(L_cont_25);

L_prim_numerator:
	PUSH(FP);
	MOV(FP, SP);
	MOV(R0,(FPARG(2)));
	CMP(IND(R0),T_INTEGER);
	JUMP_EQ(L_num_integer_eq);
	MOV(R0,INDD(FPARG(2),1));
	PUSH(R0);
	CALL(MAKE_SOB_INTEGER);
	DROP(1);
	POP(FP);
	RETURN;


L_num_integer_eq:
    MOV(R0,INDD(R0,1));
    PUSH(R0);
    CALL(MAKE_SOB_INTEGER);
    DROP(1);
    POP(FP);
    RETURN;

L_cont_25:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_numerator))); // address of Label

	MOV(IND(1030),R0);
	/* ------------ numerator ------------ */
	/* ++++++++++++ denominator ++++++++++++ */

	JUMP(L_cont_24);

L_prim_denominator:
	PUSH(FP);
	MOV(FP, SP);
	MOV(R0,IND(FPARG(2)));
	CMP(R0,T_INTEGER);
	JUMP_EQ(L_de_integer_eq);
	MOV(R0,INDD(FPARG(2),2));
	PUSH(R0);
	CALL(MAKE_SOB_INTEGER);
	DROP(1);
	POP(FP);
	RETURN;


L_de_integer_eq:
    MOV(R0,IMM(1));
    PUSH(R0);
    CALL(MAKE_SOB_INTEGER);
    DROP(1);
    POP(FP);
    RETURN;

L_cont_24:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_denominator))); // address of Label

	MOV(IND(1023),R0);
	/* ------------ denominator ------------ */
	/* ++++++++++++ / ++++++++++++ */

	JUMP(L_cont_23);

L_prim_div:
	PUSH(FP);
	MOV(FP, SP);
	CMP(FPARG(1),1);
	JUMP_EQ(L_loop_code_48);
	MOV(R0, 1); 
	CMP(IND(FPARG(2)), T_FRACTION);
	JUMP_NE(L_prim_div_not_fraction);
	MOV(R1, INDD(FPARG(2), 1));
	MOV(R2, INDD(FPARG(2), 2));
	JUMP(L_loop_code_47);
L_prim_div_not_fraction:
	MOV(R1, INDD(FPARG(2), 1));
	MOV(R2, 1);
L_loop_code_47:
	CMP(R0, FPARG(1));
	JUMP_GE(L_loop_end_47);
		MOV(R3, R0);
		ADD(R3, 2);
	CMP(IND(FPARG(R3)), T_FRACTION);
	JUMP_NE(L_prim_div_loop_not_fraction_start);
		MUL(R1, INDD(FPARG(R3), 2));
		MUL(R2, INDD(FPARG(R3), 1));
		JUMP(L_prim_div_loop_not_fraction_end);
L_prim_div_loop_not_fraction_start:
		MUL(R2, INDD(FPARG(R3), 1));
L_prim_div_loop_not_fraction_end:
		ADD(R0, 1);
		JUMP(L_loop_code_47);
L_loop_end_47:
	PUSH(R2);
	PUSH(R1);
	CALL(MAKE_SOB_FRACTION);
	DROP(2);
	JUMP(L_loop_end_48);
L_loop_code_48:
	CMP(IND(FPARG(2)), T_FRACTION);
	JUMP_NE(L_prim_div_not_fraction_special);
	MOV(R1, INDD(FPARG(2), 1));
	MOV(R2, INDD(FPARG(2), 2));
	JUMP(L_prim_div_not_fraction_special_end);
L_prim_div_not_fraction_special:
	MOV(R1, INDD(FPARG(2), 1));
	MOV(R2, 1);
L_prim_div_not_fraction_special_end:
	PUSH(R1);
	PUSH(R2);
	CALL(MAKE_SOB_FRACTION);
	DROP(2);
L_loop_end_48:

	POP(FP);
	RETURN;

L_cont_23:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_div))); // address of Label

	MOV(IND(1017), R0);
	/* ------------ / ------------ */
	/* ++++++++++++ integer->char ++++++++++++ */

	JUMP(L_cont_22);

L_prim_integer_to_char:
	PUSH(FP);
	MOV(FP, SP);
	MOV(R0,INDD(FPARG(2),1));
	PUSH(R0);
	CALL(MAKE_SOB_CHAR);
	DROP(1);
	POP(FP);
	RETURN;

L_cont_22:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_integer_to_char))); // address of Label

	MOV(IND(1026),R0);
	/* ------------ integer->char ------------ */
	/* ++++++++++++ vector-length ++++++++++++ */

	JUMP(L_cont_21);

L_prim_vector_length:
	PUSH(FP);
	MOV(FP, SP);
	MOV(R0,INDD(FPARG(2),1));
	PUSH(R0);
	CALL(MAKE_SOB_INTEGER);
	DROP(1);
	POP(FP);
	RETURN;

L_cont_21:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_vector_length))); // address of Label

	MOV(IND(1045),R0);
	/* ------------ vector-length ------------ */
	/* ++++++++++++ string-length ++++++++++++ */

	JUMP(L_cont_20);

L_prim_string_length:
	PUSH(FP);
	MOV(FP, SP);
	MOV(R0,INDD(FPARG(2),1));
	PUSH(R0);
	CALL(MAKE_SOB_INTEGER);
	DROP(1);
	POP(FP);
	RETURN;

L_cont_20:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_string_length))); // address of Label

	MOV(IND(1037),R0);
	/* ------------ string-length ------------ */
	/* ++++++++++++ char->integer ++++++++++++ */

	JUMP(L_cont_19);

L_prim_char_to_int:
	PUSH(FP);
	MOV(FP, SP);
	MOV(R0,INDD(FPARG(2),1));
	PUSH(R0);
	CALL(MAKE_SOB_INTEGER);
	DROP(1);
	POP(FP);
	RETURN;

L_cont_19:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_char_to_int))); // address of Label

	MOV(IND(1021),R0);
	/* ------------ char->integer ------------ */
	/* ++++++++++++ not ++++++++++++ */

	JUMP(L_cont_18);

L_prim_not:
	PUSH(FP);
	MOV(FP, SP);
	MOV(R0,FPARG(2));
	CMP(R0,IMM(1004));
	JUMP_EQ(L_not_ne);
	MOV(R0,IMM(1004));
	POP(FP);
	RETURN;

L_not_ne:
    MOV(R0,IMM(1002));
    POP(FP);
    RETURN;

L_cont_18:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_not))); // address of Label

	MOV(IND(1057),R0);
	/* ------------ not ------------ */
	/* ++++++++++++ set-cdr! ++++++++++++ */

	JUMP(L_cont_17);

L_prim_set_cdr:
	PUSH(FP);
	MOV(FP, SP);
	MOV(R0,FPARG(3));
	MOV(R1,FPARG(2));
	MOV(INDD(R1,2),R0);
	MOV(R0,1000);
	POP(FP);
	RETURN;

L_cont_17:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_set_cdr))); // address of Label

	MOV(IND(1036),R0);
	/* ------------ set-cdr! ------------ */
	/* ++++++++++++ set-car! ++++++++++++ */

	JUMP(L_cont_16);

L_prim_set_car:
	PUSH(FP);
	MOV(FP, SP);
	MOV(R0,FPARG(3));
	MOV(R1,FPARG(2));
	MOV(INDD(R1,1),R0);
	MOV(R0,1000);
	POP(FP);
	RETURN;

L_cont_16:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_set_car))); // address of Label

	MOV(IND(1035),R0);
	/* ------------ set-car! ------------ */
	/* ++++++++++++ zero? ++++++++++++ */

	JUMP(L_cont_15);

L_prim_isZero:
	PUSH(FP);
	MOV(FP, SP);
	MOV(R0, IND(FPARG(2)));
	CMP(R0,IMM(T_INTEGER));
	JUMP_NE(L_zero_ne);
	MOV(R0, INDD(FPARG(2),1));
	CMP(R0,IMM(0));
	JUMP_NE(L_zero_ne);
	MOV(R0,IMM(1002));
	POP(FP);
	RETURN;

L_zero_ne:
    MOV(R0,IMM(1004));
    POP(FP);
    RETURN;

L_cont_15:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_isZero))); // address of Label

	MOV(IND(1049),R0);
	/* ------------ zero? ------------ */
	/* ++++++++++++ procedure? ++++++++++++ */

	JUMP(L_cont_14);

L_prim_isProcedure:
	PUSH(FP);
	MOV(FP, SP);
	MOV(R0, IND(FPARG(2)));
	CMP(R0,IMM(T_CLOSURE));
	JUMP_NE(L_procedure_ne);
	MOV(R0,IMM(1002));
	POP(FP);
	RETURN;

L_procedure_ne:
    MOV(R0,IMM(1004));
    POP(FP);
    RETURN;

L_cont_14:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_isProcedure))); // address of Label

	MOV(IND(1032),R0);
	/* ------------ procedure? ------------ */
	/* ++++++++++++ vector? ++++++++++++ */

	JUMP(L_cont_13);

L_prim_isVector:
	PUSH(FP);
	MOV(FP, SP);
	MOV(R0, IND(FPARG(2)));
	CMP(R0,IMM(T_VECTOR));
	JUMP_NE(L_vector_ne);
	MOV(R0,IMM(1002));
	POP(FP);
	RETURN;

L_vector_ne:
    MOV(R0,IMM(1004));
    POP(FP);
    RETURN;

L_cont_13:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_isVector))); // address of Label

	MOV(IND(1048),R0);
	/* ------------ vector? ------------ */
	/* ++++++++++++ pair? ++++++++++++ */

	JUMP(L_cont_12);

L_prim_isPair:
	PUSH(FP);
	MOV(FP, SP);
	MOV(R0, IND(FPARG(2)));
	CMP(R0,IMM(T_PAIR));
	JUMP_NE(L_pair_ne);
	MOV(R0,IMM(1002));
	POP(FP);
	RETURN;

L_pair_ne:
    MOV(R0,IMM(1004));
    POP(FP);
    RETURN;

L_cont_12:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_isPair))); // address of Label

	MOV(IND(1031),R0);
	/* ------------ pair? ------------ */
	/* ++++++++++++ symbol? ++++++++++++ */

	JUMP(L_cont_11);

L_prim_isSymbol:
	PUSH(FP);
	MOV(FP, SP);
	MOV(R0, IND(FPARG(2)));
	CMP(R0,IMM(T_SYMBOL));
	JUMP_NE(L_symbol_ne);
	MOV(R0,IMM(1002));
	POP(FP);
	RETURN;

L_symbol_ne:
    MOV(R0,IMM(1004));
    POP(FP);
    RETURN;

L_cont_11:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_isSymbol))); // address of Label

	MOV(IND(1042),R0);
	/* ------------ symbol? ------------ */
	/* ++++++++++++ string? ++++++++++++ */

	JUMP(L_cont_10);

L_prim_isString:
	PUSH(FP);
	MOV(FP, SP);
	MOV(R0, IND(FPARG(2)));
	CMP(R0,IMM(T_STRING));
	JUMP_NE(L_string_ne);
	MOV(R0,IMM(1002));
	POP(FP);
	RETURN;

L_string_ne:
    MOV(R0,IMM(1004));
    POP(FP);
    RETURN;

L_cont_10:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_isString))); // address of Label

	MOV(IND(1041),R0);
	/* ------------ string? ------------ */
	/* ++++++++++++ char? ++++++++++++ */

	JUMP(L_cont_9);

L_prim_isChar:
	PUSH(FP);
	MOV(FP, SP);
	MOV(R0, IND(FPARG(2)));
	CMP(R0,IMM(T_CHAR));
	JUMP_NE(L_char_ne);
	MOV(R0,IMM(1002));
	POP(FP);
	RETURN;

L_char_ne:
    MOV(R0,IMM(1004));
    POP(FP);
    RETURN;

L_cont_9:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_isChar))); // address of Label

	MOV(IND(1022),R0);
	/* ------------ char? ------------ */
	/* ++++++++++++ rational? ++++++++++++ */

	JUMP(L_cont_8);

L_prim_isRational:
	PUSH(FP);
	MOV(FP, SP);
	MOV(R0, IND(FPARG(2)));
	CMP(R0,IMM(T_INTEGER));
	JUMP_EQ(L_rational_eq);
	CMP(R0,IMM(T_FRACTION));
	JUMP_EQ(L_rational_eq);
	MOV(R0,IMM(1004));
	POP(FP);
	RETURN;

L_rational_eq:
    MOV(R0,IMM(1002));
    POP(FP);
    RETURN;

L_cont_8:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_isRational))); // address of Label

	MOV(IND(1033),R0);
	/* ------------ rational? ------------ */
	/* ++++++++++++ number? ++++++++++++ */

	JUMP(L_cont_7);

L_prim_isNumber:
	PUSH(FP);
	MOV(FP, SP);
	MOV(R0, IND(FPARG(2)));
	CMP(R0,IMM(T_INTEGER));
	JUMP_EQ(L_number_eq);
	CMP(R0,IMM(T_FRACTION));
	JUMP_EQ(L_number_eq);
	MOV(R0,IMM(1004));
	POP(FP);
	RETURN;

L_number_eq:
    MOV(R0,IMM(1002));
    POP(FP);
    RETURN;

L_cont_7:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_isNumber))); // address of Label

	MOV(IND(1029),R0);
	/* ------------ number? ------------ */
	/* ++++++++++++ integer? ++++++++++++ */

	JUMP(L_cont_6);

L_prim_isInteger:
	PUSH(FP);
	MOV(FP, SP);
	MOV(R0, IND(FPARG(2)));
	CMP(R0,IMM(T_INTEGER));
	JUMP_NE(L_integer_ne);
	MOV(R0,IMM(1002));
	POP(FP);
	RETURN;

L_integer_ne:
    MOV(R0,IMM(1004));
    POP(FP);
    RETURN;

L_cont_6:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_isInteger))); // address of Label

	MOV(IND(1025),R0);
	/* ------------ integer? ------------ */
	/* ++++++++++++ boolean? ++++++++++++ */

	JUMP(L_cont_5);

L_prim_isBoolean:
	PUSH(FP);
	MOV(FP, SP);
	MOV(R0, IND(FPARG(2)));
	CMP(R0,IMM(T_BOOL));
	JUMP_NE(L_bool_ne);
	MOV(R0,IMM(1002));
	POP(FP);
	RETURN;

L_bool_ne:
    MOV(R0,IMM(1004));
    POP(FP);
    RETURN;

L_cont_5:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_isBoolean))); // address of Label

	MOV(IND(1020),R0);//address of boolean? is: 1020 adr of true: 1002 adr of false: 1004
	/* ------------ boolean? ------------ */
	/* ++++++++++++ null? ++++++++++++ */

	JUMP(L_cont_4);

L_prim_isNull:
	PUSH(FP);
	MOV(FP, SP);
	MOV(R0, IND(FPARG(2)));
	CMP(R0,IMM(T_NIL));
	JUMP_NE(L_nil_ne);
	MOV(R0,IMM(1002));
	POP(FP);
	RETURN;

L_nil_ne:
    MOV(R0,IMM(1004));
    POP(FP);
    RETURN;

L_cont_4:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_isNull))); // address of Label

	MOV(IND(1064),R0);
	/* ------------ null? ------------ */
	/* ++++++++++++ cdr ++++++++++++ */

	JUMP(L_cont_3);

L_prim_cdr:
	PUSH(FP);
	MOV(FP, SP);
	MOV(R0, INDD(FPARG(2),2));
	POP(FP);
	RETURN;

L_cont_3:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_cdr))); // address of Label

	MOV(IND(1055),R0);//address of cdr is: 1055
	/* ------------ cdr ------------ */
	/* ++++++++++++ car ++++++++++++ */

	JUMP(L_cont_2);

L_prim_car:
	PUSH(FP);
	MOV(FP, SP);
	MOV(R0, INDD(FPARG(2),1));
	POP(FP);
	RETURN;

L_cont_2:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_car))); // address of Label
	MOV(IND(1061),R0);//address of car is: 1061
	/* ------------ car ------------ */
	/* ++++++++++++ cons ++++++++++++ */

	JUMP(L_cont_1);

L_prim_cons:
	PUSH(FP);
	MOV(FP, SP);
	PUSH(FPARG(3)); //  Cdr
	PUSH(FPARG(2)); // Car
	CALL(MAKE_SOB_PAIR);
	DROP(IMM(2));
	POP(FP);
	RETURN;

L_cont_1:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),IMM(T_CLOSURE)); // Closure 
	MOV(INDD(R0,1),IMM(496351)); // Dummy env
	MOV(INDD(R0,2),IMM(LABEL(L_prim_cons))); // address of Label
	MOV(IND(1056),R0);// Moves closure to varfree table. address of cons is: 1056
	/* ------------ cons ------------ */	 /* LambdaSimple -  - */
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0), T_CLOSURE);
	MOV(R1, R0);  // put address of closure in r1
	MOV(R0, IMM(0));
	ADD(R0, IMM(1));
	PUSH(R0);
	CALL(MALLOC);
	DROP(1);
	MOV(INDD(R1, 1), R0); // put address of start of new env in *(r1+1) which is address on env in current closure
	//initiate loop variables
  MOV(R3, IMM(0)); // R3 is i == 0.
	MOV(R4, IMM(1)); // R4 is j == 1
	MOV(R5, IMM(0)); //R5 is |env|
 L_loop_code_1:
	CMP(R3, R5); // if R3 <= R5 enter loop
	JUMP_GE(L_loop_end_1);
		 //Entered loop
		MOV(INDD(R0, R4), INDD(FPARG(0),R3));
		ADD(R3, IMM(1));
		ADD(R4, IMM(1));
		JUMP(L_loop_code_1); 
L_loop_end_1:
		MOV(IND(R0),1001); /* num_of_params = 0 */

	MOV(INDD(R1, 2), LABEL(L_closure_code_1));
	MOV(R0, R1);
	JUMP(L_closure_end_1);

L_closure_code_1:
	
	PUSH(FP);
	MOV(FP, SP);
	CMP(FPARG(1), 0);
	JUMP_LE(L_loop_code_4);
	MOV(R1, FPARG(1));
	ADD(R1, 1);
	MOV(R1, FPARG(R1));
	PUSH(1001);
	PUSH(R1);
	CALL(MAKE_SOB_PAIR);
	DROP(2);
	MOV(R1, FPARG(1)); 
	SUB(R1, 0);
	SUB(R1, 1);
	MOV(R2, 0);
L_loop_code_3:
	CMP(R2,R1);
	JUMP_GE(L_loop_end_3);
		PUSH(R0);
		MOV(R3, FPARG(1)); 
		SUB(R3, R2);
		PUSH(FPARG(R3));
		CALL(MAKE_SOB_PAIR);
		DROP(2);
		ADD(R2, 1);
		JUMP(L_loop_code_3);
L_loop_end_3:
	MOV(R1, 0);
	ADD(R1, 2);
	MOV(FPARG(R1), R0);
	JUMP(L_loop_end_4);
L_loop_code_4:
	PUSH(1001);
	PUSH(1001);
	CALL(MAKE_SOB_PAIR);
	DROP(2);
	JUMP(L_loop_end_3);
L_loop_end_4:
	 /*pvar min=0*/
  	MOV(R0,FPARG(2))
  	 /* End pvar min=0*/	POP(FP);
	RETURN;
L_closure_end_1:
	MOV(IND(1066), R0);
	MOV(R0, 1000);


/* START OF APPLIC (lambda (app2 appl) (begin (set! app2 app2) (set! appl appl) (begin (BoxSet' app2 (lambda (s1 s2) (if (null? s1) s2 (cons (car s1) (app2 (cdr s1) s2))))) (BoxSet' appl (lambda (s1 s) (if (null? s) s1 (app2 s1 (appl (car s) (cdr s)))))) (lambda s (if (null? s) '() (appl (car s) (cdr s)))))))*/
	// start pushing params into current frame
	/* Handles consts: "#f" */
	MOV(R0, 1004);
	PUSH(R0);	
	/* Handles consts: "#f" */
	MOV(R0, 1004);
	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(2); // number of params
	 /* LambdaSimple - app2 appl - */
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0), T_CLOSURE);
	MOV(R1, R0);  // put address of closure in r1
	MOV(R0, IMM(0));
	ADD(R0, IMM(1));
	PUSH(R0);
	CALL(MALLOC);
	DROP(1);
	MOV(INDD(R1, 1), R0); // put address of start of new env in *(r1+1) which is address on env in current closure
	//initiate loop variables
  MOV(R3, IMM(0)); // R3 is i == 0.
	MOV(R4, IMM(1)); // R4 is j == 1
	MOV(R5, IMM(0)); //R5 is |env|
L_loop_code_5:
	CMP(R3, R5); // if R3 <= R5 enter loop
	JUMP_GE(L_loop_end_5);
		 //Entered loop
		MOV(INDD(R0, R4), INDD(FPARG(0),R3));
		ADD(R3, IMM(1));
		ADD(R4, IMM(1));
		JUMP(L_loop_code_5); 
L_loop_end_5:
		MOV(IND(R0),1001); /* num_of_params = 2 */

	MOV(INDD(R1, 2), LABEL(L_closure_code_2));
	MOV(R0, R1);
	JUMP(L_closure_end_2);

L_closure_code_2:
	
	PUSH(FP);
	MOV(FP, SP);
		 /*pvar min=0*/
  	MOV(R0,FPARG(2))
  	 /* End pvar min=0*/	MOV(R1,R0);
	PUSH(IMM(1));
	CALL(MALLOC);
	DROP(IMM(1));
	MOV(IND(R0),R1);	MOV(R1, 2);
	ADD(R1, 0);
	MOV(FPARG(R1), R0);
	MOV(R0, 1001);

		 /*pvar min=1*/
  	MOV(R0,FPARG(3))
  	 /* End pvar min=1*/	MOV(R1,R0);
	PUSH(IMM(1));
	CALL(MALLOC);
	DROP(IMM(1));
	MOV(IND(R0),R1);	MOV(R1, 2);
	ADD(R1, 1);
	MOV(FPARG(R1), R0);
	MOV(R0, 1001);

 /* LambdaSimple - s1 s2 - */
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0), T_CLOSURE);
	MOV(R1, R0);  // put address of closure in r1
	MOV(R0, IMM(1));
	ADD(R0, IMM(1));
	PUSH(R0);
	CALL(MALLOC);
	DROP(1);
	MOV(INDD(R1, 1), R0); // put address of start of new env in *(r1+1) which is address on env in current closure
	//initiate loop variables
  MOV(R3, IMM(0)); // R3 is i == 0.
	MOV(R4, IMM(1)); // R4 is j == 1
	MOV(R5, IMM(1)); //R5 is |env|
L_loop_code_7:
	CMP(R3, R5); // if R3 <= R5 enter loop
	JUMP_GE(L_loop_end_7);
		 //Entered loop
		MOV(INDD(R0, R4), INDD(FPARG(0),R3));
		ADD(R3, IMM(1));
		ADD(R4, IMM(1));
		JUMP(L_loop_code_7); 
L_loop_end_7:
		MOV(R2, R0);
	 //now i want to add the new parameter list to env in place 0
	PUSH(FPARG(1));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R2), R0);
//init parameters for second for loop
MOV(R3, IMM(0)); // i = 0
	MOV(R4, FPARG(1));// R4 == number of params
L_loop_code_8:
		MOV(R5, R3); // R5 = i
		ADD(R5, IMM(2)); //R5 = R5+2
		MOV(INDD(R0, R3), FPARG(R5)); //changed R0 TO INDD(R1, 1)
		ADD(R3, IMM(1));
		CMP(R3, R4);
		JUMP_LT(L_loop_code_8);
L_loop_end_8:
	MOV(INDD(R1, 2), LABEL(L_closure_code_3));
	MOV(R0, R1);
	JUMP(L_closure_end_3);

L_closure_code_3:
	
	PUSH(FP);
	MOV(FP, SP);

/* START OF APPLIC null?*/
	// start pushing params into current frame	 /*pvar min=0*/
  	MOV(R0,FPARG(2))
  	 /* End pvar min=0*/
	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(1); // number of params
		/* ****Varfree**** */
	// address of var free is 1064 
	MOV(R1, IMM(1064));
	MOV(R0, IND(R1));
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	CMP(R0, 1004);
	JUMP_EQ(L_if_else_1);
		 /*pvar min=1*/
  	MOV(R0,FPARG(3))
  	 /* End pvar min=1*/
	JUMP(L_if_end_1);
L_if_else_1:
	
/* START OF APPLIC app2*/
	// start pushing params into current frame	 /*pvar min=1*/
  	MOV(R0,FPARG(3))
  	 /* End pvar min=1*/
	PUSH(R0);	
/* START OF APPLIC cdr*/
	// start pushing params into current frame	 /*pvar min=0*/
  	MOV(R0,FPARG(2))
  	 /* End pvar min=0*/
	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(1); // number of params
		/* ****Varfree**** */
	// address of var free is 1055 
	MOV(R1, IMM(1055));
	MOV(R0, IND(R1));
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(2); // number of params
		 /*bvar maj=0 min=0*/
  	MOV(R0, FPARG(IMM(0))); /* env */
  	MOV(R1, IMM(0));
  	MOV(R2, INDD(R0,R1));
  	MOV(R1, IMM(0));
  	MOV(R0, INDD(R2, R1));
    	 /* End bvar maj=0 min=0*/
	MOV(R0,IND(R0));

PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	PUSH(R0);	
/* START OF APPLIC car*/
	// start pushing params into current frame	 /*pvar min=0*/
  	MOV(R0,FPARG(2))
  	 /* End pvar min=0*/
	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(1); // number of params
		/* ****Varfree**** */
	// address of var free is 1061 
	MOV(R1, IMM(1061));
	MOV(R0, IND(R1));
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	PUSH(R0);
	PUSH(IMM(2));	/* ****Varfree**** */
	// address of var free is 1056 
	MOV(R1, IMM(1056));
	MOV(R0, IND(R1));
	PUSH(INDD(R0,IMM(1)));
	PUSH(FPARG(-1));
	MOV(R1,FPARG(-2));
	MOV(R2,2);
	ADD(R2,IMM(3))
	MOV(R4,FP);
	ADD(R4,IMM(2));
	MOV(R7,FP);
	SUB(R7,FPARG(1));
	SUB(R7,IMM(4));
	SUB(R4,IMM(2));
	MOV(FP,R7);
	MOV(R3,IMM(0));
L_loop_code_9:
	CMP(R2,R3);
	JUMP_EQ(L_loop_end_9);
	MOV(R5,FP);
	ADD(R5,R3);
	MOV(R6,R4);
	ADD(R6,R3);
	MOV(STACK(R5),STACK(R6));
	ADD(R3,IMM(1));
	JUMP(L_loop_code_9);
L_loop_end_9:
	MOV(SP,FP);
	ADD(SP,R2);
	MOV(FP,R1);
	JUMPA(INDD(R0,IMM(2)));
L_if_end_1:	POP(FP);
	RETURN;
L_closure_end_3:
MOV(R6, R0);	 /*pvar min=0*/
  	MOV(R0,FPARG(2))
  	 /* End pvar min=0*/	MOV(IND(R0), R6);

 /* LambdaSimple - s1 s - */
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0), T_CLOSURE);
	MOV(R1, R0);  // put address of closure in r1
	MOV(R0, IMM(1));
	ADD(R0, IMM(1));
	PUSH(R0);
	CALL(MALLOC);
	DROP(1);
	MOV(INDD(R1, 1), R0); // put address of start of new env in *(r1+1) which is address on env in current closure
	//initiate loop variables
  MOV(R3, IMM(0)); // R3 is i == 0.
	MOV(R4, IMM(1)); // R4 is j == 1
	MOV(R5, IMM(1)); //R5 is |env|
L_loop_code_10:
	CMP(R3, R5); // if R3 <= R5 enter loop
	JUMP_GE(L_loop_end_10);
		 //Entered loop
		MOV(INDD(R0, R4), INDD(FPARG(0),R3));
		ADD(R3, IMM(1));
		ADD(R4, IMM(1));
		JUMP(L_loop_code_10); 
L_loop_end_10:
		MOV(R2, R0);
	 //now i want to add the new parameter list to env in place 0
	PUSH(FPARG(1));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R2), R0);
//init parameters for second for loop
MOV(R3, IMM(0)); // i = 0
	MOV(R4, FPARG(1));// R4 == number of params
L_loop_code_11:
		MOV(R5, R3); // R5 = i
		ADD(R5, IMM(2)); //R5 = R5+2
		MOV(INDD(R0, R3), FPARG(R5)); //changed R0 TO INDD(R1, 1)
		ADD(R3, IMM(1));
		CMP(R3, R4);
		JUMP_LT(L_loop_code_11);
L_loop_end_11:
	MOV(INDD(R1, 2), LABEL(L_closure_code_4));
	MOV(R0, R1);
	JUMP(L_closure_end_4);

L_closure_code_4:
	
	PUSH(FP);
	MOV(FP, SP);

/* START OF APPLIC null?*/
	// start pushing params into current frame	 /*pvar min=1*/
  	MOV(R0,FPARG(3))
  	 /* End pvar min=1*/
	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(1); // number of params
		/* ****Varfree**** */
	// address of var free is 1064 
	MOV(R1, IMM(1064));
	MOV(R0, IND(R1));
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	CMP(R0, 1004);
	JUMP_EQ(L_if_else_2);
		 /*pvar min=0*/
  	MOV(R0,FPARG(2))
  	 /* End pvar min=0*/
	JUMP(L_if_end_2);
L_if_else_2:
	
/* START OF APPLIC appl*/
	// start pushing params into current frame
/* START OF APPLIC cdr*/
	// start pushing params into current frame	 /*pvar min=1*/
  	MOV(R0,FPARG(3))
  	 /* End pvar min=1*/
	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(1); // number of params
		/* ****Varfree**** */
	// address of var free is 1055 
	MOV(R1, IMM(1055));
	MOV(R0, IND(R1));
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	PUSH(R0);	
/* START OF APPLIC car*/
	// start pushing params into current frame	 /*pvar min=1*/
  	MOV(R0,FPARG(3))
  	 /* End pvar min=1*/
	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(1); // number of params
		/* ****Varfree**** */
	// address of var free is 1061 
	MOV(R1, IMM(1061));
	MOV(R0, IND(R1));
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(2); // number of params
		 /*bvar maj=0 min=1*/
  	MOV(R0, FPARG(IMM(0))); /* env */
  	MOV(R1, IMM(0));
  	MOV(R2, INDD(R0,R1));
  	MOV(R1, IMM(1));
  	MOV(R0, INDD(R2, R1));
    	 /* End bvar maj=0 min=1*/
	MOV(R0,IND(R0));

PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	PUSH(R0);		 /*pvar min=0*/
  	MOV(R0,FPARG(2))
  	 /* End pvar min=0*/
	PUSH(R0);
	PUSH(IMM(2));	 /*bvar maj=0 min=0*/
  	MOV(R0, FPARG(IMM(0))); /* env */
  	MOV(R1, IMM(0));
  	MOV(R2, INDD(R0,R1));
  	MOV(R1, IMM(0));
  	MOV(R0, INDD(R2, R1));
    	 /* End bvar maj=0 min=0*/
	MOV(R0,IND(R0));

	PUSH(INDD(R0,IMM(1)));
	PUSH(FPARG(-1));
	MOV(R1,FPARG(-2));
	MOV(R2,2);
	ADD(R2,IMM(3))
	MOV(R4,FP);
	ADD(R4,IMM(2));
	MOV(R7,FP);
	SUB(R7,FPARG(1));
	SUB(R7,IMM(4));
	SUB(R4,IMM(2));
	MOV(FP,R7);
	MOV(R3,IMM(0));
L_loop_code_12:
	CMP(R2,R3);
	JUMP_EQ(L_loop_end_12);
	MOV(R5,FP);
	ADD(R5,R3);
	MOV(R6,R4);
	ADD(R6,R3);
	MOV(STACK(R5),STACK(R6));
	ADD(R3,IMM(1));
	JUMP(L_loop_code_12);
L_loop_end_12:
	MOV(SP,FP);
	ADD(SP,R2);
	MOV(FP,R1);
	JUMPA(INDD(R0,IMM(2)));
L_if_end_2:	POP(FP);
	RETURN;
L_closure_end_4:
MOV(R6, R0);	 /*pvar min=1*/
  	MOV(R0,FPARG(3))
  	 /* End pvar min=1*/	MOV(IND(R0), R6);

 /* LambdaSimple -  - */
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0), T_CLOSURE);
	MOV(R1, R0);  // put address of closure in r1
	MOV(R0, IMM(1));
	ADD(R0, IMM(1));
	PUSH(R0);
	CALL(MALLOC);
	DROP(1);
	MOV(INDD(R1, 1), R0); // put address of start of new env in *(r1+1) which is address on env in current closure
	//initiate loop variables
  MOV(R3, IMM(0)); // R3 is i == 0.
	MOV(R4, IMM(1)); // R4 is j == 1
	MOV(R5, IMM(1)); //R5 is |env|
 L_loop_code_13:
	CMP(R3, R5); // if R3 <= R5 enter loop
	JUMP_GE(L_loop_end_13);
		 //Entered loop
		MOV(INDD(R0, R4), INDD(FPARG(0),R3));
		ADD(R3, IMM(1));
		ADD(R4, IMM(1));
		JUMP(L_loop_code_13); 
L_loop_end_13:
		MOV(R2, R0);
	 //now i want to add the new parameter list to env in place 0
	PUSH(FPARG(1));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R2), R0);
//init parameters for second for loop
MOV(R3, IMM(0)); // i = 0
	MOV(R4, FPARG(1)); // R4 == number of params
L_loop_code_14:
		MOV(R5, R3); // R5 = i
		ADD(R5, IMM(2)); //R5 = R5+2
		MOV(INDD(R0, R3), FPARG(R5)); //changed R0 TO INDD(R1, 1)
		ADD(R3, IMM(1));
		CMP(R3, R4);
		JUMP_LT(L_loop_code_14);
L_loop_end_14:
	MOV(INDD(R1, 2), LABEL(L_closure_code_5));
	MOV(R0, R1);
	JUMP(L_closure_end_5);

L_closure_code_5:
	
	PUSH(FP);
	MOV(FP, SP);
	CMP(FPARG(1), 0);
	JUMP_LE(L_loop_code_16);
	MOV(R1, FPARG(1));
	ADD(R1, 1);
	MOV(R1, FPARG(R1));
	PUSH(1001);
	PUSH(R1);
	CALL(MAKE_SOB_PAIR);
	DROP(2);
	MOV(R1, FPARG(1)); 
	SUB(R1, 0);
	SUB(R1, 1);
	MOV(R2, 0);
L_loop_code_15:
	CMP(R2,R1);
	JUMP_GE(L_loop_end_15);
		PUSH(R0);
		MOV(R3, FPARG(1)); 
		SUB(R3, R2);
		PUSH(FPARG(R3));
		CALL(MAKE_SOB_PAIR);
		DROP(2);
		ADD(R2, 1);
		JUMP(L_loop_code_15);
L_loop_end_15:
	MOV(R1, 0);
	ADD(R1, 2);
	MOV(FPARG(R1), R0);
	JUMP(L_loop_end_16);
L_loop_code_16:
	PUSH(1001);
	PUSH(1001);
	CALL(MAKE_SOB_PAIR);
	DROP(2);
	JUMP(L_loop_end_15);
L_loop_end_16:

/* START OF APPLIC null?*/
	// start pushing params into current frame	 /*pvar min=0*/
  	MOV(R0,FPARG(2))
  	 /* End pvar min=0*/
	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(1); // number of params
		/* ****Varfree**** */
	// address of var free is 1064 
	MOV(R1, IMM(1064));
	MOV(R0, IND(R1));
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	CMP(R0, 1004);
	JUMP_EQ(L_if_else_3);
	
	/* Handles consts: "()" */
	MOV(R0, 1001);
	JUMP(L_if_end_3);
L_if_else_3:
	
/* START OF APPLIC cdr*/
	// start pushing params into current frame	 /*pvar min=0*/
  	MOV(R0,FPARG(2))
  	 /* End pvar min=0*/
	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(1); // number of params
		/* ****Varfree**** */
	// address of var free is 1055 
	MOV(R1, IMM(1055));
	MOV(R0, IND(R1));
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	PUSH(R0);	
/* START OF APPLIC car*/
	// start pushing params into current frame	 /*pvar min=0*/
  	MOV(R0,FPARG(2))
  	 /* End pvar min=0*/
	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(1); // number of params
		/* ****Varfree**** */
	// address of var free is 1061 
	MOV(R1, IMM(1061));
	MOV(R0, IND(R1));
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	PUSH(R0);
	PUSH(IMM(2));	 /*bvar maj=0 min=1*/
  	MOV(R0, FPARG(IMM(0))); /* env */
  	MOV(R1, IMM(0));
  	MOV(R2, INDD(R0,R1));
  	MOV(R1, IMM(1));
  	MOV(R0, INDD(R2, R1));
    	 /* End bvar maj=0 min=1*/
	MOV(R0,IND(R0));

	PUSH(INDD(R0,IMM(1)));
	PUSH(FPARG(-1));
	MOV(R1,FPARG(-2));
	MOV(R2,2);
	ADD(R2,IMM(3))
	MOV(R4,FP);
	ADD(R4,IMM(2));
	MOV(R7,FP);
	SUB(R7,FPARG(1));
	SUB(R7,IMM(4));
	SUB(R4,IMM(2));
	MOV(FP,R7);
	MOV(R3,IMM(0));
L_loop_code_17:
	CMP(R2,R3);
	JUMP_EQ(L_loop_end_17);
	MOV(R5,FP);
	ADD(R5,R3);
	MOV(R6,R4);
	ADD(R6,R3);
	MOV(STACK(R5),STACK(R6));
	ADD(R3,IMM(1));
	JUMP(L_loop_code_17);
L_loop_end_17:
	MOV(SP,FP);
	ADD(SP,R2);
	MOV(FP,R1);
	JUMPA(INDD(R0,IMM(2)));
L_if_end_3:	POP(FP);
	RETURN;
L_closure_end_5:
	POP(FP);
	RETURN;
L_closure_end_2:

PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/
	MOV(IND(1065), R0);
	MOV(R0, 1000);

 /* LambdaSimple - n l index - */
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0), T_CLOSURE);
	MOV(R1, R0);  // put address of closure in r1
	MOV(R0, IMM(0));
	ADD(R0, IMM(1));
	PUSH(R0);
	CALL(MALLOC);
	DROP(1);
	MOV(INDD(R1, 1), R0); // put address of start of new env in *(r1+1) which is address on env in current closure
	//initiate loop variables
  MOV(R3, IMM(0)); // R3 is i == 0.
	MOV(R4, IMM(1)); // R4 is j == 1
	MOV(R5, IMM(0)); //R5 is |env|
L_loop_code_18:
	CMP(R3, R5); // if R3 <= R5 enter loop
	JUMP_GE(L_loop_end_18);
		 //Entered loop
		MOV(INDD(R0, R4), INDD(FPARG(0),R3));
		ADD(R3, IMM(1));
		ADD(R4, IMM(1));
		JUMP(L_loop_code_18); 
L_loop_end_18:
		MOV(IND(R0),1001); /* num_of_params = 3 */

	MOV(INDD(R1, 2), LABEL(L_closure_code_6));
	MOV(R0, R1);
	JUMP(L_closure_end_6);

L_closure_code_6:
	
	PUSH(FP);
	MOV(FP, SP);

/* START OF APPLIC =*/
	// start pushing params into current frame	 /*pvar min=2*/
  	MOV(R0,FPARG(4))
  	 /* End pvar min=2*/
	PUSH(R0);		 /*pvar min=0*/
  	MOV(R0,FPARG(2))
  	 /* End pvar min=0*/
	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(2); // number of params
		/* ****Varfree**** */
	// address of var free is 1062 
	MOV(R1, IMM(1062));
	MOV(R0, IND(R1));
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	CMP(R0, 1004);
	JUMP_EQ(L_if_else_4);
		 /*pvar min=1*/
  	MOV(R0,FPARG(3))
  	 /* End pvar min=1*/
	PUSH(R0);
	PUSH(IMM(1));	/* ****Varfree**** */
	// address of var free is 1061 
	MOV(R1, IMM(1061));
	MOV(R0, IND(R1));
	PUSH(INDD(R0,IMM(1)));
	PUSH(FPARG(-1));
	MOV(R1,FPARG(-2));
	MOV(R2,1);
	ADD(R2,IMM(3))
	MOV(R4,FP);
	ADD(R4,IMM(2));
	MOV(R7,FP);
	SUB(R7,FPARG(1));
	SUB(R7,IMM(4));
	SUB(R4,IMM(2));
	MOV(FP,R7);
	MOV(R3,IMM(0));
L_loop_code_21:
	CMP(R2,R3);
	JUMP_EQ(L_loop_end_21);
	MOV(R5,FP);
	ADD(R5,R3);
	MOV(R6,R4);
	ADD(R6,R3);
	MOV(STACK(R5),STACK(R6));
	ADD(R3,IMM(1));
	JUMP(L_loop_code_21);
L_loop_end_21:
	MOV(SP,FP);
	ADD(SP,R2);
	MOV(FP,R1);
	JUMPA(INDD(R0,IMM(2)));
	JUMP(L_if_end_4);
L_if_else_4:
	
/* START OF APPLIC +*/
	// start pushing params into current frame
	/* Handles consts: "1" */
	MOV(R0, 1006);
	PUSH(R0);		 /*pvar min=2*/
  	MOV(R0,FPARG(4))
  	 /* End pvar min=2*/
	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(2); // number of params
		/* ****Varfree**** */
	// address of var free is 1016 
	MOV(R1, IMM(1016));
	MOV(R0, IND(R1));
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	PUSH(R0);	
/* START OF APPLIC cdr*/
	// start pushing params into current frame	 /*pvar min=1*/
  	MOV(R0,FPARG(3))
  	 /* End pvar min=1*/
	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(1); // number of params
		/* ****Varfree**** */
	// address of var free is 1055 
	MOV(R1, IMM(1055));
	MOV(R0, IND(R1));
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	PUSH(R0);		 /*pvar min=0*/
  	MOV(R0,FPARG(2))
  	 /* End pvar min=0*/
	PUSH(R0);
	PUSH(IMM(3));	/* ****Varfree**** */
	// address of var free is 1063 
	MOV(R1, IMM(1063));
	MOV(R0, IND(R1));
	PUSH(INDD(R0,IMM(1)));
	PUSH(FPARG(-1));
	MOV(R1,FPARG(-2));
	MOV(R2,3);
	ADD(R2,IMM(3))
	MOV(R4,FP);
	ADD(R4,IMM(2));
	MOV(R7,FP);
	SUB(R7,FPARG(1));
	SUB(R7,IMM(4));
	SUB(R4,IMM(2));
	MOV(FP,R7);
	MOV(R3,IMM(0));
L_loop_code_20:
	CMP(R2,R3);
	JUMP_EQ(L_loop_end_20);
	MOV(R5,FP);
	ADD(R5,R3);
	MOV(R6,R4);
	ADD(R6,R3);
	MOV(STACK(R5),STACK(R6));
	ADD(R3,IMM(1));
	JUMP(L_loop_code_20);
L_loop_end_20:
	MOV(SP,FP);
	ADD(SP,R2);
	MOV(FP,R1);
	JUMPA(INDD(R0,IMM(2)));
L_if_end_4:	POP(FP);
	RETURN;
L_closure_end_6:
	MOV(IND(1063), R0);
	MOV(R0, 1000);

 /* LambdaSimple - l n - */
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0), T_CLOSURE);
	MOV(R1, R0);  // put address of closure in r1
	MOV(R0, IMM(0));
	ADD(R0, IMM(1));
	PUSH(R0);
	CALL(MALLOC);
	DROP(1);
	MOV(INDD(R1, 1), R0); // put address of start of new env in *(r1+1) which is address on env in current closure
	//initiate loop variables
  MOV(R3, IMM(0)); // R3 is i == 0.
	MOV(R4, IMM(1)); // R4 is j == 1
	MOV(R5, IMM(0)); //R5 is |env|
L_loop_code_22:
	CMP(R3, R5); // if R3 <= R5 enter loop
	JUMP_GE(L_loop_end_22);
		 //Entered loop
		MOV(INDD(R0, R4), INDD(FPARG(0),R3));
		ADD(R3, IMM(1));
		ADD(R4, IMM(1));
		JUMP(L_loop_code_22); 
L_loop_end_22:
		MOV(IND(R0),1001); /* num_of_params = 2 */

	MOV(INDD(R1, 2), LABEL(L_closure_code_7));
	MOV(R0, R1);
	JUMP(L_closure_end_7);

L_closure_code_7:
	
	PUSH(FP);
	MOV(FP, SP);

/* START OF APPLIC null?*/
	// start pushing params into current frame	 /*pvar min=0*/
  	MOV(R0,FPARG(2))
  	 /* End pvar min=0*/
	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(1); // number of params
		/* ****Varfree**** */
	// address of var free is 1064 
	MOV(R1, IMM(1064));
	MOV(R0, IND(R1));
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	CMP(R0, 1004);
	JUMP_EQ(L_if_else_5);
		 /*pvar min=1*/
  	MOV(R0,FPARG(3))
  	 /* End pvar min=1*/
	JUMP(L_if_end_5);
L_if_else_5:
	
/* START OF APPLIC +*/
	// start pushing params into current frame
	/* Handles consts: "1" */
	MOV(R0, 1006);
	PUSH(R0);		 /*pvar min=1*/
  	MOV(R0,FPARG(3))
  	 /* End pvar min=1*/
	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(2); // number of params
		/* ****Varfree**** */
	// address of var free is 1016 
	MOV(R1, IMM(1016));
	MOV(R0, IND(R1));
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	PUSH(R0);	
/* START OF APPLIC cdr*/
	// start pushing params into current frame	 /*pvar min=0*/
  	MOV(R0,FPARG(2))
  	 /* End pvar min=0*/
	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(1); // number of params
		/* ****Varfree**** */
	// address of var free is 1055 
	MOV(R1, IMM(1055));
	MOV(R0, IND(R1));
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	PUSH(R0);
	PUSH(IMM(2));	/* ****Varfree**** */
	// address of var free is 1060 
	MOV(R1, IMM(1060));
	MOV(R0, IND(R1));
	PUSH(INDD(R0,IMM(1)));
	PUSH(FPARG(-1));
	MOV(R1,FPARG(-2));
	MOV(R2,2);
	ADD(R2,IMM(3))
	MOV(R4,FP);
	ADD(R4,IMM(2));
	MOV(R7,FP);
	SUB(R7,FPARG(1));
	SUB(R7,IMM(4));
	SUB(R4,IMM(2));
	MOV(FP,R7);
	MOV(R3,IMM(0));
L_loop_code_24:
	CMP(R2,R3);
	JUMP_EQ(L_loop_end_24);
	MOV(R5,FP);
	ADD(R5,R3);
	MOV(R6,R4);
	ADD(R6,R3);
	MOV(STACK(R5),STACK(R6));
	ADD(R3,IMM(1));
	JUMP(L_loop_code_24);
L_loop_end_24:
	MOV(SP,FP);
	ADD(SP,R2);
	MOV(FP,R1);
	JUMPA(INDD(R0,IMM(2)));
L_if_end_5:	POP(FP);
	RETURN;
L_closure_end_7:
	MOV(IND(1060), R0);
	MOV(R0, 1000);

 /* LambdaSimple - l - */
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0), T_CLOSURE);
	MOV(R1, R0);  // put address of closure in r1
	MOV(R0, IMM(0));
	ADD(R0, IMM(1));
	PUSH(R0);
	CALL(MALLOC);
	DROP(1);
	MOV(INDD(R1, 1), R0); // put address of start of new env in *(r1+1) which is address on env in current closure
	//initiate loop variables
  MOV(R3, IMM(0)); // R3 is i == 0.
	MOV(R4, IMM(1)); // R4 is j == 1
	MOV(R5, IMM(0)); //R5 is |env|
L_loop_code_25:
	CMP(R3, R5); // if R3 <= R5 enter loop
	JUMP_GE(L_loop_end_25);
		 //Entered loop
		MOV(INDD(R0, R4), INDD(FPARG(0),R3));
		ADD(R3, IMM(1));
		ADD(R4, IMM(1));
		JUMP(L_loop_code_25); 
L_loop_end_25:
		MOV(IND(R0),1001); /* num_of_params = 1 */

	MOV(INDD(R1, 2), LABEL(L_closure_code_8));
	MOV(R0, R1);
	JUMP(L_closure_end_8);

L_closure_code_8:
	
	PUSH(FP);
	MOV(FP, SP);

/* START OF APPLIC null?*/
	// start pushing params into current frame	 /*pvar min=0*/
  	MOV(R0,FPARG(2))
  	 /* End pvar min=0*/
	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(1); // number of params
		/* ****Varfree**** */
	// address of var free is 1064 
	MOV(R1, IMM(1064));
	MOV(R0, IND(R1));
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	CMP(R0, 1004);
	JUMP_EQ(L_if_else_6);
	
	/* Handles consts: "()" */
	MOV(R0, 1001);
	JUMP(L_if_end_6);
L_if_else_6:
	
/* START OF APPLIC list*/
	// start pushing params into current frame
/* START OF APPLIC car*/
	// start pushing params into current frame	 /*pvar min=0*/
  	MOV(R0,FPARG(2))
  	 /* End pvar min=0*/
	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(1); // number of params
		/* ****Varfree**** */
	// address of var free is 1061 
	MOV(R1, IMM(1061));
	MOV(R0, IND(R1));
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(1); // number of params
		/* ****Varfree**** */
	// address of var free is 1066 
	MOV(R1, IMM(1066));
	MOV(R0, IND(R1));
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	PUSH(R0);	
/* START OF APPLIC list-rev123e3r4se-trololo*/
	// start pushing params into current frame
/* START OF APPLIC cdr*/
	// start pushing params into current frame	 /*pvar min=0*/
  	MOV(R0,FPARG(2))
  	 /* End pvar min=0*/
	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(1); // number of params
		/* ****Varfree**** */
	// address of var free is 1055 
	MOV(R1, IMM(1055));
	MOV(R0, IND(R1));
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(1); // number of params
		/* ****Varfree**** */
	// address of var free is 1059 
	MOV(R1, IMM(1059));
	MOV(R0, IND(R1));
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	PUSH(R0);
	PUSH(IMM(2));	/* ****Varfree**** */
	// address of var free is 1065 
	MOV(R1, IMM(1065));
	MOV(R0, IND(R1));
	PUSH(INDD(R0,IMM(1)));
	PUSH(FPARG(-1));
	MOV(R1,FPARG(-2));
	MOV(R2,2);
	ADD(R2,IMM(3))
	MOV(R4,FP);
	ADD(R4,IMM(2));
	MOV(R7,FP);
	SUB(R7,FPARG(1));
	SUB(R7,IMM(4));
	SUB(R4,IMM(2));
	MOV(FP,R7);
	MOV(R3,IMM(0));
L_loop_code_27:
	CMP(R2,R3);
	JUMP_EQ(L_loop_end_27);
	MOV(R5,FP);
	ADD(R5,R3);
	MOV(R6,R4);
	ADD(R6,R3);
	MOV(STACK(R5),STACK(R6));
	ADD(R3,IMM(1));
	JUMP(L_loop_code_27);
L_loop_end_27:
	MOV(SP,FP);
	ADD(SP,R2);
	MOV(FP,R1);
	JUMPA(INDD(R0,IMM(2)));
L_if_end_6:	POP(FP);
	RETURN;
L_closure_end_8:
	MOV(IND(1059), R0);
	MOV(R0, 1000);

 /* LambdaSimple - list_of_lists n index f - */
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0), T_CLOSURE);
	MOV(R1, R0);  // put address of closure in r1
	MOV(R0, IMM(0));
	ADD(R0, IMM(1));
	PUSH(R0);
	CALL(MALLOC);
	DROP(1);
	MOV(INDD(R1, 1), R0); // put address of start of new env in *(r1+1) which is address on env in current closure
	//initiate loop variables
  MOV(R3, IMM(0)); // R3 is i == 0.
	MOV(R4, IMM(1)); // R4 is j == 1
	MOV(R5, IMM(0)); //R5 is |env|
L_loop_code_28:
	CMP(R3, R5); // if R3 <= R5 enter loop
	JUMP_GE(L_loop_end_28);
		 //Entered loop
		MOV(INDD(R0, R4), INDD(FPARG(0),R3));
		ADD(R3, IMM(1));
		ADD(R4, IMM(1));
		JUMP(L_loop_code_28); 
L_loop_end_28:
		MOV(IND(R0),1001); /* num_of_params = 4 */

	MOV(INDD(R1, 2), LABEL(L_closure_code_9));
	MOV(R0, R1);
	JUMP(L_closure_end_9);

L_closure_code_9:
	
	PUSH(FP);
	MOV(FP, SP);

/* START OF APPLIC not*/
	// start pushing params into current frame
/* START OF APPLIC null?*/
	// start pushing params into current frame	 /*pvar min=0*/
  	MOV(R0,FPARG(2))
  	 /* End pvar min=0*/
	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(1); // number of params
		/* ****Varfree**** */
	// address of var free is 1064 
	MOV(R1, IMM(1064));
	MOV(R0, IND(R1));
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(1); // number of params
		/* ****Varfree**** */
	// address of var free is 1057 
	MOV(R1, IMM(1057));
	MOV(R0, IND(R1));
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	CMP(R0, 1004);
	JUMP_EQ(L_if_else_8);
	
/* START OF APPLIC =*/
	// start pushing params into current frame	 /*pvar min=2*/
  	MOV(R0,FPARG(4))
  	 /* End pvar min=2*/
	PUSH(R0);		 /*pvar min=1*/
  	MOV(R0,FPARG(3))
  	 /* End pvar min=1*/
	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(2); // number of params
		/* ****Varfree**** */
	// address of var free is 1062 
	MOV(R1, IMM(1062));
	MOV(R0, IND(R1));
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	CMP(R0, 1004);
	JUMP_EQ(L_if_else_7);
	
/* START OF APPLIC foo-l12is35t-trololo*/
	// start pushing params into current frame	 /*pvar min=3*/
  	MOV(R0,FPARG(5))
  	 /* End pvar min=3*/
	PUSH(R0);		 /*pvar min=2*/
  	MOV(R0,FPARG(4))
  	 /* End pvar min=2*/
	PUSH(R0);		 /*pvar min=1*/
  	MOV(R0,FPARG(3))
  	 /* End pvar min=1*/
	PUSH(R0);	
/* START OF APPLIC cdr*/
	// start pushing params into current frame	 /*pvar min=0*/
  	MOV(R0,FPARG(2))
  	 /* End pvar min=0*/
	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(1); // number of params
		/* ****Varfree**** */
	// address of var free is 1055 
	MOV(R1, IMM(1055));
	MOV(R0, IND(R1));
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(4); // number of params
		/* ****Varfree**** */
	// address of var free is 1058 
	MOV(R1, IMM(1058));
	MOV(R0, IND(R1));
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	PUSH(R0);	
/* START OF APPLIC f*/
	// start pushing params into current frame
/* START OF APPLIC el1e2m2e3nt_at-trololo*/
	// start pushing params into current frame
	/* Handles consts: "0" */
	MOV(R0, 1008);
	PUSH(R0);	
/* START OF APPLIC car*/
	// start pushing params into current frame	 /*pvar min=0*/
  	MOV(R0,FPARG(2))
  	 /* End pvar min=0*/
	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(1); // number of params
		/* ****Varfree**** */
	// address of var free is 1061 
	MOV(R1, IMM(1061));
	MOV(R0, IND(R1));
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	PUSH(R0);		 /*pvar min=1*/
  	MOV(R0,FPARG(3))
  	 /* End pvar min=1*/
	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(3); // number of params
		/* ****Varfree**** */
	// address of var free is 1063 
	MOV(R1, IMM(1063));
	MOV(R0, IND(R1));
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(1); // number of params
		 /*pvar min=3*/
  	MOV(R0,FPARG(5))
  	 /* End pvar min=3*/
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	PUSH(R0);
	PUSH(IMM(2));	/* ****Varfree**** */
	// address of var free is 1056 
	MOV(R1, IMM(1056));
	MOV(R0, IND(R1));
	PUSH(INDD(R0,IMM(1)));
	PUSH(FPARG(-1));
	MOV(R1,FPARG(-2));
	MOV(R2,2);
	ADD(R2,IMM(3))
	MOV(R4,FP);
	ADD(R4,IMM(2));
	MOV(R7,FP);
	SUB(R7,FPARG(1));
	SUB(R7,IMM(4));
	SUB(R4,IMM(2));
	MOV(FP,R7);
	MOV(R3,IMM(0));
L_loop_code_31:
	CMP(R2,R3);
	JUMP_EQ(L_loop_end_31);
	MOV(R5,FP);
	ADD(R5,R3);
	MOV(R6,R4);
	ADD(R6,R3);
	MOV(STACK(R5),STACK(R6));
	ADD(R3,IMM(1));
	JUMP(L_loop_code_31);
L_loop_end_31:
	MOV(SP,FP);
	ADD(SP,R2);
	MOV(FP,R1);
	JUMPA(INDD(R0,IMM(2)));
	JUMP(L_if_end_7);
L_if_else_7:
		 /*pvar min=3*/
  	MOV(R0,FPARG(5))
  	 /* End pvar min=3*/
	PUSH(R0);	
/* START OF APPLIC +*/
	// start pushing params into current frame
	/* Handles consts: "1" */
	MOV(R0, 1006);
	PUSH(R0);		 /*pvar min=2*/
  	MOV(R0,FPARG(4))
  	 /* End pvar min=2*/
	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(2); // number of params
		/* ****Varfree**** */
	// address of var free is 1016 
	MOV(R1, IMM(1016));
	MOV(R0, IND(R1));
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	PUSH(R0);		 /*pvar min=1*/
  	MOV(R0,FPARG(3))
  	 /* End pvar min=1*/
	PUSH(R0);		 /*pvar min=0*/
  	MOV(R0,FPARG(2))
  	 /* End pvar min=0*/
	PUSH(R0);
	PUSH(IMM(4));	/* ****Varfree**** */
	// address of var free is 1058 
	MOV(R1, IMM(1058));
	MOV(R0, IND(R1));
	PUSH(INDD(R0,IMM(1)));
	PUSH(FPARG(-1));
	MOV(R1,FPARG(-2));
	MOV(R2,4);
	ADD(R2,IMM(3))
	MOV(R4,FP);
	ADD(R4,IMM(2));
	MOV(R7,FP);
	SUB(R7,FPARG(1));
	SUB(R7,IMM(4));
	SUB(R4,IMM(2));
	MOV(FP,R7);
	MOV(R3,IMM(0));
L_loop_code_30:
	CMP(R2,R3);
	JUMP_EQ(L_loop_end_30);
	MOV(R5,FP);
	ADD(R5,R3);
	MOV(R6,R4);
	ADD(R6,R3);
	MOV(STACK(R5),STACK(R6));
	ADD(R3,IMM(1));
	JUMP(L_loop_code_30);
L_loop_end_30:
	MOV(SP,FP);
	ADD(SP,R2);
	MOV(FP,R1);
	JUMPA(INDD(R0,IMM(2)));
L_if_end_7:
	JUMP(L_if_end_8);
L_if_else_8:
	
	/* Handles consts: "()" */
	MOV(R0, 1001);
L_if_end_8:	POP(FP);
	RETURN;
L_closure_end_9:
	MOV(IND(1058), R0);
	MOV(R0, 1000);

 /* LambdaSimple - f lst - */
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0), T_CLOSURE);
	MOV(R1, R0);  // put address of closure in r1
	MOV(R0, IMM(0));
	ADD(R0, IMM(1));
	PUSH(R0);
	CALL(MALLOC);
	DROP(1);
	MOV(INDD(R1, 1), R0); // put address of start of new env in *(r1+1) which is address on env in current closure
	//initiate loop variables
  MOV(R3, IMM(0)); // R3 is i == 0.
	MOV(R4, IMM(1)); // R4 is j == 1
	MOV(R5, IMM(0)); //R5 is |env|
L_loop_code_32:
	CMP(R3, R5); // if R3 <= R5 enter loop
	JUMP_GE(L_loop_end_32);
		 //Entered loop
		MOV(INDD(R0, R4), INDD(FPARG(0),R3));
		ADD(R3, IMM(1));
		ADD(R4, IMM(1));
		JUMP(L_loop_code_32); 
L_loop_end_32:
		MOV(IND(R0),1001); /* num_of_params = 2 */

	MOV(INDD(R1, 2), LABEL(L_closure_code_10));
	MOV(R0, R1);
	JUMP(L_closure_end_10);

L_closure_code_10:
	
	PUSH(FP);
	MOV(FP, SP);

/* START OF APPLIC null?*/
	// start pushing params into current frame	 /*pvar min=1*/
  	MOV(R0,FPARG(3))
  	 /* End pvar min=1*/
	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(1); // number of params
		/* ****Varfree**** */
	// address of var free is 1064 
	MOV(R1, IMM(1064));
	MOV(R0, IND(R1));
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	CMP(R0, 1004);
	JUMP_EQ(L_if_else_9);
		 /*pvar min=1*/
  	MOV(R0,FPARG(3))
  	 /* End pvar min=1*/
	JUMP(L_if_end_9);
L_if_else_9:
	
/* START OF APPLIC ma1p1-2ppl34-34t3r3o4l5o5lo*/
	// start pushing params into current frame
/* START OF APPLIC cdr*/
	// start pushing params into current frame	 /*pvar min=1*/
  	MOV(R0,FPARG(3))
  	 /* End pvar min=1*/
	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(1); // number of params
		/* ****Varfree**** */
	// address of var free is 1055 
	MOV(R1, IMM(1055));
	MOV(R0, IND(R1));
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	PUSH(R0);		 /*pvar min=0*/
  	MOV(R0,FPARG(2))
  	 /* End pvar min=0*/
	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(2); // number of params
		/* ****Varfree**** */
	// address of var free is 1054 
	MOV(R1, IMM(1054));
	MOV(R0, IND(R1));
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	PUSH(R0);	
/* START OF APPLIC f*/
	// start pushing params into current frame
/* START OF APPLIC car*/
	// start pushing params into current frame	 /*pvar min=1*/
  	MOV(R0,FPARG(3))
  	 /* End pvar min=1*/
	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(1); // number of params
		/* ****Varfree**** */
	// address of var free is 1061 
	MOV(R1, IMM(1061));
	MOV(R0, IND(R1));
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(1); // number of params
		 /*pvar min=0*/
  	MOV(R0,FPARG(2))
  	 /* End pvar min=0*/
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	PUSH(R0);
	PUSH(IMM(2));	/* ****Varfree**** */
	// address of var free is 1056 
	MOV(R1, IMM(1056));
	MOV(R0, IND(R1));
	PUSH(INDD(R0,IMM(1)));
	PUSH(FPARG(-1));
	MOV(R1,FPARG(-2));
	MOV(R2,2);
	ADD(R2,IMM(3))
	MOV(R4,FP);
	ADD(R4,IMM(2));
	MOV(R7,FP);
	SUB(R7,FPARG(1));
	SUB(R7,IMM(4));
	SUB(R4,IMM(2));
	MOV(FP,R7);
	MOV(R3,IMM(0));
L_loop_code_34:
	CMP(R2,R3);
	JUMP_EQ(L_loop_end_34);
	MOV(R5,FP);
	ADD(R5,R3);
	MOV(R6,R4);
	ADD(R6,R3);
	MOV(STACK(R5),STACK(R6));
	ADD(R3,IMM(1));
	JUMP(L_loop_code_34);
L_loop_end_34:
	MOV(SP,FP);
	ADD(SP,R2);
	MOV(FP,R1);
	JUMPA(INDD(R0,IMM(2)));
L_if_end_9:	POP(FP);
	RETURN;
L_closure_end_10:
	MOV(IND(1054), R0);
	MOV(R0, 1000);

 /* LambdaSimple - f n ans list_of_lists - */
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0), T_CLOSURE);
	MOV(R1, R0);  // put address of closure in r1
	MOV(R0, IMM(0));
	ADD(R0, IMM(1));
	PUSH(R0);
	CALL(MALLOC);
	DROP(1);
	MOV(INDD(R1, 1), R0); // put address of start of new env in *(r1+1) which is address on env in current closure
	//initiate loop variables
  MOV(R3, IMM(0)); // R3 is i == 0.
	MOV(R4, IMM(1)); // R4 is j == 1
	MOV(R5, IMM(0)); //R5 is |env|
L_loop_code_35:
	CMP(R3, R5); // if R3 <= R5 enter loop
	JUMP_GE(L_loop_end_35);
		 //Entered loop
		MOV(INDD(R0, R4), INDD(FPARG(0),R3));
		ADD(R3, IMM(1));
		ADD(R4, IMM(1));
		JUMP(L_loop_code_35); 
L_loop_end_35:
		MOV(IND(R0),1001); /* num_of_params = 4 */

	MOV(INDD(R1, 2), LABEL(L_closure_code_11));
	MOV(R0, R1);
	JUMP(L_closure_end_11);

L_closure_code_11:
	
	PUSH(FP);
	MOV(FP, SP);

/* START OF APPLIC >*/
	// start pushing params into current frame
/* START OF APPLIC list-le1n22g3th-trololo*/
	// start pushing params into current frame
	/* Handles consts: "0" */
	MOV(R0, 1008);
	PUSH(R0);		 /*pvar min=3*/
  	MOV(R0,FPARG(5))
  	 /* End pvar min=3*/
	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(2); // number of params
		/* ****Varfree**** */
	// address of var free is 1060 
	MOV(R1, IMM(1060));
	MOV(R0, IND(R1));
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	PUSH(R0);		 /*pvar min=1*/
  	MOV(R0,FPARG(3))
  	 /* End pvar min=1*/
	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(2); // number of params
		/* ****Varfree**** */
	// address of var free is 1052 
	MOV(R1, IMM(1052));
	MOV(R0, IND(R1));
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	CMP(R0, 1004);
	JUMP_EQ(L_if_else_10);
	
/* START OF APPLIC list-rev123e3r4se-trololo*/
	// start pushing params into current frame	 /*pvar min=2*/
  	MOV(R0,FPARG(4))
  	 /* End pvar min=2*/
	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(1); // number of params
		/* ****Varfree**** */
	// address of var free is 1059 
	MOV(R1, IMM(1059));
	MOV(R0, IND(R1));
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	PUSH(R0);	 /* LambdaSimple - x - */
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0), T_CLOSURE);
	MOV(R1, R0);  // put address of closure in r1
	MOV(R0, IMM(1));
	ADD(R0, IMM(1));
	PUSH(R0);
	CALL(MALLOC);
	DROP(1);
	MOV(INDD(R1, 1), R0); // put address of start of new env in *(r1+1) which is address on env in current closure
	//initiate loop variables
  MOV(R3, IMM(0)); // R3 is i == 0.
	MOV(R4, IMM(1)); // R4 is j == 1
	MOV(R5, IMM(1)); //R5 is |env|
L_loop_code_39:
	CMP(R3, R5); // if R3 <= R5 enter loop
	JUMP_GE(L_loop_end_39);
		 //Entered loop
		MOV(INDD(R0, R4), INDD(FPARG(0),R3));
		ADD(R3, IMM(1));
		ADD(R4, IMM(1));
		JUMP(L_loop_code_39); 
L_loop_end_39:
		MOV(R2, R0);
	 //now i want to add the new parameter list to env in place 0
	PUSH(FPARG(1));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R2), R0);
//init parameters for second for loop
MOV(R3, IMM(0)); // i = 0
	MOV(R4, FPARG(1));// R4 == number of params
L_loop_code_40:
		MOV(R5, R3); // R5 = i
		ADD(R5, IMM(2)); //R5 = R5+2
		MOV(INDD(R0, R3), FPARG(R5)); //changed R0 TO INDD(R1, 1)
		ADD(R3, IMM(1));
		CMP(R3, R4);
		JUMP_LT(L_loop_code_40);
L_loop_end_40:
	MOV(INDD(R1, 2), LABEL(L_closure_code_12));
	MOV(R0, R1);
	JUMP(L_closure_end_12);

L_closure_code_12:
	
	PUSH(FP);
	MOV(FP, SP);
	 /*pvar min=0*/
  	MOV(R0,FPARG(2))
  	 /* End pvar min=0*/
	PUSH(R0);		 /*bvar maj=0 min=0*/
  	MOV(R0, FPARG(IMM(0))); /* env */
  	MOV(R1, IMM(0));
  	MOV(R2, INDD(R0,R1));
  	MOV(R1, IMM(0));
  	MOV(R0, INDD(R2, R1));
    	 /* End bvar maj=0 min=0*/

	PUSH(R0);
	PUSH(IMM(2));	/* ****Varfree**** */
	// address of var free is 1051 
	MOV(R1, IMM(1051));
	MOV(R0, IND(R1));
	PUSH(INDD(R0,IMM(1)));
	PUSH(FPARG(-1));
	MOV(R1,FPARG(-2));
	MOV(R2,2);
	ADD(R2,IMM(3))
	MOV(R4,FP);
	ADD(R4,IMM(2));
	MOV(R7,FP);
	SUB(R7,FPARG(1));
	SUB(R7,IMM(4));
	SUB(R4,IMM(2));
	MOV(FP,R7);
	MOV(R3,IMM(0));
L_loop_code_41:
	CMP(R2,R3);
	JUMP_EQ(L_loop_end_41);
	MOV(R5,FP);
	ADD(R5,R3);
	MOV(R6,R4);
	ADD(R6,R3);
	MOV(STACK(R5),STACK(R6));
	ADD(R3,IMM(1));
	JUMP(L_loop_code_41);
L_loop_end_41:
	MOV(SP,FP);
	ADD(SP,R2);
	MOV(FP,R1);
	JUMPA(INDD(R0,IMM(2)));	POP(FP);
	RETURN;
L_closure_end_12:

	PUSH(R0);
	PUSH(IMM(2));	/* ****Varfree**** */
	// address of var free is 1054 
	MOV(R1, IMM(1054));
	MOV(R0, IND(R1));
	PUSH(INDD(R0,IMM(1)));
	PUSH(FPARG(-1));
	MOV(R1,FPARG(-2));
	MOV(R2,2);
	ADD(R2,IMM(3))
	MOV(R4,FP);
	ADD(R4,IMM(2));
	MOV(R7,FP);
	SUB(R7,FPARG(1));
	SUB(R7,IMM(4));
	SUB(R4,IMM(2));
	MOV(FP,R7);
	MOV(R3,IMM(0));
L_loop_code_38:
	CMP(R2,R3);
	JUMP_EQ(L_loop_end_38);
	MOV(R5,FP);
	ADD(R5,R3);
	MOV(R6,R4);
	ADD(R6,R3);
	MOV(STACK(R5),STACK(R6));
	ADD(R3,IMM(1));
	JUMP(L_loop_code_38);
L_loop_end_38:
	MOV(SP,FP);
	ADD(SP,R2);
	MOV(FP,R1);
	JUMPA(INDD(R0,IMM(2)));
	JUMP(L_if_end_10);
L_if_else_10:
		 /*pvar min=3*/
  	MOV(R0,FPARG(5))
  	 /* End pvar min=3*/
	PUSH(R0);	
/* START OF APPLIC cons*/
	// start pushing params into current frame	 /*pvar min=2*/
  	MOV(R0,FPARG(4))
  	 /* End pvar min=2*/
	PUSH(R0);	
/* START OF APPLIC foo-l12is35t-trololo*/
	// start pushing params into current frame
	/* Handles consts: "0" */
	MOV(R0, 1008);
	PUSH(R0);		 /*pvar min=1*/
  	MOV(R0,FPARG(3))
  	 /* End pvar min=1*/
	PUSH(R0);		 /*pvar min=3*/
  	MOV(R0,FPARG(5))
  	 /* End pvar min=3*/
	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(3); // number of params
		/* ****Varfree**** */
	// address of var free is 1058 
	MOV(R1, IMM(1058));
	MOV(R0, IND(R1));
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(2); // number of params
		/* ****Varfree**** */
	// address of var free is 1056 
	MOV(R1, IMM(1056));
	MOV(R0, IND(R1));
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	PUSH(R0);	
/* START OF APPLIC +*/
	// start pushing params into current frame
	/* Handles consts: "1" */
	MOV(R0, 1006);
	PUSH(R0);		 /*pvar min=1*/
  	MOV(R0,FPARG(3))
  	 /* End pvar min=1*/
	PUSH(R0);
	/* finished pusing params to fp*/
	PUSH(2); // number of params
		/* ****Varfree**** */
	// address of var free is 1016 
	MOV(R1, IMM(1016));
	MOV(R0, IND(R1));
PUSH(INDD(R0,1));// push the environment onto the stack
	CALLA(INDD(R0,2));
	DROP(2+STARG(0));
/*end of applic*/

	PUSH(R0);		 /*pvar min=0*/
  	MOV(R0,FPARG(2))
  	 /* End pvar min=0*/
	PUSH(R0);
	PUSH(IMM(4));	/* ****Varfree**** */
	// address of var free is 1053 
	MOV(R1, IMM(1053));
	MOV(R0, IND(R1));
	PUSH(INDD(R0,IMM(1)));
	PUSH(FPARG(-1));
	MOV(R1,FPARG(-2));
	MOV(R2,4);
	ADD(R2,IMM(3))
	MOV(R4,FP);
	ADD(R4,IMM(2));
	MOV(R7,FP);
	SUB(R7,FPARG(1));
	SUB(R7,IMM(4));
	SUB(R4,IMM(2));
	MOV(FP,R7);
	MOV(R3,IMM(0));
L_loop_code_37:
	CMP(R2,R3);
	JUMP_EQ(L_loop_end_37);
	MOV(R5,FP);
	ADD(R5,R3);
	MOV(R6,R4);
	ADD(R6,R3);
	MOV(STACK(R5),STACK(R6));
	ADD(R3,IMM(1));
	JUMP(L_loop_code_37);
L_loop_end_37:
	MOV(SP,FP);
	ADD(SP,R2);
	MOV(FP,R1);
	JUMPA(INDD(R0,IMM(2)));
L_if_end_10:	POP(FP);
	RETURN;
L_closure_end_11:
	MOV(IND(1053), R0);
	MOV(R0, 1000);

 /* LambdaSimple - f - */
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0), T_CLOSURE);
	MOV(R1, R0);  // put address of closure in r1
	MOV(R0, IMM(0));
	ADD(R0, IMM(1));
	PUSH(R0);
	CALL(MALLOC);
	DROP(1);
	MOV(INDD(R1, 1), R0); // put address of start of new env in *(r1+1) which is address on env in current closure
	//initiate loop variables
  MOV(R3, IMM(0)); // R3 is i == 0.
	MOV(R4, IMM(1)); // R4 is j == 1
	MOV(R5, IMM(0)); //R5 is |env|
 L_loop_code_42:
	CMP(R3, R5); // if R3 <= R5 enter loop
	JUMP_GE(L_loop_end_42);
		 //Entered loop
		MOV(INDD(R0, R4), INDD(FPARG(0),R3));
		ADD(R3, IMM(1));
		ADD(R4, IMM(1));
		JUMP(L_loop_code_42); 
L_loop_end_42:
		MOV(IND(R0),1001); /* num_of_params = 1 */

	MOV(INDD(R1, 2), LABEL(L_closure_code_13));
	MOV(R0, R1);
	JUMP(L_closure_end_13);

L_closure_code_13:
	
	PUSH(FP);
	MOV(FP, SP);
	CMP(FPARG(1), 0);
	JUMP_LE(L_loop_code_45);
	MOV(R1, FPARG(1));
	ADD(R1, 1);
	MOV(R1, FPARG(R1));
	PUSH(1001);
	PUSH(R1);
	CALL(MAKE_SOB_PAIR);
	DROP(2);
	MOV(R1, FPARG(1)); 
	SUB(R1, 1);
	SUB(R1, 1);
	MOV(R2, 0);
L_loop_code_44:
	CMP(R2,R1);
	JUMP_GE(L_loop_end_44);
		PUSH(R0);
		MOV(R3, FPARG(1)); 
		SUB(R3, R2);
		PUSH(FPARG(R3));
		CALL(MAKE_SOB_PAIR);
		DROP(2);
		ADD(R2, 1);
		JUMP(L_loop_code_44);
L_loop_end_44:
	MOV(R1, 1);
	ADD(R1, 2);
	MOV(FPARG(R1), R0);
	JUMP(L_loop_end_45);
L_loop_code_45:
	PUSH(1001);
	PUSH(1001);
	CALL(MAKE_SOB_PAIR);
	DROP(2);
	JUMP(L_loop_end_44);
L_loop_end_45:
	 /*pvar min=1*/
  	MOV(R0,FPARG(3))
  	 /* End pvar min=1*/
	PUSH(R0);	
	/* Handles consts: "()" */
	MOV(R0, 1001);
	PUSH(R0);	
	/* Handles consts: "0" */
	MOV(R0, 1008);
	PUSH(R0);		 /*pvar min=0*/
  	MOV(R0,FPARG(2))
  	 /* End pvar min=0*/
	PUSH(R0);
	PUSH(IMM(4));	/* ****Varfree**** */
	// address of var free is 1053 
	MOV(R1, IMM(1053));
	MOV(R0, IND(R1));
	PUSH(INDD(R0,IMM(1)));
	PUSH(FPARG(-1));
	MOV(R1,FPARG(-2));
	MOV(R2,4);
	ADD(R2,IMM(3))
	MOV(R4,FP);
	ADD(R4,IMM(2));
	MOV(R7,FP);
	SUB(R7,FPARG(1));
	SUB(R7,IMM(4));
	SUB(R4,IMM(2));
	MOV(FP,R7);
	MOV(R3,IMM(0));
L_loop_code_46:
	CMP(R2,R3);
	JUMP_EQ(L_loop_end_46);
	MOV(R5,FP);
	ADD(R5,R3);
	MOV(R6,R4);
	ADD(R6,R3);
	MOV(STACK(R5),STACK(R6));
	ADD(R3,IMM(1));
	JUMP(L_loop_code_46);
L_loop_end_46:
	MOV(SP,FP);
	ADD(SP,R2);
	MOV(FP,R1);
	JUMPA(INDD(R0,IMM(2)));	POP(FP);
	RETURN;
L_closure_end_13:
	MOV(IND(1050), R0);
	MOV(R0, 1000);


	/* Handles consts: "#t" */
	MOV(R0, 1002);
	CMP(R0, 1004);
	JUMP_EQ(L_if_else_11);
	
	/* Handles consts: "1" */
	MOV(R0, 1006);
	JUMP(L_if_end_11);
L_if_else_11:
	
	/* Handles consts: "2" */
	MOV(R0, 1010);
L_if_end_11:   PUSH(R0);
    MOV(R1,IMM(1000));
    CMP(R0,R1);
    JUMP_NE(L_do_print_sob);
    JUMP(L_do_not_print_sob);
    /*printf(" +++++ IN epiloge %ld +++++ \n", R0); */
L_do_print_sob:
    CALL(WRITE_SOB);
    /*printf("\n ----- IN epiloge ----- \n");*/
L_do_not_print_sob:
    STOP_MACHINE;return 0;
    }