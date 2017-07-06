/*File name: parser.c
* Compiler: MS Visual Studio 2013
* Author: Mathew Boland, 040800005
* Course: CST 8152 - Compilers, Lab Section: 012
* Assignment: 4
* Date: April 21, 2016
* Professor: Sv. Ranev
* Purpose: Parses through the program checking for syntax errors.
* Funtion list: void parser(), void match(),void syn_eh(), void syn_printe(), 
* void gen_incode(), void program(), opt_statements(), statements(),
* _statements(), statement(), assignment_statement(), assignment_expression(),
* selection_statement(), iteration_statement(), input_statement(), variable_list(),
* _variable_list(), opt_variable_list(), variable_identifier(), output_statement(),
* output_list(), arithmetic_expression(), unary_arithmetic_expression(),
* additive_arithmetic_expression(), _additive_arithmetic_expression(),
* multiplicative_arithmetic_expression(), _multiplicative_arithmetic_expression(),
* primary_arithmetic_expression(), string_expression(), _string_expression(),
* primary_string_expression(), conditional_expression(), logical_OR_expression(),
* _logical_OR_expression(), logical_AND_expression(), _logical_AND_expression(),
* relational_expression(), _relational_expression(), __relational_expression(),
* primary_a_relational_expression(), primary_s_relational_expression()
*/

/*Include files*/
#include <stdio.h>
#include <stdlib.h>

#include "parser.h"


/*Purpose: Gets buffer from input and begins parsing
* Author: Svillen Ranev
* History/Versions: 1.0
* Called functions: malar_next_token(), program(), match(), gen_incode()
* Parameters: Buffer* buffer to be parsed
* Return value: Void
*/
void parser(Buffer * in_buf){
	sc_buf = in_buf;
	lookahead = malar_next_token(sc_buf);
	program(); 
	match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed");
}

/*Purpose : To match two tokens.
* Author: Mathew Boland
* History/Versions: 1.0
* Called functions: syn_eh(), malar_next_token, syn_printe()
* Parameters: int token code, int token attribute
* Return value: Void
* Algorithm: Checks given code and attribute to see if they match.
* If they do then input code is incremented and the function returns. 
* If they don't then the syn_eh() function is called and the function
* returns.
*/
void match(int pr_token_code, int pr_token_attribute){
	/*Check if codes match*/
	if (pr_token_code != lookahead.code){
		/*codes don't match, handle error*/
		syn_eh(pr_token_code);
		return;
	}

	/*check if attribute code needs to match*/
	if (pr_token_code == KW_T || pr_token_code == LOG_OP_T || pr_token_code == ART_OP_T || pr_token_code == REL_OP_T){
		/*Attributes must match now, checking if they do*/
		if (lookahead.attribute.get_int != pr_token_attribute){
			/*attributes don't match, handle error*/
			syn_eh(pr_token_code);
			return;
		}			
	}

	/*Check if code SEOF*/
	if (lookahead.code == SEOF_T){
		return;
	}

	/*update lookahead*/
	lookahead = malar_next_token(sc_buf);

	/*check lookahead for error*/
	if (lookahead.code == ERR_T){
		syn_printe();
		lookahead = malar_next_token(sc_buf);
		synerrno++;
	}	
}

/*Purpose: To handle errors when parsing.
* Author: Mathew Boland
* History/Versions: 1.0
* Called functions: syn_printe(), malar_next_token(), exit()
* Parameters: int token code
* Return value: Void
* Algorithm: Prints error then increments the error counter.
* Then the input token is incremented until either SEOF_T or
* sync_token_code is found. If sync_token_code is not SEOF_T
* and SEOF_T is found then exit() is called. Other wise if it
* is SEOF_T then the function returns. If SEOF_T is never found
* then input token is incremented once more and the function
* returns.
*/
void syn_eh(int sync_token_code){
	syn_printe();
	synerrno++;

	/*Advance lookahead until sync_token_code found*/
	while (lookahead.code != sync_token_code){
		/*Check that SEOF was not reached, only bad if it isn't the token the search is for*/
		if (lookahead.code == SEOF_T && sync_token_code != SEOF_T){
			exit(synerrno);
		}

		/*Get next code for lookahead*/
		lookahead = malar_next_token(sc_buf);
	}

	if (sync_token_code == SEOF_T){
		return;
	}

	/*Increment token one more time and return*/
	lookahead = malar_next_token(sc_buf);
}

/*Purpose: Prints error message.
* Author: Svillen Ranev
* History/Versions: 1.0
* Called functions: printf()
* Parameters: Void
* Return value: Void
*/
void syn_printe(){
	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code){
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n", t.attribute.err_lex);
		break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("NA\n");
		break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T:/* SVID_T    3  String Variable identifier token */
		printf("%s\n", sym_table.pstvr[t.attribute.get_int].plex);
		break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n", t.attribute.flt_value);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n", t.attribute.get_int);
		break;
	case STR_T:/* STR_T     6   String literal token */
		printf("%s\n", b_setmark(str_LTBL, t.attribute.str_offset));
		break;

	case SCC_OP_T: /* 7   String concatenation operator token */
		printf("NA\n");
		break;

	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n");
		break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n", t.attribute.get_int);
		break;

	case  LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n");
		break;
	case  RPR_T: /*RPR_T    13  Right parenthesis token */
		printf("NA\n");
		break;
	case LBR_T: /*    14   Left brace token */
		printf("NA\n");
		break;
	case RBR_T: /*    15  Right brace token */
		printf("NA\n");
		break;

	case KW_T: /*     16   Keyword token */
		printf("%s\n", kw_table[t.attribute.get_int]);
		break;

	case COM_T: /* 17   Comma token */
		printf("NA\n");
		break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
		printf("NA\n");
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}/*end switch*/
}/* end syn_printe()*/

/*Purpose: Prints a String
* Author: Mathew Boland
* History/Versions: 1.0
* Called functions: printf()
* Parameters: char* String to print
* Return value: Void
* Algorithm: Print product value.
*/
void gen_incode(char* product){
	printf("%s\n", product);
}

/*
<program>  ->
PLATYPUS {<opt_statements>}

FIRST{ PLATYPUS }
*/
void program(void){
	match(KW_T, PLATYPUS);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed");
}

/*
 <opt_statements> ->
	<statements> | e

FIRST{ AVID_T, SVID_T, IF, USING, INPUT, OUTPUT, e }
*/
void opt_statements(void){
	switch (lookahead.code){
	case AVID_T:
	case SVID_T : statements();
		break;
	case KW_T:
		/*check for PLATYPUS, ELSE, THEN and REPEAT here and in statements_p()*/
		if (lookahead.attribute.get_int != PLATYPUS
			&& lookahead.attribute.get_int != ELSE
			&& lookahead.attribute.get_int != THEN
			&& lookahead.attribute.get_int != REPEAT){
			statements();
			break;
		}
	default: /*empty string - optional statements*/
		gen_incode("PLATY: Opt_statements parsed");
	}
}

/*
<statements> ->
<statement> <statements’>

FIRST{ AVID_T, SVID_T, IF, USING, INPUT, OUTPUT }
*/
void statements(void){
	statement();
	_statements();
}

/*
<statements’> ->
<statement> <statements’> | e

FIRST{ AVID_T, SVID_T, IF, USING, INPUT, OUTPUT, e }
*/
void _statements(void){
	switch (lookahead.code){
	case KW_T:
		/*check for PLATYPUS, ELSE, THEN and REPEAT here and in statements_p()*/
		if (lookahead.attribute.get_int == PLATYPUS
			|| lookahead.attribute.get_int == ELSE
			|| lookahead.attribute.get_int == THEN
			|| lookahead.attribute.get_int == REPEAT){
			break;
		}
	case AVID_T:
	case SVID_T: 	
		statement();
		_statements();
	}
}

/*
<statement> ->
<assignment statement> | <selection statement>
| <iteration statement> | <input statement> | <output statement>

FIRST{ AVID_T, SVID_T, IF, USING, INPUT, OUTPUT }
*/
void statement(void){
	/*Check if keyword*/
	if (lookahead.code == KW_T){
		/*switch on keyowrds to execute appropriate function*/
		/*Do nothing if no match, error handled after if else*/
		switch (lookahead.attribute.get_int){
		case IF:
			selection_statement();
			return;
		case USING:
			iteration_statement();
			return;
		case INPUT:
			input_statement();
			return;
		case OUTPUT:
			output_statement();
			return;
		}
	} /*Check for VIDs*/
	else if (lookahead.code == AVID_T || lookahead.code == SVID_T){
		assignment_statement();
		return;
	}

	/*Error encountered*/
	syn_printe();
}

/*
<assignment statement> ->
<assignment expression>;

FIRST{ AVID_T, SVID_T }
*/
void assignment_statement(void){
	assignment_expression();
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Assignment statement parsed");
}

/*
< assignment expression> ->
AVID = <arithmetic expression>
| SVID = <string expression>

FIRST{ AVID_T, SVID_T }
*/
void assignment_expression(void){
	if (lookahead.code == AVID_T){
		/*Parse Arithmetic assignment*/
		match(AVID_T, NO_ATTR);
		match(ASS_OP_T, NO_ATTR);
		arithmetic_expression();
		gen_incode("PLATY: Assignment expression (arithmetic) parsed");
	}
	else if (lookahead.code == SVID_T){
		/*Parse string assignment*/
		match(SVID_T, NO_ATTR);
		match(ASS_OP_T, NO_ATTR);
		string_expression();
		gen_incode("PLATY: Assignment expression (string) parsed");
	}
	else{
		/*Error occured*/
		syn_printe();
	}
}

/*
<selection statement> ->
IF (<conditional expression>)  THEN  <opt_statements>
ELSE { <opt_statements> } ;

FIRST{ IF }
*/
void selection_statement(void){
	/*IF*/
	match(KW_T, IF);
	match(LPR_T, NO_ATTR);
	conditional_expression();
	match(RPR_T, NO_ATTR);

	/*THEN*/
	match(KW_T, THEN);
	opt_statements();

	/*ELSE*/
	match(KW_T, ELSE);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: IF statement parsed");
}

/*
<iteration statement> ->
USING  (<assignment expression> , <conditional expression> , <assignment  expression> )
REPEAT {
< opt_statements>
};

FIRST{ USING }
*/
void iteration_statement(void){
	/*USING*/
	match(KW_T, USING);
	match(LPR_T, NO_ATTR);
	assignment_expression();
	match(COM_T, NO_ATTR);
	conditional_expression();
	match(COM_T, NO_ATTR);
	assignment_expression();
	match(RPR_T, NO_ATTR);

	/*REPEAT*/
	match(KW_T, REPEAT);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: USING statement parsed");
}

/*
<input statement> ->
INPUT (<variable list>);

FIRST{ INPUT }
*/
void input_statement(void){
	/*INPUT*/
	match(KW_T, INPUT);
	match(LPR_T, NO_ATTR);
	variable_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: INPUT statement parsed");
}

/*
<variable list> ->
<variable identifier> <variable list’>

FIRST{ AVID_T, SVID_T }
*/
void variable_list(void){
	variable_identifier();
	_variable_list();
	gen_incode("PLATY: Variable list parsed");
}

/*
<variable list’> ->
, <variable identifier> <variable list’>  | e

FIRST{ , , e }
*/
void _variable_list(void){
	/*Check that next token is comma*/
	if (lookahead.code != COM_T){
		/*Not comma, returning*/
		return;
	}

	/*Move onto next list*/
	match(lookahead.code, lookahead.attribute.arr_op);
	variable_identifier();
	_variable_list();
}

/*
<opt_variable list> ->
<variable list> | e

FIRST{ AVID_T, SVID_T, e }
*/
void opt_variable_list(void){
	if (lookahead.code == AVID_T || lookahead.code == SVID_T){
		/*Variable list provided*/
		variable_list();
	}
	else{
		/*No variable list given*/
		gen_incode("PLATY: Optional Variable list parsed");
	}
}

/*
<variable identifier> ->
AVID_T | SVID_T
*/
void variable_identifier(void){
	if (lookahead.code == AVID_T || lookahead.code == SVID_T){
		/*VID found*/
		match(lookahead.code, lookahead.attribute.arr_op);
	}
	else{
		/*Error*/
		syn_printe();
	}
}

/*
<output statement> ->
OUTPUT (<output list>);

FIRST{ OUTPUT }
*/
void output_statement(void){
	/*OUTPUT*/
	match(KW_T, OUTPUT);
	match(LPR_T, NO_ATTR);
	output_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: OUTPUT statement parsed");
}

/*
<output list> ->
<opt_variable list> | STR_T

FIRST{ AVID_T, SVID_T, STR_T, e }
*/
void output_list(void){
	if (lookahead.code == AVID_T || lookahead.code == SVID_T){
		/*VID found*/
		opt_variable_list();
	}
	else if(lookahead.code == STR_T){
		/*String token found*/
		match(STR_T, lookahead.attribute.arr_op);
		gen_incode("PLATY: Output list (string literal) parsed");
	}
	else{
		/*Empty*/
		gen_incode("PLATY: Output list (empty) parsed");
	}
}

/*
<arithmetic expression> - >
<unary arithmetic expression>
| <additive arithmetic expression>

FIRST{ -, +, AVID_T, FPL_T, INL_T, ( }
*/
void arithmetic_expression(void){
	if (lookahead.code == ART_OP_T){
		if (lookahead.attribute.arr_op != MULT && lookahead.attribute.arr_op != DIV){
			unary_arithmetic_expression();
			gen_incode("PLATY: Arithmetic expression parsed");
			return;
		}
	}
	else if (lookahead.code == AVID_T || lookahead.code == FPL_T ||
		lookahead.code == INL_T || lookahead.code == LPR_T){
		additive_arithmetic_expression();
		gen_incode("PLATY: Arithmetic expression parsed");
		return;
	}

	/*Error encountered*/
	syn_printe();
}

/*
<unary arithmetic expression> ->
-  <primary arithmetic expression>
| + <primary arithmetic expression>

FIRST{ -, + }
*/
void unary_arithmetic_expression(void){
	if (lookahead.code == ART_OP_T){
		if (lookahead.attribute.arr_op != MULT && lookahead.attribute.arr_op != DIV){
			match(lookahead.code, lookahead.attribute.arr_op);
			primary_arithmetic_expression();
			gen_incode("PLATY: Unary arithmetic expression parsed");
			return;
		}
	}

	/*Error encountered*/
	syn_printe();
}

/*
<additive arithmetic expression> ->
<multiplicative arithmetic expression> <additive arithmetic expression’>

FIRST{ AVID_T, FPL_T, INL_T, ( }
*/
void additive_arithmetic_expression(void){
	multiplicative_arithmetic_expression();
	_additive_arithmetic_expression();
}

/*
<additive arithmetic expression’> ->
| + <multiplicative arithmetic expression> <additive arithmetic expression’>
| - <multiplicative arithmetic expression> <additive arithmetic expression’>
| e

FIRST{ +, -, e }
*/
void _additive_arithmetic_expression(void){
	if (lookahead.code == ART_OP_T){
		if (lookahead.attribute.arr_op != MULT || lookahead.attribute.arr_op != DIV){
			match(lookahead.code, lookahead.attribute.arr_op);
			multiplicative_arithmetic_expression();
			_additive_arithmetic_expression();
			gen_incode("PLATY: Additive arithmetic expression parsed");
		}
	}
}

/*
<multiplicative arithmetic expression> ->
<primary arithmetic expression> <multiplicative arithmetic expression’>

FIRST{ AVID_T, FPL_T, INL_T, ( }
*/
void multiplicative_arithmetic_expression(void){
	primary_arithmetic_expression();
	_multiplicative_arithmetic_expression();
}

/*
<multiplicative arithmetic expression’> ->
* <primary arithmetic expression> <multiplicative arithmetic expression’>
| \ <primary arithmetic expression> <multiplicative arithmetic expression’>
| e

FIRST{ *, /, e }

*/
void _multiplicative_arithmetic_expression(void){
	if (lookahead.code == ART_OP_T){
		if (lookahead.attribute.arr_op == MULT || lookahead.attribute.arr_op == DIV){
			match(lookahead.code, lookahead.attribute.arr_op);
			primary_arithmetic_expression();
			_multiplicative_arithmetic_expression();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
		}
	}
}

/*
<primary arithmetic expression> ->
AVID_T
| FPL_T
| INL_T
| (<arithmetic expression>)

FIRST{ AVID_T, FPL_T, INL_T, ( }
*/
void primary_arithmetic_expression(void){
	if (lookahead.code == AVID_T || lookahead.code == FPL_T || lookahead.code == INL_T){
		match(lookahead.code, lookahead.attribute.arr_op);
	}
	else if (LPR_T == lookahead.code){
		match(LPR_T, lookahead.attribute.arr_op);
		arithmetic_expression();
		match(RPR_T, NO_ATTR);
	}
	else{
		syn_printe();
		return;
	}

	gen_incode("PLATY: Primary arithmetic expression parsed");
}

/*
<string expression> ->
<primary string expression> <string expression’>

FIRST{ SVID_T, STR_T }
*/
void string_expression(void){
	primary_string_expression();
	_string_expression();
}

/*
<string expression’> ->
<<  <primary string expression> <string expression’>
| e

FIRST{ SCC_OP_T, e }
*/
void _string_expression(void){
	if (lookahead.code == SCC_OP_T){
		match(SCC_OP_T, lookahead.attribute.arr_op);
		primary_string_expression();
		_string_expression();
		return;
	}

	gen_incode("PLATY: String expression parsed");
}

/*
<primary string expression> ->
SVID_T
| STR_T

FIRST{ SVID_T, STR_T }
*/
void primary_string_expression(void){
	if (lookahead.code == SVID_T || lookahead.code == STR_T){
		match(lookahead.code, lookahead.attribute.arr_op);
		gen_incode("PLATY: Primary string expression parsed");
		return;
	}

	/*Error encountered*/
	syn_printe();
}

/*
<conditional expression> ->
<logical OR  expression>

FIRST{ AVID_T, FPL_T, INL_T, SVID_T, STR_T }
*/
void conditional_expression(void){
	logical_OR_expression();
	gen_incode("PLATY: Conditional expression parsed");
}

/*
<logical  OR expression> ->
<logical AND expression> <logical OR expression’>

FIRST{ AVID_T, FPL_T, INL_T, SVID_T, STR_T }
*/
void logical_OR_expression(void){
	logical_AND_expression();
	_logical_OR_expression();
}

/*
<logical  OR expression’> ->
.OR. <logical AND expression> <logical OR expression’> | e

FIRST{ .OR., e }
*/
void _logical_OR_expression(void){
	/*Check that next token is logical operator and not AND*/
	if (lookahead.code != LOG_OP_T || lookahead.attribute.log_op == AND){
		return;
	}

	/*Logical OR operator being used*/
	match(LOG_OP_T, lookahead.attribute.arr_op);
	logical_AND_expression();
	_logical_OR_expression();
	gen_incode("PLATY: Logical OR expression parsed");
}

/*
<logical AND expression> ->
<relational expression> <logical AND expression’>

FIRST{ AVID_T, FPL_T, INL_T, SVID_T, STR_T }
*/
void logical_AND_expression(void){
	relational_expression();
	_logical_AND_expression();
}

/*
<logical AND expression’> ->
.AND. <relational expression> <logical AND expression’>
| e

FIRST{ .AND., e }
*/
void _logical_AND_expression(void){
	/*Check that next token is logical operator and not OR*/
	if (lookahead.code != LOG_OP_T || lookahead.attribute.log_op == OR){
		return;
	}

	/*Logical OR operator being used*/
	match(LOG_OP_T, lookahead.attribute.arr_op);
	relational_expression();
	_logical_AND_expression();
	gen_incode("PLATY: Logical AND expression parsed");
}

/*
<relational expression> ->
<primary a_relational expression> <relational expression’>
| <primary s_relational expression> <relational expression’’>

FIRST{ AVID_T, FPL_T, INL_T, SVID_T, STR_T }
*/
void relational_expression(void){
	if (lookahead.code == AVID_T || lookahead.code == FPL_T | lookahead.code == INL_T){
		primary_a_relational_expression();
		_relational_expression();
	}
	else if (lookahead.code == SVID_T || lookahead.code == STR_T){
		primary_s_relational_expression();
		__relational_expression();
	}
	else{
		syn_printe();
	}

	gen_incode("PLATY: Relational expression parsed");
}

/*
<relational expression’>
==  <primary a_relational expression>
| <>  <primary a_relational  expression>

| >   <primary a_relational  expression>
| <   <primary a_relational expression>

FIRST{ ==, <>, >, <}
*/
void _relational_expression(void){
	/*Check that next token is relational operator*/
	if (lookahead.code != REL_OP_T){
		syn_printe();
		return;
	}

	if (lookahead.attribute.rel_op == EQ ||
		lookahead.attribute.rel_op == NE ||
		lookahead.attribute.rel_op == GT ||
		lookahead.attribute.rel_op == LT){
		match(REL_OP_T, lookahead.attribute.arr_op);
		primary_a_relational_expression();
	}
	else{
		syn_printe();
	}
}

/*
<relational expression’’>
==  <primary s_relational expression>
| <>  <primary s_relational  expression>
|  >   <primary s_relational  expression>
| <   <primary s_relational expression>

FIRST{ ==, <>, >, <}
*/
void __relational_expression(void){
	/*Check that next token is relational operator*/
	if (lookahead.code != REL_OP_T){
		syn_printe();
		return;
	}

	if (lookahead.attribute.rel_op == EQ ||
		lookahead.attribute.rel_op == NE ||
		lookahead.attribute.rel_op == GT ||
		lookahead.attribute.rel_op == LT){
		match(REL_OP_T, lookahead.attribute.arr_op);
		primary_s_relational_expression();
	}
	else{
		syn_printe();
	}
}

/*
<primary a_relational expression> ->
AVID_T
| FPL_T
| INL_T

FIRST{ AVID_T, FPL_T, INL_T}
*/
void primary_a_relational_expression(void){
	if (lookahead.code == AVID_T ||
		lookahead.code == FPL_T ||
		lookahead.code == INL_T){
		match(lookahead.code, lookahead.attribute.arr_op);
	}
	else{
		syn_printe();
	}
	gen_incode("PLATY: Primary a_relational expression parsed");
}

/*
<primary s_relational expression> ->
<primary string expression>

FIRST{ SVID_T, STR_T }
*/
void primary_s_relational_expression(void){
	primary_string_expression();
	gen_incode("PLATY: Primary s_relational expression parsed");
}