/*File name: parser.h
* Compiler: MS Visual Studio 2013
* Author: Mathew Boland, 040800005
* Course: CST 8152 - Compilers, Lab Section: 012
* Assignment: 4
* Date: April 21, 2016
* Professor: Sv. Ranev
* Purpose: Has all the necesary includes and declarations for the parser.c
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

#ifndef PARSER_H_
#define PARSER_H_

/*Includes*/
#include "buffer.h"
#include "stable.h"
#include "token.h"

/*Defines*/
#define NO_ATTR (-1)

/*Global variables*/
static Token lookahead;
static Buffer* sc_buf;
int synerrno;

/*Externs*/
extern STD sym_table; /* Symbol Table Descriptor. Use to print Variable names. */
extern Buffer* str_LTBL; /* This buffer implements String Literal Table. Use to print String literals. */
extern int line; /* Line counter. */
extern char * kw_table[]; /* Keyword table. Use to look up keywords. */
extern Token malar_next_token(Buffer*); /* Get next token */

/*Keywords*/
enum{
	ELSE,
	IF,
	INPUT,
	OUTPUT,
	PLATYPUS,
	REPEAT,
	THEN,
	USING
};

/*function prototypes*/
void parser(Buffer * in_buf);
void match(int pr_token_code, int pr_token_attribute);
void syn_eh(int sync_token_code);
void syn_printe(void);
void gen_incode(char*);

/*Grammar production functions*/
void program(void);
void opt_statements(void);
void statements(void);
void _statements(void);
void statement(void);
void assignment_statement(void);
void assignment_expression(void);
void selection_statement(void);
void iteration_statement(void);
void input_statement(void);
void variable_list(void);
void _variable_list(void);
void opt_variable_list(void);
void variable_identifier(void);
void output_statement(void);
void output_list(void);
void arithmetic_expression(void);
void unary_arithmetic_expression(void);
void additive_arithmetic_expression(void);
void _additive_arithmetic_expression(void);
void multiplicative_arithmetic_expression(void);
void _multiplicative_arithmetic_expression(void);
void primary_arithmetic_expression(void);
void string_expression(void);
void _string_expression(void);
void primary_string_expression(void);
void conditional_expression(void);
void logical_OR_expression(void);
void _logical_OR_expression(void);
void logical_AND_expression(void);
void _logical_AND_expression(void);
void relational_expression(void);
void _relational_expression(void);
void __relational_expression(void);
void primary_a_relational_expression(void);
void primary_s_relational_expression(void);

#endif
