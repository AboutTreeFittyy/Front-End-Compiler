/*File name: scanner.c
* Compiler: MS Visual Studio 2013
* Author: Mathew Boland, 040800005
* Course: CST 8152 - Compilers, Lab Section: 012
* Assignment: 2
* Date: March 8th, 2016
* Professor: Sv. Ranev
* Purpose: Produces tokens from a buffer input containing platypus code
* Function list: scanner_init, aa_func02, aa_func03, aa_func05, aa_func08,
* aa_func11, aa_func12, malar_next_token, get_next_state, char_class, atool
*/
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

/*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"
#include "stable.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
It is defined in platy_st.c */
extern Buffer * str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */

/* Local(file) global objects - variables */
static Buffer *lex_buf;/*pointer to temporary lexeme buffer*/

/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */
static int char_class(unsigned char c); /* character class function */
static int get_next_state(int, char, int *); /* state machine function */
static int iskeyword(char * kw_lexeme); /*keywords lookup functuion */
static long atool(char * lexeme); /* converts octal string to decimal value */

int scanner_init(Buffer * sc_buf) {
	if (b_isempty(sc_buf)) return EXIT_FAILURE;/*1*/
	/* in case the buffer has been read previously  */
	b_setmark(sc_buf, 0);
	b_retract_to_mark(sc_buf);
	b_reset(str_LTBL);
	line = 1;
	return EXIT_SUCCESS;/*0*/
	/*   scerrnum = 0;  *//*no need - global ANSI C */
}

/*Purpose: To find the correct next token in the input buffer
* Author: Mathew Boland
* History/Versions: Version 1.0, March 8th, 2016
* Called functions: strlen, b_getc, b_retract, b_size, b_getcoffset
* get_next_state, getcoffset, sizeof
* Parameters: buffer
* Return value: Token
* Algorithm: Checks character to see if special case.
* If special case it will handle character accordingly.
* Otherwise it uses a transition table to find out what
* to do.
*/
Token malar_next_token(Buffer * sc_buf)
{
	Token t; /* token to return after recognition */
	unsigned char c; /* input symbol */
	int state = 0; /* initial state of the FSM */
	short lexstart;  /*start offset of a lexeme in the input buffer */
	short lexend;    /*end   offset of a lexeme in the input buffer */
	int accept = NOAS; /* type of state - initially not accepting */
	short size; /*tracks length of lexeme*/
	int i, j; /*General use ints*/
	pBuffer errorCheck; /*Used to check returns for errors*/
	char* error, stringHolder[SHRT_MAX]; /*Holds string when found temporarily, size is max buffer size*/

	/*initialize*/
	error = "RUN TIME ERROR: ";
	size = 0;

	while (1)
	{
		/* endless loop broken by token returns it will generate a warning */
		/*GET THE NEXT SYMBOL FROM THE INPUT BUFFER*/
		c = b_getc(sc_buf);

		/* special cases or token driven processing */

		switch (c)
		{
		case '=': /*processes both "=" and "=="*/
			c = b_getc(sc_buf);
			if (c == '=')
			{
				t.code = REL_OP_T;
				t.attribute.rel_op = EQ;
			}
			else
			{
				t.code = ASS_OP_T;

				/*Retract buffer by one*/
				b_retract(sc_buf);
			}
			return t;
			break;
		case ' ': /*Do nothing  to Skip white space and get next char*/
			continue;
		case '\n': /*Do nothing  to Skip line terminator and get next char*/
			line++;
			continue;
			break;
		case '(': t.code = LPR_T;
			return t;
			break;
		case ')': t.code = RPR_T;
			return t;
			break;
		case '{': t.code = LBR_T;
			return t;
			break;
		case '}': t.code = RBR_T;
			return t;
			break;
		case '>': t.code = REL_OP_T;
			t.attribute.rel_op = GT;
			return t;
			break;
		case '<': /*processes "<", "<>" and "<<"*/
			c = b_getc(sc_buf);
			if (c == '<')
			{
				t.code = SCC_OP_T;
			}
			else if (c == '>')
			{
				t.code = REL_OP_T;
				t.attribute.rel_op = NE;
			}
			else
			{
				t.code = REL_OP_T;
				t.attribute.rel_op = LT;

				/*Retract buffer by one*/
				b_retract(sc_buf);
			}
			return t;
			break;
		case '!': /*process "!<" comments*/
			c = b_getc(sc_buf);
			if (c != '<')
			{
				/*Invalid syntax, report error*/
				t.code = ERR_T;
				t.attribute.err_lex[0] = '!';
				t.attribute.err_lex[1] = c;
				t.attribute.err_lex[2] = '\0';
				/*Also check for SEOF*/
				while (c != '\n')
				{
					c = b_getc(sc_buf);
					if (c == (unsigned char)SEOF || c == 255)
					{
						t.code = SEOF_T;
						return t;
					}

				}
				line++;
				return t;
			}
			else
			{
				/*Skip through comment until end of line found*/
				while (c != '\n')
				{
					c = b_getc(sc_buf);
					if (c == (unsigned char)SEOF || c == 255)
					{
						t.code = SEOF_T;
						return t;
					}
				}
				line++;
			}
			continue;
			break;
		case ',': t.code = COM_T;
			return t;
			break;
		case '\"': /*String literal started*/
			t.code = STR_T;
			t.attribute.str_offset = b_size(str_LTBL);
			c = b_getc(sc_buf);
			i = 0;
			/*Copy string into arrayHolder*/
			while (c != '\"')
			{
				if (c == (unsigned char)SEOF || c == 255)
				{
					t.code = ERR_T;
					b_retract(sc_buf);
					j = 1;
					stringHolder[i] = '\0';
					t.attribute.err_lex[0] = '\"';
					while (j < (int)strlen(stringHolder))
					{
						/*append 3 dots if maximum length for error reached*/
						if (j == 20)
						{
							t.attribute.err_lex[17] = '.';
							t.attribute.err_lex[18] = '.';
							t.attribute.err_lex[19] = '.';
							t.attribute.err_lex[20] = '\0';
							return t;
						}
						t.attribute.err_lex[j] = stringHolder[j - 1];
						j++;
					}
					return t;
				}
				stringHolder[i] = c;
				if (c == '\n')
				{
					line++;
				}
				i++;
				c = b_getc(sc_buf);
			}
			stringHolder[i] = '\0';
			i = 0;

			/*Copy stringHolder into string buffer*/
			while (i <= (int)strlen(stringHolder))
			{
				if (c == (unsigned char)SEOF || c == 255)
				{
					t.code = ERR_T;
					b_retract(sc_buf);
					j = 1;
					b_addc(str_LTBL, '\0');
					t.attribute.err_lex[0] = '\"';
					while (j < (int)strlen(stringHolder))
					{
						/*append 3 dots if maximum length for error reached*/
						if (j == 20)
						{
							t.attribute.err_lex[17] = '.';
							t.attribute.err_lex[18] = '.';
							t.attribute.err_lex[19] = '.';
							t.attribute.err_lex[20] = '\0';
							return t;
						}
						t.attribute.err_lex[j] = stringHolder[j - 1];
						j++;
					}
					return t;
				}
				b_addc(str_LTBL, stringHolder[i]);
				i++;
			}

			return t;
			break;
		case ';': t.code = EOS_T;
			return t;
			break;
		case '-': t.code = ART_OP_T;
			t.attribute.arr_op = MINUS;
			return t;
			break;
		case '+': t.code = ART_OP_T;
			t.attribute.arr_op = PLUS;
			return t;
			break;
		case '*': t.code = ART_OP_T;
			t.attribute.arr_op = MULT;
			return t;
			break;
		case '/': t.code = ART_OP_T;
			t.attribute.arr_op = DIV;
			return t;
			break;
		case '.': /*processes ".AND." and ".OR."*/
			c = b_getc(sc_buf);
			if (c == 'A')
			{
				c = b_getc(sc_buf);
				if (c == 'N')
				{
					c = b_getc(sc_buf);
					if (c == 'D')
					{
						c = b_getc(sc_buf);
						if (c == '.')
						{
							t.code = LOG_OP_T;
							t.attribute.log_op = AND;
							return t;
						}
						else
						{
							b_retract(sc_buf);
							b_retract(sc_buf);
							b_retract(sc_buf);
							b_retract(sc_buf);
						}
					}
					else
					{
						b_retract(sc_buf);
						b_retract(sc_buf);
						b_retract(sc_buf);
					}
				}
				else
				{
					b_retract(sc_buf);
					b_retract(sc_buf);
				}
			}
			else if (c == 'O')
			{
				c = b_getc(sc_buf);
				if (c == 'R')
				{
					c = b_getc(sc_buf);
					if (c == '.')
					{
						t.code = LOG_OP_T;
						t.attribute.log_op = OR;
						return t;
					}
					else
					{
						b_retract(sc_buf);
						b_retract(sc_buf);
						b_retract(sc_buf);
					}
				}
				else
				{
					b_retract(sc_buf);
					b_retract(sc_buf);
				}
			}
			else
			{
				b_retract(sc_buf);
			}
			t.code = ERR_T;
			t.attribute.err_lex[0] = '.';
			t.attribute.err_lex[1] = '\0';
			return t;
			break;
		case (unsigned char)SEOF:
		case 255:
			t.code = SEOF_T;
			return t;
			break;
		} /*End of Switch statement*/

		/*Check if lexeme begins with a letter or digit*/
		if (!(isdigit(c)) && !(isalpha(c)))
		{
			t.code = ERR_T;
			t.attribute.err_lex[0] = c;
			t.attribute.err_lex[1] = '\0';
			return t;
		}

		/*set mark to beginning of lexeme*/
		b_setmark(sc_buf, b_getcoffset(sc_buf) - 1);

		/*Get next state*/
		state = get_next_state(state, c, &accept);

		/*Get next character*/
		c = b_getc(sc_buf);
		size++;
		while (accept == NOAS)
		{
			/*Get next state*/
			state = get_next_state(state, c, &accept);
			size++;
			/*Get next character*/
			c = b_getc(sc_buf);
		}

		/*Check for retract*/
		if (accept == ASWR)
		{
			b_retract(sc_buf);
			size--;
		}

		/*Get start and end of lexeme*/
		lexstart = b_mark(sc_buf);
		lexend = b_getcoffset(sc_buf); /*short to int, causes warning*/

		/*Create temporary buffer*/
		lex_buf = b_create(size + 1, 0, 'f');

		/*check for error*/
		if (lex_buf == NULL)
		{
			scerrnum = 10;
			t.code = ERR_T;
			for (i = 0; i < sizeof(error) && i < ERR_LEN + 1; i++)
			{
				t.attribute.err_lex[i] = error[i];

				/*If max length reached, replace last char with terminating char*/
				if (i == ERR_LEN + 1)
				{
					t.attribute.err_lex[i] = '\0';
				}
			}
			return t;
		}

		/*Retract to start of lexeme*/
		b_retract_to_mark(sc_buf);

		/*Put lexeme into temp buffer*/
		while (size != 0)
		{
			errorCheck = b_addc(lex_buf, b_getc(sc_buf));
			if (errorCheck == NULL)
			{
				scerrnum = 10;
				t.code = ERR_T;
				for (i = 0; i < sizeof(error) && i < ERR_LEN + 1; i++)
				{
					t.attribute.err_lex[i] = error[i];

					/*If max length reached, replace last char with terminating char*/
					if (i == ERR_LEN + 1)
					{
						t.attribute.err_lex[i] = '\0';
					}
				}
				return t;
			}
			size--;
		}

		/*Add terminating symbol*/
		errorCheck = b_addc(lex_buf, '\0');
		if (errorCheck == NULL)
		{
			scerrnum = 10;
			t.code = ERR_T;
			for (i = 0; i < sizeof(error) && i < ERR_LEN + 1; i++)
			{
				t.attribute.err_lex[i] = error[i];

				/*If max length reached, replace last char with terminating char*/
				if (i == ERR_LEN + 1)
				{
					t.attribute.err_lex[i] = '\0';
				}
			}
			return t;
		}

		/*Call appropriate state handling function*/
		t = (*aa_table[state])(b_setmark(lex_buf, 0));

		/*free memory*/
		b_free(lex_buf);
		return t;
	}/*end while(1)*/
}


int get_next_state(int state, char c, int *accept)
{
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif

	assert(next != IS);

#ifdef DEBUG
	if (next == IS){
		printf("Scanner Error: Illegal state:\n");
		printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
		exit(1);
	}
#endif
	*accept = as_table[next];
	return next;
}

/*Purpose: To return correct row for c value
* Author: Mathew Boland
* History/Versions: Version 1.0, March 8th, 2016
* Called functions: isalpha
* Parameters: char
* Return value: int
* Algorithm: trys to find matching values for c. When
* found, val is set to appropriate row value and returned.
*/
int char_class(unsigned char c)
{
	int val;

	if (isalpha(c))
	{
		val = 0;
	}
	else if (c == '0')
	{
		val = 1;
	}
	else if (c == '1' || c == '2' || c == '3' || c == '4' || c == '5' || c == '6' || c == '7')
	{
		val = 2;
	}
	else if (c == '8' || c == '9')
	{
		val = 3;
	}
	else if (c == '.')
	{
		val = 4;
	}
	else if (c == '#')
	{
		val = 5;
	}
	else
	{
		val = 6;
	}

	return val;
}



/*Purpose: Sets an arithmetic VID
* Author: Mathew Boland
* History/Versions: Version 1.0, March 8th, 2016
* Called functions: strlen
* Parameters: char[]
* Return value: Token
* Algorithm: Check validity, set values accordingly
*/
Token aa_func02(char lexeme[])
{
	Token t;
	int i, j;
	extern STD sym_table;

	/*initialize*/
	j = 0;

	/*Check if lexeme matches any keywords*/
	for (i = 0; i < KWT_SIZE; i++)
	{
		if (strlen(kw_table[i]) == strlen(lexeme))
		{
			while (kw_table[i][j] == lexeme[j])
			{
				if (j == (int)strlen(lexeme) - 1)
				{
					t.code = KW_T;
					t.attribute.kwt_idx = i;
					return t;
				}
				j++;
			}
		}
	}
	/*Set Token properties and install entry in symbol table*/
	/*Set token code*/
	t.code = AVID_T;

	/*Truncate lexeme to 8 chars if too long*/
	if (8 < (int)strlen(lexeme)){
		lexeme[8] = '\0';
	}

	/*check if float or Integer*/
	switch (lexeme[0])
	{
	case 'i':
	case 'o':
	case 'd':
	case 'n':
		/*Integer found*/
		t.attribute.vid_offset = st_install(sym_table, lexeme, 'I', line);
		break;
	default:
		/*Float found*/
		t.attribute.vid_offset = st_install(sym_table, lexeme, 'F', line);
	}

	/*Check for installation failure*/
	if ((-1) == t.attribute.vid_offset){
		/*Installation failed display error message*/
		printf("\nError: Install failed - Symbol Table is full.\n");

		/*Store the symbol table*/
		j = st_store(sym_table);

		/*Destroy the symbol table*/
		st_destroy(sym_table);

		/*Exit with status of symbol table stored*/
		exit(j);
	}
	return t;
}

/*Purpose: Sets a string VID token
* Author: Mathew Boland
* History/Versions: Version 1.0, March 8th, 2016
* Called functions: strlen
* Parameters: char[]
* Return value: Token
* Algorithm: Check validity, set values accordingly
*/
Token aa_func03(char lexeme[])
{
	/*Declare variables*/
	Token t;
	extern STD sym_table;
	int j;

	/*Truncate lexeme to 8 chars if too long*/
	if (8 < (int)strlen(lexeme)){
		lexeme[8] = '\0';
		lexeme[7] = '#';
	}

	/*Set Token properties and install entry in symbol table*/
	t.code = SVID_T;
	t.attribute.vid_offset = st_install(sym_table, lexeme, 'S', line);


	/*Check for installation failure*/
	if ((-1) == t.attribute.vid_offset){
		/*Installation failed display error message*/
		printf("\nError: Install failed - Symbol Table is full.\n");
		/*Store the symbol table*/
		j = st_store(sym_table);

		/*Destroy the symbol table*/
		st_destroy(sym_table);

		/*Exit with status of symbol table stored*/
		exit(j);
	}
	return t;
}

/*Purpose: Sets a float token
* Author: Mathew Boland
* History/Versions: Version 1.0, March 8th, 2016
* Called functions: strlen, atof
* Parameters: char[]
* Return value: Token
* Algorithm: Check validity, set values accordingly
*/
Token aa_func08(char lexeme[])
{
	Token t;
	int i;
	double f;

	/*convert lexeme to double, produces warning*/
	f = atof(lexeme);

	/*check if lexeme is out of range*/
	if (f != 0.0)
	{
		if (f > FLT_MAX || f < FLT_MIN)
		{
			t.code = ERR_T;
			for (i = 0; i < (int)strlen(lexeme) && i < ERR_LEN + 1; i++)
			{
				t.attribute.err_lex[i] = lexeme[i];

				/*If max length reached, replace last char with terminating char*/
				if (i == ERR_LEN)
				{
					t.attribute.err_lex[i] = '\0';
				}
			}
			return t;
		}
	}

	t.code = FPL_T;
	t.attribute.flt_value = (float)f;
	return t;
}

/*Purpose: Sets a decimal integer token
* Author: Mathew Boland
* History/Versions: Version 1.0, March 8th, 2016
* Called functions: strlen, atoi
* Parameters: char[]
* Return value: Token
* Algorithm: Check validity, set values accordingly
*/
Token aa_func05(char lexeme[])
{
	Token t;
	unsigned short int i, j;
	i = 5;

	/*convert lexeme to decimal*/
	j = (unsigned short)atoi(lexeme);
	/*check if lexeme is out of range*/
	if (atoi(lexeme) > DECIMAL_MAX)
	{
		t.code = ERR_T;
		for (i = 0; i <= strlen(lexeme) && i < ERR_LEN + 1; i++)
		{
			t.attribute.err_lex[i] = lexeme[i];

			/*If max length reached, replace last char with terminating char*/
			if (i == ERR_LEN)
			{
				t.attribute.err_lex[i] = '\0';
			}
		}
		return t;
	}

	t.code = INL_T;
	t.attribute.int_value = j;
	return t;
}

/*Purpose: Set an octal integer token
* Author: Mathew Boland
* History/Versions: Version 1.0, March 8th, 2016
* Called functions: strlen, atool
* Parameters: char[]
* Return value: Token
* Algorithm: Check validity, set values accordingly
*/
Token aa_func11(char lexeme[]){
	Token t;
	unsigned short int i, j;

	/*convert lexeme to octal*/
	j = (unsigned short)atool(lexeme);
	/*check if lexeme is out of range*/
	if (atool(lexeme) > OCTAL_MAX)
	{
		t.code = ERR_T;
		for (i = 0; i <= strlen(lexeme) && i < ERR_LEN + 1; i++)
		{
			t.attribute.err_lex[i] = lexeme[i];

			/*If max length reached, replace last char with terminating char*/
			if (i == ERR_LEN)
			{
				t.attribute.err_lex[i] = '\0';
			}
		}
		return t;
	}
	t.code = INL_T;
	t.attribute.int_value = j;
	return t;
}

/*Purpose: In case of error, creates lexeme
* error message for token
* Author: Mathew Boland
* History/Versions: Version 1.0, March 8th, 2016
* Called functions: strlen
* Parameters: char[]
* Return value: Token
* Algorithm: Puts lexeme in err_lex[]
*/
Token aa_func12(char lexeme[])
{
	Token t;
	int i;

	t.code = ERR_T;
	for (i = 0; i <= (int)strlen(lexeme) && i < ERR_LEN + 1; i++)
	{
		t.attribute.err_lex[i] = lexeme[i];


		/*If max length reached, replace last char with terminating char*/
		if (i == ERR_LEN || i == (int)strlen(lexeme))
		{
			t.attribute.err_lex[i] = '\0';
		}
	}
	return t;
}


/*Purpose: Converts a lexeme to an octal long
* Author: Mathew Boland
* History/Versions: Version 1.0, March 8th, 2016
* Called functions: atoi
* Parameters: Char *
* Return value: long
* Algorithm: Changes a lexeme string to an integer.
* Convert to octal and return.
*/
long atool(char * lexeme)
{
	/*Declare variables*/
	long int oct[50], inc, i, size, j;
	/*initialize*/
	j = atoi(lexeme);
	i = 0;
	size = 0;
	inc = 0;

	/*Convert lexeme to decimal*/
	while (j>0)
	{
		oct[i] = j % 10;
		j = j / 10;
		i++;
		size++;
	}

	/*Convert back to octal*/
	for (i = size - 1; i >= 0; i--)
	{
		inc = inc * 8;
		inc += oct[i];
	}

	return inc;
}