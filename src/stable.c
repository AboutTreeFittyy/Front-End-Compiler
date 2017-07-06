/*File name: stable.c
* Compiler: MS Visual Studio 2013
* Author: Mathew Boland, 040800005
* Course: CST 8152 - Compilers, Lab Section: 012
* Assignment: 3
* Date: March 27th, 2016
* Professor: Sv. Ranev
* Purpose: Controls the symbol tables data manipulation
* Function list:
* STD st_create(int st_size);
* int st_install(STD sym_table, char *lexeme, char type, int line);
* int st_lookup(STD sym_table, char *lexeme);
* int st_update_type(STD sym_table, int vid_offset, char v_type);
* int st_update_value(STD sym_table, int vid_offset, InitialValue i_value);
* char st_get_type(STD sym_table, int vid_offset);
* void st_destroy(STD sym_table);
* int st_print(STD sym_table);
* static void st_setsize(void);
* static void st_incoffset(void);
* int st_store(STD sym_table);
* int st_sort(STD sym_table, char s_order);
*/
#define _CRT_SECURE_NO_WARNINGS

/*Standard include files*/
#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */

/*Includes*/
#include "stable.h"
#include "buffer.h"

/*Static function prototypes*/
static void st_setsize(void);
static void st_incoffset(void);

extern STD sym_table;

/*Purpose: Creates a new empty symbol table
* Author: Mathew Boland
* History/Versions: Version 1.0, March 27th, 2016
* Called functions:
* Parameters: int (symbol tables size)
* Return value: STD (the symbol table database)
* Algorithm: Creates an array of dynamically allocated
* STVRs and a new lexeme buffer. Then sets all the values
* to their given values.
*/
STD st_create(int st_size){
	/*declare variables*/
	STD std;
	Buffer * buf;

	/*check that size is positive*/
	if (st_size <= 0){
		std.st_size = 0;
		return std;
	}

	/*allocate memory for array of size st_size*/
	std.pstvr = malloc(sizeof(STVR)* st_size);

	/*check if allocation successful*/
	if (NULL == std.pstvr){
		std.st_size = 0;
		return std;
	}

	/*create additive self-incrementing buffer*/
	buf = b_create(300, 100, 'a');

	/*check if creation successful*/
	if (NULL == buf){
		std.st_size = 0;
		free(std.pstvr);
		return std;
	}

	/*set remaining attributes to std*/
	std.plsBD = buf;
	std.st_offset = 0;
	std.st_size = st_size;
	return std;
}

/*Purpose: Installs a new entry in the symbol table
* Author: Mathew Boland
* History/Versions: Version 1.0, March 27th, 2016
* Called functions: st_lookup(), memcpy()
* Parameters: Symbol table, pointer to a lexeme, and
* the first line it was found on.
* Return value: integer indicating success or failure
* Algorithm: Checks lexeme to see if already exists and if
* table is full. If not it saves the lexeme and creates the
* lexemes bitfield then returns the offset of the lexeme.
*/
int st_install(STD sym_table, char *lexeme, char type, int line){
	/*Declare variables*/
	int currentOffset, findOffset, i, j, k, offset, length;
	STVR vr;
	unsigned short field;

	/*initialize*/
	currentOffset = sym_table.st_offset;

	/*Look up variable using st_lookup()*/
	if (0 <= (findOffset = st_lookup(sym_table, lexeme))){
		return findOffset;
	}

	/*check if table is full*/
	if (sym_table.st_size == sym_table.st_offset){
		return R_FAIL1;
	}

	/*if no lexeme found install it at current st_offset*/	
	/*set plex and o_line to their values*/
	j = b_size(sym_table.plsBD);

	/*save pointer for where its going to be saved*/
	vr.plex = sym_table.plsBD->cb_head + sym_table.plsBD->addc_offset;

	/*Copy lexeme into buffer*/
	for (i = 0; i < strlen(lexeme)+1; i++){
		b_addc(sym_table.plsBD, lexeme[i]);
	}
	

	/*check if memory reallocated*/
	if (b_rflag(sym_table.plsBD)){
		
		/*Reset variables*/
		offset = 0;
		i = 0;

		/*Go through lexemes, updating their pointers on each iteration*/
		while (i < sym_table.st_offset){
			/*Get the length of the current lexeme*/
			length = (int)strlen(sym_table.plsBD->cb_head + offset);

			/*Point this lexemes pointer to its position in the new buffer*/
			(sym_table.pstvr + i)->plex = sym_table.plsBD->cb_head + offset;

			/*Update the current offset and increment i*/
			offset = offset + length + 1;
			i++;
		}
	}

	/*Set status_field to default value*/
	field = field & ZERO;
	field = field | DEFAULT;

	/*Set status_fields type to the type of variable specified*/
	if ('I' == type){
		/*Integer variable*/
		field |= SET_INTEGER;

		/*set i_value to 0*/
		vr.i_value.int_val = 0;
	}
	else if ('F' == type){
		/*Floating point variable*/
		field |= SET_FLOAT;

		/*set i_value to 0*/
		vr.i_value.fpl_val = 0.0;
	}
	else if ('S' == type){
		/*String variable*/
		/*note that the mask for SET_STRING also sets the update flag*/
		field |= SET_STRING;

		/*set i_value to -1*/
		vr.i_value.str_offset = -1;
	}

	/*Set field into STVR*/
	vr.status_field = field;

	/*Set line value for vr*/
	vr.o_line = line;

	/*Save the lexeme*/
	sym_table.pstvr[sym_table.st_offset] = vr;

	/*Increment global st_offset*/
	st_incoffset();

	/*return this variables offset*/
	return sym_table.st_offset;	
}

/*Purpose: Searches for a particular lexeme
* Author: Mathew Boland
* History/Versions: Version 1.0, March 27th, 2016
* Called functions:
* Parameters: STD symbol table, pointer to lexeme
* Return value: Lexeme location if found, -1 if not.
* Algorithm: Cycle through each lexeme checking eachs
* contents until one is found or end of list (beginning
* since search starts from end) is found.
*/
int st_lookup(STD sym_table, char *lexeme){
	/*Declare variables*/
	int i;
	STVR * holder;

	/*Check for valid symbol table*/
	if (sym_table.st_size <= 0){
		return R_FAIL1;
	}

	/*Search through list until beginning reached*/
	for (i = sym_table.st_offset - 1; 0 <= i; i--){

		/*Check if lexeme matches stored lexeme for this entry*/
		if (0 == strcmp( (sym_table.pstvr+i)->plex, lexeme)){
			/*return value of i (current lexemes offset)*/
			return i;
		}
	}
	/*RSeturn (-1) to indicate lexeme was not found*/
	return -1;
}

/*Purpose: Updates the data type indicator in specified STVR
* Author: Mathew Boland
* History/Versions: Version 1.0, March 27th, 2016
* Called functions:
* Parameters: STD symbol table, int VID offset, char new type
* Return value: integer -1 on failure, vid_offset on success
* Algorithm: check if update flag set. If not set then change the
* type of the variable to the new specified type and set the
* update flag.
*/
int st_update_type(STD sym_table, int vid_offset, char v_type){
	/*declare variables*/
	STVR * holder;
	int testField;

	/*Test the arguments*/
	if (!sym_table.st_size || vid_offset > sym_table.st_offset || 0 > vid_offset){
		return R_FAIL2;
	}

	/*get STVR*/
	holder = &sym_table.pstvr[vid_offset];

	/*find LSB of status field*/
	testField = holder->status_field & CHK_UPDATE;

	/*check LSB to see if previously changed or is potentially a String*/
	if (0 != testField){
		return -1;
	}

	/*Not previously, updated so reset last three bits to default*/
	holder->status_field &= DEFAULT;

	/*set update bit*/
	holder->status_field |= SET_UPDATE;

	/*Set specified type*/
	if ('F' == v_type){
		/*Set type to floating point*/
		holder->status_field |= SET_FLOAT;
	}
	else if ('I' == v_type){
		/*set type to integer*/
		holder->status_field |= SET_INTEGER;
	}
	else{
		/*invalid type to change to, returning*/
		return -1;
	}
	return vid_offset;
}

/*Purpose: Updates the i_value of the specified STVR
* Author: Mathew Boland
* History/Versions: Version 1.0, March 27th, 2016
* Called functions:
* Parameters: STD symbol table, VID offset, InitialValue
* Return value: -1 on failure, vid_offset on success
* Algorithm: Get STVR from STD. Change i_value to indicated
* i_value.
*/
int st_update_value(STD sym_table, int vid_offset, InitialValue i_value){
	/*declare variables*/
	STVR * holder;

	/*Test the arguments*/
	if (!sym_table.st_size || vid_offset > sym_table.st_offset || 0 > vid_offset){
		return R_FAIL2;
	}

	/*get STVR*/
	holder = &sym_table.pstvr[vid_offset];

	/*set the new i_value*/
	holder->i_value = i_value;

	return vid_offset;
}

/*Purpose: To get the type of a given lexeme
* Author: Mathew Boland
* History/Versions: Version 1.0, March 27th, 2016
* Called functions:
* Parameters: STD symbol table, VID offset
* Return value: -1 on failure, F if floating point,
* S if String and I if integer.
* Algorithm: Get STVR then check bitfield to see
* what the type is. Return appropriate indicator.
*/
char st_get_type(STD sym_table, int vid_offset){
	/*declare variables*/
	STVR * holder;
	int field;

	/*get STVR*/
	holder = &sym_table.pstvr[vid_offset];

	/*get status_field*/
	field = holder->status_field;

	/*reset field bits other than bits 1 and 2*/
	field &= CHK_TYPE;

	if (CHK_FLOAT == field){
		/*floating-point type found*/
		return 'F';
	}
	else if (CHK_INTEGER == field){
		/*integer type found*/
		return 'I';
	}
	else if (CHK_STRING == field){
		/*String type found*/
		return 'S';
	}

	/*Variable should have one of previous types,
	something wrong must have happened. Return error.*/
	return -1;
}

/*Purpose: Frees the memory occupied by the symbol table
* Author: Mathew Boland
* History/Versions: Version 1.0, March 27th, 2016
* Called functions: free()
* Parameters: STd symbol table
* Return value: void
* Algorithm: free all allocated memory inside the symbol table
*/
void st_destroy(STD sym_table){
	/*Check for valid symbol table*/
	if (sym_table.st_size <= 0){
		return R_FAIL1;
	}
	/*free the SymbolTableVidRecord array*/
	free(sym_table.pstvr);

	/*Free the buffer*/
	b_free(sym_table.plsBD);	

	st_setsize();
}

/*Purpose: Print the contents of the symbol table to the screen
* Author: Mathew Boland
* History/Versions: Version 1.0, March 27th, 2016
* Called functions:
* Parameters: STD symbol table
* Return value: number of variables printed or -1 on failure
* Algorithm: Print header then cycle through each lexeme in the
* symbol table while printing their information.
*/
int st_print(STD sym_table){
	/*declare variables*/
	STVR * holder;
	int i, j, k, digits, currentDigit, spacing;
	char space;

	/*Check parameters*/
	if (0 == sym_table.st_offset){
		/*table is empty*/
		return -1;
	}

	/*Calculate maximum digits*/
	holder = &sym_table.pstvr[sym_table.st_offset - 1];
	k = holder->o_line;
	digits = 0;
	while (0 < k){
		k /= 10;
		digits++;
	}
	spacing = 13 - digits;

	/*print header*/
	printf("\nSymbol Table\n");
	printf("____________\n\n");
	printf("Line Number Variable Identifier\n");

	/*initialize j*/
	j = 0;
	space = ' ';


	/*Cycle through and print each variable*/
	for (i = 0; i < sym_table.st_offset; i++){
		/*get STVR*/
		holder = &sym_table.pstvr[i];
		k = holder->o_line;
		currentDigit = 0;
		while (0 < k){
			k /= 10;
			currentDigit++;
		}
		spacing = 12 - digits;
		/*Print the first line the VID appeared and the name of it*/
		printf("%*d%*c%s\n", digits - currentDigit + 1, holder->o_line, spacing, space, holder->plex);

		/*increment j*/
		j++;
	}
	return j;
}

/*Purpose: Set st_size to 0
* Author: Mathew Boland
* History/Versions: Version 1.0, March 27th, 2016
* Called functions:
* Parameters:
* Return value:
* Algorithm: Set st_size to 0 for global variable
* STD sym_table.
*/
static void st_setsize(void){
	sym_table.st_size = 0;
}

/*Purpose: Increments st_offset by 1
* Author: Mathew Boland
* History/Versions: Version 1.0, March 27th, 2016
* Called functions:
* Parameters:
* Return value:
* Algorithm: Increment the st_offset by 1 for the
* global variable STD sym_table.
*/
static void st_incoffset(void){
	++sym_table.st_offset;
}

/*Purpose: Stores the symbol table into a file
* named $stable.ste
* Author: Mathew Boland
* History/Versions: Version 1.0, March 27th, 2016
* Called functions:
* Parameters: STD symbol table
* Return value: -1 on failure, nmber of records
* stored on success.
* Algorithm: Open a file for writing. Cycle through
* all the lexemes while writing eaches information
* to the file. Once done close the file and return
* number of entries written to the file.
*/
int st_store(STD sym_table){
	/*declare variables*/
	STVR * holder;
	int i, j;
	char c;
	char *fileName;
	FILE *fi;

	/*initialize*/
	j = 0;
	fileName = "$stable.ste";

	/*open new file*/
	fi = fopen(fileName, "wt");

	/*check if file opened succesfully*/
	if (NULL == fi){
		/*File open failure*/
		return -1;
	}

	/*write st_size to file first*/
	fprintf(fi, "%d", sym_table.st_size);

	/*loop through lexemes*/
	for (i = 0; i < sym_table.st_offset; i++){
		/*get STVR*/
		holder = &sym_table.pstvr[i];

		/*write status_field in hex format, String length, line and lexeme name*/
		fprintf(fi, " %hX %d %s %d ", holder->status_field, strlen(holder->plex), holder->plex, holder->o_line);

		/*find type of lexeme*/
		c = st_get_type(sym_table, i);

		/*check if error encountered*/
		if (-1 == c){
			/*close file and exit*/
			fclose(fi);
			return -1;
		}

		/*write the initial value of the lexeme*/
		if ('F' == c){
			/*Floating-point lexeme*/
			fprintf(fi, "%.2f", holder->i_value.fpl_val);
		}
		else if ('I' == c){
			/*Integer lexeme*/
			fprintf(fi, "%d", holder->i_value.int_val);
		}
		else{
			/*String lexeme*/
			fprintf(fi, "%d", holder->i_value.str_offset);
		}
		/*increment lexeme counter*/
		j++;
	}
	/*Close file*/
	fclose(fi);

	/*Report save success*/
	printf("\nSymbol Table stored.\n");

	/*Return number of lexemes saved*/
	return j;
}

/*Purpose: To sort the symbol table by variable name.
* Author: Mathew Boland
* History/Versions: Version 1.0, March 27th, 2016
* Called functions:
* Parameters: STD symbol table, char order to sort in
* Return value: 1 on success, (-1) on failure
* Algorithm:
*/
int st_sort(STD sym_table, char s_order){
	return 0;
}