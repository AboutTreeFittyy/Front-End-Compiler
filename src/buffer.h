/*File name: buffer.h
* Compiler: MS Visual Studio 2013
* Author: Mathew Boland, 040800005
* Course: CST 8152 - Compilers, Lab Section: 012
* Assignment: 1
* Date: February 1st, 2016
* Professor: Sv. Ranev
* Purpose: Header file for a set of functions that can be used to load, delete and parse through
* an array of chars.
* Funtion list: b_create(), b_addc(), b_reset(), b_free(), b_isfull(), b_size(),
* b_capacity(), b_setmark(), b_mark(), b_mode(), b_incfactor(), b_load(),
* b_isempty(), b_eob(), b_getc(), b_print(), b_pack(), b_rflag(), b_retract(),
* b_retract_to_mark(), b_getcoffset()
*/
#ifndef BUFFER_H_
#define BUFFER_H_

/*#pragma warning(1:4001) /*to enforce C89 type comments  - to make //comments an warning */

/*#pragma warning(error:4001)/* to enforce C89 comments - to make // comments an error */

/* standard header files */
#define _CRTDBG_MAP_ALLOC

#include <crtdbg.h>
#include <stdio.h>  /* standard input/output */
#include <malloc.h> /* for dynamic memory allocation*/
#include <limits.h> /* implementation-defined data type ranges and limits */

/* constant definitions */
/* You may add your own constant definitions here */
#define R_FAIL1 -1          /* fail return value */
#define R_FAIL2 -2          /* fail return value */
#define LOAD_FAIL -2        /* load fail error */
#define SET_R_FLAG 1        /* realloc flag set value */
#define RESET_R_FLAG 0		/* reset rflag so user knows there was no reallocation */
#define MAX_INC 255			/* Maximum increment factor for mode a */
#define OVER_MAX_INC 256	/* one larger than maximum increment */


/* user data type declarations */
typedef struct BufferDescriptor {
	char *cb_head;   /* pointer to the beginning of character array (character buffer) */
	short capacity;    /* current dynamic memory size (in bytes) allocated to character buffer */
	short addc_offset;  /* the offset (in chars) to the add-character location */
	short getc_offset;  /* the offset (in chars) to the get-character location */
	short mark_offset; /* the offset (in chars) to the mark location */
	char  inc_factor; /* character array increment factor */
	char  r_flag;     /* reallocation flag */
	char  mode;       /* operational mode indicator*/
	int   eob;       /* end-of-buffer flag */
} Buffer, *pBuffer;
/*typedef Buffer *pBuffer;*/

/* function declarations */
Buffer * b_create(short int_capacity, char inc_factor, char o_mode);
pBuffer b_addc(pBuffer const pBD, char symbol);
int b_reset(Buffer * const pBD);
void b_free(Buffer * const pBD);
int b_isfull(Buffer * const pBD);
short b_size(Buffer * const pBD);
short b_capacity(Buffer * const pBD);
char * b_setmark(Buffer * const pBD, short mark);
short b_mark(Buffer * const pBD);
int b_mode(Buffer * const pBD);
size_t b_incfactor(Buffer * const pBD);
int b_load(FILE * const fi, Buffer * const pBD);
int b_isempty(Buffer * const pBD);
int b_eob(Buffer * const pBD);
char b_getc(Buffer * const pBD);
int b_print(Buffer * const pBD);
Buffer *b_pack(Buffer * const pBD);
char b_rflag(Buffer * const pBD);
short b_retract(Buffer * const pBD);
short b_retract_to_mark(Buffer * const pBD);
short b_getcoffset(Buffer * const pBD);

#endif
