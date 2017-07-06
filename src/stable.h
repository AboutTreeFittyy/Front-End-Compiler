/*File name: stable.h
* Compiler: MS Visual Studio 2013
* Author: Mathew Boland, 040800005
* Course: CST 8152 - Compilers, Lab Section: 012
* Assignment: 3
* Date: March 27th, 2016
* Professor: Sv. Ranev
* Purpose: declares all functions and structures
* needed for the symbol table.
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

#ifndef STABLE_H
#define STABLE_H

#ifndef BUFFER_H_
#include "buffer.h"
#endif

/*Mask defines*/
#define ZERO	    0x0000   /* 0000 0000 0000 0000 */
#define DEFAULT     0xFFF8   /* 1111 1111 1111 1000 */
#define SET_FLOAT	0x0002	 /* 0000 0000 0000 0010 */
#define SET_INTEGER	0x0004	 /* 0000 0000 0000 0100 */
#define SET_STRING	0x0007	 /* 0000 0000 0000 0111 */
#define CHK_FLOAT	0x0002	 /* 0000 0000 0000 0010 */
#define CHK_INTEGER	0x0004	 /* 0000 0000 0000 0100 */
#define CHK_STRING	0x0006	 /* 0000 0000 0000 0110 */
#define CHK_TYPE	0x0006   /* 0000 0000 0000 0110 */
#define SET_UPDATE  0x0001   /* 0000 0000 0000 0001 */
#define CHK_UPDATE  0x0001   /* 0000 0000 0000 0001 */

/*Structure declarations*/
typedef union InitialValue {
	int int_val; /*integer variable initial value*/
	float fpl_val; /*floating-point variable initial value*/
	int str_offset; /*string variable initial value (offset)*/
} InitialValue;


typedef struct SymbolTableVidRecord {
	unsigned short status_field; /*variable record status field*/
	char * plex; /*pointer to lexeme (VID name) in CA*/
	int o_line; /*line of first occurence*/
	InitialValue i_value; /*variable initial value*/
	void * reserved; /*reserved for future use*/
} STVR;


typedef struct SymbolTableDescriptor{
	STVR *pstvr; /*pointer to array of STVR*/
	int st_size; /*size in number of STVR elements*/
	int st_offset; /*offset in number of STVR elements*/
	Buffer *plsBD; /*pointer to the lexeme storage buffer descriptor*/
} STD;

/*Function prototypes*/
STD st_create(int st_size);
int st_install(STD sym_table, char *lexeme, char type, int line);
int st_lookup(STD sym_table, char *lexeme);
int st_update_type(STD sym_table, int vid_offset, char v_type);
int st_update_value(STD sym_table, int vid_offset, InitialValue i_value);
char st_get_type(STD sym_table, int vid_offset);
void st_destroy(STD sym_table);
int st_print(STD sym_table);
int st_store(STD sym_table);
int st_sort(STD sym_table, char s_order);

#endif
