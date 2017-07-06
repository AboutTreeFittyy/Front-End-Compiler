/*File name: buffer.c
* Compiler: MS Visual Studio 2013
* Author: Mathew Boland, 040800005
* Course: CST 8152 - Compilers, Lab Section: 012
* Assignment: 1
* Date: February 1st, 2016
* Professor: Sv. Ranev
* Purpose: A set of functions that can be used to load, delete and parse through
* an array of chars.
* Funtion list: b_create(), b_addc(), b_reset(), b_free(), b_isfull(), b_size(),
* b_capacity(), b_setmark(), b_mark(), b_mode(), b_incfactor(), b_load(),
* b_isempty(), b_eob(), b_getc(), b_print(), b_pack(), b_rflag(), b_retract(),
* b_retract_to_mark(), b_getcoffset()
*/

#include "buffer.h"


/*Purpose:
* Author: Mathew Boland
* History/Versions: Version 1.0, February 1st, 2016
* Called functions: calloc(), malloc(), b_free(), b_reset()
* Parameters: short (greater than 0, less than SHRT_MAX), char (greater than or
* equal to zero), char (m, f, a)
* Return value: Pointer to buffer
* Algorithm: Check validity of arguments, allocate memory for buffer, initialize
* buffer variables to proper values, return pointer to buffer.
*/
Buffer * b_create(short init_capacity, char inc_factor, char o_mode)
{
	/*buffer pointer to be used for ne buffer*/
	Buffer * buffer;

	/*factor used to indicate numerical value of inc_factor*/
	unsigned int factor;
	factor = (unsigned int)((unsigned char)inc_factor);

	/*Check if init_capacitys' range is valid*/
	if (init_capacity < 0)
		return NULL;

	/*Check if attempting to create non-resizable buffer of capacity 0*/
	if (init_capacity == 0 && o_mode == 'f')
		return NULL;

	/*Allocate memory for the buffer struct*/
	buffer = calloc(sizeof(Buffer), 1);

	/*Check to see if memory was successfully allocated*/
	if (buffer == NULL)
	{
		/*Free memory if not successfully allocated to avoid memory leaks*/
		return NULL;
	}

	/*Allocate memory for the char array*/
	buffer->cb_head = (char*)malloc(init_capacity * sizeof(char));

	/*Check to see if memory was successfully allocated*/
	if (buffer->cb_head == NULL)
	{
		/*Free memory if not successfully allocated to avoid memory leaks*/
		b_free(buffer);;
		return NULL;
	}

	/*Check what mode is to be used, return null and free memory if no proper condition is given*/
	if (o_mode == 'f' || inc_factor == 0)
	{
		buffer->inc_factor = 0;
		buffer->mode = 0;
	}
	else if (o_mode == 'a' && factor <= MAX_INC && factor >= 1)
	{
		buffer->inc_factor = inc_factor;
		buffer->mode = 1;
	}
	else if (o_mode == 'm' && inc_factor <= 100 && inc_factor >= 1)
	{
		buffer->inc_factor = inc_factor;
		buffer->mode = -1;
	}
	else
	{
		/*memory is freed as buffer mode was not correctly indicated*/
		b_free(buffer);
		return NULL;
	}

	/*Set buffer capacity*/
	buffer->capacity = init_capacity;

	return buffer;
}

/*Purpose: To append a new char to the end of the buffer if it is not full
* Author: Mathew Boland
* History/Versions: Version 1.0, February 1st, 2016
* Called functions: realloc()
* Parameters: buffer, char
* Return value: pBuffer
* Algorithm: Check buffer and append char if not full. Otherwise attempt to
* increment buffer capacity then append char on success.
*/
pBuffer b_addc(pBuffer const pBD, char symbol)
{
	/*Variables used for calculations*/
	long Available_Space, New_Increment, New_Capacity;

	/*Char pointer used to test success of reallocating memory*/
	char *Test_Realloc;

	/*Initialize variables*/
	pBD->r_flag = 0;

	/*Check if able to add char or if buffer capacity needs to be incremented*/
	if (pBD->capacity > pBD->addc_offset)
	{
		/*Add char to buffer*/
		pBD->cb_head[pBD->addc_offset] = symbol;
		/*Increment buffer offset*/
		pBD->addc_offset++;
		return pBD;
	}
	else if (pBD->mode == 0)
	{
		/*Capacity reached, mode set to non-incrementing, unable to add character*/
		return NULL;
	}
	else if (pBD->mode == 1)
	{
		/*Capacity reached, mode set to add increment*/
		New_Capacity = pBD->capacity + (unsigned char)pBD->inc_factor;

		/*Check for overflow*/
		if (New_Capacity < 0)
		{
			return NULL;
		}
	}
	else if (pBD->mode == -1)
	{
		/*Capacity reached, mode set to multiply increment*/
		/*Check if already at maximum capacity*/
		if (pBD->capacity == SHRT_MAX)
		{
			return NULL;
		}

		/*Calculate New Capacity*/
		Available_Space = SHRT_MAX - pBD->capacity;
		New_Increment = (long)Available_Space * pBD->inc_factor / 100;
		New_Capacity = pBD->capacity + New_Increment;

		/*Check what action to take on new capacity*/
		if (New_Capacity > SHRT_MAX)
		{
			/*Set capacity to max*/
			New_Capacity = SHRT_MAX;
		}

	}
	else
	{
		/*This should never execute, only here in case somehow no mode managed to be set*/
		return NULL;
	}

	/*Set Test_Realloc to the buffer pointer and set it equal to realloc (That way if unable to realloc the pointer isn't lost)*/
	Test_Realloc = realloc(pBD->cb_head, New_Capacity);

	/*Check if reallocation was successful*/
	if (Test_Realloc == NULL)
	{
		/*Reallocation failed, returning NULL*/
		return NULL;
	}
	else if (Test_Realloc != pBD->cb_head)
	{
		/*Pointer has changed, setting r_flag*/
		pBD->r_flag = SET_R_FLAG;
		/*Reallocation was succesful setting buffer data member cb_head to new pointer*/
		pBD->cb_head = Test_Realloc;
	}

	/*Add char to buffer*/
	pBD->cb_head[pBD->addc_offset] = symbol;
	/*Update buffer data members*/
	pBD->addc_offset++;
	pBD->capacity = (short)New_Capacity;

	return pBD;
}

/*Purpose: To reinitialize the buffer and makeit appear empty.
* Author: Mathew Boland
* History/Versions: Version 1.0, February 1st, 2016
* Called functions: none
* Parameters: Pointer to a buffer
* Return value: integer (-1 on failure, 1 on success)
* Algorithm: Set data members to zero
*/
int b_reset(Buffer * const pBD)
{
	/*Check if NULL pointer passed*/
	if (pBD == NULL)
	{
		return R_FAIL1;
	}

	/*Set buffer data members to zero*/
	pBD->addc_offset = 0;
	pBD->getc_offset = 0;
	pBD->mark_offset = 0;
	pBD->r_flag = 0;
	pBD->eob = 0;

	return 1;
}

/*Purpose: Deallocates memory reserved for buffer
* Author: Mathew Boland
* History/Versions: Version 1.0, February 1st, 2016
* Called functions: free()
* Parameters: pointer to buffer
* Return value: void
* Algorithm: call free on char array and buffer struct
*/
void b_free(Buffer * const pBD)
{
	/*Check for null*/
	if (pBD == NULL)
	{
		return;
	}

	/*check that head has been allocated before deleting*/
	if (NULL != pBD->cb_head){
		free(pBD->cb_head);
	}

	/*Free rest of buffer*/
	free(pBD);
}

/*Purpose: To indicate if the given buffer is full
* Author: Mathew Boland
* History/Versions: Version 1.0, February 1st, 2016
* Called functions: none
* Parameters: pointer to buffer
* Return value: integer (-1 on error, 1 on full and 0 on not full)
* Algorithm:
*/
int b_isfull(Buffer * const pBD)
{
	/*Check if pointer is NULL*/
	if (pBD == NULL)
		return R_FAIL1;

	/*Return 1 if capacity reached*/
	if (pBD->capacity == pBD->addc_offset)
		return 1;

	/*Return 0, buffer is not full*/
	return 0;
}

/*Purpose: To indicate the current size of the buffer
* Author: Mathew Boland
* History/Versions: Version 1.0, February 1st, 2016
* Called functions: none
* Parameters: pointer to buffer
* Return value: short (-1 on failure, size of buffer on success)
* Algorithm:
*/
short b_size(Buffer * const pBD)
{
	/*Check if NULL pointer passed*/
	if (pBD == NULL)
		return R_FAIL1;

	return pBD->addc_offset;
}

/*Purpose: To indicate the current capacity of the buffer
* Author: Mathew Boland
* History/Versions: Version 1.0, February 1st, 2016
* Called functions: none
* Parameters: pointer buffer
* Return value: short (-1 on failure, capacity of buffer on success)
* Algorithm:
*/
short b_capacity(Buffer * const pBD)
{
	/*Check if NULL pointer passed*/
	if (pBD == NULL)
		return R_FAIL1;

	return pBD->capacity;
}

/*Purpose: To set the mark
* Author: Mathew Boland
* History/Versions: Version 1.0, February 1st, 2016
* Called functions: b_size(),
* Parameters: pointer to buffer, short for mark point (must be less
* than buffer size and greater than 0)
* Return value: pointer to char
* Algorithm:
*/
char * b_setmark(Buffer * const pBD, short mark)
{
	/*Pointer to a char to be set to indicated char in buffer then returned*/
	char *markPointer;

	/*Check if NULL pointer passed*/
	if (pBD == NULL)
		return NULL;

	/*Make sure mark is within the buffer*/
	if (mark > b_size(pBD) || mark < 0)
		return NULL;

	pBD->mark_offset = mark;
	markPointer = &pBD->cb_head[mark];

	return markPointer;
}

/*Purpose: To indicate the current mark_offset
* Author: Mathew Boland
* History/Versions: Version 1.0, February 1st, 2016
* Called functions: none
* Parameters: pointer to buffer
* Return value: short (-1 on failure, mark value on success)
* Algorithm:
*/
short b_mark(Buffer * const pBD)
{
	/*Check if NULL pointer passed*/
	if (pBD == NULL)
		return R_FAIL1;

	return pBD->mark_offset;
}

/*Purpose: To indicate the current mode
* Author: Mathew Boland
* History/Versions: Version 1.0, February 1st, 2016
* Called functions: none
* Parameters: pointer to buffer
* Return value: integer (-2 on failure, mode value on success)
* Algorithm:
*/
int b_mode(Buffer * const pBD)
{
	/*Check if NULL pointer passed*/
	if (pBD == NULL)
		return R_FAIL2;

	return pBD->mode;
}

/*Purpose: To indicate the increment factor
* Author: Mathew Boland
* History/Versions: Version 1.0, February 1st, 2016
* Called functions: none
* Parameters: pointer to buffer
* Return value: size_t, (size of inc_factor on success, 256 on failure)
* Algorithm:
*/
size_t b_incfactor(Buffer * const pBD)
{
	/*Check if NULL pointer passed*/
	if (pBD == NULL)
		return OVER_MAX_INC;

	return ((unsigned char)pBD->inc_factor);
}

/*Purpose: Loads the given file into the buffer
* Author: Mathew Boland
* History/Versions: Version 1.0, February 1st, 2016
* Called functions: feof(), fgetc()
* Parameters: FILE pointer, pointer to buffer
* Return value: integer (LOAD_FAIL on loading failure, -1 on other failure,
* character count on success)
* Algorithm: Check given parameters, read in file stream until feof reached,
* return the character count.
*/
int b_load(FILE * const fi, Buffer * const pBD)
{
	/*Struct pointer used to check if b_addc returns an error*/
	struct BufferDescriptor *Check_Error;

	/*Char used to temporarily store char retrieved from fgetc()*/
	char charIn;

	/*Counter used to count total characters added into buffer*/
	short charCount;

	/*Initializing charCount*/
	charCount = 0;

	/*Check if NULL pointer passed or NULL file pointer*/
	if (pBD == NULL || fi == NULL)
		return -1;

	/*This causes a warning due to making char from integer value*/
	charIn = (char)fgetc(fi);

	/*Add characters one at a time from a file using fgetc() to the buffer with the function b_addc()*/
	while (!(feof(fi)))
	{
		/*Add the char into the buffer*/
		Check_Error = b_addc(pBD, charIn);

		/*Check if char was successfully added to the buffer*/
		if (Check_Error == NULL)
		{
			return LOAD_FAIL;
		}

		charCount++;

		/*This causes a warning due to making char from integer value*/
		charIn = (char)fgetc(fi);
	}

	return charCount;
}

/*Purpose: To indicate if the buffer is empty
* Author: Mathew Boland
* History/Versions: Version 1.0, February 1st, 2016
* Called functions: none
* Parameters: pointer to buffer
* Return value: integer (-1 on failure, 1 if empty, 0 if not empty)
* Algorithm:
*/
int b_isempty(Buffer * const pBD)
{
	/*Check if NULL pointer passed*/
	if (pBD == NULL)
		return R_FAIL1;

	/*Return 1 if buffer is empty*/
	if (pBD->addc_offset == 0)
		return 1;

	/*Return 0 if buffer is not empty*/
	return 0;
}

/*Purpose: To indicate if end of buffer has been reached
* Author: Mathew Boland
* History/Versions: Version 1.0, February 1st, 2016
* Called functions: none
* Parameters: pointer to buffer
* Return value: integer (-1 on failure, 0 if not reached, 1 if reached)
* Algorithm:
*/
int b_eob(Buffer * const pBD)
{
	/*Check if NULL pointer passed*/
	if (pBD == NULL)
		return R_FAIL1;

	return pBD->eob;
}

/*Purpose: To get the current char at getc_offset
* Author: Mathew Boland
* History/Versions: Version 1.0, February 1st, 2016
* Called functions: b_isempty()
* Parameters: pointer to buffer
* Return value: char (-2 if buffer empty, -1 on failure, char on success)
* Algorithm: check parameters, set eob then return char at getc_offset before incrementing
*/
char b_getc(Buffer * const pBD)
{
	/*Check if the buffer is empty*/
	if (b_isempty(pBD) != 0)
		return R_FAIL2;

	/*Check if end of buffer reached*/
	if (pBD->getc_offset == pBD->addc_offset)
	{
		pBD->eob = 1;
		return R_FAIL1;
	}

	/*Set eob to 0*/
	pBD->eob = 0;
	pBD->getc_offset++;
	/*return the char at getc_offset before it was incremented*/
	return pBD->cb_head[pBD->getc_offset - 1];
}

/*Purpose: To print the contents of the buffer
* Author: Mathew Boland
* History/Versions: Version 1.0, February 1st, 2016
* Called functions: b_isempty(), b_getc(), printf(), b_eob()
* Parameters: pointer to buffer
* Return value: integer (-1 on failure, total chars counted on success)
* Algorithm: Check parameters, iterate through buffer printing each char individually
* and incrementing charCount until eob has been reached.
*/
int b_print(Buffer * const pBD)
{
	/*Counts total chars*/
	int charCount;

	/*Holds current char retrieved*/
	char charIn;

	/*Initialize charCount*/
	charCount = 0;

	/*Check if NULL pointer passed*/
	if (pBD == NULL)
		return R_FAIL1;

	/*Check if buffer is empty*/
	if (b_isempty(pBD) == 1)
	{
		printf("The buffer is empty.\n");
		return R_FAIL1;
	}

	/*Set the getc_offset to the start of the buffer*/
	pBD->getc_offset = 0;

	/*Print and count each char from buffer until end of buffer reached.*/
	do{
		/*Get the current char from the buffer*/
		charIn = b_getc(pBD);

		/*Print character only if eob isn't reached*/
		if (b_eob(pBD) != 1)
			printf("%c", charIn);
		charCount++;
	} while (b_eob(pBD) != 1);

	/*Reset getc_offset, print a newline (to ensure all is printed to the screen) */
	pBD->getc_offset = 0;
	pBD->eob = 0;
	printf("\n");

	return charCount;
}

/*Purpose: To shrink (or potentially grow) the buffer to current size+1
* Author: Mathew Boland
* History/Versions: Version 1.0, February 1st, 2016
* Called functions: realloc()
* Parameters: pointer to buffer
* Return value: pointer to buffer
* Algorithm: Check parameters, reallocate memory, increase capacity by 1
*/
Buffer *b_pack(Buffer * const pBD)
{
	/*Char pointer used to test success of reallocation of memory*/
	char *Test_Realloc;

	/*Check if NULL pointer passed*/
	if (pBD == NULL)
		return NULL;

	/*Check if the current capacity is at the maximum possible capacity*/
	if (pBD->capacity == SHRT_MAX)
	{
		pBD->r_flag = RESET_R_FLAG;
		return NULL;
	}


	/*Reallocate the char pointer to current size plus 1*/
	Test_Realloc = realloc(pBD->cb_head, pBD->addc_offset + 1);

	/*Check if reallocation was successful*/
	if (Test_Realloc == NULL)
	{
		return NULL;
	}
	else if (Test_Realloc != pBD->cb_head)
	{
		/*Pointer has changed, setting r_flag*/
		pBD->r_flag = SET_R_FLAG;
		pBD->cb_head = Test_Realloc;
	}

	/*Update buffer data member addc_offset*/
	pBD->capacity = pBD->addc_offset + 1;
	return pBD;
}

/*Purpose: To indicate the status of the reallocation flag
* Author: Mathew Boland
* History/Versions: Version 1.0, February 1st, 2016
* Called functions: none
* Parameters: pointer to buffer
* Return value: char (-1 on failure, value of r_flag on success)
* Algorithm:
*/
char b_rflag(Buffer * const pBD)
{
	/*Check if NULL pointer passed*/
	if (pBD == NULL)
		return R_FAIL1;

	return pBD->r_flag;
}

/*Purpose: To set current getc_offset to the previous char
* Author: Mathew Boland
* History/Versions: Version 1.0, February 1st, 2016
* Called functions: none
* Parameters: pointer to buffer
* Return value: short (-1 on failure, getc_offset value on success)
* Algorithm:
*/
short b_retract(Buffer * const pBD)
{
	/*Check if NULL pointer passed*/
	if (pBD == NULL)
		return R_FAIL1;

	/*Check if getc_offset is at 0 since that could lead to indexing less than 0*/
	if (pBD->getc_offset == 0)
		return 0;

	pBD->getc_offset--;
	return pBD->getc_offset;
}

/*Purpose: To set the buffers getc_offset data member to the current mark
* Author: Mathew Boland
* History/Versions: Version 1.0, February 1st, 2016
* Called functions: none
* Parameters: pointer to buffer
* Return value: short (-1 on failure, value of getc_offset on success)
* Algorithm:
*/
short b_retract_to_mark(Buffer * const pBD)
{
	/*Check if NULL pointer passed*/
	if (pBD == NULL)
		return R_FAIL1;

	pBD->getc_offset = pBD->mark_offset;
	return pBD->getc_offset;
}

/*Purpose: To indicate the current getc_offset
* Author: Mathew Boland
* History/Versions: Version 1.0, February 1st, 2016
* Called functions: none
* Parameters: pointer to buffer
* Return value: short (-1 on failure, value of getc_offset on success)
* Algorithm:
*/
short b_getcoffset(Buffer * const pBD)
{
	/*Check if NULL pointer passed*/
	if (pBD == NULL)
		return R_FAIL1;

	return pBD->getc_offset;
}