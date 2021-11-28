/*
*/
/*
	INTERPRETE - interprete control characters in an input stream
    Copyright (C) 1997  Steffen Kaiser

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/* $RCSfile: INTERPRE.C $
   $Locker:  $	$Name:  $	$State: Exp $

	Useage:
		interprete <input >output
	or
		other_program its_arguments | interprete >output

	Note: You must pipe or redirect the data into 'interprete', otherwise
		it will read the console for your input!
	Tabs are expanded to 8 (eight) spaces.

	Target compilers:
		Borland C, Micro-C

*/


#ifndef lint
static char rcsid[] = 
	"$Id: INTERPRE.C 1.2 1997/01/19 07:19:29 ska Exp $";
#endif

#include <stdio.h>
#ifndef _MICROC_
#include <string.h>
#include <process.h>
#include <fcntl.h>
#include <io.h>
#else
#include <file.h>
#endif

#define BUF 1024
#define clearBuf memset(b, ' ', BUF)
char b[BUF];


#ifdef _MICROC_
#define _AL (nargs() & 0xFF)
#endif

main(void)
{	int c;
	int i,p;

/* switch the input stream into binary mode */
#ifdef _MICROC_
	FILE *pin, *pout;

	stdin->FILE_options |= F_BINARY;
	stdin->FILE_handle;			/* mov ax, stdin->FILE_handle */
	asm {
		mov bx, ax
		mov ax, 4400h			/* query information about handle */
		int 21h
		jc noDevice
		test dl, 80h
		jz noDevice
		xor dh, dh
		or dl, 32
		mov ax, 4401h
		int 21h
noDevice:
	}
	pin = setbuf(stdin, 512);
	pout = setbuf(stdout, 512);
#else
#define pin stdin
#define pout stdout
	setmode(fileno(stdin), O_BINARY);
	stdin->flags |= _F_BIN;
#endif

	clearBuf;
	i = 0;
	while(1) {
		c = getc(stdin);

		switch(c) {
			case EOF:
			case 0x1a: /* ^Z */
#ifdef _MICROC_
				fflush(pout);
				return;
#else
				return 0;
#endif
			case 0xd: i = 0; break; /* carrage return */
			case 0xa: /* new line */
				p = BUF - 1;
				while(p && b[p-1] == ' ') --p;
				b[p] = 0;
				fputs(b, pout);
				putc('\n', pout);
				clearBuf;
				break;
			case ' ':	++i; /* fall through */		/* space already there */
#ifdef _MICROC_
			case '\7': break;	/* bell */
#else
			case '\a': break; /* bell */
#endif
			case '\t': i = (i & ~7) + 8; break; /* tab */
			case '\b': if(!i--) i = 0; /* Cursor left */
				break;
			default: b[i++] = c;
				if(i >= BUF-1) {
					fputs("Buffer overflow\n", stderr);
					exit(1);
				}
		}
	}
}
