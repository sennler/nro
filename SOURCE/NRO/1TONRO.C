/*
*/
/*
    Transform *.1 files into *.NRO files
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
/* $RCSfile: 1TONRO.C $
   $Locker:  $	$Name:  $	$State: Exp $

	Transforms the quotations of an Unix-compatible NROFF
	to NRO v1.17 compatible commands.

*/

#include <stdio.h>

#include "nro.h"

#ifndef lint
static char rcsid[] = 
	"$Id: 1TONRO.C 1.2 1997/01/27 06:21:33 ska Exp $";
#endif

char dummy1 = ' ';
buffer buf[MAXLINE] = " ";
FILE *fi, *fo;
buffer cmdch = '.';
int font = 0;
FLAG nl = NO;

void ocmd();

/*
 *		print an error message, then terminate program
 */
register Error(unsigned args)
{
	char obuf[256];
	unsigned length;

	length = _format_(nargs() * 2 + &args, obuf);
	fputs("1toNro: ", stderr);
	abort(obuf);
}

void ofont(void)
{	if(nl) {
		switch(font) {
		case 1: ocmd("bo"); break;
		case 2: ocmd("ul"); break;
		case 3: ocmd("cb"); break;
		case 4: ocmd("cu"); break;
		}
		nl = NO;
	}
}

void oc(int ch)
{	ofont();
	putc(ch, fo);
	nl = NO;
}

void ocmdch(void)
{	putc(cmdch, fo);
	nl = NO;
}

void osp(void)
{	if(!nl)
		oc(' ');
}

void onl(void)
{	putc('\n', fo);
	nl = YES;
}

void onnl(void)
{	if(!nl) onl();
}

void os(char *s)
{	if(nl) while(isspace(*s)) ++s;
	if(*s) {
		ofont();
		fputs(s, fo);
	}
}

void ostr(char *s)
{	os(s);
	onnl();
}

void ocmd(char *s)
{	ocmdch();
	ostr(s);
}

void ocmdn(char *s)
{	ocmdch();
	os(s);
}

#define DIGRAPH(a,b) (((a) << 8) | (b))
unsigned digraphtbl[] = {
	DIGRAPH('"', 'a'),			/* national characters */
	DIGRAPH('"', 'o'),
	DIGRAPH('"', 'u'),
	DIGRAPH('"', 'A'),
	DIGRAPH('"', 'O'),
	DIGRAPH('"', 'U'),
	DIGRAPH('"', 's'),
	DIGRAPH('b', 'u'),			/* bullets etc. */
	0};
unsigned char maptbl[] = {
	  '„', '”', '', 'Ž', '™', 'š', 'á'
	, 'o'
};

void digraph(unsigned ch1, unsigned ch2)
{	unsigned *p;

	ch1 = (ch1 << 8) | (ch2 & 0xFF);
	ch2 = (ch2 << 8) | (ch1 >> 8);
	p = digraphtbl;
	do if(*p == ch1 || *p == ch2) {
		oc(maptbl[(p - digraphtbl) >> 1]);
		return;
	} while(*++p);
	Error("Unknown digraph %c%c", ch1 & 0xFF, ch2 & 0xFF);
}

int eow(unsigned ch)	/* ch == end of word? */
{	return !ch || isspace(ch);
}

main(void)
{	
	char *p, *s;
	FLAG cmd;

#ifndef TEST
	fi = setbuf(stdin, 512);
	fo = setbuf(stdout, 512);
#else
	fi = stdin;
	fo = stdout;
#endif

	font = 0;

	while(fgets(buf, sizeof(buf), fi)) {
		if(*(p = buf) == cmdch) {
			cmd = YES;
			if(memcmp(buf + 1, "\\\"", 2) == 0) {	/* comment */
				ocmdch();
				ocmdch();
				if(!isspace(*(p += 3)))
					osp();
			}
			else if(toupper(buf[1]) == 'C' && toupper(buf[2]) == 'C') {
				/* switch command character */
				while(*++p && !isspace(*p));
				while(isspace(*p)) ++p;
				cmdch = *p? *p: '.';
				ostr(buf);
				continue;
			}
		}
		else {
			cmd = NO;
			while(isspace(*p)) ++p;
			if(!*p) {
				onl();
				continue;
			}
		}
		--p;
		while(p = strchr(s = p + 1, '\\')) {
			if(s != p) {
				*p = '\0';
				os(s);
				*p = '\\';
			}
			switch(*++p) {
			case 'f':	/* font */
				if(cmd) /* within commands fonts has to be ignored */
					++p;
				else {
					onnl();
					switch(*++p) {
					case 'B': font = 3; break;
					case 'U':
					case 'I': font = 4; break;
					case 'P': font = 0; break;
					default: Error("Unknown font: %c", *p);
					}
					if(!eow(p[-3]) && !eow(p[1])) {
						ofont();
						ocmdn("ct");
						osp();
					}
				}
				break;
			case '(':	/* digraphs */
				digraph(*++p, *++p);
				break;
			default:
				oc('\\');
			case '-': case '\\':
				oc(*p);
			}
		}
		ostr(s);
	}
	fflush(fo);
}
