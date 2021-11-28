/*
*/
/* $RCSfile: NRO.C $
   $Locker: ska $	$Name:  $	$State: Exp $

 * NROFF clone for the PC.
 *
 * This program was taken from the SIMTEL archive. To reduce the number
 * of files in my collection, I have removed files which were either
 * irrevalent or easily recreatable. I also combined several smaller
 * separate files into single larger ones. I have left the individual
 * file headings intact should you wish to recreate the originals,
 * however the program will compile "as is".            - Dave Dunfield
 *
 * Compile command: cc nro -fop
 *
 * Target compilers: Micro-C v.3.14
 *
 * Modifications made by Steffen.Kaiser@fh-rhein-sieg.de (ska)

*/

#ifndef lint
static char rcsid[] = 
	"$Id: NRO.C 1.20 1997/01/27 06:23:44 ska Exp ska $";
#endif

#define clear(arr) memset(arr, 0, sizeof(arr))	/* clear out an array */

#include <stdio.h>
#define EXTERN
#include "nro.h"

#include "cmds.inc"


char *letter = NULL;


/*
 *		print an error message, then terminate program
 */
register Error(unsigned args)
{
	char buf[256];
	unsigned length;

	length = _format_(nargs() * 2 + &args, buf);
	fputs("***nro: ", stderr);
	abort(buf);
}


main(argc,argv)
int argc;
char *argv[];
{
        int i;
        
#ifdef TEST
pout = stdout;
#else
        pout = setbuf(stdout, 512);	/* Increase output efficency */
#endif
        init();
        for (i=1; i<argc; ++i) {
			if (*argv[i] == '-' || *argv[i] == '+') {
				if (pswitch(argv[i]) == ERR)
					Error("illegal switch: %s", argv[i]);
			}
        }
        for (i=1; i<argc; ++i) {
			if (*argv[i] != '-' && *argv[i] != '+'/* && *argv[i] != '>'*/) {
				if ((sofile[0] = fopen(argv[i],"r")) == NULL)
					Error("unable to open file %s",argv[i]);
				else {
					profile();
					fclose(sofile[0]);
				}
			}
        }
        if (!sofile[0]) {	/* this cannot be NULL, as at least one file
        						was opened on success. Here it is not
        						interesting, if a "file" or a '-m' file
        						was opened. */
			puts("Usage: nro [-n] [+n] [-pxx] [-v] [-u] [-b] [-mmacfile] infile ... [>outfile]\n");
			exit(-1);
        }
        fflush(pout);
}


/*
 *      retrieve one line of input text
 */

getlin(p,in_buf)
buffer *p;
FILE *in_buf;
{
	int i;
	int c;
	buffer *q;

	q = p;
	i = MAXLINE + 1;
	while(--i) {
redo:
		switch(c = ngetc(in_buf)) {
		case CPMEOF: case EOF:
			if(q == p) return EOF;		/* no character */
			/*FALL THROUGH*/
		case '\n': case '\0':
			goto breakWhile;
		case TOKEN:
			Error("Input stream contains invalid control character: %u", c);
		case '\r':			/* ignore */
			goto redo;
		}
		*q++ = c;
	}
breakWhile:
	*q = EOS;
	return MAXLINE - i;
}



/*
 *      initialize parameters for nro word processor
 */

init()
{

	/* first: make sure all the structures are null'ed */
	clear(dc);
	clear(cs);
	clear(pg);
	clear(co);
	clear(mac);
	clearTS();

	dc.fill = YES;
	dc.lsval = 1;
	dc.rmval = PAGEWIDTH - 1;
	dc.juval = YES;
	dc.pgchr = '#';
	dc.cmdchr = '.';
	dc.prflg = TRUE;
	pg.newpag = 1;
	pg.plval = PAGELEN;
	pg.m1val = 2;
	pg.m2val = 2;
	pg.m3val = 2;
	pg.m4val = 2;
	pg.bottom = pg.plval - pg.m4val - pg.m3val;
	pg.lastpg = 30000;
	pg.ehlim[LEFT] = pg.ohlim[LEFT] = dc.inval;
	pg.eflim[LEFT] = pg.oflim[LEFT] = dc.inval;
	pg.ehlim[RIGHT] = pg.ohlim[RIGHT] = dc.rmval;
	pg.eflim[RIGHT] = pg.oflim[RIGHT] = dc.rmval;
	co.outp = co.outbuf;
	mac.mac_emb = mac.mac_buf;	/* no stored macro definition */
	mac.mac_ppb = MACEND;	/* no pushed back ch */
}


/*
 *      get character from input file or push back buffer
 */

ngetc(infp)
FILE *infp;
{
	return (mac.mac_ppb < MACEND)? (int)*mac.mac_ppb++: getc(infp);
}



/*
 *      process input files from command line
 */

profile()
{
	buffer ibuf[MAXLINE];

	for (dc.flevel=0; dc.flevel>=0; --dc.flevel) {
		while (getlin(ibuf,sofile[dc.flevel]) != EOF) {
			if (ibuf[0] == dc.cmdchr) comand(ibuf);
			else text(ibuf);
		}
		if (dc.flevel > 0) fclose(sofile[dc.flevel]);
	}
	if (pg.lineno > 0) space(HUGE);
}



/*
 *      process switch values from command line
 */

extern char *ARGV[];
pswitch(p)
char *p;
{	char fnam[MAXLINE], *q;

	if (*p == '-') {
		switch (tolower(*++p)) {
		case 'b':
				dc.bsflg = TRUE;
				break;
		case 'm':
				++p;
				if ((sofile[0] = fopen(p,"r")) == NULL) {
					if(ARGV[0]) {	/* search for the file in the directory
									where NRO is situated */
						q = stpcpy(fnam, ARGV[0]);
						while(--q >= fnam && !strchr("\\:/", *q));
						strcpy(q + 1, p);
						if(sofile[0] = fopen(fnam, "r"))
							goto sourceIn;
					}
					Error("unable to open file %s",p);
				}
		sourceIn:
				profile();
				fclose(sofile[0]);
				sofile[0] = NULL;
				break;
		case 'p':
				++p;
				set(&pg.offset,ctod(p),'1',0,0,HUGE);
				break;
		case 'u':
				dc.buflg = TRUE;
				break;
		case 'v':
				puts("NRO version 1.01 JRD 11/25/90");
				puts("Micro-C port $Revision: 1.20 $ $Date: 1997/01/27 06:23:44 $");
				puts("Modifications made by ska");
				break;
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
				pg.lastpg = ctod(p);
				break;
		default:
				return ERR;
		}
	}
	else if (*p == '+') {
			pg.frstpg = ctod(++p);
	}
	else return ERR;
	return(OK);
}


/*
 *		initialize one tabstop
 */
void setTS(tab, column, typ, delim)
int tab, column;
buffer typ, delim;
{	struct tabulator *p;

	p = tabstop[tab];
	p->tbcolumn = column;
	p->tbtyp = typ;
	p->tbdelim = delim;
}


/*
 *		clear all tabstops
 */
void clearTS(void)
{	memset(tabstop, NOTAB, sizeof(tabstop));
	memset(tabstop, 0, sizeof(tabstop[0]));
}


int getTS(val)		/* search the tabstop val */
int val;
{	int index;

	index = 0;
	do if(tabstop[index].tbcolumn >= val) return index;
	while(++index < MAXTABSTOPS);
	return MAXTABSTOPS - 1;
}

void fixedSpaces(no)
int no;
{	co.outw += no;
	while(no > 127) {
		co.outp = puttok2(co.outp, TOK_SPACES, 0xFF);
		no -= 127;
	}
	co.outp = puttok2(co.outp, TOK_SPACES, 0x80 | no);
}

void insertStr(dest, src, destEnd, length)
buffer *dest, *src, *destEnd;
int length;
{	memmove(dest + length, dest, destEnd - dest);
	memcpy(dest, src, length);
}

void insertSpaces(no)
int no;
{	buffer spaces[MAXLINE];
	buffer *p;
	int length;

	p = co.outp;
	co.outp = spaces;
	fixedSpaces(no);
	insertStr(co.outptab, spaces, p, length = co.outp - spaces);
	co.outp = p + length;
}

/*
 *		Return current position relative to .IN
 */
curpos(void)
{
	return co.outw + dc.tival - dc.inval;
}


/*
 *		return the delimiter of a tabstop definition
 */
getTDelim(p, typ)
buffer *p;
buffer typ;
{	char *q;

	if((q = strchr(p, '/')) && p < skipwd(p))
		return (p == q + 1)? ((typ == '>')? '\0': ' '): q[1];		/* "/" -> none */
	return (typ == '>')? ' ': '\0';			/* none -> default space */
}


/*
 *		compare a memory area case-insensitive
 */
memicmp(s1, s2, length)
char *s1, *s2;
int length;
{	int d;

	if(length) {
		while((d = *s1++ - *s2++) && --length);
		return d;
	}
	return 0;
}


/*
 *		handle pragma comman
 */
void pragma(line)
char *line;
{	if(memicmp(line, "letters=", 7) == 0) {
		/* specify the letters in the used character set */
		free(letter);
		letter = strdup(line + 7);
	}
}

/*
 *		check if a character is a letter
 */
isletter(ch)
buffer ch;
{	return isalpha(ch) || letter && strchr(letter, ch);
}


comand(p)
buffer *p;
{
	int ct, val;
	int spval;
	int index;
	char argtyp;
	buffer name[MAXLINE];
	buffer *macexp;

	ct = comtyp(p, &macexp);
	if (ct == UNKNOWN) {
			printf("*** nro: unrecognized command %s\n",p);
			return;
	}
	if(ct == COMMENT) return;

	expesc(p, name);
	val = getval(name, &argtyp);

	switch (ct) {
	case BO: /* bold face */
			dc.cbflg = FALSE;
bo_:		set(&dc.boval,val,argtyp,1,0,HUGE);
			break;
	case BP: /* begin page */
			if(pg.lineno > 0) space(HUGE);
			set(&pg.curpag,val,argtyp,pg.curpag+1,-HUGE,HUGE);
			pg.newpag = pg.curpag;
			break;
	case BR: /* break */
			brk();
			break;
	case BS: /* backspaces in output */
			set(&dc.bsflg,val,argtyp,1,0,1);
			break;
	case CB: /* continuous bold face */
			dc.cbflg = TRUE;
			goto bo_;
	case CC: /* command character */
			if (!argtyp) dc.cmdchr = '.';
			else dc.cmdchr = argtyp;
			break;
	case CE: /* center */
			brk();
			set(&dc.ceval,val,argtyp,1,0,HUGE);
			break;
	case CT:	/* concat the following word with the preceeding one */
			co.outdelim = 0;
			if(*(p = nxtwrd(name)))
				text(p);
			break;
	case CU: /* continuous underline */
			dc.cuflg = TRUE;
			goto ul_;
	case CV:	/* continuous overstrike */
			dc.cvflg = TRUE;
			goto ov_;
	case DE: /* define macro */
			defmac(name,sofile[dc.flevel]);
			break;
	case EF: /* even footer */
			gettl(name,pg.efoot,&pg.eflim[0]);
			break;
	case EH: /* even header */
			gettl(name,pg.ehead,&pg.ehlim[0]);
			break;
	case EN: /* end macro definition */
			puts("***nro: missing .de command\n");
			break;
	case FI: /* fill */
			brk();
			dc.fill = YES;
			break;
	case FO: /* footer */
			gettl(name,pg.efoot,&pg.eflim[0]);
			gettl(name,pg.ofoot,&pg.oflim[0]);
			break;
	case HE: /* header */
			gettl(name,pg.ehead,&pg.ehlim[0]);
			gettl(name,pg.ohead,&pg.ohlim[0]);
			break;
	case IN: /* indenting */
			set(&dc.inval,val,argtyp,0,0,dc.rmval-1);
			dc.tival = dc.inval;
			break;
	case JU: /* justify */
			dc.juval = YES;
			break;
	case LS: /* line spacing */
			set(&dc.lsval,val,argtyp,1,1,HUGE);
			break;
	case M1: /* set topmost margin */
			set(&pg.m1val,val,argtyp,2,0,HUGE);
			break;
	case M2: /* set second top margin */
			set(&pg.m2val,val,argtyp,2,0,HUGE);
			break;
	case M3: /* set first bottom margin */
			set(&pg.m3val,val,argtyp,2,0,HUGE);
			pg.bottom = pg.plval - pg.m4val - pg.m3val;
			break;
	case M4: /* set bottom-most margin */
			set(&pg.m4val,val,argtyp,2,0,HUGE);
			pg.bottom = pg.plval - pg.m4val - pg.m3val;
			break;
	case MACRO: /* macro expansion */
			maceval(name, macexp);
			break;
	case NE: /* need n lines */
			brk();
			if ((pg.bottom-pg.lineno+1) < (val*dc.lsval)) {
				space(HUGE);
			}
			break;
	case NF: /* no fill */
			brk();
			dc.fill = NO;
			break;
	case NJ: /* no justify */
			dc.juval = NO;
			break;
	case NR: /* set number register */
			p = nxtwrd(name);
			if (!isalpha(*p))
				puts("***nro: invalid or missing number register name\n");
			else {
				index = tolower(*p) - 'a';
				val = getval(skipwd(p),&argtyp);
				set(&dc.nr[index],val,argtyp,0,-HUGE,HUGE);
			}
			break;
	case OF: /* odd footer */
			gettl(name,pg.ofoot,&pg.oflim[0]);
			break;
	case OH: /* odd header */
			gettl(name,pg.ohead,&pg.ohlim[0]);
			break;
	case OV:	/* overstrike */
			dc.cvflg = FALSE;
ov_:		set(&dc.ovval,val,argtyp,1,0,HUGE);
			if(dc.ovval) {
				p = nxtwrd(nxtwrd(name));
				if(dc.ovchar = *p) 
					if(dc.ovchar != TOKEN) break;
					else printf("****nro: %u cannot be used as overstrike character\n", TOKEN);
				dc.ovchar = 'X';
			}
			break;
	case PC: /* page number character */
			if (!argtyp) dc.pgchr = EOS;
			else dc.pgchr = argtyp;
			break;
	case PL: /* page length */
			set(&pg.plval,val,argtyp,PAGELEN,
					pg.m1val+pg.m2val+pg.m3val+pg.m4val+1,HUGE);
			pg.bottom = pg.plval - pg.m3val - pg.m4val;
			break;
	case PO: /* page offset */
			set(&pg.offset,val,argtyp,0,0,HUGE);
			break;
	case RM: /* right margin */
			set(&dc.rmval,val,argtyp,PAGEWIDTH,dc.tival+1,HUGE);
			break;
	case SO: /* source file */
			*skipwd(p = nxtwrd(name)) = EOS;
			if (!*p) break;
			if (dc.flevel+1 >= NFILES)
				Error(".so commands nested too deeply");
			if ((sofile[dc.flevel+1] = fopen(p,"r")) == NULL)
				Error("unable to open %s",p);
			++dc.flevel;
			break;
	case SP: /* space */
			set(&spval,val,argtyp,1,0,HUGE);
			space(spval);
			break;
	case TB:	/* tab to the next tabstop or to a fixed column */
			if(!argtyp) {		/* next tabstop */
				/* current position relative to .IN */
				if(tabstop[index = getTS((val = curpos()) + 1)].tbcolumn > val) {
					val = tabstop[index].tbcolumn;	/* fixed column to tab to */
					ct = tabstop[index].tbdelim;	/* tab column delimiter */
					argtyp = tabstop[index].tbtyp;	/* how to tab */
				}
				else 			/* no tab found -> ignore */
					val = -PAGEWIDTH;
			}
			else if((macexp = strchr(name, '/')) && macexp < skipwd(nxtwrd(name)))
				ct = macexp[1];

			if((val -= curpos()) >= 0) {		/* there is to tab */
				/* val == number of spaces to insert */
				switch(argtyp) {
				case '>':	/* right justify */
					insertSpaces(val);
					break;
				case '=':	/* fill; left & right justify */
					if(val > 2
					 && (index = spread(co.outptab, co.outp, val - 2))) {
						co.outp += index;
						co.outw += index;
						val = 2;
					}
					/*FALL THROUGH*/
				case '|':	/* center */
					insertSpaces(index = val >> 1);
					fixedSpaces(val - index);
					break;
				default:	/* left justify */
					fixedSpaces(val);
					break;
				}
				if(isspace(ct))
					co.outdelim = 1;
				else {
					co.outdelim = 0;
					if(ct) {
						*co.outp++ = ct;
						++co.outw;
					}
				}
			}
			else if(!co.outdelim)	/* no tab right of this pos -> space */
				co.outdelim = 1;
			co.outptab = co.outp;
			break;
	case TI: /* temporary indent */
			brk();
			set(&dc.tival,val,argtyp,0,0,dc.rmval);
			break;
	case TS:	/* set tab stops */
			p = nxtwrd(name);
			if(argtyp == '*') {		/* relative tab stops */
				clearTS();
				index = 0;
				if(ct = getTDelim(p, argtyp = *nxtwrd(p))) {
					setTS(1, val++, argtyp, ct);
					index = 1;
				}
				while(++index < MAXTABSTOPS
				 && tabstop[index - 1].tbcolumn < PAGEWIDTH)
					setTS(index
					 , tabstop[index - 1].tbcolumn + val
					 , argtyp, ct);
				break;
			}
			if(argtyp != '+')
				clearTS();
			if(argtyp)
				do switch(argtyp) {
				case '-':			/* remove tabstops */
					if(tabstop[index = getTS(val)].tbcolumn == val) {
						memmove(tabstop[index], tabstop[index + 1]
						 , (MAXTABSTOPS - 1 - index) * sizeof(tabstop[0]));
						setTS(MAXTABSTOPS - 1, NOTAB);
					}
				case '+':		/* add tabstops */
					break;
				default:		/* set new tabstops */
					if((ct = tabstop[index = getTS(val)].tbcolumn) != NOTAB) {
						if(ct < val) 					/* no tabstop free */
							continue;
						if(ct != val)		/* make room for this tabstop */
							memmove(tabstop[index + 1], tabstop[index]
							 , (MAXTABSTOPS - 1 - index) * sizeof(tabstop[0]));
					}
					setTS(index, val, argtyp, getTDelim(p, argtyp));
				} while(*(p = nxtwrd(p)));
			break;
	case UL: /* underline */
			dc.cuflg = FALSE;
ul_:		set(&dc.ulval,val,argtyp,0,1,HUGE);
			/*FALL THROUGH*/
	case ZZ:	/* pragma definition;; -> ignore in this version of NRO */
			pragma(nxtwrd(name));
			break;
	}
}



/*
 *      end current filled line
 */

brk()
{
	if(co.outp != co.outbuf) {		/* there is text in the buffer */
		*co.outp = EOS;				/* line terminator */
		put(co.outptab = co.outp = co.outbuf);
		co.outprev = co.outdelim = co.outw = 0;
	}
}



/*
 *      decodes nro command and returns its associated
 *      value.
 */

comtyp(p,m)
buffer *p;
buffer **m;
{
        buffer cmdnam[MNAMELENGTH+1];
        buffer *s, **cmd;

        /*	First, check for the comment pseudo-command	*/

        if(*++p == dc.cmdchr) return COMMENT;

        /*
        *       Second check to see if the command is a macro.
        *       If it is, truncate to two characters and return
        *       expansion in m.  Note that upper and lower case
        *       characters are handled differently for macro names,
        *       but not for normal command names.
        */
		getmacname(p, cmdnam);
        if ((s = getmac(cmdnam)) != NULL) {
			if(m) *m = s;
			return(MACRO);
        }

        /*	Third tokenize the name of the command
        */
		strupr(cmdnam);
		for(cmd = cmds; *cmd; ++cmd) {
			if(strcmp(*cmd, cmdnam) == 0)
				return (cmd - cmds) / sizeof(char*);
		}

        return(UNKNOWN);
}



/*
 *      convert string to decimal.
 *      processes only positive values.
 */

ctod(p)
char *p;
{
	/*ska	Both isdigit() and atoi() are K&R and ANSI functions; there
		should be no need to avoid in my mind */

	return isdigit(*p)? atoi(p): 0;
}


/*
 *      Define a macro
 */

defmac(p,infp)
buffer *p;
FILE *infp;
{
	buffer name[MNAMELENGTH+1];
	buffer *q;
	struct MACRO_DEF *macro;

	getmacname(nxtwrd(p), name);

	if (!isletter(*name))
		Error("missing or illegal macro definition name");

	macro = mac.mac_emb;		/* 1st free byte */
	q = macro->mac_body;		/* 1st byte of macro body */
	while(q < mac.mac_ppb && getlin(p, infp) != EOF) {
		if (p[0] == dc.cmdchr)
			switch(comtyp(p, NULL)) {
				case EN: goto breakWhile;
				case COMMENT: continue;	/* ignore comments */
			}
		q = stpcpy(q, p);	/* This will cause the macro buffer to overflow
								on failure.
								Because we terminate in that case; this
								should be acceptable */
		*q++ = '\n';
	}
breakWhile:
	if(q >= mac.mac_ppb) 
		Error("macro definition table full");
	*q = '\0';
	memcpy(macro->mac_name, name, MNAMELENGTH);
	macro->mac_length = (mac.mac_emb = q + 1) - (char*)macro;
}


/*
 *      Expand escape sequences
 */

expesc(p,q)
buffer *p;
buffer *q;
{
#define s p
#define t q
	while (*t++ = *s++) {
		if (s[-1] == '@')
			if(*s == '@') ++s;
			else {
				--t;
				if(toupper(*s) == 'N' && isalpha(s[1])) {
					t += itoda(dc.nr[toupper(s[1])-'A'],t,6) - 1;
					s += 2;
				}
			}
	}
#undef s
#undef t
}



/*
 *      Get macro definition from table
 */

buffer *getmac(name)
buffer *name;
{
	struct MACRO_DEF *macro;

	for(macro = mac.mac_buf; macro < mac.mac_emb
	 ; macro = (buffer *)macro + macro->mac_length) {
		if(memcmp(macro->mac_name, name, MNAMELENGTH) == 0) {	/* macro found */
			return macro->mac_body;
		}
	}
	return NULL;		/* macro not found */
}




/*
 *      get header or footer title
 */

gettl(p,q,limit)
buffer *p;
buffer *q;
int limit[];
{
	strcpy(q, nxtwrd(p));
	limit[LEFT] = dc.inval;
	limit[RIGHT] = dc.rmval;
}



/*
 *      retrieves optional argument following nro command.
 *      returns positive integer value with sign (if any)
 *      saved in character addressed by p_argt.
 */

getval(p,p_argt)
buffer *p;
buffer *p_argt;
{
	p = nxtwrd(p);
	*p_argt = *p;
	if(!isdigit(*p)) ++p;
	return(ctod(p));
}


/*
 *      Evaluate macro expansion
 */

maceval(p,m)
buffer *p, m[];		/* m[] must not be modified! */
{
	int ch;
	buffer *argp[10];

	*p++ = EOS;             /* replace command char with EOS */
	/*
	*       initialize argp array to substitute command
	*       string for any undefined argument
	*/
	brkarg(skipwd(p), argp, 10);

	/* Because the push back buffer is a LIFO, the macro expansion must
		must be inserted in reverse order */
	p = strchr(m, '\0');
	while(--p > m) {
		ch = *p;
		if (p[-1] == '$') {
			if (!isdigit(ch)) putbak(ch);
			else pbstr(argp[ch-'0']);
			--p;			/* skip the '$' */
		}
		else
			putbak(ch);
	}
	if((ch = *m) && ch != '$') putbak(ch);
}


/*
 *      Push back string into input stream
 */

pbstr(p)
buffer p[];
{
	buffer *q;

	if(!p) return;

	/* Because the push back buffer is a LIFO, the string must
		must be inserted in reverse order */
	q = strchr(p, '\0');
	while (--q >= p)
		putbak(*q);
}




/*
 *      Push character back into input stream
 */

putbak(c)
buffer c;
{
	if(c)
		if(mac.mac_ppb > mac.mac_emb)		/* a byte is free */
			*--mac.mac_ppb = c;
		else			/* nothing free */
			Error("push back buffer overflow");
}




/*
 *      set parameter and check range
 */

set(param,val,type,defval,minval,maxval)
int *param;
int val;
buffer type;
int defval,minval,maxval;
{
	switch(type) {
	case EOS:
			*param = defval;
			break;
	case '+':
			*param += val;
			break;
	case '-':
			*param -= val;
			break;
	default:
			*param = val;
			break;
	}
	*param = min(*param,maxval);
	*param = max(*param,minval);
}



/*
 *      skip blanks and tabs in character buffer.
 *      return number of characters skipped.
 */

buffer *skipbl(p)
buffer *p;
{
	--p;
	while (isspace(*++p));
	return(p);
}


/*
 *      skip over word and punctuation
 */

buffer *skipwd(p)
buffer *p;
{	int c;

	--p;
	while ((c = *++p) && !isspace(c))
		if(c == TOKEN) p = strchr(p + 1, TOKEN);

	return(p);
}


/*
 *		skip over word and punctation
 *		then skip any blanks
 */
buffer *nxtwrd(p)
buffer *p;
{	return skipbl(skipwd(p));
}


/*
 *	search backwards for the first character non-equal character
 */
buffer *lastnot(p, buf, c)
buffer *p, *buf;
int c;
{	while(--p >= buf && *p == c);
	return p;
}


/*
 *	Retreive a macro/command name from the line
 */
getmacname(line, name)
buffer *line, *name;
{	memset(name, 0, MNAMELENGTH + 1); /* make sure all the tailing bytes
									are equal to allow a bytewise compare */

	memcpy(name, line					/* copy the name */
	 , min(skipwd(line) - line, MNAMELENGTH));	/* length of macro name */
}


/*
 *	break a line into an argument array
 */
brkarg(line, argp, maxarg)
buffer *line, **argp;
int maxarg;
{	int c;
	buffer *p;

	memset(argp, 0, maxarg * sizeof(buffer *));	/* empty out unused slots */
	++maxarg;
	p = line;
	while(--maxarg && (c = *(p = skipbl(p))) != EOS) {
		if (c == '\'' || c == '"') {
			*argp++ = ++p;
			while (*p != c && *p != EOS) ++p;
		}
		else {
			*argp++ = p;
			p = skipwd(p);
		}
		if(*p) *p++ = EOS;
	}
}


/*
 *      space vertically n lines
 */

space(n)
int n;
{
	brk();
	if (pg.lineno > pg.bottom) return;
	if (pg.lineno == 0) phead();
	skip(min(n,pg.bottom+1-pg.lineno));
	pg.lineno += n;
	if (pg.lineno > pg.bottom) pfoot();
}

text(p)
buffer *p;
{
	int i;
	buffer wrdbuf[MAXLINE], *q;

	if (*p == ' ' || *p == EOS) leadbl(p);

/* simulate font shapes by \fB<line>\fP etc. */
	q = wrdbuf;
	if (dc.buflg) {		/* allow font shapes */
		if(dc.ulval) {
			--dc.ulval;
			q = puttok(q
			 , dc.cuflg? TOK_UNDERLINE | TOK_CONTINUOUS: TOK_UNDERLINE);
		}
		if(dc.boval) {
			--dc.boval;
			q = puttok(q
			 , dc.cbflg? TOK_BOLDFACE | TOK_CONTINUOUS: TOK_BOLDFACE);
		}
		if(dc.ovval) {
			--dc.ovval;
			q = puttok2(q
			 , dc.cvflg? TOK_OVERSTRIKE | TOK_CONTINUOUS: TOK_OVERSTRIKE
			 , dc.ovchar);
		}
	}
	expesc(p, q);
	if(q != wrdbuf) {		/* font shapes prepended */
		puttok(strchr(q = wrdbuf, '\0'), TOK_NORMAL);
	}

	if (dc.ceval) {
		center(wrdbuf);
		put(wrdbuf);
		--dc.ceval;
	}
	else if (*p == EOS						 /* all blank line */
		|| dc.fill == NO) put(wrdbuf);         /* unfilled */
	else {
		while (i = getwrd(q, p)) {
			putwrd(p);
			q += i;
		}
	}
}


/*
 *      center a line by setting tival
 */

center(p)
buffer *p;
{
	dc.tival = max((dc.rmval + dc.tival - width(p)) >> 1, 0);
}


/*
 *      expand title buffer to include character string
 */

expand(p0,c,s)
buffer *p0;
buffer c;
buffer *s;
{
	buffer tmp[MAXLINE];
	buffer *p, *q;

	p = p0 - 1;
	q = tmp - 1;
	while (*++q = *++p) {
		if (*p == c)
			q = stpcpy(q, s) - 1;
	}
	strcpy(p0,tmp);         /* copy it back */
}


/*
 *      get field from title
 */

buffer *getfield(p,q,delim)
buffer *p, *q;
buffer delim;
{
	while (*p != delim && *p != EOS)
		*q++ = *p++;
	*q = EOS;
	if (*p) ++p;
	return(p);
}



/*
 *      get non-blank word from p0 into p1.
 *      return number of characters processed.
 */

getwrd(p0,p1)
buffer *p0,*p1;
{
	int length;			/* length of word */
	buffer *p;			/* start of word */
	buffer *h;			/* end of word */

	length = (h = skipwd(p = skipbl(p0))) - p;
	memcpy(p1, p, length);
	p1[length] = EOS;

	return h - p0;
}


/*
 *      convert integer to decimal ascii string
 */

itoda(value,p,size)
int value;
buffer *p;
int size;
{
	char c[7];
	int i, j;

	if((i = sprintf(c, "%d", value) + 1) > size)
		j = i - size;
	else
		j = 0;

	strcpy(p, c + j);
	return i - j;
}


/*
 *      center title text into print buffer
 */

justcntr(p,q,limit)
buffer *p, *q;
int limit[];
{
	int len;

	len = width(p);
	q = &q[(limit[RIGHT] + limit[LEFT] - len) >> 1];
	while (*p != EOS) *q++ = *p++;
}



/*
 *      left justify title text into print buffer
 */

justleft(p,q,limit)
buffer *p, *q;
int limit;
{
	q = &q[limit];
	while (*p != EOS) *q++ = *p++;
}


/*
 *      right justify title text into print buffer
 */

justrite(p,q,limit)
buffer *p, *q;
int limit;
{
	int len;

	len = width(p);
	q = &q[limit - len];
	while (*p != EOS) *q++ = *p++;
}




/*
 *      delete leading blanks, set tival
 */

leadbl(p)
buffer *p;
{
	buffer *q;

	brk();

	q = p - 1;
	while(*++q == ' ');

	if (*q) dc.tival = q - p;
	memmove(p, q, strlen(q) + 1);
}




/*
 *      put out page footer
 */

pfoot()
{
	if (dc.prflg) {
		skip(pg.m3val);
		if (pg.m4val > 0) {
			if (pg.curpag & 1)
				puttl(pg.ofoot,pg.oflim,pg.curpag);
			else
				puttl(pg.efoot,pg.eflim,pg.curpag);
			skip(pg.m4val - 1);
		}
	}
}



/*
 *      put out page header
 */

phead()
{
	pg.curpag = pg.newpag;
	dc.prflg = pg.curpag >= pg.frstpg && pg.curpag <= pg.lastpg;

	++pg.newpag;
	if (dc.prflg) {
		if (pg.m1val > 0) {
			skip(pg.m1val - 1);
			if (pg.curpag & 1)
				puttl(pg.ohead,pg.ohlim,pg.curpag);
			else
				puttl(pg.ehead,pg.ehlim,pg.curpag);
		}
		skip(pg.m2val);
	}
	/*
	*       initialize lineno for the next page
	*/
	pg.lineno = pg.m1val + pg.m2val + 1;
}


/*
 *      print character with test for printer
 */

prchar(c,fp)
buffer c;
FILE *fp;
{
	putc(c,fp);
}


/*
 *	print overstrike character
 */
prbchar(c, fp)
buffer c;
FILE *fp;
{	prchar(c, fp);
	prchar('\b', fp);
}


/*
 *      print character n times with test for printer
 */

prnchar(c,n,fp)
buffer c;
int n;
FILE *fp;
{
	if(n)
		do prchar(c,fp);
		while(--n);
}




/*
 *      put out line with proper spacing and indenting
 */

put(p)
buffer *p;
{
	if (pg.lineno == 0 || pg.lineno > pg.bottom)
		phead();

	if (dc.prflg)
		putlin(p, pout, pg.offset + dc.tival, cs);

	dc.tival = dc.inval;
	skip(min(dc.lsval-1,pg.bottom-pg.lineno));
	pg.lineno = pg.lineno + dc.lsval;
	if (pg.lineno > pg.bottom) pfoot();
}


/*
 *	prints a character with checking for preceeding spaces
 */
prschar(ch, sp, fp)
int ch, *sp;
FILE *fp;
{	if(*sp) {
		prnchar(' ', *sp, fp);
		*sp = 0;
	}
	prchar(ch, fp);
	return 1;
}


/*
 *	print a character with checking if it can change its font
 */
prsschar(ch, c, sp, noncont, cont, fp)
int ch, c, *sp, noncont, cont;
FILE *fp;
{	if(!iscntrl(ch) && !(cont || noncont && (isletter(ch) || isdigit(ch)))) {
		++*sp;
		return 0;
	}
	prschar(c, sp, fp);
	return 1;
}


/*
 *      output a null terminated string to the file
 *      specified by pbuf.
 */

prlin(p,pbuf)
buffer *p;
FILE *pbuf;
{
	while (*p != EOS) prchar(*p++,pbuf);
}



/*
 *      output a null terminated string to the file
 *      specified by pbuf. And issue a line feed.
 		interprete the tokens (character shapes).
 */

putlin(p, fp, leadspaces, cs)
buffer *p;
FILE * fp;
int leadspaces;
struct charshape *cs;
{
	int ch, sp;
	int level, written;
	buffer *q;
#include "defcs.inc"

#include "getcs.inc"

	/* font shapes? */
				/*ska to use arethmetic OR saves some bytes in MC */
	level = (csbold | csunderl | csovrstr | strchr(p, TOKEN))? 3: 0;

nxtLevel:
	q = p - 1;
#define p q
	sp = leadspaces;
	written = 0;
	while(ch = *++p) {
		if(ch != TOKEN) {
			if(dc.bsflg) {
				if(sp) {
					prnchar(' ', sp, fp);
					sp = 0;
				}
				if(!iscntrl(ch))
					if(isletter(ch) || isdigit(ch)) {
						if(csbold) prbchar(ch, fp);
						if(csunderl) prbchar('_', fp);
						if(csovrstr) prbchar(csovrstr, fp);
					}
					else {
						if(cscbold && ch != ' ') prbchar(ch, fp);
						if(cscunderl) prbchar('_', fp);
						if(cscovrstr) prbchar(csovrstr, fp);
					}
				prchar(ch, fp);
			}
			else {			/* no backspaces in the output stream allowed */
				switch(level) {
				case 0:	/* no overstrike characters */
					written |= prschar(ch, &sp, fp);
					break;
				case 1:	/* bold face */
					written |= prsschar(ch, ch, &sp, csbold, cscbold, fp);
					break;
				case 2:	/* underlining */
					written |= prsschar('_', ch, &sp, csunderl, cscunderl, fp);
					break;
				case 3:	/* overstrike character face */
					written |= prsschar(csovrstr, ch, &sp, csovrstr, cscovrstr, fp);
					break;
				}
			}
		}
		else {
			switch(*++p) {
			case TOKEN: prchar(TOKEN, fp); break;
			case TOK_CONTINUOUS | TOK_BOLDFACE:	cscbold = YES;
			case TOK_BOLDFACE:	csbold = YES; break;
			case TOK_CONTINUOUS | TOK_UNDERLINE: cscunderl = YES;
			case TOK_UNDERLINE:	csunderl = YES; break;
			case TOK_CONTINUOUS | TOK_OVERSTRIKE:
				cscovrstr = YES;
			case TOK_OVERSTRIKE: csovrstr = *++p; break;
			case TOK_NORMAL:
				cscbold = cscunderl = cscovrstr = 
				csbold = csunderl = csovrstr = NO;
				break;
			case TOK_SPACES:
				prnchar(' ', *++p & 0x7F, pout);
				break;
			}
			++p;			/* skip over trailing TOKEN */
		}
	}

	if(!dc.bsflg && level--) {			/* output next level of font shapes */
		if(written) prchar('\r', fp);
		goto nxtLevel;
	}

	prchar('\n', fp);
#undef p
#include "putcs.inc"
}



/*
 *	put a token into the output buffer
 */
buffer *puttok(p, token)
buffer *p;
int token;
{	sprintf(p, "%c%c%c", TOKEN, token, TOKEN);
	return p + 3;
}


/*
 *	put a token into the output buffer
 */
buffer *puttok2(p, token, arg)
buffer *p;
int token, arg;
{	sprintf(p, "%c%c%c%c", TOKEN, token, arg, TOKEN);
	return p + 4;
}


/*
 *	skips back one character
 */
buffer *prechar(p)
buffer *p;
{	if(*--p != TOKEN)
		return p;
	while(*--p != TOKEN);
	return p - 1;
}



/*
 *      put out title or footer
 */

puttl(p,lim,pgno)
buffer *p;
int lim[];
int pgno;
{
	char pn[8];
	buffer t[MAXLINE];
	buffer h[MAXLINE];
	buffer delim;
	struct charshape cs;		/* the titles/footers have their own shapes */

	clear(cs);					/* no shapes initially set */
	memset(h, ' ', sizeof(h));

	itoda(pgno,pn,6);
	delim = *p++;
	p = getfield(p,t,delim);
	expand(t,dc.pgchr,pn);
	justleft(t,h,lim[LEFT]);
	p = getfield(p,t,delim);
	expand(t,dc.pgchr,pn);
	justcntr(t,h,lim);
	p = getfield(p,t,delim);
	expand(t,dc.pgchr,pn);
	justrite(t,h,lim[RIGHT]);

	lastnot(h + MAXLINE - 1, h, ' ')[1] = EOS;

	putlin(h,pout, pg.offset, cs);
}



/*
 *      put word in output buffer
 */

putwrd(wrdbuf)
buffer *wrdbuf;
{
	int w, unused, nextra, sw;
	buffer *p;

	w = width(wrdbuf);
	/* number of characters that still fits onto the current line */
	unused = dc.rmval - dc.tival - co.outw;

	if(co.outbuf != (p = co.outp)) {/* There is something in the buffer */
		sw = strlen(wrdbuf);
		nextra = co.outdelim;
		if(w + nextra > unused	/* overflow of output line */
		 || (co.outbuf + MAXLINE - 1		/* overflow of output buffer */
		  <= p + sw + nextra)) { /* line break necessary */
			/* first check, if the current word and the last word of the
				completed line shall be concated */
			if(!co.outdelim && co.outprev
			 && sw + (nextra = (p - co.outprev)) < MAXLINE - 1) {
			 	/* concate both parts of the word */
			 	insertStr(wrdbuf, p = co.outprev, wrdbuf + sw + 1, nextra);
				/* get word length of prepended part of the word */
				unused -= w;
				unused += w = width(wrdbuf);
				/* check how many delimiters were prepended */
				while(*--p == ' ')
					++unused;
				/* alter the output buffer values that the part of the word
					was never in the buffer */
				co.outp = ++p;
				/* needn't to update co.outw because it is not used in
					spread() or brk() */
			}
			if(dc.juval)		/* left&right justify? */
				co.outp += spread(co.outptab, p, unused);
			brk();				/* output the line */
			p = co.outbuf;
		}
		else {	/* no line break -> add the delimiter between the words */
			if(w && nextra) {			/* if only a token, no delimiter! */
				co.outw += nextra;
				do *p++ = ' ';
				while(--nextra);
				co.outprev = p;
			}
		}
	}
	p = co.outp = stpcpy(p, wrdbuf);

	if(w) {
		co.outw += w;
		/* check if this was an end of a sentence */
		if ((nextra = *(p = prechar(p))) == '"')
			nextra = *(p = prechar(p));

		co.outdelim = (p[-1] != dc.quote	/* quoted chars don't end sentence */
		 && strchr("?!.", nextra))? 2: 1;
	}
}


/*
 *      skips the number of lines specified by n.
 */

skip(n)
int n;
{
	if (dc.prflg && n > 0)
		prnchar('\n', n, pout);
}



/*
 *      spread words to justify right margin
 *		must not use co.outw (ska)
 */

spread(buf,outp,nextra)
buffer buf[], *outp;
int nextra;
{
	int nb,ne,nholes;
	buffer *p;

	/* calculate the number of holes */
	nholes = 0;
	p = buf - 1;
	while(++p < outp)
		if(*p == ' ') {
			++nholes;
			while(*++p == ' ');
		}

	nextra = ne = min(nextra, MAXLINE - 2 - (outp - buf));

	if(ne <= 0 || !nholes) return 0;

	dc.sprdir = ~dc.sprdir;

	p = outp + ne;		/* new end of string */
	while(p > outp) {
		if((*--p = *--outp) == ' ') {
			/* found a word delimiter -> fill */
			if(--nholes)
				ne -= nb = (dc.sprdir)? ne/nholes: (ne - 1)/nholes + 1;
			else
				nb = p - outp;
			while(outp > buf && outp[-1] == ' ')
				++nb, --outp;
			while(nb--)
				*--p = ' ';
		}
	}
	return nextra;
}



/*
 *      compute width of character string
 */

width(s)
buffer *s;
{
	int w, ch;

	w = 0;
	--s;
	while ((ch = *++s) != EOS) {
		if(ch == TOKEN) {
			if(s[1] == TOK_SPACES)
				w += s[2] & 0x7F;
			s = strchr(s + 1, TOKEN);
		}
		else ++w;
	}
	return(w);
}
