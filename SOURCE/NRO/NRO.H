/*
*/
/* $RCSfile: NRO.H $
   $Locker:  $	$Name:  $	$State: Exp $

 * NRO.H: Parameter file for NRO word processor
 *
 * Stephen L. Browning
 * 5723 North Parker Avenue
 * Indianapolis, Indiana 46220
 *
 * Ported to MS C 5.1 by John Dudeck 11/25/90.
 *
 * Modifications made by Steffen.Kaiser@fh-rhein-sieg.de
 */

/* some or all of this may be unnecessary for you */

#define EOS		'\0'
#define CPMEOF	0x1a
#define TRUE	-1
#define FALSE	0
#define OK		0

/* end of possible bogus hacks */

/*
 *      The following parameters may be defined in bdscio.h
 */

#define YES     1
#define NO      0
#define ERR     -1
#define FLAG	unsigned char	/* Micro-C does not always promote to (int) */

#define buffer unsigned char	/* make character buffers sign-less to better
		handle 8bit cleaness */

/* Macros for the commands						##!mkcmds
BO  bold face 
BP  begin page 
BR  break 
BS  allow/disallow '\b' in output 
CB  continuous boldface 
CC  control character 
CE  center 
CT  concat the following word with preceeding one
CU  continuous underline 
CV  continuous overstrike character (as UL, but with customized character)
DE  define macro 
EF  footer for even numbered pages 
EH  header for even numbered pages 
EN  end macro definition 
FI  fill 
FO  footer 
HE  header 
IN  indent 
JU  justify 
LS  line spacing 
M1  top margin 
M2  second top margin 
M3  first bottom margin 
M4  bottom-most margin 
NE  need n lines 
NF  no fill 
NJ  no justify 
NR  set number register 
OF  footer for odd numbered pages 
OH  header for odd numbered pages 
OV  overstrike character (as UL, but with customized character)
PC  page number character 
PL  page lenght 
PO  page offset 
RM  right margin 
SO  source file 
SP  line space 
TB  tab
TI  temp indent 
TS  tab stops
UL  underline 
ZZ  pragma definitions to NRO
*/

#define MACRO   -3      /* macro definition */
#define COMMENT	-2		/* comment line */
#define UNKNOWN -1

/*
 *      MAXLINE is set to a value slightly larger
 *      than twice the longest expected input line.
 *      Because of the way underlining is handled, the
 *      input line which is to be underlined, can almost
 *      triple in length.  Unlike normal underlining and
 *      boldfacing, continuous underlining affects all
 *      characters in the buffer, and represents the
 *      worst case condition.  If the distance between
 *      the left margin and the right margin is greater
 *      than about 65 characters, and continuous underlining
 *      is in effect, there is a high probability of buffer
 *      overflow.
 */

#define MAXLINE 512            /* was 200 */
#define PAGELEN  66
#define PAGEWIDTH 80
#define MAXTABSTOPS 32
#define HUGE    256
#define LEFT    0               /* indecies into header margin limit arrays */
#define RIGHT   1
#define NFILES  4               /* nesting depth for input files */
#define NOTAB -1

/*	The macro handling has been modified due to allow a better useage of
	the available memory. It is done like the combined data and stack
	segments:
	<lowest address>			<--- mac_buf
		macro definitions
	<lowest unused byte>		<--- mac_emb
	<highest unsed byte>		<--- mac_ppb
		pushed back characters
	<highest address>			<--- mac_buf + sizeof(mac_buf)

	due to make things more easier the way the macro definitions are stored
	is modified, too: (see MACRO_DEF)
	This forms a single relatively chained list.

	This makes the lookup of macros somewhat slower, but it should be
	acceptable.

	Arethmetics:
		If mac_emb == mac_ppb, macro buffer used up, no further macro
		definition nor any character can be pushed back.
		If mac_ppb == mac_buf + sizeof(mac_buf), no pushed back character.
*/

#define MACBUF (5*1024)		/* this is approx. equally sized than orig. */
#define MNAMELENGTH 2		/* length of stored macro names */

struct MACRO_DEF {
	unsigned mac_length;	/* complete length of this macro definition */
	buffer mac_name[MNAMELENGTH];		/* macro name, two characters long */
	buffer mac_body[1];		/* first byte of macro body */
};

struct macros {
	buffer mac_buf[MACBUF];	/* buffer for macros & pushed back characters */
	buffer *mac_emb;			/* first unused byte within buffer */
	buffer *mac_ppb;			/* first byte of pushed back characters */
};


/* control parameters for nro */

struct docctl {
        int fill;       /* fill if YES, init = YES              */
        int lsval;      /* current line spacing, init = 1       */
        int inval;      /* current indent, >= 0, init = 0       */
        int rmval;      /* current right margin, init = 60      */
        int tival;      /* current temp indent, init = 0        */
        int ceval;      /* number of lines to center, init = 0  */
        int ulval;      /* number of lines to underline, init = 0 */
        FLAG cuflg;      /* continuously underline, init = FALSE */
        int juval;      /* justify if YES, init = YES           */
        int boval;      /* number of lines to bold face, init = 0 */
        FLAG cbflg;		/* continuously bold face, init = FALSE */
        FLAG bsflg;      /* can output contain '\b', init = FALSE */
        FLAG buflg;      /* enable bold and underline, init = FALSE */
        buffer pgchr;     /* page number character, init = '#'    */
        buffer cmdchr;    /* command character, init = '.'        */
        buffer quote;		/* quotation character, init = EOS == none */
        FLAG prflg;      /* print on or off, init = TRUE         */
        FLAG cvflg;		/* continuous overstrike, init = FALSE */
        int ovval;		/* number of lines to overstrike, init = 0 */
        buffer ovchar;	/* character to overstrike with, init = none */
        int sprdir;     /* direction for spread(), init = 0     */
        int flevel;     /* nesting depth for source cmd, init = 0 */
        int nr[26];     /* number registers     */
};


/* output buffer control parameters */

struct cout {
	buffer outbuf[MAXLINE];		/* output of filled text */
	buffer *outp;				/* pointer to next free byte in the buffer */
	buffer *outprev;			/* previous word */
	buffer *outptab;			/* previous tabulator */
	int outw;					/* width of text "on the screen" */
	int outdelim;				/* number of delimiters to be prepended */
};

/* character shape control parameters */
/* tokens are inserted into the output buffer and then will be interpreted by
	the output function.
	Tokens conforms to:
		TOKEN
		<token>
		<its arguments>
		TOKEN
	Neither <toke> nor <its arguments> may contain TOKEN or EOS.

*/

#define TOKEN	'\x1b'
#define TOK_CONTINUOUS 0x01		/* continuously change character shape */
#define TOK_BOLDFACE 0xFE
#define TOK_UNDERLINE 0xFC
#define TOK_OVERSTRIKE 0xF8		/* argument: character to overstrike with */
#define TOK_SPACES 0xF7			/* argument: spaces to be inserted */

#define TOK_NORMAL 0x1e			/* disable all font shapes */

struct charshape {
/*##!mkcsinc */
	FLAG csbold;			/* if to use bold face */
	FLAG csunderl;			/* if to use underlining */
	buffer csovrstr;		/* if != 0: the overstrike character */
	FLAG cscbold;			/* if to use continuous bold face */
	FLAG cscunderl;			/* if to use continuous underlining */
	FLAG cscovrstr;			/* if to use continuous overstriking */
};


/* page control parameters for nro */

struct page {
        int curpag;     /* current output page number, init =0  */
        int newpag;     /* next output page number, init = 1    */
        int lineno;     /* next line to be printed, init = 0    */
        int plval;      /* page length in lines, init = 66      */
        int m1val;      /* margin before and including header   */
        int m2val;      /* margin after header                  */
        int m3val;      /* margin after last text line          */
        int m4val;      /* bottom margin, including footer      */
        int bottom;     /* last live line on page
                                        = plval - m3val - m4val */
        int offset;     /* page offset from left, init = 0      */
        int frstpg;     /* first page to print, init = 0        */
        int lastpg;     /* last page to print, init = 30000     */
        int ehlim[2];   /* left/right margins for headers/footers       */
        int ohlim[2];   /* init = 0 and PAGEWIDTH                       */
        int eflim[2];
        int oflim[2];
        buffer ehead[MAXLINE];    /* top of page title, init = '\n'       */
        buffer ohead[MAXLINE];
        buffer efoot[MAXLINE];    /* bottom of page title, init = '\n'    */
        buffer ofoot[MAXLINE];
};

/* tabstop control parameters */

struct tabulator {
	int tbcolumn;		/* place of tab relative to current .IN value */
	buffer tbtyp;		/* how to justify text */
						/* '>': right justify
						   '|': center
						   '=': fill
						   else: left justify (default)
						*/
	buffer tbdelim;		/* how to delimit this colum from next one */
						/* '\0': no delimiter (default unless '>')
						   ' ' or '\t': one space (default if '>')
						   else: the character
						*/
};


#ifndef EXTERN
#define EXTERN extern
#endif

EXTERN struct docctl dc;
EXTERN struct charshape cs;		/* character shape of "normal text" */
EXTERN struct page pg;
EXTERN FILE   *oub; /* output file handle? */
EXTERN FILE   *pout; /* output printer handle? */
EXTERN struct cout co;
EXTERN FILE   *sofile[NFILES];  /* input file buffers   */
EXTERN struct macros mac;
#define MACEND (mac.mac_buf + sizeof(mac.mac_buf))
EXTERN struct tabulator tabstop[MAXTABSTOPS];


/* some prototypes */
buffer *puttok();
buffer *puttok2();
buffer *skipbl();
buffer *skipwd();
buffer *nxtwrd();
void clearTS();
buffer *getmac();
