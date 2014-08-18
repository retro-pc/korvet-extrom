/* A Bison parser, made by GNU Bison 3.0.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2013 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 1 "zmac.y" /* yacc.c:339  */

// GWP - keep track of version via hand-maintained date stamp.
#define VERSION "19sep2013"

/*
 *  zmac -- macro cross-assembler for the Zilog Z80 microprocessor
 *
 *  Bruce Norskog	4/78
 *
 *  Last modification  1-18-87 by cdk
 *  This assembler is modeled after the Intel 8080 macro cross-assembler
 *  for the Intel 8080 by Ken Borgendale.  The major features are:
 *	1.  Full macro capabilities
 *	2.  Conditional assembly
 *	3.  A very flexible set of listing options and pseudo-ops
 *	4.  Symbol table output
 *	5.  Error report
 *	6.  Elimination of sequential searching
 *	7.  Commenting of source
 *	8.  Facilities for system definiton files
 *
 * Revision History:
 *
 * jrp	3-8-82	Converted to run on Vax, updated syntax to conform better
 *		to the Zilog standard.
 *
 * jrp	3-15-82	Added underscore as a character type in the lex table
 *		'numpart' (0x5F).
 *
 *		Changed maximum number of characters in a label to 15
 *		from 7. Note that 'putsymtab' uses this value inside
 *		of a quoted string, so we use 15.
 *
 * jrp	2-15-83	Fixed 'getlocal' to return better local labels. It used
 *		to crash after 6 invocations.
 *
 * jrp	6-7-83	Fixed bug in the ADD IX,... instruction.
 *
 * jrp	5-11-84	Added code to print unused labels out with the symbol table
 *		Also sped up the macro processor by using stdio.
 *
 * jrp 5-22-84	Added include files ala ormac
 *
 * jrp 8-27-84	Added PHASE/DEPHASE commands
 *
 * cdk 9-20-86	Converted to run on a Pyramid.  This meant changing yylval
 *		to be a %union, and then putting in the appropriate
 *		typecasts where ints are pointers are used interchangeably.
 *		The current version still probably won't run on machines where
 *		sizeof(int) != sizeof(char *).
 *		Also changed emit() to use varargs, and got rid of the
 *		old style = in front of yacc action code.
 *			-Colin Kelley  vu-vlsi!colin
 *
 * cdk 10-2-86	Added some more typecasts to keep lint a little happier.
 *		Removed several unused variables.  Changed most vars
 *		declared as char to int, since many of them were being
 *		compared with -1!  I still don't know what's going on with
 *		est[][] being malloc'd and free'd everywhere...it looks pretty
 *		fishy...
 *
 * cdk 1-18-87  Added MIO code to emulate 'mfile' using malloc()'d memory.
 *		This was needed to get the code to work when compiled under
 *		MSC 4.0 on a PC, and it's probably faster anyway.
 *
 * cdk 2-5-87	Added 'cmp' as a synonym for 'cp', 'jmp' as a synonym for
 *		'jp', and added tolerance of accumulator specification for arithmetic
 *		and logical instructions.  (For example, 'or a,12' is now accepted,
 *		same as 'or 12'.)
 *
 * gwp 12-29-08	Changes to allow compilation with modern C compiler and using bison
 *		as the .y to .c converter.  assert, tstate pseudo-ops.
 *		t(), tilo(), tihi() functions.  ==, <=, >=, !=, !, <, > operators.
 *		-c to turn cycle counts off in listing.  Usage, -h and version.
 *
 * gwp 9-26-10	Add ocf() and setocf to track and set op code fetch counts.
 *		Add sett as an alias for tstate
 *
 * gwp 12-30-11	Add undocumented instructions sl1, pfix, pfiy, in (c), out (c),0
 *		bit/set/res (ixy+d),reg and ld/inc/dec ixylh.
 *
 * gwp 2-8-12   Increase MAXIFS massively due to massive lowt macro
 *
 * gwp 2-11-12  Support 32 bit constants.  '%' alias for MOD.  Add defd, dword.
 *		lo(x) and hi(x) for easy low and high byte extraction.  Allow
 *		filenames longer than 15 characters.  All output to "zout" subdirectory
 *		of source file.
 *
 * gwp 2-15-12	Perform multiple passes while equates are changing.  Support
 *		.label for temporary label definitions and _label for file
 *		scoped labels.  Allow '.' within labels.  Assert listing bugfix.
 *
 * gwp 4-27-12	Implement $ prefixed hex constants and double-quoted strings.
 *
 * gwp 6-30-12	Minor changes to allow compilation with gcc.
 *
 * gwp 9-05-12	incbin added.
 *
 * gwp 11-24-12	Fix macro expansion bug when symbol larger than MAXSYMBOLSIZE
 *		due to file name prepending when symbol starts with '_'.
 *
 * gwp 12-04-12	Optional JR promotion and JP demotion errors.  Output a warning
 *		if no execute address given.  Output ".bds" file to enable easy
 *		simple source level debugging.
 *
 * gwp 4-14-13	Parens in expressions, else, .pseudo, full set of C operators
 *		with conventional precedence and various aliases and code
 *		changes to make source similar to zmac 1.3 on internet.
 *
 * gwp 5-5-13	.cmd,.cas,.lcas,.bin output.  dc (both MACRO-80 and EDAS types).
 *		lo, hi renamed to low, high and make unary operators.  Allow
 *		label::, placeholder public, extern declarations.  Bug fixes
 *		in defs, t, ocf, tihi, tilo in phase mode.  Initial support
 *		for -I include directories. 0x hex constants. --mras flag for
 *		limited MRAS compatibility (allows $ in labels, $? to start
 *		labels).
 *
 * gwp 4-6-13	--rel for .rel (Microsoft linker) output and extern, public,
 *		aseg, cseg, dseg in support (big emit + expression revamp).
 *		-I follows C preprocessor convention, output relative to
 *		current directory.  8080 mnemonics, .z80, .8080, -z, -8.
 *		Change .bin to .cim.  Warn on labels not in first column.
 *
 * gwp 8-11-13	Allow $ to start identifiers and do '$' dropping when macro
 *              parsed so we no longer need to drop '$' in identifiers. 
 *              Even $FCB allowed, with warning.  Add --zmac for backwards
 *		compatibility with zmac.  ` now used for joining in macros.
 *		Most reserved words can be used as labels or variables.
 *		Free-form title, name, comment, subttl parsing.  Allow #arg
 *		for macro arguments (in --mras mode).  Support <CR> delimited
 *		files.  Add .ams output.  Integrate documentation (--doc).
 */

#define MIO		/* use emulation routines from mio.c */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <time.h>

#ifdef WIN32
#include <windows.h>	// just for colouring the output
#endif

#include "zi80dis.h"

#ifdef vax11c
#define unlink(filename) delete(filename)
#endif

#ifdef MIO
FILE *mfopen();
#else
#define mfopen(filename,mode) fopen(filename,mode)
#define mfclose(filename,mode) fclose(filename,mode) 
#define mfputc(c,f) putc(c,f)
#define mfgetc(f) getc(f)
#define mfseek(f,loc,origin) fseek(f,loc,origin)
#define mfread(ptr,size,nitems,f) fread(ptr,size,nitems,f)
#define mfwrite(ptr,size,nitems,f) fread(ptr,size,nitems,f)
#endif /* MIO */

/*
 * DEBUG turns on pass reporting.
 * Macro debug and Token debug enables.
#define	DEBUG
#define	M_DEBUG	
#define	T_DEBUG
 */

#define ITEMTABLESIZE	10000
#define TEMPBUFSIZE	(1000+MAXSYMBOLSIZE)
#define LINEBUFFERSIZE	1000
#define EMITBUFFERSIZE	200
#define MAXSYMBOLSIZE	40
#define IFSTACKSIZE	20
// GWP - I use lots of if's with my lowt macro
#define MAXIFS		65536
#define TITLELEN	50
#define BINPERLINE	16
#define	PARMMAX		25
#define MAXEXP		25
#define SYMMAJIC	07203
#define	NEST_IN		32
#define MAXPASS		32
#define MAXINCPATH	32


void yyerror(char *err)
{}		/* we will do our own error printing */

struct	item	{
	char	*i_string;
	int	i_value;
	int	i_token;
	int	i_uses;
	int	i_scope;
	int	i_chain;
};

#define SCOPE_NONE	(0)
#define SCOPE_PROGRAM	(1)
#define SCOPE_DATA	(2)
#define SCOPE_PUBLIC	(4)
#define SCOPE_EXTERNAL	(8)
#define SCOPE_NORELOC	(16)

#define SCOPE_SEGMASK	(3)
#define SCOPE_SEG(s)	((s) & SCOPE_SEGMASK)

struct expr {
	int e_value;
	int e_scope;
	int e_token;
	struct item *e_item;
	struct expr *e_left;
	struct expr *e_right;
};

#define EXPR_SEG(e)	SCOPE_SEG(e->e_scope)

FILE	*fout,
	*fbuf,
	*fbds,
	*fcmd,
	*fcas,
	*flcas,
	*fcim,
	*fams,
	*frel,
	*fin[NEST_IN],
	*now_file ;

int	pass2;	/*set when pass one completed*/
int	outpass; 	// set when we decide to stop doing passes */
int	passfail;	// set when an error means passes should not continue
int	passretry;	// set when an inconsistency will require another pass
int	dollarsign ;	/* location counter */
int	olddollar ;	/* kept to put out binary */
int	oldothdollar;	// output address of next .cmd/.cas/.lcas block
int	emit_addr;	// were code and data are being placed in memory
int	tstates;	// cumulative T-states
int	ocf;		// cumulative op code fetches
int	llseq;		// local label sequence number
int	mras;		// MRAS semi-compatibility mode
int	zcompat;	// Original zmac compatibility mode
char	modstr[8];	// Replacement string for '?' in labels when MRAS compatible
int	relopt;		// Only output .rel files.
char	progname[8];	// Program name for .rel output
int	note_depend;	// Print names of files included
int	firstcol;
int	logcol;
int	coloncnt;

// Include file search path
char	*incpath[MAXINCPATH];
int	incpath_cnt;

/* program counter save for PHASE/DEPHASE */
int	phdollar, phbegin, phaseflag ;

char	*src_name[NEST_IN] ;
int	linein[NEST_IN] ;
int	now_in ;


// These first 5 errors are singled out in listerr1() for reasons I don't
// quite understand.
#define bflag	0	/* balance error */
#define eflag	1	/* expression error */
#define fflag	2	/* syntax error */
#define iflag	3	/* bad digits */
#define mflag	4	/* multiply defined */

#define pflag	5	/* phase error */
#define uflag	6	/* undeclared used */
#define vflag	7	/* value out of range */
#define oflag	8	/* phase/dephase error */
#define aflag	9	/* assert failed */
#define jflag	10	/* JP could be JR */
#define rflag	11	/* expression not relocatable */
#define gflag	12	/* incorrect register */
#define zflag	13	/* Z-80 instruction */

#define FIRSTWARN	warn_hex
#define	warn_hex	14
#define warn_notimpl	15

#define FLAGS	16	/* number of errors and warnings */

char	err[FLAGS];
int	keeperr[FLAGS];
char	errlet[FLAGS]="BEFIMPUVOAJRGZHN";
char	*errname[FLAGS]={
	"Balance",
	"Expression",
	"Syntax",
	"Digit",
	"Mult. def.",
	"Phase",
	"Undeclared",
	"Value",
	"Phase/Dephase",
	"Assertion failure",
	"Use JR",
	"Not relocatable",
	"Register usage",
	"Z-80 instruction in 8080 mode",
	"$hex constant interpreted as symbol",
	"Not implemented"
};


unsigned char inpbuf[LINEBUFFERSIZE];
unsigned char *inpptr;

char	linebuf[LINEBUFFERSIZE];
char	*lineptr;
char	*linemax = linebuf+LINEBUFFERSIZE;

char	outbin[BINPERLINE];
char	*outbinp = outbin;
char	*outbinm = outbin+BINPERLINE;

char	outoth[256];
int	outoth_cnt = 0;

char	emitbuf[EMITBUFFERSIZE];
char	*emitptr;

char	ifstack[IFSTACKSIZE];
char	*ifptr;
char	*ifstmax = ifstack+IFSTACKSIZE-1;


char	expif[MAXIFS];
char	*expifp;
char	*expifmax = expif+MAXIFS;

char	hexadec[] = "0123456789ABCDEF" ;


int	nitems;
int	linecnt;
int	nbytes;
int	invented;
int	npass;
int	njrpromo;


char	tempbuf[TEMPBUFSIZE];
char	*tempmax = tempbuf+TEMPBUFSIZE-1;

char	inmlex;
char	arg_flag;
int	parm_number;
int	exp_number;
char	symlong[] = "Symbol/number too long";
int	raw;

int	disp;
#define FLOC	PARMMAX
#define TEMPNUM	PARMMAX+1
union exprec {
	char *param;
	int value;
};
union exprec	*est;
union exprec	*est2;
union exprec	*expstack[MAXEXP];
int	expptr;

int	floc;
int	mfptr;
FILE	*mfile;

char	*writesyms;


char	*title;
char	titlespace[TITLELEN];
char	*timp;
char	*sourcef;
/* changed to cope with filenames longer than 14 chars -rjm 1998-12-15 */
char	src[1024];
char	bin[1024];
char	mtmp[1024];
char	listf[1024];
char	bds[1024];
char	oth[1024];

char	bopt = 1,
	copt = 1,	/* cycle counts in listings by default */
	edef = 1,
	eopt = 1,
	fdef = 0,
	fopt = 0,
	gdef = 1,
	gopt = 1,
	iopt = 0 ,	/* list include files */
	jopt = 0,
	JPopt = 0,
	lstoff = 0,
	lston = 0,	/* flag to force listing on */
	lopt = 0,
	mdef = 0,
	mopt = 0,
	nopt = 1 ,	/* line numbers on as default */
	oopt = 0,
	popt = 1,	/* form feed as default page eject */
	sopt = 0,	/* turn on symbol table listing */
	topt = 1,	/* terse, only error count to terminal */
	printer_output = 0, // GWP - printer style output
	z80,
	saveopt;

char default_jopt, default_JPopt, default_z80 = 1;

char	xeq_flag = 0;
int	xeq;

time_t	now;
int	line;
int	page = 1;

struct stab {
	char	t_name[MAXSYMBOLSIZE+1];
	int	t_value;
	int	t_token;
};

// GWP - support for cycle count tracking (and opens door for efficient .cmd, etc. output)

unsigned char memory[1 << 16];
char memflag[1 << 16];
enum {
	MEM_DATA = 1,
	MEM_INST = 2,
	MEM_T_SET = 4
};
int tstatesum[1 << 16];
int ocfsum[1 << 16];

// GWP - expression handling extensions for .rel output.
void advance_segment(int step);
void expr_reloc_check(struct expr *ex);
void expr_number_check(struct expr *ex);
void expr_scope_same(struct expr *ex1, struct expr *ex2);
void expr_word_check(struct expr *ex);
int is_number(struct expr *ex);
int is_external(struct expr *ex);
struct expr *expr_num(int value);
struct expr *expr_alloc(void);
struct expr *expr_op(struct expr *left, int token, struct expr *right, int value);
struct expr *expr_op_sc(struct expr *left, int token, struct expr *right, int value);
void expr_free(struct expr *ex);
int can_extend_link(struct expr *ex);
void extend_link(struct expr *ex);
void putrelop(int op);
#define RELOP_BYTE	(1)
#define RELOP_WORD	(2)
#define RELOP_HIGH	(3)
#define RELOP_LOW	(4)
#define RELOP_NOT	(5)
#define RELOP_NEG	(6)
#define RELOP_SUB	(7)
#define RELOP_ADD	(8)
#define RELOP_MUL	(9)
#define RELOP_DIV	(10)
#define RELOP_MOD	(11)
struct item *locate(char *name);
// Data descriptions for emit()
#define E_CODE		(0)
#define E_DATA		(1)
#define E_CODE8		(2)
#define E_CODE16	(3)
int segment;
#define SEG_ABS		(0)
#define SEG_CODE	(1)
#define SEG_DATA	(2)
int seg_pos[4]; // may eventually support SEG_COMMON
int seg_size[4];
int rel_main;
int segchange;

/*
 *  push back character
 */
int	peekc;
int	nextline_peek;

/* function prototypes */
void error(char *as);
void usage(char *msg, char *param);
void help();
void erreport();
void errorprt(int errnum);
void mlex(char *look);
void popsi();
void suffix(char *str, char *suff);
char *basename(char *filename);
char *getsuffix(char *str);
void outpath(char *out, char *src, char *suff);
void casname(char *out, char *src);
void putm(int c);
void insymtab(char *name);
void outsymtab(char *name);
void putsymtab();
void clear();
void setmem(int addr, int value, int type);
void setvars();
void flushbin();
void flushoth();
void lineout();
void puthex(int byte, FILE *buf);
void putcas(int byte);
void putrelbits(int count, int bits);
void putrel(int byte);
void putrelname(char *str);
void putrelextaddr(int extaddr);
void putrelcmd(int cmd);
void putrelsegref(int scope, int addr);
void flushrel(void);
void lsterr1();
void lsterr2(int lst);
void copyname(char *st1, char *st2);
void next_source(char *sp);
void incbin(char *filename);
void dc(int count, int value);
char *getmraslocal();

#define RELCMD_PUBLIC	(0)
#define RELCMD_COMMON	(1)
#define RELCMD_PROGNAME	(2)
#define RELCMD_LIBLOOK	(3)
#define RELCMD_EXTLINK	(4)
#define RELCMD_COMSIZE	(5)
#define RELCMD_EXTCHAIN	(6)
#define RELCMD_PUBVALUE	(7)
#define RELCMD_EXTMINUS	(8)
#define RELCMD_EXTPLUS	(9)
#define RELCMD_DATASIZE	(10)
#define RELCMD_SETLOC	(11)
#define RELCMD_CODESIZE	(13)
#define RELCMD_ENDMOD	(14)
#define RELCMD_ENDPROG	(15)

/*
 *  add a character to the output line buffer
 */
int addtoline(int ac)
{
	/* check for EOF from stdio */
	if (ac == -1)
		ac = 0 ;
	if (inmlex)
		return(ac);
	if (lineptr >= linemax)
		error("line buffer overflow");
	*lineptr++ = ac;
	return(ac);
}

/*
 *  put values in buffer for outputing
 */

void emit(int bytes, int desc, struct expr *data, ...)
{
	int type, i, args, dsize;
	va_list ap;

	if (relopt && segchange) {
		segchange = 0;
		putrelcmd(RELCMD_SETLOC);
		putrelsegref(segment, seg_pos[segment]);
	}

	// External references only supported in .rel output.
	if (!relopt && data && (data->e_scope & SCOPE_EXTERNAL))
		err[uflag]++;

	va_start(ap, data);

	type = desc == E_DATA ? MEM_DATA : MEM_INST;

	// Check emit is not adding instructions to the buffer.
	if (desc != E_DATA && emitptr != emitbuf)
		fprintf(stderr, "internal inconsistency in t-state counting\n");

	dsize = 0;
	args = bytes;
	if (desc == E_DATA) {
		args = 0;
		dsize = bytes;
	}
	else if (desc == E_CODE16)
		dsize = 2;
	else if (desc == E_CODE8)
		dsize = 1;

	for (i = 0; i < args; i++)
	{
		if (emitptr >= &emitbuf[EMITBUFFERSIZE])
			error("emit buffer overflow");
		else {
			int addr = (emit_addr + (emitptr - emitbuf)) & 0xffff;
			*emitptr = va_arg(ap, int);
			if (segment == SEG_CODE) 
				setmem(addr, *emitptr, type);
			putrel(*emitptr);
			emitptr++;
		}
	}

	va_end(ap);

	for (i = 0; i < dsize; i++) {
		int addr = (emit_addr + (emitptr - emitbuf)) & 0xffff;
		*emitptr = data->e_value >> (i * 8);
		if (segment == SEG_CODE)
			setmem(addr, *emitptr, type);
		emitptr++;
	}

	advance_segment(args + dsize);

	if (desc != E_DATA)
	{
		int eaddr = emit_addr, low, fetch, low8080, addr_after;

		// emitbuf is OK since this only happens with single emits

		if (!z80) {
			// Check for invalid 8080 instructions.
			int op = emitbuf[0] & 0xff;
			if (op == 0x08 || op == 0x10 || op == 0x18 ||
			    op == 0x20 || op == 0x28 || op == 0x30 ||
			    op == 0x38 || op == 0xCB || op == 0xD9 ||
			    op == 0xDD || op == 0xED || op == 0xFD)
			{
				err[zflag]++;
			}
		}

		zi_tstates(emitbuf, &low, 0, &fetch, &low8080, 0);
		if (!z80)
			low = low8080;

		// Sanity check
		if (low <= 0)
		{
			fprintf(stderr, "undefined instruction on %02x %02x (assembler or diassembler broken)\n",
				emitbuf[0], emitbuf[1]);
		}

		// Special case to catch promotion of djnz to DEC B JP NZ
		// Even update the tstatesum[] counter though that seems to
		// me to be above and beyond.
		if (emitbuf[0] == 5 && args == 2) {
			tstatesum[eaddr] = tstates;
			ocfsum[eaddr] = ocf;
			memflag[eaddr] |= MEM_T_SET;
			eaddr++;
			tstates += low;
			ocf += fetch;
			low = 10;
			// still 1 fetch
		}

		// Double setting of both sides of tstatesum[] seems like too
		// much, but must be done in the isolated instruction case:
		// org x ; inc a ; org y

		tstatesum[eaddr] = tstates;
		ocfsum[eaddr] = ocf;
		memflag[eaddr] |= MEM_T_SET;

		// Well, OK, should we default to high or low???
		// Guess it should be whatever makes sense for you
		// to get here which, generally, is the low.

		// low it is.

		tstates += low;
		ocf += fetch;

		addr_after = (emit_addr + (emitptr - emitbuf)) & 0xffff;

		tstatesum[addr_after] = tstates;
		ocfsum[addr_after] = ocf;
		memflag[addr_after] |= MEM_T_SET;
	}

	if (relopt && outpass && dsize > 0) {
		if (dsize == 1) {
			if (is_number(data))
				putrel(data->e_value);
			else if (can_extend_link(data)) {
				extend_link(data);
				putrelop(RELOP_BYTE);
				putrel(0);
			}
			else {
				err[rflag]++;

				putrel(0);
			}
		}
		else if (dsize == 2) {
			int handled = 0;
			if (data->e_scope & SCOPE_EXTERNAL) {
				struct item *var = 0;
				int offset = 0;
				// Simple external reference.
				if (is_external(data))
					var = data->e_item;
				else if (is_external(data->e_left) &&
					data->e_token == '+' &&
					is_number(data->e_right))
				{
					var = data->e_left->e_item;
					offset = data->e_right->e_value;
				}
				else if (is_number(data->e_left) &&
					data->e_token == '+' &&
					is_external(data->e_right))
				{
					offset = data->e_left->e_value;
					var = data->e_right->e_item;
				}
				else if (is_external(data->e_left) &&
					data->e_token == '-' &&
					is_number(data->e_right))
				{
					var = data->e_left->e_item;
					offset = data->e_right->e_value;
				}

				if (var && offset) {
					putrelcmd(data->e_token == '-' ?
						RELCMD_EXTMINUS : RELCMD_EXTPLUS);
					// Theoretically we could put a
					// program or data relative value here...
					putrelsegref(SEG_ABS, offset);
				}

				if (var) {
					if (var->i_chain == 0) {
						putrel(0);
						putrel(0);
					}
					else {
						putrelbits(1, 1);
						putrelextaddr(var->i_chain);
					}
					var->i_chain = (segment << 16) |
						((dollarsign + args) & 0xffff);
					handled = 1;
				}
			}
			else if ((data->e_scope & ~SCOPE_PUBLIC) == 0) {
				// nice constant value
				putrel(data->e_value);
				putrel(data->e_value >> 8);
				handled = 1;
			}
			else if (!(data->e_scope & SCOPE_NORELOC)) {
				// relocatable value
				putrelbits(1, 1);
				putrelbits(2, data->e_scope);
				putrelbits(8, data->e_value);
				putrelbits(8, data->e_value >> 8);
				handled = 1;
			}

			if (!handled) {
				if (can_extend_link(data)) {
					extend_link(data);
					putrelop(RELOP_WORD);
					putrel(0);
					putrel(0);
				}
				else {
					err[rflag]++;
					putrel(data->e_value);
					putrel(data->e_value >> 8);
				}
			}
		}
		else if (dsize == 4) {
			// Only numbers are allowed.
			if (data->e_scope != 0) {
				err[vflag]++;
				if (data->e_scope & SCOPE_NORELOC)
					err[rflag]++;
			}
			for (i = 0; i < dsize; i++)
				putrel(data->e_value >> (i * 8));
		}
		else
			error("internal dsize error");
	}
}

#define ET_NOARG_DISP	(0)
#define ET_NOARG	(1)
#define ET_BYTE		(2)
#define ET_WORD		(5)

void emit1(int opcode, int regvalh, struct expr *data, int type)
{
	if (type == ET_BYTE && (data->e_value < -128 || data->e_value > 255))
		err[vflag]++;

	if (regvalh & 0x10000) {
		switch (type) {
		case ET_NOARG_DISP:
			emit(2, E_CODE, 0, regvalh >> 8, opcode);
			break;
		case ET_BYTE:
			emit(2, E_CODE8, data, regvalh >> 8, opcode);
			break;
		}
	}
	else if (regvalh & 0x8000) {
		switch (type) {
		case ET_NOARG_DISP:
			if (opcode & 0x8000)
				emit(4, E_CODE, 0, regvalh >> 8, opcode >> 8, disp, opcode);
			else
				emit(3, E_CODE, 0, regvalh >> 8, opcode, disp);
			break;
		case ET_NOARG:
			emit(2, E_CODE, 0, regvalh >> 8, opcode);
			break;
		case ET_BYTE:
			emit(3, E_CODE8, data, regvalh >> 8, opcode, disp);
			break;
		case ET_WORD:
			emit(2, E_CODE16, data, regvalh >> 8, opcode);
		}
	} else
		switch(type) {
		case ET_NOARG_DISP:
		case ET_NOARG:
			if (opcode & 0100000)
				emit(2, E_CODE, 0, opcode >> 8, opcode);
			else
				emit(1, E_CODE, 0, opcode);
			break;
		case ET_BYTE:
			emit(1, E_CODE8, data, opcode);
			break;
		case ET_WORD:
			if (opcode & 0100000)
				emit(2, E_CODE16, data, opcode >> 8, opcode);
			else
				emit(1, E_CODE16, data, opcode);
		}
}




void emitdad(int rp1,int rp2)
{
	if (rp1 & 0x8000)
		emit(2, E_CODE, 0, rp1 >> 8, rp2 + 9);
	else
		emit(1, E_CODE, 0, rp2 + 9);
}


void emitjr(int opcode, struct expr *dest)
{
	int disp = dest->e_value - dollarsign - 2;

	if (dest->e_scope & SCOPE_NORELOC)
		err[rflag]++;

	// Can't relative jump to other segments or an external
	if (((dest->e_scope & (SCOPE_SEGMASK | SCOPE_EXTERNAL)) != segment ||
		disp > 127 || disp < -128) && z80)
	{
		if (jopt) {
			njrpromo++;
			switch (opcode) {
			case 0x10: // DJNZ
				emit(2, E_CODE16, dest, 0x05, 0xC2); // DEC B, JP NZ
				break;
			case 0x18: // JR
				emit(1, E_CODE16, dest, 0xC3); // JP
				break;
			case 0x20: // JR NZ
				emit(1, E_CODE16, dest, 0xC2); // JP NZ
				break;
			case 0x28: // JR Z
				emit(1, E_CODE16, dest, 0xCA); // JP Z
				break;
			case 0x30: // JR NC
				emit(1, E_CODE16, dest, 0xD2); // JP NC
				break;
			case 0x38: // JR C
				emit(1, E_CODE16, dest, 0xDA); // JP C
				break;
			default:
				err[vflag]++;	// shouldn't happen!
				expr_free(dest);
				break;
			}
		}
		else {
			err[vflag]++;
			expr_free(dest);
		}
	}
	else {
		emit(2, E_CODE, 0, opcode, disp);
		expr_free(dest);
	}
}

void checkjp(int op, struct expr *dest)
{
	op &= 0x030;
	// Only applies to Z-80 output and if JP optimization checking is on.
	// JR only has z, nz, nc, c
	// A jump to the current segment might have been optimizable
	if (z80 && JPopt && (op == 0 || op == 010 || op == 020 || op == 030) &&
		(dest->e_scope & (SCOPE_SEGMASK | SCOPE_EXTERNAL)) == segment)
	{
		int disp = dest->e_value - dollarsign - 2;
		if (disp >= -128 && disp <= 127)
			err[jflag]++;
	}
}

/*
 *  put out a byte of binary 
 */
void putbin(int v)
{
	if(!outpass || !bopt) return;
	*outbinp++ = v;
	if (outbinp >= outbinm) flushbin();

	outoth[outoth_cnt++] = v;
	if (outoth_cnt == 256) flushoth();
}



/*
 *  output one line of binary in INTEL standard form
 */
void flushbin()
{
	char *p;
	int check=outbinp-outbin;

	if (!outpass || !bopt)
		return;
	nbytes += check;
	if (check) {
		putc(':', fbuf);
		puthex(check, fbuf);
		puthex(olddollar>>8, fbuf);
		puthex(olddollar, fbuf);
		puthex(0, fbuf);
		check += (olddollar >> 8) + olddollar;
		olddollar += (outbinp-outbin);
		for (p=outbin; p<outbinp; p++) {
			puthex(*p, fbuf);
			check += *p;
		}
		puthex(256-check, fbuf);
		putc('\n', fbuf);
		outbinp = outbin;
	}
}



/*
 *  put out one byte of hex
 */
void puthex(int byte, FILE *buf)
{
	putc(hexadec[(byte >> 4) & 017], buf);
	putc(hexadec[byte & 017], buf);
}

void flushoth()
{
	int i, checksum;

	if (!outpass || !bopt || outoth_cnt == 0)
		return;

	fprintf(fcmd, "%c%c%c%c", 1, outoth_cnt + 2, oldothdollar, oldothdollar >> 8);
	fwrite(outoth, outoth_cnt, 1, fcmd);

	putcas(0x3c);
	putcas(outoth_cnt);
	putcas(oldothdollar);
	putcas(oldothdollar >> 8);
	checksum = oldothdollar + (oldothdollar >> 8);
	for (i = 0; i < outoth_cnt; i++) {
		putcas(outoth[i]);
		checksum += outoth[i];
	}
	putcas(checksum);

	oldothdollar += outoth_cnt;
	outoth_cnt = 0;
}

int casbit, casbitcnt = 0;

void putcas(int byte)
{
	fputc(byte, flcas);

	// Buffer 0 stop bit and the 8 data bits.
	casbit = (casbit << 9) | (byte & 0xff);
	casbitcnt += 9;
	while (casbitcnt >= 8) {
		casbitcnt -= 8;
		fputc(casbit >> casbitcnt, fcas);
	}
}

void casname(char *out, char *src)
{
	char *base = basename(src);
	int i;

	out[0] = 'N';
	for (i = 1; i < 6; i++)
		out[i] = ' ';

	for (i = 0; *base && i < 6; base++) {
		if (strcmp(base, ".z") == 0 || strcmp(base, ".Z") == 0)
			break;

		if (*base >= 'a' && *base <= 'z') {
			*out++ = *base - ('a' - 'A');
			i++;
		}
		else if (*base >= 'A' && *base <= 'Z') {
			*out++ = *base;
			i++;
		}
	}
}

int relbit, relbitcnt = 0;

void putrelbits(int count, int bits)
{
	if (!outpass || !relopt)
		return;

	relbit <<= count;
	relbit |= bits & ((1 << count) - 1);
	relbitcnt += count;

	while (relbitcnt >= 8) {
		relbitcnt -= 8;
		fputc(relbit >> relbitcnt, frel);
	}
}

void putrel(int byte)
{
	// Add 0 bit indicating byte to load at next counter
	putrelbits(1, 0);
	// Add byte to load
	putrelbits(8, byte);
}

void putrelname(char *str)
{
	int len = strlen(str);

	// .rel file format can do strings 7 long but for compatibility
	// we restrict them to 6.  I believe this is important because
	// extended link functions require a character when they wish to
	// operate on an external symbol.
	if (len > 6)
		len = 6;
	putrelbits(3, len);
	while (len-- > 0) {
		int ch = *str++;
		if (ch >= 'a' && ch <= 'z')
			ch -= 'a' - 'A';
		putrelbits(8, ch);
	}
}

void putrelsegref(int scope, int addr)
{
	putrelbits(2, scope);
	putrelbits(8, addr);
	putrelbits(8, addr >> 8);
}

void putrelextaddr(int extaddr)
{
	putrelsegref(extaddr >> 16, extaddr);
}


void putrelcmd(int relcmd)
{
	putrelbits(1, 1);
	putrelbits(2, 0);
	putrelbits(4, relcmd);
}

void flushrel(void)
{
	if (relbitcnt > 0)
		putrelbits(8 - relbitcnt, 0);

	if (relopt)
		fflush(frel);
}

/*
 *  put out a line of output -- also put out binary
 */
void list(int optarg)
{
	char *	p;
	int	i;
	int  lst;
	char seg = ' ';

	if (!expptr)
		linecnt++;
	addtoline('\0');
	if (outpass) {
		lst = iflist();
		if (lst) {
			lineout();
			if (nopt)
				fprintf(fout, "%4d:", linein[now_in]);

			if (copt)
			{
			    if (emitptr > emitbuf && (memflag[emit_addr] & MEM_INST))
			    {
			        int low, high, fetch, low8080, high8080;
			        zi_tstates(memory + emit_addr, &low, &high, &fetch, &low8080, &high8080);
				if (!z80) {
					low = low8080;
					high = high8080;
				}

				// Special case to catch promotion of djnz to DEC B JP NZ
				if (memory[emit_addr] == 5 && emitptr - emitbuf == 4) {
					low += 10;
					high += 10;
				}

			    	fprintf(fout, nopt ? "%5d" : "%4d", tstatesum[emit_addr]);

				fprintf(fout, "+%d", low);
				if (low != high)
				    fprintf(fout, "+%d", high - low);
			    }
			    else
			    {
			        fprintf(fout, nopt ? "%5s-" : "%4s-", "");
			    }
			}

			if (nopt || copt)
				fprintf(fout, "\t");

			puthex(optarg >> 8, fout);
			puthex(optarg, fout);
			if (relopt)
				seg = " '\"!"[segment];
			fputc(seg, fout);
			fputc(' ', fout);
			for (p = emitbuf; (p < emitptr) && (p - emitbuf < 4); p++) {
				puthex(*p, fout);
			}
			for (i = 4 - (p-emitbuf); i > 0; i--)
				fputs("  ", fout);

			putc('\t', fout);
			fputs(linebuf, fout);
		}

		if (bopt) {
			fprintf(fbds, "%04x %04x d ", dollarsign, emit_addr);
			for (p = emitbuf; p < emitptr; p++)
				fprintf(fbds, "%02x", *p & 0xff);
			fprintf(fbds, "\n");
			fprintf(fbds, "%04x %04x s %s", dollarsign, emit_addr, linebuf);

			for (p = emitbuf; p < emitptr; p++)
				putbin(*p);
		}


		p = emitbuf+4;
		while (lst && gopt && p < emitptr) {
			lineout();
			if (nopt) putc('\t', fout);
			fputs("      ", fout);
			for (i = 0; (i < 4) && (p < emitptr);i++) {
				puthex(*p, fout);
				p++;
			}
			putc('\n', fout);
		}

		lsterr2(lst);
	} else
		lsterr1();
	dollarsign += emitptr - emitbuf;
	emit_addr += emitptr - emitbuf;
	emitptr = emitbuf;
	lineptr = linebuf;
	advance_segment(emitptr - emitbuf);
}



/*
 *  keep track of line numbers and put out headers as necessary
 */
void lineout()
{
	if (!printer_output)
		return;

	if (line == 60) {
		if (popt)
			putc('\014', fout);	/* send the form feed */
		else
			fputs("\n\n\n\n\n", fout);
		line = 0;
	}
	if (line == 0) {
		fprintf(fout, "\n\n%s %s\t%s\t Page %d\n\n\n",
			&timp[4], &timp[20], title, page++);
		line = 4;
	}
	line++;
}


/*
 *  cause a page eject
 */
void eject()
{
	if (printer_output)
		return;

	if (outpass && iflist()) {
		if (popt) {
			putc('\014', fout);	/* send the form feed */
		} else {
			while (line < 65) {
				line++;
				putc('\n', fout);
			}
		}
	}
	line = 0;
}


/*
 *  space n lines on the list file
 */
void space(int n)
{
	int	i ;
	if (outpass && iflist())
		for (i = 0; i<n; i++) {
			lineout();
			putc('\n', fout);
		}
}

/*
 *  Error handling - pass 1
 */
void lsterr1()
{
	int i;
	for (i = 0; i <= mflag; i++)
		if (err[i]) {
			if (topt)
				errorprt(i);
			passfail = 1;
			err[i] = 0;
		}
}


/*
 *  Error handling - pass 2.
 */
void lsterr2(int lst)
{
	int i;
	for (i=0; i<FLAGS; i++)
		if (err[i]) {
			if (i < FIRSTWARN)
				passfail = 1;
			if (lst) {
				lineout();
				fprintf(fout, "%c %s %s\n",
					errlet[i], errname[i],
					i < FIRSTWARN ? "error" : "warning");
			}
			err[i] = 0;
			keeperr[i]++;
			if (i > mflag && topt)
				errorprt(i);
		}

	fflush(fout);	/*to avoid putc(har) mix bug*/
}

/*
 *  print diagnostic to error terminal
 */
void errorprt(int errnum)
{
	fprintf(stderr,"%s(%d) : %s %s\n%s\n",
		src_name[now_in], linein[now_in], errname[errnum],
			errnum < FIRSTWARN ? "error" : "warning", linebuf) ;
	fflush(stderr) ;
	return ;
}


/*
 *  list without address -- for comments and if skipped lines
 */
void list1()
{
	int lst;

	addtoline('\0');
	lineptr = linebuf;
	if (!expptr) linecnt++;
	if (outpass) {
		if (lst = iflist()) {
			lineout();
			if (nopt)
				fprintf(fout, "%4d:\t", linein[now_in]);
			if (copt)
				fprintf(fout, "\t");
			fprintf(fout, "\t\t%s", linebuf);
			lsterr2(lst);
		}
		if (bopt)
			fprintf(fbds, "%04x %04x s %s", dollarsign, emit_addr, linebuf);
	}
	else
		lsterr1();
}


/*
 *  see if listing is desired
 */
int iflist()
{
	int i, j;

	if (lston)
		return(1) ;
	if (lopt)
		return(0);
	if (*ifptr && !fopt)
		return(0);
	if (!lstoff && !expptr)
		return(1);
	j = 0;
	for (i=0; i<FLAGS; i++)
		if (err[i])
			j++;
	if (expptr)
		return(mopt || j);
	if (eopt && j)
		return(1);
	return(0);
}

// GWP - This avoids an apparent bug in bison as it tries to start out the
// Not needed under gcc which defines __STDC__ so I guard it to prevent
// warnings.
// yyparse() function with yyparse() ; { }.
#ifndef __STDC__
#define __STDC__
#endif

#define PSTITL	(0)	/* title */
#define PSRSYM	(1)	/* rsym */
#define PSWSYM	(2)	/* wsym */
#define PSINC	(3)	/* include file */

#define SPTITL	(0)	/* title */
#define SPSBTL	(1)	/* sub title */
#define SPNAME	(2)	/* name */
#define SPCOM	(3)	/* comment */


#line 1488 "zmac.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif


/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    STRING = 258,
    NOOPERAND = 259,
    ARITHC = 260,
    ADD = 261,
    LOGICAL = 262,
    AND = 263,
    OR = 264,
    XOR = 265,
    ANDAND = 266,
    OROR = 267,
    BIT = 268,
    CALL = 269,
    INCDEC = 270,
    DJNZ = 271,
    EX = 272,
    IM = 273,
    PHASE = 274,
    DEPHASE = 275,
    TK_IN = 276,
    JR = 277,
    LD = 278,
    TK_OUT = 279,
    PUSHPOP = 280,
    RET = 281,
    SHIFT = 282,
    RST = 283,
    REGNAME = 284,
    IXYLH = 285,
    ACC = 286,
    C = 287,
    RP = 288,
    HL = 289,
    INDEX = 290,
    AF = 291,
    AFp = 292,
    SP = 293,
    MISCREG = 294,
    COND = 295,
    SPCOND = 296,
    NUMBER = 297,
    UNDECLARED = 298,
    END = 299,
    ORG = 300,
    ASSERT = 301,
    TSTATE = 302,
    T = 303,
    TILO = 304,
    TIHI = 305,
    SETOCF = 306,
    OCF = 307,
    LOW = 308,
    HIGH = 309,
    DC = 310,
    DEFB = 311,
    DEFD = 312,
    DEFS = 313,
    DEFW = 314,
    EQU = 315,
    DEFL = 316,
    LABEL = 317,
    EQUATED = 318,
    WASEQUATED = 319,
    DEFLED = 320,
    MULTDEF = 321,
    MOD = 322,
    SHL = 323,
    SHR = 324,
    NOT = 325,
    LT = 326,
    GT = 327,
    LE = 328,
    GE = 329,
    NE = 330,
    IF_TK = 331,
    ELSE_TK = 332,
    ENDIF_TK = 333,
    ARGPSEUDO = 334,
    INCBIN = 335,
    LIST = 336,
    MINMAX = 337,
    MACRO = 338,
    MNAME = 339,
    OLDMNAME = 340,
    ARG = 341,
    ENDM = 342,
    MPARM = 343,
    ONECHAR = 344,
    TWOCHAR = 345,
    JRPROMOTE = 346,
    JPERROR = 347,
    PUBLIC = 348,
    EXTRN = 349,
    MRAS_MOD = 350,
    SETSEG = 351,
    INSTSET = 352,
    LXI = 353,
    DAD = 354,
    STAX = 355,
    STA = 356,
    SHLD = 357,
    LDAX = 358,
    LHLD = 359,
    LDA = 360,
    MVI = 361,
    MOV = 362,
    INXDCX = 363,
    INRDCR = 364,
    PSW = 365,
    JUMP8 = 366,
    JP = 367,
    CALL8 = 368,
    ALUI8 = 369,
    SPECIAL = 370,
    RAWTOKEN = 371,
    LOCAL = 372,
    UNARY = 373
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE YYSTYPE;
union YYSTYPE
{
#line 1423 "zmac.y" /* yacc.c:355  */

	struct expr *exprptr;
	struct item *itemptr;
	int ival;
	char *cval;
	

#line 1652 "zmac.c" /* yacc.c:355  */
};
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);



/* Copy the second part of user declarations.  */
#line 1549 "zmac.y" /* yacc.c:358  */

char  *cp;
int  i;

#line 1671 "zmac.c" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  2
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   3299

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  143
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  50
/* YYNRULES -- Number of rules.  */
#define YYNRULES  272
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  497

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   373

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     134,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   131,     2,   139,   140,   130,   122,     2,
     136,   137,   128,   126,   135,   127,     2,   129,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   119,     2,
     124,   123,   125,   118,     2,     2,     2,     2,     2,     2,
     138,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   141,     2,   142,   121,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,   120,     2,   132,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   133
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,  1556,  1556,  1559,  1564,  1581,  1585,  1618,  1634,  1657,
    1679,  1688,  1694,  1699,  1709,  1734,  1749,  1751,  1753,  1764,
    1773,  1801,  1807,  1811,  1859,  1862,  1914,  1921,  1928,  1932,
    1936,  1947,  1956,  1961,  1986,  2002,  2019,  2046,  2048,  2050,
    2055,  2057,  2099,  2101,  2105,  2116,  2118,  2122,  2138,  2141,
    2150,  2158,  2169,  2172,  2184,  2187,  2190,  2193,  2196,  2199,
    2207,  2210,  2213,  2216,  2219,  2222,  2225,  2228,  2231,  2234,
    2237,  2240,  2243,  2246,  2249,  2252,  2255,  2258,  2261,  2264,
    2267,  2270,  2273,  2276,  2279,  2282,  2285,  2292,  2295,  2304,
    2306,  2309,  2312,  2315,  2318,  2328,  2338,  2344,  2350,  2353,
    2356,  2359,  2362,  2365,  2368,  2371,  2374,  2405,  2411,  2414,
    2417,  2425,  2431,  2443,  2449,  2452,  2458,  2464,  2470,  2479,
    2482,  2488,  2494,  2503,  2509,  2518,  2524,  2534,  2544,  2547,
    2550,  2563,  2570,  2573,  2576,  2579,  2586,  2593,  2596,  2608,
    2619,  2633,  2643,  2671,  2681,  2689,  2697,  2699,  2701,  2703,
    2707,  2709,  2711,  2714,  2714,  2717,  2728,  2730,  2733,  2735,
    2737,  2741,  2752,  2755,  2757,  2762,  2773,  2775,  2778,  2780,
    2783,  2789,  2791,  2794,  2797,  2802,  2807,  2813,  2818,  2821,
    2831,  2838,  2840,  2843,  2845,  2848,  2853,  2858,  2861,  2863,
    2866,  2871,  2877,  2879,  2885,  2890,  2896,  2898,  2904,  2909,
    2913,  2915,  2918,  2924,  2931,  2941,  2943,  2948,  2958,  2960,
    2965,  2975,  2977,  2984,  2986,  2990,  2995,  3001,  3011,  3016,
    3021,  3030,  3039,  3048,  3054,  3067,  3076,  3091,  3104,  3115,
    3118,  3131,  3134,  3137,  3140,  3143,  3146,  3149,  3152,  3155,
    3161,  3164,  3167,  3170,  3173,  3176,  3179,  3182,  3185,  3188,
    3191,  3205,  3208,  3211,  3214,  3217,  3220,  3223,  3230,  3239,
    3248,  3255,  3260,  3267,  3269,  3271,  3273,  3275,  3277,  3282,
    3295,  3299,  3302
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "STRING", "NOOPERAND", "ARITHC", "ADD",
  "LOGICAL", "AND", "OR", "XOR", "ANDAND", "OROR", "BIT", "CALL", "INCDEC",
  "DJNZ", "EX", "IM", "PHASE", "DEPHASE", "TK_IN", "JR", "LD", "TK_OUT",
  "PUSHPOP", "RET", "SHIFT", "RST", "REGNAME", "IXYLH", "ACC", "C", "RP",
  "HL", "INDEX", "AF", "AFp", "SP", "MISCREG", "COND", "SPCOND", "NUMBER",
  "UNDECLARED", "END", "ORG", "ASSERT", "TSTATE", "T", "TILO", "TIHI",
  "SETOCF", "OCF", "LOW", "HIGH", "DC", "DEFB", "DEFD", "DEFS", "DEFW",
  "EQU", "DEFL", "LABEL", "EQUATED", "WASEQUATED", "DEFLED", "MULTDEF",
  "MOD", "SHL", "SHR", "NOT", "LT", "GT", "LE", "GE", "NE", "IF_TK",
  "ELSE_TK", "ENDIF_TK", "ARGPSEUDO", "INCBIN", "LIST", "MINMAX", "MACRO",
  "MNAME", "OLDMNAME", "ARG", "ENDM", "MPARM", "ONECHAR", "TWOCHAR",
  "JRPROMOTE", "JPERROR", "PUBLIC", "EXTRN", "MRAS_MOD", "SETSEG",
  "INSTSET", "LXI", "DAD", "STAX", "STA", "SHLD", "LDAX", "LHLD", "LDA",
  "MVI", "MOV", "INXDCX", "INRDCR", "PSW", "JUMP8", "JP", "CALL8", "ALUI8",
  "SPECIAL", "RAWTOKEN", "LOCAL", "'?'", "':'", "'|'", "'^'", "'&'", "'='",
  "'<'", "'>'", "'+'", "'-'", "'*'", "'/'", "'%'", "'!'", "'~'", "UNARY",
  "'\\n'", "','", "'('", "')'", "'F'", "'#'", "'$'", "'['", "']'",
  "$accept", "statements", "statement", "maybecolon", "label.part",
  "public.list", "public.part", "extrn.list", "extrn.part", "operation",
  "parm.list", "maybeocto", "parm.element", "locals", "local.list",
  "local.element", "arg.list", "arg.element", "allreg", "reg", "ixylhreg",
  "reg8", "m", "realreg", "mem", "memxy", "evenreg", "evenreg8",
  "pushable", "pushable8", "bcdesp", "bcdehlsp", "mar", "condition",
  "spcondition", "db.list", "db.list.element", "dw.list",
  "dw.list.element", "dd.list", "dd.list.element", "lxexpression",
  "expression", "parenexpr", "noparenexpr", "symbol", "al", "arg_on",
  "arg_off", "raw_on", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
     365,   366,   367,   368,   369,   370,   371,   372,    63,    58,
     124,    94,    38,    61,    60,    62,    43,    45,    42,    47,
      37,    33,   126,   373,    10,    44,    40,    41,    70,    35,
      36,    91,    93
};
# endif

#define YYPACT_NINF -320

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-320)))

#define YYTABLE_NINF -212

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -320,  2243,  -320,  -320,   -44,  -320,  -320,  -320,  -320,  -320,
    2076,   -93,   -82,  -320,  1646,   -23,  2076,  2076,   241,   241,
     -72,   -57,   -38,  -320,  -320,  2439,   130,   -24,  -320,  -320,
    -320,  2076,  2076,  2076,  2076,  2076,  2076,  -320,  -320,  -320,
    -320,  -320,  2076,  -320,  2076,  2076,  2076,  2076,  2076,  -320,
    2076,  2752,  -320,  -320,  -320,  -320,    -2,  -320,  2787,  -320,
    2820,  2855,  -320,    37,  -320,  -320,    58,  -320,  -320,  -320,
    -320,  -320,   -12,  1675,   368,   188,   413,   570,   683,   727,
    2076,   935,   165,  2076,    -3,  2076,  2076,  -320,   833,  1355,
     135,  2105,   228,    91,    51,  2076,  1770,  2076,  2076,  2076,
    2076,  1388,  1489,  2076,  2076,  2076,  -320,  -320,  -320,    13,
      13,   281,  2076,  2076,   281,  2076,  2076,   173,   173,    13,
     173,  2076,   979,  2076,  2076,  -320,   -18,  2076,  2076,  2076,
      -9,  -320,  -320,    72,   145,  -320,  -320,  -320,  -320,  -320,
    -320,  -320,  -320,  -320,  -320,  -320,  -320,  2491,  2351,  2076,
    2076,  2076,  2076,  2076,  2076,  2076,  2076,  2076,  2076,  2076,
    2076,  2076,  2076,  2076,  2076,  2076,  2076,  2076,  2076,  2076,
    2076,  2076,  2076,  2076,  -320,  -320,    67,  -320,  -320,  -320,
    -320,   241,  -320,   241,  -320,  3127,  -320,  -320,    73,  -320,
      86,  -320,  1522,  -320,  -320,  -320,  -320,  -320,  -320,  -320,
    3127,    92,  -320,  -320,  -320,  -320,    94,  3127,   100,  -320,
    -320,  3127,   104,  -320,  3127,   113,  -320,  3127,   131,  -320,
    3127,  2614,  -320,  -320,  -320,   146,  -320,  3127,  -320,  -320,
    -320,   181,  -320,  -320,  -320,  -320,  3127,   147,   150,   232,
    3127,  3127,  1799,   151,   154,  3127,   156,  3127,   160,  1079,
     167,   174,   183,  1828,  3127,   186,  -320,  -320,  -320,  -320,
    -320,  -320,  -320,  -320,  -320,   187,  3127,  -320,  2888,  3127,
    3127,  3127,  3127,   182,   189,   191,  2683,  -320,  -320,   192,
    -320,  3127,   195,  -320,  3127,  2590,   196,  -320,  3127,   222,
     246,  -320,  -320,   198,  -320,  -320,  3127,  3127,  -320,  3127,
    3127,   200,  -320,  -320,   201,  -320,  -320,  3127,  1617,   202,
    3127,  3127,  3127,  -320,  2923,  2956,  2718,  -320,   209,   204,
    -320,  -320,  -320,  3169,  3160,    53,  2307,  2152,  -320,   -29,
     -29,   117,   117,   117,   117,   457,  3092,  3160,    53,  3169,
     457,   117,   117,   -21,   -21,  -320,  -320,  -320,   205,  -320,
    -320,  -320,   877,   176,   203,  1923,   877,   240,   877,   877,
     877,   877,    51,  2076,   307,   310,   207,   215,   218,   219,
    2076,   325,   220,   526,  1123,   234,   224,   327,   281,  -320,
    -320,  -320,  -320,  2076,  1489,  2076,  -320,  2076,  2076,  -320,
    -320,    90,  -320,  1952,  2076,   173,   225,  2076,  -320,  -320,
    2076,   316,  -320,  -320,  2076,  -320,  -320,  3127,  -320,  -320,
    -320,  -320,  -320,  2519,  -320,  3127,  -320,  -320,  -320,  3127,
    -320,  3127,  -320,  3127,  -320,  3127,  -320,   230,  3127,  -320,
    -320,   231,  -320,   335,  1981,  -320,  3127,  -320,   233,  -320,
    1223,  -320,  3127,   236,   237,  -320,  -320,  -320,   238,   242,
    -320,  -320,   244,  -320,  -320,  2991,  -320,  -320,  3024,  -320,
     247,  -320,   246,  -320,  3127,  -320,  -320,  3127,  3059,  -320,
      98,  -320,  3127,  -320,   281,   221,   243,   245,   354,   249,
    1252,  -320,  -320,  -320,  -320,  -320,  -320,   316,  -320,  -320,
    -320,  -320,  -320,  -320,  -320,  3127,  -320
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       2,     0,     1,    36,   263,   264,   266,   267,   268,   265,
       0,     0,     0,   270,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   272,     3,     0,    37,   150,   216,   218,
     224,     0,     0,     0,     0,     0,     0,   217,   220,   221,
     222,   225,     0,   219,     0,     0,     0,     0,     0,   223,
       0,     0,   213,   214,    10,    11,   271,    24,     0,    34,
       0,     0,   263,     0,    42,    44,     0,    45,    47,    30,
      31,    32,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   141,     0,     0,
       0,     0,     0,   104,    50,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   270,   269,   149,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     4,     0,     0,     0,     0,
      38,    41,   154,     0,     0,   151,   257,   258,   259,   260,
     261,   262,   252,   255,   256,   254,   253,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     9,   271,     0,    25,    26,    27,
      28,     0,    29,     0,    23,    49,   174,   170,   175,   176,
       0,   173,     0,    70,   166,   167,    72,   168,   169,   178,
      57,   175,   194,   195,    67,    69,     0,    55,   175,    73,
      74,    59,   175,    75,    60,   175,    76,    61,   175,    77,
      62,     0,   199,   197,   198,     0,   196,    52,   175,   190,
     191,     0,    84,    90,   181,   182,   103,     0,     0,     0,
     139,   140,     0,     0,     0,   131,     0,   101,     0,     0,
       0,     0,     0,     0,   136,   213,   185,   186,   189,   188,
      92,    93,   187,   105,    82,   178,    53,    12,     0,   142,
     143,   144,   145,     0,   219,     0,     0,   203,   202,   146,
     200,   204,   148,   208,   210,     0,   147,   205,   207,     0,
     162,   184,   183,     0,    89,   115,   117,   125,   111,   123,
     113,     0,   172,   171,     0,    91,    85,    97,     0,     0,
      51,   100,    54,     5,     0,     0,     0,    39,   156,   153,
     155,   215,   251,   233,   235,   237,   248,   249,   231,   238,
     239,   243,   244,   245,   246,   247,     0,   234,   236,   232,
     241,   240,   242,   226,   227,   229,   228,   230,     0,    21,
      43,    46,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    13,
      18,    16,    17,     0,     0,     0,    14,     0,     0,   271,
     165,     0,   163,     0,     0,     0,     0,     0,     6,     7,
       0,   158,    33,   152,     0,    20,    71,    58,   193,   192,
      86,   177,   180,     0,    68,    56,    87,    88,    78,    63,
      79,    64,    80,    65,    81,    66,    94,   178,    99,   127,
     128,     0,   134,     0,     0,   130,   102,   119,     0,   118,
       0,   106,     0,   213,   214,   212,   126,   120,   213,   214,
     116,   124,     0,   135,    83,     0,   201,   209,     0,   206,
       0,    35,     0,   121,   109,   107,    98,    96,     0,   161,
       0,   159,   250,   179,     0,     0,     0,     0,     0,     0,
       0,    19,    15,    22,   164,     8,   157,     0,    95,   129,
     133,   132,   114,   110,   137,   138,   160
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -320,  -320,  -320,  -320,  -320,  -320,   208,  -320,   210,  -320,
    -320,  -320,    69,  -320,  -320,   -96,  -320,   -70,   -62,   -86,
    -320,  -115,    61,   -61,  -320,   -83,   -88,    19,  -320,  -320,
    -223,  -320,   -74,   -50,   306,  -320,    12,  -320,    15,  -320,
      20,    11,   -10,   -81,  -319,    28,  -320,   300,  -163,  -320
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,    24,   131,    25,    63,    64,    66,    67,   126,
     133,   134,   135,   402,   470,   471,   391,   392,   193,   194,
     195,   301,   302,   197,   198,   199,   233,   293,   260,   261,
     234,   410,   235,   225,   226,   279,   280,   286,   287,   282,
     283,   447,   147,    52,    53,    65,   290,    56,   176,    72
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      51,   206,   251,   304,    58,   306,    60,    61,   264,   252,
     255,   265,   348,   204,   209,   213,   216,   219,   262,  -153,
     232,   136,   137,   138,   139,   140,   141,   244,   250,    26,
     237,   259,   142,   238,   143,   144,   145,   146,   154,    27,
     148,    54,   186,   263,   228,   189,   154,    68,   292,   292,
     295,   291,    55,   298,   444,   449,   303,   303,   292,   303,
      59,   149,    69,   185,   200,   207,   211,   214,   217,   220,
     221,   227,   309,   236,   449,   240,   241,    70,   245,   247,
     186,   254,   228,   189,   175,   266,   268,   269,   270,   271,
     272,   276,   281,   284,   285,   288,    71,   169,   170,   171,
     172,   173,   296,   297,   184,   299,   300,   171,   172,   173,
     317,   307,   310,   311,   312,   132,   313,   314,   315,   316,
     154,   155,   156,   222,   157,   158,   159,   160,   161,   294,
     409,   223,   224,   239,   416,   196,   205,   210,   305,   323,
     324,   325,   326,   327,   328,   329,   330,   331,   332,   333,
     334,   335,   336,   337,   338,   339,   340,   341,   342,   343,
     344,   345,   346,   347,   186,   187,   228,   189,   229,   202,
     203,   180,   181,   230,   248,   165,   166,   167,   168,   169,
     170,   171,   172,   173,   154,   155,   156,   231,   320,    28,
     127,   128,   182,   183,   186,   187,   228,   189,   229,   202,
     203,   349,   186,   230,   228,   189,   318,   319,   352,   229,
     408,    68,   129,   191,   230,   354,   355,   186,   187,   201,
     189,   353,   202,   203,   461,   462,   460,   356,   191,   357,
      29,    30,   486,   487,   396,   358,    31,    32,    33,   359,
      34,    35,    36,   169,   170,   171,   172,   173,   360,   130,
      37,    38,    39,    40,    41,   202,   203,   186,    42,   228,
     189,   256,   202,   203,   257,   450,   361,   229,   202,   203,
     366,   249,   230,   229,   202,   203,   426,    43,   230,   427,
     465,   363,   364,   417,    62,   365,   368,   451,   435,   369,
     406,   370,   443,   448,   414,   371,   418,   420,   422,   424,
     446,   231,   373,     5,     6,     7,     8,     9,   389,   374,
     186,   441,   228,   189,    44,    45,   380,   454,   375,    46,
      47,   377,   378,   381,   192,   382,   401,   384,    49,    50,
     385,   388,   390,   393,   303,   394,   395,   397,   258,   405,
     411,   429,   407,   132,   431,   413,   415,   430,   419,   421,
     423,   425,   432,   428,   433,   434,   437,   438,   453,   469,
     436,   452,   466,   442,   442,   474,   475,   476,   478,    28,
    -112,  -108,  -122,   455,   281,   284,  -211,   458,   288,   480,
     490,   483,   491,   442,   464,   492,   493,   467,   403,   350,
     468,   496,   484,   351,   472,   246,   456,   186,   187,   188,
     189,   489,   190,   459,   463,   457,   289,     0,   191,     0,
      29,    30,     0,   488,    28,     0,    31,    32,    33,   494,
      34,    35,    36,     0,     0,     0,     0,     0,     0,     0,
      37,    38,    39,    40,    41,     0,     0,     0,    42,     0,
       0,     0,   186,   187,   208,   189,     0,     0,     0,     0,
       0,     0,     0,   191,     0,    29,    30,    43,     0,     0,
       0,    31,    32,    33,     0,    34,    35,    36,     0,     0,
     495,     0,     0,     0,     0,    37,    38,    39,    40,    41,
       0,     0,     0,    42,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    44,    45,     0,     0,     0,    46,
      47,     0,    43,     0,   192,     0,     0,     0,    49,    50,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   154,   155,   156,    28,   157,   158,
     159,   160,     0,     0,     0,     0,     0,     0,     0,    44,
      45,     0,     0,     0,    46,    47,     0,     0,     0,   192,
       0,     0,     0,    49,    50,   186,   187,   228,   189,     0,
       0,     0,     0,     0,     0,   439,     0,     0,    29,    30,
       0,    28,     0,     0,    31,    32,    33,     0,    34,    35,
      36,   167,   168,   169,   170,   171,   172,   173,    37,    38,
      39,    40,    41,     0,     0,     0,    42,     0,     0,   186,
     187,   212,   189,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    29,    30,     0,    43,     0,     0,    31,    32,
      33,     0,    34,    35,    36,     0,     0,     0,     0,     0,
       0,     0,    37,    38,    39,    40,    41,     0,     0,     0,
      42,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    44,    45,     0,     0,     0,    46,    47,    43,
       0,     0,   440,     0,     0,     0,    49,    50,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    28,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    44,    45,     0,     0,
       0,    46,    47,     0,     0,     0,   192,     0,     0,     0,
      49,    50,   186,   187,   215,   189,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    29,    30,     0,    28,     0,
       0,    31,    32,    33,     0,    34,    35,    36,     0,     0,
       0,     0,     0,     0,     0,    37,    38,    39,    40,    41,
       0,     0,     0,    42,     0,     0,   186,   187,   218,   189,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    29,
      30,     0,    43,     0,     0,    31,    32,    33,     0,    34,
      35,    36,     0,     0,     0,     0,     0,     0,     0,    37,
      38,    39,    40,    41,     0,     0,     0,    42,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    44,
      45,     0,     0,     0,    46,    47,    43,     0,     0,   192,
       0,     0,     0,    49,    50,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    28,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    44,    45,     0,     0,     0,    46,    47,
       0,     0,   186,   192,   228,   189,     0,    49,    50,     0,
       0,     0,     0,     0,     0,    29,    30,     0,    28,     0,
       0,    31,    32,    33,     0,    34,    35,    36,     0,     0,
       0,     0,     0,     0,     0,    37,    38,    39,    40,    41,
       0,     0,     0,    42,     0,     0,   186,   187,   228,   189,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    29,
      30,     0,    43,     0,     0,    31,    32,    33,     0,    34,
      35,    36,     0,     0,     0,     0,    28,     0,     0,    37,
      38,    39,    40,    41,     0,     0,     0,    42,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    44,
      45,     0,     0,     0,    46,    47,    43,   222,     0,   242,
       0,   243,     0,    49,    50,   223,   224,    29,    30,     0,
      28,     0,     0,    31,    32,    33,     0,    34,    35,    36,
       0,     0,     0,     0,     0,     0,     0,    37,    38,    39,
      40,    41,     0,    44,    45,    42,     0,     0,    46,    47,
       0,   222,     0,   192,     0,     0,     0,    49,    50,   223,
     224,    29,    30,     0,    43,     0,     0,    31,    32,    33,
       0,    34,    35,    36,     0,     0,     0,     0,     0,     0,
       0,    37,    38,    39,    40,    41,     0,     0,     0,    42,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    44,    45,     0,     0,     0,    46,    47,    43,     0,
       0,    48,     0,     0,     0,    49,    50,     0,     0,     0,
      28,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    44,    45,     0,     0,     0,
      46,    47,   372,   354,   355,   308,     0,     0,     0,    49,
      50,    29,    30,     0,    28,     0,     0,    31,    32,    33,
       0,    34,    35,    36,     0,     0,     0,     0,     0,     0,
       0,    37,    38,    39,    40,    41,     0,     0,     0,    42,
       0,     0,     0,     0,     0,     0,     0,   202,   203,     0,
       0,     0,     0,     0,     0,    29,    30,     0,    43,     0,
       0,    31,    32,    33,     0,    34,    35,    36,     0,     0,
       0,     0,     0,     0,     0,    37,    38,    39,    40,    41,
       0,     0,     0,    42,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    44,    45,     0,     0,     0,
      46,    47,    43,   445,     0,    48,     0,     0,     0,    49,
      50,     0,     0,     0,    28,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    44,
      45,     0,     0,    28,    46,    47,   479,   354,   355,    48,
       0,     0,     0,    49,    50,    29,    30,     0,     0,     0,
       0,    31,    32,    33,     0,    34,    35,    36,     0,     0,
       0,   186,     0,   228,   189,    37,    38,    39,    40,    41,
       0,     0,     0,    42,    29,    30,     0,     0,     0,     0,
      31,    32,    33,     0,    34,    35,    36,     0,     0,     0,
       0,     0,    43,     0,    37,    38,    39,    40,    41,     0,
       0,     0,    42,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    43,     0,     0,     0,     0,     0,     0,     0,    44,
      45,     0,     0,     0,    46,    47,    28,     0,     0,    48,
       0,     0,     0,    49,    50,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    44,    45,
       0,     0,     0,    46,    47,     0,     0,   222,    48,    28,
       0,   273,    49,    50,     0,     0,   224,    29,    30,     0,
       0,     0,     0,    31,    32,    33,     0,    34,    35,    36,
       0,     0,     0,     0,     0,     0,     0,    37,    38,    39,
      40,    41,     0,     0,     0,    42,     0,     0,     0,     0,
      29,    30,     0,     0,     0,     0,    31,    32,    33,     0,
      34,    35,    36,     0,    43,     0,     0,     0,     0,     0,
      37,    38,    39,    40,    41,     0,     0,     0,    42,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   274,   275,     0,
       0,    44,    45,     0,     0,     0,    46,    47,     0,     0,
      28,    48,   277,     0,     0,    49,    50,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    44,    45,     0,     0,     0,    46,
      47,     0,     0,    28,    48,     0,     0,     0,    49,    50,
       0,    29,    30,     0,     0,     0,     0,    31,    32,    33,
       0,    34,    35,    36,     0,     0,     0,     0,     0,     0,
       0,    37,    38,    39,    40,    41,   354,   355,     0,    42,
       0,     0,     0,     0,    29,    30,     0,     0,     0,     0,
      31,    32,    33,     0,    34,    35,    36,     0,    43,   278,
       0,     0,     0,     0,    37,    38,    39,    40,    41,     0,
       0,     0,    42,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    43,     0,     0,     0,    44,    45,     0,    28,     0,
      46,    47,     0,     0,     0,    48,     0,     0,     0,    49,
      50,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    28,    44,    45,
       0,   202,   203,    46,    47,     0,     0,     0,    48,    29,
      30,     0,    49,    50,     0,    31,    32,    33,     0,    34,
      35,    36,     0,     0,     0,     0,    28,     0,     0,    37,
      38,    39,    40,    41,     0,     0,     0,    42,    29,    30,
       0,     0,     0,     0,    31,    32,    33,     0,    34,    35,
      36,     0,     0,     0,     0,     0,    43,     0,    37,    38,
      39,    40,    41,     0,     0,     0,    42,    29,    30,     0,
       0,     0,     0,    31,    32,    33,     0,    34,    35,    36,
       0,     0,     0,     0,     0,    43,     0,    37,    38,    39,
      40,    41,     0,    44,    45,    42,     0,     0,    46,    47,
       0,     0,     0,    48,     0,     0,     0,    49,    50,     0,
       0,     0,     0,     0,    43,     0,     0,     0,     0,     0,
       0,    28,    44,    45,     0,     0,     0,    46,    47,     0,
      57,     0,    48,     0,     0,     0,    49,    50,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      28,    44,    45,     0,     0,     0,    46,    47,     0,   -48,
       0,    48,    29,    30,     0,    49,    50,     0,    31,    32,
      33,     0,    34,    35,    36,     0,     0,     0,     0,    28,
       0,   367,    37,    38,    39,    40,    41,     0,     0,     0,
      42,    29,    30,     0,     0,     0,     0,    31,    32,    33,
       0,    34,    35,    36,     0,     0,     0,     0,     0,    43,
     376,    37,    38,    39,    40,    41,     0,     0,     0,    42,
      29,    30,     0,     0,     0,     0,    31,    32,    33,     0,
      34,    35,    36,     0,     0,     0,     0,     0,    43,     0,
      37,    38,    39,    40,    41,     0,    44,    45,    42,     0,
       0,    46,    47,     0,   267,     0,    48,     0,     0,     0,
      49,    50,     0,     0,     0,     0,     0,    43,     0,     0,
       0,     0,     0,     0,    28,    44,    45,     0,     0,     0,
      46,    47,     0,     0,     0,    48,     0,     0,     0,    49,
      50,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    28,    44,    45,     0,     0,     0,    46,
      47,     0,     0,     0,    48,    29,    30,     0,    49,    50,
       0,    31,    32,    33,     0,    34,    35,    36,     0,     0,
       0,     0,    28,     0,     0,    37,    38,    39,    40,    41,
       0,     0,     0,    42,    29,    30,     0,     0,     0,     0,
      31,    32,    33,     0,    34,    35,    36,     0,     0,     0,
       0,     0,    43,   477,    37,    38,    39,    40,    41,     0,
       0,     0,    42,    29,    30,     0,     0,     0,     0,    31,
      32,    33,     0,    34,    35,    36,     0,     0,     0,     0,
       0,    43,   445,    37,    38,    39,    40,    41,     0,    44,
      45,    42,     0,     0,    46,    47,     0,     0,     0,    48,
     412,     0,     0,    49,    50,     0,     0,     0,     0,     0,
      43,     0,     0,     0,     0,     0,     0,    28,    44,    45,
       0,     0,     0,    46,    47,     0,     0,     0,    48,     0,
       0,     0,    49,    50,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    28,    44,    45,     0,
       0,     0,    46,    47,     0,     0,     0,    48,    29,    30,
       0,    49,    50,     0,    31,    32,    33,     0,    34,    35,
      36,     0,     0,     0,     0,     0,     0,     0,    37,    38,
      39,    40,    41,     0,     0,     0,    42,    29,    30,     0,
       0,     0,     0,    31,    32,    33,     0,    34,    35,    36,
     149,   150,   151,   152,     0,    43,     0,    37,    38,    39,
      40,    41,     0,     0,     0,    42,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    43,     0,     0,     0,     0,     0,
       0,     0,    44,    45,     0,     0,     0,    46,    47,     0,
       0,     0,    48,     0,     0,     0,    49,    50,     0,   154,
     155,   156,     0,   157,   158,   159,   160,   161,     0,     0,
       0,    44,    45,     0,     0,     0,    46,    47,     0,     0,
       0,   253,     0,     2,     3,    49,    50,   -40,   -40,   -40,
     -40,   -40,   -40,   -40,     0,     0,   -40,   -40,   -40,   -40,
     -40,   -40,   -40,   -40,   -40,   -40,   -40,   -40,   -40,   -40,
     -40,   -40,   163,   164,   165,   166,   167,   168,   169,   170,
     171,   172,   173,     0,     0,     0,     4,   -40,   -40,   -40,
     -40,     0,     0,     0,   -40,     0,     0,     0,   -40,   -40,
     -40,   -40,   -40,     0,     0,     5,     6,     7,     8,     9,
       0,     0,     0,     0,     0,   149,   150,   151,     0,    10,
      11,    12,    13,   -40,    14,     0,     0,   -40,    15,     0,
     -40,     0,     0,     0,    16,    17,    18,    19,    20,    21,
      22,   -40,   -40,   -40,   -40,   -40,   -40,   -40,   -40,   -40,
     -40,   -40,   -40,     0,   -40,   -40,   -40,   -40,    23,   149,
     150,   151,   152,   153,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   154,   155,   156,   -40,   157,   158,
     159,   160,   161,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   154,   155,
     156,     0,   157,   158,   159,   160,   161,   163,   164,   165,
     166,   167,   168,   169,   170,   171,   172,   173,     0,     0,
       0,     0,     0,    73,    74,    75,    76,    77,    78,    79,
       0,     0,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    95,     0,   162,
       0,   163,   164,   165,   166,   167,   168,   169,   170,   171,
     172,   173,     0,    96,    97,    98,    99,     0,     0,     0,
     100,     0,     0,   322,   101,   102,   103,   104,   105,   149,
     150,   151,   152,   153,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   106,
       0,     0,     0,   107,     0,     0,   108,   149,   150,   151,
     152,   153,     0,     0,     0,     0,     0,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,     0,
     121,   122,   123,   124,     0,     0,     0,     0,   154,   155,
     156,     0,   157,   158,   159,   160,   161,     0,     0,     0,
       0,     0,     0,   125,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   154,   155,   156,     0,
     157,   158,   159,   160,   161,     0,     0,     0,   149,   150,
     151,   152,   153,     0,     0,     0,     0,     0,     0,   162,
       0,   163,   164,   165,   166,   167,   168,   169,   170,   171,
     172,   173,   149,   150,   151,   152,   153,     0,   321,     0,
       0,     0,     0,     0,     0,     0,     0,   162,     0,   163,
     164,   165,   166,   167,   168,   169,   170,   171,   172,   173,
       0,     0,     0,     0,     0,     0,   473,   154,   155,   156,
       0,   157,   158,   159,   160,   161,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   154,   155,   156,     0,   157,   158,   159,   160,   161,
       0,   149,   150,   151,   152,   153,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   162,     0,
     163,   164,   165,   166,   167,   168,   169,   170,   171,   172,
     173,     0,     0,     0,   386,   387,   149,   150,   151,   152,
     153,     0,   162,     0,   163,   164,   165,   166,   167,   168,
     169,   170,   171,   172,   173,     0,     0,     0,     0,   362,
     154,   155,   156,     0,   157,   158,   159,   160,   161,     0,
     149,   150,   151,   152,   153,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   154,   155,   156,     0,   157,
     158,   159,   160,   161,     0,   149,   150,   151,   152,   153,
       0,   162,     0,   163,   164,   165,   166,   167,   168,   169,
     170,   171,   172,   173,     0,     0,     0,     0,   383,   154,
     155,   156,     0,   157,   158,   159,   160,   161,   149,   150,
     151,   152,   153,     0,     0,     0,   162,     0,   163,   164,
     165,   166,   167,   168,   169,   170,   171,   172,   173,     0,
       0,     0,     0,   400,   154,   155,   156,     0,   157,   158,
     159,   160,   161,   149,   150,   151,   152,   153,     0,     0,
     162,     0,   163,   164,   165,   166,   167,   168,   169,   170,
     171,   172,   173,     0,     0,     0,   174,   154,   155,   156,
       0,   157,   158,   159,   160,   161,   149,   150,   151,   152,
     153,     0,     0,     0,     0,   162,     0,   163,   164,   165,
     166,   167,   168,   169,   170,   171,   172,   173,     0,     0,
       0,   177,   154,   155,   156,     0,   157,   158,   159,   160,
     161,   149,   150,   151,   152,   153,     0,     0,   162,     0,
     163,   164,   165,   166,   167,   168,   169,   170,   171,   172,
     173,     0,     0,     0,   178,   154,   155,   156,     0,   157,
     158,   159,   160,   161,   149,   150,   151,   152,   153,     0,
       0,     0,     0,   162,     0,   163,   164,   165,   166,   167,
     168,   169,   170,   171,   172,   173,     0,     0,     0,   179,
     154,   155,   156,     0,   157,   158,   159,   160,   161,   149,
     150,   151,   152,   153,     0,     0,   162,     0,   163,   164,
     165,   166,   167,   168,   169,   170,   171,   172,   173,     0,
       0,     0,   379,   154,   155,   156,     0,   157,   158,   159,
     160,   161,   149,   150,   151,   152,   153,     0,     0,     0,
       0,   162,     0,   163,   164,   165,   166,   167,   168,   169,
     170,   171,   172,   173,     0,     0,     0,   398,   154,   155,
     156,     0,   157,   158,   159,   160,   161,   149,   150,   151,
     152,   153,     0,     0,   162,     0,   163,   164,   165,   166,
     167,   168,   169,   170,   171,   172,   173,     0,     0,     0,
     399,   154,   155,   156,     0,   157,   158,   159,   160,   161,
     149,   150,   151,   152,   153,     0,     0,     0,     0,   162,
       0,   163,   164,   165,   166,   167,   168,   169,   170,   171,
     172,   173,     0,     0,     0,   481,   154,   155,   156,     0,
     157,   158,   159,   160,   161,   149,   150,   151,   152,   153,
       0,     0,   162,     0,   163,   164,   165,   166,   167,   168,
     169,   170,   171,   172,   173,     0,     0,     0,   482,   154,
     155,   156,     0,   157,   158,   159,   160,   161,   149,     0,
     151,     0,     0,     0,     0,     0,     0,   162,     0,   163,
     164,   165,   166,   167,   168,   169,   170,   171,   172,   173,
       0,     0,     0,   485,   154,   155,   156,     0,   157,   158,
     159,   160,   161,     0,     0,     0,     0,     0,     0,     0,
     162,   404,   163,   164,   165,   166,   167,   168,   169,   170,
     171,   172,   173,     0,     0,     0,     0,   154,   155,   156,
       0,   157,   158,   159,   160,   161,   154,   155,   156,     0,
     157,   158,   159,   160,   161,   162,     0,   163,   164,   165,
     166,   167,   168,   169,   170,   171,   172,   173,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   164,   165,   166,   167,   168,   169,   170,   171,   172,
     173,     0,   166,   167,   168,   169,   170,   171,   172,   173
};

static const yytype_int16 yycheck[] =
{
      10,    75,    90,   118,    14,   120,    16,    17,    94,    90,
      91,    94,   175,    75,    76,    77,    78,    79,    92,    43,
      82,    31,    32,    33,    34,    35,    36,    88,    90,     1,
      33,    92,    42,    36,    44,    45,    46,    47,    67,    83,
      50,   134,    29,    93,    31,    32,    67,    19,   109,   110,
     111,    38,   134,   114,   373,   374,   117,   118,   119,   120,
      83,     8,   134,    73,    74,    75,    76,    77,    78,    79,
      80,    81,   122,    83,   393,    85,    86,   134,    88,    89,
      29,    91,    31,    32,    86,    95,    96,    97,    98,    99,
     100,   101,   102,   103,   104,   105,   134,   126,   127,   128,
     129,   130,   112,   113,   116,   115,   116,   128,   129,   130,
     119,   121,   122,   123,   124,   139,   134,   127,   128,   129,
      67,    68,    69,    32,    71,    72,    73,    74,    75,   110,
     353,    40,    41,   136,   357,    74,    75,    76,   119,   149,
     150,   151,   152,   153,   154,   155,   156,   157,   158,   159,
     160,   161,   162,   163,   164,   165,   166,   167,   168,   169,
     170,   171,   172,   173,    29,    30,    31,    32,    33,    34,
      35,   134,   135,    38,    39,   122,   123,   124,   125,   126,
     127,   128,   129,   130,    67,    68,    69,   136,    43,     1,
      60,    61,   134,   135,    29,    30,    31,    32,    33,    34,
      35,   134,    29,    38,    31,    32,   134,   135,   135,    33,
      34,   183,    82,    40,    38,    34,    35,    29,    30,    31,
      32,   135,    34,    35,   134,   135,   389,   135,    40,   135,
      42,    43,   134,   135,   308,   135,    48,    49,    50,   135,
      52,    53,    54,   126,   127,   128,   129,   130,   135,   119,
      62,    63,    64,    65,    66,    34,    35,    29,    70,    31,
      32,    33,    34,    35,    36,    31,   135,    33,    34,    35,
      38,   136,    38,    33,    34,    35,   362,    89,    38,   362,
     395,   135,   135,   357,    43,   135,   135,   375,   369,   135,
     352,   135,   373,   374,   356,   135,   358,   359,   360,   361,
     374,   136,   135,    62,    63,    64,    65,    66,    86,   135,
      29,   373,    31,    32,   126,   127,   134,   378,   135,   131,
     132,   135,   135,   134,   136,   134,   117,   135,   140,   141,
     135,   135,    86,   135,   395,   135,   135,   135,   110,   134,
     137,    34,   352,   139,   137,   355,   356,    37,   358,   359,
     360,   361,   137,   363,   136,   136,    31,   137,    31,    43,
     370,   137,   137,   373,   374,   135,   135,    32,   135,     1,
     134,   134,   134,   383,   384,   385,   134,   387,   388,   135,
     137,   134,   137,   393,   394,    31,   137,   397,   319,   181,
     400,   487,   462,   183,   404,    89,   384,    29,    30,    31,
      32,   475,    34,   388,   393,   385,   106,    -1,    40,    -1,
      42,    43,    -1,   474,     1,    -1,    48,    49,    50,   480,
      52,    53,    54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      62,    63,    64,    65,    66,    -1,    -1,    -1,    70,    -1,
      -1,    -1,    29,    30,    31,    32,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    40,    -1,    42,    43,    89,    -1,    -1,
      -1,    48,    49,    50,    -1,    52,    53,    54,    -1,    -1,
     480,    -1,    -1,    -1,    -1,    62,    63,    64,    65,    66,
      -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   126,   127,    -1,    -1,    -1,   131,
     132,    -1,    89,    -1,   136,    -1,    -1,    -1,   140,   141,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    67,    68,    69,     1,    71,    72,
      73,    74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   126,
     127,    -1,    -1,    -1,   131,   132,    -1,    -1,    -1,   136,
      -1,    -1,    -1,   140,   141,    29,    30,    31,    32,    -1,
      -1,    -1,    -1,    -1,    -1,    39,    -1,    -1,    42,    43,
      -1,     1,    -1,    -1,    48,    49,    50,    -1,    52,    53,
      54,   124,   125,   126,   127,   128,   129,   130,    62,    63,
      64,    65,    66,    -1,    -1,    -1,    70,    -1,    -1,    29,
      30,    31,    32,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    42,    43,    -1,    89,    -1,    -1,    48,    49,
      50,    -1,    52,    53,    54,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    62,    63,    64,    65,    66,    -1,    -1,    -1,
      70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   126,   127,    -1,    -1,    -1,   131,   132,    89,
      -1,    -1,   136,    -1,    -1,    -1,   140,   141,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   126,   127,    -1,    -1,
      -1,   131,   132,    -1,    -1,    -1,   136,    -1,    -1,    -1,
     140,   141,    29,    30,    31,    32,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    42,    43,    -1,     1,    -1,
      -1,    48,    49,    50,    -1,    52,    53,    54,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    62,    63,    64,    65,    66,
      -1,    -1,    -1,    70,    -1,    -1,    29,    30,    31,    32,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    42,
      43,    -1,    89,    -1,    -1,    48,    49,    50,    -1,    52,
      53,    54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    62,
      63,    64,    65,    66,    -1,    -1,    -1,    70,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   126,
     127,    -1,    -1,    -1,   131,   132,    89,    -1,    -1,   136,
      -1,    -1,    -1,   140,   141,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   126,   127,    -1,    -1,    -1,   131,   132,
      -1,    -1,    29,   136,    31,    32,    -1,   140,   141,    -1,
      -1,    -1,    -1,    -1,    -1,    42,    43,    -1,     1,    -1,
      -1,    48,    49,    50,    -1,    52,    53,    54,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    62,    63,    64,    65,    66,
      -1,    -1,    -1,    70,    -1,    -1,    29,    30,    31,    32,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    42,
      43,    -1,    89,    -1,    -1,    48,    49,    50,    -1,    52,
      53,    54,    -1,    -1,    -1,    -1,     1,    -1,    -1,    62,
      63,    64,    65,    66,    -1,    -1,    -1,    70,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   126,
     127,    -1,    -1,    -1,   131,   132,    89,    32,    -1,   136,
      -1,   138,    -1,   140,   141,    40,    41,    42,    43,    -1,
       1,    -1,    -1,    48,    49,    50,    -1,    52,    53,    54,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    62,    63,    64,
      65,    66,    -1,   126,   127,    70,    -1,    -1,   131,   132,
      -1,    32,    -1,   136,    -1,    -1,    -1,   140,   141,    40,
      41,    42,    43,    -1,    89,    -1,    -1,    48,    49,    50,
      -1,    52,    53,    54,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    62,    63,    64,    65,    66,    -1,    -1,    -1,    70,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   126,   127,    -1,    -1,    -1,   131,   132,    89,    -1,
      -1,   136,    -1,    -1,    -1,   140,   141,    -1,    -1,    -1,
       1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   126,   127,    -1,    -1,    -1,
     131,   132,    33,    34,    35,   136,    -1,    -1,    -1,   140,
     141,    42,    43,    -1,     1,    -1,    -1,    48,    49,    50,
      -1,    52,    53,    54,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    62,    63,    64,    65,    66,    -1,    -1,    -1,    70,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    34,    35,    -1,
      -1,    -1,    -1,    -1,    -1,    42,    43,    -1,    89,    -1,
      -1,    48,    49,    50,    -1,    52,    53,    54,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    62,    63,    64,    65,    66,
      -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   126,   127,    -1,    -1,    -1,
     131,   132,    89,    90,    -1,   136,    -1,    -1,    -1,   140,
     141,    -1,    -1,    -1,     1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   126,
     127,    -1,    -1,     1,   131,   132,    33,    34,    35,   136,
      -1,    -1,    -1,   140,   141,    42,    43,    -1,    -1,    -1,
      -1,    48,    49,    50,    -1,    52,    53,    54,    -1,    -1,
      -1,    29,    -1,    31,    32,    62,    63,    64,    65,    66,
      -1,    -1,    -1,    70,    42,    43,    -1,    -1,    -1,    -1,
      48,    49,    50,    -1,    52,    53,    54,    -1,    -1,    -1,
      -1,    -1,    89,    -1,    62,    63,    64,    65,    66,    -1,
      -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   126,
     127,    -1,    -1,    -1,   131,   132,     1,    -1,    -1,   136,
      -1,    -1,    -1,   140,   141,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   126,   127,
      -1,    -1,    -1,   131,   132,    -1,    -1,    32,   136,     1,
      -1,     3,   140,   141,    -1,    -1,    41,    42,    43,    -1,
      -1,    -1,    -1,    48,    49,    50,    -1,    52,    53,    54,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    62,    63,    64,
      65,    66,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,
      42,    43,    -1,    -1,    -1,    -1,    48,    49,    50,    -1,
      52,    53,    54,    -1,    89,    -1,    -1,    -1,    -1,    -1,
      62,    63,    64,    65,    66,    -1,    -1,    -1,    70,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    90,    -1,
      -1,   126,   127,    -1,    -1,    -1,   131,   132,    -1,    -1,
       1,   136,     3,    -1,    -1,   140,   141,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   126,   127,    -1,    -1,    -1,   131,
     132,    -1,    -1,     1,   136,    -1,    -1,    -1,   140,   141,
      -1,    42,    43,    -1,    -1,    -1,    -1,    48,    49,    50,
      -1,    52,    53,    54,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    62,    63,    64,    65,    66,    34,    35,    -1,    70,
      -1,    -1,    -1,    -1,    42,    43,    -1,    -1,    -1,    -1,
      48,    49,    50,    -1,    52,    53,    54,    -1,    89,    90,
      -1,    -1,    -1,    -1,    62,    63,    64,    65,    66,    -1,
      -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    -1,    -1,   126,   127,    -1,     1,    -1,
     131,   132,    -1,    -1,    -1,   136,    -1,    -1,    -1,   140,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,   126,   127,
      -1,    34,    35,   131,   132,    -1,    -1,    -1,   136,    42,
      43,    -1,   140,   141,    -1,    48,    49,    50,    -1,    52,
      53,    54,    -1,    -1,    -1,    -1,     1,    -1,    -1,    62,
      63,    64,    65,    66,    -1,    -1,    -1,    70,    42,    43,
      -1,    -1,    -1,    -1,    48,    49,    50,    -1,    52,    53,
      54,    -1,    -1,    -1,    -1,    -1,    89,    -1,    62,    63,
      64,    65,    66,    -1,    -1,    -1,    70,    42,    43,    -1,
      -1,    -1,    -1,    48,    49,    50,    -1,    52,    53,    54,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    62,    63,    64,
      65,    66,    -1,   126,   127,    70,    -1,    -1,   131,   132,
      -1,    -1,    -1,   136,    -1,    -1,    -1,   140,   141,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    -1,    -1,    -1,
      -1,     1,   126,   127,    -1,    -1,    -1,   131,   132,    -1,
     134,    -1,   136,    -1,    -1,    -1,   140,   141,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       1,   126,   127,    -1,    -1,    -1,   131,   132,    -1,   134,
      -1,   136,    42,    43,    -1,   140,   141,    -1,    48,    49,
      50,    -1,    52,    53,    54,    -1,    -1,    -1,    -1,     1,
      -1,    32,    62,    63,    64,    65,    66,    -1,    -1,    -1,
      70,    42,    43,    -1,    -1,    -1,    -1,    48,    49,    50,
      -1,    52,    53,    54,    -1,    -1,    -1,    -1,    -1,    89,
      32,    62,    63,    64,    65,    66,    -1,    -1,    -1,    70,
      42,    43,    -1,    -1,    -1,    -1,    48,    49,    50,    -1,
      52,    53,    54,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      62,    63,    64,    65,    66,    -1,   126,   127,    70,    -1,
      -1,   131,   132,    -1,   134,    -1,   136,    -1,    -1,    -1,
     140,   141,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,
      -1,    -1,    -1,    -1,     1,   126,   127,    -1,    -1,    -1,
     131,   132,    -1,    -1,    -1,   136,    -1,    -1,    -1,   140,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     1,   126,   127,    -1,    -1,    -1,   131,
     132,    -1,    -1,    -1,   136,    42,    43,    -1,   140,   141,
      -1,    48,    49,    50,    -1,    52,    53,    54,    -1,    -1,
      -1,    -1,     1,    -1,    -1,    62,    63,    64,    65,    66,
      -1,    -1,    -1,    70,    42,    43,    -1,    -1,    -1,    -1,
      48,    49,    50,    -1,    52,    53,    54,    -1,    -1,    -1,
      -1,    -1,    89,    32,    62,    63,    64,    65,    66,    -1,
      -1,    -1,    70,    42,    43,    -1,    -1,    -1,    -1,    48,
      49,    50,    -1,    52,    53,    54,    -1,    -1,    -1,    -1,
      -1,    89,    90,    62,    63,    64,    65,    66,    -1,   126,
     127,    70,    -1,    -1,   131,   132,    -1,    -1,    -1,   136,
     137,    -1,    -1,   140,   141,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    -1,    -1,    -1,    -1,     1,   126,   127,
      -1,    -1,    -1,   131,   132,    -1,    -1,    -1,   136,    -1,
      -1,    -1,   140,   141,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     1,   126,   127,    -1,
      -1,    -1,   131,   132,    -1,    -1,    -1,   136,    42,    43,
      -1,   140,   141,    -1,    48,    49,    50,    -1,    52,    53,
      54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    62,    63,
      64,    65,    66,    -1,    -1,    -1,    70,    42,    43,    -1,
      -1,    -1,    -1,    48,    49,    50,    -1,    52,    53,    54,
       8,     9,    10,    11,    -1,    89,    -1,    62,    63,    64,
      65,    66,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   126,   127,    -1,    -1,    -1,   131,   132,    -1,
      -1,    -1,   136,    -1,    -1,    -1,   140,   141,    -1,    67,
      68,    69,    -1,    71,    72,    73,    74,    75,    -1,    -1,
      -1,   126,   127,    -1,    -1,    -1,   131,   132,    -1,    -1,
      -1,   136,    -1,     0,     1,   140,   141,     4,     5,     6,
       7,     8,     9,    10,    -1,    -1,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,   120,   121,   122,   123,   124,   125,   126,   127,
     128,   129,   130,    -1,    -1,    -1,    43,    44,    45,    46,
      47,    -1,    -1,    -1,    51,    -1,    -1,    -1,    55,    56,
      57,    58,    59,    -1,    -1,    62,    63,    64,    65,    66,
      -1,    -1,    -1,    -1,    -1,     8,     9,    10,    -1,    76,
      77,    78,    79,    80,    81,    -1,    -1,    84,    85,    -1,
      87,    -1,    -1,    -1,    91,    92,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,   103,   104,   105,   106,
     107,   108,   109,    -1,   111,   112,   113,   114,   115,     8,
       9,    10,    11,    12,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    67,    68,    69,   134,    71,    72,
      73,    74,    75,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    67,    68,
      69,    -1,    71,    72,    73,    74,    75,   120,   121,   122,
     123,   124,   125,   126,   127,   128,   129,   130,    -1,    -1,
      -1,    -1,    -1,     4,     5,     6,     7,     8,     9,    10,
      -1,    -1,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    -1,   118,
      -1,   120,   121,   122,   123,   124,   125,   126,   127,   128,
     129,   130,    -1,    44,    45,    46,    47,    -1,    -1,    -1,
      51,    -1,    -1,   142,    55,    56,    57,    58,    59,     8,
       9,    10,    11,    12,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
      -1,    -1,    -1,    84,    -1,    -1,    87,     8,     9,    10,
      11,    12,    -1,    -1,    -1,    -1,    -1,    98,    99,   100,
     101,   102,   103,   104,   105,   106,   107,   108,   109,    -1,
     111,   112,   113,   114,    -1,    -1,    -1,    -1,    67,    68,
      69,    -1,    71,    72,    73,    74,    75,    -1,    -1,    -1,
      -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    67,    68,    69,    -1,
      71,    72,    73,    74,    75,    -1,    -1,    -1,     8,     9,
      10,    11,    12,    -1,    -1,    -1,    -1,    -1,    -1,   118,
      -1,   120,   121,   122,   123,   124,   125,   126,   127,   128,
     129,   130,     8,     9,    10,    11,    12,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   118,    -1,   120,
     121,   122,   123,   124,   125,   126,   127,   128,   129,   130,
      -1,    -1,    -1,    -1,    -1,    -1,   137,    67,    68,    69,
      -1,    71,    72,    73,    74,    75,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    67,    68,    69,    -1,    71,    72,    73,    74,    75,
      -1,     8,     9,    10,    11,    12,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   118,    -1,
     120,   121,   122,   123,   124,   125,   126,   127,   128,   129,
     130,    -1,    -1,    -1,   134,   135,     8,     9,    10,    11,
      12,    -1,   118,    -1,   120,   121,   122,   123,   124,   125,
     126,   127,   128,   129,   130,    -1,    -1,    -1,    -1,   135,
      67,    68,    69,    -1,    71,    72,    73,    74,    75,    -1,
       8,     9,    10,    11,    12,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    67,    68,    69,    -1,    71,
      72,    73,    74,    75,    -1,     8,     9,    10,    11,    12,
      -1,   118,    -1,   120,   121,   122,   123,   124,   125,   126,
     127,   128,   129,   130,    -1,    -1,    -1,    -1,   135,    67,
      68,    69,    -1,    71,    72,    73,    74,    75,     8,     9,
      10,    11,    12,    -1,    -1,    -1,   118,    -1,   120,   121,
     122,   123,   124,   125,   126,   127,   128,   129,   130,    -1,
      -1,    -1,    -1,   135,    67,    68,    69,    -1,    71,    72,
      73,    74,    75,     8,     9,    10,    11,    12,    -1,    -1,
     118,    -1,   120,   121,   122,   123,   124,   125,   126,   127,
     128,   129,   130,    -1,    -1,    -1,   134,    67,    68,    69,
      -1,    71,    72,    73,    74,    75,     8,     9,    10,    11,
      12,    -1,    -1,    -1,    -1,   118,    -1,   120,   121,   122,
     123,   124,   125,   126,   127,   128,   129,   130,    -1,    -1,
      -1,   134,    67,    68,    69,    -1,    71,    72,    73,    74,
      75,     8,     9,    10,    11,    12,    -1,    -1,   118,    -1,
     120,   121,   122,   123,   124,   125,   126,   127,   128,   129,
     130,    -1,    -1,    -1,   134,    67,    68,    69,    -1,    71,
      72,    73,    74,    75,     8,     9,    10,    11,    12,    -1,
      -1,    -1,    -1,   118,    -1,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,    -1,    -1,    -1,   134,
      67,    68,    69,    -1,    71,    72,    73,    74,    75,     8,
       9,    10,    11,    12,    -1,    -1,   118,    -1,   120,   121,
     122,   123,   124,   125,   126,   127,   128,   129,   130,    -1,
      -1,    -1,   134,    67,    68,    69,    -1,    71,    72,    73,
      74,    75,     8,     9,    10,    11,    12,    -1,    -1,    -1,
      -1,   118,    -1,   120,   121,   122,   123,   124,   125,   126,
     127,   128,   129,   130,    -1,    -1,    -1,   134,    67,    68,
      69,    -1,    71,    72,    73,    74,    75,     8,     9,    10,
      11,    12,    -1,    -1,   118,    -1,   120,   121,   122,   123,
     124,   125,   126,   127,   128,   129,   130,    -1,    -1,    -1,
     134,    67,    68,    69,    -1,    71,    72,    73,    74,    75,
       8,     9,    10,    11,    12,    -1,    -1,    -1,    -1,   118,
      -1,   120,   121,   122,   123,   124,   125,   126,   127,   128,
     129,   130,    -1,    -1,    -1,   134,    67,    68,    69,    -1,
      71,    72,    73,    74,    75,     8,     9,    10,    11,    12,
      -1,    -1,   118,    -1,   120,   121,   122,   123,   124,   125,
     126,   127,   128,   129,   130,    -1,    -1,    -1,   134,    67,
      68,    69,    -1,    71,    72,    73,    74,    75,     8,    -1,
      10,    -1,    -1,    -1,    -1,    -1,    -1,   118,    -1,   120,
     121,   122,   123,   124,   125,   126,   127,   128,   129,   130,
      -1,    -1,    -1,   134,    67,    68,    69,    -1,    71,    72,
      73,    74,    75,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
     128,   129,   130,    -1,    -1,    -1,    -1,    67,    68,    69,
      -1,    71,    72,    73,    74,    75,    67,    68,    69,    -1,
      71,    72,    73,    74,    75,   118,    -1,   120,   121,   122,
     123,   124,   125,   126,   127,   128,   129,   130,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   121,   122,   123,   124,   125,   126,   127,   128,   129,
     130,    -1,   123,   124,   125,   126,   127,   128,   129,   130
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,   144,     0,     1,    43,    62,    63,    64,    65,    66,
      76,    77,    78,    79,    81,    85,    91,    92,    93,    94,
      95,    96,    97,   115,   145,   147,   188,    83,     1,    42,
      43,    48,    49,    50,    52,    53,    54,    62,    63,    64,
      65,    66,    70,    89,   126,   127,   131,   132,   136,   140,
     141,   185,   186,   187,   134,   134,   190,   134,   185,    83,
     185,   185,    43,   148,   149,   188,   150,   151,   188,   134,
     134,   134,   192,     4,     5,     6,     7,     8,     9,    10,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    44,    45,    46,    47,
      51,    55,    56,    57,    58,    59,    80,    84,    87,    98,
      99,   100,   101,   102,   103,   104,   105,   106,   107,   108,
     109,   111,   112,   113,   114,   134,   152,    60,    61,    82,
     119,   146,   139,   153,   154,   155,   185,   185,   185,   185,
     185,   185,   185,   185,   185,   185,   185,   185,   185,     8,
       9,    10,    11,    12,    67,    68,    69,    71,    72,    73,
      74,    75,   118,   120,   121,   122,   123,   124,   125,   126,
     127,   128,   129,   130,   134,    86,   191,   134,   134,   134,
     134,   135,   134,   135,   116,   185,    29,    30,    31,    32,
      34,    40,   136,   161,   162,   163,   165,   166,   167,   168,
     185,    31,    34,    35,   161,   165,   175,   185,    31,   161,
     165,   185,    31,   161,   185,    31,   161,   185,    31,   161,
     185,   185,    32,    40,    41,   176,   177,   185,    31,    33,
      38,   136,   161,   169,   173,   175,   185,    33,    36,   136,
     185,   185,   136,   138,   166,   185,   177,   185,    39,   136,
     161,   169,   186,   136,   185,   186,    33,    36,   110,   166,
     171,   172,   175,   176,   162,   168,   185,   134,   185,   185,
     185,   185,   185,     3,    89,    90,   185,     3,    90,   178,
     179,   185,   182,   183,   185,   185,   180,   181,   185,   190,
     189,    38,   166,   170,   170,   166,   185,   185,   166,   185,
     185,   164,   165,   166,   164,   170,   164,   185,   136,   176,
     185,   185,   185,   134,   185,   185,   185,   119,   134,   135,
      43,   137,   142,   185,   185,   185,   185,   185,   185,   185,
     185,   185,   185,   185,   185,   185,   185,   185,   185,   185,
     185,   185,   185,   185,   185,   185,   185,   185,   191,   134,
     149,   151,   135,   135,    34,    35,   135,   135,   135,   135,
     135,   135,   135,   135,   135,   135,    38,    32,   135,   135,
     135,   135,    33,   135,   135,   135,    32,   135,   135,   134,
     134,   134,   134,   135,   135,   135,   134,   135,   135,    86,
      86,   159,   160,   135,   135,   135,   175,   135,   134,   134,
     135,   117,   156,   155,   119,   134,   161,   185,    34,   173,
     174,   137,   137,   185,   161,   185,   173,   175,   161,   185,
     161,   185,   161,   185,   161,   185,   162,   168,   185,    34,
      37,   137,   137,   136,   136,   186,   185,    31,   137,    39,
     136,   161,   185,   186,   187,    90,   175,   184,   186,   187,
      31,   169,   137,    31,   166,   185,   179,   183,   185,   181,
     191,   134,   135,   184,   185,   164,   137,   185,   185,    43,
     157,   158,   185,   137,   135,   135,    32,    32,   135,    33,
     135,   134,   134,   134,   160,   134,   134,   135,   166,   175,
     137,   137,    31,   137,   166,   185,   158
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,   143,   144,   144,   145,   145,   145,   145,   145,   145,
     145,   145,   145,   145,   145,   145,   145,   145,   145,   145,
     145,   145,   145,   145,   145,   145,   145,   145,   145,   145,
     145,   145,   145,   145,   145,   145,   145,   146,   146,   146,
     147,   147,   148,   148,   149,   150,   150,   151,   152,   152,
     152,   152,   152,   152,   152,   152,   152,   152,   152,   152,
     152,   152,   152,   152,   152,   152,   152,   152,   152,   152,
     152,   152,   152,   152,   152,   152,   152,   152,   152,   152,
     152,   152,   152,   152,   152,   152,   152,   152,   152,   152,
     152,   152,   152,   152,   152,   152,   152,   152,   152,   152,
     152,   152,   152,   152,   152,   152,   152,   152,   152,   152,
     152,   152,   152,   152,   152,   152,   152,   152,   152,   152,
     152,   152,   152,   152,   152,   152,   152,   152,   152,   152,
     152,   152,   152,   152,   152,   152,   152,   152,   152,   152,
     152,   152,   152,   152,   152,   152,   152,   152,   152,   152,
     153,   153,   153,   154,   154,   155,   156,   156,   157,   157,
     157,   158,   159,   159,   159,   160,   161,   161,   162,   162,
     163,   164,   164,   165,   166,   166,   166,   167,   167,   168,
     168,   169,   169,   170,   170,   171,   171,   171,   172,   172,
     173,   173,   174,   174,   175,   175,   176,   176,   177,   177,
     178,   178,   179,   179,   179,   180,   180,   181,   182,   182,
     183,   184,   184,   185,   185,   186,   187,   187,   187,   187,
     187,   187,   187,   187,   187,   187,   187,   187,   187,   187,
     187,   187,   187,   187,   187,   187,   187,   187,   187,   187,
     187,   187,   187,   187,   187,   187,   187,   187,   187,   187,
     187,   187,   187,   187,   187,   187,   187,   187,   187,   187,
     187,   187,   187,   188,   188,   188,   188,   188,   188,   189,
     190,   191,   192
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     2,     2,     3,     4,     4,     6,     3,
       2,     2,     3,     4,     4,     6,     4,     4,     4,     6,
       5,     4,     6,     3,     2,     3,     3,     3,     3,     3,
       2,     2,     2,     5,     2,     5,     1,     0,     1,     2,
       0,     2,     1,     3,     1,     1,     3,     1,     1,     2,
       1,     2,     2,     2,     2,     2,     4,     2,     4,     2,
       2,     2,     2,     4,     4,     4,     4,     2,     4,     2,
       2,     4,     2,     2,     2,     2,     2,     2,     4,     4,
       4,     4,     2,     4,     2,     2,     4,     4,     4,     2,
       2,     2,     2,     2,     4,     6,     4,     2,     4,     4,
       2,     2,     4,     2,     1,     2,     4,     4,     4,     4,
       6,     2,     4,     2,     6,     2,     4,     2,     4,     4,
       4,     4,     4,     2,     4,     2,     4,     4,     4,     6,
       4,     2,     6,     6,     4,     4,     2,     6,     6,     2,
       2,     1,     2,     2,     2,     2,     2,     2,     2,     1,
       0,     1,     3,     0,     1,     2,     0,     3,     0,     1,
       3,     1,     0,     1,     3,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     3,     1,     4,
       3,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     3,     1,     1,     1,     1,     3,     1,     1,     3,
       1,     1,     1,     1,     1,     3,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       5,     3,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     1,     1,     1,     1,     1,     1,     0,
       0,     0,     0
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yystacksize);

        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 4:
#line 1564 "zmac.y" /* yacc.c:1646  */
    { 
		// An identfier without a colon all by itself on a line
		// will be interpreted as a label.  But there's a very
		// good chance it is a misspelling of an instruction or
		// pseudo-op name creating silent errors.  Since the condition
		// is unusual we print a warning.  Unless it is followed by
		// a colon in which case there's no ambiguity.
		if ((yyvsp[-1].itemptr) && !firstcol && coloncnt == 0 && outpass) {
			fprintf(stderr, "%s(%d): warning: '%s' treated as label (instruction typo?)\n",
				src_name[now_in], linein[now_in], (yyvsp[-1].itemptr)->i_string);
			fprintf(stderr, "\tAdd a colon or move to first column to stop this warning.\n");
		}

		if ((yyvsp[-1].itemptr)) list(dollarsign);
		else  list1();
	}
#line 3696 "zmac.c" /* yacc.c:1646  */
    break;

  case 5:
#line 1581 "zmac.y" /* yacc.c:1646  */
    {
		list(dollarsign);
	}
#line 3704 "zmac.c" /* yacc.c:1646  */
    break;

  case 6:
#line 1585 "zmac.y" /* yacc.c:1646  */
    {
		expr_reloc_check((yyvsp[-1].exprptr));
		switch((yyvsp[-3].itemptr)->i_token) {
		case UNDECLARED: case WASEQUATED:
			if ((yyvsp[-3].itemptr)->i_token == WASEQUATED &&
				((yyvsp[-3].itemptr)->i_value != (yyvsp[-1].exprptr)->e_value ||
				 (((yyvsp[-3].itemptr)->i_scope ^ (yyvsp[-1].exprptr)->e_scope) & SCOPE_SEGMASK)))
			{
				if (outpass)
					err[pflag]++;
				else
					passretry = 1;
			}

			(yyvsp[-3].itemptr)->i_token = EQUATED;
			(yyvsp[-3].itemptr)->i_value = (yyvsp[-1].exprptr)->e_value;
			(yyvsp[-3].itemptr)->i_scope |= (yyvsp[-1].exprptr)->e_scope;
			break;
		default:
			// m80 allows multiple equates as long as the value
			// does not change.  So does newer zmac.
			if ((yyvsp[-3].itemptr)->i_value != (yyvsp[-1].exprptr)->e_value ||
				(((yyvsp[-3].itemptr)->i_scope ^ (yyvsp[-1].exprptr)->e_scope) & SCOPE_SEGMASK))
			{
				err[mflag]++;
				(yyvsp[-3].itemptr)->i_token = MULTDEF;
			}
		}

		list((yyvsp[-1].exprptr)->e_value);
		expr_free((yyvsp[-1].exprptr));
	}
#line 3741 "zmac.c" /* yacc.c:1646  */
    break;

  case 7:
#line 1618 "zmac.y" /* yacc.c:1646  */
    {
		expr_reloc_check((yyvsp[-1].exprptr));
		switch((yyvsp[-3].itemptr)->i_token) {
		case UNDECLARED: case DEFLED:
			(yyvsp[-3].itemptr)->i_token = DEFLED;
			(yyvsp[-3].itemptr)->i_value = (yyvsp[-1].exprptr)->e_value;
			(yyvsp[-3].itemptr)->i_scope = ((yyvsp[-3].itemptr)->i_scope & SCOPE_SEGMASK) | (yyvsp[-1].exprptr)->e_scope;
			break;
		default:
			err[mflag]++;
			(yyvsp[-3].itemptr)->i_token = MULTDEF;
		}
		list((yyvsp[-1].exprptr)->e_value);
		expr_free((yyvsp[-1].exprptr));
	}
#line 3761 "zmac.c" /* yacc.c:1646  */
    break;

  case 8:
#line 1634 "zmac.y" /* yacc.c:1646  */
    {
		int val3 = (yyvsp[-3].exprptr)->e_value;
		int val5 = (yyvsp[-1].exprptr)->e_value;
		expr_reloc_check((yyvsp[-3].exprptr));
		expr_reloc_check((yyvsp[-1].exprptr));
		expr_scope_same((yyvsp[-3].exprptr), (yyvsp[-1].exprptr));
		switch ((yyvsp[-5].itemptr)->i_token) {
		case UNDECLARED: case DEFLED:
			(yyvsp[-5].itemptr)->i_token = DEFLED;
			(yyvsp[-5].itemptr)->i_scope |= (yyvsp[-3].exprptr)->e_scope;
			if ((yyvsp[-4].itemptr)->i_value)	/* max */
				list((yyvsp[-5].itemptr)->i_value = (val3 > val5? val3:val5));
			else list((yyvsp[-5].itemptr)->i_value = (val3 < val5? val3:val5));
			break;
		default:
			err[mflag]++;
			(yyvsp[-5].itemptr)->i_token = MULTDEF;
			list((yyvsp[-5].itemptr)->i_value);
		}
		expr_free((yyvsp[-3].exprptr));
		expr_free((yyvsp[-1].exprptr));
	}
#line 3788 "zmac.c" /* yacc.c:1646  */
    break;

  case 9:
#line 1657 "zmac.y" /* yacc.c:1646  */
    {
		expr_number_check((yyvsp[-1].exprptr));
		if (ifptr >= ifstmax)
			error("Too many ifs");
		else {
			if (pass2) {
				*++ifptr = *expifp++;
				if (*ifptr != !((yyvsp[-1].exprptr)->e_value)) err[pflag]++;
			} else {
				if (expifp >= expifmax)
					error("Too many ifs!");
				*expifp++ = !((yyvsp[-1].exprptr)->e_value);
				*++ifptr = !((yyvsp[-1].exprptr)->e_value);
			}
		}
		saveopt = fopt;
		fopt = 1;
		list((yyvsp[-1].exprptr)->e_value);
		fopt = saveopt;
		expr_free((yyvsp[-1].exprptr));
	}
#line 3814 "zmac.c" /* yacc.c:1646  */
    break;

  case 10:
#line 1679 "zmac.y" /* yacc.c:1646  */
    {
		/* FIXME: it would be nice to spot repeated ELSEs, but how? */
		*ifptr = !*ifptr;
		saveopt = fopt;
		fopt = 1;
		list1();
		fopt = saveopt;
	}
#line 3827 "zmac.c" /* yacc.c:1646  */
    break;

  case 11:
#line 1688 "zmac.y" /* yacc.c:1646  */
    {
		if (ifptr == ifstack) err[bflag]++;
		else --ifptr;
		list1();
	}
#line 3837 "zmac.c" /* yacc.c:1646  */
    break;

  case 12:
#line 1694 "zmac.y" /* yacc.c:1646  */
    {
		list(dollarsign);
		peekc = 0;
	}
#line 3846 "zmac.c" /* yacc.c:1646  */
    break;

  case 13:
#line 1699 "zmac.y" /* yacc.c:1646  */
    {
		expr_reloc_check((yyvsp[-1].exprptr));
		xeq_flag++;
		xeq = (yyvsp[-1].exprptr)->e_value & 0xffff;
		list((yyvsp[-1].exprptr)->e_value);
		peekc = 0;
		rel_main = (((yyvsp[-1].exprptr)->e_scope & SCOPE_SEGMASK) << 16) | xeq;
		expr_free((yyvsp[-1].exprptr));
	}
#line 3860 "zmac.c" /* yacc.c:1646  */
    break;

  case 14:
#line 1709 "zmac.y" /* yacc.c:1646  */
    {
		expr_number_check((yyvsp[-1].exprptr));
		if ((yyvsp[-1].exprptr)->e_value < 0) err[vflag]++;
		if ((yyvsp[-1].exprptr)->e_value > 0) {
			if (!phaseflag) {
				list(dollarsign);
				flushbin();
				flushoth();
				dollarsign += (yyvsp[-1].exprptr)->e_value;
				olddollar += (yyvsp[-1].exprptr)->e_value;
				oldothdollar += (yyvsp[-1].exprptr)->e_value;
				emit_addr += (yyvsp[-1].exprptr)->e_value;
				advance_segment((yyvsp[-1].exprptr)->e_value);
				putrelcmd(RELCMD_SETLOC);
				putrelsegref(segment, seg_pos[segment]);
			}
			else
				dc((yyvsp[-1].exprptr)->e_value, 0);
		}
		else
			list1();

		expr_free((yyvsp[-1].exprptr));
	}
#line 3889 "zmac.c" /* yacc.c:1646  */
    break;

  case 15:
#line 1734 "zmac.y" /* yacc.c:1646  */
    {
		expr_number_check((yyvsp[-3].exprptr));
		expr_number_check((yyvsp[-1].exprptr));
		if ((yyvsp[-3].exprptr)->e_value < 0) err[vflag]++;
		if ((yyvsp[-1].exprptr)->e_value < -128 || (yyvsp[-1].exprptr)->e_value > 127) err[vflag]++;
		if ((yyvsp[-3].exprptr)->e_value > 0) {
			dc((yyvsp[-3].exprptr)->e_value, (yyvsp[-1].exprptr)->e_value);
		}
		else
			list1();

		expr_free((yyvsp[-3].exprptr));
		expr_free((yyvsp[-1].exprptr));
	}
#line 3908 "zmac.c" /* yacc.c:1646  */
    break;

  case 16:
#line 1749 "zmac.y" /* yacc.c:1646  */
    { emit(1, E_DATA, expr_num((yyvsp[-1].ival) | 0x80)); list(dollarsign); }
#line 3914 "zmac.c" /* yacc.c:1646  */
    break;

  case 17:
#line 1751 "zmac.y" /* yacc.c:1646  */
    { emit(1, E_DATA, expr_num((yyvsp[-1].ival))); emit(1, E_DATA, expr_num(((yyvsp[-1].ival) >> 8) | 0x80)); list(dollarsign); }
#line 3920 "zmac.c" /* yacc.c:1646  */
    break;

  case 18:
#line 1754 "zmac.y" /* yacc.c:1646  */
    {
			for (cp = (yyvsp[-1].cval); *cp != '\0'; cp++)
				if (!cp[1])
					emit(1, E_DATA, expr_num(*cp | 0x80));
				else
					emit(1, E_DATA, expr_num(*cp));

			list(dollarsign);
		}
#line 3934 "zmac.c" /* yacc.c:1646  */
    break;

  case 19:
#line 1765 "zmac.y" /* yacc.c:1646  */
    {
			expr_number_check((yyvsp[-3].exprptr));
			expr_number_check((yyvsp[-1].exprptr));
			dc((yyvsp[-3].exprptr)->e_value, (yyvsp[-1].exprptr)->e_value);
			expr_free((yyvsp[-3].exprptr));
			expr_free((yyvsp[-1].exprptr));
		}
#line 3946 "zmac.c" /* yacc.c:1646  */
    break;

  case 20:
#line 1773 "zmac.y" /* yacc.c:1646  */
    {
		list1();
		switch ((yyvsp[-4].itemptr)->i_value) {

		case PSTITL:	/* title */
			lineptr = linebuf;
			cp = tempbuf;
			title = titlespace;
			while ((*title++ = *cp++) && (title < &titlespace[TITLELEN]));
			*title = 0;
			title = titlespace;
			break;

		case PSRSYM:	/* rsym */
			if (pass2) break;
			insymtab(tempbuf);
			break;

		case PSWSYM:	/* wsym */
			writesyms = malloc(strlen(tempbuf)+1);
			strcpy(writesyms, tempbuf);
			break;
		case PSINC:	/* include file */
			next_source(tempbuf) ;
			break ;
		}
	}
#line 3978 "zmac.c" /* yacc.c:1646  */
    break;

  case 21:
#line 1801 "zmac.y" /* yacc.c:1646  */
    {
		fprintf(stderr, "Missing argument of '%s'\n", (yyvsp[-3].itemptr)->i_string);
		err[fflag]++;
		list(dollarsign);
	}
#line 3988 "zmac.c" /* yacc.c:1646  */
    break;

  case 22:
#line 1807 "zmac.y" /* yacc.c:1646  */
    {
		incbin(tempbuf);
	}
#line 3996 "zmac.c" /* yacc.c:1646  */
    break;

  case 23:
#line 1811 "zmac.y" /* yacc.c:1646  */
    {
		int quote = 0;
		char *p, *q;
		switch ((yyvsp[-2].itemptr)->i_value) {
		case SPTITL:
			cp = tempbuf;
			title = titlespace;
			if (*cp == '\'' || *cp == '"')
				quote = *cp++;
			while ((*title++ = *cp++) && (title < &titlespace[TITLELEN]));
			if (quote && title > titlespace + 1 && title[-2] == quote)
				title[-2] = '\0';
			title = titlespace;
			list1();
			break;
		case SPSBTL:
			err[warn_notimpl]++;
			list1();
			break;
		case SPNAME:
			// Drop surrounding ('') if present
			p = tempbuf;
			q = strchr(tempbuf, '\0') - 1;
			if (*p == '(' && *q == ')' && q > p) p++, q--;
			if (*p == '\'' && *q == '\'' && q > p) p++, q--;
			q[1] = '\0';
			strncpy(progname, p, sizeof progname);
			progname[sizeof progname - 1] = '\0';
			list1();
			break;
		case SPCOM:
			quote = *tempbuf;
			list1();
			for (;;) {
				raw = 1;
				yychar = yylex();
				list1();
				if (yychar == 0)
					break;
				if (*tempbuf == quote) {
					yychar = yylex();
					break;
				}
			}
			break;
		}
	}
#line 4048 "zmac.c" /* yacc.c:1646  */
    break;

  case 24:
#line 1859 "zmac.y" /* yacc.c:1646  */
    {
		goto dolopt; }
#line 4055 "zmac.c" /* yacc.c:1646  */
    break;

  case 25:
#line 1862 "zmac.y" /* yacc.c:1646  */
    {
		int enable = (yyvsp[-1].exprptr)->e_value;
		expr_number_check((yyvsp[-1].exprptr));
		expr_free((yyvsp[-1].exprptr));
		goto doloptA;
	dolopt:
		enable = 1;
	doloptA:
		linecnt++;
		if (outpass) {
			lineptr = linebuf;
			switch ((yyvsp[-2].itemptr)->i_value) {
			case 0:	/* list */
				if (enable < 0) lstoff = 1;
				if (enable > 0) lstoff = 0;
				break;

			case 1:	/* eject */
				if (enable) eject();
				break;

			case 2:	/* space */
				if ((line + enable) > 60) eject();
				else space(enable);
				break;

			case 3:	/* elist */
				eopt = edef;
				if (enable < 0) eopt = 0;
				if (enable > 0) eopt = 1;
				break;

			case 4:	/* fopt */
				fopt = fdef;
				if (enable < 0) fopt = 0;
				if (enable > 0) fopt = 1;
				break;

			case 5:	/* gopt */
				gopt = gdef;
				if (enable < 0) gopt = 1;
				if (enable > 0) gopt = 0;
				break;

			case 6: /* mopt */
				mopt = mdef;
				if (enable < 0) mopt = 0;
				if (enable > 0) mopt = 1;
			}
		}
	}
#line 4111 "zmac.c" /* yacc.c:1646  */
    break;

  case 26:
#line 1914 "zmac.y" /* yacc.c:1646  */
    {
		expr_number_check((yyvsp[-1].exprptr));
		jopt = !!(yyvsp[-1].exprptr)->e_value;
		list1();
		expr_free((yyvsp[-1].exprptr));
	}
#line 4122 "zmac.c" /* yacc.c:1646  */
    break;

  case 27:
#line 1921 "zmac.y" /* yacc.c:1646  */
    {
		expr_number_check((yyvsp[-1].exprptr));
		JPopt = !!(yyvsp[-1].exprptr)->e_value;
		list1();
		expr_free((yyvsp[-1].exprptr));
	}
#line 4133 "zmac.c" /* yacc.c:1646  */
    break;

  case 28:
#line 1928 "zmac.y" /* yacc.c:1646  */
    {
		list1();
	}
#line 4141 "zmac.c" /* yacc.c:1646  */
    break;

  case 29:
#line 1932 "zmac.y" /* yacc.c:1646  */
    {
		list1();
	}
#line 4149 "zmac.c" /* yacc.c:1646  */
    break;

  case 30:
#line 1936 "zmac.y" /* yacc.c:1646  */
    {
		char *p = strchr(modstr, '\0') - 1;
		for (; p >= modstr; p--) {
			(*p)++;
			if (*p < 'Z')
				break;
			*p = 'A';
		}
		list1();
	}
#line 4164 "zmac.c" /* yacc.c:1646  */
    break;

  case 31:
#line 1947 "zmac.y" /* yacc.c:1646  */
    {
		if (relopt && segment != (yyvsp[-1].itemptr)->i_value) {
			segment = (yyvsp[-1].itemptr)->i_value;
			segchange = 1;
			dollarsign = seg_pos[(yyvsp[-1].itemptr)->i_value];
		}
		list1();
	}
#line 4177 "zmac.c" /* yacc.c:1646  */
    break;

  case 32:
#line 1956 "zmac.y" /* yacc.c:1646  */
    {
		z80 = (yyvsp[-1].itemptr)->i_value;
		list1();
	}
#line 4186 "zmac.c" /* yacc.c:1646  */
    break;

  case 33:
#line 1961 "zmac.y" /* yacc.c:1646  */
    {
		(yyvsp[-4].itemptr)->i_token = MNAME;
		(yyvsp[-4].itemptr)->i_value = mfptr;
#ifdef M_DEBUG
		fprintf (stderr, "[UNDECLARED MACRO %s]\n", (yyvsp[-4].itemptr)->i_string);
#endif
		mfseek(mfile, (long)mfptr, 0);
		cp = 0;

		// Because of locals the parser has to look ahead.
		// We'll have buffered that as we usually do so just a
		// matter of picking that up and cancelling any look-ahead.
		*lineptr = '\0';
		cp = strchr(linebuf, '\n');
		if (cp) {
			cp++;
			peekc = -1;
		}
		yychar = YYEMPTY;

		list1();
		mlex(cp);
		parm_number = 0;
	}
#line 4215 "zmac.c" /* yacc.c:1646  */
    break;

  case 34:
#line 1986 "zmac.y" /* yacc.c:1646  */
    {
		(yyvsp[-1].itemptr)->i_token = MNAME;
#ifdef M_DEBUG
		fprintf (stderr, "[OLDNAME MACRO %s]\n", (yyvsp[-1].itemptr)->i_string);
#endif
		while (yychar != ENDM && yychar) {
			while (yychar != '\n' && yychar)
				yychar = yylex();
			list1();
			yychar = yylex();
		}
		while (yychar != '\n' && yychar) yychar = yylex();
		list1();
		yychar = yylex();
	}
#line 4235 "zmac.c" /* yacc.c:1646  */
    break;

  case 35:
#line 2002 "zmac.y" /* yacc.c:1646  */
    {
#ifdef M_DEBUG
		fprintf (stderr, "[MNAME %s]\n", (yyvsp[-3].itemptr)->i_string);
#endif
		(yyvsp[-3].itemptr)->i_uses++ ;
		arg_flag = 0;
		parm_number = 0;
		list(dollarsign);
		expptr++;
		est = est2;
		est2 = NULL; // GWP - this may leak, but it avoids double-free crashes
		est[FLOC].value = floc;
		est[TEMPNUM].value = exp_number++;
		floc = (yyvsp[-3].itemptr)->i_value;
		mfseek(mfile, (long)floc, 0);
	}
#line 4256 "zmac.c" /* yacc.c:1646  */
    break;

  case 36:
#line 2019 "zmac.y" /* yacc.c:1646  */
    {
		err[fflag]++;
		arg_flag = 0;
		parm_number = 0;

		if (est2)
		{
			int i;
			for (i=0; i<PARMMAX; i++) {
				if (est2[i].param) {
#ifdef M_DEBUG
	fprintf (stderr, "[Freeing2 arg%u(%p)]\n", i, est2[i].param),
#endif
					free(est2[i].param);
				}
			}
			free(est2);
			est2 = NULL;
		}

		while(yychar != '\n' && yychar != '\0') yychar = yylex();
		list(dollarsign);
		yyclearin;yyerrok;
	}
#line 4285 "zmac.c" /* yacc.c:1646  */
    break;

  case 37:
#line 2046 "zmac.y" /* yacc.c:1646  */
    { (yyval.ival) = 0; }
#line 4291 "zmac.c" /* yacc.c:1646  */
    break;

  case 38:
#line 2048 "zmac.y" /* yacc.c:1646  */
    { (yyval.ival) = 1; }
#line 4297 "zmac.c" /* yacc.c:1646  */
    break;

  case 39:
#line 2050 "zmac.y" /* yacc.c:1646  */
    { (yyval.ival) = 2; }
#line 4303 "zmac.c" /* yacc.c:1646  */
    break;

  case 40:
#line 2055 "zmac.y" /* yacc.c:1646  */
    {	(yyval.itemptr) = NULL;	}
#line 4309 "zmac.c" /* yacc.c:1646  */
    break;

  case 41:
#line 2057 "zmac.y" /* yacc.c:1646  */
    {
		coloncnt = (yyvsp[0].ival);
		(yyvsp[-1].itemptr)->i_scope |= segment;
		if ((yyvsp[0].ival) == 2)
			(yyvsp[-1].itemptr)->i_scope |= SCOPE_PUBLIC;

		if ((yyvsp[-1].itemptr)->i_string[0] != '.')
			llseq++;

		switch((yyvsp[-1].itemptr)->i_token) {
		case UNDECLARED:
			if (pass2)
				err[pflag]++;
			else {
				(yyvsp[-1].itemptr)->i_token = LABEL;
				(yyvsp[-1].itemptr)->i_value = dollarsign;
			}
			break;
		case LABEL:
			if (!pass2) {
				(yyvsp[-1].itemptr)->i_token = MULTDEF;
				err[mflag]++;
			} else if ((yyvsp[-1].itemptr)->i_value != dollarsign) {
				// XXX - perhaps only allow retrys if JR promotions are in play?
				if (outpass) {
					if (!passfail)
						err[pflag]++;
				}
				else {
					(yyvsp[-1].itemptr)->i_value = dollarsign;
					passretry = 1;
				}
			}
			break;
		default:
			err[mflag]++;
			(yyvsp[-1].itemptr)->i_token = MULTDEF;
		}
	}
#line 4353 "zmac.c" /* yacc.c:1646  */
    break;

  case 44:
#line 2105 "zmac.y" /* yacc.c:1646  */
    {
		(yyvsp[0].itemptr)->i_scope |= SCOPE_PUBLIC;
		if (pass2) {
			if ((yyvsp[0].itemptr)->i_token == UNDECLARED) {
				err[uflag]++;
			}
		}
	}
#line 4366 "zmac.c" /* yacc.c:1646  */
    break;

  case 47:
#line 2122 "zmac.y" /* yacc.c:1646  */
    {
		if (pass2 && (yyvsp[0].itemptr)->i_scope != SCOPE_NONE && !((yyvsp[0].itemptr)->i_scope & SCOPE_EXTERNAL)) {
			fprintf(stderr, "Label scope change\n");
			err[fflag]++;
		}
		(yyvsp[0].itemptr)->i_scope |= SCOPE_EXTERNAL;
		if (pass2) {
			if ((yyvsp[0].itemptr)->i_token != UNDECLARED) {
				fprintf(stderr, "External label defined locally.\n");
				err[fflag]++;
			}
		}
	}
#line 4384 "zmac.c" /* yacc.c:1646  */
    break;

  case 48:
#line 2139 "zmac.y" /* yacc.c:1646  */
    { emit1((yyvsp[0].itemptr)->i_value, 0, 0, ET_NOARG); }
#line 4390 "zmac.c" /* yacc.c:1646  */
    break;

  case 49:
#line 2142 "zmac.y" /* yacc.c:1646  */
    {
			// XXX - maybe splitting out CPI is better?
			if (!z80 && (yyvsp[-1].itemptr)->i_value == 0166641)
				emit1(0376, 0, (yyvsp[0].exprptr), ET_BYTE);
			else
				err[fflag]++;
		}
#line 4402 "zmac.c" /* yacc.c:1646  */
    break;

  case 50:
#line 2151 "zmac.y" /* yacc.c:1646  */
    {
			if (!z80 && (yyvsp[0].itemptr)->i_value < 2)
				emit(1, E_CODE, 0, 007 | ((yyvsp[0].itemptr)->i_value << 3));
			else
				err[fflag]++;
		}
#line 4413 "zmac.c" /* yacc.c:1646  */
    break;

  case 51:
#line 2159 "zmac.y" /* yacc.c:1646  */
    {
		if (z80 || (yyvsp[-1].itemptr)->i_value == 0303) {
			checkjp(0, (yyvsp[0].exprptr));
			emit(1, E_CODE16, (yyvsp[0].exprptr), 0303);
		}
		else
			// can't optimize jump on plus
			emit(1, E_CODE16, (yyvsp[0].exprptr), 0362);
	}
#line 4427 "zmac.c" /* yacc.c:1646  */
    break;

  case 52:
#line 2170 "zmac.y" /* yacc.c:1646  */
    {	emit(1, E_CODE16, (yyvsp[0].exprptr), 0315);	}
#line 4433 "zmac.c" /* yacc.c:1646  */
    break;

  case 53:
#line 2173 "zmac.y" /* yacc.c:1646  */
    {
		// accepts rst 0-7 or rst 0,8,16,...,56
		int vec = (yyvsp[0].exprptr)->e_value;
		expr_number_check((yyvsp[0].exprptr));
		if ((vec > 7 || vec < 0) && (vec & ~(7 << 3)))
			err[vflag]++;
		if (vec > 7) vec >>= 3;
		emit(1, E_CODE, 0, (yyvsp[-1].itemptr)->i_value + ((vec & 7) << 3));
		expr_free((yyvsp[0].exprptr));
	}
#line 4448 "zmac.c" /* yacc.c:1646  */
    break;

  case 54:
#line 2185 "zmac.y" /* yacc.c:1646  */
    { emit1((yyvsp[-1].itemptr)->i_value, 0, (yyvsp[0].exprptr), ET_BYTE); }
#line 4454 "zmac.c" /* yacc.c:1646  */
    break;

  case 55:
#line 2188 "zmac.y" /* yacc.c:1646  */
    { emit1(0306, 0, (yyvsp[0].exprptr), ET_BYTE); }
#line 4460 "zmac.c" /* yacc.c:1646  */
    break;

  case 56:
#line 2191 "zmac.y" /* yacc.c:1646  */
    { emit1(0306, 0, (yyvsp[0].exprptr), ET_BYTE); }
#line 4466 "zmac.c" /* yacc.c:1646  */
    break;

  case 57:
#line 2194 "zmac.y" /* yacc.c:1646  */
    { emit1(0306 + ((yyvsp[-1].itemptr)->i_value << 3), 0, (yyvsp[0].exprptr), ET_BYTE); }
#line 4472 "zmac.c" /* yacc.c:1646  */
    break;

  case 58:
#line 2197 "zmac.y" /* yacc.c:1646  */
    { emit1(0306 + ((yyvsp[-3].itemptr)->i_value << 3), 0, (yyvsp[0].exprptr), ET_BYTE); }
#line 4478 "zmac.c" /* yacc.c:1646  */
    break;

  case 59:
#line 2200 "zmac.y" /* yacc.c:1646  */
    {
			if (!z80 && (yyvsp[-1].itemptr)->i_value == 7)
				emit(1, E_CODE16, (yyvsp[0].exprptr), 0364);
			else
				emit1(0306 | ((yyvsp[-1].itemptr)->i_value << 3), 0, (yyvsp[0].exprptr), ET_BYTE);
		}
#line 4489 "zmac.c" /* yacc.c:1646  */
    break;

  case 60:
#line 2208 "zmac.y" /* yacc.c:1646  */
    { emit1(0306 | ((yyvsp[-1].itemptr)->i_value << 3), 0, (yyvsp[0].exprptr), ET_BYTE); }
#line 4495 "zmac.c" /* yacc.c:1646  */
    break;

  case 61:
#line 2211 "zmac.y" /* yacc.c:1646  */
    { emit1(0306 | ((yyvsp[-1].itemptr)->i_value << 3), 0, (yyvsp[0].exprptr), ET_BYTE); }
#line 4501 "zmac.c" /* yacc.c:1646  */
    break;

  case 62:
#line 2214 "zmac.y" /* yacc.c:1646  */
    { emit1(0306 | ((yyvsp[-1].itemptr)->i_value << 3), 0, (yyvsp[0].exprptr), ET_BYTE); }
#line 4507 "zmac.c" /* yacc.c:1646  */
    break;

  case 63:
#line 2217 "zmac.y" /* yacc.c:1646  */
    { emit1(0306 | ((yyvsp[-3].itemptr)->i_value << 3), 0, (yyvsp[0].exprptr), ET_BYTE); }
#line 4513 "zmac.c" /* yacc.c:1646  */
    break;

  case 64:
#line 2220 "zmac.y" /* yacc.c:1646  */
    { emit1(0306 | ((yyvsp[-3].itemptr)->i_value << 3), 0, (yyvsp[0].exprptr), ET_BYTE); }
#line 4519 "zmac.c" /* yacc.c:1646  */
    break;

  case 65:
#line 2223 "zmac.y" /* yacc.c:1646  */
    { emit1(0306 | ((yyvsp[-3].itemptr)->i_value << 3), 0, (yyvsp[0].exprptr), ET_BYTE); }
#line 4525 "zmac.c" /* yacc.c:1646  */
    break;

  case 66:
#line 2226 "zmac.y" /* yacc.c:1646  */
    { emit1(0306 | ((yyvsp[-3].itemptr)->i_value << 3), 0, (yyvsp[0].exprptr), ET_BYTE); }
#line 4531 "zmac.c" /* yacc.c:1646  */
    break;

  case 67:
#line 2229 "zmac.y" /* yacc.c:1646  */
    { emit1(0200 + ((yyvsp[0].ival) & 0377), (yyvsp[0].ival), 0, ET_NOARG_DISP); }
#line 4537 "zmac.c" /* yacc.c:1646  */
    break;

  case 68:
#line 2232 "zmac.y" /* yacc.c:1646  */
    { emit1(0200 + ((yyvsp[0].ival) & 0377), (yyvsp[0].ival), 0, ET_NOARG_DISP); }
#line 4543 "zmac.c" /* yacc.c:1646  */
    break;

  case 69:
#line 2235 "zmac.y" /* yacc.c:1646  */
    { emit(1, E_CODE, 0, 0206); }
#line 4549 "zmac.c" /* yacc.c:1646  */
    break;

  case 70:
#line 2238 "zmac.y" /* yacc.c:1646  */
    { emit1(0200 + ((yyvsp[-1].itemptr)->i_value << 3) + ((yyvsp[0].ival) & 0377), (yyvsp[0].ival), 0, ET_NOARG_DISP); }
#line 4555 "zmac.c" /* yacc.c:1646  */
    break;

  case 71:
#line 2241 "zmac.y" /* yacc.c:1646  */
    { emit1(0200 + ((yyvsp[-3].itemptr)->i_value << 3) + ((yyvsp[0].ival) & 0377), (yyvsp[0].ival), 0, ET_NOARG_DISP); }
#line 4561 "zmac.c" /* yacc.c:1646  */
    break;

  case 72:
#line 2244 "zmac.y" /* yacc.c:1646  */
    { emit1(0200 + ((yyvsp[-1].itemptr)->i_value << 3) + ((yyvsp[0].ival) & 0377), (yyvsp[0].ival), 0, ET_NOARG_DISP); }
#line 4567 "zmac.c" /* yacc.c:1646  */
    break;

  case 73:
#line 2247 "zmac.y" /* yacc.c:1646  */
    { emit1(0200 + ((yyvsp[-1].itemptr)->i_value << 3) + ((yyvsp[0].ival) & 0377), (yyvsp[0].ival), 0, ET_NOARG_DISP); }
#line 4573 "zmac.c" /* yacc.c:1646  */
    break;

  case 74:
#line 2250 "zmac.y" /* yacc.c:1646  */
    { emit1(0200 + ((yyvsp[-1].itemptr)->i_value << 3) + ((yyvsp[0].ival) & 0377), (yyvsp[0].ival), 0, ET_NOARG_DISP); }
#line 4579 "zmac.c" /* yacc.c:1646  */
    break;

  case 75:
#line 2253 "zmac.y" /* yacc.c:1646  */
    { emit1(0200 + ((yyvsp[-1].itemptr)->i_value << 3) + ((yyvsp[0].ival) & 0377), (yyvsp[0].ival), 0, ET_NOARG_DISP); }
#line 4585 "zmac.c" /* yacc.c:1646  */
    break;

  case 76:
#line 2256 "zmac.y" /* yacc.c:1646  */
    { emit1(0200 + ((yyvsp[-1].itemptr)->i_value << 3) + ((yyvsp[0].ival) & 0377), (yyvsp[0].ival), 0, ET_NOARG_DISP); }
#line 4591 "zmac.c" /* yacc.c:1646  */
    break;

  case 77:
#line 2259 "zmac.y" /* yacc.c:1646  */
    { emit1(0200 + ((yyvsp[-1].itemptr)->i_value << 3) + ((yyvsp[0].ival) & 0377), (yyvsp[0].ival), 0, ET_NOARG_DISP); }
#line 4597 "zmac.c" /* yacc.c:1646  */
    break;

  case 78:
#line 2262 "zmac.y" /* yacc.c:1646  */
    { emit1(0200 + ((yyvsp[-3].itemptr)->i_value << 3) + ((yyvsp[0].ival) & 0377), (yyvsp[0].ival), 0, ET_NOARG_DISP); }
#line 4603 "zmac.c" /* yacc.c:1646  */
    break;

  case 79:
#line 2265 "zmac.y" /* yacc.c:1646  */
    { emit1(0200 + ((yyvsp[-3].itemptr)->i_value << 3) + ((yyvsp[0].ival) & 0377), (yyvsp[0].ival), 0, ET_NOARG_DISP); }
#line 4609 "zmac.c" /* yacc.c:1646  */
    break;

  case 80:
#line 2268 "zmac.y" /* yacc.c:1646  */
    { emit1(0200 + ((yyvsp[-3].itemptr)->i_value << 3) + ((yyvsp[0].ival) & 0377), (yyvsp[0].ival), 0, ET_NOARG_DISP); }
#line 4615 "zmac.c" /* yacc.c:1646  */
    break;

  case 81:
#line 2271 "zmac.y" /* yacc.c:1646  */
    { emit1(0200 + ((yyvsp[-3].itemptr)->i_value << 3) + ((yyvsp[0].ival) & 0377), (yyvsp[0].ival), 0, ET_NOARG_DISP); }
#line 4621 "zmac.c" /* yacc.c:1646  */
    break;

  case 82:
#line 2274 "zmac.y" /* yacc.c:1646  */
    { emit1(0145400 + ((yyvsp[-1].itemptr)->i_value << 3) + ((yyvsp[0].ival) & 0377), (yyvsp[0].ival), 0, ET_NOARG_DISP); }
#line 4627 "zmac.c" /* yacc.c:1646  */
    break;

  case 83:
#line 2277 "zmac.y" /* yacc.c:1646  */
    { emit1(0xCB00 + ((yyvsp[-3].itemptr)->i_value << 3) + ((yyvsp[0].ival) & 0377), (yyvsp[-2].ival), 0, ET_NOARG_DISP); }
#line 4633 "zmac.c" /* yacc.c:1646  */
    break;

  case 84:
#line 2280 "zmac.y" /* yacc.c:1646  */
    { emit1((yyvsp[-1].itemptr)->i_value + (((yyvsp[0].ival) & 0377) << 3) + 4, (yyvsp[0].ival), 0, ET_NOARG_DISP); }
#line 4639 "zmac.c" /* yacc.c:1646  */
    break;

  case 85:
#line 2283 "zmac.y" /* yacc.c:1646  */
    { emit1((yyvsp[-1].itemptr)->i_value + (((yyvsp[0].ival) & 0377) << 3) + 4, (yyvsp[0].ival), 0, ET_NOARG_DISP); }
#line 4645 "zmac.c" /* yacc.c:1646  */
    break;

  case 86:
#line 2286 "zmac.y" /* yacc.c:1646  */
    { if ((yyvsp[-3].itemptr)->i_value == 1)
				emit(2,E_CODE,0,0355,0112+(yyvsp[0].ival));
			else
				emit(2,E_CODE,0,0355,0102+(yyvsp[0].ival));
		}
#line 4655 "zmac.c" /* yacc.c:1646  */
    break;

  case 87:
#line 2293 "zmac.y" /* yacc.c:1646  */
    { emitdad((yyvsp[-2].ival),(yyvsp[0].ival)); }
#line 4661 "zmac.c" /* yacc.c:1646  */
    break;

  case 88:
#line 2296 "zmac.y" /* yacc.c:1646  */
    {
			if ((yyvsp[-2].ival) != (yyvsp[0].ival)) {
				fprintf(stderr,"ADD mar, mar error\n");
				err[gflag]++;
			}
			emitdad((yyvsp[-2].ival),(yyvsp[0].ival));
		}
#line 4673 "zmac.c" /* yacc.c:1646  */
    break;

  case 89:
#line 2304 "zmac.y" /* yacc.c:1646  */
    { emitdad(040, (yyvsp[0].ival)); }
#line 4679 "zmac.c" /* yacc.c:1646  */
    break;

  case 90:
#line 2307 "zmac.y" /* yacc.c:1646  */
    { emit1(((yyvsp[-1].itemptr)->i_value << 3) + ((yyvsp[0].ival) & 0377) + 3, (yyvsp[0].ival), 0, ET_NOARG); }
#line 4685 "zmac.c" /* yacc.c:1646  */
    break;

  case 91:
#line 2310 "zmac.y" /* yacc.c:1646  */
    { emit1(((yyvsp[-1].itemptr)->i_value << 3) + ((yyvsp[0].ival) & 0377) + 3, (yyvsp[0].ival), 0, ET_NOARG); }
#line 4691 "zmac.c" /* yacc.c:1646  */
    break;

  case 92:
#line 2313 "zmac.y" /* yacc.c:1646  */
    { emit1((yyvsp[-1].itemptr)->i_value + ((yyvsp[0].ival) & 0377), (yyvsp[0].ival), 0, ET_NOARG); }
#line 4697 "zmac.c" /* yacc.c:1646  */
    break;

  case 93:
#line 2316 "zmac.y" /* yacc.c:1646  */
    { emit1((yyvsp[-1].itemptr)->i_value + ((yyvsp[0].ival) & 0377), (yyvsp[0].ival), 0, ET_NOARG); }
#line 4703 "zmac.c" /* yacc.c:1646  */
    break;

  case 94:
#line 2319 "zmac.y" /* yacc.c:1646  */
    {
			int bit = (yyvsp[-2].exprptr)->e_value;
			expr_number_check((yyvsp[-2].exprptr));
			expr_free((yyvsp[-2].exprptr));
			if (bit < 0 || bit > 7)
				err[vflag]++;
			emit1((yyvsp[-3].itemptr)->i_value + ((bit & 7) << 3) + ((yyvsp[0].ival) & 0377), (yyvsp[0].ival), 0, ET_NOARG_DISP);
		}
#line 4716 "zmac.c" /* yacc.c:1646  */
    break;

  case 95:
#line 2329 "zmac.y" /* yacc.c:1646  */
    {
			int bit = (yyvsp[-4].exprptr)->e_value;
			expr_number_check((yyvsp[-4].exprptr));
			expr_free((yyvsp[-4].exprptr));
			if (bit < 0 || bit > 7)
				err[vflag]++;
			emit1((yyvsp[-5].itemptr)->i_value + ((bit & 7) << 3) + ((yyvsp[0].ival) & 0377), (yyvsp[-2].ival), 0, ET_NOARG_DISP);
		}
#line 4729 "zmac.c" /* yacc.c:1646  */
    break;

  case 96:
#line 2339 "zmac.y" /* yacc.c:1646  */
    {
		checkjp((yyvsp[-2].ival), (yyvsp[0].exprptr));
		emit(1, E_CODE16, (yyvsp[0].exprptr), 0302 + (yyvsp[-2].ival));
	}
#line 4738 "zmac.c" /* yacc.c:1646  */
    break;

  case 97:
#line 2345 "zmac.y" /* yacc.c:1646  */
    {
		checkjp((yyvsp[-1].itemptr)->i_value, (yyvsp[0].exprptr));
		emit(1, E_CODE16, (yyvsp[0].exprptr), (yyvsp[-1].itemptr)->i_value);
	}
#line 4747 "zmac.c" /* yacc.c:1646  */
    break;

  case 98:
#line 2351 "zmac.y" /* yacc.c:1646  */
    { emit1(0351, (yyvsp[-1].ival), 0, ET_NOARG); }
#line 4753 "zmac.c" /* yacc.c:1646  */
    break;

  case 99:
#line 2354 "zmac.y" /* yacc.c:1646  */
    { emit(1, E_CODE16, (yyvsp[0].exprptr), 0304 + (yyvsp[-2].ival)); }
#line 4759 "zmac.c" /* yacc.c:1646  */
    break;

  case 100:
#line 2357 "zmac.y" /* yacc.c:1646  */
    { emit(1, E_CODE16, (yyvsp[0].exprptr), (yyvsp[-1].itemptr)->i_value); }
#line 4765 "zmac.c" /* yacc.c:1646  */
    break;

  case 101:
#line 2360 "zmac.y" /* yacc.c:1646  */
    { emitjr(030,(yyvsp[0].exprptr)); }
#line 4771 "zmac.c" /* yacc.c:1646  */
    break;

  case 102:
#line 2363 "zmac.y" /* yacc.c:1646  */
    { emitjr((yyvsp[-3].itemptr)->i_value + (yyvsp[-2].ival), (yyvsp[0].exprptr)); }
#line 4777 "zmac.c" /* yacc.c:1646  */
    break;

  case 103:
#line 2366 "zmac.y" /* yacc.c:1646  */
    { emitjr((yyvsp[-1].itemptr)->i_value, (yyvsp[0].exprptr)); }
#line 4783 "zmac.c" /* yacc.c:1646  */
    break;

  case 104:
#line 2369 "zmac.y" /* yacc.c:1646  */
    { emit(1, E_CODE, 0, (yyvsp[0].itemptr)->i_value); }
#line 4789 "zmac.c" /* yacc.c:1646  */
    break;

  case 105:
#line 2372 "zmac.y" /* yacc.c:1646  */
    { emit(1, E_CODE, 0, 0300 + (yyvsp[0].ival)); }
#line 4795 "zmac.c" /* yacc.c:1646  */
    break;

  case 106:
#line 2375 "zmac.y" /* yacc.c:1646  */
    {
			// Many constraints on byte access to IX/IY registers.
			if (((yyvsp[-2].ival) | (yyvsp[0].ival)) >> 16) {
				int a = (yyvsp[-2].ival);
				int b = (yyvsp[0].ival);

				// Only ixh,ixh; ixh,ixl; ixl,ixh; ixl,ixl allowed.
				if (a >> 16 && b >> 16) {
					if (a >> 8 != b >> 8) {
						fprintf(stderr, "LD cannot move between ix and iy\n");
						err[gflag]++;
					}
				}
				else {
					int c = b >> 16 ? a : b;
					// No access to h, l, (hl), (ix), (iy)
					if (c == 4 || c == 5 || (c & 0xff) == 6) {
						fprintf(stderr, "LD cannot combine i/xy/lh and h,l,(hl),(ix) or (iy).\n");
						err[gflag]++;
					}
				}
			}

			if (((yyvsp[-2].ival) & 0377) == 6 && ((yyvsp[0].ival) & 0377) == 6) {
				fprintf(stderr,"LD reg, reg error: can't do memory to memory\n");
				err[gflag]++;
			}
			emit1(0100 + (((yyvsp[-2].ival) & 7) << 3) + ((yyvsp[0].ival) & 7),(yyvsp[-2].ival) | (yyvsp[0].ival), 0, ET_NOARG_DISP);
		}
#line 4829 "zmac.c" /* yacc.c:1646  */
    break;

  case 107:
#line 2406 "zmac.y" /* yacc.c:1646  */
    {
			if ((yyvsp[-2].ival) == 6 && (yyvsp[0].ival) == 6) err[gflag]++;
			emit1(0100 + (((yyvsp[-2].ival) & 7) << 3) + ((yyvsp[0].ival) & 7),(yyvsp[-2].ival) | (yyvsp[0].ival), 0, ET_NOARG_DISP);
		}
#line 4838 "zmac.c" /* yacc.c:1646  */
    break;

  case 108:
#line 2412 "zmac.y" /* yacc.c:1646  */
    { emit1(6 + (((yyvsp[-2].ival) & 0377) << 3), (yyvsp[-2].ival), (yyvsp[0].exprptr), ET_BYTE); }
#line 4844 "zmac.c" /* yacc.c:1646  */
    break;

  case 109:
#line 2415 "zmac.y" /* yacc.c:1646  */
    { emit1(6 + (((yyvsp[-2].ival) & 0377) << 3), (yyvsp[-2].ival), (yyvsp[0].exprptr), ET_BYTE); }
#line 4850 "zmac.c" /* yacc.c:1646  */
    break;

  case 110:
#line 2418 "zmac.y" /* yacc.c:1646  */
    {	if ((yyvsp[-4].ival) != 7) {
				fprintf(stderr,"LD reg, (RP) error\n");
				err[gflag]++;
			}
			else emit(1, E_CODE, 0, 012 + (yyvsp[-1].itemptr)->i_value);
		}
#line 4861 "zmac.c" /* yacc.c:1646  */
    break;

  case 111:
#line 2426 "zmac.y" /* yacc.c:1646  */
    {
			if ((yyvsp[0].ival) != 0 && (yyvsp[0].ival) != 2) err[gflag]++;
			emit(1, E_CODE, 0, 012 + ((yyvsp[0].ival) << 3));
		}
#line 4870 "zmac.c" /* yacc.c:1646  */
    break;

  case 112:
#line 2432 "zmac.y" /* yacc.c:1646  */
    {
			if ((yyvsp[-2].ival) != 7) {
				fprintf(stderr,"LD reg, (expr) error: A only valid destination\n");
				err[gflag]++;
			}
			else {
				expr_word_check((yyvsp[0].exprptr));
				emit(1, E_CODE16, (yyvsp[0].exprptr), 072);
			}
		}
#line 4885 "zmac.c" /* yacc.c:1646  */
    break;

  case 113:
#line 2444 "zmac.y" /* yacc.c:1646  */
    {
			expr_word_check((yyvsp[0].exprptr));
			emit(1, E_CODE16, (yyvsp[0].exprptr), 072);
		}
#line 4894 "zmac.c" /* yacc.c:1646  */
    break;

  case 114:
#line 2450 "zmac.y" /* yacc.c:1646  */
    { emit(1, E_CODE, 0, 2 + (yyvsp[-3].itemptr)->i_value); }
#line 4900 "zmac.c" /* yacc.c:1646  */
    break;

  case 115:
#line 2453 "zmac.y" /* yacc.c:1646  */
    {
			if ((yyvsp[0].ival) != 0 && (yyvsp[0].ival) != 2) err[gflag]++;
			emit(1, E_CODE, 0, 2 + ((yyvsp[0].ival) << 3));
		}
#line 4909 "zmac.c" /* yacc.c:1646  */
    break;

  case 116:
#line 2459 "zmac.y" /* yacc.c:1646  */
    {
			expr_word_check((yyvsp[-2].exprptr));
			emit(1, E_CODE16, (yyvsp[-2].exprptr), 062);
		}
#line 4918 "zmac.c" /* yacc.c:1646  */
    break;

  case 117:
#line 2465 "zmac.y" /* yacc.c:1646  */
    {
			expr_word_check((yyvsp[0].exprptr));
			emit(1, E_CODE16, (yyvsp[0].exprptr), 062);
		}
#line 4927 "zmac.c" /* yacc.c:1646  */
    break;

  case 118:
#line 2471 "zmac.y" /* yacc.c:1646  */
    {
			if ((yyvsp[-2].ival) != 7) {
				fprintf(stderr,"LD reg, MISCREG error: A only valid destination\n");
				err[gflag]++;
			}
			else emit(2, E_CODE, 0, 0355, 0127 + (yyvsp[0].itemptr)->i_value);
		}
#line 4939 "zmac.c" /* yacc.c:1646  */
    break;

  case 119:
#line 2480 "zmac.y" /* yacc.c:1646  */
    { emit(2, E_CODE, 0, 0355, 0107 + (yyvsp[-2].itemptr)->i_value); }
#line 4945 "zmac.c" /* yacc.c:1646  */
    break;

  case 120:
#line 2483 "zmac.y" /* yacc.c:1646  */
    {
			expr_word_check((yyvsp[0].exprptr));
			emit1(1 + ((yyvsp[-2].ival) & 060), (yyvsp[-2].ival), (yyvsp[0].exprptr), ET_WORD);
		}
#line 4954 "zmac.c" /* yacc.c:1646  */
    break;

  case 121:
#line 2489 "zmac.y" /* yacc.c:1646  */
    {
			expr_word_check((yyvsp[0].exprptr));
			emit1(1 + ((yyvsp[-2].ival) & 060), (yyvsp[-2].ival), (yyvsp[0].exprptr), ET_WORD);
		}
#line 4963 "zmac.c" /* yacc.c:1646  */
    break;

  case 122:
#line 2495 "zmac.y" /* yacc.c:1646  */
    {
			expr_word_check((yyvsp[0].exprptr));
			if (((yyvsp[-2].ival) & 060) == 040)
				emit1(052, (yyvsp[-2].ival), (yyvsp[0].exprptr), ET_WORD);
			else
				emit(2, E_CODE16, (yyvsp[0].exprptr), 0355, 0113 + (yyvsp[-2].ival));
		}
#line 4975 "zmac.c" /* yacc.c:1646  */
    break;

  case 123:
#line 2504 "zmac.y" /* yacc.c:1646  */
    {
			expr_word_check((yyvsp[0].exprptr));
			emit1(052, 040, (yyvsp[0].exprptr), ET_WORD);
		}
#line 4984 "zmac.c" /* yacc.c:1646  */
    break;

  case 124:
#line 2510 "zmac.y" /* yacc.c:1646  */
    {
			expr_word_check((yyvsp[-2].exprptr));
			if (((yyvsp[0].ival) & 060) == 040)
				emit1(042, (yyvsp[0].ival), (yyvsp[-2].exprptr), ET_WORD);
			else
				emit(2, E_CODE16, (yyvsp[-2].exprptr), 0355, 0103 + (yyvsp[0].ival));
		}
#line 4996 "zmac.c" /* yacc.c:1646  */
    break;

  case 125:
#line 2519 "zmac.y" /* yacc.c:1646  */
    {
			expr_word_check((yyvsp[0].exprptr));
			emit1(042, 040, (yyvsp[0].exprptr), ET_WORD);
		}
#line 5005 "zmac.c" /* yacc.c:1646  */
    break;

  case 126:
#line 2525 "zmac.y" /* yacc.c:1646  */
    {
			if ((yyvsp[-2].ival) != 060) {
				fprintf(stderr,"LD evenreg error\n");
				err[gflag]++;
			}
			else
				emit1(0371, (yyvsp[0].ival), 0, ET_NOARG);
		}
#line 5018 "zmac.c" /* yacc.c:1646  */
    break;

  case 127:
#line 2535 "zmac.y" /* yacc.c:1646  */
    {
			if ((yyvsp[-2].itemptr)->i_value != 020) {
				fprintf(stderr,"EX RP, HL error\n");
				err[gflag]++;
			}
			else
				emit(1, E_CODE, 0, 0353);
		}
#line 5031 "zmac.c" /* yacc.c:1646  */
    break;

  case 128:
#line 2545 "zmac.y" /* yacc.c:1646  */
    { emit(1, E_CODE, 0, 010); }
#line 5037 "zmac.c" /* yacc.c:1646  */
    break;

  case 129:
#line 2548 "zmac.y" /* yacc.c:1646  */
    { emit1(0343, (yyvsp[0].ival), 0, ET_NOARG); }
#line 5043 "zmac.c" /* yacc.c:1646  */
    break;

  case 130:
#line 2551 "zmac.y" /* yacc.c:1646  */
    {
			if ((yyvsp[-2].ival) != 7) {
				fprintf(stderr,"IN reg, (expr) error\n");
				err[gflag]++;
			}
			else	{
				if ((yyvsp[0].exprptr)->e_value < 0 || (yyvsp[0].exprptr)->e_value > 255)
					err[vflag]++;
				emit(1, E_CODE8, (yyvsp[0].exprptr), (yyvsp[-3].itemptr)->i_value);
			}
		}
#line 5059 "zmac.c" /* yacc.c:1646  */
    break;

  case 131:
#line 2564 "zmac.y" /* yacc.c:1646  */
    {
			if ((yyvsp[0].exprptr)->e_value < 0 || (yyvsp[0].exprptr)->e_value > 255)
				err[vflag]++;
			emit(1, E_CODE8, (yyvsp[0].exprptr), (yyvsp[-1].itemptr)->i_value);
		}
#line 5069 "zmac.c" /* yacc.c:1646  */
    break;

  case 132:
#line 2571 "zmac.y" /* yacc.c:1646  */
    { emit(2, E_CODE, 0, 0355, 0100 + ((yyvsp[-4].ival) << 3)); }
#line 5075 "zmac.c" /* yacc.c:1646  */
    break;

  case 133:
#line 2574 "zmac.y" /* yacc.c:1646  */
    { emit(2, E_CODE, 0, 0355, 0160); }
#line 5081 "zmac.c" /* yacc.c:1646  */
    break;

  case 134:
#line 2577 "zmac.y" /* yacc.c:1646  */
    { emit(2, E_CODE, 0, 0355, 0160); }
#line 5087 "zmac.c" /* yacc.c:1646  */
    break;

  case 135:
#line 2580 "zmac.y" /* yacc.c:1646  */
    {
			if ((yyvsp[-2].exprptr)->e_value < 0 || (yyvsp[-2].exprptr)->e_value > 255)
				err[vflag]++;
			emit(1, E_CODE8, (yyvsp[-2].exprptr), (yyvsp[-3].itemptr)->i_value);
		}
#line 5097 "zmac.c" /* yacc.c:1646  */
    break;

  case 136:
#line 2587 "zmac.y" /* yacc.c:1646  */
    {
			if ((yyvsp[0].exprptr)->e_value < 0 || (yyvsp[0].exprptr)->e_value > 255)
				err[vflag]++;
			emit(1, E_CODE8, (yyvsp[0].exprptr), (yyvsp[-1].itemptr)->i_value);
		}
#line 5107 "zmac.c" /* yacc.c:1646  */
    break;

  case 137:
#line 2594 "zmac.y" /* yacc.c:1646  */
    { emit(2, E_CODE, 0, 0355, 0101 + ((yyvsp[0].ival) << 3)); }
#line 5113 "zmac.c" /* yacc.c:1646  */
    break;

  case 138:
#line 2597 "zmac.y" /* yacc.c:1646  */
    {
			expr_number_check((yyvsp[0].exprptr));
			if ((yyvsp[0].exprptr)->e_value != 0) {
				fprintf(stderr, "Can only output 0 to port C with OUT\n");
				err[vflag]++;
			}
			expr_free((yyvsp[0].exprptr));

			emit(2, E_CODE8, 0, 0355, 0101 + (6 << 3));
		}
#line 5128 "zmac.c" /* yacc.c:1646  */
    break;

  case 139:
#line 2609 "zmac.y" /* yacc.c:1646  */
    {
			int im = (yyvsp[0].exprptr)->e_value;
			expr_number_check((yyvsp[0].exprptr));
			expr_free((yyvsp[0].exprptr));
			if (im > 2 || im < 0)
				err[vflag]++;
			else
				emit(2, E_CODE, 0, (yyvsp[-1].itemptr)->i_value >> 8, (yyvsp[-1].itemptr)->i_value + ((im + (im > 0)) << 3));
		}
#line 5142 "zmac.c" /* yacc.c:1646  */
    break;

  case 140:
#line 2620 "zmac.y" /* yacc.c:1646  */
    {
			expr_number_check((yyvsp[0].exprptr));
			if (phaseflag) {
				err[oflag]++;
			} else {
				phaseflag = 1;
				phdollar = dollarsign;
				dollarsign = (yyvsp[0].exprptr)->e_value;
				phbegin = dollarsign;
			}
			expr_free((yyvsp[0].exprptr));
		}
#line 5159 "zmac.c" /* yacc.c:1646  */
    break;

  case 141:
#line 2634 "zmac.y" /* yacc.c:1646  */
    {
			if (!phaseflag) {
				err[oflag]++;
			} else {
				phaseflag = 0;
				dollarsign = phdollar + dollarsign - phbegin;
			}
		}
#line 5172 "zmac.c" /* yacc.c:1646  */
    break;

  case 142:
#line 2644 "zmac.y" /* yacc.c:1646  */
    {
			expr_reloc_check((yyvsp[0].exprptr));
			// Cannot org to the other segment (but absolute values are OK)
			if (relopt && segment && ((yyvsp[0].exprptr)->e_scope & SCOPE_SEGMASK) != segment)
				err[rflag]++;
			if (phaseflag) {
				err[oflag]++;
				dollarsign = phdollar + dollarsign - phbegin;
				phaseflag = 0;
			}
			if ((yyvsp[0].exprptr)->e_value-dollarsign) {
				flushbin();
				flushoth();
				olddollar = (yyvsp[0].exprptr)->e_value;
				oldothdollar = (yyvsp[0].exprptr)->e_value;
				dollarsign = (yyvsp[0].exprptr)->e_value;
				emit_addr = (yyvsp[0].exprptr)->e_value;
				seg_pos[segment] = dollarsign;
				if (seg_pos[segment] > seg_size[segment])
					seg_size[segment] = seg_pos[segment];
				putrelcmd(RELCMD_SETLOC);
				putrelsegref(segment, seg_pos[segment]);
				segchange = 0;
			}
			expr_free((yyvsp[0].exprptr));
		}
#line 5203 "zmac.c" /* yacc.c:1646  */
    break;

  case 143:
#line 2672 "zmac.y" /* yacc.c:1646  */
    {
			expr_number_check((yyvsp[0].exprptr));
			if (outpass && !(yyvsp[0].exprptr)->e_value)
			{
				err[aflag]++;
			}
			expr_free((yyvsp[0].exprptr));
		}
#line 5216 "zmac.c" /* yacc.c:1646  */
    break;

  case 144:
#line 2682 "zmac.y" /* yacc.c:1646  */
    {
			expr_number_check((yyvsp[0].exprptr));
			tstates = (yyvsp[0].exprptr)->e_value;
			tstatesum[emit_addr] = tstates;
			expr_free((yyvsp[0].exprptr));
		}
#line 5227 "zmac.c" /* yacc.c:1646  */
    break;

  case 145:
#line 2690 "zmac.y" /* yacc.c:1646  */
    {
			expr_number_check((yyvsp[0].exprptr));
			ocf = (yyvsp[0].exprptr)->e_value;
			ocfsum[emit_addr] = ocf;
			expr_free((yyvsp[0].exprptr));
		}
#line 5238 "zmac.c" /* yacc.c:1646  */
    break;

  case 153:
#line 2714 "zmac.y" /* yacc.c:1646  */
    { (yyval.ival) = 0; }
#line 5244 "zmac.c" /* yacc.c:1646  */
    break;

  case 154:
#line 2714 "zmac.y" /* yacc.c:1646  */
    { (yyval.ival) = 1; }
#line 5250 "zmac.c" /* yacc.c:1646  */
    break;

  case 155:
#line 2718 "zmac.y" /* yacc.c:1646  */
    {
			(yyvsp[0].itemptr)->i_token = MPARM;
			if (parm_number >= PARMMAX)
				error("Too many parameters");
			(yyvsp[0].itemptr)->i_value = parm_number++;
			(yyvsp[0].itemptr)->i_scope = (yyvsp[-1].ival);
			(yyvsp[0].itemptr)->i_chain = 0;
		}
#line 5263 "zmac.c" /* yacc.c:1646  */
    break;

  case 157:
#line 2730 "zmac.y" /* yacc.c:1646  */
    { list1(); }
#line 5269 "zmac.c" /* yacc.c:1646  */
    break;

  case 161:
#line 2742 "zmac.y" /* yacc.c:1646  */
    {
			(yyvsp[0].itemptr)->i_token = MPARM;
			if (parm_number >= PARMMAX)
				error("Too many parameters");
			(yyvsp[0].itemptr)->i_value = parm_number++;
			(yyvsp[0].itemptr)->i_scope = 0;
			(yyvsp[0].itemptr)->i_chain = 1;
		}
#line 5282 "zmac.c" /* yacc.c:1646  */
    break;

  case 165:
#line 2763 "zmac.y" /* yacc.c:1646  */
    {
			cp = malloc(strlen(tempbuf)+1);
#ifdef M_DEBUG
			fprintf (stderr, "[Arg%u(%p): %s]\n", parm_number, cp, tempbuf);
#endif
			est2[parm_number++].param = cp;
			strcpy(cp, tempbuf);
		}
#line 5295 "zmac.c" /* yacc.c:1646  */
    break;

  case 170:
#line 2784 "zmac.y" /* yacc.c:1646  */
    {
			(yyval.ival) = (yyvsp[0].itemptr)->i_value;
		}
#line 5303 "zmac.c" /* yacc.c:1646  */
    break;

  case 173:
#line 2794 "zmac.y" /* yacc.c:1646  */
    { if ((yyvsp[0].itemptr)->i_value != 070) err[gflag]++; (yyval.ival) = 6; }
#line 5309 "zmac.c" /* yacc.c:1646  */
    break;

  case 174:
#line 2798 "zmac.y" /* yacc.c:1646  */
    {
			(yyval.ival) = (yyvsp[0].itemptr)->i_value;
		}
#line 5317 "zmac.c" /* yacc.c:1646  */
    break;

  case 175:
#line 2803 "zmac.y" /* yacc.c:1646  */
    {
			(yyval.ival) = (yyvsp[0].itemptr)->i_value;
		}
#line 5325 "zmac.c" /* yacc.c:1646  */
    break;

  case 176:
#line 2808 "zmac.y" /* yacc.c:1646  */
    {
			(yyval.ival) = (yyvsp[0].itemptr)->i_value;
		}
#line 5333 "zmac.c" /* yacc.c:1646  */
    break;

  case 177:
#line 2814 "zmac.y" /* yacc.c:1646  */
    {
			(yyval.ival) = 6;
		}
#line 5341 "zmac.c" /* yacc.c:1646  */
    break;

  case 179:
#line 2822 "zmac.y" /* yacc.c:1646  */
    {
			expr_number_check((yyvsp[-1].exprptr));
			disp = (yyvsp[-1].exprptr)->e_value;
			expr_free((yyvsp[-1].exprptr));
			(yyval.ival) = ((yyvsp[-2].itemptr)->i_value & 0177400) | 6;
			if (disp > 127 || disp < -128)
				err[vflag]++;
		}
#line 5354 "zmac.c" /* yacc.c:1646  */
    break;

  case 180:
#line 2832 "zmac.y" /* yacc.c:1646  */
    {
			disp = 0;
			(yyval.ival) = ((yyvsp[-1].itemptr)->i_value & 0177400) | 6;
		}
#line 5363 "zmac.c" /* yacc.c:1646  */
    break;

  case 183:
#line 2843 "zmac.y" /* yacc.c:1646  */
    { if ((yyvsp[0].ival) & 1) err[gflag]++; (yyval.ival) = (yyvsp[0].ival) << 3; }
#line 5369 "zmac.c" /* yacc.c:1646  */
    break;

  case 184:
#line 2845 "zmac.y" /* yacc.c:1646  */
    { (yyval.ival) = (yyvsp[0].itemptr)->i_value; }
#line 5375 "zmac.c" /* yacc.c:1646  */
    break;

  case 185:
#line 2849 "zmac.y" /* yacc.c:1646  */
    {
			(yyval.ival) = (yyvsp[0].itemptr)->i_value;
		}
#line 5383 "zmac.c" /* yacc.c:1646  */
    break;

  case 186:
#line 2854 "zmac.y" /* yacc.c:1646  */
    {
			(yyval.ival) = (yyvsp[0].itemptr)->i_value;
		}
#line 5391 "zmac.c" /* yacc.c:1646  */
    break;

  case 188:
#line 2861 "zmac.y" /* yacc.c:1646  */
    { if ((yyvsp[0].ival) & 1) err[gflag]++; (yyval.ival) = (yyvsp[0].ival) << 3; }
#line 5397 "zmac.c" /* yacc.c:1646  */
    break;

  case 189:
#line 2863 "zmac.y" /* yacc.c:1646  */
    { (yyval.ival) = (yyvsp[0].itemptr)->i_value; }
#line 5403 "zmac.c" /* yacc.c:1646  */
    break;

  case 190:
#line 2867 "zmac.y" /* yacc.c:1646  */
    {
			(yyval.ival) = (yyvsp[0].itemptr)->i_value;
		}
#line 5411 "zmac.c" /* yacc.c:1646  */
    break;

  case 191:
#line 2872 "zmac.y" /* yacc.c:1646  */
    {
			(yyval.ival) = (yyvsp[0].itemptr)->i_value;
		}
#line 5419 "zmac.c" /* yacc.c:1646  */
    break;

  case 193:
#line 2880 "zmac.y" /* yacc.c:1646  */
    {
			(yyval.ival) = (yyvsp[0].itemptr)->i_value;
		}
#line 5427 "zmac.c" /* yacc.c:1646  */
    break;

  case 194:
#line 2886 "zmac.y" /* yacc.c:1646  */
    {
			(yyval.ival) = (yyvsp[0].itemptr)->i_value;
		}
#line 5435 "zmac.c" /* yacc.c:1646  */
    break;

  case 195:
#line 2891 "zmac.y" /* yacc.c:1646  */
    {
			(yyval.ival) = (yyvsp[0].itemptr)->i_value;
		}
#line 5443 "zmac.c" /* yacc.c:1646  */
    break;

  case 197:
#line 2899 "zmac.y" /* yacc.c:1646  */
    {
			(yyval.ival) = (yyvsp[0].itemptr)->i_value;
		}
#line 5451 "zmac.c" /* yacc.c:1646  */
    break;

  case 198:
#line 2905 "zmac.y" /* yacc.c:1646  */
    {
			(yyval.ival) = (yyvsp[0].itemptr)->i_value;
		}
#line 5459 "zmac.c" /* yacc.c:1646  */
    break;

  case 199:
#line 2910 "zmac.y" /* yacc.c:1646  */
    {	(yyval.ival) = 030;	}
#line 5465 "zmac.c" /* yacc.c:1646  */
    break;

  case 202:
#line 2919 "zmac.y" /* yacc.c:1646  */
    {
			emit(1, E_DATA, expr_num((yyvsp[0].ival)));
			emit(1, E_DATA, expr_num((yyvsp[0].ival)>>8));
		}
#line 5474 "zmac.c" /* yacc.c:1646  */
    break;

  case 203:
#line 2925 "zmac.y" /* yacc.c:1646  */
    {
			cp = (yyvsp[0].cval);
			while (*cp != '\0')
				emit(1,E_DATA,expr_num(*cp++));
		}
#line 5484 "zmac.c" /* yacc.c:1646  */
    break;

  case 204:
#line 2932 "zmac.y" /* yacc.c:1646  */
    {
			if (is_number((yyvsp[0].exprptr)) && ((yyvsp[0].exprptr)->e_value < -128 || (yyvsp[0].exprptr)->e_value > 255))
				err[vflag]++;
			emit(1, E_DATA, (yyvsp[0].exprptr));
		}
#line 5494 "zmac.c" /* yacc.c:1646  */
    break;

  case 207:
#line 2949 "zmac.y" /* yacc.c:1646  */
    {
			if ((yyvsp[0].exprptr)->e_value < -32768 || (yyvsp[0].exprptr)->e_value > 65535) {
				err[vflag]++;
			}
			emit(2, E_DATA, (yyvsp[0].exprptr));
		}
#line 5505 "zmac.c" /* yacc.c:1646  */
    break;

  case 210:
#line 2966 "zmac.y" /* yacc.c:1646  */
    {
			// Can't check overflow as I only have 32 bit ints.
			emit(4, E_DATA, (yyvsp[0].exprptr));
		}
#line 5514 "zmac.c" /* yacc.c:1646  */
    break;

  case 212:
#line 2978 "zmac.y" /* yacc.c:1646  */
    {
			(yyval.exprptr) = expr_num((yyvsp[0].ival));
		}
#line 5522 "zmac.c" /* yacc.c:1646  */
    break;

  case 215:
#line 2991 "zmac.y" /* yacc.c:1646  */
    {	(yyval.exprptr) = (yyvsp[-1].exprptr);	}
#line 5528 "zmac.c" /* yacc.c:1646  */
    break;

  case 216:
#line 2996 "zmac.y" /* yacc.c:1646  */
    {
			err[eflag]++;
			(yyval.exprptr) = expr_num(0);
		}
#line 5537 "zmac.c" /* yacc.c:1646  */
    break;

  case 217:
#line 3002 "zmac.y" /* yacc.c:1646  */
    {
			(yyval.exprptr) = expr_alloc();
			(yyval.exprptr)->e_token = 'v';
			(yyval.exprptr)->e_item = (yyvsp[0].itemptr);
			(yyval.exprptr)->e_scope = (yyvsp[0].itemptr)->i_scope;
			(yyval.exprptr)->e_value = (yyvsp[0].itemptr)->i_value;
			(yyvsp[0].itemptr)->i_uses++;
		}
#line 5550 "zmac.c" /* yacc.c:1646  */
    break;

  case 218:
#line 3012 "zmac.y" /* yacc.c:1646  */
    {
			(yyval.exprptr) = expr_num((yyvsp[0].ival));
		}
#line 5558 "zmac.c" /* yacc.c:1646  */
    break;

  case 219:
#line 3017 "zmac.y" /* yacc.c:1646  */
    {
			(yyval.exprptr) = expr_num((yyvsp[0].ival));
		}
#line 5566 "zmac.c" /* yacc.c:1646  */
    break;

  case 220:
#line 3022 "zmac.y" /* yacc.c:1646  */
    {
			(yyval.exprptr) = expr_alloc();
			(yyval.exprptr)->e_token = 'v';
			(yyval.exprptr)->e_item = (yyvsp[0].itemptr); // not necessary
			(yyval.exprptr)->e_scope = (yyvsp[0].itemptr)->i_scope;
			(yyval.exprptr)->e_value = (yyvsp[0].itemptr)->i_value;
		}
#line 5578 "zmac.c" /* yacc.c:1646  */
    break;

  case 221:
#line 3031 "zmac.y" /* yacc.c:1646  */
    {
			(yyval.exprptr) = expr_alloc();
			(yyval.exprptr)->e_token = 'v';
			(yyval.exprptr)->e_item = (yyvsp[0].itemptr); // not necessary
			(yyval.exprptr)->e_scope = (yyvsp[0].itemptr)->i_scope;
			(yyval.exprptr)->e_value = (yyvsp[0].itemptr)->i_value;
		}
#line 5590 "zmac.c" /* yacc.c:1646  */
    break;

  case 222:
#line 3040 "zmac.y" /* yacc.c:1646  */
    {
			(yyval.exprptr) = expr_alloc();
			(yyval.exprptr)->e_token = 'v';
			(yyval.exprptr)->e_item = (yyvsp[0].itemptr); // not necessary
			(yyval.exprptr)->e_scope = (yyvsp[0].itemptr)->i_scope;
			(yyval.exprptr)->e_value = (yyvsp[0].itemptr)->i_value;
		}
#line 5602 "zmac.c" /* yacc.c:1646  */
    break;

  case 223:
#line 3049 "zmac.y" /* yacc.c:1646  */
    {
			(yyval.exprptr) = expr_num(dollarsign);
			(yyval.exprptr)->e_scope = segment;
		}
#line 5611 "zmac.c" /* yacc.c:1646  */
    break;

  case 224:
#line 3055 "zmac.y" /* yacc.c:1646  */
    {
			(yyval.exprptr) = expr_alloc();
			(yyval.exprptr)->e_token = 'u';
			(yyval.exprptr)->e_item = (yyvsp[0].itemptr);
			(yyval.exprptr)->e_scope = (yyvsp[0].itemptr)->i_scope;
			(yyval.exprptr)->e_value = 0;

			if (!((yyvsp[0].itemptr)->i_scope & SCOPE_EXTERNAL)) {
				err[uflag]++;
			}
		}
#line 5627 "zmac.c" /* yacc.c:1646  */
    break;

  case 225:
#line 3068 "zmac.y" /* yacc.c:1646  */
    {
			(yyval.exprptr) = expr_alloc();
			(yyval.exprptr)->e_token = 'm';
			(yyval.exprptr)->e_item = (yyvsp[0].itemptr);
			(yyval.exprptr)->e_scope = (yyvsp[0].itemptr)->i_scope;
			(yyval.exprptr)->e_value = 0;
		}
#line 5639 "zmac.c" /* yacc.c:1646  */
    break;

  case 226:
#line 3077 "zmac.y" /* yacc.c:1646  */
    {
			(yyval.exprptr) = expr_op((yyvsp[-2].exprptr), '+', (yyvsp[0].exprptr), (yyvsp[-2].exprptr)->e_value + (yyvsp[0].exprptr)->e_value);

			// Can't operate on external labels.
			// But we can add constants to any scope.
			if (!(((yyvsp[-2].exprptr)->e_scope | (yyvsp[0].exprptr)->e_scope) & SCOPE_EXTERNAL) &&
				(((yyvsp[-2].exprptr)->e_scope && SCOPE_SEGMASK) == 0 ||
				((yyvsp[0].exprptr)->e_scope && SCOPE_SEGMASK) == 0))
			{
				(yyval.exprptr)->e_scope &= ~(SCOPE_NORELOC | SCOPE_SEGMASK);
				(yyval.exprptr)->e_scope |= ((yyvsp[-2].exprptr)->e_scope | (yyvsp[0].exprptr)->e_scope & SCOPE_SEGMASK);
			}
		}
#line 5657 "zmac.c" /* yacc.c:1646  */
    break;

  case 227:
#line 3092 "zmac.y" /* yacc.c:1646  */
    {
			(yyval.exprptr) = expr_op_sc((yyvsp[-2].exprptr), '-', (yyvsp[0].exprptr), (yyvsp[-2].exprptr)->e_value - (yyvsp[0].exprptr)->e_value);

			// But we can subtract a constant.
			if (!(((yyvsp[-2].exprptr)->e_scope | (yyvsp[0].exprptr)->e_scope) & SCOPE_EXTERNAL) &&
				(((yyvsp[0].exprptr)->e_scope & SCOPE_SEGMASK) == 0))
			{
				(yyval.exprptr)->e_scope &= ~(SCOPE_NORELOC | SCOPE_SEGMASK);
				(yyval.exprptr)->e_scope |= ((yyvsp[-2].exprptr)->e_scope & SCOPE_SEGMASK);
			}
		}
#line 5673 "zmac.c" /* yacc.c:1646  */
    break;

  case 228:
#line 3105 "zmac.y" /* yacc.c:1646  */
    {
			int val = 0;
			if ((yyvsp[0].exprptr)->e_value == 0)
				err[eflag]++;
			else
				val = (yyvsp[-2].exprptr)->e_value / (yyvsp[0].exprptr)->e_value;

			(yyval.exprptr) = expr_op((yyvsp[-2].exprptr), '/', (yyvsp[0].exprptr), val);
		}
#line 5687 "zmac.c" /* yacc.c:1646  */
    break;

  case 229:
#line 3116 "zmac.y" /* yacc.c:1646  */
    {	(yyval.exprptr) = expr_op((yyvsp[-2].exprptr), '*', (yyvsp[0].exprptr), (yyvsp[-2].exprptr)->e_value * (yyvsp[0].exprptr)->e_value); }
#line 5693 "zmac.c" /* yacc.c:1646  */
    break;

  case 230:
#line 3119 "zmac.y" /* yacc.c:1646  */
    {
			int val;
		domod:
			val = 0;
			if ((yyvsp[0].exprptr)->e_value == 0)
				err[eflag]++;
			else
				val = (yyvsp[-2].exprptr)->e_value % (yyvsp[0].exprptr)->e_value;

			(yyval.exprptr) = expr_op((yyvsp[-2].exprptr), '%', (yyvsp[0].exprptr), val);
		}
#line 5709 "zmac.c" /* yacc.c:1646  */
    break;

  case 231:
#line 3132 "zmac.y" /* yacc.c:1646  */
    {	goto domod;	}
#line 5715 "zmac.c" /* yacc.c:1646  */
    break;

  case 232:
#line 3135 "zmac.y" /* yacc.c:1646  */
    {	(yyval.exprptr) = expr_op((yyvsp[-2].exprptr), '&', (yyvsp[0].exprptr), (yyvsp[-2].exprptr)->e_value & (yyvsp[0].exprptr)->e_value); }
#line 5721 "zmac.c" /* yacc.c:1646  */
    break;

  case 233:
#line 3138 "zmac.y" /* yacc.c:1646  */
    {	(yyval.exprptr) = expr_op((yyvsp[-2].exprptr), '&', (yyvsp[0].exprptr), (yyvsp[-2].exprptr)->e_value & (yyvsp[0].exprptr)->e_value); }
#line 5727 "zmac.c" /* yacc.c:1646  */
    break;

  case 234:
#line 3141 "zmac.y" /* yacc.c:1646  */
    {	(yyval.exprptr) = expr_op((yyvsp[-2].exprptr), '|', (yyvsp[0].exprptr), (yyvsp[-2].exprptr)->e_value | (yyvsp[0].exprptr)->e_value); }
#line 5733 "zmac.c" /* yacc.c:1646  */
    break;

  case 235:
#line 3144 "zmac.y" /* yacc.c:1646  */
    {	(yyval.exprptr) = expr_op((yyvsp[-2].exprptr), '|', (yyvsp[0].exprptr), (yyvsp[-2].exprptr)->e_value | (yyvsp[0].exprptr)->e_value); }
#line 5739 "zmac.c" /* yacc.c:1646  */
    break;

  case 236:
#line 3147 "zmac.y" /* yacc.c:1646  */
    {	(yyval.exprptr) = expr_op((yyvsp[-2].exprptr), '^', (yyvsp[0].exprptr), (yyvsp[-2].exprptr)->e_value ^ (yyvsp[0].exprptr)->e_value); }
#line 5745 "zmac.c" /* yacc.c:1646  */
    break;

  case 237:
#line 3150 "zmac.y" /* yacc.c:1646  */
    {	(yyval.exprptr) = expr_op((yyvsp[-2].exprptr), '^', (yyvsp[0].exprptr), (yyvsp[-2].exprptr)->e_value ^ (yyvsp[0].exprptr)->e_value); }
#line 5751 "zmac.c" /* yacc.c:1646  */
    break;

  case 238:
#line 3153 "zmac.y" /* yacc.c:1646  */
    {	(yyval.exprptr) = expr_op((yyvsp[-2].exprptr), (yyvsp[-1].ival), (yyvsp[0].exprptr), (yyvsp[-2].exprptr)->e_value << (yyvsp[0].exprptr)->e_value); }
#line 5757 "zmac.c" /* yacc.c:1646  */
    break;

  case 239:
#line 3156 "zmac.y" /* yacc.c:1646  */
    {
			int val = (yyvsp[0].exprptr)->e_value == 0 ? (yyvsp[-2].exprptr)->e_value : (((yyvsp[-2].exprptr)->e_value >> 1) & ((0x7fff << 16) | 0xffff)) >> ((yyvsp[0].exprptr)->e_value - 1);
			(yyval.exprptr) = expr_op((yyvsp[-2].exprptr), (yyvsp[-1].ival), (yyvsp[0].exprptr), val);
		}
#line 5766 "zmac.c" /* yacc.c:1646  */
    break;

  case 240:
#line 3162 "zmac.y" /* yacc.c:1646  */
    {	(yyval.exprptr) = expr_op_sc((yyvsp[-2].exprptr), '<', (yyvsp[0].exprptr), (yyvsp[-2].exprptr)->e_value < (yyvsp[0].exprptr)->e_value); }
#line 5772 "zmac.c" /* yacc.c:1646  */
    break;

  case 241:
#line 3165 "zmac.y" /* yacc.c:1646  */
    {	(yyval.exprptr) = expr_op_sc((yyvsp[-2].exprptr), '=', (yyvsp[0].exprptr), (yyvsp[-2].exprptr)->e_value == (yyvsp[0].exprptr)->e_value); }
#line 5778 "zmac.c" /* yacc.c:1646  */
    break;

  case 242:
#line 3168 "zmac.y" /* yacc.c:1646  */
    {	(yyval.exprptr) = expr_op_sc((yyvsp[-2].exprptr), '>', (yyvsp[0].exprptr), (yyvsp[-2].exprptr)->e_value > (yyvsp[0].exprptr)->e_value); }
#line 5784 "zmac.c" /* yacc.c:1646  */
    break;

  case 243:
#line 3171 "zmac.y" /* yacc.c:1646  */
    {	(yyval.exprptr) = expr_op_sc((yyvsp[-2].exprptr), (yyvsp[-1].ival), (yyvsp[0].exprptr), (yyvsp[-2].exprptr)->e_value < (yyvsp[0].exprptr)->e_value); }
#line 5790 "zmac.c" /* yacc.c:1646  */
    break;

  case 244:
#line 3174 "zmac.y" /* yacc.c:1646  */
    {	(yyval.exprptr) = expr_op_sc((yyvsp[-2].exprptr), (yyvsp[-1].ival), (yyvsp[0].exprptr), (yyvsp[-2].exprptr)->e_value > (yyvsp[0].exprptr)->e_value); }
#line 5796 "zmac.c" /* yacc.c:1646  */
    break;

  case 245:
#line 3177 "zmac.y" /* yacc.c:1646  */
    {	(yyval.exprptr) = expr_op_sc((yyvsp[-2].exprptr), (yyvsp[-1].ival), (yyvsp[0].exprptr), (yyvsp[-2].exprptr)->e_value <= (yyvsp[0].exprptr)->e_value); }
#line 5802 "zmac.c" /* yacc.c:1646  */
    break;

  case 246:
#line 3180 "zmac.y" /* yacc.c:1646  */
    {	(yyval.exprptr) = expr_op_sc((yyvsp[-2].exprptr), (yyvsp[-1].ival), (yyvsp[0].exprptr), (yyvsp[-2].exprptr)->e_value >= (yyvsp[0].exprptr)->e_value); }
#line 5808 "zmac.c" /* yacc.c:1646  */
    break;

  case 247:
#line 3183 "zmac.y" /* yacc.c:1646  */
    {	(yyval.exprptr) = expr_op_sc((yyvsp[-2].exprptr), (yyvsp[-1].ival), (yyvsp[0].exprptr), (yyvsp[-2].exprptr)->e_value != (yyvsp[0].exprptr)->e_value); }
#line 5814 "zmac.c" /* yacc.c:1646  */
    break;

  case 248:
#line 3186 "zmac.y" /* yacc.c:1646  */
    {	(yyval.exprptr) = expr_op((yyvsp[-2].exprptr), (yyvsp[-1].ival), (yyvsp[0].exprptr), (yyvsp[-2].exprptr)->e_value && (yyvsp[0].exprptr)->e_value); }
#line 5820 "zmac.c" /* yacc.c:1646  */
    break;

  case 249:
#line 3189 "zmac.y" /* yacc.c:1646  */
    {	(yyval.exprptr) = expr_op((yyvsp[-2].exprptr), (yyvsp[-1].ival), (yyvsp[0].exprptr), (yyvsp[-2].exprptr)->e_value || (yyvsp[0].exprptr)->e_value); }
#line 5826 "zmac.c" /* yacc.c:1646  */
    break;

  case 250:
#line 3192 "zmac.y" /* yacc.c:1646  */
    {
			expr_number_check((yyvsp[-4].exprptr));
			if ((yyvsp[-4].exprptr)->e_value) {
				(yyval.exprptr) = (yyvsp[-2].exprptr);
				expr_free((yyvsp[0].exprptr));
			}
			else {
				(yyval.exprptr) = (yyvsp[0].exprptr);
				expr_free((yyvsp[-2].exprptr));
			}
			expr_free((yyvsp[-4].exprptr));
		}
#line 5843 "zmac.c" /* yacc.c:1646  */
    break;

  case 251:
#line 3206 "zmac.y" /* yacc.c:1646  */
    {	(yyval.exprptr) = (yyvsp[-1].exprptr);	}
#line 5849 "zmac.c" /* yacc.c:1646  */
    break;

  case 252:
#line 3209 "zmac.y" /* yacc.c:1646  */
    {	(yyval.exprptr) = expr_op((yyvsp[0].exprptr), '~', 0, ~(yyvsp[0].exprptr)->e_value);	}
#line 5855 "zmac.c" /* yacc.c:1646  */
    break;

  case 253:
#line 3212 "zmac.y" /* yacc.c:1646  */
    {	(yyval.exprptr) = expr_op((yyvsp[0].exprptr), '~', 0, ~(yyvsp[0].exprptr)->e_value);	}
#line 5861 "zmac.c" /* yacc.c:1646  */
    break;

  case 254:
#line 3215 "zmac.y" /* yacc.c:1646  */
    {	(yyval.exprptr) = expr_op((yyvsp[0].exprptr), '!', 0, !(yyvsp[0].exprptr)->e_value);	}
#line 5867 "zmac.c" /* yacc.c:1646  */
    break;

  case 255:
#line 3218 "zmac.y" /* yacc.c:1646  */
    {	(yyval.exprptr) = (yyvsp[0].exprptr); /* no effect */	}
#line 5873 "zmac.c" /* yacc.c:1646  */
    break;

  case 256:
#line 3221 "zmac.y" /* yacc.c:1646  */
    {	(yyval.exprptr) = expr_op((yyvsp[0].exprptr), '-', 0, -(yyvsp[0].exprptr)->e_value);	}
#line 5879 "zmac.c" /* yacc.c:1646  */
    break;

  case 257:
#line 3224 "zmac.y" /* yacc.c:1646  */
    {
			expr_reloc_check((yyvsp[0].exprptr));
			(yyval.exprptr) = expr_num(tstatesum[phaseaddr((yyvsp[0].exprptr)->e_value)]);
			expr_free((yyvsp[0].exprptr));
		}
#line 5889 "zmac.c" /* yacc.c:1646  */
    break;

  case 258:
#line 3231 "zmac.y" /* yacc.c:1646  */
    {
			int low, low8080, fetch;
			expr_reloc_check((yyvsp[0].exprptr));
			zi_tstates(memory + phaseaddr((yyvsp[0].exprptr)->e_value), &low, 0, &fetch, &low8080, 0);
			(yyval.exprptr) = expr_num(z80 ? low : low8080);
			expr_free((yyvsp[0].exprptr));
		}
#line 5901 "zmac.c" /* yacc.c:1646  */
    break;

  case 259:
#line 3240 "zmac.y" /* yacc.c:1646  */
    {
			int high, high8080, fetch;
			expr_reloc_check((yyvsp[0].exprptr));
			zi_tstates(memory + phaseaddr((yyvsp[0].exprptr)->e_value), 0, &high, &fetch, 0, &high8080);
			(yyval.exprptr) = expr_num(z80 ? high : high8080);
			expr_free((yyvsp[0].exprptr));
		}
#line 5913 "zmac.c" /* yacc.c:1646  */
    break;

  case 260:
#line 3249 "zmac.y" /* yacc.c:1646  */
    {
			expr_reloc_check((yyvsp[0].exprptr));
			(yyval.exprptr) = expr_num(ocfsum[phaseaddr((yyvsp[0].exprptr)->e_value)]);
			expr_free((yyvsp[0].exprptr));
		}
#line 5923 "zmac.c" /* yacc.c:1646  */
    break;

  case 261:
#line 3256 "zmac.y" /* yacc.c:1646  */
    {
			(yyval.exprptr) = expr_op((yyvsp[0].exprptr), (yyvsp[-1].ival), 0, (yyvsp[0].exprptr)->e_value & 0xff);
		}
#line 5931 "zmac.c" /* yacc.c:1646  */
    break;

  case 262:
#line 3261 "zmac.y" /* yacc.c:1646  */
    {
			(yyval.exprptr) = expr_op((yyvsp[0].exprptr), (yyvsp[-1].ival), 0, ((yyvsp[0].exprptr)->e_value >> 8) & 0xff);
		}
#line 5939 "zmac.c" /* yacc.c:1646  */
    break;

  case 269:
#line 3282 "zmac.y" /* yacc.c:1646  */
    { int i;
		if (expptr >= MAXEXP)
			error("Macro expansion level");
		est2 = (union exprec *) malloc((PARMMAX + 4) * sizeof *est2);
		expstack[expptr] = est2;
		for (i=0; i<PARMMAX; i++)
			est2[i].param = 0;
		arg_flag++;
	}
#line 5953 "zmac.c" /* yacc.c:1646  */
    break;

  case 270:
#line 3295 "zmac.y" /* yacc.c:1646  */
    {	arg_flag++;	}
#line 5959 "zmac.c" /* yacc.c:1646  */
    break;

  case 271:
#line 3299 "zmac.y" /* yacc.c:1646  */
    {	arg_flag = 0;	}
#line 5965 "zmac.c" /* yacc.c:1646  */
    break;

  case 272:
#line 3302 "zmac.y" /* yacc.c:1646  */
    { raw = 1; }
#line 5971 "zmac.c" /* yacc.c:1646  */
    break;


#line 5975 "zmac.c" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 3304 "zmac.y" /* yacc.c:1906  */

/*extern int	yylval;*/

#define F_END	0
#define OTHER	1
#define SPACE	2
#define DIGIT	3
#define LETTER	4
#define STARTER 5
#define DOLLAR	6


/*
 *  This is the table of character classes.  It is used by the lexical
 *  analyser. (yylex())
 */
char	charclass[] = {
	F_END,	OTHER,	OTHER,	OTHER,	OTHER,	OTHER,	OTHER,	OTHER,
	OTHER,	SPACE,	OTHER,	OTHER,	OTHER,	SPACE,	OTHER,	OTHER,
	OTHER,	OTHER,	OTHER,	OTHER,	OTHER,	OTHER,	OTHER,	OTHER,
	OTHER,	OTHER,	OTHER,	OTHER,	OTHER,	OTHER,	OTHER,	OTHER,
	SPACE,	OTHER,	OTHER,	OTHER,	DOLLAR,	OTHER,	OTHER,	OTHER,	//  !"#$%&'
	OTHER,	OTHER,	OTHER,	OTHER,	OTHER,	OTHER,	STARTER,OTHER,	// ()*+,-./
	DIGIT,	DIGIT,	DIGIT,	DIGIT,	DIGIT,	DIGIT,	DIGIT,	DIGIT,	// 01234567
	DIGIT,	DIGIT,	OTHER,	OTHER,	OTHER,	OTHER,	OTHER,	STARTER,// 89:;<=>?
	STARTER,LETTER, LETTER, LETTER, LETTER, LETTER, LETTER, LETTER,	// @ABCDEFG
	LETTER, LETTER, LETTER, LETTER, LETTER, LETTER, LETTER, LETTER,	// HIJKLMNO
	LETTER, LETTER, LETTER, LETTER, LETTER, LETTER, LETTER, LETTER,	// PQRSTUVW
	LETTER, LETTER, LETTER, OTHER,	OTHER,	OTHER,	OTHER,	LETTER,	// XYZ[\]^_
	OTHER,	LETTER, LETTER, LETTER, LETTER, LETTER, LETTER, LETTER,	// `abcdefg
	LETTER, LETTER, LETTER, LETTER, LETTER, LETTER, LETTER, LETTER,	// hijklmno
	LETTER, LETTER, LETTER, LETTER, LETTER, LETTER, LETTER, LETTER,	// pqrstuvw
	LETTER, LETTER, LETTER, OTHER,	OTHER,	OTHER,	OTHER,	OTHER,	// xyz{|}~
};


/*
 *  the following table tells which characters are parts of numbers.
 *  The entry is non-zero for characters which can be parts of numbers.
 */
char	numpart[] = {
	0,	0,	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	'0',	'1',	'2',	'3',	'4',	'5',	'6',	'7',
	'8',	'9',	0,	0,	0,	0,	0,	0,
	0,	'A',	'B',	'C',	'D',	'E',	'F',	0,
	'H',	0,	0,	0,	0,	0,	0,	'O',
	0,	'Q',	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	0,	'a',	'b',	'c',	'd',	'e',	'f',	0,
	'h',	0,	0,	0,	0,	0,	0,	'o',
	0,	'q',	0,	0,	0,	0,	0,	0,
	'x',	0,	0,	0,	0,	0,	0,	0,
	0};




/*
 *  the following table is a list of assembler mnemonics;
 *  for each mnemonic the associated machine-code bit pattern
 *  and symbol type are given.
 *
 *  The i_uses field is overloaded to indicate the possible uses for
 *  a token.
 */

#define VERB	(1)	/* opcode or psuedo-op */
#define I8080	(2)	/* used in 8080 instructions */
#define Z80	(4)	/* used in Z80 instructions */
#define UNDOC	(8)	/* used only in undocumented instructions */
#define TERM	(16)	/* can appear in expressions (not all marked) */

struct	item	keytab[] = {
	{"*mod",	0,	MRAS_MOD,	VERB },
	{".8080",	0,	INSTSET,	VERB },
	{"a",		7,	ACC,		I8080 | Z80 },
	{"aci",		0316,	ALUI8,		VERB | I8080 },
	{"adc",		1,	ARITHC,		VERB | I8080 | Z80  },
	{"add",		0,	ADD,		VERB | I8080 | Z80  },
	{"adi",		0306,	ALUI8,		VERB | I8080 },
	{"af",		060,	AF,		Z80 },
	{"ana",		4,	ARITHC,		VERB | I8080},
	{"and",		4,	AND,		VERB | Z80 | TERM },
	{"ani",		0346,	ALUI8,		VERB | I8080 },
	{".ascii",	0,	DEFB,		VERB },
	{".aseg",	SEG_ABS,SETSEG,		VERB },
	{".aset",	0,	DEFL,		VERB },
	{".assert",	0,	ASSERT,		VERB },
	{"b",		0,	REGNAME,	I8080 | Z80 },
	{"bc",		0,	RP,		Z80 },
	{"bit",		0145500,BIT,		VERB | Z80 },
	{".block",	0,	DEFS,		VERB },
	{".byte",	0,	DEFB,		VERB },
	{"c",		1,	C,		I8080 | Z80 },
	{"call",	0315,	CALL,		VERB | I8080 | Z80 },
	{"cc",		0334,	CALL8,		VERB | I8080 },
	{"ccf",		077,	NOOPERAND,	VERB | Z80 },
	{"cm",		0374,	CALL8,		VERB | I8080 },
	{"cma",		057,	NOOPERAND,	VERB | I8080 },
	{"cmc",		077,	NOOPERAND,	VERB | I8080 },
	{"cmp",		7,	LOGICAL,	VERB | I8080 },
	{"cnc",		0324,	CALL8,		VERB | I8080 },
	{"cnz",		0304,	CALL8,		VERB | I8080 },
	{".comment",	SPCOM,	SPECIAL,	VERB },
	{".cond",	0,	IF_TK,		VERB },
	{"cp",		7,	LOGICAL,	VERB | I8080 | Z80 },
	{"cpd",		0166651,NOOPERAND,	VERB | Z80 },
	{"cpdr",	0166671,NOOPERAND,	VERB | Z80 },
	{"cpe",		0354,	CALL8,		VERB | I8080 },
	{"cpi",		0166641,NOOPERAND,	VERB | I8080 | Z80 },
	{"cpir",	0166661,NOOPERAND,	VERB | Z80 },
	{"cpl",		057,	NOOPERAND,	VERB | Z80 },
	{"cpo",		0344,	CALL8,		VERB | I8080 },
	{".cseg",	SEG_CODE,SETSEG,	VERB },
	{"cz",		0314,	CALL8,		VERB | I8080 },
	{"d",		2,	REGNAME,	I8080 | Z80 },
	{"daa",		0047,	NOOPERAND,	VERB | I8080 | Z80 },
	{"dad",		0,	DAD,		VERB | I8080 },
	{".db",		0,	DEFB,		VERB },
	{".dc",		0,	DC,		VERB },
	{"dcr",		1,	INRDCR,		VERB | I8080 },
	{"dcx",		1,	INXDCX,		VERB | I8080 },
	{"de",		020,	RP,		Z80 },
	{"dec",		1,	INCDEC,		VERB | I8080 | Z80 },
	{".defb",	0,	DEFB,		VERB },
	{".defd",	0,	DEFD,		VERB },
	{".defl",	0,	DEFL,		VERB },
	{".defm",	0,	DEFB,		VERB },
	{".defs",	0,	DEFS,		VERB },
	{".defw",	0,	DEFW,		VERB },
	{".dephase",	0,	DEPHASE,	VERB },
	{"di",		0363,	NOOPERAND,	VERB | I8080 | Z80 },
	{"djnz",	020,	DJNZ,		VERB | Z80 },
	{".ds",		0,	DEFS,		VERB },
	{".dseg",	SEG_DATA,SETSEG,	VERB },
	{".dw",		0,	DEFW,		VERB },
	{".dword",	0,	DEFD,		VERB },
	{"e",		3,	REGNAME,	I8080 | Z80 },
	{"ei",		0373,	NOOPERAND,	VERB | I8080 | Z80 },
	{".eject",	1,	LIST,		VERB },
	{".elist",	3,	LIST,		VERB },
	{".else",	0,	ELSE_TK,	VERB },
	{".end",	0,	END,		VERB },
	{".endc",	0,	ENDIF_TK,	VERB },
	{".endif",	0,	ENDIF_TK,	VERB },
	{".endm", 	0,	ENDM,		VERB },
	{".entry",	0,	PUBLIC,		VERB },
	{"eq",		0,	'=',		0 },
	{".equ",	0,	EQU,		VERB },
	{"ex",		0,	EX,		VERB | Z80 },
	{".ext",	0,	EXTRN,		VERB },
	{".extern",	0,	EXTRN,		VERB },
	{".extrn",	0,	EXTRN,		VERB },
	{"exx",		0331,	NOOPERAND,	VERB | Z80 },
	{".flist",	4,	LIST,		VERB },
	{"ge",		0,	GE,		0 },
	{".glist",	5,	LIST,		VERB },
	{".global",	0,	PUBLIC,		VERB },
	{"gt",		0,	GT,		0 },
	{"h",		4,	REGNAME,	I8080 | Z80 },
	{"halt",	0166,	NOOPERAND,	VERB | Z80 },
	{"high",	0,	HIGH,		0 },
	{"hl",		040,	HL,		Z80 },
	{"hlt",		0166,	NOOPERAND,	VERB | I8080 },
	{"i",		0,	MISCREG,	Z80 },
	{".if",		0,	IF_TK,		VERB },
	{"im",		0166506,IM,		VERB | Z80 },
	{"in",		0333,	TK_IN,		VERB | I8080 | Z80 },
	{"inc",		0,	INCDEC,		VERB | Z80 },
	{".incbin", 	0, 	INCBIN,		VERB },
	{".include",	PSINC,	ARGPSEUDO,	VERB },
	{"ind",		0166652,NOOPERAND,	VERB | Z80 },
	{"indr",	0166672,NOOPERAND,	VERB | Z80 },
	{"ini",		0166642,NOOPERAND,	VERB | Z80 },
	{"inir",	0166662,NOOPERAND,	VERB | Z80 },
	{"inr",		0,	INRDCR,		VERB | I8080 },
	{"inx",		0,	INXDCX,		VERB | I8080 },
	{"ix",		0156440,INDEX,		Z80 },
	{"ixh",		0x1DD04,IXYLH,		Z80 | UNDOC },
	{"ixl",		0x1DD05,IXYLH,		Z80 | UNDOC },
	{"iy",		0176440,INDEX,		Z80 },
	{"iyh",		0x1FD04,IXYLH,		Z80 | UNDOC },
	{"iyl",		0x1FD05,IXYLH,		Z80 | UNDOC },
	{"jc",		0332,	JUMP8,		VERB | I8080 },
	{"jm",		0372,	JUMP8,		VERB | I8080 },
	{"jmp",		0303,	JP,		VERB | I8080 },
	{"jnc",		0322,	JUMP8,		VERB | I8080 },
	{"jnz",		0302,	JUMP8,		VERB | I8080 },
	{"jp",		0,	JP,		VERB | I8080 | Z80 },
	{"jpe",		0352,	JUMP8,		VERB | I8080 },
	{".jperror",	0,	JPERROR,	VERB },
	{"jpo",		0342,	JUMP8,		VERB | I8080 },
	{"jr",		040,	JR,		VERB | Z80 },
	{".jrpromote",	0,	JRPROMOTE,	VERB },
	{"jz",		0312,	JUMP8,		VERB | I8080 },
	{"l",		5,	REGNAME,	I8080 | Z80 },
	{"ld",		0,	LD,		VERB | Z80 },
	{"lda",		0,	LDA,		VERB | I8080 },
	{"ldax",	0,	LDAX,		VERB | I8080 },
	{"ldd",		0166650,NOOPERAND,	VERB | Z80 },
	{"lddr",	0166670,NOOPERAND,	VERB | Z80 },
	{"ldi",		0166640,NOOPERAND,	VERB | Z80 },
	{"ldir",	0166660,NOOPERAND,	VERB | Z80 },
	{"le",		0,	LE,		0 },
	{"lhld",	0,	LHLD,		VERB | I8080 },
	{".list",	0,	LIST,		VERB },
	{".local",	0,	LOCAL,		VERB },
	{"low",		0,	LOW,		0 },
	{"lt",		0,	LT,		0 },
	{"lxi",		0,	LXI,		VERB | I8080 },
	{"m",		070,	COND,		I8080 | Z80 },
	{".macro",	0,	MACRO,		VERB },
	{".max",	1,	MINMAX,		VERB },
	{".min",	0,	MINMAX,		VERB },
	{".mlist",	6,	LIST,		VERB },
	{"mod",		0,	MOD,		0 },
	{"mov",		0,	MOV,		VERB | I8080 },
	{"mvi",		0,	MVI,		VERB | I8080 },
	{".name",	SPNAME,	SPECIAL,	VERB },
	{"nc",		020,	SPCOND,		0 },
	{"ne",		0,	NE,		0 },
	{"neg",		0166504,NOOPERAND,	VERB | Z80 },
	{".nolist",	-1,	LIST,		VERB },
	{"nop",		0,	NOOPERAND,	VERB | I8080 | Z80 },
	{"not",		0,	NOT,		0 },
	{"nv",		040,	COND,		Z80 },
	{"nz",		0,	SPCOND,		Z80 },
	{"ocf",		0,	OCF,		0 },
	{"or",		6,	OR,		VERB | Z80 | TERM },
	{"ora",		6,	LOGICAL,	VERB | I8080 },
	{".org",	0,	ORG,		VERB },
	{"ori",		0366,	ALUI8,		VERB | I8080 },
	{"otdr",	0166673,NOOPERAND,	VERB | Z80 },
	{"otir",	0166663,NOOPERAND,	VERB | Z80 },
	{"out",		0323,	TK_OUT,		VERB | I8080 | Z80 },
	{"outd",	0166653,NOOPERAND,	VERB | Z80 },
	{"outi",	0166643,NOOPERAND,	VERB | Z80 },
	{"p",		060,	COND,		Z80 },
	{".page",	1,	LIST,		VERB },
	{"pchl",	0351,	NOOPERAND,	VERB | I8080 },
	{"pe",		050,	COND,		Z80 },
	{"pfix",	0xdd,	NOOPERAND,	VERB | Z80 | UNDOC },
	{"pfiy",	0xfd,	NOOPERAND,	VERB | Z80 | UNDOC },
	{".phase",	0,	PHASE,		VERB },
	{"po",		040,	COND,		Z80 },
	{"pop",		0301,	PUSHPOP,	VERB | I8080 | Z80 },
	{"psw", 	060,	PSW,		I8080 },
	{".public",	0,	PUBLIC,		VERB },
	{"push",	0305,	PUSHPOP,	VERB | I8080 | Z80 },
	{"r",		010,	MISCREG,	Z80 },
	{"ral",		027,	NOOPERAND,	VERB | I8080 },
	{"rar",		037,	NOOPERAND,	VERB | I8080 },
	{"rc",		0330,	NOOPERAND,	VERB | I8080 },
	{".read",	PSINC,	ARGPSEUDO,	VERB },
	{"res",		0145600,BIT,		VERB | Z80 },
	{"ret",		0311,	RET,		VERB | I8080 | Z80 },
	{"reti",	0166515,NOOPERAND,	VERB | Z80 },
	{"retn",	0166505,NOOPERAND,	VERB | Z80 },
	{"rl",		2,	SHIFT,		VERB | Z80 },
	{"rla",		027,	NOOPERAND,	VERB | Z80 },
	{"rlc",		0,	SHIFT,		VERB | I8080 | Z80 },
	{"rlca",	07,	NOOPERAND,	VERB | Z80 },
	{"rld",		0166557,NOOPERAND,	VERB | Z80 },
	{"rm",		0370,	NOOPERAND,	VERB | I8080 },
	{".rmem",	0,	DEFS,		VERB },
	{"rnc",		0320,	NOOPERAND,	VERB | I8080 },
	{"rnz",		0300,	NOOPERAND,	VERB | I8080 },
	{"rp",		0360,	NOOPERAND,	VERB | I8080 },
	{"rpe",		0350,	NOOPERAND,	VERB | I8080 },
	{"rpo",		0340,	NOOPERAND,	VERB | I8080 },
	{"rr",		3,	SHIFT,		VERB | Z80 },
	{"rra",		037,	NOOPERAND,	VERB | Z80 },
	{"rrc",		1,	SHIFT,		VERB | I8080 | Z80 },
	{"rrca",	017,	NOOPERAND,	VERB | Z80 },
	{"rrd",		0166547,NOOPERAND,	VERB | Z80 },
	{"rst",		0307,	RST,		VERB | I8080 | Z80 },
	{".rsym",	PSRSYM,	ARGPSEUDO,	VERB },
	{"rz",		0310,	NOOPERAND,	VERB | I8080 },
	{"sbb",		3,	ARITHC,		VERB | I8080 },
	{"sbc",		3,	ARITHC,		VERB | Z80 },
	{"sbi",		0336,	ALUI8,		VERB | I8080 },
	{"scf",		067,	NOOPERAND,	VERB | Z80 },
	{"set",		0145700,BIT,		VERB | Z80 },
	{".setocf",	0,	SETOCF,		VERB },
	{".sett",	0,	TSTATE,		VERB },
	{"shl",		0,	SHL,		TERM },
	{"shld",	0,	SHLD,		VERB | I8080 },
	{"shr",		0,	SHR,		TERM },
	{"sl1",		6,	SHIFT,		VERB | Z80 | UNDOC },
	{"sla",		4,	SHIFT,		VERB | Z80 },
	{"sll",		6,	SHIFT,		VERB | Z80 },
	{"sp",		060,	SP,		I8080 | Z80 },
	{".space",	2,	LIST,		VERB },
	{"sphl",	0371,	NOOPERAND,	VERB | I8080 },
	{"sra",		5,	SHIFT,		VERB | Z80 },
	{"srl",		7,	SHIFT,		VERB | Z80 },
	{"sta",		0,	STA,		VERB | I8080 },
	{"stax",	0,	STAX,		VERB | I8080 },
	{"stc",		067,	NOOPERAND,	VERB | I8080 },
	{"sub",		2,	LOGICAL,	VERB | I8080 | Z80 },
	{".subttl",	SPSBTL,	SPECIAL,	VERB },
	{"sui",		0326,	ALUI8,		VERB | I8080 },
	{"t",		0,	T,		0 },
	{".text",	0,	DEFB,		VERB },
	{"tihi",	0,	TIHI,		0 },
	{"tilo",	0,	TILO,		0 },
	{".title",	SPTITL,	SPECIAL,	VERB },
	{".tstate",	0,	TSTATE,		VERB },
	{"v",		050,	COND,		Z80 },
	{".word",	0,	DEFW,		VERB },
	{".wsym",	PSWSYM,	ARGPSEUDO,	VERB },
	{"xchg",	0353,	NOOPERAND,	VERB | I8080 },
	{"xor",		5,	XOR,		VERB | Z80 | TERM },
	{"xra",		5,	LOGICAL,	VERB | I8080 },
	{"xri",		0356,	ALUI8,		VERB | I8080 },
	{"xthl",	0343,	NOOPERAND,	VERB | I8080 },
	{"z",		010,	SPCOND,		Z80 },
	{".z80",	1,	INSTSET,	VERB },
};

/*
 *  user-defined items are tabulated in the following table.
 */

struct item	itemtab[ITEMTABLESIZE];
struct item	*itemmax = itemtab+ITEMTABLESIZE;





/*
 *  lexical analyser, called by yyparse.
 */
int yylex()
{
	int c;
	char *p;
	int radix;
	int sep;
	char *d0, *dn;
	int exclude, include, overflow;

	if (arg_flag)
		return(getarg());

	if (raw) {
		int skip = 1;
		p = tempbuf;
		while ((c = nextchar()) != '\n' && c) {
			if (p >= tempmax) {
				*p = '\0';
				printf("was parsing '%s'\n", tempbuf);
				error(symlong);
			}
			if (!skip || charclass[c] != SPACE) {
				*p++ = c;
				skip = 0;
			}
		}
		if (c == 0)
			peekc = c;

		*p-- = '\0';

		while (p >= tempbuf && charclass[*p] == SPACE)
			*p-- = '\0';

		raw = 0;

		return RAWTOKEN;
	}

for (;;) switch(charclass[c = nextchar()]) {
	case F_END:
		if (expptr) {
			popsi();
			continue;
		} else return(0);

	case SPACE:
		while (charclass[c = nextchar()] == SPACE)
			;
		peekc = c;
		logcol++;
		break;
	case LETTER:
	case STARTER:
	case DIGIT:
	case DOLLAR:
	spectok:
		firstcol = getcol() == 1;

		radix = -1; // might be a number
		p = tempbuf;
		do {
			if (p >= tempmax) {
				*tempmax = '\0';
				printf("was parsing '%s'\n", tempbuf);
				error(symlong);
			}
			*p = (c >= 'A' && c <= 'Z') ? c + 'a' - 'A' : c;
			if (mras && *p == '?') {
				char *q;

				radix = 0; // can't be a number even if it looks like it

				if (expptr)
					q = getmraslocal();
				else
					for (q = modstr; *q == '@'; q++)
						;

				if (*q) {
					strcpy(p, q);
					p = strchr(p, '\0') - 1;
				}
				else
					*p = '?';
			}
			p++;
			c = nextchar();
		} while	(charclass[c]==LETTER || charclass[c]==DIGIT ||
			charclass[c]==STARTER || charclass[c]==DOLLAR);

		*p = '\0';
		// Special case for AF'
		if (c == '\'' && strcmp(tempbuf, "af") == 0)
			return AFp;

		peekc = c;

		// Pass off '?' (XXX but, technically, should only be done in expression context)
		if (strcmp(tempbuf, "?") == 0)
			return '?';

		// Pass off '$'
		if (strcmp(tempbuf, "$") == 0)
			return '$';

		// Look ahead at what we have.
		while (charclass[c] == SPACE)
			c = nextchar();

		peekc = c;

		//printf("%d %s\n", logcol, tempbuf);
		// If logcol == 0 then if c == ':' we're a label for sure.
		// If logcol == 1 if c == ':' we're a label, change logcol
		//    otherwise we're op or pseudo
		// If logcol == 0 and c == '\n' or ';' then we're alone so
		//	we give tokenization a chance otherwise label
		// If logcol >= 2 we're in the arguments
		//
		// There is quite a lot of unrealziaed scope for error
		// detection and helpful warnings.

		 // Default to any tokenization.
		exclude = 0;
		include = 0;

		if (logcol >= 2) {
			exclude = VERB;
			include = TERM;
		}
		else if (logcol == 0 && c != ';' && c != '\n')
			exclude = VERB;
		else if (logcol == 1 && c == ':') {
			exclude = VERB;
			logcol = 0;
		}

		logcol++;

		// Look for possible numbers.
		// 0x<hex> $<hex> <hex>h <octal>o <octal>q <binary>b
		// <decimal> <decimal>d
		// Suffix formats must start with 0-9.

		if (radix) {
			if (tempbuf[0] == '0' && tempbuf[1] == 'x' && tempbuf[2]) {
				radix = 16;
				d0 = tempbuf + 2;
				dn = p;
			} else if (tempbuf[0] == '$') {
				radix = 16;
				d0 = tempbuf + 1;
				dn = p;
			}
			else if (tempbuf[0] >= '0' && tempbuf[0] <= '9') {
				d0 = tempbuf;
				dn = p - 1;
				switch (*dn) {
				case 'o':
				case 'q':
					radix = 8;
					break;
				case 'd':
					radix = 10;
					break;
				case 'h':
					radix = 16;
					break;
				case 'b':
					radix = 2;
					break;
				default:
					radix = 10;
					dn++;
				}
			}
		}

		// We may have a number on our hands.
		if (radix > 0) {
			overflow = 0;
			yylval.ival = 0;

			for (; d0 < dn; d0++) {
				unsigned int ovchk = (unsigned int)yylval.ival;
				c = *d0 - (*d0 > '9' ? ('a' - 10) : '0');
				if (c < 0 || c >= radix) {
					radix = 0;
					break;
				}
				if (ovchk * radix / radix != ovchk)
					overflow = 1;

				yylval.ival *= radix;
				yylval.ival += c;
			}
		}

		// If we're in the first logical column and the token starts with
		// '$' then we'll force it to be a label even though it could be
		// a $hex constant. This will allow $FCB as a label.
		// Thus we must also allow symbol lookup a chance to override number
		// parsing if we start with a '$'.

		if (tempbuf[0] == '$') {
			if (logcol == 1 || locate(tempbuf)->i_token) {
				if (radix > 0)
					err[warn_hex]++;
				radix = 0;
			}
		}

		if (radix > 0) {
			// Might be line skipping time, though.
			if (*ifptr)
				return skipline(c);

			if (overflow) {
				err[iflag]++;
				yylval.ival = 0;
			}
			return NUMBER;
		}

		// Too late to do '$' concatenation of numbers.  But zmac
		// didn't allow for that previously at any rate.
		if (zcompat) {
			char *q = tempbuf;
			// Normal zmac operation requires we ignore $ in identifiers
			for (p = q; *p; p++)
				if (*p != '$')
					*q++ = *p;

			*q = '\0';
			p = q;
		}

		// GWP - boy, this should be a warning or error
		if (p - tempbuf > MAXSYMBOLSIZE) {
			p = tempbuf + MAXSYMBOLSIZE;
			*p = '\0';
		}

		return tokenofitem(UNDECLARED, exclude, include);

	default:
		if (*ifptr)
			return(skipline(c));

		if (mras && getcol() == 1 && c == '*')
			goto spectok;

		switch(c) {
		int corig;
		case ':':
			if (logcol == 1) {
				// Make sure "label:ret", "label: ret",
				// "label: :ret", "label: : ret" work out OK.
				// But stop fooling around once we've done the VERB
				peekc = nextchar();
				if (charclass[peekc] == SPACE)
					logcol--;
			}
			return c;
		case ';':
			return(skipline(c));
		case '\'':
		case '"':
			sep = c;
			p = tempbuf;
			p[1] = 0;
			do	switch(c = nextchar())	{
			case '\0':
			case '\n':
				err[bflag]++;
				goto retstring;
			default:
				if (c == sep && (c = nextchar()) != sep) {
				retstring:
					peekc = c;
					*p = '\0';
					if ((p-tempbuf) >2) {
						yylval.cval = tempbuf;
						return(STRING);
					} else if (p-tempbuf == 2)	{
						p = tempbuf;
						yylval.ival = *p++ ;
						yylval.ival |= *p<<8;
						return(TWOCHAR);
					} else	{
						p = tempbuf;
						yylval.ival = *p++;
						return(ONECHAR);
					}
				}
				*p++ = c;
			} while (p < tempmax);
			/*
			 *  if we break out here, our string is longer than
			 *  our input line
			 */
			error("string buffer overflow");
		case '<':
			corig = c;
			switch (c = nextchar ()) {
			case '=':
				return LE;
			case '<':
				return SHL;
			case '>':
				return NE;
			default:
				peekc = c;
				return corig;
			}
			/* break; suppress "unreachable" warning for tcc */
		case '>':
			corig = c;
			switch (c = nextchar ()) {
			case '=':
				return GE;
			case '>':
				return SHR;
			default:
				peekc = c;
				return corig;
			}
			/* break; suppress "unreachable" warning for tcc */
		case '!':
			corig = c;
			switch (c = nextchar ()) {
			case '=':
				return NE;
			default:
				peekc = c;
				return corig;
			}
			/* break; suppress "unreachable" warning for tcc */
		case '=':
			corig = c;
			switch (c = nextchar ()) {
			case '=':
				return '=';
			default:
				peekc = c;
				return corig;
			}
			/* break; suppress "unreachable" warning for tcc */

		case '&':
			corig = c;
			if ((c = nextchar()) == '&')
				return ANDAND;
			else {
				peekc = c;
				return corig;
			}
			/* break; suppress "unreachable" warning for tcc */
		case '|':
			corig = c;
			if ((c = nextchar()) == '|')
				return OROR;
			else {
				peekc = c;
				return corig;
			}
			/* break; suppress "unreachable" warning for tcc */
		default:
			return(c);
		}
	}
}

// Verify keytab is in alphabetical order.
int check_keytab()
{
	int i;
	char *prev;

	for (i = 0; i < sizeof(keytab) / sizeof(keytab[0]); i++) {
		char *next = keytab[i].i_string;
		next += *next == '.';
		if (i != 0) {
			if (strcmp(prev, next) >= 0) {
				printf("keytab error: %s >= %s\n", prev, next);
				return 0;
			}
		}
		prev = next;
	}

	printf("keytab OK\n");

	return 1;
}


struct item *keyword(char *name)
{
	int  r, l, u;
	struct item *ip;

	/*
	 *  binary search
	 */
	l = 0;
	u = (sizeof keytab/sizeof keytab[0])-1;
	while (l <= u) {
		char *key;
		i = (l+u)/2;
		ip = &keytab[i];
		key = ip->i_string;
		r = strcmp(name + (name[0] == '.'), key + (key[0] == '.'));
		if (r == 0) {
			// Do not allow ".foo" to match "foo"
			if (name[0] == '.' && key[0] != '.')
				break;

			return ip;
		}
		if (r < 0)
			u = i-1;
		else
			l = i+1;
	}

	return 0;
}

struct item *locate(char *name)
{
	struct item *ip;
	/*
	 *  hash into item table
	 */
	int hash = 0;
	char *p = name;
	while (*p) hash += *p++;
	hash %= ITEMTABLESIZE;
	ip = &itemtab[hash];

	for (;;) {
		if (ip->i_token == 0)
			break;
		if (strcmp(name, ip->i_string) == 0)
			break;
		if (++ip >= itemmax)
			ip = itemtab;
	}

	return ip;
}

/*
 *  return the token associated with the string pointed to by
 *  tempbuf.  if no token is associated with the string, associate
 *  deftoken with the string and return deftoken.
 *  in either case, cause yylval to point to the relevant
 *  symbol table entry.
 *
 *  Only keys not matching the keyexclude will be returned allowing
 *  context-dependent tokenization.  Unless they match keyinclude.
 */

int tokenofitem(int deftoken, int keyexclude, int keyinclude)
{
	char *p;
	struct item *ip;
	int  i;

#ifdef T_DEBUG
	fputs("'tokenofitem entry'	", stderr) ;
	fputs(tempbuf, stderr) ;
#endif

	ip = keyword(tempbuf);
	if (ip) {
		if (ip->i_uses & keyinclude)
			goto found;

		if (!(ip->i_uses & keyexclude))
			goto found;
	}

	// This is really my own thing rather than old zmac, but zmac
	// didn't support it and it does depend on '$' crushing a bit.
	if (zcompat) {
	    // '_' prefixed labels are local to the file
	    if (tempbuf[0] == '_') {
		    strcat(tempbuf, "$");
		    strcat(tempbuf, basename(src_name[now_in]));
	    }

	    // '.' prefixed labels are local between labels
	    if (tempbuf[0] == '.') {
		    char *p = tempbuf;
		    while (*p) p++;
		    sprintf(p, "$%d", llseq);
	    }
	}

	ip = locate(tempbuf);

	if (ip->i_token)
		goto found;

	if (!deftoken) {
		i = 0 ;
		goto token_done ;
	}
	if (++nitems > ITEMTABLESIZE-20)
		error("item table overflow");
	ip->i_string = malloc(strlen(tempbuf)+1);
	ip->i_token = deftoken;
	ip->i_uses = 0;
	strcpy(ip->i_string, tempbuf);

found:
	if (*ifptr) {
		if (ip->i_token == ENDIF_TK) {
			i = ENDIF_TK;
			goto token_done ;
		}
		if (ip->i_token == ELSE_TK) {
			/* We must only honour the ELSE if it is not
			   in a nested failed IF/ELSE */
			char forbid = 0;
			char *ifstackptr;
			for (ifstackptr = ifstack; ifstackptr != ifptr; ++ifstackptr) {
				if (*ifstackptr) {
					forbid = 1;
					break;
				}
			}
			if (!forbid) {
				i = ELSE_TK;
				goto token_done;
			}
		}
		if (ip->i_token == IF_TK) {
			if (ifptr >= ifstmax)
				error("Too many ifs");
			else *++ifptr = 1;
		}
		i = skipline(' ');
		goto token_done ;
	}
	yylval.itemptr = ip;
	i = ip->i_token;
token_done:
#ifdef T_DEBUG
	fputs("\t'tokenofitem exit'\n", stderr) ;
#endif
	return(i) ;
}


/*
 *  interchange two entries in the item table -- used by custom_qsort
 */
void interchange(int i, int j)
{
	struct item *fp, *tp;
	struct item temp;

	fp = &itemtab[i];
	tp = &itemtab[j];
	temp.i_string = fp->i_string;
	temp.i_value = fp->i_value;
	temp.i_token = fp->i_token;
	temp.i_uses = fp->i_uses;
	temp.i_scope = fp->i_scope;
	temp.i_chain = fp->i_chain;

	fp->i_string = tp->i_string;
	fp->i_value = tp->i_value;
	fp->i_token = tp->i_token;
	fp->i_uses = tp->i_uses;
	fp->i_scope = tp->i_scope;
	fp->i_chain = tp->i_chain;

	tp->i_string = temp.i_string;
	tp->i_value = temp.i_value;
	tp->i_token = temp.i_token;
	tp->i_uses = temp.i_uses;
	tp->i_scope = temp.i_scope;
	tp->i_chain = temp.i_chain;
}



/*
 *  quick sort -- used by putsymtab to sort the symbol table
 */
void custom_qsort(int m, int n)
{
	int  i, j;

	if (m < n) {
		i = m;
		j = n+1;
		for (;;) {
			do i++; while(strcmp(itemtab[i].i_string,
					itemtab[m].i_string) < 0);
			do j--; while(strcmp(itemtab[j].i_string,
					itemtab[m].i_string) > 0);
			if (i < j) interchange(i, j); else break;
		}
		interchange(m, j);
		custom_qsort(m, j-1);
		custom_qsort(j+1, n);
	}
}

int getcol()
{
	return inpptr - inpbuf;
}

/*
 *  get the next character
 */
int nextchar()
{
	int c, ch;
	unsigned char *p;
	char *getlocal();

	if (peekc != -1) {
		c = peekc;
		peekc = -1;
		return c;
	}

	if (inpptr) {
		// Double nul indicates EOF for macros
		if (expptr && inpptr[0] == '\0' && inpptr[1] == '\0') {
			inpptr = 0;
			return 0;
		}

		if (!expptr && getcol() == 0) {
			void analyze_inpbuf(void);
			linein[now_in]++;
			analyze_inpbuf();
		}

		c = *inpptr++;

		addtoline(c);

		if (*inpptr == '\0')
			inpptr = 0;

		return c;
	}

	inpptr = inpbuf;
	logcol = 0;
	p = inpbuf;

	// XXX - should check for input line overflow!

	// If invoking a macro then pull the next line from it.
	if (expptr) {
		for (;;) {
			ch = getm();

			if (ch == '\1') { /* expand argument */
				ch = getm() - 'A';
				if (ch >= 0 && ch < PARMMAX && est[ch].param) {
					strcpy(p, est[ch].param);
					p = strchr(p, '\0');
				}
			}
			else if (ch == '\2') {	/*  local symbol  */
				ch = getm() - 'A';
				if (ch >= 0 && ch < PARMMAX && est[ch].param)
					strcpy(p, est[ch].param);
				else
					strcpy(p, getlocal(ch, est[TEMPNUM].value));

				p = strchr(p, '\0');
			}
			else {
				if (ch == 0)
					break;

				*p++ = ch;

				if (ch == '\n')
					break;
			}
		}
		*p = '\0';
		p[1] = ch;
	}
	else {
		if (nextline_peek != -1) {
			*p++ = nextline_peek;
			nextline_peek = -1;
		}
		for (;;) {
			ch = getc(now_file);

			if (ch == '\r') {
				nextline_peek = getc(now_file);
				if (nextline_peek == '\n')
					nextline_peek = -1;
				else if (nextline_peek == EOF) {
					*p++ = '\n';
					nextline_peek = -1;
					ch = EOF;
					break;
				}
				ch = '\n';
			}

			if (ch == EOF)
				break;

			*p++ = ch;

			if (ch == '\n') 
				break;
		}

		*p = '\0';

		/* if EOF, check for include file */
		if (ch == EOF) {
			if (now_in) {
				fclose(fin[now_in]) ;
				free(src_name[now_in]);
				now_file = fin[--now_in];
			}
			else if (p == inpbuf)
				return 0;
	
			if (linein[now_in] < 0) {
				lstoff = 1;
				linein[now_in] = -linein[now_in];
			} else {
				lstoff = 0 ;
			}

			if (outpass) {
				if (iflist()) {
					lineout();
					fprintf(fout, "**** %s ****\n", src_name[now_in]) ;
				}
				if (bopt)
					fprintf(fbds, "%04x %04x f %s\n", dollarsign, emit_addr, src_name[now_in]);
			}

			if (p != inpbuf) {
				*p++='\n';
				*p = '\0';
			}
			else
				inpptr = 0;
		}
	}

	return nextchar();
}

char *skipspace(char *p)
{
	while (charclass[*p] == SPACE)
		p++;

	return p;
}

// Look at inpbuf and try to determine what logical column we are starting
// at.  We could put all of the work in here and keep yylex simple but for
// now we share the load.

void analyze_inpbuf(void)
{
	int cc;
	char *p, *q;
	struct item *ip;

	// Default if we find nothing to override
	logcol = 0;

	// We'll only worry about one case for now.  When you start with
	// whitespace yet there are 3 columns.  If so then we change logcol
	// to -1 to compensate.  If the 2nd column is a VERB.

	// Start by recognizing space and skipping or aborting if not found
	p = inpbuf;
	if (charclass[*p] != SPACE)
		return;

	p = skipspace(p);

	// Now skip over a token or abort if we don't find one

	cc = charclass[*p];
	if (cc != LETTER && cc != STARTER && cc != DIGIT && cc != DOLLAR)
		return;

	for (;;) {
		cc = charclass[*p];
		if (cc == LETTER || cc == STARTER || cc == DIGIT || cc == DOLLAR)
			p++;
		else
			break;
	}

	// We could skip space-separated colons now, but if we see a colon
	// the issue has been decided to do that because it is easier.
	if (*p == ':')
		return;

	p = skipspace(p);

	// Another token to skip past.
	// But we need to examine it to see if it is a verb.

	cc = charclass[*p];
	if (cc != LETTER && cc != STARTER && cc != DIGIT && cc != DOLLAR)
		return;

	q = p;
	for (;;) {
		cc = charclass[*p];
		if (cc == LETTER || cc == STARTER || cc == DIGIT || cc == DOLLAR)
			p++;
		else
			break;
	}

	// Must have space to skip over
	if (charclass[*p] != SPACE)
		return;

	// This 2nd token must be a verb.
	cc = *p;
	*p = '\0';
	ip = keyword(q);
	*p = cc;
	if (!ip || !(ip->i_uses & VERB))
		return;

	// Now skip over space.  If there's anything but a comment or end
	// of the line then we've may have 3 logical columns.
	// "ld a, 5" can throw that off, but we've done the verb check.

	p = skipspace(p);

	if (*p != ';' && *p != '\n' && *p != '\0')
		logcol--;
}


/*
 *  skip to rest of the line -- comments and if skipped lines
 */
int skipline(int ac)
{
	int  c;

	c = ac;
	while (c != '\n' && c != '\0')
		c = nextchar();
	return('\n');
}

void add_incpath(char *dir)
{
	char *p;

	if (incpath_cnt >= MAXINCPATH) {
		fprintf(stderr, "Sorry, can only handle %d include paths\n", MAXINCPATH);
		exit(1);
	}

	p = malloc(strlen(dir) + 1);
	strcpy(p, dir);

	incpath[incpath_cnt++] = dir;
}

FILE *open_incpath(char *filename, char *mode)
{
	char quote;
	int i;
	char path[1024];
	FILE *fp;

	// Due to the way parsing works the string can be specified
	// without quotes or will allow quotes but include them.  Instead
	// of fooling with the parsing I just strip the quotes.  I think
	// you can still include a file that starts with a single or double
	// quote by quoting it, but that's an awful thing to do to yourself.

	quote = *filename;
	if (quote == '"' || quote == '\'') {
		strcpy(filename, filename + 1);
		if (strrchr(filename, quote))
			*strrchr(filename, quote) = '\0';
	}

	// First look for included file in same directory as source file.

	strcpy(path, src_name[now_in]);
	*basename(path) = '\0';
	strcat(path, filename);
	fp = fopen(path, mode);
	if (fp) {
		if (note_depend && outpass)
			printf("%s\n", path);
		return fp;
	}

	for (i = 0; i < incpath_cnt; i++) {
		sprintf(path, "%s/%s", incpath[i], filename);
		fp = fopen(path, mode);
		if (fp) {
			if (note_depend && outpass)
				printf("%s\n", path);
			return fp;
		}
	}

	if (note_depend && outpass)
		printf("%s\n", filename);

	return fopen(filename, mode);
}

void version()
{
	fprintf(stderr, "zmac version " VERSION "\n");
}

//
// Print out a usage message and exit.
//
void usage(char *msg, char *param)
{
	fprintf(stderr, msg, param);
	fprintf(stderr, "\n");
	version();
	fprintf(stderr, "usage: zmac [-8bcefghijJlLmnopstz] [-I dir] file[.z]\n");
	fprintf(stderr, "other opts: --rel --mras --zmac --dep --help --doc --version\n");
	fprintf(stderr, "  zmac -h for more detail about options.\n");
	exit(1);
}

void help()
{
	version();

	fprintf(stderr, "\t--version show version number\n");
	fprintf(stderr, "\t--help\tshow this help message\n");
	fprintf(stderr, "\t-8\tuse 8080 interpretation of mnemonics\n");
	fprintf(stderr, "\t-b\tno binary (.hex,.cmd,.cas, etc.) output\n");
	fprintf(stderr, "\t-c\tno cycle counts in listing\n");
	fprintf(stderr, "\t-e\terror list only\n");
	fprintf(stderr, "\t-f\tprint if skipped lines\n");
	fprintf(stderr, "\t-g\tdo not list extra code\n");
	fprintf(stderr, "\t-h\tshow this information about options and quit\n");
	fprintf(stderr, "\t-i\tdo not list include files\n");
	fprintf(stderr, "\t-I dir\tadd 'dir' to include file search path\n");
	fprintf(stderr, "\t-j\tpromote relative jumps to absolute as needed\n");
	fprintf(stderr, "\t-J\twarn when a jump could be relative\n");
	fprintf(stderr, "\t-l\tno list\n");
	fprintf(stderr, "\t-L\tforce listing of everything\n");
	fprintf(stderr, "\t-m\tprint macro expansions\n");
	fprintf(stderr, "\t-n\tput line numbers off\n");
	fprintf(stderr, "\t-o\tlist to standard output\n");
	fprintf(stderr, "\t-p\tput out four \\n's for eject\n");
	fprintf(stderr, "\t-s\tdon't produce a symbol list\n");
	fprintf(stderr, "\t-t\toutput error count instead of list of errors\n");
	fprintf(stderr, "\t-z\tuse Z-80 interpretation of mnemonics\n");
	fprintf(stderr, "\t--dep\tlist files included\n");
	fprintf(stderr, "\t--mras\tlimited MRAS/EDAS compatibility\n");
	fprintf(stderr, "\t--rel\toutput .rel file only\n");
	fprintf(stderr, "\t--zmac\tcompatibility with original zmac\n");
	fprintf(stderr, "\t--doc\toutput documentation as HTML file\n");

	exit(0);
}

int main(int argc, char *argv[])
{
	struct item *ip;
	int  i;
	int  files;
#ifdef DBUG
	extern  yydebug;
#endif

	fout = stdout ;
	fin[0] = stdin ;
	now_file = stdin ;
	files = 0;

	// Special flag for unit testing.
	if (argc > 1 && strcmp(argv[1], "--test") == 0)
		exit(!check_keytab());

	for (i=1; i<argc; i++) {
		int skip = 0;
		if (strcmp(argv[i], "--mras") == 0) {
			mras = 1;
			continue;
		}

		if (strcmp(argv[i], "--rel") == 0) {
			relopt = 1;
			bopt = 0;
			continue;
		}

		if (strcmp(argv[i], "--zmac") == 0) {
			zcompat = 1;
			continue;
		}

		if (strcmp(argv[i], "--dep") == 0) {
			note_depend = 1;
			continue;
		}

		if (strcmp(argv[i], "--help") == 0) {
			help();
			continue;
		}

		if (strcmp(argv[i], "--doc") == 0) {
			extern void doc(void);
			doc();
			exit(0);
			continue; // not reached
		}

		if (strcmp(argv[i], "--version") == 0) {
			version();
			exit(0);
			continue; // not reached
		}

		if (*argv[i] == '-') while (*++argv[i]) {
			switch(*argv[i]) {

			case '8':	/* Equivalent to .8080 */
				default_z80 = 0;
				continue;

			case 'b':	/*  no binary  */
				bopt = 0;
				continue;

			case 'c':	/*  no cycle counts in listing */
				copt-- ;
				continue;

#ifdef DBUG
			case 'd':	/*  debug  */
				yydebug++;
				continue;
#endif

			case 'e':	/*  error list only  */
				eopt = 0;
				edef = 0;
				continue;

			case 'f':	/*  print if skipped lines  */
				fopt++;
				fdef++;
				continue;

			case 'g':	/*  do not list extra code  */
				gopt = 0;
				gdef = 0;
				continue;

			case 'h':
				help();
				continue;

			case 'i':	/* do not list include files */
				iopt = 1 ;
				continue ;

			case 'I':
				if (argv[i][1])
					add_incpath(argv[i] + 1);
				else {
					i++;
					if (i < argc)
						add_incpath(argv[i]);
					else
						usage("missing argument to -I option", 0);
				}
				skip = 1;
				break;

			case 'l':	/*  no list  */
				lopt++;
				continue;

			case 'L':	/*  force listing of everything */
				lston++;
				continue;

			case 'j':	// promote relative jumps to absolute as needed
				default_jopt = 1;
				continue;

			case 'J':	// error when JR instructions could replace JP
				default_JPopt = 1;
				continue;

			case 'm':	/*  print macro expansions  */
				mdef++;
				mopt++;
				continue;

			case 'n':	/*  put line numbers off */
				nopt-- ;
				continue;

			case 'o':	/*  list to standard output  */
				oopt++;
				continue;

			case 'p':	/*  put out four \n's for eject */
				popt-- ;
				continue;

			case 'P':	// GWP - printer style output (headers, page separation, etc.)
				printer_output = 1;
				continue;

			case 's':	/*  don't produce a symbol list  */
				sopt++;
				continue;

			case 't':	/*  output only number of errors */
				topt = 0;
				continue;

			case 'z':	/* Equivalent to .z80 */
				default_z80 = 1;
				continue;

			default:	/*  error  */
				usage("Unknown option", 0);

			}
			if (skip)
				break;
		}
		else if (files++ == 0) {
			sourcef = argv[i];
			strcpy(src, sourcef);
			if ((now_file = fopen(src, "r")) == NULL) {
				if (!*getsuffix(src))
					suffix(src, ".z");
				if ((now_file = fopen(src, "r")) == NULL)
					usage("Cannot open source file '%s'", src);
			}
			now_in = 0;
			fin[now_in] = now_file ;
			src_name[now_in] = src ;
		} else if (files)
			usage("Too many arguments", 0);
	}


	if (files == 0)
		usage("No source file", 0);

	{
		char outdir[1025];
		outpath(outdir, sourcef, 0);
#ifdef WIN32
		_mkdir(outdir);
#else
		mkdir(outdir, 0777);
#endif
	}

	if (bopt) {
		outpath(bds, sourcef, ".bds");
		fbds = fopen(bds, "w");
		if (fbds == NULL)
			error("Cannot create .bds file");

		fprintf(fbds, "binary-debuggable-source\n");

		outpath(oth, sourcef, ".cmd");
		fcmd = fopen(oth, "wb");
		if (fcmd == NULL)
			error("Cannot create .cmd file");

		outpath(oth, sourcef, ".cas");
		fcas = fopen(oth, "wb");
		if (fcas == NULL)
			error("Cannot create .cas file");

		outpath(oth, sourcef, ".lcas");
		flcas = fopen(oth, "wb");
		if (flcas == NULL)
			error("Cannot create .lcas file");

		// Tape header
		for (i = 0; i < 255; i++) {
			fputc(0, flcas);
			fputc(0x55, fcas);
		}
		fputc(0xA5, flcas);
		fputc(0x7F, fcas);
		casname(oth, sourcef);
		putcas(0x55);
		for (i = 0; i < 6; i++)
			putcas(oth[i]);

		outpath(oth, sourcef, ".cim");
		fcim = fopen(oth, "wb");
		if (fcim == NULL)
			error("Cannot create .cim file");

		outpath(oth, sourcef, ".ams");
		fams = fopen(oth, "wb");
		if (fams == NULL)
			error("Cannot create .ams file");

		outpath(bin, sourcef, ".hex");
#ifdef MSDOS
		if (( fbuf = fopen(bin, "wb")) == NULL)
#else
		if (( fbuf = fopen(bin, "w")) == NULL)
#endif
			error("Cannot create .hex file");
	}
	else if (relopt) {
		outpath(oth, sourcef, ".rel");
		frel = fopen(oth, "wb");
		if (frel == NULL)
			error("Cannot create .rel file");

		strncpy(progname, basename(sourcef), sizeof progname);
		progname[sizeof progname - 1] = '\0';
	}
	if (!lopt && !oopt) {
		outpath(listf, sourcef, ".lst");
		if ((fout = fopen(listf, "w")) == NULL)
			error("Cannot create list file");
	} else
		fout = stdout ;
	outpath(mtmp, sourcef, ".tmp");
#ifdef MSDOS
	mfile = mfopen(mtmp,"w+b") ;
#else
	mfile = mfopen(mtmp,"w+") ;
#endif
	if (mfile == NULL) {
		error("Cannot create temp file");
	}
	/*unlink(mtmp);*/

	/*
	 *  get the time
	 */
	time(&now);
	timp = ctime(&now);
	timp[16] = 0;
	timp[24] = 0;

	title = sourcef;
	/*
	 * pass 1
	 */
#ifdef DEBUG
	fputs("DEBUG-pass 1\n", stderr) ;
#endif
	clear();
	setvars();
	outpass = 0;
	yyparse();

	// GWP - errors should stop us, but the listing is very useful.

	pass2++;

	for (npass = 2; npass < MAXPASS; npass++) {
		if (passfail || npass == MAXPASS - 1)
			outpass = 1;

		if (outpass) {
			putrelcmd(RELCMD_PROGNAME);
			putrelname(progname);
		}

		ip = &itemtab[-1];
		while (++ip < itemmax) {
			// Output list of public labels.  m80 will let
			// equates and aseg values be public so we do, too.
			if (outpass && ip->i_token && (ip->i_scope & SCOPE_PUBLIC)) {
				putrelcmd(RELCMD_PUBLIC);
				putrelname(ip->i_string);
			}

			/* reset use count */
			ip->i_uses = 0 ;

			/* set macro names, equated and defined names */
			switch	(ip->i_token) {
			case MNAME:
				ip->i_token = OLDMNAME;
				break;

			case EQUATED:
				ip->i_token = WASEQUATED;
				break;

			case DEFLED:
				if (zcompat)
					ip->i_token = UNDECLARED;
				break;
			}
		}

		if (outpass) {
			// m80 outputs data size as an absolute value, but
			// code size as code segment relative.  Odd, but
			// I'll follow suit.
			putrelcmd(RELCMD_DATASIZE);
			putrelsegref(SEG_ABS, seg_size[SEG_DATA]);

			putrelcmd(RELCMD_CODESIZE);
			putrelsegref(SEG_CODE, seg_size[SEG_CODE]);
		}

		// In case we hit 'end' inside an included file
		while (now_in > 0) {
			fclose(fin[now_in]);
			free(src_name[now_in]);
			now_file = fin[--now_in];
		}
		setvars();
		fseek(now_file, (long)0, 0);

	#ifdef DEBUG
		fprintf(stderr, "DEBUG- pass %d\n", npass) ;
	#endif

		yyparse();

		if (outpass || passfail)
			break;

		if (!passretry)
			outpass = 1;
	}

	if (bopt) {
		flushbin();
		flushoth();
		putc(':', fbuf);
		if (xeq_flag) {
			puthex(0, fbuf);
			puthex(xeq >> 8, fbuf);
			puthex(xeq, fbuf);
			puthex(1, fbuf);
			puthex(255-(xeq >> 8)-xeq, fbuf);
			fprintf(fcmd, "%c%c%c%c", 2, 2, xeq, xeq >> 8);
			fflush(fcmd);
			putcas(0x78);
			putcas(xeq);
			putcas(xeq >> 8);
		} else
			for	(i = 0; i < 10; i++)
				putc('0', fbuf);
		putc('\n', fbuf);
		fflush(fbuf);
		// "Play Cas" seems to require trailing zeros to work
		// properly.  And we need to output at least one zero byte
		// to flush out the final high speed bits.
		for (i = 0; i < 6; i++)
			putcas(0);
	}

	if (relopt) {
		struct item *ip;
		// Output external symbols and value of public symbols
		for (ip = itemtab; ip < itemmax; ip++) {
			if (ip->i_token == UNDECLARED && (ip->i_scope && SCOPE_EXTERNAL)) {
				putrelcmd(RELCMD_EXTCHAIN);
				// Chain value will have top two bits set appropriately
				putrelextaddr(ip->i_chain);
				putrelname(ip->i_string);
			}
			if (ip->i_scope & SCOPE_PUBLIC)
			{
				putrelcmd(RELCMD_PUBVALUE);
				putrelsegref(ip->i_scope, ip->i_value);
				putrelname(ip->i_string);
			}
		}

		// End module, entry address if any
		putrelcmd(RELCMD_ENDMOD);
		putrelextaddr(rel_main);
		flushrel(); // byte alignment expected after end module

		// End .rel file
		putrelcmd(RELCMD_ENDPROG);
		flushrel();
	}

	if (xeq_flag == 0) {
#if WIN32
		CONSOLE_SCREEN_BUFFER_INFO inf;
		HANDLE hOut = GetStdHandle(STD_OUTPUT_HANDLE);
		GetConsoleScreenBufferInfo(hOut, &inf);
		SetConsoleTextAttribute(hOut, FOREGROUND_RED|FOREGROUND_GREEN|FOREGROUND_INTENSITY);
#endif
		fprintf(stderr, "Warning: no entry address (forgot \"end label\")\n");
		fflush(stderr);
#if WIN32
		SetConsoleTextAttribute(hOut, inf.wAttributes);
#endif
	}
	else if (bopt) {
		fprintf(fbds, "%04x e\n", xeq);
	}

	if (bopt) {
		int low = 0;
		int high = sizeof(memory) - 1;
		int chk;
		int filelen;
		char leafname[] = "FILENAMEBIN";

		while (low < sizeof(memory) && (memflag[low] & (MEM_INST | MEM_DATA)) == 0)
			low++;

		while (high >= 0 && (memflag[high] & (MEM_INST | MEM_DATA)) == 0)
			high--;

		if (high >= low)
			fwrite(memory + low, high + 1 - low, 1, fcim);

		// AMSDOS binary file output (A for Amstrad, code from zmac 1.3)
		filelen = (high + 1) - low;

		chk = 0;
		putc(0, fams);
		for (i = 0; i < 11; i++) {
			putc(leafname[i], fams);
			chk += leafname[i];
		}
		for (i = 0; i < 6; i++)
			putc(0, fams);

		putc(2, fams); // Unprotected binary
		chk += 2;
		putc(0, fams);
		putc(0, fams);
		putc(low & 0xff, fams);
		chk += low & 0xff;
		putc(low >> 8, fams);
		chk += low >> 8;
		putc(0, fams);
		putc(filelen & 0xff, fams);
		chk += filelen & 0xff;
		putc(filelen >> 8, fams);
		chk += filelen >> 8;
		putc(xeq & 0xff, fams);
		chk += xeq & 0xff;
		putc(xeq >> 8, fams);
		chk += xeq >> 8;
		for (i = 28; i < 64; i++)
			putc(0, fams);

		putc(filelen & 0xff, fams);
		chk += filelen & 0xff;
		putc(filelen >> 8, fams);
		chk += filelen >> 8;
		putc(0, fams); // this would be used if filelen > 64K
		putc(chk & 0xff, fams);
		putc(chk >> 8, fams);

		for (i = 69; i < 128; i++)
			putc(0, fams);

		if (filelen > 0)
			fwrite(memory + low, filelen, 1, fams);

		if (filelen & 0x7f)
			putc(0x1a, fams); // CP/M EOF character
	}

	if (bopt) {
		struct item *tp;

		for (tp = itemtab; tp < itemmax; tp++) {
			if (tp->i_token == LABEL)
				fprintf(fbds, "%04x a %s\n", tp->i_value, tp->i_string);
		}
	}

	if (!lopt)
		fflush(fout);
	if (writesyms)
		outsymtab(writesyms);
	if (eopt)
		erreport();
	if (!lopt && !sopt)
		putsymtab();
	if (!lopt) {
		eject();
		fflush(fout);
	}
	// GWP - some things (like balance errors in macro definitions) do
	// not show up until you use them.  So just in case we print an error
	// count here as not to confuse the programmer who is unlikely to check
	// the listing for errors if none are shown in the command window.
	if (counterr() > 0)
		fprintf(stderr, "%d errors (see listing if no diagnostics appeared here)\n", counterr());
	if (countwarn() > 0)
		fprintf(stderr, "%d warnings (see listing if no diagnostics appeared here)\n", countwarn());
	exit(counterr() > 0);
}


/*
 *  set some data values before each pass
 */
void setvars()
{
	int  i;

	peekc = -1;
	inpptr = 0;
	nextline_peek = -1;
	raw = 0;
	linein[now_in] = linecnt = 0;
	exp_number = 0;
	emitptr = emitbuf;
	lineptr = linebuf;
	ifptr = ifstack;
	expifp = expif;
	*ifptr = 0;
	dollarsign = 0;
	emit_addr = 0;
	olddollar = 0;
	oldothdollar = 0;
	phaseflag = 0;
	for (i=0; i<FLAGS; i++) err[i] = 0;
	tstates = 0;
	ocf = 0;
	llseq = 0;
	passfail = 0;
	passretry = 0;
	njrpromo = 0;
	jopt = default_jopt;
	JPopt = default_JPopt;
	strcpy(modstr, "@@@@");
	segment = SEG_CODE;
	memset(seg_pos, 0, sizeof(seg_pos));
	memset(seg_size, 0, sizeof(seg_size));
	segchange = 0;
	z80 = default_z80;
}

//
// Clear out cycle counts and memory.
//

void clear()
{
	int i;

	for (i = 0; i < sizeof(memory) / sizeof(memory[0]); i++)
	{
		memory[i] = 0;
		memflag[i] = 0;
		tstatesum[i] = 0;
	}
}

void setmem(int addr, int value, int type)
{
	value &= 0xff;
	if (memory[addr] != value) {
		if (outpass) {
			if (!passfail)
				err[pflag]++;
		}
		else
			passretry = 1;
	}

	memory[addr] = value;
	memflag[addr] |= type;
}

/*
 *  print out an error message and die
 */
void error(char *as)
{
	*linemax = 0;
	fprintf(fout, "%s\n", linebuf);
	fflush(fout);
	fprintf(stderr, "%s\n", as) ;
	exit(1);
}


/*
 *  output the symbol table
 */
void putsymtab()
{
	struct item *tp, *fp;
	int  i, j, k, t, rows;
	char c, c1, seg = ' ';
	int numcol = printer_output ? 4 : 1;

	if (!nitems)
		return;

	/* compact the table so unused and UNDECLARED entries are removed */
	tp = &itemtab[-1];
	for (fp = itemtab; fp<itemmax; fp++) {
		if (fp->i_token == UNDECLARED && !(fp->i_scope && SCOPE_EXTERNAL)) {
			nitems--;
			continue;
		}
		if (fp->i_token == 0)
			continue;
		tp++;
		if (tp != fp) {
			tp->i_string = fp->i_string;
			tp->i_value = fp->i_value;
			tp->i_token = fp->i_token;
			tp->i_uses = fp->i_uses;
			tp->i_scope = fp->i_scope;
			tp->i_chain = fp->i_chain;
		}
	}

	tp++;
	tp->i_string = "{";	/* } */

	/*  sort the table */
	custom_qsort(0, nitems-1);

	title = "**  Symbol Table  **";

	rows = (nitems+numcol-1) / numcol;
	if (rows+5+line > 60)
		eject();
	lineout();
	fprintf(fout,"\n\n\nSymbol Table:\n\n") ;
	line += 4;

	for (i=0; i<rows; i++) {
		for(j=0; j<numcol; j++) {
			k = rows*j+i;
			if (k < nitems) {
				tp = &itemtab[k];
				t = tp->i_token;
				c = ' ' ;
				if (t == EQUATED || t == DEFLED)
					c = '=' ;
				if (tp->i_uses == 0)
					c1 = '+' ;
				else
					c1 = ' ' ;

				// GWP - decided I don't care about uses
				// even if it were accurate.
				// TODO: Should use maxsymbol size in there,
				// but makes output harder to read.

				fprintf(fout, "%-15s%c", tp->i_string, c);

				if (relopt)
					seg = " '\"!"[tp->i_scope & SCOPE_SEGMASK];

				if (tp->i_value >> 16)
					fprintf(fout, "%8x%c", tp->i_value, seg);
				else
					fprintf(fout, "%4x%c    ", tp->i_value & 0xffff, seg);

				if (tp->i_scope & SCOPE_EXTERNAL)
					fprintf(fout, " (extern)");

				if (tp->i_scope & SCOPE_PUBLIC)
					fprintf(fout, " (public)");
			}
		}
		lineout();
		putc('\n', fout);
	}
}




/*
 *  put out error report
 */
void erreport()
{
	int i, numerr, numwarn;

	if (line > 49) eject();
	lineout();
	numerr = 0;
	for (i=0; i<FIRSTWARN; i++) numerr += keeperr[i];
	numwarn = 0;
	for (i = FIRSTWARN; i < FLAGS; i++) numwarn += keeperr[i];
	if (numerr || numwarn) {
		fputs("\n\n\nError + Warning report:\n\n", fout);
		fprintf(fout, "%6d errors\n", numerr);
		fprintf(fout, "%6d warnings\n", numwarn);
		line += 6;
	} else {
		fputs("\n\n\nStatistics:\n", fout);
		line += 3;
	}

	for (i=0; i<FLAGS; i++)
		if (keeperr[i]) {
			lineout();
			fprintf(fout, "%6d %c -- %s %s\n",
				keeperr[i], errlet[i], errname[i],
				i < FIRSTWARN ? "error" : "warnings");
		}

	if (line > 52) eject();
	lineout();
	fprintf(fout, "\n%6d\tpasses\n", npass);
	fprintf(fout, "%6d\tjr promotions\n", njrpromo);
	fprintf(fout, "%6d\tsymbols\n", nitems);
	fprintf(fout, "%6d\tbytes\n", nbytes);
	line += 4;
	if (mfptr) {
		if (line > 53) eject();
		lineout();
		fprintf(fout, "\n%6d\tmacro calls\n", exp_number);
		fprintf(fout, "%6d\tmacro bytes\n", mfptr);
		fprintf(fout, "%6d\tinvented symbols\n", invented/2);
		line += 3;
	}
}

/*
 * count errors (GWP - added to set exit code)
 */
int counterr()
{
	int i, numerr = 0;
	for (i=0; i<FIRSTWARN; i++) numerr += keeperr[i];
	return numerr;
}

// Count warnings
int countwarn()
{
	int i, numwarn = 0;
	for (i = FIRSTWARN; i < FLAGS; i++)
		numwarn += keeperr[i];
	return numwarn;
}

char *mlook;

int nextmac()
{
	int ch;

	if (mlook) {
		if (*mlook)
			ch = *mlook++;
		else
			mlook = 0;
	}

	if (!mlook)
		ch = nextchar();

	return ch;
}

/*
 *  lexical analyser for macro definition
 */
void mlex(char *look)
{
	char  *p;
	int  c;
	int  t;
	int octo;
	int zcompat_save;
	char symbuf[TEMPBUFSIZE];

	/*
	 *  move text onto macro file, changing formal parameters
	 */
#ifdef	M_DEBUG
	fprintf(stderr,"enter 'mlex'\n") ;
#endif
	inmlex++;

	mlook = look;

	c = nextmac();
for (;;) {
	octo = 0;
	if (c == '#') {
		c = nextmac();
		if (charclass[c] != STARTER && charclass[c] != LETTER) {
			putm('#');
			continue;
		}
		octo = 1;
	}

	switch(charclass[c]) {

	case DIGIT:
		while (numpart[c]) {
			putm(c);
			c = nextmac();
		}
		continue;

	case STARTER:
	case LETTER:
		t = 0;
		p = symbuf;
		do {
			if (p >= tempmax) {
				*tempmax = '\0';
				printf("was parsing '%s' in macro definition\n", tempbuf);
				error(symlong);
			}
			*p++ = c;
			if (t < MAXSYMBOLSIZE)
				tempbuf[t++] = (c >= 'A' && c <= 'Z')  ?
					c+'a'-'A' : c;
			c = nextmac();
		} while	(charclass[c]==LETTER || charclass[c]==DIGIT || charclass[c]==STARTER);

		tempbuf[t] = 0;
		*p++ = '\0';
		p = symbuf;
		// Bit of dancing to allow VERBs as parameters.
		// We could allow anything if parm.element accepted more.
		// Can't allow token expansion at this point.  Doesn't make
		// sense and could screw things up.
		zcompat_save = zcompat;
		zcompat = 0;
		t = tokenofitem(0, 0, 0);
		if (t != ENDM)
			t = tokenofitem(0, VERB, 0);

		zcompat = zcompat_save;

		if (t == MPARM) {
			if (octo != yylval.itemptr->i_scope)
				t = MPARM + 1;
			else
				octo = 0;
		}

		if (octo) { putm('#'); octo = 0; }

		if (t != MPARM) {
			for (p = symbuf; *p; p++)
				putm(*p);
		}
		else {
			if (*(yylval.itemptr->i_string) == '?' || yylval.itemptr->i_chain)
				putm('\2');
			else
				putm('\1');
			putm(yylval.itemptr->i_value + 'A');
		}
		if (t == ENDM) goto done;
		continue;

	case F_END:
		printf("Warning: macro went until end of file.\n");
		if (expptr) {
			popsi();
			c = nextmac();
			continue;
		}

		goto done;

	default:
		if (c == '\n') {
			linecnt++;
		}
		if (c != '\1' && c != '`') putm(c);
		c = nextmac();
	}
}

	/*
	 *  finish off the file entry
	 */
done:
	while(c != EOF && c != '\n' && c != '\0') c = nextmac();
	linecnt++;
	putm('\n');
	putm('\n');
	putm(0);

	for (c=0; c<ITEMTABLESIZE; c++)
		if (itemtab[c].i_token == MPARM) {
			itemtab[c].i_token = UNDECLARED;
		}
	inmlex = 0;
#ifdef	M_DEBUG
	fprintf(stderr,"exit 'mlex'\n") ;
#endif
}



/*
 *  lexical analyser for the arguments of a macro call
 */
int getarg()
{
	int c;
	char *p;
	static int comma;
	int quote;

	*tempbuf = 0;
	yylval.cval = tempbuf;
	while(charclass[c = nextchar()] == SPACE);

	switch(c) {

	case '\0':
		popsi();
	case '\n':
	case ';':
		comma = 0;
		return(skipline(c));

	case ',':
		if (comma) {
			comma = 0;
			return(',');
		}
		else {
			comma++;
			return(ARG);
		}

	case '\'':
	case '\"':
		quote = c;
		p = tempbuf;
		if (!zcompat)
			*p++ = c;

		do {
			c = nextchar();
			if (c == '\0' || c == '\n') {
				peekc = c;
				*p = 0;
				err[bflag]++;
				return ARG;
			}
			else if (c == quote) {
				if ((c = nextchar()) != quote) {
					if (!zcompat)
						*p++ = quote;
					peekc = c;
					*p = '\0';
					comma++;
					return ARG;
				}
			}
			else
				*p++ = c;
		} while (p < tempmax);
		*tempmax = '\0';
		printf("was parsing macro argument '%s'\n", tempbuf);
		error(symlong);
		return 0; // not reached

	default:  /* unquoted string */
		p = tempbuf;
		peekc = c;
		do switch(c = nextchar()) {
			case '\0':
			case '\n':
			case '\t':
			case ' ':
			case ',':
				peekc = c;
				*p = '\0';
				comma++;
				return(ARG);
			default:
				*p++ = c;
		} while (p < tempmax);
		error("macro argument too long");
		return 0; // not reached
	}
}





/*
 *  add a suffix to a string
 */
void suffix(char *str, char *suff)
{
	strcpy(getsuffix(str), suff);
}

char *basename(char *filename)
{
	char *base, *p;

	base = filename;
	for (p = filename; *p; p++) {
		if (*p == '/' || *p == '\\') {
			base = p + 1;
		}
	}

	return base;
}

char *getsuffix(char *str)
{
	char *suffix = 0;
	str = basename(str);
	for (; *str; str++) {
		if (*str == '.')
			suffix = str;
	}
	return suffix ? suffix : str;
}

// Construct output file given input path.
// Essentially files for "file.z" are sent to "zout/file.suffix".
// And for "dir/file.z" they are "zout/file.suffix"

void outpath(char *out, char *src, char *suff)
{
	strcpy(out, "zout");
	if (!suff)
		return;

	strcat(out, "/");
	strcat(out, basename(src));
	suffix(out, suff);
}


/*
 *  put out a byte to the macro file, keeping the offset
 */
void putm(int c)
{
	mfptr++;
	mfputc(c, mfile);
}



/*
 *  get a byte from the macro file
 */
int getm()
{
	int ch;

	floc++;
	ch = mfgetc(mfile);
	if (ch == EOF) {
		ch = 0;
		fprintf(stderr, "bad macro read\n");
	}
	return ch;
}



/*
 *  pop standard input
 */
void popsi()
{
	int  i;

	for (i=0; i<PARMMAX; i++) {
		if (est[i].param) free(est[i].param);
	}
	floc = est[FLOC].value;
	free(est);
	expptr--;
	est = expptr ? expstack[expptr-1] : 0;
	mfseek(mfile, (long)floc, 0);
	if (lineptr > linebuf) lineptr--;
}



/*
 *  return a unique name for a local symbol
 *  c is the parameter number, n is the macro number.
 */

char *getlocal(int c, int n)
{
	static char local_label[10];

	invented++;
	if (c >= 26)
		c += 'a' - '0';
	sprintf(local_label, "?%c%04d", c+'a', n) ;
	return(local_label);
}

char *getmraslocal()
{
	static char mras_local[32];
	char *p = mras_local + sizeof mras_local - 1;
	int n = est[TEMPNUM].value;

	*p = '\0';
	for (; n > 0; n /= 26)
		*--p = 'A' + n % 26;


	return p;
}


/*
 *  read in a symbol table
 */
void insymtab(char *name)
{
	struct stab *t;
	int  s, i;
	FILE *sfile;

	t = (struct stab *) tempbuf;
	if (!(sfile = fopen(name, "rb")))
		return;
	fread((char *)t, 1, sizeof *t, sfile);
	if (t->t_value != SYMMAJIC)
		return;

	s = t->t_token;
	for (i=0; i<s; i++) {
		fread((char *)t, 1, sizeof *t, sfile);
		if (tokenofitem(UNDECLARED, 0, 0) != UNDECLARED)
			continue;
		yylval.itemptr->i_token = t->t_token;
		yylval.itemptr->i_value = t->t_value;
		if (t->t_token == MACRO)
			yylval.itemptr->i_value += mfptr;
	}

	while ((s = fread(tempbuf, 1, TEMPBUFSIZE, sfile)) > 0) {
		mfptr += s;
		mfwrite(tempbuf, 1, s, mfile) ;
	}
	fclose(sfile);
}



/*
 *  write out symbol table
 */
void outsymtab(char *name)
{
	struct stab *t;
	struct item *ip;
	int  i;
	FILE *sfile;

	t = (struct stab *) tempbuf;
	if (!(sfile = fopen(name, "wb")))
		return;
	for (ip=itemtab; ip<itemmax; ip++) {
		if (ip->i_token == UNDECLARED) {
			ip->i_token = 0;
			nitems--;
		}
	}

	copyname(title, (char *)t);
	t->t_value = SYMMAJIC;
	t->t_token = nitems;
	fwrite((char *)t, 1, sizeof *t, sfile);

	for (ip=itemtab; ip<itemmax; ip++) {
		if (ip->i_token != 0) {
			t->t_token = ip->i_token;
			t->t_value = ip->i_value;
			copyname(ip->i_string, (char *)t);
			fwrite((char *)t, 1, sizeof *t, sfile);
		}
	}

	mfseek(mfile, (long)0, 0);
	while((i = mfread(tempbuf, 1, TEMPBUFSIZE, mfile) ) > 0)
		fwrite(tempbuf, 1, i, sfile);

	fclose(sfile);
}



/*
 *  copy a name into the symbol file
 */
void copyname(char *st1, char *st2)
{
	char  *s1, *s2;
	int  i;

	i = (MAXSYMBOLSIZE+2) & ~01;
	s1 = st1;
	s2 = st2;

	while((*s2++ = *s1++)) i--;		/* -Wall-ishness :-) -RJM */
	while(--i > 0) *s2++ = '\0';
}

/* get the next source file */
void next_source(char *sp)
{

	if(now_in == NEST_IN -1)
		error("Too many nested includes") ;
	if ((now_file = open_incpath(sp, "r")) == NULL) {
		char ebuf[1024] ;
		sprintf(ebuf,"Can't open include file: %s", sp) ;
		error(ebuf) ;
	}
	if (outpass && iflist()) {
		lineout() ;
		fprintf(fout, "**** %s ****\n",sp) ;
	}

	if (outpass && bopt)
		fprintf(fbds, "%04x %04x f %s\n", dollarsign, emit_addr, sp);

	/* save the list control flag with the current line number */
	if (lstoff)
		linein[now_in] = - linein[now_in] ;

	/* no list if include files are turned off */
	lstoff |= iopt ;

	/* save the new file descriptor. */
	fin[++now_in] = now_file ;
	/* start with line 0 */
	linein[now_in] = 0 ;
	/* save away the file name */
	src_name[now_in] = malloc(strlen(sp)+1) ;
	strcpy(src_name[now_in],sp) ;
}

int phaseaddr(int addr)
{
	if (!phaseflag)
		return addr;

	if (addr < phbegin || addr > dollarsign) {
		err[vflag]++;
		if (pass2)
			fprintf(stderr, "$%04x outside current phase area\n", addr);
		return 0;
	}

	return phdollar + (addr - phbegin);
}

// Include contents of named file as binary data.
void incbin(char *filename)
{
	FILE *fp = open_incpath(filename, "rb");
	int ch;
	int start = dollarsign;
	int last = start;

	if (!fp) {
		char ebuf[1024];
		sprintf(ebuf, "Can't binary include file: %s", filename);
		error(ebuf);
		return;
	}

	// Avoid emit() because it has a small buffer and it'll spam the listing.
	while ((ch = fgetc(fp)) != EOF) {
		if (segment == SEG_CODE)
			setmem(emit_addr, ch, MEM_DATA);
		emit_addr++;
		emit_addr &= 0xffff;
		last = dollarsign;
		dollarsign++;
		dollarsign &= 0xffff;

		putbin(ch);
		putrel(ch);
	}

	fclose(fp);

	// Do our own list() work as we emit bytes manually.

	addtoline('\0');

	if (outpass && iflist()) {
		lineout();

		if (nopt)
			fprintf(fout, "%4d:", linein[now_in]);

		if (copt)
		        fprintf(fout, nopt ? "%5s-" : "%4s-", "");

		if (nopt || copt)
			fprintf(fout, "\t");

		puthex(start >> 8, fout);
		puthex(start, fout);
		fprintf(fout, " .. ");
		puthex(last >> 8, fout);
		puthex(last, fout);

		putc('\t', fout);

		fputs(linebuf, fout);

		lineptr = linebuf;
	}
}

void dc(int count, int value)
{
	int start = dollarsign;

	// Avoid emit() because it has a small buffer and it'll spam the listing.
	while (count-- > 0) {
		if (segment == SEG_CODE)
			setmem(emit_addr, value, MEM_DATA);
		emit_addr++;
		emit_addr &= 0xffff;
		dollarsign++;
		dollarsign &= 0xffff;

		putbin(value);
		putrel(value);
	}

	// Do our own list() work as we emit bytes manually.

	addtoline('\0');

	if (outpass && iflist()) {
		lineout();

		if (nopt)
			fprintf(fout, "%4d:", linein[now_in]);

		if (copt)
		        fprintf(fout, nopt ? "%5s-" : "%4s-", "");

		if (nopt || copt)
			fprintf(fout, "\t");

		puthex(start >> 8, fout);
		puthex(start, fout);
		fprintf(fout, " .. ");
		puthex((dollarsign - 1) >> 8, fout);
		puthex((dollarsign - 1), fout);
		putc(' ', fout);
		puthex(value, fout);
		putc('\t', fout);
		fputs(linebuf, fout);
		lsterr2(1);

		lineptr = linebuf;
	}
	else
		lsterr1();
}

void advance_segment(int step)
{
	int top = seg_pos[segment] += step;
	seg_pos[segment] &= 0xffff;
	if (top >= 0x10000)
		top = 0xffff;

	if (top > seg_size[segment])
		seg_size[segment] = top;
}

void expr_reloc_check(struct expr *ex)
{
	if (!relopt) return;
	if (ex->e_scope & (SCOPE_EXTERNAL | SCOPE_NORELOC))
		err[rflag]++;
}

void expr_number_check(struct expr *ex)
{
	if (!relopt) return;
	expr_reloc_check(ex);
	if (ex->e_scope & SCOPE_SEGMASK)
		err[rflag]++;
}

void expr_scope_same(struct expr *ex1, struct expr *ex2)
{
	if (!relopt) return;
	if ((ex1->e_scope & SCOPE_SEGMASK) != (ex2->e_scope & SCOPE_SEGMASK))
		err[rflag]++;
}

void expr_word_check(struct expr *ex)
{
	if (ex->e_value < -32768 || ex->e_value > 65535) {
		err[vflag]++;
	}
}

int is_number(struct expr *ex)
{
	return ex && (ex->e_scope & ~SCOPE_PUBLIC) == 0;
}

int is_external(struct expr *ex)
{
	return ex && (ex->e_scope & SCOPE_EXTERNAL) && !ex->e_left && !ex->e_right &&
		ex->e_item;
}

struct expr *expr_alloc(void)
{
	struct expr *ex = malloc(sizeof *ex);

	ex->e_value = 0;
	ex->e_scope = 0;
	ex->e_token = 0;
	ex->e_item = 0;
	ex->e_left = 0;
	ex->e_right = 0;

	return ex;
}

struct expr *expr_num(int value)
{
	struct expr *ex = expr_alloc();
	ex->e_value = value;
	ex->e_token = '0';

	return ex;
}

// Expression consruction for operators that subtract/compare.
// They produce a valid result if operating on numbers in the same segment.
struct expr *expr_op_sc(struct expr *left, int token, struct expr *right, int value)
{
	struct expr *ex = expr_op(left, token, right, value);

	if (!(ex->e_scope & SCOPE_EXTERNAL) &&
		((left->e_scope ^ right->e_scope) & SCOPE_SEGMASK) == 0)
	{
		// Result relocatable and a simple number
		ex->e_scope &= ~(SCOPE_NORELOC | SCOPE_SEGMASK);
	}

	return ex;
}

struct expr *expr_op(struct expr *left, int token, struct expr *right, int value)
{
	struct expr *ex = expr_alloc();

	ex->e_value = value;
	ex->e_token = token;
	ex->e_left = left;
	ex->e_right = right;

	// Combining two numbers will be fine as long as they're not
	// flagged as external or already not relocatable.  In which case
	// it is up to the particular operator to allow the value
	// to become valid.

	ex->e_scope = left->e_scope;
	if (left->e_scope & SCOPE_SEGMASK)
		ex->e_scope |= SCOPE_NORELOC;
	if (right) {
		ex->e_scope |= right->e_scope;
		if (right->e_scope & SCOPE_SEGMASK)
			ex->e_scope |= SCOPE_NORELOC;
	}

	return ex;
}

void expr_free(struct expr *ex)
{
	if (!ex)
		return;

	expr_free(ex->e_left);
	expr_free(ex->e_right);
	free(ex);
}

int synth_op(struct expr *ex, int gen)
{
	if (ex->e_token == '&' && is_number(ex->e_right) &&
		ex->e_right->e_value == 255)
	{
		if (gen) {
			extend_link(ex->e_left);
			putrelop(RELOP_LOW);
			return 1;
		}
		return can_extend_link(ex->e_left);
	}

	return 0;
}

int link_op(struct expr *ex)
{
	if (!ex)
		return 0;

	switch (ex->e_token) {
	case HIGH: return RELOP_HIGH;
	case LOW: return RELOP_LOW;
	case '~': return RELOP_NOT;
	case '-': return !ex->e_right ? RELOP_NEG : RELOP_SUB;
	case '+': return RELOP_ADD;
	case '*': return RELOP_MUL;
	case '/': return RELOP_DIV;
	case '%': return RELOP_MOD;
	default: return 0;
	}
}

int can_extend_link(struct expr *ex)
{
	if (!ex)
		return 1;

	// If we have a value available then we're good.
	if (!(ex->e_scope & SCOPE_NORELOC))
		return 1;

	// Might be able to synthesize the operation.
	if (synth_op(ex, 0))
		return 1;

	// Otherwise, the operator must be supported and the children
	// must be linkable.

	return link_op(ex) && can_extend_link(ex->e_left) && can_extend_link(ex->e_right);
}

void extend_link(struct expr *ex)
{
	int op;

	if (!ex)
		return;

	if (synth_op(ex, 1))
		return;

	extend_link(ex->e_left);
	extend_link(ex->e_right);

	op = link_op(ex);
	if (op) {
		putrelop(op);
		return;
	}

	putrelcmd(RELCMD_EXTLINK);

	if (is_external(ex)) {
		char *str = ex->e_item->i_string;
		int len = strlen(str);
		if (len > 6)
			len = 6;
		putrelbits(3, 1 + len);
		putrelbits(8, 'B');
		while (len-- > 0) {
			int ch = *str++;
			if (ch >= 'a' && ch <= 'z')
				ch -= 'a' - 'A';
			putrelbits(8, ch);
		}
	}
	else {
		putrelbits(3, 4);
		putrelbits(8, 'C');
		putrelbits(8, ex->e_scope & SCOPE_SEGMASK);
		putrelbits(8, ex->e_value);
		putrelbits(8, ex->e_value >> 8);
	}
}

void putrelop(int op)
{
	putrelcmd(RELCMD_EXTLINK);

	putrelbits(3, 2);
	putrelbits(8, 'A');
	putrelbits(8, op);
}
