/*
*/
/* $RCSfile: MKCSINC.C $
   $Locker:  $	$Name:  $	$State: Exp $

	Creates the include files to save/restore a charshape structure.

*/

#include <stdio.h>

#ifndef lint
static char rcsid[] = 
	"$Id: MKCSINC.C 1.1 1997/01/19 07:19:29 ska Exp $";
#endif

char buf[3092];

main(void)
{	char *p;
	int map;
	unsigned cnt;
	FILE *get, *put, *def;

	map = cnt = 0;

	if(!(get = fopen("getcs.inc", "wt")))
		abort("Cannot open \"getcs.in\" for writing");
	if(!(put = fopen("putcs.inc", "wt")))
		abort("Cannot open \"putcs.in\" for writing");
	if(!(def = fopen("defcs.inc", "wt")))
		abort("Cannot open \"defcs.in\" for writing");

	while(fgets(buf, sizeof(buf), stdin)) {
		if(map) {
			if(memcmp(buf, "};", 2) == 0) break;
			if(!(p = strchr(buf, ';'))) continue;
			fprintf(def, "%s\n", buf);
			*p = '\0';
			while(--p >= buf && !isspace(*p));
			++p;
			fprintf(get, "%s = cs->%s;\n", p, p);
			fprintf(put, "cs->%s = %s;\n", p, p);
		}
		else if(strstr(buf, "##!mkcsinc"))
			map = 1;
	}
	fclose(put);
	fclose(get);
	fclose(def);
}
