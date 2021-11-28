/*
*/
/* $RCSfile: MKCMDS.C $
   $Locker:  $	$Name:  $	$State: Exp $

	Updates the command names and their macros.

*/

#include <stdio.h>

#ifndef lint
static char rcsid[] = 
	"$Id: MKCMDS.C 1.3 1997/01/19 07:19:29 ska Exp $";
#endif

char buf[3092];

main(void)
{	char *p;
	int map;
	unsigned cnt;

	map = cnt = 0;

	puts("char *cmds[] = {");
	while(fgets(buf, sizeof(buf), stdin)) {
		if(map) {
			if(!(p = strtok(buf, " \t\r\n"))) continue;
			if(memcmp(buf, "*/", 2) == 0) break;
			printf("#define %s %u\n", p, cnt++);
			printf("\t\t\"%s\",\n", p);
		}
		else if(strstr(buf, "##!mkcmds"))
			map = 1;
	}
	puts("NULL };");
}
