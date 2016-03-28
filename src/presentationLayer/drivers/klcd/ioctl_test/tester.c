/*  This work is licensed under a Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
 *
 *  Copyright(c) 2014 Hong Moon 	All Rights Reserved
 */

/* description: an ioctl test program.

		a user level test program to check the ioctl functionality ofthe implemented Linux device driver, which controls a 16x2  			character LCD (with HD44780 LCD controller) with 4 bit mode.
  		
		The LCD is interfaced with a micro-controller using GPIO pins.

		(Tested on Linux 3.8.13)

   name:	Hong Moon (hsm5xw@gmail.com)
   date:	2014-Sept
   platform:	Beaglebone Black
*/


#include <stdio.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <sys/ioctl.h>
#include <stdlib.h>
#include <time.h>
 
#include "driver.h"

int main ( int argc, char *argv[] )
{
	struct ioctl_mesg msg;
	const char *ioctl_command;
	char command;
	int fd;

	char * pEnd1;
	char * pEnd2;

	char tmpstr[5];
	char counter = 0;
	int i, j, k;

    	if ( argc != 1 ) {
        	printf( "Usage: %s, no parameters expected\n\n", argv[0] );
		return -1;
    	}
	
while ( 1 )
{
	//  ************ argument setting ******************************************************
//	ioctl_command 	= argv[1];
//	printf("ioctl command debug: %c \n", *ioctl_command);

//	memset(  msg.kbuf, ' ', sizeof(char) * MAX_BUF_LENGTH );
//	strncpy( msg.kbuf, argv[2],  MAX_BUF_LENGTH);
//	msg.kbuf[MAX_BUF_LENGTH-1] = '\0';  // add null terminator to prevent buffer overflow

	msg.lineNumber   = 0; //(unsigned int) strtoul(argv[3],&pEnd1,10);
	msg.nthCharacter = 0; //(unsigned int) strtoul(argv[4],&pEnd2,10);

	//****************************************************************************************   

	counter = 0;
	for (k = 0; k < 4; j++)
	{
		memset(  msg.kbuf, ' ', sizeof(char) * MAX_BUF_LENGTH );
		msg.kbuf[MAX_BUF_LENGTH-1] = '\0';  // add null terminator to prevent buffer overflow
		for (j = 0; j < 4; j++)
		{
			sprintf(tmpstr, "%d", counter);
			memcpy( (msg.kbuf + (j*20)), tmpstr, strlen(tmpstr));
			for (i = 0; i < 16; i++)
			{
				if (counter != 0)
				{
					msg.kbuf[(j*20) + 4 + i] = counter;
				}
				counter++;
			}
		}
		fd = open("/dev/klcd", O_WRONLY | O_NDELAY);
		if(fd < 0)
		{
			printf("[User level Debug] ERR: Unable to open klcd \n");
			return -1;
		}

		if( ioctl( fd, (unsigned int) IOCTL_PRINT_ON_FIRSTLINE, &msg) < 0)
			perror("[ERROR] IOCTL_PRINT_ON_FIRSTLINE \n");			

		close(fd);	
		sleep(5);
	}

//	printf("KLCD User level Test Program \n");
}
}
