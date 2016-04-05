//============================================================================
// Name        : Help.h
// Author      : aav
// Created on  : 31 марта 2016 г.
// Version     : v.0.1
// Copyright   : Non nobis, Domine, non nobis, sed nomini tuo da gloriam.
// Description : --help provider
//============================================================================

#ifndef HELP_H_
#define HELP_H_

#include "Logger.h"

namespace Help
{

	int32_t Parser(char* argv, char* argp)
	{
		std::string argval(argv);
		int32_t len = 0;

		if ( argval == "--help" || argval == "-h" )
		{
			std::cout << "Help for Devices Layer:" << std::endl;
			std::cout << " --help | -h - This message" << std::endl;
			std::cout << " --loglevel <level> | -l <level> - Level of log messages that writes to log file" << std::endl;

			// Print severity levels
			int32_t lvl = 0;
			std::cout << " log level list: " << (severity_level) lvl;
			for ( lvl+=1 ;lvl < severity_level::last; lvl++)
			{
				std::cout << ", " << (severity_level) lvl;
			}
			std::cout << std::endl;

			std::cout << " --logsize <size> | -s <size> - Size of one log file (release only)" << std::endl;
			std::cout << " --logname <filename> | -f <filename> - Root of file name for log file" << std::endl;

			len = 1;
		}
		return len;
	}

}


#endif /* HELP_H_ */
