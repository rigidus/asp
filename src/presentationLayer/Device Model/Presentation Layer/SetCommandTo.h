/*
 * SetCommandTo.h
 *
 *  Created on: 23 февр. 2016 г.
 *      Author: drema
 */

#ifndef SETCOMMANDTO_H_
#define SETCOMMANDTO_H_

#include <string>

namespace setCommandTo
{

void Device(uint32_t txid, std::string device, std::string command,
			std::string parameters, std::string adresat);

void Client(uint32_t eventFlag, std::string device, std::string command, std::string parameters);

}



#endif /* SETCOMMANDTO_H_ */
