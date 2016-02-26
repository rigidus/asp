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

enum CommandType { Transaction, Event };

void Device(uint32_t txid, std::string device, std::string command,
			std::string parameters, std::string adresat);

void Client(CommandType eventFlag, std::string device, std::string command, std::string parameters);

void Manager(std::string device);

}



#endif /* SETCOMMANDTO_H_ */
