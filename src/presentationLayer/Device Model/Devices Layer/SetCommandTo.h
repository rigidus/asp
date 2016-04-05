//============================================================================
// Name        : SetCommandTo.h
// Author      : aav
// Created on  : 23 февр. 2016 г.
// Version     : v.0.1
// Copyright   : Non nobis, Domine, non nobis, sed nomini tuo da gloriam.
// Description : Facade for device and transaction manager.
// All exchange must use that interface.
//============================================================================

#ifndef SETCOMMANDTO_H_
#define SETCOMMANDTO_H_

#include <string>
#include <sstream>
#include "SeverityLevels.h"

namespace SetTo
{

enum CommandType { Transaction, Event };

void Device(uint32_t txid, std::string device, std::string command,
			std::string parameters, std::string adresat);

void Client(CommandType eventFlag, std::string device, std::string command, std::string parameters);

void Manager(std::string device);

void sendErrorToClient(std::stringstream& errorText);

void CommonLog(severity_level lvl, const std::string message);
void LocalLog(const std::string device, severity_level lvl, const std::string message);


}



#endif /* SETCOMMANDTO_H_ */
