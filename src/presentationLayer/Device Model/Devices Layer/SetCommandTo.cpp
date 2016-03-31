/*
 * SetCommandTo.cpp
 *
 *  Created on: 23 февр. 2016 г.
 *      Author: drema
 */

#include "SetCommandTo.h"
#include "CDeviceManager.h"
#include "Logger.h"

void SetTo::sendErrorToClient(std::stringstream& errorText)
{
	std::stringstream jsonError;
	jsonError << "{ \"error\":\"" << errorText.str() << "\"}";

	SetTo::Client(SetTo::Event, BsnsLogic::s_abstractName, "", jsonError.str());
}

void SetTo::Device(uint32_t txid, std::string device, std::string command,
			std::string parameters, std::string adresat)
{
	CDeviceManager* devMgr = CDeviceManager::deviceManager();
	if (devMgr)
	{
		std::cout << "setCommandTo::Device: " << device << " from " << adresat << std::endl;
		devMgr->setCommandToDevice(txid, device, command, parameters, adresat);
	}
	else
	{
		// отправить назад сообщение, что устройства еще не настроены
		std::stringstream error;
		error << "ERROR! setCommandTo::Device: Manager not found.";
		sendErrorToClient(error);

		std::cout << error.str() << std::endl;
	}

}

void SetTo::Client(CommandType eventFlag, std::string device, std::string command, std::string parameters)
{
	CDeviceManager* devMgr = CDeviceManager::deviceManager();
	if (devMgr)
	{
		std::cout << "setCommandTo::Client: Set command to client from " << device << std::endl;
		devMgr->setCommandToClient(eventFlag, device, command, parameters);
	}
	else
	{
		// отправить назад сообщение, что устройства еще не настроены
		std::stringstream error;
		error << "ERROR! setCommandTo::Device: Manager not found.";
		sendErrorToClient(error);

		std::cout << error.str() << std::endl;
	}

}

void SetTo::Manager(std::string device)
{
	CDeviceManager* devMgr = CDeviceManager::deviceManager();
		if (devMgr)
		{
			std::cout << "setCommandTo::Manager: Set command to manager from " << device << std::endl;
			devMgr->ackClient(device);
		}
		else
		{
			// отправить назад сообщение, что устройства еще не настроены
			std::cout << "ERROR! setCommandTo::Manager: Device manager not found." << std::endl;
		}

}

void SetTo::CommonLog(severity_level level, std::string message)
{
	Logger::SetToCommonLog(level, message);
}

void SetTo::LocalLog(std::string device, severity_level level, std::string message)
{
	Logger::SetToLocalLog(device, level, message);
}
