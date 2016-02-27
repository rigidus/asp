/*
 * SetCommandTo.cpp
 *
 *  Created on: 23 февр. 2016 г.
 *      Author: drema
 */

#include "SetCommandTo.h"
#include "CDeviceManager.h"

void setCommandTo::Device(uint32_t txid, std::string device, std::string command,
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
		// TODO отправить назад сообщение, что устройства еще не настроены
		std::cout << "ERROR! setCommandTo::Device: not found." << std::endl;
	}

}

void setCommandTo::Client(CommandType eventFlag, std::string device, std::string command, std::string parameters)
{
	CDeviceManager* devMgr = CDeviceManager::deviceManager();
	if (devMgr)
	{
		std::cout << "setCommandTo::Client: Set command to client from " << device << std::endl;
		devMgr->setCommandToClient(eventFlag, device, command, parameters);
	}
	else
	{
		// TODO отправить назад сообщение, что устройства еще не настроены
		std::cout << "ERROR! setCommandTo::Client: Device manager not found." << std::endl;
	}

}

void setCommandTo::Manager(std::string device)
{
	CDeviceManager* devMgr = CDeviceManager::deviceManager();
		if (devMgr)
		{
			std::cout << "setCommandTo::Manager: Set command to manager from " << device << std::endl;
			devMgr->ackClient(device);
		}
		else
		{
			// TODO отправить назад сообщение, что устройства еще не настроены
			std::cout << "ERROR! setCommandTo::Manager: Device manager not found." << std::endl;
		}

}
