//============================================================================
// Name        : SetCommandTo.cpp
// Author      : aav
// Created on  : 23 февр. 2016 г.
// Version     : v.0.1
// Copyright   : Non nobis, Domine, non nobis, sed nomini tuo da gloriam.
// Description : Facade instances for device and transaction manager.
//============================================================================

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
		{
			std::stringstream log;
			log << "setCommandTo::Device: " << device << " from " << adresat;
			SetTo::CommonLog(trace, log.str());
		}
		devMgr->setCommandToDevice(txid, device, command, parameters, adresat);
	}
	else
	{
		// отправить назад сообщение, что устройства еще не настроены
		std::stringstream error;
		error << "ERROR! setCommandTo::Device: Manager not found.";
		sendErrorToClient(error);

		SetTo::CommonLog(severity_level::error, error.str());
	}

}

void SetTo::Client(CommandType eventFlag, std::string device, std::string command, std::string parameters)
{
	CDeviceManager* devMgr = CDeviceManager::deviceManager();
	if (devMgr)
	{
		{
			std::stringstream log;
			log << "setCommandTo::Client: Set command to client from " << device;
			SetTo::CommonLog(trace, log.str());
		}
		devMgr->setCommandToClient(eventFlag, device, command, parameters);
	}
	else
	{
		// отправить назад сообщение, что устройства еще не настроены
		std::stringstream error;
		error << "ERROR! setCommandTo::Device: Manager not found.";
		sendErrorToClient(error);

		SetTo::CommonLog(severity_level::error, error.str());
	}

}

void SetTo::Manager(std::string device)
{
	CDeviceManager* devMgr = CDeviceManager::deviceManager();
		if (devMgr)
		{
			{
				std::stringstream log;
				log << "setCommandTo::Manager: Set command to manager from " << device;
				SetTo::CommonLog(trace, log.str());
			}
			devMgr->ackClient(device);
		}
		else
		{
			// отправить назад сообщение, что устройства еще не настроены
			SetTo::CommonLog(severity_level::error, "ERROR! setCommandTo::Manager: Device manager not found.");
		}

}

void SetTo::CommonLog(severity_level level, const std::string message)
{
	Logger::SetToCommonLog(level, message);
}

void SetTo::LocalLog(const std::string device, severity_level level, const std::string message)
{
	Logger::SetToLocalLog(device, level, message);
}
