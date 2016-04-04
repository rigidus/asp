/*
 * BsnsLogic.h
 *
 *  Created on: 24 февр. 2016 г.
 *      Author: alex
 */

#ifndef BSNSLOGIC_H_
#define BSNSLOGIC_H_

#include "CAbstractDevice.h"
#include <devices/CBaseDevice.h>
#include <devices/HttpClient.h>
#include <devices/HttpDevLayerClient.h>

class BsnsLogic: public CAbstractDevice
{

public:

	BsnsLogic(CBaseDevice* pDevice, const std::string& abstractName):
		CAbstractDevice(pDevice, abstractName) {}

	static const std::string s_abstractName;

	static CAbstractDevice* createDevice(const std::string& abstractName, const std::string& devName)
	{

		{
			std::stringstream log;
			log << "BsnsLogic::createDevice: Create concrete device " << devName;
			SetTo::LocalLog("device_factory", trace, log.str());
		}

		CBaseDevice* cDev = nullptr;

		// INTEGRATE DEVICE SECTION: место для создания конкретных устройств типа клиент
		{
			// Создание клиента "http client"
			if ( HttpClient::s_concreteName == devName)
			{
				cDev = reinterpret_cast<CBaseDevice*> (new HttpClient());

				return new BsnsLogic(cDev, abstractName);
			}

			if ( HttpDevLayerClient::s_concreteName == devName)
			{
				cDev = reinterpret_cast<CBaseDevice*> (new HttpDevLayerClient());

				return new BsnsLogic(cDev, abstractName);
			}

			// INTEGRATE DEVICE SECTION: добавь создание нового устройства сюда

		}

		return nullptr;
	}

	virtual void sendCommand(const std::string& command, const std::string& pars)
	{
		{
			std::stringstream log;
			log << "BsnsLogic::sendCommand: " << command << "; " << pars;
			SetTo::LocalLog(c_abstractName, trace, log.str());
		}

		device()->sendCommand(command, pars);
	}
};


#endif /* BSNSLOGIC_H_ */
