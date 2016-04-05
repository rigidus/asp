//============================================================================
// Name        : UserButtonAbstract.h
// Author      : aav
// Created on  : 14 марта 2016 г.
// Version     : v.0.1
// Copyright   : Non nobis, Domine, non nobis, sed nomini tuo da gloriam.
// Description : Abstract device for all user buttons
//============================================================================

#ifndef USERBUTTONABSTRACT_H_
#define USERBUTTONABSTRACT_H_

#include "CAbstractDevice.h"
#include <devices/CBaseDevice.h>
#include <devices/UserButton.h>

class AbstractUserButton: public CAbstractDevice
{

public:

	AbstractUserButton(CBaseDevice* pDevice, const std::string& abstractName):
		CAbstractDevice(pDevice, abstractName) {}

	static const std::string s_abstractName;

	static CAbstractDevice* createDevice(const std::string& abstractName, const std::string& devName)
	{

		{
			std::stringstream log;
			log << "AbstractUserButton::createDevice: Create concrete device " << devName;
			SetTo::LocalLog("device_factory", trace, log.str());
		}

		CBaseDevice* cDev = nullptr;

		// INTEGRATE DEVICE SECTION: место для создания конкретных устройств типа шлагбаум
		{

			// Создание конкретного такого абстракного типа шлагбаума с конкретной моделью "палка"
			if ( CUserButton::s_concreteName == devName)
			{
				cDev = reinterpret_cast<CBaseDevice*> (new CUserButton());

				// Connect concrete device to communication devices
				if (cDev->connectToCommCtl())
				{
					return new AbstractUserButton(cDev, abstractName);
				}
			}

			// INTEGRATE DEVICE SECTION: добавь создание нового устройства сюда

		}
		return nullptr;
	}

	virtual void sendCommand(const std::string& command, const std::string& pars)
	{
		{
			std::stringstream log;
			log << "AbstractUserButton::sendCommand: " << command << "; " << pars;
			SetTo::LocalLog(c_abstractName, trace, log.str());
		}

		device()->sendCommand(command, pars);
	}

};


#endif /* USERBUTTONABSTRACT_H_ */
