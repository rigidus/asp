//============================================================================
// Name        : ShlagbaumAbstract.h
// Author      : aav
// Created on  : 17 февр. 2016 г.
// Version     : v.0.1
// Copyright   : Non nobis, Domine, non nobis, sed nomini tuo da gloriam.
// Description : Abstract shlagbaum.
//============================================================================

#ifndef SHLAGBAUMABSTRACT_H_
#define SHLAGBAUMABSTRACT_H_

#include "CAbstractDevice.h"
#include <devices/CBaseDevice.h>
#include <devices/gpioShlagbaum.h>

class AbstractShlagbaum: public CAbstractDevice
{

public:

	AbstractShlagbaum(CBaseDevice* pDevice, const std::string& abstractName):
		CAbstractDevice(pDevice, abstractName) {}

	static const std::string s_abstractName;

	static CAbstractDevice* createDevice(const std::string& abstractName, const std::string& devName)
	{

		{
			std::stringstream log;
			log << "AbstractShlagbaum::createDevice: Create concrete device " << devName;
			SetTo::LocalLog("device_factory", trace, log.str());
		}

		CBaseDevice* cDev = nullptr;

		// INTEGRATE DEVICE SECTION: место для создания конкретных устройств типа шлагбаум
		{

			// Создание конкретного такого абстракного типа шлагбаума с конкретной моделью "палка"
			if ( CGPIOShlagbaum::s_concreteName == devName)
			{
				cDev = reinterpret_cast<CBaseDevice*> (new CGPIOShlagbaum());

				// Connect concrete device to communication devices
				if (cDev->connectToCommCtl())
				{
					return new AbstractShlagbaum(cDev, abstractName);
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
			log << "AbstractShlagbaum::sendCommand: " << command << "; " << pars;
			SetTo::LocalLog(c_abstractName, trace, log.str());
		}

		device()->sendCommand(command, pars);
	}
};

#endif /* SHLAGBAUMABSTRACT_H_ */
