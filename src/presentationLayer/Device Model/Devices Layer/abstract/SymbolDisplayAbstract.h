/*
 * SymbolDisplayAbstract.h
 *
 *  Created on: 16 марта 2016 г.
 *      Author: bvl
 */

#ifndef SYMBOLDISPLAYABSTRACT_H_
#define SYMBOLDISPLAYABSTRACT_H_

#include "CAbstractDevice.h"
#include <devices/CBaseDevice.h>
#include <devices/winstar16x2.h>

class AbstractSymbolDisplay: public CAbstractDevice
{

public:

	AbstractSymbolDisplay(CBaseDevice* pDevice, const std::string& abstractName):
		CAbstractDevice(pDevice, abstractName) {}

	static const std::string s_abstractName;

	static CAbstractDevice* createDevice(const std::string& abstractName, const std::string& devName)
	{

		std::cout << "SymbolDisplayShlagbaum::createDevice: Create concrete device " << devName << std::endl;

		CBaseDevice* cDev = nullptr;

		// INTEGRATE DEVICE SECTION: место для создания конкретных устройств типа шлагбаум
		{

			// Создание конкретного такого абстракного типа шлагбаума с конкретной моделью "палка"
			if ( CSLCDWinstar16x2::s_concreteName == devName)
			{
				cDev = reinterpret_cast<CBaseDevice*> (new CSLCDWinstar16x2());

				// Connect concrete device to communication devices
				if (cDev->connectToCommCtl())
				{
					return new AbstractSymbolDisplay(cDev, abstractName);
				}
			}

			// INTEGRATE DEVICE SECTION: добавь создание нового устройства сюда

		}
		return nullptr;
	}

	virtual void sendCommand(const std::string& command, const std::string& pars)
	{
		std::cout << "AbstractSymbolDisplay::sendCommand: " << command << "; " << pars << std::endl;
		device()->sendCommand(command, pars);
	}
};

#endif /* SYMBOLDISPLAYABSTRACT_H_ */
