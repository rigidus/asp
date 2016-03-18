/*
 * SymbolDisplayAbstract.h
 *
 *  Created on: 16 марта 2016 г.
 *      Author: bvl
 */

#ifndef DISPLAYABSTRACT_H_
#define DISPLAYABSTRACT_H_

#include "CAbstractDevice.h"
#include <devices/CBaseDevice.h>
#include <devices/LCD_winstar16x2.h>

class AbstractDisplay: public CAbstractDevice
{

public:

	AbstractDisplay(CBaseDevice* pDevice, const std::string& abstractName):
		CAbstractDevice(pDevice, abstractName) {}

	static const std::string s_abstractName;

	static CAbstractDevice* createDevice(const std::string& abstractName, const std::string& devName)
	{

		std::cout << "AbstractDisplay::createDevice: Create concrete device " << devName << std::endl;

		CBaseDevice* cDev = nullptr;

		// INTEGRATE DEVICE SECTION: место для создания конкретных устройств
		{

			// Создание конкретного такого абстракного типа дисплея с конкретной моделью "Winstar 16x2"
			if ( CSLCDWinstar16x2::s_concreteName == devName)
			{
				cDev = reinterpret_cast<CBaseDevice*> (new CSLCDWinstar16x2());

				// Connect concrete device to communication devices
				if (cDev->connectToCommCtl())
				{
					return new AbstractDisplay(cDev, abstractName);
				}
			}

			// INTEGRATE DEVICE SECTION: добавь создание нового устройства сюда

		}
		return nullptr;
	}

	virtual void sendCommand(const std::string& command, const std::string& pars)
	{
		std::cout << "AbstractDisplay::sendCommand: " << command << "; " << pars << std::endl;
		device()->sendCommand(command, pars);
	}
};

#endif /* DISPLAYABSTRACT_H_ */
