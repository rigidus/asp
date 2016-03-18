/*
 * PrinterAbstract.h
 *
 *  Created on: 18 марта 2016 г.
 *      Author: bvl
 */

#ifndef PRINTERABSTRACT_H_
#define PRINTERABSTRACT_H_

#include "CAbstractDevice.h"
#include <devices/CBaseDevice.h>
#include <devices/PRN_vkp80ii_usb.h>

class AbstractPrinter: public CAbstractDevice
{

public:

	AbstractPrinter(CBaseDevice* pDevice, const std::string& abstractName):
		CAbstractDevice(pDevice, abstractName) {}

	static const std::string s_abstractName;

	static CAbstractDevice* createDevice(const std::string& abstractName, const std::string& devName)
	{

		std::cout << "AbstractPrinter::createDevice: Create concrete device " << devName << std::endl;

		CBaseDevice* cDev = nullptr;

		// INTEGRATE DEVICE SECTION: место для создания конкретных устройств типа шлагбаум
		{

			// Создание конкретного такого абстракного типа шлагбаума с конкретной моделью "палка"
			if ( CPRN_vkp80ii_usb::s_concreteName == devName)
			{
				cDev = reinterpret_cast<CBaseDevice*> (new CPRN_vkp80ii_usb());

				// Connect concrete device to communication devices
				if (cDev->connectToCommCtl())
				{
					return new AbstractPrinter(cDev, abstractName);
				}
			}

			// INTEGRATE DEVICE SECTION: добавь создание нового устройства сюда

		}
		return nullptr;
	}

	virtual void sendCommand(const std::string& command, const std::string& pars)
	{
		std::cout << "AbstractPrinter::sendCommand: " << command << "; " << pars << std::endl;
		device()->sendCommand(command, pars);
	}
};

#endif /* PRINTERABSTRACT_H_ */
