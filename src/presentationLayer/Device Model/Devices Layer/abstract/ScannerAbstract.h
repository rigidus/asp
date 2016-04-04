/*
 * ScannerAbstract.h
 *
 *  Created on: 1 april 2016 г.
 *      Author: bvl
 */

#ifndef _SCANNERABSTRACT_H_
#define _SCANNERABSTRACT_H_

#include "CAbstractDevice.h"
#include <devices/CBaseDevice.h>
#include <devices/SCN_quantumT_usb.h>

class AbstractScanner: public CAbstractDevice
{

public:

	AbstractScanner(CBaseDevice* pDevice, const std::string& abstractName):
		CAbstractDevice(pDevice, abstractName) {}

	static const std::string s_abstractName;

	static CAbstractDevice* createDevice(const std::string& abstractName, const std::string& devName)
	{

		std::cout << "AbstractScanner::createDevice: Create concrete device " << devName << std::endl;

		CBaseDevice* cDev = nullptr;

		// INTEGRATE DEVICE SECTION: место для создания конкретных устройств типа шлагбаум
		{

			// Создание конкретного такого абстракного типа шлагбаума с конкретной моделью "палка"
			if ( CSCN_quantumT_usb::s_concreteName == devName)
			{
				cDev = reinterpret_cast<CBaseDevice*> (new CSCN_quantumT_usb());

				// Connect concrete device to communication devices
				if (cDev->connectToCommCtl())
				{
					return new AbstractScanner(cDev, abstractName);
				}
			}

			// INTEGRATE DEVICE SECTION: добавь создание нового устройства сюда

		}
		return nullptr;
	}

	virtual void sendCommand(const std::string& command, const std::string& pars)
	{
		std::cout << "AbstractScanner::sendCommand: " << command << "; " << pars << std::endl;
		device()->sendCommand(command, pars);
	}

};


#endif /* _SCANNERABSTRACT_H_ */
