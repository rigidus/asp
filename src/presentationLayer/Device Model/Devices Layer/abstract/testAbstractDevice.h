/*
 * testAbstractDevice.h
 *
 *  Created on: 5 марта 2016 г.
 *      Author: drema
 */

#ifndef TESTABSTRACTDEVICE_H_
#define TESTABSTRACTDEVICE_H_


#include "CAbstractDevice.h"
#include <devices/CBaseDevice.h>
#include <devices/testConcreteDevice.h>

class CTestAbstractDevice: public CAbstractDevice
{

public:

	CTestAbstractDevice(CBaseDevice* pDevice, const std::string& abstractName):
		CAbstractDevice(pDevice, abstractName) {}

	static const std::string s_abstractName;

	static CAbstractDevice* createDevice(const std::string& abstractName, const std::string& devName)
	{

		std::cout << "CTestAbstractDevice::createDevice: Create concrete device " << devName << std::endl;

		CBaseDevice* cDev = nullptr;

		// INTEGRATE DEVICE SECTION: место для создания конкретных устройств типа для теста менеджера
		{

			// Создание конкретного такого абстракного типа устройства
			if ( CTestConcreteDevice::s_concreteName == devName)
			{
				cDev = reinterpret_cast<CBaseDevice*> (new CTestConcreteDevice());

				// Connect concrete device to communication devices
				if (cDev->connectToCommCtl())
				{
					return new CTestAbstractDevice(cDev, abstractName);
				}
			}

			// INTEGRATE DEVICE SECTION: добавь создание нового устройства сюда

		}
		return nullptr;
	}

	virtual void sendCommand(const std::string& command, const std::string& pars)
	{
		std::cout << "CTestAbstractDevice::sendCommand: " << command << "; " << pars << std::endl;
		device()->sendCommand(command, pars);
	}
};


#endif /* TESTABSTRACTDEVICE_H_ */
