/*
 * ShlagbaumAbstract.h
 *
 *  Created on: 18 февр. 2016 г.
 *      Author: alex
 */

#ifndef SHLAGBAUMABSTRACT_H_
#define SHLAGBAUMABSTRACT_H_

#include "CAbstractDevice.h"
#include <devices/CBaseDevice.h>
#include <devices/testShlagbaum.h>

class AbstractShlagbaum: public CAbstractDevice
{

public:

	AbstractShlagbaum(CBaseDevice* pDevice, const std::string& abstractName):
		CAbstractDevice(pDevice, abstractName) {}

	static const std::string s_abstractName;

	static CAbstractDevice* createDevice(const std::string& abstractName, const std::string& devName)
	{

		std::cout << "Create concrete device " << devName << std::endl;

		CBaseDevice* cDev = nullptr;

		if ( ShlagbaumPalka::s_concreteName == devName)
		{
			cDev = reinterpret_cast<CBaseDevice*> (new ShlagbaumPalka());

			// Connect concrete device to communication devices
			if (cDev->connectToCommCtl())
			{
				return new AbstractShlagbaum(cDev, abstractName);
			}
		}

		return nullptr;
	}

	virtual void sendCommand(const std::string& command, const std::string& pars)
	{
		// TODO: parse command and parameters and call concrete command of concrete device
		device()->sendCommand(command, pars);
	}
};

const std::string AbstractShlagbaum::s_abstractName = "shlagbaum";


#endif /* SHLAGBAUMABSTRACT_H_ */
