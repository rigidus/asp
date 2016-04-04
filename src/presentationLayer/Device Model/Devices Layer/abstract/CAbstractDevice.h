/*
 * CAbstractDevice.h
 *
 *  Created on: 17 февр. 2016 г.
 *      Author: alex
 */

#ifndef CABSTRACTDEVICE_H_
#define CABSTRACTDEVICE_H_

#include <devices/CBaseDevice.h>

class CAbstractDevice: private noncopyable
{
protected:
	CBaseDevice* concreteDevice;
	const std::string c_abstractName;

public:

	CAbstractDevice(CBaseDevice* pDevice, const std::string& abstractName):
		concreteDevice(pDevice),
		c_abstractName(abstractName)
	{
		{
			std::stringstream log;
			log << "CAbstractDevice constructor: Abstract Device: " << abstractName << " created.";
			SetTo::LocalLog("device_factory", debug, log.str());
		}
	}

	virtual ~CAbstractDevice()
	{
		delete concreteDevice;
	}

	CBaseDevice* device()
	{ return concreteDevice; }

	const std::string& deviceAbstractName()
	{ return c_abstractName; }

	const std::string& deviceConcreteName()
	{ return concreteDevice->c_name; }

	virtual void sendCommand(const std::string& command, const std::string& pars)=0;


};

#endif /* CABSTRACTDEVICE_H_ */

