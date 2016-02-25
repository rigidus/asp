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

class BsnsLogic: public CAbstractDevice
{

public:

	BsnsLogic(CBaseDevice* pDevice, const std::string& abstractName):
		CAbstractDevice(pDevice, abstractName) {}

	static const std::string s_abstractName;

	static CAbstractDevice* createDevice(const std::string& abstractName, const std::string& devName)
	{

		std::cout << "Create concrete device " << devName << std::endl;

		CBaseDevice* cDev = nullptr;

		if ( HttpClient::s_concreteName == devName)
		{
			cDev = reinterpret_cast<CBaseDevice*> (new HttpClient());

			return new BsnsLogic(cDev, abstractName);
		}

		return nullptr;
	}

	virtual void sendCommand(const std::string& command, const std::string& pars)
	{
		// TODO: parse command and parameters and call concrete command of concrete device
		std::cout << "BsnsLogic::sendCommand: " << command << "; " << pars << std::endl;
		device()->sendCommand(command, pars);
	}
};


#endif /* BSNSLOGIC_H_ */
