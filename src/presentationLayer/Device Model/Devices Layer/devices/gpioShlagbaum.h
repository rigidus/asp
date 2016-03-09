/*
 * gpioShlagbaum.h
 *
 *  Created on: 5 марта 2016 г.
 *      Author: drema
 */

#ifndef GPIOSHLAGBAUM_H_
#define GPIOSHLAGBAUM_H_


#include "devices/CBaseDevice.h"
#include "GlobalThreadPool.h"

class CGPIOShlagbaum: public CBaseDevice
{

// CodecType protoCodec;

public:

	CGPIOShlagbaum(): CBaseDevice(s_concreteName) {}

	~CGPIOShlagbaum()
	{
		CBaseDevice::disconnectFromCommCtl();
	}

	static const std::string s_concreteName;

	std::vector<uint8_t> rcvData;

	virtual void sendCommand(const std::string command, const std::string pars)
	{
		std::cout << "GPIOShlagbaum::sendCommand: performs command: " << command << "[" << pars << "]" << std::endl;

		std::list<std::vector<uint8_t> > data;

		if (m_commCtl.size() == 0)
		{
			std::cout << "ERROR! ShlagbaumPalka::sendCommand: communication devices has lost" << std::endl;
			return;
		}

		// command "up"
		if (m_commCtl[0])
			m_commCtl[0]->send(data);

	}

	virtual bool connectToCommCtl()
	{
		return CBaseDevice::connectToCommCtl();
	}

};


#endif /* GPIOSHLAGBAUM_H_ */
