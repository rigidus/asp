/*
 * testShlagbaum.h
 *
 *  Created on: 18 февр. 2016 г.
 *      Author: alex
 */

#ifndef TESTSHLAGBAUM_H_
#define TESTSHLAGBAUM_H_


#include "devices/CBaseDevice.h"
#include "GlobalThreadPool.h"

class ShlagbaumPalka: public CBaseDevice
{

// CodecType protoCodec;

public:

	ShlagbaumPalka(): CBaseDevice(s_concreteName) {}

	~ShlagbaumPalka()
	{
		CBaseDevice::disconnectFromCommCtl();
	}

	static const std::string s_concreteName;

	std::vector<uint8_t> rcvData;

	virtual void sendCommand(const std::string command, const std::string pars)
	{
		std::cout << "ShlagbaumPalka::sendCommand: performs command: " << command << "[" << pars << "]" << std::endl;

		rcvData.clear();
		rcvData.push_back('A');
		rcvData.push_back('C');
		rcvData.push_back('K');
		std::list<std::vector<uint8_t>> data;
		data.push_back(rcvData);

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

#endif /* TESTSHLAGBAUM_H_ */
