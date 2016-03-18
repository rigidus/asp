/*
 * winstar16x2.h
 *
 *  Created on: 16 марта 2016 г.
 *      Author: bvl
 */

#ifndef WINSTAR16x2_H_
#define WINSTAR16x2_H_


#include "devices/CBaseDevice.h"
#include "GlobalThreadPool.h"

class CSLCDWinstar16x2: public CBaseDevice
{

// CodecType protoCodec;

public:

	CSLCDWinstar16x2(): CBaseDevice(s_concreteName) {}

	~CSLCDWinstar16x2()
	{
		CBaseDevice::disconnectFromCommCtl();
	}

	static const std::string s_concreteName;

	std::vector<uint8_t> rcvData;

	virtual void sendCommand(const std::string command, const std::string pars)
	{
		std::cout << "CSLCDWinstar16x2::sendCommand: performs command: " << command << "[" << pars << "]" << std::endl;

		std::list<std::vector<uint8_t> > data;

		if (m_commCtl.size() == 0)
		{
			std::cout << "ERROR! CSLCDWinstar16x2::sendCommand: communication devices has lost" << std::endl;
			return;
		}

		// command "up"
		if (m_commCtl[0])
			m_commCtl[0]->send(data);

		setCommandTo::Manager(c_name);

	}

	virtual bool connectToCommCtl()
	{
		return CBaseDevice::connectToCommCtl();
	}

};


#endif /* WINSTAR16x2_H_ */
