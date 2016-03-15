/*
 * testShlagbaum.h
 *
 *  Created on: 18 февр. 2016 г.
 *      Author: alex
 */

#ifndef TESTCONCRETEDEVICE_H_
#define TESTCONCRETEDEVICE_H_


#include "devices/CBaseDevice.h"
#include "GlobalThreadPool.h"

class CTestConcreteDevice: public CBaseDevice
{

// CodecType protoCodec;

public:

	CTestConcreteDevice(): CBaseDevice(s_concreteName) {}

	~CTestConcreteDevice()
	{
		CBaseDevice::disconnectFromCommCtl();
	}

	static const std::string s_concreteName;

	static const std::string s_answerMessage;

	std::vector<uint8_t> rcvData;

	virtual void sendCommand(const std::string command, const std::string pars)
	{
		std::cout << "CTestConcreteDevice::sendCommand: performs command: " << command << "[" << pars << "]" << std::endl;

		// command "up"
		setCommandTo::Client(setCommandTo::Transaction, c_name, "", pars);

	}

	virtual bool connectToCommCtl()
	{
		return CBaseDevice::connectToCommCtl();
	}

};

#endif /* TESTCONCRETEDEVICE_H_ */