/*
 * PRN_vkp80ii_usb.h
 *
 *  Created on: 18 марта 2016 г.
 *      Author: bvl
 */

#ifndef VKP80II_USB_H_
#define VKP80II_USB_H_


#include "devices/CBaseDevice.h"
#include "GlobalThreadPool.h"

class CPRN_vkp80ii_usb: public CBaseDevice
{

// CodecType protoCodec;

public:

	CPRN_vkp80ii_usb(): CBaseDevice(s_concreteName) {}

	~CPRN_vkp80ii_usb()
	{
		CBaseDevice::disconnectFromCommCtl();
	}

	static const std::string s_concreteName;

	std::vector<uint8_t> rcvData;

	virtual void sendCommand(const std::string command, const std::string pars)
	{
		std::cout << "CPRN_vkp80ii_usb::sendCommand: performs command: " << command << "[" << pars << "]" << std::endl;

		std::list<std::vector<uint8_t> > data;

		if (m_commCtl.size() == 0)
		{
			std::cout << "ERROR! CPRN_vkp80ii_usb::sendCommand: communication devices has lost" << std::endl;
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


#endif /* VKP80II_USB_H_ */
