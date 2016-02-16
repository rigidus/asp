///////////////////////////////////////////////////////////
//  CBaseDevice.h
//  Implementation of the Class CBaseDevice
//  Created on:      19-���-2016 19:58:07
//  Original author: user-PC
///////////////////////////////////////////////////////////

#if !defined(EA_AAF6E551_FF21_4908_B83B_A548A70782BC__INCLUDED_)
#define EA_AAF6E551_FF21_4908_B83B_A548A70782BC__INCLUDED_

#include <boost/noncopyable.hpp>
#include <boost/thread/thread.hpp>
#include <boost/thread/mutex.hpp>
#include <boost/smart_ptr.hpp>

using namespace boost;

#include "CBaseCodec.h"
#include "CBaseCommCtl.h"

class CBaseDevice: private noncopyable
{

public:
	const std::string c_name;

	CBaseDevice(const std::string& str);
	virtual ~CBaseDevice();
	CBaseDevice(const CBaseDevice& theCBaseDevice);

	virtual void sendCommand(const std::string command, const std::string pars)=0;
	virtual bool connectToCommCtl()=0;
	virtual void disconnectFromCommCtl()=0;

	const std::vector<CBaseCommCtl*>& getCommCtl();

	static void performEvent(CBaseDevice* device, std::vector<uint8_t>& rcvData)
	{
		std::cout << "CBaseDevice Perform Event From Device: " << device->c_name << ": ";
		for (auto v: rcvData) std::cout << v << " ";
		std::cout << std::endl;
	}

protected:

	std::vector<CBaseCommCtl*> m_commCtl;

};


#endif // !defined(EA_AAF6E551_FF21_4908_B83B_A548A70782BC__INCLUDED_)
