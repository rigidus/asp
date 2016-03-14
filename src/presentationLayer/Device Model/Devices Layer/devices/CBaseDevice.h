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

#include <CBaseCodec.h>
#include <CBaseCommCtl.h>
#include <Settings.h>
#include <CPinCtl.h>
#include <CSerialPortCtl.h>
#include <SetCommandTo.h>

class CBaseDevice: private noncopyable
{

public:
	const std::string c_name;

	CBaseDevice(const std::string& deviceName);
	virtual ~CBaseDevice();
	CBaseDevice(const CBaseDevice& theCBaseDevice);

	virtual void sendCommand(const std::string command, const std::string pars)=0;
	virtual bool connectToCommCtl()=0;

	const std::vector< shared_ptr<CBaseCommCtl> >& getCommCtl();

	void performEvent(std::vector<uint8_t>& rcvData);
	void performTransaction(std::vector<uint8_t>& rcvData);

protected:

	std::vector< shared_ptr<CBaseCommCtl> > m_commCtl;

	void addCommDevice(shared_ptr<CBaseCommCtl> commCtl);

	virtual void disconnectFromCommCtl();

	/*** Templates ***/

	template<class T>
	shared_ptr<CBaseCommCtl> takeCommDevice(const std::string& commName)
	{

		if ( commName.find(T::s_name) != std::string::npos)
		{
			std::cout << "CBaseDevice::takeCommDevice: Take communication device: " << commName << std::endl;

			return T::takeCommCtl(this, commName);
		}

		return nullptr;
	}

};


#endif // !defined(EA_AAF6E551_FF21_4908_B83B_A548A70782BC__INCLUDED_)
