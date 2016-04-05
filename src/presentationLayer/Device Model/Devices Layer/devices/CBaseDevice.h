//============================================================================
// Name        : CBaseDevice
// Author      : aav
// Created on  : 9 февр. 2016 г.
// Version     : v.0.1
// Copyright   : Non nobis, Domine, non nobis, sed nomini tuo da gloriam.
// Description : Base class for concrete devices. Any concrete device has to inherit it.
//============================================================================

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
#include <CDisplayCtl.h>
#include <CPrnCtl.h>
#include <CSerialPortCtl.h>
#include <CCharDevCtl.h>
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

	virtual void performEvent(std::string& commDeviceName, std::vector<uint8_t>& rcvData);
	virtual void performTransaction(std::vector<uint8_t>& rcvData);

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
			{
				std::stringstream log;
				log << "CBaseDevice::takeCommDevice: Take communication device: " << commName;
				SetTo::LocalLog(c_name, trace, log.str());
			}

			return T::takeCommCtl(this, commName);
		}

		return nullptr;
	}

};


#endif // !defined(EA_AAF6E551_FF21_4908_B83B_A548A70782BC__INCLUDED_)
