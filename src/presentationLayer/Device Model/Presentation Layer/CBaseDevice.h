///////////////////////////////////////////////////////////
//  CBaseDevice.h
//  Implementation of the Class CBaseDevice
//  Created on:      19-���-2016 19:58:07
//  Original author: user-PC
///////////////////////////////////////////////////////////

#if !defined(EA_AAF6E551_FF21_4908_B83B_A548A70782BC__INCLUDED_)
#define EA_AAF6E551_FF21_4908_B83B_A548A70782BC__INCLUDED_

#include <boost/thread/thread.hpp>
#include <boost/thread/mutex.hpp>
#include <boost/smart_ptr.hpp>

#include "CBaseCodec.h"
#include "CBaseCommCtl.h"

class CBaseDevice
{

public:
	CBaseDevice();
	virtual ~CBaseDevice();
	CBaseDevice(const CBaseDevice& theCBaseDevice);

	CBaseCodec& GetCBaseCodec();
	void SetCBaseCodec(CBaseCodec* newVal);
	CBaseCommCtl& GetCBaseCommCtl();
	void SetCBaseCommCtl(CBaseCommCtl* newVal);

private:
	CBaseCodec m_codec;
	CBaseCommCtl* m_commCtl;
	CBaseCodec *m_CBaseCodec;
	CBaseCommCtl *m_CBaseCommCtl;

};

union AbstractDevice
{
	IAbstractDevice
	ShlagbaumPalka shlagbaum;
};

union Abstract

union ConcreteDevice
{
PrinterPechatalka		printer;
PresentSensorIamhere
PassSensorIamgetout
DisplayText16x2
DataMassStorage
KkmPoluchalka
};


class CBaseDeviceFactory
{
	CBaseDeviceFactory() {}

	boost::mutex mut;

public:

	static CBaseDeviceFactory* getFactory()
	{
		static CBaseDeviceFactory* ptr = nullptr;

		boost::mutex::scoped_lock(mut);

		if (ptr == nullptr) ptr = new CBaseDeviceFactory;

		return ptr;
	}

	ConcreteDevice* createDevice(std::string abstractName, std::string realName)
	{
		ConcreteDevice cDevice;

		return cDevice;
	}

};

#endif // !defined(EA_AAF6E551_FF21_4908_B83B_A548A70782BC__INCLUDED_)
