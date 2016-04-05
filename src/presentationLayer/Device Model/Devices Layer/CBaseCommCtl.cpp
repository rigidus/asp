//============================================================================
// Name        : CBaseCommCtl.cpp
// Author      : aav
// Created on  : 19 февр. 2016 г.
// Version     : v.0.1
// Copyright   : Non nobis, Domine, non nobis, sed nomini tuo da gloriam.
// Description : Base class for communication device instances
//============================================================================

#include "CBaseCommCtl.h"
#include "devices/CBaseDevice.h"

CBaseCommCtl::CBaseCommCtl(CBaseDevice* device, const std::string& commName): m_device(device)
{
	m_deviceName = m_device->c_name;
	m_commName = commName;
	m_device = device;
}


CBaseCommCtl::~CBaseCommCtl(){

}


uint32_t CBaseCommCtl::send(std::vector<uint8_t> sendData)
{

	BOOST_ASSERT_MSG(false, "CBaseCommCtl::send must redefine in the child class");

	return  0;
}


uint32_t CBaseCommCtl::send(std::list<std::vector<uint8_t> > sendData)
{

	BOOST_ASSERT_MSG(false, "CBaseCommCtl::send must redefine in the child class");

	return  0;
}

