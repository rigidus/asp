///////////////////////////////////////////////////////////
//  CBaseCommCtl.cpp
//  Implementation of the Class CBaseCommCtl
//  Created on:      19-���-2016 19:58:07
//  Original author: user-PC
///////////////////////////////////////////////////////////

#include "CBaseCommCtl.h"
#include "devices/CBaseDevice.h"

CBaseCommCtl::CBaseCommCtl(CBaseDevice* device, const std::string& commName): m_device(device)
{
	m_deviceName = m_device->c_name;
	m_commName = commName;
}


CBaseCommCtl::~CBaseCommCtl(){

}


bool CBaseCommCtl::receive(int rcvData){

	return false;
}


uint32_t CBaseCommCtl::send(std::list<std::vector<uint8_t> > sendData){

	return  0;
}


int CBaseCommCtl::setSettings(std::string deviceName){

	return 0;
}
