///////////////////////////////////////////////////////////
//  CBaseDevice.cpp
//  Implementation of the Class CBaseDevice
//  Created on:      19-���-2016 19:58:07
//  Original author: user-PC
///////////////////////////////////////////////////////////

#include "CBaseDevice.h"


CBaseDevice::CBaseDevice(const std::string& str):
	c_name(str)
{

}



CBaseDevice::~CBaseDevice(){

}


const std::vector<CBaseCommCtl*>& CBaseDevice::getCommCtl(){

	return m_commCtl;
}
