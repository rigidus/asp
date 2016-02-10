///////////////////////////////////////////////////////////
//  CBaseDevice.cpp
//  Implementation of the Class CBaseDevice
//  Created on:      19-���-2016 19:58:07
//  Original author: user-PC
///////////////////////////////////////////////////////////

#include "CBaseDevice.h"


CBaseDevice::CBaseDevice(const std::string& str):
	c_name(str),
	m_codec(nullptr),
	m_commCtl(nullptr)
{

}



CBaseDevice::~CBaseDevice(){

}


CBaseCodec& CBaseDevice::getCodec(){

	return *m_codec;
}


CBaseCommCtl& CBaseDevice::getCommCtl(){

	return *m_commCtl;
}
