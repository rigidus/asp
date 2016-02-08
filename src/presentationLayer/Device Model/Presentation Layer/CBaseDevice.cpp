///////////////////////////////////////////////////////////
//  CBaseDevice.cpp
//  Implementation of the Class CBaseDevice
//  Created on:      19-���-2016 19:58:07
//  Original author: user-PC
///////////////////////////////////////////////////////////

#include "CBaseDevice.h"


CBaseDevice::CBaseDevice(){

}



CBaseDevice::~CBaseDevice(){

}



CBaseDevice::CBaseDevice(const CBaseDevice& theCBaseDevice){

}





CBaseCodec& CBaseDevice::GetCBaseCodec(){

	return *m_CBaseCodec;
}


void CBaseDevice::SetCBaseCodec(CBaseCodec* newVal){

	m_CBaseCodec = newVal;
}


CBaseCommCtl& CBaseDevice::GetCBaseCommCtl(){

	return *m_CBaseCommCtl;
}


void CBaseDevice::SetCBaseCommCtl(CBaseCommCtl* newVal){

	m_CBaseCommCtl = newVal;
}
