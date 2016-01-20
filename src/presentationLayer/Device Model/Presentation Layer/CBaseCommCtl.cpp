///////////////////////////////////////////////////////////
//  CBaseCommCtl.cpp
//  Implementation of the Class CBaseCommCtl
//  Created on:      19-џэт-2016 19:58:07
//  Original author: user-PC
///////////////////////////////////////////////////////////

#include "CBaseCommCtl.h"


CBaseCommCtl::CBaseCommCtl(){

}



CBaseCommCtl::~CBaseCommCtl(){

}



CBaseCommCtl::CBaseCommCtl(const CBaseCommCtl& theCBaseCommCtl){

}





bool CBaseCommCtl::receive(int rcvData){

	return false;
}


uint32_t CBaseCommCtl::send(std::list<std::vector<uint8_t> > sendData){

	return  NULL;
}


int CBaseCommCtl::setSettings(std::strring deviceName){

	return 0;
}