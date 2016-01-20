///////////////////////////////////////////////////////////
//  CSerialPortCtl.cpp
//  Implementation of the Class CSerialPortCtl
//  Created on:      19-џэт-2016 19:58:08
//  Original author: user-PC
///////////////////////////////////////////////////////////

#include "CSerialPortCtl.h"


CSerialPortCtl::CSerialPortCtl(){

}



CSerialPortCtl::~CSerialPortCtl(){

}



CSerialPortCtl::CSerialPortCtl(const CSerialPortCtl& theCSerialPortCtl){

}





bool CSerialPortCtl::receive(int rcvData){

	return false;
}


uint32_t CSerialPortCtl::send(std::list<std::vector<uint8_t> > sendData){

	return  NULL;
}


int CSerialPortCtl::setSettings(std::strring deviceName){

	return 0;
}