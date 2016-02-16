///////////////////////////////////////////////////////////
//  CSerialPortCtl.cpp
//  Implementation of the Class CSerialPortCtl
//  Created on:      19-���-2016 19:58:08
//  Original author: user-PC
///////////////////////////////////////////////////////////

#include "CSerialPortCtl.h"


CSerialPortCtl::CSerialPortCtl(CBaseDevice* device, std::string& gpioName):
	CBaseCommCtl(device, gpioName),
	m_serialPort(m_ioService)
{

}



CSerialPortCtl::~CSerialPortCtl(){

}


bool CSerialPortCtl::receive(int rcvData){

	return false;
}


uint32_t CSerialPortCtl::send(std::list<std::vector<uint8_t> > sendData){

	return  0;
}


int CSerialPortCtl::setSettings(std::string deviceName){

	return 0;
}
