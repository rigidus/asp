///////////////////////////////////////////////////////////
//  CSerialPortCtl.cpp
//  Implementation of the Class CSerialPortCtl
//  Created on:      19-���-2016 19:58:08
//  Original author: user-PC
///////////////////////////////////////////////////////////

#include "CSerialPortCtl.h"


CSerialPortCtl::CSerialPortCtl(asio::io_service& ioService):
	m_serialPort(ioService)
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
