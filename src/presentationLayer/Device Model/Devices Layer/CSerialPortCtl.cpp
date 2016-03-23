///////////////////////////////////////////////////////////
//  CSerialPortCtl.cpp
//  Implementation of the Class CSerialPortCtl
//  Created on:      19-���-2016 19:58:08
//  Original author: user-PC
///////////////////////////////////////////////////////////

#include "CSerialPortCtl.h"

namespace boostio = boost::iostreams;
namespace boostfs = boost::filesystem;

const std::string CSerialPortCtl::s_name = "uart";
const std::string CSerialPortCtl::gpioPath = "/sys/class/gpio/";
std::map<std::string, shared_ptr<CBaseCommCtl> > CSerialPortCtl::busyUarts;

CSerialPortCtl::CSerialPortCtl(CBaseDevice* device, const std::string& gpioName):
	CBaseCommCtl(device, gpioName),
	m_serialPort(m_ioService)
{

}


CSerialPortCtl::~CSerialPortCtl(){

}


bool CSerialPortCtl::receive(int rcvData){

	return false;
}


uint32_t CSerialPortCtl::send(std::vector<uint8_t> sendData)
{
	std::list<std::vector<uint8_t> > lst;
	lst.push_back(sendData);

	return send(lst);
}


uint32_t CSerialPortCtl::send(std::list<std::vector<uint8_t> > sendData){

	return  0;
}


int CSerialPortCtl::setSettings(std::string deviceName){

	return 0;
}


shared_ptr<CBaseCommCtl> CSerialPortCtl::takeCommCtl(CBaseDevice* device, const std::string& uartName)
{
	// TODO: realize function
	std::cout << "CSerialPortCtl::takeCommCtl try to take " << uartName << std::endl;

	try{
		// check busy pin
		if (busyUarts.find(uartName) != busyUarts.end())
		{
			std::cout << "CSerialPortCtl::takeCommCtl getting " << uartName << " failed: gpio is busy or not existing" << std::endl;
			return nullptr;
		}
		// OK! pin is free

		// create CPinCtl for pinNum
		shared_ptr<CBaseCommCtl> uartCtl((CBaseCommCtl*) new CSerialPortCtl(device, uartName));
		std::pair<std::string, shared_ptr<CBaseCommCtl> > pr(uartName, uartCtl);
		busyUarts.insert(pr);
		// OK! pin is made as busied and stored

		std::cout << "CSerialPortCtl::takeCommCtl take " << uartName << " successfully" << std::endl;

		return uartCtl;
	}

	catch(boost::exception& ex)
	{
		std::cout << "CSerialPortCtl::takeCommCtl exception: " << boost::diagnostic_information(ex) << std::endl;
		return nullptr;
	}

	catch(std::exception& ex)
	{
		std::cout << "CSerialPortCtl::takeCommCtl exception: " << ex.what() << std::endl;
		return nullptr;
	}

	catch(...)
	{
		std::cout << "CSerialPortCtl::takeCommCtl unknown exception: " << std::endl;
		return nullptr;
	}

	return nullptr;
}

void CSerialPortCtl::freeCommCtl(CBaseDevice* device, const std::string& uartName)
{
	if (device == nullptr) return;

	std::cout << "freeUartCtl try to free " << uartName << std::endl;

	// check busy pin

	auto it = busyUarts.find(uartName);

	if ( it == busyUarts.end())
	{
		return;
	}
	// OK! uart is busy

	shared_ptr<CBaseCommCtl> uartCtl(it->second);
	if ( uartCtl->m_deviceName != device->c_name)
	{
		return;
	}

	// free uart
	std::cout << "getUartCtl " << uartName << " is free" << std::endl;

	busyUarts.erase(it);

}
