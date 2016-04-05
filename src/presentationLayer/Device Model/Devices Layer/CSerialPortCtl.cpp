//============================================================================
// Name        : CSerialPortCtl.cpp
// Author      : aav
// Created on  : 19 февр. 2016 г.
// Version     : v.0.1
// Copyright   : Non nobis, Domine, non nobis, sed nomini tuo da gloriam.
// Description : Multitone instance for communication resources of the UART type.
//============================================================================

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
	{
		std::stringstream log;
		log << "CSerialPortCtl::takeCommCtl try to take " << uartName;
		SetTo::LocalLog(device->c_name, trace, log.str());
	}

	try{
		// check busy pin
		if (busyUarts.find(uartName) != busyUarts.end())
		{
			{
				std::stringstream log;
				log << "CSerialPortCtl::takeCommCtl getting " << uartName << " failed: gpio is busy or not existing";
				SetTo::LocalLog(device->c_name, error, log.str());
			}
			return nullptr;
		}
		// OK! uart is free

		// create CPinCtl for pinNum
		shared_ptr<CBaseCommCtl> uartCtl((CBaseCommCtl*) new CSerialPortCtl(device, uartName));
		std::pair<std::string, shared_ptr<CBaseCommCtl> > pr(uartName, uartCtl);
		busyUarts.insert(pr);
		// OK! uart is made as busied and stored

		{
			std::stringstream log;
			log << "CSerialPortCtl::takeCommCtl take " << uartName << " successfully";
			SetTo::LocalLog(device->c_name, debug, log.str());
		}

		return uartCtl;
	}

	catch(boost::exception& ex)
	{
		{
			std::stringstream log;
			log << "ERROR! CSerialPortCtl::takeCommCtl: exception: " << boost::diagnostic_information(ex);
			SetTo::LocalLog(device->c_name, error, log.str());
		}
		return nullptr;
	}

	catch(std::exception& ex)
	{
		{
			std::stringstream log;
			log << "ERROR! CSerialPortCtl::takeCommCtl: exception: " << ex.what();
			SetTo::LocalLog(device->c_name, error, log.str());
		}
		return nullptr;
	}

	catch(...)
	{
		SetTo::LocalLog(device->c_name, error, "ERROR! CSerialPortCtl::takeCommCtl: unknown exception");
		return nullptr;
	}

	return nullptr;
}

void CSerialPortCtl::freeCommCtl(CBaseDevice* device, const std::string& uartName)
{
	if (device == nullptr);
	{
		SetTo::CommonLog(error, "CSerialPortCtl::freeCommCtl: No device");
		return;
	}

	{
		std::stringstream log;
		log << "CSerialPortCtl::getCommCtl: try to free " << uartName;
		SetTo::LocalLog(device->c_name, trace, log.str());
	}

	// check busy pin

	auto it = busyUarts.find(uartName);

	if ( it == busyUarts.end())
	{
		{
			std::stringstream log;
			log << "ERROR: CSerialPortCtl::freeCommCtl uart " << uartName << " not busy or not exist";
			SetTo::LocalLog(device->c_name, error, log.str());
		}
		return;
	}
	// OK! uart is busy

	shared_ptr<CBaseCommCtl> uartCtl(it->second);
	if ( uartCtl->m_deviceName != device->c_name)
	{
		{
			std::stringstream log;
			log << "ERROR: CSerialPortCtl::freeCommCtl: found " << uartName << " isn't connected to device " << device->c_name;
			SetTo::LocalLog(device->c_name, error, log.str());
		}
		return;
	}

	// free uart
	{
		std::stringstream log;
		log << "CSerialPortCtl::freeCommCtl: " << uartName << " is free";
		SetTo::LocalLog(device->c_name, error, log.str());
	}

	busyUarts.erase(it);

}
