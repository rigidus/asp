///////////////////////////////////////////////////////////
//  CPinCtl.cpp
//  Implementation of the Class CPinCtl
//  Created on:      19-���-2016 19:58:08
//  Original author: user-PC
///////////////////////////////////////////////////////////

#include "CPinCtl.h"

namespace boostio = boost::iostreams;
namespace boostfs = boost::filesystem;

const std::string CPinCtl::s_name = "gpio";
const std::string CPinCtl::gpioPath = "/sys/class/gpio/";
std::map<std::string, shared_ptr<CBaseCommCtl> > CPinCtl::busyPins;

shared_ptr<CBaseCommCtl> CPinCtl::takeCommCtl(CBaseDevice* device, const std::string& gpioName)
{

	std::cout << "CPinCtl::takeCommCtl try to take " << gpioName << std::endl;

	try{
		// check busy pin
		if (busyPins.find(gpioName) != busyPins.end())
		{
			std::cout << "CPinCtl::takeCommCtl getting " << gpioName << " failed: gpio is busy or not existing" << std::endl;
			return nullptr;
		}
		// OK! pin is free

		// check files for pin

		boostio::stream_buffer<boostio::file_sink> bufExport(gpioPath+"export");
		std::ostream fileExport(&bufExport);
		fileExport << gpioName;

//		std::string pinPath = CPinCtl::gpioPath + "/" + gpioName + "/";
//
//		if ( CPinCtl::fileIsExist( pinPath+"direction" ) == false)
//		{
//			std::cout << "CPinCtl::takeCommCtl getting" << gpioName << " failed: direction not found" << std::endl;
//			return nullptr;
//		}
//
//		if ( CPinCtl::fileIsExist( pinPath+"value" ) == false)
//		{
//			std::cout << "CPinCtl::takeCommCtl getting" << gpioName << " failed: value not found" << std::endl;
//			return nullptr;
//		}
//
//		if ( CPinCtl::fileIsExist( pinPath+"edge" ) == false)
//		{
//			std::cout << "CPinCtl::takeCommCtl getting" << gpioName << " failed: edge not found" << std::endl;
//			return nullptr;
//		}
//
//		if ( CPinCtl::fileIsExist( pinPath+"active_low" ) == false)
//		{
//			std::cout << "CPinCtl::takeCommCtl getting" << gpioName << " failed: active_low not found" << std::endl;
//			return nullptr;
//		}
		// OK! pin is present

		// create CPinCtl for pinNum
		shared_ptr<CBaseCommCtl> pinCtl( (CBaseCommCtl*) new CPinCtl(device, gpioName) );
		busyPins.emplace(std::make_pair(gpioName, pinCtl));
		// OK! pin is made as busied and stored

		std::cout << "CPinCtl::takeCommCtl take " << gpioName << " successfully" << std::endl;

		return pinCtl;
	}

	catch(boost::exception& ex)
	{
		std::cout << "CPinCtl::takeCommCtl exception: " << boost::diagnostic_information(ex) << std::endl;
		return nullptr;
	}

	catch(std::exception& ex)
	{
		std::cout << "CPinCtl::takeCommCtl exception: " << ex.what() << std::endl;
		return nullptr;
	}

	catch(...)
	{
		std::cout << "CPinCtl::takeCommCtl unknown exception: " << std::endl;
		return nullptr;
	}

	return nullptr;
}


void CPinCtl::freeCommCtl(CBaseDevice* device, const std::string& gpioName)
{

	if (device == nullptr) return;

	std::cout << "getPinCtl try to free " << gpioName << std::endl;

	// check busy pin

	auto it = busyPins.find(gpioName);

	if ( it == busyPins.end())
	{
		return;
	}
	// OK! pin is busy

	shared_ptr<CBaseCommCtl> pinCtl(it->second);
	if ( pinCtl->m_deviceName != device->c_name)
	{
		return;
	}

	// free pin
	boostio::stream_buffer<boostio::file_sink> bufExport(gpioPath+"unexport");
	std::ostream fileExport(&bufExport);
	fileExport << gpioName;

	std::cout << "getPinCtl " << gpioName << " is free" << std::endl;

	busyPins.erase(it);
}


bool CPinCtl::fileIsExist(const std::string& fileName)
{
	boostfs::file_status fStatus = boostfs::status(fileName);
	return boostfs::is_regular(fStatus);
}


// Functions - non static members

CPinCtl::CPinCtl(CBaseDevice* device, const std::string& gpioName):
		CBaseCommCtl(device, gpioName),
		m_timeout(0)
{
	// TODO: initialize pin with pars

}

CPinCtl::~CPinCtl(){

}

bool CPinCtl::receive(int rcvData){

	return false;
}


uint32_t CPinCtl::send(std::list<std::vector<uint8_t> > sendData)
{

	std::cout << "CPinCtl send command to pin and imitate reply ACK as callback" << std::endl;

	GlobalThreadPool::get().AddTask(0, boost::bind(CBaseDevice::performEvent, m_device, *sendData.begin() ));

	return  0;
}


int CPinCtl::setSettings(std::string deviceName){

	return 0;
}
