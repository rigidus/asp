///////////////////////////////////////////////////////////
//  CPinCtl.cpp
//  Implementation of the Class CPinCtl
//  Created on:      19-���-2016 19:58:08
//  Original author: user-PC
///////////////////////////////////////////////////////////

#include "CPinCtl.h"
#include <map>

namespace boostio = boost::iostreams;
namespace boostfs = boost::filesystem;

const std::string CPinCtl::gpioPath = "/sys/class/gpio/";

CBaseCommCtl* CPinCtl::getPinCtl(CBaseDevice* device, const std::string& gpioName)
{
	static std::map<std::string, CPinCtl*> busyPins;

	CPinCtl* pinCtl = nullptr;

	if (pinCtl == nullptr)
	{

		std::cout << "getPinCtl try to get " << gpioName << std::endl;

		try{
			// check busy pin
			if (busyPins.find(gpioName) != busyPins.end())
			{
				std::cout << "getPinCtl getting " << gpioName << " failed: gpio is busy or not existing" << std::endl;
				return nullptr;
			}
			// OK! pin is free

			// check files for pin

			boostio::stream_buffer<boostio::file_sink> bufExport(gpioPath+"export");
			std::ostream fileExport(&bufExport);
			fileExport << gpioName;

//			std::string pinPath = CPinCtl::gpioPath + "/" + gpioName + "/";
//
//			if ( CPinCtl::fileIsExist( pinPath+"direction" ) == false)
//			{
//				std::cout << "getPinCtl getting" << gpioName << " failed: direction not found" << std::endl;
//				return nullptr;
//			}
//
//			if ( CPinCtl::fileIsExist( pinPath+"value" ) == false)
//			{
//				std::cout << "getPinCtl getting" << gpioName << " failed: value not found" << std::endl;
//				return nullptr;
//			}
//
//			if ( CPinCtl::fileIsExist( pinPath+"edge" ) == false)
//			{
//				std::cout << "getPinCtl getting" << gpioName << " failed: edge not found" << std::endl;
//				return nullptr;
//			}
//
//			if ( CPinCtl::fileIsExist( pinPath+"active_low" ) == false)
//			{
//				std::cout << "getPinCtl getting" << gpioName << " failed: active_low not found" << std::endl;
//				return nullptr;
//			}
			// OK! pin is present

			// create CPinCtl for pinNum
			pinCtl = new CPinCtl(device, gpioName);
			busyPins.emplace(std::make_pair(gpioName, pinCtl));
			// OK! pin is made as busied and stored

		}

		catch(boost::exception& ex)
		{
			std::cout << "getPinCtl exception: " << boost::diagnostic_information(ex) << std::endl;
			return nullptr;
		}

		catch(std::exception& ex)
		{
			std::cout << "getPinCtl exception: " << ex.what() << std::endl;
			return nullptr;
		}

		catch(...)
		{
			std::cout << "getPinCtl unknown exception: " << std::endl;
			return nullptr;
		}

		std::cout << "getPinCtl get " << gpioName << "successfully" << std::endl;

	}

	return pinCtl;
}

bool CPinCtl::fileIsExist(const std::string& fileName)
{
	boostfs::file_status fStatus = boostfs::status(fileName);
	return boostfs::is_regular(fStatus);
}

CPinCtl::CPinCtl(CBaseDevice* device, const std::string& gpioName):
		CBaseCommCtl(device),
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

	std::cout << "CPinCtl send command to pin and reply ACK" << std::endl;

	GlobalThreadPool::get().AddTask(0, boost::bind(CBaseDevice::performEvent, m_device, *sendData.begin() ));

	return  0;
}


int CPinCtl::setSettings(std::string deviceName){

	return 0;
}
