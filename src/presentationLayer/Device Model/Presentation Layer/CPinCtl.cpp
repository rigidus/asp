///////////////////////////////////////////////////////////
//  CPinCtl.cpp
//  Implementation of the Class CPinCtl
//  Created on:      19-���-2016 19:58:08
//  Original author: user-PC
///////////////////////////////////////////////////////////

#include "CPinCtl.h"
#include "SetCommandTo.h"
#include <sys/inotify.h>

namespace boostio = boost::iostreams;
namespace boostfs = boost::filesystem;

const std::string CPinCtl::s_name = "gpio";
const std::string CPinCtl::gpioPath = "/sys/class/gpio/";
std::map<std::string, shared_ptr<CBaseCommCtl> > CPinCtl::busyPins;
boost::thread* CPinCtl::thrNotify = nullptr;
std::vector<CPinCtl::TPinData> CPinCtl::PinData;
bool CPinCtl::stopFlag = false;

shared_ptr<CBaseCommCtl> CPinCtl::takeCommCtl(CBaseDevice* device, const std::string& gpioName)
{

	std::cout << "CPinCtl::takeCommCtl try to take " << gpioName << std::endl;

	try{
		// check busy pin
		if (busyPins.find(gpioName) != busyPins.end())
		{
			std::cout << "ERROR! CPinCtl::takeCommCtl: getting " << gpioName << " failed: gpio is busy or not existing" << std::endl;
			return nullptr;
		}
		// OK! pin is free

		// check files for pin

		boostio::stream_buffer<boostio::file_sink> bufExport(gpioPath+"export");
		std::ostream fileExport(&bufExport);
		fileExport << gpioName;

		std::string pinPath = CPinCtl::gpioPath + "/" + gpioName + "/";

//		if ( CPinCtl::fileIsExist( pinPath+"direction" ) == false)
//		{
//			std::cout << "ERROR! CPinCtl::takeCommCtl getting" << gpioName << " failed: direction not found" << std::endl;
//			return nullptr;
//		}
//
//		if ( CPinCtl::fileIsExist( pinPath+"value" ) == false)
//		{
//			std::cout << "ERROR! CPinCtl::takeCommCtl getting" << gpioName << " failed: value not found" << std::endl;
//			return nullptr;
//		}
//
//		if ( CPinCtl::fileIsExist( pinPath+"edge" ) == false)
//		{
//			std::cout << "ERROR! CPinCtl::takeCommCtl getting" << gpioName << " failed: edge not found" << std::endl;
//			return nullptr;
//		}
//
//		if ( CPinCtl::fileIsExist( pinPath+"active_low" ) == false)
//		{
//			std::cout << "ERROR! CPinCtl::takeCommCtl getting" << gpioName << " failed: active_low not found" << std::endl;
//			return nullptr;
//		}
		// OK! pin is present

		// create CPinCtl for pinNum
		shared_ptr<CBaseCommCtl> pinCtl( (CBaseCommCtl*) new CPinCtl(device, gpioName) );
		busyPins.emplace(std::make_pair(gpioName, pinCtl));
		// OK! pin is made as busied and stored

		std::cout << "CPinCtl::takeCommCtl: take " << gpioName << " successfully" << std::endl;

		return pinCtl;
	}

	catch(boost::exception& ex)
	{
		std::cout << "ERROR! CPinCtl::takeCommCtl: exception: " << boost::diagnostic_information(ex) << std::endl;
		return nullptr;
	}

	catch(std::exception& ex)
	{
		std::cout << "ERROR! CPinCtl::takeCommCtl: exception: " << ex.what() << std::endl;
		return nullptr;
	}

	catch(...)
	{
		std::cout << "ERROR! CPinCtl::takeCommCtl unknown exception: " << std::endl;
		return nullptr;
	}

	return nullptr;
}


void CPinCtl::freeCommCtl(CBaseDevice* device, const std::string& gpioName)
{

	if (device == nullptr) return;

	std::cout << "CPinCtl::freeCommCtl: getPinCtl try to free " << gpioName << std::endl;

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

	std::cout << "CPinCtl::freeCommCtl: " << gpioName << " is free" << std::endl;

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

	std::cout << "CPinCtl::send: command to pin and imitate reply ACK as callback" << std::endl;

	/*
	 * Постановка задачи на отправку сообщения на верхний уровень менеджеру
	 */

	std::vector<uint8_t> answer;
	for (auto& vect: sendData)
		for (auto v: vect)
			answer.push_back(v);

	m_device->performEvent(answer);

	return  0;
}


int CPinCtl::setSettings(std::string deviceName){

	return 0;
}

void CPinCtl::startNotifier()
{

	thrNotify = new boost::thread(Notifier);

}

void CPinCtl::stopNotifier()
{
	stopFlag = true;
	thrNotify->join();
	delete thrNotify;

}

void CPinCtl::Notifier()
{

	int d_inoty = inotify_init();

	for (auto busyPin: busyPins)
	{
		std::string fname(gpioPath + busyPin.first + "/value");

		TPinData pin;
		pin.fd = open( fname.c_str(), O_RDWR | O_NONBLOCK);

		if (pin.fd == -1)
		{
			std::cout << "ERROR! CPinCtl::Notifier didn't find file: " << fname << ". Notifier exits." << std::endl;
 			return;
		}

		pin.events = IN_MODIFY;
		pin.watch =  inotify_add_watch(d_inoty, fname.c_str(), pin.events);
		pin.name = busyPin.first;

		PinData.push_back(pin);
	}

	fd_set readset, excpset;

	while(stopFlag == false)
	{
		FD_ZERO(&readset);
		FD_ZERO(&excpset);
		FD_SET(d_inoty, &readset);
		FD_SET(d_inoty, &excpset);

		struct timeval tv;
		tv.tv_sec = 0;
		tv.tv_usec = 100000;
		select(d_inoty + 1, &readset, NULL, &excpset, &tv);

		if (FD_ISSET(d_inoty, &excpset)){

			std::cout << "CPinCtl::Notifier: Inotify exception 0x%X\n" << std::endl;
		}

		if (FD_ISSET(d_inoty, &readset)){

			struct inotify_event einoty;
			ssize_t rdlen=0;
			rdlen = read(d_inoty, (char*) &einoty, sizeof(struct inotify_event));
			if (rdlen){

				if (einoty.mask){

					TPinData pindata;
					for (auto pin: PinData)
					{
						if (pin.watch == einoty.wd)
						{
							pindata = pin;
							break;
						}
					}

					uint32_t mask = einoty.mask & pindata.events;

					// Calling callback functions
					if (mask & IN_MODIFY)
					{
						std::cout << "CPinCtl::Notifier: Inotify found modify file " << pindata.name << std::endl;

						lseek(pindata.fd, 0, SEEK_SET);
						char Value = 0;
						size_t size = read(pindata.fd, &Value, 1);
						if (size == 1)
						{
							std::cout << "GPIO was changed: " << Value << std::endl;
							std::vector<uint8_t> data;
							data.push_back(Value);

							auto it = busyPins.find(pindata.name);
							if ( it != busyPins.end())
							{
								it->second->myDevice().performEvent(data);
							}
						}
					}

				}
			}
		}
	}


}
