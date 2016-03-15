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
		fileExport << &gpioName[4]; // 4 - is gpio number position

		std::string pinPath = CPinCtl::gpioPath + "/" + gpioName + "/";

		if ( CPinCtl::fileIsExist( pinPath+"direction" ) == false)
		{
			std::cout << "ERROR! CPinCtl::takeCommCtl getting" << gpioName << " failed: direction not found" << std::endl;
			return nullptr;
		}

		if ( CPinCtl::fileIsExist( pinPath+"value" ) == false)
		{
			std::cout << "ERROR! CPinCtl::takeCommCtl getting" << gpioName << " failed: value not found" << std::endl;
			return nullptr;
		}

		if ( CPinCtl::fileIsExist( pinPath+"edge" ) == false)
		{
			std::cout << "ERROR! CPinCtl::takeCommCtl getting" << gpioName << " failed: edge not found" << std::endl;
			return nullptr;
		}

		if ( CPinCtl::fileIsExist( pinPath+"active_low" ) == false)
		{
			std::cout << "ERROR! CPinCtl::takeCommCtl getting" << gpioName << " failed: active_low not found" << std::endl;
			return nullptr;
		}
		// OK! pin is present

		// Create TPinData
		std::string fname(pinPath + "value");

		TPinData pin;
		pin.filename = fname;
		pin.fd = open( fname.c_str(), O_RDONLY | O_NONBLOCK);

		if (pin.fd == -1)
		{
			std::cout << "ERROR! CPinCtl::takeCommCtl didn't open file: " << fname << " as read-only." << std::endl;
 			return nullptr;
		}

		pin.events = IN_CLOSE;
		pin.watch = 0;
		pin.name = gpioName;
		pin.oldvalue = 0;

		// create CPinCtl for pinNum
		shared_ptr<CBaseCommCtl> pinCtl( (CBaseCommCtl*) new CPinCtl(device, pin) );
		std::pair<std::string, shared_ptr<CBaseCommCtl> > pr(gpioName, pinCtl);
		busyPins.insert(pr);

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
CPinCtl::CPinCtl(CBaseDevice* device, TPinData& pinData):
		CBaseCommCtl(device, pinData.name),
		m_PinData(pinData),
		m_timeout(0)
{
	std::vector<settings::CommGPIOConfig> configList =
			settings::getGPIOByDevice(device->c_name, pinData.name);

	settings::CommGPIOConfig config;
	for (auto v: configList)
	{
		if (v.name == pinData.name)
		{
			config = v;
			break;
		}
	}

	std::string pinPath = CPinCtl::gpioPath + "/" + pinData.name + "/";

	{
		std::string strActiveLow("0");
		boostio::stream_buffer<boostio::file_sink> bufExport(gpioPath+"active_low");
		std::ostream fileExport(&bufExport);
		fileExport << strActiveLow;
	}

	{
		std::string strDir("in");
		if (config.direction)
			strDir = "out";

		boostio::stream_buffer<boostio::file_sink> bufExport(gpioPath+"direction");
		std::ostream fileExport(&bufExport);
		fileExport << strDir;
	}

	{
		std::string strEdge("both");
		boostio::stream_buffer<boostio::file_sink> bufExport(gpioPath+"edge");
		std::ostream fileExport(&bufExport);
		fileExport << strEdge;
	}

	{
		std::string strDef("0");
		if (config.def_value)
			strDef = "1";

		boostio::stream_buffer<boostio::file_sink> bufExport(gpioPath+"value");
		std::ostream fileExport(&bufExport);
		fileExport << strDef;
	}

}

CPinCtl::~CPinCtl(){

}


uint32_t CPinCtl::send(std::list<std::vector<uint8_t> > sendData)
{

	std::cout << "CPinCtl::send: command 'write value' to '" << m_PinData.name << "'." << std::endl;

	/*
	 * Парсинг команды записи в конкретный файл пина
	 * value, direction, edge, active_low
	 */

	if ( sendData.size() == 1 && sendData.begin()->size() > 2 )
	{
		std::string fname(gpioPath + m_PinData.name + "/");

		std::vector<uint8_t>& data = sendData.back();

		uint8_t filetype = data[0];

		char* beginData = (char*) &data[1];
		char* endData = (char*) &data[data.size()-1];
		std::string value(beginData, endData);

		switch(filetype)
		{
		case 0: // value
			fname += "value";
			break;
		case 1: // direction
			fname += "direction";
			break;
		case 2:
			fname += "edge";
			break;
		default:
			std::cout << "ERROR! CPinCtl::send: File type " << filetype << " incorrect" << std::endl;
			return 0;
		}

		int32_t fd = open( fname.c_str(), O_WRONLY | O_NONBLOCK);
		if (fd == -1)
		{
			std::cout << "ERROR! CPinCtl::send: File '" << fname << "' don't opened for writing." << std::endl;
			return 0;
		}

		uint32_t len = write(fd, value.c_str(), value.size());

		close(fd);

		return len;
	}

	if ( sendData.size() != 1)
	{
		std::cout << "ERROR! CPinCtl::send: Incorrect argument size: list size = "
				<< sendData.size() << std::endl;
	}
	else
	if ( sendData.begin()->size() < 2 )
	{
		std::cout << "ERROR! CPinCtl::send: Incorrect argument size: vector size = "
		<< sendData.begin()->size() << std::endl;
	}

	return  0;
}


int8_t CPinCtl::getPinValue()
{
	std::cout << "CPinCtl::getPinValue: From " << m_PinData.filename << std::endl;

	char Value = 0;
	size_t size = read(m_PinData.fd, &Value, 1);

	if (size != 1)
	{
		std::cout << "ERROR! CPinCtl::getPinValue: Not successful read from " << m_PinData.filename << std::endl;
	}

	return Value;
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

	std::cout << "GPIO Notifier started with " << busyPins.size() << " pin controls" << std::endl;

	for (auto busyPin: busyPins)
	{
		CPinCtl* pctl = (CPinCtl*) busyPin.second.get();
		if ( pctl == nullptr)
		{
			std::cout << "ERROR! CPinCtl::Notifier didn't find pointer to GPIO device: " << busyPin.first << ". Notifier exits." << std::endl;
 			return;
		}

		TPinData& PinData = pctl->m_PinData;

		if (PinData.fd == -1)
		{
			std::cout << "ERROR! CPinCtl::Notifier didn't find file: " << PinData.filename << ". Notifier exits." << std::endl;
 			return;
		}

		PinData.watch =  inotify_add_watch(d_inoty, PinData.filename.c_str(), PinData.events);
	}


	while(stopFlag == false)
	{

		boost::this_thread::sleep(boost::posix_time::milliseconds(100));

		for (auto busyPin: busyPins)
		{
			TPinData& PinData = ((CPinCtl*) busyPin.second.get())->m_PinData;

			lseek(PinData.fd, 0, SEEK_SET);

			char Value = 0;
			size_t size = read(PinData.fd, &Value, 1);
			if (size == 1)
			{
				if ( PinData.oldvalue != Value )
				{
					PinData.oldvalue = Value;

					std::cout << "GPIO Notifier: GPIO was changed: " << PinData.name << " = " << Value << std::endl;
					std::vector<uint8_t> data;
					data.push_back(Value);

					auto it = busyPins.find(PinData.name);
					if ( it != busyPins.end())
					{
						std::cout << "GPIO Notifier: Call Device Event for: " << it->second->myDevice().c_name << std::endl;

						it->second->myDevice().performEvent(data);

						std::cout << "GPIO Notifier: Device Event Ready for: " << it->second->myDevice().c_name << std::endl;
					}
				}
			}

		}

//		if (FD_ISSET(d_inoty, &excpset)){
//
//			std::cout << "CPinCtl::Notifier: Inotify exception " << std::endl;
//
//			struct inotify_event einoty;
//			ssize_t rdlen=0;
//			rdlen = read(d_inoty, (char*) &einoty, sizeof(struct inotify_event));
//		}
//
//		if (FD_ISSET(d_inoty, &readset)){
//
//			std::cout << "CPinCtl::Notifier: Inotify read " << std::endl;
//
//			struct inotify_event einoty;
//			ssize_t rdlen=0;
//			rdlen = read(d_inoty, (char*) &einoty, sizeof(struct inotify_event));
//		}
//
//		if (FD_ISSET(d_inoty, &writeset)){
//
//			std::cout << "CPinCtl::Notifier: Inotify write " << std::endl;
//
//			struct inotify_event einoty;
//			ssize_t rdlen=0;
//			rdlen = read(d_inoty, (char*) &einoty, sizeof(struct inotify_event));
//			if (rdlen){
//
//				if (einoty.mask){
//
//					TPinData pindata;
//					for (auto pin: PinData)
//					{
//						if (pin.watch == einoty.wd)
//						{
//							pindata = pin;
//							break;
//						}
//					}
//
//					uint32_t mask = einoty.mask & pindata.events;
//
//					// Calling callback functions
//					if (mask & IN_MODIFY)
//					{
//						std::cout << "CPinCtl::Notifier: Inotify found modify file " << pindata.name << std::endl;
//
//						lseek(pindata.fd, 0, SEEK_SET);
//						char Value = 0;
//						size_t size = read(pindata.fd, &Value, 1);
//						if (size == 1)
//						{
//							std::cout << "GPIO was changed: " << Value << std::endl;
//							std::vector<uint8_t> data;
//							data.push_back(Value);
//
//							auto it = busyPins.find(pindata.name);
//							if ( it != busyPins.end())
//							{
//								it->second->myDevice().performEvent(data);
//							}
//						}
//					}
//
//				}
//			}
//		}
	}

	std::cout << "Notifier exits" << std::endl;

}
