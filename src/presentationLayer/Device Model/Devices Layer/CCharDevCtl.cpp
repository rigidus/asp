#include <CCharDevCtl.h>
#include "SetCommandTo.h"
#include <sys/inotify.h>

namespace boostio = boost::iostreams;
namespace boostfs = boost::filesystem;

const std::string CCharDevCtl::s_name = "scn_usb";
const std::string CCharDevCtl::devicePath = "/dev/input/";
std::map<std::string, shared_ptr<CBaseCommCtl> > CCharDevCtl::busyCharDevs;
boost::thread* CCharDevCtl::thrNotify = nullptr;
bool CCharDevCtl::stopFlag = false;

bool CCharDevCtl::checkFiles()
{
	std::string devicePath = CCharDevCtl::devicePath + CCharDevCtl::s_name;

	if ( CCharDevCtl::fileIsExist( devicePath ) == false)
	{
		std::cout << "ERROR! CCharDevCtl::takeCommCtl getting '" << CCharDevCtl::s_name << "'  failed: device not found" << std::endl;
		return false;
	}

	// OK! device is present

	return true;
}

void CCharDevCtl::setupDevice()
{

	std::cout << "CCharDevCtl::setupDevice: " << m_CharDevData.name << std::endl;

}

settings::CommCharDevConfig CCharDevCtl::getCharDevConfig(const std::string& deviceName)
{

	std::vector<settings::CommCharDevConfig> configList =
			settings::getCharDevsByDevice(deviceName);

	for (auto v: configList)
	{
		if ( v.name == deviceName )
			return v;
	}

	settings::CommCharDevConfig empty = { deviceName };
	return empty;
}


shared_ptr<CBaseCommCtl> CCharDevCtl::takeCommCtl(CBaseDevice* device, const std::string& deviceName)
{

	std::cout << "CCharDevCtl::takeCommCtl try to take " << deviceName << std::endl;

	try{
/*
		if (deviceName.size() < 5)
		{
			std::cout << "ERROR! CCharDevCtl::takeCommCtl: scanner name size < 5" << std::endl;
			return nullptr;
		}
*/
		// check files for pin

		boostio::stream_buffer<boostio::file_sink> bufExport(devicePath); //+"export");
		std::ostream fileExport(&bufExport);

		// TODO: grabli, значимая фиксированная позиция в строке
		fileExport << &deviceName[4]; // 4 - is gpio number position

		// Create TPinData
		std::string devicePath = CCharDevCtl::devicePath + deviceName;
		std::string fname(devicePath);

		TCharDevData chardevice;
		chardevice.filename = fname;
		chardevice.fd = -1;
		chardevice.events = IN_CLOSE;
		chardevice.watch = 0;
		chardevice.name = deviceName;
		chardevice.oldvalue = 0;

		settings::CommCharDevConfig config = getCharDevConfig(deviceName);

		// create CPinCtl for pinNum
		shared_ptr<CBaseCommCtl> deviceCtl( (CBaseCommCtl*) new CCharDevCtl(device, config, chardevice) );
		std::pair<std::string, shared_ptr<CBaseCommCtl> > pr(deviceName, deviceCtl);
		busyCharDevs.insert(pr);

		// OK! pin is made as busied and stored

		std::cout << "CCharDevCtl::takeCommCtl: take " << deviceName << " successfully" << std::endl;

		return deviceCtl;
	}

	catch(boost::exception& ex)
	{
		std::cout << "ERROR! CCharDevCtl::takeCommCtl: exception: " << boost::diagnostic_information(ex) << std::endl;
		return nullptr;
	}

	catch(std::exception& ex)
	{
		std::cout << "ERROR! CCharDevCtl::takeCommCtl: exception: " << ex.what() << std::endl;
		return nullptr;
	}

	catch(...)
	{
		std::cout << "ERROR! CCharDevCtl::takeCommCtl unknown exception: " << std::endl;
		return nullptr;
	}

	return nullptr;
}


void CCharDevCtl::freeCommCtl(CBaseDevice* device, const std::string& deviceName)
{

	if (device == nullptr) return;

	std::cout << "CCharDevCtl::freeCommCtl: getCharDevCtl try to free " << deviceName << std::endl;

	// check busy device

	auto it = busyCharDevs.find(deviceName);

	if ( it == busyCharDevs.end())
	{
		return;
	}
	// OK! device is busy

	shared_ptr<CBaseCommCtl> deviceCtl(it->second);
	if ( deviceCtl->m_deviceName != device->c_name)
	{
		return;
	}

	// free device
//	boostio::stream_buffer<boostio::file_sink> bufExport(gpioPath+"unexport");
//	std::ostream fileExport(&bufExport);
//	fileExport << gpioName;

	std::cout << "CCharDevCtl::freeCommCtl: " << deviceName << " is free" << std::endl;

	busyCharDevs.erase(it);
}

bool CCharDevCtl::fileIsExist(const std::string& fileName)
{
	boostfs::file_status fStatus = boostfs::status(fileName);
	return boostfs::is_regular(fStatus);
}

// Functions - non static members
CCharDevCtl::CCharDevCtl(CBaseDevice* device, const settings::CommCharDevConfig& config, TCharDevData& deviceData):
		CBaseCommCtl(device, deviceData.name),
		m_Config(config),
		m_CharDevData(deviceData),
		m_timeout(0)
{

}

CCharDevCtl::~CCharDevCtl(){

}

uint32_t CCharDevCtl::send(std::list<std::vector<uint8_t> > sendData)
{
	return 0;
}

uint32_t CCharDevCtl::send(std::vector<uint8_t> sendData)
{
	return 0;
}

int8_t CCharDevCtl::getCharDevValue(std::string &value) //!!!
{
	std::cout << "CCharDevCtl::getCharDevValue: From "  << m_CharDevData.filename << std::endl;
/*
	lseek(m_CharDevData.fd, 0, SEEK_SET);

	char Value = 0;
	size_t size = read(m_CharDevData.fd, &Value, 1);

	if (size != 1)
	{
		std::cout << "ERROR! CPinCtl::getPinValue: Not successful read from " << m_PinData.filename << std::endl;
		return -1;
	}

	std::cout << "CPinCtl::getPinValue: From " << m_PinData.filename <<", got value = " << Value << std::endl;
*/


	return 0;
}

void CCharDevCtl::startNotifier()
{

	thrNotify = new boost::thread(Notifier);

}

void CCharDevCtl::stopNotifier()
{
	stopFlag = true;
	thrNotify->join();
	delete thrNotify;

}

void CCharDevCtl::Notifier()
{

	int d_inoty = inotify_init();

	std::cout << "CharDev Notifier started with " << busyCharDevs.size() << " pin controls" << std::endl;

	for (auto busyCharDev: busyCharDevs)
	{
		CCharDevCtl* pctl = (CCharDevCtl*) busyCharDev.second.get();
		if ( pctl == nullptr)
		{
			std::cout << "ERROR! CCharDevCtl::Notifier didn't find pointer to character device: " << busyCharDev.first << ". Notifier exits." << std::endl;
 			return;
		}

		if (pctl->checkFiles() == false)
		{
			std::cout << "ERROR! CCharDevCtl::Notifier didn't find important file of character device: " << busyCharDev.first << ". Notifier exits." << std::endl;
 			return;
		}

		pctl->setupDevice();

		TCharDevData& CharDevData = pctl->m_CharDevData;
		CharDevData.fd = open( CharDevData.filename.c_str(), O_RDONLY | O_NONBLOCK);
		if (CharDevData.fd == -1)
		{
			std::cout << "ERROR! CCharDevCtl::Notifier didn't open file: " << CharDevData.filename << ". Notifier exits." << std::endl;
 			return;
		}

		CharDevData.watch =  inotify_add_watch(d_inoty, CharDevData.filename.c_str(), CharDevData.events);
	}

/*
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

						it->second->myDevice().performEvent(PinData.name, data);

						std::cout << "GPIO Notifier: Device Event Ready for: " << it->second->myDevice().c_name << std::endl;
					}
				}
			}

		}
	}
*/
	std::cout << "Notifier exits" << std::endl;

}
