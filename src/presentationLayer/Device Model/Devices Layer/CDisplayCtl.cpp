///////////////////////////////////////////////////////////
//  CDisplayCtl.cpp
//  Implementation of the Class CDisplayCtl
///////////////////////////////////////////////////////////

#include "CDisplayCtl.h"
#include "SetCommandTo.h"
//#include <sys/inotify.h>

namespace boostio = boost::iostreams;
namespace boostfs = boost::filesystem;

const std::string CDisplayCtl::s_name = "klcd";
const std::string CDisplayCtl::displayPath = "/dev/";
//std::map<std::string, shared_ptr<CBaseCommCtl> > CDisplayCtl::busyDisplays;
//boost::thread* CDisplayCtl::thrNotify = nullptr;
//bool CDisplayCtl::stopFlag = false;

bool CDisplayCtl::checkFiles()
{
	std::string displayPath = CDisplayCtl::displayPath + "/" + m_DisplayData.name;

	if ( CDisplayCtl::fileIsExist( displayPath ) == false)
	{
		std::cout << "ERROR! CDisplayCtl::takeCommCtl getting '" << m_DisplayData.name << "'  failed: file not found" << std::endl;
		return false;
	}
/*
	if ( CPinCtl::fileIsExist( pinPath+"value" ) == false)
	{
		std::cout << "ERROR! CPinCtl::takeCommCtl getting '" << m_PinData.name << "' failed: value not found" << std::endl;
		return false;
	}

	if ( CPinCtl::fileIsExist( pinPath+"edge" ) == false)
	{
		std::cout << "ERROR! CPinCtl::takeCommCtl getting '" << m_PinData.name << "' failed: edge not found" << std::endl;
		return false;
	}

	if ( CPinCtl::fileIsExist( pinPath+"active_low" ) == false)
	{
		std::cout << "ERROR! CPinCtl::takeCommCtl getting '" << m_PinData.name << "' failed: active_low not found" << std::endl;
		return false;
	}
	// OK! pin is present
*/
	return true;
}


void CDisplayCtl::setupDisplay()
{

	std::cout << "CDisplayCtl::setupDisplay: " << m_DisplayData.name << std::endl;

	//const settings::CommDisplayConfig& config = m_Config; //!!!fix: use it

	std::string displayPath = CDisplayCtl::displayPath + "/" + m_DisplayData.name;

	//!!!fix display setup
	std::cout << "CDisplayCtl::setupDisplay: setting on = " << displayPath << " [fake]" << std::endl;
/*
	{
		std::string strActiveLow("0");
//		boostio::stream_buffer<boostio::file_sink> bufExport(pinPath+"active_low");
		std::ostream fileExport(&bufExport);

//		std::cout << "CDisplayCtl::setupDisplay: active_low = " << strActiveLow << std::endl;

		fileExport << strActiveLow;
	}

	{
		std::string strDir("in");
		if (config.direction)
			strDir = "out";

		boostio::stream_buffer<boostio::file_sink> bufExport(pinPath+"direction");
		std::ostream fileExport(&bufExport);

		std::cout << "CPinCtl::setupGPIO: direction = " << strDir << std::endl;

		fileExport << strDir;
	}

	{
		std::string strEdge("both");
		boostio::stream_buffer<boostio::file_sink> bufExport(pinPath+"edge");
		std::ostream fileExport(&bufExport);

		std::cout << "CPinCtl::setupGPIO: edge = " << strEdge << std::endl;

		fileExport << strEdge;
	}

	{
		std::string strDef("0");
		if (config.def_value)
			strDef = "1";

		boostio::stream_buffer<boostio::file_sink> bufExport(pinPath+"value");
		std::ostream fileExport(&bufExport);

		std::cout << "CPinCtl::setupGPIO: default value = " << strDef << std::endl;

		fileExport << strDef;
	}
*/
}


settings::CommDisplayConfig CDisplayCtl::getDisplayConfig(CBaseDevice* device, const std::string& displayName)
{

	std::vector<settings::CommDisplayConfig> configList =
			settings::getDisplayByDevice(device->c_name); //, displayName);
/*
	for (auto v: configList)
	{
		if ( v.name == displayName )
			return v;
	}
*/
	settings::CommDisplayConfig empty = { displayName, "/dev/klcd" }; //!!!fix hardcode
	return empty;
}


shared_ptr<CBaseCommCtl> CDisplayCtl::takeCommCtl(CBaseDevice* device, const std::string& displayName)
{

	std::cout << "CDisplayCtl::takeCommCtl try to take " << displayName << std::endl;

	try{

		std::cout << "ATTENTION! CDisplayCtl::takeCommCtl: getting " << displayPath << displayName << std::endl;
		boostio::stream_buffer<boostio::file_sink> bufExport(displayPath+displayName);
		std::ostream fileExport(&bufExport);

		// TODO: grabli, значимая фиксированная позиция в строке
//		fileExport << &gpioName[4]; // 4 - is gpio number position

		// Create TPinData
//		std::string displayPath = CDisplayCtl::displayPath + "/" + displayName + "/";
		std::cout << "ATTENTION! CDisplayCtl::takeCommCtl: create fname " << displayPath << displayName << std::endl;
		std::string fname(CDisplayCtl::displayPath + displayName);

		TDisplayData display;
		display.filename = fname;
		display.fd = -1;
		display.name = displayName;

		std::cout << "ATTENTION! CDisplayCtl::takeCommCtl: getDisplayConfig " << displayPath << displayName << std::endl;
		settings::CommDisplayConfig config = getDisplayConfig(device, displayName);

		// create CDisplayCtl for displayNum
		std::cout << "ATTENTION! CDisplayCtl::takeCommCtl: New CDisplayCtl " << displayPath << displayName << std::endl;

		shared_ptr<CBaseCommCtl> displayCtl( (CBaseCommCtl*) new CDisplayCtl(device, config, display) );
		std::cout << "ATTENTION! CDisplayCtl::takeCommCtl: pr(displayName, displayCtl) " << displayPath << displayName << std::endl;
		std::pair<std::string, shared_ptr<CBaseCommCtl> > pr(displayName, displayCtl);
//		busyPins.insert(pr);
//
		// OK! pin is made as busied and stored

		std::cout << "CDisplayCtl::takeCommCtl: take " << displayName << " successfully [fake]" << std::endl;

		return displayCtl;
	}

	catch(boost::exception& ex)
	{
		std::cout << "ERROR! CDisplayCtl::takeCommCtl: exception: " << boost::diagnostic_information(ex) << std::endl;
		return nullptr;
	}

	catch(std::exception& ex)
	{
		std::cout << "ERROR! CDisplayCtl::takeCommCtl: exception: " << ex.what() << std::endl;
		return nullptr;
	}

	catch(...)
	{
		std::cout << "ERROR! CDisplayCtl::takeCommCtl unknown exception: " << std::endl;
		return nullptr;
	}

	return nullptr;
}


void CDisplayCtl::freeCommCtl(CBaseDevice* device, const std::string& displayName)
{

	if (device == nullptr) return;

	std::cout << "CDisplayCtl::freeCommCtl: getDisplayCtl try to free " << displayName << std::endl;

//	// check busy device
//
//	auto it = busyPins.find(gpioName);
//
//	if ( it == busyPins.end())
//	{
//		return;
//	}
//	// OK! pin is busy
//
//	shared_ptr<CBaseCommCtl> displayCtl(it->second);
//	if ( displayCtl->m_deviceName != device->c_name)
//	{
//		return;
//	}
//
//	// free pin
//	boostio::stream_buffer<boostio::file_sink> bufExport(gpioPath+"unexport");
//	std::ostream fileExport(&bufExport);
//	fileExport << gpioName;

	std::cout << "CDisplayCtl::freeCommCtl: " << displayName << " is free [fake]" << std::endl;

//	busyPins.erase(it);
}


bool CDisplayCtl::fileIsExist(const std::string& fileName)
{
	boostfs::file_status fStatus = boostfs::status(fileName);
	return boostfs::is_regular(fStatus);
}


// Functions - non static members
CDisplayCtl::CDisplayCtl(CBaseDevice* device, const settings::CommDisplayConfig& config, TDisplayData& displayData):
		CBaseCommCtl(device, displayData.name),
		m_Config(config),
		m_DisplayData(displayData)
//		m_timeout(0)
{

}

CDisplayCtl::~CDisplayCtl(){

}


uint32_t CDisplayCtl::send(std::list<std::vector<uint8_t> > sendData)
{

	std::cout << "CDisplayCtl::send: command 'write data' to '" << m_DisplayData.name << "'." << std::endl;

	/*
	 * Парсинг команды записи в конкретный файл пина
	 * value, direction, edge, active_low
	 */

	if ( sendData.size() == 1 && sendData.begin()->size() > 2 )
	{
		std::string fname(displayPath + m_DisplayData.name);

		std::vector<uint8_t>& data = sendData.back();

		uint8_t cmdtype = data[0];

		char* beginData = (char*) &data[1];
		char* endData = (char*) &data[data.size()-1];
		std::string value(beginData, endData);

		switch(cmdtype) //!!!fix
		{
		/*
		case 0: // clear
			fname += "value";
			break;
		case 1: // direction
			fname += "direction";
			break;
		case 2:
			fname += "edge";
			break;
		*/
		default:
			std::cout << "ERROR! CDisplayCtl::send: cmdtype " << cmdtype << " incorrect" << std::endl;
			return 0;
		}

		int32_t fd = open( fname.c_str(), O_WRONLY | O_NONBLOCK);
		if (fd == -1)
		{
			std::cout << "ERROR! CDisplayCtl::send: File '" << fname << "' isn't opened for writing." << std::endl;
			return 0;
		}

		uint32_t len = write(fd, value.c_str(), value.size());

		close(fd);

		return len;
	}

	if ( sendData.size() != 1) //!!!fix
	{
		std::cout << "ERROR! CDisplayCtl::send: Incorrect argument size: list size = "
				<< sendData.size() << std::endl;
	}
	else
	if ( sendData.begin()->size() < 2 ) //!!!fix
	{
		std::cout << "ERROR! CDisplayCtl::send: Incorrect argument size: vector size = "
		<< sendData.begin()->size() << std::endl;
	}

	return  0;
}