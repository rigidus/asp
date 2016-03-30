///////////////////////////////////////////////////////////
//  CDisplayCtl.cpp
//  Implementation of the Class CDisplayCtl
///////////////////////////////////////////////////////////

#include "CDisplayCtl.h"
#include "SetCommandTo.h"
#include <sys/ioctl.h>
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

	for (auto v: configList)
	{
		if ( v.name == displayName )
		{
			std::cout << "getDisplayConfig: " << displayName << std::endl;

			return v;
		}
	}

	settings::CommDisplayConfig empty = { displayName, "/dev/klcd" }; //!!!fix hardcode
	std::cout << "getDisplayConfig: returning EMPTY! " << displayName << std::endl;
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
		m_DisplayData(displayData),
		m_timeout(0)
{

}

CDisplayCtl::~CDisplayCtl(){

}


uint32_t CDisplayCtl::send(std::vector<uint8_t> sendData)
{
	std::list<std::vector<uint8_t> > lst;
	std::cout << "CDisplayCtl::send: <vector> sendData.size == " << sendData.size() << std::endl;
	lst.push_back(sendData);

	return send(lst);
}

uint32_t CDisplayCtl::utf8towinstar(std::string* const value)
{
	std::string::iterator strItWinstar = value->begin();

	for (std::string::iterator strItOrig = value->begin(); strItOrig < value->end(); strItOrig++)
	{
		switch ((uint8_t) *strItOrig)
		{
		case 0xD0:

			if (++strItOrig == value->end())
			{
				std::cout << "CDisplayCtl::utf2winstar: unexpected end of string " << *strItOrig << std::endl;
				return 0;
			}

			switch ((uint8_t) *strItOrig)
			{
			case 0x90: //A
				*strItWinstar = 0x41;
				break;
			case 0x91: //Б
				*strItWinstar = 0xA0;
				break;
			case 0x92: //В
				*strItWinstar = 0x42;
				break;
			case 0x93: //Г
				*strItWinstar = 0xA1;
				break;
			case 0x94: //Д
				*strItWinstar = 0xE0;
				break;
			case 0x95: //Е
				*strItWinstar = 0x45;
				break;
			case 0x81: //Ё
				*strItWinstar = 0xA2;
				break;
			case 0x96: //Ж
				*strItWinstar = 0xA3;
				break;
			case 0x97: //З
				*strItWinstar = 0xA4;
				break;
			case 0x98: //И
				*strItWinstar = 0xA5;
				break;
			case 0x99: //Й
				*strItWinstar = 0xA6;
				break;
			case 0x9A: //К
				*strItWinstar = 0x4B;
				break;
			case 0x9B: //Л
				*strItWinstar = 0xA7;
				break;
			case 0x9C: //М
				*strItWinstar = 0x4D;
				break;
			case 0x9D: //Н
				*strItWinstar = 0x48;
				break;
			case 0x9E: //О
				*strItWinstar = 0x4F;
				break;
			case 0x9F: //П
				*strItWinstar = 0xA8;
				break;
			case 0xA0: //Р
				*strItWinstar = 0x50;
				break;
			case 0xA1: //С
				*strItWinstar = 0x43;
				break;
			case 0xA2: //Т
				*strItWinstar = 0x54;
				break;
			case 0xA3: //У
				*strItWinstar = 0xA9;
				break;
			case 0xA4: //Ф
				*strItWinstar = 0xAA;
				break;
			case 0xA5: //Х
				*strItWinstar = 0x58;
				break;
			case 0xA6: //Ц
				*strItWinstar = 0xE1;
				break;
			case 0xA7: //Ч
				*strItWinstar = 0xAB;
				break;
			case 0xA8: //Ш
				*strItWinstar = 0xAC;
				break;
			case 0xA9: //Щ
				*strItWinstar = 0xE2;
				break;
			case 0xAA: //Ъ
				*strItWinstar = 0xAD;
				break;
			case 0xAB: //Ы
				*strItWinstar = 0xAE;
				break;
			case 0xAC: //Ь
				*strItWinstar = 0x62;
				break;
			case 0xAD: //Э
				*strItWinstar = 0xAF;
				break;
			case 0xAE: //Ю
				*strItWinstar = 0xB0;
				break;
			case 0xAF: //Я
				*strItWinstar = 0xB1;
				break;
			case 0xB0: //а
				*strItWinstar = 0x61;
				break;
			case 0xB1: //б
				*strItWinstar = 0xB2;
				break;
			case 0xB2: //в
				*strItWinstar = 0xB3;
				break;
			case 0xB3: //г
				*strItWinstar = 0xB4;
				break;
			case 0xB4: //д
				*strItWinstar = 0xE3;
				break;
			case 0xB5: //е
				*strItWinstar = 0x65;
				break;
			case 0xB6: //ж
				*strItWinstar = 0xB6;
				break;
			case 0xB7: //з
				*strItWinstar = 0xB7;
				break;
			case 0xB8: //и
				*strItWinstar = 0xB8;
				break;
			case 0xB9: //й
				*strItWinstar = 0xB9;
				break;
			case 0xBA: //к
				*strItWinstar = 0xBA;
				break;
			case 0xBB: //л
				*strItWinstar = 0xBB;
				break;
			case 0xBC: //м
				*strItWinstar = 0xBC;
				break;
			case 0xBD: //н
				*strItWinstar = 0xBD;
				break;
			case 0xBE: //о
				*strItWinstar = 0x6F;
				break;
			case 0xBF: //п
				*strItWinstar = 0xBE;
				break;
			default:
				std::cout << "CDisplayCtl::utf2winstar: wrong decoding D0 " << *strItOrig << std::endl;
				break;
			}
			break;

		case 0xD1:

			if (++strItOrig == value->end())
			{
				std::cout << "CDisplayCtl::utf2winstar: unexpected end of string " << *strItOrig << std::endl;
				return 0;
			}

			switch ((uint8_t)*strItOrig)
			{
			case 0x80: //р
				*strItWinstar = 0x70;
				break;
			case 0x81: //с
				*strItWinstar = 0x63;
				break;
			case 0x82: //т
				*strItWinstar = 0xBF;
				break;
			case 0x83: //у
				*strItWinstar = 0x79;
				break;
			case 0x84: //ф
				*strItWinstar = 0xE4;
				break;
			case 0x85: //х
				*strItWinstar = 0x78;
				break;
			case 0x86: //ц
				*strItWinstar = 0xE5;
				break;
			case 0x87: //ч
				*strItWinstar = 0xC0;
				break;
			case 0x88: //ш
				*strItWinstar = 0xC1;
				break;
			case 0x89: //щ
				*strItWinstar = 0xE6;
				break;
			case 0x8A: //ъ
				*strItWinstar = 0xC2;
				break;
			case 0x8B: //ы
				*strItWinstar = 0xC3;
				break;
			case 0x8C: //ь
				*strItWinstar = 0xC4;
				break;
			case 0x8D: //э
				*strItWinstar = 0xC5;
				break;
			case 0x8E: //ю
				*strItWinstar = 0xC6;
				break;
			case 0x8F: //я
				*strItWinstar = 0xC7;
				break;
			case 0x91: //ё
				*strItWinstar = 0xB5;
				break;
			default:
				std::cout << "CDisplayCtl::utf2winstar: wrong decoding D1 " << *strItOrig << std::endl;
				break;
			}
			break;

		case 0xE2: //№

			if (++strItOrig == value->end())
			{
				std::cout << "CDisplayCtl::utf2winstar: unexpected end of string " << *strItOrig << std::endl;
				return 0;
			}

			switch ((uint8_t) *strItOrig)
			{
			case 0x84: //№

				if (++strItOrig == value->end())
				{
					std::cout << "CDisplayCtl::utf2winstar: unexpected end of string " << *strItOrig << std::endl;
					return 0;
				}

				switch ((uint8_t) *strItOrig)
				{
				case 0x96: //№
					*strItWinstar = 0xCC;
					break;
				default:
					std::cout << "CDisplayCtl::utf2winstar: wrong decoding E284 " << *strItOrig << std::endl;
					break;
				}
				break;
			default:
				std::cout << "CDisplayCtl::utf2winstar: wrong decoding E2 " << *strItOrig << std::endl;
				break;
			}
			break;

		default:
			*strItWinstar = *strItOrig;
			break;
		}

		strItWinstar++;

	}

	value->resize(std::distance(value->begin(), strItWinstar));

	return 0;
}

uint32_t CDisplayCtl::send(std::list<std::vector<uint8_t> > sendData)
{
	std::cout << "CDisplayCtl::send: command 'write data' to '" << m_DisplayData.name << "'." << std::endl;

	/*
	 * Парсинг команды записи в конкретный файл пина
	 * value, direction, edge, active_low
	 */

	std::cout << "CDisplayCtl::send: list<vector> sendData.size == " << sendData.size() << std::endl;

	if ( sendData.size() == 1 ) //!!!fix "&& sendData.begin()->size() > 2 )"
	{
		std::string fname(displayPath + m_DisplayData.name);

		std::vector<uint8_t>& data = sendData.back();

		std::cout << "CDisplayCtl::send: data.size is " << data.size() << std::endl;

		uint8_t cmdtype = data[0];
		uint8_t screenId;

//		char* beginData = (char*) &data[1];
//		char* endData = (char*) &data[data.size()-1];
		std::string value; //(beginData, endData);

		int32_t fd = open( fname.c_str(), O_WRONLY | O_NONBLOCK);
		if (fd == -1)
		{
			std::cout << "ERROR! CDisplayCtl::send: File '" << fname << "' isn't opened for writing." << std::endl;
			return 0;
		}

		uint32_t len = 0;

		switch(cmdtype) //!!!fix
		{
		case '0': // clear
			std::cout << "CDisplayCtl::send: cmdtype is CLEAR" << std::endl;
			if( ioctl( fd, (unsigned int) IOCTL_CLEAR_DISPLAY, value.c_str()) < 0)
				std::cout << "ERROR! CDisplayCtl::send: IOCTL_CLEAR_DISPLAY" << std::endl;

			close(fd);

			return len;
			break;

		case '1': // show
			std::cout << "CDisplayCtl::send: cmdtype is SHOW" << std::endl;

			screenId = data[1];

//			std::cout << "CDisplayCtl::send: screenId is " <<  << std::endl;

			switch (screenId)
			{
			case 0:
				std::cout << "CDisplayCtl::send: showing screenId 0" << std::endl;
				value = "Нажмите кнопку";
				break;
			case 1:
				std::cout << "CDisplayCtl::send: showing screenId 1" << std::endl;
				value = "Печатаем...";
				break;
			case 2:
				std::cout << "CDisplayCtl::send: showing screenId 2" << std::endl;
				value = "Заберите билет";
				break;
			case 3:
				std::cout << "CDisplayCtl::send: showing screenId 3" << std::endl;
				value = "Открываем...";
				break;
			case 4:
				std::cout << "CDisplayCtl::send: showing screenId 4" << std::endl;
				value = "Проезжайте!";
				break;
			case 5:
				std::cout << "CDisplayCtl::send: showing screenId 5" << std::endl;
				value = "Закрываем...";
				break;

			default:
				std::cout << "ERROR: CDisplayCtl::send: no matching screenId!" << std::endl;
			}


			break;
		default:
			std::cout << "ERROR! CDisplayCtl::send: cmdtype " << cmdtype << " incorrect" << std::endl;
			return 0;
		}

		if( ioctl( fd, (unsigned int) IOCTL_CLEAR_DISPLAY, value.c_str()) < 0)
			std::cout << "ERROR! CDisplayCtl::send: IOCTL_CLEAR_DISPLAY" << std::endl;

		close(fd);


		fd = open( fname.c_str(), O_WRONLY | O_NONBLOCK);
		if (fd == -1)
		{
			std::cout << "ERROR! CDisplayCtl::send: File '" << fname << "' isn't opened for writing." << std::endl;
			return 0;
		}
		std::cout << "CDisplayCtl::send: to " << fname.c_str() << " command " << IOCTL_PRINT_ON_FIRSTLINE << " line " << value.c_str() << std::endl;
		utf8towinstar(&value);
		std::cout << "CDisplayCtl::send: to " << fname.c_str() << " command " << IOCTL_PRINT_ON_FIRSTLINE << " processed line " << value.c_str() << std::endl;

		if( ioctl( fd, (unsigned int) IOCTL_PRINT_ON_FIRSTLINE, value.c_str()) < 0)
			std::cout << "ERROR! CDisplayCtl::send: IOCTL_PRINT_DISPLAY" << std::endl;
//		write(fd, value.c_str(), value.size());

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
