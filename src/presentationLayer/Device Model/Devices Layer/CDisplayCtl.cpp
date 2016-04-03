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

bool CDisplayCtl::checkFiles()
{
	std::string displayPath = CDisplayCtl::displayPath + "/" + m_DisplayData.name;

	if ( CDisplayCtl::fileIsExist( displayPath ) == false)
	{
		{
			std::stringstream log;
			log << "ERROR! CDisplayCtl::takeCommCtl getting '" << m_DisplayData.name << "'  failed: file not found";
			SetTo::LocalLog(m_deviceName, error, log.str());
		}
		return false;
	}

	return true;
}


void CDisplayCtl::setupDisplay()
{
	{
		std::stringstream log;
		log << "CDisplayCtl::setupDisplay: " << m_DisplayData.name;
		SetTo::LocalLog(m_deviceName, trace, log.str());
	}

	std::string displayPath = CDisplayCtl::displayPath + "/" + m_DisplayData.name;

	//!!!fix display setup
	{
		std::stringstream log;
		log << "CDisplayCtl::setupDisplay: setting on = " << displayPath << " [fake]";
		SetTo::LocalLog(m_deviceName, debug, log.str());
	}
}


settings::CommDisplayConfig CDisplayCtl::getDisplayConfig(CBaseDevice* device, const std::string& displayName)
{

	std::vector<settings::CommDisplayConfig> configList =
			settings::getDisplayByDevice(device->c_name); //, displayName);

	for (auto v: configList)
	{
		if ( v.name == displayName )
		{
			{
				std::stringstream log;
				log << "CDisplayCtl::getDisplayConfig: " << displayName;
				SetTo::LocalLog(device->c_name, debug, log.str());
			}

			return v;
		}
	}

	settings::CommDisplayConfig empty = { displayName, "/dev/klcd" }; //!!!fix hardcode

	{
		std::stringstream log;
		log << "CDisplayCtl::getDisplayConfig: returning EMPTY! " << displayName;
		SetTo::LocalLog(device->c_name, error, log.str());
	}

	return empty;
}


shared_ptr<CBaseCommCtl> CDisplayCtl::takeCommCtl(CBaseDevice* device, const std::string& displayName)
{

	{
		std::stringstream log;
		log << "CDisplayCtl::takeCommCtl try to take " << displayName;
		SetTo::LocalLog(device->c_name, trace, log.str());
	}

	try{

		{
			std::stringstream log;
			log << "CDisplayCtl::takeCommCtl: getting " << displayPath << displayName;
			SetTo::LocalLog(device->c_name, trace, log.str());
		}
		boostio::stream_buffer<boostio::file_sink> bufExport(displayPath+displayName);
		std::ostream fileExport(&bufExport);

		// Create TPinData
		{
			std::stringstream log;
			log << "CDisplayCtl::takeCommCtl: create fname " << displayPath << displayName;
			SetTo::LocalLog(device->c_name, trace, log.str());
		}
		std::string fname(CDisplayCtl::displayPath + displayName);

		TDisplayData display;
		display.filename = fname;
		display.fd = -1;
		display.name = displayName;

		{
			std::stringstream log;
			log << "CDisplayCtl::takeCommCtl: getDisplayConfig " << displayPath << displayName;
			SetTo::LocalLog(device->c_name, trace, log.str());
		}
		settings::CommDisplayConfig config = getDisplayConfig(device, displayName);

		// create CDisplayCtl for displayNum
		{
			std::stringstream log;
			log << "CDisplayCtl::takeCommCtl: New CDisplayCtl " << displayPath << displayName;
			SetTo::LocalLog(device->c_name, trace, log.str());
		}

		shared_ptr<CBaseCommCtl> displayCtl( (CBaseCommCtl*) new CDisplayCtl(device, config, display) );
		{
			std::stringstream log;
			log << "CDisplayCtl::takeCommCtl: pr(displayName, displayCtl) " << displayPath << displayName;
			SetTo::LocalLog(device->c_name, trace, log.str());
		}
		std::pair<std::string, shared_ptr<CBaseCommCtl> > pr(displayName, displayCtl);

		{
			std::stringstream log;
			log << "CDisplayCtl::takeCommCtl: take " << displayName << " successfully [fake]";
			SetTo::LocalLog(device->c_name, debug, log.str());
		}

		return displayCtl;
	}

	catch(boost::exception& ex)
	{
		{
			std::stringstream log;
			log << "ERROR! CDisplayCtl::takeCommCtl: exception: " << boost::diagnostic_information(ex);
			SetTo::LocalLog(device->c_name, error, log.str());
		}
		return nullptr;
	}

	catch(std::exception& ex)
	{
		{
			std::stringstream log;
			log << "ERROR! CDisplayCtl::takeCommCtl: exception: " << ex.what();
			SetTo::LocalLog(device->c_name, error, log.str());
		}
		return nullptr;
	}

	catch(...)
	{
		SetTo::LocalLog(device->c_name, error, "ERROR! CDisplayCtl::takeCommCtl unknown exception");
		return nullptr;
	}

	return nullptr;
}


void CDisplayCtl::freeCommCtl(CBaseDevice* device, const std::string& displayName)
{

	{
		std::stringstream log;
		log << "CDisplayCtl::freeCommCtl: getDisplayCtl try to free " << displayName;
		SetTo::LocalLog(device->c_name, trace, log.str());
	}

	if (device == nullptr) return;

	{
		std::stringstream log;
		log << "CDisplayCtl::freeCommCtl: " << displayName << " is free [fake]";
		SetTo::LocalLog(device->c_name, debug, log.str());
	}
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
	{
		std::stringstream log;
		log << "CDisplayCtl::send: <vector> sendData.size == " << sendData.size();
		SetTo::LocalLog(m_deviceName, trace, log.str());
	}
	lst.push_back(sendData);

	return send(lst);
}

uint32_t CDisplayCtl::utf8towinstar(std::string* const value)
{
	size_t iWnstr = 0;
	for (size_t iUTF8 = 0; iUTF8 < value->size(); iUTF8++, iWnstr++)
	{
		switch ((uint8_t) (*value)[iUTF8])
		{
		case 0xD0:

			if (++iUTF8 == value->size())
			{
				{
					std::stringstream log;
					log << "CDisplayCtl::utf8towinstar: unexpected end of string " << value[iUTF8];
					SetTo::LocalLog(m_deviceName, error, log.str());
				}
				return 0;
			}

			switch ((uint8_t) (*value)[iUTF8])
			{
			case 0x90: //A
				(*value)[iWnstr] = 0x41;
				break;
			case 0x91: //Б
				(*value)[iWnstr] = 0xA0;
				break;
			case 0x92: //В
				(*value)[iWnstr] = 0x42;
				break;
			case 0x93: //Г
				(*value)[iWnstr] = 0xA1;
				break;
			case 0x94: //Д
				(*value)[iWnstr] = 0xE0;
				break;
			case 0x95: //Е
				(*value)[iWnstr] = 0x45;
				break;
			case 0x81: //Ё
				(*value)[iWnstr] = 0xA2;
				break;
			case 0x96: //Ж
				(*value)[iWnstr] = 0xA3;
				break;
			case 0x97: //З
				(*value)[iWnstr] = 0xA4;
				break;
			case 0x98: //И
				(*value)[iWnstr] = 0xA5;
				break;
			case 0x99: //Й
				(*value)[iWnstr] = 0xA6;
				break;
			case 0x9A: //К
				(*value)[iWnstr] = 0x4B;
				break;
			case 0x9B: //Л
				(*value)[iWnstr] = 0xA7;
				break;
			case 0x9C: //М
				(*value)[iWnstr] = 0x4D;
				break;
			case 0x9D: //Н
				(*value)[iWnstr] = 0x48;
				break;
			case 0x9E: //О
				(*value)[iWnstr] = 0x4F;
				break;
			case 0x9F: //П
				(*value)[iWnstr] = 0xA8;
				break;
			case 0xA0: //Р
				(*value)[iWnstr] = 0x50;
				break;
			case 0xA1: //С
				(*value)[iWnstr] = 0x43;
				break;
			case 0xA2: //Т
				(*value)[iWnstr] = 0x54;
				break;
			case 0xA3: //У
				(*value)[iWnstr] = 0xA9;
				break;
			case 0xA4: //Ф
				(*value)[iWnstr] = 0xAA;
				break;
			case 0xA5: //Х
				(*value)[iWnstr] = 0x58;
				break;
			case 0xA6: //Ц
				(*value)[iWnstr] = 0xE1;
				break;
			case 0xA7: //Ч
				(*value)[iWnstr] = 0xAB;
				break;
			case 0xA8: //Ш
				(*value)[iWnstr] = 0xAC;
				break;
			case 0xA9: //Щ
				(*value)[iWnstr] = 0xE2;
				break;
			case 0xAA: //Ъ
				(*value)[iWnstr] = 0xAD;
				break;
			case 0xAB: //Ы
				(*value)[iWnstr] = 0xAE;
				break;
			case 0xAC: //Ь
				(*value)[iWnstr] = 0x62;
				break;
			case 0xAD: //Э
				(*value)[iWnstr] = 0xAF;
				break;
			case 0xAE: //Ю
				(*value)[iWnstr] = 0xB0;
				break;
			case 0xAF: //Я
				(*value)[iWnstr] = 0xB1;
				break;
			case 0xB0: //а
				(*value)[iWnstr] = 0x61;
				break;
			case 0xB1: //б
				(*value)[iWnstr] = 0xB2;
				break;
			case 0xB2: //в
				(*value)[iWnstr] = 0xB3;
				break;
			case 0xB3: //г
				(*value)[iWnstr] = 0xB4;
				break;
			case 0xB4: //д
				(*value)[iWnstr] = 0xE3;
				break;
			case 0xB5: //е
				(*value)[iWnstr] = 0x65;
				break;
			case 0xB6: //ж
				(*value)[iWnstr] = 0xB6;
				break;
			case 0xB7: //з
				(*value)[iWnstr] = 0xB7;
				break;
			case 0xB8: //и
				(*value)[iWnstr] = 0xB8;
				break;
			case 0xB9: //й
				(*value)[iWnstr] = 0xB9;
				break;
			case 0xBA: //к
				(*value)[iWnstr] = 0xBA;
				break;
			case 0xBB: //л
				(*value)[iWnstr] = 0xBB;
				break;
			case 0xBC: //м
				(*value)[iWnstr] = 0xBC;
				break;
			case 0xBD: //н
				(*value)[iWnstr] = 0xBD;
				break;
			case 0xBE: //о
				(*value)[iWnstr] = 0x6F;
				break;
			case 0xBF: //п
				(*value)[iWnstr] = 0xBE;
				break;
			default:
				{
					std::stringstream log;
					log << "CDisplayCtl::utf8towinstar: wrong decoding D0 " << value[iUTF8];
					SetTo::LocalLog(m_deviceName, error, log.str());
				}
				break;
			}
			break;

		case 0xD1:

			if (++iUTF8 == value->size())
			{
				{
					std::stringstream log;
					log << "CDisplayCtl::utf8towinstar: unexpected end of string " << value[iUTF8];
					SetTo::LocalLog(m_deviceName, error, log.str());
				}
				return 0;
			}

			switch ((uint8_t) (*value)[iUTF8])
			{
			case 0x80: //р
				(*value)[iWnstr] = 0x70;
				break;
			case 0x81: //с
				(*value)[iWnstr] = 0x63;
				break;
			case 0x82: //т
				(*value)[iWnstr] = 0xBF;
				break;
			case 0x83: //у
				(*value)[iWnstr] = 0x79;
				break;
			case 0x84: //ф
				(*value)[iWnstr] = 0xE4;
				break;
			case 0x85: //х
				(*value)[iWnstr] = 0x78;
				break;
			case 0x86: //ц
				(*value)[iWnstr] = 0xE5;
				break;
			case 0x87: //ч
				(*value)[iWnstr] = 0xC0;
				break;
			case 0x88: //ш
				(*value)[iWnstr] = 0xC1;
				break;
			case 0x89: //щ
				(*value)[iWnstr] = 0xE6;
				break;
			case 0x8A: //ъ
				(*value)[iWnstr] = 0xC2;
				break;
			case 0x8B: //ы
				(*value)[iWnstr] = 0xC3;
				break;
			case 0x8C: //ь
				(*value)[iWnstr] = 0xC4;
				break;
			case 0x8D: //э
				(*value)[iWnstr] = 0xC5;
				break;
			case 0x8E: //ю
				(*value)[iWnstr] = 0xC6;
				break;
			case 0x8F: //я
				(*value)[iWnstr] = 0xC7;
				break;
			case 0x91: //ё
				(*value)[iWnstr] = 0xB5;
				break;
			default:
				{
					std::stringstream log;
					log << "CDisplayCtl::utf8towinstar: wrong decoding D1 " << value[iUTF8];
					SetTo::LocalLog(m_deviceName, error, log.str());
				}
				break;
			}
			break;

		case 0xE2: //№

			if (++iUTF8 == value->size())
			{
				{
					std::stringstream log;
					log << "CDisplayCtl::utf8towinstar: unexpected end of string " << value[iUTF8];
					SetTo::LocalLog(m_deviceName, error, log.str());
				}
				return 0;
			}

			switch ((uint8_t) (*value)[iUTF8])
			{
			case 0x84: //№

				if (++iUTF8 == value->size())
				{
					{
						std::stringstream log;
						log << "CDisplayCtl::utf8towinstar: unexpected end of string " << value[iUTF8];
						SetTo::LocalLog(m_deviceName, error, log.str());
					}
					return 0;
				}

				switch ((uint8_t) (*value)[iUTF8])
				{
				case 0x96: //№
					(*value)[iWnstr] = 0xCC;
					break;
				default:
					{
						std::stringstream log;
						log << "CDisplayCtl::utf8towinstar: wrong decoding E284 " << value[iUTF8];
						SetTo::LocalLog(m_deviceName, error, log.str());
					}
					break;
				}
				break;
			default:
				{
					std::stringstream log;
					log << "CDisplayCtl::utf8towinstar: wrong decoding E2 " << value[iUTF8];
					SetTo::LocalLog(m_deviceName, error, log.str());
				}
				break;
			}
			break;

		default:
			(*value)[iWnstr] = (*value)[iUTF8];
			break;
		}
	}

	value->resize(iWnstr);

	return 0;
}

uint32_t CDisplayCtl::send(std::list<std::vector<uint8_t> > sendData)
{
	{
		std::stringstream log;
		log << "CDisplayCtl::send: command 'write data' to '" << m_DisplayData.name << "'.";
		SetTo::LocalLog(m_deviceName, trace, log.str());
	}

	/*
	 * Парсинг команды записи в конкретный файл пина
	 * value, direction, edge, active_low
	 */

	{
		std::stringstream log;
		log << "CDisplayCtl::send: list<vector> sendData.size == " << sendData.size();
		SetTo::LocalLog(m_deviceName, trace, log.str());
	}

	if ( sendData.size() == 1 ) //!!!fix "&& sendData.begin()->size() > 2 )"
	{
		std::string fname(displayPath + m_DisplayData.name);

		std::vector<uint8_t>& data = sendData.back();

		{
			std::stringstream log;
			log << "CDisplayCtl::send: data.size is " << data.size();
			SetTo::LocalLog(m_deviceName, trace, log.str());
		}

		uint8_t cmdtype = data[0];
		uint8_t screenId;

		std::string value; //(beginData, endData);

		int32_t fd = open( fname.c_str(), O_WRONLY | O_NONBLOCK);
		if (fd == -1)
		{
			{
				std::stringstream log;
				log << "ERROR! CDisplayCtl::send: File '" << fname << "' isn't opened for writing.";
				SetTo::LocalLog(m_deviceName, error, log.str());
			}
			return 0;
		}

		uint32_t len = 0;

		switch(cmdtype) //!!!fix
		{
		case '0': // clear
			{
				std::stringstream log;
				log <<"CDisplayCtl::send: cmdtype is CLEAR";
				SetTo::LocalLog(m_deviceName, trace, log.str());
			}

			if( ioctl( fd, (unsigned int) IOCTL_CLEAR_DISPLAY, value.c_str()) < 0)
			{
				std::stringstream log;
				log <<"ERROR! CDisplayCtl::send: IOCTL_CLEAR_DISPLAY";
				SetTo::LocalLog(m_deviceName, trace, log.str());
			}

			close(fd);

			return len;
			break;

		case '1': // show
			{
				std::stringstream log;
				log <<"CDisplayCtl::send: cmdtype is SHOW";
				SetTo::LocalLog(m_deviceName, trace, log.str());
			}

			screenId = data[1];

			{
				std::stringstream log;

				switch (screenId)
				{
				case 0:
					log << "CDisplayCtl::send: showing screenId 0" << std::endl;
					SetTo::LocalLog(m_deviceName, trace, log.str());
					value = "Нажмите кнопку";
					break;
				case 1:
					log << "CDisplayCtl::send: showing screenId 1" << std::endl;
					SetTo::LocalLog(m_deviceName, trace, log.str());
					value = "Печатаем...";
					break;
				case 2:
					log << "CDisplayCtl::send: showing screenId 2" << std::endl;
					SetTo::LocalLog(m_deviceName, trace, log.str());
					value = "Заберите билет";
					break;
				case 3:
					log << "CDisplayCtl::send: showing screenId 3" << std::endl;
					SetTo::LocalLog(m_deviceName, trace, log.str());
					value = "Открываем...";
					break;
				case 4:
					log << "CDisplayCtl::send: showing screenId 4" << std::endl;
					SetTo::LocalLog(m_deviceName, trace, log.str());
					value = "Проезжайте!";
					break;
				case 5:
					log << "CDisplayCtl::send: showing screenId 5" << std::endl;
					SetTo::LocalLog(m_deviceName, trace, log.str());
					value = "Закрываем...";
					break;

				default:
					log << "ERROR: CDisplayCtl::send: no matching screenId!";
					SetTo::LocalLog(m_deviceName, error, log.str());
					break;
				}
			}
			break;
		default:
			{
				std::stringstream log;
				log << "ERROR! CDisplayCtl::send: cmdtype " << cmdtype << " incorrect";
				SetTo::LocalLog(m_deviceName, error, log.str());
			}
			return 0;
		}

		if( ioctl( fd, (unsigned int) IOCTL_CLEAR_DISPLAY, value.c_str()) < 0)
		{
			std::stringstream log;
			log << "ERROR! CDisplayCtl::send: IOCTL_CLEAR_DISPLAY";
			SetTo::LocalLog(m_deviceName, error, log.str());
		}

		close(fd);


		fd = open( fname.c_str(), O_WRONLY | O_NONBLOCK);
		if (fd == -1)
		{
			{
				std::stringstream log;
				log << "ERROR! CDisplayCtl::send: File '" << fname << "' isn't opened for writing.";
				SetTo::LocalLog(m_deviceName, error, log.str());
			}
			return 0;
		}

		{
			std::stringstream log;
			log << "CDisplayCtl::send: to " << fname.c_str() << " command "
					<< IOCTL_PRINT_ON_FIRSTLINE << " line " << value.c_str();
			SetTo::LocalLog(m_deviceName, trace, log.str());
		}
		utf8towinstar(&value);

		{
			std::stringstream log;
			log << "CDisplayCtl::send: to " << fname.c_str() << " command "
					<< IOCTL_PRINT_ON_FIRSTLINE << " processed line " << value.c_str();
			SetTo::LocalLog(m_deviceName, trace, log.str());
		}

		if( ioctl( fd, (unsigned int) IOCTL_PRINT_ON_FIRSTLINE, value.c_str()) < 0)
		{
			std::stringstream log;
			log << "ERROR! CDisplayCtl::send: IOCTL_PRINT_DISPLAY";
			SetTo::LocalLog(m_deviceName, error, log.str());
		}

		close(fd);

		return len;
	}

	if ( sendData.size() != 1) //!!!fix
	{
		std::stringstream log;
		log << "ERROR! CDisplayCtl::send: Incorrect argument size: list size = "
				<< sendData.size();
		SetTo::LocalLog(m_deviceName, error, log.str());
	}
	else
	if ( sendData.begin()->size() < 2 ) //!!!fix
	{
		std::stringstream log;
		log << "ERROR! CDisplayCtl::send: Incorrect argument size: vector size = "
				<< sendData.begin()->size();
		SetTo::LocalLog(m_deviceName, error, log.str());
	}

	return  0;
}
