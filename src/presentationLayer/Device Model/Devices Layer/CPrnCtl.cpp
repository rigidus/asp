///////////////////////////////////////////////////////////
//  CPrnCtl.cpp
//  Implementation of the Class CPrnCtl
///////////////////////////////////////////////////////////

#include "CPrnCtl.h"
#include "SetCommandTo.h"
#include <sys/ioctl.h>
#include <iostream>
#include <fstream>
#include <stdlib.h>

namespace boostio = boost::iostreams;
namespace boostfs = boost::filesystem;

const std::string CPrnCtl::s_name = "prn_usb";
const std::string CPrnCtl::printerPath = "/dev/usb";

bool CPrnCtl::checkFiles()
{
	std::string printerPath = CPrnCtl::printerPath + "/" + m_PrinterData.name;

	if ( CPrnCtl::fileIsExist( printerPath ) == false)
	{
		{
			std::stringstream log;
			log << "ERROR! CPrnCtl::takeCommCtl getting '" << m_PrinterData.name << "'  failed: file not found";
			SetTo::LocalLog(m_deviceName, error, log.str());
		}
		return false;
	}

	return true;
}

void CPrnCtl::setupPrinter()
{

	{
		std::stringstream log;
		log << "CPrnCtl::setupPrinter: " << m_PrinterData.name;
		SetTo::LocalLog(m_deviceName, trace, log.str());
	}

	//const settings::CommDisplayConfig& config = m_Config; //!!!fix: use it

	std::string printerPath = CPrnCtl::printerPath + "/" + m_PrinterData.name;

	{
		std::stringstream log;
		log << "CPrnCtl::setupPrinter: setting on = " << printerPath << " [fake]";
		SetTo::LocalLog(m_deviceName, debug, log.str());
	}

}

settings::CommPrinterConfig CPrnCtl::getPrinterConfig(CBaseDevice* device, const std::string& printerName)
{

	std::vector<settings::CommPrinterConfig> configList =
			settings::getPrinterByDevice(device->c_name); //, printerName);

	for (auto v: configList)
	{
		if ( v.name == printerName )
		{
			{
				std::stringstream log;
				log << "getPrinterConfig: " << printerName;
				SetTo::LocalLog(device->c_name, trace, log.str());
			}

			return v;
		}
	}

	settings::CommPrinterConfig empty = { printerName, "/dev/usb/lp0" }; //!!!fix hardcode

	{
		std::stringstream log;
		log << "getPrinterConfig: returning EMPTY! " << printerName;
		SetTo::LocalLog(device->c_name, error, log.str());
	}

	return empty;
}

shared_ptr<CBaseCommCtl> CPrnCtl::takeCommCtl(CBaseDevice* device, const std::string& printerName)
{

	{
		std::stringstream log;
		log << "CPrinterCtl::takeCommCtl try to take " << printerName;
		SetTo::LocalLog(device->c_name, trace, log.str());
	}

	try{

		{
			std::stringstream log;
			log << "CPrinterCtl::takeCommCtl: getting " << printerPath << printerName;
			SetTo::LocalLog(device->c_name, trace, log.str());
		}

		boostio::stream_buffer<boostio::file_sink> bufExport(printerPath+printerName);
		std::ostream fileExport(&bufExport);


		// Create TPinData
		{
			std::stringstream log;
			log << "CPrnCtl::takeCommCtl: create fname " << printerPath << printerName;
			SetTo::LocalLog(device->c_name, trace, log.str());
		}
		std::string fname(CPrnCtl::printerPath + printerName);

		TPrinterData printer;
		printer.filename = fname;
		printer.fd = -1;
		printer.name = printerName;

		{
			std::stringstream log;
			log << "CPrinterCtl::takeCommCtl: getPrinterConfig " << printerPath << printerName << printerName;
			SetTo::LocalLog(device->c_name, trace, log.str());
		}
		settings::CommPrinterConfig config = getPrinterConfig(device, printerName);

		// create CDisplayCtl for displayNum
		{
			std::stringstream log;
			log << "ATTENTION! CPrinterCtl::takeCommCtl: New CPrnCtl " << printerPath << printerName;
			SetTo::LocalLog(device->c_name, trace, log.str());
		}

		shared_ptr<CBaseCommCtl> printerCtl( (CBaseCommCtl*) new CPrnCtl(device, config, printer) );

		{
			std::stringstream log;
			log << "ATTENTION! CPrnCtl::takeCommCtl: pr(printerName, printerCtl) " << printerPath << printerName;
			SetTo::LocalLog(device->c_name, trace, log.str());
		}

		std::pair<std::string, shared_ptr<CBaseCommCtl> > pr(printerName, printerCtl);

		{
			std::stringstream log;
			log << "CPrnCtl::takeCommCtl: take " << printerName << " successfully [fake]";
			SetTo::LocalLog(device->c_name, trace, log.str());
		}

		return printerCtl;
	}

	catch(boost::exception& ex)
	{
		{
			std::stringstream log;
			log << "ERROR! CPrnCtl::takeCommCtl: exception: " << boost::diagnostic_information(ex);
			SetTo::LocalLog(device->c_name, error, log.str());
		}
		return nullptr;
	}

	catch(std::exception& ex)
	{
		{
			std::stringstream log;
			log << "ERROR! CPrnCtl::takeCommCtl: exception: " << ex.what();
			SetTo::LocalLog(device->c_name, error, log.str());
		}
		return nullptr;
	}

	catch(...)
	{
		SetTo::LocalLog(device->c_name, error, "ERROR! CPrnCtl::takeCommCtl: unknown exception");
		return nullptr;
	}

	return nullptr;
}

void CPrnCtl::freeCommCtl(CBaseDevice* device, const std::string& printerName)
{

	if (device == nullptr)
	{
		SetTo::CommonLog(error, "CPrnCtl::freeCommCtl: No device");
		return;
	}

	{
		std::stringstream log;
		log << "CPrnCtl::freeCommCtl: Try to free " << printerName;
		SetTo::LocalLog(device->c_name, trace, log.str());
	}

	{
		std::stringstream log;
		log << "CPrnCtl::freeCommCtl: " << printerName << " is free [fake]";
		SetTo::LocalLog(device->c_name, debug, log.str());
	}

}

bool CPrnCtl::fileIsExist(const std::string& fileName)
{
	boostfs::file_status fStatus = boostfs::status(fileName);
	return boostfs::is_regular(fStatus);
}

// Functions - non static members
CPrnCtl::CPrnCtl(CBaseDevice* device, const settings::CommPrinterConfig& config, TPrinterData& printerData):
		CBaseCommCtl(device, printerData.name),
		m_Config(config),
		m_PrinterData(printerData),
		m_timeout(0)
{

}

CPrnCtl::~CPrnCtl(){

}

uint32_t CPrnCtl::send(std::vector<uint8_t> sendData)
{
	std::list<std::vector<uint8_t> > lst;

	{
		std::stringstream log;
		log << "CPrnCtl::send: <vector> sendData.size == " << sendData.size();
		SetTo::LocalLog(m_deviceName, trace, log.str());
	}

	lst.push_back(sendData);

	return send(lst);
}

uint32_t CPrnCtl::send(std::list<std::vector<uint8_t> > sendData)
{
	{
		std::stringstream log;
		log << "CPrnCtl::send: command 'write data' to '" << m_PrinterData.name << "'." << sendData.size();
		SetTo::LocalLog(m_deviceName, trace, log.str());
	}

	/*
	 * Парсинг команды записи в конкретный файл пина
	 * value, direction, edge, active_low
	 */

	{
		std::stringstream log;
		log << "CPrnCtl::send: list<vector> sendData.size == " << sendData.size();
		SetTo::LocalLog(m_deviceName, trace, log.str());
	}

	if ( sendData.size() == 1 ) //!!!fix "&& sendData.begin()->size() > 2 )"
	{
		std::vector<uint8_t>& data = sendData.back();

		{
			std::stringstream log;
			log << "CPrnCtl::send: data.size is " << data.size();
			SetTo::LocalLog(m_deviceName, trace, log.str());
		}

		uint8_t cmdtype = data[0];

		uint32_t len = 0;
		int32_t fd;
		char* beginData = (char*) &data[1];
		char* endData = (char*) &data[data.size()-1];
		std::string value(beginData, endData);
		std::ofstream myfile ("/aspp/tmpcheck.html");

		switch(cmdtype) //!!!fix
		{
		case '0': // clear - not listed
			{
				std::stringstream log;
				log << "CPrnCtl::send: cmdtype is CLEAR";
				SetTo::LocalLog(m_deviceName, debug, log.str());
			}
			break;

		case '1': // print
			{
				std::stringstream log;
				log << "CPrnCtl::send: cmdtype is PRINT";
				SetTo::LocalLog(m_deviceName, debug, log.str());
			}

			if (!(myfile.is_open()))
			{
				{
					std::stringstream log;
					log << "ERROR! CPrnCtl::send: File '" << "/aspp/tmpcheck.html" << "' isn't opened for writing.";
					SetTo::LocalLog(m_deviceName, error, log.str());
				}
				return 0;
			}

			myfile << value.c_str();
			myfile.close();

			std::system("wkhtmltopdf.sh --margin-top 1mm --margin-bottom 1mm --margin-left 1mm --margin-right 1mm --page-width 80mm --page-height 120mm /aspp/tmpcheck.html /aspp/tmpcheck.pdf"); // -s A6
			std::system("lp /aspp/tmpcheck.pdf");


			break;
		default:
			{
				std::stringstream log;
				log << "ERROR! CPrnCtl::send: cmdtype " << cmdtype << " incorrect";
				SetTo::LocalLog(m_deviceName, error, log.str());
			}
			return 0;
		}


		return len;
	}

	if ( sendData.size() != 1) //!!!fix
	{
		{
			std::stringstream log;
			log << "ERROR! CPrnCtl::send: Incorrect argument size: list size = "
					<< sendData.size();
			SetTo::LocalLog(m_deviceName, error, log.str());
		}
	}
	else
	if ( sendData.begin()->size() < 2 ) //!!!fix
	{
		{
			std::stringstream log;
			log << "ERROR! CPrnCtl::send: Incorrect argument size: vector size = "
					<< sendData.begin()->size();
			SetTo::LocalLog(m_deviceName, error, log.str());
		}
	}

	return  0;
}
