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
		std::cout << "ERROR! CPrnCtl::takeCommCtl getting '" << m_PrinterData.name << "'  failed: file not found" << std::endl;
		return false;
	}

	return true;
}

void CPrnCtl::setupPrinter()
{

	std::cout << "CPrnCtl::setupPrinter: " << m_PrinterData.name << std::endl;

	//const settings::CommDisplayConfig& config = m_Config; //!!!fix: use it

	std::string printerPath = CPrnCtl::printerPath + "/" + m_PrinterData.name;

	//!!!fix display setup
	std::cout << "CPrnCtl::setupPrinter: setting on = " << printerPath << " [fake]" << std::endl;

}

settings::CommPrinterConfig CPrnCtl::getPrinterConfig(CBaseDevice* device, const std::string& printerName)
{

	std::vector<settings::CommPrinterConfig> configList =
			settings::getPrinterByDevice(device->c_name); //, printerName);

	for (auto v: configList)
	{
		if ( v.name == printerName )
		{
			std::cout << "getPrinterConfig: " << printerName << std::endl;

			return v;
		}
	}

	settings::CommPrinterConfig empty = { printerName, "/dev/usb/lp0" }; //!!!fix hardcode
	std::cout << "getPrinterConfig: returning EMPTY! " << printerName << std::endl;
	return empty;
}

shared_ptr<CBaseCommCtl> CPrnCtl::takeCommCtl(CBaseDevice* device, const std::string& printerName)
{

	std::cout << "CPrinterCtl::takeCommCtl try to take " << printerName << std::endl;

	try{

		std::cout << "ATTENTION! CPrinterCtl::takeCommCtl: getting " << printerPath << printerName << std::endl;
		boostio::stream_buffer<boostio::file_sink> bufExport(printerPath+printerName);
		std::ostream fileExport(&bufExport);

		// TODO: grabli, значимая фиксированная позиция в строке
//		fileExport << &gpioName[4]; // 4 - is gpio number position

		// Create TPinData
//		std::string displayPath = CDisplayCtl::displayPath + "/" + displayName + "/";
		std::cout << "ATTENTION! CPrnCtl::takeCommCtl: create fname " << printerPath << printerName << std::endl;
		std::string fname(CPrnCtl::printerPath + printerName);

		TPrinterData printer;
		printer.filename = fname;
		printer.fd = -1;
		printer.name = printerName;

		std::cout << "ATTENTION! CPrinterCtl::takeCommCtl: getPrinterConfig " << printerPath << printerName << std::endl;
		settings::CommPrinterConfig config = getPrinterConfig(device, printerName);

		// create CDisplayCtl for displayNum
		std::cout << "ATTENTION! CPrinterCtl::takeCommCtl: New CPrnCtl " << printerPath << printerName << std::endl;

		shared_ptr<CBaseCommCtl> printerCtl( (CBaseCommCtl*) new CPrnCtl(device, config, printer) );
		std::cout << "ATTENTION! CPrnCtl::takeCommCtl: pr(printerName, printerCtl) " << printerPath << printerName << std::endl;
		std::pair<std::string, shared_ptr<CBaseCommCtl> > pr(printerName, printerCtl);
//		busyPins.insert(pr);
//
		// OK! pin is made as busied and stored

		std::cout << "CPrnCtl::takeCommCtl: take " << printerName << " successfully [fake]" << std::endl;

		return printerCtl;
	}

	catch(boost::exception& ex)
	{
		std::cout << "ERROR! CPrnCtl::takeCommCtl: exception: " << boost::diagnostic_information(ex) << std::endl;
		return nullptr;
	}

	catch(std::exception& ex)
	{
		std::cout << "ERROR! CPrnCtl::takeCommCtl: exception: " << ex.what() << std::endl;
		return nullptr;
	}

	catch(...)
	{
		std::cout << "ERROR! CPrnCtl::takeCommCtl unknown exception: " << std::endl;
		return nullptr;
	}

	return nullptr;
}

void CPrnCtl::freeCommCtl(CBaseDevice* device, const std::string& printerName)
{

	if (device == nullptr) return;

	std::cout << "CPrnCtl::freeCommCtl: getPrinterCtl try to free " << printerName << std::endl;

	std::cout << "CPrnCtl::freeCommCtl: " << printerName << " is free [fake]" << std::endl;

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
		m_PrinterData(printerData)
//		m_timeout(0)
{

}

CPrnCtl::~CPrnCtl(){

}

uint32_t CPrnCtl::send(std::vector<uint8_t> sendData)
{
	std::list<std::vector<uint8_t> > lst;
	std::cout << "CPrnCtl::send: <vector> sendData.size == " << sendData.size() << std::endl;
	lst.push_back(sendData);

	return send(lst);
}

uint32_t CPrnCtl::send(std::list<std::vector<uint8_t> > sendData)
{
	std::cout << "CPrnCtl::send: command 'write data' to '" << m_PrinterData.name << "'." << std::endl;

	/*
	 * Парсинг команды записи в конкретный файл пина
	 * value, direction, edge, active_low
	 */

	std::cout << "CPrnCtl::send: list<vector> sendData.size == " << sendData.size() << std::endl;

	if ( sendData.size() == 1 ) //!!!fix "&& sendData.begin()->size() > 2 )"
	{
//		std::string fname(printerPath + m_PrinterData.name);

		std::vector<uint8_t>& data = sendData.back();

		std::cout << "CPrnCtl::send: data.size is " << data.size() << std::endl;

		uint8_t cmdtype = data[0];
//		uint8_t screenId;

		uint32_t len = 0;
		int32_t fd;
		char* beginData = (char*) &data[1];
		char* endData = (char*) &data[data.size()-1];
		std::string value(beginData, endData);
		std::ofstream myfile ("/aspp/tmpcheck.html");
//		std::string fname("/aspp/tmpcheck.html");

		switch(cmdtype) //!!!fix
		{
		case '0': // clear - not listed
			std::cout << "CPrnCtl::send: cmdtype is CLEAR" << std::endl;


			break;
		case '1': // print
			std::cout << "CPrnCtl::send: cmdtype is PRINT" << std::endl;

//			fd = open( fname.c_str(), O_WRONLY | O_NONBLOCK);
			if (!(myfile.is_open()))
			{
				std::cout << "ERROR! CPrnCtl::send: File '" << "/aspp/tmpcheck.html" << "' isn't opened for writing." << std::endl;
				return 0;
			}

			myfile << value.c_str();
			myfile.close();

			std::system("wkhtmltopdf.sh -s A6 /aspp/tmpcheck.html /aspp/tmpcheck.pdf");
			std::system("lp /aspp/tmpcheck.pdf");


			break;
		default:
			std::cout << "ERROR! CPrnCtl::send: cmdtype " << cmdtype << " incorrect" << std::endl;
			return 0;
		}


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
