/*
 * CSettings.cpp
 *
 *  Created on: 8 февр. 2016 г.
 *      Author: alex
 */

#include "CSettings.h"

#include <iostream>

namespace database {

#include "device_config.h"
#include "proto_config.h"
#include "devcomm_config.h"

CSettings::CSettings() {
	// TODO Auto-generated constructor stub

}

CSettings::~CSettings() {
	// TODO Auto-generated destructor stub
}

const std::vector<CSettings::DeviceConfig> CSettings::getDeviceConfig()
{
	std::vector<CSettings::DeviceConfig> baseList;

	for (CSettings::DeviceConfig* v: device_config::deviceList)
	{
		baseList.push_back( *v );
	}

	return baseList;
}

const CSettings::PrinterPilotProtoConfig CSettings::getPrinterPilotProtoByName(std::string deviceName, std::string protoName)
{
	// TODO: make config and return it
	CSettings::PrinterPilotProtoConfig devnull;

	return devnull;
}

const CSettings::WinStarProtoConfig CSettings::getWinstarProtoByName(std::string deviceName, std::string protoName)
{
	// TODO: make config and return it
	CSettings::WinStarProtoConfig devnull;

	return devnull;
}

const CSettings::MassStorageProtoConfig CSettings::getMassStorageProtoByName(std::string protoName)
{
	// TODO: make config and return it
	CSettings::MassStorageProtoConfig devnull;

	return devnull;
}

const CSettings::KKMProtoConfig CSettings::getKKMProtoByName(std::string deviceName, std::string protoName)
{
	// TODO: make config and return it
	CSettings::KKMProtoConfig devnull;

	return devnull;
}

const CSettings::CommGPIOConfig CSettings::getGPIOByDevice(std::string deviceName, uint32_t commIndex)
{
	// TODO: make config and return it
	CSettings::CommGPIOConfig devnull;

	return devnull;
}

const CSettings::CommUARTConfig CSettings::getUARTByDevice(std::string deviceName, uint32_t commIndex)
{
	// TODO: make config and return it
	CSettings::CommUARTConfig devnull;

	return devnull;
}

} /* namespace database */
