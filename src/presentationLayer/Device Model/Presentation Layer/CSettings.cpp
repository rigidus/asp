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


const std::vector<std::string> CSettings::getGPIONamesByDevice(const std::string& deviceName)
{
	std::vector<std::string> names;

	for (DeviceConfig* v: device_config::deviceList)
	{
		if (v->concreteName == deviceName)
		{
			for (std::string commName: v->comm)
			{
				names.push_back(commName);
			}
			break;
		}
	}

	return names;
}


const std::vector<CSettings::CommGPIOConfig> CSettings::getGPIOByDevice(const std::string deviceName, const std::string gpioName)
{
	// TODO: make config and return it
	std::vector<CSettings::CommGPIOConfig> devs;

	for (CSettings::DeviceConfig* v: device_config::deviceList)
	{
		if (v->abstractName == deviceName)
		{

			// find device
			// iteration for comms
			for (std::string commName: v->comm)
			{
				// find config by comm name
				uint32_t size = sizeof(devcomm_config::gpioConfigList) / sizeof(devcomm_config::gpioConfigList[0]);
				for (uint32_t i = 0; i < size; ++i)
				{
					if (commName == devcomm_config::gpioConfigList[i]->name)
					{
						devs.push_back(*devcomm_config::gpioConfigList[i]);
						break;
					}
				}
			}
		}
	}

	return devs;
}

const std::vector<CSettings::CommUARTConfig> CSettings::getUARTByDevice(const std::string deviceName)
{
	// TODO: make config and return it
	std::vector<CSettings::CommUARTConfig> devs;

	return devs;
}

} /* namespace database */
