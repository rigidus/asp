/*
 * settings.cpp
 *
 *  Created on: 8 февр. 2016 г.
 *      Author: alex
 */

#include "Settings.h"

#include <iostream>

namespace settings {

const std::vector<DeviceConfig> getDeviceConfig()
{
	// TODO: reading device config from DB
	std::vector<settings::DeviceConfig> baseList;

	for (settings::DeviceConfig* v: settings::deviceList)
	{
		baseList.push_back( *v );
	}

	return baseList;
}


const std::vector<std::string> getCommNamesByDevice(const std::string& deviceName)
{
	std::vector<std::string> names;

	for (DeviceConfig* v: settings::deviceList)
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


const std::vector<CommGPIOConfig> getGPIOByDevice(const std::string deviceName, const std::string gpioName)
{
	// make config and return it
	std::vector<settings::CommGPIOConfig> devs;

	for (settings::DeviceConfig* v: settings::deviceList)
	{
		if (v->concreteName == deviceName)
		{

			// find device
			// iteration for comms
			for (std::string commName: v->comm)
			{
				// find config by comm name
				uint32_t size = settings::gpioConfigList.size();
				for (uint32_t i = 0; i < size; ++i)
				{
					if (commName == settings::gpioConfigList[i]->name)
					{
						devs.push_back(*settings::gpioConfigList[i]);
						break;
					}
				}
			}
		}
	}

	return devs;
}

const std::vector<CommUARTConfig> getUARTByDevice(const std::string deviceName)
{
	// TODO: make config and return it
	std::vector<settings::CommUARTConfig> devs;

	return devs;
}

} /* namespace database */
