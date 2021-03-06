//============================================================================
// Name        : Settings.cpp
// Author      : aav
// Created on  : 8 февр. 2016 г.
// Version     : v.0.1
// Copyright   : Non nobis, Domine, non nobis, sed nomini tuo da gloriam.
// Description : Setting interface instances for device and communications
//============================================================================

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

const std::vector<CommDisplayConfig> getDisplayByDevice(const std::string deviceName)
{
	// make config and return it
	std::vector<settings::CommDisplayConfig> devs;

	std::cout << "getDisplayByDevice: " << deviceName << std::endl;
	for (settings::DeviceConfig* v: settings::deviceList)
	{
		if (v->concreteName == deviceName)
		{

			// find device
			// iteration for comms
			for (std::string commName: v->comm)
			{
				// find config by comm name
				uint32_t size = settings::displayConfigList.size();
				for (uint32_t i = 0; i < size; ++i)
				{
					if (commName == settings::displayConfigList[i]->name)
					{
						std::cout << "getDisplayByDevice: found " << commName << std::endl;
						devs.push_back(*settings::displayConfigList[i]);
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

const std::vector<CommPrinterConfig> getPrinterByDevice(const std::string deviceName)
{
	// make config and return it
	std::vector<settings::CommPrinterConfig> devs;

	std::cout << "getPrinterByDevice: " << deviceName << std::endl;
	for (settings::DeviceConfig* v: settings::deviceList)
	{
		if (v->concreteName == deviceName)
		{

			// find device
			// iteration for comms
			for (std::string commName: v->comm)
			{
				// find config by comm name
				uint32_t size = settings::printerConfigList.size();
				for (uint32_t i = 0; i < size; ++i)
				{
					if (commName == settings::printerConfigList[i]->name)
					{
						std::cout << "getPrinterByDevice: found " << commName << std::endl;
						devs.push_back(*settings::printerConfigList[i]);
						break;
					}
				}
			}
		}
	}

	return devs;
}

const std::vector<CommCharDevConfig> getCharDevsByDevice(const std::string deviceName)
{
	// make config and return it
	std::vector<settings::CommCharDevConfig> devs;

	std::cout << "getCharDevsByDevice: " << deviceName << std::endl;
	for (settings::DeviceConfig* v: settings::deviceList)
	{
		if (v->concreteName == deviceName)
		{

			// find device
			// iteration for comms
			for (std::string commName: v->comm)
			{
				// find config by comm name
				uint32_t size = settings::chardevsConfigList.size();
				for (uint32_t i = 0; i < size; ++i)
				{
					if (commName == settings::chardevsConfigList[i]->name)
					{
						std::cout << "getCharDevsByDevice: found " << commName << std::endl;
						devs.push_back(*settings::chardevsConfigList[i]);
						break;
					}
				}
			}
		}
	}

	return devs;
}
} /* namespace database */
