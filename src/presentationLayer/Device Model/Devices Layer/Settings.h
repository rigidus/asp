/*
 * CSettings.h
 *
 *  Created on: 8 февр. 2016 г.
 *      Author: alex
 */

#ifndef SETTINGS_H_
#define SETTINGS_H_

#include <string>
#include <vector>

#include <boost/cstdint.hpp>

namespace settings {

using namespace boost;

	struct AllDeviceConf
	{
		std::string conName;
		std::string abstractName;
		std::string proto;
		std::vector<std::string> comm;
		bool enable;
	};

	struct DeviceConfig
	{
		std::string concreteName;
		std::string abstractName;
		std::string proto;
		std::vector<std::string> comm;
		bool enable;
	};

	struct CommGPIOConfig
	{
		std::string name;
		bool direction; // 0 - in, 1 - out
		bool def_value;		// 0 - off 1 - on
		uint32_t pulseLen;
	};

	struct CommUARTConfig
	{
		std::string name;
		uint32_t speed;
		uint32_t bits;
		uint32_t stop;
		bool even;
	};

	struct HttpClientConfig
	{
		std::string name;
		std::string host;
		std::string port;
	};

	const std::vector<DeviceConfig> getDeviceConfig();

	const std::vector<std::string> getCommNamesByDevice(const std::string& deviceName);
	const std::vector<CommGPIOConfig> getGPIOByDevice(const std::string deviceName, const std::string gpioName);
	const std::vector<CommUARTConfig> getUARTByDevice(const std::string deviceName);

	extern std::vector<DeviceConfig*> deviceList;
	extern std::vector<CommGPIOConfig*> gpioConfigList;
	extern std::vector<CommUARTConfig*> uartConfigList;

} /* namespace database */

#endif /* SETTINGS_H_ */
