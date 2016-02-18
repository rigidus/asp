/*
 * CSettings.h
 *
 *  Created on: 8 февр. 2016 г.
 *      Author: alex
 */

#ifndef CSETTINGS_H_
#define CSETTINGS_H_

#include <string>
#include <vector>

#include <boost/cstdint.hpp>
#include <boost/variant.hpp>

using namespace boost;

namespace database {

class CSettings {

public:

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

	struct CommCURLConfig
	{
		std::string name;
		std::string host;
		std::string port;
	};

public:
	CSettings();
	~CSettings();


	const std::vector<DeviceConfig> getDeviceConfig();
	const std::vector<std::string> getGPIONamesByDevice(const std::string& deviceName);
	const std::vector<CommGPIOConfig> getGPIOByDevice(const std::string deviceName, const std::string gpioName);
	const std::vector<CommUARTConfig> getUARTByDevice(const std::string deviceName);

};

} /* namespace database */

#endif /* CSETTINGS_H_ */
