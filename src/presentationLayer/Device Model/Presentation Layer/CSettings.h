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

	struct PrinterPilotProtoConfig
	{

	};

	struct WinStarProtoConfig
	{

	};

	struct MassStorageProtoConfig
	{

	};

	struct KKMProtoConfig
	{

	};

	struct CommGPIOConfig
	{
		bool direction; // 0 - in, 1 - out
		bool def_value;		// 0 - off 1 - on
		uint32_t pulseLen;
	};

	struct CommUARTConfig
	{
		uint32_t speed;
		uint32_t bits;
		uint32_t stop;
		bool even;
	};

public:
	CSettings();
	~CSettings();


	const std::vector<CSettings::DeviceConfig> getDeviceConfig();
	const PrinterPilotProtoConfig getPrinterPilotProtoByName(std::string deviceName, std::string protoName);
	const WinStarProtoConfig getWinstarProtoByName(std::string deviceName, std::string protoName);
	const MassStorageProtoConfig getMassStorageProtoByName(std::string protoName);
	const KKMProtoConfig getKKMProtoByName(std::string deviceName, std::string protoName);
	const CommGPIOConfig getGPIOByDevice(std::string deviceName, uint32_t commIndex);
	const CommUARTConfig getUARTByDevice(std::string deviceName, uint32_t commIndex);

};

} /* namespace database */

#endif /* CSETTINGS_H_ */
