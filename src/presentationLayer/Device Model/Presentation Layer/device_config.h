/*
 * device_config.h
 *
 *  Created on: 8 февр. 2016 г.
 *      Author: alex
 */

#ifndef DEVICE_CONFIG_H_
#define DEVICE_CONFIG_H_

#include "CSettings.h"

using namespace database;

namespace device_config {

	CSettings::DeviceConfig shlagbaum1 =
	{
			"shlagbaum palka",
			"shlagbaum_in",
			"",
			{ "gpio12", "gpio18", "gpio8", "uart2" },
			true
	};

	CSettings::DeviceConfig shlagbaum2 =
	{
			"shlagbaum palka",
			"shlagbaum_out",
			"",
			{ "gpio13", "gpio19", "gpio9", "gpio3" },
			true
	};

	CSettings::DeviceConfig printer =
	{
			"pechatalka_ru",
			"printer",
			"printer_proto",
			{"uart4"},
			true
	};

	CSettings::DeviceConfig presentSensor =
	{
			"i_am_here",
			"present sensor",
			"",
			{ "gpio5", "gpio6" },
			true
	};

	CSettings::DeviceConfig passSensor =
	{
			"get_out_from",
			"pass sensor",
			"",
			{ "gpio7", "gpio1" },
			true
	};

	CSettings::DeviceConfig display =
	{
			"winstar16x2",
			"textdisplay16x2",
			"winstar",
			{ "gpio20", "gpio21", "gpio22", "gpio23", "gpio24", "gpio25", "gpio26" },
			true
	};

	CSettings::DeviceConfig cardReader =
	{
			"sd-card",
			"card reader",
			"mass_storage",
			{ "sda", "sdb", "sdc" },
			true
	};

	CSettings::DeviceConfig kkm =
	{
			"poluchalka",
			"kkm",
			"proto_kkm",
			{ "uart1" },
			true
	};

	CSettings::DeviceConfig* deviceList[] =
	{
			&shlagbaum1,
//			&shlagbaum2,
//			&printer,
//			&presentSensor,
//			&passSensor,
//			&display,
//			&cardReader,
//			&kkm
	};

};


#endif /* DEVICE_CONFIG_H_ */
