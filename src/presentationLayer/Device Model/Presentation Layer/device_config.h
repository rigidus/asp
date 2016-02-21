/*
 * device_config.h
 *
 *  Created on: 8 февр. 2016 г.
 *      Author: alex
 */

#ifndef DEVICE_CONFIG_H_
#define DEVICE_CONFIG_H_

#include <Settings.h>

namespace device_config {

	settings::DeviceConfig shlagbaum1 =
	{
			"shlagbaum palka",
			"shlagbaum_in",
			"",
			{ "gpio12", "gpio18", "gpio8", "uart2" },
			true
	};

	settings::DeviceConfig shlagbaum2 =
	{
			"shlagbaum palka",
			"shlagbaum_out",
			"",
			{ "gpio13", "gpio19", "gpio9", "gpio3" },
			true
	};

	settings::DeviceConfig printer =
	{
			"pechatalka_ru",
			"printer",
			"printer_proto",
			{"uart4"},
			true
	};

	settings::DeviceConfig presentSensor =
	{
			"i_am_here",
			"present sensor",
			"",
			{ "gpio5", "gpio6" },
			true
	};

	settings::DeviceConfig passSensor =
	{
			"get_out_from",
			"pass sensor",
			"",
			{ "gpio7", "gpio1" },
			true
	};

	settings::DeviceConfig display =
	{
			"winstar16x2",
			"textdisplay16x2",
			"winstar",
			{ "gpio20", "gpio21", "gpio22", "gpio23", "gpio24", "gpio25", "gpio26" },
			true
	};

	settings::DeviceConfig cardReader =
	{
			"sd-card",
			"card reader",
			"mass_storage",
			{ "sda", "sdb", "sdc" },
			true
	};

	settings::DeviceConfig kkm =
	{
			"poluchalka",
			"kkm",
			"proto_kkm",
			{ "uart1" },
			true
	};

	settings::DeviceConfig* deviceList[] =
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
