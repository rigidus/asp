/*
 * device_config.h
 *
 *  Created on: 8 февр. 2016 г.
 *      Author: alex
 */

#include <Settings.h>

#define enabled_device	true
#define disabled_device false

settings::DeviceConfig client_http =
{
		"logic_http",
		"logic_bsns_layer",
		"",
		{  },
		enabled_device
};

settings::DeviceConfig client_http_dev_layer =
{
		"logic_http_dev_layer",
		"logic_dev_layer",
		"",
		{  },
		enabled_device
};

settings::DeviceConfig test_device =
{
		"concrete_device",
		"abstract_device",
		"",
		{  },
		enabled_device
};

settings::DeviceConfig shlagbaum1 =
{
		"shlagbaum_gpio",
		"shlagbaum_in",
		"",
		{ "gpio12", "gpio18", "gpio8", "uart2" },
		enabled_device
};

settings::DeviceConfig shlagbaum2 =
{
		"shlagbaum_gpio",
		"shlagbaum_out",
		"",
		{ "gpio13", "gpio19", "gpio9", "gpio3" },
		disabled_device
};

settings::DeviceConfig printer =
{
		"pechatalka_ru",
		"printer",
		"printer_proto",
		{"uart4"},
		disabled_device
};

settings::DeviceConfig presentSensor =
{
		"i_am_here",
		"present sensor",
		"",
		{ "gpio5", "gpio6" },
		disabled_device
};

settings::DeviceConfig passSensor =
{
		"get_out_from",
		"pass sensor",
		"",
		{ "gpio7", "gpio1" },
		disabled_device
};

settings::DeviceConfig display =
{
		"winstar16x2",
		"textdisplay16x2",
		"winstar",
		{ "gpio20", "gpio21", "gpio22", "gpio23", "gpio24", "gpio25", "gpio26" },
		disabled_device
};

settings::DeviceConfig kkm =
{
		"poluchalka",
		"kkm",
		"proto_kkm",
		{ "uart1" },
		disabled_device
};

settings::DeviceConfig* devices[] =
{
		&client_http,
		&client_http_dev_layer,
		&test_device,
		&shlagbaum1,
//		&shlagbaum2,
//		&printer,
//		&presentSensor,
//		&passSensor,
//		&display,
//		&kkm
};

settings::CommGPIOConfig gpio12 =
{
		"gpio12",
		true,	// out
		false,	// value = 0
		3000	// default pulse value, ms
};

settings::CommGPIOConfig gpio18 =
{
		"gpio18",
		true,	// out
		false,	// value = 0
		3000	// default pulse value, ms
};

settings::CommGPIOConfig gpio8 =
{
		"gpio8",
		false,	// in, signal from car present
		true,	// value = 1
		300		// check delay, ms
};

settings::CommUARTConfig uart2 =
{
		"uart2",
		115200,
		8, 1, true
};

settings::CommGPIOConfig* gpioConfigs[] =
{
		&gpio8,
		&gpio12,
		&gpio18
};

settings::CommUARTConfig* uartConfigs[] =
{
		&uart2
};

std::vector<settings::CommGPIOConfig*> settings::gpioConfigList(gpioConfigs, &gpioConfigs[sizeof(gpioConfigs)/sizeof(gpioConfigs[0])]);
std::vector<settings::CommUARTConfig*> settings::uartConfigList(uartConfigs, &uartConfigs[sizeof(uartConfigs)/sizeof(uartConfigs[0])]);
std::vector<settings::DeviceConfig*> settings::deviceList(devices, &devices[sizeof(devices)/sizeof(devices[0])]);

