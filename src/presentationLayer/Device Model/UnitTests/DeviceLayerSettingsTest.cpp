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

settings::DeviceConfig* devices[] =
{
		&client_http,
		&client_http_dev_layer,
		&test_device,
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

