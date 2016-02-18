/*
 * comm_config.h
 *
 *  Created on: 8 февр. 2016 г.
 *      Author: alex
 */

#ifndef DEVCOMM_CONFIG_H_
#define DEVCOMM_CONFIG_H_

using namespace database;

namespace devcomm_config {

	CSettings::CommGPIOConfig gpio12 =
	{
			"gpio12",
			true,	// out
			false,	// value = 0
			3000	// default pulse value, ms
	};

	CSettings::CommGPIOConfig gpio18 =
	{
			"gpio18",
			true,	// out
			false,	// value = 0
			3000	// default pulse value, ms
	};

	CSettings::CommGPIOConfig gpio8 =
	{
			"gpio8",
			false,	// in, signal from car present
			true,	// value = 1
			300		// check delay, ms
	};

	CSettings::CommUARTConfig uart2 =
	{
			"uart2",
			115200,
			8, 1, true
	};

	CSettings::CommCURLConfig curlBusinnessLogic =
	{
			"bl",
			"http://localhost/businness_logic",
			"80"
	};

	CSettings::CommCURLConfig curlWebInterface =
	{
			"wi",
			"http://localhost/web_interface",
			"80"
	};

	CSettings::CommCURLConfig curlTestInterface =
	{
			"ti",
			"http://localhost/test_interface",
			"80"
	};

	CSettings::CommGPIOConfig* gpioConfigList[] =
	{
			&gpio8,
			&gpio12,
			&gpio18
	};

	CSettings::CommUARTConfig* uartConfigList[] =
	{
			&uart2
	};

	CSettings::CommCURLConfig* curlConfigList[] =
	{
			&curlBusinnessLogic,
			&curlWebInterface,
			&curlTestInterface
	};

};


#endif /* DEVCOMM_CONFIG_H_ */
