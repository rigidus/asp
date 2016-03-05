/*
 * comm_config.h
 *
 *  Created on: 8 февр. 2016 г.
 *      Author: alex
 */

#ifndef DEVCOMM_CONFIG_H_
#define DEVCOMM_CONFIG_H_

#include "Settings.h"

namespace devcomm_config {

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

	settings::HttpClientConfig curlBusinnessLogic =
	{
			"bl",
			"http://localhost/businness_logic",
			"80"
	};

	settings::HttpClientConfig curlWebInterface =
	{
			"wi",
			"http://localhost/web_interface",
			"80"
	};

	settings::HttpClientConfig curlTestInterface =
	{
			"ti",
			"http://localhost/test_interface",
			"80"
	};

	settings::CommGPIOConfig* gpioConfigList[] =
	{
			&gpio8,
			&gpio12,
			&gpio18
	};

	settings::CommUARTConfig* uartConfigList[] =
	{
			&uart2
	};

	settings::HttpClientConfig* curlConfigList[] =
	{
			&curlBusinnessLogic,
			&curlWebInterface,
			&curlTestInterface
	};

};


#endif /* DEVCOMM_CONFIG_H_ */
