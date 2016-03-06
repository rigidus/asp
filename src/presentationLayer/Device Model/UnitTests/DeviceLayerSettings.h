/*
 * DeviceLayerSettings.h
 *
 *  Created on: 6 марта 2016 г.
 *      Author: drema
 */

#ifndef DEVICELAYERSETTINGS_H_
#define DEVICELAYERSETTINGS_H_


#include <Settings.h>

#define enabled_device	true
#define disabled_device false

namespace test_device_config {

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

	settings::DeviceConfig* deviceList[] =
	{
			&client_http,
			&client_http_dev_layer,
			&test_device
	};

};


#endif /* DEVICELAYERSETTINGS_H_ */
