/*
 * presentationMain.cpp
 *
 *  Created on: 8 февр. 2016 г.
 *      Author: alex
 */

#include "CSettings.h"

#include <iostream>

int main()
{

	database::CSettings sets;

	std::vector<database::CSettings::DeviceConfig> devList = sets.getDeviceConfig();

	for (auto v: devList)
	{
		if (v.abstractName.size() == 0) continue;
		if (v.enable == false) continue;

		std::cout << v.abstractName << std::endl;

		if (v.proto.size() != 0) std::cout << " " << v.proto << std::endl;

		for (auto comm: v.comm)
			std::cout << "  " << comm << std::endl;

	}

	return 0;
}

