/*
 * CDeviceManager.cpp
 *
 *  Created on: 8 февр. 2016 г.
 *      Author: alex
 */

#include "CDeviceManager.h"

CDeviceManager* CDeviceManager::ptr = nullptr;
boost::mutex CDeviceManager::mut;

CDeviceManager::CDeviceManager(std::vector<settings::DeviceConfig> devConfig) {

	CDeviceFactory&	factory = CDeviceFactory::getFactory();

	for (auto v: devConfig)
	{
		if (v.abstractName.size() == 0) continue;
		if (v.enable == false) continue;

		std::cout << v.abstractName << std::endl;

		if (v.proto.size() != 0) std::cout << " " << v.proto << std::endl;

		DeviceCtl devCtl;
		boost::shared_ptr<CAbstractDevice> sPtr( factory.deviceFactory(v.abstractName, v.concreteName) );
		devCtl.devInstance = sPtr;

		if (sPtr.get() != nullptr)
			devices.emplace( std::make_pair(v.abstractName, devCtl) );

	}

}

CDeviceManager::~CDeviceManager() {

}

void CDeviceManager::setCommandToDevice(uint32_t txid, std::string device, std::string command, std::string parameters)
{
	if (devices.size())
	{
		/*
		 *  Постановка задачи на отправку команды на абстрактное устройство.
		 */
		if ( devices.find("shlagbaum_in") != devices.end() )
			GlobalThreadPool::get().AddTask(0, boost::bind(sendCommand<AbstractShlagbaum>, devices[device].devInstance.get(), command, parameters));
	}
	else
	{
		std::cout << "No instanced devices found" << std::endl;
	}

}
