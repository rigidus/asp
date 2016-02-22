/*
 * CDeviceManager.h
 *
 *  Created on: 8 февр. 2016 г.
 *      Author: alex
 */

#ifndef CDEVICEMANAGER_H_
#define CDEVICEMANAGER_H_

#include <boost/thread/thread.hpp>
#include <boost/thread/mutex.hpp>
#include <boost/smart_ptr.hpp>
#include "Settings.h"
#include "GlobalThreadPool.h"
#include "CDeviceFactory.h"

class CDeviceManager {

private:
	CDeviceManager(std::vector<settings::DeviceConfig> devConfig);
	~CDeviceManager();

	static CDeviceManager* ptr;
	static boost::mutex mut;

	struct DeviceCtl
	{
		boost::shared_ptr<CAbstractDevice> devInstance;

		struct Task
		{
			uint32_t txId;
			mythreadpool::TTaskFunc taskFn;
		};

		std::queue<Task> taskQue;
	};

	std::map< std::string, DeviceCtl > devices;

public:

	enum DeviceManagerType { WorkingSet = 1, TestingSet};

	static CDeviceManager* deviceManagerFactory( DeviceManagerType type)
	{
		boost::mutex::scoped_lock(mut);

		if (ptr == nullptr)
		{
			if (type == WorkingSet)
			{
				ptr = new CDeviceManager(settings::fromDBDevices());
			}

			if (type == TestingSet)
			{
				ptr = new CDeviceManager(settings::fromTestDevices());
			}
		}
		return ptr;
	}

	static CDeviceManager* deviceManager()
	{
		if (ptr == nullptr)
			std::cout << "ERROR: deviceManager is NULL" << std::endl;

		return ptr;
	}

	static void destroyDeviceManager()
	{
		delete ptr;
		ptr = nullptr;
	}

	template<typename T>
	static void sendCommand(CAbstractDevice* iDev, std::string command, std::string pars)
	{
		iDev->sendCommand(command, pars);
	}

	// Class functions
	void setCommandToDevice(uint32_t txid, std::string device, std::string command, std::string parameters);

};

#endif /* CDEVICEMANAGER_H_ */
