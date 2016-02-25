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
			std::string adresat;
			std::string abstract;
			std::string concrete;
			mythreadpool::TTaskFunc taskFn;
		};

		std::queue<Task> taskQue;
	};

	std::map< std::string, DeviceCtl > devices;

	bool popDeviceTask(std::string device, DeviceCtl::Task& task)
	{
		for (auto& v: devices)
		{
			if (v.second.devInstance->deviceConcreteName() == device)
			{
				if (v.second.taskQue.size() == 0)
					return false;

				task = v.second.taskQue.front();
				v.second.taskQue.pop();

				return true;
			}
		}

		return false;
	}

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

	void setCommandToClient(uint32_t eventFlag, std::string device, std::string command, std::string parameters);

	template<typename T>
	static void sendCommand(CAbstractDevice* iDev, std::string command, std::string pars)
	{
		iDev->sendCommand(command, pars);
	}

	// Class functions
	void setCommandToDevice(uint32_t txid, std::string device, std::string command,
			std::string parameters, std::string adresat);

};

#endif /* CDEVICEMANAGER_H_ */
