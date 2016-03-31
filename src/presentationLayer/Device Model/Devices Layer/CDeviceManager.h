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
#include "SetCommandTo.h"

class CDeviceManager {

private:
	CDeviceManager(std::vector<settings::DeviceConfig> devConfig);
	~CDeviceManager();

	static CDeviceManager* ptr;
	static boost::mutex mut;
	static boost::mutex queMutex;

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

	uint32_t m_EventCounter;

	bool popDeviceTask(std::string concreteDevice, DeviceCtl::Task& task);

public:

	static CDeviceManager* deviceManagerFactory( std::vector<settings::DeviceConfig> devConfig );

	static CDeviceManager* deviceManager();

	static void destroyDeviceManager();

	void setCommandToClient(SetTo::CommandType eventFlag, std::string device, std::string command, std::string parameters);

	static void sendCommand(CAbstractDevice* abstractDevice, std::string command, std::string pars);

	// Class functions
	void setCommandToDevice(uint32_t txid, std::string device, std::string command,
			std::string parameters, std::string adresat);

	void ackClient(std::string concreteDevice);

};

#endif /* CDEVICEMANAGER_H_ */
