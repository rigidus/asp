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

	std::cout << "Created devices:" << std::endl;
	for (auto v: devices)
	{
		std::cout << "- " << v.first << "[" << v.second.devInstance.get() << "]" << std::endl;
	}

}

CDeviceManager::~CDeviceManager() {

}

void CDeviceManager::setCommandToDevice(uint32_t txid, std::string abstractDevice, std::string command,
			std::string parameters, std::string adresat)
{
	if (devices.size())
	{
		/*
		 *  Постановка задачи в очередь на отправку команды на абстрактное устройство.
		 */
		// TODO выделить функцию с поиском по имени устройства
		auto it = devices.find(abstractDevice);
		if ( it != devices.end() )
		{

			std::cout << "CDeviceManager::setCommandToDevice: device " << abstractDevice << " found." << std::endl;

			DeviceCtl::Task task;
			task.txId = txid;
			task.adresat = adresat;
			task.abstract = it->second.devInstance->deviceAbstractName();
			task.concrete = it->second.devInstance->deviceConcreteName();

			task.taskFn = boost::bind(sendCommand, devices[abstractDevice].devInstance.get(), command, parameters);

			it->second.taskQue.push(task);

			/*
			 * Если задача в очереди одна, то отправку на устройство сделать незамедлительно
			 */
			if (it->second.taskQue.size() == 1)
			{

				std::cout << "CDeviceManager::setCommandToDevice: task added to " << abstractDevice << std::endl;

				/*
				 *  Постановка задачи на отправку команды на абстрактное устройство.
				 *  // TODO Сделать выбор по устрйоствам и выделить в отдельную функцию
				 */
				GlobalThreadPool::get().AddTask(0, task.taskFn);
			}

		}
		else
		{

			std::cout << "CDeviceManager::setCommandToDevice: " << abstractDevice << " not found in device list." << std::endl;

		}

	}
	else
	{
		std::cout << "No instanced devices found" << std::endl;
	}

}

void CDeviceManager::setCommandToClient(uint32_t eventFlag, std::string concreteDevice, std::string command, std::string parameters)
{
	/* TODO
	 * Событие надо просто разослать всем клиентам с помощью постановки задач
	 * Ответ на команду надо послать только адресату
	 */

	if ( eventFlag == false)
	{
		// Transaction
		DeviceCtl::Task task;
		if ( popDeviceTask(concreteDevice, task) == false )
		{
			// TODO Послать адресату сообщение, что транзакция была потеряна и выйти

			std::cout << "CDeviceManager::setCommandToClient: Transaction lost" << std::endl;
			return;
		}

		if ( devices.find(task.adresat) != devices.end() )
		{

			if (task.adresat == "logic")
			{
				// TODO Set task to adresat
				std::cout << "Set task for transaction " << task.txId << " to: "
						<< task.adresat << std::endl;

				GlobalThreadPool::get().AddTask(0, boost::bind(sendCommand, devices[task.adresat].devInstance.get(), command, parameters));

				/*
				 * Если в очереди есть еще задачи, то отправить следующую
				 */
				auto it = devices.find(task.abstract);
				if (it->second.taskQue.size())
				{
					/*
					 *  Постановка задачи на отправку команды на абстрактное устройство.
					 */
					DeviceCtl::Task& newTask = it->second.taskQue.front();
					GlobalThreadPool::get().AddTask(0, newTask.taskFn);

					std::string abstractName = it->second.devInstance.get()->deviceAbstractName();
					std::cout << "CDeviceManager::setCommandToDevice: task added to " << abstractName << std::endl;
				}

			}
		}
		else
		{
			std::cout << "Client '" << task.adresat << "'  not found in device list." << std::endl;
		}
	}
	else
	{
		// Event
		// TODO Set task to decision logic to businness logic

		// TODO Set task to logic
		std::cout << "Set event to: ???" << std::endl;

		if ( devices.find("logic") != devices.end() )
			GlobalThreadPool::get().AddTask(0, boost::bind(sendCommand, devices["logic"].devInstance.get(), command, parameters));

		// TODO Set task to web-interface

	}

}
