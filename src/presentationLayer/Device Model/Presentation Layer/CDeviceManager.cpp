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

void CDeviceManager::setCommandToDevice(uint32_t txid, std::string device, std::string command,
			std::string parameters, std::string adresat)
{
	if (devices.size())
	{
		/*
		 *  Постановка задачи в очередь на отправку команды на абстрактное устройство.
		 */
		// TODO выделить функцию с поиском по имени устройства
		auto it = devices.find("shlagbaum_in");
		if ( it != devices.end() )
		{
			DeviceCtl::Task task;
			task.txId = txid;
			task.adresat = adresat;
			task.abstract = it->second.devInstance->deviceAbstractName();
			task.concrete = it->second.devInstance->deviceConcreteName();

			task.taskFn = boost::bind(sendCommand<AbstractShlagbaum>, devices[device].devInstance.get(), command, parameters);

			it->second.taskQue.push(task);

			/*
			 * Если задача в очереди одна, то отправку на устройство сделать незамедлительно
			 */
			if (it->second.taskQue.size() == 1)
			{
				/*
				 *  Постановка задачи на отправку команды на абстрактное устройство.
				 *  // TODO Выделить в отдельную функцию с мьютексом
				 */
				if ( devices.find("shlagbaum_in") != devices.end() )
					GlobalThreadPool::get().AddTask(0, boost::bind(sendCommand<AbstractShlagbaum>, devices[device].devInstance.get(), command, parameters));
			}

		}

	}
	else
	{
		std::cout << "No instanced devices found" << std::endl;
	}

}

void CDeviceManager::setCommandToClient(uint32_t eventFlag, std::string device, std::string command, std::string parameters)
{
	/* TODO
	 * Событие надо просто разослать всем клиентам с помощью постановки задач
	 * Ответ на команду надо послать только адресату
	 */

	if ( eventFlag == false)
	{
		// Transaction
		DeviceCtl::Task task;
		if ( popDeviceTask(device, task) == false )
		{
			// TODO Послать адресату сообщение, что транзакция была потеряна и выйти
			std::cout << "Transaction lost" << std::endl;
			return;
		}

		// TODO Set task to adresat
		std::cout << "Set task for transaction " << task.txId << " to: "
				<< task.adresat << std::endl;

		if ( devices.find("logic") != devices.end() )
			GlobalThreadPool::get().AddTask(0, boost::bind(sendCommand<BsnsLogic>, devices[device].devInstance.get(), command, parameters));

	}
	else
	{
		// Event
		// TODO Set task to decision logic to businness logic

		// TODO Set task to logic
		std::cout << "Set event to: ???" << std::endl;

		if ( devices.find("logic") != devices.end() )
			GlobalThreadPool::get().AddTask(0, boost::bind(sendCommand<BsnsLogic>, devices[device].devInstance.get(), command, parameters));

		// TODO Set task to web-interface

	}

}
