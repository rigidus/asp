/*
 * CDeviceManager.cpp
 *
 *  Created on: 8 февр. 2016 г.
 *      Author: alex
 */

#include "CDeviceManager.h"
#include "CPinCtl.h"

CDeviceManager* CDeviceManager::ptr = nullptr;
boost::mutex CDeviceManager::mut;
boost::mutex CDeviceManager::queMutex;

CDeviceManager::CDeviceManager(std::vector<settings::DeviceConfig> devConfig) {

	m_EventCounter = 0;

	CDeviceFactory&	factory = CDeviceFactory::getFactory();

	for (auto v: devConfig)
	{
		if (v.abstractName.size() == 0) continue;
		if (v.enable == false) continue;

		{
			std::stringstream log;
			log << "CDeviceManager constructor: for " << v.abstractName << " started";
			SetTo::CommonLog(debug, log.str());
		}

		if (v.proto.size() != 0)
		{
			std::stringstream log;
			log << "- " << v.proto;
			SetTo::CommonLog(debug, log.str());
		}

		DeviceCtl devCtl;
		boost::shared_ptr<CAbstractDevice> sPtr( factory.deviceFactory(v.abstractName, v.concreteName) );
		devCtl.devInstance = sPtr;

		if (sPtr.get() != nullptr)
		{
			std::pair< std::string, DeviceCtl > pr(v.abstractName, devCtl);
			devices.insert(pr);
		}
	}

	// INTEGRATE COMMDEVICE SECTION: добавь сюда все startNotifier, которые есть в проекте
	{
		CPinCtl::startNotifier();
	}

	{
		std::stringstream log;
		log << "CDeviceManager constructor: Created device list:";
		SetTo::CommonLog(debug, log.str());
	}

	for (auto v: devices)
	{
		std::stringstream log;
		log << "- " << v.first << "[" << v.second.devInstance.get() << "]";
		SetTo::CommonLog(debug, log.str());
	}

}

CDeviceManager::~CDeviceManager() {

}

CDeviceManager* CDeviceManager::deviceManagerFactory( std::vector<settings::DeviceConfig> devConfig)
{
	boost::mutex::scoped_lock(mut);

	if (ptr == nullptr)
	{
		ptr = new CDeviceManager(devConfig);
	}
	return ptr;
}

CDeviceManager* CDeviceManager::deviceManager()
{
	if (ptr == nullptr)
	{
		std::stringstream log;
		log << "ERROR! CDeviceManager::deviceManager: is NULL";
		SetTo::CommonLog(error, log.str());
	}

	return ptr;
}

void CDeviceManager::destroyDeviceManager()
{
	// INTEGRATE COMMDEVICE SECTION: добавь сюда все stopNotifier, которые есть в проекте
	{
		CPinCtl::stopNotifier();
	}

	delete ptr;
	ptr = nullptr;
}

void CDeviceManager::sendCommand(CAbstractDevice* abstractDevice, std::string command, std::string pars)
{
	if (abstractDevice == nullptr)
	{
		{
			std::stringstream log;
			log << "ERROR! CDeviceManager::sendCommand: abstractDevice is null";
			SetTo::CommonLog(error, log.str());
		}
		return;
	}

	{
		std::stringstream log;
		log << "CDeviceManager::sendCommand: Start Task for '" << abstractDevice->device()->c_name << "'";
		SetTo::CommonLog(trace, log.str());
	}

	abstractDevice->sendCommand(command, pars);
}

void CDeviceManager::setCommandToDevice(uint32_t txid, std::string abstractDevice, std::string command,
			std::string parameters, std::string adresat)
{
	boost::mutex::scoped_lock lock(queMutex);

	if (devices.size())
	{
		/*
		 *  Постановка задачи в очередь на отправку команды на абстрактное устройство.
		 */
		// TODO выделить функцию с поиском по имени устройства
		auto it = devices.find(abstractDevice);
		if ( it != devices.end() )
		{

			{
				std::stringstream log;
				log << "CDeviceManager::setCommandToDevice: device " << abstractDevice << " found.";
				SetTo::CommonLog(trace, log.str());
			}

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
			{
				std::stringstream log;
				log << "CDeviceManager::setCommandToDevice: " << it->first << " queue has "
						<< it->second.taskQue.size() << " task(s).";
				SetTo::CommonLog(trace, log.str());
			}

			if (it->second.taskQue.size() == 1)
			{

				{
					std::stringstream log;
					log << "CDeviceManager::setCommandToDevice: task added to " << abstractDevice;
					SetTo::CommonLog(debug, log.str());
				}

				/*
				 *  Постановка задачи на отправку команды на абстрактное устройство.
				 */
				GlobalThreadPool::get().AddTask(0, task.taskFn);
			}

		}
		else
		{
			{
				std::stringstream log;
				log << "CDeviceManager::setCommandToDevice: " << abstractDevice << " not found in device list.";
				SetTo::CommonLog(debug, log.str());
			}
		}
	}
	else
	{
		SetTo::CommonLog(error, "No instanced devices found" );
	}

}

void CDeviceManager::setCommandToClient(SetTo::CommandType eventFlag, std::string concreteDevice, std::string command, std::string parameters)
{
	/*
	 * Событие надо просто разослать всем клиентам с помощью постановки задач
	 * Ответ на команду надо послать только адресату
	 */

	boost::mutex::scoped_lock lock(queMutex);

	if ( eventFlag ==  SetTo::CommandType::Transaction )
	{
		// Transaction
		DeviceCtl::Task task;
		if ( popDeviceTask(concreteDevice, task) == false )
		{
			// Послать адресату сообщение, что транзакция была потеряна и выйти

			std::stringstream error;
			error << "ERROR! CDeviceManager::setCommandToClient: Transaction lost.";
			SetTo::sendErrorToClient(error);

			SetTo::CommonLog(severity_level::error, error.str());
			return;
		}

		auto itAdresat = devices.find(task.adresat);
		if ( itAdresat != devices.end() )
		{

			if (task.adresat == "logic_bsns_layer")
			{
				// Set task to adresat
				{
					std::stringstream log;
					log << "CDeviceManager::setCommandToClient: Set task for transaction " << task.txId << " to: "
							<< task.adresat;
					SetTo::CommonLog(trace, log.str());
				}

				DeviceCtl::Task clientTask;
				clientTask.txId = task.txId;
				clientTask.adresat = "";
				clientTask.abstract = itAdresat->second.devInstance->deviceAbstractName();
				clientTask.concrete = itAdresat->second.devInstance->deviceConcreteName();

				std::stringstream strparams;
				strparams << "{\"txid\":" << task.txId << ", \"device\":\""
						<< task.abstract << "\", " << parameters.c_str() << "}";

				clientTask.taskFn = boost::bind(sendCommand, devices[task.adresat].devInstance.get(), command, strparams.str());

				itAdresat->second.taskQue.push(clientTask);

				/*
				 * Если задача в очереди клиента одна, то отправку на устройство сделать незамедлительно
				 */
				{
					std::stringstream log;
					log << "CDeviceManager::setCommandToCient: " << itAdresat->first << " queue has "
						<<  itAdresat->second.taskQue.size() << " task(s).";
					SetTo::CommonLog(trace, log.str());
				}

				if (itAdresat->second.taskQue.size() == 1)
				{

					{
						std::stringstream log;
						log << "CDeviceManager::setCommandToClient: task added to " << itAdresat->first;
						SetTo::CommonLog(debug, log.str());
					}

					/*
					 *  Постановка задачи на отправку команды на абстрактное устройство.
					 */

					GlobalThreadPool::get().AddTask(0, clientTask.taskFn);

				}

				/*
				 * Если в очереди есть еще задачи для устройства, то отправить следующую
				 */
				auto it = devices.find(task.abstract);

				{
					std::stringstream log;
					log << "CDeviceManager::setCommandToClient: " << it->first << " queue has "
							<< it->second.taskQue.size() << " task(s).";
					SetTo::CommonLog(trace, log.str());
				}

				if (it->second.taskQue.size())
				{
					/*
					 *  Постановка задачи на отправку команды на абстрактное устройство.
					 */
					DeviceCtl::Task& newTask = it->second.taskQue.front();
					GlobalThreadPool::get().AddTask(0, newTask.taskFn);

					std::string abstractName = it->second.devInstance.get()->deviceAbstractName();

					{
						std::stringstream log;
						log << "CDeviceManager::setCommandToClient: task added to " << abstractName;
						SetTo::CommonLog(debug, log.str());
					}
				}

			}
		}
		else
		{
			{
				std::stringstream log;
				log << "CDeviceManager::setCommandToClient: Adresat '" << task.adresat << "'  not found in device list.";
				SetTo::CommonLog(debug, log.str());
			}
		}
	}
	else
	{
		// Event !!!
		// Event не значит, что транзакция отработана, а наоборот, поэтому очередь девайса не трогаем

		// Отправка команды на бизнес-логику
		auto itAdresat = devices.find("logic_bsns_layer");
		if ( itAdresat != devices.end() )
		{
			DeviceCtl::Task clientTask;
			clientTask.txId = 0;
			clientTask.adresat = "";
			clientTask.abstract = "logic_bsns_layer";
			clientTask.concrete = "logic_http";

			{
				std::stringstream log;
				log << "CDeviceManager::setCommandToClient: Set event to '"
						<< clientTask.abstract <<"' from " << concreteDevice << " as Event.";
				SetTo::CommonLog(trace, log.str());
			}

			std::stringstream strparams;
			for (auto& v: devices)
			{
				if (v.second.devInstance->deviceConcreteName() == concreteDevice)
				{
					strparams << "{\"eventid\":" << m_EventCounter << ", \"device\":\""
							<< v.first.c_str() << "\", " << parameters.c_str() << "}";
				}
			}

			m_EventCounter++;

			clientTask.taskFn = boost::bind(sendCommand, devices[clientTask.abstract].devInstance.get(), command, strparams.str());

			itAdresat->second.taskQue.push(clientTask);

			/*
			 * Если задача в очереди клиента одна, то отправку на устройство сделать незамедлительно
			 */
			{
				std::stringstream log;
				log << "CDeviceManager::setCommandToClient: " << itAdresat->first << " queue has "
						<<  itAdresat->second.taskQue.size() << " task(s).";
				SetTo::CommonLog(trace, log.str());
			}

			if (itAdresat->second.taskQue.size() == 1)
			{

				{
					std::stringstream log;
					log << "CDeviceManager::setCommandToDevice: task added to " << itAdresat->first;
					SetTo::CommonLog(debug, log.str());
				}

				/*
				 *  Постановка задачи на отправку команды на абстрактное устройство.
				 */

				GlobalThreadPool::get().AddTask(0, clientTask.taskFn);

			}

		}

		// TODO Set task to pinger

	}

}

void CDeviceManager::ackClient(std::string concreteDevice)
{
	boost::mutex::scoped_lock lock(queMutex);

	DeviceCtl::Task task;
	if ( popDeviceTask(concreteDevice, task) == false )
	{
		// Удивиться, что транзакция клиента была профачена. WTF! It's a BUG!
		std::stringstream error;
		error << "ERROR! CDeviceManager::ackClient: Transaction for " << concreteDevice << " lost";
		SetTo::sendErrorToClient(error);

		SetTo::CommonLog(severity_level::error, error.str());
		return;
	}

	{
		std::stringstream log;
		log << "CDeviceManager::ackClient: Check queue for '" << concreteDevice << "'";
		SetTo::CommonLog(trace, log.str());
	}

	// Послать следующую команду из очереди на клиент

	/*
	 * Если в очереди есть еще задачи для устройства, то отправить следующую
	 */
	auto it = devices.begin();

	for (; it != devices.end(); ++it)
	{
		if (it->second.devInstance.get()->deviceConcreteName() == concreteDevice )
		{

			std::string abstractName = it->second.devInstance.get()->deviceAbstractName();

			{
				std::stringstream log;
				log << "CDeviceManager::ackClient: " << it->first << " queue has "
						<<  it->second.taskQue.size() << " task(s).";
				SetTo::CommonLog(trace, log.str());
			}

			if (it->second.taskQue.size())
			{
				/*
				 *  Постановка задачи на отправку команды на абстрактное устройство.
				 */
				DeviceCtl::Task& newTask = it->second.taskQue.front();
				GlobalThreadPool::get().AddTask(0, newTask.taskFn);

				{
					std::stringstream log;
					log << "CDeviceManager::ackClient: task added to '" << abstractName << "'";
					SetTo::CommonLog(debug, log.str());
				}
			}
			else
			{
				{
					std::stringstream log;
					log << "CDeviceManager::ackClient: queue of device '" << abstractName << "' is empty.";
					SetTo::CommonLog(debug, log.str());
				}
			}

			break;
		}
	}


}

bool CDeviceManager::popDeviceTask(std::string concreteDevice, DeviceCtl::Task& task)
{
	for (auto& v: devices)
	{
		if (v.second.devInstance->deviceConcreteName() == concreteDevice)
		{
			if (v.second.taskQue.size() == 0)
			{
				{
					std::stringstream log;
					log << "CDeviceManager::popDeviceTask: not found tasks in queue for '" << concreteDevice << "'";
					SetTo::CommonLog(trace, log.str());
				}
				return false;
			}

			task = v.second.taskQue.front();
			v.second.taskQue.pop();

			{
				std::stringstream log;
				log << "CDeviceManager::popDeviceTask: found tasks in queue for '" << concreteDevice <<  "'. Task popped.";
				SetTo::CommonLog(trace, log.str());
			}

			return true;
		}
	}

	{
		std::stringstream log;
		log << "ERROR: CDeviceManager::popDeviceTask: not found device '" << concreteDevice << "'";
		SetTo::CommonLog(error, log.str());
	}

	return false;
}
