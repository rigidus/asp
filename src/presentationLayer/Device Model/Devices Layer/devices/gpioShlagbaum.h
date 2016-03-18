/*
 * gpioShlagbaum.h
 *
 *  Created on: 5 марта 2016 г.
 *      Author: drema
 */

#ifndef GPIOSHLAGBAUM_H_
#define GPIOSHLAGBAUM_H_

#include "devices/CBaseDevice.h"
#include "GlobalThreadPool.h"
#include "CPinCtl.h"
#include "SetCommandTo.h"

class CGPIOShlagbaum: public CBaseDevice
{
	/* m_CommCtl indexes:
	* 0 - sensor 'position Open' when 0
	* 1 - relay for 'action Open' when active 1
	* 2 - sensor 'position Close' when 0
	* 3 - relay for 'action Close' when active 1
	* 4 - sensor 'Car Present' when 0
	*/
	enum PinFunc { IsOpen, DoOpen, IsClose, DoClose, IsCarPresent };

	enum TState { Idle, InProcess, Closed, Opened, CarPresent, InError };
	boost::atomic<TState> actualState;

	TState StateDetector()
	{
		TState state = actualState.load(boost::memory_order_relaxed);

		// На процесс не обращаем внимания
		if (state == InProcess) return state;

		bool realOpen = ((CPinCtl*)m_commCtl[IsOpen].get())->getPinValue() == '0';
		bool realClose = ((CPinCtl*)m_commCtl[IsClose].get())->getPinValue() == '0';
		bool realCarPresent = ((CPinCtl*)m_commCtl[IsCarPresent].get())->getPinValue() == '0';

		// если состояния концевиков равны, это проблема
		if ( realOpen == realClose)
		{
			actualState = InError;
			return InError;
		}

		// если есть машина, но шлагбаум опущен, это проблема
		if ( realClose && realCarPresent )
		{
			actualState = InError;
			return InError;
		}

		// если есть машина и шлагбаум поднят, это есть машина
		if ( realOpen && realCarPresent )
		{
			actualState = CarPresent;
			return CarPresent;
		}

		if ( realOpen && !CarPresent )
		{
			actualState = Opened;
			return Opened;
		}

		if ( realOpen && !CarPresent )
		{
			actualState = Closed;
			return Closed;
		}

		return state;
	}

public:

	CGPIOShlagbaum(): CBaseDevice(s_concreteName), actualState(Idle) {}

	~CGPIOShlagbaum()
	{
		CBaseDevice::disconnectFromCommCtl();
	}

	static const std::string s_concreteName;

	std::vector<uint8_t> rcvData;

	virtual void sendCommand(const std::string command, const std::string pars)
	{
		std::cout << "GPIOShlagbaum::sendCommand: performs command: " << command << "[" << pars << "]" << std::endl;

		std::list<std::vector<uint8_t> > data;

		if (m_commCtl.size() == 0)
		{
			std::cout << "ERROR! GPIOShlagbaum::sendCommand: communication devices has lost" << std::endl;
			return;
		}

		TState state = StateDetector();

		// Сообщаем о невозможности работать со шлагбаумом
		if (state == InError)
		{
			std::stringstream str;
			str << "\"device\" : \"" << c_name << "\"state\" : \"unknown\"}";
			setCommandTo::Client(setCommandTo::CommandType::Transaction, c_name, "send", str.str());
		}

		if (state == InProcess)
		{
			std::stringstream str;
			str << "\"device\" : \"" << c_name << "\"state\" : \"in_process\"}";
			setCommandTo::Client(setCommandTo::CommandType::Transaction, c_name, "send", str.str());
		}

		// command "down"
		// Закрывает шлагбаум, если под ним нет машины и если уже нет процесса
		// Ждет факта закрытия по событию концевика
		// Возможные ответы: шлагбаум в работе, есть машина, уже закрыт, ошибка состояния, успех
		if (command == "down")
		{
			if ( state == Opened ) // Это гарантия, что шлагбаум поднят и машины нет
			{
				std::vector<uint8_t> data;
				data.push_back(0);
				data.push_back('1');
				m_commCtl[DoClose]->send(data);

				boost::this_thread::sleep(boost::posix_time::milliseconds(3000));

				data[1] = '0';
				m_commCtl[DoClose]->send(data);

				TState postState = StateDetector();

				if (postState == Closed)
				{
					std::stringstream str;
					str << "\"device\" : \"" << c_name << "\"state\" : \"closed\"}";
					setCommandTo::Client(setCommandTo::CommandType::Transaction, c_name, "send", str.str());
				}else
				if (postState == Opened)
				{
					std::stringstream str;
					str << "\"device\" : \"" << c_name << "\"state\" : \"opened\"}";
					setCommandTo::Client(setCommandTo::CommandType::Transaction, c_name, "send", str.str());
				}else
				{
					std::stringstream str;
					str << "\"device\" : \"" << c_name << "\"state\" : \"unknown\"}";
					setCommandTo::Client(setCommandTo::CommandType::Transaction, c_name, "send", str.str());
				}
			}
			else
			{
				if (state == Closed)
				{
					std::stringstream str;
					str << "\"device\" : \"" << c_name << "\"state\" : \"closed\"}";
					setCommandTo::Client(setCommandTo::CommandType::Transaction, c_name, "send", str.str());
				}

				if (state == CarPresent)
				{
					std::stringstream str;
					str << "\"device\" : \"" << c_name << "\"state\" : \"opened\", " << "\"car_present\" : \"true\" }";
					setCommandTo::Client(setCommandTo::CommandType::Transaction, c_name, "send", str.str());
				}
			}
		}

		// command "up"
		// Безусловно открывает шлагбаум, если уже нет процесса
		// Ждет факта открытия по событию концевика
		// Возможные ответы: шлагбаум в работе, уже открыт, ошибка состояния, успех
		if (command == "up")
		{
			if ( state == Closed ) // Это гарантия, что шлагбаум опущен
			{
				std::vector<uint8_t> data;
				data.push_back(0);
				data.push_back('1');
				m_commCtl[DoOpen]->send(data);

				boost::this_thread::sleep(boost::posix_time::milliseconds(3000));

				data[1] = '0';
				m_commCtl[DoOpen]->send(data);

				TState postState = StateDetector();

				if (postState == Closed)
				{
					std::stringstream str;
					str << "\"device\" : \"" << c_name << "\"state\" : \"closed\"}";
					setCommandTo::Client(setCommandTo::CommandType::Transaction, c_name, "send", str.str());
				}

				if (postState == Opened)
				{
					std::stringstream str;
					str << "\"device\" : \"" << c_name << "\"state\" : \"opened\"}";
					setCommandTo::Client(setCommandTo::CommandType::Transaction, c_name, "send", str.str());
				}

				if (postState == InError)
				{
					std::stringstream str;
					str << "\"device\" : \"" << c_name << "\"state\" : \"unknown\"}";
					setCommandTo::Client(setCommandTo::CommandType::Transaction, c_name, "send", str.str());
				}
			}
			else
			{
				if (state == Opened)
				{
					std::stringstream str;
					str << "\"device\" : \"" << c_name << "\"state\" : \"opened\"}";
					setCommandTo::Client(setCommandTo::CommandType::Transaction, c_name, "send", str.str());
				}
			}
		}

		// command "is_car"
		// Возвращает состояние датчика car_present
		// Возможные ответы: датчик не найден, есть машина, нет машины
		if (command == "is_car")
		{
			if (state == CarPresent)
			{
				std::stringstream str;
				str << "\"device\" : \"" << c_name << "\"state\" : \"opened\", " << "\"car_present\" : \"true\" }";
				setCommandTo::Client(setCommandTo::CommandType::Transaction, c_name, "send", str.str());
			}
			else
			{
				if (state == Opened)
				{
					std::stringstream str;
					str << "\"device\" : \"" << c_name << "\"state\" : \"opened\", " << "\"car_present\" : \"false\" }";
					setCommandTo::Client(setCommandTo::CommandType::Transaction, c_name, "send", str.str());
				}

				if (state == Closed)
				{
					std::stringstream str;
					str << "\"device\" : \"" << c_name << "\"state\" : \"closed\", " << "\"car_present\" : \"false\" }";
					setCommandTo::Client(setCommandTo::CommandType::Transaction, c_name, "send", str.str());
				}

				if (state == InError)
				{
					std::stringstream str;
					str << "\"device\" : \"" << c_name << "\"state\" : \"unknown\", " << "\"car_present\" : \"false\" }";
					setCommandTo::Client(setCommandTo::CommandType::Transaction, c_name, "send", str.str());
				}
			}
		}

		// command "state"
		// Возвращает позицию шлагбаума по концевикам
		// Возможные ответы: в процессе, ошибка состояния, закрыт, открыт
		if (command == "state")
		{
			if (state == Opened)
			{
				std::stringstream str;
				str << "\"device\" : \"" << c_name << "\"state\" : \"opened\", " << "\"car_present\" : \"false\" }";
				setCommandTo::Client(setCommandTo::CommandType::Transaction, c_name, "send", str.str());
			}

			if (state == Closed)
			{
				std::stringstream str;
				str << "\"device\" : \"" << c_name << "\"state\" : \"closed\", " << "\"car_present\" : \"false\" }";
				setCommandTo::Client(setCommandTo::CommandType::Transaction, c_name, "send", str.str());
			}
		}

	}

	virtual bool connectToCommCtl()
	{
		return CBaseDevice::connectToCommCtl();
	}

};


#endif /* GPIOSHLAGBAUM_H_ */
