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
	enum PinFunc { IsOpen, DoOpen, IsClose, DoClose, IsCarPresent, LastFunction };

	enum TState { Idle, InProcessUp, InProcessDown, Closed, Opened, CarPresent, InError, InErrorCommDevice };
	boost::atomic<TState> actualState;

	TState StateDetector()
	{
		TState state = actualState.load(boost::memory_order_relaxed);

		// На процесс не обращаем внимания
		if (state == InProcessUp || state == InProcessDown) return state;

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

	// TODO: Сделать отправку транзакций по ТЗ
	// TODO: Сделать отправку событий с глобальным счетчиком евентов
	// TODO: Научиться использовать сериализтор рапидджейсон
	// TODO: Добавлять нужные поля в менеджере транзакций, отсюда отправлять только параметры
	void SendAnswerToClient(TState state,  std::string command, setCommandTo::CommandType type = setCommandTo::CommandType::Transaction)
	{

		if (state == Closed)
		{
			std::stringstream str;
			str << "\"command\":\"" << command << "\", " << "\"state\" : \"closed\"";
			setCommandTo::Client(type, c_name, "send", str.str());
		}

		if (state == Opened)
		{
			std::stringstream str;
			str << "\"command\":\"" << command << "\", " << "\"state\" : \"opened\", " << "\"car_present\" : \"false\"";
			setCommandTo::Client(type, c_name, "send", str.str());
		}

		if (state == InError)
		{
			std::stringstream str;
			str << "\"command\":\"" << command << "\", " << "\"state\" : \"unknown\"";
			setCommandTo::Client(type, c_name, "send", str.str());
		}

		if (state == InErrorCommDevice)
		{
			std::stringstream str;
			str << "\"command\":\"" << command << "\", " << "\"state\" : \"communication devices failed\"";
			setCommandTo::Client(type, c_name, "send", str.str());
		}

		if (state == InProcessUp)
		{
			std::stringstream str;
			str << "\"command\":\"" << command << "\", " << "\"state\" : \"in process up\", " << "\"car_present\" : \"false\"";
			setCommandTo::Client(type, c_name, "send", str.str());
		}

		if (state == InProcessDown)
		{
			std::stringstream str;
			str << "\"command\":\"" << command << "\", " << "\"state\" : \"in process down\", " << "\"car_present\" : \"false\"";
			setCommandTo::Client(type, c_name, "send", str.str());
		}

		if (state == CarPresent)
		{
			std::stringstream str;
			str << "\"command\":\"" << command << "\", " << "\"state\" : \"opened\", " << "\"car_present\" : \"true\"";
			setCommandTo::Client(type, c_name, "send", str.str());
		}

		if (state == Idle)
		{
			std::stringstream str;
			str << "\"command\":\"" << command << "\", " << "\"state\" : \"idle\"";
			setCommandTo::Client(type, c_name, "send", str.str());
		}

	}

	// Распознать пин, с которого пришел евент
	PinFunc getPinFunc(std::string& name)
	{
		uint32_t i = 0;
		while ( i < m_commCtl.size() && m_commCtl[i]->m_commName != name ) ++i;

		if ( i == m_commCtl.size())
		{
			std::cout << "ERROR! GPIOShlagbaum::sendCommand: communication device '" << name << "' has lost in '"
					<< c_name << "'"  << std::endl;

			actualState = InErrorCommDevice;
			SendAnswerToClient(InErrorCommDevice, "error", setCommandTo::CommandType::Event);
			return LastFunction;
		}

		return (PinFunc) i;
	}


public:

	CGPIOShlagbaum(): CBaseDevice(s_concreteName), actualState(Idle) {}

	~CGPIOShlagbaum()
	{
		CBaseDevice::disconnectFromCommCtl();
	}

	static const std::string s_concreteName;

	std::vector<uint8_t> rcvData;

	// Информацию с него
	// составить параметры команды и отправить на клиента
	virtual void performEvent(std::string& commDeviceName, std::vector<uint8_t>& rcvData)
	{
		std::cout << "GPIOShlagbaum::performEvent: from '" << commDeviceName << "'" << std::endl;

		if (m_commCtl.size() < LastFunction)
		{
			std::cout << "ERROR! GPIOShlagbaum::sendCommand: communication devices has lost in '" << c_name << "'" << std::endl;

			actualState = InErrorCommDevice;
			SendAnswerToClient(InErrorCommDevice, "error", setCommandTo::CommandType::Event);
			return;
		}

		PinFunc i = getPinFunc(commDeviceName);

		if ( i == LastFunction)
		{
			return;
		}

		TState state = actualState.load(boost::memory_order_relaxed);

		if ( i == IsOpen )
		{
			if ( state == InProcessUp )
			{
				std::stringstream str;
				str << "\"command\":\"up\", " << "\"parameters\":{ \"result\":\"OK\" }";
				setCommandTo::Client(setCommandTo::CommandType::Event, c_name, "send", str.str());

				actualState = Opened;

				std::vector<uint8_t> data;
				data.push_back(0);
				data.push_back('0');
				m_commCtl[DoOpen]->send(data);

				return;
			}
		}

		if ( i == IsClose )
		{
			if ( state == InProcessDown )
			{
				std::stringstream str;
				str << "\"command\":\"down\", " << "\"parameters\":{ \"result\":\"OK\" }";
				setCommandTo::Client(setCommandTo::CommandType::Event, c_name, "send", str.str());

				actualState = Closed;

				std::vector<uint8_t> data;
				data.push_back(0);
				data.push_back('0');
				m_commCtl[DoClose]->send(data);

				return;
			}
		}

		if ( i == IsCarPresent )
		{
			TState state = StateDetector();



			if ( state == CarPresent )
			{
				std::stringstream str;
				str << "\"command\":\"car_in\", " << "\"state\" : \"opened\", " << "\"car_present\" : \"true\"";
				setCommandTo::Client(setCommandTo::CommandType::Event, c_name, "send", str.str());
			}
			else
			{
				if ( state == Opened )
				{
					std::stringstream str;
					str << "\"command\":\"car_out\", " << "\"state\" : \"opened\", " << "\"car_present\" : \"false\"";
					setCommandTo::Client(setCommandTo::CommandType::Event, c_name, "send", str.str());
				}

				if ( state == Closed )
				{
					std::stringstream str;
					str << "\"command\":\"car_out\", " << "\"state\" : \"closed\", " << "\"car_present\" : \"false\"";
					setCommandTo::Client(setCommandTo::CommandType::Event, c_name, "send", str.str());
				}
			}
		}

	}

	virtual void sendCommand(const std::string command, const std::string pars)
	{
		std::cout << "GPIOShlagbaum::sendCommand: performs command: " << command << "[" << pars << "]" << std::endl;

		std::list<std::vector<uint8_t> > data;

		if (m_commCtl.size() < LastFunction)
		{
			std::cout << "ERROR! GPIOShlagbaum::sendCommand: communication devices has lost" << std::endl;

			actualState = InErrorCommDevice;
			SendAnswerToClient(InErrorCommDevice, command);
			return;
		}

		TState state = StateDetector();

		// Сообщаем о невозможности работать со шлагбаумом
		if ( state == InError || state == InProcessUp || state == InProcessDown )
		{
			SendAnswerToClient(state, command);
			return;
		}

		// command "down"
		// Закрывает шлагбаум, если под ним нет машины и если уже нет процесса
		// Ждет факта закрытия по событию концевика
		// Возможные ответы: шлагбаум в работе, есть машина, уже закрыт, ошибка состояния, успех
		if (command == "down")
		{
			if ( state == Opened ) // Это гарантия, что шлагбаум поднят и машины нет
			{
				actualState = InProcessDown;

				std::vector<uint8_t> data;
				data.push_back(0);
				data.push_back('1');
				m_commCtl[DoClose]->send(data);

				boost::this_thread::sleep(boost::posix_time::milliseconds(3000));

				if (actualState == InProcessDown)
				{
					actualState = Idle;
					data[1] = '0';
					m_commCtl[DoClose]->send(data);
				}

				TState postState = StateDetector();
				if ( postState == InError )
				{
					SendAnswerToClient(postState, command);
				}

				return;
			}
			else
			{
				SendAnswerToClient(state, command);
				return;
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

				actualState = InProcessUp;

				std::vector<uint8_t> data;
				data.push_back(0);
				data.push_back('1');
				m_commCtl[DoOpen]->send(data);

				boost::this_thread::sleep(boost::posix_time::milliseconds(3000));

				if (actualState == InProcessUp)
				{
					actualState = Idle;

					data[1] = '0';
					m_commCtl[DoOpen]->send(data);
				}

				TState postState = StateDetector();
				if ( postState == InError )
				{
					SendAnswerToClient(postState, command);
				}

				return;
			}
			else
			{
				SendAnswerToClient(state, command);
				return;
			}
		}


		// command "state"
		// Возвращает позицию шлагбаума по концевикам
		// Возможные ответы: в процессе, ошибка состояния, закрыт, открыт
		if (command == "state")
		{
			SendAnswerToClient(state, command);
			return;
		}

	}

	virtual bool connectToCommCtl()
	{
		return CBaseDevice::connectToCommCtl();
	}

};


#endif /* GPIOSHLAGBAUM_H_ */
