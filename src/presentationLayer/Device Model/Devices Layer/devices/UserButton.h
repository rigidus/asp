/*
 * UserButton.h
 *
 *  Created on: 14 марта 2016 г.
 *      Author: drema
 */

#ifndef USERBUTTON_H_
#define USERBUTTON_H_

#include "devices/CBaseDevice.h"
#include "GlobalThreadPool.h"
#include "SetCommandTo.h"
#include "CPinCtl.h"

class CUserButton: public CBaseDevice
{

public:

	CUserButton(): CBaseDevice(s_concreteName) {}

	~CUserButton()
	{
		CBaseDevice::disconnectFromCommCtl();
	}

	static const std::string s_concreteName;

	std::vector<uint8_t> rcvData;

	virtual void sendCommand(const std::string command, const std::string pars)
	{
		std::cout << "UserButton::sendCommand: performs command: " << command << "[" << pars << "]" << std::endl;

		std::list<std::vector<uint8_t> > data;

		if (m_commCtl.size() == 0)
		{
			std::cout << "ERROR! UserButton::sendCommand: communication devices has lost" << std::endl;
			return;
		}

		if (m_commCtl[0])
		{
			// TODO: command "get value"
			if (command == "get_value")
			{
				uint8_t Value = ((CPinCtl*)m_commCtl[0].get())->getPinValue();

				// Create JSON for 'value': Free, Pushed, GPIO Error
				std::stringstream answer;

				/* TODO: сделать общий генератор идшек для событий */
				answer << "{\"eventid\":" << 0 << ", \"device\":\"" << c_name << "\", ";

				switch(Value)
				{
				case '0':
					answer << "\"state\":\"" << "\"free\"}" << std::endl;
					setCommandTo::Client( setCommandTo::Transaction, c_name, "answer: ", answer.str());
					break;
				case '1':
					answer << "\"state\":\"" << "\"pushed\"}" << std::endl;
					setCommandTo::Client( setCommandTo::Transaction, c_name, "answer: ", answer.str());
					break;
				default:
					answer << "\"state\":\"" << "\"gpio error\"}" << std::endl;
					setCommandTo::Client( setCommandTo::Transaction, c_name, "answer: ", answer.str());
				}

			}
		}

	}

	virtual bool connectToCommCtl()
	{
		return CBaseDevice::connectToCommCtl();
	}


	virtual void performEvent(std::vector<uint8_t>& rcvData)
	{
		std::cout << "CUserButton::performEvent: performs Event from device: " << c_name << ": ";
		for (auto v: rcvData) std::cout << v << " ";
		std::cout << std::endl;

		if (rcvData.size() == 1 && rcvData[0] =='1')
		{
			// TODO: Вызывать установку задачи для клиента по имени абстрактного девайса
			// Сейчас вызывается по конкретному
			std::stringstream answer;

			/* TODO: сделать общий генератор идшек для событий */
			answer << "{\"eventid\":" << 0 << ", \"device\":\"" << c_name << "\", \"command\":\"press\"}";

			setCommandTo::Client( setCommandTo::Event, c_name, "answer: ", answer.str());

		}
	}


};



#endif /* USERBUTTON_H_ */
