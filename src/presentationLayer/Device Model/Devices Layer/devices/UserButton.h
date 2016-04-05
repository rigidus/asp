//============================================================================
// Name        : testShlagbaum.h
// Author      : aav
// Created on  : 18 февр. 2016 г.
// Version     : v.0.1
// Copyright   : Non nobis, Domine, non nobis, sed nomini tuo da gloriam.
// Description : Concrete device
// "User hardware button on the checkpoint for income to parking"
//============================================================================

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
		{
			std::stringstream log;
			log << "UserButton::sendCommand: performs command: " << command << "[" << pars << "]";
			SetTo::LocalLog(c_name, trace, log.str());
		}

		std::list<std::vector<uint8_t> > data;

		if (m_commCtl.size() == 0)
		{
			SetTo::LocalLog(c_name, trace, "ERROR! UserButton::sendCommand: communication devices has lost");
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
					SetTo::Client( SetTo::Transaction, c_name, "answer: ", answer.str());
					break;
				case '1':
					answer << "\"state\":\"" << "\"pushed\"}" << std::endl;
					SetTo::Client( SetTo::Transaction, c_name, "answer: ", answer.str());
					break;
				default:
					answer << "\"state\":\"" << "\"gpio error\"}" << std::endl;
					SetTo::Client( SetTo::Transaction, c_name, "answer: ", answer.str());
				}

			}
		}

	}

	virtual bool connectToCommCtl()
	{
		return CBaseDevice::connectToCommCtl();
	}


	virtual void performEvent(std::string& commDeviceName, std::vector<uint8_t>& rcvData)
	{
		{
			std::stringstream log;
			log << "CUserButton::performEvent: performs Event from device: " << c_name << ": ";
			for (auto v: rcvData) log << v << " ";

			SetTo::LocalLog(c_name, trace, log.str());
		}


		if (rcvData.size() == 1 && rcvData[0] =='1')
		{
			// TODO: Вызывать установку задачи для клиента по имени абстрактного девайса
			// Сейчас вызывается по конкретному
			std::stringstream answer;

			answer << "\"command\" : \"press\"";

			SetTo::Client( SetTo::Event, c_name, "send", answer.str());

		}
	}


};



#endif /* USERBUTTON_H_ */
