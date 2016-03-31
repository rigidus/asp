/*
 * winstar16x2.h
 *
 *  Created on: 16 марта 2016 г.
 *      Author: bvl
 */

#ifndef WINSTAR16x2_H_
#define WINSTAR16x2_H_


#include "devices/CBaseDevice.h"
#include "GlobalThreadPool.h"
#include "CDisplayCtl.h"
#include "SetCommandTo.h"

#include "rapidjson/document.h"
#include "rapidjson/writer.h"
#include "rapidjson/stringbuffer.h"


class CSLCDWinstar16x2: public CBaseDevice
{

// CodecType protoCodec;

public:

	CSLCDWinstar16x2(): CBaseDevice(s_concreteName) {}

	~CSLCDWinstar16x2()
	{
		CBaseDevice::disconnectFromCommCtl();
	}

	static const std::string s_concreteName;

	std::vector<uint8_t> rcvData;

	virtual void sendCommand(const std::string command, const std::string pars)
	{
		using namespace rapidjson;

		const std::string attrError("error"); // Опциональный атрибут, отменяет все остальные атрибуты
		const std::string attrScreen("screen"); // Опциональный атрибут, отменяет все остальные атрибуты

		std::cout << "CSLCDWinstar16x2::sendCommand: performs command: " << command << "[" << pars << "]" << std::endl;

//		std::list<std::vector<uint8_t> > data;
		std::vector<uint8_t> data;

		if (m_commCtl.size() == 0)
		{
			std::cout << "ERROR! CSLCDWinstar16x2::sendCommand: communication devices has lost" << std::endl;
			return;
		}

		if ( command == "clear" )
		{
			data.push_back('0');
		}

		if ( command == "show" )
		{
			data.push_back('1');

			// Parse a JSON string into DOM.
		    std::vector< ASCII<>::Ch > jsonArray(pars.length() + 1, 0);
			memcpy(&jsonArray[0], pars.c_str(), (pars.length()) * sizeof(jsonArray[0]));
			jsonArray[pars.length()] = 0;

			Document d;
			d.ParseInsitu(&jsonArray[0]);
			if (d.HasParseError() == true)
			{
				memcpy(&jsonArray[0], pars.c_str(), (pars.length()+1) * sizeof(jsonArray[0]));
				jsonArray[pars.length()] = 0;

				// отправить назад сообщение, что json не распарсился
				std::stringstream error;
				error << "ERROR! CSLCDWinstar16x2::sendCommand: parameters JSON has wrong format: " << &jsonArray[0];
//				setCommandTo::sendErrorToClient(error);

				std::cout << error.str() << std::endl;

				return;
			}


			std::cout << "CSLCDWinstar16x2::sendCommand: JSON was parsed correctly." << std::endl;

			// Stringify the DOM
			StringBuffer buffer;
			Writer<StringBuffer> writer(buffer);
			d.Accept(writer);
			std::cout << buffer.GetString() << std::endl;

			if (d.HasMember(attrError.c_str()) == true)
			{
				// Обработка ошибок составления документа
				Value& valError = d[attrError.c_str()];
				if (valError.IsString())
				{
					std::stringstream error;
					error << "Error has received: " << valError.GetString();

					// TODO: Error parser from logic

					std::cout << error.str() << std::endl;
				}
				else
				{
					std::stringstream error;
					error << "Error has received but value isn't String";

//					setCommandTo::sendErrorToClient(error);
					std::cout << error.str() << std::endl;
				}

				return;
			}

			if (d.HasMember(attrScreen.c_str()) == false)
			{
				// отправить сообщение, что txid не найден и выйти
				std::stringstream error;
				error << "ERROR! CSLCDWinstar16x2::sendCommand: JSON attribute '" << attrScreen << "' not found.";
//				setCommandTo::sendErrorToClient(error);

				std::cout << error.str() << std::endl;

				return;
			}

			Value& valScreen = d[attrScreen.c_str()];

			if (valScreen.IsNumber() == false)
			{
				// отправить сообщение, что txid не число и выйти
				std::stringstream error;
				error << "ERROR! CSLCDWinstar16x2::sendCommand: JSON attribute '" << attrScreen << "' isn't number type";
//				setCommandTo::sendErrorToClient(error);

				std::cout << error.str() << std::endl;

				return;
			}

			uint32_t screenId = (uint32_t) valScreen.GetInt();

			std::cout << "CSLCDWinstar16x2::sendCommand: screenId is " << screenId << std::endl;


			data.push_back(screenId);
//			data.push_back();
		}
		std::cout << "CSLCDWinstar16x2::sendCommand: data.size == " << data.size() << std::endl;

		// command "up"
		if (m_commCtl[0])
			m_commCtl[0]->send(data);

		SetTo::Manager(c_name);

	}

	virtual bool connectToCommCtl()
	{
		return CBaseDevice::connectToCommCtl();
	}

};

#endif /* WINSTAR16x2_H_ */
