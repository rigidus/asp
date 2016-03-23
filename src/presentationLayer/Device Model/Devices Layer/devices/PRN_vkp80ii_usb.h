/*
 * PRN_vkp80ii_usb.h
 *
 *  Created on: 18 марта 2016 г.
 *      Author: bvl
 */

#ifndef VKP80II_USB_H_
#define VKP80II_USB_H_


#include "devices/CBaseDevice.h"
#include "GlobalThreadPool.h"
#include "CPrnCtl.h"
#include "SetCommandTo.h"

#include "rapidjson/document.h"
#include "rapidjson/writer.h"
#include "rapidjson/stringbuffer.h"


class CPRN_vkp80ii_usb: public CBaseDevice
{

// CodecType protoCodec;

public:

	CPRN_vkp80ii_usb(): CBaseDevice(s_concreteName) {}

	~CPRN_vkp80ii_usb()
	{
		CBaseDevice::disconnectFromCommCtl();
	}

	static const std::string s_concreteName;

	std::vector<uint8_t> rcvData;

	virtual void sendCommand(const std::string command, const std::string pars)
	{
		using namespace rapidjson;

		const std::string attrError("error"); // Опциональный атрибут, отменяет все остальные атрибуты
		const std::string attrTicket("ticket"); // Опциональный атрибут, отменяет все остальные атрибуты
		const std::string attrBarcode("barcode"); // Опциональный атрибут, отменяет все остальные атрибуты

		std::cout << "CPRN_vkp80ii_usb::sendCommand: performs command: " << command << "[" << pars << "]" << std::endl;

//		std::list<std::vector<uint8_t> > data;
		std::vector<uint8_t> data;

		if (m_commCtl.size() == 0)
		{
			std::cout << "ERROR! CPRN_vkp80ii_usb::sendCommand: communication devices has lost" << std::endl;
			return;
		}

		if ( command == "clear" ) //!!!fix not implemented, candidate to be deleted.
		{
			data.push_back('0');
		}

		if ( command == "print" )
		{
			data.push_back('1');

			// Parse a JSON string into DOM.
		    std::vector< UTF8<>::Ch > jsonArray(pars.length() + 1, 0);
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
				error << "ERROR! CPRN_vkp80ii_usb::sendCommand: parameters JSON has wrong format: " << &jsonArray[0];
//				setCommandTo::sendErrorToClient(error);

				std::cout << error.str() << std::endl;
// */
				return;
			}

			std::cout << "CPRN_vkp80ii_usb::sendCommand: JSON was parsed correctly." << std::endl;

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

			if (d.HasMember(attrTicket.c_str()) == false)
			{
				// отправить сообщение, что txid не найден и выйти
				std::stringstream error;
				error << "ERROR! CPRN_vkp80ii_usb::sendCommand: JSON attribute '" << attrTicket << "' not found.";
//				setCommandTo::sendErrorToClient(error);

				std::cout << error.str() << std::endl;

				return;
			}

			Value& valTicket = d[attrTicket.c_str()];

			if (valTicket.IsArray() == false)
			{
				// отправить сообщение, что Ticket не массив и выйти
				std::stringstream error;
				error << "ERROR! CPRN_vkp80ii_usb::sendCommand: JSON attribute '" << attrTicket << "' isn't Array type";
//				setCommandTo::sendErrorToClient(error);

				std::cout << error.str() << std::endl;

				return;
			}

			std::stringstream htmlTicket;
			htmlTicket << "\
			<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\">\
			<HTML>\
			    <HEAD>\
				<META http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">\
			    <TITLE></TITLE>\
			    </HEAD>\
			<BODY>\
			<TABLE BORDER=\"0\">\
			<TR><TD>\n";

			for (SizeType i = 0; i < valTicket.Size(); i++) // Uses SizeType instead of size_t
			 {
				htmlTicket << valTicket[i].GetString();
				htmlTicket << "\n</TD></TR><TR><TD>\n";
			 }

/*
			std::string tempstr = htmlTicket.str();
			data[1](tempstr.begin(), tempstr.end());
*/
			if (d.HasMember(attrBarcode.c_str()) == false)
			{
				std::cout << "CPRN_vkp80ii_usb::sendCommand: JSON attribute '" << attrBarcode << "' not found." << std::endl;
			}
			else
			{
				std::cout << "CPRN_vkp80ii_usb::sendCommand: JSON attribute '" << attrBarcode << "' found!" << std::endl;

				Value& valBarcode = d[attrBarcode.c_str()];
				if (valBarcode.IsString() == false)
				{
					// отправить сообщение, что Ticket не массив и выйти
					std::stringstream error;
					error << "ERROR! CPRN_vkp80ii_usb::sendCommand: JSON attribute '" << attrBarcode << "' isn't String type";
	//				setCommandTo::sendErrorToClient(error);
					std::cout << error.str() << std::endl;
	//				return;
				}
				else
				{
					std::cout << "CPRN_vkp80ii_usb::sendCommand: barcode is '" << valBarcode.GetString() << "' found!" << std::endl;

					htmlTicket << "<IMG src=/aspp/barcode.gif>";
				}

			}

			htmlTicket << "</TD></TR></TABLE></BODY></HTML>";

			while (!htmlTicket.eof()) {
					data.push_back(htmlTicket.get());
				}
		}


/*
		int i;
		for (i=1; i < htmlTicket.str(); i++)
		{

		}
		*/
		// command "up"
		if (m_commCtl[0])
			m_commCtl[0]->send(data);

		setCommandTo::Manager(c_name);
	}

	virtual bool connectToCommCtl()
	{
		return CBaseDevice::connectToCommCtl();
	}

};


#endif /* VKP80II_USB_H_ */
