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

		{
			std::stringstream log;
			log << "CPRN_vkp80ii_usb::sendCommand: performs command: " << command << "[" << pars << "]";
			SetTo::LocalLog(c_name, trace, log.str());
		}

		std::vector<uint8_t> data;

		if (m_commCtl.size() == 0)
		{
			{
				std::stringstream log;
				log << "ERROR! CPRN_vkp80ii_usb::sendCommand: communication devices has lost";
				SetTo::LocalLog(c_name, error, log.str());
			}
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

				SetTo::LocalLog(c_name, severity_level::error, error.str());

				return;
			}

			{
				std::stringstream log;
				log << "CPRN_vkp80ii_usb::sendCommand: JSON was parsed correctly.";
				SetTo::LocalLog(c_name, trace, log.str());
			}

			// Stringify the DOM
			StringBuffer buffer;
			Writer<StringBuffer> writer(buffer);
			d.Accept(writer);

			{
				std::stringstream log;
				log << "CPRN_vkp80ii_usb::sendCommand: string buffer = " << buffer.GetString();
				SetTo::LocalLog(c_name, trace, log.str());
			}

			if (d.HasMember(attrError.c_str()) == true)
			{
				// Обработка ошибок составления документа
				Value& valError = d[attrError.c_str()];
				if (valError.IsString())
				{
					std::stringstream error;
					error << "Error has received: " << valError.GetString();

					// TODO: Error parser from logic

					SetTo::LocalLog(c_name, severity_level::error, error.str());
				}
				else
				{
					std::stringstream error;
					error << "Error has received but value isn't String";

//					setCommandTo::sendErrorToClient(error);
					SetTo::LocalLog(c_name, severity_level::error, error.str());
				}

				return;
			}

			if (d.HasMember(attrTicket.c_str()) == false)
			{
				// отправить сообщение, что txid не найден и выйти
				std::stringstream error;
				error << "ERROR! CPRN_vkp80ii_usb::sendCommand: JSON attribute '" << attrTicket << "' not found.";
//				setCommandTo::sendErrorToClient(error);

				SetTo::LocalLog(c_name, severity_level::error, error.str());

				return;
			}

			Value& valTicket = d[attrTicket.c_str()];

			if (valTicket.IsArray() == false)
			{
				// отправить сообщение, что Ticket не массив и выйти
				std::stringstream error;
				error << "ERROR! CPRN_vkp80ii_usb::sendCommand: JSON attribute '" << attrTicket << "' isn't Array type";
//				setCommandTo::sendErrorToClient(error);

				SetTo::LocalLog(c_name, severity_level::error, error.str());

				return;
			}

			std::stringstream htmlTicket;
			htmlTicket << "\n"
			"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\">\n"
			"<HTML>\n"
			    "<HEAD>\n"
				"<META http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">\n"
			    "<TITLE></TITLE>\n"
			    "</HEAD>\n"
			"<BODY marginwidth=\"0\" leftmargin=\"0\" marginheight=\"0\" topmargin=\"0\">\n"
			"<BASEFONT SIZE=\"3\">\n"
			"<FONT FACE=\"Arial\">\n"
			"<TABLE BORDER=\"0\" CELLPADDING=\"0\" CELLSPACING=\"0\">\n"
			"<TR>";

			for (SizeType i = 0; i < valTicket.Size(); i++) // Uses SizeType instead of size_t
			 {
				switch (i)
				{
				case 1: // WELCOME!
					htmlTicket << "<TH><FONT size=3><B>\n";
					htmlTicket << valTicket[i].GetString();
					htmlTicket << "\n</B></FONT></TH></TR><TR>";
					break;
				case 0:
				case 2: // skipping horizontal breakers
				case 8:
				case 12:
				case 24:
					break;
				case 3: // Number of ticket
					htmlTicket << "<TD align=right><FONT size=4>\n";
					htmlTicket << valTicket[i].GetString();
					htmlTicket << "\n</FONT></TD></TR><TR>";
					break;
				case 4: // Main data on this ticket
				case 5:
				case 6:
				case 7:
					htmlTicket << "<TD>\n";
					htmlTicket << valTicket[i].GetString();
					htmlTicket << "\n</TD></TR><TR>";
					break;
				case 9: // Follow to
					htmlTicket << "<TD align=center>\n";
					htmlTicket << valTicket[i].GetString();
					htmlTicket << "\n</TD></TR><TR>";
					break;
				case 10: // place# to follow
					htmlTicket << "<TD align=center><FONT size=4><B>\n";
					htmlTicket << valTicket[i].GetString();
					htmlTicket << "\n</B></FONT></TD></TR><TR>";
					break;
				case 11: // sector# to follow
					htmlTicket << "<TD align=center><FONT size=4><B>\n";
					htmlTicket << valTicket[i].GetString();
					htmlTicket << "\n</B></FONT></TD></TR><TR>";
					break;
				case 13:// Misc info
				case 14:
				case 15:
				case 16:
				case 17:
				case 18:
				case 19:
				case 20:
				case 21:
				case 22:
				case 23:
				case 25:
				case 26:
					htmlTicket << "<TD><FONT size=1>\n";
					htmlTicket << valTicket[i].GetString();
					htmlTicket << "\n</FONT></TH></TR><TR>";
					break;
				default:
					htmlTicket << "<TD>\n";
					htmlTicket << valTicket[i].GetString();
					htmlTicket << "\n</TD></TR><TR>";
					break;
				}
			 }

/*
			std::string tempstr = htmlTicket.str();
			data[1](tempstr.begin(), tempstr.end());
*/
			if (d.HasMember(attrBarcode.c_str()) == false)
			{
				{
					std::stringstream log;
					log << "CPRN_vkp80ii_usb::sendCommand: JSON attribute '" << attrBarcode << "' not found.";
					SetTo::LocalLog(c_name, trace, log.str());
				}
			}
			else
			{
				{
					std::stringstream log;
					log << "CPRN_vkp80ii_usb::sendCommand: JSON attribute '" << attrBarcode << "' found!";
					SetTo::LocalLog(c_name, trace, log.str());
				}

				Value& valBarcode = d[attrBarcode.c_str()];
				char bcgenStr[50];
				char tmpstr1[20], tmpstr2[20];
				std::string compressedStr;
				memset(bcgenStr, 0, sizeof(bcgenStr));
				memset(tmpstr1, 0, sizeof(tmpstr1));
				memset(tmpstr2, 0, sizeof(tmpstr2));
//				memset(compressedStr, 0, sizeof(compressedStr));

				if (valBarcode.IsString() == false)
				{
					// отправить сообщение, что Ticket не массив и выйти
					std::stringstream error;
					error << "ERROR! CPRN_vkp80ii_usb::sendCommand: JSON attribute '" << attrBarcode << "' isn't String type";
	//				setCommandTo::sendErrorToClient(error);
					SetTo::LocalLog(c_name, severity_level::error, error.str());
	//				return;
				}
				else
				{
					{
						std::stringstream log;
						log << "CPRN_vkp80ii_usb::sendCommand: barcode is '" << valBarcode.GetString() << "' found!";
						SetTo::LocalLog(c_name, trace, log.str());
					}

					strcpy(tmpstr1, valBarcode.GetString());

					int i, j;
					j = 0;
					for (i = 0; tmpstr1[i] != '\0'; i++)
					{
						if (tmpstr1[i] != ' ')
						{
							tmpstr2[j] = tmpstr1[i];
							j++;
						}
					}

					sprintf(bcgenStr, "barcode -b \"%s\" -p 110x297mm -t 1x6 -o /aspp/barcode.ps", tmpstr2); //valBarcode.GetString());

					{
						std::stringstream log;
						log << "CPRN_vkp80ii_usb::sendCommand: compressed barcode is " << tmpstr2;
						SetTo::LocalLog(c_name, trace, log.str());
					}

					{
						std::stringstream log;
						log << "CPRN_vkp80ii_usb::sendCommand: barcode returned: " << std::system(bcgenStr);
						SetTo::LocalLog(c_name, trace, log.str());
					}

					{
						std::stringstream log;
						log << "CPRN_vkp80ii_usb::sendCommand: gs returned: " << std::system("gs -dQUIET -dSAFER -dBATCH "
								"-dNOPAUSE -dNOPROMPT -dMaxBitmap=500000000 -dAlignToPixels=0 -dGridFitTT=2 -dTextAlphaBits=4 "
								"-dGraphicsAlphaBits=4 -sDEVICE=jpeg -sOutputFile=/aspp/barcode.jpg /aspp/barcode.ps");
						SetTo::LocalLog(c_name, trace, log.str());
					}

//					htmlTicket.unget();htmlTicket.unget(); // removing ">\n"
					htmlTicket << "<TD background=\"./barcode.jpg\"  height=200 width=310>\n</TD>";
				}

			}

			htmlTicket << "</TR></TABLE></FONT></BODY></HTML>";

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
		{
			{
				std::stringstream log;
				log << "CPRN_vkp80ii_usb::sendCommand: executing send(data)...";
				SetTo::LocalLog(c_name, trace, log.str());
			}
			m_commCtl[0]->send(data);
		}
		else
		{
			std::stringstream log;
			log << "ERROR! CPRN_vkp80ii_usb::sendCommand: failed executing send(data) due to nil m_commCtl";
			SetTo::LocalLog(c_name, error, log.str());
		}

		SetTo::Manager(c_name);
	}

	virtual bool connectToCommCtl()
	{
		return CBaseDevice::connectToCommCtl();
	}

};


#endif /* VKP80II_USB_H_ */
