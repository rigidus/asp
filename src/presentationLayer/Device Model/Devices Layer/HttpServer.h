/*
 * HttpServer.h
 *
 *  Created on: 21 февр. 2016 г.
 *      Author: drema
 */

#ifndef HTTPSERVER_H_
#define HTTPSERVER_H_

#include <mongoose/mongoose.h>

#include "rapidjson/document.h"
#include "rapidjson/writer.h"
#include "rapidjson/stringbuffer.h"

#include <iostream>
#include <vector>

#include <boost/thread/thread.hpp>
#include <boost/smart_ptr.hpp>

#include "SetCommandTo.h"
#include "abstract/BsnsLogic.h"

/*
 * Test message for server:
 * curl -v -H "Content-Type: application/json" -X POST
 * -d '{"txid":12, "device":"shlagbaum_in", "command":"down",
 * "parameters":{"state":"open", "car":"present"} }'
 * http://localhost:8000
 */

namespace httpserver
{

using namespace rapidjson;

struct mg_serve_http_opts s_http_server_opts;

const char* kTypeNames[] =
    { "Null", "False", "True", "Object", "Array", "String", "Number" };

const std::string attrError("error"); // Опциональный атрибут, отменяет все остальные атрибуты
const std::string attrTxId("txid"); // Обязательный атрибут - номер транзакции
const std::string attrDev("device"); // Обязательный атрибут - имя абстрактного устройства
const std::string attrCmd("command"); // Обязательный атрибут - команда на устройство
const std::string attrPars("parameters"); // Опциональный атрибут - параметры команды


void cb_HttpServer(struct mg_connection* nc, int ev, void* p)
{
	struct http_message *hmsg = (struct http_message*) p;

	if (ev == MG_EV_HTTP_REQUEST)
	{
		std::cout << "httpServer::cb_HttpServer: HTTP Server received new data." << std::endl;

//		HOWTO: take remote address and port in mongoose
//		char* addr = inet_ntoa(nc->sa.sin.sin_addr); // client address
//		int port = nc->sa.sin.sin_port;

		nc->flags |= MG_F_SEND_AND_CLOSE;
		mg_printf(nc, "%s", "HTTP/1.1 200 OK\r\n\r\n\r\n");

		// Parse a JSON string into DOM.
	    std::vector< ASCII<>::Ch > jsonArray(hmsg->body.len+1, 0);
		memcpy(&jsonArray[0], hmsg->body.p, (hmsg->body.len+1) * sizeof(jsonArray[0]));
		jsonArray[hmsg->body.len] = 0;

		Document d;
		d.ParseInsitu(&jsonArray[0]);
		if (d.HasParseError() == true)
		{
			memcpy(&jsonArray[0], hmsg->body.p, (hmsg->body.len+1) * sizeof(jsonArray[0]));
			jsonArray[hmsg->body.len] = 0;

			// отправить назад сообщение, что json не распарсился
			std::stringstream error;
			error << "ERROR! httpServer::cb_HttpServer: JSON has wrong format: " << &jsonArray[0];
			setCommandTo::sendErrorToClient(error);

			std::cout << error.str() << std::endl;

			return;
		}


		std::cout << "httpServer::cb_HttpServer: JSON was parsed correctly." << std::endl;

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
				std::cout << error.str() << std::endl;
			}
			else
			{
				std::stringstream error;
				error << "Error has received but value isn't String";
				std::cout << error.str() << std::endl;
			}

			return;
		}

		if (d.HasMember(attrTxId.c_str()) == false)
		{
			// отправить сообщение, что txid не найден и выйти
			std::stringstream error;
			error << "ERROR! httpServer::cb_HttpServer: JSON attribute '" << attrTxId << "' not found.";
			setCommandTo::sendErrorToClient(error);

			std::cout << error.str() << std::endl;

			return;
		}

		if (d.HasMember(attrDev.c_str()) == false)
		{
			// отправить сообщение, что device не найден и выйти
			std::stringstream error;
			error << "ERROR! httpServer::cb_HttpServer: JSON attribute '" << attrDev << "' not found.";
			setCommandTo::sendErrorToClient(error);

			std::cout << error.str() << std::endl;

			return;
		}

		if (d.HasMember(attrCmd.c_str()) == false)
		{
			// отправить сообщение, что command не найден и выйти
			std::stringstream error;
			error << "ERROR! httpServer::cb_HttpServer: JSON attribute '" << attrCmd << "' not found.";
			setCommandTo::sendErrorToClient(error);

			std::cout << error.str() << std::endl;

			return;
		}

		Value valParams(kObjectType);
		if (d.HasMember(attrPars.c_str()) == true)
		{
			// Если опциональные параметры существуют, то добавить из документа
			std::cout << "httpServer::cb_HttpServer: JSON attribute '" << attrPars << "' has found." << std::endl;
			valParams = d[attrPars.c_str()];
		}

		Value& valTxId = d[attrTxId.c_str()];
		Value& valDevice = d[attrDev.c_str()];
		Value& valCommand = d[attrCmd.c_str()];

		if (valTxId.IsNumber() == false)
		{
			// отправить сообщение, что txid не число и выйти
			std::stringstream error;
			error << "ERROR! httpServer::cb_HttpServer: JSON attribute '" << attrTxId << "' isn't number type";
			setCommandTo::sendErrorToClient(error);

			std::cout << error.str() << std::endl;

			return;
		}

		if (valDevice.IsString() == false)
		{
			// отправить сообщение, что девайс не строка и выйти
			std::stringstream error;
			error << "ERROR! httpServer::cb_HttpServer: JSON attribute '" << attrDev << "' isn't string type";
			setCommandTo::sendErrorToClient(error);

			std::cout << error.str() << std::endl;

			return;
		}

		if (valCommand.IsString() == false)
		{
			// отправить назад сообщение, что команда не строка и выйти
			std::stringstream error;
			error << "ERROR! httpServer::cb_HttpServer: JSON attribute '" << attrCmd << "' isn't string type";
			setCommandTo::sendErrorToClient(error);

			std::cout << error.str() << std::endl;

			return;
		}

		if (valParams.IsObject() == false)
		{
			// отправить назад сообщение, что параметры не объект и выйти
			std::stringstream error;
			std::cout << "ERROR! httpServer::cb_HttpServer: JSON attribute '" << attrPars << "' isn't object type";
			setCommandTo::sendErrorToClient(error);

			std::cout << error.str() << std::endl;

			return;
		}

		uint32_t TxId = (uint32_t) valTxId.GetInt();
		std::string Device(valDevice.GetString(), valDevice.GetStringLength());
		std::string Command(valCommand.GetString(), valCommand.GetStringLength());

		std::cout << attrTxId << ": " << TxId << std::endl;
		std::cout << attrDev << ": " << Device << std::endl;
		std::cout << attrCmd << ": " << Command << std::endl;

		std::cout <<  attrPars << ": " << std::endl;

		// Stringify parameters
		StringBuffer strParams;
		Writer<StringBuffer> writerParams(strParams);
		valParams.Accept(writerParams);
		std::cout << strParams.GetString() << std::endl;

		for (Value::ConstMemberIterator itr = valParams.MemberBegin();
			itr != valParams.MemberEnd(); ++itr)
		{
			std::cout << "Type of parameter '" << itr->name.GetString()
					<< "' is " << kTypeNames[itr->value.GetType()] << std::endl;
		}

		std::cout << "httpServer::cb_HttpServer: JSON OK!" << std::endl;

		setCommandTo::Device(TxId, Device, Command, strParams.GetString(), "logic_bsns_layer");

	}

}

void httpServerThread()
{
	struct mg_mgr mgr;
	struct mg_connection *nc;

	mg_mgr_init(&mgr, NULL);
	nc = mg_bind(&mgr, "8000", cb_HttpServer);

	// Setup http server parameters
	mg_set_protocol_http_websocket(nc);
	s_http_server_opts.document_root = ".";
	s_http_server_opts.dav_document_root = ".";
	s_http_server_opts.enable_directory_listing = "no";

	// infinite web server cycle
	for(;;)
	{
		boost::this_thread::interruption_point();
		mg_mgr_poll(&mgr, 100);
	}

	mg_mgr_free(&mgr);
}

boost::thread startHttpServer()
{
	boost::thread thr(httpServerThread);

	return thr;
}

}



#endif /* HTTPSERVER_H_ */
