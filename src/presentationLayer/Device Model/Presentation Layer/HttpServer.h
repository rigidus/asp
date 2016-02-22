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

#include "CDeviceManager.h"

namespace httpserver
{

using namespace rapidjson;

struct mg_serve_http_opts s_http_server_opts;

const char* kTypeNames[] =
    { "Null", "False", "True", "Object", "Array", "String", "Number" };

const std::string attrTxId("txid"); // Обязательный атрибут - номер транзакции
const std::string attrDev("device"); // Обязательный атрибут - имя абстрактного устройства
const std::string attrCmd("command"); // Обязательный атрибут - команда на устройство
const std::string attrPars("parameters"); // Опциональный атрибут - параметры команды

void cb_HttpServer(struct mg_connection* nc, int ev, void* p)
{
	struct http_message *hmsg = (struct http_message*) p;

	if (ev == MG_EV_HTTP_REQUEST)
	{
		nc->flags |= MG_F_CLOSE_IMMEDIATELY;
		char* addr = inet_ntoa(nc->sa.sin.sin_addr); // client address
		int port = nc->sa.sin.sin_port;

	    // Parse a JSON string into DOM.
	    std::vector< ASCII<>::Ch > jsonArray(hmsg->body.len+1, 0);
		memcpy(&jsonArray[0], hmsg->body.p, (hmsg->body.len+1) * sizeof(jsonArray[0]));
		jsonArray[hmsg->body.len] = 0;

		Document d;
		d.ParseInsitu(&jsonArray[0]);
		if (d.HasParseError() == false)
		{
			std::cout << "JSON was parsed correctly." << std::endl;

			// Stringify the DOM
			StringBuffer buffer;
			Writer<StringBuffer> writer(buffer);
			d.Accept(writer);
			std::cout << buffer.GetString() << std::endl;

			if (d.HasMember(attrTxId.c_str()) == false)
			{
				// TODO отправить сообщение, что txid не найден и выйти
				std::cout << "JSON attribute '" << attrTxId << "' not found" << std::endl;
				return;
			}

			if (d.HasMember(attrDev.c_str()) == false)
			{
				// TODO отправить сообщение, что device не найден и выйти
				std::cout << "JSON attribute '" << attrDev << "' not found" << std::endl;
				return;
			}

			if (d.HasMember("command") == false)
			{
				// TODO отправить сообщение, что txid не найден и выйти
				std::cout << "JSON attribute '" << attrCmd << "' not found" << std::endl;
				return;
			}

			Value valParams(kObjectType);
			if (d.HasMember(attrPars.c_str()) == true)
			{
				// Если опциональные параметры существуют, то добавить из документа
				std::cout << "JSON attribute '" << attrPars << "' has found" << std::endl;
				valParams = d[attrPars.c_str()];
			}

			Value& valTxId = d[attrTxId.c_str()];
			Value& valDevice = d[attrDev.c_str()];
			Value& valCommand = d[attrCmd.c_str()];

			if (valTxId.IsNumber() == false)
			{
				// TODO отправить сообщение, что txid не число и выйти
				std::cout << "JSON attribute '" << attrTxId << "' isn't number type" << std::endl;
				return;
			}

			if (valDevice.IsString() == false)
			{
				// TODO отправить сообщение, что девайс не строка и выйти
				std::cout << "JSON attribute '" << attrDev << "' isn't string type" << std::endl;
				return;
			}

			if (valCommand.IsString() == false)
			{
				// TODO отправить назад сообщение, что команда не строка и выйти
				std::cout << "JSON attribute '" << attrCmd << "' isn't string type" << std::endl;
				return;
			}

			if (valParams.IsObject() == false)
			{
				// TODO отправить назад сообщение, что параметры не объект и выйти
				std::cout << "JSON attribute '" << attrPars << "' isn't object type" << std::endl;
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

			std::cout << "JSON OK!" << std::endl;

			CDeviceManager* devMgr = CDeviceManager::deviceManager();
			if (devMgr)
			{
				std::cout << "Set task for device" << std::endl;
				devMgr->setCommandToDevice(TxId, Device, Command, strParams.GetString());
			}
			else
			{
				// TODO отправить назад сообщение, что устройства еще не настроены
				std::cout << "Device manager not found." << std::endl;
			}
		}
		else
		{
			memcpy(&jsonArray[0], hmsg->body.p, (hmsg->body.len+1) * sizeof(jsonArray[0]));
			jsonArray[hmsg->body.len] = 0;

			// TODO отправить назад сообщение, что json не распарсился
			std::cout << "JSON has wrong format: " << &jsonArray[0] << std::endl;
		}

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

	std::cout << "Starting web server" << std::endl;

	// infinite web server cycle
	for(;;)
	{
		boost::this_thread::interruption_point();
		mg_mgr_poll(&mgr, 1000);
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
