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

static const char* kTypeNames[] =
    { "Null", "False", "True", "Object", "Array", "String", "Number" };

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
			std::cout << "JSON OK!" << std::endl;

			// Stringify the DOM
			StringBuffer buffer;
			Writer<StringBuffer> writer(buffer);
			d.Accept(writer);
			std::cout << buffer.GetString() << std::endl;

			Value& valDevice = d["device"];
			Value& valCommand = d["command"];

			if (d.HasMember("parameters") == false)
			{
				 d["parameters"] = "{}";
			}
			Value& valParams = d["parameters"];

			if (valDevice.IsString() == false)
			{
				// TODO отправить сообщение, что девайс не строка и выйти
			}

			if (valCommand.IsString() == false)
			{
				// TODO отправить назад сообщение, что команда не строка и выйти
			}

			if (valParams.IsObject() == false)
			{
				// TODO отправить назад сообщение, что параметры не объект и выйти
			}

			std::string Device(valDevice.GetString(), valDevice.GetStringLength());
			std::string Command(valCommand.GetString(), valCommand.GetStringLength());

			std::cout << "Device: " << Device << std::endl;
			std::cout << "Command: " << Command << std::endl;

			std::cout << "Parameters: " << std::endl;

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

			CDeviceManager* devMgr = CDeviceManager::deviceManager();
			if (devMgr)
			{
				devMgr->setCommandToDevice(Device, Command, strParams.GetString());
			}
		}
		else
		{
			memcpy(&jsonArray[0], hmsg->body.p, (hmsg->body.len+1) * sizeof(jsonArray[0]));
			jsonArray[hmsg->body.len] = 0;

			std::cout << "JSON has wrong format" << std::endl;
			std::cout << &jsonArray[0] << std::endl;

			// TODO отправить назад сообщение, что json не распарсился

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
