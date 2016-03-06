/*
 * DevicesLayerTest.cpp
 *
 *  Created on: 5 марта 2016 г.
 *      Author: alex
 */

#include <HttpServer.h>

#include "devices/HttpClient.h"
#include "devices/HttpDevLayerClient.h"

#include <iostream>

#include <boost/any.hpp>
#include <boost/bind.hpp>
#include <boost/thread.hpp>
#include <Settings.h>
#include "GlobalThreadPool.h"
#include "CDeviceManager.h"
#include "SetCommandTo.h"

#include "../Devices Layer/CDeviceManager.cpp"
#include "../Devices Layer/SetCommandTo.cpp"
#include "../Devices Layer/devices/CBaseDevice.cpp"
#include "../Devices Layer/CBaseCommCtl.cpp"
#include "../Devices Layer/CPinCtl.cpp"
#include "../Devices Layer/CSerialPortCtl.cpp"
#include "../Devices Layer/CBaseCodec.cpp"
#include "../Devices Layer/CPinCodec.cpp"
#include "../Devices Layer/Settings.cpp"
#include "../Devices Layer/NameInstanses.cpp"

#include "DevicesLayerTests.h"
#include "DeviceLayerSettings.h"
volatile boost::atomic<uint32_t> CDevicesLayerTest::stopServer(0);
struct mg_serve_http_opts CDevicesLayerTest::s_http_server_opts;

using namespace mythreadpool;
using namespace rapidjson;

volatile int HttpClient::s_exit_flag = 0;
boost::mutex HttpClient::HTTPconnectMutex;
volatile int HttpDevLayerClient::s_exit_flag = 0;
boost::mutex HttpDevLayerClient::HTTPconnectMutex;

void CDevicesLayerTest::cb_HttpServer(struct mg_connection* nc, int ev, void* p)
{

	struct http_message *hmsg = (struct http_message*) p;

	if (ev == MG_EV_HTTP_REQUEST)
	{
		nc->flags |= MG_F_SEND_AND_CLOSE;
		mg_printf(nc, "%s", "HTTP/1.1 200 OK\r\n\r\n\r\n");

		std::string str(hmsg->body.p, &hmsg->body.p[hmsg->body.len]);
		std::cout << "CDevicesLayerTest::cb_HttpServer: Received: " << (char*) hmsg->body.p << std::endl;

		stopServer.fetch_add(1, boost::memory_order_relaxed);
	}
}


void CDevicesLayerTest::httpServerThread()
{
	struct mg_mgr mgr;
	struct mg_connection *nc;

	mg_mgr_init(&mgr, NULL);
	nc = mg_bind(&mgr, "8100", cb_HttpServer);

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

boost::thread CDevicesLayerTest::startHttpServer()
{
	boost::thread thr(httpServerThread);

	return thr;
}


const std::vector<settings::DeviceConfig> CDevicesLayerTest::getDeviceConfig()
{
	std::vector<settings::DeviceConfig> baseList;

	for (settings::DeviceConfig* v: test_device_config::deviceList)
	{
		baseList.push_back( *v );
	}

	return baseList;
}



// Test 1
void CDevicesLayerTest::CTestCreateDestroyLoop::runTest()
{

	std::cout << "Start test: " << getName() << std::endl;

	CDeviceManager::deviceManagerFactory( CDevicesLayerTest::getDeviceConfig() );

	GlobalThreadPool::get();

	boost::this_thread::sleep(boost::posix_time::milliseconds(100));

	GlobalThreadPool::stop();

	CDeviceManager::destroyDeviceManager();

}

//	Test 2 Постановка в очередь на устройство 100 команд без задержки
void CDevicesLayerTest::CTestSendReceiveLoop::runTest()
{

	std::cout << "Start test: " << getName() << std::endl;

	boost::thread thrHttpLogicLayerServer = CDevicesLayerTest::startHttpServer();
	boost::thread thrHttpDevLayerServer = httpserver::startHttpServer();

	CDeviceManager::deviceManagerFactory( CDevicesLayerTest::getDeviceConfig() );
	GlobalThreadPool::get();

	boost::this_thread::sleep(boost::posix_time::milliseconds(100));

	uint32_t i = 0;
	uint32_t maxCommands = 1;
	for (i=0; i < maxCommands; ++i)
	{
		std::stringstream strPar;

		strPar << "{\"txid\":" << i << ", \"device\":\"abstract_device\", \"command\":\"down\", \"parameters\":{\"state\":\"open\", \"car\":\"present\"} }";

		setCommandTo::Device(0, "logic_dev_layer", "", strPar.str(), "");
	}

	while (stopServer.load(boost::memory_order_relaxed) < maxCommands)
	{
		boost::this_thread::sleep(boost::posix_time::milliseconds(100));
	}

	thrHttpLogicLayerServer.interrupt();
	thrHttpLogicLayerServer.join();
	thrHttpDevLayerServer.interrupt();
	thrHttpDevLayerServer.join();

	GlobalThreadPool::stop();
	CDeviceManager::destroyDeviceManager();

}
