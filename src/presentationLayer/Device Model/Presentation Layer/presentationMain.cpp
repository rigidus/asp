/*
 * presentationMain.cpp
 *
 *  Created on: 8 февр. 2016 г.
 *      Author: alex
 */

#include "HttpServer.h"
#include "devices/HttpClient.h"

#include <iostream>

#include <boost/any.hpp>
#include <boost/bind.hpp>
#include <boost/thread.hpp>
#include <Settings.h>
#include "GlobalThreadPool.h"
#include "CDeviceManager.h"
#include "SetCommandTo.h"

using namespace mythreadpool;
using namespace rapidjson;

volatile int HttpClient::s_exit_flag = 0;

// Abstract names
// full
const std::string shlagbaum1("shlagbaum_in");
const std::string shlagbaum2("shlagbaum_out");
const std::string printer1("printer");
const std::string photosensor1("pass_photosensor");
const std::string photosensor2("present_photosensor");
const std::string display1("display");
const std::string massstorage1("sd_card");
const std::string kkm1("kkm");

// common
const std::string AbstractShlagbaum::s_abstractName = "shlagbaum";
const std::string BsnsLogic::s_abstractName = "logic";

// concrete names
const std::string ShlagbaumPalka::s_concreteName = "shlagbaum palka";
const std::string HttpClient::s_concreteName = "logic_http";

// Test 1
void testCreateAndDestroy()
{

	CDeviceManager::deviceManagerFactory(CDeviceManager::TestingSet);

	std::string dev("shlagbaum_in");
	std::string cmd("command_from_json");
	std::string pars("pars_from_json");

	GlobalThreadPool::get();

	setCommandTo::Device(0, dev, cmd, pars, "TEST");

	GlobalThreadPool::stop();

	CDeviceManager::destroyDeviceManager();

}


int main()
{

	boost::thread thrHttpServer = httpserver::startHttpServer();

//  Test1 Создание и разрушение классов устройств, запуск и остановка их работы 100 раз в цикле
//  	При разных конфигурациях, в т.ч. с отсутствием устройств
//	for (int i=0; i < 100; ++i)	// for test of create and destroy
//	{
//		std::cout << "------------- start new instance ---------------- " <<  i << std::endl;
//
//		testCreateAndDestroy();
//	}

	CDeviceManager::deviceManagerFactory(CDeviceManager::TestingSet);
	GlobalThreadPool::get();

//	Test 2 Постановка в очередь на устройство 100 команд без задержки

	for (int i=0; i < 100; ++i)
	{
		setCommandTo::Client(setCommandTo::Event, "logic", "",
				"{\"txid\":12, \"device\":\"shlagbaum_in\", \"command\":\"down\", \"parameters\":{\"state\":\"open\", \"car\":\"present\"} }");
	}

	for (;;)
	{
		boost::this_thread::sleep(boost::posix_time::milliseconds(1000));
	}

	GlobalThreadPool::stop();
	CDeviceManager::destroyDeviceManager();

	return 0;
}

