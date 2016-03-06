/*
 * presentationMain.cpp
 *
 *  Created on: 8 февр. 2016 г.
 *      Author: alex
 */

#include "HttpServer.h"
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

using namespace mythreadpool;
using namespace rapidjson;

volatile int HttpClient::s_exit_flag = 0;
boost::mutex HttpClient::HTTPconnectMutex;
volatile int HttpDevLayerClient::s_exit_flag = 0;
boost::mutex HttpDevLayerClient::HTTPconnectMutex;


int main()
{

	boost::thread thrHttpServer = httpserver::startHttpServer();

	CDeviceManager::deviceManagerFactory( settings::getDeviceConfig() );
	GlobalThreadPool::get();

	for (;;)
	{
		boost::this_thread::sleep(boost::posix_time::milliseconds(1000));
	}

	GlobalThreadPool::stop();
	CDeviceManager::destroyDeviceManager();

	return 0;
}

