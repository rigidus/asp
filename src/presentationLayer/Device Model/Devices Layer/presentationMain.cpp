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

	sigset_t sigset;
	int32_t sig;

	sigemptyset(&sigset);
	sigaddset(&sigset, SIGINT);
	sigaddset(&sigset, SIGQUIT);
	sigaddset(&sigset, SIGTERM);
	sigaddset(&sigset, SIGTSTP);

	const int32_t signalReloadDeviceConfig = SIGRTMIN;
	sigaddset(&sigset, signalReloadDeviceConfig);	// This is reload device config signal

	for(;;)
	{

		sigprocmask(SIG_BLOCK, &sigset, NULL);
		sigwait(&sigset, &sig);

		if ( sig == signalReloadDeviceConfig)
		{
			std::cout << "Main: Catched signal to reload device config" << std::endl;
			GlobalThreadPool::stop();
			CDeviceManager::destroyDeviceManager();
			CDeviceManager::deviceManagerFactory( settings::getDeviceConfig() );
			GlobalThreadPool::get();
			continue;
		}

		switch(sig)
		{
		case SIGINT:
		case SIGQUIT:
		case SIGTERM:
		case SIGTSTP:
			std::cout << "Main: Catched signal to stopping application" << std::endl;
			GlobalThreadPool::stop();
			CDeviceManager::destroyDeviceManager();
			return 0;

		default:
			std::cout << "ERROR! Main: Catched not registered signal " << sig << std::endl;
			break;
		}
	}

	return 0;
}

