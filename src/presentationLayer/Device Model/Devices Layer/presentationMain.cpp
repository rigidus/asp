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
#include "Logger.h"
#include "Help.h"

using namespace mythreadpool;
using namespace rapidjson;

volatile int HttpClient::s_exit_flag = 0;
boost::mutex HttpClient::HTTPconnectMutex;
volatile int HttpDevLayerClient::s_exit_flag = 0;
boost::mutex HttpDevLayerClient::HTTPconnectMutex;

void ParameterParser(int argc, char* argv[])
{
	uint32_t cnt = argc;
	uint32_t index = 1;
	while (cnt > 1)
	{
		int32_t len = 0;

		len = Help::Parser(argv[index], argv[index+1]);
		if (len)
			exit(0);

		len = Logger::LogParamParser(argv[index], argv[index+1]);

//		if (len == 0)
//			len == // call next param parser

		if (len == 0)
			len = 1;

		index += len;
		cnt -= len;
	}

}

int main(int argc, char* argv[])
{

	ParameterParser(argc, argv);

#ifdef NDEBUG
	Logger::initReleaseLogging();
#else
	Logger::initDebugLogging();
#endif

//  Log examples. It's placed temporary here
//	SetTo::CommonLog(debug, "Debug Test");
//	SetTo::LocalLog("printer" , critical, "Car income");
//	SetTo::CommonLog(info, "Info Test");
//	SetTo::LocalLog("printer", trace, "Function end");
//	SetTo::CommonLog(warning, "Warning Test");
//	SetTo::LocalLog("shlagbaum_in", info, "Car registered");
//	SetTo::CommonLog(trace, "Trace Test");
//	SetTo::LocalLog("shlagbaum_in", critical, "Stack overflow");
//	SetTo::CommonLog(critical, "Critical Test");
// End Log examples

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
			{
				std::stringstream log;
				log << "Main: Catched signal to reload device config";
				SetTo::CommonLog(info, log.str());
			}
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
			{
				std::stringstream log;
				log << "Main: Catched signal to stopping application";
				SetTo::CommonLog(info, log.str());
			}
			GlobalThreadPool::stop();
			CDeviceManager::destroyDeviceManager();
			return 0;

		default:
			{
				std::stringstream log;
				log << "ERROR! Main: Catched not registered signal " << sig;
				SetTo::CommonLog(error, log.str());
			}
			break;
		}
	}

	return 0;
}

