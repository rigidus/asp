/*
 * presentationMain.cpp
 *
 *  Created on: 8 февр. 2016 г.
 *      Author: alex
 */

#include "HttpServer.h"

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

// concrete names
const std::string ShlagbaumPalka::s_concreteName = "shlagbaum palka";
const std::string ClientHttp::s_concreteName = "";

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

//  Test1 in the future
	for (int i=0; i<10; ++i)	// for test of create and destroy
	{
		std::cout << "------------- start new instance ---------------- " <<  i << std::endl;

		testCreateAndDestroy();
	}

	CDeviceManager::deviceManagerFactory(CDeviceManager::TestingSet);
	GlobalThreadPool::get();

	for (;;)
	{
		boost::this_thread::sleep(boost::posix_time::milliseconds(1000));
	}

	return 0;
}

