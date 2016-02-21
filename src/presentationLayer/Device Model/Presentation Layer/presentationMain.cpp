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

#include "CDeviceManager.h"
#include "GlobalThreadPool.h"

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

void sendError2BL(std::string)
{
	// TODO: create send answer to businness logic
}

void cb_commandFromHttpClient(std::string jsonDoc)
{

	std::cout << jsonDoc << std::endl;

//	Document workDoc;
//	workDoc.Parse(jsonDoc.c_str());
//
//	CDeviceManager* pdm = CDeviceManager::getDeviceManager();
//	CAbstractDevice* pAbstractDev = nullptr;
//
//	if (workDoc.HasMember("txid") == false) sendError2BL("txid not found");
//
//	if (workDoc.HasMember("device") == false) sendError2BL("device not found");
//	else pAbstractDev = pdm->getAbstractDevice(workDoc["device"]);
//
//	if (workDoc.HasMember("command") == false) sendError2BL("command not found");
//
//	std::string pars("");
//	if (workDoc.HasMember("parameters") == true) pars = workDoc["parameters"];
//
//	std::string strDevice(workDoc["device"]);
//	std::string strCommand(workDoc["command"]);
//	if ( (strDevice == shlagbaum1) || (strDevice == shlagbaum2) )
//		g_thrPool.AddTask(0, boost::bind(sendCommand<AbstractShlagbaum>, pAbstractDev, strCommand, pars));

//	if ( (strDevice == printer1) )
//		g_thrPool.AddTask(0, boost::bind(sendCommand<AbstractPrinter>, pAbstractDev, strCommand, pars));
//
//	if ( (strDevice == photosensor1) )
//		g_thrPool.AddTask(0, boost::bind(sendCommand<AbstractPassSensor>, pAbstractDev, strCommand, pars));
//
//	if ( (strDevice == photosensor2) )
//		g_thrPool.AddTask(0, boost::bind(sendCommand<AbstractPresentSensor>, pAbstractDev, strCommand, pars));

	// TODO: Add tasks for other abstract devices

}


void testCreateAndDestroy()
{

	CDeviceManager* devManager = CDeviceManager::deviceManagerFactory(CDeviceManager::TestingSet);

	std::string dev("shlagbaum_in");
	std::string cmd("command_from_json");
	std::string pars("pars_from_json");

	GlobalThreadPool::get();

	if (devManager)
	{
		devManager->setCommandToDevice(dev, cmd, pars);
	}
	else
	{
		std::cout << "No instanced device manager found" << std::endl;
	}

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

//	CDeviceManager* devManager =
	CDeviceManager::deviceManagerFactory(CDeviceManager::TestingSet);
	GlobalThreadPool::get();

	for (;;)
	{
		boost::this_thread::sleep(boost::posix_time::milliseconds(1000));
	}

	return 0;
}

