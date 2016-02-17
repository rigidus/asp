/*
 * presentationMain.cpp
 *
 *  Created on: 8 февр. 2016 г.
 *      Author: alex
 */

#include <iostream>

#include <boost/any.hpp>
#include <boost/bind.hpp>

#include "rapidjson/document.h"
#include "rapidjson/writer.h"
#include "rapidjson/stringbuffer.h"

#include "CSettings.h"
#include "CBaseDevice.h"
#include "MyThreadPool.h"
#include "CDeviceManager.h"
#include "TestDevices.h"
#include "CBaseDeviceFactory.h"
#include "GlobalThreadPool.h"
#include "curlAdapter.h"

using namespace mythreadpool;
using namespace rapidjson;
using namespace devices;

const std::string shlagbaum1("shlagbaum_in");
const std::string shlagbaum2("shlagbaum_out");
const std::string printer1("printer");
const std::string photosensor1("pass_photosensor");
const std::string photosensor2("present_photosensor");
const std::string display1("display");
const std::string massstorage1("sd_card");
const std::string kkm1("kkm");

void sendError2BL(std::string)
{
	// TODO: create send answer to businness logic
}

template<typename T>
static void sendCommand(CAbstractDevice* iDev, std::string command, std::string pars)
{
	iDev->sendCommand(command, pars);
}

void cb_commandFromBL(std::string jsonDoc)
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

template<typename T>
void printdev(T v)
{
//	static_assert(false, "Error");
}

template<>
void printdev<database::CSettings::DeviceConfig>(database::CSettings::DeviceConfig v)
{
	std::cout << "abstract DeviceConfig" << std::endl;
	std::cout << v.abstractName << std::endl;//	for (int i=0; i<10; ++i)	// for test of create and destroy
	//	{
	//		std::cout << "------------- start new instance ---------------- " <<  i << std::endl;
	//
	//		testAndDestroy();
	//	}


	if (v.proto.size() != 0) std::cout << " " << v.proto << std::endl;

	for (auto comm: v.comm)
		std::cout << "  " << comm << std::endl;
}

void testAndDestroy()
{
	struct DeviceCtl
	{
		boost::shared_ptr<CAbstractDevice> devInstance;

		struct Task
		{
			uint32_t txId;
			mythreadpool::TTaskFunc taskFn;
		};

		std::queue<Task> taskQue;
	};

	std::vector<DeviceCtl> devices;

	database::CSettings sets;
	std::vector<database::CSettings::DeviceConfig> devList = sets.getDeviceConfig();

	CBaseDeviceFactory&	factory = CBaseDeviceFactory::getFactory();

	for (auto v: devList)
	{
		if (v.abstractName.size() == 0) continue;
		if (v.enable == false) continue;

		std::cout << v.abstractName << std::endl;

		if (v.proto.size() != 0) std::cout << " " << v.proto << std::endl;

		DeviceCtl devCtl;
		boost::shared_ptr<CAbstractDevice> sPtr( factory.deviceFactory(v.abstractName, v.concreteName) );
		devCtl.devInstance = sPtr;

		if (sPtr.get() != nullptr)
			devices.push_back(devCtl);

	}

	if (devices.size())
	{
		std::string cmd("command_from_json");
		std::string pars("pars_from_json");

		std::cout << "Set task for device0 to thread pool" << std::endl;

		GlobalThreadPool::get().AddTask(0, boost::bind(sendCommand<AbstractShlagbaum>, devices[0].devInstance.get(), cmd, pars));

		boost::this_thread::sleep(boost::posix_time::microseconds(1000));

	}
	else
	{
		std::cout << "No instanced device found" << std::endl;
	}

	GlobalThreadPool::stop();
}


int main()
{

//  Test1 in the future
//	for (int i=0; i<10; ++i)	// for test of create and destroy
//	{
//		std::cout << "------------- start new instance ---------------- " <<  i << std::endl;
//
//		testAndDestroy();
//	}

	CurlAdapter webServer;
	if (webServer.CurlStart(cb_commandFromBL) == false) std::cout << "False start" << std::endl;
	if (webServer.AddRequest("") == false) std::cout << "False add request" << std::endl;
	webServer.Update();

	boost::this_thread::sleep(boost::posix_time::milliseconds(5000));

	return 0;
}

