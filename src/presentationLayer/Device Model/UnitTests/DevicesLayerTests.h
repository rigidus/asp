/*
 * DevicesLayerTests.h
 *
 *  Created on: 5 марта 2016 г.
 *      Author: alex
 */

#ifndef DEVICESLAYERTESTS_H_
#define DEVICESLAYERTESTS_H_


#define UNITTEST

#include <cppunit/TestCase.h>
#include <cppunit/ui/text/TextTestRunner.h>
#include <cppunit/XmlOutputterHook.h>
#include <cppunit/XmlOutputter.h>

#include <iostream>

#include <CTcpServerManager.h>
#include <CTcpClient.h>
#include <Settings.h>

#include <boost/intrusive_ptr.hpp>
#include <boost/atomic.hpp>

class CDevicesLayerTest
{

private:

	static struct mg_serve_http_opts s_http_server_opts;

	static void httpServerThread();
	static boost::thread startHttpServer();
	static void cb_HttpServer(struct mg_connection* nc, int ev, void* p);
	static volatile boost::atomic<uint32_t> stopServer;
	static const std::vector<settings::DeviceConfig> getDeviceConfig();

public:

	class CTestCreateDestroyLoop: public CppUnit::TestCase
	{
	public:
		CTestCreateDestroyLoop(std::string str):
			TestCase(str)
		{}

		virtual ~CTestCreateDestroyLoop() {}

		virtual void runTest();
	};

	class CTestSendReceiveLoop: public CppUnit::TestCase
	{
	public:
		CTestSendReceiveLoop(std::string str):
			TestCase(str)
		{}

		virtual ~CTestSendReceiveLoop() {}

		virtual void runTest();
	};

	CTestCreateDestroyLoop* testCreateDestroy;
	CTestSendReceiveLoop* testSendReceiveLoop;

};


#endif /* DEVICESLAYERTESTS_H_ */
