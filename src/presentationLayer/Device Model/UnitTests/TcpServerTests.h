#ifndef MULTITCPIPSERVERTEST
#define MULTITCPIPSERVERTEST

#define UNITTEST

#include <cppunit/TestCase.h>
#include <cppunit/ui/text/TextTestRunner.h>
#include <cppunit/XmlOutputterHook.h>
#include <cppunit/XmlOutputter.h>

#include <iostream>

#include <CTcpServerManager.h>
#include <CTcpClient.h>

class CTestTcpIpMultiServer
{

private:

	static char s_testData[];
	static condition_variable s_connect, s_read;
	static mutex s_mutex;
	static std::vector<uint8_t> s_resultData;

public: // callbacks

	/**
	 * Callback from CTcpConnectionListener when data received
	 *
	 * @param socket
	 * 			It's client socket
	 * @param data
	 * 			Received data
	 * @param clientName
	 * 			Name of the client
	 */
	static void DoReceive(asio::ip::tcp::socket&, std::vector<uint8_t>, std::string&);

	/**
	 * Callback from CTcpConnectionListener when client connected
	 *
	 * @param socket
	 * 			It's client socket
	 * @param clientName
	 * 			Name of the client
	 */
	static void DoConnect(asio::ip::tcp::socket&, std::string&);

	/**
	 * Callback from CTcpConnectionListener when client disconnected
	 *
	 * @param clientName
	 * 			Name of the client
	 */
	static void DoDisconnect(std::string&);

public:

	class CTestConnection: public CppUnit::TestCase
	{
	public:
		CTestConnection(std::string str):
			TestCase(str)
		{}

		virtual ~CTestConnection() {}

		virtual void runTest();
	};

	class CTestSendReceive: public CppUnit::TestCase
	{
	public:
		CTestSendReceive(std::string str):
			TestCase(str)
		{}

		virtual ~CTestSendReceive() {}

		virtual void runTest();
	};

	CTestConnection* testConnection;
	CTestSendReceive* testSendReceive;

};


#endif // MULTITCPIPSERVERTEST
