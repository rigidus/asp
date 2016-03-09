#include "TcpServerTests.h"

void CTcpIpMultiServerTest::DoReceive(
		asio::ip::tcp::socket& socket,
		uint8_t* data,
		std::size_t rcvSize,
		std::string& clientName)
{

	CFileLog::cfilelog() << "CTestTcpIpMultiServer::DoReceive size=" << rcvSize << std::endl;

	s_resultData.clear();
	s_resultData.reserve(rcvSize);
	for (std::size_t i = 0; i < rcvSize; ++i)
		s_resultData.push_back(data[i]);

	s_read.notify_one();

}


void CTcpIpMultiServerTest::DoConnect(asio::ip::tcp::socket& socket, std::string& clientName) {

	CFileLog::cfilelog() << "CTestTcpIpMultiServer::DoConnect" << std::endl;

	s_connect.notify_one();

}


void CTcpIpMultiServerTest::DoDisconnect(std::string& clientName) {

	CFileLog::cfilelog() << "CTestTcpIpMultiServer::DoDisconnect" << std::endl;

}

static void send_message(CTcpClient* client, std::vector<uint8_t>& data)
{
	CFileLog::cfilelog() << "static send_message" << std::endl;

	// delay to enter condition_variable control: for sync test
	this_thread::sleep(posix_time::milliseconds(2));

	client->send_message(data);
}

char CTcpIpMultiServerTest::s_testData[] = {
						(char) 0x00, (char) 0x01, (char) 0x02, (char) 0x03, (char) 0x04, (char) 0x05, (char) 0x06,
						(char) 0x07, (char) 0x08, (char) 0x09, (char) 0x0a, (char) 0x0b, (char) 0x0c, (char) 0x0d, (char) 0x0e,
						(char) 0x0f, (char) 0x00, (char) 0x01, (char) 0x02, (char) 0x03, (char) 0x04, (char) 0x05, (char) 0x06,
						(char) 0x07, (char) 0x08, (char) 0x09, (char) 0x0a, (char) 0x0b, (char) 0x0c, (char) 0x0d, (char) 0x0e,
						(char) 0x0f, (char) 0x00, (char) 0x01, (char) 0x02, (char) 0x03, (char) 0x04, (char) 0x05, (char) 0x06,
						(char) 0x07, (char) 0x08, (char) 0x09, (char) 0x0a, (char) 0x0b, (char) 0x0c, (char) 0x0d, (char) 0x0e,
						(char) 0x0f, (char) 0x00, (char) 0x01, (char) 0x02, (char) 0x03, (char) 0x04, (char) 0x05, (char) 0x06,
						(char) 0x07, (char) 0x08, (char) 0x09, (char) 0x0a, (char) 0x0b, (char) 0x0c, (char) 0x0d, (char) 0x0e,
						(char) 0x0f, (char) 0x00, (char) 0x01, (char) 0x02, (char) 0x03, (char) 0x04, (char) 0x05, (char) 0x06,
						(char) 0x07, (char) 0x08, (char) 0x09, (char) 0x0a, (char) 0x0b, (char) 0x0c, (char) 0x0d, (char) 0x0e,
						(char) 0x0f, (char) 0x00, (char) 0x01, (char) 0x02, (char) 0x03, (char) 0x04, (char) 0x05, (char) 0x06,
						(char) 0x07, (char) 0x08, (char) 0x09, (char) 0x0a, (char) 0x0b, (char) 0x0c, (char) 0x0d, (char) 0x0e,
						(char) 0x0f, (char) 0x00, (char) 0x01, (char) 0x02, (char) 0x03, (char) 0x04, (char) 0x05, (char) 0x06,
						(char) 0x07, (char) 0x08, (char) 0x09, (char) 0x0a, (char) 0x0b, (char) 0x0c, (char) 0x0d, (char) 0x0e,
						(char) 0x0f, (char) 0x00, (char) 0x01, (char) 0x02, (char) 0x03, (char) 0x04, (char) 0x05, (char) 0x06,
						(char) 0x07, (char) 0x08, (char) 0x09, (char) 0x0a, (char) 0x0b, (char) 0x0c, (char) 0x0d, (char) 0x0e,
						(char) 0x0f, (char) 0x00, (char) 0x01, (char) 0x02, (char) 0x03, (char) 0x04, (char) 0x05, (char) 0x06,
						(char) 0x07, (char) 0x08, (char) 0x09, (char) 0x0a, (char) 0x0b, (char) 0x0c, (char) 0x0d, (char) 0x0e,
						(char) 0x0f, (char) 0x00, (char) 0x01, (char) 0x02, (char) 0x03, (char) 0x04, (char) 0x05, (char) 0x06,
						(char) 0x07, (char) 0x08, (char) 0x09, (char) 0x0a, (char) 0x0b, (char) 0x0c, (char) 0x0d, (char) 0x0e,
						(char) 0x0f, (char) 0x00, (char) 0x01, (char) 0x02, (char) 0x03, (char) 0x04, (char) 0x05, (char) 0x06,
						(char) 0x07, (char) 0x08, (char) 0x09, (char) 0x0a, (char) 0x0b, (char) 0x0c, (char) 0x0d, (char) 0x0e,
						(char) 0x0f, (char) 0x00, (char) 0x01, (char) 0x02, (char) 0x03, (char) 0x04, (char) 0x05, (char) 0x06,
						(char) 0x07, (char) 0x08, (char) 0x09, (char) 0x0a, (char) 0x0b, (char) 0x0c, (char) 0x0d, (char) 0x0e,
						(char) 0x0f, (char) 0x00, (char) 0x01, (char) 0x02, (char) 0x03, (char) 0x04, (char) 0x05, (char) 0x06,
						(char) 0x07, (char) 0x08, (char) 0x09, (char) 0x0a, (char) 0x0b, (char) 0x0c, (char) 0x0d, (char) 0x0e,
						(char) 0x0f, (char) 0x00, (char) 0x01, (char) 0x02, (char) 0x03, (char) 0x04, (char) 0x05, (char) 0x06,
						(char) 0x07, (char) 0x08, (char) 0x09, (char) 0x0a, (char) 0x0b, (char) 0x0c, (char) 0x0d, (char) 0x0e,
						(char) 0x0f, (char) 0x00, (char) 0x01, (char) 0x02, (char) 0x03, (char) 0x04, (char) 0x05, (char) 0x06,
						(char) 0x07, (char) 0x08, (char) 0x09, (char) 0x0a, (char) 0x0b, (char) 0x0c, (char) 0x0d, (char) 0x0e,
						(char) 0x0f, (char) 0x00, (char) 0x01, (char) 0x02, (char) 0x03, (char) 0x04, (char) 0x05, (char) 0x06,
						(char) 0x07, (char) 0x08, (char) 0x09, (char) 0x0a, (char) 0x0b, (char) 0x0c, (char) 0x0d, (char) 0x0e,
						(char) 0x0f, (char) 0x00, (char) 0x01, (char) 0x02, (char) 0x03, (char) 0x04, (char) 0x05, (char) 0x06,
						(char) 0x07, (char) 0x08, (char) 0x09, (char) 0x0a, (char) 0x0b, (char) 0x0c, (char) 0x0d, (char) 0x0e,
						(char) 0x0f, (char) 0x00, (char) 0x01, (char) 0x02, (char) 0x03, (char) 0x04, (char) 0x05, (char) 0x06,
						(char) 0x07, (char) 0x08, (char) 0x09, (char) 0x0a, (char) 0x0b, (char) 0x0c, (char) 0x0d, (char) 0x0e,
						(char) 0x0f
};

std::vector<uint8_t> CTcpIpMultiServerTest::s_resultData;
condition_variable CTcpIpMultiServerTest::s_connect;
condition_variable CTcpIpMultiServerTest::s_read;
mutex CTcpIpMultiServerTest::s_mutex;
mutex CTcpIpMultiServerTest::s_mutex2;

void CTcpIpMultiServerTest::CTestConnection::runTest() {

	CFileLog::cfilelog() << "---" << std::endl;
	CFileLog::cfilelog() << "CTestTcpIpMultiServer::CTestConnection started" << std::endl;

	CTcpServerManager serverMgr;

	CTcpConnectionListener listener;
	listener.setListenerFunctions(
		CTcpIpMultiServerTest::DoReceive,
		CTcpIpMultiServerTest::DoConnect,
		CTcpIpMultiServerTest::DoDisconnect);

	serverMgr.createServer("127.0.0.1", 20500, listener);

	unique_lock<mutex> lock(s_mutex);

	CTcpClient* client = CTcpClient::clientFactory("127.0.0.1", 20500);

	boost::thread clientThr(&CTcpClient::startClient, client);

	if (s_connect.do_wait_for(lock, {10,0}) == true)
	{
		CPPUNIT_ASSERT_EQUAL_MESSAGE("CTestTcpIpMultiServer::CTestConnection WRONG", true, client->is_connected());

		this_thread::sleep(posix_time::microseconds(500));
	}
	else
	{
		CPPUNIT_ASSERT_EQUAL_MESSAGE("CTestTcpIpMultiServer::CTestConnection timed out", 0, 1);
	}

	client->stop();
	clientThr.join();
	delete client;

//	client.Disconnect();
}

void CTcpIpMultiServerTest::CTestSendReceive::runTest() {

	CFileLog::cfilelog() << "---" << std::endl;
	CFileLog::cfilelog() << "CTestTcpIpMultiServer::CTestSendReceive started " << std::endl;

	CTcpServerManager serverMgr;

	CTcpConnectionListener listener;
	listener.setListenerFunctions(
		CTcpIpMultiServerTest::DoReceive,
		CTcpIpMultiServerTest::DoConnect,
		CTcpIpMultiServerTest::DoDisconnect);

	CTcpServer* server = serverMgr.createServer("127.0.0.1", 20500, listener);

	unique_lock<mutex> lock(s_mutex);

	CTcpClient* client = CTcpClient::clientFactory("127.0.0.1", 20500);

	boost::thread clientThr(&CTcpClient::startClient, client);

	if (s_connect.do_wait_for(lock, {5,0}) == true)
	{
		CPPUNIT_ASSERT_EQUAL_MESSAGE("CTestTcpIpMultiServer::CTestConnection WRONG", true, client->is_connected());

		this_thread::sleep(posix_time::microseconds(500));
	}
	else
	{
		CPPUNIT_ASSERT_EQUAL_MESSAGE("CTestTcpIpMultiServer::CTestConnection timed out", 0, 1);
	}

	{
		std::vector<uint8_t> expectedData;

		for (auto v: s_testData)
			expectedData.push_back(v);

		unique_lock<mutex> lockSendRcv(s_mutex2);

		boost::thread sendthr(send_message, client, expectedData);

		if (s_read.do_wait_for(lockSendRcv, {10,0} ) == false)
		{
			CPPUNIT_ASSERT_EQUAL_MESSAGE("CTestTcpIpMultiServer::CTestSendReceive wait data timed out", 0, 1);
		}

		sendthr.join();

		CPPUNIT_ASSERT_EQUAL_MESSAGE("CTestTcpIpMultiServer::CTestSendReceive expected length != result length",
				expectedData.size(), CTcpIpMultiServerTest::s_resultData.size());

		uint32_t size = expectedData.size();
		for (uint32_t i = 0; i < size; ++i) {
			CPPUNIT_ASSERT_EQUAL_MESSAGE("CTestTcpIpMultiServer::CTestSendReceive receive data WRONG",
				expectedData[i], CTcpIpMultiServerTest::s_resultData[i]);
		}

		std::list<std::vector<uint8_t> > sendData;
		sendData.push_back(s_resultData);
		server->send(0, sendData);

	}

	client->stop();
	clientThr.join();
	delete client;
}
