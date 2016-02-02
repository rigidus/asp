
#include "TestIntegrator.h"

int main()
{

	CTestTcpIpMultiServer testTcpIpMultiServer;

	testTcpIpMultiServer.testConnection = new CTestTcpIpMultiServer::CTestConnection("class: CTcpIpMultiServer, test: CtestConnection");
	testTcpIpMultiServer.testSendReceive = new CTestTcpIpMultiServer::CTestSendReceive("class: CTcpIpMultiServer, test: CtestSendReceive");

	CppUnit::TextTestRunner runner;
	runner.addTest(testTcpIpMultiServer.testConnection);
	runner.addTest(testTcpIpMultiServer.testSendReceive);

	runner.run();

	std::ofstream outFile("testResult.xml");
	CppUnit::XmlOutputter outputer(&runner.result(), outFile);
	outputer.write();

}
