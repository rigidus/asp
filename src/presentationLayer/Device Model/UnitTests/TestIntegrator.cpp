
#include "TestIntegrator.h"

int main()
{

	CTestTcpIpMultiServer testTcpIpMultiServer;

	CppUnit::TextTestRunner runner;
	runner.addTest(&testTcpIpMultiServer.testConnection);
	runner.addTest(&testTcpIpMultiServer.testSendReceive);

	runner.run();

	std::ofstream outFile("testResult.xml");
	CppUnit::XmlOutputter outputer(&runner.result(), outFile);
	outputer.write();

}
