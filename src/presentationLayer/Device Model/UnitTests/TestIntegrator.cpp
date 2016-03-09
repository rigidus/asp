
#include "TestIntegrator.h"

int main()
{

	CTcpIpMultiServerTest testTcpIpMultiServer;
	CDevicesLayerTest testDevicesLayer;

	testTcpIpMultiServer.testConnection = new CTcpIpMultiServerTest::CTestConnection("class: CTcpIpMultiServer, test: CtestConnection");
	testTcpIpMultiServer.testSendReceive = new CTcpIpMultiServerTest::CTestSendReceive("class: CTcpIpMultiServer, test: CtestSendReceive");
	testDevicesLayer.testCreateDestroy = new CDevicesLayerTest::CTestCreateDestroyLoop("module: Devices Layer, test: Create and Destroy CDevicesManager");
	testDevicesLayer.testSendReceiveLoop = new CDevicesLayerTest::CTestSendReceiveLoop("module: Devices Layer, test: Send command to device and check answer");

	CppUnit::TextTestRunner runner;
	runner.addTest(testTcpIpMultiServer.testConnection);
	runner.addTest(testTcpIpMultiServer.testSendReceive);
	runner.addTest(testDevicesLayer.testCreateDestroy);
	runner.addTest(testDevicesLayer.testSendReceiveLoop);

	runner.run();

	std::ofstream outFile("testResult.xml");
	CppUnit::XmlOutputter outputer(&runner.result(), outFile);
	outputer.write();

}
