///////////////////////////////////////////////////////////
//  CTcpServer.h
//  Implementation of the Class CTcpServer
//  Created on:      20-џэт-2016 16:20:20
//  Original author: user-PC
///////////////////////////////////////////////////////////

#if !defined(EA_99964F95_BDA0_4486_95D0_C18060E08E90__INCLUDED_)
#define EA_99964F95_BDA0_4486_95D0_C18060E08E90__INCLUDED_

#include "CTcpServerManager.h"

public class CTcpServer
{

public:
	CTcpServer();
	virtual ~CTcpServer();
	CTcpServer(const CTcpServer& theCTcpServer);

	CTcpServer(boost:asio::io_service ioService, CTcpConnectionListener* listener, uint32_t localPort, uint32_t maxBufSize, uint32_t msgTimeout, uint32_t msgFragmentTimeout, uint32_t maxConnections);
	CTcpConnection* createNewConnection(CTcpSocket* tcpSocket);
	uint32_t getMaxConnections();
	void setMaxConnections(uint32_t maxConnections);
	void startServer();
	void stopServer();

private:
	uint32_t m_localPort;
	uint32_t m_maxBufSzie;
	uint32_t m_messageTimeout;
	uint32_t m_messageFragmentTimeout;
	CTcpConnectionListener* m_pConnectionListener;
	CTcpServerManager *m_CTcpServerManager;

};
#endif // !defined(EA_99964F95_BDA0_4486_95D0_C18060E08E90__INCLUDED_)
