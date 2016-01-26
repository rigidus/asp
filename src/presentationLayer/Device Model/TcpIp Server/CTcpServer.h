///////////////////////////////////////////////////////////
//  CTcpServer.h
//  Implementation of the Class CTcpServer
//  Created on:      20-���-2016 16:20:20
//  Original author: user-PC
///////////////////////////////////////////////////////////

#if !defined(EA_99964F95_BDA0_4486_95D0_C18060E08E90__INCLUDED_)
#define EA_99964F95_BDA0_4486_95D0_C18060E08E90__INCLUDED_

#include <asio.hpp>
using namespace boost;

#include "CTcpConnection.h"
#include "CTcpSocketFactory.h"

class CTcpConnectionListener;

class CTcpServer
{

public:
	virtual ~CTcpServer();
	CTcpServer(const CTcpServer& rhs);
	CTcpServer(asio::io_service& ioService, CTcpConnectionListener* listener, uint32_t localPort, uint32_t maxBufSize, uint32_t msgTimeout, uint32_t msgFragmentTimeout, uint32_t maxConnections);

	CTcpConnection* createNewConnection(asio::ip::tcp::socket* tcpSocket);
	uint32_t getMaxConnections();
	void setMaxConnections(uint32_t maxConnections);
	void startServer();
	void stopServer();

private:
	uint32_t m_localPort;
	uint32_t m_maxBufSize;
	uint32_t m_messageTimeout;
	uint32_t m_messageFragmentTimeout;
	CTcpConnectionListener* m_pConnectionListener;
	CTcpSocketFactory* m_pSocketFactory;

};
#endif // !defined(EA_99964F95_BDA0_4486_95D0_C18060E08E90__INCLUDED_)
