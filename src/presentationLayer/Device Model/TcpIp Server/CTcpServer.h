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

class CTcpServer: public noncopyable
{

public:
	CTcpServer(asio::io_service& ioService, CTcpConnectionListener* listener, uint32_t localPort);
	CTcpServer(asio::io_service& ioService, CTcpConnectionListener* listener, uint32_t localPort, uint32_t maxBufSize, uint32_t msgTimeout, uint32_t msgFragmentTimeout, uint32_t maxConnections);
	virtual ~CTcpServer();

	uint32_t getMaxConnections();
	void startServer();
	void stopServer();

private:

	asio::ip::tcp::acceptor* m_pAcceptor;
	asio::io_service& m_ioService;
	uint32_t m_localPort;
	uint32_t m_maxBufSize;
	uint32_t m_messageTimeout;
	uint32_t m_messageFragmentTimeout;
	uint32_t m_maxConnection;
	CTcpConnectionListener* m_pConnectionListener;
	CTcpSocketFactory* m_pSocketFactory;
	system::error_code m_lastErrorCode;

	void handle_accept(CTcpConnection::PtrCTcpConnection newConnection, const system::error_code& error);

};
#endif // !defined(EA_99964F95_BDA0_4486_95D0_C18060E08E90__INCLUDED_)
