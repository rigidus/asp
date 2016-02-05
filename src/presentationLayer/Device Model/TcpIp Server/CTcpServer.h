///////////////////////////////////////////////////////////
//  CTcpServer.h
//  Implementation of the Class CTcpServer
//  Created on:      20-���-2016 16:20:20
//  Original author: user-PC
///////////////////////////////////////////////////////////

#if !defined(EA_99964F95_BDA0_4486_95D0_C18060E08E90__INCLUDED_)
#define EA_99964F95_BDA0_4486_95D0_C18060E08E90__INCLUDED_

#include "boost_include.h"
using namespace boost;

#include "CTcpConnection.h"
#include "../include/FileLog.h"

class CTcpConnectionListener;

/*
 * Asynchronous TCP/IP server
 */

class CTcpServer: public noncopyable
{

public:
	CTcpServer(CTcpConnectionListener& listener, uint32_t localPort);
	CTcpServer(CTcpConnectionListener& listener, uint32_t localPort, uint32_t maxBufSize, uint32_t msgTimeout, uint32_t msgFragmentTimeout, uint32_t maxConnections);
	virtual ~CTcpServer();

	uint32_t getMaxConnections();

	// Start async accepting that is 1th step io_service server cycle
	void startServer();

	// Close LISTEN and all established connections
	void stopServer();

	// Function sends data frame to client on the connection
	// This is the TEST function
	// In real need to use connection class for send data
	void send(uint32_t clientNum, std::list<std::vector<uint8_t> >& data);

	asio::io_service& ioService();

private:

	asio::io_service m_ioService;
	uint32_t m_localPort;
	uint32_t m_maxBufSize;
	uint32_t m_messageTimeout;
	uint32_t m_messageFragmentTimeout;
	uint32_t m_maxConnection;
	CTcpConnectionListener& m_ConnectionListener;
	asio::ip::tcp::acceptor m_acceptor;

	std::vector<CTcpConnection::PtrCTcpConnection> m_connections;
	void handle_accept(CTcpConnection::PtrCTcpConnection newConnection, const system::error_code& error);

};
#endif // !defined(EA_99964F95_BDA0_4486_95D0_C18060E08E90__INCLUDED_)
