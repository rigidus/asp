///////////////////////////////////////////////////////////
//  CTcpServer.cpp
//  Implementation of the Class CTcpServer
//  Created on:      20-���-2016 16:20:20
//  Original author: user-PC
///////////////////////////////////////////////////////////

#include "CTcpServer.h"

using namespace boost::asio::ip;


CTcpServer::CTcpServer(asio::io_service& ioService, CTcpConnectionListener* listener, uint32_t localPort, uint32_t maxBufSize, uint32_t msgTimeout, uint32_t msgFragmentTimeout, uint32_t maxConnections):
				m_ioService(ioService),
				m_localPort(localPort),
				m_maxBufSize(maxBufSize),
				m_messageTimeout(msgTimeout),
				m_messageFragmentTimeout(msgFragmentTimeout),
				m_maxConnection(maxConnections),
				m_pConnectionListener(listener),
				m_lastErrorCode(0)
{
	CFileLog::cfilelog() << "create CTcpServer, port: " << localPort << std::endl;

	m_pSocketFactory = CTcpSocketFactory::getSocketFactory();

	m_pAcceptor = new tcp::acceptor(ioService, tcp::endpoint(tcp::v4(), localPort));

	CFileLog::cfilelog() << "CTcpServer, port: " << localPort << ", created" << std::endl;
}


CTcpServer::CTcpServer(asio::io_service& ioService, CTcpConnectionListener* listener, uint32_t localPort):
				m_ioService(ioService),
				m_localPort(localPort),
				m_maxBufSize(32768),	// default buffer size
				m_messageTimeout(10000),	// default value
				m_messageFragmentTimeout(1000), // default value
				m_maxConnection(1000),		// default maximum connection number
				m_pConnectionListener(listener),
				m_lastErrorCode(0)
{
	CFileLog::cfilelog() << "create CTcpServer, port: " << localPort << std::endl;

	m_pSocketFactory = CTcpSocketFactory::getSocketFactory();

	m_pAcceptor = new tcp::acceptor(ioService, tcp::endpoint(tcp::v4(), localPort));

	CFileLog::cfilelog() << "CTcpServer, port: " << localPort << ", created" << std::endl;
}

CTcpServer::~CTcpServer()
{
	CFileLog::cfilelog() << "CTcpServer, port: " << m_localPort << ", deleted" << std::endl;
}


uint32_t CTcpServer::getMaxConnections(){

	CFileLog::cfilelog() << "CTcpServer::getMaxConnections" << std::endl;

	return m_maxConnection;

}


void CTcpServer::startServer(){

	CFileLog::cfilelog() << "CTcpServer::startServer" << std::endl;

	if (m_pAcceptor->is_open() == true) return;

//	m_pAcceptor->listen(m_maxConnection, m_lastErrorCode);

	CTcpConnection::PtrCTcpConnection = CTcpConnection::createNewConnection(
			m_ioService,
			m_pConnectionListener,
			m_maxBufSize,
			m_messageTimeout,
			m_messageFragmentTimeout);

	m_pAcceptor->async_accept()

}


void CTcpServer::stopServer(){

	CFileLog::cfilelog() << "CTcpServer::stopServer" << std::endl;

	m_pAcceptor->cancel();

	if (m_pAcceptor->is_open() == true)
		m_pAcceptor->close();

}

void CTcpServer::handle_accept(CTcpConnection::PtrCTcpConnection newConnection, const system::error_code& error)
{
	if (!error)
		CFileLog::cfilelog() << "CTcpServer::handle_accept: new connection created" << std::endl;
	else
		CFileLog::cfilelog() << "CTcpServer::handle_accept: new connection didn't create with error: " <<  error << std::endl;
}
