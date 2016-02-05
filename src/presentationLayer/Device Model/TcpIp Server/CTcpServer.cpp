///////////////////////////////////////////////////////////
//  CTcpServer.cpp
//  Implementation of the Class CTcpServer
//  Created on:      20-���-2016 16:20:20
//  Original author: user-PC
///////////////////////////////////////////////////////////

#include "CTcpServer.h"

using namespace boost::asio::ip;


CTcpServer::CTcpServer(CTcpConnectionListener& listener, uint32_t localPort, uint32_t maxBufSize, uint32_t msgTimeout, uint32_t msgFragmentTimeout, uint32_t maxConnections):
				m_localPort(localPort),
				m_maxBufSize(maxBufSize),
				m_messageTimeout(msgTimeout),
				m_messageFragmentTimeout(msgFragmentTimeout),
				m_maxConnection(maxConnections),
				m_ConnectionListener(listener),
				m_acceptor(tcp::acceptor(m_ioService, tcp::endpoint(tcp::v4(), localPort)))
{
	CFileLog::cfilelog() << "CTcpServer, port: " << localPort << ", created" << std::endl;
}


CTcpServer::CTcpServer(CTcpConnectionListener& listener, uint32_t localPort):
				m_localPort(localPort),
				m_maxBufSize(32768),	// default buffer size
				m_messageTimeout(10000),	// default value
				m_messageFragmentTimeout(1000), // default value
				m_maxConnection(1000),		// default maximum connection number
				m_ConnectionListener(listener),
				m_acceptor(tcp::acceptor(m_ioService, tcp::endpoint(tcp::v4(), localPort)))
{
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

	CTcpConnection::PtrCTcpConnection pointer = CTcpConnection::createNewConnection(
			m_ioService,
			m_ConnectionListener,
			m_maxBufSize,
			m_messageTimeout,
			m_messageFragmentTimeout);

	m_acceptor.async_accept(
			pointer->socket(),
			bind(
				&CTcpServer::handle_accept,
				this,
				pointer,
				boost::asio::placeholders::error
			)
	);
}


void CTcpServer::send(uint32_t clientNum, std::list<std::vector<uint8_t> >& data)
{
	if (clientNum >= m_connections.size()) return;

	m_connections[clientNum]->send(data);
}


void CTcpServer::stopServer(){

	CFileLog::cfilelog() << "CTcpServer::stopServer" << std::endl;

	m_acceptor.cancel();

	m_acceptor.close();

	for (auto v: m_connections)
		v->stop();

}

void CTcpServer::handle_accept(CTcpConnection::PtrCTcpConnection newConnection, const system::error_code& error)
{
	CFileLog::cfilelog() << "CTcpServer::handle_accept: new connection created with error_code=" <<
			error.message() << std::endl;

	if (!error)
	{

		m_ConnectionListener.DoConnect(newConnection->socket(), newConnection->getClientName());

		m_connections.push_back(newConnection);

		newConnection->start();
	}
	else
	{
		CFileLog::cfilelog() << "CTcpServer::handle_accept: new connection didn't create with error: " <<  error << std::endl;

		std::stringstream strError;
		strError << newConnection->getClientName() << " Connection Accept error: " << error;
		std::string textError(strError.str());
		m_ConnectionListener.DoConnect(newConnection->socket(), textError);
	}
}

asio::io_service& CTcpServer::ioService() {
	return m_ioService;
}

