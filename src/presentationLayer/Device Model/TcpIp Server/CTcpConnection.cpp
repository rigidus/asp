///////////////////////////////////////////////////////////
//  CTcpConnection.cpp
//  Implementation of the Class CTcpConnection
//  Created on:      20-���-2016 16:20:19
//  Original author: user-PC
///////////////////////////////////////////////////////////

#include "CTcpConnection.h"

CTcpConnection::CTcpConnection(
				boost::asio::io_service& ioService,
				CTcpConnectionListener* listener,
				uint32_t maxBufSize,
				uint32_t messageTimeout,
				uint32_t messageFragmentTimeout):
				m_socket(ioService),
				m_listener(listener),
				m_messageTimeout(messageTimeout),
				m_messageFragmentTimeout(messageFragmentTimeout)
{
	m_tselLocal.resize(maxBufSize);
}


CTcpConnection::~CTcpConnection(){

}

CTcpConnection::PtrCTcpConnection CTcpConnection::createNewConnection(
		boost::asio::io_service& ioService,
		CTcpConnectionListener* listener,
		uint32_t maxBufSize,
		uint32_t messageTimeout,
		uint32_t messageFragmentTimeout)
{

	return PtrCTcpConnection(new CTcpConnection(ioService, listener, maxBufSize, messageTimeout, messageFragmentTimeout));
}

tcp::socket& CTcpConnection::socket(){

	return m_socket;
}

void CTcpConnection::start(){

	uint8_t* data = const_cast<uint8_t*>(&m_tselLocal[0]);
	m_socket.async_read_some(boost::asio::buffer(data, m_tselLocal.size()),
	        boost::bind(
	        		&CTcpConnection::handle_read,
					shared_from_this(),
					boost::asio::placeholders::error,
					boost::asio::placeholders::bytes_transferred)
	);

}

uint32_t CTcpConnection::send(std::list<std::vector<uint8_t> >& sendData){

	uint32_t len = 0;

	for (auto& v: sendData){

		len += v.size();

		boost::asio::async_write(
				m_socket,
				boost::asio::buffer(v),
				boost::bind(
						&CTcpConnection::handle_write,
						shared_from_this()
				)
		);

	}

	return  len;
}

void CTcpConnection::handle_write()
{

}

void CTcpConnection::handle_read(const boost::system::error_code& err, std::size_t bt_transferred) {

	uint8_t* data = const_cast<uint8_t*>(&m_tselLocal[0]);
	m_socket.async_read_some(boost::asio::buffer(data, m_tselLocal.size()),
	        boost::bind(
	        		&CTcpConnection::handle_read,
					shared_from_this(),
					boost::asio::placeholders::error,
					boost::asio::placeholders::bytes_transferred)
	);

	if (boost::system::errc::connection_aborted == err ||
		boost::system::errc::connection_reset == err ||
		boost::system::errc::connection_refused == err
		)
		m_listener->m_fnDoDisconnect(m_clientName);
	else
		m_listener->m_fnDoReceive(m_socket, m_tselLocal, m_clientName);

}

uint32_t CTcpConnection::getMessageTimeout(){

	return  m_messageTimeout;
}


void CTcpConnection::setMessageTimeout(uint32_t messageTimeout){
	m_messageTimeout = messageTimeout;
}


void CTcpConnection::setMessageFragmentTimeout(uint32_t messageFragmentTimeout){
	m_messageFragmentTimeout = messageFragmentTimeout;
}


uint32_t CTcpConnection::getMessageFragmentTimeout(){

	return  m_messageFragmentTimeout;
}


std::string& CTcpConnection::getClientName(){

	return m_clientName;
}
