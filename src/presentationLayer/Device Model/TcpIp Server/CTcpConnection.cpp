///////////////////////////////////////////////////////////
//  CTcpConnection.cpp
//  Implementation of the Class CTcpConnection
//  Created on:      20-���-2016 16:20:19
//  Original author: user-PC
///////////////////////////////////////////////////////////

#include "CTcpConnection.h"

CTcpConnection::CTcpConnection(boost::asio::io_service& ioService,
				CTcpConnectionListener* listener,
				uint32_t maxBufSize,
				uint32_t messageTimeout,
				uint32_t messageFragmentTimeout)
				: c_maxBufferSize(8192), c_connectionNum(10) {

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

	time_t now = time(0);

	// hardcode start datetime string
	m_startDayTime = "{ datetime: \"}";
	m_startDayTime += ctime(&now);
	m_startDayTime += "\"}";

	boost::asio::async_write(
			m_socket,
			boost::asio::buffer(m_startDayTime),
			boost::bind(
					&CTcpConnection::handle_write,
					shared_from_this(),
					boost::asio::placeholders::error,
					boost::asio::placeholders::bytes_transferred
			)
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
						shared_from_this(),
						boost::asio::placeholders::error,
						boost::asio::placeholders::bytes_transferred
				)
		);

	}

	return  len;
}

void CTcpConnection::handle_write()
{

}

void CTcpConnection::handle_read(const boost::system::error_code& error){

	// TODO: call Listener callback and copy data to receive buffer

	boost::asio::async_read_until(
			m_socket,
			)

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


void CTcpConnection::onReceive(system::error_code& err, size_t bytes){

}


uint8_t CTcpConnection::read(std::vector<uint8_t>& readData){

	return  0;
}


std::string& CTcpConnection::getClientName(){

	return m_clientName;
}
