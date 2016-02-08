///////////////////////////////////////////////////////////
//  CTcpConnection.cpp
//  Implementation of the Class CTcpConnection
//  Created on:      20-���-2016 16:20:19
//  Original author: user-PC
///////////////////////////////////////////////////////////

#include "CTcpConnection.h"

CTcpConnection::CTcpConnection(
				boost::asio::io_service& ioService,
				CTcpConnectionListener& listener,
				uint32_t maxBufSize,
				uint32_t messageTimeout,
				uint32_t messageFragmentTimeout):
				m_socket(ioService),
				m_listener(listener),
				m_messageTimeout(messageTimeout),
				m_messageFragmentTimeout(messageFragmentTimeout),
				m_isStoped(true)
{
	m_tselLocal.resize(maxBufSize);
}


CTcpConnection::~CTcpConnection(){

	CFileLog::cfilelog() << "CTcpConnection deleted" << std::endl;

}

CTcpConnection::PtrCTcpConnection CTcpConnection::createNewConnection(
		boost::asio::io_service& ioService,
		CTcpConnectionListener& listener,
		uint32_t maxBufSize,
		uint32_t messageTimeout,
		uint32_t messageFragmentTimeout)
{
	CFileLog::cfilelog() << "CTcpConnection::createNewConnection with maxBufSize=" << maxBufSize << std::endl;

	return PtrCTcpConnection(new CTcpConnection(ioService, listener, maxBufSize, messageTimeout, messageFragmentTimeout));
}

tcp::socket& CTcpConnection::socket(){

	return m_socket;
}

void CTcpConnection::start(){

	CFileLog::cfilelog() << "CTcpConnection::start" << std::endl;

	if (m_isStoped == false)
		return;

	m_isStoped = false;

	uint8_t* data = const_cast<uint8_t*>(&m_tselLocal[0]);
	m_socket.async_read_some(boost::asio::buffer(data, m_tselLocal.size()),
	        boost::bind(
	        		&CTcpConnection::handle_read,
					shared_from_this(),
					boost::asio::placeholders::error,
					boost::asio::placeholders::bytes_transferred)
	);

}

void CTcpConnection::stop()
{
	CFileLog::cfilelog() << "CTcpConnection::stop" << std::endl;

	if (m_isStoped == true)
		return;

	m_isStoped = true;

	m_socket.cancel();
	m_socket.close();
}

uint32_t CTcpConnection::send(std::list<std::vector<uint8_t> >& sendData)
{

	CFileLog::cfilelog() << "CTcpConnection::send" << std::endl;

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

void CTcpConnection::handle_write(){

	CFileLog::cfilelog() << "CTcpConnection::handle_write" << std::endl;

}

void CTcpConnection::handle_read(const boost::system::error_code& err, std::size_t bt_transferred) {

	CFileLog::cfilelog() << "CTcpConnection::handle_read: error_code=" << err.message()
			<< ", size=" << bt_transferred << std::endl;

	if (m_isStoped == true)
		return;

	uint8_t* data = const_cast<uint8_t*>(&m_tselLocal[0]);

	if (err)
	{
		CFileLog::cfilelog() << "CTcpConnection::handle_read: error code detected. Error code = " << err << std::endl;

		m_listener.m_fnDoDisconnect(m_clientName);

		stop();
	}else{
		uint8_t* pData = &m_tselLocal[0];
		m_listener.m_fnDoReceive(m_socket, pData, bt_transferred, m_clientName);

		m_socket.async_read_some(
				asio::buffer(data, m_tselLocal.size()),
				bind(
	        		&CTcpConnection::handle_read,
					shared_from_this(),
					boost::asio::placeholders::error,
					boost::asio::placeholders::bytes_transferred
			)
		);
	}
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
