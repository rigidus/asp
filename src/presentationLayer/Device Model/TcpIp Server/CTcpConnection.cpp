///////////////////////////////////////////////////////////
//  CTcpConnection.cpp
//  Implementation of the Class CTcpConnection
//  Created on:      20-���-2016 16:20:19
//  Original author: user-PC
///////////////////////////////////////////////////////////

#include "CTcpConnection.h"


CTcpConnection::CTcpConnection(): c_maxBufferSize(8192), c_connectionNum(10){

}





CTcpConnection::CTcpConnection(asio::ip::tcp::socket& socket,
				CTcpConnectionListener* listener,
				uint32_t maxBufSize,
				uint32_t messageTimeout,
				uint32_t messageFragmentTimeout)
				: c_maxBufferSize(8192), c_connectionNum(10) {

}


CTcpConnection::~CTcpConnection(){

}


uint32_t CTcpConnection::send(std::list<std::vector<uint8_t> >& sendData){

	return  0;
}


uint32_t CTcpConnection::getMessageTimeout(){

	return  0;
}


void CTcpConnection::setMessageTimeout(uint32_t messageTimeout){

}


void CTcpConnection::setMessageFragmentTimeout(uint32_t messageFragmentTimeout){

}


uint32_t CTcpConnection::getMessageFragmentTimeout(){

	return  0;
}


void CTcpConnection::onReceive(system::error_code& err, size_t bytes){

}


uint8_t CTcpConnection::read(std::vector<uint8_t>& readData){

	return  0;
}


std::string& CTcpConnection::getClientName(){

	return m_clientName;
}
