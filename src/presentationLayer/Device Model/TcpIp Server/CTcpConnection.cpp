///////////////////////////////////////////////////////////
//  CTcpConnection.cpp
//  Implementation of the Class CTcpConnection
//  Created on:      20-џэт-2016 16:20:19
//  Original author: user-PC
///////////////////////////////////////////////////////////

#include "CTcpConnection.h"


CTcpConnection::CTcpConnection(){

}





CTcpConnection::CTcpConnection(ip::tcp::socket& socket, CTcpConnectionListener* listener, uint32_t maxBufSize, uint32_t messageTimeout, uint32_t messageFragmentTimeout){

}


CTcpConnection::~CTcpConnection(){

}


uint32_t CTcpConnection::send(std::list<std::vector<uint8_t> >& sendData){

	return  NULL;
}


uint32_t CTcpConnection::getMessageTimeout(){

	return  NULL;
}


void CTcpConnection::setMessageTimeout(uint32_t messageTimeout){

}


void CTcpConnection::setMessageFragmentTimeout(uint32_t messageFragmentTimeout){

}


uint32_t CTcpConnection::getMessageFragmentTimeout(){

	return  NULL;
}


void CTcpConnection::onReceive(error_code& err, size_t bytes){

}


uint8_t CTcpConnection::read(std::vector<uint8_t>& readData){

	return  NULL;
}


std::string& CTcpConnection::getClientName(){

	return  NULL;
}