///////////////////////////////////////////////////////////
//  CTcpServer.cpp
//  Implementation of the Class CTcpServer
//  Created on:      20-џэт-2016 16:20:20
//  Original author: user-PC
///////////////////////////////////////////////////////////

#include "CTcpServer.h"


CTcpServer::CTcpServer(){

}



CTcpServer::~CTcpServer(){

}



CTcpServer::CTcpServer(const CTcpServer& theCTcpServer){

}





CTcpServer::CTcpServer(boost:asio::io_service ioService, CTcpConnectionListener* listener, uint32_t localPort, uint32_t maxBufSize, uint32_t msgTimeout, uint32_t msgFragmentTimeout, uint32_t maxConnections){

}


CTcpConnection* CTcpServer::createNewConnection(CTcpSocket* tcpSocket){

	return  NULL;
}


uint32_t CTcpServer::getMaxConnections(){

	return  NULL;
}


void CTcpServer::setMaxConnections(uint32_t maxConnections){

}


void CTcpServer::startServer(){

}


void CTcpServer::stopServer(){

}