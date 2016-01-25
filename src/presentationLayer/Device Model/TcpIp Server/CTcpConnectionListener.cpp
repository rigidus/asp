///////////////////////////////////////////////////////////
//  CTcpConnectionListener.cpp
//  Implementation of the Class CTcpConnectionListener
//  Created on:      20-���-2016 16:20:20
//  Original author: user-PC
///////////////////////////////////////////////////////////

#include "CTcpConnectionListener.h"


CTcpConnectionListener::CTcpConnectionListener(){

}



CTcpConnectionListener::CTcpConnectionListener(const CTcpConnectionListener& theCTcpConnectionListener){

}





CTcpConnectionListener::CTcpConnectionListener(TFnDoReceive fnDoReceive, TFnDoConnect fnDoConnect, TFnDoDisconnect fnDoDisconnect){

}


CTcpConnectionListener::~CTcpConnectionListener(){

}


void CTcpConnectionListener::startWaitingThread(){

}


void CTcpConnectionListener::stopListenThread(){

}


void CTcpConnectionListener::DoReceiving(std::vector<uint8_t>& rcvData){

}


void CTcpConnectionListener::DoDisconnect(uint16_t port){

}


void CTcpConnectionListener::DoConnect(uint16_t port){

}
