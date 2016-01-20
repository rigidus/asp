///////////////////////////////////////////////////////////
//  CTcpConnectionListener.cpp
//  Implementation of the Class CTcpConnectionListener
//  Created on:      20-џэт-2016 16:20:20
//  Original author: user-PC
///////////////////////////////////////////////////////////

#include "CTcpConnectionListener.h"


CTcpConnectionListener::CTcpConnectionListener(){

}



CTcpConnectionListener::CTcpConnectionListener(const CTcpConnectionListener& theCTcpConnectionListener){

}





CTcpConnectionListener::CTcpConnectionListener(TFnReceive fnDoReceive, TFnConnect fnDoConnect, TFnDisconnect fnDoDisconnect){

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


void CTcpConnectionListener::DoConnect(uin16_t port){

}