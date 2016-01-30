///////////////////////////////////////////////////////////
//  CTcpConnectionListener.cpp
//  Implementation of the Class CTcpConnectionListener
//  Created on:      20-���-2016 16:20:20
//  Original author: user-PC
///////////////////////////////////////////////////////////

#include "CTcpConnectionListener.h"


CTcpConnectionListener::CTcpConnectionListener():
	m_fnDoReceive(nullptr),
	m_fnDoConnect(nullptr),
	m_fnDoDisconnect(nullptr)
{

}


CTcpConnectionListener::CTcpConnectionListener(const CTcpConnectionListener& theCTcpConnectionListener){

	m_fnDoReceive = theCTcpConnectionListener.m_fnDoReceive;
	m_fnDoConnect = theCTcpConnectionListener.m_fnDoConnect;
	m_fnDoDisconnect = theCTcpConnectionListener.m_fnDoDisconnect;

}


CTcpConnectionListener::~CTcpConnectionListener(){

}


void CTcpConnectionListener::setListenerFunctions(TFnDoReceive fnDoReceive, TFnDoConnect fnDoConnect, TFnDoDisconnect fnDoDisconnect){

	m_fnDoReceive = fnDoReceive;
	m_fnDoConnect = fnDoConnect;
	m_fnDoDisconnect = fnDoDisconnect;

}


void CTcpConnectionListener::DoReceiving(
				asio::ip::tcp::socket& socket,
				std::vector<uint8_t> rcvData,
				std::string& clientName)
{
	if (m_fnDoReceive != nullptr)
		m_fnDoReceive(socket, rcvData, clientName);
}


void CTcpConnectionListener::DoConnect(asio::ip::tcp::socket& socket, std::string& clientName)
{
	if (m_fnDoConnect != nullptr)
		m_fnDoConnect(socket, clientName);
}


void CTcpConnectionListener::DoDisconnect(std::string& clientName)
{
	if (m_fnDoDisconnect != nullptr)
		m_fnDoDisconnect(clientName);
}
