///////////////////////////////////////////////////////////
//  CTcpServerManager.cpp
//  Implementation of the Class CTcpServerManager
//  Created on:      20-���-2016 16:20:20
//  Original author: user-PC
///////////////////////////////////////////////////////////

#include "CTcpServerManager.h"


CTcpServerManager::CTcpServerManager(const CTcpServerManager& rhs){

}


CTcpServerManager::CTcpServerManager(uint32_t port):
		m_localPort(port),
		m_bindAddress("127.0.0.1"),
		m_pConnectionListener(nullptr),
		m_maxConnection(100),
		m_msgTimeout(10000),
		m_msgFragmentTimeout(5000),
		m_maxBufSize(8192)
{

}


CTcpServerManager::CTcpServerManager(uint32_t port, std::string bindAddr, CTcpConnectionListener* listener):
		m_localPort(port),
		m_bindAddress(bindAddr),
		m_pConnectionListener(listener),
		m_maxConnection(100),
		m_msgTimeout(10000),
		m_msgFragmentTimeout(5000),
		m_maxBufSize(8192)
{

}

CTcpServerManager::~CTcpServerManager(){

}

void CTcpServerManager::startListening(){

}


CTcpConnectionListener* CTcpServerManager::createServer(){

	CFileLog::cfilelog() << "CTcpServerManager::createServer" << std::endl;

	CTcpConnectionListener* listener = m_pConnectionListener;

	try {
		if (m_pConnectionListener == nullptr) m_pConnectionListener = new CTcpConnectionListener;
		listener = m_pConnectionListener;

		CTcpServer* server = new CTcpServer(m_ioService, listener, m_localPort, m_maxBufSize, m_msgTimeout, m_msgFragmentTimeout, m_maxConnection);
	}

	catch(std::bad_alloc& ex)
	{

		return nullptr;
	}

	return  listener;
}


void CTcpServerManager::stopListening(){

}
