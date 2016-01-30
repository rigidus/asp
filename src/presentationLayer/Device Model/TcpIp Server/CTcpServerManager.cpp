///////////////////////////////////////////////////////////
//  CTcpServerManager.cpp
//  Implementation of the Class CTcpServerManager
//  Created on:      20-���-2016 16:20:20
//  Original author: user-PC
///////////////////////////////////////////////////////////

#include "CTcpServerManager.h"


CTcpServerManager::CTcpServerManager(uint32_t port):
		m_localPort(port),
		m_bindAddress("127.0.0.1"),
		m_pConnectionListener(nullptr),
		m_maxConnection(100),
		m_msgTimeout(10000),
		m_msgFragmentTimeout(5000),
		m_maxBufSize(8192)
{
	CFileLog::cfilelog() << "create CTcpServerManager: " << port << std::endl;

	m_ioService.run();
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
	CFileLog::cfilelog() << "create CTcpServerManager: " << port << "; " << bindAddr << std::endl;

}

CTcpServerManager::~CTcpServerManager(){

	CFileLog::cfilelog() << "delete CTcpServerManager: " << std::endl;

	for (auto v: m_Servers){
		v->stopServer();
		delete v;
	}

	CFileLog::cfilelog() << "delete CTcpServerManager: deleted" << std::endl;
}

void CTcpServerManager::startListening(uint32_t index){

	CFileLog::cfilelog() << "CTcpServerManager::startListening: " << index << std::endl;

	if (m_Servers.size() <= index) return;

	m_Servers[index]->startServer();

	CFileLog::cfilelog() << "CTcpServerManager::startListening: server " << index << " started " << std::endl;
}


CTcpConnectionListener* CTcpServerManager::createServer(){

	CFileLog::cfilelog() << "CTcpServerManager::createServer" << std::endl;

	CTcpConnectionListener* listener = m_pConnectionListener;

	try {
		if (m_pConnectionListener == nullptr) m_pConnectionListener = new CTcpConnectionListener;
		listener = m_pConnectionListener;

		CTcpServer* server = new CTcpServer(m_ioService, listener, m_localPort, m_maxBufSize, m_msgTimeout, m_msgFragmentTimeout, m_maxConnection);

		m_Servers.push_back(server);
	}

	catch(std::bad_alloc& ex)
	{

		return nullptr;
	}

	CFileLog::cfilelog() << "CTcpServerManager::createServer: server " << m_Servers.size()-1 << " created" << std::endl;

	return  listener;
}

void CTcpServerManager::deleteServer(uint32_t index){

	CFileLog::cfilelog() << "CTcpServerManager::deleteServer: " << index << std::endl;

	if (m_Servers.size() <= index) return;

	m_Servers[index]->stopServer();
	delete m_Servers[index];

	m_Servers.erase(m_Servers.begin() + index);

	CFileLog::cfilelog() << "CTcpServerManager::deleteServer: server deleted: " << index << std::endl;
}

void CTcpServerManager::stopListening(uint32_t index){

	CFileLog::cfilelog() << "CTcpServerManager::stopListening: " << index << std::endl;

	if (m_Servers.size() <= index) return;

	m_Servers[index]->stopServer();

	CFileLog::cfilelog() << "CTcpServerManager::stopListening: server stop: " << index << std::endl;
}
