///////////////////////////////////////////////////////////
//  CTcpServerManager.cpp
//  Implementation of the Class CTcpServerManager
//  Created on:      20-���-2016 16:20:20
//  Original author: user-PC
///////////////////////////////////////////////////////////

#include "CTcpServerManager.h"


CTcpServerManager::CTcpServerManager():
		m_maxConnection(100),
		m_msgTimeout(10000),
		m_msgFragmentTimeout(5000),
		m_maxBufSize(8192)
{
	CFileLog::cfilelog() << "create CTcpServerManager: " << std::endl;

}


CTcpServerManager::~CTcpServerManager(){

	CFileLog::cfilelog() << "delete CTcpServerManager: " << std::endl;

	deleteAllServers();
}

void CTcpServerManager::startListening(CTcpServer* server){

	CFileLog::cfilelog() << "CTcpServerManager::startListening: "  << std::endl;

	server->startServer();

	server->ioService().run();

	CFileLog::cfilelog() << "CTcpServerManager::io_service cancelled" << std::endl;

}


CTcpServer* CTcpServerManager::createServer(std::string host, uint32_t port, CTcpConnectionListener& listener){

	CFileLog::cfilelog() << "CTcpServerManager::createServer" << std::endl;

	try {

		CTcpServer* server = new CTcpServer(listener, port, m_maxBufSize, m_msgTimeout, m_msgFragmentTimeout, m_maxConnection);

		boost::thread* thread = new boost::thread(&CTcpServerManager::startListening, server);

		ServerPars pars;
		pars.pServer = server;
		pars.pThr = thread;
		pars.host = host;
		pars.port = port;

		m_Servers.push_back(pars);
	}

	catch(std::exception& ex)
	{
		CFileLog::cfilelog() << "CTcpServerManager::createServer: server wasn't created: " <<
				ex.what() << std::endl;

		return nullptr;
	}

	CFileLog::cfilelog() << "CTcpServerManager::createServer: server " << m_Servers.size()-1 << " created" << std::endl;

	return  m_Servers.back().pServer;
}

void CTcpServerManager::deleteAllServers()
{
	CFileLog::cfilelog() << "CTcpServerManager::deleteAllServers starts" << std::endl;

	for (auto v: m_Servers)	{
		v.pServer->stopServer();

		v.pThr->join();

		delete v.pThr;
		delete v.pServer;
	}

	CFileLog::cfilelog() << "CTcpServerManager::deleteAllServers successfully" << std::endl;
}

void CTcpServerManager::deleteServer(std::string host, uint32_t port){

	CFileLog::cfilelog() << "CTcpServerManager::deleteServer: " << host.c_str() << ":" << port << std::endl;

	ServerPars pars = {nullptr, nullptr, 0, 0};

	int32_t i = 0, num = -1;
	for (auto v: m_Servers)	{
		if (v.host == host && v.port == port) {
			pars = v;
			num = i;
			break;
		}
		++i;
	}

	if (num > -1)
	{
		pars.pServer->stopServer();

		pars.pThr->join();
		delete pars.pThr;
		delete pars.pServer;
		m_Servers.erase(m_Servers.begin() + i);
	}

	CFileLog::cfilelog() << "CTcpServerManager::deleteServer: server deleted: " << num << std::endl;
}

