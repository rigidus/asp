///////////////////////////////////////////////////////////
//  CTcpServerManager.h
//  Implementation of the Class CTcpServerManager
//  Created on:      20-���-2016 16:20:20
//  Original author: user-PC
///////////////////////////////////////////////////////////

#if !defined(EA_9BAFE2FC_C8D3_45d1_926D_EB0725E750FE__INCLUDED_)
#define EA_9BAFE2FC_C8D3_45d1_926D_EB0725E750FE__INCLUDED_

#include <string>
#include <vector>
#include <stdexcept>

#include "boost_include.h"
using namespace boost;

#include "CTcpConnectionListener.h"
#include "CTcpServer.h"

#include "../include/FileLog.h"

class CTcpServerManager: public noncopyable
{

public:
	virtual ~CTcpServerManager();

	CTcpServerManager();
	CTcpServer* createServer(std::string host, uint32_t port, CTcpConnectionListener& listener);
	void deleteServer(std::string host, uint32_t port);
	void deleteAllServers();

private:

	static void startListening(CTcpServer* server);

	struct ServerPars{
		CTcpServer* pServer;
		boost::thread* pThr;
		std::string host;
		uint32_t port;
	};

	std::vector<ServerPars> m_Servers;
	uint32_t m_maxConnection;
	uint32_t m_msgTimeout;
	uint32_t m_msgFragmentTimeout;
	uint32_t m_maxBufSize;

};

#endif //!defined(EA_9BAFE2FC_C8D3_45d1_926D_EB0725E750FE__INCLUDED_)
