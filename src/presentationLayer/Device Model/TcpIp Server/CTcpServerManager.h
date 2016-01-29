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

#include <asio.hpp>
#include <utility.hpp>
using namespace boost;

#include "CTcpConnectionListener.h"
#include "CTcpSocketFactory.h"
#include "CTcpServer.h"

#include "../include/FileLog.h"

class CTcpServerManager: public noncopyable
{

public:
	virtual ~CTcpServerManager();

	CTcpServerManager(uint32_t port);
	CTcpServerManager(uint32_t port, std::string bindAddr, CTcpConnectionListener* listener);
	void startListening(uint32_t index);
	CTcpConnectionListener* createServer();
	void deleteServer(uint32_t index);
	void stopListening(uint32_t index);

private:
	uint32_t m_localPort;
	std::string m_bindAddress;
	CTcpConnectionListener* m_pConnectionListener;
	std::vector<CTcpServer*> m_Servers;
	uint32_t m_maxConnection;
	uint32_t m_msgTimeout;
	uint32_t m_msgFragmentTimeout;
	uint32_t m_maxBufSize;
	asio::io_service m_ioService;

};
#endif // !defined(EA_9BAFE2FC_C8D3_45d1_926D_EB0725E750FE__INCLUDED_)
