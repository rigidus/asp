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

#include <asio.hpp>
using namespace boost;

#include "CTcpConnectionListener.h"
#include "CTcpSocketFactory.h"
#include "CTcpServer.h"

class CTcpServerManager
{

public:
	virtual ~CTcpServerManager();
	CTcpServerManager(const CTcpServerManager& rhs);

	CTcpServerManager(uint32_t port);
	CTcpServerManager(uint32_t port, std::string bindAddr, CTcpConnectionListener* listener);
	void startListening();
	CTcpConnectionListener* createServer();
	void stopListening();

private:
	uint32_t m_localPort;
	std::string m_bindAddress;
	CTcpSocketFactory* m_pServerSocketFactory;
	CTcpConnectionListener* m_pConnectionListener;
	std::vector<CTcpServer*> m_pServer;
	uint32_t m_maxConnection;
	uint32_t m_messageTimeout;
	uint32_t m_messageFragmentTimeout;
	asio::io_service m_ioService;

};
#endif // !defined(EA_9BAFE2FC_C8D3_45d1_926D_EB0725E750FE__INCLUDED_)
