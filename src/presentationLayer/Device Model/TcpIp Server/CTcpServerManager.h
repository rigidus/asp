///////////////////////////////////////////////////////////
//  CTcpServerManager.h
//  Implementation of the Class CTcpServerManager
//  Created on:      20-џэт-2016 16:20:20
//  Original author: user-PC
///////////////////////////////////////////////////////////

#if !defined(EA_9BAFE2FC_C8D3_45d1_926D_EB0725E750FE__INCLUDED_)
#define EA_9BAFE2FC_C8D3_45d1_926D_EB0725E750FE__INCLUDED_

public class CTcpServerManager
{

public:
	CTcpServerManager();
	virtual ~CTcpServerManager();
	CTcpServerManager(const CTcpServerManager& theCTcpServerManager);

	CTcpServerManager(uint32_t port);
	CTcpServerManager(uint32_t port, std::string bindAddr, listener);
	void startListening();
	CConnectionListener* createServer();
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
	boost::asio::io_service m_ioService;

};
#endif // !defined(EA_9BAFE2FC_C8D3_45d1_926D_EB0725E750FE__INCLUDED_)
