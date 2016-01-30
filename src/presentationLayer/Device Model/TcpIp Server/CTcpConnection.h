///////////////////////////////////////////////////////////
//  CTcpConnection.h
//  Implementation of the Class CTcpConnection
//  Created on:      20-���-2016 16:20:19
//  Original author: user-PC
///////////////////////////////////////////////////////////

#if !defined(EA_6A05EB90_FC49_43f0_9433_51FEBF903706__INCLUDED_)
#define EA_6A05EB90_FC49_43f0_9433_51FEBF903706__INCLUDED_

#include <ctime>

#include <iostream>
#include <string>
#include <list>

#include "boost_include.h"

using namespace boost;
using namespace boost::asio::ip;

#include "CTcpConnectionListener.h"

/**
 * Class encapsulate boost::asio::ip::tcp::socket and additional features: - send one
 * block as several frames - data logging - callback for receiving
 */
class CTcpConnection: public enable_shared_from_this<CTcpConnection>
{

public:

	typedef boost::shared_ptr<CTcpConnection> PtrCTcpConnection;

	static PtrCTcpConnection createNewConnection(
			boost::asio::io_service& ioService,
			CTcpConnectionListener* listener,
			uint32_t maxBufSize,
			uint32_t messageTimeout,
			uint32_t messageFragmentTimeout);

	virtual ~CTcpConnection();
	tcp::socket& socket();
	uint32_t send(std::list<std::vector<uint8_t> >& sendData);

	void start();
	uint32_t getMessageTimeout();
	void setMessageTimeout(uint32_t messageTimeout);
	void setMessageFragmentTimeout(uint32_t messageFragmentTimeout);
	uint32_t getMessageFragmentTimeout();
	std::string& getClientName();

private:
	CTcpConnection(boost::asio::io_service& ioService, CTcpConnectionListener* listener, uint32_t maxBufSize, uint32_t messageTimeout, uint32_t messageFragmentTimeout);

	void handle_write();
	void handle_read(const boost::system::error_code& error, std::size_t bytes_transferred);

	static int32_t s_ConnectionCounter;
	static boost::mutex s_mutexCounter;
	std::vector<uint8_t> m_tselRemote;
	std::vector<uint8_t> m_tselLocal;
	asio::ip::tcp::socket m_socket;
	CTcpConnectionListener* m_listener;
	uint32_t m_messageTimeout;
	uint32_t m_messageFragmentTimeout;
	std::string m_clientName;
};
#endif // !defined(EA_6A05EB90_FC49_43f0_9433_51FEBF903706__INCLUDED_)
