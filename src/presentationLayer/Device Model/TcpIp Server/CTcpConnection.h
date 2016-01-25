///////////////////////////////////////////////////////////
//  CTcpConnection.h
//  Implementation of the Class CTcpConnection
//  Created on:      20-���-2016 16:20:19
//  Original author: user-PC
///////////////////////////////////////////////////////////

#if !defined(EA_6A05EB90_FC49_43f0_9433_51FEBF903706__INCLUDED_)
#define EA_6A05EB90_FC49_43f0_9433_51FEBF903706__INCLUDED_

#include <string>
#include <list>

#include <asio.hpp>
#include <system/error_code.hpp>
#include <thread/thread.hpp>
#include <thread/mutex.hpp>

using namespace boost;

class  CTcpConnectionListener;

/**
 * Class encapsulate boost::asio::ip::tcp::socket and additional features: - send one
 * block as several frames - data logging - callback for receiving
 */
class CTcpConnection
{

public:
	CTcpConnection();

	CTcpConnection(asio::ip::tcp::socket& socket, CTcpConnectionListener* listener, uint32_t maxBufSize, uint32_t messageTimeout, uint32_t messageFragmentTimeout);
	virtual ~CTcpConnection();
	uint32_t send(std::list<std::vector<uint8_t> >& sendData);
	uint32_t getMessageTimeout();
	void setMessageTimeout(uint32_t messageTimeout);
	void setMessageFragmentTimeout(uint32_t messageFragmentTimeout);
	uint32_t getMessageFragmentTimeout();
	void onReceive(system::error_code& err, size_t bytes);
	uint8_t read(std::vector<uint8_t>& readData);
	std::string& getClientName();

private:
	static int32_t s_ConnectionCounter;
	static boost::mutex s_mutexCounter;
	const uint32_t c_maxBufferSize;
	const uint32_t c_connectionNum;
	CTcpConnectionListener* m_listener;
	asio::ip::tcp::socket* m_pSocket;
	std::vector<uint8_t> m_tselRemote;
	std::vector<uint8_t> m_tselLocal;
	uint32_t m_messageTimeout;
	uint32_t m_messageFragmentTimeout;
	uint32_t m_maxBufSize;
	bool m_closed;
	std::string m_clientName;

};
#endif // !defined(EA_6A05EB90_FC49_43f0_9433_51FEBF903706__INCLUDED_)
