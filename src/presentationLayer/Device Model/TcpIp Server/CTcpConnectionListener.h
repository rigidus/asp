///////////////////////////////////////////////////////////
//  CTcpConnectionListener.h
//  Implementation of the Class CTcpConnectionListener
//  Created on:      20-���-2016 16:20:20
//  Original author: user-PC
///////////////////////////////////////////////////////////

#if !defined(EA_6E0CD10A_725B_4ab9_89BB_03FD158DB4B4__INCLUDED_)
#define EA_6E0CD10A_725B_4ab9_89BB_03FD158DB4B4__INCLUDED_

#include <asio.hpp>
using namespace boost;

typedef void(*TFnDoReceive)(asio::ip::tcp::socket&, std::vector<uint8_t>, std::string&);
typedef void(*TFnDoConnect)(asio::ip::tcp::socket&, std::string&);
typedef void(*TFnDoDisconnect)(std::string&);

class CTcpConnectionListener
{

public:
	CTcpConnectionListener();
	CTcpConnectionListener(const CTcpConnectionListener& listener);

	void setListenerFunctions(TFnDoReceive fnDoReceive, TFnDoConnect fnDoConnect, TFnDoDisconnect fnDoDisconnect);
	virtual ~CTcpConnectionListener();
	void startWaitingThread();
	void stopListenThread();
	void DoReceiving(std::vector<uint8_t>& rcvData);
	void DoDisconnect(uint16_t port);
	void DoConnect(uint16_t port);

private:
	TFnDoReceive m_fnDoReceive;
	TFnDoConnect m_fnDoConnect;
	TFnDoDisconnect m_fnDoDisconnect;

};
#endif // !defined(EA_6E0CD10A_725B_4ab9_89BB_03FD158DB4B4__INCLUDED_)
