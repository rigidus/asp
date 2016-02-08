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

typedef void(*TFnDoReceive)(asio::ip::tcp::socket&, uint8_t*, std::size_t, std::string&);
typedef void(*TFnDoConnect)(asio::ip::tcp::socket&, std::string&);
typedef void(*TFnDoDisconnect)(std::string&);

class CTcpConnectionListener
{

public:
	CTcpConnectionListener();
	CTcpConnectionListener(const CTcpConnectionListener& listener);
	virtual ~CTcpConnectionListener();

	void setListenerFunctions(TFnDoReceive fnDoReceive, TFnDoConnect fnDoConnect, TFnDoDisconnect fnDoDisconnect);
	void DoReceiving(asio::ip::tcp::socket& socket, uint8_t* rcvData, std::size_t rcvSize, std::string& clientName);
	void DoConnect(asio::ip::tcp::socket& socket, std::string& clientName);
	void DoDisconnect(std::string& clientName);

	TFnDoReceive m_fnDoReceive;
	TFnDoConnect m_fnDoConnect;
	TFnDoDisconnect m_fnDoDisconnect;

};
#endif // !defined(EA_6E0CD10A_725B_4ab9_89BB_03FD158DB4B4__INCLUDED_)
