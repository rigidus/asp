///////////////////////////////////////////////////////////
//  CTcpConnectionListener.h
//  Implementation of the Class CTcpConnectionListener
//  Created on:      20-џэт-2016 16:20:20
//  Original author: user-PC
///////////////////////////////////////////////////////////

#if !defined(EA_6E0CD10A_725B_4ab9_89BB_03FD158DB4B4__INCLUDED_)
#define EA_6E0CD10A_725B_4ab9_89BB_03FD158DB4B4__INCLUDED_

#include "CTcpServerManager.h"
#include "CTcpServer.h"

public class CTcpConnectionListener
{

public:
	CTcpConnectionListener();
	CTcpConnectionListener(const CTcpConnectionListener& theCTcpConnectionListener);

	CTcpConnectionListener(TFnReceive fnDoReceive, TFnConnect fnDoConnect, TFnDisconnect fnDoDisconnect);
	virtual ~CTcpConnectionListener();
	void startWaitingThread();
	void stopListenThread();
	void DoReceiving(std::vector<uint8_t>& rcvData);
	void DoDisconnect(uint16_t port);
	void DoConnect(uin16_t port);

private:
	CTcpServerManager* m_CTcpServerManager;
	CTcpServer* m_CTcpServer;
	TFnDoReceive m_fnDoReceive;
	TFnDoConnect m_fnDoConnect;
	TFnDoDisconnect m_fnDoDisconnect;

};
#endif // !defined(EA_6E0CD10A_725B_4ab9_89BB_03FD158DB4B4__INCLUDED_)
