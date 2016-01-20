///////////////////////////////////////////////////////////
//  CTcpSocketCtl.h
//  Implementation of the Class CTcpSocketCtl
//  Created on:      19-џэт-2016 19:58:09
//  Original author: user-PC
///////////////////////////////////////////////////////////

#if !defined(EA_FABDEA08_BF86_45b2_9B2E_0C13F5B36478__INCLUDED_)
#define EA_FABDEA08_BF86_45b2_9B2E_0C13F5B36478__INCLUDED_

#include "CBaseCommCtl.h"

public class CTcpSocketCtl : private CBaseCommCtl
{

public:
	CTcpSocketCtl();
	virtual ~CTcpSocketCtl();
	CTcpSocketCtl(const CTcpSocketCtl& theCTcpSocketCtl);

	bool receive(int rcvData);
	uint32_t send(std::list<std::vector<uint8_t> > sendData);
	int setSettings(std::strring deviceName);

private:
	uint32_t m_address;
	std::string m_host;
	uin16_t m_port;

};
#endif // !defined(EA_FABDEA08_BF86_45b2_9B2E_0C13F5B36478__INCLUDED_)
