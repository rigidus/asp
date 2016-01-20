///////////////////////////////////////////////////////////
//  CSerialPortCtl.h
//  Implementation of the Class CSerialPortCtl
//  Created on:      19-џэт-2016 19:58:08
//  Original author: user-PC
///////////////////////////////////////////////////////////

#if !defined(EA_0925CBD4_E13C_48f3_93E7_16C4D33C203E__INCLUDED_)
#define EA_0925CBD4_E13C_48f3_93E7_16C4D33C203E__INCLUDED_

#include "CBaseCommCtl.h"

public class CSerialPortCtl : private CBaseCommCtl
{

public:
	CSerialPortCtl();
	virtual ~CSerialPortCtl();
	CSerialPortCtl(const CSerialPortCtl& theCSerialPortCtl);

	bool receive(int rcvData);
	uint32_t send(std::list<std::vector<uint8_t> > sendData);
	int setSettings(std::strring deviceName);

private:
	asio::io_service m_ioService;
	asio::serial_port m_serialPort;

};
#endif // !defined(EA_0925CBD4_E13C_48f3_93E7_16C4D33C203E__INCLUDED_)
