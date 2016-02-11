///////////////////////////////////////////////////////////
//  CSerialPortCtl.h
//  Implementation of the Class CSerialPortCtl
//  Created on:      19-���-2016 19:58:08
//  Original author: user-PC
///////////////////////////////////////////////////////////

#if !defined(EA_0925CBD4_E13C_48f3_93E7_16C4D33C203E__INCLUDED_)
#define EA_0925CBD4_E13C_48f3_93E7_16C4D33C203E__INCLUDED_

#include "CBaseCommCtl.h"

#include <boost/asio/serial_port.hpp>

using namespace boost;

class CSerialPortCtl : public CBaseCommCtl
{

public:
	CSerialPortCtl(CBaseDevice* device, std::string& gpioName);
	virtual ~CSerialPortCtl();

	bool receive(int rcvData);
	uint32_t send(std::list<std::vector<uint8_t> > sendData);
	int setSettings(std::string deviceName);

private:
	asio::io_service m_ioService;
	asio::serial_port m_serialPort;

};
#endif // !defined(EA_0925CBD4_E13C_48f3_93E7_16C4D33C203E__INCLUDED_)
