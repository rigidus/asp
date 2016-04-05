//============================================================================
// Name        : CSerialPortCtl.h
// Author      : aav
// Created on  : 19 февр. 2016 г.
// Version     : v.0.1
// Copyright   : Non nobis, Domine, non nobis, sed nomini tuo da gloriam.
// Description : Multitone for communication resources of the UART type.
//============================================================================

#if !defined(EA_0925CBD4_E13C_48f3_93E7_16C4D33C203E__INCLUDED_)
#define EA_0925CBD4_E13C_48f3_93E7_16C4D33C203E__INCLUDED_

#include "CBaseCommCtl.h"
#include "devices/CBaseDevice.h"
#include "GlobalThreadPool.h"

#include <iostream>
#include <map>

#include <boost/filesystem.hpp>
#include <boost/iostreams/device/file.hpp>
#include <boost/iostreams/stream.hpp>
#include <boost/asio/serial_port.hpp>


using namespace boost;

class CSerialPortCtl : public CBaseCommCtl
{

public:

	/*
	 * Function takes GPIO resourse for concrete device
	 * @param device - pointer to concrete device use for call callback for inform about event from device
	 * @param gpioName - name of the GPIO resourse for taking
	 * @return - pointer to busied resource or nullptr when resourse is busy or not existing
	 */
	static shared_ptr<CBaseCommCtl> takeCommCtl(CBaseDevice* device, const std::string& uartName);

	/*
	 * Function frees UART resource for it can be taking by another device in the future
	 * @param device - pointer to concrete device for control. Device-taker can free UART resourse only
	 * @param uartName - name of the UART resourse for free
	 */
	static void freeCommCtl(CBaseDevice* device, const std::string& uartName);

	virtual ~CSerialPortCtl();

	bool receive(int rcvData);
	uint32_t send(std::vector<uint8_t> sendData);
	uint32_t send(std::list<std::vector<uint8_t> > sendData);
	int setSettings(std::string deviceName);

	static const std::string s_name;

private:
	CSerialPortCtl(CBaseDevice* device, const std::string& gpioName);

	asio::io_service m_ioService;
	asio::serial_port m_serialPort;

	// check existing file on the filesystem
	// It use here for check interface tty files only
	static bool fileIsExist(const std::string& fileName);

	static std::map<std::string, shared_ptr<CBaseCommCtl> > busyUarts;

	static const std::string gpioPath;
};
#endif // !defined(EA_0925CBD4_E13C_48f3_93E7_16C4D33C203E__INCLUDED_)
