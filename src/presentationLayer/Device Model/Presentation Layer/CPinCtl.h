///////////////////////////////////////////////////////////
//  CPinCtl.h
//  Implementation of the Class CPinCtl
//  Created on:      19-���-2016 19:58:08
//  Original author: user-PC
///////////////////////////////////////////////////////////

#if !defined(EA_F6FA3185_3A4D_4043_A499_06D6A2FDBFCF__INCLUDED_)
#define EA_F6FA3185_3A4D_4043_A499_06D6A2FDBFCF__INCLUDED_

#include "CBaseCommCtl.h"
#include "CBaseDevice.h"
#include "GlobalThreadPool.h"

#include <iostream>
#include <boost/filesystem.hpp>
#include <boost/iostreams/device/file.hpp>
#include <boost/iostreams/stream.hpp>

namespace io = boost::iostreams;

class CPinCtl : private CBaseCommCtl
{

public:

	static bool fileIsExist(const std::string& fileName);
	static CBaseCommCtl* takePinCtl(CBaseDevice* device, const std::string& gpioName);
	static void freePinCtl(CBaseDevice* device, const std::string& gpioName);

	void freePinCtl(const std::string& gpioName);

	virtual ~CPinCtl();

	bool receive(int rcvData);
	uint32_t send(std::list<std::vector<uint8_t> > sendData);
	int setSettings(std::string deviceName);

private:
	CPinCtl(CBaseDevice* device, const std::string& gpioName);

	static std::map<std::string, CPinCtl*> busyPins;

	static const std::string gpioPath;

	std::filebuf fBuf;
	std::fstream fLog;

	uint32_t m_timeout;

};

#endif // !defined(EA_F6FA3185_3A4D_4043_A499_06D6A2FDBFCF__INCLUDED_)
