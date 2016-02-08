///////////////////////////////////////////////////////////
//  CPinCtl.h
//  Implementation of the Class CPinCtl
//  Created on:      19-���-2016 19:58:08
//  Original author: user-PC
///////////////////////////////////////////////////////////

#if !defined(EA_F6FA3185_3A4D_4043_A499_06D6A2FDBFCF__INCLUDED_)
#define EA_F6FA3185_3A4D_4043_A499_06D6A2FDBFCF__INCLUDED_

#include "CBaseCommCtl.h"

#include <iostream>
#include <boost/filesystem/fstream.hpp>

using namespace boost;

class CPinCtl : private CBaseCommCtl
{

public:

	static CBaseCommCtl* getPintCtl(uint32_t pinNum);

	virtual ~CPinCtl();
	CPinCtl(const CPinCtl& theCPinCtl);

	bool receive(int rcvData);
	uint32_t send(std::list<std::vector<uint8_t> > sendData);
	int setSettings(std::string deviceName);

private:
	CPinCtl();
	filesystem::fstream m_direction;
	filesystem::fstream m_pullup;
	filesystem::fstream m_value;
	uint32_t m_timeout;

};
#endif // !defined(EA_F6FA3185_3A4D_4043_A499_06D6A2FDBFCF__INCLUDED_)
