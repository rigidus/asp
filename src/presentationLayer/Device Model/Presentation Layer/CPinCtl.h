///////////////////////////////////////////////////////////
//  CPinCtl.h
//  Implementation of the Class CPinCtl
//  Created on:      19-џэт-2016 19:58:08
//  Original author: user-PC
///////////////////////////////////////////////////////////

#if !defined(EA_F6FA3185_3A4D_4043_A499_06D6A2FDBFCF__INCLUDED_)
#define EA_F6FA3185_3A4D_4043_A499_06D6A2FDBFCF__INCLUDED_

#include "CBaseCommCtl.h"

public class CPinCtl : private CBaseCommCtl
{

public:
	CPinCtl();
	virtual ~CPinCtl();
	CPinCtl(const CPinCtl& theCPinCtl);

	bool receive(int rcvData);
	uint32_t send(std::list<std::vector<uint8_t> > sendData);
	int setSettings(std::strring deviceName);

private:
	bool m_default;
	bool m_direction;
	asio::file_stream m_stream;
	uint32_t m_timeout;
	bool m_value;

};
#endif // !defined(EA_F6FA3185_3A4D_4043_A499_06D6A2FDBFCF__INCLUDED_)
