///////////////////////////////////////////////////////////
//  CBaseCommCtl.h
//  Implementation of the Class CBaseCommCtl
//  Created on:      19-���-2016 19:58:07
//  Original author: user-PC
///////////////////////////////////////////////////////////

#if !defined(EA_96F2CEBA_CC67_4b9a_B21B_8FBBD7D55F20__INCLUDED_)
#define EA_96F2CEBA_CC67_4b9a_B21B_8FBBD7D55F20__INCLUDED_

#include <list>
#include <vector>
#include <string>

#include <boost/cstdint.hpp>

class CBaseCommCtl
{

public:
	CBaseCommCtl();
	virtual ~CBaseCommCtl();
	CBaseCommCtl(const CBaseCommCtl& theCBaseCommCtl);

	virtual bool receive(int rcvData) = 0;
	virtual uint32_t send(std::list<std::vector<uint8_t> > sendData) = 0;
	virtual int setSettings(std::string deviceName) = 0;

private:
	std::string m_deviceName;

};
#endif // !defined(EA_96F2CEBA_CC67_4b9a_B21B_8FBBD7D55F20__INCLUDED_)
