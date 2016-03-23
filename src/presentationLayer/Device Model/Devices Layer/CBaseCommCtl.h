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

#include <boost/assert.hpp>
#include <boost/noncopyable.hpp>
#include <boost/cstdint.hpp>

class CBaseDevice;

using namespace boost;

class CBaseCommCtl: private noncopyable
{

public:
	virtual ~CBaseCommCtl();

	virtual uint32_t send(std::list<std::vector<uint8_t> > sendData);
	virtual uint32_t send(std::vector<uint8_t> sendData);

	CBaseDevice& myDevice()
	{
		return *m_device;
	}

	std::string m_commName;
	std::string m_deviceName;

protected:
	CBaseCommCtl(CBaseDevice* device, const std::string& commName);

	CBaseDevice* m_device;

};

#endif // !defined(EA_96F2CEBA_CC67_4b9a_B21B_8FBBD7D55F20__INCLUDED_)
