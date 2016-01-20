///////////////////////////////////////////////////////////
//  CBaseCommCtl.h
//  Implementation of the Class CBaseCommCtl
//  Created on:      19-џэт-2016 19:58:07
//  Original author: user-PC
///////////////////////////////////////////////////////////

#if !defined(EA_96F2CEBA_CC67_4b9a_B21B_8FBBD7D55F20__INCLUDED_)
#define EA_96F2CEBA_CC67_4b9a_B21B_8FBBD7D55F20__INCLUDED_

public class CBaseCommCtl
{

public:
	CBaseCommCtl();
	virtual ~CBaseCommCtl();
	CBaseCommCtl(const CBaseCommCtl& theCBaseCommCtl);

	bool receive(int rcvData);
	uint32_t send(std::list<std::vector<uint8_t> > sendData);
	int setSettings(std::strring deviceName);

private:
	std::string m_deviceName;

};
#endif // !defined(EA_96F2CEBA_CC67_4b9a_B21B_8FBBD7D55F20__INCLUDED_)
