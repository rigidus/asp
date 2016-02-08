///////////////////////////////////////////////////////////
//  CBaseDevice.h
//  Implementation of the Class CBaseDevice
//  Created on:      19-���-2016 19:58:07
//  Original author: user-PC
///////////////////////////////////////////////////////////

#if !defined(EA_AAF6E551_FF21_4908_B83B_A548A70782BC__INCLUDED_)
#define EA_AAF6E551_FF21_4908_B83B_A548A70782BC__INCLUDED_

#include "CBaseCodec.h"
#include "CBaseCommCtl.h"

class CBaseDevice
{

public:
	CBaseDevice();
	virtual ~CBaseDevice();
	CBaseDevice(const CBaseDevice& theCBaseDevice);

	CBaseCodec& GetCBaseCodec();
	void SetCBaseCodec(CBaseCodec* newVal);
	CBaseCommCtl& GetCBaseCommCtl();
	void SetCBaseCommCtl(CBaseCommCtl* newVal);

private:
	CBaseCodec m_codec;
	CBaseCommCtl* m_commCtl;
	CBaseCodec *m_CBaseCodec;
	CBaseCommCtl *m_CBaseCommCtl;

};
#endif // !defined(EA_AAF6E551_FF21_4908_B83B_A548A70782BC__INCLUDED_)
