///////////////////////////////////////////////////////////
//  CBaseDevice.cpp
//  Implementation of the Class CBaseDevice
//  Created on:      19-���-2016 19:58:07
//  Original author: user-PC
///////////////////////////////////////////////////////////

#include "CBaseDevice.h"


CBaseDevice::CBaseDevice(const std::string& str):
	c_name(str)
{

}



CBaseDevice::~CBaseDevice(){

}


const std::vector< shared_ptr<CBaseCommCtl> >& CBaseDevice::getCommCtl(){

	return m_commCtl;
}


void CBaseDevice::addCommDevice(shared_ptr<CBaseCommCtl> commCtl)
{
	if (commCtl != nullptr)
	{
		m_commCtl.push_back(commCtl);

		std::cout << "addCommDevice: " << commCtl->m_commName << " connected to " << c_name << std::endl;
	}
}


bool CBaseDevice::connectToCommCtl()
{

	std::vector<std::string> commNames = settings::getCommNamesByDevice(c_name);

	if (commNames.size() == 0)
		return false;

	for (auto comm: commNames)
	{
		std::cout << "  " << comm << std::endl;

		addCommDevice( takeCommDevice<CPinCtl>(comm) );
		addCommDevice( takeCommDevice<CSerialPortCtl>(comm) );

	}

	return m_commCtl.size() == commNames.size();
}


void CBaseDevice::disconnectFromCommCtl()
{

	for (auto comm: m_commCtl)
	{
		CPinCtl::freeCommCtl(this, comm->m_commName);

		// TODO: все сыпется при добавлении этой строчки
		CSerialPortCtl::freeCommCtl(this, comm->m_commName);
	}
}

