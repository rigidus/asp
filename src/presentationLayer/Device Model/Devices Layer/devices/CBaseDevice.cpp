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


void CBaseDevice::performEvent(std::string& commDeviceName, std::vector<uint8_t>& rcvData)
{
	if (rcvData.size() == 0)
	{
		SetTo::LocalLog("base_device", error, "ERROR! CBaseDevice::performEvent: Data size = 0");
		return;
	}

	{
		std::stringstream log;
		log << "CBaseDevice::performEvent: performs Event from device: " << c_name << ": ";
		for (auto v: rcvData) log << v << " ";

		SetTo::LocalLog("base_device", trace, log.str());
	}

	// TODO: Вызвать установку задачи для клиента по имени абстрактного девайса
	char* beginData = (char*) &rcvData[0];
	char* endData = (char*) &rcvData[rcvData.size()-1];
	std::string answer(beginData, endData);

	SetTo::Client( SetTo::Event, c_name, "answer: ", answer);
}


void CBaseDevice::performTransaction(std::vector<uint8_t>& rcvData)
{
	if (rcvData.size() == 0)
	{
		SetTo::LocalLog("base_device", error, "ERROR! CBaseDevice::performTransaction: Data size = 0");
		return;
	}

	{
		std::stringstream log;
		log << "CBaseDevice::performTransaction: performs Transaction from device: " << c_name << ": ";
		for (auto v: rcvData) log << v << " ";

		SetTo::LocalLog("base_device", trace, log.str());
	}

	// Вызвать установку задачи для клиента по имени абстрактного девайса
	char* beginData = (char*) &rcvData[0];
	char* endData = (char*) &rcvData[rcvData.size()-1];
	std::string answer(beginData, endData);

	SetTo::Client( SetTo::Transaction, c_name, "answer: ", answer);
}


void CBaseDevice::addCommDevice(shared_ptr<CBaseCommCtl> commCtl)
{
	if (commCtl != nullptr)
	{
		m_commCtl.push_back(commCtl);

		{
			std::stringstream log;
			log << "CBaseDevice::addCommDevice: " << commCtl->m_commName << " was added to " << c_name;
			SetTo::LocalLog(c_name, debug, log.str());
		}
	}
}


bool CBaseDevice::connectToCommCtl()
{

	std::vector<std::string> commNames = settings::getCommNamesByDevice(c_name);

	for (auto comm: commNames)
	{
		{
			std::stringstream log;
			log << "CBaseDevice::connectToCommCtl: " << c_name << " is trying to connect to " << comm;
			SetTo::LocalLog(c_name, trace, log.str());
		}

		addCommDevice( takeCommDevice<CPinCtl>(comm) );
		addCommDevice( takeCommDevice<CSerialPortCtl>(comm) );
		addCommDevice( takeCommDevice<CDisplayCtl>(comm));
		addCommDevice( takeCommDevice<CPrnCtl>(comm));
		addCommDevice( takeCommDevice<CCharDevCtl>(comm));

	}

	return m_commCtl.size() == commNames.size();
}


void CBaseDevice::disconnectFromCommCtl()
{

	for (auto comm: m_commCtl)
	{
		CPinCtl::freeCommCtl(this, comm->m_commName);
		CSerialPortCtl::freeCommCtl(this, comm->m_commName);
		CDisplayCtl::freeCommCtl(this, comm->m_commName);
		CPrnCtl::freeCommCtl(this, comm->m_commName);
	}
}

