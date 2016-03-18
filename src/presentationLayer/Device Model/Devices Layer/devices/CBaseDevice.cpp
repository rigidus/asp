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
	std::cout << "CBaseDevice::performEvent: performs Event from device: " << c_name << ": ";

	if (rcvData.size() == 0)
	{
		std::cout << "ERROR! CBaseDevice::performEvent: Data size = 0" << std::endl;
		return;
	}

	for (auto v: rcvData) std::cout << v << " ";
	std::cout << std::endl;

	// TODO: Вызвать установку задачи для клиента по имени абстрактного девайса
	char* beginData = (char*) &rcvData[0];
	char* endData = (char*) &rcvData[rcvData.size()-1];
	std::string answer(beginData, endData);

	setCommandTo::Client( setCommandTo::Event, c_name, "answer: ", answer);
}


void CBaseDevice::performTransaction(std::vector<uint8_t>& rcvData)
{
	std::cout << "CBaseDevice::performTransaction: performs Transaction from device: " << c_name << ": " << std::endl;

	if (rcvData.size() == 0)
	{
		std::cout << "ERROR! CBaseDevice::performTransaction: Data size = 0" << std::endl;
		return;
	}

	for (auto v: rcvData) std::cout << v << " ";
	std::cout << std::endl;

	// Вызвать установку задачи для клиента по имени абстрактного девайса
	char* beginData = (char*) &rcvData[0];
	char* endData = (char*) &rcvData[rcvData.size()-1];
	std::string answer(beginData, endData);

	setCommandTo::Client( setCommandTo::Transaction, c_name, "answer: ", answer);
}


void CBaseDevice::addCommDevice(shared_ptr<CBaseCommCtl> commCtl)
{
	if (commCtl != nullptr)
	{
		m_commCtl.push_back(commCtl);

		std::cout << "CBaseDevice::addCommDevice: " << commCtl->m_commName << " was added to " << c_name << std::endl;
	}
}


bool CBaseDevice::connectToCommCtl()
{

	std::vector<std::string> commNames = settings::getCommNamesByDevice(c_name);

	for (auto comm: commNames)
	{
		std::cout << "CBaseDevice::connectToCommCtl: " << c_name << " connected to " << comm << std::endl;

		addCommDevice( takeCommDevice<CPinCtl>(comm) );
		addCommDevice( takeCommDevice<CSerialPortCtl>(comm) );
		addCommDevice( takeCommDevice<CDisplayCtl>(comm));

	}

	return m_commCtl.size() == commNames.size();
}


void CBaseDevice::disconnectFromCommCtl()
{

	for (auto comm: m_commCtl)
	{
		CPinCtl::freeCommCtl(this, comm->m_commName);
		CSerialPortCtl::freeCommCtl(this, comm->m_commName);
	}
}

