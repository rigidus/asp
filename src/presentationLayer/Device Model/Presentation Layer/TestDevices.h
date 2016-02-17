/*
 * TestDevices.h
 *
 *  Created on: 9 февр. 2016 г.
 *      Author: alex
 */

#ifndef TESTDEVICES_H_
#define TESTDEVICES_H_

#include "CBaseDevice.h"
#include "CAbstractDevice.h"
#include "GlobalThreadPool.h"
#include "CPinCtl.h"
#include "CSettings.h"
#include "device_config.h"
#include "devcomm_config.h"

// concrete classes

class ShlagbaumPalka: public CBaseDevice
{

// CodecType protoCodec;

public:

	ShlagbaumPalka(): CBaseDevice(s_concreteName) {}

	~ShlagbaumPalka()
	{
		CBaseDevice::disconnectFromCommCtl();
	}

	static const std::string s_concreteName;

	std::vector<uint8_t> rcvData;

	virtual void sendCommand(const std::string command, const std::string pars)
	{
		std::cout << "ShlagbaumPalka performs command: " << command << "[" << pars << "]" << std::endl;

		rcvData.clear();
		rcvData.push_back('A');
		rcvData.push_back('C');
		rcvData.push_back('K');
		std::list<std::vector<uint8_t>> data;
		data.push_back(rcvData);

		// command "up"
		if (m_commCtl[0])
			m_commCtl[0]->send(data);

	}

	virtual bool connectToCommCtl()
	{
		return CBaseDevice::connectToCommCtl();
	}

};

const std::string ShlagbaumPalka::s_concreteName = "shlagbaum palka";

//class PrinterPechatalka: public CBaseDevice
//{
//
//};
//
//class PresentSensorIamhere: public CBaseDevice
//{
//
//};
//
//class PassSensorIamgetout: public CBaseDevice
//{
//
//};	for (auto devCtl: devices)
//
//class DisplayText16x2: public CBaseDevice
//{
//
//};
//
//class DataMassStorage: public CBaseDevice
//{
//
//};
//
//class KkmPoluchalka: public CBaseDevice
//{
//
//};

// abstract classes

class AbstractShlagbaum: public CAbstractDevice
{

public:

	AbstractShlagbaum(CBaseDevice* pDevice, const std::string& abstractName):
		CAbstractDevice(pDevice, abstractName) {}

	static const std::string s_abstractName;

	static CAbstractDevice* createDevice(const std::string& abstractName, const std::string& devName)
	{

		std::cout << "Create concrete device " << devName << std::endl;

		CBaseDevice* cDev = nullptr;

		if ( ShlagbaumPalka::s_concreteName == devName)
		{
			cDev = reinterpret_cast<CBaseDevice*> (new ShlagbaumPalka());
		}

		// Connect concrete device to communication devices
		if (cDev != nullptr && cDev->connectToCommCtl())
		{
			return new AbstractShlagbaum(cDev, abstractName);
		}

		return nullptr;
	}

	virtual void sendCommand(const std::string& command, const std::string& pars)
	{
		// TODO: parse command and parameters and call concrete command of concrete device
		device()->sendCommand(command, pars);
	}
};

const std::string AbstractShlagbaum::s_abstractName = "shlagbaum";

//class AbstractPrinter: public IAbstractDevice
//{
//
//};
//
//class AbstractPassSensor: public IAbstractDevice
//{
//
//};
//
//class AbstractPresentSensor: public IAbstractDevice
//{
//
//};
//
//class AbstractDisplay: public IAbstractDevice
//{
//
//};
//
//class AbstractMassStorage: public IAbstractDevice
//{
//
//};
//
//class AbstractKKM: public IAbstractDevice
//{
//
//};


#endif /* TESTDEVICES_H_ */
