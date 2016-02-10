/*
 * TestDevices.h
 *
 *  Created on: 9 февр. 2016 г.
 *      Author: alex
 */

#ifndef TESTDEVICES_H_
#define TESTDEVICES_H_

#include "CBaseDevice.h"

// concrete classes

class ShlagbaumPalka: public CBaseDevice
{
public:

	ShlagbaumPalka(): CBaseDevice(s_concreteName) {}

	static const std::string s_concreteName;

	virtual void sendCommand(const std::string command, const std::string pars)
	{
		std::cout << "ShlagbaumPalka performs command: " << command << "[" << pars << "]" << std::endl;
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
//};
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
class IAbstractDevice: private noncopyable
{
private:
	CBaseDevice* concreteDevice;
	const std::string c_abstractName;

public:

	IAbstractDevice(CBaseDevice* pDevice, const std::string& abstractName):
		concreteDevice(pDevice),
		c_abstractName(abstractName) {

		std::cout << "Abstract Device: " << abstractName << " created." << std::endl;
	}

	virtual ~IAbstractDevice() {}

	CBaseDevice* device() { return concreteDevice; }
	const std::string& deviceAbstractName() { return c_abstractName; }
	const std::string& deviceConcreteName() { return concreteDevice->c_name; }

	virtual void sendCommand(const std::string& command, const std::string& pars)=0;


};


class AbstractShlagbaum: public IAbstractDevice
{

public:

	AbstractShlagbaum(CBaseDevice* pDevice, const std::string& abstractName):
		IAbstractDevice(pDevice, abstractName) {}

	static const std::string s_abstractName;

	static IAbstractDevice* createDevice(const std::string& abstractName, const std::string& devName)
	{

		if ( ShlagbaumPalka::s_concreteName == devName)
		{
			CBaseDevice* cDev = new ShlagbaumPalka;
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
