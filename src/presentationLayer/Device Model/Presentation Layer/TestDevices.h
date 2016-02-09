/*
 * TestDevices.h
 *
 *  Created on: 9 февр. 2016 г.
 *      Author: alex
 */

#ifndef TESTDEVICES_H_
#define TESTDEVICES_H_

#include "CBaseDevice.h"

// abstract classes
class IAbstractDevice
{
public:
	void sendCommand(std::string command, std::string pars)=0;

	// Create concrete device by abstract and concrete names
	// Abstract name describes functional
	// Concrete name describes protocol and communication
	CBaseDevice* createDevice(std::string abstractName, std::string concreteName)
	{

		// TODO: select virtual constructor
			// TODO: call concrete device factory

		return (CBaseDevice*) nullptr;
	}

};


class AbstractShlagbaum: public IAbstractDevice
{

	CBaseDevice* concreteDevice;

public:
	void sendCommand(std::string command, std::string pars)
	{
		// TODO: parse command and parameters and call concrete command of concrete device
	}
};

class AbstractPrinter: public IAbstractDevice
{

};

class AbstractPassSensor: public IAbstractDevice
{

};

class AbstractPresentSensor: public IAbstractDevice
{

};

class AbstractDisplay: public IAbstractDevice
{

};

class AbstractMassStorage: public IAbstractDevice
{

};

class AbstractKKM: public IAbstractDevice
{

};

// concrete classes

class ShlagbaumPalka: public CBaseDevice
{
public:

	static void fn()
	{
		std::cout << "ShlagbaumPalka" << std::endl;
	}
};

class PrinterPechatalka: public CBaseDevice
{

};

class PresentSensorIamhere: public CBaseDevice
{

};

class PassSensorIamgetout: public CBaseDevice
{

};

class DisplayText16x2: public CBaseDevice
{

};

class DataMassStorage: public CBaseDevice
{

};

class KkmPoluchalka: public CBaseDevice
{

};

#endif /* TESTDEVICES_H_ */
