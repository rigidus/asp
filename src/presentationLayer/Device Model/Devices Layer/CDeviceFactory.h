//============================================================================
// Name        : CDeviceFactory.h
// Author      : aav
// Created on  : 10 февр. 2016 г.
// Version     : v.0.1
// Copyright   : Non nobis, Domine, non nobis, sed nomini tuo da gloriam.
// Description : Abstract device factory
//============================================================================

#ifndef CDEVICEFACTORY_H_
#define CDEVICEFACTORY_H_

#include "devices/CBaseDevice.h"
#include "abstract/testAbstractDevice.h"
#include "abstract/ShlagbaumAbstract.h"
#include "abstract/BsnsLogic.h"
#include "abstract/UserButtonAbstract.h"
#include "abstract/DisplayAbstract.h"
#include "abstract/PrinterAbstract.h"
#include "abstract/ScannerAbstract.h"

class CDeviceFactory
{
	CDeviceFactory() {}

	boost::mutex mut;

public:

	static CDeviceFactory& getFactory()
	{
		static CDeviceFactory* ptr = nullptr;

		boost::mutex::scoped_lock(mut);

		if (ptr == nullptr) ptr = new CDeviceFactory;

		return *ptr;
	}


	CAbstractDevice* deviceFactory(const std::string& abstractName, const std::string& devName)
	{
		{
			std::stringstream log;
			log << "CDeviceFactory::deviceFactory started";
			SetTo::LocalLog("device_factory", trace, log.str());
		}

		std::vector<CAbstractDevice*> devs;

		CAbstractDevice* dev = nullptr;

		// INTEGRATE ABSTRACT SECTION: добавь создание нового абстракта сюда
		{
			dev = createAbstractDevice<AbstractShlagbaum>(abstractName, devName);
			if (dev) devs.push_back(dev);

			dev = createAbstractDevice<BsnsLogic>(abstractName, devName);
			if (dev) devs.push_back(dev);

			dev = createAbstractDevice<AbstractUserButton>(abstractName, devName);
			if (dev) devs.push_back(dev);

			dev = createAbstractDevice<AbstractPrinter>(abstractName, devName);
			if (dev) devs.push_back(dev);

			dev = createAbstractDevice<AbstractDisplay>(abstractName, devName);
			if (dev) devs.push_back(dev);

			dev = createAbstractDevice<AbstractScanner>(abstractName, devName);
			if (dev) devs.push_back(dev);

//			dev = createAbstractDevice<AbstractPassSensor>(abstractName, devName);
//			if (dev) devs.push_back(dev);
//			dev = createAbstractDevice<AbstractPresentSensor>(abstractName, devName);
//			if (dev) devs.push_back(dev);
//			dev = createAbstractDevice<AbstractMassStorage>(abstractName, devName);
//			if (dev) devs.push_back(dev);
//			dev = createAbstractDevice<AbstractKKM>(abstractName, devName);
//			if (dev) devs.push_back(dev);

			dev = createAbstractDevice<CTestAbstractDevice>(abstractName, devName);
			if (dev) devs.push_back(dev);

		}

		if (devs.size() > 1)
		{
			{
				std::stringstream log;
				log << "ERROR! CDeviceFactory::deviceFactory: detected device configuration error: more than 1 abstract device created";
				SetTo::LocalLog("device_factory", error, log.str());
			}

			for (CAbstractDevice* d: devs)
				delete d;

			return nullptr;
		}

		if (devs.size() == 0)
		{
			{
				std::stringstream log;
				log << "ERROR! CDeviceFactory::deviceFactory detected device configuration error: no device created";
				SetTo::LocalLog("device_factory", error, log.str());
			}

			return nullptr;
		}

		{
			std::stringstream log;
			log << "CDeviceFactory::deviceFactory created device: " << abstractName ;
			SetTo::LocalLog("device_factory", debug, log.str());
		}

		return devs[0];
	}

private:
	template<class T>
	const std::string& abstractClassName()
	{
		return T::abstractName;
	}

	template<class T>
	CAbstractDevice* createAbstractDevice(const std::string& abstractName, const std::string& devName)
	{
		CAbstractDevice* dev = nullptr;

		{
			std::stringstream log;
			log << "CDeviceFactory::createAbstractDevice: Try create abstract device '" << abstractName << "' as '" << T::s_abstractName << "'" ;
			SetTo::LocalLog("device_factory", trace, log.str());
		}

		if ( abstractName.find(T::s_abstractName) != std::string::npos)
		{
			dev = T::createDevice(abstractName, devName);
		}

		return dev;
	}

};


#endif /* CDEVICEFACTORY_H_ */
