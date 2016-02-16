/*
 * CBaseDeviceFactory.h
 *
 *  Created on: 10 февр. 2016 г.
 *      Author: alex
 */

#ifndef CBASEDEVICEFACTORY_H_
#define CBASEDEVICEFACTORY_H_

#include "CBaseDevice.h"

class CBaseDeviceFactory
{
	CBaseDeviceFactory() {}

	boost::mutex mut;

public:

	static CBaseDeviceFactory& getFactory()
	{
		static CBaseDeviceFactory* ptr = nullptr;

		boost::mutex::scoped_lock(mut);

		if (ptr == nullptr) ptr = new CBaseDeviceFactory;

		return *ptr;
	}


	IAbstractDevice* deviceFactory(const std::string& abstractName, const std::string& devName)
	{
		IAbstractDevice* dev = nullptr;

		if ( (dev = createAbstractDevice<AbstractShlagbaum>(abstractName, devName)) != nullptr )
			return dev;

//		if ( (dev = createAbstractDevice<AbstractPrinter>(abstractName, devName)) != nullptr )
//			return dev;
//
//		if ( (dev = createAbstractDevice<AbstractPassSensor>(abstractName, devName)) != nullptr )
//			return dev;
//
//		if ( (dev = createAbstractDevice<AbstractPresentSensor>(abstractName, devName)) != nullptr )
//			return dev;
//
//		if ( (dev = createAbstractDevice<AbstractDisplay>(abstractName, devName)) != nullptr )
//			return dev;
//
//		if ( (dev = createAbstractDevice<AbstractMassStorage>(abstractName, devName)) != nullptr )
//			return dev;
//
//		if ( (dev = createAbstractDevice<AbstractKKM>(abstractName, devName)) != nullptr )
//			return dev;

		// TODO: log error, devicename not found
		return dev;
	}

private:
	template<class T>
	const std::string& abstractClassName()
	{
		return T::abstractName;
	}

	template<class T>
	IAbstractDevice* createAbstractDevice(const std::string& abstractName, const std::string& devName)
	{
		IAbstractDevice* dev = nullptr;

		std::cout << "Create abstract device: " << abstractName << std::endl;

		if ( devName.find(T::s_abstractName) != std::string::npos)
		{
			dev = T::createDevice(abstractName, devName);
		}

		return dev;
	}

};


#endif /* CBASEDEVICEFACTORY_H_ */
