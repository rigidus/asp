/*
 * CDeviceManager.h
 *
 *  Created on: 8 февр. 2016 г.
 *      Author: alex
 */

#ifndef CDEVICEMANAGER_H_
#define CDEVICEMANAGER_H_

#include <boost/thread/thread.hpp>
#include <boost/thread/mutex.hpp>
#include <boost/smart_ptr.hpp>

namespace devices {

class CDeviceManager {
private:
	CDeviceManager();

	static boost::mutex mut;

public:

	static CDeviceManager* getDeviceManager()
	{
		static CDeviceManager* ptr = nullptr;

		boost::mutex::scoped_lock(mut);

		if (ptr == nullptr) ptr = new CDeviceManager;

		return ptr;
	}

	~CDeviceManager();
};

} /* namespace devices */

#endif /* CDEVICEMANAGER_H_ */
